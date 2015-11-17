/*******************************************************************************
 * 
 * A fast JSON parser implementing RFC 7159.
 * 
 * The most prominent change compared to the initial revision is the allowance
 * of all data types as root values, not just objects and arrays.
 * 
 * Usage_Hints:
 *   $(UL
 *     $(LI This parser only supports UTF-8 without BOM.)
 *     $(LI When a JSON object has duplicate keys, the last one in the set will
 *          determine the value of associative-array entries or struct fields.)
 *     $(LI `BigInt` and large number parsing are not implemented currently, but
 *          all integral types as well as minimal exact representations of many
 *          `double` values are supported.)
 *   )
 * 
 * Authors:
 *   $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 * 
 * Copyright:
 *   © 2015 $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 * 
 * License:
 *   $(LINK2 http://www.gnu.org/licenses/gpl-3.0, GNU General Public License 3.0)
 * 
 **************************************/
module fast.json;

import core.bitop;
import core.simd;
import core.stdc.stdlib;
import core.stdc.string;

version (GNU) import gcc.attribute;
version (GNU) import gcc.builtins;
version (LDC) import ldc.gccbuiltins_x86;

import std.algorithm;
import std.ascii;
import std.conv;
import std.datetime;
import std.exception;
import std.file;
import std.json;
import std.range;
import std.stdio;
import std.string;
import std.traits;
import std.uni;

import fast.buffer;
import fast.cstring;
import fast.helpers;
import fast.parsing;


/*******************************************************************************
 * 
 * Loads a file as JSON text and validates the used parts. This includes a UTF-8
 * validation on strings.
 *
 * Params:
 *   fname = The file name to load.
 *
 * Returns:
 *   A JSON file object exposing the `Json` API.
 *
 **************************************/
auto parseJSONFile(uint vl = validateUsed)(in char[] fname)
{ return parseJSONFile(fname.representation); }

/// ditto
auto parseJSONFile(uint vl = validateUsed)(in ubyte[] fname)
{ return Json!vl.File(fname); }


/*******************************************************************************
 * 
 * Loads a JSON string and validates the used parts. This includes a UTF-8
 * validation on strings.
 *
 * Params:
 *   text = The string to load.
 *
 * Returns:
 *   A `Json` struct.
 *
 **************************************/
auto parseJSON(uint vl = validateUsed, T : const(char)[])(T text) nothrow
{ return parseJSONTextImpl!vl(text); }


/*******************************************************************************
 * 
 * Load a file as JSON text that is considered 100% correct. No checks will be
 * performed, not even if you try to read a number as a string.
 *
 * Params:
 *   fname = The file name to load.
 *
 * Returns:
 *   A JSON file object exposing the `Json` API.
 *
 **************************************/
auto parseTrustedJSONFile(uint vl = trustedSource)(in char[] fname)
{ return parseTrustedJSONFile!vl(fname.representation); }

/// ditto
auto parseTrustedJSONFile(uint vl = trustedSource)(in ubyte[] fname)
{ return Json!vl.File(fname); }


/*******************************************************************************
 * 
 * Load a JSON string that is considered 100% correct. No checks will be
 * performed, not even if you try to read a number as a string.
 *
 * Params:
 *   text = The string to load.
 *
 * Returns:
 *   A `Json` struct.
 *
 **************************************/
auto parseTrustedJSON(uint vl = trustedSource, T : const(char)[])(T text) nothrow
{ return parseJSONTextImpl!vl(text); }


private auto parseJSONTextImpl(uint vl, T : const(char)[])(T text)
{
	// We need to append 16 zero bytes for SSE to work, and if that reallocates the char[]
	// we can declare it unique/immutable and don't need to allocate when returning JSON strings.
	auto oldPtr = text.ptr;
	text ~= "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
	static if (!is(T == string)) if (oldPtr !is text.ptr)
		return Json!(vl, false)(assumeUnique(text));
	return Json!(vl, false)(text);
}


/*******************************************************************************
 * 
 * Validates a JSON text file.
 *
 * Params:
 *   fname = The file name to load.
 *
 * Throws:
 *   JSONException on validation errors.
 *
 **************************************/
void validateJSONFile(in char[] fname)
{ validateJSONFile(fname.representation); }

/// ditto
void validateJSONFile(in ubyte[] fname)
{ Json!(validateAll, true).File(fname).skipValue(); }


/// JSON data types returned by `peek`.
enum DataType : ubyte
{
	string, number, object, array, boolean, null_
}


/// Validation strength of JSON parser
enum
{
	trustedSource,  /// Assume 100% correct JSON and speed up parsing.
	validateUsed,   /// Ignore errors in skipped portions.
	validateAll,    /// Do a complete validation of the JSON data.
}


/// A UDA used to remap enum members or struct field names to JSON strings.
struct JsonMapping { string[string] map; }


/*******************************************************************************
 * 
 * This is a forward JSON parser for picking off items of interest on the go.
 * It neither produces a node structure, nor does it produce events. Instead you
 * can peek at the value type that lies ahead and/or directly consume a JSON
 * value from the parser. Objects and arrays can be iterated over via `foreach`,
 * while you can also directly ask for one or multiple keys of an object.
 * 
 * Prams:
 *   vl = Validation level. Any of `trustedSource`, `validateUsed` or
 *        `validateAll`.
 *   validateUtf8 = If validation is enabled, this also checks UTF-8 encoding
 *                  of JSON strings.
 * 
 **************************************/
struct Json(uint vl = validateUsed, bool validateUtf8 = vl > trustedSource)
	if (vl > trustedSource || !validateUtf8)
{
private:

	enum isTrusted     = vl == trustedSource;
	enum skipAllInter  = vl == trustedSource;
	enum isValidating  = vl >= validateUsed;
	enum isValidateAll = vl == validateAll;

	const(char)*    m_text    = void;
	const(char*)    m_start   = void;
	size_t          m_nesting = 0;
	RaiiArray!char  m_mem;
	bool            m_isString = false;


public:

	@disable this();
	@disable this(this);


	/*******************************************************************************
	 * 
	 * Constructor taking a `string` for fast slicing.
	 * 
	 * JSON strings without escape sequences can be returned as slices.
	 *
	 * Params:
	 *   text = The JSON text to parse.
	 *
	 **************************************/
	nothrow
	this(string text)
	{
		import core.memory;
		m_isString = GC.query(text.ptr) !is ReturnType!(GC.query).init;
		this(cast(const char[]) text);
	}


	/*******************************************************************************
	 * 
	 * Constructor taking a `const char[]`.
	 * 
	 * JSON strings allocate on the GC heap when returned.
	 *
	 * Params:
	 *   text = The JSON text to parse.
	 *
	 **************************************/
	@nogc pure nothrow
	this(const char[] text)
	{
		m_start = m_text = text.ptr;
		skipWhitespace!false();
	}


	/+
	 ╔══════════════════════════════════════════════════════════════════════════════
	 ║ ⚑ String
	 ╚══════════════════════════════════════════════════════════════════════════════
	 +/

	/*******************************************************************************
	 * 
	 * Reads a string off the JSON text.
	 *
	 * Params:
	 *   allowNull = Allow `null` as a valid option for the string.
	 *
	 * Returns:
	 *   A GC managed string.
	 *
	 **************************************/
	string read(T)(bool allowNull = true) if (is(T == string))
	{
		if (!allowNull || peek == DataType.string)
		{
			auto borrowed = borrowString();
			return m_isString ? borrowed.assumeUnique() : borrowed.idup;
		}
		return readNull();
	}


	/*******************************************************************************
	 * 
	 * Reads an enumeration off the JSON text.
	 *
	 **************************************/
	T read(T)() if (is(T == enum))
	{
		enum mapping = buildRemapTable!T;
		auto oldPos = m_text;
		auto text = borrowString();
		foreach (m; mapping)
			if (text.length == m.json.length && memcmp(text.ptr, m.json.ptr, m.json.length) == 0)
				return m.d;
		m_text = oldPos;
		static if (isValidating)
			handleError(format("Could not find enum member `%s` in `%s`", text, T.stringof));
		assert(0);
	}


	/*******************************************************************************
	 * 
	 * Reads a string off the JSON text with limited lifetime.
	 * 
	 * The reference to this slice is not guaranteed to be valid after the JSON
	 * parser has been destroyed or another object key or string value has been
	 * parsed. So make a copy before you continue parsing.
	 *
	 * Returns:
	 *   If the string had no escape sequences in it, the returned array is a
	 *   slice of the JSON text buffer, otherwise temporary copy.
	 *
	 **************************************/
	const(char)[] borrowString()
	{
		expect('"', "at start of string");
		auto escFreeStart = m_text;

		if (scanString!validateUtf8())
		{
			// Fast path here is to return a slice of the JSON if it doesn't contain escapes.
			size_t length = m_text - escFreeStart;
			skipOnePlusWhitespace!skipAllInter();
			return escFreeStart[0 .. length];
		}
		else
		{
			// Otherwise we copy to a separate memory area managed by this parser instance.
			size_t length = 0;
			bool eos = false;
			goto CopyToBuffer;
			do
			{
				do
				{
					m_mem.capacityNeeded( length + 4 );
					uint decoded = decodeEscape( &m_mem[length] );
					length += decoded;
				}
				while (*m_text == '\\');

				escFreeStart = m_text;
				eos = scanString!validateUtf8();
			CopyToBuffer:
				size_t escFreeLength = m_text - escFreeStart;
				m_mem.capacityNeeded( length + escFreeLength );
				memcpy( m_mem.ptr + length, escFreeStart, escFreeLength );
				length += escFreeLength;
			}
			while (!eos);
			skipOnePlusWhitespace!skipAllInter();
			return m_mem[0 .. length];
		}
	}


	private bool scanString(bool validate)()
	{
		static if (validate)
		{
			import std.system;
			
			while (true)
			{
				// Stop for control-characters, \, " and anything non-ASCII.
				m_text.seekToRanges!"\0\x1F\"\"\\\\\x7F\xFF";
				
				// Handle printable ASCII range
				if (*m_text == '"')
					return true;
				if (*m_text == '\\')
					return false;
				
				// Anything else better be UTF-8
				uint u = *cast(uint*) m_text;
				version (LittleEndian) u = bswap(u);
				
				// Filter overlong ASCII and missing follow byte.
				if (
					(u & 0b111_00000_11_000000_00000000_00000000) == 0b110_00000_10_000000_00000000_00000000 &&
					(u > 0b110_00001_10_111111_11111111_11111111))
					m_text += 2;
				// Handle overlong representation, UTF-16 surrogate pairs and missing follow bytes.
				else if (
					(u & 0b1111_0000_11_000000_11_000000_00000000) == 0b1110_0000_10_000000_10_000000_00000000 &&
					(u & 0b0000_1111_00_100000_00_000000_00000000) != 0b0000_1101_00_100000_00_000000_00000000 &&
					(u > 0b1110_0000_10_011111_10_111111_11111111))
					m_text += 3;
				// Handle missing follow bytes, Handle overlong representation and out of valid range (max. 0x10FFFF)
				else if (
					(u & 0b11111_000_11_000000_11_000000_11_000000) == 0b11110_000_10_000000_10_000000_10_000000 &&
					(u > 0b11110_000_10_001111_10_111111_10_111111) && (u < 0b11110_100_10_010000_10_000000_10_000000))
					m_text += 4;
				// Handle invalid code units.
				else if (*m_text < ' ' || *m_text == 0x7F)
					expectNot("is a disallowed control character in strings");
				else if (*m_text >= 0x80 && *m_text <= 0xBF)
					expectNot("is a UTF-8 follow byte and cannot start a sequence");
				else
					expectNot("is not a valid UTF-8 sequence start");
			}
		}
		else
		{
			m_text.seekToAnyOf!("\\\"\0");
			return *m_text == '"';
		}
	}


	private int matchString(string key)()
	{
		return m_text.fixedTermStrCmp!(char, key, "\"\0", "\\")(&stringCompareCallback);
	}


	private bool stringCompareCallback(ref immutable(char)* key, ref const(char)* str)
	{
		do
		{
			auto key4 = cast(char[4]*) key;
			char[4] buf = *key4;
			uint bytes = decodeEscape(buf.ptr);
			if (buf !is *key4)
				return false;
			key += bytes;
		}
		while (str[0] == '\\');
		return true;
	}


	private static immutable escapes = {
		char[256] result = '\0';
		result['"'] = '"';
		result['\\'] = '\\';
		result['/'] = '/';
		result['b'] = '\b';
		result['f'] = '\f';
		result['n'] = '\n';
		result['r'] = '\r';
		result['t'] = '\t';
		return result;
	}();


	private void skipEscape()
	{
		static if (isValidateAll)
		{
			if (m_text[1] != 'u')
			{
				// Normal escape sequence. 2 bytes removed.
				if (!escapes[*++m_text])
					expectNot("in escape sequence");
				m_text++;
			}
			else
			{
				// UTF-16
				m_text += 2;
				decodeUtf16HexToCodepoint();
			}
		}
		else m_text += 2;
	}


	private uint decodeEscape(scope char* dst)
	{
		if (m_text[1] != 'u')
		{
			// Normal escape sequence. 2 bytes removed.
			dst[0] = escapes[m_text[1]];
			static if (isValidating)
				if (!dst[0])
					handleError("Invalid escape sequence");
			m_text += 2;
			return 1;
		}
		else
		{
			// UTF-16
			m_text += 2;
			uint cp = decodeUtf16HexToCodepoint();

			if (cp >= 0xD800 && cp <= 0xDBFF)
			{
				dst[0] = cast(char)(0b11110_000 | cp >> 18);
				dst[1] = cast(char)(0b10_000000 | cp >> 12 & 0b00_111111);
				dst[2] = cast(char)(0b10_000000 | cp >> 6  & 0b00_111111);
				dst[3] = cast(char)(0b10_000000 | cp       & 0b00_111111);
				return 4;
			}
			else if (cp >= 0x800)
			{
				dst[0] = cast(char)(0b1110_0000 | cp >> 12);
				dst[1] = cast(char)(0b10_000000 | cp >> 6  & 0b00_111111);
				dst[2] = cast(char)(0b10_000000 | cp       & 0b00_111111);
				return 3;
			}
			else if (cp >= 0x80)
			{
				dst[0] = cast(char)(0b110_00000 | cp >> 6);
				dst[1] = cast(char)(0b10_000000 | cp       & 0b00_111111);
				return 2;
			}
			else
			{
				dst[0] = cast(char)(cp);
				return 1;
			}
		}
	}


	private dchar decodeUtf16HexToCodepoint()
	{
		import std.typecons;

		uint cp, hi;
		foreach (i; staticIota!(0, 2))
		{
			static if (isValidating)
			{
				if (auto badByte = hexDecode4(m_text, cp))
				{
					m_text = badByte;
					expectNot("is not a hex digit");
				}
			}
			else
			{
				cp = hexDecode4(m_text);
			}
			
			static if (i == 0)
			{
				// Is this a high surrogate (followed by a low surrogate) or not ?
				if (cp < 0xD800 || cp > 0xDBFF)
					break;
				hi = cp - 0xD800 + 0x40 << 10;
			}
			else static if (i == 1)
			{
				static if (isValidating)
				{
					if (cp < 0xDC00 || cp > 0xDFFF)
						handleError("The UTF-16 escape produced an invalid code point.");
					cp -= 0xDC00;
				}
				cp |= hi;
			}
		}

		static if (isValidating)
			if (cp > 0x10FFFF || cp >= 0xD800 && cp <= 0xDFFF)
				handleError("The UTF-16 escape produced an invalid code point.");

		return cp;
	}


	private void skipString(bool skipInter)()
	{
		m_text++;
		skipRestOfString!skipInter();
	}


	private void skipRestOfString(bool skipInter)()
	{
		while (!scanString!isValidateAll())
			skipEscape();
		skipOnePlusWhitespace!skipInter();
	}


	/+
	 ╔══════════════════════════════════════════════════════════════════════════════
	 ║ ⚑ Number
	 ╚══════════════════════════════════════════════════════════════════════════════
	 +/

	/*******************************************************************************
	 * 
	 * Reads a number off the JSON text.
	 * 
	 * If you ask for an unsigned value, no minus sign will be accepted in the JSON,
	 * otherwise all features of JSON numbers will be available. In particular large
	 * integers can be given in scientific notation.
	 *
	 * Params:
	 *   N = Built-in numerical type that should be returned.
	 *
	 * Returns:
	 *   The parsed number.
	 *
	 * Throws:
	 *   JSONException, on invalid JSON or integer overflow.
	 *
	 **************************************/
	N read(N)() if (isNumeric!N && !is(N == enum))
	{
		N n = void;
		static if (isUnsigned!N)
			enum NumberOptions opt = {};
		else
			enum NumberOptions opt = { minus:true };
		if (parseNumber!opt(m_text, n))
			skipWhitespace!skipAllInter();
		else static if (isValidating)
			handleError(format("Could not convert JSON number to `%s`", N.stringof));
		return n;
	}


	private void skipNumber(bool skipInter)()
	{
		static if (isValidateAll)
		{
			if (*m_text == '-')
				m_text++;
			if (*m_text == '0')
				m_text++;
			else
				trySkipDigits();
			if (*m_text == '.')
			{
				m_text++;
				trySkipDigits();
			}
			if ((*m_text | 0x20) == 'e')
			{
				m_text++;
				if (*m_text == '+' || *m_text == '-')
					m_text++;
				trySkipDigits();
			}
			skipWhitespace!false();
		}
		else
		{
			m_text.skipCharRanges!"\t\n\r\r  ++-.09EEee";
			static if (skipInter)
				m_text.skipAllOf!"\t\n\r ,";
		}
	}


	static if (isValidateAll)
	{
		private void trySkipDigits()
		{
			if (*m_text - '0' > 9)
				expectNot("in number literal");
			m_text.skipAllOf!"0123456789";
		}
	}


	/+
	 ╔══════════════════════════════════════════════════════════════════════════════
	 ║ ⚑ Object
	 ╚══════════════════════════════════════════════════════════════════════════════
	 +/

	/*******************************************************************************
	 * 
	 * Reads a plain old data struct off the JSON text.
	 *
	 * Params:
	 *   T = Type of struct that should be returned.
	 *
	 * Returns:
	 *   A struct of type `T`.
	 *
	 **************************************/
	T read(T)() if (is(T == struct) && __traits(isPOD, T))
	{
		nest('{', "on start of object");

		T t;
		if (*m_text != '}') while (true)
		{
			auto key = borrowString();
			static if (!skipAllInter)
			{
				expect(':', "between key and value");
				skipWhitespace!false();
			}

			enum mapping = buildRemapTable!T;
			foreach (m; mapping)
			{
				if (key.length == m.json.length && memcmp(key.ptr, m.json.ptr, m.json.length) == 0)
				{
					mixin("alias keyT = typeof(T." ~ m.d ~ ");");
					mixin("t." ~ m.d ~ " = read!keyT;");
					goto Success;
				}
			}
			skipValue();

		Success:
			if (*m_text == '}')
				break;

			static if (!skipAllInter)
			{
				expect(',', "between key-value pairs");
				skipWhitespace!false();
			}
		}
		
		unnest();
		return t;
	}


	/*******************************************************************************
	 * 
	 * Reads a plain old data struct or `null` off the JSON text.
	 * 
	 * Params:
	 *   T = Type of struct pointer that should be returned.
	 *
	 * Returns:
	 *   A pointer to a newly filled struct of type `T` on the GC heap.
	 *
	 **************************************/
	T read(T)() if (is(PointerTarget!T == struct) && __traits(isPOD, PointerTarget!T))
	{
		if (peek == DataType.null_)
			return readNull();
		T tp = new PointerTarget!T;
		*tp = read!(PointerTarget!T)();
		return tp;
	}


	/*******************************************************************************
	 * 
	 * Reads an associative-array off a JSON text.
	 * 
	 * The key type must be `string`, the value type can be any type otherwise
	 * supported by the parser.
	 *
	 * Params:
	 *   T = The type of AA to return.
	 *
	 * Returns:
	 *   A newly filled associative array.
	 *
	 **************************************/
	T read(T)() if (is(KeyType!T == string))
	{
		T aa;
		foreach (key; byKey)
			aa[key] = read!(ValueType!T)();
		return aa;
	}


	/*******************************************************************************
	 * 
	 * An alias to the `singleKey` method. Instead of `json.singleKey!"something"`
	 * you can write `json.something`. Read the notes on `singleKey`.
	 *
	 **************************************/
	alias opDispatch = singleKey;


	/*******************************************************************************
	 * 
	 * Skips all keys of an object except the first occurence with the given key
	 * name.
	 *
	 * Params:
	 *   name = the key name of interest
	 *
	 * Returns:
	 *   A temporary struct, a proxy to the parser, that will automatically seek to
	 *   the end of the current JSON object on destruction.
	 *
	 * Throws:
	 *   JSONException when the key is not found in the object or parsing errors
	 *   occur.
	 * 
	 * Note:
	 *   Since this is an on the fly parser, you can only get one key from an
	 *   object with this method. Use `keySwitch` or `foreach(key; json)` to get
	 *   values from multiple keys.
	 * 
	 * See_Also:
	 *   keySwitch
	 *
	 **************************************/
	@property SingleKey singleKey(string name)()
	{
		nest('{', "on start of object");
		
		if (*m_text != '}') while (true)
		{
			auto key = borrowString();
			static if (!skipAllInter)
			{
				expect(':', "between key and value");
				skipWhitespace!false();
			}
			
			if (key.length == name.length && memcmp(key.ptr, name.ptr, name.length) == 0)
				return SingleKey(this);

			skipValueImpl!skipAllInter();
			
			if (*m_text == '}')
				break;
			
			static if (!skipAllInter)
			{
				expect(',', "between key-value pairs");
				skipWhitespace!false();
			}
		}
		
		unnest();
		static if (isValidating)
			handleError("Key not found.");
		assert(0);
	}


	/*******************************************************************************
	 * 
	 * Selects from a set of given keys in an object and calls the corresponding
	 * delegate. The difference to `singleKey` when invoked with a single key is
	 * that `keySwitch` will not error out if the key is non-existent and may
	 * trigger the delegate multiple times, if the JSON object has duplicate keys.
	 *
	 * Params:
	 *   Args = the names of the keys
	 *   dlg = the delegates corresponding to the keys
	 *
	 * Throws:
	 *   JSONException when the key is not found in the object or parsing errors
	 *   occur.
	 * 
	 **************************************/
	void keySwitch(Args...)(scope void delegate()[Args.length] dlg...)
	{
		nest('{', "on start of object");
		
		if (*m_text != '}') while (true)
		{
			auto key = borrowString();
			static if (!skipAllInter)
			{
				expect(':', "between key and value");
				skipWhitespace!false();
			}
			
			auto oldPos = m_text;
			foreach (i, arg; Args)
			{
				if (key.length == arg.length && memcmp(key.ptr, arg.ptr, arg.length) == 0)
				{
					dlg[i]();
					goto Next;
				}
			}
			skipValue();
			
		Next:
			if (*m_text == '}')
				break;
			
			static if (!skipAllInter) if (oldPos !is m_text)
			{
				expect(',', "after key-value pair");
				skipWhitespace!false();
			}
		}
		
		unnest();
	}
	
	
	private int byKeyImpl(scope int delegate(ref const char[]) foreachBody)
	{
		nest('{', "at start of foreach over object");

		int result = 0;
		if (*m_text != '}') while (true)
		{
			auto key = borrowString();
			static if (!skipAllInter)
			{
				expect(':', "between key and value");
				skipWhitespace!false;
			}

			if (iterationGuts!"{}"(result, key, foreachBody, "after key-value pair"))
				break;
		}

		unnest();
		return result;
	}


	/*******************************************************************************
	 * 
	 * Iterate the keys of an JSON object with `foreach`.
	 * 
	 * Notes:
	 *   $(UL
	 *     $(LI If you want to store the key, you need to duplicate it.)
	 *   )
	 * 
	 * Example:
	 * ---
	 * uint id;
	 * foreach (key; json.byKey)
	 *     if (key == "id")
	 *         id = json.read!uint;
	 * ---
	 **************************************/
	@safe @nogc pure nothrow
	@property int delegate(scope int delegate(ref const char[])) byKey()
	{
		return &byKeyImpl;
	}


	/+
	 ╔══════════════════════════════════════════════════════════════════════════════
	 ║ ⚑ Array handling
	 ╚══════════════════════════════════════════════════════════════════════════════
	 +/

	/*******************************************************************************
	 * 
	 * Reads a dynamic array off the JSON text.
	 * 
	 **************************************/
	T read(T)() if (isDynamicArray!T && !isSomeString!T)
	{
		import std.array;
		Appender!T app;
		foreach (i; this)
			app.put(read!(typeof(T.init[0])));
		return app.data;
	}


	/*******************************************************************************
	 * 
	 * Reads a static array off the JSON text.
	 * 
	 * When validation is enabled, it is an error if the JSON array has a different
	 * length lengths don't match up. Otherwise unset elements receive their initial
	 * value.
	 *
	 **************************************/
	T read(T)() if (isStaticArray!T)
	{
		T sa = void;
		size_t cnt;
		foreach (i; this)
		{
			if (i < T.length)
				sa[i] = read!(typeof(T.init[0]));
			cnt = i + 1;
		}
		static if (isValidating)
		{
			if (cnt != T.length)
				handleError(format("Static array size mismatch. Expected %s, got %s", T.length, cnt));
		}
		else
		{
			foreach (i; cnt .. T.length)
				sa[i] = T.init;
		}
		return sa;
	}


	/*******************************************************************************
	 * 
	 * Iterate over a JSON array via `foreach`.
	 *
	 **************************************/
	int opApply(scope int delegate(const size_t) foreachBody)
	{
		nest('[', "at start of foreach over array");

		int result = 0;
		if (*m_text != ']') for (size_t idx = 0; true; idx++)
			if (iterationGuts!"[]"(result, idx, foreachBody, "after array element"))
				break;

		unnest();
		return result;
	}


	/+
	 ╔══════════════════════════════════════════════════════════════════════════════
	 ║ ⚑ Boolean
	 ╚══════════════════════════════════════════════════════════════════════════════
	 +/

	/*******************************************************************************
	 * 
	 * Reads a boolean value off the JSON text.
	 *
	 **************************************/
	bool read(T)() if (is(T == bool))
	{
		return skipBoolean!(skipAllInter, isValidating)();
	}


	private bool skipBoolean(bool skipInter, bool validate = isValidateAll)()
	{
		static immutable char[4][2] keywords = [ "true", "alse" ];
		auto isFalse = *m_text == 'f';
		static if (validate)
			if (*cast(char[4]*) &m_text[isFalse] != keywords[isFalse])
				handleError("`true` or `false` expected.");
		m_text += isFalse ? 5 : 4;
		skipWhitespace!skipInter();
		return !isFalse;
	}


	/+
	 ╔══════════════════════════════════════════════════════════════════════════════
	 ║ ⚑ Null
	 ╚══════════════════════════════════════════════════════════════════════════════
	 +/

	/*******************************************************************************
	 * 
	 * Reads `null` off the JSON text.
	 *
	 **************************************/
	typeof(null) readNull()
	{
		skipNull!(skipAllInter, isValidating)();
		return null;
	}


	private void skipNull(bool skipInter, bool validate = isValidateAll)()
	{
		static if (validate)
			if (*cast(const uint*) m_text != *cast(const uint*) "null".ptr)
				handleError("`null` expected.");
		m_text += 4;
		skipWhitespace!skipInter();
	}


	/+
	 ╔══════════════════════════════════════════════════════════════════════════════
	 ║ ⚑ Helpers and Error Handling
	 ╚══════════════════════════════════════════════════════════════════════════════
	 +/

	/*******************************************************************************
	 * 
	 * Skips the next JSON value if you are not interested.
	 *
	 **************************************/
	void skipValue()
	{
		skipValueImpl!skipAllInter();
	}


	private void skipValueImpl(bool skipInter)()
	{
		with (DataType) final switch (peek)
		{
			case string:
				skipString!skipInter();
				break;
			case number:
				skipNumber!skipInter();
				break;
			case object:
				static if (isValidateAll)
				{
					foreach (_; this.byKey)
						break;
				}
				else
				{
					m_text++;
					seekObjectEnd();
					skipOnePlusWhitespace!skipInter();
				}
				break;
			case array:
				static if (isValidateAll)
				{
					foreach (_; this)
						break;
				}
				else
				{
					m_text++;
					seekArrayEnd();
					skipOnePlusWhitespace!skipInter();
				}
				break;
			case boolean:
				skipBoolean!skipInter();
				break;
			case null_:
				skipNull!skipInter();
				break;
		}
	}


	/*******************************************************************************
	 * 
	 * Returns the type of data that is up next in the JSON text.
	 *
	 **************************************/
	@property DataType peek()
	{
		static immutable trans = {
			DataType[256] result = cast(DataType) ubyte.max;
			result['{'] = DataType.object;
			result['['] = DataType.array;
			result['-'] = DataType.number;
			foreach (i; '0' .. '9'+1)
				result[i] = DataType.number;
			result['"'] = DataType.string;
			result['t'] = DataType.boolean;
			result['f'] = DataType.boolean;
			result['n'] = DataType.null_;
			return result;
		}();
		
		DataType vt = trans[*m_text];
		static if (isValidating)
			if (vt == ubyte.max)
				expectNot("while peeking at next value type");
		return vt;
	}


	private void nest(char c, string msg)
	{
		expect(c, msg);
		skipWhitespace!false();
		m_nesting++;
	}


	private void unnest()
	in { assert(m_nesting > 0); }
	body
	{
		if (--m_nesting == 0)
		{
			skipOnePlusWhitespace!false();
			static if (isValidating)
				if (*m_text != '\0')
					handleError("Expected end of JSON.");
		}
		else skipOnePlusWhitespace!skipAllInter();
	}


	private bool iterationGuts(char[2] braces, T, D)(ref int result, T idx, scope D dlg,
		string missingCommaMsg)
	{
		auto oldPos = m_text;
		static if (isValidateAll)
		{
			if (result)
			{
				skipValueImpl!(!isValidateAll)();
				goto PastValue;
			}
		}
		result = dlg(idx);
		if (oldPos is m_text)
			skipValueImpl!(!isValidateAll)();
		
	PastValue:
		if (*m_text == braces[1])
			return true;
		
		static if (!isValidateAll) if (result)
		{
			seekAggregateEnd!braces();
			return true;
		}
		
		static if (!skipAllInter) if (oldPos !is m_text)
		{
			expect(',', missingCommaMsg);
			skipWhitespace!false();
		}
		return false;
	}


	static if (!isValidateAll)
	{
		private void seekObjectEnd()
		{
			seekAggregateEnd!"{}"();
		}


		private void seekArrayEnd()
		{
			seekAggregateEnd!"[]"();
		}


		private void seekAggregateEnd(immutable char[2] parenthesis)()
		{
			size_t nesting = 1;
			while (true)
			{
				m_text.seekToAnyOf!(parenthesis ~ "\"\0");
				final switch (*m_text)
				{
					case parenthesis[0]:
						m_text++;
						nesting++;
						break;
					case parenthesis[1]:
						if (--nesting == 0)
							return;
						m_text++;
						break;
					case '"':
						// Could skip ':' or ',' here by passing `true`, but we skip it above anyways.
						skipString!false();
				}
			}
		}
	}


	/// This also increments the JSON read pointer.
	private void expect(char c, string msg)
	{
		static if (isValidating)
			if (*m_text != c)
				expectImpl(c, msg);
		m_text++;
	}


	private void expectNot(char c, string msg)
	{
		static if (isValidating)
			if (*m_text == c)
				expectNot(msg);
	}


	static if (isValidating)
	{
		@noinline
		private void expectNot(string msg)
		{
			string tmpl = isPrintable(*m_text)
				? "Character '%s' %s."
				: "Byte 0x%02x %s.";
			handleError(format(tmpl, *m_text, msg));
		}


		@noinline
		private void expectImpl(char c, string msg)
		{
			string tmpl = isPrintable(*m_text)
				? "Expected '%s', but found '%s' %s."
				: "Expected '%s', but found byte 0x%02x %s.";
			handleError(format(tmpl, c, *m_text, msg));
		}


		@noinline
		private void handleError(string msg)
		{
			size_t line;
			const(char)* p = m_start;
			const(char)* last;
			do
			{
				last = p;
				p.skipToNextLine();
				line++;
			}
			while (p <= m_text);
			
			size_t pos;
			pos += last[0 .. m_text - last].byGrapheme.walkLength;
			
			throw new JSONException(msg, line.to!int, pos.to!int);
		}
	}


	@forceinline @nogc pure nothrow
	private void skipOnePlusWhitespace(bool skipInter)()
	{
		m_text++;
		skipWhitespace!skipInter();
	}


	@forceinline @nogc pure nothrow
	private void skipWhitespace(bool skipInter)()
	{
		static if (skipInter)
			m_text.skipAllOf!"\t\n\r ,:";
		else
			m_text.skipAsciiWhitespace();
	}


	debug
	{
		private void printState()
		{
			writeln( ">", m_text[0 .. 16], "<" );
			stdout.flush();
		}
	}


	private static struct SingleKey
	{
		alias json this;

		private Json* m_pjson;
		private const(char*) m_oldPos;

		@safe @nogc pure nothrow
		@property ref Json json()
		{
			return *m_pjson;
		}

		this(ref Json json)
		{
			m_pjson = &json;
			m_oldPos = json.m_text;
		}

		~this()
		{
			static if (isValidateAll)
			{
				if (*json.m_text != '}')
				{
					if (m_oldPos !is json.m_text)
					{
						json.expect(',', "after key-value pair");
						json.skipWhitespace!false();
					}
					while (true)
					{
						json.skipString!false();
						json.expect(':', "between key and value");
						json.skipWhitespace!false();
						json.skipValueImpl!false();

						if (*json.m_text == '}')
							break;

						json.expect(',', "after key-value pair");
						json.skipWhitespace!false();
					}
				}
			}
			else
			{
				json.seekObjectEnd();
			}
			json.unnest();
		}
	}


	private static struct File
	{
		alias m_json this;
		
		private size_t m_len;
		Json m_json;
		
		@disable this();
		@disable this(this);
		
		this(const(ubyte)[] fname)
		{
			version (Posix)
			{
				import core.sys.posix.fcntl;
				import core.sys.posix.sys.mman;
				import core.sys.posix.unistd;

				version (linux)
					enum O_CLOEXEC = octal!2000000;
				else version (OSX)  // Requires at least OS X 10.7 Lion
					enum O_CLOEXEC = 0x1000000;
				else static assert(0, "Not implemented");
				
				int fd = { return open(charPtr!fname, O_RDONLY | O_NOCTTY | O_CLOEXEC); }();
				assert(fcntl(fd, F_GETFD) & FD_CLOEXEC, "Could not set O_CLOEXEC.");
				
				if (fd == -1)
					throw new ErrnoException("Could not open JSON file for reading.");
				scope(exit) close(fd);
				
				// Get the file size
				stat_t info;
				if (fstat(fd, &info) == -1)
					throw new ErrnoException("Could not get JSON file size.");

				// Ensure we have 16 extra bytes
				size_t pagesize = sysconf(_SC_PAGESIZE);
				ulong fsize = ulong(info.st_size + pagesize - 1) / pagesize * pagesize;
				bool zeroPage = fsize < info.st_size + 16;
				if (zeroPage)
					fsize += pagesize;
				if (fsize > size_t.max)
					throw new FileException("JSON file too large to be mapped in RAM.");
				m_len = cast(size_t) fsize;
				
				// Map the file
				void* mapping = mmap(null, m_len, PROT_READ, MAP_PRIVATE, fd, 0);
				if (mapping == MAP_FAILED)
					throw new ErrnoException("Could not map JSON file.");
				scope(failure)
					munmap(mapping, m_len);
				
				// Get a zero-page up behind the JSON text
				if (zeroPage)
				{
					void* offs = mapping + m_len - pagesize;
					if (mmap(offs, pagesize, PROT_READ, MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0) == MAP_FAILED)
						throw new ErrnoException("Could not map zero-page behind JSON text.");
				}

				// Initialize the parser on the JSON text
				m_json = (cast(char*) mapping)[0 .. cast(size_t) info.st_size];
			}
			else static assert(0, "Not implemented");
		}
		
		
		this(const(char)[] fname)
		{
			this(fname.representation);
		}
		
		
		nothrow
		~this()
		{
			version (Posix)
			{
				import core.sys.posix.sys.mman;
				munmap(cast(void*) m_json.m_start, m_len);
			}
		}
	}
}


private template buildRemapTable(T)
{
	import std.typetuple;

	static if (is(T == enum))
	{
		struct Remap { T d; string json; }
		enum members = EnumMembers!T;
	}
	else
	{
		struct Remap { string d; string json; }
		enum members = FieldNameTuple!T;
	}
	enum mapping = getUDA!(T, JsonMapping).map;

	template Impl(size_t a, size_t b)
	{
		static if (b - a > 1)
		{
			alias Impl = TypeTuple!(Impl!(a, (b + a) / 2), Impl!((b + a) / 2, b));
		}
		else static if (b - a == 1)
		{
			static if (is(T == enum))
				enum key = members[a].to!string;
			else
				alias key = members[a];
			static if ((key in mapping) !is null)
				enum mapped = mapping[key];
			else
				alias mapped = key;
			alias Impl = TypeTuple!(Remap(members[a], mapped));
		}
		else alias Impl = TypeTuple!();
	}

	alias buildRemapTable = Impl!(0, members.length);
}


unittest
{
	struct Counter
	{
		size_t array, object, key, string, number, boolean, null_;
	}

	void valueHandler(ref Json!validateAll.File json, ref Counter ctr)
	{
		with (DataType) final switch (json.peek)
		{
			case array:
				ctr.array++;
				foreach (_; json)
					valueHandler(json, ctr);
				break;
			case object:
				ctr.object++;
				foreach(key; json.byKey)
				{
					ctr.key++;
					valueHandler(json, ctr);
				}
				break;
			case string:
				ctr.string++;
				json.skipValue();
				break;
			case number:
				ctr.number++;
				json.skipValue();
				break;
			case boolean:
				ctr.boolean++;
				json.skipValue();
				break;
			case null_:
				ctr.null_++;
				json.skipValue();
				break;
		}
	}

	void passFile(string fname, Counter valid)
	{
		auto json = Json!validateAll.File(fname);
		Counter ctr;
		valueHandler(json, ctr);
		assert(ctr == valid, fname);
	}

	void failFile(string fname)
	{
		auto json = Json!validateAll.File(fname);
		Counter ctr;
		assertThrown!JSONException(valueHandler(json, ctr), fname);
	}

	// Tests that need to pass according to RFC 7159
	passFile("test/pass1.json",  Counter( 6,  4, 33, 21, 32,  4,  2));
	passFile("test/pass2.json",  Counter(19,  0,  0,  1,  0,  0,  0));
	passFile("test/pass3.json",  Counter( 0,  2,  3,  2,  0,  0,  0));
	passFile("test/fail1.json",  Counter( 0,  0,  0,  1,  0,  0,  0));
	passFile("test/fail18.json", Counter(20,  0,  0,  1,  0,  0,  0));

	// Tests that need to fail
	foreach (i; chain(iota(2, 18), iota(19, 34)))
		failFile("test/fail" ~ i.to!string ~ ".json");

	// Deserialization
	struct Test
	{
		string text1;
		string text2;
		string text3;
		double dbl = 0;
		float flt = 0;
		ulong ul;
		uint ui;
		ushort us;
		ubyte ub;
		long lm, lp;
		int im, ip;
		short sm, sp;
		byte bm, bp;
		bool t, f;
		Test* tp1, tp2;
		int[2] sa;
		int[] da;
		Test[string] aa;
		SearchPolicy e;
	}

	Test t1 = {
		text1 : "abcde",
		text2 : "",
		text3 : null,
		dbl   : 1.1,
		flt   : -1.1,
		ul    : ulong.max,
		ui    : uint.max,
		us    : ushort.max,
		ub    : ubyte.max,
		lm    : long.min,
		lp    : long.max,
		im    : int.min,
		ip    : int.max,
		sm    : short.min,
		sp    : short.max,
		bm    : byte.min,
		bp    : byte.max,
		t     : true,
		f     : false,
		tp1   : null,
		tp2   : new Test("This is", "a", "test."),
		sa    : [ 33, 44 ],
		da    : [ 5, 6, 7 ],
		aa    : [ "hash" : Test("x", "y", "z") ],
		e     : SearchPolicy.linear
	};
	Test t2 = parseJSON(`{
		"text1" : "abcde",
		"text2" : "",
		"text3" : null,
		"dbl"   : 1.1,
		"flt"   : -1.1,
		"ul"    : ` ~ ulong.max.to!string ~ `,
		"ui"    : ` ~ uint.max.to!string ~ `,
		"us"    : ` ~ ushort.max.to!string ~ `,
		"ub"    : ` ~ ubyte.max.to!string ~ `,
		"lm"    : ` ~ long.min.to!string ~ `,
		"lp"    : ` ~ long.max.to!string ~ `,
		"im"    : ` ~ int.min.to!string ~ `,
		"ip"    : ` ~ int.max.to!string ~ `,
		"sm"    : ` ~ short.min.to!string ~ `,
		"sp"    : ` ~ short.max.to!string ~ `,
		"bm"    : ` ~ byte.min.to!string ~ `,
		"bp"    : ` ~ byte.max.to!string ~ `,
		"t"     : true,
		"f"     : false,
		"tp1"   : null,
		"tp2"   : { "text1": "This is", "text2": "a", "text3": "test." },
		"sa"    : [ 33, 44 ],
		"da"    : [ 5, 6, 7 ],
		"aa"    : { "hash" : { "text1":"x", "text2":"y", "text3":"z" } },
		"e"     : "linear"
	}`).read!Test;

	assert(t2.tp2 && *t1.tp2 == *t2.tp2);
	assert(t1.da == t2.da);
	assert(t1.aa == t2.aa);
	t2.tp2 = t1.tp2;
	t2.da = t1.da;
	t2.aa = t1.aa;
	assert(t1 == t2);
}

// Test case for Issue #4
unittest
{
	auto str = `{"initiator_carrier_code":null,"a":"b"}`;
	auto js = parseTrustedJSON(str);
	foreach(key; js.byKey)
	{
		if(key == "initiator_carrier_code")
		{
			auto t = js.read!string;
			assert(t is null);
		}
	}
}

// Test case for Issue #5
unittest
{
	import std.utf;
	auto str = `{"a":"SΛNNO𐍈€한"}`;
	str.validate;
	validateJSON(str);
}
