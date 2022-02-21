/*******************************************************************************
 * 
 * Functions for formatting data into strings and back.
 * 
 * Authors:
 *   $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 * 
 * Copyright:
 *   © 2017 $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 * 
 * License:
 *   $(LINK2 http://www.gnu.org/licenses/gpl-3.0, GNU General Public License 3.0)
 * 
 **************************************/
module fast.format;

//import core.stdc.stdlib;
//import core.stdc.string;
import core.bitop;
import std.traits;
import std.typecons;
import std.typetuple;
import fast.internal.helpers;


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Hex String
 ╚══════════════════════════════════════════════════════════════════════════════
 +/


/**
 * Converts an unsigned type into a fixed width 8 digits hex string using lower-case letters.
 * 
 * Params:
 *   n = the number to convert
 * 
 * Returns:
 *   hexadecimal representation of $(D n), lower-case letters
 */
@safe pure nothrow @nogc
char[2 * U.sizeof] hexStrLower(U)(Unqual!U n) if (isUnsigned!U)
{
	char[2 * U.sizeof] hex = void;
	foreach_reverse (i; 0 .. 2 * U.sizeof)
	{
		U d = n & U(0xF);
		hex[i] = cast(char) (d < 10 ? '0' + d : 'a' + d - 10);
		n >>= 4;
	}
	return hex;
}


/**
 * Converts an unsigned type into a fixed width 8 digits hex string using upper-case letters.
 * 
 * Params:
 *   n = the number to convert
 * 
 * Returns:
 *   hexadecimal representation of $(D n), upper-case letters
 */
@safe pure nothrow @nogc
char[2 * U.sizeof] hexStrUpper(U)(U n) if (isUnsigned!U)
{
	char[2 * U.sizeof] hex = void;
	foreach_reverse (i; 0 .. 2 * U.sizeof)
	{
		U d = n & U(0xF);
		hex[i] = cast(char) (d < 10 ? '0' + d : 'A' + d - 10);
		n >>= 4;
	}
	return hex;
}


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Decimal String
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

template decDigits(T) if (isIntegral!T)
{
	static if (is(T == ulong))
		enum decDigits = 20;
	else static if (is(T == long))
		enum decDigits = 19;
	else static if (is(T == uint) || is(T == int))
		enum decDigits = 10;
	else static if (is(T == ushort) || is(T == short))
		enum decDigits = 5;
	else static if (is(T == ubyte) || is(T == byte))
		enum decDigits = 3;
}

template decDigits(T) if (isFloatingPoint!T)
{
	static if (is(T == float))
		enum decDigits = 9;
	else static if (is(T == double))
		enum decDigits = 18;
}

enum decChars(T) = decDigits!T + isSigned!T;

@safe pure nothrow short decCharsVal(T)(T v) if (isIntegral!T) {
	int maxsize = 10;
	short digits = 1;
	if (v < 0) {
		digits = 2;
		v *= -1;
	}

	// calculate left of the decimal
	while (digits < decChars!T) {
		if (v < maxsize) {
			return digits;
		}
		maxsize *= 10;
		digits++;
	}
	return decChars!T;
}

@safe nothrow short decCharsVal(T)(T v) if (isFloatingPoint!T) {
	int maxsize = 10;
	uint u = cast(uint) (v < 0 ? -v : v);
	T dec = (v < 0 ? (-v) : (v)) - u;
	short digits = 1;
	if (dec != 0)
		digits++;
	if (v < 0)
		digits++;
	

	// calculate left of the decimal
	while (digits < decChars!uint) {
		if (u < maxsize) {
			break;
		}
		maxsize *= 10;
		digits++;
	}

	// calc decimals
	while (digits < decChars!T && dec > 0) {
		dec *= 10;
		uint val = cast(uint) dec;
		dec -= val;
		if (dec == 0.) {
			return digits;
		}
		digits++;
	}

	return digits;
}

@safe pure nothrow @nogc
RevFillStr!(decChars!I) decStr(I)(I i) if (isFloatingPoint!I)
{
	RevFillStr!(decChars!I) str;

	bool signed = i < 0;
	uint u = cast(uint) (i < 0 ? -i : i);

	I dec = (i < 0 ? (-i) : (i)) - u;
	
	short digits = signed ? 2 : 1;	
	int maxsize = 10;
	// calculate left of the decimal
	while (digits < decChars!uint) {
		if (u < maxsize) {
			break;
		}
		maxsize *= 10;
		digits++;
	}

	char[decDigits!I - 3] decimals = void;
	foreach (ref d; decimals) d = 0;
	if (dec != 0) {
		int j;
		do {
			dec *= 10;
			uint val = cast(uint) dec;
			decimals[j++] = char('0' + val % 10);
			dec -= val;
		} while (dec > 0 && j < decimals.length - digits);
		bool found_num;
		foreach_reverse(d; decimals) {
			if (d > 0 && d > '0' && d <= '9' && !found_num) 
				found_num = true;
			
			if (found_num) 			
				str ~= d;
			
		}
		str ~= '.';
	}

	do
	{
		str ~= char('0' + u % 10);
		u /= 10;
	}
	while (u > 0);

	static if (isSigned!I) if (signed)
		str ~= '-';

	return str;
}

@safe pure nothrow @nogc
RevFillStr!(decChars!I) decStr(I)(I i) if (isIntegral!I)
{
	RevFillStr!(decChars!I) str;

	static if (isSigned!I)
	{
		bool signed = i < 0;
		UnsignedOf!I u = i < 0 ? -i : i;
	}
	else alias u = i;

	do
	{
		str ~= char('0' + u % 10);
		u /= 10;
	}
	while (u);

	static if (isSigned!I) if (signed)
		str ~= '-';

	return str;
}

/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Formatting
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

template hasKnownSpaceRequirement(T)
{
	static if (isIntegral!T || isPointer!T)
		enum hasKnownSpaceRequirement = true;
	else
		enum hasKnownSpaceRequirement = false;
}


template spaceRequirement(string format, T) if (hasKnownSpaceRequirement!T)
{
	static if (isIntegral!T)
	{
		static if (format == "%s" || format == "%d")
			enum spaceRequirement = decChars!T;
		else static if (isUnsigned!T && (format == "%x" || format == "%X"))
			enum spaceRequirement = 2 * T.sizeof;
		else static assert (0, "Don't know how to handle " ~ T.stringof ~ " as " ~ format);
	}
	else static if (isPointer!T)
	{
		static if (format == "%s" || format == "%p")
			enum spaceRequirement = 2 * T.sizeof;
		else static assert (0, "Don't know how to handle " ~ T.stringof ~ " as " ~ format);
	}
	else static assert (0, "Don't know how to handle " ~ T.stringof);
}


enum spaceRequirements(string format, Args...)() if (allSatisfy!(hasKnownSpaceRequirement, Args))
{
	size_t sum = 0;
	
	alias parts = tokenizedFormatString!format;
	foreach (i; staticIota!(0, parts.length))
	{
		static if (parts[i][1] == size_t.max)
			sum += parts[i][0].length;
		else
			sum += spaceRequirement!(parts[i][0], Args[parts[i][1]]);
	}
	
	return sum;
}


ptrdiff_t indexOf(T)(T s, char c) pure nothrow {
	immutable c1 = c;

	ptrdiff_t i;
	foreach (const c2; s)
	{
		if (c1 == c2)
			return i;
		++i;
	}
	return -1;
}
template tokenizedFormatString(string fmt)
{
	enum impl()
	{
		Tuple!(string, size_t)[8] parts;
		size_t i = 0;
		size_t j = 0;
		string rest = fmt;
		while (1)
		{
			ptrdiff_t markerPos = indexOf(rest,'%');
			if (markerPos < 0) {
				if (rest.length) parts[j++] = tuple(rest, size_t.max);
				return parts;
			}
			if (markerPos)
			{
				parts[j++] = tuple(rest[0 .. markerPos], size_t.max);
				rest = rest[markerPos .. $];
			}

			// TODO: more complex formats
			parts[j++] = tuple(rest[0 .. 2], i++);
			rest = rest[2 .. $];
		}
		
	}

	enum result = impl();
	static immutable Tuple!(string, size_t)[result.length] tokenizedFormatString = result;
}


enum formatStringArgCount(string format)()
{
	size_t count = 0;

	alias parts = tokenizedFormatString!format;
	foreach (i; staticIota!(0, parts.length))
		if (parts[i][0] != null && parts[i][1] != size_t.max && parts[i][1] >= count)
			count = parts[i][1] + 1;

	return count;
}


template format(string fmt)
{
	import std.exception;

	enum argCnt = formatStringArgCount!fmt;

	enum codeGen()
	{
		string code = `pure nothrow string format(`;
		foreach (i; staticIota!(0, argCnt))
		{
			if (i) code ~= `, `;
			code ~= std.string.format("A%s", i);
		}
		code ~= `)(`;
		foreach (i; staticIota!(0, argCnt))
		{
			if (i) code ~= `, `;
			code ~= std.string.format("A%s a%s", i, i);
		}
		code ~= `, char[] buffer = new char[](spaceRequirements!(fmt`;
		foreach (i; staticIota!(0, argCnt))
		code ~= std.string.format(", A%s", i);
		code ~= `))) { return assumeUnique(formattedWrite!fmt(buffer.ptr`;
		foreach (i; staticIota!(0, argCnt))
		code ~= std.string.format(", a%s", i);
		code ~= `)); }`;
		return code;
	}

	mixin(codeGen());
}


template formata(string fmt)
{
	enum argCnt = formatStringArgCount!fmt;

	enum codeGen()
	{
		string code = `pure nothrow @nogc char[] formata(`;
		foreach (i; staticIota!(0, argCnt))
		{
			if (i) code ~= `, `;
			code ~= std.string.format("A%s", i);
		}
		code ~= `)(`;
		foreach (i; staticIota!(0, argCnt))
		{
			if (i) code ~= `, `;
			code ~= std.string.format("A%s a%s", i, i);
		}
		code ~= `, void* buffer = alloca(spaceRequirements!(fmt`;
		foreach (i; staticIota!(0, argCnt))
			code ~= std.string.format(", A%s", i);
		code ~= `))) { return formattedWrite!fmt(cast(char*) buffer`;
		foreach (i; staticIota!(0, argCnt))
			code ~= std.string.format(", a%s", i);
		code ~= `); }`;
		return code;
	}

	mixin(codeGen());
}


template formats(string fmt)
{
	enum argCnt = formatStringArgCount!fmt;
	
	enum codeGen()
	{
		string code = `@safe pure nothrow @nogc auto formats(`;
		foreach (i; staticIota!(0, argCnt))
		{
			if (i) code ~= `, `;
			code ~= std.string.format("A%s", i);
		}
		code ~= `)(`;
		foreach (i; staticIota!(0, argCnt))
		{
			if (i) code ~= `, `;
			code ~= std.string.format("A%s a%s", i, i);
		}
		code ~= `))) { LimitedScopeBuffer!(char, spaceRequirements!(fmt`;
		foreach (i; staticIota!(0, argCnt))
			code ~= std.string.format(", A%s", i);
		code ~= `)) buffer; buffer.length = formattedWrite!fmt(buffer.ptr`;
		foreach (i; staticIota!(0, argCnt))
			code ~= std.string.format(", a%s", i);
		code ~= `).length; return buffer; }`;
		return code;
	}
	
	mixin(codeGen());
}
char[] formattedWrite(string format, Args...)(char* buffer, Args args)
{
	import ldc.intrinsics;
	char* it = buffer;

	alias parts = tokenizedFormatString!format;
	foreach (i; staticIota!(0, parts.length))
	{
		static if (parts[i][0] != null && parts[i][1] == size_t.max)
		{
			// Direct string copy
			llvm_memcpy( it, parts[i][0].ptr, parts[i][0].length );
			it += parts[i][0].length;
		}
		else static if (parts[i][0] != null)
		{
			// Formatted argument
			formattedWriteItem!(parts[i][0])( it, args[parts[i][1]] );
		}
	}

	return buffer[0 .. it - buffer];
}


pure nothrow
void formattedWriteItem(string format, T)(ref char* buffer, T t)
	if (isUnsigned!T && format == "%x")
{
	alias RT = ReturnType!(hexStrLower!T);
	*cast(RT*) buffer = hexStrLower!T(t);
	buffer += RT.length;
}


pure nothrow
void formattedWriteItem(string format, T)(ref char* buffer, T t)
	if (isUnsigned!T && format == "%X")
{
	alias RT = ReturnType!(hexStrUpper!T);
	*cast(RT*) buffer = hexStrUpper!T(t);
	buffer += RT.length;
}
import std.format;

nothrow 
void formattedWriteItem(string format, T)(ref char* buffer, T t)
	if (format == "%s" || format == "%d" || format == "%f")
{
	import ldc.intrinsics;
	static if (isIntegral!T || isFloatingPoint!T) auto str = decStr(t);
	else auto str = t;
	
	static if (is(typeof(str) : char)){
		llvm_memcpy( buffer, &str, char.sizeof );
		buffer += char.sizeof;
	}
	else {
		ptrdiff_t quote_idx = str.indexOf('"');
		auto str_ptr = str.ptr;
		size_t remaining = str.length;
		while (quote_idx > -1) {
			llvm_memcpy(buffer, str_ptr, quote_idx);
			buffer += quote_idx;
			*buffer = '\\';
			buffer++;
			str_ptr += quote_idx;
			remaining -= quote_idx;
			quote_idx = indexOf(buffer[0 .. str.length - quote_idx],'"');
		}
		llvm_memcpy( buffer, str_ptr, remaining );
		buffer += remaining;
	}
	
}


pure nothrow
void formattedWriteItem(string format)(ref char* buffer, void* p)
	if (format == "%s" || format == "%p")
{
	buffer.formattedWriteItem!"%X"( cast(size_t) p );
}


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Helper Structs
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

struct RevFillStr(size_t n)
{
private:

	size_t offset = n;
	char[n] buffer = '\0';


public:

	alias opSlice this;

	@safe pure nothrow @nogc
	void opOpAssign(string op : "~")(char ch)
	in
	{
		assert( offset > 0 );
	}
	body
	{
		buffer[--offset] = ch;
	}


	@safe pure nothrow @nogc
	@property inout(char)[] opSlice() inout
	{
		return buffer[offset .. n];
	}


	@safe pure nothrow @nogc
	@property inout(char)* ptr() inout
	{
		return &buffer[offset];
	}


	@safe pure nothrow @nogc
	@property size_t length() const
	{
		return n - offset;
	}
}

bool isValidDchar(dchar c) pure nothrow @safe @nogc
{
    return c < 0xD800 || (c > 0xDFFF && c <= 0x10FFFF);
}