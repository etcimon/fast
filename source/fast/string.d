/**
 * Fast, non-allocating string functions.
 *
 * Authors:
 *   $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 *
 * Copyright:
 *   © 2013 $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 *
 * License:
 *   $(LINK2 http://www.gnu.org/licenses/gpl-3.0, GNU General Public License 3.0)
 */
module fast.string;

import core.bitop;
import std.algorithm;
import std.stdio;
import std.string;
import std.range;
import std.traits;

import fast.internal;


/**
 * Splits a string in two around one or more compile-time known code units.
 *
 * Params:
 *   match = An expression that matches all characters around which a split should occur.
 *   str = The string to scan.
 *   before = The part before the split is stored here. If no character in $(D match) is found, the original string is returned here.
 *   after = The part after the split is stored here. If no character in $(D match) is found, $(D null) is returned here.
 *   splitter = If not $(D null), this pointer will receive a copy of the splitting char.
 *
 * Returns:
 *   $(D true), iff a split occured.
 */
bool split(string match)(scope inout(char[]) str, ref inout(char)[] before, ref inout(char)[] after, char* splitter = null)
{
	immutable pos = min(str.length, SimdMatcher!match.find(str.ptr, str.ptr + str.length));
	before = str[0 .. pos];
	if (pos < str.length) {
		after = str[pos+1 .. $];
		if (splitter) *splitter = str[pos];
		return true;
	}
	after = null;
	return false;
}

/**
 * Similar to the overload for strings, this function works a little faster as it lacks boundary checks.
 * It assumes that one of the characters in $(D match) is actually contained in the string.
 *
 * Params:
 *   match = An expression that matches all characters around which a split should occur.
 *   ptr = The string to scan.
 *   before = The part before the split is stored here. If no character in $(D match) is found, the original string is returned here.
 *   after = The pointer to the part after the split is stored here.
 * 
 * Returns:
 *   The char that caused the split. (From $(D match).)
 */
char split(string match)(scope inout(char*) ptr, ref inout(char)[] before, ref inout(char)* after)
{
	immutable pos = SimdMatcher!match.find(str.ptr);
	before = ptr[0 .. pos];
	after = ptr + pos + 1;
	return ptr[pos];
}

/**
 * Finds the first occurance of a set of compile-time known code units in a string.
 * While the algorithm is O(n) in relation to the count of given code units, the overhead
 * when using it on short strings wights more for only 1 or 2 code units.
 *
 * Params:
 *   match = An expression that matches all characters around which a split should occur.
 *   str = The string to search for a code unit.
 *
 * Returns:
 *   If a match is found, the index into the string is returned.
 *   Otherwise an invalid index is returned. Check with if $(D (result &lt; str.length)).
 *
 * See_Also:
 *   split, $(LINK2 http://mischasan.wordpress.com/2011/11/09/the-generic-sse2-loop/, The Generic SSE2 Loop)
 *
 * Example:
 * ---
 * // Check if there is a '/' or '\' in the string
 * auto pos = str.find!(`or(=/,=\$(RPAREN)`$(RPAREN);
 * if (pos < str.length) { }
 * ---
 */
size_t find(string match)(in char[] str)
{
	return SimdMatcher!match.find(str.ptr, str.ptr + str.length);
}

/**
 * Same as the overload for strings, but with only a char*, making it faster as it cannot do a
 * boundary check.
 *
 * Sometimes when looking for a character it is helpful to append it as a sentinel to the char buffer
 * and then use this function instead of the slower one that checks the boundary constantly.
 *
 * Example:
 * ---
 * // Find an '$(RPAREN)' in a buffer of 1024 bytes using an additional sentinel.
 * size_t length = 1024;
 * char[] buffer = new char[](length+1);
 * buffer[length] = '$(RPAREN)';
 * auto pos = buffer.ptr.find!('=$(RPAREN)');
 * if (pos < length) { // was an actual find before the sentinel }
 * ---
 */
size_t find(string match)(in char* ptr)
{
	return SimdMatcher!match.find(ptr);
}



private:

template SimdMatcher(string match)
{
	import std.string, core.simd;
	
	static if (match != strip(match)) {
		// Reinstanciate the template with any whitespace stripped from the match string.
		alias SimdMatcher = SimdMatcher!(strip(match));
	} else {
		/* For SSE in DMD I am blocked by:
		 * https://d.puremagic.com/issues/show_bug.cgi?id=8047
		 * https://d.puremagic.com/issues/show_bug.cgi?id=11585
		 */
		enum isUsingSSE = hasSSE2 && (isLDC || isGDC);
		enum isSingleChar = match.length == 2 && match[0] == '=';
		static if (isSingleChar) enum singleChar = match[1];
		static if (isUsingSSE) {
			// Using MOVMSKB we get one boolean per bit in a 16-bit value.
			alias Word = ubyte16;
			alias Mask = uint;
			enum sparseness = 1;
		} else {
			// The fallback is to work with machine words and tricky bit-twiddling algorithms.
			// As a result we get machine words where matching bytes have the high bit set.
			alias Word = size_t;
			alias Mask = size_t;
			enum sparseness = 8;
		}
		enum matchCode = genMatchCode!isUsingSSE("*wp");
		// Used in generic comparison code
		enum lows = size_t.max / 0xFF;
		enum highs = lows * 0x80;
		
		enum betterUseTables()
		{
			return (isDMD && matchCode.complexity >= 4)
			|| (isGDC && matchCode.complexity >= 18)
			|| (isLDC && matchCode.complexity >= 18);
		}
		
		static if (betterUseTables) {
			immutable matchTable = genMatchTable();
			
			size_t find(scope inout(char*) b, scope inout(char*) e)
			{
				import core.stdc.string;
				
				// catch "strlen" and "memchr" like calls, that are highly optimized compiler built-ins.
				static if (isSingleChar) {
					return memchr(b, singleChar, e - b) - b;
				} else {
					if (b >= e) return 0;
					
					size_t off = cast(size_t) b % ushort.sizeof;
					ushort* wp = cast(ushort*) (b - off);
					ushort* we = cast(ushort*) alignPtrNext(e, ushort.sizeof);
					if (off) {
						// Throw away bytes from before start of the string
						if (auto mask = matchTable[*wp] >> off)
							return bsf(mask);
						if (++wp is we) return size_t.max;
					}
					
					do {
						if (auto mask = matchTable[*wp])
							return bsf(mask) + (cast(char*) wp - b);
					} while (++wp !is we);
					return size_t.max;
				}
			}
			
			size_t find(scope inout(char*) b)
			{
				import core.stdc.string;
				
				// catch "strlen" and "memchr" like calls, that are highly optimized compiler built-ins.
				static if (isSingleChar && singleChar == '\0') {
					return strlen(b);
				} else static if (isSingleChar && isDMD) { // DMD is better off using optimized C library code.
					return memchr(b, singleChar, e - b) - b;
				} else {
					size_t off = cast(size_t) b % ushort.sizeof;
					ushort* wp = cast(ushort*) (b - off);
					if (off) {
						// Throw away bytes from before start of the string
						if (auto mask = matchTable[*wp] >> off)
							return bsf(mask);
					}
					
					do {
						if (auto mask = matchTable[*wp])
							return bsf(mask) + (cast(char*) wp - b);
					} while (true);
				}
			}
		} else {
			size_t find(scope inout(char*) b, scope inout(char*) e)
			{
				import core.stdc.string, core.simd, std.simd;
				version (LDC) {
					import ldc.gccbuiltins_x86;
				} else version (GNU) {
					import gcc.builtins;
				}
				
				// catch "strlen" and "memchr" like calls, that are highly optimized compiler built-ins.
				static if (isSingleChar) {
					return memchr(b, singleChar, e - b) - b;
				} else {
					if (b >= e) return 0;
					
					size_t off = cast(size_t) b % Word.sizeof;
					Word* wp = cast(Word*) (b - off);
					Word* we = cast(Word*) alignPtrNext(e, Word.sizeof);
					if (off) {
						// Throw away bytes from before start of the string
						if (auto mask = (mixin(matchCode.code)) >> (off * sparseness))
							return bsf(mask) / sparseness;
						if (++wp is we) return size_t.max;
					}
					
					do {
						if (auto mask = mixin(matchCode.code))
							return bsf(mask) / sparseness + (cast(char*) wp - b);
					} while (++wp !is we);
					return size_t.max;
				}
			}
			
			size_t find(scope inout(char*) b)
			{
				import core.stdc.string, core.simd, std.simd;
				version (LDC) {
					import ldc.gccbuiltins_x86;
				} else version (GNU) {
					import gcc.builtins;
				}
				
				// catch "strlen" and "memchr" like calls, that are highly optimized compiler built-ins.
				static if (isSingleChar && singleChar == '\0') {
					return strlen(b);
				} else static if (isSingleChar && isDMD) { // DMD is better off using optimized C library code.
					return memchr(b, singleChar, e - b) - b;
				} else {
					size_t off = cast(size_t) b % Word.sizeof;
					Word* wp = cast(Word*) (b - off);
					if (off) {
						// Throw away bytes from before start of the string
						if (auto mask = (mixin(matchCode.code)) >> (off * sparseness))
							return bsf(mask) / sparseness;
					}
					
					do {
						if (auto mask = mixin(matchCode.code))
							return bsf(mask) / sparseness + (cast(char*) wp - b);
					} while (true);
				}
			}
		}
		
		enum genMatchCode(bool sse)(string var)
		{
			import std.ascii, std.exception;
			
			struct Code {
				string code;
				size_t complexity = 1;
			}
			Code result;
			string[] nesting;
			
			with (result) {
				for (size_t i = 0; i < match.length;) {
					string handleChar() {
						char c = match[i+1];
						switch (c) {
							case 0:
								return `'\0'`;
							case '\\':
								return `'\\'`;
							case "'"[0]:
								return `'\''`;
							default:
								return `'` ~ c ~ `'`;
						}
					}
					
					if (match[i] == '=') {
						static if (sse) {
							code ~= "maskEqual(" ~ var ~ ", SIMDFromScalar!(ubyte16, " ~ handleChar() ~ "))";
						} else if (match[i+1] == 0) {
							code ~= "" ~ var ~ " - lows & ~" ~ var;
						} else {
							code ~= "(" ~ var ~ " ^ lows * " ~ handleChar() ~ ") - lows & ~(" ~ var ~ " ^ lows * " ~ handleChar() ~ ")";
						}
						i += 2;
					} else if (match[i] == '<') {
						static if (sse)
							code ~= "maskGreater(SIMDFromScalar!(ubyte16, " ~ handleChar() ~ "), " ~ var ~ ")";
						else
							code ~= "maskLessGeneric!'" ~ handleChar() ~ "'(" ~ var ~ ")";
						i += 2;
					} else if (match[i] == '>') {
						static if (sse)
							code ~= "maskGreater(SIMDFromScalar!(ubyte16, " ~ handleChar() ~ "), " ~ var ~ ")";
						else
							code ~= "maskGreaterGeneric!'" ~ handleChar() ~ "'(" ~ var ~ ")";
						i += 2;
					} else if (match[i .. $].startsWith("or(")) {
						static if (sse) {
							nesting ~= ", ";
							code ~= "or(";
						} else {
							nesting ~= " | ";
						}
						complexity++;
						i += 3;
					} else if (match[i] == ',') {
						enforce(nesting.length, "',' on top level");
						code ~= nesting[$-1];
						i++;
					} else if (match[i] == ')') {
						enforce(nesting.length, "Unbalanced closing parenthesis");
						nesting.length--;
						static if (sse) {
							code ~= ")";
						}
						i++;
					} else if (match[i].isWhite) {
						i++;
					} else {
						throw new Exception(format("Unexpected character at index %s: 0x%02x", i, match[i]));
					}
				}
				static if (sse) {
					code = "__builtin_ia32_pmovmskb128(" ~ code ~ ")";
				} else {
					code = "(" ~ code ~ ") & highs";
				}
			}
			return result;
		}
		
		enum genMatchTable()
		{
			ubyte[1 << 16] table;
			ubyte[256] lut;
			foreach (i; 0 .. 256) {
				lut[i] = (mixin(genMatchCode!false("i").code) >> 7) & 1;
			}
			foreach (i; 0 .. 256) foreach (k; 0 .. 256) {
				table[i * 256 + k] = cast(ubyte) (lut[i] << 1 | lut[k]);
			}
			return table;
		}
	}
}

/**
 * Template for searching a fixed value in a word sized memory block (i.e. 1, 2, 4 or 8 bytes).
 *
 * Params:
 *   value = The value you are looking for.
 *   word = The data word to search for the value.
 *
 * Returns:
 *   non-zero, iff the value is contained in the data word.
 *   Specifically it returns 0x80 for every byte of the word that was a match and 0x00 for others.
 *
 * See_Also:
 *   http://graphics.stanford.edu/~seander/bithacks.html#ValueInWord
 */
T maskEqualGeneric(ubyte value, T)(T word) @safe pure nothrow
	if (isUnsigned!T)
{
	// This value results in 0x01 for each byte of a T value.
	enum lows = T.max / 0xFF;
	static if (value == 0) {
		enum highs = lows * 0x80;
		return (word - lows) & ~word & highs;
	} else {
		enum xor = lows * value;
		return maskEqualGeneric!0(word ^ xor);
	}
}

T maskLessGeneric(ubyte value, T)(T word) @safe pure nothrow
	if (isUnsigned!T && value <= 128)
{
	enum lows = T.max / 0xFF;
	enum highs = lows * 0x80;
	return (word - lows * value) & ~word & highs;
}

T maskGreaterGeneric(ubyte value, T)(T word) @safe pure nothrow
	if (isUnsigned!T && value <= 127)
{
	enum lows = T.max / 0xFF;
	enum highs = lows * 0x80;
	return (word + lows * (127 - value) | word) & highs;
}

T orGeneric(T)(T a, T b) @safe pure nothrow
	if (isUnsigned!T)
{
	return a | b;
}
