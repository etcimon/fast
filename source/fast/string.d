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
 *   ch... = The code unit(s) that initiates the split. It is typically an ASCII symbol.
 *   str = The string to scan.
 *   before = The part before the split is stored here. If no character in $(D ch...) is found, the original string is returned here.
 *   after = The part after the split is stored here. If no character in $(D ch...) is found, $(D null) is returned here.
 *
 * Returns:
 *   $(D true), iff the symbol was found in the string.
 */
bool split(ch...)(scope inout(char[]) str, ref inout(char)[] before, ref inout(char)[] after, char* splitter = null)
{
	immutable pos = min (str.length, findImpl!(false, ch)(str.ptr, str.length));
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
 * It assumes that one of the characters in $(D ch) is actually contained in the string.
 *
 * Returns:
 *   The char that caused the split. (One of $(D ch).)
 */
char split(ch...)(scope inout(char*) ptr, ref inout(char)[] before, ref inout(char)* after)
{
	immutable pos = findImpl!(true, ch)(ptr);
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
 *   str = The string to search for a code unit.
 *   ch = The code unit(s) to find in the string.
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
 * auto pos = str.find!('/', '\\');
 * if (pos < str.length) { }
 * ---
 */
size_t find(ch...)(scope inout(char[]) str) { return findImpl!(false, ch)(str.ptr, str.length); }

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
 * auto pos = buffer.ptr.find!('$(RPAREN)');
 * if (pos < length) { // was an actual find before the sentinel }
 * ---
 */
size_t find(ch...)(scope inout(char*) ptr) { return findImpl!(true, ch)(ptr); }

private size_t findImpl(bool ignoreLength, ch...)(scope inout(char*) ptr, in size_t length = size_t.max)
{
	import core.stdc.string, core.simd, std.simd;

	// catch "strlen" and "memchr" like calls, that are highly optimized compiler built-ins.
	static if (ch.length == 1 && ignoreLength && ch[0] == '\0')
		return strlen(ptr);
	else static if (ch.length == 1 && (!ignoreLength || isDMD)) // DMD is better off using optimized C library code.
		return memchr(ptr, ch[0], length) - ptr;
	else {
		if (length == 0) return 0;
		static if (hasSSE2 && (isLDC || isGDC)) { // SSE2 enhanced code path
			alias Word = ubyte16;
			enum maskMixin = "moveMask(maskEqual(*mp, SIMDFromScalar!(ubyte16, ch[%1$d])))";
			enum sparseness = 1;
		} else { // basic code path without SSE (assumes memory protection has at best word size granularity)
			alias Word = size_t;
			enum maskMixin = "contains!(ch[%1$d])(*mp)";
			enum sparseness = 8;
		}

		size_t off = cast(size_t) ptr % Word.sizeof;
		Word* mp = cast(Word*) (ptr - off);
		Word* e = cast(Word*) alignPtrNext(ptr + length, Word.sizeof);
		if (off) {
			// Throw away bytes from before start of the string
			if (auto mask = (mixin(ctfeJoin!ch(maskMixin, " | "))) >> (off * sparseness))
				return bsf(mask) / sparseness;
			if (++mp is e && !ignoreLength) return size_t.max;
		}

		do {
			if (auto mask = mixin(ctfeJoin!ch(maskMixin, " | ")))
				return bsf(mask) / sparseness + (cast(char*) mp - ptr);
		} while (++mp !is e || ignoreLength);
		return size_t.max;
	}
}



private:

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
T contains(ubyte value, T)(T word) @safe pure nothrow if (isUnsigned!T)
{
	// This value results in 0x01 for each byte of a T value.
	enum lows = T.max / 0xFF;
	static if (value == 0) {
		enum highs = lows * 0x80;
		return (word - lows) & ~word & highs;
	} else {
		enum xor = lows * value;
		return contains!0(word ^ xor);
	}
}
