/*******************************************************************************
 * 
 * Converts between UTF-8 and UTF-16.
 * 
 * Authors:
 *   $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 * 
 * Copyright:
 *   © 2013 $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 * 
 * License:
 *   $(LINK2 http://www.gnu.org/licenses/gpl-3.0, GNU General Public License 3.0)
 * 
 **************************************/
module fast.cstring; @nogc nothrow:

import core.stdc.stdlib;
import core.stdc.string;
//import std.traits;
import fast.buffer;


/**
 * Converts a string to a wstring using a buffer provided by the user.
 * To get the buffer requirements call $(D wstringSize) on your source buffer.
 *
 * Params:
 *   src = The UTF-8 string to convert.
 *   dst = The destination buffer for the conversion.
 *
 * Returns:
 *   The part of the destination buffer used for the conversion as a $(D wchar[]).
 *   A terminating zero is appended, so the result.ptr can be passed into Windows APIs.
 */
pure
wchar[] string2wstring(in char[] src, wchar* dst)
{
	const char* srcEnd = src.ptr + src.length;
	const(char)* srcIt = src.ptr;
	wchar* dstIt = dst;

	while (srcIt !is srcEnd)
	{
		// how long is the byte sequence
		int len = 0;
		uint mask = 0b1000_0000;
		while(*srcIt & mask)
		{
			mask >>= 1;
			len++;
		}

		// get payload of first byte
		dchar ch = *srcIt++ & (mask - 1);

		while (--len > 0)
		{
			// make space for 6 more bits
			ch <<= 6;
			ch |= *srcIt++ & 0b0011_1111;
		}

		// do we need to store a surrogate pair ?
		static if (is(wchar == dchar))
		{
			*dstIt++ = ch;
		}
		else if (ch > wchar.max)
		{
			*dstIt++ = (ch >> 10) | 0xD800;
			*dstIt++ = (ch & 0b11_1111_1111) | 0xDC00;
		}
		else
		{
			*dstIt++ = cast(wchar) ch;
		}
	}
	*dstIt = 0;

	return dst[0 .. dstIt - dst];
}

/**
 * Calculates the required buffer size in bytes for a string to wchar[] conversion.
 * Room for a terminating '\0' is included.
 *
 * Params:
 *   src = The source string.
 *
 * Returns:
 *   The maximum byte count the source string could require, including the terminating '\0'.
 *
 * See_Also:
 *   string2wstring
 *   
 */
@safe pure
size_t string2wstringSize(in char[] src)
{
	enum limit = size_t.max / wchar.sizeof - 1;
	return src.length <= limit ? wchar.sizeof * (src.length + 1) : size_t.max;
}


/**
 * Converts a wstring to a string using a buffer provided by the user.
 * To get the buffer requirements call $(D stringSize) on your source buffer.
 *
 * Params:
 *   src = The UTF-8 string to convert.
 *   dst = The destination buffer for the conversion.
 *
 * Returns:
 *   The part of the destination buffer used for the conversion as a $(D wchar[]).
 *   A terminating zero is appended, so the result.ptr can be passed into Windows APIs.
 */
pure
char[] wstring2string(in wchar[] src, char* dst)
{
	const wchar* srcEnd = src.ptr + src.length;
	const(wchar)* srcIt = src.ptr;
	char* dstIt = dst;

	while (srcIt !is srcEnd)
	{
		if (*srcIt < 0x80)
		{
			*dstIt++ = cast(char) *srcIt++;
		}
		else if (*srcIt < 0x800)
		{
			*dstIt++ = cast(char) (0b_11000000 | *srcIt >> 6);
			*dstIt++ = 0b_10000000 | 0b_00111111 & *srcIt++;
		}
		if (*srcIt < 0xD800 || *srcIt > 0xDBFF)
		{
			// anything else within the BMP (<= 0xFFFF), but not a high surrogate
			*dstIt++ = 0b_11100000 | *srcIt >> 12;
			*dstIt++ = 0b_10000000 | 0b_00111111 & *srcIt >> 6;
			*dstIt++ = 0b_10000000 | 0b_00111111 & *srcIt++;
		}
		else
		{
			// high surrogate, assume correct encoding and that the next wchar is the low surrogate
			dchar decoded;
			decoded = (*srcIt++ & 0b11_1111_1111) << 10;
			decoded |= (*srcIt++ & 0b11_1111_1111);
			*dstIt++ = 0b_11110000 | decoded >> 18;
			*dstIt++ = 0b_10000000 | 0b_00111111 & decoded >> 12;
			*dstIt++ = 0b_10000000 | 0b_00111111 & decoded >> 6;
			*dstIt++ = 0b_10000000 | 0b_00111111 & decoded;
		}
	}
	*dstIt = 0;
	
	return dst[0 .. dstIt - dst];
}

/**
 * Calculates the required buffer size in bytes for a wstring to char[] conversion.
 * Room for a terminating '\0' is included.
 *
 * Params:
 *   src = The source string.
 *
 * Returns:
 *   The maximum byte count the source string could require, including the terminating '\0'.
 *
 * See_Also:
 *   wstring2string
 *   
 */
@safe pure
size_t wstring2stringSize(in wchar[] src)
{
	enum limit = (size_t.max / char.sizeof - 1) / 3;
	return src.length <= limit ? char.sizeof * (3 * src.length + 1) : size_t.max;
}


/**
 * Replaces $(D std.utf.toUTFz) with a version that uses the stack as long as the required bytes for the output are
 * <= 1k. Longer strings use $(D malloc) to create a buffer for the conversion. It is freed at least at the end of the
 * scope.
 * 
 * Params:
 *   str = The source string to convert.
 *
 * See_Also:
 *   toWstring
 * 
 * Example:
 * ---
 * string text = "Hello, world!";
 * WinApiW(wcharPtr!text);
 * ---
 */
auto wcharPtr(alias str)(void* buffer = string2wstringSize(str) <= allocaLimit ? alloca(string2wstringSize(str)) : null)
{
	// In any case we have to return a proper InstantBuffer, so that free() is called in the dtor at some point.
	return TempBuffer!wchar(
		string2wstring(str, cast(wchar*) (buffer ? buffer : malloc(string2wstringSize(str)))),
		buffer is null);
}

/// ditto
immutable(wchar)* wcharPtr(alias wstr)()
	if (is(typeof(wstr) == wstring) && __traits(compiles, { enum wstring e = wstr; }))
{
	// D string literals (known at compile time) are always \0-terminated.
	return wstr.ptr;
}

/**
 * $(D char*) version of $(D wcharPtr). Basically it appends a \0 to the input.
 * The function uses $(D malloc) for strings of lengths 1024 and above.
 * 
 * Params:
 *   str = The source string to convert to a C UTF-8 string
 * 
 * Note:
 *   Do not use this to call Windows ANSI functions! Always use wide-char
 *   functions on this operating system unless you want to deal with codepages.
 *
 * Example:
 * ---
 * string text = "Hello, world!";
 * linuxApi(charPtr!text);
 * ---
 */
auto charPtr(alias str)(void* buffer = alloca(str.length + 1))
	if (is(typeof(str) : const(char)[]))
{
	char* dst = cast(char*) memcpy(buffer ? buffer : malloc(str.length + 1), str.ptr, str.length);
	dst[str.length] = '\0';
	return TempBuffer!char(dst[0 .. str.length], buffer is null);
}

/// ditto
immutable(char)* charPtr(alias str)()
	if (__traits(compiles, { enum string e = str; }))
{
	// D string literals (known at compile time) are always \0-terminated.
	return str.ptr;
}

/**
 * This overload allocates the required memory from an existing stack buffer.
 *
 * Params:
 *   str = The source string to convert to a C UTF-8 string
 *   sb = The stack buffer to allocate from
 * 
 * Note:
 *   Always assign the result to an auto variable first for RAII to work correctly.
 */
StackBufferEntry!char charPtr(SB)(const(char)[] str, ref SB sb)
	if (is(SB == StackBuffer!bytes, bytes...))
{
	auto buffer = sb.alloc!char(str.length + 1);
	memcpy(buffer.ptr, str.ptr, str.length);
	buffer[str.length] = '\0';
	return buffer;
}

/**
 * Returns the given $(D ptr) up to but not including the \0 as a $(D char[]).
 */
inout(char)[] asString(inout(char*) ptr) @trusted pure
{
	if (ptr is null) return null;
	return ptr[0 .. strlen(ptr)];
}