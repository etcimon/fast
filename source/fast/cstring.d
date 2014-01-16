/**
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
 */
module fast.uniconv;

import core.stdc.stdlib;


/**
 * Converts a string to a wstring using a buffer provided by the user.
 * To get the buffer requirements call $(D toWStringSize) on your source buffer.
 *
 * Params:
 *   src = The UTF-8 string to convert.
 *   dst = The destination buffer for the conversion.
 *
 * Returns:
 *   The part of the destination buffer used for the conversion as a $(D wchar[]).
 *   A terminating zero is appended, so the result.ptr can be passed into Windows APIs.
 */
wchar[] toWstring(scope inout(char[]) src, wchar* dst) pure nothrow
{
	inout char* srcEnd = src.ptr + src.length;
	inout(char)* srcIt = src.ptr;
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
		if (ch > wchar.max)
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
 * Calculates the required length for a string to wstring conversion.
 * Room for a terminating '\0' is included.
 *
 * Params:
 *   src = The source string.
 *
 * Returns:
 *   The maximal length the source string would require as a wstring plus '\0'.
 *
 * See_Also:
 *   toWstring
 *   
 */
size_t wstringSize(scope inout(char[]) src) @safe pure nothrow
{
	return 2 * (src.length + 1);
}

/**
 * Replaces $(D std.utf.toUTFz) with a version that uses the stack as long as
 * the input length is <= 511 chars. Longer strings use $(D malloc) to create
 * a buffer for the conversion. It is freed at least at the end of the scope.
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
auto wcharPtr(alias str)(void* buffer = wstringSize(str) <= allocaLimit ? alloca(wstringSize(str)) : null)
{
	// In any case we have to return a proper InstantBuffer, so that free() is called in the dtor at some point.
	return InstantBuffer!wchar(
		toWstring(str, cast(wchar*) (buffer ? buffer : malloc(wstringSize(str)))).ptr,
		!buffer);
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
auto charPtr(alias str)(void* buffer = str.length < allocaLimit ? alloca(str.length + 1) : null)
{
	import core.stdc.string;
	char* dst = cast(char*) memcpy(buffer ? buffer : malloc(str.length + 1), str.ptr, str.length);
	dst[str.length] = '\0';
	return InstantBuffer!char(dst, !buffer);
}

/// ditto
immutable(char)* charPtr(alias str)() if (__traits(compiles, { enum e = str; }))
{
	// D string literals (known at compile time) are always \0-terminated.
	return str.ptr;
}



private:

private enum allocaLimit = 1024;

struct InstantBuffer(T)
{
	T* ptr;
	bool callFree;
	
	alias ptr this;
	
	@disable this(this);
	~this() { if (callFree) free(ptr); }
}
