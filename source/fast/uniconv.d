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
wchar[] toWString(scope inout(char[]) src, wchar* dst) pure nothrow
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
 *   toWString
 *   
 */
size_t toWStringSize(scope inout(char[]) src) @safe pure nothrow
{
	return 2 * (src.length + 1);
}

/**
 * A mixin template that converts a string to a wstring on the stack.
 * It is the user's responsibility to check that $(D toWStringSize) on the
 * source string is within sane limits for a stack allocation.
 *
 * Params:
 *   pointerName = A variable name for the resulting wchar[].
 *   str = The source string to convert.
 *
 * See_Also:
 *   toWString
 * 
 * Example:
 * ---
 * string text = "Hello, world!";
 * mixin stackToWString!("wstr", text);
 * SomeFunc(wstr.ptr);
 * ---
 */
mixin template stackToWString(string pointerName, alias str)
{
	import core.stdc.stdlib : alloca;
	mixin("wchar[] " ~ pointerName ~ " = toWString(str, cast(wchar*) alloca(toWStringSize(str)));");
}