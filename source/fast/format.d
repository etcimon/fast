/*******************************************************************************
 * 
 * Functions for formatting data into strings and back.
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
module fast.format;

import std.traits;


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
