/***************************************************************************************************
 * 
 * Definitions that abstract from the architecture or operating system.
 * 
 * As far as possible these will alias existing definitons from OS headers to facilitate integration
 * with other code.
 * 
 * Authors:
 *   $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 * 
 * Copyright:
 *   © 2016 $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 * 
 * License:
 *   $(LINK2 http://www.gnu.org/licenses/gpl-3.0, GNU General Public License 3.0)
 * 
 **************************************************************************************************/
module fast.internal.sysdef;


private enum 一ARCHITECTURE一;

version (X86_64) {
	enum isAMD64 = true;
	enum isX86   = false;
} else version (X86) {
	enum isAMD64 = false;
	enum isX86   = true;
}

version (X86_64)
	enum hasSSE2 = true;
else
	enum hasSSE2 = false;


private enum 一OPERATING一SYSTEM一;

version (Posix)
	enum isPosix = true;
else
	enum isPosix = false;

version (Windows)
	enum isWindows = true;
else
	enum isWindows = false;

/*******************************************************************************
 * 
 * Despite Phobos' use of `char[]` UTF-8 strings for file names, their internal
 * representation in the operating system is a sequence of 8- or 16-bit values.
 * On Windows this means that one could get invalid surrogate pairings and on
 * Linux, a file name can have any 8-bit encoding that keeps '/' at the same
 * code point as ASCII. That's why portable file names should only use a subset
 * of ASCII that is interpreted the same in all supported encodings.
 * 
 * MSDN mentions that file paths should be treated as a sequence of `WCHAR`:
 * https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx#maxpath
 *
 **************************************/
static if (isPosix)
	alias FileChar = ubyte;
else version (Windows)
	alias FileChar = ushort;
else static assert(0, "Not implemented");

alias Filename = FileChar[];


private enum 一COMPILER一UNIFICATION一;

version (LDC) {
	enum isLDC = true;
	enum isGDC = false;
	enum isDMD = false;
} else version (GNU) {
	enum isLDC = false;
	enum isGDC = true;
	enum isDMD = false;
} else version (DigitalMars) {
	enum isLDC = false;
	enum isGDC = false;
	enum isDMD = true;
}

version (DigitalMars)
{
	enum noinline;
	enum forceinline;
	enum sse4;
}
else version (GNU)
{
	import gcc.attribute;
	enum noinline    = gcc.attribute.attribute("noinline");
	enum forceinline = gcc.attribute.attribute("forceinline");
	enum sse4_2      = gcc.attribute.attribute("target", "sse4.2");
}
else version (LDC)
{
	import ldc.attributes;
	enum noinline = ldc.attributes.optStrategy("none");
	enum forceinline = ldc.attributes.llvmAttr("always_inline", true);
	enum sse4_2      = ldc.attributes.target("+sse4.2");
}

version (assert)
	enum isRelease = false;
else
	enum isRelease = true;

version (D_PIC)
	enum isPIC = true;
else
	enum isPIC = false;
