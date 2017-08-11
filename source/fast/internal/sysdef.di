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


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Operating System
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

/*******************************************************************************
 * 
 * Despite Phobos' use of `char[]` UTF-8 strings for file names, their internal
 * representation in the operating system is a sequence of `ubyte`s or
 * `ushort`s. In particular they are not a subset of any Unicode encoding and
 * their visual representation may require transcoding unless they are portable,
 * which basically means a subset of ASCII excluding any characters treated
 * specially or regarded invalid by some operating systems.
 *
 **************************************/
version (Posix)
{
	alias FileChar = ubyte;
}
else version (Windows)
{
	import core.sys.windows.windows : WCHAR;
	alias FileChar = WCHAR;
}
else static assert(0, "Not implemented");

alias Filename = FileChar[];


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Compiler Unification
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

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
	enum noinline;
	enum forceinline;
	enum sse4_2      = ldc.attributes.target("+sse4.2");
}


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Architecture
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

version (X86_64)
	enum hasSSE2 = true;
else
	enum hasSSE2 = false;


version (assert)
	enum isRelease = false;
else
	enum isRelease = true;


version (Posix)
	enum isPosix = true;
else
	enum isPosix = false;

version (X86_64) {
	enum isAMD64 = true;
	enum isX86   = false;
} else version (X86) {
	enum isAMD64 = false;
	enum isX86   = true;
}


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
