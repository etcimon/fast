/*******************************************************************************
 * 
 * Helper functions that serve general purposes.
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
module fast.helpers;

import std.traits;


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Meta programming
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

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

version (D_PIC)
	enum isPIC = true;
else
	enum isPIC = false;

static if (__VERSION__ < 2067)
{
	import std.typetuple;

	//Required for FieldNameTuple
	private enum NameOf(alias T) = T.stringof;

	/**
	 * Get as an expression tuple the names of the fields of a struct, class, or
	 * union. This consists of the fields that take up memory space, excluding the
	 * hidden fields like the virtual function table pointer or a context pointer
	 * for nested types. If $(D T) isn't a struct, class, or union returns an
	 * expression tuple with an empty string.
	 */
	template FieldNameTuple(T)
	{
		static if (is(T == struct) || is(T == union))
			alias FieldNameTuple = staticMap!(NameOf, T.tupleof[0 .. $ - isNested!T]);
		else static if (is(T == class))
			alias FieldNameTuple = staticMap!(NameOf, T.tupleof);
		else
			alias FieldNameTuple = TypeTuple!"";
	}
}


// 2.071 fixed visibility rules, so we need to roll our own staticIota.
static if (__VERSION__ < 2071)
{
	import std.typecons : staticIota;
}
else
{
	import std.typecons : AliasSeq;

	template staticIota(int beg, int end)
	{
		static if (beg + 1 >= end)
		{
			static if (beg >= end)
			{
				alias staticIota = AliasSeq!();
			}
			else
			{
				alias staticIota = AliasSeq!(+beg);
			}
		}
		else
		{
			enum mid = beg + (end - beg) / 2;
			alias staticIota = AliasSeq!(staticIota!(beg, mid), staticIota!(mid, end));
		}
	}
}


/**
 * For any integral type, returns the unsigned type of the same bit-width.
 */
template UnsignedOf(I) if (isIntegral!I)
{
	static if (isUnsigned!I)
		alias UnsignedOf = I;
	else static if (is(I == long))
		alias UnsignedOf = ulong;
	else static if (is(I == int))
		alias UnsignedOf = uint;
	else static if (is(I == short))
		alias UnsignedOf = ushort;
	else static if (is(I == byte))
		alias UnsignedOf = ubyte;
	else static assert (0, "Not implemented");
}


/// Helper mixins to force enable/diasble inlining via pragma on recent compilers.
enum inlineTrue = "static if (__VERSION__ > 2_067) pragma(inline, true);";
/// ditto
enum inlineFalse = "static if (__VERSION__ > 2_067) pragma(inline, false);";


version (GNU)
{
	import gcc.attribute;
	enum noinline    = gcc.attribute.attribute("noinline");
	// GDC stops compilation when instructed to inline a function from an imported module.
	version (GdcLibrary)
		enum forceinline;
	else
		enum forceinline = gcc.attribute.attribute("forceinline");
	enum sse4        = gcc.attribute.attribute("target", "sse4");
}
else
{
	enum noinline;
	enum forceinline;
	enum sse4;
}


version (X86_64)
	enum hasSSE2 = true;
else
	enum hasSSE2 = false;


version (assert)
	enum isRelease = false;
else
	enum isRelease = true;


/**
 * Generates a mixin string for repeating code. It can be used to unroll variadic arguments.
 * A format string is instantiated a certain number times with an incrementing parameter.
 * The results are then concatenated using an optional joiner.
 *
 * Params:
 *   length = Number of elements you want to join. It is passed into format() as an incrementing number from [0 .. count$(RPAREN).
 *   fmt = The format string to apply on each instanciation. Use %1d$ to refer to the current index multiple times when necessary.
 *   joiner = Optional string that will be placed between instances. It could be a space or an arithmetic operation.
 *
 * Returns:
 *   The combined elements as a mixin string.
 *
 * See_Also:
 *   $(LINK2 http://forum.dlang.org/thread/vqfvihyezbmwcjkmpzin@forum.dlang.org, A simple way to do compile time loop unrolling)
 */
enum ctfeJoin(size_t length)(in string fmt, in string joiner = null)
{
	import std.range : iota;
	import std.string : format;
	import std.algorithm : map;
	
	// BUG: Cannot use, join(), as it "cannot access the nested function 'ctfeJoin'".
	string result;
	foreach (inst; map!(i => format(fmt, i))(iota(length))) {
		if (result && joiner) result ~= joiner;
		result ~= inst;
	}
	return result;
}


enum getUDA(alias sym, T)()
{
	foreach (uda; __traits(getAttributes, sym))
		static if (is(typeof(uda) == T))
			return uda;
	return T.init;
}


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Bit operations
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

/*******************************************************************************
 * 
 * Count leading zeroes.
 *
 * Params:
 *   u = the unsigned value to scan
 *
 * Returns:
 *   The number of leading zero bits before the first one bit. If `u` is `0`,
 *   the result is undefined.
 *
 **************************************/
version (DigitalMars)
{
	@safe @nogc pure nothrow
	ubyte clz(U)(U u) if (is(U == uint) || is(U == ulong))
	{
		mixin(inlineTrue);
		import core.bitop;
		enum max = 8 * U.sizeof - 1;
		static if (isX86 && is(U == ulong))
		{
			uint a = u >> 32;
			return cast(ubyte) (max - (a ? 32 + bsr(a) : bsr(cast(uint) u)));
		}
		else return cast(ubyte) (max - bsr(u));
	}
}
else version (GNU)
{
	import gcc.builtins;
	alias clz = __builtin_clz;
	version (X86_64)
	{
		@safe @nogc pure nothrow
		ubyte clz(ulong u)
		{
			return cast(ubyte) __builtin_clzl(u);
		}
	}
	else
	{
		@safe @nogc pure nothrow
		ubyte clz(ulong u)
		{
			uint a = u >> 32;
			return cast(ubyte) (a ? __builtin_clzl(a) : 32 + __builtin_clzl(cast(uint) u));
		}
	}
}
else version (LDC)
{
	@safe @nogc pure nothrow
	ubyte clz(U)(U u) if (is(U == uint) || is(U == ulong))
	{
		import ldc.intrinsics;
		return cast(ubyte) llvm_ctlz(u, false);
	}
}


version (X86)
{
	@safe @nogc pure nothrow
	int bsr(ulong u)
	{
		import core.bitop;
		uint a = u >> 32;
		return a ? 32 + core.bitop.bsr(a) : core.bitop.bsr(cast(uint) u);
	}


	@safe @nogc pure nothrow
	int bsf(ulong u)
	{
		import core.bitop;
		uint a = cast(uint) u;
		return a ? core.bitop.bsf(a) : 32 + core.bitop.bsf(u >> 32);
	}
}


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ 
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

pure nothrow @nogc
{
	/**
	 * Aligns a pointer to the closest multiple of $(D pot) (a power of two),
	 * which is equal to or larger than $(D value).
	 */
	T* alignPtrNext(T)(scope T* ptr, in size_t pot)
	in { assert(pot > 0 && pot.isPowerOf2); }
	body { return cast(T*) ((cast(size_t) ptr + (pot - 1)) & -pot); }
	unittest { assert(alignPtrNext(cast(void*) 65, 64) == cast(void*) 128); }
}


@nogc @safe pure nothrow
{
	/// Returns whether the (positive) argument is an integral power of two.
	@property bool isPowerOf2(in size_t n)
	in { assert(n > 0); }
	body { return (n & n - 1) == 0; }

	version (LDC) {
		import core.simd;
		pragma(LDC_intrinsic, "llvm.x86.sse2.pmovmskb.128")
			uint moveMask(ubyte16);
	} else version (GNU) {
		import gcc.builtins;
		alias moveMask = __builtin_ia32_pmovmskb128;
	}
	
	template SIMDFromScalar(V, alias scalar)
	{
		// This wrapper is needed for optimal performance with LDC and
		// doesn't hurt GDC's inlining.
		V SIMDFromScalar() {
			enum V asVectorEnum = scalar;
			return asVectorEnum;
		}
	}


	template SIMDFromString(string str) if (str.length <= 16)
	{
		import core.simd, std.algorithm, std.range, std.string;

		private enum data = chain(str.representation, 0.repeat(16 - str.length)).array;

		static if (!isDMD)
			immutable ubyte16 SIMDFromString = data;
		else version (D_PIC)
		{
			import std.format;
			void SIMDFromString() @safe @nogc pure nothrow
			{
				mixin(format("asm @trusted @nogc pure nothrow { naked; db %(%s,%); }", data));
			}
		}
		else static if (isX86)
			align(16) __gshared ubyte[16] SIMDFromString = data;
		else
			__gshared ubyte16 SIMDFromString = data;
	}
}
