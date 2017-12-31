/***************************************************************************************************
 * 
 * Helper functions that serve general purposes.
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
 **************************************************************************************************/
module fast.internal.helpers;

import std.traits;
import fast.internal.sysdef;


private enum 一META一PROGRAMMING一;

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


private enum 一BIT一OPERATIONS一;

static import core.bitop;

alias bsr = core.bitop.bsr;
alias bsf = core.bitop.bsf;

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
	@safe @nogc pure nothrow U
	clz(U)(U u) if (is(Unqual!U == uint) || is(Unqual!U == size_t))
	{
		pragma(inline, true);
		enum U max = 8 * U.sizeof - 1;
		return max - bsr(u);
	}

	static if (isX86)
	{
		@safe @nogc pure nothrow uint
		clz(U)(U u) if (is(Unqual!U == ulong))
		{
			pragma(inline, true);
			uint hi = u >> 32;
			return hi ? 31 - bsr(hi) : 63 - bsr(cast(uint)u);
		}
	}
}
else version (GNU)
{
	import gcc.builtins;
	alias clz = __builtin_clz;
	static if (isX86)
	{
		@safe @nogc pure nothrow uint
		clz(ulong u)
		{
			uint hi = u >> 32;
			return hi ? __builtin_clz(hi) : 32 + __builtin_clz(cast(uint)u);
		}
	}
	else alias clz = __builtin_clzl;
}
else version (LDC)
{
	@safe @nogc pure nothrow U
	clz(U)(U u) if (is(Unqual!U == uint) || is(Unqual!U == size_t))
	{
		pragma(inline, true);
		import ldc.intrinsics;
		return llvm_ctlz(u, false);
	}

	static if (isX86)
	{
		@safe @nogc pure nothrow uint
		clz(U)(U u) if (is(Unqual!U == ulong))
		{
			pragma(inline, true);
			import ldc.intrinsics;
			return cast(uint)llvm_ctlz(u, false);
		}
	}
}
static if (__VERSION__ < 2071)
{
	// < 2.071 did not have 64-bit bsr/bsf on x86.
	@safe @nogc pure nothrow uint
	bsr(U)(U u) if (is(Unqual!U == ulong))
	{
		pragma(inline, true);
		uint hi = u >> 32;
		return hi ? bsr(hi) + 32 : bsr(cast(uint)u);
	}

	@safe @nogc pure nothrow uint
	bsf(U)(U u) if (is(Unqual!U == ulong))
	{
		pragma(inline, true);
		uint lo = cast(uint)u;
		return lo ? bsf(lo) : 32 + bsf(u >> 32);
	}
}
unittest
{
	assert(clz(uint(0x01234567)) == 7);
	assert(clz(ulong(0x0123456701234567)) == 7);
	assert(clz(ulong(0x0000000001234567)) == 7+32);
	assert(bsr(uint(0x01234567)) == 24);
	assert(bsr(ulong(0x0123456701234567)) == 24+32);
	assert(bsr(ulong(0x0000000001234567)) == 24);
	assert(bsf(uint(0x76543210)) == 4);
	assert(bsf(ulong(0x7654321076543210)) == 4);
	assert(bsf(ulong(0x7654321000000000)) == 4+32);
}


private enum 一UNITTESTING一;

// Insert a dummy main when unittesting outside of dub.
version (VibeCustomMain) {} else version (unittest) void main() {}


private enum 一MISCELLANEOUS一;

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
