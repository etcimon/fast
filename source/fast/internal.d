module fast.internal;

import core.simd;
import std.simd;


package:

/**
 * Generates a mixin string for repeating code. It can be used to unroll variadic arguments.
 * A format string is instantiated a certain number times with an incrementing parameter.
 * The results are then concatenated using an optional joiner.
 *
 * Params:
 *   elements = The elements you want to join. Only the length of this tuple is actually used and passed into format() as an incrementing number from [0 .. count$(RPAREN).
 *   fmt = The format string to apply on each instanciation. Use %1d$ to refer to the current index multiple times when necessary.
 *   joiner = Optional string that will be placed between instances. It could be a space or an arithmetic operation.
 *
 * Returns:
 *   The combined elements as a mixin string.
 *
 * See_Also:
 *   $(LINK2 http://forum.dlang.org/thread/vqfvihyezbmwcjkmpzin@forum.dlang.org, A simple way to do compile time loop unrolling)
 */
enum ctfeJoin(elements...)(in string fmt, in string joiner = null)
{
	import std.range : iota;
	import std.string : format;
	import std.algorithm : map;

	// BUG: Cannot use, join(), as it "cannot access the nested function 'ctfeJoin'".
	string result;
	foreach (inst; map!(i => format(fmt, i))(iota(elements.length))) {
		if (result && joiner) result ~= joiner;
		result ~= inst;
	}
	return result;
}

pure nothrow
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

@safe pure nothrow
{
	/// Returns whether the (positive) argument is an integral power of two.
	@property bool isPowerOf2(in size_t n)
	in { assert(n > 0); }
	body { return (n & n - 1) == 0; }


	version (X86)
		enum hasSSE2 = sseVer >= SIMDVer.SSE2;
	else version (X86_64)
		enum hasSSE2 = sseVer >= SIMDVer.SSE2;
	else
		enum hasSSE2 = false;

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

	version (LDC) {
		pragma(LDC_intrinsic, "llvm.x86.sse2.pmovmskb.128")
		uint moveMask(ubyte16);
	} else version (GNU) {
		import gcc.builtins;
		alias moveMask = __builtin_ia32_pmovmskb128;
	}

	template SIMDFromScalar(V, alias scalar)
	{
		version (LDC) {
			// I couldn't define an immutable vector otherwise.
			static if(is(V == double2))
				alias double[2] VectorType;
			else static if(is(V == float4))
				alias float[4] VectorType;
			else static if(is(V == long2))
				alias long[2] VectorType;
			else static if(is(V == ulong2))
				alias ulong[2] VectorType;
			else static if(is(V == int4))
				alias int[4] VectorType;
			else static if(is(V == uint4))
				alias uint[4] VectorType;
			else static if(is(V == short8))
				alias short[8] VectorType;
			else static if(is(V == ushort8))
				alias ushort[8] VectorType;
			else static if(is(V == byte16))
				alias byte[16] VectorType;
			else static if(is(V == ubyte16))
				alias ubyte[16] VectorType;
			else
				static assert(0, "Incorrect type ");
			immutable VectorType arr = scalar;
			ref V get() @trusted { return *cast(V*) &arr; }
			alias SIMDFromScalar = get;
		} else {
			immutable V asVector = scalar;
			alias SIMDFromScalar = asVector;
		}
	}
}



private:

version (benchmark)
{

	struct Benchmark(R)
	{
		string title;
		R function() run;
	}

	Benchmark!R benchmark(R)(string title, R function() run)
	{
		return Benchmark!R(title, run);
	}

	void run(R)(in string title, in R expectation, in Benchmark!R[] benchmarks...)
	{
		import core.time, std.stdio, std.exception, std.string;

		writeln("\x1b[1m", title, "\x1b[0m");
		writeln();
		ulong reference;
		foreach (i, ref bm; benchmarks) {
			// Check that the result is as expected...
			auto actual = bm.run();
			enforce(actual == expectation, format(`Benchmark "%s" did not result as expected in "%s", but in "%s".`,
			                                       bm.title, expectation, actual));
			ulong iters = 0;
			immutable t1 = TickDuration.currSystemTick;
			TickDuration t2;
			do {
				foreach (k; 0 .. 1_000)
					bm.run();
				iters++;
				t2 = TickDuration.currSystemTick;
			} while (!(t2 - t1).seconds);
			ulong times = iters * 1_000 * 1_000_000_000 / (t2 - t1).nsecs;
			if (i == 0) {
				reference = times;
				writefln("  %-22s: %10s per second", bm.title, times);
			} else if (reference <= times) {
				writefln("\x1b[1m  %-22s: %10s per second (done in %.0f%% of time !)\x1b[0m", bm.title, times, 100.0 * reference / times);
			} else {
				writefln("  %-22s: %10s per second (slower by factor %.1f)", bm.title, times, 1.0 * reference / times);
			}
		}
		writeln();
	}

	void main()
	{
		import std.stdio, std.algorithm, std.regex, std.utf, std.conv;
		import fast.string, fast.uniconv;

		static immutable pathname = "hello/i_am_a/path_name\\with_several_different\\slashes";
		static zeroterm = "wefwfnqwefnw(eknwoemkf)moorroijqwoijqioqo(vqwojkpjavnal(nvo(eirvn$wefwfnqwefnw(eknwoemkf)moorroijqwoijqioqo(vqwojkpjavnal(nvo(eirvn$wefwfnqwefnw(eknwoemkf)moorroijqwoijqioqo(vqwojkpjavnal(nvo(eirvn$\0";
		static pathSepRegex = ctRegex!`[/\\]`;
		enum pathnameWStringLength = to!wstring(pathname).length;

		run("Convert a string to a wchar*...", cast(wchar)'\0',
		    benchmark ("toUTFz", () { return toUTFz!(wchar*)(pathname)[pathnameWStringLength]; }),
		    benchmark ("uniconv.stackToWString", () { mixin stackToWString!("result", pathname); return result.ptr[pathnameWStringLength]; }),
		);

		run ("Check that there are no tabs in a string...", true,
		    benchmark ("std.string.indexOf", () { return std.string.indexOf(zeroterm, '\t') == -1; }),
		    benchmark ("algorithm.countUntil", () { return countUntil(zeroterm, '\t') == -1; }),
		    benchmark ("foreach", () { foreach (ref c; zeroterm) if (c == '\t') return false; return true; }),
		    benchmark ("fast.string.find", () { return !(fast.string.find!'\t'(zeroterm) < zeroterm.length); }),
		);

		run ("Find terminating zero in a string...", zeroterm.length - 1,
		    benchmark ("std.string.indexOf", () { return cast(size_t) std.string.indexOf(zeroterm, '\0'); }),
		    benchmark ("algorithm.countUntil", () { return cast(size_t) countUntil(zeroterm, '\0'); }),
		    benchmark ("while(*ptr) ptr++", () { auto ptr = zeroterm.ptr; while (*ptr) ptr++; return cast(size_t)  (ptr - zeroterm.ptr); }),
		    benchmark ("fast.string.find", () { return fast.string.find!'\0'(zeroterm.ptr); }),
		);

		run ("Split a path by '/' or '\\'...", "slashes",
		    benchmark ("std.regex.splitter", () { string last; auto range = splitter(pathname, pathSepRegex); while (!range.empty) { last = range.front; range.popFront(); } return last; }),
		    benchmark ("std.regex.split", () { return split(pathname, pathSepRegex)[$-1]; }),
		    benchmark ("fast.string.split", () { string before, after = pathname; while (fast.string.split!('\\', '/')(after, before, after)) {} return after; }),
		);

		writeln("Benchmark done!");
	}
}
