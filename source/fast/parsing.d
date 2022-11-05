/***************************************************************************************************
 * 
 * Text parsing functionality.
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
module fast.parsing;

import std.traits;
import fast.internal.sysdef;
import std.array : staticArray;

static enum isAMD64 = false;

/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Hexadecimal
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

/*******************************************************************************
 * 
 * Decodes a single hexadecimal character.
 *
 * Params:
 *   c = The hexadecimal digit.
 *
 * Returns:
 *   `c` converted to an integer.
 *
 **************************************/
@safe @nogc pure nothrow
uint hexDecode(char c)
{
	return c + 9 * (c >> 6) & 15;
}


@nogc pure nothrow
uint hexDecode4(ref const(char)* hex)
{
	uint x = *cast(uint*) &hex;
	hex += 4;
	x = (x & 0x0F0F0F0F) + 9 * (x >> 6 & 0x01010101);
	version (LittleEndian)
	{
		return x >> 24 | x >> 12 & 0xF0 | x & 0xF00 | x << 12 & 0xF000;
	}
	else
	{
		x = (x | x >> 4) & 0x00FF00FF;
		return (x | x >> 8) & 0x0000FFFF;
	}
}


@nogc pure nothrow
inout(char)* hexDecode4(ref inout(char)* hex, out uint result)
{
	foreach (i; 0 .. 4)
	{
		result *= 16;
		char ch = cast(char) (hex[i] - '0');
		if (ch <= 9)
		{
			result += ch;
		}
		else
		{
			ch = cast(char) ((ch | 0x20) - 0x31);
			if (ch <= 5)
				result += ch + 10;
			else
				return hex + i;
		}
	}
	hex += 4;
	return null;
}
unittest
{
	string x = "aF09";
	const(char)* p = x.ptr;
	uint result;
	hexDecode4(p, result);
	assert(result == 0xAF09);
}


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ Numbers
 ╚══════════════════════════════════════════════════════════════════════════════
 +/


/// Options for `parseNumber`.
struct NumberOptions
{
	/// Allows the minus sign as the first character and thus negative numbers.
	bool minus;
}


/*******************************************************************************
 * 
 * Parse a number from a character read pointer.
 * 
 * On success, the read pointer is set behind the number.
 *
 * Params:
 *   opt = Selects features for the implementation. Less features make the
 *         parser faster.
 *   str = The read pointer.
 *   n = A reference to a number to be overwritten with the result.
 *
 * Returns:
 *   An indication of success. Typically the function fails when a number cannot
 *   be stored in an integer of the given size or invalid characters are
 *   encountered.
 *
 **************************************/
@nogc nothrow
bool parseNumber(NumberOptions opt, N)(ref const(char)* str, ref N n) if (isNumeric!N)
{
	import fast.internal.helpers;
	import std.range;

	// Integer types larger than the mantissa of N.
	static if (N.sizeof <= size_t.sizeof)
	{
		alias U = size_t;
		alias I = ptrdiff_t;
	}
	else
	{
		alias U = ulong;
		alias I = long;
	}
	
	// Largest value of type U that can be multiplied by 10 and have a digit added without overflow.
	enum canHoldOneMoreDigit = (U.max - 9) / 10;
	static if (isFloatingPoint!N)
	{
		enum significandRightShift = 8 * U.sizeof - N.mant_dig + 1;
		//enum lastSignificandBit = U(2) << 8 * U.sizeof - N.mant_dig;
		enum firstFractionBit   = U(1) << 8 * U.sizeof - N.mant_dig;
		//enum remainderBits = U.max - N.mant_dig + 1;
		enum expShift = N.mant_dig - 1;
		enum expBias = N.max_exp - 1;
	}

	static if (isFloatingPoint!N)
	{

		// Largest power of 10 that fits into a float of type N. The factor 5 here is correct, as the 2s
		// go in as an increment in the exponent, that is neglectable here.
		enum pow10MaxF = {
			U v = 1; uint exp;
			while (v <= ((U(1) << N.mant_dig) - 1) / 5) { v *= 5; exp++; }
			return exp;
		}();
		__gshared static bool pow10Fb = false;
		__gshared static N[pow10MaxF] pow10F;
		if (!pow10Fb) {
			int i = 0;
			foreach (v; N(10).recurrence!((a, n) => 10 * a[n-1]).take(pow10MaxF)) {
				pow10F[i++] = v;
			}
			pow10Fb = true;
		}
		enum pow5Max = {
			U v = 1; uint exp;
			while (v <= (U.max / 5)) { v *= 5; exp++; }
			return exp;
		}();
		__gshared static bool pow5b = false;
		__gshared static U[pow5Max] pow5;
		
		if (!pow5b) {
			int i = 0;
			foreach (v; U(5).recurrence!((a, n) => 5 * a[n-1]).take(pow5Max)) {
				pow5[i++] = v;
			}
			pow5b = true;
		}
	}
	else
	{
		enum pow10Max = {
			U v = 1; uint exp;
			while (v <= (U.max / 10)) { v *= 10; exp++; }
			return exp;
		}();
		__gshared static bool pow10b = false;
		
		__gshared static U[pow10Max] pow10;
		if (!pow10b) {
			int i = 0;
			foreach (v; U(10).recurrence!((a, n) => 10 * a[n-1]).take(pow10Max)) {
				pow10[i++] = v;
			}
			pow10b = true;
		}
	}

	const(char)* p = str;
	const(char)* point = null;
	U significand = 0;
	size_t exponent = 0;
	size_t expAdjust = void;
	bool expSign = void;
	static if (isFloatingPoint!N)
	{
		U exp2 = void;
		bool roundUp = false;
	}
	
	/////////////////// SIGN BIT HANDLING ///////////////////
	
	// Check for the sign.
	static if (opt.minus)
	{
		bool sign = (*p == '-');
		if (sign)
			p++;
	}
	
	/////////////////// INTEGRAL PART OF SIGNIFICAND ///////////////////
	
	uint digit = *p - '0';
	if (digit == 0)
	{
		// We have a single zero.
		p++;
	}
	else if (digit <= 9)
	{
		// Regular case of one or more digits.
		do
		{
			if (significand > canHoldOneMoreDigit)
				goto BigMantissa;
		BigMantissaNotSoMuch:
			significand = 10 * significand + digit;
			digit = *++p - '0';
		}
		while (digit <= 9);
	}
	else return false;
	
	/////////////////// FRACTIONAL PART OF SIGNIFICAND ///////////////////
	
	if (*p == '.')
	{
		point = ++p;
		digit = *p - '0';
		if (digit > 9)
			digit = 0;
		else do
		{
			if (significand > canHoldOneMoreDigit)
				goto BigMantissa;
			significand = 10 * significand + digit;
			digit = *++p - '0';
		}
		while (digit <= 9);
	}
	
	/////////////////// EXPONENT HANDLING ///////////////////

	expAdjust = (point is null) ? 0 : p - point;
	if ((*p | 0x20) == 'e')
	{
		p++;
		expSign = (*p == '-');
		if (expSign || *p == '+')
			p++;
		digit = *p - '0';
		if (digit > 9)
			return false;
		do
		{
			if (exponent > canHoldOneMoreDigit)
				goto BigExponent;
			exponent = 10 * exponent + digit;
			digit = *++p - '0';
		}
		while (digit <= 9);
	}
	
	if (expAdjust)
	{
		if (expSign)
		{
			if (exponent > size_t.max - expAdjust)
				goto BigExponentAdjustForDecimalPoint;
			exponent += expAdjust;
		}
		else if (exponent >= expAdjust)
		{
			exponent -= expAdjust;
		}
		else
		{
			// Amount of fraction digits turns exponent from positive to negative.
			expAdjust -= exponent;
			exponent = expAdjust;
			expSign = true;
		}
	}

	/////////////////// RESULT ASSEMBLY ///////////////////

	static if (isFloatingPoint!N)
	{
		if (significand == 0 || exponent == 0)
		{
			// The significand is the unsigned result.
			static if (opt.minus)
				if (sign)
					n = -N(significand);
			n = +N(significand);
			str = p;
			return true;
		}

		// Try the floating-point fast path: The significand's bits, as well as the 10^x exponent can be expressed
		// accurately as a float of type N. We just need to divide or multiply them based on the signedness of the
		// exponent.
		exp2 = bsr(significand);
		if (exp2 - bsf(significand) < N.mant_dig && exponent <= pow10MaxF)
		{
			N b = pow10F[exponent - 1];
			static if (opt.minus)
				if (sign)
					b = -b;
			n = expSign ? significand / b : significand * b;
			str = p;
			return true;
		}
		else if (exponent <= pow5Max)
		{
			// Special case, mostly to handle the little bit of extra precision that comes from
			// converting a double to its string representation. The last base-10 digit doesn't quite
			// fit back into a double, but we don't need to resort to arbitrary precision math just yet.
			if (expSign)
			{
				U divisor = pow5[exponent - 1];
				static if (isAMD64 && (isLDC || isGDC))
				{
					// AMD64 can divide 128-bit numbers by 64-bit numbers directly.
					size_t expDivisor = clz(divisor);
					divisor <<= expDivisor;
					exp2 = expDivisor - exponent - bigDiv(significand, divisor);
					significand <<= 1;
				}
				else
				{
					// We perform an iterative division.
					U dividend = significand << 8 * U.sizeof - 1 - exp2;
					U quotient = dividend / divisor;
					dividend %= divisor;

					size_t lzs = clz(quotient);
					exp2 -= exponent + lzs;
					significand = quotient << ++lzs;
					size_t accuracy = 8 * U.sizeof - lzs;
					while (accuracy < N.mant_dig)
					{
						lzs = clz(dividend);
						dividend <<= lzs;
						quotient = dividend / divisor;
						dividend %= divisor;
						significand |= quotient << (8 * U.sizeof - lzs) >> accuracy;
						accuracy += lzs;
					}
				}

				// Assemble floating point value from bits.
				roundUp = (significand & firstFractionBit) != 0;
				significand >>= significandRightShift;
				if (roundUp)
				{
					significand++;
					significand &= ~(U(1) << N.mant_dig - 1);
					if (significand == 0)
						++exp2;
				}

				U* result = cast(U*) &n;
				*result = exp2 + expBias << expShift | significand;
				static if (opt.minus)
					*result |= U(sign) << U.sizeof * 8 - 1;
				str = p;
				return true;
			}
			//else assert(0, "Not implemented");
		}
		//else assert(0, "Not implemented");
	}
	else
	{
		import fast.intmath;

		if (exponent && significand)
		{
			// We need to account for the exponent.
			U pow = pow10[exponent - 1];
			if (expSign)
			{
				// Negative exponent, if we get a fractional result, abort.
				if (significand % pow)
					return false;
				significand /= pow;
			}
			else static if (U.sizeof < ulong.sizeof)
			{
				// Multiply using a bigger result type
				ulong prod = ulong(significand) * pow;
				if (prod > U.max)
					return false;
				significand = cast(U) prod;
			}
			else
			{
				// If the multiply will overflow, abort.
				bool overflowed;
				significand = mulu(significand, pow, overflowed);
				if (overflowed)
					return false;
			}
		}

		n = cast(N) significand;
		static if (isSigned!N && opt.minus)
		{
			if (significand > U(N.max) + sign)
				return false;
			if (sign)
				n = -n;
		}
		else if (significand > N.max)
			return false;
		str = p;
		return true;
	}

BigMantissa:
	if (significand <= (significand.max - digit) / 10)
		goto BigMantissaNotSoMuch;
//	assert(0, "Not implemented");

BigExponent:
//	assert(0, "Not implemented");

BigExponentAdjustForDecimalPoint:
//	assert(0, "Not implemented");
	return false;
}


private template PowData(U, U base)
{
	import std.range;

	// Largest power of `base` that fits into an integer of type U.
	enum powMax = { U v = 1; uint exp; while (v <= U.max / base) { v *= base; exp++; } return exp; }();
	
	// Table of powers of `base`. (We skip base^0)
	static immutable U[powMax] pows = base.recurrence!((a, n) => base * a[n-1]).take(powMax);
}


static if (isAMD64 && (isLDC || isGDC))
{
	@nogc pure nothrow
	private size_t bigDiv(ref size_t a, size_t b)
	in
	{
		assert(b > size_t.max / 2, "High bit of divisor must be set.");
	}
	body
	{
		// Make sure that the division will yield exactly 32 or 64 significant bits.
		import fast.internal.helpers;
		size_t lza = clz(a);
		version (LDC)
		{
			import ldc.llvmasm;
			a <<= lza;
			if (a >= b) { a >>= 1; lza--; }
			a = __asm!ulong("
				xor %rax, %rax
				divq $2
				", "={rax},{rdx},rm", a, b);
		}
		else version (GNU)
		{
			size_t dividend = a << lza;
			if (dividend >= b) { dividend >>= 1; lza--; }
			asm { "
				xor %%rax, %%rax
				divq %3
				" : "=&a" a, "=d" dividend : "d" dividend, "rm" b; }
		}
		return ++lza;
	}
	
	unittest
	{
		size_t a = size_t.max / 11;
		size_t b = size_t.max / 5;
		version (X86_64)
		{
			import fast.internal.helpers;
			long exp = clz(b);   // Positive base-2 exponent
			b <<= exp;
			exp -= bigDiv(a, b);
			assert(a == 0xE8BA2E8BA2E8BA2AUL);
			assert(exp == -2);
		}
	}
}


/+
 ╔══════════════════════════════════════════════════════════════════════════════
 ║ ⚑ String Scanning and Comparison
 ╚══════════════════════════════════════════════════════════════════════════════
 +/

/*******************************************************************************
 * 
 * Compares a string of unknown length against a statically known key.
 * 
 * This function also handles escapes and requires one or more terminator chars.
 *
 * Params:
 *   C = Character with.
 *   key = The static key string.
 *   terminators = A list of code units that terminate the string.
 *   special = A list of code units that are handled by the user callback. Use
 *             this for escape string handling. Default is `null`.
 *   p_str = Pointer to the string for the comparison. After the function call
 *           it will be behind the last matching character.
 *   callback = User callback to handle special escape characters if `special`
 *              is non-empty.
 *
 * Returns:
 *   A code with following meanings: -1 = not equal, terminator character hit,
 *   0 = not equal, but string not exhausted, 1 = string equals key.
 *
 **************************************/
int fixedTermStrCmp(C, immutable C[] key, immutable C[] terminators, immutable C[] special = null)
	(ref const(C)* p_str, scope bool delegate(ref immutable(char)*, ref const(char)*) callback = null)
in
{
	assert(special.length == 0 || callback !is null);
}
body
{
	import std.algorithm, std.range;
	
	static immutable byte[256] classify =
		iota(256).map!(c => terminators.canFind(c) ? byte(-1) : special.canFind(c) ? 1 : 0).staticArray;
	
	immutable(C)* p_key = key.ptr;
	immutable C* e_key = p_key + key.length;
	
	while (p_key !is e_key)
	{
		int clazz = *p_str <= 0xFF ? classify[*p_str] : 0;
		
		if (clazz < 0)
		{
			return clazz;
		}
		else if (clazz == 0)
		{
			if (*p_str != *p_key)
				return clazz;
			
			p_str++;
			p_key++;
		}
		else if (clazz > 0)
		{
			if (!callback(p_key, p_str))
				return 0;
		}
	}
	
	return classify[*p_str & 0xFF] < 0;
}


/*
@nogc nothrow
void fixedStringCompareSSE4()
{
	enum words     = key.length / 16;
	enum remainder = key.length % 16;
	enum contains0 = key.canFind('\0');     // For SSE4.2 string search.
	static assert(!contains0, "Not implemented");

	size_t remaining = e - b;
	auto p = b;

	foreach (i; staticIota!(0, words))
	{
		auto backup = p;
		p.vpcmpistri!(char, key[16 * i .. 16 * i + 16], Operation.equalElem, Polarity.negateValid);
		p = backup;
		p.vpcmpistri!(char, key[16 * i .. 16 * i + 16], Operation.equalElem, Polarity.negateValid);
	}
}
*/


@forceinline @nogc nothrow pure
void seekToAnyOf(string cs)(ref const(char)* p)
{
	bool found = false;
	while (*p) {
		foreach(c; cs) {
			if (c == *p) {
				found = true;
				break;
			}
		}
		if (found) break; else p++;
	}
	//p.vpcmpistri!(char, sanitizeChars(cs), Operation.equalAnyElem);
}


@forceinline nothrow
void seekToRanges(string cs)(ref const(char)* p)
{
	bool found = false;
	while (*p) {
		for(int i = 0; i < cs.length; i+=2) {
			if (cs[i] <= *p && cs[i+1] >= *p) {
				found = true;
				break;
			}
		}
		if (found) break; else p++;
	}
	//p.vpcmpistri!(char, sanitizeRanges(cs), Operation.inRanges);
}


/*******************************************************************************
 * 
 * Searches for a specific character known to appear in the stream and skips the
 * read pointer over it.
 *
 * Params:
 *   c = the character
 *   p = the read pointer
 *
 **************************************/
@forceinline @nogc nothrow pure
void seekPast(char c)(ref const(char)* p)
{
	while (*p){
		if (c == *p) {
			p++;
			break;
		}
		else p++;
	}
	//p.vpcmpistri!(char, c.repeat(16).to!string, Operation.equalElem);
	
}


/*******************************************************************************
 * 
 * Skips the read pointer over characters that fall into any of up to 8 ranges
 * of characters. The first character in `cs` is the start of the first range,
 * the second character is the end. This is repeated for any other character
 * pair. A character falls into a range from `a` to `b` if `a <= *p <= b`.
 *
 * Params:
 *   cs = the character ranges
 *   p = the read pointer
 *
 **************************************/
@forceinline @nogc nothrow pure
void skipCharRanges(string cs)(ref const(char)* p)
{
	import std.range : chunks;
	while (*p) {
		bool found = false;
		for(int i = 0; i < cs.length; i+=2) {
			if (cs[i] <= *p && cs[i+1] >= *p) {
				found = true;
				break;
			}
		}
		if (found) p++; else break;
	}
	//p.vpcmpistri!(char, cs, Operation.inRanges, Polarity.negate);
}


/*******************************************************************************
 * 
 * Skips the read pointer over all and any of the given characters.
 *
 * Params:
 *   cs = the characters to skip over
 *   p = the read pointer
 *
 **************************************/
@forceinline @nogc nothrow pure
void skipAllOf(string cs)(ref const(char)* p)
{ 
	while (*p) {
		bool found = false;
		foreach(c; cs) {
			if (c == *p) {
				found = true;
				break;
			}
		}
		if (found) p++; else break;
	}
		
	//p.vpcmpistri!(char, cs, Operation.equalAnyElem, Polarity.negate);
}


/*******************************************************************************
 * 
 * Skips the read pointer over ASCII white-space comprising '\t', '\r', '\n' and
 * ' '.
 *
 * Params:
 *   p = the read pointer
 *
 **************************************/
@forceinline @nogc nothrow pure
void skipAsciiWhitespace(ref const(char)* p)
{
	if (*p == ' ')
		p++;
	if (*p > ' ')
		return;
	p.skipAllOf!" \t\r\n";
}


/*******************************************************************************
 * 
 * Sets the read pointer to the start of the next line.
 *
 * Params:
 *   p = the read pointer
 *
 **************************************/
@forceinline @nogc nothrow pure
void skipToNextLine(ref const(char)* p)
{
	// Stop at next \r, \n or \0.
	enum cmp_to = "\x09\x0B\x0C\x0E";
	while (*p && (*p != cmp_to[0] && *p != cmp_to[1] && *p != cmp_to[2] && *p != cmp_to[3]) )
		p++;

	//p.vpcmpistri!(char, "\x01\x09\x0B\x0C\x0E\xFF", Operation.inRanges, Polarity.negate);
	if (p[0] == '\r') p++;
	if (p[0] == '\n') p++;
}


private enum sanitizeChars(string cs)
{
	import std.exception;

	bool has0 = false;
	foreach (c; cs) if (!c) { has0 = true; break; }
	assert(has0, "Parsers are required to also check for \0 when looking for specific chars.");
	
	return cs;
}


private enum sanitizeRanges(string cs)
{
	import std.exception;

	bool has0 = false;
	foreach (i; 0 .. cs.length / 2) if (!cs[2*i]) { has0 = true; break; }
	assert(has0, "Parsers are required to also check for \0 when looking for specific chars.");
	return cs;
}

