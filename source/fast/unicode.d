/***************************************************************************************************
 * 
 * Functions to work with the Unicode Transformation Format.
 * 
 * Grapheme clusters:
 *   A grapheme cluster is roughly speaking what the user would perceive as the smallest unit in a
 *   writing system. Their count can be thought of as a caret position in a text editor. In
 *   particular at grapheme cluster level, different normalization forms (NFC, NFD) become
 *   transparent. The default definition used here is independent of the user's locale.
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
module fast.unicode;

import fast.internal.unicode_tables;
import fast.internal.sysdef;
//import std.simd;


/*******************************************************************************
 * 
 * Enumeration for the Unicode "General Category" used to roughly classify
 * codepoints into letters, punctuation etc.
 *
 **************************************/
alias GeneralCategory = DerivedGeneralCategory.Enum;


/*******************************************************************************
 * 
 * A customizable structure providing information on a code point. It consists
 * of a Unicode `property` in the form of an `enum` (e.g. `GeneralCategory`) and
 * a `length` in bytes of the code point in UTF-8.
 *
 **************************************/
struct CodePointInfo(Enum)
{
	alias property this;
	size_t length;
	Enum   property;
}


/*******************************************************************************
 * 
 * Counts the number of grapheme clusters (character count) in a UTF string.
 * 
 * This function uses "extended grapheme clusters" as defined in Unicode:
 * http://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries
 * 
 * When invalid byte sequences are encountered, each byte that does not make up
 * a code point will be counted as one grapheme as visual representations of
 * such broken strings will often show a square with the hexadecimal byte value
 * in them.
 *
 * Params:
 *   str = the UTF-8 string
 *
 * Returns:
 *   the number of grapheme clusters
 *
 **************************************/
@nogc @trusted pure nothrow size_t
countGraphemes(scope const(char)[] str)
{
	enum numValues = GraphemeBreakProperty.Enum.max + 1;
	static immutable graphemeBreakRules =
	{
		// GB999
		byte[numValues][numValues] graphemeBreaks = true;
		with (GraphemeBreakProperty.Enum)
		{
			// GB12 + GB13 (special handling)
			foreach (i; 0 .. numValues)
				graphemeBreaks[i][Regional_Indicator] = -1;
			// GB11
			graphemeBreaks[ZWJ][Glue_After_Zwj] = false;
			graphemeBreaks[ZWJ][E_Base_GAZ] = false;
			// GB10 (special handling)
			graphemeBreaks[E_Base]    [E_Modifier] = false;
			graphemeBreaks[E_Base_GAZ][E_Modifier] = false;
			graphemeBreaks[Extend]    [E_Modifier] = -1;
			// GB9b
			foreach (i; 0 .. numValues)
				graphemeBreaks[Prepend][i] = false;
			// GB9a
			foreach (i; 0 .. numValues)
				graphemeBreaks[i][SpacingMark] = false;
			// GB9
			foreach (i; 0 .. numValues)
			{
				graphemeBreaks[i][Extend] = false;
				graphemeBreaks[i][ZWJ] = false;
			}
			graphemeBreaks[E_Base]    [Extend] = -1;
			graphemeBreaks[E_Base_GAZ][Extend] = -1;
			// GB8
			graphemeBreaks[LVT][T] = false;
			graphemeBreaks[T]  [T] = false;
			// GB7
			graphemeBreaks[LV][V] = false;
			graphemeBreaks[LV][T] = false;
			graphemeBreaks[V] [V] = false;
			graphemeBreaks[V] [T] = false;
			// GB6
			graphemeBreaks[L][L] = false;
			graphemeBreaks[L][V] = false;
			graphemeBreaks[L][LV] = false;
			graphemeBreaks[L][LVT] = false;
			// GB5
			foreach (i; 0 .. numValues)
			{
				graphemeBreaks[i][Control] = true;
				graphemeBreaks[i][CR] = true;
				graphemeBreaks[i][LF] = true;
			}
			// GB4
			foreach (i; 0 .. numValues)
			{
				graphemeBreaks[Control][i] = true;
				graphemeBreaks[CR]     [i] = true;
				graphemeBreaks[LF]     [i] = true;
			}
			// GB3
			graphemeBreaks[CR][LF] = false;
			// Additional homebrew top level rule to break before and after invalid characters
			foreach (i; 0 .. numValues)
			{
				graphemeBreaks[i][__] = true;
				graphemeBreaks[__][i] = true;
			}
		}
		return graphemeBreaks;
	}();

	size_t graphemeCount = 0;
	auto p = str.ptr;
	auto graphemeStart = p;
	GraphemeBreakProperty.Enum last, next;
	bool riEven, inEmojiBaseExtension;

	@noinline @safe @nogc pure nothrow bool
	complexRules()
	{
		pragma(inline, false);
		with (GraphemeBreakProperty.Enum)
		{
			if (next == Regional_Indicator)
			{
				// For GB12 + GB13 we need break only after a complete country code (2 indicators).
				if (last == Regional_Indicator)
					return riEven = !riEven;
				riEven = true;
				return false;
			}
			else if (next == Extend)
			{
				inEmojiBaseExtension = true;
				return false;
			}
			else if (inEmojiBaseExtension)
			{
				return inEmojiBaseExtension = false;
			}
			return true;
		}
	}

	@forceinline void
	graphemeCountImpl(S)(ref S str)
	{
		version (LDC) pragma(inline, true);
		auto cpi = getProperty!GraphemeBreakProperty(str);
		auto next = cpi.property;
		byte isBoundary = graphemeBreakRules[last][next];
		if (isBoundary < 0 ? complexRules() : isBoundary)
		{
			graphemeCount++;
			static if (is(S == const(char)*))
				graphemeStart = str;
			else
				graphemeStart = str.ptr;
			inEmojiBaseExtension = false;
		}
		static if (is(S == const(char)*))
			str += cpi.length;
		else
			str = str[cpi.length..$];
		last = next;
	}

	if (str.length >= 4) 
	{
		const e = str.ptr + str.length - 4;
		do
			graphemeCountImpl(p);
		while (p <= e);
		str = str[p - str.ptr..$];
	}
	while (str.length)
		graphemeCountImpl(str);
	return graphemeCount;
}


/*******************************************************************************
 * 
 * Retrieves the "General Category" of the first code point in some UTF-8
 * string. For broken UTF-8, the property is set to `GeneralCategory.__` (`0`).
 *
 * Params:
 *   str = the UTF-8 encoded text, which must not be empty
 *
 * Returns:
 *   a code point information struct consisting of a the fields `property`,
 *   containing the `GeneralCategory` enumeration and the `length` of the code
 *   point in bytes.
 * 
 **************************************/
@property @safe @nogc pure nothrow CodePointInfo!GeneralCategory
generalCategory(scope const(char)[] str)
{
	return getProperty!DerivedGeneralCategory(str);
}
unittest
{
	assert("क".generalCategory == GeneralCategory.Other_Letter);
	assert("̸".generalCategory == GeneralCategory.Nonspacing_Mark);
	assert("\xFF".generalCategory == GeneralCategory.__);
}



private:

@forceinline pure @nogc nothrow auto
getProperty(Property, S)(scope S str) if (is(S == const(char)*) || is(S == const(char)[]))
in
{
	static if (is(S == const(char)[]))
		assert(str.length != 0, "No code units passed in.");
}
out
{
	assert(__result <= Property.Enum.max);
}
body
{
	version (LDC) pragma(inline, true);
	import fast.internal.helpers;

	alias Enum = Property.Enum;
	alias CPI = CodePointInfo!Enum;
	// Fast path for ASCII.
	size_t idx = Property.level0[0][str[0]];
	if (byte(str[0]) >= 0) return CPI(1, cast(Enum)idx);
	// On multi-byte sequences, set the length to 1 for invalid sequences (idx == 0).
	size_t length = clz(str[0] ^ 0xFFu) - 24;
	// Safely return invalid code point of 1 byte length if string exhausted.
	static if (is(S == const(char)[]))
		if (length > str.length)
			return CPI(1, cast(Enum)0);
	// Otherwise use lookup table hierarchy to determine if code units form a valid code point
	if (idx > Enum.max) {
		idx = Property.level1[idx - Enum.max - 1][str[1]];
		if (idx > Enum.max) {
			idx = Property.level2[idx - Enum.max - 1][str[2]];
			if (idx > Enum.max)
				idx = Property.level3[idx - Enum.max - 1][str[3]];
		}
	}
	if (idx)
		return CPI(length, cast(Enum)idx);
	else
		return CPI(1, cast(Enum)0);
}
