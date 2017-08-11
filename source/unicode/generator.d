/***************************************************************************************************
 * 
 * Helper program to generate the lookup tables required for certain Unicode algorithms.
 * This code is conforming with Unicode 10.0.0.
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
module unicode.generator;
import std.conv;
import std.exception;
import core.bitop;
import std.stdio;
import std.string;
import std.algorithm;
import std.meta;
import std.path;

enum PropertyType
{
	catalog, enumeration, binary, string, numeric, miscellaneous
}

struct Property
{
	string name;
	string value;
}

struct Entry
{
	bool isSet = false;
	Property[] properties;
}

struct Line
{
	uint rangeStart;
	uint rangeEnd;
	string[] properties;
}

struct UnicodeCharacterDatabase
{
	PropertyType type;
	Entry[] entries;
	size_t[string] enumerationValues;
	string varName;

	this(string filename, PropertyType type)
	{
		import std.algorithm;
		import std.stdio;
		import std.uni;

		this.type = type;
		this.entries = new Entry[](0x110000);
		this.enumerationValues[null] = 0;
		this.varName = baseName(filename, ".txt");
		Line[] defaults;
		Line[] actuals;
		bool abbreviates = false;
		string enumOverridePrefix;
		string enumOverride;

		foreach (line; File(filename).byLine())
		{
			bool isDefault = false;
			char[] code;
			Line data;

			// Special @missing line syntax ?
			static immutable isMissingStr = "# @missing: ";
			static immutable propNameStr = "# Property:	";
			if (line.startsWith(isMissingStr))
			{
				isDefault = true;
				code = line[isMissingStr.length..$];
			}
			else if (line.startsWith(propNameStr))
			{
				abbreviates = true;
				enumOverridePrefix = "# "~line[propNameStr.length..$].idup~"=";
			}
			else if (abbreviates && line.startsWith(enumOverridePrefix))
			{
				enumOverride = line[enumOverridePrefix.length..$].idup;
			}
			else
			{
				// Split between code and comment section
				auto commentSplit = findSplit(line, "#");
				code = commentSplit[0];
			}
			code = strip!isWhite(code);
			if (code.length == 0)
				continue;

			uint fieldIdx = 0;
			foreach (field; splitter(code, ';'))
			{
				field = strip!isWhite(field);
				switch (fieldIdx)
				{
					case 0: // Code point(s)
						auto range = findSplit(field, "..");
						data.rangeStart = to!uint(range[0], 16);
						data.rangeEnd = range[1] == ".." ? to!uint(range[2], 16) : data.rangeStart;
						enforce(data.rangeEnd <= 0x10FFFF);
						enforce(data.rangeStart <= data.rangeEnd);
						data.rangeEnd++;
						break;
					default:
						string ifield = enumOverride ? enumOverride : field.idup;
						data.properties ~= ifield;
						if (type == PropertyType.enumeration)
						{
							if (ifield !in enumerationValues)
								enumerationValues[ifield] = enumerationValues.length;
						}
				}
				fieldIdx++;
			}
			if (type == PropertyType.enumeration)
				enforce(fieldIdx >= 2);
			else assert(0, "Not implemented");

			if (isDefault)
				defaults ~= data;
			else
				actuals ~= data;
		}

		foreach (set; [defaults, actuals])
		{
			foreach (ref definition; set)
			{
				foreach (cp; definition.rangeStart .. definition.rangeEnd)
				{
					final switch (type) with (PropertyType)
					{
						case catalog:
							assert(0, "Not implemented");
						case enumeration:
							enforce(definition.properties.length == 1);
							entries[cp].properties = [Property(null, definition.properties[0])];
							entries[cp].isSet = true;
							break;
						case binary:
						case string:
						case numeric:
						case miscellaneous:
							assert(0, "Not implemented");
					}
				}
			}
		}

		foreach (cp; 0 .. 0x110000)
			enforce(entries[cp].isSet);
	}

	struct TableEntry
	{
		ubyte[][] byteSeqs;
		string enumerationValue;
		Table* subEntries;
		
		string toString()
		{
			if (subEntries)
				return subEntries.to!string();
			else
				return enumerationValue;
		}
	}
	
	struct Table
	{
		uint level, idx;
		TableEntry[256] entries;
		
		size_t toHash() const nothrow
		{
			size_t result;
			foreach (i; 0 .. 256)
			{
				if (entries[i].subEntries)
					result = hashOf(entries[i].subEntries.idx, result);
				else
					result = hashOf(entries[i].enumerationValue, result);
			}
			return hashOf(level, result);
		}
		
		bool opEquals(ref const Table key) const
		{
			foreach (i; 0 .. 256)
			{
				if ((this.entries[i].subEntries is null) != (key.entries[i].subEntries is null))
					return false;
				if (this.entries[i].subEntries)
				{
					if (this.entries[i].subEntries.idx != key.entries[i].subEntries.idx)
						return false;
				}
				else if (this.entries[i].enumerationValue != key.entries[i].enumerationValue)
				{
					return false;
				}
			}
			return this.level == key.level;
		}
	}

	string generateEnumerationCode()
	{
		auto lookup = new Table;
		uint[4] levelAssignments;
		foreach (dchar cp; 0 .. 0x110000)
		{
			ubyte[] byteSeq;
			if (cp < 128)
			{
				byteSeq ~= cast(char)cp;
			}
			else
			{
				uint topBit = 6;
				uint bits = cp;
				do
				{
					byteSeq = char(bits & 0x3F | 0x80) ~ byteSeq;
					bits >>= 6;
					topBit--;
				}
				while (bits && bsr(bits) >= topBit);
				byteSeq = cast(char)(0xFE << topBit | bits) ~ byteSeq;
			}
			auto table = lookup;
			foreach (uint i, cu; byteSeq)
			{
				auto entry = &table.entries[cu];
				if (entry.subEntries)
				{
					table = entry.subEntries;
				}
				else if (entry.enumerationValue is null)
				{
					entry.byteSeqs = [byteSeq];
					entry.enumerationValue = entries[cp].properties[0].value;
					break;
				}
				else if (entry.enumerationValue == entries[cp].properties[0].value)
				{
					entry.byteSeqs ~= byteSeq;
					break;
				}
				else
				{
					auto subTable = new Table(i+1);
					foreach (byteSeq2; entry.byteSeqs)
					{
						subTable.entries[byteSeq2[i+1]].enumerationValue = entry.enumerationValue;
						subTable.entries[byteSeq2[i+1]].byteSeqs = [byteSeq2];
					}
					entry.byteSeqs = null;
					entry.enumerationValue = null;
					entry.subEntries = subTable;
				}
				table = entry.subEntries;
			}
		}

		Table*[Table] tableSet;
		Table*[uint][4] tableByIdx;
		tableByIdx[0][0] = lookup;

		void assignIndices(Table* table, uint level = 0)
		{
			foreach (i, entry; table.entries)
			{
				if (entry.subEntries)
				{
					assignIndices(entry.subEntries, level + 1);
					if (auto dup = *entry.subEntries in tableSet)
					{
						entry.subEntries = *dup;
					}
					else
					{
						entry.subEntries.idx = levelAssignments[level + 1]++;
						tableByIdx[level + 1][entry.subEntries.idx] = entry.subEntries;
						tableSet[*entry.subEntries] = entry.subEntries;
					}
				}
			}
		}
		assignIndices(lookup);
		levelAssignments[0] = 1;

		writefln("%s: Using %s tables with a total size: %s KiB",
			varName, sum(levelAssignments[]), sum(levelAssignments[]) / 4f);
		stdout.flush(); // in case we are buffered

		auto level0 = new ubyte[256][](levelAssignments[0]);
		auto level1 = new ubyte[256][](levelAssignments[1]);
		auto level2 = new ubyte[256][](levelAssignments[2]);
		auto level3 = new ubyte[256][](levelAssignments[3]);

		foreach (level, bin; AliasSeq!(level0, level1, level2, level3))
		{
			foreach (idx; 0 .. levelAssignments[level])
			{
				Table* table = tableByIdx[level][idx];
				enforce(table.idx   == idx);
				enforce(table.level == level);
				enforce(levelAssignments[level] + enumerationValues.length <= 256,
					format("Sum of tables and enumarations at level %s exceeds ubyte storage capacity", level));
				foreach (i, ref entry; table.entries)
				{
					if (entry.subEntries)
						bin[idx][i] = cast(ubyte)(entry.subEntries.idx + enumerationValues.length);
					else
						bin[idx][i] = cast(ubyte)enumerationValues[entry.enumerationValue];
				}
			}
		}

		// Write struct with enum
		string code = "struct " ~ varName ~ "\n{\n";
		auto sortedEnum = new string[](enumerationValues.length);
		foreach (key, value; enumerationValues)
			sortedEnum[value] = key;
		code ~= "\tenum Enum : size_t\n\t{\n\t\t";
		foreach (key, value; sortedEnum)
			code ~= (value ? value : "__") ~ ", ";
		code ~= "\n\t}\n\n";
		foreach (k, bin; AliasSeq!(level0, level1, level2, level3))
		{
			code ~= "\tstatic immutable ubyte[256][" ~ to!string(bin.length) ~ "] level" ~ to!string(k) ~ " = [\n";
			foreach (i; 0 .. bin.length)
				code ~= "\t\t[" ~ format("%(%s,%)", bin[i]) ~ "],\n";
			code ~= "\t];\n";
		}
		code ~= "}\n\n";
		return code;
	}
}

alias UCD = UnicodeCharacterDatabase;

void main()
{
	string code = "module fast.internal.unicode_tables;\n\n";
	UCD ucd;

	ucd = UCD("../ucd/auxiliary/GraphemeBreakProperty.txt", PropertyType.enumeration);
	code ~= ucd.generateEnumerationCode();
	ucd = UCD("../ucd/extracted/DerivedGeneralCategory.txt", PropertyType.enumeration);
	code ~= ucd.generateEnumerationCode();
	ucd = UCD("../ucd/extracted/DerivedLineBreak.txt", PropertyType.enumeration);
	code ~= ucd.generateEnumerationCode();

	auto tableFile = File("../source/fast/internal/unicode_tables.d", "w");
	tableFile.write(code);
}