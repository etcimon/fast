/***************************************************************************************************
 * 
 * Internal benchmark module.
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
module fast.internal.benchmarks;

version (benchmark):

void main()
{
	import std.stdio;
	import core.stdc.string, core.stdc.stddef, core.stdc.stdlib;
	import std.array, std.stdio, std.algorithm, std.regex, std.utf, std.conv, std.string, std.range;
	import fast.string, fast.cstring, fast.buffer, fast.format, fast.json;
	import std.format : formattedWrite;

	static immutable nums = { ulong[1uL << 8] nums = void; foreach (i; 0 .. nums.length) nums[i] = (1uL << (64 - 8)) * i; return nums; }();
	static immutable part1 = "C:\\";
	static immutable part2 = "Documents and Settings\\User\\My Documents\\My Downloads\\";
	static immutable part3 = "Fast.zip";
	static immutable pathname = "hello/i_am_a/path_name\\with_several_different\\slashes";
	static immutable zeroterm = "wefwfnqwefnw(eknwoemkf)moorroijqwoijq&oqo(vqwojkpjavnal(nvo(eirvn$wefwfnqwefnw(eknwoemkf)moorroijqwoihqioqo(vqwojkpjavnal(nvo(eirvn$wefwfnqwef\"w(eknwoemkf)moorroijqwoijqioqo(vqwojkpjavnal(nvo(eirvn$\0";
	static pathSepRegex = ctRegex!`[/\\]`;
	enum pathnameWStringLength = to!(immutable(wchar_t)[])(pathname).length;

	unicode();

	jsonCoordinates!true();
	jsonCoordinates!false();

	run ("Format strings for integers...", 13093,
		benchmark ("std.*.format", () { uint check; foreach (ulong num; nums) { string str = format("decimal: %s, hex: %x", num, num); check += str[9]; } return check; } ),
		benchmark ("fast.*.format", () { uint check; foreach (ulong num; nums) { string str = fast.format.format!"decimal: %s, hex: %x"(num, num); check += str[9]; } return check; } ),
		benchmark ("fast.*.formata", () { uint check; foreach (ulong num; nums) { char[] str = formata!"decimal: %s, hex: %x"(num, num); check += str[9]; } return check; } ),
		);

	run ("Convert 256 numbers to fixed width hex strings...", 0x20,
		benchmark ("std.*.formattedWrite", () { Appender!(char[]) app; app.reserve(16); char check = 0; foreach (ulong num; nums) { app.formattedWrite("%016X", num); check += app.data[0]; app.clear(); } return check; }),
		benchmark ("fast.*.hexStrUpper", () { char[16] str; char check = 0; foreach (ulong num; nums) { str = hexStrUpper(num); check += str[0]; } return check; }),
		);

	run ("Concatenate a known number of strings...", part1.length + part2.length + part3.length,
		benchmark ("std.array.appender", () { auto app = appender(part1); app ~= part2; app ~= part3; return app.data.length; }),
		benchmark ("~", () { string path = part1 ~ part2 ~ part3; return path.length; }),
		benchmark ("fast.string.concat", () { size_t length; { auto path = concat!(part1, part2, part3); length = path.length; } return length; }),
		);

	run ("Allocate a temporary char buffer and fill it with 0xFF...", '\xFF',
		benchmark ("new", () { auto str = new char[](zeroterm.length); return str[$-1]; }),
		benchmark ("malloc", () { auto ptr = cast(char*) malloc(zeroterm.length); scope(exit) free(ptr); memset(ptr, 0xFF, zeroterm.length); return ptr[zeroterm.length-1]; }),
		benchmark ("fast.buffer.tempBuffer", () { char result; { auto buf = tempBuffer!(char, zeroterm.length); memset(buf, 0xFF, zeroterm.length); result = buf[$-1]; } return result; }),
		);

	run("Convert a string to a wchar*...", wchar('\0'),
		benchmark ("toUTFz", () { return toUTFz!(wchar*)(pathname)[pathnameWStringLength]; }),
		benchmark ("cstring.wcharPtr", () { wchar result; { auto buf = wcharPtr!pathname; result = buf.ptr[pathnameWStringLength]; } return result; }),
		);

	run("Convert a string to a char*...", '\0',
		benchmark ("toUTFz", () { return toUTFz!(char*)(pathname)[pathname.length]; }),
		benchmark ("toStringz", () { return cast(char) toStringz(pathname)[pathname.length]; }),
		benchmark ("cstring.charPtr", () { return cast(char) charPtr!pathname[pathname.length]; }),
		);

	run ("Split a string at each occurance of <, >, & and \"...", "w(eknwoemkf)moorroijqwoijqioqo(vqwojkpjavnal(nvo(eirvn$\0",
		benchmark (`while+if with 4 cond.`, () { string before; immutable(char*) stop = zeroterm.ptr + zeroterm.length; immutable(char)* iter = zeroterm.ptr; immutable(char)* done = zeroterm.ptr; if (iter !is stop) do { char c = *iter++; if (c == '<' || c == '>' || c == '&' || c == '"') { before = done[0 .. iter - done]; done = iter; }} while (iter !is stop); return done[0 .. stop - done]; }),
		benchmark ("fast.string.split", () { string before, after = zeroterm; while (fast.string.split!`or(or(=<,=>),or(=&,="))`(after, before, after)) {} return before; }),
		);

	run ("Split a path by '/' or '\\'...", "slashes",
		benchmark ("std.regex.split", () { return split(pathname, pathSepRegex)[$-1]; }),
		benchmark ("std.regex.splitter", () { string last; auto range = splitter(pathname, pathSepRegex); while (!range.empty) { last = range.front; range.popFront(); } return last; }),
		benchmark ("fast.string.split", () { string before, after = pathname; while (fast.string.split!`or(=\,=/)`(after, before, after)) {} return before; }),
		);

	writeln("Benchmark done!");
}



private:

void unicode()
{
	import std.range, std.uni, std.string, std.meta;
	import fast.unicode;

	static immutable string devanagari = cast(string)"तदपि कही गुर बारंिह बारा। समुझि परी कछु मति अनुसारा।।
भाषाबद्ध करबि मैं सोई। मोरें मन प्रबोध जेंिह होई।।
जस कछु बुधि बिबेक बल मेरें। तस कहिहउँ हियँ हरि के प्रेरें।।
निज संदेह मोह भ्रम हरनी। करउँ कथा भव सरिता तरनी।।
बुध बिश्राम सकल जन रंजनि। रामकथा कलि कलुष बिभंजनि।।
रामकथा कलि पंनग भरनी। पुनि बिबेक पावक कहुँ अरनी।।
रामकथा कलि कामद गाई। सुजन सजीवनि मूरि सुहाई।।
सोइ बसुधातल सुधा तरंगिनि। भय भंजनि भ्रम भेक भुअंगिनि।।
असुर सेन सम नरक निकंदिनि। साधु बिबुध कुल हित गिरिनंदिनि।।
संत समाज पयोधि रमा सी। बिस्व भार भर अचल छमा सी।।
जम गन मुहँ मसि जग जमुना सी। जीवन मुकुति हेतु जनु कासी।।
रामहि प्रिय पावनि तुलसी सी। तुलसिदास हित हियँ हुलसी सी।।
सिवप्रय मेकल सैल सुता सी। सकल सिद्धि सुख संपति रासी।।
सदगुन सुरगन अंब अदिति सी। रघुबर भगति प्रेम परमिति सी।।
".representation.repeat(10).join.array();
	static immutable string latin = "A gory knife had been found close to the murdered man, and it had been
recognized by somebody as belonging to Muff Potter--so the story ran.
And it was said that a belated citizen had come upon Potter washing
himself in the \"branch\" about one or two o'clock in the morning, and
that Potter had at once sneaked off--suspicious circumstances,
especially the washing which was not a habit with Potter. It was also
said that the town had been ransacked for this \"murderer\" (the public
are not slow in the matter of sifting evidence and arriving at a
verdict), but that he could not be found. Horsemen had departed down
all the roads in every direction, and the Sheriff \"was confident\" that
he would be captured before night.
".repeat(10).join.array();

	void benchCountGraphemes(alias text)(size_t count)
	{
		run ("Count graphemes in " ~ text.stringof ~ " text...", count,
			benchmark ("byGrapheme.walkLength", () { return text.byGrapheme.walkLength(); }),
			benchmark ("fast.graphemeCount", () { return text.countGraphemes(); }),
			);
	}
	benchCountGraphemes!devanagari(5430);
	benchCountGraphemes!latin(7210);
}


void jsonCoordinates(bool integral)()
{
	// A variant of https://github.com/kostya/benchmarks with less coordinate tuples,
	// since we repeat the test runs until a time span of one second passed.
	import core.memory;
	import std.algorithm;
	import std.ascii;
	import std.format;
	import std.random;
	import std.range;
	import std.typecons;
	import fast.internal.helpers;

	enum coordCount = 10_000;
	auto rng = Mt19937(0);
	__gshared string text = "{\n  \"coordinates\": [\n";
	foreach (i; 0 .. coordCount)
	{
		static if (integral)
		{
			text ~= format("    {\n      \"x\": %s,\n      \"y\": %s,\n      \"z\": %s,\n" ~
				"      \"name\": \"%s %s\",\n      \"opts\": {\n        \"1\": [\n          1,\n          true\n" ~
				"        ]\n      }\n    }", uniform(0, 10_000, rng), uniform(0, 10_000, rng), uniform(0, 10_000, rng),
				iota(5).map!(_ => lowercase[uniform(0, $, rng)]), uniform(0, 10000, rng));
		}
		else
		{
			text ~= format("    {\n      \"x\": %.17g,\n      \"y\": %.17g,\n      \"z\": %.17g,\n" ~
				"      \"name\": \"%s %s\",\n      \"opts\": {\n        \"1\": [\n          1,\n          true\n" ~
				"        ]\n      }\n    }", uniform(0.0, 1.0, rng), uniform(0.0, 1.0, rng), uniform(0.0, 1.0, rng),
				iota(5).map!(_ => lowercase[uniform(0, $, rng)]), uniform(0, 10000, rng));
		}
		text ~= (i == coordCount - 1) ? "\n" : ",\n";
	}
	text ~= "  ],\n  \"info\": \"some info\"\n}\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
	text = text[0 .. $-16];

	GC.collect();

	// Dlang on x86 with optimizations rounds up double additions.
	static if (integral)
	{
		version (X86)
			enum expect = tuple(4986L, 4997L, 4988L);
		else
			enum expect = tuple(5003L, 4979L, 4971L);
	}
	else static if (isDMD && isX86 && !isRelease || isGDC && isX86)
		enum expect = tuple(0.49823454184104704, 0.50283215330409059, 0.49828840592580270);
	else static if (!isX86 || !isRelease)
		enum expect = tuple(0.49683911677479053, 0.50166077554665356, 0.49647639699603635);
	else
		enum expect = tuple(0.49823454184171062, 0.50283215330485886, 0.49828840592673407);

	run!(1, coordCount)("JSON 3D coordinates (" ~ (integral ? "integers" : "floating-point") ~ ")", expect,
		benchmark("std.json", {
				import std.json;
				
				auto json = parseJSON(text);
				auto coordinates = json["coordinates"].array;
				size_t len = coordinates.length;
				static if (integral)
					long x, y, z;
				else
					double x = 0, y = 0, z = 0;
				foreach (i; 0 .. len)
				{
					auto coord = coordinates[i];
					static if (integral)
					{
						x += coord["x"].integer;
						y += coord["y"].integer;
						z += coord["z"].integer;
					}
					else
					{
						x += coord["x"].floating;
						y += coord["y"].floating;
						z += coord["z"].floating;
					}
				}

				return tuple(x / long(len), y / long(len), z / long(len));
			}),
//		benchmark("stdx.data.json", {
//				import stdx.data.json.lexer;
//				import stdx.data.json.parser;
//
//				auto json = parseJSONStream!(LexOptions.useBigInt)(text);
//				json.skipToKey("coordinates");
//				size_t len;
//				double x = 0, y = 0, z = 0;
//				json.readArray(delegate() @trusted {
//						json.readObject!(typeof(json))(delegate(string key) @trusted {
//							if (key == "x")
//									x += json.readDouble();
//								else if (key == "y")
//									y += json.readDouble();
//								else if (key == "z")
//									z += json.readDouble();
//								else
//									json.skipValue();
//							});
//						len++;
//					});
//
//				return tuple(x / len, y / len, z / len);
//			}),
		benchmark("fast.json", {
				import fast.json;

				auto json = Json!(validateAll, true)(text);
				long len;

				static if (integral)
				{
					long x, y, z;
					foreach (i; json.coordinates)
					{
						json.keySwitch!("x", "y", "z")(
							{ x += json.read!long; },
							{ y += json.read!long; },
							{ z += json.read!long; }
							);
						len++;
					}
				}
				else
				{
					double x = 0, y = 0, z = 0;
					foreach (i; json.coordinates)
					{
						json.keySwitch!("x", "y", "z")(
							{ x += json.read!double; },
							{ y += json.read!double; },
							{ z += json.read!double; }
						);
						len++;
					}
				}

				return tuple(x / len, y / len, z / len);
			}),
		);
}


/*******************************************************************************
 * 
 * Runs a set of `Benchmark`s and prints comparing runtime statistics. The
 * functions are always called until at least a second of time has passed.
 *
 * Params:
 *   innerLoop = how many iterations to perform without looking at the clock
 *   mul = typically `1`, unless the called functions repeat an action multiple
 *         times and you want to see that reflected in the output
 *   title = short overall title of this comparing benchmark
 *   expectation = return value, that is expected from all the tested functions
 *                 for validation purposes and to counter dead-code elimination.
 *   benchmarks = A set of `Benchmark`s to be run and compared. The first one in
 *                the list acts as a reference timing for the others.
 *
 **************************************/
void run(uint innerLoop = 1000, uint mul = 1, R)(in string title, in R expectation, in Benchmark!R[] benchmarks...)
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
			foreach (k; 0 .. innerLoop)
				bm.run();
			iters++;
			t2 = TickDuration.currSystemTick;
		} while (!(t2 - t1).seconds);
		ulong times = iters * innerLoop * mul * 1_000_000_000 / (t2 - t1).nsecs;
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


/*******************************************************************************
 * 
 * Functor to create `Benchmark` structs.
 *
 * Params:
 *   title = displayed string when the statistics of `run` are displayed
 *   run   = the benchmarked function
 *
 * Returns:
 *   a `Benchmark` from the given information
 *
 **************************************/
Benchmark!R benchmark(R)(string title, R function() run)
{
	return Benchmark!R(title, run);
}


/*******************************************************************************
 * 
 * Information about a benchmarked function.
 *
 **************************************/
struct Benchmark(R)
{
	string title;
	R function() run;
}
