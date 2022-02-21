/**
 * Fast buffer implementation.
 *
 * Authors:
 *   $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 *
 * Copyright:
 *   © 2015 $(LINK2 mailto:Marco.Leise@gmx.de, Marco Leise)
 *
 * License:
 *   $(LINK2 http://www.gnu.org/licenses/gpl-3.0, GNU General Public License 3.0)
 */
module fast.buffer; nothrow

//import core.stdc.stdint;
//import core.stdc.stdlib;
import std.range;


enum allocaLimit = 2048;


/*******************************************************************************
 * 
 * Fixed maximum number of items on the stack. Memory is a static stack buffer.
 * This buffer can be filled up and cleared for reuse.
 *
 **************************************/
struct LimitedScopeBuffer(T, size_t n)
{
private:

	T[n]   m_data;
	size_t m_used;


public:

	@safe pure nothrow @nogc
	@property inout(T)* ptr() inout
	{
		return m_data.ptr;
	}


	@safe pure nothrow @nogc
	@property size_t length() const
	{
		return m_used;
	}

	@safe pure nothrow @nogc
	@property void length(size_t value)
	in
	{
		assert( value <= n );
	}
	body
	{
		m_used = value;
	}


	@safe pure nothrow @nogc
	inout(T)[] opSlice() inout
	{
		return m_data[0 .. m_used];
	}
}


struct TempBuffer(T)
{
	T[] slice;
	bool callFree;
	
	@disable this(this);

	~this() nothrow
	{
		if (this.callFree)
			free(this.slice.ptr);
	}

	T[] opSlice() @safe pure nothrow { return this.slice[]; }
	T[] opSlice(size_t a, size_t b) @safe pure nothrow { return this.slice[a .. b]; }
	T[] opSliceAssign(const(T)[] value, size_t a, size_t b) @safe pure nothrow { return this.slice[a .. b] = value; }
	ref T opIndex(size_t idx) @safe pure nothrow { return this.slice[idx]; }
	@property size_t size() @safe pure nothrow { return T.sizeof * this.slice.length; }
	@property size_t length() @safe pure nothrow { return this.slice.length; }
	alias opDollar = length;
	@property T* ptr() @trusted pure nothrow { return this.slice.ptr; } // must use .ptr here for zero length strings
	alias ptr this;

	auto makeOutputRange()
	{
		struct OutputRange
		{
			T* ptr;
			size_t idx;

			void put(T)(auto ref T t) { ptr[idx++] = t; }
			T[] opSlice() pure nothrow { return ptr[0 .. idx]; }
		}
		return OutputRange(this.slice.ptr, 0);
	}
}


TempBuffer!T tempBuffer(T, alias length, size_t allocaLimit = .allocaLimit)
	(void* buffer = (T.sizeof * length <= allocaLimit) ? alloca(T.sizeof * length) : null)
{
	return TempBuffer!T((cast(T*) (
		buffer is null
		? malloc(T.sizeof * length)
		: buffer))[0 .. length],
	buffer is null);
}


/*******************************************************************************
 * 
 * Returns a structure to your stack that contains a buffer of $(D bytes) size.
 * Memory is allocated by calling `.alloc!T(count)` on it in order to get
 * `count` elements of type `T`. The return value will be a RAII structure
 * that releases the memory back to the stack buffer upon destruction, so it can
 * be reused. The pointer within that RAII structure is aligned to
 * `T.alignof`. If the internal buffer isn't enough to fulfill the request
 * including padding from alignment, then `malloc()` is used instead.
 * 
 * Warning:
 *   Always keep the return value of `.alloc()` around on your stack until
 *   you are done with its contents. Never pass it directly into functions as
 *   arguments!
 *
 * Params:
 *   bytes = The size of the buffer on the stack.
 *
 * Returns:
 *   A stack buffer allocator.
 *
 **************************************/
auto stackBuffer(size_t bytes)() @trusted pure
{
	// All that remains of this after inlining is a stack pointer decrement and
	// a mov instruction for the `null`.
	StackBuffer!bytes result = void;
	result.last = cast(StackBufferEntry!void*) &result.last;
	result.sentinel = null;
	return result;
}


auto asOutputRange(T)(T* t) @safe pure
{
	struct PointerRange
	{
	private:

		T* start;
		T* ptr;

	public:

		void put()(auto ref const(T) t) pure
		{
			*this.ptr++ = t;
		}

		T[] opSlice() pure
		{
			return this.start[0 .. this.ptr - this.start];
		}
	}
	static assert(isOutputRange!(PointerRange, T));
	return PointerRange(t, t);
}


enum bufferArg(alias size)()
{
	return "((size <= allocaLimit) ? alloca(size) : null)";
}



package:

struct StackBuffer(size_t bytes)
{
private:
	
	void[bytes] space = void;
	StackBufferEntry!void* last;
	void* sentinel;
	
public:
	
	@disable this(this);
	
	@trusted
	StackBufferEntry!T alloc(T)(size_t howMany)
	{
		enum max = size_t.max / T.sizeof;
		alias SBE = StackBufferEntry!T;
		T* target = cast(T*) (cast(uintptr_t) this.last.ptr / T.alignof * T.alignof);
		if (target > this.space.ptr && cast(uintptr_t) (target - cast(T*) this.space.ptr) >= howMany)
			return SBE(target - howMany, this.last);
		else
			// TODO: Respect alignment here as well by padding. Optionally also embed a length in the heap block, so we can provide slicing of the whole thing.
			return SBE(howMany <= max ? cast(T*) malloc(T.sizeof * howMany) : null);
	}
}

struct StackBufferEntry(T)
{
private:

	StackBufferEntry!void* prev;

	this(T* ptr) pure { this.ptr = ptr; }

	this(T* ptr, ref StackBufferEntry!void* last) pure
	{
		this.ptr = ptr;
		this.prev = last;
		last = cast(StackBufferEntry!void*) &this;
	}


public:
	
	T* ptr;
	
	static if (!is(T == void))
	{
		@disable this(this);
	
		~this() @trusted
		{
			if (this.prev)
			{
				StackBufferEntry!void* it = this.prev;
				while (it.prev) it = it.prev;
				auto last = cast(StackBufferEntry!void**) &prev.ptr;
				*last = this.prev;
			}
			else free(this.ptr);
		}

		@system pure nothrow @nogc
		ref inout(T) opIndex(size_t idx) inout
		{
			return ptr[idx];
		}

		@system pure nothrow @nogc
		inout(T)[] opSlice(size_t a, size_t b) inout
		{
			return ptr[a .. b];
		}

		@safe pure nothrow @nogc
		@property auto range()
		{
			return ptr.asOutputRange();
		}
	}
}
