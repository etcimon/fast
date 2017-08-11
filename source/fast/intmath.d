/***************************************************************************************************
 * 
 * Supplementary integer math functions.
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
module fast.intmath;

import fast.internal.helpers;


version (LDC)
{
	@safe @nogc pure nothrow
	ulong mulu(ulong x, ulong y, ref bool overflow)
	{
		import ldc.intrinsics;
		auto res = llvm_umul_with_overflow(x, y);
		overflow = res.overflow;
		return res.result;
	}
}
else static if (isPosix && isGDC && (isAMD64 || isX86))
{
	@nogc pure nothrow
	ulong mulu(ulong x, ulong y, ref bool overflow)
	{
		version (GNU)
		{
			ulong lo;
			version (X86) asm { "
					cmp $0, 4+%2
					je 1f
					cmp $0, 4%3
					je 1f
					movb $1, %1
					1:
					mov 4+%2, %%eax
					mull %3
					jno 2f
					movb $1, %1
					2:
					mov %%eax, %%ecx
					mov %2, %%eax
					mull 4%3
					jno 3f
					movb $1, %1
					3:
					add %%eax, %%ecx
					jno 4f
					movb $1, %1
					4:
					mov %2, %%eax
					mull %3
					add %%ecx, %%edx
					jnc 5f
					movb $1, %1
					5:
					" : "=&A" lo, "+*m" overflow : "m" x, "m" y : "ecx"; }
			else asm { "mul %3\njno 1f\nmovb $1, %1\n1:\n" : "=a" lo, "+*m" overflow : "a" x, "r" y : "rdx"; }
			return lo;
		}
	}
}
else
{
	// DMD is already faster than my ASM code above, no need to improve. Good job Walter et al.
	import core.checkedint;
	alias mulu = core.checkedint.mulu;
}
