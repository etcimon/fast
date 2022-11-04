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
import fast.internal.sysdef;
import core.checkedint;
import ldc.attributes;

uint mulu()(uint x, uint y, ref bool overflow)
{
    immutable ulong r = ulong(x) * ulong(y);
    if (r >> 32)
        overflow = true;
    return cast(uint) r;
}

@(ldc.attributes.optStrategy("none"))
ulong mulu()(ulong x, uint y, ref bool overflow)
{
    ulong r = x * y;
    if (x >> 32 &&
            r / x != y) 
        overflow = true;
    return r;
}

@(ldc.attributes.optStrategy("none"))
ulong mulu()(ulong x, ulong y, ref bool overflow)
{
    immutable ulong r = x * y;
    if ((x | y) >> 32 &&
            x &&
            r / x != y) // error __multi3 not defined when optimized
        overflow = true;
    return r;
}