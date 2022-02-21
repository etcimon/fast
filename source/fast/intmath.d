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


	// DMD is already faster than my ASM code above, no need to improve. Good job Walter et al.
	//import core.checkedint;
	alias mulu = core.checkedint.mulu;
