/*

 Package: dyncall
 Library: dyncallback
 File: dyncallback/dyncall_callback_mips_n64.S
 Description: Callback Thunk - Implementation for mips64 n64
 License:

   Copyright (c) 2016-2018 Tassilo Philipp <tphilipp@potion-studios.com>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

*/

	/* input:
		$t8    -> thunk
		$t8+56 -> cb handler
		$t8+64 -> userdata
	*/

/* Frame size is 160b for hard- and 128b for soft-float, as follows: */
/*   DCargs(fregs:64? + iregs:64 + regcnts:8 + sp:8) + rval:8 + ra:8 */
#if defined(DC__ABI_HARDFLOAT)
SP_SP = 160
#else
SP_SP = 96
#endif
SP_LR     = SP_SP-8
SP_RVAL   = SP_SP-16
SP_ARG_SP = SP_SP-24
SP_ARG_RC = SP_SP-32

	.section .mdebug.abi64
	.previous
	.abicalls
	.text
	.align  2
	.globl dcCallbackThunkEntry
	.ent   dcCallbackThunkEntry
dcCallbackThunkEntry:
	.set    noreorder

	/* Prolog. */
	daddiu $sp, $sp, -SP_SP   /* open frame */
	sd     $ra, SP_LR($sp)    /* save link register */

	.frame $fp,SP_SP,$31  /* specify our frame: fp,size,lr; creates virt $fp */
	                      /* code below doesn't use $fp though, as n/a with -O1 */
	/* Init return value */
	sd $zero, SP_RVAL($sp)

	/* Store reg args where our DCargs member arrays are, in local stack area */
	sd  $4,     0($sp)
	sd  $5,     8($sp)
	sd  $6,    16($sp)
	sd  $7,    24($sp)
	sd  $8,    32($sp)
	sd  $9,    40($sp)
	sd $10,    48($sp)
	sd $11,    56($sp)
#if defined(DC__ABI_HARDFLOAT)
	s.d $f12,  64($sp)
	s.d $f13,  72($sp)
	s.d $f14,  80($sp)
	s.d $f15,  88($sp)
	s.d $f16,  96($sp)
	s.d $f17, 104($sp)
	s.d $f18, 112($sp)
	s.d $f19, 120($sp)
#endif

	/* Init DCarg's reg_counts and stackptr. */
	sd  $zero, SP_ARG_RC($sp)   /* reg_count */
	daddiu $4, $sp, SP_SP
	sd     $4, SP_ARG_SP($sp)   /* stackptr */

	/* Prepare callback handler call. */
	move   $4, $24          /* Param 0 = DCCallback*, $24/$t8 holds DCThunk* */
	move   $5, $sp          /* Param 1 = ptr to where DCArgs* is stored */
	daddiu $6, $sp, SP_RVAL /* Param 2 = results ptr to 8b of local stack data */
	ld     $7, 64($24)      /* Param 3 = userdata pointer */

	ld     $25, 56($24)     /* store handler entry in $25/$t9, required for PIC */
	jalr   $25              /* jump */
	nop                     /* branch delay nop */

	/* Copy result in corresponding registers $2-$3 ($v0-$v1) and $f0 */
	ld     $2, SP_RVAL($sp)   /* note: ignoring 2nd possible retval in $3, here */
#if defined(DC__ABI_HARDFLOAT)
	l.d    $f0, SP_RVAL($sp)
#endif

	/* Epilog. Tear down frame and return. */
	ld     $ra, SP_LR($sp)  /* restore return address */
	daddiu $sp, $sp, SP_SP  /* close frame */
	j      $ra              /* return */
	nop                     /* branch delay nop */

	.set    reorder
	.end    dcCallbackThunkEntry
	.ident  "handwritten"

