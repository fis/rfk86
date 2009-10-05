;;; ti86.inc -*- mode: z80-asm -*-
;;; TI-86 specific constants

;;; Copyright (c) 2009, Heikki Kallasjoki.
;;; All rights reserved.
;;; License terms: see README.txt, "Legal Disclaimer".

;;; ROM routines
;;; ============

_ex_ahl_bde:		equ $45f3
_set_abs_src_addr:	equ $4647
_set_mm_num_bytes:	equ $464f
_clrLCD:		equ $4a7e
_clrScrn:		equ $4a82
_homeup:		equ $4a95
_runindicon:		equ $4aad
_runindicoff:		equ $4ab1
_set_abs_dest_addr:	equ $5285
_mm_ldir:		equ $52ed
_mm_lddr:		equ $52f1
_getkey:		equ $55aa

;;; RAM locations
;;; =============

_winBtm:		equ $d13e
_asapvar:		equ $d6fc
_asm_exec_ram:		equ $d748

;;; Graph flags
;;; ===========

graphflags:		equ $03
graphdraw:		equ 0
