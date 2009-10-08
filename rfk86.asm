;;; rfk86.asm -*- mode: z80-asm -*-
;;; TI-86 robotfindskitten port

;;; Copyright (c) 2009, Heikki Kallasjoki.
;;; All rights reserved.
;;; License terms: see README.txt, "Legal Disclaimer".

include 'ti86.inc'
include 'messages.inc'

	org _asm_exec_ram


;;; Memory use:
;;;
;;; Single messages from 7 to 86 characters; max length 96 chars.
;;; Raw message text takes 18458 bytes, plus the index 408 bytes;
;;; a total of 18866 bytes. This won't fit in the loadable memory,
;;; so we load just the program + message index, and read in only
;;; the selected messages.
;;;
;;; RAM page 1 ($8000 .. $bfff) mostly free for scratch use.
;;;
;;;   $8100 .. $813f (64 bytes, 512 bits)
;;;     bitmap for selected messages (during init)
;;;   $8100 .. $823f (7*32 = 224 bytes)
;;;     screen map: one byte per character cell
;;;     00 = empty space, 01 = kitten, 02.. = non-kitten object
;;;   $8800 .. $8803 (4 bytes)
;;;     robot position variable
;;;   $8f00 .. $8fff (128 bytes)
;;;     used to hold the compressed message before decomrpessing to $9000+x
;;;   $8ffe .. $8fff (2 bytes)
;;;     a very temporary word if variables are needed
;;;   $9000 .. $977f (20*96 = 1920 bytes)
;;;     selected message texts; 3 rows of 32 bytes, wrapped


nko_bitmap:	equ $8100
screen_map:	equ $8100
robot_pos:	equ $8800
compressed:	equ $8f00
temp:		equ $8ffe
nko_text:	equ $9000


;;; Changing this might have all sorts of ramifications.
nko_count:	equ 20
;;; This should be changeable without problems, though.
robot_char:	equ 77
;;; And this one should really match with the font.
font_chars:	equ 78
;;; While this *really* should match with the messages.pl alphabet.
msg_chars:	equ 76


;;; ==========================
;;; Initialization and Cleanup
;;; ==========================

;;; start: initialization code

start:
	call _runindicoff

	push ix
	push iy

	;; welcome screen

	call splash

	ld a, 0x41
	out (6), a			; map RAM page 1 at $8000..$bfff

	;; zero the scratch area, we rely on that

	ld hl, nko_bitmap
	ld c, 1
	call bzero

	ld hl, nko_text
	ld c, (nko_count * 96 + 255)/256
	call bzero

	;; initialize permanent screen structures

	ld hl, $fc00 + 43*16
	ld a, 0xff
	ld b, 32
.draw_screen_line:
	ld (hl), a
	inc hl
	djnz .draw_screen_line

	;; build a bitmap of selected messages

select_messages:

	di

	ld b, nko_count
.nko_bitmap_select:
	ld h, nko_bitmap >> 8
	exx
	xor a
	ld b, 6
	call rand			; a <- rand : $00 .. $3f
	exx
	cp (messages_count/8)-1
	jr nc, .nko_bitmap_select	; too big, retry
	ld l, a				; hl -> random nko_bitmap byte
	exx
	xor a
	ld b, 3
	call rand			; a <- rand : 0 .. 7
	exx
	rlca
	rlca
	rlca
	or 0x47				; a <- "bit *, a"
	ld (.nko_bitmap_test), a
	or 0x80				; a <- "set *, a"
	ld (.nko_bitmap_set), a
	ld a, (hl)			; a <- nko_bitmap byte
.nko_bitmap_test: equ $+1
	bit 0, a
	jr nz, .nko_bitmap_select	; already selected, retry
.nko_bitmap_set: equ $+1
	set 0, a
	ld (hl), a			; update nko_bitmap
	djnz .nko_bitmap_select

	ei

	;; find the start of message data

	ld hl, _asapvar
	rst 20h				; OP1 <- program name
	rst 10h				; bde <- program data

	ld a, b
	ld hl, messages_data - _asm_exec_ram + 4
	add hl, de
	ex de, hl
	adc a, 0
	ld b, a				; bde <- message data start

	;; load selected messages

load_messages:

	ld ix, messages_index

	ld hl, nko_bitmap
	ld (.bitmap_pos), hl
	ld a, 0x47			; a <- "bit 0, a"
	ld (.bitmap_test), a
	ld hl, nko_text
	ld (.text_pos), hl

.load_loop:

	;; check if current message is selected

	ld a, 0x41
	out (6), a			; map RAM page 1 at $8000..$bfff

.bitmap_pos: equ $+1
	ld a, (nko_bitmap)		; updated
.bitmap_test: equ $+1
	bit 0, a			; updated
	ld c, (ix+0)			; c <- msg len
	jr z, .load_skip

	;; copy a bytes from bde into the decompression zone

	ld l, c
	xor a
	ld h, a				; ahl <- msg len
	call _set_mm_num_bytes

	call _ex_ahl_bde		; ahl <- message data pointer
	call _set_abs_src_addr
	call _ex_ahl_bde		; bde <- message data pointer

	ld hl, compressed - $8000
	ld a, 1				; ahl <- dest pointer, fixed location
	call _set_abs_dest_addr

	call _mm_ldir

	ld a, 0x41
	out (6), a			; map RAM page 1 at $8000..$bfff

	;; LZ77/Huffman-decompress the message

	push bc
	push de
.text_pos: equ $+1
	ld de, nko_text
	call decompress
	pop de
	pop bc

	;; update the destination pointer

	ld hl, .text_pos
	ld a, (hl)
	add a, 96
	ld (hl), a
	jr nc, .samepage
	inc hl
	inc (hl)
.samepage:

	;; update source pointers (always)

.load_skip: ; expected: c = msg len
	ld a, e
	add a, c
	ld e, a
	jr nc, .load_skip_rest
	inc d
	jr nz, .load_skip_rest
	inc b
.load_skip_rest:

	ld a, (.bitmap_test)
	add a, 0x08			; a: "bit n, *" -> "bit n+1, *"
	jp p, .samebyte			; no change in sign, same byte
	ld hl, (.bitmap_pos)
	inc hl
	ld (.bitmap_pos), hl
	ld a, 0x47			; a <- "bit 0, a"
.samebyte:
	ld (.bitmap_test), a

	inc ix
	push ix
	pop hl
	ld a, b
	ld bc, messages_data-1
	scf
	sbc hl, bc
	ld b, a
	jr nz, .load_loop

	;; initialize the data structures

init:

	;; clear screen_map

	ld hl, screen_map
	ld c, 1				; 256 > 32*7
	call bzero

	;; position kitten and NKOs randomly

	ld a, nko_count+1
	ld (temp), a

.init_pos:

	;; select random X/Y coordinates

	xor a
	ld b, 3
	call rand			; a <- [0, 7], randomly
	cp 7
	jr z, .init_pos			; Y=7, too big, retry
	ld l, a				; l <- random Y coord

	xor a
	ld b, 5
	call rand
	ld h, a				; h <- random X coord

	;; update screen_map

	ex de, hl			; de <- (X, Y) coords

	xor a				; a <- 0, clear carry
	ld h, a				; h <- 0

	ld a, e				; a <- Y      [0 .. 6]
	rlca				; a <- 2*Y    [0 .. 12]
	rlca				; a <- 4*Y    [0 .. 24]
	rlca				; a <- 8*Y    [0 .. 48]
	rlca				; a <- 16*Y   [0 .. 96]
	rlca				; a <- 32*Y   [0 .. 192]
	add d				; a <- 32*Y+X [0 .. 223]
	ld l, a				; hl <- byte offset

	ld bc, screen_map
	add hl, bc			; hl <- screen map pointer

	ld a, (hl)
	or a
	jr nz, .init_pos		; already occupied, retry

	ld a, (temp)
	ld (hl), a			; put current obj to map

	;; if object was zero, this is actually the robot; save and stop looping

	or a
	jr nz, .init_continue

	ld (robot_pos), de
	ld (robot_pos+2), hl

	ld a, robot_char
	ld b, d
	ld c, e
	call put_char

	jr main

.init_continue:

	;; draw a random character at that point

	ex de, hl			; hl <- XY coords

	xor a
	ld b, 6
	call rand
	inc a				; a <- random char [1, 64]
	ld b, h
	ld c, l
	call put_char

	;; proceed to next object

	ld hl, temp
	dec (hl)
	jr .init_pos			; loop broken in the middle


;;; exit: clean-up code

exit:

	ld a, 8
	ld (_winBtm), a

	call _runindicon
	call _clrScrn
	call _homeup

	pop iy
	pop ix
	ret


;;; ============
;;; Main program
;;; ============

main:
	;; get next operation, branch with it

	call _getkey
	cp key_table_size
	jr nc, main			; unknown key

	sla a				; a <- key_table offset
	ld b, 0
	ld c, a				; bc <- key_table offset
	ld hl, key_table
	add hl, bc			; hl <- key_table pointer

	ld e, (hl)
	inc hl
	ld d, (hl)			; de <- key_table value

	ex de, hl
	ld de, main			; return address
	push de
	xor a				; a = 0 in handlers
	jp (hl)				; branch to handler


key_table_size: equ 8

key_table:
	dw key_none	; $00 ..
	dw key_right	; $01 .. kRight
	dw key_left	; $02 .. kLeft
	dw key_up	; $03 .. kUp
	dw key_down	; $04 .. kDown
	dw key_none	; $05 ..
	dw key_none	; $06 ..
	dw key_exit	; $07 .. kExit

key_none:
	ret

key_exit:
	pop af		; get rid of return address
	jr exit		; clean up and quit

key_right:
	ld bc, (robot_pos)
	ld a, b
	cp 31
	ret z				; X=31, can't move right
	push bc
	pop de
	inc d				; de <- (X+1, Y)
	ld hl, (robot_pos+2)
	inc hl				; hl <- new screen_map (+1)
	jr key_move

key_left:
	ld bc, (robot_pos)
	add b
	ret z				; X=0, can't move left
	push bc
	pop de
	dec d				; de <- (X-1, Y)
	ld hl, (robot_pos+2)
	dec hl				; hl <- new screen_map (-1)
	jr key_move

key_up:
	ld bc, (robot_pos)
	add c
	ret z				; Y=0, can't move up
	push bc
	pop de
	dec e				; de <- (X, Y-1)
	ld hl, (robot_pos+2)
	push bc
	ld bc, 32			; carry = 0 from "add c"
	sbc hl, bc			; hl <- new screen_map (-32)
	pop bc
	jr key_move

key_down:
	ld bc, (robot_pos)
	ld a, c
	cp 6
	ret z				; Y=6, can't move down
	push bc
	pop de
	inc e				; de <- (X, Y+1)
	ld hl, (robot_pos+2)
	push bc
	ld bc, 32
	add hl, bc			; hl <- new screen_map (+32)
	pop bc
	jr key_move

key_move:
	;; test for obstacles
	ld a, (hl)			; a <- object in new position
	or a
	jr nz, hit_object
	;; update robot_pos variables and the screen
	ld (robot_pos), de
	ld (robot_pos+2), hl
	call put_char			; put a space in old pos
	ld a, robot_char
	ld b, d
	ld c, e
	call put_char			; put a robot in new pos
	ret

hit_object:
	;; check for kitten (A=1)

	dec a
	jr z, found_kitten

	;; find the corresponding text
	;;  nko_text + obj*96, 0 <= obj < nko_count (20)

	dec a				; a <- obj
	or a				; clear carry
	ld l, 0				; l <- 0 (for shifting bits in)

	rra
	rr l
	rra
	rr l				; al <- obj*64
	ld b, a
	ld c, l				; bc <- obj*64
	rra
	rr l
	ld h, a				; hl <- obj*32
	add hl, bc			; hl <- obj*96

	ld bc, nko_text
	add hl, bc
	ex de, hl			; de <- nko_text + obj*96

	;; write the characters on screen
	;; 96 chars, to rows 7..9, from de

	ld bc, 7			; (X, Y) <- (0, 7)
	ld a, 96			; counter

.message_loop:
	push af
	push bc
	ld a, (de)
	inc de
	call put_char
	pop bc
	inc b
	ld a, b
	cp 32
	jr nz, .message_same_row
	ld b, 0
	inc c
.message_same_row:
	pop af
	dec a
	jr nz, .message_loop

	ret

found_kitten:
	pop af		; get rid of return address
	jp splash_exit	; show message and quit


;;; ===================================
;;; Huffman/LZ77 decompression routines
;;; ===================================


;;; decompress: combined LZ77/Huffman decompression
;;;    in: de - target address to decompress current data to
;;;   out: de - points to first byte after decompressed data
;;;  mess: a, b, c, h, l

decompress:
	;; reset the decompression bit-reading code

	xor a
	ld (read_bit_offset), a
	ld a, 0x46
	ld (read_bit_bit), a

	;; read and process symbols

.decompress_loop:

	ld hl, hufftree
	call read_huffman		; a <- next lit/len symbol

	cp msg_chars
	ret z				; token == N: end of message
	jr nc, .decompress_lz77		; token > N: (length, distance) pair
	;; token < N: literal byte
	ld (de), a
	inc de
	jr .decompress_loop

	;; handle LZ77 encoded parts
	;;  see the messages.pl tables to make sense of this

.decompress_lz77:

	;; parse the length token in 'a'

	sub msg_chars+1			; a <- C -- see messages.pl
	ld c, a				; c <- C (for safe-keeping)
	cp 8
	jr c, .lz77_len_direct		; direct-length code: l == len-3
	rra				; a <- C >> 1; carry=0 here
	srl a				; a <- C >> 2
	dec a
	ld b, a				; b <- number of extra bits
	ld a, c
	and 3
	or 4				; a <- "1.." with .. from C low bits
	call read_bits
	ld c, a				; c <- len-3
.lz77_len_direct:

	;; read in and parse the distance code

	xor a				; a <- 0: default dist-1
	ld b, a				; make sure b is always 0 later
	call read_bit
	jr z, .lz77_dist_direct
	ld b, 4
	call read_bits			; a <- lz77 distance code value C
	cp 4
	jr c, .lz77_dist_direct		; direct-length code: a == dist-1
	rra				; a <- C >> 1; carry has lowest bit of a
	dec a
	ld b, a				; b <- number of extra bits; carry still valid
	rla				; stick lowest bit of C back to a
	and 1
	or 2				; a <- "1." with . from C low bit
	call read_bits			; a <- dist-1
.lz77_dist_direct:

	;; generate the repeat sequence with ldir

	ld l, a
	ld a, e
	scf
	sbc a, l
	ld l, a				; l <- e - dist
	ld a, d
	sbc a, 0
	ld h, a				; hl <- de - dist

	ld a, c
	add a, 3
	ld c, a				; bc <- len

	ldir

	jr .decompress_loop


;;; read_huffman: read a Huffman-encoded symbol
;;;    in: hl - huffman tree root
;;;   out: a - next symbol
;;;  mess: b, h, l

read_huffman_go_right:
	;; descend to right branch
	inc hl
read_huffman_enter:
	ld b, (hl)
	inc hl
	ld h, (hl)
	ld l, b
read_huffman:
	ld a, (hl)
	or a
	jr nz, .read_huffman_descend	; was an inner node
	inc hl
	ld a, (hl)
	ret
.read_huffman_descend:
	call read_bit
	jr nz, read_huffman_go_right
	;; descend to left branch
	dec hl
	jr read_huffman_enter


;;; read_bit: read a bit from the decompressed data
;;;    in: -
;;;   out: zero flag set based on next bit
;;;  mess: just the flags

read_bit:
	or a				; clear carry
	push hl
read_bit_offset: equ $+1
	ld hl, compressed		; low byte modified
read_bit_bit: equ $+1
	bit 0, (hl)			; bit index modified
	push af				; save the flags
	;; increment position
	ld hl, read_bit_bit
	ld a, (hl)
	add a, 0x08
	jp p, read_bit_no_advance	; did not wrap, still in same byte
	ld (hl), 0x46			; (hl) <- "bit 0, (hl)"
	ld hl, read_bit_offset
	inc (hl)
	jr read_bit_done
read_bit_no_advance:
	ld (hl), a
read_bit_done:
	pop af				; restore flags for customer
	pop hl
	ret


;;; read_bits: read a multi-bit (up to 8) unit
;;;    in: b - number of bits to read
;;;   out: a - those bits rotated in from the right
;;;        b - constant 0

read_bits:
.read_bits_loop:
	or a
	call read_bit
	jr z, .read_bits_zero
	scf
.read_bits_zero:
	rla
	djnz .read_bits_loop
	ret


;;; the Huffman tree data

	include 'huffman.inc'


;;; ============================
;;; Screen manipulation routines
;;; ============================

;;; put_char: put a single character on screen
;;;    in: a - character to put
;;;        b - screen X coordinate (char cells)
;;;        c - screen Y coordinate (char cells)
;;;   out: -
;;;  mess: a, b, c, h, l, ix

put_char:
	;; distinguish between font chars in the same byte

	srl a				; a <- font byte offset
	ld (.put_char_font_offset), a	; font byte offset updated
	ld a, 0x38			; a <- "jr c, *"
	jr c, .put_char_low
	ld a, 0x30			; a <- "jr nc, *"
.put_char_low:
	ld (.put_char_low_jump), a

	;; distinguish between screen chars in the same byte

	srl b				; b <- screen X byte offset
	ld a, 0x38			; a <- "jr c, *"
	jr c, .put_char_right
	ld a, 0x30			; a <- "jr nc, *"
.put_char_right:
	ld (.put_char_right_jump), a

	;; locate the screen pixels for the first row:
	;;     $fc00 + Y*6*16 + X/2
	;;   = ($fc00/16 + Y*6)*16 + X/2
	;;   = ($fc00/16 + Y*4 + Y*2)*16 + X/2
	;; 0 <= Y <= 9, so Y*6 will not carry over

	or a ; clear carry

	ld h, $0f
	ld a, c				; a <- Y
	rla				; a <- Y*2
	ld c, a				; c <- Y*2
	rla				; a <- Y*4
	add c				; a <- Y*6
	or $c0				; a <- $fc00/16 (low byte) + Y*6, no carry
	rla
	rl h
	rla
	rl h
	rla
	rl h
	rla
	rl h				; ha <- $fc00 + Y*6*16
	add b
	ld l, a				; hl <- $fc00 + Y*6*16 + X/2

	;; add the extra offset if Y >= 7 (Y*2 >= 14)

	ld a, c				; a <- Y*2
	cp 14
	jr c, .put_char_no_extra	; Y*2-14 < 0 => Y*2 < 14
	ld bc, 4*16			; four rows of gap
	add hl, bc
.put_char_no_extra:

	;; draw all rows

	ld ix, font
	ld b, 6

.put_char_loop:

	;; fetch font pixels to a

.put_char_font_offset: equ $+2
	ld a, (ix+0)			; a <- font byte; offset updated
	scf
.put_char_low_jump:
	jr c, .put_char_was_low		; skip if drawing the low nybble
	rrca
	rrca
	rrca
	rrca
.put_char_was_low:			; a [low nybble] <- font pixels

	;; write to screen

	scf
.put_char_right_jump:
	jr c, .put_char_was_right
	;; put pixels to the left nybble,  (hl) = [xxxx yyyy]
	ld c, (hl)			; c    <- [xxxx yyyy]
	rld				; (hl) <- [yyyy aaaa]
	ld a, c				; a    <- [xxxx yyyy]
	rld				; (hl) <- [aaaa yyyy]
	jr .put_char_row_done
.put_char_was_right:
	;; put pixels to the right nybble, (hl) = [xxxx yyyy]
	ld c, a				; c    <- [???? aaaa]
	rld				; (hl) <- [yyyy aaaa]
					; a    <- [???? xxxx]
	rld				; (hl) <- [aaaa xxxx]
	ld a, c				; a    <- [???? aaaa]
	rld				; (hl) <- [xxxx aaaa]
.put_char_row_done:

	;; increment font/screen pointers one row

	ld a, b
	ld bc, font_chars/2
	add ix, bc			; ix <- next font plane (chars/2 bytes)
	ld c, 16
	add hl, bc			; hl <- next screen row (16 bytes)
	ld b, a

	djnz .put_char_loop

	ret


font:
	incbin 'font.bin'


;;; =========
;;; Utilities
;;; =========


;;; bzero: zero memory with 256-byte granularity
;;;    in: c - number of 256-byte pages to clear
;;;        hl - start address for clearing
;;;   out: a - constant 0
;;;        b - constant 0
;;;        c - constant 0
;;;        hl - points at the end of the memory
;;;  mess: -

bzero:
	xor a
	ld b, a
.bzero_loop:
	ld (hl), a
	inc hl
	djnz .bzero_loop
	dec c
	jr nz, .bzero_loop
	ret


;;; rand: pseudo-random number generator
;;; This is the Galois LFSR (taps 16, 14, 13, 11), should
;;; have a period of 2^16-1 from any non-zero state.
;;;    in: b - number of bits to extract (up to 8)
;;;   out: a - b bits rotated in from the right
;;;        b - constant 0
;;;  mess: c, d, e

rand:
	ld de, 0xace1		; de <- current state
.rand_loop:
	rrc e
	rla			; a <- rotate in output bit
	rlc e			; de <- current state (restored)
	or a			; carry flag <- 0
	bit 0, a		; zero flag <- output bit
	jr z, .skipxor
	scf			; carry flag <- 1 (output bit)
	ld c, a
	ld a, d
	rra			; a <- new state sans xor (high half)
	rr e			; e <- new state (low half)
	xor 0x34		; a <- new state (high half)
	ld d, a			; de <- new state
	ld a, c
	djnz .rand_loop
	jr .rand_done
.skipxor:
	rr d
	rr e			; de <- new state
	djnz .rand_loop
.rand_done:
	ld (rand+1), de		; update new state
	ret


;;; splash: handle all the splash screen mess
;;; This procedure probably messes all registers.

splash:
	;; copy frame 1 to RAM

	ld de, logo_size
	ld hl, logo1_data + 4
	call splash_copy

	;; write to LCD

	ld a, 0x41
	out (6), a			; map RAM page 1 at $8000..$bfff

	ld bc, logo_size
	ld de, $fc00
	ld hl, $9000
	ldir				; block copy

	;; save the six bytes after plot screen

	ld bc, 6
	ld de, splash_save
	ld hl, $ce00-6
	ldir

	;; copy frame 2 to RAM, write to plot screen

	ld de, logo_size
	ld hl, logo2_data + 4
	call splash_copy

	ld a, 0x41
	out (6), a

	ld bc, logo_size
	ld de, $ca00
	ld hl, $9000
	ldir

	;; set up the interrupt handler

	di

	exx
	ld bc, $3c00			; $3c           -> ($c0+$3c)*$100 = $fc00; normal LCD
	ld de, $0236			; $3c^$36 = $0a -> ($c0+$0a)*$100 = $ca00; alt. LCD
	ld hl, splash_loop_carry	; used to terminate the loop
	ld (hl), 0x37			; (hl) <- "scf"
	exx

	;; loop until keypress

	ld hl, splash_loop
	call splash_int_activate

	;; make sure we have normal LCD active, cleanup

	ld a, $3c
	out (0), a
	call _clrLCD

	ld bc, 6
	ld de, $ce00-6
	ld hl, splash_save
	ldir
	set graphdraw, (iy+graphflags)	; flag graph screen as corrupted

	ret


;;; splash_loop: user code loop ran during the splash

splash_loop:
	ld b, 1
	call rand			; clock the LFSR to seed RNG
splash_loop_carry:
	scf				; changed to "or a" when finished
	jr c, splash_loop
	ret


;;; splash_copy: copies image data from non-loaded part of the program
;;;    in: de - number of bytes to copy
;;;        hl - offset from start of program
;;;   out: data copied to $9000
;;;  mess: a, b, d, e, h, l

splash_copy:
	xor a
	ex de, hl			; ahl <- number of bytes
	call _set_mm_num_bytes		; length:
	ex de, hl

	push hl
	ld hl, _asapvar
	rst 20h				; OP1 <- program name
	rst 10h				; bde <- program data
	pop hl

	xor a
	add hl, de
	adc a, b			; ahl <- splash data
	call _set_abs_src_addr		; source: splash data

	ld a, 1
	ld hl, $1000			; destination: $9000
	call _set_abs_dest_addr

	call _mm_ldir

	ret


;;; splash_int_activate: insert the interrupt handler in place

splash_int_activate:
	push hl				; save user routine location

	;; fill the IM 2 pointer page ($8e00..$8eff) with $8f

	ld bc, 255
	ld de, $8e01
	ld hl, $8e00
	ld (hl), $8f
	ldir

	;; copy interrupt handler to $8f8f

	ld bc, splash_int_size
	ld de, $8f8f
	ld hl, splash_int
	ldir

	;; enable the handler

	ld a, $8e
	ld i, a

	im 2
	ei

	;; call user routine

	pop hl
	ld bc, .splash_int_user_done
	push bc				; return address
	jp (hl)
.splash_int_user_done:

	;; user routine finished, go back to IM 1

	im 1
	ret


;;; splash_int: greyscale interrupt handler

splash_int:
	ex af, af' ;'
	exx

	;; skip if LCD off (or maybe refreshing?)

	in a, (3)
	bit 1, a
	jr z, .splash_int_exit

	;; count d as (2, 1, 0, 2, 1, 0, ...); flip on 1 and 0

	ld a, d
	sub 1
	jr c, .splash_int_reset		; reset d and flip
	jr z, .splash_int_flip		; just flip
	ld d, a				; save counter
	jr .splash_int_exit		; don't flip this time

	;; page-flipping code

.splash_int_reset:
	ld a, 2
.splash_int_flip:
	ld d, a				; save counter
	ld a, b
	xor e
	ld b, a
	out (c), b

.splash_int_exit:

	;; check for keys to continue

	ld a, %1000001
	out (1), a
	nop
	nop
	in a, (1)
	xor 0xff
	and 0xfe			; mask out 'enter' and some others
	jr z, .splash_int_nokey

	ld (hl), 0xb7			; (hl) <- "or a"

.splash_int_nokey:

	;; return from the interrupt

	ex af, af' ;'
	exx
	ei
	reti

splash_int_size: equ $ - splash_int


;;; splash variables

splash_save:
	db 0, 0, 0, 0, 0, 0

splash_exit_row_1:
	;; You found kitten!
	db 25, 47, 53, 0, 38, 47, 53, 46, 36, 0, 43, 41, 52, 52, 37, 46, 31
splash_exit_row_2:
	;; Way to go, robot!
	db 23, 33, 57, 0, 52, 47, 0, 39, 47, 28, 0, 50, 47, 34, 47, 52, 31
splash_exit_row_3:
	;; press clear to quit
	db 48, 50, 37, 51, 51, 0, 35, 44, 37, 33, 50, 0, 52, 47, 0, 49, 53, 41, 52

;;; splash_exit: victory splash screen

splash_exit:
	;; display the celebration message

	call _clrLCD

	ld hl, splash_exit_row_1
	ld bc, $0801
	ld d, 17
	call .splash_putrow

	ld hl, splash_exit_row_2
	ld bc, $0803
	ld d, 17
	call .splash_putrow

	ld hl, splash_exit_row_3
	ld bc, $0d09
	ld d, 19
	call .splash_putrow

	ld de, victory_size
	ld hl, victory_data + 4
	call splash_copy		; copy victory.bin to $9000

	ld a, 0x41
	out (6), a			; map RAM page 1 at $8000..$bfff

	ld bc, victory_size
	ld de, $fc00 + 30*16
	ld hl, $9000
	ldir				; block copy

	;; set up the interrupt handler

	di

	exx
	ld bc, $3c00			; $3c           -> normal LCD
	ld de, $0200			; $3c^$00 = $3c -> normal LCD (no flip)
	ld hl, splash_exit_loop_carry	; used to terminate the loop
	ld (hl), 0x37			; (hl) <- "scf"
	exx

	;; loop until keypress

	ld hl, splash_exit_loop
	call splash_int_activate

	;; go to normal exit cleanup

	jp exit

.splash_putrow:
	ld a, (hl)
	push bc
	push hl
	call put_char
	pop hl
	pop bc
	inc hl
	inc b
	dec d
	jr nz, .splash_putrow
	ret


;;; splash_exit_loop: user code loop ran during the exit splash

splash_exit_loop:
	;; Animation:
	;; Y range: rows 34 .. 53
	;;  Robot: bytes 1 .. 5  >>>
	;; Kitten: bytes 9 .. 14 <<<
	ld bc, $1409			; b <- 20 (rows); c <- 9 (for adds)
	ld hl, $fc00+34*16+1		; hl <- first animated byte
.exit_animation:
	;; animate robot
	ld a, (hl)			; a <- byte 1 value
	ld d, h
	ld e, l				; de <- hl (for putting byte 1 back)
	srl a
	inc hl
	rr (hl)
	inc hl
	rr (hl)
	inc hl
	rr (hl)
	inc hl
	rr (hl)				; rotate bytes 2 .. 5
	jr nc, .exit_animation_no_carry_1
	or 0x80				; carry from byte 5, add to byte 1
.exit_animation_no_carry_1:
	ld (de), a			; put byte 1 back
	;; animate kitten
	ld a, l
	add c
	ld l, a				; hl <- byte 14 (no carry possible)
	ld a, (hl)			; a <- byte 14 value
	ld d, h
	ld e, l				; de <- hl (for putting byte 14 back)
	sla a
	dec hl
	rl (hl)
	dec hl
	rl (hl)
	dec hl
	rl (hl)
	dec hl
	rl (hl)
	dec hl
	rl (hl)				; rotate bytes 9 .. 13
	jr nc, .exit_animation_no_carry_2
	or 1				; carry from byte 9, add to byte 14
.exit_animation_no_carry_2:
	ld (de), a			; put byte 14 back
	dec hl
	ld a, l
	add c
	ld l, a
	ld a, h
	adc a, 0
	ld h, a				; hl <- next row, byte 1 (with carry)
	djnz .exit_animation
	;; wait a moment
	halt
	halt
	halt
	halt
splash_exit_loop_carry:
	scf				; changed to "or a" when finished
	jr c, splash_exit_loop
	ret


;;; ============
;;; Message data
;;; ============

LOAD_SIZE: equ $ - _asm_exec_ram + messages_count

messages_index:
messages_data: equ messages_index + messages_count
	incbin 'messages.bin'

;;; ==================
;;; Splash screen data
;;; ==================

	org $ - _asm_exec_ram ; we use these as offsets

logo_size: equ 1024

logo1_data:
	incbin 'logo-1.bin'
logo2_data:
	incbin 'logo-2.bin'

victory_size: equ 448

victory_data:
	incbin 'victory.bin'
