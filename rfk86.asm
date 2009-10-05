;;; rfk86.asm -*- mode: z80-asm -*-
;;; TI-86 robotfindskitten port

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
;;;   $8ffe .. $8fff (2 bytes)
;;;     a very temporary word if variables are needed
;;;   $9000 .. $977f (20*96 = 1920 bytes)
;;;     selected message texts; 3 rows of 32 bytes, wrapped


nko_bitmap:	equ $8100
screen_map:	equ $8100
robot_pos:	equ $8800
temp:		equ $8ffe
nko_text:	equ $9000


;;; Changing this might have all sorts of ramifications.
nko_count:	equ 20
;;; This should be changeable without problems, though.
robot_char:	equ 77
;;; And this one should really match with the font
font_chars:	equ 78


;;; ==========================
;;; Initialization and Cleanup
;;; ==========================

;;; start: initialization code

start:
	call _runindicoff
	call _clrLCD

	push ix
	push iy

	;; welcome screen

	call splash

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

	ld a, e
	add a, (messages_data - _asm_exec_ram + 4) & 0xff
	ld e, a
	ld a, d
	adc a, (messages_data - _asm_exec_ram + 4) >> 8
	ld d, a
	ld a, b
	adc a, 0
	ld b, a				; bde <- message data start

	;; load selected messages

load_messages:

	;; clear the nko_text area

	ld ix, messages_index

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

	;; copy a bytes from bde into the message space

	ld l, c
	xor a
	ld h, a				; ahl <- msg len
	call _set_mm_num_bytes

	call _ex_ahl_bde		; ahl <- message data pointer
	call _set_abs_src_addr
	call _ex_ahl_bde		; bde <- message data pointer

.text_pos: equ $+1
	ld hl, nko_text - $8000
	ld a, 1				; ahl <- dest pointer, always in page 1
	call _set_abs_dest_addr

	call _mm_ldir

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

	call _runindicon

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
	ret


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

	ld hl, logo1_data + 4
	call splash_copy

	;; write to LCD

	ld a, 0x41
	out (6), a			; map RAM page 1 at $8000..$bfff

	ld bc, logo_size
	ld de, $fc00
	ld hl, $9000
	ldir				; block copy

	;; copy frame 2 to RAM, write to plot screen

	ld hl, logo2_data + 4
	call splash_copy
	ld a, 0x41
	out (6), a

	ld bc, logo_size
	ld de, $fc00
	ld hl, $9000

	;; 3c -> ($c0+$3c)*$100 = $fc00
	;; 0a -> ($c0+$0a)*$100 = $ca00

	;; set up interrupt handler

.splash_loop:
	jr .splash_loop


;;; splash_copy: helper, copies frame from prog+hl to target

splash_copy:
	;; find the splash screen image

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

	xor a
	ld hl, logo_size
	call _set_mm_num_bytes		; length: frame size (1024 bytes)

	call _mm_ldir

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
