;1k MazezaM - an implementation of my ZX Spectrum game for the ZX81.
;Copyright (C) 2010, Malcolm Tyrrell.
;Use and distribute under the terms of the GPL v3.0 or later.

;NOTES:
;The binary produced by assembling this code constitutes a ready-made .p
;file. Having the header as an editable part of the assembly meant I could 
;set system variables and use some spare buffers for code.

;I use "call ROM_CLS", "call ROM_DECODE" and "rst 10H", so there has to
;be enough room for their stack.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CHARACTER CODES

DEFC CHR_SPACE		= 0x00
DEFC CHR_BLOCK		= 0x80
DEFC CHR_NEW_LINE	= 0x76
DEFC CHR_PERSON		= 0x34
DEFC CHR_EVEN_ROW	= 0x08
DEFC CHR_ODD_ROW	= 0x88
DEFC CHR_DOOR		= 0x2e

DEFC CHR_RETRY		= 0x37		;R
DEFC CHR_NEXT		= 0x38		;S

DEFC CHR_LEFT		= 0x34		;Z
DEFC CHR_RIGHT		= 0x35		;X
DEFC CHR_UP			= 0x36		;K
DEFC CHR_DOWN		= 0x26		;M
DEFC CHR_LEFT_ALT	= 0x21		;5
DEFC CHR_RIGHT_ALT	= 0x24		;8
DEFC CHR_UP_ALT		= 0x23		;7
DEFC CHR_DOWN_ALT	= 0x22		;6

DEFC CHR_END_OF_PRINT	= 0x43		;Unused character for delimeter

;; ROM ROUTINES

DEFC ROM_CLS		= 0x0a2a 	;Clears the screen
DEFC ROM_DECODE 	= 0x07bd	;Converts keycode in BC to char in (HL)

;RAMTOP doesn't get saved, so I have to set it manually at the beginning
;of the program. Setting VAR_RAMTOP to RAMTOP ensures 16K compatibility.
DEFC VAR_RAMTOP		= 0x4004
DEFC RAMTOP			= 0x4400

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYSTEM VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Several of these are exploited for data storage

;The address at which loading starts.
org 4009H

VAR_VERSN:		defb 0x00 
VAR_E_PPC:		defw 0x001e
VAR_D_FILE:		defw D_FILE
VAR_DF_CC:		defw DF_CC
VAR_VARS:		defw VARS
VAR_DEST:		defw 0x0000
VAR_E_LINE:		defw E_LINE
VAR_CH_ADD:		defw basicLine10 - 1
VAR_X_PTR:		defw 0xC000
VAR_STKBOT:		defw E_LINE
VAR_STKEND:		defw E_LINE
VAR_BERG:		defb 0x00
VAR_MEM:		defw 0x405d
VAR_NOT_USED1:	defb 0x00
VAR_DF_SZ:		defb 0x02
VAR_S_TOP:		defw 0x0000
VAR_LAST_K:		defw 0xffff
VAR_DEBOUNCE:	defb 0xff
VAR_MARGIN:		defb 0x37
VAR_NXTLIN:		defw basicLine10
VAR_OLDPPC:		defw 0x0000
VAR_FLAGX:		defb 0x00
VAR_STRLEN:		defw 0x0000
VAR_T_ADDR:		defw 0x0c8d
VAR_SEED:		defw 0x0000
VAR_FRAMES:		defw 0xf97d
VAR_COORDS_X:	defb 0x00
VAR_COORDS_Y:	defb 0x00
VAR_PR_CC:		defb 0xbc
VAR_S_POSN_COL:	defb 0x21
VAR_S_POSN_LINE:defb 0x18
VAR_CDFLAG:		defb 0x40

;I use the 65 bytes in the following buffers for some subroutines and
;some data.

;VAR_PRBUFF:
;	defb 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
;	defb 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
;	defb 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
;	defb 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
;	defb 0xff
;VAR_MEMBOT:
;	defb 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
;	defb 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
;	defb 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
;	defb 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
;
;VAR_NOT_USED2:		 defw 0xff, 0xff 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SUB-ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Get the level dimensions in D (height) and E (width).
;Sets A to zero
;Sets HL to the levelPtr
getDim:
levelAddressMinus1:
	ld HL, levels			;The word gets overwritten
	xor A				;zero A
	rld
	ld D, A				;height in D
	rld
	ld E, A				;width in E
	rld				;Restore (HL)
	ret

;Given that H is Y, L is X and E is width,
;make HL point at the (X,Y) position in the display file
;make A be the character at the position
;set Z if the character is a space
pointToDFilePosition:
	;calculate the number of characters to skip
	ld B, H				;put Y in B
	ld A, L				;add X
	inc A
addRow:
	add E				;add a row (E is width)
	add 0x03				;two walls and a newline
	djnz addRow			;loop for each Y
	ld HL, (VAR_D_FILE)
	ld C, A				;now BC = A since B is 0
	add HL, BC
	ld A, (HL)			;Let A be the character at HL
	or A				;Test for a space
	ret

;Draw a row of blocks suitable for the top and bottom of the level.
drawBlockRow:
	ld B, E
	inc B
	inc B
	ld A, CHR_BLOCK
drawTopBlock:
	rst 0x10
	djnz drawTopBlock

	ld A, CHR_NEW_LINE
	rst 0x10
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

youEscaped:
	defb 0x3e, 0x34, 0x3a, 0x00, 0x2a, 0x38, 0x28, 0x26, 0x35, 0x2a, 0x29
	defb CHR_NEW_LINE
	defb CHR_END_OF_PRINT
	
wellDone:
	defb 0x00
	defb 0x3c, 0x2a, 0x31, 0x31, 0x00, 0x29, 0x34, 0x33, 0x2a
	defb CHR_END_OF_PRINT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC PROGRAM LINE 10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

basicLine10:
	defb 0x00, 0x00
	defw (usrLineEnd - usrLineStart)
usrLineStart:
	defb 0xf5, 0xd4
	defb 0x1c
	defb 126, 143, 1, 26, 0, 0
	defb 0x76
usrLineEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASSEMBLY PROGRAM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ENSURE 16K COMPATIBILITY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

initialization:
	ld HL, RAMTOP
	ld (VAR_RAMTOP), HL

;; WAIT FOR A KEY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

startKey:
	call getKey

;; GET DIMENSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

resetLevel:
	call ROM_CLS			;Clear the display
	call getDim			;Height in D, width in E
	inc HL				;HL = start and finish byte
	rld
	ld (startAddressMinus1 + 1), A
	ld B, A				;Let B = start
	rld
	ld (finishAddressMinus1 + 1), A
	rld				;Restore (HL)
	ld (coordsAddressMinus1 + 1), A	;Let X = 0
	ld A, D
	inc A
	sub B
	ld (coordsAddressMinus1 + 2), A	;Let Y = height + 1 - start

	xor A
	or D				;Check for height == 0 (completion)
	jr nz, drawLevel

;; COMPLETION SCREEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ld HL, youEscaped
	call print			;Print "YOU ESCAPED"
	call print			;Print "WELL DONE"
	ld HL, levels			;Reset the levelPtr to level 0
	ld (levelAddressMinus1 + 1), HL
	call getKey
	jr resetLevel

;; DRAW THE LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

drawLevel:
	call drawBlockRow
	ld C, 0x01

;; DRAW THE LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ASSUMES:
; * height in D
; * width in E
; * start in (startAddressMinus1 + 1)
; * finish in (finishAddressMinus1 + 1)
; * Byte and bit prior to start of level bits in HL and C

heightLoop:
	; Check for start
	ld A, D
startAddressMinus1:
	cp 0x00				;The byte gets overwritten
	ld A, CHR_BLOCK
	jr nz, notStart
	ld A, CHR_PERSON
notStart:
	rst 0x10
	
	ld B, E				;let B = width
widthLoop:
	srl C				;shift the bit
	jr nc, dontShift		;do I need to shift a byte?
	inc HL				;shift the byte
	ld C, 0x80

dontShift:
	ld A, (HL)
	and A, C			;Block or space?
	ld A, CHR_SPACE
	jr z, drawSpaceOrBlock
	ld A, CHR_EVEN_ROW
	bit 0, D			;is D odd?
	jr z, drawSpaceOrBlock
	ld A, CHR_ODD_ROW
drawSpaceOrBlock:
	rst 0x10
	djnz widthLoop			;loop if B > 0 

	; Check for finish
	ld A, D
finishAddressMinus1:
	cp 0x00				;the byte gets overwritten
	ld A, CHR_BLOCK
	jr nz, notFinish
	ld A, CHR_SPACE
notFinish:
	rst 0x10
	ld A, CHR_NEW_LINE
	rst 0x10

	dec D
	jr nz, heightLoop		;loop if D > 0

	call drawBlockRow

	inc HL
	ld (nextLevelAddressMinus1 + 1), HL	;store the address of the next level

;; GET KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; Get a key
waitForKey:
	call getKey
	ld B, A				;store the key in B
	call getDim
	ld A, B				;restore the key from B

;; RETRY LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cp CHR_RETRY
resetLevel2:
	jp z, resetLevel

;; ADVANCE TO NEXT LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cp CHR_NEXT
	jr nz, keyIsntNext

goToNextLevel:
nextLevelAddressMinus1:
	ld HL, 0x0000			;The word gets overwritten
	ld (levelAddressMinus1 + 1), HL
	jr resetLevel2

keyIsntNext:
coordsAddressMinus1:
	ld HL, 0x0000			;put Y in H and X in L (overwritten)

;; GO LEFT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cp CHR_LEFT
	jr z, keyIsLeft
	cp CHR_LEFT_ALT
	jr nz, keyIsntLeft

keyIsLeft:
	dec L				;Let L = X - 1
	call pointToDFilePosition
	jr nz, somethingToTheLeft
	ld (HL), CHR_PERSON
	inc HL
	ld (HL), CHR_SPACE
	jr movePersonToLeft

somethingToTheLeft:
	cp CHR_NEW_LINE
	jp z, waitForKey
	ld HL, (coordsAddressMinus1 + 1)	;put Y in H and X in L
	ld L, 1				;consider the row's last position
	call pointToDFilePosition
	jp nz, pushBlocked		;check for a space
	ld B, A				;let B = 0
	ld C, E				;let C = width - 1
	dec C
	ld D, H				;let DE = HL
	ld E, L
	inc HL
	ldir				;shift the row
	dec HL				;blank the row's last position
	ld (HL), CHR_SPACE

movePersonToLeft:
	ld HL, coordsAddressMinus1 + 1
	dec (HL)			;x = x - 1
	xor A				;zero A to skip rest of key handling

keyIsntLeft:

;; GO RIGHT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cp CHR_RIGHT
	jr z, keyIsRight
	cp CHR_RIGHT_ALT
	jr nz, keyIsntRight

keyIsRight:
	inc L
	call pointToDFilePosition
	jr nz, somethingToTheRight
	ld (HL), CHR_PERSON
	dec HL
	ld (HL), CHR_SPACE
	ld A, (coordsAddressMinus1 + 1)	;check for X = 0
	or A
	jr nz, movePersonToRight
	ld (HL), CHR_DOOR
	jr movePersonToRight

somethingToTheRight:
	cp CHR_NEW_LINE			;check for the exit
	jr nz, somethingInWay
	jr goToNextLevel

somethingInWay:
	ld HL, (coordsAddressMinus1 + 1)	;put Y in H and X in L
	ld L, E				;consider the row's last position
	call pointToDFilePosition
	jr nz, pushBlocked		;check for a space
	ld B, A				;let B = 0
	ld C, E				;let C = width - 1
	dec C
	ld D, H				;let DE = HL
	ld E, L
	dec HL
	lddr				;shift the row
	inc HL				;blank the row's first position
	ld (HL), CHR_SPACE

movePersonToRight:
	ld HL, coordsAddressMinus1 + 1
	inc (HL)			;x = x + 1
	xor A				;zero A to skip rest of key handling

keyIsntRight:

;; GO UP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cp CHR_UP
	jr z, keyIsUp
	cp CHR_UP_ALT
	jr nz, keyIsntUp

keyIsUp:
	dec H
	call pointToDFilePosition
	jr nz, somethingAbove
	call upAndDownCode
	add HL, BC
	ld (HL), CHR_SPACE
	ld HL, coordsAddressMinus1 + 2
	dec (HL)			;y = y - 1
	xor A				;zero A to skip rest of key handling

somethingAbove:
keyIsntUp:

;; GO DOWN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cp CHR_DOWN
	jr z, keyIsDown
	cp CHR_DOWN_ALT
	jr nz, keyIsntDown

keyIsDown:
	inc H
	call pointToDFilePosition
	jr nz, somethingBelow
	call upAndDownCode
	sbc HL, BC
	ld (HL), CHR_SPACE
	ld HL, coordsAddressMinus1 + 2
	inc (HL)			;y = y + 1
	
keyIsntDown:
somethingBelow:
pushBlocked:
	jp waitForKey

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MORE SUB-ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Get a key in A.
getKey:
	ld BC, (VAR_LAST_K)
	ld A, C				;If no key use A = C = ffH
	cp 0xff				;Check for no key pressed
	jr z, checkForNewKey		;Don't decode if it's a key up
	call ROM_DECODE			;Convert the key code to a char
	ld A, (HL)
checkForNewKey:
oldKeyAddressMinus1:
	ld D, 0x00			;The byte stores the old key.
	ld (oldKeyAddressMinus1 + 1), A
	cp D				;Does the new key = old key?
	jr z, getKey
	cp 0xff				;Does the new key = no key?
	jr z, getKey
	ret

;Print 7 new-lines, 11 spaces, and then print the characters at HL,
;stopping when you hit CHR_END_OF_PRINT.

print:
	ld A, CHR_NEW_LINE
	ld B, 0x07
printNewLine:
	rst 0x10
	djnz printNewLine
	xor A				;A is space
	ld B, 0x0a
printSpace:
	rst 0x10
	djnz printSpace
printString:
	ld A, (HL)
	inc HL
	cp CHR_END_OF_PRINT
	ret z
	rst 10H
	jr printString

;Some common code to save 1 byte.
upAndDownCode:
	ld (HL), CHR_PERSON
	ld B, 0x00
	ld A, 0x03
	add A, E
	ld C, A
	ret

	;nop
	;nop
	;nop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEVEL DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

levels:
INCLUDE "levels.asm"

levelEnd:
	defb 0x01

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISPLAY FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

D_FILE:
	defb 0x76

;; WELCOME SCREEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This gets overwritten.
DF_CC:
	defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	defb 0x96, 0x80, 0x9d, 0xb0, 0x80, 0xb2, 0xa6
	defb 0xbf, 0xaa, 0xbf, 0xa6, 0xb2, 0x80, 0x96
	defb 0x76
	defb 0x76
	defb 0x76
	defb 00, 0x00, 0x00, 0x00
	defb 0x10, 0x28, 0x11, 0x00, 0x1e, 0x1c, 0x1d, 0x1c, 0x00
	defb 0x32, 0x26, 0x31, 0x28, 0x34, 0x31, 0x32, 0x00
	defb 0x39, 0x3e, 0x37, 0x37, 0x2a, 0x31, 0x31
	defb 0x76
	defb 0x76
	defb 0x76
	; Goal
	defb 0x96, 0xac, 0xb4, 0xa6, 0xb1, 0x96
	defb 0x76
	defb 0x76
	; Push the rows left and right to
	defb 0x35, 0x3a, 0x38, 0x2d, 0x00, 0x39, 0x2d, 0x2a
	defb 0x00, 0x37, 0x34, 0x3c, 0x38, 0x00, 0x31, 0x2a
	defb 0x2b, 0x39, 0x00, 0x26, 0x33, 0x29, 0x00, 0x37
	defb 0x2e, 0x2c, 0x2d, 0x39, 0x00, 0x39, 0x34
	defb 0x76
	; escape.
	defb 0x2a, 0x38, 0x28, 0x26, 0x35, 0x2a, 0x1b
	defb 0x76
	defb 0x76
	defb 0x76
	; keys
	defb 0x96, 0xb0, 0xaa, 0xbe, 0xb8, 0x96
	defb 0x76
	defb 0x76
	defb 0x3a, 0x35, 0x0e, 0x00, 0x00, 0x00, 0x00, CHR_UP
	defb 0x18, CHR_UP_ALT
	defb 0x76
	defb 0x29, 0x34, 0x3c, 0x33, 0x0e, 0x00, 0x00, CHR_DOWN
	defb 0x18, CHR_DOWN_ALT
	defb 0x76
	defb 0x31, 0x2a, 0x2b, 0x39, 0x0e, 0x00, 0x00, CHR_LEFT
	defb 0x18, CHR_LEFT_ALT
	defb 0x76
	defb 0x37, 0x2e, 0x2c, 0x2d, 0x39, 0x0e, 0x00, CHR_RIGHT
	defb 0x18, CHR_RIGHT_ALT
	defb 0x76
	defb 0x37, 0x2a, 0x39, 0x37, 0x3e, 0x0e, 0x00, CHR_RETRY
	defb 0x76
	defb 0x38, 0x30, 0x2e, 0x35, 0x0e, 0x00, 0x00, CHR_NEXT
	defb 0x76
	defb 0x76
	defb 0x76
	;press a key
	defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	defb 0x35, 0x37, 0x2a, 0x38, 0x38, 0x00, 0x26, 0x33, 0x3e, 0x00
	defb 0x30, 0x2a, 0x3e
	;to start
	;defb 0x00, 0x39, 0x34, 0x00, 0x38, 0x39, 0x26, 0x37, 0x39
	defb 0x76
	defb 0x76

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARS AND BYTE AT E_LINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VARS:
	defb 0x80

E_LINE:
	defb 0x0b

