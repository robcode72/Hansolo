;------------------------------------------------------------------------------
; This file is part of the ZX Spectrum libzx library by Sebastian Mihai, 2016
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;
; Random number routines
;
;------------------------------------------------------------------------------

	lastRandomNumber db 33
	romPointer dw 3		; our random numbers are, in part, based on reading
						; bytes from the 16kb ROM, whose contents are "pretty
						; random"

; Gets an 8-bit random number. 
; It is computed using a combination of:
;     - the last returned random number
;     - a byte from ROM, in increasing order
;     - current values of various registers
;     - a flat incremented value
;
; Output:
; 		A - next random number
get_next_random:
	push af
	
	push hl
	; advance ROM pointer
	ld hl, romPointer
	ld c, (hl)
	inc hl
	ld b, (hl)				; BC := word (romPointer)
	ld hl, 3
	add hl, bc				; HL := ROM pointer advanced by 3
	ld a, h
	and %00111111
	ld h, a					; H := H mod %00111111
							; essentially, HL := HL mod 16384, to make sure
							; HL points at a ROM location
	ld (romPointer), hl		; save new location
	pop hl
	
	; now compute the random number
	
	pop bc					; BC := AF
	rlc c
	rlc b
	ld a, (lastRandomNumber)
	add a, b				; current register values are "pretty random"
	add a, c				; so add them in the mix
	add a, 47
	add a, d
	add a, e
	add a, h
	add a, l
	
	ld hl, romPointer
	add a, (hl)				; the contents of the ROM are "pretty random"
							; so add it in the mix
	
	ld hl, lastRandomNumber
	ld (hl), a				; save this number
	
	ret

