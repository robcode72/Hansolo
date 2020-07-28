;------------------------------------------------------------------------------
; This file is part of the libzx examples suite
;                      by Sebastian Mihai, 2016, http://sebastianmihai.com
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Example 5: demonstrate a nice colour cycling effect
;------------------------------------------------------------------------------

efect1:
;main: org 33000					; stay above ULA-contended memory
	jp start					; jump to the beginning of our program
	
	include "libzx\libzx.asm"	; include the libzx library

	stack ds 512				; reserve some bytes for our stack
	endstack:					; mark end of our stack
	
	message db "COLOUR CYCLING ROUTINE", 0
	frameCounter db 0
	
start:
	ld sp, endstack				; set up our new stack (we point to the end 
								; because the stack grows into lower addresses)
	call initialize_libzx		; initialize the libzx library
	
	; this is where the example begins
	
	; clear screen
	ld a, FLASH_OFF | BRIGHT_ON | PAPER_BLACK | INK_WHITE
	call clear_vram_to_attribute
	
	ld hl, message				; HL := address of message
	ld b, 10					; row
	ld c, 5						; column
	call text_print_at			; call routine
	
cycle_colours:
	halt						; synchronize with video interrupt
	
	; rotate colours of the title by setting the ink to values between
	; 0 and 7 inclusive
	ld a, (frameCounter)
	srl a
	srl a
	and %00000111		; A := (frameCounter/4) mod 8
	or FLASH_OFF | PAPER_BLACK	; "add in" the other attributes
	ld d, a

	ld a, (frameCounter)
	srl a
	
	; comment out these two lines to not go through BRIGHT ON colours too
	and %00000001		; every two frames change brightness
	xor %00000001		; flip value to cycle dark->bright
	
	sla a
	sla a				; in essence, we're flipping through this sequence
	sla a				; blue -> bright blue -> red -> bright red -> etc.
	sla a				; by switching the colour every 4 frames
	sla a				; and the brightness from 0 to 1 every 2 frames
	sla a
	or d
	
	ld d, a				; D := attribute value
	ld b, 10			; row
	ld c, 5				; column
	ld e, 22
	call bitmaps_colours_set_many
	
	; increment counter
	ld hl, frameCounter
	inc (hl)
	
	jp cycle_colours
	

;end main	; this is needed so the Pasmo assembler generates an
			; "auto-running" BASIC header for us
			;
			; the only way on the ZX Spectrum to run a program automatically
			; after loading it (that is, without requiring the user to issue
			; a "RUN" command manually), is via a loader written in BASIC
			; which simply loads the main program block (written in assembler)
			; after which it jumps into its beginning
