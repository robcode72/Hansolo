;------------------------------------------------------------------------------
; This file is part of the ZX Spectrum libzx library by Sebastian Mihai, 2016
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;
; This is the main include file of the libzx library.
;
; Aggregates all library functionality. This is a collection of routines 
; which are not specific to a game.
; Roughly speaking, they provide functionality one level above the hardware, 
; abstracting certain ZX Spectrum-specific details.
;
;------------------------------------------------------------------------------

	libzxAuthor db "libzx is written by Sebastian Mihai, 2016"
	
	include "bitmaps.asm"
	include "font.asm"
	include "text.asm"
	include "keyboard.asm"
	include "random.asm"
	include "utilities.asm"
	include "sound.asm"

initialize_libzx:
	call initialize_bitmaps
	call initialize_bitmaps_vram
	call initialize_custom_font
	call initialize_keyboard
	
	call initialize_sound_manager
	
	ld a, SOUND_MODE_INDEPENDENT
	call sound_manager_set_mode
	
	ret

