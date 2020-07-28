;------------------------------------------------------------------------------
; This file is part of the ZX Spectrum libzx library by Sebastian Mihai, 2016
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;
; Various utility routines
;
;------------------------------------------------------------------------------

	COLOUR_BLACK equ 0
	COLOUR_BLUE equ 1
	COLOUR_RED equ 2
	COLOUR_MAGENTA equ 3
	COLOUR_GREEN equ 4
	COLOUR_CYAN equ 5
	COLOUR_YELLOW equ 6
	COLOUR_WHITE equ 7


; Draw a tiled rectangular region
; 		C - screen Y coordinate
;	  IXL - screen X coordinate
;      HL - pointer to 8 bytes sprite data
;       D - number of tiles horizontally
;       E - number of tiles vertically
repeat_tile_background:
repeat_tile_background_vertical:
	push bc
	push ix
	push de
	
repeat_tile_background_horizontal:
	push de
	push bc
	push hl
	
	ld a, 8
	call draw_bitmap_OR_blitted_background
	
	pop hl
	pop bc
	pop de
	
	ld a, ixl
	add a, 8
	ld ixl, a			; next column
	
	dec d
	ld a, d
	cp 0
	jp nz, repeat_tile_background_horizontal
	; END INNER LOOP
	
	pop de
	pop ix
	pop bc
	
	ld a, c
	add a, 8
	ld c, a				; next row
	
	dec e
	ld a, e
	cp 0
	jp nz, repeat_tile_background_vertical
	
	ret


; This is the address of a ROM routine, which is invoked normally, via call
;
; Prints a string at the specified location
;
; Input:
; 		A - colour number
ROM_ROUTINE_SET_BORDER_COLOUR equ 8859
