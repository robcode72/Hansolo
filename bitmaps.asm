;------------------------------------------------------------------------------
; This file is part of the ZX Spectrum libzx library by Sebastian Mihai, 2016
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;
; Aggregates all bitmaps and bitmap buffer (background, screen) routines
;
; There are duplicate routines targeting the background buffer and the screen
; buffer. The reason for duplication is performance.
; 
; +-------------------+
; |                   |
; | background buffer |-----
; |                   |     \         +--------------+
; +-------------------+      -------->|              |
;                                     | video memory |
; sprites drawn directly ------------>|              |
; to video ram, appearing             +--------------+
; in "front" of the background
;
; Copying a chunk of the background buffer to the video memory, followed 
; immediately by drawing a bitmap directly to the video memory achieves
; "movable" sprites that don't "erase" the background when they move.
;
;------------------------------------------------------------------------------

	;--------------------------------------------------------------------------
	; Constants and data used by all bitmap routines
	;--------------------------------------------------------------------------
	VISIBLE_VIDEO_MEMORY equ 16384	; start of the video ram
	VIDEO_SEGMENT_SIZE equ 2048		; size in bytes of a third of a screen
	VIDEO_SEGMENT_LINES equ 64		; horizontal lines in a segment
	VIDEO_ATTRIBUTES_SIZE equ 768	; size in bytes of the attributes area
	VIDEO_TOTAL_SIZE equ 3*VIDEO_SEGMENT_SIZE + VIDEO_ATTRIBUTES_SIZE
	VISIBLE_ATTRIBUTES_MEMORY equ VISIBLE_VIDEO_MEMORY + 3*VIDEO_SEGMENT_SIZE
	
	; used to look up video memory addresses in the background buffer
	video_memory_Y_lookup_background ds 192*2
						; reserve 2 bytes for each of the 192
						; possible screen Y values
	
	; used to look up video memory addresses in the video ram
	video_memory_Y_lookup_vram ds 192*2
						; reserve 2 bytes for each of the 192
						; possible screen Y values

	; the background buffer is used to keep sprites from "erasing" the space
	; behind them as they move
	backgroundVideoBuffer:
		videoBufferPixelData ds VIDEO_TOTAL_SIZE-VIDEO_ATTRIBUTES_SIZE
		videoBufferAttributeData ds VIDEO_ATTRIBUTES_SIZE, %00111000
		
	BUFFER_TO_SCREEN_OFFSET equ VISIBLE_VIDEO_MEMORY - backgroundVideoBuffer

	;--------------------------------------------------------------------------
	; Includes
	;--------------------------------------------------------------------------
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_vram.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_background.asm"
	
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_vram_or_masked.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_vram_or.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_vram_xor.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_vram_not_and.asm"
	
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_background_or.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_background_xor.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_background_not_and.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_background_or_masked.asm"
	
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_buffer.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_utilities.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\bitmaps\bitmaps_colours.asm"
