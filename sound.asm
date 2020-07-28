;------------------------------------------------------------------------------
; This file is part of the ZX Spectrum libzx library by Sebastian Mihai, 2016
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;
; Aggregates functionality from the:
;     - effects player
;     - music player
;
; While there a consumer can choose to play only sounds, or only music, the 
; sound manager defined here allows BOTH sound and music playback in the 
; following modes:
;     0. Independent music and effects
;            in this mode, the manager attempts to play both sounds and music
;            during every frame
;            - Advantages: both effects and music can be heard at the same time
;            - Disadvantages: high CPU usage
;     1. Music interrupted by effects
;            in this mode, whenever an effect has to play, music will not play
;            during the same frame, instead being delayed until after effects
;            queue becomes empty
;            - Advantages: lower CPU usage per frame
;            - Disadvantages: that the music stops and resumes after the effect
;                             stops is noticeable immediately
;     2. Silent music when effects play
;            in this mode, whenever an effect has to play, music will
;            continue to play muted (not actually outputting any sound)
;            - Advantages: lower CPU usage per frame
;            - Disadvantages: chunks of music will not be heard while an effect
;                             is playing
;
;------------------------------------------------------------------------------

	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\sound\sound_effects.asm"
	include "C:\Program Files (x86)\TommyGun.AGD\Projects\Monu\Monu\libzx\sound\sound_music.asm"

	SOUND_MODE_INDEPENDENT equ 0
	SOUND_MODE_INTERRUPTED_MUSIC equ 1
	SOUND_MODE_MUTED_MUSIC equ 2
	
	soundMode db SOUND_MODE_INDEPENDENT

	
initialize_sound_manager:
	call initialize_sound
	call initialize_music
	ret

	
; Sets the current mode, which determines 
; the behaviour of sound_manager_continue_playing
;
; Input:
; 		A - mode
sound_manager_set_mode:
	ld (soundMode), a
	ret
	
	
; Load the music queue with a provided buffer of sounds.
;
; Output:
;        BC - address of beginning of buffer (see above for format)
;        DE - number of buffer entries
sound_manager_music_load:
	call music_queue_load
	ret
	

; Rewinds music back to the beginning of the buffer
;
sound_manager_music_rewind:
	call music_queue_rewind
	ret

	
; Pauses music playback
;	
sound_manager_music_pause:
	call music_queue_pause
	ret

	
; Resumes music playback
;
sound_manager_music_resume:
	call music_queue_resume
	ret
	
	
; Adds a sound with the specified characteristics to the queue. 
;
; NOTE: Since to get DE's value we multiply by FREQUENCY, and since to get
;       HL's value we divide by FREQUENCY, to have two sounds of different
;       frequencies playing for the same amount of time, both DE and HL values
;       must change.
;
; Input:
;      DE - time slice duration (calculated as FREQUENCY * SECONDS)
;           (how much time the CPU is blocked playing this sound each frame)
;      BC - pitch (calculated as 437500 / FREQUENCY - 30.125)
;       A - number of frames during which the sound will be played
sound_manager_effects_enqueue:
	call sound_queue_add
	ret


; Clears all sounds in queue
;
sound_manager_effects_clear:
	call sound_queue_clear
	ret


; Locking the effects queue prevents further effects from being enqueued
;
sound_manager_effects_lock:
	call sound_queue_lock
	ret

	
; Unlocks effects queue
;
sound_manager_effects_unlock:
	call sound_queue_unlock
	ret
	

; Called every frame to play music and/or sound effects
;
sound_manager_continue_playing:
	ld a, (soundMode)
	cp SOUND_MODE_INDEPENDENT
	jp z, sound_manager_play_independent
	cp SOUND_MODE_INTERRUPTED_MUSIC
	jp z, sound_manager_play_interrupted_music

sound_manager_continue_playing_muted_music:
	; use the "muted" music playing routine, which doesn't actually
	; output any sound
	call sound_continue_playing		; A := whether a sound was played
	cp 1
	jp z, sound_manager_continue_playing_muted_music_mute; a sound played, so 
														 ; we play no music
	call music_continue_playing
	ret
sound_manager_continue_playing_muted_music_mute:
	call music_continue_playing_muted
	ret
	
sound_manager_play_interrupted_music:
	; only play music when there is no sound to play
	call sound_continue_playing		; A := whether a sound was played
	cp 1
	jp z, sound_manager_play_interrupted_music_no_music	; a sound played, so 
														; we play no music
	call music_continue_playing
sound_manager_play_interrupted_music_no_music:
	ret

sound_manager_play_independent:
	; always play both sounds and music
	call sound_continue_playing
	call music_continue_playing
	ret

