
evnt01 equ $
       ld a,0
       ld hl,vari
       cp (hl)
       jp nc,b00091
       ld bc,1799
       call random
       ld a,65
       ld c,a
       ld a,(varrnd)
       add a,c
       ld (varrnd),a
       ld a,0
       ld hl,vara
       cp (hl)
       jp nz,b00087
       ld a,1
       ld c,a
       ld a,(vari)
       sub c
       ld (vari),a
b00087 jp b00099
b00091 ld a,0
       ld (vari),a
b00099 ld a,(ix+8)
       ld (varo),a
       ld a,(ix+9)
       ld (varp),a
       ld a,200
       cp (ix+9)
       jp nc,b00173
       ld a,100
       cp (ix+8)
       jp nc,b00173
       ld a,1
       ld (scno),a
       call nwscr
       call scrr
       ld a,8
       ld (ix+9),a
       ret
b00173 ld a,(joyval)
       and 4
       jp z,b00199
       call jump
       ld a,30
       ld (sndtyp),a
b00199 ld a,(joyval)
       and 2
       jp z,b00282
       call cangr
       jp nz,b00282
       ld a,4
       ld (ix+6),a
       ld a,0
       ld hl,vara
       cp (hl)
       jp nz,b00273
       call animsp
       ld a,2
       cp (ix+7)
       jp nz,b00273
       ld a,9
       ld (sndtyp),a
b00273 inc (ix+9)
       inc (ix+9)
b00282 ld a,(joyval)
       and 1
       jp z,b00366
       call cangl
       jp nz,b00366
       ld a,10
       ld (ix+6),a
       ld a,0
       ld hl,vara
       cp (hl)
       jp nz,b00335
       call animsp
b00335 ld a,2
       cp (ix+7)
       jp nz,b00357
       ld a,9
       ld (sndtyp),a
b00357 dec (ix+9)
       dec (ix+9)
b00366 call cangd
       jp nz,b00380
       call ifall
b00380 ld b,DEADLY
       call tded
       cp b
       jp nz,b00577
       ld hl,(sndptr)
       ld de,41
       add hl,de
       call isnd
       ld hl,deadf
       ld (hl),h
       ld a,6
       ld (23693),a
       ld (23695),a
       ld a,20
       ld (dispx),a
       ld a,8
       ld (dispy),a
       ld a,(numlif)
       call disply
       ld a,2
       ld (23693),a
       ld (23695),a
       ld a,20
       ld (dispx),a
       ld a,1
       ld (dispy),a
       ld a,2
       call dmsg
       ld a,6
       ld (23693),a
       ld (23695),a
       ld a,20
       ld (dispx),a
       ld a,25
       ld (dispy),a
       call dscore
       ld a,2
       ld (23693),a
       ld (23695),a
       ld a,20
       ld (dispx),a
       ld a,19
       ld (dispy),a
       ld a,1
       call dmsg
b00577 jp grav