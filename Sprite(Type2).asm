
evnt03 equ $
       ld a,0
       cp (ix+11)
       jp nz,d00110
       call cangl
       jp nz,d00097
       dec (ix+9)
       dec (ix+9)
       ld a,16
       ld c,a
       ld a,(ix+9)
       sub c
       ld (ix+9),a
       call cangd
       jp nz,d00072
       ld a,1
       ld (ix+11),a
d00072 ld a,16
       ld c,a
       ld a,(ix+9)
       add a,c
       ld (ix+9),a
       jp d00106
d00097 ld a,1
       ld (ix+11),a
d00106 jp d00200
d00110 call cangr
       jp nz,d00191
       inc (ix+9)
       inc (ix+9)
       ld a,16
       ld c,a
       ld a,(ix+9)
       add a,c
       ld (ix+9),a
       call cangd
       jp nz,d00167
       ld a,0
       ld (ix+11),a
d00167 ld a,16
       ld c,a
       ld a,(ix+9)
       sub c
       ld (ix+9),a
       jp d00200
d00191 ld a,0
       ld (ix+11),a
d00200 ld a,0
       cp (ix+11)
       jp nz,d00247
       ld a,0
       ld hl,vara
       cp (hl)
       jp nz,d00242
       ld a,1
       ld (ix+6),a
       call animsp
d00242 jp d00276
d00247 ld a,0
       ld hl,vara
       cp (hl)
       jp nz,d00276
       ld a,0
       ld (ix+6),a
       call animbk
d00276 ld b,0
       call sktyp
       jp nc,d00328
       ld a,0
       ld hl,vari
       cp (hl)
       jp nc,d00310
       jp d00328
d00310 ld a,25
       ld (vari),a
       ld hl,deadf
       ld (hl),h
d00328 ld a,9
       ld (ix+6),a
       ld b,0
       call sktyp
       jp nc,d00462
       ld (ix+5),255
       ld a,1
       ld c,a
       ld a,(vara)
       sub c
       ld (vara),a
       ld a,0
       ld hl,vara
       cp (hl)
       jp nz,d00400
       ld hl,nexlev
       ld (hl),h
d00400 ld a,100
       call addsc
       ld hl,(sndptr)
       ld de,41
       add hl,de
       call isnd
       ld a,6
       ld (23693),a
       ld (23695),a
       ld a,20
       ld (dispx),a
       ld a,25
       ld (dispy),a
       call dscore
d00462 ret