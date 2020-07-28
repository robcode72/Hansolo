
evnt05 equ $
       ld a,0
       cp (ix+11)
       jp nz,f00110
       call cangl
       jp nz,f00097
       dec (ix+9)
       dec (ix+9)
       ld a,16
       ld c,a
       ld a,(ix+9)
       sub c
       ld (ix+9),a
       call cangd
       jp nz,f00072
       ld a,1
       ld (ix+11),a
f00072 ld a,16
       ld c,a
       ld a,(ix+9)
       add a,c
       ld (ix+9),a
       jp f00106
f00097 ld a,1
       ld (ix+11),a
f00106 jp f00200
f00110 call cangr
       jp nz,f00191
       inc (ix+9)
       inc (ix+9)
       ld a,16
       ld c,a
       ld a,(ix+9)
       add a,c
       ld (ix+9),a
       call cangd
       jp nz,f00167
       ld a,0
       ld (ix+11),a
f00167 ld a,16
       ld c,a
       ld a,(ix+9)
       sub c
       ld (ix+9),a
       jp f00200
f00191 ld a,0
       ld (ix+11),a
f00200 ld a,0
       cp (ix+11)
       jp nz,f00247
       ld a,0
       ld hl,vara
       cp (hl)
       jp nz,f00242
       ld a,1
       ld (ix+6),a
       call animsp
f00242 jp f00276
f00247 ld a,0
       ld hl,vara
       cp (hl)
       jp nz,f00276
       ld a,0
       ld (ix+6),a
       call animbk
f00276 ld b,0
       call sktyp
       jp nc,f00328
       ld a,0
       ld hl,vari
       cp (hl)
       jp nc,f00310
       jp f00328
f00310 ld a,25
       ld (vari),a
       ld hl,deadf
       ld (hl),h
f00328 ret