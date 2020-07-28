
evnt02 equ $
       ld a,0
       cp (ix+11)
       jp nz,c00110
       call cangl
       jp nz,c00097
       dec (ix+9)
       dec (ix+9)
       ld a,16
       ld c,a
       ld a,(ix+9)
       sub c
       ld (ix+9),a
       call cangd
       jp nz,c00072
       ld a,1
       ld (ix+11),a
c00072 ld a,16
       ld c,a
       ld a,(ix+9)
       add a,c
       ld (ix+9),a
       jp c00106
c00097 ld a,1
       ld (ix+11),a
c00106 jp c00200
c00110 call cangr
       jp nz,c00191
       inc (ix+9)
       inc (ix+9)
       ld a,16
       ld c,a
       ld a,(ix+9)
       add a,c
       ld (ix+9),a
       call cangd
       jp nz,c00167
       ld a,0
       ld (ix+11),a
c00167 ld a,16
       ld c,a
       ld a,(ix+9)
       sub c
       ld (ix+9),a
       jp c00200
c00191 ld a,0
       ld (ix+11),a
c00200 ld a,0
       cp (ix+11)
       jp nz,c00247
       ld a,0
       ld hl,vara
       cp (hl)
       jp nz,c00242
       ld a,3
       ld (ix+6),a
       call animsp
c00242 jp c00276
c00247 ld a,0
       ld hl,vara
       cp (hl)
       jp nz,c00276
       ld a,2
       ld (ix+6),a
       call animbk
c00276 ld b,0
       call sktyp
       jp nc,c00328
       ld a,0
       ld hl,vari
       cp (hl)
       jp nc,c00310
       jp c00328
c00310 ld a,25
       ld (vari),a
       ld hl,deadf
       ld (hl),h
c00328 ret