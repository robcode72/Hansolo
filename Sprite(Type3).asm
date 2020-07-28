
evnt04 equ $
       ld a,0
       cp (ix+11)
       jp nz,e00051
       call cangu
       jp nz,e00039
       dec (ix+8)
       dec (ix+8)
       jp e00047
e00039 ld a,1
       ld (ix+11),a
e00047 jp e00083
e00051 call cangd
       jp nz,e00074
       inc (ix+8)
       inc (ix+8)
       jp e00083
e00074 ld a,0
       ld (ix+11),a
e00083 ld a,0
       ld hl,vara
       cp (hl)
       jp nz,e00104
       call animsp
e00104 ld b,0
       call sktyp
       jp nc,e00280
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
e00280 ret