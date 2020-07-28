
evnt11 equ $
       ld a,0
       cp (ix+5)
       jp nz,l00035
       ld a,(varo)
       ld (ix+8),a
       ld a,(varp)
       ld (ix+9),a
l00035 ld a,2
       cp (ix+5)
       jp nz,l00068
       ld a,1
       ld c,a
       ld a,(vara)
       add a,c
       ld (vara),a
l00068 ret