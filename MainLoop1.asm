
evnt12 equ $
       ld a,1
       ld hl,vara
       cp (hl)
       jp nz,m00032
       ld a,0
       ld (vara),a
       jp m00052
m00032 ld a,1
       ld c,a
       ld a,(vara)
       add a,c
       ld (vara),a
m00052 ret