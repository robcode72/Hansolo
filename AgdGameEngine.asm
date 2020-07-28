;
; -- Arcade Game Designer Engine --------------------------------------------------------------
;

; Global definitions.

MAP    equ 64768           ; properties map buffer.
loopc  equ 23681           ; loop counter system variable.

; Block characteristics.

PLATFM equ 1               ; platform.
WALL   equ PLATFM + 1      ; solid wall.
LADDER equ WALL + 1        ; ladder.
FODDER equ LADDER + 1      ; fodder block.
DEADLY equ FODDER + 1      ; deadly block.
CUSTOM equ DEADLY + 1      ; custom block.
NUMTYP equ CUSTOM + 1      ; number of types.

; Sprites.

NUMSPR equ 12              ; number of sprites.
TABSIZ equ 15              ; size of each entry.
SPRBUF equ NUMSPR * TABSIZ ; size of entire table.
NMESIZ equ 5               ; bytes stored in nmetab for each sprite.
X      equ 8               ; new x coordinate of sprite.
Y      equ X + 1           ; new y coordinate of sprite.
PAM1ST equ 5               ; first sprite parameter, old x (ix+5).
COLDST equ 15              ; maximum collision distance

; Sound.

SNDSIZ equ 10              ; elements to sound.


; Game starts here.  No reason why screen data couldn't go between start and contrl to put them in
; contended RAM, leaving the code and rest of the game in uncontended memory at 32768 and beyond.

START_ADDR equ 31232       ; set the start address of the game
start  org START_ADDR
AAA_START defw $           ; matched to AAA_END we can work out how much memory has being used
;start   equ $

; If a font is required...
;       ld hl,font-256      ; address of font.
;       ld (23606),hl       ; set up game font.

       jp game             ; start the game.

contrl defb 1              ; control, 1 = keyboard, 0 = Kempston.
joyval defb 0              ; joystick reading.
obno   defb 0              ; object no.
frmno  defb 0              ; selected frame.
chno   defb 0              ; char number.
sprno  defb 0              ; presently selected sprite.

; Don't change the order of these.  Menu routine relies on winlft following wintop.

wintop defb 1              ; window top
winlft defb 1              ; window left
winhgt defb 22             ; window height
winwid defb 22             ; window width
numspr defb 4              ; number of sprites.
numch  defb 5              ; number of chars.
numsc  defb 2              ; number of screens.
totfrm defb 11             ; total sprite frames.
nummsg defb 4              ; number of messages.
numsnd defb 3              ; number of sounds.
sndno  defb 0              ; sound number.
sndsel defw fx1            ; selected sound.
msgno  defb 0              ; message number.
numob  defb 1              ; number of objects in game.
edget  defb 0              ; screen edge.
edgeb  defb 0              ; screen edge.
edgel  defb 0              ; screen edge.
edger  defb 0              ; screen edge.
scno   defb 0              ; present screen number.
numlif defb 3              ; number of lives.
vara   defb 0              ; general-purpose variable.
varb   defb 0              ; general-purpose variable.
varc   defb 0              ; general-purpose variable.
vard   defb 0              ; general-purpose variable.
vare   defb 0              ; general-purpose variable.
varf   defb 0              ; general-purpose variable.
varg   defb 0              ; general-purpose variable.
varh   defb 0              ; general-purpose variable.
vari   defb 0              ; general-purpose variable.
varj   defb 0              ; general-purpose variable.
vark   defb 0              ; general-purpose variable.
varl   defb 0              ; general-purpose variable.
varm   defb 0              ; general-purpose variable.
varn   defb 0              ; general-purpose variable.
varo   defb 0              ; general-purpose variable.
varp   defb 0              ; general-purpose variable.
dispx  defb 0              ; cursor x position.
dispy  defb 0              ; cursor y position.
varrnd defb 255            ; last random number.
varobj defb 254            ; last object number.
nexlev defb 0              ; next level flag.
restfl defb 0              ; restart screen flag.
deadf  defb 0              ; dead flag.
gamwon defb 0              ; game won flag.

; Make sure pointers are arranged in the same order as the data itself.

frmptr defw frmlst         ; sprite frames.
sprptr defw sprgfx         ; sprite graphics.
blkptr defw chgfx          ; block graphics.
colptr defw bcol           ; address of char colours.
proptr defw bprop          ; address of char properties.
scrptr defw maprtab        ; address of map rooms.
nmeptr defw nmedat         ; enemy start positions.
msgptr defw msgdat         ; pointer to messages.
objptr defw objdat         ; pointer to objects.
sndptr defw fx1            ; pointer to sounds.
eop    defw 65535          ; pointer to last used byte of RAM.

; Assorted game routines which can go in contended memory.

; Wait for keypress.

prskey call debkey         ; debounce key.
prsky0 call 654            ; return keyboard state in e.
       ld a,e              ; get value.
       inc a               ; is it 255?
       jr z,prsky0         ; yes, repeat until key pressed.
       call joykey         ; read joystick/keyboard.

; Debounce keypress.

debkey call 654            ; D=shift, E=key.
       ld a,e              ; get value.
       inc a               ; is it 255?
       jr nz,debkey        ; no - loop until key is released.
       ret

; Delay routine.

delay  push bc             ; store loop counter.
       halt                ; wait for interrupt.
       ld a,(23672)        ; clock low.
       rra                 ; rotate bit into carry.
       call c,plsnd        ; time to play sound.
       pop bc              ; restore counter.
       djnz delay          ; repeat.
       ret

; Clear sprite table.

xspr   ld hl,sprtab        ; sprite table.
       ld b,SPRBUF         ; length of table.
xspr0  ld (hl),255         ; clear one byte.
       inc hl              ; move to next byte.
       djnz xspr0          ; repeat for rest of table.
       ret

; Initialise all objects.

iniob  ld ix,(objptr)      ; objects table.
       ld a,(numob)        ; number of objects in the game.
       ld b,a              ; loop counter.
       ld de,38            ; distance between objects.
iniob0 ld a,(ix+35)        ; start screen.
       ld (ix+32),a        ; set start screen.
       ld a,(ix+36)        ; find start x.
       ld (ix+33),a        ; set start x.
       ld a,(ix+37)        ; get initial y.
       ld (ix+34),a        ; set y coord.
       add ix,de           ; point to next object.
       djnz iniob0         ; repeat.
       ret

; Screen synchronisation.

vsync  ld a,(sndtyp)       ; sound to play.
       and a               ; any sound?
       jp z,vsync1         ; no.
       ld b,a              ; outer loop.
       ld c,0              ; value to write to speaker.
vsync2 ld a,c              ; get speaker value.
       out (254),a         ; write to speaker.
       xor 248             ; toggle all except the border bits.
       ld c,a              ; store value for next time.
       ld d,b              ; store loop counter.
vsync3 ld hl,clock         ; previous clock setting.
       ld a,(23672)        ; current clock setting.
       cp (hl)             ; subtract last reading.
       jp nz,vsync4        ; yes, no more processing please.
       djnz vsync3         ; loop.
       ld b,d              ; restore loop counter.
       djnz vsync2         ; continue noise.
vsync4 ld a,d              ; where we got to.
       ld (sndtyp),a       ; remember for next time.
vsync1 ld hl,clock         ; last clock reading.
vsync0 ld a,(23672)        ; current clock reading.
       cp (hl)             ; are they the same?
       jr z,vsync0         ; yes, wait until clock changes.
       ld (hl),a           ; set new clock reading.
       ret
clock  defb 0
sndtyp defb 0

; Redraw the screen.

; Remove old copy of all sprites for redraw.

redraw push ix             ; place sprite pointer on stack.
       call droom          ; show screen layout.
       call shwob          ; draw objects.
       ld b,NUMSPR         ; sprites to draw.
       ld ix,sprtab        ; sprite table.
redrw0 ld a,(ix)           ; old sprite type.
       inc a               ; is it enabled?
       jr z,redrw1         ; no, find next one.
       ld a,(ix+3)         ; sprite x.
       cp 177              ; beyond maximum?
       jr nc,redrw1        ; yes, nothing to draw.
       push bc             ; store sprite counter.
       call sspria         ; show single sprite.
       pop bc              ; retrieve sprite counter.
redrw1 ld de,TABSIZ        ; distance to next odd/even entry.
       add ix,de           ; next sprite.
       djnz redrw0         ; repeat for remaining sprites.
       pop ix              ; retrieve sprite pointer.
       ret

; Nobody uses the dig routines so we'll put them in contended RAM.
; Dig down.

digd   ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       add a,16            ; look down 16 pixels.
       ld l,a              ; coords in hl.
       jr digv

; Dig up.

digu   ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       sub 2               ; look 2 pixels above player.
       ld l,a              ; coords in hl.
digv   ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       call fdchk          ; standard fodder check.
       inc hl              ; look right one cell.
       call fdchk          ; do the fodder stuff.
       ld a,(dispy)        ; y coordinate.
       and 7               ; position straddling block cells.
       ret z               ; no more checks needed.
       inc hl              ; look to third cell.
       jp fdchk            ; do the fodder processing.
                           ; return with zero flag set accordingly.

; Dig right.

digr   ld l,(ix+8)         ; x coordinate.
       ld a,(ix+9)         ; y coordinate.
       add a,16            ; look right 16 pixels.
       ld h,a              ; coords in hl.
       jr digh

; Dig left.

digl   ld l,(ix+8)         ; x coordinate.
       ld a,(ix+9)         ; y coordinate.
       sub 2               ; look 2 pixels to left of sprite.
       ld h,a              ; coords in hl.
digh   ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       call fdchk          ; standard fodder check.
       ld de,32            ; distance to next cell down.
       add hl,de           ; look down one cell.
       call fdchk          ; do the fodder stuff.
       ld a,(dispx)        ; x coordinate.
       and 7               ; position straddling block cells.
       ret z               ; no more checks needed.
       ld de,32            ; distance to next cell down.
       add hl,de           ; look to third cell.
       jp fdchk            ; do the fodder processing.
                           ; return with zero flag set accordingly.

; Fill routines, complete opposite of DIG.
; Fill down.

filld  ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       add a,23            ; look down 23 pixels.
       ld l,a              ; coords in hl.
       jr fillv

; Fill up.

fillu  ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       sub 8               ; look 8 pixels above player.
       ld l,a              ; coords in hl.
fillv  ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       call emchk          ; standard empty check.
       inc hl              ; look right one cell.
       call emchk          ; do the empty stuff.
       ld a,(dispy)        ; y coordinate.
       and 7               ; position straddling block cells.
       ret z               ; no more checks needed.
       inc hl              ; look to third cell.
       jp emchk            ; do the fodder processing.
                           ; return with zero flag set accordingly.

; Fill right.

fillr  ld l,(ix+8)         ; x coordinate.
       ld a,(ix+9)         ; y coordinate.
       add a,23            ; look right 23 pixels.
       ld h,a              ; coords in hl.
       jr fillh

; Fill left.

filll  ld l,(ix+8)         ; x coordinate.
       ld a,(ix+9)         ; y coordinate.
       sub 8               ; look 8 pixels to left of sprite.
       ld h,a              ; coords in hl.
fillh  ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       call emchk          ; standard empty check.
       ld de,32            ; distance to next cell down.
       add hl,de           ; look down one cell.
       call emchk          ; do the empty cell stuff.
       ld a,(dispx)        ; x coordinate.
       and 7               ; position straddling block cells.
       ret z               ; no more checks needed.
       ld de,32            ; distance to next cell down.
       add hl,de           ; look to third cell.
       jp emchk            ; do the space processing.
                           ; return with zero flag set accordingly.

fdchk  ld a,(hl)           ; fetch cell.
       cp FODDER           ; is it fodder?
       ret nz              ; no.
       ld (hl),0           ; rewrite block type.
       push hl             ; store pointer to block.
       ld de,MAP           ; address of map.
       and a               ; clear carry flag for subtraction.
       sbc hl,de           ; find simple displacement for block.
       ld a,l              ; low byte is y coordinate.
       and 31              ; column position 0 - 31.
       ld (dispy),a        ; set up y position.
       add hl,hl           ; multiply displacement by 8.
       add hl,hl
       add hl,hl
       ld a,h              ; x coordinate now in h.
       ld (dispx),a        ; set the display coordinate.
       xor a               ; block to write.
       call pattr          ; write block.
       pop hl              ; restore block pointer.
       ret

emchk  ld a,(hl)           ; fetch cell.
       and a               ; is it empty space?
       ret nz              ; no.
       push hl             ; store pointer to block.
       ld de,MAP           ; address of map.
       and a               ; clear carry flag for subtraction.
       sbc hl,de           ; find simple displacement for block.
       ld a,l              ; low byte is y coordinate.
       and 31              ; column position 0 - 31.
       ld (dispy),a        ; set up y position.
       add hl,hl           ; multiply displacement by 8.
       add hl,hl
       add hl,hl
       ld a,h              ; x coordinate now in h.
       ld (dispx),a        ; set the display coordinate.
       ld a,b              ; block to write.
       push bc             ; store block.
       call pattr          ; write block.
       pop bc              ; retrieve block.
       pop hl              ; restore block pointer.
       ret


; Main game code starts here.

game   equ $

       call setpal         ; set up ULAplus palette.
       call setedg         ; set screen edge coordinates.
       ld hl,MAP           ; block properties.
       ld de,MAP+1         ; next byte.
       ld bc,767           ; size of property map.
       ld (hl),WALL        ; write default property.
       ldir
       call iniob          ; initialise objects.
       xor a               ; put zero in accumulator.
       ld (gamwon),a       ; reset game won flag.
       ld hl,score
       ld b,6              ; digits to initialise.
init0  ld (hl),'0'         ; write zero digit.
       inc hl              ; next column.
       djnz init0          ; repeat for all digits.
mapst  ld a,34             ; start position on map.
       ld (roomtb),a       ; set up position in table, if there is one.
       call initsc         ; set up first screen.
       ld ix,sprtab        ; default to first element in table.
evini  call evnt09         ; game initialisation event.

; Two restarts.
; First restart - clear all sprites and initialise everything.

rstrt  call rsevt          ; restart events.
       call xspr           ; clear sprite table.
       call sprlst         ; fetch pointer to screen sprites.
       call ispr           ; initialise sprite table.
       jr rstrt0

; Second restart - clear all but player, and don't initialise him.

rstrtn call rsevt          ; restart events.
       call nspr           ; clear all non-player sprites.
       call sprlst         ; fetch pointer to screen sprites.
       call kspr           ; initialise sprite table, no more players.


; Set up the player and/or enemy sprites.

rstrt0 xor a               ; zero in accumulator.
       ld (nexlev),a       ; reset next level flag.
       ld (restfl),a       ; reset restart flag.
       ld (deadf),a        ; reset dead flag.
       call droom          ; show screen layout.
       call shwob          ; draw objects.
       ld ix,sprtab        ; address of sprite table, even sprites.
       call dspr           ; display sprites.
       ld ix,sprtab+TABSIZ ; address of first odd sprite.
       call dspr           ; display sprites.

mloop  call vsync          ; synchronise with display.
       ld ix,sprtab        ; address of sprite table, even sprites.
       call dspr           ; display even sprites.

       call plsnd          ; play sounds.
       call vsync          ; synchronise with display.
       ld ix,sprtab+TABSIZ ; address of first odd sprite.
       call dspr           ; display odd sprites.
       call pspr           ; process sprites.
       call joykey         ; joystick and keyboard routines.

; Main loop events.

evlp1  call evnt12         ; called once per main loop (main loop 1 event).
evlp2  call evnt13         ; called once per main loop (main loop 2 event).

       call bsort          ; sort sprites.
       ld a,(nexlev)       ; finished level flag.
       and a               ; has it been set?
       jr nz,newlev        ; yes, go to next level.
       ld a,(gamwon)       ; finished game flag.
       and a               ; has it been set?
       jr nz,evwon         ; yes, finish the game.
       ld a,(restfl)       ; finished level flag.
       cp 1                ; has it been set?
       jr z,rstrt          ; yes, go to next level.
       cp 2                ; has it been set?
       jr z,rstrtn         ; yes, go to next level.
;       call debug

       ld a,(deadf)        ; dead flag.
       and a               ; is it non-zero?
       jr nz,pdead         ; yes, player dead.

; back to start of main loop.

       ld bc,49150         ; keyboard row H - ENTER.
       in a,(c)            ; read it.
       rra                 ; rotate bit for ENTER into carry.
qoff   jp mloop            ; switched to a jp nz,mloop during test mode.
       ret
newlev ld a,(scno)         ; current screen.
       ld hl,numsc         ; total number of screens.
       inc a               ; next screen.
       cp (hl)             ; reached the limit?
       jr nc,evwon         ; yes, game finished.
       ld (scno),a         ; set new level number.
       jp rstrt            ; restart, clearing all aliens.
evwon  call evnt14         ; game completed event.
       jp tidyup           ; tidy up and return to BASIC/calling routine.

; Player dead.

pdead  xor a               ; zeroise accumulator.
       ld (deadf),a        ; reset dead flag.
evdie  call evnt15         ; death subroutine.
       ld hl,numlif        ; number of lives.
       dec (hl)            ; one less.
       jp nz,rstrt         ; restart game.
tidyup ld hl,10072         ; BASIC likes this in alternate hl.
       exx                 ; flip hl into alternate registers.
       ld bc,score         ; return pointing to score so BASIC programmer can store high-score.
       ret

; Restart event.

rsevt  ld ix,sprtab        ; default to first element in table.
evrs   jp evnt10           ; jump to restart event.


; Objects code.

gtobj  ld a,(obno)         ; object number.
       ld h,a              ; number in h.
gtobj0 ld d,38             ; multiplier.
       call imul           ; find address.
       ld de,(objptr)      ; object data.
       add hl,de           ; find address of object.
       ret

; Copy number passed in a to string position bc, right-justified.

num2ch ld l,a              ; put accumulator in l.
       ld h,0              ; blank high byte of hl.
       ld a,32             ; leading spaces.
       ld de,100           ; hundreds column.
       call numdg          ; show digit.
       ld de,10            ; tens column.
       call numdg          ; show digit.
       or 16               ; last digit is always shown.
       ld de,1             ; units column.
numdg  and 48              ; clear carry, clear digit.
numdg1 sbc hl,de           ; subtract from column.
       jr c,numdg0         ; nothing to show.
       or 16               ; something to show, make it a digit.
       inc a               ; increment digit.
       jr numdg1           ; repeat until column is zero.
numdg0 add hl,de           ; restore total.
       cp 32               ; leading space?
       ret z               ; yes, don't write that.
       ld (bc),a           ; write digit to buffer.
       inc bc              ; next buffer position.
       ret



; Multiply h by d and return in hl.

imul   ld e,d              ; HL = H * D
       ld c,h              ; make c first multiplier.
       ld hl,0             ; zeroise total.
       ld d,h              ; zeroise high byte so de=multiplier.
       ld b,8              ; repeat 8 times.
imul1  rr c                ; rotate rightmost bit into carry.
       jr nc,imul2         ; wasn't set.
       add hl,de           ; bit was set, so add de.
       and a               ; reset carry.
imul2  rl e                ; shift de 1 bit left.
       rl d
       djnz imul1          ; repeat 8 times.
       ret


; Initialise a sound.

isnd   ld de,(ch1ptr)      ; first pointer.
       ld a,(de)           ; get first byte.
       inc a               ; reached the end?
       jr z,isnd1          ; that'll do.
       ld de,(ch2ptr)      ; second pointer.
       ld a,(de)           ; get first byte.
       inc a               ; reached the end?
       jr z,isnd2          ; that'll do.
       ld de,(ch3ptr)      ; final pointer.
       ld a,(de)           ; get first byte.
       inc a               ; reached the end?
       jr z,isnd3          ; that'll do.
       ret
isnd1  ld (ch1ptr),hl      ; set up the sound.
       ret
isnd2  ld (ch2ptr),hl      ; set up the sound.
       ret
isnd3  ld (ch3ptr),hl      ; set up the sound.
       ret


ch1ptr defw spmask
ch2ptr defw spmask
ch3ptr defw spmask

plsnd  call plsnd1         ; first channel.
       call plsnd2         ; second one.
       call plsnd3         ; final channel.

; Write the contents of our AY buffer to the AY registers.

w8912  ld hl,snddat        ; start of AY-3-8912 register data.
       ld de,14*256        ; start with register 0, 14 to write.
       ld c,253            ; low byte of port to write.
w8912a ld b,255            ; port 65533=select soundchip register.
       out (c),e           ; tell chip which register we're writing.
       ld a,(hl)           ; value to write.
       ld b,191            ; port 49149=write value to register.
       out (c),a           ; this is what we're putting there.
       inc e               ; next sound chip register.
       inc hl              ; next byte to write.
       dec d               ; decrement loop counter.
       jp nz,w8912a        ; repeat until done.
       ret

snddat defw 0              ; tone registers, channel A.
       defw 0              ; channel B tone registers.
       defw 0              ; as above, channel C.
sndwnp defb 0              ; white noise period.
sndmix defb 60             ; tone/noise mixer control.
sndv1  defb 0              ; channel A amplitude/envelope generator.
sndv2  defb 0              ; channel B amplitude/envelope.
sndv3  defb 0              ; channel C amplitude/envelope.
       defw 0              ; duration of each note.
       defb 0

plwn   inc hl              ; next byte of sound.
       and 56              ; check if we're bothering with white noise.
       ret nz              ; we're not.
       ld a,(hl)           ; fetch byte.
       ld (sndwnp),a       ; set white noise period.
       ret


plsnd2 call cksnd2         ; check sound for first channel.
       cp 255              ; reached end?
       jr z,silen2         ; silence this channel.
       and 15              ; sound bits.
       ld (sndv2),a        ; set volume for channel.
       ld a,(sndmix)       ; mixer byte.
       and 237             ; remove bits for this channel.
       ld b,a              ; store in b register.
       call plmix          ; fetch mixer details.
       and 18              ; mixer bits we want.
       or b                ; combine with mixer bits.
       ld (sndmix),a       ; new mixer value.
       call plwn           ; white noise check.
       inc hl              ; tone low.
       ld e,(hl)           ; fetch value.
       inc hl              ; tone high.
       ld d,(hl)           ; fetch value.
       ld (snddat+2),de    ; set tone.
       inc hl              ; next bit of sound.
       ld (ch2ptr),hl      ; set pointer.
       ret

plsnd3 call cksnd3         ; check sound for first channel.
       cp 255              ; reached end?
       jr z,silen3         ; silence last channel.
       and 15              ; sound bits.
       ld (sndv3),a        ; set volume for channel.
       ld a,(sndmix)       ; mixer byte.
       and 219             ; remove bits for this channel.
       ld b,a              ; store in b register.
       call plmix          ; fetch mixer details.
       and 36              ; mixer bits we want.
       or b                ; combine with mixer bits.
       ld (sndmix),a       ; new mixer value.
       call plwn           ; white noise check.
       inc hl              ; tone low.
       ld e,(hl)           ; fetch value.
       inc hl              ; tone high.
       ld d,(hl)           ; fetch value.
       ld (snddat+4),de    ; set tone.
       inc hl              ; next bit of sound.
       ld (ch3ptr),hl      ; set pointer.
       ret

plmix  ld a,(hl)           ; fetch mixer byte.
       and 192             ; mix bits are d6 and d7.
       rlca                ; rotate into d0 and d1.
       rlca
       ld e,a              ; displacement in de.
       ld d,0
       push hl             ; store pointer on stack.
       ld hl,mixtab        ; mixer table.
       add hl,de           ; point to mixer byte.
       ld a,(hl)           ; fetch mixer value.
       pop hl              ; restore pointer.
       ret
mixtab defb 63,56,7,0      ; mixer byte settings.

silen1 xor a               ; zero.
       ld (sndv1),a        ; sound off.
       ld a,(sndmix)       ; mixer byte.
       or 9                ; mix bits off.
       ld (sndmix),a       ; mixer setting for channel.
       ret
silen2 xor a               ; zero.
       ld (sndv2),a        ; sound off.
       ld a,(sndmix)       ; mixer byte.
       or 18               ; mix bits off.
       ld (sndmix),a       ; mixer setting for channel.
       ret
silen3 xor a               ; zero.
       ld (sndv3),a        ; sound off.
       ld a,(sndmix)       ; mixer byte.
       or 36               ; mix bits off.
       ld (sndmix),a       ; mixer setting for channel.
       ret
cksnd1 ld hl,(ch1ptr)      ; pointer to sound.
       ld a,(hl)           ; fetch mixer/flag.
       ret
cksnd2 ld hl,(ch2ptr)      ; pointer to sound.
       ld a,(hl)           ; fetch mixer/flag.
       ret
cksnd3 ld hl,(ch3ptr)      ; pointer to sound.
       ld a,(hl)           ; fetch mixer/flag.
       ret

plsnd1 call cksnd1         ; check sound for first channel.
       cp 255              ; reached end?
       jr z,silen1         ; silence first channel.
       and 15              ; sound bits.
       ld (sndv1),a        ; set volume for channel.
       ld a,(sndmix)       ; mixer byte.
       and 246             ; remove bits for this channel.
       ld b,a              ; store in b register.
       call plmix          ; fetch mixer details.
       and 9               ; mixer bits we want.
       or b                ; combine with mixer bits.
       ld (sndmix),a       ; new mixer value.
       call plwn           ; white noise check.
       inc hl              ; tone low.
       ld e,(hl)           ; fetch value.
       inc hl              ; tone high.
       ld d,(hl)           ; fetch value.
       ld (snddat),de      ; set tone.
       inc hl              ; next bit of sound.
       ld (ch1ptr),hl      ; set pointer.
       ret


; Objects handling.
; 32 bytes for image
; 3 for room, x and y
; 3 for starting room, x and y.
; 254 = disabled.
; 255 = object in player's pockets.

; Show items present.

shwob  ld hl,(objptr)      ; objects table.
       ld de,32            ; distance to room number.
       add hl,de           ; point to room data.
       ld a,(numob)        ; number of objects in the game.
       ld b,a              ; loop counter.
shwob0 push bc             ; store count.
       push hl             ; store item pointer.
       ld a,(scno)         ; current location.
       cp (hl)             ; same as an item?
       call z,dobj         ; yes, display object.
       pop hl              ; restore pointer.
       pop bc              ; restore counter.
       ld de,38            ; distance to next item.
       add hl,de           ; point to it.
       djnz shwob0         ; repeat for others.
       ret


; Display object.
; hl must point to object's room number.

dobj   inc hl              ; point to x.
dobj0  ld de,dispx         ; coordinates.
       ldi                 ; transfer x coord.
       ldi                 ; transfer y too.
       ld de,65501         ; minus 35.
       add hl,de           ; point to image.
       jp sprite           ; draw this sprite.

; Pick up object number held in the accumulator.

getob  ld hl,numob         ; number of objects in game.
       cp (hl)             ; are we checking past the end?
       ret nc              ; yes, can't get non-existent item.
       call gotob          ; check if we already have it.
       ret z               ; we already do.
       ex de,hl            ; object address in de.
       ld hl,scno          ; current screen.
       cp (hl)             ; is it on this screen?
       ex de,hl            ; object address back in hl.
       jr nz,getob0        ; not on screen, so nothing to delete.
       ld (hl),255         ; pick it up.
       inc hl              ; point to x coord.
getob1 ld e,(hl)           ; x coord.
       inc hl              ; back to y coord.
       ld d,(hl)           ; y coord.
       ld (dispx),de       ; set display coords.
       ld de,65502         ; minus graphic size.
       add hl,de           ; point to graphics.
       jp sprite           ; delete object sprite.
getob0 ld (hl),255         ; pick it up.
       ret

; Got object check.
; Call with object in accumulator, returns zero set if in pockets.

gotob  ld hl,numob         ; number of objects in game.
       cp (hl)             ; are we checking past the end?
       jr nc,gotob0        ; yes, we can't have a non-existent object.
       call findob         ; find the object.
gotob1 cp 255              ; in pockets?
       ret
gotob0 ld a,254            ; missing.
       jr gotob1


findob ld hl,(objptr)      ; objects.
       and a               ; is it zero?
       jr z,fndob1         ; yes, skip loop.
       ld b,a              ; loop counter in b.
       ld de,38            ; size of each object.
fndob2 add hl,de           ; point to next one.
       djnz fndob2         ; repeat until we find address.
fndob1 ld de,32            ; distance to room it's in.
       add hl,de           ; point to room.
       ld a,(hl)           ; fetch status.
       ret


; Drop object number.

drpob  ld hl,numob         ; number of objects in game.
       cp (hl)             ; are we checking past the end?
       ret nc              ; yes, can't drop non-existent item.
       call gotob          ; make sure object is in inventory.
       ld a,(scno)         ; screen number.
       cp (hl)             ; already on this screen?
       ret z               ; yes, nothing to do.
       ld (hl),a           ; bring onto screen.
       inc hl              ; point to x coord.
       ld a,(ix+8)         ; sprite x coordinate.
       ld (hl),a           ; set x coord.
       ld a,(ix+9)         ; sprite y coordinate.
       inc hl              ; point to object y.
       ld (hl),a           ; set the y position.
       dec hl              ; back to x.
       jp getob1           ; display object.

; Seek objects at sprite position.

skobj  ld hl,(objptr)      ; pointer to objects.
       ld de,32            ; distance to room number.
       add hl,de           ; point to room data.
       ld de,38            ; size of each object.
       ld a,(numob)        ; number of objects in game.
       ld b,a              ; set up the loop counter.
skobj0 ld a,(scno)         ; current room number.
       cp (hl)             ; is object in here?
       call z,skobj1       ; yes, check coordinates.
       add hl,de           ; point to next object in table.
       djnz skobj0         ; repeat for all objects.
       ld a,255            ; end of list and nothing found, return 255.
       ret
skobj1 inc hl              ; point to x coordinate.
       ld a,(hl)           ; get coordinate.
       sub (ix+3)          ; subtract sprite x.
       add a,15            ; add sprite height minus one.
       cp 31               ; within range?
       jp nc,skobj2        ; no, ignore object.
       inc hl              ; point to y coordinate now.
       ld a,(hl)           ; get coordinate.
       sub (ix+4)          ; subtract the sprite y.
       add a,15            ; add sprite width minus one.
       cp 31               ; within range?
       jp nc,skobj3        ; no, ignore object.
       pop de              ; remove return address from stack.
       ld a,(numob)        ; objects in game.
       sub b               ; subtract loop counter.
       ret                 ; accumulator now points to object.
skobj3 dec hl              ; back to y position.
skobj2 dec hl              ; back to room.
       ret


; Spawn a new sprite.

spawn  ld hl,sprtab        ; sprite table.
       ld a,NUMSPR         ; number of sprites.
       ld de,TABSIZ        ; size of each entry.
spaw0  ex af,af'           ; store loop counter.
       ld a,(hl)           ; get sprite type.
       inc a               ; is it an unused slot?
       jr z,spaw1          ; yes, we can use this one.
       add hl,de           ; point to next sprite in table.
       ex af,af'           ; restore loop counter.
       dec a               ; one less iteration.
       jr nz,spaw0         ; keep going until we find a slot.
       ret                 ; didn't find one.
spaw1  push ix             ; existing sprite address on stack.
       ld (spptr),hl       ; store spawned sprite address.
       ld (hl),c           ; set the type.
       inc hl              ; point to image.
       ld (hl),b           ; set the image.
       inc hl              ; next byte.
       ld (hl),0           ; frame zero.
       inc hl              ; next byte.
       ld a,(ix+X)         ; x coordinate.
       ld (hl),a           ; set sprite coordinate.
       inc hl              ; next byte.
       ld a,(ix+Y)         ; y coordinate.
       ld (hl),a           ; set sprite coordinate.
       inc hl              ; next byte.
       ex de,hl            ; swap address into de.
       ld hl,(spptr)       ; restore address of details.
       ld bc,5             ; number of bytes to duplicate.
       ldir                ; copy first version to new version.
       ex de,hl            ; swap address into de.
       ld a,(ix+10)        ; direction of original.
       ld (hl),a           ; set the direction.
       inc hl              ; next byte.
       ld (hl),b           ; reset parameter.
       inc hl              ; next byte.
       ld (hl),b           ; reset parameter.
       inc hl              ; next byte.
       ld (hl),b           ; reset parameter.
       inc hl              ; next byte.
       ld (hl),b           ; reset parameter.
rtssp  ld ix,(spptr)       ; address of new sprite.
evis1  call evnt11         ; call sprite initialisation event.
       ld ix,(spptr)       ; address of new sprite.
       call sspria         ; display the new sprite.
       pop ix              ; address of original sprite.
       ret

spptr  defw 0              ; spawned sprite pointer.
seed   defw 0              ; seed for random numbers.
seed2  defb 0              ; second seed.
score  defb '000000'       ; player's score.
grbase defw 15360          ; graphics base address.
scrsiz defw 484            ; screen size.

checkx ld a,(dispx)        ; x position.
       cp 24               ; off screen?
       ret c               ; no, it's okay.
       pop hl              ; remove return address from stack.
       ret

; Displays the current score.

dscore ld hl,(23606)       ; font pointer.
       ld (grbase),hl      ; set up graphics base.
       call checkx         ; make sure we're in a printable range.
       ld hl,score         ; score text.
       ld b,6              ; digits to display.
dscor0 push bc             ; place counter onto the stack.
       push hl
       ld a,(hl)           ; fetch character.
       call pchar          ; display character.
       call gaadd          ; get attribute address.
       ld a,(23693)        ; current cell colours.
       ld (hl),a           ; write to attribute cell.
       ld hl,dispy         ; y coordinate.
       inc (hl)            ; move along one.
       pop hl
       inc hl              ; next score column.
       pop bc              ; retrieve character counter.
       djnz dscor0         ; repeat for all digits.
       ld hl,(blkptr)      ; blocks.
       ld (grbase),hl      ; set graphics base.
       ret

; Adds number in the accumulator to the score.

addsc  ld hl,score+3       ; hundreds column.
       ld b,100            ; hundreds.
       call incsc          ; increase score.
       inc hl              ; tens column.
       ld b,10             ; tens.
       call incsc          ; increase score.
       inc hl              ; units column.
       ld b,1              ; units.
incsc  ld c,a              ; store amount to add in c register.
incsc1 sub b               ; subtract column total.
       jr c,incsc0         ; too much, restore value.
       ld c,a              ; new amount in c register.
       push hl             ; store column position.
       call incsc2         ; do the increment.
       pop hl              ; restore column.
       ld a,c              ; restore number left to add.
       jp incsc
incsc0 ld a,c              ; restore previous value.
       ret

incsc2 inc (hl)            ; add one to column.
       ld a,(hl)           ; fetch column total.
       cp '9'+1            ; gone beyond range of digits?
       ret c               ; no, carry on.
       ld (hl),'0'         ; make this a zero.
       dec hl              ; back one column.
       jr incsc2

; Get print address.

gprad  ld a,(dispx)        ; returns scr. add. in de.
       ld e,a              ; place in e for now.
       and 24              ; which of 3 segments do we need?
       add a,64            ; add 64 for start address of screen.
       ld d,a              ; that's our high byte.
       ld a,e              ; restore x coordinate.
       rrca                ; multiply by 32.
       rrca
       rrca
       and 224             ; lines within segment.
       ld e,a              ; set up low byte for x.
       ld a,(dispy)        ; now get y coordinate.
       add a,e             ; add to low byte.
       ld e,a              ; final low byte.
       ret

; Get property buffer address of char at (dispx, dispy) in hl.

pradd  ld a,(dispx)        ; x coordinate.
       rrca                ; multiply by 32.
       rrca
       rrca
       ld l,a              ; store shift in l.
       and 3               ; high byte bits.
       add a,253           ; 88 * 256 = 64768, start of properties map.
       ld h,a              ; that's our high byte.
       ld a,l              ; restore shift result.
       and 224             ; only want low bits.
       ld l,a              ; put into low byte.
       ld a,(dispy)        ; fetch y coordinate.
       and 31              ; should be in range 0 - 31.
       add a,l             ; add to low byte.
       ld l,a              ; new low byte.
       ret

; Get attribute address of char at (dispx, dispy) in hl.

gaadd  ld a,(dispx)        ; x coordinate.
       rrca                ; multiply by 32.
       rrca
       rrca
       ld l,a              ; store shift in l.
       and 3               ; high byte bits.
       add a,88            ; 88 * 256 = 22528, start of screen attributes.
       ld h,a              ; that's our high byte.
       ld a,l              ; restore shift result.
       and 224             ; only want low bits.
       ld l,a              ; put into low byte.
       ld a,(dispy)        ; fetch y coordinate.
       and 31              ; should be in range 0 - 31.
       add a,l             ; add to low byte.
       ld l,a              ; new low byte.
       ret

pchar  rlca                ; multiply char by 8.
       rlca
       rlca
       ld e,a              ; store shift in e.
       and 7               ; only want high byte bits.
       ld d,a              ; store in d.
       ld a,e              ; restore shifted value.
       and 248             ; only want low byte bits.
       ld e,a              ; that's the low byte.
       ld hl,(grbase)      ; address of graphics.
       add hl,de           ; add displacement.
pchark call gprad          ; get screen address.
       ldi                 ; transfer byte.
       dec de              ; back again.
       inc d               ; next screen row down.
       ldi                 ; transfer byte.
       dec de              ; back again.
       inc d               ; next screen row down.
       ldi                 ; transfer byte.
       dec de              ; back again.
       inc d               ; next screen row down.
       ldi                 ; transfer byte.
       dec de              ; back again.
       inc d               ; next screen row down.
       ldi                 ; transfer byte.
       dec de              ; back again.
       inc d               ; next screen row down.
       ldi                 ; transfer byte.
       dec de              ; back again.
       inc d               ; next screen row down.
       ldi                 ; transfer byte.
       dec de              ; back again.
       inc d               ; next screen row down.
       ldi                 ; transfer byte.
       ret

; Print attributes, properties and pixels.

pattr  ld b,a              ; store cell in b register for now.
       ld e,a              ; displacement in e.
       ld d,0              ; no high byte.
       ld hl,(proptr)      ; pointer to properties.
       add hl,de           ; property cell address.
       ld c,(hl)           ; fetch byte.
       call pradd          ; get property buffer address.
       ld (hl),c           ; write property.
       ld a,b              ; restore cell.

; Print attributes, no properties.

panp   ld e,a              ; displacement in e.
       ld d,0              ; no high byte.
       ld hl,(colptr)      ; pointer to colours.
       add hl,de           ; colour cell address.
       ld c,(hl)           ; fetch byte.
       call gaadd          ; get attribute address.
       ld (hl),c           ; write colour.
       ld a,b              ; restore cell.

; Print character pixels, no more.

pchr   call pchar          ; show character in accumulator.
       ld hl,dispy         ; y coordinate.
       inc (hl)            ; move along one.
       ret

; Shifter sprite routine for objects.

sprit7 xor 7
       inc a
sprit3 rl l                ; shift into position.
       rl c
       rl h
       dec a               ; one less iteration.
       jp nz,sprit3
       ld a,l
       ld l,c
       ld c,h
       ld h,a
       jp sprit0           ; now apply to screen.

sprite push hl             ; store sprite graphic address.
       call scadd          ; get screen address in hl.
       ex de,hl            ; switch to de.
       pop hl              ; restore graphic address.
       ld a,(dispy)        ; y position.
       and 7               ; position straddling cells.
       ld b,a              ; store in b register.
       ld a,16             ; pixel height.
sprit1 ex af,af'
       ld c,(hl)           ; fetch first byte.
       inc hl              ; next byte.
       push hl             ; store source address.
       ld l,(hl)
       ld h,0
       ld a,b              ; position straddling cells.
       and a               ; is it zero?
       jr z,sprit0         ; yes, apply to screen.
       cp 5
       jr nc,sprit7
       and a               ; clear carry.
sprit2 rr c
       rr l
       rr h
       dec a
       jp nz,sprit2
sprit0 ld a,(de)           ; fetch screen image.
       xor c               ; merge with graphic.
       ld (de),a           ; write to screen.
       inc e               ; next screen byte.
       ld a,(de)           ; fetch screen image.
       xor l               ; combine with graphic.
       ld (de),a           ; write to screen.
       inc de              ; next screen address.
       ld a,(de)           ; fetch screen image.
       xor h               ; combine with graphic.
       ld (de),a           ; write to screen.
       dec de              ; left to middle byte.
       dec e               ; back to start byte.
       inc d               ; increment line number.
       ld a,d              ; segment address.
       and 7               ; reached end of segment?
       jp nz,sprit6        ; no, just do next line within cell.
       ld a,e              ; low byte.
       add a,32            ; look down.
       ld e,a              ; new address.
       jp c,sprit6         ; done.
       ld a,d              ; high byte.
       sub 8               ; start of segment.
       ld d,a              ; new high byte.
sprit6 pop hl              ; restore source address.
       inc hl              ; next source byte.
       ex af,af'
       dec a
       jp nz,sprit1
       ret

; Get room address.

groom  ld a,(scno)         ; screen number.
groomx ld de,0             ; start at zero.
       ld hl,(scrptr)      ; pointer to screens.
       and a               ; is it the first one?
groom1 jr z,groom0         ; no more screens to skip.
       ld c,(hl)           ; low byte of screen size.
       inc hl              ; point to high byte.
       ld b,(hl)           ; high byte of screen size.
       inc hl              ; next address.
       ex de,hl            ; put total in hl, pointer in de.
       add hl,bc           ; skip a screen.
       ex de,hl            ; put total in de, pointer in hl.
       dec a               ; one less iteration.
       jr groom1           ; loop until we reach the end.
groom0 ld hl,(scrptr)      ; pointer to screens.
       add hl,de           ; add displacement.
       ld a,(numsc)        ; number of screens.
       ld d,0              ; zeroise high byte.
       ld e,a              ; displacement in de.
       add hl,de           ; add double displacement to address.
       add hl,de
       ret

; Draw present room.

droom  ld a,(wintop)       ; window top.
       ld (dispx),a        ; set x coordinate.
droom2 ld hl,(blkptr)      ; blocks.
       ld (grbase),hl      ; set graphics base.
       call groom          ; get address of current room.
       xor a               ; zero in accumulator.
       ld (comcnt),a       ; reset compression counter.
       ld a,(winhgt)       ; height of window.
droom0 push af             ; store row counter.
       ld a,(winlft)       ; window left edge.
       ld (dispy),a        ; set cursor position.
       ld a,(winwid)       ; width of window.
droom1 push af             ; store column counter.
       call flbyt          ; decompress next byte on the fly.
       push hl             ; store address of cell.
       call pattr          ; show attributes and block.
       pop hl              ; restore cell address.
       pop af              ; restore loop counter.
       dec a               ; one less column.
       jr nz,droom1        ; repeat for entire line.
       ld a,(dispx)        ; x coord.
       inc a               ; move down one line.
       ld (dispx),a        ; set new position.
       pop af              ; restore row counter.
       dec a               ; one less row.
       jr nz,droom0        ; repeat for all rows.
       ret

; Decompress bytes on-the-fly.

flbyt  ld a,(comcnt)       ; compression counter.
       and a               ; any more to decompress?
       jr nz,flbyt1        ; yes.
       ld a,(hl)           ; fetch next byte.
       inc hl              ; point to next cell.
       cp 255              ; is this byte a control code?
       ret nz              ; no, this byte is uncompressed.
       ld a,(hl)           ; fetch byte type.
       ld (combyt),a       ; set up the type.
       inc hl              ; point to quantity.
       ld a,(hl)           ; get quantity.
       inc hl              ; point to next byte.
flbyt1 dec a               ; one less.
       ld (comcnt),a       ; store new quantity.
       ld a,(combyt)       ; byte to expand.
       ret


combyt defb 0              ; byte type compressed.
comcnt defb 0              ; compression counter.

; Ladder down check.

laddd  ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       add a,16            ; look down 16 pixels.
       ld l,a              ; coords in hl.
       jr laddv

; Ladder up check.

laddu  ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       add a,15            ; look 2 pixels above feet.
       ld l,a              ; coords in hl.
laddv  ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       call ldchk          ; standard ladder check.
       ret nz              ; no way through.
       inc hl              ; look right one cell.
       call ldchk          ; do the check.
       ret nz              ; impassable.
       ld a,(dispy)        ; y coordinate.
       and 7               ; position straddling block cells.
       ret z               ; no more checks needed.
       inc hl              ; look to third cell.
       call ldchk          ; do the check.
       ret                 ; return with zero flag set accordingly.

; Can go up check.

cangu  ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       sub 2               ; look up 2 pixels.
       ld l,a              ; coords in hl.
       ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       call lrchk          ; standard left/right check.
       ret nz              ; no way through.
       inc hl              ; look right one cell.
       call lrchk          ; do the check.
       ret nz              ; impassable.
       ld a,(dispy)        ; y coordinate.
       and 7               ; position straddling block cells.
       ret z               ; no more checks needed.
       inc hl              ; look to third cell.
       call lrchk          ; do the check.
       ret                 ; return with zero flag set accordingly.

; Can go down check.

cangd  ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       add a,16            ; look down 16 pixels.
       ld l,a              ; coords in hl.
       ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       call plchk          ; block, platform check..
       ret nz              ; no way through.
       inc hl              ; look right one cell.
       call plchk          ; block, platform check..
       ret nz              ; impassable.
       ld a,(dispy)        ; y coordinate.
       and 7               ; position straddling block cells.
       ret z               ; no more checks needed.
       inc hl              ; look to third cell.
       call plchk          ; block, platform check..
       ret                 ; return with zero flag set accordingly.

; Can go left check.

cangl  ld l,(ix+8)         ; x coordinate.
       ld a,(ix+9)         ; y coordinate.
       sub 2               ; look left 2 pixels.
       ld h,a              ; coords in hl.
       jr cangh            ; test if we can go there.

; Can go right check.

cangr  ld l,(ix+8)         ; x coordinate.
       ld a,(ix+9)         ; y coordinate.
       add a,16            ; look right 16 pixels.
       ld h,a              ; coords in hl.

cangh  ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       call lrchk          ; standard left/right check.
       ret nz              ; no way through.
       ld de,32            ; distance to next cell.
       add hl,de           ; look down.
       call lrchk          ; do the check.
       ret nz              ; impassable.
       ld a,(dispx)        ; x coordinate.
       and 7               ; position straddling block cells.
       ret z               ; no more checks needed.
       add hl,de           ; look down to third cell.
       call lrchk          ; do the check.
       ret                 ; return with zero flag set accordingly.


; Check left/right movement is okay.

lrchk  ld a,(hl)           ; fetch map cell.
       cp WALL             ; is it passable?
       jr z,lrchkx         ; no.
       cp FODDER           ; fodder has to be dug.
       jr z,lrchkx         ; not passable.
       xor a               ; report it as okay.
       ret
lrchkx xor a               ; reset all bits.
       inc a
       ret

; Check platform or solid item is not in way.

plchk  ld a,(hl)           ; fetch map cell.
       cp WALL             ; is it passable?
       jr z,lrchkx         ; no.
       cp FODDER           ; fodder has to be dug.
       jr z,lrchkx         ; not passable.
       cp PLATFM           ; platform is solid.
       jr z,plchkx         ; not passable.
plchk0 xor a               ; report it as okay.
       ret
plchkx ld a,(dispx)        ; x coordinate.
       and 7               ; position straddling blocks.
       jr z,lrchkx         ; on platform, deny movement.
       jr plchk0

; Check ladder is available.

ldchk  ld a,(hl)           ; fetch cell.
       cp LADDER           ; is it a ladder?
       ret                 ; return with zero flag set accordingly.

; Touched deadly block check.
; Returns with DEADLY (must be non-zero) in accumulator if true.

tded   ld l,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       ld de,31            ; default distance to next line down.
       cp b                ; is this the required block?
       ret z               ; yes.
       inc hl              ; next cell.
       ld a,(hl)           ; fetch type.
       cp b                ; is this deadly/custom?
       ret z               ; yes.
       ld a,(dispy)        ; horizontal position.
       ld c,a              ; store column in c register.
       and 7               ; is it straddling cells?
       jr z,tded0          ; no.
       inc hl              ; last cell.
       ld a,(hl)           ; fetch type.
       cp b                ; is this the block?
       ret z               ; yes.
       dec de              ; one less cell to next row down.
tded0  add hl,de           ; point to next row.
       ld a,(hl)           ; fetch left cell block.
       cp b                ; is this fatal?
       ret z               ; yes.
       inc hl              ; next cell.
       ld a,(hl)           ; fetch type.
       cp b                ; is this fatal?
       ret z               ; yes.
       ld a,c              ; horizontal position.
       and 7               ; is it straddling cells?
       jr z,tded1          ; no.
       inc hl              ; last cell.
       ld a,(hl)           ; fetch type.
       cp b                ; is this fatal?
       ret z               ; yes.
tded1  ld a,(dispx)        ; vertical position.
       and 7               ; is it straddling cells?
       ret z               ; no, job done.
       add hl,de           ; point to next row.
       ld a,(hl)           ; fetch left cell block.
       cp b                ; is this fatal?
       ret z               ; yes.
       inc hl              ; next cell.
       ld a,(hl)           ; fetch type.
       cp b                ; is this fatal?
       ret z               ; yes.
       ld a,c              ; horizontal position.
       and 7               ; is it straddling cells?
       ret z               ; no.
       inc hl              ; last cell.
       ld a,(hl)           ; fetch final type.
       ret                 ; return with final type in accumulator.


; Fetch block type at (dispx, dispy).

tstbl  ld a,(dispx)        ; fetch x coord.
       rlca                ; divide by 8,
       rlca                ; and multiply by 32.
       ld d,a              ; store in d.
       and 224             ; mask off high bits.
       ld e,a              ; low byte.
       ld a,d              ; restore shift result.
       and 3               ; high bits.
       ld d,a              ; got displacement in de.
       ld a,(dispy)        ; y coord.
       rra                 ; divide by 8.
       rra
       rra
       and 31              ; only want 0 - 31.
       add a,e             ; add to displacement.
       ld e,a              ; displacement in de.
       ld hl,MAP           ; position of dummy screen.
       add hl,de           ; point to address.
       ld a,(hl)           ; fetch byte there.
       ret

; Jump - if we can.

jump   ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       add a,16            ; look down 16 pixels.
       ld l,a              ; coords in hl.
       ld (dispx),hl       ; set up test coordinates.
       and 7               ; are we on platform boundary?
       ret nz              ; no, cannot jump.
       call tstbl          ; get map address.
       call plchk          ; block, platform check..
       jr nz,jump0         ; it's solid, we can jump.
       inc hl              ; look right one cell.
       call plchk          ; block, platform check..
       jr nz,jump0         ; it's solid, we can jump.
       ld a,(dispy)        ; y coordinate.
       and 7               ; position straddling block cells.
       ret z               ; no more checks needed.
       inc hl              ; look to third cell.
       call plchk          ; block, platform check..
       ret z               ; not solid, don't jump.
jump0  ld hl,jtab          ; jump table start.
       ld (ix+13),l        ; set jump low.
       ld (ix+14),h        ; set jump high.
       ret


; Multiply h by a and return in hl.
; Does this need to be in the game?

hmul   ld c,h              ; make c first multiplier.
cmul   ld hl,0             ; zeroise total.
       ld e,a              ; copy accumulator to de.
       ld d,h              ; zeroise high byte.
       ld b,8              ; repeat 8 times.
hmul1  rr c                ; rotate rightmost bit into carry.
       jr nc,hmul2         ; wasn't set.
       add hl,de           ; bit was set, so add de.
       and a               ; reset carry.
hmul2  rl e                ; shift de 1 bit left.
       rl d
       djnz hmul1          ; repeat 8 times.
       ret


; Random numbers code.

; Pseudo-random number generator.
; Steps a pointer through the ROM (held in seed), returning the contents
; of the byte at that location.

random ld hl,(seed)        ; pointer to ROM.
       ld a,(seed2)        ; previous seed.
       add a,(hl)          ; combine with number from location.
       ld (seed2),a        ; second seed.
       res 5,h             ; stay within first 8K of ROM.
       xor l               ; more randomness.
       inc hl              ; increment pointer.
       ld (seed),hl        ; new position.
       and c               ; use mask.
       cp b                ; is it less than parameter?
       ld (varrnd),a       ; set random number.
       ret c               ; yes, number is good.
       jp random           ; go round again.

; Keys defined by game designer.
keys    defb 3, 11, 27, 19, 35, 8, 16

; Keyboard test routine.

ktest  ld c,a              ; key to test in c.
       and 7               ; mask bits d0-d2 for row.
       inc a               ; in range 1-8.
       ld b,a              ; place in b.
       srl c               ; divide c by 8
       srl c               ; to find position within row.
       srl c
       ld a,5              ; only 5 keys per row.
       sub c               ; subtract position.
       ld c,a              ; put in c.
       ld a,254            ; high byte of port to read.
ktest0 rrca                ; rotate into position.
       djnz ktest0         ; repeat until we've found relevant row.
       in a,(254)          ; read port (a=high, 254=low).
ktest1 rra                 ; rotate bit out of result.
       dec c               ; loop counter.
       jp nz,ktest1        ; repeat until bit for position in carry.
       ret


; Joystick and keyboard reading routines.

joykey ld a,(contrl)       ; control flag.
       and a               ; is it the keyboard?
       jr z,joyjoy         ; no, it's joystick.

; Keyboard controls.

       ld hl,keys+6        ; address of last key.
       ld e,0              ; zero reading.
       ld d,7              ; keys to read.
joyke0 ld a,(hl)           ; get key from table.
       call ktest          ; being pressed?
       ccf                 ; complement the carry.
       rl e                ; rotate into reading.
       dec hl              ; next key.
       dec d               ; one less to do.
       jp nz,joyke0        ; repeat for all keys.
       ld a,e              ; place result in accumulator.
       ld (joyval),a       ; put that into joyval.
       ret

; Kempston joystick controls.

joyjoy ld bc,31            ; port for Kempston interface.
       in a,(c)            ; read it.
       ld e,a              ; copy to e register.
       ld a,(keys+6)       ; key seven.
       call ktest          ; being pressed?
       jr c,joyjo0         ; not pressed.
       set 6,e             ; set bit d6.
joyjo0 ld a,(keys+5)       ; key six.
       call ktest          ; being pressed?
       jr c,joyjo1         ; not pressed.
       set 5,e             ; set bit d5.
joyjo1 ld a,e              ; copy e register to accumulator.
       ld (joyval),a       ; remember value.
       ret


; Display message.

dmsg   ld hl,nummsg        ; total messages.
       cp (hl)             ; does this one exist?
       ret nc              ; no, nothing to display.
       ld hl,(msgptr)      ; pointer to messages.
       call getwrd         ; get message number.
dmsg3  ld de,(23606)       ; font.
       ld (grbase),de      ; set up for text display.
       call checkx         ; make sure we're in a printable range.
dmsg0  push hl             ; store string pointer.
       ld a,(hl)           ; fetch byte to display.
       and 127             ; remove any end marker.
       cp 13               ; newline character?
       jr z,dmsg1
       call pchar          ; display character.
       call gaadd          ; get attribute address.
       ld a,(23693)        ; current cell colours.
       ld (hl),a           ; write to attribute cell.
       ld hl,dispy         ; y coordinate.
       inc (hl)            ; move along one.
dmsg2  pop hl
       ld a,(hl)           ; fetch last character.
       rla                 ; was it the end?
       ret c               ; yes, job done.
       inc hl              ; next character to display.
       jr dmsg0
dmsg1  ld hl,dispx         ; x coordinate.
       inc (hl)            ; newline.
       ld a,(hl)           ; fetch position.
       cp 24               ; past screen edge?
       jr c,dmsg4          ; no, it's okay.
       ld (hl),0           ; restart at top.
dmsg4  inc hl              ; y coordinate.
       ld (hl),0           ; carriage return.
       jr dmsg2



; On entry: hl points to word list
;           a contains word number.

getwrd and a               ; first word in list?
       ret z               ; yep, don't search.
       ld b,a
getwd0 ld a,(hl)
       inc hl
       cp 128              ; found end?
       jr c,getwd0         ; no, carry on.
       djnz getwd0         ; until we have right number.
       ret



; Bubble sort.

bsort  ld b,NUMSPR - 1     ; sprites to swap.
       ld ix,sprtab        ; sprite table.
bsort0 push bc             ; store loop counter for now.

       ld a,(ix)           ; first sprite type.
       inc a               ; is it switched off?
       jr z,swemp          ; yes, may need to switch another in here.

       ld a,(ix+TABSIZ)    ; check next slot exists.
       inc a               ; is it enabled?
       jr z,bsort2         ; no, nothing to swap.

       ld a,(ix+(3+TABSIZ)); fetch next sprite's coordinate.
       cp (ix+3)           ; compare with this x coordinate.
       jr c,bsort1         ; next sprite is higher - may need to switch.
bsort2 ld de,TABSIZ        ; distance to next odd/even entry.
       add ix,de           ; next sprite.
       pop bc              ; retrieve loop counter.
       djnz bsort0         ; repeat for remaining sprites.
       ret

bsort1 ld a,(ix+TABSIZ)    ; sprite on/off flag.
       inc a               ; is it enabled?
       jr z,bsort2         ; no, nothing to swap.
       call swspr          ; swap positions.
       jr bsort2

swemp  ld a,(ix+TABSIZ)    ; next table entry.
       inc a               ; is that one on?
       jr z,bsort2         ; no, nothing to swap.
       call swspr          ; swap positions.
       jr bsort2

; Swap sprites.

swspr  push ix             ; table address on stack.
       pop hl              ; pop into hl pair.
       ld d,h              ; copy to de pair.
       ld e,l
       ld bc,TABSIZ        ; distance to second entry.
       add hl,bc           ; point to second sprite entry.
       ld b,TABSIZ         ; bytes to swap.
swspr0 ld c,(hl)           ; fetch second byte.
       ld a,(de)           ; fetch first byte.
       ld (hl),a           ; copy to second.
       ld a,c              ; second byte in accumulator.
       ld (de),a           ; copy to first sprite entry.
       inc de              ; next byte.
       inc hl              ; next byte.
       djnz swspr0         ; swap all bytes in table entry.
       ret



; Process sprites.

pspr   ld b,NUMSPR         ; sprites to process.
       ld ix,sprtab        ; sprite table.
pspr1  push bc             ; store loop counter for now.
       ld a,(ix)           ; fetch sprite type.
       cp 8                ; within range of sprite types?
       call c,pspr2        ; yes, process this one.
       ld de,TABSIZ        ; distance to next odd/even entry.
       add ix,de           ; next sprite.
       pop bc              ; retrieve loop counter.
       djnz pspr1          ; repeat for remaining sprites.
       ret
pspr2  ld (ogptr),ix       ; store original sprite pointer.
       call pspr3          ; do the routine.
rtorg  ld ix,(ogptr)       ; restore original pointer to sprite.
rtorg0 ret
pspr3  ld hl,evtyp0        ; sprite type events list.
       call addac2         ; point to address of routine.
       ld e,(hl)           ; address low.
       inc hl              ; next byte of address.
       ld d,(hl)           ; address high.
       ex de,hl            ; swap address into hl.
       jp (hl)             ; go there.
ogptr  defw 0              ; original sprite pointer.

; Address of each sprite type's routine.

evtyp0 defw evnt01
evtyp1 defw evnt02
evtyp2 defw evnt03
evtyp3 defw evnt04
evtyp4 defw evnt05
evtyp5 defw evnt06
evtyp6 defw evnt07
evtyp7 defw evnt08


; Display sprites.

dspr   ld b,NUMSPR/2       ; number of sprites to display.
dspr0  push bc             ; store loop counter for now.
       ld a,(ix)           ; get sprite type.
       inc a               ; is it enabled?
       jr nz,dspr1         ; yes, it needs deleting.
dspr5  ld a,(ix+5)         ; new type.
       inc a               ; is it enabled?
       jr nz,dspr3         ; yes, it needs drawing.
dspr2  ld a,(ix+5)         ; new type.
       ld b,(ix+6)         ; new image number.
       ld c,(ix+7)         ; new frame.
       ld d,(ix+8)         ; new x coord.
       ld e,(ix+9)         ; new y coord.
       ld (ix),a           ; set type.
       ld (ix+1),b         ; set image.
       ld (ix+2),c         ; set frame.
       ld (ix+3),d         ; set x coord.
       ld (ix+4),e         ; set y coord.
       ld de,TABSIZ*2      ; distance to next odd/even entry.
       add ix,de           ; next sprite.
       pop bc              ; retrieve loop counter.
       djnz dspr0          ; repeat for remaining sprites.
       ret
dspr1  ld a,(ix+3)         ; old x coord.
       cp 177              ; beyond maximum?
       jr nc,dspr5         ; yes, don't delete it.
       ld a,(ix+5)         ; type of new sprite.
       inc a               ; is this enabled?
       jr nz,dspr4         ; yes, display both.
dspr6  call sspria         ; show single sprite.
       jp dspr2
dspr4  ld a,(ix+8)         ; new x coord.
       cp 177              ; beyond maximum?
       jr nc,dspr6         ; yes, don't display it.
       call sspric         ; delete old sprite, draw new one simultaneously.
       jp dspr2
dspr3  ld a,(ix+8)         ; new x coord.
       cp 177              ; beyond maximum?
       jr nc,dspr2         ; yes, don't display it.
       call ssprib         ; show single sprite.
       jp dspr2


; Get sprite address calculations.
; gspran = new sprite, gsprad = old sprite.

gspran ld l,(ix+8)         ; new x coordinate.
       ld h,(ix+9)         ; new y coordinate.
       ld (dispx),hl       ; set display coordinates.
       ld a,l              ; x coordinate.
       cp 177              ; beyond maximum?
       ret nc              ; yes, don't display it.

       ld a,(ix+6)         ; new sprite image.
       call gfrm           ; fetch start frame for this sprite.
       ld a,(hl)           ; frame in accumulator.
       add a,(ix+7)        ; new add frame number.
       jp gspra0

gsprad ld l,(ix+3)         ; x coordinate.
       ld h,(ix+4)         ; y coordinate.
       ld (dispx),hl       ; set display coordinates.
       ld a,l              ; x coordinate.
       cp 177              ; beyond maximum?
       ret nc              ; yes, don't display it.

       ld a,(ix+1)         ; sprite image.
       call gfrm           ; fetch start frame for this sprite.
       ld a,(hl)           ; frame in accumulator.
       add a,(ix+2)        ; add frame number.
gspra0 rrca                ; multiply by 128.
       ld d,a              ; store in d.
       and 128             ; low byte bit.
       ld e,a              ; got low byte.
       ld a,d              ; restore result.
       and 127             ; high byte bits.
       ld d,a              ; displacement high byte.
       ld hl,(sprptr)      ; address of play sprites.
       add hl,de           ; point to frame.

       ld a,(dispy)        ; y coordinate.
       rlca                ; multiply by 32.
       rlca                ; already a multiple
       rlca                ; of 2, so just 4
       rlca                ; shifts needed.
       and 96              ; displacement for y-position frame.
       ld e,a              ; put displacement in low byte of de.
       ld d,0              ; zero the high byte.
       add hl,de           ; add to sprite address.
       ex de,hl            ; need it in de for now.
       ld a,(dispy)        ; pre-shifted so find y coord.
       and 6               ; position within byte boundary.
       call maskad         ; get mask address.
       jp scadd            ; calculate screen address.

; These are the sprite routines.
; sspria = single sprite, old (ix).
; ssprib = single sprite, new (ix+5).
; sspric = both sprites, old (ix) and new (ix+5).

sspria call gsprad         ; get old sprite address.
       ld a,16             ; vertical lines.
sspri0 ex af,af'           ; store line counter away in alternate registers.
       call dline          ; draw a line.
       ex af,af'           ; restore line counter.
       dec a               ; one less to go.
       jp nz,sspri0
       ret

ssprib call gspran         ; get new sprite address.
       ld a,16             ; vertical lines.
       jp sspri0

sspric call gsprad         ; get old sprite address.
       exx                 ; store addresses.
       call gspran         ; get new sprite addresses.
       ld a,16             ; vertical lines.
sspri1 ex af,af'           ; store line counter away in alternate registers.
       call dline          ; draw a line.
       exx                 ; restore old addresses.
       call dline          ; delete a line.
       exx                 ; flip to new sprite addresses.
       ex af,af'           ; restore line counter.
       dec a               ; one less to go.
       jp nz,sspri1
       ret


dline  ld a,(de)           ; graphic data.
       and c               ; mask away what's not needed.
       xor (hl)            ; XOR with what's there.
       ld (hl),a           ; bung it in.
       inc l               ; next screen address.
       inc l               ; next screen address.
       ld a,(de)           ; fetch data.
       and b               ; mask away unwanted bits.
       xor (hl)            ; XOR with what's there.
       ld (hl),a           ; bung it in.
       inc de              ; next graphic.
       dec l               ; one character cell to the left.
       ld a,(de)           ; second bit of data.
       xor (hl)            ; XOR with what's there.
       ld (hl),a           ; bung it in.
       inc de              ; point to next line of data.
       dec l               ; another char left.

; Line drawn, now work out next target address.

nline  inc h               ; increment pixel.
       ld a,h              ; get pixel address.
       and 7               ; straddling character position?
       ret nz              ; no, we're on next line already.
       ld a,h              ; get pixel address.
       sub 8               ; subtract 8 for start of segment.
       ld h,a              ; new high byte of address.
       ld a,l              ; get low byte of address.
       add a,32            ; one line down.
       ld l,a              ; new low byte.
       ret nc              ; not reached next segment yet.
       ld a,h              ; address high.
       add a,8             ; add 8 to next segment.
       ld h,a              ; new high byte.
       ret


maskad ld hl,spmask        ; pointer to mask table.
       ld c,a              ; low byte of table displacement.
       ld b,0              ; no high byte.
       add hl,bc           ; add displacement to pointer.
       ld c,(hl)           ; left mask.
       inc hl
       ld b,(hl)           ; right mask.
       ret


; This routine returns a screen address for (dispx, dispy) in hl.

scadd  ld a,(dispx)        ; returns screen address of coordinates
       ld l,a              ; (dispx, dispy) in hl.
       and 7               ; line 0-7 within character square.
       add a,64            ; 64 * 256 = 16384 (Start of screen display)
       ld h,a              ; line * 256.
       ld a,l
       rrca
       rrca
       rrca
       and 24              ; segment 0-2 multiplied by 8
       add a,h             ; add to h (so multiply by 8 * 256 = 2048)
       ld h,a
       ld a,l              ; 8 character squares per segment.
       rlca                ; divide x by 8 and multiply by 32,
       rlca                ; net calculation: multiply by 4.
       and 224             ; mask off bits we don't want.
       ld l,a              ; vertical coordinate calculation done.
       ld a,(dispy)        ; y coordinate.
       rrca                ; only need to divide by 8.
       rrca
       rrca
       and 31              ; squares 0 - 31 across screen.
       add a,l             ; add to total so far.
       ld l,a              ; hl = address of screen.
       ret


spmask defb 255,0,63,192,15,240,3,252

       
; Animates a sprite.

animsp ld a,(ix+6)         ; sprite image.
       call gfrm           ; get frame data.
       inc hl              ; point to frames.
       ld a,(ix+7)         ; sprite frame.
       inc a               ; next one along.
       cp (hl)             ; reached the last frame?
       jr c,anims0         ; no, not yet.
       xor a               ; start at first frame.
anims0 ld (ix+7),a         ; new frame.
       ret
animbk ld a,(ix+6)         ; sprite image.
       call gfrm           ; get frame data.
       inc hl              ; point to frames.
       ld a,(ix+7)         ; sprite frame.
       and a               ; first one?
       jr nz,rtanb0        ; yes, start at end.
       ld a,(hl)           ; last sprite.
rtanb0 dec a               ; next one along.
       jr anims0           ; set new frame.

; Check for collision with other sprite, strict enforcement.

sktyp  ld hl,sprtab        ; sprite table.
       ld a,NUMSPR         ; number of sprites.
sktyp0 ex af,af'           ; store loop counter.
       ld (skptr),hl       ; store pointer to sprite.
       ld a,(hl)           ; get sprite type.
       cp b                ; is it the type we seek?
       jr z,coltyp         ; yes, we can use this one.
sktyp1 ld hl,(skptr)       ; retrieve sprite pointer.
       ld de,TABSIZ        ; size of each entry.
       add hl,de           ; point to next sprite in table.
       ex af,af'           ; restore loop counter.
       dec a               ; one less iteration.
       jr nz,sktyp0        ; keep going until we find a slot.
       ld hl,0             ; default to ROM address - no sprite.
       ld (skptr),hl       ; store pointer to sprite.
       add a,1             ; don't return with zero flag set.
       ret                 ; didn't find one.
skptr  defw 0              ; search pointer.

coltyp ld de,X             ; distance to x position in table.
       add hl,de           ; point to coords.
       ld e,(hl)           ; fetch x coordinate.
       inc hl              ; now point to y.
       ld d,(hl)           ; that's y coordinate.

; Drop into collision detection.

colx16 ld a,(ix+X)         ; x coord.
       sub e               ; subtract x.
       add a,COLDST        ; add maximum distance.
       cp 31               ; within x range?
       jr nc,sktyp1        ; no - they've missed.
       ld a,(ix+Y)         ; y coord.
       sub d               ; subtract y.
colxa  add a,COLDST        ; add maximum distance.
colxb  cp 31               ; within y range?
       jr nc,sktyp1
       ret                 ; carry set if there's a collision.

; Multiply accumulator by 2 and add to hl.

addac2 rlca                ; multiply by 2.
       ld e,a              ; store in e.
       and 1               ; high byte bits.
       ld d,a              ; displacement high byte.
       ld a,e              ; restore shift result.
       and 254             ; low byte bits.
       ld e,a              ; displacement low.
       add hl,de           ; add to hl.
       ret


disply ld bc,displ0        ; display space.
       call num2ch         ; convert accumulator to string.
       dec bc              ; back one character.
       ld a,(bc)           ; fetch digit.
       or 128              ; insert end marker.
       ld (bc),a           ; new value.
       ld hl,displ0        ; display space.
       jp dmsg3            ; display the string.
displ0 defb 0,0,0,13+128


; Initialise screen.

initsc ld a,(roomtb)       ; whereabouts in the map are we?
       call tstsc          ; find displacement.
       cp 255              ; is it valid?
       ret z               ; no, it's rubbish.
       ld (scno),a         ; store new room number.
       ret

; Test screen.

tstsc  ld hl,mapdat-10     ; start of map data, subtract ten for negative.
       ld b,a              ; store room in b for now.
       add a,10            ; add ten in case we're negative.
       ld e,a              ; screen into e.
       ld d,0              ; zeroise d.
       add hl,de           ; add displacement to map data.
       ld a,(hl)           ; find room number there.
       ret

; Screen left.

scrl   ld a,(roomtb)       ; present room table pointer.
       dec a               ; room left.
scrl0  call tstsc          ; test screen.
       inc a               ; is there a screen this way?
       ret z               ; no, return to loop.
       ld a,b              ; restore room displacement.
       ld (roomtb),a       ; new room table position.
       call initsc         ; set new screen.
       ld hl,restfl        ; restart screen flag.
       ld (hl),2           ; set it.
       ret
scrr   ld a,(roomtb)       ; room table pointer.
       inc a               ; room right.
       jr scrl0
scru   ld a,(roomtb)       ; room table pointer.
       sub 10              ; room up.
       jr scrl0
scrd   ld a,(roomtb)       ; room table pointer.
       add a,10            ; room down.
       jr scrl0

; Jump to new screen.

nwscr  ld hl,mapdat        ; start of map data.
       ld bc,256*80        ; zero room count, 80 to search.
nwscr0 cp (hl)             ; have we found a match for screen?
       jr z,nwscr1         ; yes, set new point in map.
       inc hl              ; next room.
       inc c               ; count rooms.
       djnz nwscr0         ; keep looking.
       ret
nwscr1 ld a,c              ; room displacement.
       ld (roomtb),a       ; set the map position.
       ret


; Jump table.
; TG:JUMP TABLE
jtab    defb 234, 244, 247, 252
        defb 252, 254, 254, 254
        defb   0,   0,   0,   2
        defb   2,   2,   4,   4
        defb   8,  12,  16,  16
        defb  16,   0,  99
; TG:JUMP TABLE

; Gravity processing.

grav   ld l,(ix+13)        ; jump pointer low.
       ld h,(ix+14)        ; jump pointer high.
       ld a,l              ; low byte in accumulator.
       or h                ; merge in high byte.
       ret z               ; if neither is set, we're not in the air.
       ld a,(hl)           ; pixels to move.
       cp 99               ; reached the end?
       jr nz,grav0         ; no, continue.
       dec hl              ; go back to previous value.
       ld a,(hl)           ; fetch that from table.
grav0  inc hl              ; point to next table entry.
       ld (ix+13),l        ; store new pointer low.
       ld (ix+14),h        ; store new pointer high.
       and a               ; any movement required?
       ret z               ; no, not this time.
       cp 128              ; is it up or down?
       jr nc,gravu         ; it's up.
gravd  ld b,a              ; set pixels to move.
gravd0 push bc             ; sotre pixel count for now.
       call cangd          ; can we go down?
       pop bc              ; get pixel count again.
       jr nz,gravst        ; can't move down, so stop.
       inc (ix+8)          ; adjust new x coord.
       djnz gravd0
       ret
gravu  neg                 ; flip the sign so it's positive.
       ld b,a              ; set pixels to move.
gravu0 push bc             ; store pixel counter on stack.
       call cangu          ; can we go up?
       pop bc              ; retrieve pixel counter.
       jp nz,ifalls        ; can't move up, go down next.
       dec (ix+8)          ; adjust new x coord.
       djnz gravu0
       ret
gravst ld hl,0             ; null value in pointer.
       ld (ix+13),l        ; store new pointer low.
       ld (ix+14),h        ; store new pointer high.
       ret


; Initiate fall check.

ifall  ld l,(ix+13)        ; jump pointer low.
       ld h,(ix+14)        ; jump pointer high.
       ld a,l              ; low byte in accumulator.
       or h                ; merge in high byte.
       ret nz              ; if either is set, we're already in the air.
       ld a,(ix+8)         ; x coordinate.
       ld h,(ix+9)         ; y coordinate.
       add a,16            ; look down 16 pixels.
       ld l,a              ; coords in hl.
       ld (dispx),hl       ; set up test coordinates.
       call tstbl          ; get map address.
       call plchk          ; block, platform check.
       ret nz              ; it's solid, don't fall.
       inc hl              ; look right one cell.
       call plchk          ; block, platform check.
       ret nz              ; it's solid, don't fall.
       ld a,(dispy)        ; y coordinate.
       and 7               ; position straddling block cells.
       jr z,ifalls         ; no more checks needed.
       inc hl              ; look to third cell.
       call plchk          ; block, platform check.
       ret nz              ; it's solid, don't fall.

; We're falling.  Look for first movement down within table.

ifalls ld hl,jtab          ; jump table start.
ifal0  inc hl              ; point to next value.
       ld a,(hl)           ; fetch value.
       and a               ; is it moving at all?
       jr z,ifal0          ; no, get next value.
       cp 99               ; reached end of table?
       ret z               ; yes, don't fall.
       cp 128              ; is it going up?
       jr nc,ifal0         ; yes, looking for first movement down.
       ld (ix+13),l        ; set jump low.
       ld (ix+14),h        ; set jump high.
       ret



; Get frame data for a particular sprite.

gfrm   rlca                ; multiple of 2.
       ld e,a              ; copy to de.
       ld d,0              ; no high byte as max sprite is 128.
       ld hl,(frmptr)      ; frames used by game.
       add hl,de           ; point to frame start.
       ret

; Find sprite list for current room.

sprlst ld a,(scno)         ; screen number.
sprls2 ld hl,(nmeptr)      ; pointer to enemies.
       ld b,a              ; loop counter in b register.
       and a               ; is it the first screen?
       ret z               ; yes, don't need to search data.
       ld de,NMESIZ        ; bytes to skip.
sprls1 ld a,(hl)           ; fetch type of sprite.
       inc a               ; is it an end marker?
       jr z,sprls0         ; yes, end of this room.
       add hl,de           ; point to next sprite in list.
       jr sprls1           ; continue until end of room.
sprls0 inc hl              ; point to start of next screen.
       djnz sprls1         ; continue until room found.
       ret


; Clear all but a single player sprite.

nspr   ld b,NUMSPR         ; sprite slots in table.
       ld ix,sprtab        ; sprite table.
       ld de,TABSIZ        ; distance to next odd/even entry.
nspr0  ld a,(ix)           ; fetch sprite type.
       and a               ; is it a player?
       jr z,nspr1          ; yes, keep this one.
       ld (ix),255         ; delete sprite.
       ld (ix+5),255       ; remove next type.
       add ix,de           ; next sprite.
       djnz nspr0          ; one less space in the table.
       ret
nspr1  ld (ix),255         ; delete sprite.
       add ix,de           ; point to next sprite.
       djnz nspr2          ; one less to do.
       ret
nspr2  ld (ix),255         ; delete sprite.
       ld (ix+5),255       ; remove next type.
       add ix,de           ; next sprite.
       djnz nspr2          ; one less space in the table.
       ret


; Two initialisation routines.
; Initialise sprites - copy everything from list to table.

ispr   ld b,NUMSPR         ; sprite slots in table.
       ld ix,sprtab        ; sprite table.
ispr2  ld a,(hl)           ; fetch byte.
       cp 255              ; is it an end marker?
       ret z               ; yes, no more to do.
ispr1  ld a,(ix)           ; fetch sprite type.
       cp 255              ; is it enabled yet?
       jr nz,ispr4         ; yes, try another slot.
       ld a,(ix+5)         ; next type.
       cp 255              ; is it enabled yet?
       jr z,ispr3          ; no, process this one.
ispr4  ld de,TABSIZ        ; distance to next odd/even entry.
       add ix,de           ; next sprite.
       djnz ispr1          ; repeat for remaining sprites.
       ret                 ; no more room in table.
ispr3  call cpsp           ; initialise a sprite.
       djnz ispr2          ; one less space in the table.
       ret

; Initialise sprites - but not player, we're keeping the old one.

kspr   ld b,NUMSPR         ; sprite slots in table.
       ld ix,sprtab        ; sprite table.
kspr2  ld a,(hl)           ; fetch byte.
       cp 255              ; is it an end marker?
       ret z               ; yes, no more to do.
       and a               ; is it a player sprite?
       jr nz,kspr1         ; no, add to table as normal.
       ld de,NMESIZ        ; distance to next item in list.
       add hl,de           ; point to next one.
       jr kspr2
kspr1  ld a,(ix)           ; fetch sprite type.
       cp 255              ; is it enabled yet?
       jr nz,kspr4         ; yes, try another slot.
       ld a,(ix+5)         ; next type.
       cp 255              ; is it enabled yet?
       jr z,kspr3          ; no, process this one.
kspr4  ld de,TABSIZ        ; distance to next odd/even entry.
       add ix,de           ; next sprite.
       djnz kspr1          ; repeat for remaining sprites.
       ret                 ; no more room in table.
kspr3  call cpsp           ; copy sprite to table.
       djnz kspr2          ; one less space in the table.
       ret

; Copy sprite from list to table.

cpsp   ld a,(hl)           ; fetch byte from table.
       ld (ix),a           ; set up type.
       ld (ix+PAM1ST),a    ; set up type.
       inc hl              ; move to next byte.
       ld a,(hl)           ; fetch byte from table.
       ld (ix+6),a         ; set up image.
       inc hl              ; move to next byte.
       ld a,(hl)           ; fetch byte from table.
       ld (ix+3),200       ; set initial coordinate off screen.
       ld (ix+8),a         ; set up coordinate.
       inc hl              ; move to next byte.
       ld a,(hl)           ; fetch byte from table.
       ld (ix+9),a         ; set up coordinate.
       inc hl              ; move to next byte.
       ld a,(hl)           ; fetch byte from table.
       ld (ix+10),a        ; set up direction.
       inc hl              ; move to next byte.
       xor a               ; zeroes in accumulator.
       ld (ix+7),a         ; reset frame number.
;       ld (ix+12),a        ; reset parameter B.
       ld (ix+13),a        ; reset jump pointer low.
       ld (ix+14),a        ; reset jump pointer high.
       push ix             ; store ix pair.
       push hl             ; store hl pair.
       push bc
evis0  call evnt11         ; perform reset screen event.
       pop bc
       pop hl              ; restore hl.
       pop ix              ; restore ix.
       ld de,TABSIZ        ; distance to next odd/even entry.
       add ix,de           ; next sprite.
       ret

; Set palette routine and data.

setpal ld bc,48955         ; register select.
       ld a,64             ; mode select.
       out (c),a           ; set ULAplus mode.
       ld b,255            ; data write.
       ld a,1              ; mode on.
       out (c),a           ; switch on ULAplus.

       ld b,64             ; number of palette table entries to write.
setpa1 ld hl,palett        ; palette we want.
       ld e,0              ; register number.
setpa0 push bc             ; store counter.
       ld b,191            ; register select.
       ld a,e              ; register number to write.
       out (c),a           ; write to port.
       ld b,255            ; data select.
       ld a,(hl)           ; get colour data from table.
       out (c),a           ; write to port.
       inc e               ; next clut entry.
       inc hl              ; next table entry.
       pop bc              ; restore counter from stack.
       djnz setpa0         ; set rest of palette.
       ret

setedg ld a,(wintop)       ; top of window.
       ld b,a              ; copy to b.
       rlca                ; multiply by 8.
       rlca
       rlca
       ld (edget),a        ; set top of screen.
       ld a,(winhgt)       ; get height.
       add a,b             ; add height of window.
       rlca                ; multiply by 8.
       rlca
       rlca
       sub 16              ; subtract height of sprites.
       ld (edgeb),a        ; set bottom of screen.
       ld a,(winlft)       ; left of window.
       ld b,a              ; copy to b.
       rlca                ; multiply by 8.
       rlca
       rlca
       ld (edgel),a        ; set left of screen.
       ld a,(winwid)       ; get width.
       add a,b             ; add width of window.
       rlca                ; multiply by 8.
       rlca
       rlca
       sub 16              ; subtract height of sprites.
       ld (edger),a        ; set bottom of screen.
       ret

; ULAplus Palette.

; 48955 = register select port.
; 65339 = data read/write port.

; 48955 = write to register:
; d0-d5 : select register sub-group.
; d6-d7 : select register group.
;         00 = sub-group determines entry in palette table.
;         64 = mode select, write d0 to 65339 to toggle mode on/off.

; 65339 = data read/write:
; d0-d1 : blue intensity (last bit duplicated so Bb is Bbb)
; d2-d4 : red intensity
; d5-d7 : green intensity

; Ink colours.
;                            gggrrrbb

palett defb 0              ; 000000000 = black.
       defb 3              ; 100000111 = light blue.
       defb 28             ; 000101000 = dull red.
       defb 31             ; 000111100 = pink.
       defb 224            ; 111000011 = bluey green.
       defb 227            ; 000011100 = purple.
       defb 252            ; 100111000 = bright orange.
       defb 255            ; 111111011 = yellowish white.

; Paper colours.

       defb 0
       defb 3
       defb 28
       defb 31
       defb 224
       defb 227
       defb 252
       defb 255

; CLUT 2 ink.

       defb 0              ; 000000000 = black.
       defb 15             ; 000011111 = purple.
       defb 120            ; 001111000 = reddy orange.
       defb 128            ; 100000000 = dark green.
       defb 244            ; 111101000 = lime green.
       defb 195            ; 110000111 = cyan.
       defb 248            ; 111110000 = bright yellow.
       defb 255            ; 111111111 = bright white.

; CLUT 2 paper.

       defb 0
       defb 131            ; 100000111 = light blue.
       defb 28             ; 000111000 = bright red.
       defb 146            ; 100100100 = elephant grey.
       defb 224            ; 111000000 = green.
       defb 226            ; 111000100 = cyan.
       defb 252            ; 111111000 = yellow.
       defb 255            ; 111111111 = bright white.


; CLUT 3 ink.

       defb 0              ; 000000000 = black.
       defb 219            ; 110110111 = elephant grey.
       defb 28             ; 000111000 = bright red.
       defb 30             ; 000100000 = pink.
       defb 244            ; 111101000 = lime green.
       defb 44             ; 001011000 = brown.
       defb 252            ; 111111000 = bright yellow.
       defb 39             ; 001001111 = yellowish white.

; CLUT 3 paper.

       defb 0
       defb 109            ; 011011011 = dark grey.
       defb 28             ; 000111000 = bright red.
       defb 146            ; 100100100 = elephant grey.
       defb 16             ; 000100000 = red, dull.
       defb 131            ; 100000111 = light blue.
       defb 252            ; 111111000 = yellow.
       defb 240            ; 111100000 = yellow/green

; CLUT 4 ink.

       defb 226            ; 111000100 = cyan.
       defb 131            ; 100000111 = light blue.
       defb 20             ; 000101000 = dull red.
       defb 30             ; 000111100 = pink.
       defb 225            ; 111000011 = bluey green.
       defb 14             ; 000011100 = purple.
       defb 156            ; 100111000 = bright orange.
       defb 253            ; 111111011 = yellowish white.

; CLUT 4 paper.

       defb 0
       defb 131            ; 100000111 = light blue.
       defb 28             ; 000111000 = bright red.
       defb 146            ; 100100100 = elephant grey.
       defb 224
       defb 80             ; 010100000 = brown.
       defb 252
       defb 64             ; 010000000 = green.

endpal equ $


; Sprite table.
; ix+0  = type.
; ix+1  = sprite image number.
; ix+2  = frame.
; ix+3  = x coord.
; ix+4  = y coord.

; ix+5  = new type.
; ix+6  = new image number.
; ix+7  = new frame.
; ix+8  = new x coord.
; ix+9  = new y coord.

; ix+10 = direction.
; ix+11 = parameter 1.
; ix+12 = parameter 2.
; ix+13 = jump pointer low.
; ix+14 = jump pointer high.



sprtab equ $

       org sprtab + ( NUMSPR * TABSIZ )

; Map room table.
; TG:MAPROOMTABLE
maprtab defw 0
; TG:MAPROOMTABLE

; Map room data.
; TG:MAPROOMS
rmdat0  defb   0
; TG:MAPROOMS


       ; past top of map.
       defb 255, 255, 255, 255, 255, 255, 255, 255, 255, 255

; TG:MAPTABLE
mapdat defb   0
; TG:MAPTABLE

       ; past bottom of map.
       defb 255, 255, 255, 255, 255, 255, 255, 255, 255, 255

; current room
roomtb defb 0  

; Frame list.
; Starting frame number for each sprite image, followed by number of frames.
; TG:SPRITE FRAME LIST
frmlst defb 0,1
; TG:SPRITE FRAME LIST

; Screen sprite data.

; sprite type (event), sprite no (index into frmlst), y pixel position, x pixel position, sprite direction
; TG:MAP SPRITES
nmedat defb   0

; Sprites.

; TG:SPRITE DATA
sprgfx defb 0  
; TG:SPRITE DATA

; TG:TILES
; Character block graphics.
chgfx  defb 0

; Block colours.
bcol   defb 0

; Block properties.
bprop  defb 0
; TG:TILES

; Sounds.
; TG:SOUNDS
       ; Author: Jonathan Cauldwell
       ; Name: Sound Effect 1
       ; Description: Sound Effect 1
fx1    defb 128+15         ; volume and mixer.
       defb 31             ; white noise.
       defw 1000           ; tone register.
       defb 128+15         ; volume and mixer.
       defb 25             ; white noise.
       defw 1000           ; tone register.
       defb 128+14         ; volume and mixer.
       defb 19             ; white noise.
       defw 1000           ; tone register.
       defb 128+13         ; volume and mixer.
       defb 13             ; white noise.
       defw 1000           ; tone register.
       defb 128+12         ; volume and mixer.
       defb 7              ; white noise.
       defw 1000           ; tone register.
       defb 128+11         ; volume and mixer.
       defb 0              ; white noise.
       defw 1000           ; tone register.
       defb 128+10         ; volume and mixer.
       defb 6              ; white noise.
       defw 1000           ; tone register.
       defb 128+8          ; volume and mixer.
       defb 12             ; white noise.
       defw 1000           ; tone register.
       defb 128+6          ; volume and mixer.
       defb 18             ; white noise.
       defw 1000           ; tone register.
       defb 128+3          ; volume and mixer.
       defb 24             ; white noise.
       defw 1000           ; tone register.
       defb 255
       ; Author: Jonathan Cauldwell
       ; Name: Sound Effect 2
       ; Description: Sound Effect 2
fx2    defb 064+15         ; volume and mixer.
       defb 27             ; white noise.
       defw 1000           ; tone register.
       defb 064+14         ; volume and mixer.
       defb 31             ; white noise.
       defw 2000           ; tone register.
       defb 064+13         ; volume and mixer.
       defb 28             ; white noise.
       defw 3000           ; tone register.
       defb 064+12         ; volume and mixer.
       defb 31             ; white noise.
       defw 2000           ; tone register.
       defb 064+11         ; volume and mixer.
       defb 29             ; white noise.
       defw 1000           ; tone register.
       defb 064+10         ; volume and mixer.
       defb 31             ; white noise.
       defw 2000           ; tone register.
       defb 064+9          ; volume and mixer.
       defb 30             ; white noise.
       defw 3000           ; tone register.
       defb 064+8          ; volume and mixer.
       defb 31             ; white noise.
       defw 2000           ; tone register.
       defb 064+7          ; volume and mixer.
       defb 31             ; white noise.
       defw 1000           ; tone register.
       defb 064+6          ; volume and mixer.
       defb 31             ; white noise.
       defw 2000           ; tone register.
       defb 255
; TG:SOUNDS

; Messages.
; TG:STRINGS
msgdat defb 'GAME OVER', 141
       defb 'GET READY', 141
; TG:STRINGS

; Objects graphics and properties.
; TG:OBJECTS
objdat defw 0 
; TG:OBJECTS
       defb 99             ; temporary marker.

; Game-specific events code.
; TG:EVENTS
include "Player(Type0).asm"
include "Sprite(Type1).asm"
include "Sprite(Type2).asm"
include "Sprite(Type3).asm"
include "Sprite(Type4).asm"
include "Sprite(Type5).asm"
include "Sprite(Type6).asm"
include "Sprite(Type7).asm"
include "GameInitialisation.asm"
include "RestartScreen.asm"
include "InitialiseSprite.asm"
include "MainLoop1.asm"
include "MainLoop2.asm"
include "CompletedGame.asm"
include "KillPlayer.asm"
; TG:EVENTS


AAA_END defw $  ; work out where the end of used memory is
end START_ADDR  ; make pasmo auto start the .tap file
