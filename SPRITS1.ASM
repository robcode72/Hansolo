;-----------------------------------------------------------------------
;
; GRAFNO:	Imprime gr�ficos del tama�o indicado m�s sus atributos correspondientes.
; Esta rutina se emplear� para los gr�ficos de cualquier tama�o a excepci�n de los tiles
; El gr�fico deber� estar precedido de 3 bytes que indican:
;		1: Alto en pixels del gr�fico
;		2: Alto en caracteres.
;		3: Ancho en caracteres.
;
; Entrada:	D: L�nea
;		E: Columna
;		HL: Direcci�n del gr�fico en memoria.
;
; La rutina se encarga de poner el ancho y alto del gr�fico en su lugar correspondiente:
;		GRAFNOAL+1: Alto del gr�fico en pixels.
;		GRAFNO03+1: Alto del gr�fico en caracteres.
;		GRAFNOAN+1 y GRAFNO05+1: Ancho del gr�fico en caracteres.

GRAFNO:		ld a,(hl)
		ld (GRAFNOAL+1), a
		inc hl
		ld a,(hl)
		ld (GRAFNO03+1),a
		inc hl
		ld a,(hl)
		ld (GRAFNOAN+1),a
		ld (GRAFNO05+1),a
		inc hl
		push de
		call DIRDF
GRAFNOAL:	ld b, 0 ; Alto del gr�fico en pixels.
GRAFNO01:	push bc
GRAFNOAN:	ld b,0 ; Ancho del gr�fico en caracteres
		push de
GRAFNO02:	ld a, (hl)
		ld (de), a
		inc hl
		inc de
		djnz GRAFNO02
		pop de
		call SIGL
		pop bc
		djnz GRAFNO01
		pop de ; imprimir los atributos.
		push hl
		call DIRATR
		pop de
GRAFNO03:	ld b, 0 ; Alto del gr�fico en caracteres.
GRAFNO04:	push bc
		push hl
GRAFNO05:	ld b, 0 ; Ancho del gr�fico en caracteres.
GRAFNO06:	ld a,(de)
		ld (hl),a
		inc hl
		inc de
		djnz GRAFNO06
		pop hl
		ld bc, 32
		add hl, bc
		pop bc
		djnz GRAFNO04
		ret

; FIN GRAFNO
;
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;
; DIRATR: CALCULA LA DIRECCI�N EN EL ATTR EN BASE A COL Y FIL EN BAJA RESOLUCI�N.
;
; Entrada:	D: L�nea.
;		E: Columna,
; Salida:	HL: Direcci�n de memoria del atributo.

DIRATR:	ld l,d
	ld h,0
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld b,88
	ld c,e
	add hl, bc
	ret

; FIN DIRATR
;
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;
;SIGL: Calcula la direcci�n en pantalla de la siguiente l�nea a una dada.
;
;	Entrada:	DE: direcci�n de memoria en la pantalla actual.
;	Salida:		DE: direcci�n de la siguiente l�nea.

SIGL:   INC     D
        LD      A,D 
        AND     7 
        RET     NZ 
        LD      A,E 
        ADD     A,32 
        LD      E,A 
        RET     C 
        LD      A,D 
        SUB     8 
        LD      D,A 
        RET

; FIN SIGL
;
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;
; DIRDF: CALCULA LA DIRECCI�N DE PANTALLA EN BASE A COL Y FIL EN BAJA RESOLUCI�N.
;
; Entrada:	D: L�nea
;		E: Columna.
; Salida:	DE: Direcci�n de pantalla

DIRDF:	ld a,d
	and 24
	add a, 64
	ld b,a
	ld a,d
	rrca
	rrca
	rrca
	and 224
	add a,e
	ld e,a
	ld d,b
	ret

; FIN DIRDF
;
;-----------------------------------------------------------------------

