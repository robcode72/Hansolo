;-----------------------------------------------------------------------
;
; GRAFNO:	Imprime gráficos del tamaño indicado más sus atributos correspondientes.
; Esta rutina se empleará para los gráficos de cualquier tamaño a excepción de los tiles
; El gráfico deberá estar precedido de 3 bytes que indican:
;		1: Alto en pixels del gráfico
;		2: Alto en caracteres.
;		3: Ancho en caracteres.
;
; Entrada:	D: Línea
;		E: Columna
;		HL: Dirección del gráfico en memoria.
;
; La rutina se encarga de poner el ancho y alto del gráfico en su lugar correspondiente:
;		GRAFNOAL+1: Alto del gráfico en pixels.
;		GRAFNO03+1: Alto del gráfico en caracteres.
;		GRAFNOAN+1 y GRAFNO05+1: Ancho del gráfico en caracteres.

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
GRAFNOAL:	ld b, 0 ; Alto del gráfico en pixels.
GRAFNO01:	push bc
GRAFNOAN:	ld b,0 ; Ancho del gráfico en caracteres
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
GRAFNO03:	ld b, 0 ; Alto del gráfico en caracteres.
GRAFNO04:	push bc
		push hl
GRAFNO05:	ld b, 0 ; Ancho del gráfico en caracteres.
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
; DIRATR: CALCULA LA DIRECCIÓN EN EL ATTR EN BASE A COL Y FIL EN BAJA RESOLUCIÓN.
;
; Entrada:	D: Línea.
;		E: Columna,
; Salida:	HL: Dirección de memoria del atributo.

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
;SIGL: Calcula la dirección en pantalla de la siguiente línea a una dada.
;
;	Entrada:	DE: dirección de memoria en la pantalla actual.
;	Salida:		DE: dirección de la siguiente línea.

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
; DIRDF: CALCULA LA DIRECCIÓN DE PANTALLA EN BASE A COL Y FIL EN BAJA RESOLUCIÓN.
;
; Entrada:	D: Línea
;		E: Columna.
; Salida:	DE: Dirección de pantalla

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

