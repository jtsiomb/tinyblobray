; vi:set filetype=nasm ts=8:
	org 100h
	bits 16

	section .text
	jmp main

%define VMEM_WIN_SIZE	65536
%define MAX_STEPS	64

	section .data
f2	dd 2.0
f255	dd 255.0
f160	dd 160.0
f100	dd 100.0
faspect	dd 1.333333
dthres	dd 0.01
farclip	dd 20.0

	section .bss
winoffs	resd 1
px	resd 1
py	resd 1
tmp	resd 1
rdir	resd 3
rpos	resd 3
	
	section .text
main:
	mov eax, cr0		; Start probe, get CR0
	and eax, 0fffffff3h	; clear TS and EM to force fpu access
	mov cr0, eax		; store control word
	finit

	mov ax, 4f02h	; set video mode
	mov bx, 10eh	; 320x200x16 (565)
	int 10h

	push word 0a000h
	pop es

.mainloop:
	xor eax, eax
	mov [winoffs], eax
	call set_vmem_win	; reset vmem window
	xor di, di		; di will be the vmem pointer within the window
	mov ecx, VMEM_WIN_SIZE	; ecx will count down the vmem window bytes
	xor ebx, ebx		; ebx: y
.yloop:
	mov [py], ebx
	xor eax, eax		; eax: x
.xloop:
	push ecx		; save byte counter
	mov [px], eax
	xor eax, eax

	fild dword [py]
	fild dword [px]
	call calc_pixel	; args: x(0), y(1). returns rgb(0,1,2)
	; red result
	fmul dword [f255]
	fistp dword [ds:tmp]
	mov eax, [tmp]
	shl ax, 8
	and ax, 0f800h
	; green result
	fmul dword [f255]
	fistp dword [tmp]
	mov ebx, [tmp]
	shl bx, 3
	and bx, 7e0h
	or ax, bx
	; blue result
	fmul dword [f255]
	fistp dword [tmp]
	mov ebx, [tmp]
	shr bx, 3
	or ax, bx

	mov [es:di], ax		; write packed pixel
	inc di
	inc di

	; end of inner loop, check if we need to move the vmem window
	pop ecx			; restore byte counter
	sub ecx, 2		; (ecx counts bytes, 2 bytes per pixel)
	jnz .skip_winmove
	; increment winoffs and call set_vmem_win with it
	mov eax, [winoffs]
	inc eax
	call set_vmem_win
	mov [winoffs], eax	; update winoffs memory with new value
	xor di, di		; reset the vmem pointer
	mov ecx, VMEM_WIN_SIZE	; reset the counter
.skip_winmove:

	mov eax, [px]
	inc eax			; x++
	cmp eax, 320
	jnz .xloop
	; end of xloop

	mov ebx, [py]
	inc ebx			; y++
	cmp ebx, 200
	jnz .yloop
	; end of yloop
	
	; check for keypress and loop back if there isn't one
	push eax
	in al, 60h
	mov dl, al
	pop eax
	dec dl
	jnz .mainloop

	; restore video mode
	mov ah, 4fh
	mov al, 2
	mov bx, 3
	int 10h

	; exit
	mov ax, 4c00h
	int 21h

; set_vmem_win sets the window offset
set_vmem_win:
	mov dx, ax		; window offset dx <- arg
	mov ax, 4f05h		; select window
	xor bx, bx		; window A
	int 10h
	ret

; calc_pixel(x [st0], y [st1]) -> r [st0], g [st1], b [st2]
calc_pixel:
	call calc_prim_dir
	xor eax, eax
	mov [rpos], eax
	mov [rpos + 4], eax
	mov [rpos + 8], eax

	mov ecx, MAX_STEPS
	fldz	; push dummy prev distance
.steploop:
	fstp st0 ; pop previous iteration distance
	; if pos.z > farclip break
	fld dword [rpos + 8]
	;fld dword [farclip]
	;fcomip st0, st1
	;jb .escape
	fcom dword [farclip]
	fstsw ax
	fwait
	sahf
	ja .escape

	; calculate distance field and break if below threshold
	fld dword [rpos + 4]
	fld dword [rpos]
	call distfield
	;fld dword [dthres]
	;fcomip st0, st1
	;ja .done
	fcom dword [dthres]
	fstsw ax
	fwait
	sahf
	jb .done

	; not good enough, step some more
	; posx = posx + dirx * dist
	fld dword [rdir]	; { dirx, dist }
	fmul st0, st1		; { dirx * dist, dist }
	fld dword [rpos]	; { posx, dirx * dist, dist }
	fadd			; { posx + dirx * dist, dist }
	fstp dword [rpos]
	; posy = posy + diry * dist
	fld dword [rdir + 4]
	fmul st0, st1
	fld dword [rpos + 4]
	fadd
	fstp dword [rpos + 4]
	; posz = posz + dirz * dist
	fld dword [rdir + 8]
	fmul st0, st1
	fld dword [rpos + 8]
	fadd
	fstp dword [rpos + 8]

	dec ecx
	jnz .steploop
.escape:
	; didn't hit anything, return black
	fstp st0
	fldz
	fldz
	fldz
	ret
.done:
	; success (distance is still in st0)
	fld dword [rpos + 8]
	fld dword [rpos + 4]
	fld dword [rpos]
	call shade
	ret

; calc_prim_dir(x [st0], y [st1]): sets global rdir vector
calc_prim_dir:
	; x = 1.3333 * (px / 160.0 - 1.0)
	fdiv dword [f160]	; st0 <- px / 160.0
	fld1
	fsub			; st0 <- st0 - 1.0
	fmul dword [faspect]	; st0 <- st0 * aspect
	fxch			; exchange with st1 to work on py now
	; y = 1.0 - (py / 100.0)
	fdiv dword [f100]	; st0 <- py / 100.0
	fld1
	fsubr			; st0 <- 1.0 - st0
	; z = 1.0 / tan(50deg / 2.0) ~ 2.14... ~ PI - 1
	fld1
	fldpi
	fsubr
	; we end up with st {z, y, x}
	fxch st2		; put them in the correct order
	call normalize
	fstp dword [rdir]	; store x
	fstp dword [rdir + 4]	; store y
	fstp dword [rdir + 8]	; store z
	ret

; length(x [st0], y [st1], z [st2]): returns length in st0
length:
	fld st0
	fmul		; square x {x*x, y, z}
	fxch
	fld st0
	fmul		; square y {y*y, x*x, z}
	fadd		; st0 <- x*x+y*y (st1 is z now)
	fxch
	fld st0
	fmul		; square z {z*z, y*y+x*x}
	fadd		; st0 <- x*x+y*y+z*z
	fsqrt
	ret

; normalize(x [st0], y [st1], z [st2]): normalize in place
normalize:
	; push a copy of the vector to work with
	fld st2		; push z
	fld st2		; push y
	fld st2		; push x
	call length	; now st{len, x, y, z}
	fld1
	fdivr		; {1/len, x, y, z}
	fxch st3	; {z, x, y, 1/len}
	fmul st0, st3	; {z/len, x, y, 1/len}
	fxch st2	; {y, x, z/len, 1/len}
	fmul st0, st3	; {y/len, x, z/len, 1/len}
	fxch st1	; {x, y/len, z/len, 1/len}
	fmul st0, st3	; {x/len, y/len, z/len, 1/len}
	ffree st3
	ret

; distfield(x, y, z): (st0, st1, st2) -> st0
distfield:
	fld dword [ballpos]	; {bx, x, y, z}
	fsub			; {x - bx, y, z}
	fxch st1		; {y, x - bx, z}
	fld dword [ballpos + 4]	; {y, by, x - bx, z}
	fsub			; {y - by, x - bx, z}
	fxch st2		; {z, x - bx, y - by}
	fld dword [ballpos + 8]	; {bz, z, x - bx, y - by}
	fsub			; {z - bz, x - bx, y - by}
	call length		; {len}
	fld dword [ballrad]	; {rad, len}
	fsub			; {len - rad}
	ret

; shade(x, y, z, dist): (st0, st1, st2, st3) -> [st0, st1, st2]
shade:
	; TODO
	fstp st0
	fstp st0
	fstp st0
	fstp st0
	fld1
	fldz
	fld1
	ret

;	section .bss
;ballpos resd 3
	section .data
ballpos	dd 0.0
	dd 0.0
	dd 8.0
ballrad dd 1.0
