; vi:set filetype=nasm ts=8:
	org 100h
	bits 16

	section .text
	jmp main

%define VMEM_WIN_SIZE	65536
	section .data
f255:	dd 255.0

	section .bss
winoffs: resd 1
px:	resd 1
py:	resd 1
tmp:	resd 1
	
	section .text
main:
	finit

	mov ax, 4f02h	; set video mode
	mov bx, 10eh	; 320x200x16 (565)
	int 10h

	push word 0a000h
	pop es

mainloop:
	xor eax, eax
	mov [winoffs], eax
	call set_vmem_win	; reset vmem window
	xor di, di		; di will be the vmem pointer within the window
	mov ecx, VMEM_WIN_SIZE	; ecx will count down the vmem window bytes
	xor ebx, ebx		; ebx: y
yloop:
	mov [py], ebx
	xor eax, eax		; eax: x
xloop:
	push ecx		; save byte counter
	mov [px], eax
	xor eax, eax

	;fild dword [py]
	;fild dword [px]
	;call calc_pixel	; args: x(0), y(1). returns rgb(0,1,2)
	; red result
	;fmul dword [f255]
	fld dword [f255]
	fistp dword [tmp]
	fwait
	mov [tmp], eax
	shl ax, 8
	and ax, 0f800h
	; green result
	;fmul dword [f255]
	;fld dword [f255]
	;fistp dword [tmp]
	fwait
	;mov [tmp], ebx
	;shl bx, 3
	;and bx, 7e0h
	;or ax, bx
	; blue result
	;fldz
	;fmul dword [f255]
	;fistp dword [tmp]
	fwait
	;mov [tmp], ebx
	;shr bx, 3
	;or ax, bx
	;mov ax, 0f800h

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
	jnz xloop
	; end of xloop

	mov ebx, [py]
	inc ebx			; y++
	cmp ebx, 200
	jnz yloop
	; end of yloop
	
	; check for keypress and loop back if there isn't one
	push eax
	in al, 60h
	mov dl, al
	pop eax
	dec dl
	jnz mainloop

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
	ffree st0
	ffree st1
	fldz		; blue = 0
	fld1		; green = 1
	fld1		; red = 1
	ret
