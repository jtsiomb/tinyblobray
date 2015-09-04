	org 100h

	mov ah, 4fh
	mov al, 2	; set video mode
	mov bx, 10eh	; 320x200x16 (565)
	int 10h

	push word 0a000h
	pop es
	xor di, di
	mov ecx, 16000
	mov eax, 0ffffffffh
	rep stosd

kbwait:
	in al, 60h
	dec al
	jnz kbwait

	mov ah, 4fh
	mov al, 2
	mov bx, 3
	int 10h

	mov ax, 4c00h
	int 21h
