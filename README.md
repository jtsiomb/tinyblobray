Tiny metaball ray-marching in x86/x87 assembly
==============================================

About
-----
A size-coding experiment, to see if I could write a 3D metaball raytracer in 256
bytes (spoilers: I can't, it ended up around 1k). It runs in 16bit real mode
under DOS, although `exit` is the only DOS system call used, and the program is
compiled as a 16bit flat binary, so DOS is used just for the convenience of
loading and running it.

This program runs in 320x200 16bpp using VESA (VBE) 1.2, which should work on
most graphics cards out there.

License
-------
Author: John Tsiombikas <nuclear@member.fsf.org>
I release this code as public domain. Feel free to use it any way you see fit.

Instructions
------------
To build the program you need nasm, and an invocation of the form:
`nasm -o test.com -f bin test.asm` (see included GNU makefile). There's also a
RUN shellscript which uses dosbox to run the resulting binary, but you could
also move it to a proper DOS machine and run it there (warning: it will be
slow on old machines, it is real-time raytracing).

I've also included an SDL scaffolding with a GNU/Linux makefile which builds the
assembly code in "hosted" mode (i.e. without all the DOS/VBE parts), and can be
used to test (most) of the code more easily. It's all in the test directory, so
go there and type make to build that.
