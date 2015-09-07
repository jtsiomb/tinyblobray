obj = main.o test.o
bin = test

CFLAGS = -pedantic -Wall -g `pkg-config --cflags sdl`
LDFLAGS = `pkg-config --libs sdl`

$(bin): $(obj)
	$(CC) -o $@ $(obj) $(LDFLAGS)

test.o: ../test.asm
	nasm -o $@ -DHOSTED -f elf64 -g $<

.PHONY: clean
clean:
	rm -f $(obj) $(bin)
