test.com: test.asm
	nasm -o $@ -f bin $<

size.com: size.asm
	nasm -o $@ -f bin $<

.PHONY: clean
clean:
	rm -f test.com size.com
