test.com: test.asm
	nasm -o $@ -f bin $<

.PHONY: clean
clean:
	rm -f test.com
