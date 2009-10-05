# rfk86:Makefile - "build system"

prog = rfk86

default: $(prog).86p

.PHONY : default clean

$(prog).86p: rfk86.asm messages.bin font.bin logo-1.bin victory.bin
	./ti86asm.pl $<

messages.bin: messages.txt
	./messages.pl

font.bin: font.png
	./font.pl

logo-1.bin: logo.png
	./image.pl logo

victory.bin: victory.png
	./image.pl victory bw

clean:
	$(RM) $(prog).86p $(prog).bin $(prog).sym
	$(RM) messages.inc messages.bin
	$(RM) font.bin logo-1.bin logo-2.bin victory.bin
