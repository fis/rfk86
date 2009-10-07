# rfk86:Makefile - "build system"

# Copyright (c) 2009, Heikki Kallasjoki.
# All rights reserved.
# License terms: see README.txt, "Legal Disclaimer".

prog = rfk86

default: $(prog).86p

.PHONY : default clean

$(prog).86p: rfk86.asm messages.bin font.bin logo-1.bin victory.bin
	./ti86asm.pl $<

messages.bin: messages.txt messages.pl
	./messages.pl

font.bin: font.png font.pl
	./font.pl

logo-1.bin: logo.png image.pl
	./image.pl logo

victory.bin: victory.png image.pl
	./image.pl victory bw

clean:
	$(RM) $(prog).86p $(prog).bin $(prog).sym
	$(RM) messages.bin messages.inc huffman.inc
	$(RM) font.bin logo-1.bin logo-2.bin victory.bin
