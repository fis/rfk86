# rfk86:Makefile - "build system"

# Copyright (c) 2009, Heikki Kallasjoki.
# All rights reserved.
# License terms: see README.txt, "Legal Disclaimer".

prog = rfk86

rel := $(shell git describe --tags --always | sed -e 's/-.*//')
rev := $(shell git rev-parse $(rel))

relnum := $(patsubst v%,%,$(rel))
pr := $(prog)-$(relnum)

default: $(prog).86p

.PHONY : default clean web web-clean

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

# website maintenance tasks, only for git checkouts

ifdef rel

web: $(prog).86p web-clean
	mkdir -p web/out
	(cd web ; ./build.pl)
	cp $(prog).86p web/rfk86.css web/rfk86.ttf web/rfk86.eot web/*.png web/out/
	git archive --format=tar --prefix=$(pr)/ $(rel) | tar x -C web/out
	$(RM) -r web/out/$(pr)/web
	sed -re 's/\.GIT {37}/.$(rev)/' < README.txt > web/out/$(pr)/README.txt
	tar zcf web/out/$(pr).tar.gz -C web/out $(pr)
	$(RM) -r web/out/$(pr)

web-clean:
	$(RM) -r web/out

endif