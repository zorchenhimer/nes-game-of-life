.PHONY: all env clean chr
.PRECIOUS: images/%.bmp

CHRUTIL = ../go-nes/bin/chrutil
METATILES = ../go-nes/bin/metatiles

NAME=gol
NESCFG = nes_nrom.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = \
	main.asm \
	neighbors.inc

CHR = images/main.chr

all: bin/ bin/$(NAME).nes

send: all
	./edlink bin/$(NAME).nes

bin/:
	-mkdir bin

bin/%.nes: bin/%.o
	ld65 $(LDFLAGS) -o $@ $^

bin/%.o: $(SOURCES) $(CHR)
	ca65 $(CAFLAGS) -o $@ main.asm

images/%.chr: images/%.bmp
	$(CHRUTIL) $< -o $@

images/%.bmp: images/%.aseprite
	aseprite -b $< --save-as $@

neighbors.inc: generate-neighbors.go
	go run $<
