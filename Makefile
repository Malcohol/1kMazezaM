PYTHON = python3
# Important: There are a few programs with the name z80asm.
# This one is part of the z88dk compiler suite.
Z80ASM = z80asm

all: MazezaM.p

levels.asm: levels.mzm
	$(PYTHON) mzm-tools/mzm-convert.py -z -o levels.asm levels.mzm

MazezaM.p: MazezaM.asm levels.asm
	if !($(Z80ASM) -b MazezaM.asm); then cat MazezaM.err; fi
	mv -f MazezaM.bin MazezaM.p

clean:
	rm -f MazezaM.p MazezaM.o levels.asm
