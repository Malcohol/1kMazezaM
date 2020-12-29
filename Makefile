all: MazezaM.p

levels.asm: levels.mzm
	python mzm-tools/mzm-convert.py -z levels.mzm > levels.asm

MazezaM.p: MazezaM.asm levels.asm
	if !(z80asm -b MazezaM.asm); then cat MazezaM.err; fi
	mv -f MazezaM.bin MazezaM.p

clean:
	rm -f MazezaM.p MazezaM.o levels.asm
