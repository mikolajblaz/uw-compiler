CC=stack ghc --
# TODO change to ghc

SRC=$(wildcard src/*.hs)
LLVM_SRC=$(wildcard src/Llvm/*.hs) $(SRC)
LEX=src/ParLatte.hs src/LexLatte.hs
BINS=insc_llvm
RUNTIME=lib/runtime.bc

all: $(LEX) $(BINS) $(RUNTIME)

insc_llvm: $(LEX) $(LLVM_SRC)
	$(CC) -isrc --make src/Llvm/Compiler.hs -o insc_llvm

src/ParLatte.y src/LexLatte.x: src/Latte.cf
	cd src; /home/students/inf/PUBLIC/MRJP/bin/bnfc Latte.cf
	cp lib/ErrM.hs src/

src/ParLatte.hs: src/ParLatte.y
	cd src; happy -gca ParLatte.y

src/LexLatte.hs: src/LexLatte.x
	cd src; alex -g LexLatte.x

lib/runtime.bc:
	cd lib; llvm-as -o runtime.bc runtime.ll

run: all
	./insc_llvm ./tests/test.ins    # generates tests/in1.ll and tests/in1.bc
	lli tests/test.bc               # runtime.bc already linked

clean:
	-rm -f src/*.log src/*.aux src/*.hi src/*.o src/*.dvi
	-rm -f src/**/*.log src/**/*.aux src/**/*.hi src/**/*.o src/**/*.dvi
	-rm -f **/*.bc tests/*.ll
	-rm -f $(LEX)
	-rm -f $(BINS)

distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* Latte.dtd XMLLatte.* Makefile*

.PHONY: clean distclean
