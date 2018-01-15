CC=ghc
SRC=$(wildcard src/*.hs) $(wildcard src/Llvm/*.hs)
LEX=src/ParLatte.hs src/LexLatte.hs
BINS=latc_llvm latc
RUNTIME=lib/runtime.bc

all: $(LEX) $(BINS) $(RUNTIME)

latc: latc_llvm
	ln -s latc_llvm latc

latc_llvm: $(LEX) $(SRC)
	$(CC) -isrc --make src/Llvm/CompilerIO.hs -o latc_llvm

src/ParLatte.y src/LexLatte.x: src/Latte.cf
	cd src; /home/students/inf/PUBLIC/MRJP/bin/bnfc --functor Latte.cf
	cp lib/ErrM.hs src/

src/ParLatte.hs: src/ParLatte.y
	cd src; happy -gca ParLatte.y

src/LexLatte.hs: src/LexLatte.x
	cd src; alex -g LexLatte.x

lib/runtime.bc: lib/runtime.ll
	cd lib; llvm-as -o runtime.bc runtime.ll

test: all
	./test.sh tests/official/good/

clean:
	-rm -f src/*.log src/*.aux src/*.hi src/*.o src/*.dvi
	-rm -f src/**/*.log src/**/*.aux src/**/*.hi src/**/*.o src/**/*.dvi
	-rm -f **/*.bc tests/*.ll
	-rm -f $(LEX)
	-rm -f $(BINS)

distclean: clean
	cd src; rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* Latte.dtd XMLLatte.*

.PHONY: clean distclean
