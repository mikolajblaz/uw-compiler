SRC=$(wildcard src/*.hs)
JVM_SRC=$(wildcard src/Jvm/*.hs) $(SRC)
LLVM_SRC=$(wildcard src/Llvm/*.hs) $(SRC)
LEX=src/ParInstant.hs src/LexInstant.hs
BINS=insc_jvm insc_llvm
RUNTIME=lib/Runtime.class lib/runtime.bc

all: $(LEX) $(BINS) $(RUNTIME)

insc_jvm: $(LEX) $(JVM_SRC)
	ghc -isrc --make src/Jvm/Compiler.hs -o insc_jvm

insc_llvm: $(LEX) $(LLVM_SRC)
	ghc -isrc --make src/Llvm/Compiler.hs -o insc_llvm

src/ParInstant.y src/LexInstant.x: src/Instant.cf
	cd src; bnfc Instant.cf
	cp lib/ErrM.hs src/

src/ParInstant.hs: src/ParInstant.y
	cd src; happy -gca ParInstant.y

src/LexInstant.hs: src/LexInstant.x
	cd src; alex -g LexInstant.x

lib/Runtime.class:
	cd lib; java -jar jasmin.jar Runtime.j

lib/runtime.bc:
	cd lib; llvm-as -o runtime.bc runtime.ll

run: all
	./insc_jvm ./tests/test.ins     # generates tests/in1.j and tests/in1.class
	java -cp lib:tests test         # runs program, needs Runtime.class, hence lib in classpath
	./insc_llvm ./tests/test.ins    # generates tests/in1.ll and tests/in1.bc
	lli tests/test.bc               # runtime.bc already linked

clean:
	-rm -f src/*.log src/*.aux src/*.hi src/*.o src/*.dvi
	-rm -f src/**/*.log src/**/*.aux src/**/*.hi src/**/*.o src/**/*.dvi
	-rm -f **/*.class tests/*.j
	-rm -f **/*.bc tests/*.ll
	-rm -f $(LEX)
	-rm -f $(BINS)

distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* ComposOp.* Instant.dtd XMLInstant.* Makefile*

.PHONY: clean distclean
