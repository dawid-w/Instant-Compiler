all: insc_jvm insc_llvm

insc_jvm: jvm.hs
	ghc jvm.hs -o ../insc_jvm

insc_llvm: llvm.hs
	ghc llvm.hs -o ../insc_llvm

clean:
	rm -f ./*.hi
	rm -f ./*.o
