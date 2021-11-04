
all: 
	make -C src

clean:
	rm -f ./insc_llvm ./insc_jvm
	make clean -C src