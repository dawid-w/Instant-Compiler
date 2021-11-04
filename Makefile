
all: 
	make -C src

clean:
	rm ./insc_llvm ./insc_jvm
	rm ./src/*.hi
	rm ./src/*.o