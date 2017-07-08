#!/bin/bash

function compile() {
	echo build.sh: Compiling...
	ghc -O -threaded -o chip8 main.hs emu.hs op.hs
}

if [ $# -ge 1 ]; then
	if [ $1 == "-c" ] || [ $1 == "--clean" ]; then
		echo build.sh: Cleaning compiled files...
		rm -f main.hi main.o emu.hi emu.o op.hi op.o chip8
		exit
	fi

	compile
	if [ $1 == "-r" ] || [ $1 == "--run" ]; then
		echo build.sh: Running...
		./chip8
	fi
else
	compile
fi
