#!/bin/bash

ghc -threaded -o chip8 main.hs emu.hs op.hs
if [ $# -ge 1 ]; then
	if [ $1 == "-r" ] || [ $1 == "--run" ]; then
		./chip8
	fi
fi
