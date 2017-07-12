# curly-succotash
Simple Chip-8 Emulator written in Haskell. Written to lean some Haskell, and as [an example emulator](https://nivent.github.io/blog/demystifying-emulators/). It was not written to be particularly fast, good, or user-friendly.

## How to Use
Assuming you have [Haskell](https://www.haskell.org/) installed, the project comes with a [shell script](https://github.com/NivenT/curly-succotash/blob/master/build.sh) that makes it easy to build and run the program. The usage is
```shell
./build.sh                  #Builds the program
./build.sh (-r | --run)     #Build and run the program
./build.sh (-c | --clean)   #Delete any compiled (.hi or .o) files
```
If you want to play a specific game then edit [this line](https://github.com/NivenT/curly-succotash/blob/master/main.hs#L17) in the code.

## Controls
The Chip-8 keyboard is pictured below

1 | 2 | 3 | C
--| --| --|--
4 | 5 | 6 | D
7 | 8 | 9 | E
A | 0 | B | F

These keys are mapped onto the following

1 | 2 | 3 | 4
--| --| --|--
Q | W | E | R
A | S | D | F
Z | X | C | V
