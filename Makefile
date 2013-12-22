#Makefile
#Haskell

GHC = ghc
FILE = file.hs
OBJ = file.o
DEP = file.hi

main: $(FILE)
	$(GHC) $(FILE) -o $@

clean:
	\rm -f *.hi *.o *.*~ main
