#Makefile
#Haskell

GHC = ghc
FILES = Pixels.hs Effects.hs led-display.hs
OBJS = Pixels.o Effects.o led-display.o

all:
	$(GHC) --make led-display.hs
	\rm -f *.hi *.o *.*~
	
led-display: clean $(OBJS)
	$(GHC) -o $@ $(OBJS)

%.o: %.hs
	$(GHC) -c $<

clean:
	\rm -f *.hi *.o *.*~ led-display
