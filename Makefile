GHCFLAGS = -O3

.PHONY: all
all: wsa

wsa: Main.hs Trans.hs
	ghc $(GHCFLAGS) -o wsa Main

.PHONY: clean
clean:
	rm -f wsa *.hi *.o
