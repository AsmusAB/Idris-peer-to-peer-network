IDRIS ?= idris
PKG   := peer

build:
	  gcc c/ip.c -c -o c/ip.o
	  $(IDRIS) --build ${PKG}.ipkg

clean:
	  $(IDRIS) --clean ${PKG}.ipkg

rebuild: clean build

install: build
		$(IDRIS) Main.idr -o Peer -p contrib

run:
	./Peer