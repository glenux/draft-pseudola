all: build

build: 
	corebuild parser.native
	@#cp -L datalove.native bin/datalove

clean:
	corebuild -clean

edit:
	$(EDITOR) *.ml* *.miniml


