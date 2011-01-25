.PHONY: all
all: Map

.PHONY : clean
clean: clean-Map


# Rewrite Map

.PHONY: Map clean-Map
Map:
	make -C Map -f Makefile all
clean-Map:
	make -C Map -f Makefile clean
