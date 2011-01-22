.PHONY: all
all : todo size GaaSomEnDatalog NormalForm

# Todo

.PHONY: todo clean-todo
todo:
	make -C todo -f Makefile all
clean-todo:
	make -C todo -f Makefile clean

# Size

.PHONY: size clean-size
size:
	make -C size -f Makefile all
clean-size:
	make -C size -f Makefile clean

# GaaSomEnDatalog

.PHONY: GaaSomEnDatalog clean-GaaSomEnDatalog
GaaSomEnDatalog:
	make -C GaaSomEnDatalog -f Makefile all
clean-GaaSomEnDatalog:
	make -C GaaSomEnDatalog -f Makefile clean

# NormalForm

.PHONY: NormalForm clean-NormalForm
NormalForm:
	make -C NormalForm -f Makefile all
clean-NormalForm:
	make -C NormalForm -f Makefile clean



.PHONY : clean
clean: clean-todo clean-size clean-GaaSomEnDatalog clean-NormalForm

