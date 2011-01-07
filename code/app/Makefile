.PHONY: all
all : todo size GaaSomEnDatalog

.PHONY: todo clean-todo
todo:
	make -C todo -f Makefile all
clean-todo:
	make -C todo -f Makefile clean

.PHONY: size clean-size
size:
	make -C size -f Makefile all
clean-size:
	make -C size -f Makefile clean

.PHONY: GaaSomEnDatalog clean-GaaSomEnDatalog
GaaSomEnDatalog:
	make -C GaaSomEnDatalog -f Makefile all
clean-GaaSomEnDatalog:
	make -C GaaSomEnDatalog -f Makefile clean


.PHONY : clean
clean: clean-todo clean-size clean-GaaSomEnDatalog

