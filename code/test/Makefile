.PHONY: all
all : parsers pp

.PHONY: parsers clean-parsers
parsers:
	make -C parsers -f Makefile all
clean-parsers:
	make -C parsers -f Makefile clean

.PHONY: pp clean-pp
parsers:
	make -C pp -f Makefile all
clean-parsers:
	make -C pp -f Makefile clean

.PHONY: clean
clean: clean-parsers clean-pp
