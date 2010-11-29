.PHONY: all
all : parsers

.PHONY: parsers clean-parsers
parsers:
	make -C parsers -f Makefile all
clean-parsers:
	make -C parsers -f Makefile clean


.PHONY: clean
clean: clean-parsers
