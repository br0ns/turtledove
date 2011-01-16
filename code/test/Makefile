.PHONY: all
all : parsers pp NormalForm

# Parsers

.PHONY: parsers clean-parsers
parsers:
	make -C parsers -f Makefile all
clean-parsers:
	make -C parsers -f Makefile clean

# Pritty Printer

.PHONY: pp clean-pp
pp:
	make -C pp -f Makefile all
clean-pp:
	make -C pp -f Makefile clean

# NormalForm

.PHONY: NormalForm clean-NormalForm
NormalForm:
	make -C NormalForm -f Makefile all
clean-NormalForm:
	make -C NormalForm -f Makefile clean


# Global Clean

.PHONY: clean
clean: clean-parsers clean-pp clean-NormalForm


