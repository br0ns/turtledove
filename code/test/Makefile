.PHONY: all
all : pp

# Parsers

#.PHONY: parsers clean-parsers
#parsers:
#	make -C parsers -f Makefile all
#clean-parsers:
#	make -C parsers -f Makefile clean

# Pritty Printer

.PHONY: pp clean-pp
pp:
	make -C pp -f Makefile all
clean-pp:
	make -C pp -f Makefile clean

# Global Clean

.PHONY: clean
clean: clean-parsers clean-pp


