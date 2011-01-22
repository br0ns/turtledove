.PHONY: all
all : rule

.PHONY: rule clean-rule
rule: 
	make -C rule -f Makefile all
clean-rule :
	make -C rule -f Makefile clean


.PHONY: clean
clean: clean-rule
