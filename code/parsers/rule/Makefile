LEXmlton = mllex
LEXnj = ml-lex
LEXmos = mosmllex

YACCmlton = mlyacc
YACCnj = ml-yacc
YACCmos = mlsmlyac

LEX = $(LEXmlton)
YACC = $(YACCmlton)

PROGRAM = rule
LEXPROG = $(PROGRAM).lex
YACCPROG = $(PROGRAM).yacc


.PHONY: all
all : lex yacc mlton


lex : $(LEXPROG)
	$(LEX) $(LEXPROG)


yacc : $(YACCPROG)
	$(YACC) $(YACCPROG)


.PHONY: clean
clean : 
	-rm $(YACCPROG).*
	-rm $(LEXPROG).*

mlton : 
	mlton Rule.mlb
	# Just for show to see if it compiles. 
	# Remove the executable again as it is useless
	-rm Rule
