LEXmlton = mllex
LEXnj = ml-lex
LEXmos = mosmllex

YACCmlton = mlyacc
YACCnj = ml-yacc
YACCmos = mlsmlyac

LEX = $(LEXmlton)
YACC = $(YACCmlton)

PROGRAM = SML
LEXPROG = $(PROGRAM).lex
YACCPROG = $(PROGRAM).yacc


.PHONY: all
all : lex yacc


lex : $(LEXPROG)
	$(LEX) $(LEXPROG)


yacc : $(YACCPROG)
	$(YACC) $(YACCPROG)


.PHONY: clean
clean : 
	-rm $(YACCPROG).*
	-rm $(LEXPROG).*
