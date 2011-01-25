CODEPATH = ../../../../../code/app/NormalForm
FILES = $(CODEPATH)/files
MLB = $(CODEPATH)/files/test.mlb
NF = $(CODEPATH)/NormalForm

.PHONY: all
all: sumlige collatz composelist evallist mapfil mellemsaml sumlige udvaelg

.PHONY : clean
clean :
	-rm trace/*.trace

.PHONY: sumlige
sumlige:
	$(NF) -v --no-column $(MLB)  $(FILES)/sumlige.sml > trace/sumlige.trace
.PHONY: collatz
collatz:
	$(NF) -v --no-column $(MLB)  $(FILES)/collatz.sml > trace/collatz.trace
.PHONY: composelist
composelist:
	$(NF) -v --no-column $(MLB)  $(FILES)/composelist.sml > trace/composelist.trace
.PHONY: evallist
evallist:
	$(NF) -v --no-column $(MLB)  $(FILES)/evallist.sml > trace/evallist.trace
.PHONY: mapfil
mapfil:
	$(NF) -v --no-column $(MLB)  $(FILES)/mapfil.sml > trace/mapfil.trace
.PHONY: mellemsaml
mellemsaml:
	$(NF) -v --no-column $(MLB)  $(FILES)/mellemsaml.sml > trace/mellemsaml.trace
.PHONY: udvaelg
udvaelg:
	$(NF) -v --no-column $(MLB)  $(FILES)/udvaelg.sml > trace/udvaelg.trace

