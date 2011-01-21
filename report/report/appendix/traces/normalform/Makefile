CODEPATH = ../../../../../code/test/NormalForm
FILES = $(CODEPATH)/files
MLB = $(CODEPATH)/files/test.mlb
NFT = $(CODEPATH)/NormalFormTest

.PHONY: all
all: sumlige collatz composelist evallist mapfil mellemsaml sumlige udvaelg

.PHONY : clean
clean :
	-rm trace/*.trace

sumlige:
	$(NFT) --no-column $(MLB)  $(FILES)/sumlige.sml > trace/sumlige.trace

collatz:
	$(NFT) --no-column $(MLB)  $(FILES)/collatz.sml > trace/collatz.trace

composelist:
	$(NFT) --no-column $(MLB)  $(FILES)/composelist.sml > trace/composelist.trace

evallist:
	$(NFT) --no-column $(MLB)  $(FILES)/evallist.sml > trace/evallist.trace

mapfil:
	$(NFT) --no-column $(MLB)  $(FILES)/mapfil.sml > trace/mapfil.trace

mellemsaml:
	$(NFT) --no-column $(MLB)  $(FILES)/mellemsaml.sml > trace/mellemsaml.trace

udvaelg:
	$(NFT) --no-column $(MLB)  $(FILES)/udvaelg.sml > trace/udvaelg.trace

