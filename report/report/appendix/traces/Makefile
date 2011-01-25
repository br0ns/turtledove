APPPATH = ../../../../code/app

# NormalForm

NFPATH = $(APPPATH)/NormalForm
NFOUTPUT = normalise

NFFILES = $(NFPATH)/files
NFMLB = $(NFFILES)/test.mlb
NF = $(NFPATH)/NormalForm

# Rewrite Map

RWMPATH = $(APPPATH)/Rewrite/Map
RWMOUTPUT = rewrite/map

RWMFILES = $(RWMPATH)/files
RWMMLB = $(RWMFILES)/test.mlb
RWM = $(RWMPATH)/RewriteMap

.PHONY: all
all: normalise rewrite-map

.PHONY : clean
clean : clean-normalise clean-rewrite-map



.PONY: normalise
normalise: normal-sumlige normal-collatz normal-composelist normal-evallist normal-mapfil normal-mellemsaml normal-sumlige normal-udvaelg

.PHONY: clean-normalise
clean-normalise: 
	-rm $(NFOUTPUT)/*.trace


.PHONY: normal-sumlige
normal-sumlige:
	$(NF) -v --no-column $(NFMLB)  $(NFFILES)/sumlige.sml > $(NFOUTPUT)/sumlige.trace
.PHONY: normal-collatz
normal-collatz:
	$(NF) -v --no-column $(NFMLB)  $(NFFILES)/collatz.sml > $(NFOUTPUT)/collatz.trace
.PHONY: normal-composelist
normal-composelist:
	$(NF) -v --no-column $(NFMLB)  $(NFFILES)/composelist.sml > $(NFOUTPUT)/composelist.trace
.PHONY: normal-evallist
normal-evallist:
	$(NF) -v --no-column $(NFMLB)  $(NFFILES)/evallist.sml > $(NFOUTPUT)/evallist.trace
.PHONY: normal-mapfil
normal-mapfil:
	$(NF) -v --no-column $(NFMLB)  $(NFFILES)/mapfil.sml > $(NFOUTPUT)/mapfil.trace
.PHONY: normal-mellemsaml
normal-mellemsaml:
	$(NF) -v --no-column $(NFMLB)  $(NFFILES)/mellemsaml.sml > $(NFOUTPUT)/mellemsaml.trace
.PHONY: normal-udvaelg
normal-udvaelg:
	$(NF) -v --no-column $(NFMLB)  $(NFFILES)/udvaelg.sml > $(NFOUTPUT)/udvaelg.trace


.PHONY: rewrite-map
rewrite-map: rewrite-map-collatz rewrite-map-udvaelg rewrite-map-mapfil rewrite-map-evallist

.PHONY: clean-rewrite-map
clean-rewrite-map:
	-rm $(RWMOUTPUT)/*.trace


.PHONY: rewrite-map-collatz
rewrite-map-collatz:
	$(RWM) --no-column $(RWMMLB)  $(RWMFILES)/collatz.sml > $(RWMOUTPUT)/collatz.trace

.PHONY: rewrite-map-udvaelg
rewrite-map-udvaelg:
	$(RWM) --no-column $(RWMMLB)  $(RWMFILES)/udvaelg.sml > $(RWMOUTPUT)/udvaelg.trace

.PHONY: rewrite-map-mapfil
rewrite-map-mapfil:
	$(RWM) --no-column $(RWMMLB)  $(RWMFILES)/mapfil.sml > $(RWMOUTPUT)/mapfil.trace

.PHONY: rewrite-map-evallist
rewrite-map-evallist:
	$(RWM) --no-column $(RWMMLB)  $(RWMFILES)/evallist.sml > $(RWMOUTPUT)/evallist.trace
