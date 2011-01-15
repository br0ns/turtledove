SML = mlton

.PHONY : all
all: NormalFormTest

NormalFormTest : NormalFormTest.mlb NormalFormTest.sml
	$(SML) NormalFormTest.mlb

.PHONY : clean
clean :
	-rm NormalFormTest
