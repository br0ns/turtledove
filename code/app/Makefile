.PHONY: all
all : Todo Size SourceExplorer NormalForm Rewrite README.html

.PHONY : clean
clean: clean-Todo clean-Size clean-SourceExplorer clean-NormalForm clean-Rewrite clean-README

# README

README.html : README.md
	markdown README.md > README.html

.PHONY: clean-README
clean-README :
	-rm README.html

# Todo

.PHONY: Todo clean-Todo
Todo:
	make -C Todo -f Makefile all
clean-Todo:
	make -C Todo -f Makefile clean

# Size

.PHONY: size clean-size
Size:
	make -C Size -f Makefile all
clean-Size:
	make -C Size -f Makefile clean

# SourceExplorer

.PHONY: SourceExplorer clean-SourceExplorer
SourceExplorer:
	make -C SourceExplorer -f Makefile all
clean-SourceExplorer:
	make -C SourceExplorer -f Makefile clean

# NormalForm

.PHONY: NormalForm clean-NormalForm
NormalForm:
	make -C NormalForm -f Makefile all
clean-NormalForm:
	make -C NormalForm -f Makefile clean

# Rewrite

.PHONY: Rewrite clean-Rewrite
Rewrite:
	make -C Rewrite -f Makefile all
clean-Rewrite:
	make -C Rewrite -f Makefile clean

