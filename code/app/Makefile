.PHONY: all
all : todo size SourceExplorer NormalForm

# Todo

.PHONY: todo clean-todo
todo:
	make -C todo -f Makefile all
clean-todo:
	make -C todo -f Makefile clean

# Size

.PHONY: size clean-size
size:
	make -C size -f Makefile all
clean-size:
	make -C size -f Makefile clean

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



.PHONY : clean
clean: clean-todo clean-size clean-SourceExplorer clean-NormalForm

