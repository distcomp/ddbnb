all: dcbc-calls.png dcbc-tree.png

preview : dcbc-calls.png
	open $<

%.png : %.gv
	cat $< | dot -Tpng > $@

clean:
	rm -f *.png
