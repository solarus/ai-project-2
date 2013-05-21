ff_path=include/Metric-FF-v2.1

all: install

install: bin/metric-ff www/planner.cgi
	echo "### To finish up the installation process run `make parser-python2' or `make parser-python3' depending on your default python interpreter."

www/planner.cgi: planner-haskell/cabal-dev/bin/planner-haskell
	cd www; \
	ln -s ../planner-haskell/cabal-dev/bin/planner-haskell planner.cgi

planner-haskell/cabal-dev/bin/planner-haskell:
	cd planner-haskell; \
	cabal-dev install

parser-python2:
	cd www; \
	rm -f parser.cg; \
	ln -s parser2.cgi parser.cgi

parser-python3:
	cd www; \
	rm -f parser.cgi; \
	ln -s parser3.cgi parser.cgi

bin/metric-ff:
	make -C $(ff_path)
	cp $(ff_path)/ff bin/metric-ff

clean:
	make -C $(ff_path) clean
	rm -f $(ff_path)/ff bin/metric-ff www/parser.cgi www/planner.cgi
