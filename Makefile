ff_path=include/Metric-FF-v2.1
gf_files=$(wildcard www/*.gf)
gf_objs=$(gf_files:.gf=.gfo)

all: install

gfos: $(gf_objs)

www/%.gfo: www/%.gf
	gf --batch $<

install: bin/metric-ff www/planner.cgi gfos
	@echo "### To finish up the installation process run \`parser-python2' or \`make parser-python3' depending on your default python interpreter."

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
	rm -f www/*.gfo
	rm -f ai-project-2.tar.gz

archive:
	git archive --format=tar.gz --prefix=ai-project-2/ -o ai-project-2.tar.gz HEAD
