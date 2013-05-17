pdf:
	emacs -q --batch --eval "(setq org-latex-to-pdf-process '(\"latexmk -f -pdf %f\") org-export-latex-hyperref-format \"\\ref{%s}\")" --visit report.org --funcall org-export-as-pdf

clean:
	rm -f report.{aux,bbl,bcf,blg,dvi,fdb_latexmk,fls,log,out,run.xml,tex}
	rm -f texput.{fls,log}
	rm -f report-blx.bib
	rm -f *converted-to.pdf
