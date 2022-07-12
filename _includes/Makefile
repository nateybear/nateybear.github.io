.PHONY: all

all: resume.pdf resume.html

resume.pdf: resume.tex
	pdflatex resume.tex
	rm -f resume.aux resume.bbl resume.blg resume.log resume.out resume.toc resume.xdv
	cp resume.pdf ../assets/resume

resume.html: resume.tex
	pandoc resume.tex -o resume.html