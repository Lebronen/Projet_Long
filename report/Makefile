all:
	# -shell escape : nécessaire pour minted
	pdflatex -shell-escape rapport-cotrez-shay.tex
	# on compile une deuxième fois pour les références
	pdflatex -shell-escape rapport-cotrez-shay.tex

clean:
	rm -rf *bak *~ *out *log *aux _minted-report *.aex
