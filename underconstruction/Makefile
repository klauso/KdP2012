all: pdf html sync

html:	
	racket	-l	racket/gui	-l-	scribble/run	++xref-in	setup/xref	load-collections-xref	--htmls	--redirect-main	http://docs.racket-lang.org/	script.scrbl
	chmod a+rw ./script/*
	
pdf:	
	racket	-l	racket/gui -l-	scribble/run	++xref-in	setup/xref	load-collections-xref	--pdf	script.scrbl	
	chmod a+rw script.pdf   
	mv ./script.pdf ./script    
	
sync:	
	scp -r ./script kos@login.mathematik.uni-marburg.de:/app/home/kos/public_html/teaching/kdp2012/


		