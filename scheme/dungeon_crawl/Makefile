all: crawl

crawl:
	gsc -link -e '(load "/usr/local/Gambit-C/lib/syntax-case.scm")' crawl.scm
	gcc crawl_.c crawl.c -lgambc -lncurses -o crawl

run: crawl
	./crawl

clean:
	rm crawl
	rm crawl_.c
	rm crawl.c


