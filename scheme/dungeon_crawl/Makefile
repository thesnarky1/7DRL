all: crawl

crawl:
	gsc -link crawl.scm
	gcc crawl_.c crawl.c -lgambc -lncurses -o crawl

run: crawl
	./crawl

clean:
	rm crawl
	rm crawl_.c
	rm crawl.c


