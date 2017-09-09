all: islisp.js.org
	go install
	cd islisp.js.org && make

islisp.js.org:
	git clone https://github.com/ta2gch/islisp.js.org
