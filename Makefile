all: native web

.PHONY: native web

native:
	go install

web: islisp.js.org
	cd islisp.js.org \
	&& git pull \
	&& gopherjs build -m main.go \
	&& git commit -am 'Updated by iris' \
	&& git push

islisp.js.org:
	git clone https://github.com/ta2gch/islisp.js.org
