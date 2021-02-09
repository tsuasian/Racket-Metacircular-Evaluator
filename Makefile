COLLECTSPATH=$(shell racket -e '(require setup/dirs)' -e '(display (path->string (find-collects-dir)))')

RACOEXEFLAGS=--collects-path $(shell readlink -m $(COLLECTSPATH)) --exf-clear ++exf -m ++exf -U ++exf --

.PHONY : all
all : mceval lazy-mceval

.PHONY : clean
clean :
	rm -rf compiled mceval lazy-mceval run-tests

mceval : mceval.rkt
	raco exe $(RACOEXEFLAGS) -o $@ $<

lazy-mceval : lazy-mceval.rkt
	raco exe $(RACOEXEFLAGS) -o $@ $<

run-tests : run-tests.rkt mceval.rkt lazy-mceval.rkt
	raco exe $(RACOEXEFLAGS) -o $@ $<

.PHONY : test
test : run-tests
	./run-tests
