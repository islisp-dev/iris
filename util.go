package main

import "io"

type Reader struct {
	err error
	ru  rune
	sz  int
	rr  io.RuneReader
}

func NewReader(r io.RuneReader) *Reader {
	b := new(Reader)
	b.rr = r
	b.ru, b.sz, b.err = b.rr.ReadRune()
	return b
}

func (tr *Reader) PeekRune() (rune, int, error) {
	return tr.ru, tr.sz, tr.err
}

func (tr *Reader) ReadRune() (rune, int, error) {
	r := tr.ru
	s := tr.sz
	e := tr.err
	tr.ru, tr.sz, tr.err = tr.rr.ReadRune()
	return r, s, e
}
