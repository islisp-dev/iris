package main

import "io"

// Reader is like bufio.Reader but has PeekRune
// which returns a rune without advancing pointer
type Reader struct {
	err error
	ru  rune
	sz  int
	rr  io.RuneReader
}

// NewReader creates interal reader from io.RuneReader
func NewReader(r io.RuneReader) *Reader {
	b := new(Reader)
	b.rr = r
	b.ru, b.sz, b.err = b.rr.ReadRune()
	return b
}

// PeekRune returns a rune without advancing pointer
func (r *Reader) PeekRune() (rune, int, error) {
	return r.ru, r.sz, r.err
}

// ReadRune returns a rune with advancing pointer
func (r *Reader) ReadRune() (rune, int, error) {
	ru := r.ru
	sz := r.sz
	err := r.err
	r.ru, r.sz, r.err = r.rr.ReadRune()
	return ru, sz, err
}

// ExpReader interface type is the interface for reading expressions.
type ExpReader interface {
	ReadExp() (*Object, error)
}

// ReadExp returns a expression that is a pointer to Object
func (r *Reader) ReadExp() (*Object, error) {
	exp, err := Parse(r)
	return exp, err
}
