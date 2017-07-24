package main

type ExpReader interface {
	ReadExp() (*Object, error)
}

func (r *Reader) ReadExp() (*Object, error) {
	exp, err := Parse(r)
	return exp, err
}
