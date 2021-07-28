package util

import "strings"

type StringReadCloser struct {
	*strings.Reader
}

func (_ StringReadCloser) Close() error {
	return nil
}
