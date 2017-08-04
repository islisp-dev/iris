package environment

import (
	"github.com/ta2gch/iris/runtime/ilos"
)

type stack []map[ilos.Instance]ilos.Instance

func newStack() stack {
	return []map[ilos.Instance]ilos.Instance{map[ilos.Instance]ilos.Instance{}}
}

func (s stack) Get(key ilos.Instance) (ilos.Instance, bool) {
	for i := len(s) - 1; i >= 0; i-- {
		if v, ok := s[i][key]; ok {
			return v, true
		}
	}
	return nil, false
}

func (s stack) Set(key, value ilos.Instance) bool {
	for i := len(s) - 1; i >= 0; i-- {
		if _, ok := s[i][key]; ok {
			s[i][key] = value
			return true
		}
	}
	return false
}

func (s stack) Define(key, value ilos.Instance) {
	s[0][key] = value
}
