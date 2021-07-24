// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package ilos

type stack []map[Instance]Instance

func NewStack() stack {
	return []map[Instance]Instance{map[Instance]Instance{}}
}

func (s stack) Get(key Instance) (Instance, bool) {
	for i := len(s) - 1; i >= 0; i-- {
		if v, ok := s[i][key]; ok {
			return v, true
		}
	}
	return nil, false
}

func (s stack) Set(key, value Instance) bool {
	for i := len(s) - 1; i >= 0; i-- {
		if _, ok := s[i][key]; ok {
			s[i][key] = value
			return true
		}
	}
	return false
}

func (s stack) Define(key, value Instance) bool {
	if _, ok := s[len(s)-1][key]; !ok {
		s[len(s)-1][key] = value
		return true
	}
	s[len(s)-1][key] = value
	return false
}

func (s stack) Delete(key Instance) {
	delete(s[len(s)-1], key)
}

func (s stack) Append(t stack) stack {
	u := stack{}
	u = append(u, s...)
	u = append(u, t...)
	return u
}
