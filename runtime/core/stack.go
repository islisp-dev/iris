// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

type stack []map[string]Instance

func NewStack() stack {
	return []map[string]Instance{map[string]Instance{}}
}

func (s stack) Get(key Instance) (Instance, bool) {
	for i := len(s) - 1; i >= 0; i-- {
		if v, ok := s[i][key.String()]; ok {
			return v, true
		}
	}
	return nil, false
}

func (s stack) Set(key, value Instance) bool {
	for i := len(s) - 1; i >= 0; i-- {
		if _, ok := s[i][key.String()]; ok {
			s[i][key.String()] = value
			return true
		}
	}
	return false
}

func (s stack) Define(key, value Instance) bool {
	if _, ok := s[len(s)-1][key.String()]; !ok {
		s[len(s)-1][key.String()] = value
		return true
	}
	s[len(s)-1][key.String()] = value
	return false
}

func (s stack) Delete(key Instance) {
	delete(s[len(s)-1], key.String())
}

func (s stack) Append(t stack) stack {
	u := stack{}
	u = append(u, s...)
	u = append(u, t...)
	return u
}
