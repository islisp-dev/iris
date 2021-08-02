// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

type stack []Map

func NewStack() stack {
	return []Map{NewHashMap()}
}

func (s stack) Get(key Instance) (Instance, bool) {
	for i := len(s) - 1; i >= 0; i-- {
		if v, err := s[i].Get(key); err == nil {
			return v.(Instance), true
		}
	}
	return nil, false
}

func (s stack) Set(key, value Instance) bool {
	for i := len(s) - 1; i >= 0; i-- {
		if _, err := s[i].Get(key); err == nil {
			s[i].Set(key, value)
			return true
		}
	}
	return false
}

func (s stack) Define(key, value Instance) bool {
	if _, err := s[len(s)-1].Get(key); err != nil {
		s[len(s)-1].Set(key, value)
		return true
	}
	s[len(s)-1].Set(key, value)
	return false
}

func (s stack) Delete(key Instance) {
	s[len(s) - 1].Delete(key)
}

func (s stack) Append(t stack) stack {
	u := stack{}
	u = append(u, s...)
	u = append(u, t...)
	return u
}
