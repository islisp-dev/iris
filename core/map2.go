// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

type map2 map[[2]string]Instance

func NewMap2() map2 {
	return map[[2]string]Instance{}
}

func (s map2) Get(key1, key2 Instance) (Instance, bool) {
	if v, ok := s[[2]string{key1.String(), key2.String()}]; ok {
		return v, true
	}
	return nil, false
}
func (s map2) Set(key1, key2, value Instance) {
	s[[2]string{key1.String(), key2.String()}] = value
}

func (s map2) Delete(key1, key2 Instance) (Instance, bool) {
	if v, ok := s[[2]string{key1.String(), key2.String()}]; ok {
		delete(s, [2]string{key1.String(), key2.String()})
		return v, true
	}
	return nil, false
}
