// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package env

import (
	"github.com/ta2gch/iris/runtime/ilos"
)

type map2 map[[2]ilos.Instance]ilos.Instance

func NewMap2() map2 {
	return map[[2]ilos.Instance]ilos.Instance{}
}

func (s map2) Get(key1, key2 ilos.Instance) (ilos.Instance, bool) {
	if v, ok := s[[2]ilos.Instance{key1, key2}]; ok {
		return v, true
	}
	return nil, false
}
func (s map2) Set(key1, key2, value ilos.Instance) {
	s[[2]ilos.Instance{key1, key2}] = value
}

func (s map2) Delete(key1, key2 ilos.Instance) (ilos.Instance, bool) {
	if v, ok := s[[2]ilos.Instance{key1, key2}]; ok {
		delete(s, [2]ilos.Instance{key1, key2})
		return v, true
	}
	return nil, false
}
