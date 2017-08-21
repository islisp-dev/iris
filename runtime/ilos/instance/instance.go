// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"
	"reflect"

	"github.com/ta2gch/iris/runtime/ilos"
)

//
// instance
//

func newInstance(c ilos.Class, s ...interface{}) ilos.Instance {
	p := []ilos.Instance{}
	for _, q := range c.Supers() {
		p = append(p, newInstance(q, s...))
	}
	t := map[ilos.Instance]ilos.Instance{}
	for argName, argValue := range s[0].(map[ilos.Instance]ilos.Instance) {
		if slotName, ok := c.Initarg(argName); ok {
			t[slotName] = argValue
		}
	}
	return Instance{c, p, t}
}

func NewInstance(class ilos.Class, supers []ilos.Instance) ilos.Instance {
	return Instance{class, supers, map[ilos.Instance]ilos.Instance{}}
}

type Instance struct {
	class  ilos.Class
	Supers []ilos.Instance
	slots  map[ilos.Instance]ilos.Instance
}

func (i Instance) Class() ilos.Class {
	return i.class
}

func (i Instance) GetSlotValue(key ilos.Instance, class ilos.Class) (ilos.Instance, bool) {
	if v, ok := i.slots[key]; ok && reflect.DeepEqual(i.class, class) {
		return v, ok
	}
	for _, s := range i.Supers {
		if v, ok := s.GetSlotValue(key, class); ok {
			return v, ok
		}
	}
	return nil, false
}

func (i Instance) SetSlotValue(key ilos.Instance, value ilos.Instance, class ilos.Class) bool {
	if reflect.DeepEqual(i.class, class) {
		i.slots[key] = value
		return true
	}
	for _, s := range i.Supers {
		if ok := s.SetSlotValue(key, value, class); ok {
			return ok
		}
	}
	return false
}

func (i Instance) Slots() map[ilos.Instance]ilos.Instance {
	m := map[ilos.Instance]ilos.Instance{}
	for k, v := range i.slots {
		m[k] = v
	}
	for _, c := range i.Supers {
		if _, ok := c.(Instance); ok {
			for k, v := range c.(Instance).Slots() {
				m[k] = v
			}
		}
	}
	return m
}

func (i Instance) String() string {
	c := i.Class().String()
	return fmt.Sprintf("#%v %v>", c[:len(c)-1], i.Slots())
}
