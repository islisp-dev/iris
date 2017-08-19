// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"

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
	/*
		for _, slotName := range c.Slots() {
			if _, ok := t[slotName]; !ok {
				t[slotName], _ = c.Initform(slotName)
				t[slotName], _ = Eval(local, global, t[slotName])
			}
		}
	*/
	return instance{c, p, t}
}

type instance struct {
	class  ilos.Class
	supers []ilos.Instance
	slots  map[ilos.Instance]ilos.Instance
}

func (i instance) Class() ilos.Class {
	return i.class
}

func (i instance) GetSlotValue(key ilos.Instance, class ilos.Class) (ilos.Instance, bool) {
	if v, ok := i.slots[key]; ok && i.class == class {
		return v, ok
	}
	for _, s := range i.supers {
		if v, ok := s.GetSlotValue(key, class); ok {
			return v, ok
		}
	}
	return nil, false
}

func (i instance) SetSlotValue(key ilos.Instance, value ilos.Instance, class ilos.Class) bool {
	if _, ok := i.slots[key]; ok && i.class == class {
		i.slots[key] = value
		return true
	}
	for _, s := range i.supers {
		if ok := s.SetSlotValue(key, value, class); ok {
			return ok
		}
	}
	return false
}

func (i instance) Slots() map[ilos.Instance]ilos.Instance {
	m := map[ilos.Instance]ilos.Instance{}
	for k, v := range i.slots {
		m[k] = v
	}
	for _, c := range i.supers {
		if _, ok := c.(*instance); ok {
			for k, v := range c.(*instance).Slots() {
				m[k] = v
			}
		}
	}
	return m
}

func (i instance) String() string {
	c := i.Class().String()
	return fmt.Sprintf("#%v %v>", c[:len(c)-1], i.Slots())
}
