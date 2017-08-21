// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"
	"reflect"

	"github.com/ta2gch/iris/runtime/environment"
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

func Create(local, global environment.Environment, c ilos.Instance, i ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	p := []ilos.Instance{}
	for _, q := range c.(ilos.Class).Supers() {
		s, err := Create(local, global, q, i...)
		if err != nil {
			return nil, err
		}
		p = append(p, s)
	}
	return InitializeObject(local, global, Instance{c.(ilos.Class), p, map[ilos.Instance]ilos.Instance{}}, i...)
}

func InitializeObject(local, global environment.Environment, object ilos.Instance, inits ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	for _, super := range object.(Instance).supers {
		InitializeObject(local, global, super, inits...)
	}
	for i := 0; i < len(inits); i += 2 {
		argName := inits[i]
		argValue := inits[i+1]
		if slotName, ok := object.Class().Initarg(argName); ok {
			object.(Instance).SetSlotValue(slotName, argValue, object.Class())
		}
	}
	for _, slotName := range object.Class().Slots() {
		if _, ok := object.(Instance).GetSlotValue(slotName, object.Class()); !ok {
			if form, ok := object.Class().Initform(slotName); ok {
				value, _ := form.(Applicable).Apply(local, global)
				object.(Instance).SetSlotValue(slotName, value, object.Class())
			}
		}
	}
	return object, nil
}

type Instance struct {
	class  ilos.Class
	supers []ilos.Instance
	slots  map[ilos.Instance]ilos.Instance
}

func (i Instance) Class() ilos.Class {
	return i.class
}

func (i Instance) GetSlotValue(key ilos.Instance, class ilos.Class) (ilos.Instance, bool) {
	if v, ok := i.slots[key]; ok && reflect.DeepEqual(i.class, class) {
		return v, ok
	}
	for _, s := range i.supers {
		if v, ok := s.(Instance).GetSlotValue(key, class); ok {
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
	for _, s := range i.supers {
		if ok := s.(Instance).SetSlotValue(key, value, class); ok {
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
	for _, c := range i.supers {
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
