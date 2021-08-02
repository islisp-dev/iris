// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"fmt"
)

// instance

func Create(e Environment, c Instance, i ...Instance) Instance {
	p := []Instance{}
	for _, q := range c.(Class).Supers() {
		p = append(p, Create(e, q, i...))
	}
	return InitializeObject(e, BasicInstance{c.(Class), p, NewHashMap()}, i...)
}

func InitializeObject(e Environment, object Instance, inits ...Instance) Instance {
	for _, super := range object.(BasicInstance).supers {
		InitializeObject(e, super, inits...)
	}
	for i := 0; i < len(inits); i += 2 {
		argName := inits[i]
		argValue := inits[i+1]
		if slotName, ok := object.Class().Initarg(argName); ok {
			for _, s := range object.Class().Slots() {
				if DeepEqual(slotName, s) {
					object.(BasicInstance).SetSlotValue(slotName, argValue, object.Class())
					break
				}
			}
		}
	}
	for _, slotName := range object.Class().Slots() {
		if _, ok := object.(BasicInstance).GetSlotValue(slotName, object.Class()); !ok {
			if form, ok := object.Class().Initform(slotName); ok {
				value, _ := form.(Applicable).Apply(e.NewDynamic())
				object.(BasicInstance).SetSlotValue(slotName, value, object.Class())
			}
		}
	}
	return object
}

type BasicInstance struct {
	class  Class
	supers []Instance
	slots  Map
}

func (i BasicInstance) Class() Class {
	return i.class
}

func (i BasicInstance) GetSlotValue(key Instance, class Class) (Instance, bool) {
	if DeepEqual(i.class, class) {
		if v, err := i.slots.Get(key); err == nil {
			return v.(Instance), true
		}
		for _, s := range class.Slots() {
			if DeepEqual(s, key) {
				return Nil, false
			}
		}
	}
	for _, s := range i.supers {
		if v, ok := s.(BasicInstance).GetSlotValue(key, class); ok {
			return v, true
		}
	}
	return nil, false
}

func (i BasicInstance) SetSlotValue(key Instance, value Instance, class Class) (ok bool) {
	ok = false
	if DeepEqual(i.class, class) {
		i.slots.Set(key, value)
		ok = true
	}
	for _, s := range i.supers {
		ok = ok || s.(BasicInstance).SetSlotValue(key, value, class)
	}
	return ok
}

func (i BasicInstance) getAllSlots() Map {
	m := NewHashMap()
	for _, key := range i.slots.Keys() {
		val, _ := i.slots.Get(key)
		m.Set(key, val)
	}
	for _, c := range i.supers {
		if _, ok := c.(BasicInstance); ok {
			slots := c.(BasicInstance).getAllSlots()
			for _, key := range slots.Keys() {
				val, _ := slots.Get(key)
				m.Set(key, val)
			}
		}
	}
	return m
}

func (i BasicInstance) String() string {
	c := i.Class().String()
	return fmt.Sprintf("#%v %v>", c[:len(c)-1], i.getAllSlots())
}
