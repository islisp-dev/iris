// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"fmt"
	"reflect"
)

// instance

func Create(e Environment, c Instance, i ...Instance) Instance {
	p := []Instance{}
	for _, q := range c.(Class).Supers() {
		p = append(p, Create(e, q, i...))
	}
	return InitializeObject(e, BasicInstance{c.(Class), p, map[Instance]Instance{}}, i...)
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
				if slotName == s {
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

type slots map[Instance]Instance

func (s slots) String() string {
	str := "{"
	for k, v := range s {
		str += fmt.Sprintf(`%v: %v, `, k, v)
	}
	if len(str) == 1 {
		return ""
	}
	return str[:len(str)-2] + "}"
}

type BasicInstance struct {
	class  Class
	supers []Instance
	slots  slots
}

func (i BasicInstance) Class() Class {
	return i.class
}

func (i BasicInstance) GetSlotValue(key Instance, class Class) (Instance, bool) {
	if v, ok := i.slots[key]; ok && reflect.DeepEqual(i.class, class) {
		return v, ok
	}
	for _, s := range i.supers {
		if v, ok := s.(BasicInstance).GetSlotValue(key, class); ok {
			return v, ok
		}
	}
	return nil, false
}

func (i BasicInstance) SetSlotValue(key Instance, value Instance, class Class) bool {
	if reflect.DeepEqual(i.class, class) {
		i.slots[key] = value
		return true
	}
	for _, s := range i.supers {
		if ok := s.(BasicInstance).SetSlotValue(key, value, class); ok {
			return ok
		}
	}
	return false
}

func (i BasicInstance) getAllSlots() slots {
	m := slots{}
	for k, v := range i.slots {
		m[k] = v
	}
	for _, c := range i.supers {
		if _, ok := c.(BasicInstance); ok {
			for k, v := range c.(BasicInstance).getAllSlots() {
				m[k] = v
			}
		}
	}
	return m
}

func (i BasicInstance) String() string {
	c := i.Class().String()
	return fmt.Sprintf("#%v %v>", c[:len(c)-1], i.getAllSlots())
}
