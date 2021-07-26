// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"fmt"
)

type AssociateList struct {
	List []Pair
}

type Pair struct {
	Key   interface{}
	Value Instance
}

func NewAssociateList(es ...Pair) *AssociateList {
	return &AssociateList{es}
}

func (l AssociateList) Find(k Instance) (v Instance, ok bool) {
	i := 0
	for {
		if len(l.List) == i || DeepEqual(l.List[i].Key, k) {
			break
		}
		i++
	}
	if i == len(l.List) {
		ok = false
	} else {
		ok = true
		v = l.List[i].Value
	}
	return
}

func (l *AssociateList) Delete(k interface{}) (ok bool) {
	i := 0
	for {
		if len(l.List) == i || DeepEqual(l.List[i].Key, k) {
			break
		}
		i++
	}
	if i == len(l.List) {
		ok = false
	} else {
		ok = true
		l.List = append(l.List[:i], l.List[i+1:]...)
		l.Delete(k)
	}
	return
}

func (l *AssociateList) Insert(k interface{}, v Instance) {
	l.List = append([]Pair{Pair{k, v}}, l.List...)
}

func (l *AssociateList) Set(k interface{}, v Instance) {
	l.Delete(k)
	l.Insert(k, v)
}

func (s AssociateList) String() string {
	str := "{"
	for _, kv := range s.List {
		str += fmt.Sprintf(`%v: %v, `, kv.Key, kv.Value)
	}
	if len(str) == 1 {
		return ""
	}
	return str[:len(str)-2] + "}"
}

type StandardClass struct {
	name      Instance
	supers    []Class
	slots     []Instance
	initforms *AssociateList
	initargs  *AssociateList
	metaclass Class
	abstractp Instance
}

func (c StandardClass) Equal(i Instance) bool {
	ic, ok := i.(StandardClass)
	return ok && c.name == ic.name
}

func NewStandardClass(name Instance, supers []Class, slots []Instance, initforms, initargs *AssociateList, metaclass Class, abstractp Instance) Class {
	return StandardClass{name, supers, slots, initforms, initargs, metaclass, abstractp}
}

func (p StandardClass) Supers() []Class {
	return p.supers
}

func (p StandardClass) Slots() []Instance {
	return p.slots
}

func (p StandardClass) Initform(arg Instance) (Instance, bool) {
	return p.initforms.Find(arg)
}

func (p StandardClass) Initarg(arg Instance) (Instance, bool) {
	return p.initargs.Find(arg)
}

func (p StandardClass) Class() Class {
	return p.metaclass
}

func (p StandardClass) String() string {
	return fmt.Sprint(p.name)
}
