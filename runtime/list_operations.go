// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// TODO: listp, craete-list, list, reverse, nreverse, append, member, mapcar, mapc, mapcan, maplist, mapcon, assoc

func Listp(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Cons, obj) {
		return T, nil
	}
	return Nil, nil
}

func CreateList(_, _ *environment.Environment, i ilos.Instance, initialElement ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Integer, i); err != nil {
		return nil, err
	}
	if len(initialElement) > 1 {
		return nil, instance.New(class.ProgramError)
	}
	elm := Nil
	if len(initialElement) == 1 {
		elm = initialElement[0]
	}
	cons := Nil
	for j := 0; j < int(i.(instance.Integer)); j++ {
		cons = instance.New(class.Cons, elm, cons)
	}
	return cons, nil
}

func List(_, _ *environment.Environment, objs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	cons := Nil
	for i := len(objs) - 1; i >= 0; i-- {
		cons = instance.New(class.Cons, objs[i], cons)
	}
	return cons, nil
}

func Reverse(_, _ *environment.Environment, list ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.List, list); err != nil {
		return nil, err
	}
	cons := Nil
	for _, car := range list.(instance.List).Slice() {
		cons = instance.New(class.Cons, car, cons)
	}
	return cons, nil
}

func Nreverse(_, _ *environment.Environment, list ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: tests literal object
	if err := ensure(class.List, list); err != nil {
		return nil, err
	}
	cons := Nil
	for _, car := range list.(instance.List).Slice() {
		cons = instance.New(class.Cons, car, cons)
	}
	return cons, nil
}

func Append(_, _ *environment.Environment, lists ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// Ref: https://github.com/sbcl/sbcl/blob/fe4faef65315c6ad52b3b89b62b6c6497cb78d09/src/code/list.lisp#L364

	result, err := List(nil, nil, Nil)
	if err != nil {
		return nil, err
	}
	cdr := result
	if err := ensure(class.List, lists...); err != nil {
		return nil, err
	}
	for _, list := range lists {
		for _, elt := range list.(instance.List).Slice() {
			it, err := List(nil, nil, elt)
			if err != nil {
				return nil, err
			}
			cdr.(*instance.Cons).Cdr = it
			cdr = cdr.(*instance.Cons).Cdr
		}
	}
	return result.(*instance.Cons).Cdr, nil
}
