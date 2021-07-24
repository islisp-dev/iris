// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/runtime/core"

// Listp returns t if obj is a list (instance of class list); otherwise, returns
// nil. obj may be any ISLISP object.
func Listp(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.ListClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// CreateList returns a list of length i. If initial-element is given, the
// elements of the new list are initialized with this object; otherwise, the
// initialization is implementation defined. An error shall be signaled if the
// requested list cannot be allocated (error-id. cannot-create-list). An error
// shall be signaled if i is not a non-negative integer (error-id.
// domain-error).initial-element may be any ISLISP object.
func CreateList(e core.Environment, i core.Instance, initialElement ...core.Instance) (core.Instance, core.Instance) {
	if ok, _ := Integerp(e, i); core.DeepEqual(ok, Nil) {
		return nil, core.NewDomainError(e, i, core.IntegerClass)
	}
	if len(initialElement) > 1 {
		return SignalCondition(e, core.NewArityError(e), Nil)
	}
	elm := Nil
	if len(initialElement) == 1 {
		elm = initialElement[0]
	}
	cons := Nil
	for j := 0; j < int(i.(core.Integer)); j++ {
		cons = core.NewCons(elm, cons)
	}
	return cons, nil
}

// List returns a new list whose length is the number of arguments and whose
// elements are the arguments in the same order as in the list-form. An error
// shall be signaled if the requested list cannot be allocated (error-id.
// cannot-create-list). Each obj may be any ISLISP object.
func List(e core.Environment, objs ...core.Instance) (core.Instance, core.Instance) {
	cons := Nil
	for i := len(objs) - 1; i >= 0; i-- {
		cons = core.NewCons(objs[i], cons)
	}
	return cons, nil
}

// Reverse returns a list whose elements are those of the given list, but in
// reverse order. An error shall be signaled if list is not a list (error-id.
// domain-error). For reverse, no side-effect to the given list occurs. The
// resulting list is permitted but not required to share structure with the
// input list.
func Reverse(e core.Environment, list core.Instance) (core.Instance, core.Instance) {
	if ok, _ := Listp(e, list); core.DeepEqual(ok, Nil) {
		return nil, core.NewDomainError(e, list, core.ListClass)
	}
	cons := Nil
	for _, car := range list.(core.List).Slice() {
		cons = core.NewCons(car, cons)
	}
	return cons, nil
}

// Nreverse returns a list whose elements are those of the given list, but in
// reverse order. An error shall be signaled if list is not a list (error-id.
// domain-error). For nreverse, the conses which make up the top level of the
// given list are permitted, but not required, to be side-effected in order to
// produce this new list. nreverse should never be called on a literal object.
func Nreverse(e core.Environment, list core.Instance) (core.Instance, core.Instance) {
	// TODO: tests literal object
	if ok, _ := Listp(e, list); core.DeepEqual(ok, Nil) {
		return nil, core.NewDomainError(e, list, core.ListClass)
	}
	cons := Nil
	for _, car := range list.(core.List).Slice() {
		cons = core.NewCons(car, cons)
	}
	return cons, nil
}

// Append returns the result of appending all of the lists, or () if given no
// lists. An error shall be signaled if any list is not a list (error-id.
// domain-error). This function does not modify its arguments. It is
// implementation defined whether and when the result shares structure with its
// list arguments. An error shall be signaled if the list cannot be allocated
// (error-id. cannot-create-list).
func Append(e core.Environment, lists ...core.Instance) (core.Instance, core.Instance) {
	// Ref: https://github.com/sbcl/sbcl/blob/fe4faef65315c6ad52b3b89b62b6c6497cb78d09/src/code/list.lisp#L364

	result, err := List(e, Nil)
	if err != nil {
		return nil, err
	}
	cdr := result
	for _, list := range lists {
		if ok, _ := Listp(e, list); core.DeepEqual(ok, Nil) {
			return nil, core.NewDomainError(e, list, core.ListClass)
		}
	}
	for _, list := range lists {
		for _, elt := range list.(core.List).Slice() {
			it, err := List(e, elt)
			if err != nil {
				return nil, err
			}
			cdr.(*core.Cons).Cdr = it
			cdr = cdr.(*core.Cons).Cdr
		}
	}
	return result.(*core.Cons).Cdr, nil
}

// Member returnes the first sublist of list whose car is obj  if list contains
// at least one occurrence of obj (as determined by eql).  Otherwise, nil is
// returned. An error shall be signaled if list is not a list (error-id.
// domain-error).
func Member(e core.Environment, obj, list core.Instance) (core.Instance, core.Instance) {
	if ok, _ := Listp(e, list); core.DeepEqual(ok, Nil) {
		return nil, core.NewDomainError(e, list, core.ListClass)
	}
	if !core.InstanceOf(core.ConsClass, list) || core.DeepEqual(list.(*core.Cons).Car, obj) {
		return list, nil
	}
	if !core.InstanceOf(core.ConsClass, list.(*core.Cons).Cdr) {
		return list.(*core.Cons).Cdr, nil
	}
	return Member(e, obj, list.(*core.Cons).Cdr)
}

// Mapcar operates on successive elements of the lists. function is applied to
// the first element of each list, then to the second element of each list, and
// so on. The iteration terminates when the shortest list runs out, and excess
// elements in other lists are ignored. The value returned by mapcar is a list
// of the results of successive calls to function.
func Mapcar(e core.Environment, function, list1 core.Instance, lists ...core.Instance) (core.Instance, core.Instance) {
	lists = append([]core.Instance{list1}, lists...)
	if ok, _ := Functionp(e, function); core.DeepEqual(ok, Nil) {
		return nil, core.NewDomainError(e, function, core.FunctionClass)
	}
	for _, list := range lists {
		if ok, _ := Listp(e, list); core.DeepEqual(ok, Nil) {
			return nil, core.NewDomainError(e, list, core.ListClass)
		}
	}
	arguments := []core.Instance{}
	rests := []core.Instance{}
	for _, list := range lists {
		if core.DeepEqual(list, Nil) {
			return Nil, nil
		}
		arguments = append(arguments, list.(*core.Cons).Car)
		rests = append(rests, list.(*core.Cons).Cdr)
	}
	car, err := function.(core.Applicable).Apply(e.NewDynamic(), arguments...)
	if err != nil {
		return nil, err
	}
	var cdr core.Instance
	for _, list := range lists {
		if ok, _ := Listp(e, list); core.DeepEqual(ok, Nil) {
			cdr = Nil
		}
	}
	if core.DeepEqual(cdr, nil) {
		cdr, err = Mapcar(e, function, rests[0], rests[1:]...)
		if err != nil {
			return nil, err
		}
	}
	return Cons(e, car, cdr)
}

// Mapc is like mapcar except that the results of applying function are not
// accumulated; list1 is returned.
func Mapc(e core.Environment, function, list1 core.Instance, lists ...core.Instance) (core.Instance, core.Instance) {
	_, err := Mapcar(e, function, list1, lists...)
	if err != nil {
		return nil, err
	}
	return list1, nil
}

// Mapcan is like mapcar respectively, except that the results of applying
// function are combined into a list by the use of an operation that performs a
// destructive form of append rather than list.
func Mapcan(e core.Environment, function, list1 core.Instance, lists ...core.Instance) (core.Instance, core.Instance) {
	list, err := Mapcar(e, function, list1, lists...)
	if err != nil {
		return nil, err
	}
	append, _ := e.Function.Get(core.NewSymbol("APPEND"))
	return Apply(e, append, list)
}

// Maplist is like mapcar except that function is applied to successive sublists
// of the lists. function is first applied to the lists themselves, and then to
// the cdr of each list, and then to the cdr of the cdr of each list, and so on.
func Maplist(e core.Environment, function, list1 core.Instance, lists ...core.Instance) (core.Instance, core.Instance) {
	lists = append([]core.Instance{list1}, lists...)
	if ok, _ := Functionp(e, function); core.DeepEqual(ok, Nil) {
		return nil, core.NewDomainError(e, function, core.FunctionClass)
	}
	for _, list := range lists {
		if ok, _ := Listp(e, list); core.DeepEqual(ok, Nil) {
			return nil, core.NewDomainError(e, list, core.ListClass)
		}
	}
	arguments := []core.Instance{}
	rests := []core.Instance{}
	for _, list := range lists {
		if core.DeepEqual(list, Nil) {
			return Nil, nil
		}
		arguments = append(arguments, list)
		rests = append(rests, list.(*core.Cons).Cdr)
	}
	car, err := function.(core.Applicable).Apply(e.NewDynamic(), arguments...)
	if err != nil {
		return nil, err
	}
	var cdr core.Instance
	for _, list := range lists {
		if ok, _ := Listp(e, list); core.DeepEqual(ok, Nil) {
			cdr = Nil
		}
	}
	if core.DeepEqual(cdr, nil) {
		cdr, err = Maplist(e, function, rests[0], rests[1:]...)
		if err != nil {
			return nil, err
		}
	}
	return Cons(e, car, cdr)
}

// Mapl is like maplist except that the results of applying function are not
// accumulated; list1 is returned.
func Mapl(e core.Environment, function, list1 core.Instance, lists ...core.Instance) (core.Instance, core.Instance) {
	_, err := Maplist(e, function, list1, lists...)
	if err != nil {
		return nil, err
	}
	return list1, nil
}

// Mapcon is like maplist respectively, except that the results of applying
// function are combined into a list by the use of an operation that performs a
// destructive form of append rather than list.
func Mapcon(e core.Environment, function, list1 core.Instance, lists ...core.Instance) (core.Instance, core.Instance) {
	list, err := Maplist(e, function, list1, lists...)
	if err != nil {
		return nil, err
	}
	append, _ := e.Function.Get(core.NewSymbol("APPEND"))
	return Apply(e, append, list)
}

// Assoc returns the first cons if assocation-list contains at least one cons
// whose car is obj (as determined by eql). Otherwise, nil is returned. An error
// shall be signaled if association-list is not a list of conses (error-id.
// domain-error).
func Assoc(e core.Environment, obj, associationList core.Instance) (core.Instance, core.Instance) {
	if ok, _ := Listp(e, associationList); core.DeepEqual(ok, Nil) {
		return nil, core.NewDomainError(e, associationList, core.ListClass)
	}
	if !core.InstanceOf(core.ConsClass, associationList) {
		return Nil, nil
	}
	car := associationList.(*core.Cons).Car
	cdr := associationList.(*core.Cons).Cdr
	if ok, _ := Consp(e, car); core.DeepEqual(ok, Nil) {
		return nil, core.NewDomainError(e, car, core.ConsClass)
	}
	if core.DeepEqual(car.(*core.Cons).Car, obj) { // eql
		return car, nil
	}
	return Assoc(e, obj, cdr)
}

// Null returns t if obj is nil; otherwise, returns nil obj may be any ISLISP
// object.
func Null(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.DeepEqual(obj, Nil) {
		return T, nil
	}
	return Nil, nil
}
