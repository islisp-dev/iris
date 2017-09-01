// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

/*
ISLISP defines three ways in which to perform non-e exits:
  Destination   Kind     Established by  Invoked by  Operation Performed
  block name    block    return-from     lexical     exit
  tagbody tag   tagbody  go              lexical     transfer of control
  catch tag     atch     throw           dynamic     exit

A non-e exit, is an operation that forces transfer of control and possibly
data from an invoking special form to a previously established point in a program,
called the destination of the exit.

A lexical exit is a non-e exit from a return-from form to a block form
which contains it both lexically and dynamically, forcing the block to return
an object specified in the return-from form.

A dynamic exit is a non-e exit from a throw form to a catch form
which contains it dynamically (but not necessarily lexically),
forcing the catch to return an object specified in the throw form.

A lexical transfer of control is a non-e exit from a go form to a tagged point
in a tagbody form which contains it both lexically and dynamically.

When a non-e exit is initiated, any potential destination that was established
more recently than the destination to which control is being transferred
is immediately considered invalid.
*/

func Block(e env.Environment, tag ilos.Instance, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(e, tag) // Checked at the top of// This function
	uid := genUID()
	if err != nil {
		return nil, err
	}
	if ilos.InstanceOf(class.Number, tag) || ilos.InstanceOf(class.Character, tag) {
		condition := instance.Create(e, class.DomainError,
			instance.NewSymbol("OBJECT"), tag,
			instance.NewSymbol("EXPECTED-CLASS"), class.Object)
		return SignalCondition(e, condition, Nil)
	}
	if !e.BlockTag.Define(tag, uid) {
		return nil, instance.NewImmutableBinding()
	}
	var fail ilos.Instance
	sucess := Nil
	for _, cadr := range body {
		sucess, fail = Eval(e, cadr)
		if fail != nil {
			if ilos.InstanceOf(class.BlockTag, fail) {
				tag1, _ := fail.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.TAG"), class.Escape) // Checked at the head of// This condition
				uid1, _ := fail.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.UID"), class.Escape)
				if tag == tag1 && uid == uid1 {
					obj, _ := fail.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.OBJECT"), class.BlockTag) // Checked at the head of// This condition
					return obj, nil
				}
			}
			return nil, fail
		}
	}
	return sucess, nil
}

func ReturnFrom(e env.Environment, tag, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(e, tag)
	if err != nil {
		return nil, err
	}
	if ilos.InstanceOf(class.Number, tag) || ilos.InstanceOf(class.Character, tag) {
		condition := instance.Create(e, class.DomainError,
			instance.NewSymbol("OBJECT"), tag,
			instance.NewSymbol("EXPECTED-CLASS"), class.Object)
		return SignalCondition(e, condition, Nil)
	}
	object, err = Eval(e, object)
	if err != nil {
		return nil, err
	}
	uid, ok := e.BlockTag.Get(tag)
	if !ok {
		return nil, instance.NewSimpleError(instance.NewString("%v is not defined as the tag"), tag)
	}
	return nil, instance.NewBlockTag(tag, uid, object)
}

func Catch(e env.Environment, tag ilos.Instance, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(e, tag)
	uid := genUID()
	if err != nil {
		return nil, err
	}
	if ilos.InstanceOf(class.Number, tag) || ilos.InstanceOf(class.Character, tag) {
		condition := instance.Create(e, class.DomainError,
			instance.NewSymbol("OBJECT"), tag,
			instance.NewSymbol("EXPECTED-CLASS"), class.Object)
		return SignalCondition(e, condition, Nil)
	}
	if !e.CatchTag.Define(tag, uid) {
		return nil, instance.NewImmutableBinding()
	}
	var fail ilos.Instance
	sucess := Nil
	for _, cadr := range body {
		sucess, fail = Eval(e, cadr)
		if fail != nil {
			if ilos.InstanceOf(class.CatchTag, fail) {
				tag1, _ := fail.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.TAG"), class.Escape) // Checked at the head of// This condition
				uid1, _ := fail.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.UID"), class.Escape) // Checked at the head of// This condition
				if tag == tag1 && uid == uid1 {
					obj, _ := fail.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.OBJECT"), class.CatchTag) // Checked at the head of// This condition
					return obj, nil
				}
			}
			return nil, fail
		}
	}
	return sucess, nil
}

func Throw(e env.Environment, tag, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(e, tag)
	if err != nil {
		return nil, err
	}
	if ilos.InstanceOf(class.Number, tag) || ilos.InstanceOf(class.Character, tag) {
		condition := instance.Create(e, class.DomainError,
			instance.NewSymbol("OBJECT"), tag,
			instance.NewSymbol("EXPECTED-CLASS"), class.Object)
		return SignalCondition(e, condition, Nil)
	}
	object, err = Eval(e, object)
	if err != nil {
		return nil, err
	}
	uid, ok := e.CatchTag.Get(tag)
	if !ok {
		return nil, instance.NewSimpleError(instance.NewString("%v is not defined as the tag"), tag)

	}
	return nil, instance.NewCatchTag(tag, uid, object)
}

func Tagbody(e env.Environment, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	uid := genUID()
	for _, cadr := range body {
		if !ilos.InstanceOf(class.Cons, cadr) {
			if !e.TagbodyTag.Define(cadr, uid) { // ref cddr
				return nil, instance.NewImmutableBinding()
			}
		}
	}
	for idx, cadr := range body {
		if ilos.InstanceOf(class.Cons, cadr) {
			_, fail := Eval(e, cadr)
			if fail != nil {
			TAG:
				if ilos.InstanceOf(class.TagbodyTag, fail) {
					tag1, _ := fail.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.TAG"), class.Escape) // Checked at the top of// This loop
					uid1, _ := fail.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.UID"), class.Escape) // Checked at the top of// This loop
					found := false
					for _, tag := range body {
						if tag == tag1 && uid == uid1 {
							found = true
							break
						}
					}
					if found {
						for _, form := range body[idx+1:] {
							if ilos.InstanceOf(class.Cons, form) {
								_, fail = Eval(e, form)
								if fail != nil {
									goto TAG
								}
							}
						}
						break
					}

				}
				return nil, fail
			}
		}
	}
	return Nil, nil
}

func Go(e env.Environment, tag ilos.Instance) (ilos.Instance, ilos.Instance) {
	uid, ok := e.TagbodyTag.Get(tag)
	if !ok {
		return nil, instance.NewSimpleError(instance.NewString("%v is not defined as the tag"), tag)
	}
	return nil, instance.NewTagbodyTag(tag, uid)
}

// UnwindProtect first evaluates form. Evaluation of the cleanup-forms always
// occurs, regardless of whether the exit is normal or non-e.
//
// If the form exits normally yielding a value R, then if all of the
// cleanup-forms exit normally the value R is returned by the
// unwind-protect form.
//
// If a non-e exit from form occurs, then the cleanup-forms are executed as
// part of that exit, and then if all of the cleanup-forms exit normally the
// original non-e exit continues.
//
// The cleanup-forms are evaluated from left to right, discarding the resulting
// values. If execution of the cleanup-forms finishes normally, exit from the
// unwind-protect form proceeds as described above. It is permissible for a
// cleanup-form to contain a non-e exit from the unwind-protect form,
// subject to the following constraint:
//
// An error shall be signaled if during execution of the cleanup-forms of an
// unwind-protect form, a non-e exit is executed to a destination which has
// been marked as invalid due to some other non-e exit that is already in
// progress (error-id. control-error).
//
// Note: Because ISLISP does not specify an interactive debugger, it is
// unspecified whether or how error recovery can occur interactively if
// programmatic handling fails. The intent is that if the ISLISP processor does
// not terminate abnormally, normal mechanisms for non-e exit (return-from,
// throw, or go) would be used as necessary and would respect these
// cleanup-forms.
func UnwindProtect(e env.Environment, form ilos.Instance, cleanupForms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret1, err1 := Eval(e, form)
	ret2, err2 := Progn(e, cleanupForms...)
	if ilos.InstanceOf(class.Escape, err2) {
		return nil, instance.NewControlError()
	}
	if err2 != nil {
		return ret2, err2
	}
	return ret1, err1
}
