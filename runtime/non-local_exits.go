// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
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

func Block(e ilos.Environment, tag ilos.Instance, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	uid := ilos.NewInteger(uniqueInt())
	if ilos.InstanceOf(ilos.NumberClass, tag) || ilos.InstanceOf(ilos.CharacterClass, tag) {
		return SignalCondition(e, ilos.NewDomainError(e, tag, ilos.ObjectClass), Nil)
	}
	if !e.BlockTag.Define(tag, uid) {
		return SignalCondition(e, ilos.NewImmutableBinding(e), Nil)
	}
	var fail ilos.Instance
	sucess := Nil
	for _, cadr := range body {
		sucess, fail = Eval(e, cadr)
		if fail != nil {
			if ilos.InstanceOf(ilos.BlockTagClass, fail) {
				tag1, _ := fail.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.TAG"), ilos.EscapeClass) // Checked at the head of// This condition
				uid1, _ := fail.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.UID"), ilos.EscapeClass)
				if tag == tag1 && uid == uid1 {
					obj, _ := fail.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.OBJECT"), ilos.BlockTagClass) // Checked at the head of// This condition
					e.BlockTag.Delete(tag)
					return obj, nil
				}
			}
			e.BlockTag.Delete(tag)
			return nil, fail
		}
	}
	e.BlockTag.Delete(tag)
	return sucess, nil
}

func ReturnFrom(e ilos.Environment, tag, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(ilos.NumberClass, tag) || ilos.InstanceOf(ilos.CharacterClass, tag) {
		return SignalCondition(e, ilos.NewDomainError(e, tag, ilos.ObjectClass), Nil)
	}
	object, err := Eval(e, object)
	if err != nil {
		return nil, err
	}
	uid, ok := e.BlockTag.Get(tag)
	if !ok {
		return SignalCondition(e, ilos.NewControlError(e), Nil)
	}
	return nil, ilos.NewBlockTag(tag, uid, object)
}

func Catch(e ilos.Environment, tag ilos.Instance, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(e, tag)
	uid := ilos.NewInteger(uniqueInt())
	if err != nil {
		return nil, err
	}
	if ilos.InstanceOf(ilos.NumberClass, tag) || ilos.InstanceOf(ilos.CharacterClass, tag) {
		return SignalCondition(e, ilos.NewDomainError(e, tag, ilos.ObjectClass), Nil)
	}
	if !e.CatchTag.Define(tag, uid) {
		return SignalCondition(e, ilos.NewImmutableBinding(e), Nil)
	}
	var fail ilos.Instance
	sucess := Nil
	for _, cadr := range body {
		sucess, fail = Eval(e, cadr)
		if fail != nil {
			if ilos.InstanceOf(ilos.CatchTagClass, fail) {
				tag1, _ := fail.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.TAG"), ilos.EscapeClass) // Checked at the head of// This condition
				uid1, _ := fail.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.UID"), ilos.EscapeClass) // Checked at the head of// This condition
				if tag == tag1 && uid == uid1 {
					obj, _ := fail.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.OBJECT"), ilos.CatchTagClass) // Checked at the head of// This condition
					e.CatchTag.Delete(tag)
					return obj, nil
				}
			}
			e.CatchTag.Delete(tag)
			return nil, fail
		}
	}
	e.CatchTag.Delete(tag)
	return sucess, nil
}

func Throw(e ilos.Environment, tag, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(e, tag)
	if err != nil {
		return nil, err
	}
	if ilos.InstanceOf(ilos.NumberClass, tag) || ilos.InstanceOf(ilos.CharacterClass, tag) {
		return SignalCondition(e, ilos.NewDomainError(e, tag, ilos.ObjectClass), Nil)
	}
	object, err = Eval(e, object)
	if err != nil {
		return nil, err
	}
	uid, ok := e.CatchTag.Get(tag)
	if !ok {
		return SignalCondition(e, ilos.NewControlError(e), Nil)

	}
	return nil, ilos.NewCatchTag(tag, uid, object)
}

func Tagbody(e ilos.Environment, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	uid := ilos.NewInteger(uniqueInt())
	for _, cadr := range body {
		if !ilos.InstanceOf(ilos.ConsClass, cadr) {
			if !e.TagbodyTag.Define(cadr, uid) { // ref cddr
				return SignalCondition(e, ilos.NewImmutableBinding(e), Nil)
			}
		}
	}
	for idx, cadr := range body {
		if ilos.InstanceOf(ilos.ConsClass, cadr) {
			_, fail := Eval(e, cadr)
			if fail != nil {
			TAG:
				if ilos.InstanceOf(ilos.TagbodyTagClass, fail) {
					tag1, _ := fail.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.TAG"), ilos.EscapeClass) // Checked at the top of// This loop
					uid1, _ := fail.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.UID"), ilos.EscapeClass) // Checked at the top of// This loop
					found := false
					for _, tag := range body {
						if tag == tag1 && uid == uid1 {
							found = true
							break
						}
					}
					if found {
						for _, form := range body[idx+1:] {
							if ilos.InstanceOf(ilos.ConsClass, form) {
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

func Go(e ilos.Environment, tag ilos.Instance) (ilos.Instance, ilos.Instance) {
	uid, ok := e.TagbodyTag.Get(tag)
	if !ok {
		return SignalCondition(e, ilos.NewControlError(e), Nil)
	}
	return nil, ilos.NewTagbodyTag(tag, uid)
}

// UnwindProtect first evaluates form. Evaluation of the cleanup-forms always
// occurs, regardless of whether the exit is normal or non-e. If the form exits
// normally yielding a value R, then if all of the cleanup-forms exit normally
// the value R is returned by the unwind-protect form. If a non-e exit from form
// occurs, then the cleanup-forms are executed as part of that exit, and then if
// all of the cleanup-forms exit normally the original non-e exit continues. The
// cleanup-forms are evaluated from left to right, discarding the resulting
// values. If execution of the cleanup-forms finishes normally, exit from the
// unwind-protect form proceeds as described above. It is permissible for a
// cleanup-form to contain a non-e exit from the unwind-protect form, subject to
// the following constraint: An error shall be signaled if during execution of
// the cleanup-forms of an unwind-protect form, a non-e exit is executed to a
// destination which has been marked as invalid due to some other non-e exit
// that is already in progress (error-id. control-error). Note: Because ISLISP
// does not specify an interactive debugger, it is unspecified whether or how
// error recovery can occur interactively if programmatic handling fails. The
// intent is that if the ISLISP processor does not terminate abnormally, normal
// mechanisms for non-e exit (return-from, throw, or go) would be used as
// necessary and would respect these cleanup-forms.
func UnwindProtect(e ilos.Environment, form ilos.Instance, cleanupForms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret1, err1 := Eval(e, form)
	ret2, err2 := Progn(e, cleanupForms...)
	if err2 != nil {
		if ilos.InstanceOf(ilos.EscapeClass, err2) {
			return SignalCondition(e, ilos.NewControlError(e), Nil)
		}
		return ret2, err2
	}
	return ret1, err1
}
