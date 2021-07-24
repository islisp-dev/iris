// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/runtime/core"

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

func Block(e core.Environment, tag core.Instance, body ...core.Instance) (core.Instance, core.Instance) {
	uid := core.NewInteger(uniqueInt())
	if core.InstanceOf(core.NumberClass, tag) || core.InstanceOf(core.CharacterClass, tag) {
		return SignalCondition(e, core.NewDomainError(e, tag, core.ObjectClass), Nil)
	}
	if !e.BlockTag.Define(tag, uid) {
		return SignalCondition(e, core.NewImmutableBinding(e), Nil)
	}
	var fail core.Instance
	sucess := Nil
	for _, cadr := range body {
		sucess, fail = Eval(e, cadr)
		if fail != nil {
			if core.InstanceOf(core.BlockTagClass, fail) {
				tag1, _ := fail.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.TAG"), core.EscapeClass) // Checked at the head of// This condition
				uid1, _ := fail.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.UID"), core.EscapeClass)
				if tag == tag1 && uid == uid1 {
					obj, _ := fail.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.OBJECT"), core.BlockTagClass) // Checked at the head of// This condition
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

func ReturnFrom(e core.Environment, tag, object core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.NumberClass, tag) || core.InstanceOf(core.CharacterClass, tag) {
		return SignalCondition(e, core.NewDomainError(e, tag, core.ObjectClass), Nil)
	}
	object, err := Eval(e, object)
	if err != nil {
		return nil, err
	}
	uid, ok := e.BlockTag.Get(tag)
	if !ok {
		return SignalCondition(e, core.NewControlError(e), Nil)
	}
	return nil, core.NewBlockTag(tag, uid, object)
}

func Catch(e core.Environment, tag core.Instance, body ...core.Instance) (core.Instance, core.Instance) {
	var err core.Instance
	tag, err = Eval(e, tag)
	uid := core.NewInteger(uniqueInt())
	if err != nil {
		return nil, err
	}
	if core.InstanceOf(core.NumberClass, tag) || core.InstanceOf(core.CharacterClass, tag) {
		return SignalCondition(e, core.NewDomainError(e, tag, core.ObjectClass), Nil)
	}
	if !e.CatchTag.Define(tag, uid) {
		return SignalCondition(e, core.NewImmutableBinding(e), Nil)
	}
	var fail core.Instance
	sucess := Nil
	for _, cadr := range body {
		sucess, fail = Eval(e, cadr)
		if fail != nil {
			if core.InstanceOf(core.CatchTagClass, fail) {
				tag1, _ := fail.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.TAG"), core.EscapeClass) // Checked at the head of// This condition
				uid1, _ := fail.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.UID"), core.EscapeClass) // Checked at the head of// This condition
				if tag == tag1 && uid == uid1 {
					obj, _ := fail.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.OBJECT"), core.CatchTagClass) // Checked at the head of// This condition
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

func Throw(e core.Environment, tag, object core.Instance) (core.Instance, core.Instance) {
	var err core.Instance
	tag, err = Eval(e, tag)
	if err != nil {
		return nil, err
	}
	if core.InstanceOf(core.NumberClass, tag) || core.InstanceOf(core.CharacterClass, tag) {
		return SignalCondition(e, core.NewDomainError(e, tag, core.ObjectClass), Nil)
	}
	object, err = Eval(e, object)
	if err != nil {
		return nil, err
	}
	uid, ok := e.CatchTag.Get(tag)
	if !ok {
		return SignalCondition(e, core.NewControlError(e), Nil)

	}
	return nil, core.NewCatchTag(tag, uid, object)
}

func Tagbody(e core.Environment, body ...core.Instance) (core.Instance, core.Instance) {
	uid := core.NewInteger(uniqueInt())
	for _, cadr := range body {
		if !core.InstanceOf(core.ConsClass, cadr) {
			if !e.TagbodyTag.Define(cadr, uid) { // ref cddr
				return SignalCondition(e, core.NewImmutableBinding(e), Nil)
			}
		}
	}
	for idx, cadr := range body {
		if core.InstanceOf(core.ConsClass, cadr) {
			_, fail := Eval(e, cadr)
			if fail != nil {
			TAG:
				if core.InstanceOf(core.TagbodyTagClass, fail) {
					tag1, _ := fail.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.TAG"), core.EscapeClass) // Checked at the top of// This loop
					uid1, _ := fail.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.UID"), core.EscapeClass) // Checked at the top of// This loop
					found := false
					for _, tag := range body {
						if tag == tag1 && uid == uid1 {
							found = true
							break
						}
					}
					if found {
						for _, form := range body[idx+1:] {
							if core.InstanceOf(core.ConsClass, form) {
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

func Go(e core.Environment, tag core.Instance) (core.Instance, core.Instance) {
	uid, ok := e.TagbodyTag.Get(tag)
	if !ok {
		return SignalCondition(e, core.NewControlError(e), Nil)
	}
	return nil, core.NewTagbodyTag(tag, uid)
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
func UnwindProtect(e core.Environment, form core.Instance, cleanupForms ...core.Instance) (core.Instance, core.Instance) {
	ret1, err1 := Eval(e, form)
	ret2, err2 := Progn(e, cleanupForms...)
	if err2 != nil {
		if core.InstanceOf(core.EscapeClass, err2) {
			return SignalCondition(e, core.NewControlError(e), Nil)
		}
		return ret2, err2
	}
	return ret1, err1
}
