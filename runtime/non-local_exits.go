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

/*
ISLISP defines three ways in which to perform non-local exits:
  Destination   Kind     Established by  Invoked by  Operation Performed
  block name    block    return-from     lexical     exit
  tagbody tag   tagbody  go              lexical     transfer of control
  catch tag     atch     throw           dynamic     exit

A non-local exit, is an operation that forces transfer of control and possibly
data from an invoking special form to a previously established point in a program,
called the destination of the exit.

A lexical exit is a non-local exit from a return-from form to a block form
which contains it both lexically and dynamically, forcing the block to return
an object specified in the return-from form.

A dynamic exit is a non-local exit from a throw form to a catch form
which contains it dynamically (but not necessarily lexically),
forcing the catch to return an object specified in the throw form.

A lexical transfer of control is a non-local exit from a go form to a tagged point
in a tagbody form which contains it both lexically and dynamically.

When a non-local exit is initiated, any potential destination that was established
more recently than the destination to which control is being transferred
is immediately considered invalid.
*/

func Block(local, global *environment.Environment, tag ilos.Instance, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(local, global, tag) // Checked at the top of// This function
	if err != nil {
		return nil, err
	}
	if instance.Of(class.Number, tag) || instance.Of(class.Character, tag) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         tag,
			"EXPECTED-CLASS": class.Object,
		})
	}
	if !local.BlockTag.Define(tag, nil) {
		return nil, instance.New(class.ProgramError)
	}
	var fail ilos.Instance
	sucess := Nil
	for _, cadr := range body {
		sucess, fail = Eval(local, global, cadr)
		if fail != nil {
			if instance.Of(class.BlockTag, fail) {
				tag1, _ := fail.GetSlotValue(instance.New(class.Symbol, "TAG"), class.Escape) // Checked at the head of// This condition
				if tag == tag1 {
					obj, _ := fail.GetSlotValue(instance.New(class.Symbol, "OBJECT"), class.BlockTag) // Checked at the head of// This condition
					return obj, nil
				}
			}
			return nil, fail
		}
	}
	return sucess, nil
}

func ReturnFrom(local, global *environment.Environment, tag, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(local, global, tag)
	if err != nil {
		return nil, err
	}
	if instance.Of(class.Number, tag) || instance.Of(class.Character, tag) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         tag,
			"EXPECTED-CLASS": class.Object,
		})
	}
	object, err = Eval(local, global, object)
	if err != nil {
		return nil, err
	}
	if _, ok := local.BlockTag.Get(tag); !ok {
		return nil, instance.New(class.SimpleError, map[string]ilos.Instance{
			"FORMAT-STRING":    instance.New(class.String, "%v is not defined as the tag"),
			"FORMAT-ARGUMENTS": tag,
		})
	}
	return nil, instance.New(class.BlockTag, map[string]ilos.Instance{
		"TAG":    tag,
		"OBJECT": object,
	})
}

func Catch(local, global *environment.Environment, tag ilos.Instance, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(local, global, tag)
	if err != nil {
		return nil, err
	}
	if instance.Of(class.Number, tag) || instance.Of(class.Character, tag) {
		return nil, instance.New(class.DomainError, tag, class.Object)
	}
	if !local.CatchTag.Define(tag, nil) {
		return nil, instance.New(class.ProgramError)
	}
	var fail ilos.Instance
	sucess := Nil
	for _, cadr := range body {
		sucess, fail = Eval(local, global, cadr)
		if fail != nil {
			if instance.Of(class.CatchTag, fail) {
				tag1, _ := fail.GetSlotValue(instance.New(class.Symbol, "TAG"), class.Escape) // Checked at the head of// This condition
				if tag == tag1 {
					obj, _ := fail.GetSlotValue(instance.New(class.Symbol, "OBJECT"), class.CatchTag) // Checked at the head of// This condition
					return obj, nil
				}
			}
			return nil, fail
		}
	}
	return sucess, nil
}

func Throw(local, global *environment.Environment, tag, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(local, global, tag)
	if err != nil {
		return nil, err
	}
	if instance.Of(class.Number, tag) || instance.Of(class.Character, tag) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         tag,
			"EXPECTED-CLASS": class.Object,
		})
	}
	object, err = Eval(local, global, object)
	if err != nil {
		return nil, err
	}
	if _, ok := local.CatchTag.Get(tag); !ok {
		return nil, instance.New(class.SimpleError, map[string]ilos.Instance{
			"FORMAT-STRING":    instance.New(class.String, "%v is not defined as the tag"),
			"FORMAT-ARGUMENTS": tag,
		})
	}
	return nil, instance.New(class.CatchTag, map[string]ilos.Instance{
		"TAG":    tag,
		"OBJECT": object,
	})
}

func Tagbody(local, global *environment.Environment, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	for idx, cadr := range body {
		cddr := instance.New(class.GeneralVector, body[idx+1:])
		if !instance.Of(class.Cons, cadr) {
			if !local.TagbodyTag.Define(cadr, cddr) {
				return nil, instance.New(class.ProgramError)
			}
		}
	}
	for _, cadr := range body {
		if instance.Of(class.Cons, cadr) {
			_, fail := Eval(local, global, cadr)
			if fail != nil {
			TAG:
				if instance.Of(class.TagbodyTag, fail) {
					tag, _ := fail.GetSlotValue(instance.New(class.Symbol, "TAG"), class.Escape) // Checked at the top of// This loop
					found := false
					for _, tag1 := range body {
						if tag == tag1 {
							found = true
							break
						}
					}
					if found {
						forms, _ := local.TagbodyTag.Get(tag) // Checked in the function, tagbodyGo
						for _, form := range forms.(instance.GeneralVector) {
							if instance.Of(class.Cons, form) {
								_, fail = Eval(local, global, form)
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

func Go(local, global *environment.Environment, tag ilos.Instance) (ilos.Instance, ilos.Instance) {
	if _, ok := local.TagbodyTag.Get(tag); !ok {
		return nil, instance.New(class.SimpleError, map[string]ilos.Instance{
			"FORMAT-STRING":    instance.New(class.String, "%v is not defined as the tag"),
			"FORMAT-ARGUMENTS": tag,
		})
	}
	return nil, instance.New(class.TagbodyTag, map[string]ilos.Instance{"TAG": tag})
}
