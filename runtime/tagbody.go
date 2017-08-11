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

func tagbody(local, global *environment.Environment, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	for idx, cadr := range body {
		cddr := instance.New(class.GeneralVector, body[idx+1:])
		if !instance.Of(class.Cons, cadr) {
			local.TagbodyTag.Define(cadr, cddr)
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
	return instance.New(class.Null), nil
}

func tagbodyGo(local, global *environment.Environment, tag ilos.Instance) (ilos.Instance, ilos.Instance) {
	if _, ok := local.TagbodyTag.Get(tag); !ok {
		return nil, instance.New(class.SimpleError, map[string]ilos.Instance{
			"FORMAT-STRING":    instance.New(class.String, "%v is not defined as the tag"),
			"FORMAT-ARGUMENTS": tag,
		})
	}
	return nil, instance.New(class.TagbodyTag, map[string]ilos.Instance{"TAG": tag})
}
