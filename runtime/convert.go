// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func Convert(e env.Environment, object, class1 ilos.Instance) (ilos.Instance, ilos.Instance) {
	switch object.Class() {
	case class.Character:
		switch class1 {
		case instance.NewSymbol("<CHARACTER>"):
			return object, nil
		case instance.NewSymbol("<INTEGER>"):
			return instance.NewInteger(int(rune(object.(instance.Character)))), nil
		case instance.NewSymbol("<FLOAT>"):
		case instance.NewSymbol("<SYMBOL>"):
		case instance.NewSymbol("<STRING>"):
			return instance.NewString([]rune(object.String()[2:])), nil
		case instance.NewSymbol("<GENERAL-VECTOR>"):
		case instance.NewSymbol("<LIST>"):
		}
	case class.Integer:
		switch class1 {
		case instance.NewSymbol("<CHARACTER>"):
			return instance.NewCharacter(rune(int(object.(instance.Integer)))), nil
		case instance.NewSymbol("<INTEGER>"):
			return object, nil
		case instance.NewSymbol("<FLOAT>"):
			return instance.NewFloat(float64(int(object.(instance.Integer)))), nil
		case instance.NewSymbol("<SYMBOL>"):
		case instance.NewSymbol("<STRING>"):
			return instance.NewString([]rune(object.String())), nil
		case instance.NewSymbol("<GENERAL-VECTOR>"):
		case instance.NewSymbol("<LIST>"):
		}
	case class.Float:
		switch class1 {
		case instance.NewSymbol("<CHARACTER>"):
		case instance.NewSymbol("<INTEGER>"):
		case instance.NewSymbol("<FLOAT>"):
			return object, nil
		case instance.NewSymbol("<SYMBOL>"):
		case instance.NewSymbol("<STRING>"):
			return instance.NewString([]rune(object.String())), nil
		case instance.NewSymbol("<GENERAL-VECTOR>"):
		case instance.NewSymbol("<LIST>"):
		}
	case class.Symbol:
		switch class1 {
		case instance.NewSymbol("<CHARACTER>"):
		case instance.NewSymbol("<INTEGER>"):
		case instance.NewSymbol("<FLOAT>"):
		case instance.NewSymbol("<SYMBOL>"):
			return object, nil
		case instance.NewSymbol("<STRING>"):
			return instance.NewString([]rune(object.String())), nil
		case instance.NewSymbol("<GENERAL-VECTOR>"):
		case instance.NewSymbol("<LIST>"):
		}
	case class.String:
		switch class1 {
		case instance.NewSymbol("<CHARACTER>"):
		case instance.NewSymbol("<INTEGER>"):
			return ParseNumber(e, object)
		case instance.NewSymbol("<FLOAT>"):
			return ParseNumber(e, object)
		case instance.NewSymbol("<SYMBOL>"):
		case instance.NewSymbol("<STRING>"):
			return object, nil
		case instance.NewSymbol("<GENERAL-VECTOR>"):
			v := make([]ilos.Instance, len(object.(instance.String)))
			for i, c := range object.(instance.String) {
				v[i] = instance.NewCharacter(c)
			}
			return instance.NewGeneralVector(v), nil
		case instance.NewSymbol("<LIST>"):
			l := Nil
			s := object.(instance.String)
			for i := len(s) - 1; i >= 0; i-- {
				l = instance.NewCons(instance.NewCharacter(s[i]), l)
			}
			return l, nil
		}
	case class.GeneralVector:
		switch class1 {
		case instance.NewSymbol("<CHARACTER>"):
		case instance.NewSymbol("<INTEGER>"):
		case instance.NewSymbol("<FLOAT>"):
		case instance.NewSymbol("<SYMBOL>"):
		case instance.NewSymbol("<STRING>"):
		case instance.NewSymbol("<GENERAL-VECTOR>"):
			return object, nil
		case instance.NewSymbol("<LIST>"):
			return List(e, object.(instance.GeneralVector)...)
		}
	case class.List:
		switch class1 {
		case instance.NewSymbol("<CHARACTER>"):
		case instance.NewSymbol("<INTEGER>"):
		case instance.NewSymbol("<FLOAT>"):
		case instance.NewSymbol("<SYMBOL>"):
		case instance.NewSymbol("<STRING>"):
		case instance.NewSymbol("<GENERAL-VECTOR>"):
			v := instance.NewGeneralVector([]ilos.Instance{})
			car, cdr, i := object.(*instance.Cons).Car, object.(*instance.Cons).Cdr, 0
			for cdr != Nil {
				SetElt(e, car, v, instance.NewInteger(i))
				car, cdr, i = cdr.(*instance.Cons).Car, cdr.(*instance.Cons).Cdr, i+1
			}
			return v, nil
		case instance.NewSymbol("<LIST>"):
			return object, nil
		}
	}
	return SignalCondition(e, instance.NewDomainError(e, object, class.Object), Nil)
}
