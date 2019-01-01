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
	object, err := Eval(e, object)
	if err != nil {
		return nil, err
	}
	class1, err = Class(e, class1)
	if err != nil {
		return nil, err
	}
	switch object.Class().String() {
	case class.Character.String():
		switch class1.(instance.BuiltInClass).String() {
		case class.Character.String():
			return object, nil
		case class.Integer.String():
			return instance.NewInteger(int(rune(object.(instance.Character)))), nil
		case class.Float.String():
		case class.Symbol.String():
		case class.String.String():
			return instance.NewString([]rune(object.String()[2:])), nil
		case class.GeneralVector.String():
		case class.List.String():
		}
	case class.Integer.String():
		switch class1.String() {
		case class.Character.String():
			return instance.NewCharacter(rune(int(object.(instance.Integer)))), nil
		case class.Integer.String():
			return object, nil
		case class.Float.String():
			return instance.NewFloat(float64(int(object.(instance.Integer)))), nil
		case class.Symbol.String():
		case class.String.String():
			return instance.NewString([]rune(object.String())), nil
		case class.GeneralVector.String():
		case class.List.String():
		}
	case class.Float.String():
		switch class1.String() {
		case class.Character.String():
		case class.Integer.String():
			return instance.NewInteger(int(float64(object.(instance.Float)))), nil
		case class.Float.String():
			return object, nil
		case class.Symbol.String():
		case class.String.String():
			return instance.NewString([]rune(object.String())), nil
		case class.GeneralVector.String():
		case class.List.String():
		}
	case class.Symbol.String():
		switch class1.String() {
		case class.Character.String():
		case class.Integer.String():
		case class.Float.String():
		case class.Symbol.String():
			return object, nil
		case class.String.String():
			return instance.NewString([]rune(object.String())), nil
		case class.GeneralVector.String():
		case class.List.String():
		}
	case class.String.String():
		switch class1.String() {
		case class.Character.String():
		case class.Integer.String():
			return ParseNumber(e, object)
		case class.Float.String():
			return ParseNumber(e, object)
		case class.Symbol.String():
		case class.String.String():
			return object, nil
		case class.GeneralVector.String():
			v := make([]ilos.Instance, len(object.(instance.String)))
			for i, c := range object.(instance.String) {
				v[i] = instance.NewCharacter(c)
			}
			return instance.NewGeneralVector(v), nil
		case class.List.String():
			l := Nil
			s := object.(instance.String)
			for i := len(s) - 1; i >= 0; i-- {
				l = instance.NewCons(instance.NewCharacter(s[i]), l)
			}
			return l, nil
		}
	case class.GeneralVector.String():
		switch class1.String() {
		case class.Character.String():
		case class.Integer.String():
		case class.Float.String():
		case class.Symbol.String():
		case class.String.String():
		case class.GeneralVector.String():
			return object, nil
		case class.List.String():
			return List(e, object.(instance.GeneralVector)...)
		}
	case class.List.String():
		switch class1.String() {
		case class.Character.String():
		case class.Integer.String():
		case class.Float.String():
		case class.Symbol.String():
		case class.String.String():
		case class.GeneralVector.String():
			v := instance.NewGeneralVector([]ilos.Instance{})
			car, cdr, i := object.(*instance.Cons).Car, object.(*instance.Cons).Cdr, 0
			for cdr != Nil {
				SetElt(e, car, v, instance.NewInteger(i))
				car, cdr, i = cdr.(*instance.Cons).Car, cdr.(*instance.Cons).Cdr, i+1
			}
			return v, nil
		case class.List.String():
			return object, nil
		}
	}
	return SignalCondition(e, instance.NewDomainError(e, object, class.Object), Nil)
}
