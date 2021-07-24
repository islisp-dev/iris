// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func Convert(e ilos.Environment, object, class1 ilos.Instance) (ilos.Instance, ilos.Instance) {
	object, err := Eval(e, object)
	if err != nil {
		return nil, err
	}
	class1, err = Class(e, class1)
	if err != nil {
		return nil, err
	}
	switch object.Class().String() {
	case instance.CharacterClass.String():
		switch class1.(instance.BuiltInClass).String() {
		case instance.CharacterClass.String():
			return object, nil
		case instance.IntegerClass.String():
			return instance.NewInteger(int(rune(object.(instance.Character)))), nil
		case instance.FloatClass.String():
		case instance.SymbolClass.String():
		case instance.StringClass.String():
			return instance.NewString([]rune(object.String()[2:])), nil
		case instance.GeneralVectorClass.String():
		case instance.ListClass.String():
		}
	case instance.IntegerClass.String():
		switch class1.String() {
		case instance.CharacterClass.String():
			return instance.NewCharacter(rune(int(object.(instance.Integer)))), nil
		case instance.IntegerClass.String():
			return object, nil
		case instance.FloatClass.String():
			return instance.NewFloat(float64(int(object.(instance.Integer)))), nil
		case instance.SymbolClass.String():
		case instance.StringClass.String():
			return instance.NewString([]rune(object.String())), nil
		case instance.GeneralVectorClass.String():
		case instance.ListClass.String():
		}
	case instance.FloatClass.String():
		switch class1.String() {
		case instance.CharacterClass.String():
		case instance.IntegerClass.String():
			return instance.NewInteger(int(float64(object.(instance.Float)))), nil
		case instance.FloatClass.String():
			return object, nil
		case instance.SymbolClass.String():
		case instance.StringClass.String():
			return instance.NewString([]rune(object.String())), nil
		case instance.GeneralVectorClass.String():
		case instance.ListClass.String():
		}
	case instance.SymbolClass.String():
		switch class1.String() {
		case instance.CharacterClass.String():
		case instance.IntegerClass.String():
		case instance.FloatClass.String():
		case instance.SymbolClass.String():
			return object, nil
		case instance.StringClass.String():
			return instance.NewString([]rune(object.String())), nil
		case instance.GeneralVectorClass.String():
		case instance.ListClass.String():
		}
	case instance.StringClass.String():
		switch class1.String() {
		case instance.CharacterClass.String():
		case instance.IntegerClass.String():
			return ParseNumber(e, object)
		case instance.FloatClass.String():
			return ParseNumber(e, object)
		case instance.SymbolClass.String():
		case instance.StringClass.String():
			return object, nil
		case instance.GeneralVectorClass.String():
			v := make([]ilos.Instance, len(object.(instance.String)))
			for i, c := range object.(instance.String) {
				v[i] = instance.NewCharacter(c)
			}
			return instance.NewGeneralVector(v), nil
		case instance.ListClass.String():
			l := Nil
			s := object.(instance.String)
			for i := len(s) - 1; i >= 0; i-- {
				l = instance.NewCons(instance.NewCharacter(s[i]), l)
			}
			return l, nil
		}
	case instance.GeneralVectorClass.String():
		switch class1.String() {
		case instance.CharacterClass.String():
		case instance.IntegerClass.String():
		case instance.FloatClass.String():
		case instance.SymbolClass.String():
		case instance.StringClass.String():
		case instance.GeneralVectorClass.String():
			return object, nil
		case instance.ListClass.String():
			return List(e, object.(instance.GeneralVector)...)
		}
	case instance.ListClass.String():
		switch class1.String() {
		case instance.CharacterClass.String():
		case instance.IntegerClass.String():
		case instance.FloatClass.String():
		case instance.SymbolClass.String():
		case instance.StringClass.String():
		case instance.GeneralVectorClass.String():
			v := instance.NewGeneralVector([]ilos.Instance{})
			car, cdr, i := object.(*instance.Cons).Car, object.(*instance.Cons).Cdr, 0
			for cdr != Nil {
				SetElt(e, car, v, instance.NewInteger(i))
				car, cdr, i = cdr.(*instance.Cons).Car, cdr.(*instance.Cons).Cdr, i+1
			}
			return v, nil
		case instance.ListClass.String():
			return object, nil
		}
	}
	return SignalCondition(e, instance.NewDomainError(e, object, instance.ObjectClass), Nil)
}
