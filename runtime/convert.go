// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
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
	case ilos.CharacterClass.String():
		switch class1.(ilos.BuiltInClass).String() {
		case ilos.CharacterClass.String():
			return object, nil
		case ilos.IntegerClass.String():
			return ilos.NewInteger(int(rune(object.(ilos.Character)))), nil
		case ilos.FloatClass.String():
		case ilos.SymbolClass.String():
		case ilos.StringClass.String():
			return ilos.NewString([]rune(object.String()[2:])), nil
		case ilos.GeneralVectorClass.String():
		case ilos.ListClass.String():
		}
	case ilos.IntegerClass.String():
		switch class1.String() {
		case ilos.CharacterClass.String():
			return ilos.NewCharacter(rune(int(object.(ilos.Integer)))), nil
		case ilos.IntegerClass.String():
			return object, nil
		case ilos.FloatClass.String():
			return ilos.NewFloat(float64(int(object.(ilos.Integer)))), nil
		case ilos.SymbolClass.String():
		case ilos.StringClass.String():
			return ilos.NewString([]rune(object.String())), nil
		case ilos.GeneralVectorClass.String():
		case ilos.ListClass.String():
		}
	case ilos.FloatClass.String():
		switch class1.String() {
		case ilos.CharacterClass.String():
		case ilos.IntegerClass.String():
			return ilos.NewInteger(int(float64(object.(ilos.Float)))), nil
		case ilos.FloatClass.String():
			return object, nil
		case ilos.SymbolClass.String():
		case ilos.StringClass.String():
			return ilos.NewString([]rune(object.String())), nil
		case ilos.GeneralVectorClass.String():
		case ilos.ListClass.String():
		}
	case ilos.SymbolClass.String():
		switch class1.String() {
		case ilos.CharacterClass.String():
		case ilos.IntegerClass.String():
		case ilos.FloatClass.String():
		case ilos.SymbolClass.String():
			return object, nil
		case ilos.StringClass.String():
			return ilos.NewString([]rune(object.String())), nil
		case ilos.GeneralVectorClass.String():
		case ilos.ListClass.String():
		}
	case ilos.StringClass.String():
		switch class1.String() {
		case ilos.CharacterClass.String():
		case ilos.IntegerClass.String():
			return ParseNumber(e, object)
		case ilos.FloatClass.String():
			return ParseNumber(e, object)
		case ilos.SymbolClass.String():
		case ilos.StringClass.String():
			return object, nil
		case ilos.GeneralVectorClass.String():
			v := make([]ilos.Instance, len(object.(ilos.String)))
			for i, c := range object.(ilos.String) {
				v[i] = ilos.NewCharacter(c)
			}
			return ilos.NewGeneralVector(v), nil
		case ilos.ListClass.String():
			l := Nil
			s := object.(ilos.String)
			for i := len(s) - 1; i >= 0; i-- {
				l = ilos.NewCons(ilos.NewCharacter(s[i]), l)
			}
			return l, nil
		}
	case ilos.GeneralVectorClass.String():
		switch class1.String() {
		case ilos.CharacterClass.String():
		case ilos.IntegerClass.String():
		case ilos.FloatClass.String():
		case ilos.SymbolClass.String():
		case ilos.StringClass.String():
		case ilos.GeneralVectorClass.String():
			return object, nil
		case ilos.ListClass.String():
			return List(e, object.(ilos.GeneralVector)...)
		}
	case ilos.ListClass.String():
		switch class1.String() {
		case ilos.CharacterClass.String():
		case ilos.IntegerClass.String():
		case ilos.FloatClass.String():
		case ilos.SymbolClass.String():
		case ilos.StringClass.String():
		case ilos.GeneralVectorClass.String():
			v := ilos.NewGeneralVector([]ilos.Instance{})
			car, cdr, i := object.(*ilos.Cons).Car, object.(*ilos.Cons).Cdr, 0
			for cdr != Nil {
				SetElt(e, car, v, ilos.NewInteger(i))
				car, cdr, i = cdr.(*ilos.Cons).Car, cdr.(*ilos.Cons).Cdr, i+1
			}
			return v, nil
		case ilos.ListClass.String():
			return object, nil
		}
	}
	return SignalCondition(e, ilos.NewDomainError(e, object, ilos.ObjectClass), Nil)
}
