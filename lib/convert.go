// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/core"

func Convert(e core.Environment, object, class1 core.Instance) (core.Instance, core.Instance) {
	object, err := Eval(e, object)
	if err != nil {
		return nil, err
	}
	class1, err = Class(e, class1)
	if err != nil {
		return nil, err
	}
	switch object.Class().String() {
	case core.CharacterClass.String():
		switch class1.(core.BuiltInClass).String() {
		case core.CharacterClass.String():
			return object, nil
		case core.IntegerClass.String():
			return core.NewInteger(int(rune(object.(core.Character)))), nil
		case core.FloatClass.String():
		case core.SymbolClass.String():
		case core.StringClass.String():
			return core.NewString([]rune(object.String()[2:])), nil
		case core.GeneralVectorClass.String():
		case core.ListClass.String():
		}
	case core.IntegerClass.String():
		switch class1.String() {
		case core.CharacterClass.String():
			return core.NewCharacter(rune(int(object.(core.Integer)))), nil
		case core.IntegerClass.String():
			return object, nil
		case core.FloatClass.String():
			return core.NewFloat(float64(int(object.(core.Integer)))), nil
		case core.SymbolClass.String():
		case core.StringClass.String():
			return core.NewString([]rune(object.String())), nil
		case core.GeneralVectorClass.String():
		case core.ListClass.String():
		}
	case core.FloatClass.String():
		switch class1.String() {
		case core.CharacterClass.String():
		case core.IntegerClass.String():
			return core.NewInteger(int(float64(object.(core.Float)))), nil
		case core.FloatClass.String():
			return object, nil
		case core.SymbolClass.String():
		case core.StringClass.String():
			return core.NewString([]rune(object.String())), nil
		case core.GeneralVectorClass.String():
		case core.ListClass.String():
		}
	case core.SymbolClass.String():
		switch class1.String() {
		case core.CharacterClass.String():
		case core.IntegerClass.String():
		case core.FloatClass.String():
		case core.SymbolClass.String():
			return object, nil
		case core.StringClass.String():
			return core.NewString([]rune(object.String())), nil
		case core.GeneralVectorClass.String():
		case core.ListClass.String():
		}
	case core.StringClass.String():
		switch class1.String() {
		case core.CharacterClass.String():
		case core.IntegerClass.String():
			return ParseNumber(e, object)
		case core.FloatClass.String():
			return ParseNumber(e, object)
		case core.SymbolClass.String():
		case core.StringClass.String():
			return object, nil
		case core.GeneralVectorClass.String():
			v := make([]core.Instance, len(object.(core.String)))
			for i, c := range object.(core.String) {
				v[i] = core.NewCharacter(c)
			}
			return core.NewGeneralVector(v), nil
		case core.ListClass.String():
			l := Nil
			s := object.(core.String)
			for i := len(s) - 1; i >= 0; i-- {
				l = core.NewCons(core.NewCharacter(s[i]), l)
			}
			return l, nil
		}
	case core.GeneralVectorClass.String():
		switch class1.String() {
		case core.CharacterClass.String():
		case core.IntegerClass.String():
		case core.FloatClass.String():
		case core.SymbolClass.String():
		case core.StringClass.String():
		case core.GeneralVectorClass.String():
			return object, nil
		case core.ListClass.String():
			return List(e, object.(core.GeneralVector)...)
		}
	case core.ListClass.String():
		switch class1.String() {
		case core.CharacterClass.String():
		case core.IntegerClass.String():
		case core.FloatClass.String():
		case core.SymbolClass.String():
		case core.StringClass.String():
		case core.GeneralVectorClass.String():
			v := core.NewGeneralVector([]core.Instance{})
			car, cdr, i := object.(*core.Cons).Car, object.(*core.Cons).Cdr, 0
			for cdr != Nil {
				SetElt(e, car, v, core.NewInteger(i))
				car, cdr, i = cdr.(*core.Cons).Car, cdr.(*core.Cons).Cdr, i+1
			}
			return v, nil
		case core.ListClass.String():
			return object, nil
		}
	}
	return SignalCondition(e, core.NewDomainError(e, object, core.ObjectClass), Nil)
}
