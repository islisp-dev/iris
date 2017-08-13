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

//TODO: char<, char>, char<=, char>=

// Characterp returns t if obj is a character (instance of class character);
// otherwise, returns nil. obj may be any ISLISP object.
func Characterp(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Character, obj) {
		return T, nil
	}
	return Nil, nil
}

// CharEqual tests whether char1 is the same character as char2.
func CharEqual(_, _ *environment.Environment, c1, c2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if !instance.Of(class.Character, c1) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         c1,
			"EXPECTED-CLASS": class.Character,
		})
	}
	if !instance.Of(class.Character, c2) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         c2,
			"EXPECTED-CLASS": class.Character,
		})
	}
	if c1 == c2 {
		return T, nil
	}
	return Nil, nil
}

// CharNotEqual if and only if they are not char=.
func CharNotEqual(_, _ *environment.Environment, c1, c2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := CharEqual(nil, nil, c1, c2)
	if err != nil {
		return nil, err
	}
	return Not(nil, nil, ret)
}

// CharGreaterThan tests whether char1 is greater than char2.
// An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).
func CharGreaterThan(_, _ *environment.Environment, c1, c2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if !instance.Of(class.Character, c1) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         c1,
			"EXPECTED-CLASS": class.Character,
		})
	}
	if !instance.Of(class.Character, c2) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         c2,
			"EXPECTED-CLASS": class.Character,
		})
	}
	if c1.(instance.Character) > c2.(instance.Character) {
		return T, nil
	}
	return Nil, nil
}

// CharGreaterThanOrEqual tests whether char1 is greater than or equal to char2.
// An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).
func CharGreaterThanOrEqual(_, _ *environment.Environment, c1, c2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gt, err := CharGreaterThan(nil, nil, c1, c2)
	if err != nil {
		return nil, err
	}
	eq, err := CharEqual(nil, nil, c1, c2)
	if err != nil {
		return nil, err
	}
	if gt == Nil && eq == Nil {
		return Nil, nil
	}
	return T, nil
}

// CharLessThan tests whether char1 is less than char2.
// An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).
func CharLessThan(_, _ *environment.Environment, c1, c2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gt, err := CharGreaterThanOrEqual(nil, nil, c1, c2)
	if err != nil {
		return nil, err
	}
	return Not(nil, nil, gt)
}

// CharLessThanOrEqual tests whether char1 is less than or equal to char2.
// An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).
func CharLessThanOrEqual(_, _ *environment.Environment, c1, c2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gt, err := CharGreaterThan(nil, nil, c1, c2)
	if err != nil {
		return nil, err
	}
	return Not(nil, nil, gt)
}
