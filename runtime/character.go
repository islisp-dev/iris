// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
)

// Characterp returns t if obj is a character (instance of class character);
// otherwise, returns nil. obj may be any ISLISP object.
func Characterp(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(ilos.CharacterClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// CharEqual tests whether char1 is the same character as char2.
func CharEqual(e ilos.Environment, char1, char2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.CharacterClass, char1, char2); err != nil {
		return nil, err
	}
	if char1 == char2 {
		return T, nil
	}
	return Nil, nil
}

// CharNotEqual if and only if they are not char=.
func CharNotEqual(e ilos.Environment, char1, char2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := CharEqual(e, char1, char2)
	if err != nil {
		return nil, err
	}
	return Not(e, ret)
}

// CharGreaterThan tests whether char1 is greater than char2. An error shall be
// signaled if either char1 or char2 is not a character (error-id. domain-error).
func CharGreaterThan(e ilos.Environment, char1, char2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.CharacterClass, char1, char2); err != nil {
		return nil, err
	}
	if char1.(ilos.Character) > char2.(ilos.Character) {
		return T, nil
	}
	return Nil, nil
}

// CharGreaterThanOrEqual tests whether char1 is greater than or equal to char2.
// An error shall be signaled if either char1 or char2 is not a character
// (error-id. domain-error).
func CharGreaterThanOrEqual(e ilos.Environment, char1, char2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gt, err := CharGreaterThan(e, char1, char2)
	if err != nil {
		return nil, err
	}
	eq, err := CharEqual(e, char1, char2)
	if err != nil {
		return nil, err
	}
	if gt == Nil && eq == Nil {
		return Nil, nil
	}
	return T, nil
}

// CharLessThan tests whether char1 is less than char2. An error shall be
// signaled if either char1 or char2 is not a character (error-id. domain-error).
func CharLessThan(e ilos.Environment, char1, char2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gt, err := CharGreaterThanOrEqual(e, char1, char2)
	if err != nil {
		return nil, err
	}
	return Not(e, gt)
}

// CharLessThanOrEqual tests whether char1 is less than or equal to char2. An
// error shall be signaled if either char1 or char2 is not a character
// (error-id. domain-error).
func CharLessThanOrEqual(e ilos.Environment, char1, char2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gt, err := CharGreaterThan(e, char1, char2)
	if err != nil {
		return nil, err
	}
	return Not(e, gt)
}
