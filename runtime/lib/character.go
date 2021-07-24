// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/runtime/core"

// Characterp returns t if obj is a character (instance of class character);
// otherwise, returns nil. obj may be any ISLISP object.
func Characterp(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.CharacterClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// CharEqual tests whether char1 is the same character as char2.
func CharEqual(e core.Environment, char1, char2 core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.CharacterClass, char1, char2); err != nil {
		return nil, err
	}
	if core.DeepEqual(char1, char2) {
		return T, nil
	}
	return Nil, nil
}

// CharNotEqual if and only if they are not char=.
func CharNotEqual(e core.Environment, char1, char2 core.Instance) (core.Instance, core.Instance) {
	ret, err := CharEqual(e, char1, char2)
	if err != nil {
		return nil, err
	}
	return Not(e, ret)
}

// CharGreaterThan tests whether char1 is greater than char2. An error shall be
// signaled if either char1 or char2 is not a character (error-id. domain-error).
func CharGreaterThan(e core.Environment, char1, char2 core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.CharacterClass, char1, char2); err != nil {
		return nil, err
	}
	if char1.(core.Character) > char2.(core.Character) {
		return T, nil
	}
	return Nil, nil
}

// CharGreaterThanOrEqual tests whether char1 is greater than or equal to char2.
// An error shall be signaled if either char1 or char2 is not a character
// (error-id. domain-error).
func CharGreaterThanOrEqual(e core.Environment, char1, char2 core.Instance) (core.Instance, core.Instance) {
	gt, err := CharGreaterThan(e, char1, char2)
	if err != nil {
		return nil, err
	}
	eq, err := CharEqual(e, char1, char2)
	if err != nil {
		return nil, err
	}
	if core.DeepEqual(gt, Nil) && core.DeepEqual(eq, Nil) {
		return Nil, nil
	}
	return T, nil
}

// CharLessThan tests whether char1 is less than char2. An error shall be
// signaled if either char1 or char2 is not a character (error-id. domain-error).
func CharLessThan(e core.Environment, char1, char2 core.Instance) (core.Instance, core.Instance) {
	gt, err := CharGreaterThanOrEqual(e, char1, char2)
	if err != nil {
		return nil, err
	}
	return Not(e, gt)
}

// CharLessThanOrEqual tests whether char1 is less than or equal to char2. An
// error shall be signaled if either char1 or char2 is not a character
// (error-id. domain-error).
func CharLessThanOrEqual(e core.Environment, char1, char2 core.Instance) (core.Instance, core.Instance) {
	gt, err := CharGreaterThan(e, char1, char2)
	if err != nil {
		return nil, err
	}
	return Not(e, gt)
}
