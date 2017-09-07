// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"strings"

	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// Stringp returns t if obj is a string (instance of class string); otherwise,
// returns nil. obj may be any ISLISP object.
func Stringp(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(class.String, obj) {
		return T, nil
	}
	return Nil, nil
}

// CreateString returns a string of length i. If initial-character is given,
// then the characters of the new string are initialized with this character,
// otherwise the initialization is implementation defined. An error shall be
// signaled if the requested string cannot be allocated (error-id.
// cannot-create-string). An error shall be signaled if i is not a non-negative
// integer or if initial-character is not a character (error-id. domain-error).
func CreateString(e env.Environment, i ilos.Instance, initialElement ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if !ilos.InstanceOf(class.Integer, i) || int(i.(instance.Integer)) < 0 {
		return SignalCondition(e, instance.NewDomainError(e, i, class.Object), Nil)
	}
	if len(initialElement) > 1 {
		return SignalCondition(e, instance.NewArityError(e), Nil)
	}
	n := int(i.(instance.Integer))
	v := make([]rune, n)
	for i := 0; i < n; i++ {
		if len(initialElement) == 0 {
			v[i] = 0
		} else {
			if err := ensure(e, class.Character, initialElement[0]); err != nil {
				return nil, err
			}
			v[i] = rune(initialElement[0].(instance.Character))
		}
	}
	return instance.NewString(v), nil
}

// StringEqual tests whether string1 is the same string as string2.
func StringEqual(e env.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.String, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(instance.String)) == string(string2.(instance.String)) {
		return T, nil
	}
	return Nil, nil
}

// StringNotEqual tests whether string1 not is the same string as string2.
func StringNotEqual(e env.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := StringEqual(e, string1, string2)
	if err != nil {
		return nil, err
	}
	return Not(e, ret)
}

// StringGreaterThan tests whether string1 is greater than string2.
func StringGreaterThan(e env.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.String, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(instance.String)) > string(string2.(instance.String)) {
		return T, nil
	}
	return Nil, nil
}

// StringGreaterThanOrEqual tests whether string1 is greater than or equal to
// string2.
func StringGreaterThanOrEqual(e env.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.String, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(instance.String)) >= string(string2.(instance.String)) {
		return T, nil
	}
	return Nil, nil
}

// StringLessThan tests whether string1 is less than string2.
func StringLessThan(e env.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.String, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(instance.String)) < string(string2.(instance.String)) {
		return T, nil
	}
	return Nil, nil
}

// StringLessThanOrEqual tests whether string1 is less than or equal to string2.
func StringLessThanOrEqual(e env.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.String, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(instance.String)) <= string(string2.(instance.String)) {
		return T, nil
	}
	return Nil, nil
}

// CharIndex returns the position of char in string, The search starts from the
// position indicated by start-position (which is 0-based and defaults to 0).
// The value returned if the search succeeds is an offset from the beginning of
// the string, not from the starting point. If the char does not occur in the
// string, nil is returned. The function char= is used for the comparisons. An
// error shall be signaled if char is not a character or if string is not a
// string (error-id. domain-error).
func CharIndex(e env.Environment, char, str ilos.Instance, startPosition ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.Character, char); err != nil {
		return nil, err
	}
	if err := ensure(e, class.String, str); err != nil {
		return nil, err
	}
	if len(startPosition) > 1 {
		return SignalCondition(e, instance.NewArityError(e), Nil)
	}
	n := 0
	if len(startPosition) == 1 {
		if err := ensure(e, class.Integer, startPosition[0]); err != nil {
			return nil, err
		}
		n = int(startPosition[0].(instance.Integer))
	}
	s := string(str.(instance.String)[n:])
	c := rune(char.(instance.Character))
	i := strings.IndexRune(s, c)
	if i < 0 {
		return Nil, nil
	}
	return instance.NewInteger(i + n), nil
}

// StringIndex returns the position of the given substring within string. The
// search starts from the position indicated by start-position (which is 0-based
// and defaults to 0). The value returned if the search succeeds is an offset
// from the beginning of the string, not from the starting point. If that
// substring does not occur in the string, nil is returned. Presence of the
// substring is done by sequential use of char= on corresponding elements of the
// two strings. An error shall be signaled if either substring or string is not
// a string (error-id. domain-error).
func StringIndex(e env.Environment, sub, str ilos.Instance, startPosition ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.String, sub); err != nil {
		return nil, err
	}
	if err := ensure(e, class.String, str); err != nil {
		return nil, err
	}
	if len(startPosition) > 1 {
		return SignalCondition(e, instance.NewArityError(e), Nil)
	}
	n := 0
	if len(startPosition) == 1 {
		if err := ensure(e, class.Integer, startPosition[0]); err != nil {
			return nil, err
		}
		n = int(startPosition[0].(instance.Integer))
	}
	s := string(str.(instance.String)[n:])
	c := string(sub.(instance.String))
	i := strings.Index(s, c)
	if i < 0 {
		return Nil, nil
	}
	return instance.NewInteger(i + n), nil
}

// StringAppend returns a single string containing a sequence of characters that
// results from appending the sequences of characters of each of the strings, or
// "" if given no strings. An error shall be signaled if any string is not a
// string (error-id. domain-error). This function does not modify its arguments.
// It is implementation defined whether and when the result shares structure
// with its string arguments. An error shall be signaled if the string cannot be
// allocated (error-id. cannot-create-string).
func StringAppend(e env.Environment, str ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret := ""
	for _, s := range str {
		if err := ensure(e, class.String, s); err != nil {
			return nil, err
		}
		ret += string(s.(instance.String))
	}
	return instance.NewString([]rune(ret)), nil
}
