// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"strings"

	"github.com/islisp-dev/iris/runtime/ilos"
)

// Stringp returns t if obj is a string (instance of class string); otherwise,
// returns nil. obj may be any ISLISP object.
func Stringp(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(ilos.StringClass, obj) {
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
func CreateString(e ilos.Environment, i ilos.Instance, initialElement ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if !ilos.InstanceOf(ilos.IntegerClass, i) || int(i.(ilos.Integer)) < 0 {
		return SignalCondition(e, ilos.NewDomainError(e, i, ilos.ObjectClass), Nil)
	}
	if len(initialElement) > 1 {
		return SignalCondition(e, ilos.NewArityError(e), Nil)
	}
	n := int(i.(ilos.Integer))
	v := make([]rune, n)
	for i := 0; i < n; i++ {
		if len(initialElement) == 0 {
			v[i] = 0
		} else {
			if err := ensure(e, ilos.CharacterClass, initialElement[0]); err != nil {
				return nil, err
			}
			v[i] = rune(initialElement[0].(ilos.Character))
		}
	}
	return ilos.NewString(v), nil
}

// StringEqual tests whether string1 is the same string as string2.
func StringEqual(e ilos.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.StringClass, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(ilos.String)) == string(string2.(ilos.String)) {
		return T, nil
	}
	return Nil, nil
}

// StringNotEqual tests whether string1 not is the same string as string2.
func StringNotEqual(e ilos.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := StringEqual(e, string1, string2)
	if err != nil {
		return nil, err
	}
	return Not(e, ret)
}

// StringGreaterThan tests whether string1 is greater than string2.
func StringGreaterThan(e ilos.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.StringClass, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(ilos.String)) > string(string2.(ilos.String)) {
		return T, nil
	}
	return Nil, nil
}

// StringGreaterThanOrEqual tests whether string1 is greater than or equal to
// string2.
func StringGreaterThanOrEqual(e ilos.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.StringClass, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(ilos.String)) >= string(string2.(ilos.String)) {
		return T, nil
	}
	return Nil, nil
}

// StringLessThan tests whether string1 is less than string2.
func StringLessThan(e ilos.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.StringClass, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(ilos.String)) < string(string2.(ilos.String)) {
		return T, nil
	}
	return Nil, nil
}

// StringLessThanOrEqual tests whether string1 is less than or equal to string2.
func StringLessThanOrEqual(e ilos.Environment, string1, string2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.StringClass, string1, string2); err != nil {
		return nil, err
	}
	if string(string1.(ilos.String)) <= string(string2.(ilos.String)) {
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
func CharIndex(e ilos.Environment, char, str ilos.Instance, startPosition ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.CharacterClass, char); err != nil {
		return nil, err
	}
	if err := ensure(e, ilos.StringClass, str); err != nil {
		return nil, err
	}
	if len(startPosition) > 1 {
		return SignalCondition(e, ilos.NewArityError(e), Nil)
	}
	n := 0
	if len(startPosition) == 1 {
		if err := ensure(e, ilos.IntegerClass, startPosition[0]); err != nil {
			return nil, err
		}
		n = int(startPosition[0].(ilos.Integer))
	}
	s := string(str.(ilos.String)[n:])
	c := rune(char.(ilos.Character))
	i := strings.IndexRune(s, c)
	if i < 0 {
		return Nil, nil
	}
	return ilos.NewInteger(i + n), nil
}

// StringIndex returns the position of the given substring within string. The
// search starts from the position indicated by start-position (which is 0-based
// and defaults to 0). The value returned if the search succeeds is an offset
// from the beginning of the string, not from the starting point. If that
// substring does not occur in the string, nil is returned. Presence of the
// substring is done by sequential use of char= on corresponding elements of the
// two strings. An error shall be signaled if either substring or string is not
// a string (error-id. domain-error).
func StringIndex(e ilos.Environment, sub, str ilos.Instance, startPosition ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.StringClass, sub); err != nil {
		return nil, err
	}
	if err := ensure(e, ilos.StringClass, str); err != nil {
		return nil, err
	}
	if len(startPosition) > 1 {
		return SignalCondition(e, ilos.NewArityError(e), Nil)
	}
	n := 0
	if len(startPosition) == 1 {
		if err := ensure(e, ilos.IntegerClass, startPosition[0]); err != nil {
			return nil, err
		}
		n = int(startPosition[0].(ilos.Integer))
	}
	s := string(str.(ilos.String)[n:])
	c := string(sub.(ilos.String))
	i := strings.Index(s, c)
	if i < 0 {
		return Nil, nil
	}
	return ilos.NewInteger(i + n), nil
}

// StringAppend returns a single string containing a sequence of characters that
// results from appending the sequences of characters of each of the strings, or
// "" if given no strings. An error shall be signaled if any string is not a
// string (error-id. domain-error). This function does not modify its arguments.
// It is implementation defined whether and when the result shares structure
// with its string arguments. An error shall be signaled if the string cannot be
// allocated (error-id. cannot-create-string).
func StringAppend(e ilos.Environment, str ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret := ""
	for _, s := range str {
		if err := ensure(e, ilos.StringClass, s); err != nil {
			return nil, err
		}
		ret += string(s.(ilos.String))
	}
	return ilos.NewString([]rune(ret)), nil
}
