// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/reader/parser"
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// Numberp returns t if obj is a number (instance of class number); otherwise,
// returns nil. The obj may be any ISLISP object.
func Numberp(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Number, obj) {
		return T, nil
	}
	return Nil, nil
}

// ParseNumber scans (as if by read) and if the resulting lexeme
// is the textual representation of a number, the number it represents is returned.
//
// An error shall be signaled if string is not a string (error-id. domain-error).
// An error shall be signaled if string is not the textual representation
// of a number (error-id. cannot-parse-number).
func ParseNumber(_, _ *environment.Environment, str ilos.Instance) (ilos.Instance, ilos.Instance) {
	if !instance.Of(class.String, str) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         str,
			"EXPECTED-CLASS": class.String,
		})
	}
	ret, err := parser.ParseAtom(string(str.(instance.String)))
	if err != nil || !instance.Of(class.Number, ret) {
		return nil, instance.New(class.ParseError, map[string]ilos.Instance{
			"STRING":         str,
			"EXPECTED-CLASS": class.Number,
		})
	}
	return ret, err
}

// EqualNumber returns t if x1 has the same mathematical value as x2 ;
// otherwise, returns nil. An error shall be signaled if either x1 or x2 is not a number
// (error-id. domain-error).
//
// Note: = differs from eql because = compares only the mathematical values of its arguments,
// whereas eql also compares the representations
func EqualNumber(_, _ *environment.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Number, x1) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         x1,
			"EXPECTED-CLASS": class.String,
		})
	}
	if instance.Of(class.Number, x2) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         x2,
			"EXPECTED-CLASS": class.String,
		})
	}
	ret := false
	switch {
	case instance.Of(class.Integer, x1) && instance.Of(class.Integer, x2):
		ret = x1 == x2
	case instance.Of(class.Integer, x1) && instance.Of(class.Float, x2):
		ret = float64(x1.(instance.Integer)) == float64(x2.(instance.Float))
	case instance.Of(class.Float, x1) && instance.Of(class.Integer, x2):
		ret = float64(x1.(instance.Float)) == float64(x2.(instance.Integer))
	case instance.Of(class.Float, x1) && instance.Of(class.Float, x2):
		ret = x1 == x2
	}
	if ret {
		return T, nil
	}
	return Nil, nil
}

// NotEqualNumber returns t if x1 and x2 have mathematically distinct values;
// otherwise, returns nil. An error shall be signaled if either x1 or x2 is not
// a number (error-id. domain-error).
func NotEqualNumber(_, _ *environment.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := EqualNumber(nil, nil, x1, x2)
	if err != nil {
		return ret, err
	}
	ret, err = Not(nil, nil, ret)
	return ret, err
}
