// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"math"

	"github.com/islisp-dev/iris/reader/parser"
	"github.com/islisp-dev/iris/reader/tokenizer"
	"github.com/islisp-dev/iris/runtime/env"
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/class"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

// Numberp returns t if obj is a number (instance of class number); otherwise,
// returns nil. The obj may be any ISLISP object.
func Numberp(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(class.Number, obj) {
		return T, nil
	}
	return Nil, nil
}

// ParseNumber scans (as if by read) and if the resulting lexeme is the textual
// representation of a number, the number it represents is returned. An error
// shall be signaled if string is not a string (error-id. domain-error). An
// error shall be signaled if string is not the textual representation of a
// number (error-id. cannot-parse-number).
func ParseNumber(e env.Environment, str ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.String, str); err != nil {
		return nil, err
	}
	ret, err := parser.ParseAtom(tokenizer.NewToken(string(str.(instance.String)), -1, -1))
	if err != nil || !ilos.InstanceOf(class.Number, ret) {
		return SignalCondition(e, instance.NewParseError(e, str, class.Number), Nil)
	}
	return ret, err
}

// NumberEqual returns t if x1 has the same mathematical value as x2 ;
// otherwise, returns nil. An error shall be signaled if either x1 or x2 is not
// a number (error-id. domain-error). Note: = differs from eql because =
// compares only the mathematical values of its arguments, whereas eql also
// compares the representations
func NumberEqual(e env.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.Number, x1, x2); err != nil {
		return nil, err
	}
	ret := false
	switch {
	case ilos.InstanceOf(class.Integer, x1) && ilos.InstanceOf(class.Integer, x2):
		ret = x1 == x2
	case ilos.InstanceOf(class.Integer, x1) && ilos.InstanceOf(class.Float, x2):
		ret = float64(x1.(instance.Integer)) == float64(x2.(instance.Float))
	case ilos.InstanceOf(class.Float, x1) && ilos.InstanceOf(class.Integer, x2):
		ret = float64(x1.(instance.Float)) == float64(x2.(instance.Integer))
	case ilos.InstanceOf(class.Float, x1) && ilos.InstanceOf(class.Float, x2):
		ret = x1 == x2
	}
	if ret {
		return T, nil
	}
	return Nil, nil
}

// NumberNotEqual returns t if x1 and x2 have mathematically distinct values;
// otherwise, returns nil. An error shall be signaled if either x1 or x2 is not
// a number (error-id. domain-error).
func NumberNotEqual(e env.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := NumberEqual(e, x1, x2)
	if err != nil {
		return ret, err
	}
	return Not(e, ret)
}

// NumberGreaterThan returns t if x1 is greater than x2
func NumberGreaterThan(e env.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.Number, x1, x2); err != nil {
		return nil, err
	}
	ret := false
	switch {
	case ilos.InstanceOf(class.Integer, x1) && ilos.InstanceOf(class.Integer, x2):
		ret = float64(x1.(instance.Integer)) > float64(x2.(instance.Integer))
	case ilos.InstanceOf(class.Integer, x1) && ilos.InstanceOf(class.Float, x2):
		ret = float64(x1.(instance.Integer)) > float64(x2.(instance.Float))
	case ilos.InstanceOf(class.Float, x1) && ilos.InstanceOf(class.Integer, x2):
		ret = float64(x1.(instance.Float)) > float64(x2.(instance.Integer))
	case ilos.InstanceOf(class.Float, x1) && ilos.InstanceOf(class.Float, x2):
		ret = float64(x1.(instance.Float)) > float64(x2.(instance.Float))
	}
	if ret {
		return T, nil
	}
	return Nil, nil
}

// NumberGreaterThanOrEqual returns t if x1 is greater than or = x2
func NumberGreaterThanOrEqual(e env.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gt, err := NumberGreaterThan(e, x1, x2)
	if err != nil {
		return nil, err
	}
	eq, err := NumberEqual(e, x1, x2)
	if err != nil {
		return nil, err
	}
	if gt == Nil && eq == Nil {
		return Nil, nil
	}
	return T, nil
}

// NumberLessThan returns t if x1 is less than x2
func NumberLessThan(e env.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	ge, err := NumberGreaterThanOrEqual(e, x1, x2)
	if err != nil {
		return nil, err
	}
	return Not(e, ge)
}

// NumberLessThanOrEqual returns t if x1 is less than or = x2
func NumberLessThanOrEqual(e env.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gt, err := NumberGreaterThan(e, x1, x2)
	if err != nil {
		return nil, err
	}
	return Not(e, gt)
}

// Add returns the sum, respectively, of their arguments. If all arguments are
// integers, the result is an integer. If any argument is a ﬂoat, the result is
// a ﬂoat. When given no arguments, + returns 0. An error shall be signaled if
// any x is not a number (error-id. domain-error).
func Add(e env.Environment, x ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	flt := false
	sum := 0.0
	for _, a := range x {
		f, b, err := convFloat64(e, a)
		if err != nil {
			return nil, err
		}
		flt = flt || b
		sum += f
	}
	if flt {
		return instance.NewFloat(sum), nil
	}
	return instance.NewInteger(int(sum)), nil
}

// Multiply returns the product, respectively, of their arguments. If all
// arguments are integers, the result is an integer. If any argument is a ﬂoat,
// the result is a ﬂoat. When given no arguments, Multiply returns 1. An error
// shall be signaled if any x is not a number (error-id. domain-error).
func Multiply(e env.Environment, x ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	flt := false
	pdt := 1.0
	for _, a := range x {
		f, b, err := convFloat64(e, a)
		if err != nil {
			return nil, err
		}
		pdt *= f
		flt = flt || b
	}
	if flt {
		return instance.NewFloat(pdt), nil
	}
	return instance.NewInteger(int(pdt)), nil
}

// Substruct returns its additive inverse. An error shall be signaled if x is
// not a number (error-id. domain-error). If an implementation supports a -0.0
// that is distinct from 0.0, then (- 0.0) returns -0.0; in implementations
// where -0.0 and 0.0 are not distinct, (- 0.0) returns 0.0. Given more than one
// argument, x1 … xn , - returns their successive differences, x1 −x2 − … −xn.
// An error shall be signaled if any x is not a number (error-id. domain-error).
func Substruct(e env.Environment, x ilos.Instance, xs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if len(xs) == 0 {
		ret, err := Substruct(e, instance.NewInteger(0), x)
		return ret, err
	}
	sub, flt, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	for _, a := range xs {
		f, b, err := convFloat64(e, a)
		if err != nil {
			return nil, err
		}
		sub -= f
		flt = flt || b
	}
	if flt {
		return instance.NewFloat(sub), nil
	}
	return instance.NewInteger(int(sub)), nil
}

// Quotient returns the quotient of those numbers. The result is an integer if
// dividend and divisor are integers and divisor evenly divides dividend ,
// otherwise it will be a ﬂoat. Given more than two arguments, quotient operates
// iteratively on each of the divisor1 … divisorn as in dividend /divisor1 / …
// /divisorn. The type of the result follows from the two-argument case because
// the three-or-more-argument quotient can be defined as follows: An error shall
// be signaled if dividend is not a number (error-id. domain-error). An error
// shall be signaled if any divisor is not a number (error-id. domain-error). An
// error shall be signaled if any divisor is zero (error-id. division-by-zero).
func Quotient(e env.Environment, dividend, divisor1 ilos.Instance, divisor ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	divisor = append([]ilos.Instance{divisor1}, divisor...)
	quotient, flt, err := convFloat64(e, dividend)
	if err != nil {
		return nil, err
	}
	for _, a := range divisor {
		f, b, err := convFloat64(e, a)
		if err != nil {
			return nil, err
		}
		if f == 0.0 {
			arguments := Nil
			for i := len(divisor) - 1; i >= 0; i-- {
				arguments = instance.NewCons(divisor[i], arguments)
			}
			return SignalCondition(e, instance.NewArithmeticError(e, instance.NewSymbol("QUOTIENT"), arguments), Nil)
		}
		if !flt && !b && int(quotient)%int(f) != 0 {
			flt = true
		}
		quotient /= f
	}
	if flt {
		return instance.NewFloat(quotient), nil
	}
	return instance.NewInteger(int(quotient)), nil
}

// Reciprocal returns the reciprocal of its argument x ; that is, 1/x . An error
// shall be signaled if x is zero (error-id. division-by-zero).
func Reciprocal(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	return Quotient(e, instance.NewInteger(1), x)
}

// Max returns the greatest (closest to positive infinity) of its arguments. The
// comparison is done by >. An error shall be signaled if any x is not a number
// (error-id. domain-error).
func Max(e env.Environment, x ilos.Instance, xs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	max := x
	for _, y := range xs {
		ret, err := NumberGreaterThan(e, y, max)
		if err != nil {
			return nil, err
		}
		if ret == T {
			max = y
		}
	}
	return max, nil
}

// Min returns the least (closest to negative infinity) of its arguments. The
// comparison is done by <. An error shall be signaled if any x is not a number
// (error-id. domain-error).
func Min(e env.Environment, x ilos.Instance, xs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	min := x
	for _, y := range xs {
		ret, err := NumberLessThan(e, y, min)
		if err != nil {
			return nil, err
		}
		if ret == T {
			min = y
		}
	}
	return min, nil
}

// Abs returns the absolute value of its argument. An error shall be signaled if
// x is not a number (error-id. domain-error).
func Abs(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := NumberLessThan(e, x, instance.NewInteger(0))
	if err != nil {
		return nil, err
	}
	if ret == T {
		return Substruct(e, x)
	}
	return x, nil
}

// Exp returns e raised to the power x , where e is the base of the natural
// logarithm. An error shall be signaled if x is not a number (error-id.
// domain-error).
func Exp(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	f, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return instance.NewFloat(math.Exp(f)), nil
}

// Log returns the natural logarithm of x. An error shall be signaled if x is
// not a positive number (error-id. domain-error).
func Log(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	f, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	if f <= 0.0 {
		return SignalCondition(e, instance.NewDomainError(e, x, class.Number), Nil)
	}
	return instance.NewFloat(math.Log(f)), nil
}

// Expt returns x1 raised to the power x2. The result will be an integer if x1
// is an integer and x2 is a non-negative integer. An error shall be signaled if
// x1 is zero and x2 is negative, or if x1 is zero and x2 is a zero ﬂoat, or if
// x1 is negative and x2 is not an integer.
func Expt(e env.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, af, err := convFloat64(e, x1)
	if err != nil {
		return nil, err
	}
	b, bf, err := convFloat64(e, x2)
	if err != nil {
		return nil, err
	}
	if !af && !bf && b >= 0 {
		return instance.NewInteger(int(math.Pow(a, b))), nil
	}
	if (a == 0 && b < 0) || (a == 0 && bf && b == 0) || (a < 0 && bf) {
		operation := instance.NewSymbol("EXPT")
		operands, err := List(e, x1, x2)
		if err != nil {
			return nil, err
		}
		return SignalCondition(e, instance.NewArithmeticError(e, operation, operands), Nil)
	}
	return instance.NewFloat(math.Pow(a, b)), nil
}

// Sqrt returns the non-negative square root of x. An error shall be signaled if
// x is not a non-negative number (error-id. domain-error).
func Sqrt(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	if a < 0.0 {
		return SignalCondition(e, instance.NewDomainError(e, x, class.Number), Nil)
	}
	if math.Ceil(math.Sqrt(a)) == math.Sqrt(a) {
		return instance.NewInteger(int(math.Sqrt(a))), nil
	}
	return instance.NewFloat(math.Sqrt(a)), nil
}

// Pi is an approximation of π.
var Pi = instance.NewFloat(3.141592653589793)

// Sin returns the sine of x . x must be given in radians. An error shall be
// signaled if x is not a number (error-id. domain-error).
func Sin(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return instance.NewFloat(math.Sin(a)), nil
}

// Cos returns the cosine of x . x must be given in radians. An error shall be
// signaled if x is not a number (error-id. domain-error).
func Cos(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return instance.NewFloat(math.Cos(a)), nil
}

// Tan returns the tangent of x . x must be given in radians. An error shall be
// signaled if x is not a number (error-id. domain-error).
func Tan(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return instance.NewFloat(math.Tan(a)), nil
}

// Atan returns the arc tangent of x. The result is a (real) number that lies
// between −π/2 and π/2 (both exclusive). An error shall be signaled if x is not
// a number (error-id. domain-error).
func Atan(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return instance.NewFloat(math.Atan(a)), nil
}

// Atan2 returns the phase of its representation in polar coordinates. If x1 is
// zero and x2 is negative, the result is positive. If x1 and x2 are both zero,
// the result is implementation defined. An error shall be signaled if x is not
// a number (error-id. domain-error). The value of atan2 is always between −π
// (exclusive) and π (inclusive) when minus zero is not supported; when minus
// zero is supported, the range includes −π. The signs of x1 (indicated as y)
// and x2 (indicated as x) are used to derive quadrant information.
func Atan2(e env.Environment, x1, x2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x1)
	if err != nil {
		return nil, err
	}
	b, _, err := convFloat64(e, x2)
	if err != nil {
		return nil, err
	}
	if a == 0 && b == 0 {
		operation := instance.NewSymbol("ATAN2")
		operands, err := List(e, x1, x2)
		if err != nil {
			return nil, err
		}
		return SignalCondition(e, instance.NewArithmeticError(e, operation, operands), Nil)
	}
	return instance.NewFloat(math.Atan2(a, b)), nil
}

// Sinh returns the hyperbolic sine of x . x must be given in radians. An error
// shall be signaled if x is not a number (error-id. domain-error).
func Sinh(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return instance.NewFloat(math.Sinh(a)), nil
}

// Cosh returns the hyperbolic cosine of x . x must be given in radians. An
// error shall be signaled if x is not a number (error-id. domain-error).
func Cosh(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return instance.NewFloat(math.Cosh(a)), nil
}

// Tanh returns the hyperbolic tangent of x . x must be given in radians. An
// error shall be signaled if x is not a number (error-id. domain-error).
func Tanh(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return instance.NewFloat(math.Tanh(a)), nil
}

// Atanh returns the hyperbolic arc tangent of x. An error shall be signaled if
// x is not a number with absolute value less than 1 (error-id. domain-error).
func Atanh(e env.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	if math.Abs(a) >= 1 {
		return SignalCondition(e, instance.NewDomainError(e, x, class.Number), Nil)
	}
	return instance.NewFloat(math.Atanh(a)), nil
}
