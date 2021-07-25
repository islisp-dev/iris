// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import (
	"math"

	"github.com/islisp-dev/iris/reader/parser"
	"github.com/islisp-dev/iris/reader/tokenizer"
	"github.com/islisp-dev/iris/runtime/core"
)

// Numberp returns t if obj is a number (instance of class number); otherwise,
// returns nil. The obj may be any ISLISP object.
func Numberp(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.NumberClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// ParseNumber scans (as if by read) and if the resulting lexeme is the textual
// representation of a number, the number it represents is returned. An error
// shall be signaled if string is not a string (error-id. domain-error). An
// error shall be signaled if string is not the textual representation of a
// number (error-id. cannot-parse-number).
func ParseNumber(e core.Environment, str core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.StringClass, str); err != nil {
		return nil, err
	}
	ret, err := parser.ParseAtom(e, tokenizer.NewToken(string(str.(core.String)), -1, -1))
	if err != nil || !core.InstanceOf(core.NumberClass, ret) {
		return SignalCondition(e, core.NewParseError(e, str, core.NumberClass), Nil)
	}
	return ret, err
}

// NumberEqual returns t if x1 has the same mathematical value as x2 ;
// otherwise, returns nil. An error shall be signaled if either x1 or x2 is not
// a number (error-id. domain-error). Note: = differs from eql because =
// compares only the mathematical values of its arguments, whereas eql also
// compares the representations
func NumberEqual(e core.Environment, x1, x2 core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.NumberClass, x1, x2); err != nil {
		return nil, err
	}
	ret := false
	switch {
	case core.InstanceOf(core.IntegerClass, x1) && core.InstanceOf(core.IntegerClass, x2):
		ret = core.DeepEqual(x1, x2)
	case core.InstanceOf(core.IntegerClass, x1) && core.InstanceOf(core.FloatClass, x2):
		ret = core.DeepEqual(float64(x1.(core.Integer)), float64(x2.(core.Float)))
	case core.InstanceOf(core.FloatClass, x1) && core.InstanceOf(core.IntegerClass, x2):
		ret = float64(x1.(core.Float)) == float64(x2.(core.Integer))
	case core.InstanceOf(core.FloatClass, x1) && core.InstanceOf(core.FloatClass, x2):
		ret = core.DeepEqual(x1, x2)
	}
	if ret {
		return T, nil
	}
	return Nil, nil
}

// NumberNotEqual returns t if x1 and x2 have mathematically distinct values;
// otherwise, returns nil. An error shall be signaled if either x1 or x2 is not
// a number (error-id. domain-error).
func NumberNotEqual(e core.Environment, x1, x2 core.Instance) (core.Instance, core.Instance) {
	ret, err := NumberEqual(e, x1, x2)
	if err != nil {
		return ret, err
	}
	return Not(e, ret)
}

// NumberGreaterThan returns t if x1 is greater than x2
func NumberGreaterThan(e core.Environment, x1, x2 core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.NumberClass, x1, x2); err != nil {
		return nil, err
	}
	ret := false
	switch {
	case core.InstanceOf(core.IntegerClass, x1) && core.InstanceOf(core.IntegerClass, x2):
		ret = float64(x1.(core.Integer)) > float64(x2.(core.Integer))
	case core.InstanceOf(core.IntegerClass, x1) && core.InstanceOf(core.FloatClass, x2):
		ret = float64(x1.(core.Integer)) > float64(x2.(core.Float))
	case core.InstanceOf(core.FloatClass, x1) && core.InstanceOf(core.IntegerClass, x2):
		ret = float64(x1.(core.Float)) > float64(x2.(core.Integer))
	case core.InstanceOf(core.FloatClass, x1) && core.InstanceOf(core.FloatClass, x2):
		ret = float64(x1.(core.Float)) > float64(x2.(core.Float))
	}
	if ret {
		return T, nil
	}
	return Nil, nil
}

// NumberGreaterThanOrEqual returns t if x1 is greater than or = x2
func NumberGreaterThanOrEqual(e core.Environment, x1, x2 core.Instance) (core.Instance, core.Instance) {
	gt, err := NumberGreaterThan(e, x1, x2)
	if err != nil {
		return nil, err
	}
	eq, err := NumberEqual(e, x1, x2)
	if err != nil {
		return nil, err
	}
	if core.DeepEqual(gt, Nil) && core.DeepEqual(eq, Nil) {
		return Nil, nil
	}
	return T, nil
}

// NumberLessThan returns t if x1 is less than x2
func NumberLessThan(e core.Environment, x1, x2 core.Instance) (core.Instance, core.Instance) {
	ge, err := NumberGreaterThanOrEqual(e, x1, x2)
	if err != nil {
		return nil, err
	}
	return Not(e, ge)
}

// NumberLessThanOrEqual returns t if x1 is less than or = x2
func NumberLessThanOrEqual(e core.Environment, x1, x2 core.Instance) (core.Instance, core.Instance) {
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
func Add(e core.Environment, x ...core.Instance) (core.Instance, core.Instance) {
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
		return core.NewFloat(sum), nil
	}
	return core.NewInteger(int(sum)), nil
}

// Multiply returns the product, respectively, of their arguments. If all
// arguments are integers, the result is an integer. If any argument is a ﬂoat,
// the result is a ﬂoat. When given no arguments, Multiply returns 1. An error
// shall be signaled if any x is not a number (error-id. domain-error).
func Multiply(e core.Environment, x ...core.Instance) (core.Instance, core.Instance) {
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
		return core.NewFloat(pdt), nil
	}
	return core.NewInteger(int(pdt)), nil
}

// Substruct returns its additive inverse. An error shall be signaled if x is
// not a number (error-id. domain-error). If an implementation supports a -0.0
// that is distinct from 0.0, then (- 0.0) returns -0.0; in implementations
// where -0.0 and 0.0 are not distinct, (- 0.0) returns 0.0. Given more than one
// argument, x1 … xn , - returns their successive differences, x1 −x2 − … −xn.
// An error shall be signaled if any x is not a number (error-id. domain-error).
func Substruct(e core.Environment, x core.Instance, xs ...core.Instance) (core.Instance, core.Instance) {
	if len(xs) == 0 {
		ret, err := Substruct(e, core.NewInteger(0), x)
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
		return core.NewFloat(sub), nil
	}
	return core.NewInteger(int(sub)), nil
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
func Quotient(e core.Environment, dividend, divisor1 core.Instance, divisor ...core.Instance) (core.Instance, core.Instance) {
	divisor = append([]core.Instance{divisor1}, divisor...)
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
				arguments = core.NewCons(divisor[i], arguments)
			}
			return SignalCondition(e, core.NewArithmeticError(e, core.NewSymbol("QUOTIENT"), arguments), Nil)
		}
		if !flt && !b && int(quotient)%int(f) != 0 {
			flt = true
		}
		quotient /= f
	}
	if flt {
		return core.NewFloat(quotient), nil
	}
	return core.NewInteger(int(quotient)), nil
}

// Reciprocal returns the reciprocal of its argument x ; that is, 1/x . An error
// shall be signaled if x is zero (error-id. division-by-zero).
func Reciprocal(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	return Quotient(e, core.NewInteger(1), x)
}

// Max returns the greatest (closest to positive infinity) of its arguments. The
// comparison is done by >. An error shall be signaled if any x is not a number
// (error-id. domain-error).
func Max(e core.Environment, x core.Instance, xs ...core.Instance) (core.Instance, core.Instance) {
	max := x
	for _, y := range xs {
		ret, err := NumberGreaterThan(e, y, max)
		if err != nil {
			return nil, err
		}
		if core.DeepEqual(ret, T) {
			max = y
		}
	}
	return max, nil
}

// Min returns the least (closest to negative infinity) of its arguments. The
// comparison is done by <. An error shall be signaled if any x is not a number
// (error-id. domain-error).
func Min(e core.Environment, x core.Instance, xs ...core.Instance) (core.Instance, core.Instance) {
	min := x
	for _, y := range xs {
		ret, err := NumberLessThan(e, y, min)
		if err != nil {
			return nil, err
		}
		if core.DeepEqual(ret, T) {
			min = y
		}
	}
	return min, nil
}

// Abs returns the absolute value of its argument. An error shall be signaled if
// x is not a number (error-id. domain-error).
func Abs(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	ret, err := NumberLessThan(e, x, core.NewInteger(0))
	if err != nil {
		return nil, err
	}
	if core.DeepEqual(ret, T) {
		return Substruct(e, x)
	}
	return x, nil
}

// Exp returns e raised to the power x , where e is the base of the natural
// logarithm. An error shall be signaled if x is not a number (error-id.
// domain-error).
func Exp(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	f, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewFloat(math.Exp(f)), nil
}

// Log returns the natural logarithm of x. An error shall be signaled if x is
// not a positive number (error-id. domain-error).
func Log(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	f, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	if f <= 0.0 {
		return SignalCondition(e, core.NewDomainError(e, x, core.NumberClass), Nil)
	}
	return core.NewFloat(math.Log(f)), nil
}

// Expt returns x1 raised to the power x2. The result will be an integer if x1
// is an integer and x2 is a non-negative integer. An error shall be signaled if
// x1 is zero and x2 is negative, or if x1 is zero and x2 is a zero ﬂoat, or if
// x1 is negative and x2 is not an integer.
func Expt(e core.Environment, x1, x2 core.Instance) (core.Instance, core.Instance) {
	a, af, err := convFloat64(e, x1)
	if err != nil {
		return nil, err
	}
	b, bf, err := convFloat64(e, x2)
	if err != nil {
		return nil, err
	}
	if !af && !bf && b >= 0 {
		return core.NewInteger(int(math.Pow(a, b))), nil
	}
	if (a == 0 && b < 0) || (a == 0 && bf && b == 0) || (a < 0 && bf) {
		operation := core.NewSymbol("EXPT")
		operands, err := List(e, x1, x2)
		if err != nil {
			return nil, err
		}
		return SignalCondition(e, core.NewArithmeticError(e, operation, operands), Nil)
	}
	return core.NewFloat(math.Pow(a, b)), nil
}

// Sqrt returns the non-negative square root of x. An error shall be signaled if
// x is not a non-negative number (error-id. domain-error).
func Sqrt(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	if a < 0.0 {
		return SignalCondition(e, core.NewDomainError(e, x, core.NumberClass), Nil)
	}
	if math.Ceil(math.Sqrt(a)) == math.Sqrt(a) {
		return core.NewInteger(int(math.Sqrt(a))), nil
	}
	return core.NewFloat(math.Sqrt(a)), nil
}

// Pi is an approximation of π.
var Pi = core.NewFloat(3.141592653589793)

// Sin returns the sine of x . x must be given in radians. An error shall be
// signaled if x is not a number (error-id. domain-error).
func Sin(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewFloat(math.Sin(a)), nil
}

// Cos returns the cosine of x . x must be given in radians. An error shall be
// signaled if x is not a number (error-id. domain-error).
func Cos(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewFloat(math.Cos(a)), nil
}

// Tan returns the tangent of x . x must be given in radians. An error shall be
// signaled if x is not a number (error-id. domain-error).
func Tan(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewFloat(math.Tan(a)), nil
}

// Atan returns the arc tangent of x. The result is a (real) number that lies
// between −π/2 and π/2 (both exclusive). An error shall be signaled if x is not
// a number (error-id. domain-error).
func Atan(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewFloat(math.Atan(a)), nil
}

// Atan2 returns the phase of its representation in polar coordinates. If x1 is
// zero and x2 is negative, the result is positive. If x1 and x2 are both zero,
// the result is implementation defined. An error shall be signaled if x is not
// a number (error-id. domain-error). The value of atan2 is always between −π
// (exclusive) and π (inclusive) when minus zero is not supported; when minus
// zero is supported, the range includes −π. The signs of x1 (indicated as y)
// and x2 (indicated as x) are used to derive quadrant information.
func Atan2(e core.Environment, x1, x2 core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x1)
	if err != nil {
		return nil, err
	}
	b, _, err := convFloat64(e, x2)
	if err != nil {
		return nil, err
	}
	if a == 0 && b == 0 {
		operation := core.NewSymbol("ATAN2")
		operands, err := List(e, x1, x2)
		if err != nil {
			return nil, err
		}
		return SignalCondition(e, core.NewArithmeticError(e, operation, operands), Nil)
	}
	return core.NewFloat(math.Atan2(a, b)), nil
}

// Sinh returns the hyperbolic sine of x . x must be given in radians. An error
// shall be signaled if x is not a number (error-id. domain-error).
func Sinh(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewFloat(math.Sinh(a)), nil
}

// Cosh returns the hyperbolic cosine of x . x must be given in radians. An
// error shall be signaled if x is not a number (error-id. domain-error).
func Cosh(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewFloat(math.Cosh(a)), nil
}

// Tanh returns the hyperbolic tangent of x . x must be given in radians. An
// error shall be signaled if x is not a number (error-id. domain-error).
func Tanh(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewFloat(math.Tanh(a)), nil
}

// Atanh returns the hyperbolic arc tangent of x. An error shall be signaled if
// x is not a number with absolute value less than 1 (error-id. domain-error).
func Atanh(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	a, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	if math.Abs(a) >= 1 {
		return SignalCondition(e, core.NewDomainError(e, x, core.NumberClass), Nil)
	}
	return core.NewFloat(math.Atanh(a)), nil
}
