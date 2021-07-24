// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"math"

	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func convInt(e ilos.Environment, z ilos.Instance) (int, ilos.Instance) {
	if err := ensure(e, instance.IntegerClass, z); err != nil {
		return 0, err
	}
	return int(z.(instance.Integer)), nil
}

// Integerp returns t if obj is an integer (instance of class integer);
// otherwise, returns nil. obj may be any ISLISP object.
func Integerp(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(instance.IntegerClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// Div returns the greatest integer less than or equal to the quotient of z1 and
// z2. An error shall be signaled if z2 is zero (error-id. division-by-zero).
func Div(e ilos.Environment, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, err := convInt(e, z1)
	if err != nil {
		return nil, err
	}
	b, err := convInt(e, z2)
	if err != nil {
		return nil, err
	}
	if b == 0 {
		operation := instance.NewSymbol("DIV")
		operands, err := List(e, z1, z2)
		if err != nil {
			return nil, err
		}
		return SignalCondition(e, instance.NewArithmeticError(e, operation, operands), Nil)
	}
	if a*b < 0 { // Issue #2
		return instance.NewInteger(a/b - 1), nil
	}
	return instance.NewInteger(a / b), nil
}

// Mod returns the remainder of the integer division of z1 by z2. The sign of
// the result is the sign of z2. The result lies between 0 (inclusive) and z2
// (exclusive), and the difference of z1 and this result is divisible by z2
// without remainder. An error shall be signaled if either z1 or z2 is not an
// integer (error-id. domain-error).
func Mod(e ilos.Environment, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	f, err := Div(e, z1, z2)
	if err != nil {
		return nil, err
	}
	g, err := Multiply(e, f, z2)
	if err != nil {
		return nil, err
	}
	return Substruct(e, z1, g) // Issue #2
}

// Gcd returns the greatest common divisor of its integer arguments. The result
// is a non-negative integer. For nonzero arguments the greatest common divisor
// is the largest integer z such that z1 and z2 are integral multiples of z. An
// error shall be signaled if either z1 or z2 is not an integer (error-id.
// domain-error).
func Gcd(e ilos.Environment, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gcd := func(x, y int) int {
		for y != 0 {
			x, y = y, x%y
		}
		return x
	}
	a, err := convInt(e, z1)
	if err != nil {
		return nil, err
	}
	b, err := convInt(e, z2)
	if err != nil {
		return nil, err
	}
	return instance.NewInteger(gcd(a, b)), nil
}

// Lcm returns the least common multiple of its integer arguments. An error
// shall be signaled if either z1 or z2 is not an integer (error-id.
// domain-error).
func Lcm(e ilos.Environment, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gcd := func(x, y int) int {
		for y != 0 {
			x, y = y, x%y
		}
		return x
	}
	a, err := convInt(e, z1)
	if err != nil {
		return nil, err
	}
	b, err := convInt(e, z2)
	if err != nil {
		return nil, err
	}
	return instance.NewInteger(a * b / gcd(a, b)), nil
}

// Isqrt Returns the greatest integer less than or equal to the exact positive
// square root of z . An error shall be signaled if z is not a non-negative
// integer (error-id. domain-error).
func Isqrt(e ilos.Environment, z ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, err := convInt(e, z)
	if err != nil {
		return nil, err
	}
	if a < 0 {
		return SignalCondition(e, instance.NewDomainError(e, z, instance.NumberClass), Nil)
	}
	return instance.NewInteger(int(math.Sqrt(float64(a)))), nil
}
