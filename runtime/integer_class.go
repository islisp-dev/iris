package runtime

import (
	"math"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func convInt(z ilos.Instance) (int, ilos.Instance) {
	if instance.Of(class.Integer, z) {
		return 0, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         z,
			"EXPECTED-CLASS": class.Integer,
		})
	}
	return int(z.(instance.Integer)), nil
}

// Integerp returns t if obj is an integer (instance of class integer);
// otherwise, returns nil. obj may be any ISLISP object.
func Integerp(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Integer, obj) {
		return T, nil
	}
	return Nil, nil
}

// Div returns the greatest integer less than or equal to the quotient of z1 and z2.
// An error shall be signaled if z2 is zero (error-id. division-by-zero).
func Div(_, _ *environment.Environment, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, err := convInt(z1)
	if err != nil {
		return nil, err
	}
	b, err := convInt(z2)
	if err != nil {
		return nil, err
	}
	if b == 0 {
		return nil, instance.New(class.DivisionByZero, map[string]ilos.Instance{
			"OPERATION": instance.New(class.Symbol, "DIV"),
			"OPERANDS":  instance.New(class.Cons, z1, instance.New(class.Cons, z2, Nil)),
		})
	}
	return instance.New(class.Integer, a/b), nil
}

// Mod returns the remainder of the integer division of z1 by z2.
// The sign of the result is the sign of z2. The result lies
// between 0 (inclusive) and z2 (exclusive), and the difference of z1
// and this result is divisible by z2 without remainder.
//
// An error shall be signaled if either z1 or z2 is not an integer (error-id. domain-error).
func Mod(_, _ *environment.Environment, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, err := convInt(z1)
	if err != nil {
		return nil, err
	}
	b, err := convInt(z2)
	if err != nil {
		return nil, err
	}
	if b == 0 {
		return nil, instance.New(class.DivisionByZero, map[string]ilos.Instance{
			"OPERATION": instance.New(class.Symbol, "MOD"),
			"OPERANDS":  instance.New(class.Cons, z1, instance.New(class.Cons, z2, Nil)),
		})
	}
	return instance.New(class.Integer, a%b), nil
}

// Gcd returns the greatest common divisor of its integer arguments.
// The result is a non-negative integer. For nonzero arguments
// the greatest common divisor is the largest integer z such that
// z1 and z2 are integral multiples of z.
//
// An error shall be signaled if either z1 or z2 is not an integer
// (error-id. domain-error).
func Gcd(_, _ *environment.Environment, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gcd := func(x, y int) int {
		for y != 0 {
			x, y = y, x%y
		}
		return x
	}
	a, err := convInt(z1)
	if err != nil {
		return nil, err
	}
	b, err := convInt(z2)
	if err != nil {
		return nil, err
	}
	return instance.New(class.Integer, gcd(a, b)), nil
}

// Lcm returns the least common multiple of its integer arguments.
//
// An error shall be signaled if either z1 or z2 is not an integer
// (error-id. domain-error).
func Lcm(_, _ *environment.Environment, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	gcd := func(x, y int) int {
		for y != 0 {
			x, y = y, x%y
		}
		return x
	}
	a, err := convInt(z1)
	if err != nil {
		return nil, err
	}
	b, err := convInt(z2)
	if err != nil {
		return nil, err
	}
	return instance.New(class.Integer, a*b/gcd(a, b)), nil
}

// Isqrt Returns the greatest integer less than or equal to
// the exact positive square root of z . An error shall be signaled
// if z is not a non-negative integer (error-id. domain-error).
func Isqrt(_, _ *environment.Environment, z ilos.Instance) (ilos.Instance, ilos.Instance) {
	a, err := convInt(z)
	if err != nil {
		return nil, err
	}
	if a < 0 {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         z,
			"EXPECTED-CLASS": class.Number,
		})
	}
	return instance.New(class.Integer, int(math.Sqrt(float64(a)))), nil
}
