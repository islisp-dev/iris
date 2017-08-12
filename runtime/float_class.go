package runtime

import (
	"math"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// The value of MostPositiveFloat is the implementation-dependent
// ﬂoating-point number closest to positive infinity.
//
// The value of MostNegativeFloat is the implementation-dependent
// ﬂoating-point number closest to negative infinity.
var (
	MostPositiveFloat = instance.New(class.Float, math.MaxFloat64)
	MostNegativeFloat = instance.New(class.Float, -math.MaxFloat64)
)

// Floatp returns t if obj is a ﬂoat (instance of class float);
// otherwise, returns nil. The obj may be any ISLISP object.
func Floatp(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Float, obj) {
		return T, nil
	}
	return Nil, nil
}

// Float returns x itself if it is an instance of the class float
// and returns a ﬂoating-point approximation of x otherwise.
// An error shall be signaled if x is not a number (error-id. domain-error).
func Float(_, _ *environment.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	f, _, err := convFloat64(x)
	if err != nil {
		return nil, err
	}
	return instance.New(class.Float, f), nil
}

// Floor returns the greatest integer less than or equal to x .
// That is, x is truncated towards negative infinity. An error
// shall be signaled if x is not a number (error-id. domain-error).
func Floor(_, _ *environment.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	f, _, err := convFloat64(x)
	if err != nil {
		return nil, err
	}
	return instance.New(class.Integer, int(math.Floor(f))), nil
}

// Ceiling Returns the smallest integer that is not smaller than x.
// That is, x is truncated towards positive infinity. An error
// shall be signaled if x is not a number (error-id. domain-error).
func Ceiling(_, _ *environment.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	f, _, err := convFloat64(x)
	if err != nil {
		return nil, err
	}
	return instance.New(class.Integer, int(math.Ceil(f))), nil
}

// Truncate returns the integer between 0 and x (inclusive) that is nearest to x.
// That is, x is truncated towards zero. An error shall be signaled
// if x is not a number (error-id. domain-error).
func Truncate(_, _ *environment.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	f, _, err := convFloat64(x)
	if err != nil {
		return nil, err
	}
	return instance.New(class.Integer, int(math.Trunc(f))), nil
}

// Round returns the integer nearest to x.
// If x is exactly halfway between two integers, the even one is chosen.
// An error shall be signaled if x is not a number (error-id. domain-error).
func Round(_, _ *environment.Environment, x ilos.Instance) (ilos.Instance, ilos.Instance) {
	f, _, err := convFloat64(x)
	if err != nil {
		return nil, err
	}
	return instance.New(class.Integer, int(math.Floor(f+.5))), nil
}
