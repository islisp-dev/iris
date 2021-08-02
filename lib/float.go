// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import (
	"math"

	"github.com/islisp-dev/iris/core"
)

// The value of MostPositiveFloat is the implementation-dependent ﬂoating-point
// number closest to positive infinity. The value of MostNegativeFloat is the
// implementation-dependent ﬂoating-point number closest to negative infinity.
var (
	MostPositiveFloat = core.NewFloat(math.MaxFloat64)
	MostNegativeFloat = core.NewFloat(-math.MaxFloat64)
)

// Floatp returns t if obj is a ﬂoat (instance of class float); otherwise,
// returns nil. The obj may be any ISLISP object.
func Floatp(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.FloatClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// Float returns x itself if it is an instance of the class float and returns a
// ﬂoating-point approximation of x otherwise. An error shall be signaled if x
// is not a number (error-id. domain-error).
func Float(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	f, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewFloat(f), nil
}

// Floor returns the greatest integer less than or equal to x . That is, x is
// truncated towards negative infinity. An error shall be signaled if x is not a
// number (error-id. domain-error).
func Floor(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	f, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewInteger(int(math.Floor(f))), nil
}

// Ceiling Returns the smallest integer that is not smaller than x. That is, x
// is truncated towards positive infinity. An error shall be signaled if x is
// not a number (error-id. domain-error).
func Ceiling(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	f, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewInteger(int(math.Ceil(f))), nil
}

// Truncate returns the integer between 0 and x (inclusive) that is nearest to
// x. That is, x is truncated towards zero. An error shall be signaled if x is
// not a number (error-id. domain-error).
func Truncate(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	f, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewInteger(int(math.Trunc(f))), nil
}

// Round returns the integer nearest to x. If x is exactly halfway between two
// integers, the even one is chosen. An error shall be signaled if x is not a
// number (error-id. domain-error).
func Round(e core.Environment, x core.Instance) (core.Instance, core.Instance) {
	f, _, err := convFloat64(e, x)
	if err != nil {
		return nil, err
	}
	return core.NewInteger(int(math.Floor(f + .5))), nil
}
