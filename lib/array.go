// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import (
	"github.com/islisp-dev/iris/core"
)

// BasicArrayP returns t if obj is a basic-array (instance of class
// basic-array); otherwise, returns nil. obj may be any ISLISP object.
func BasicArrayP(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.BasicArrayClass, obj); err != nil {
		return Nil, nil
	}
	return T, nil
}

// BasicArrayStarP returns t if obj is a basic-array* (instance of class
// <basic-array*>); otherwise, returns nil. obj may be any ISLISP object.
func BasicArrayStarP(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.BasicArrayStarClass, obj); err != nil {
		return Nil, nil
	}
	return T, nil
}

// GeneralArrayStarP returns t if obj is a general-array* (instance of class
// <general-array*>); otherwise, returns nil. obj may be any ISLISP object.
func GeneralArrayStarP(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.GeneralArrayStarClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// CreateArray creates an array of the given dimensions. The dimensions argument
// is a list of non-negative integers. The result is of class general-vector if
// there is only one dimension, or of class <general-array*> otherwise. If
// initial-element is given, the elements of the new array are initialized with
// this object, otherwise the initialization is implementation defined. An error
// shall be signaled if the requested array cannot be allocated (error-id.
// cannot-create-array). An error shall be signaled if dimensions is not a
// proper list of non-negative integers (error-id. domain-error).
// initial-element may be any ISLISP object
func CreateArray(e core.Environment, dimensions core.Instance, initialElement ...core.Instance) (core.Instance, core.Instance) {
	length, err := Length(e, dimensions)
	if err != nil {
		return nil, err
	}
	for i := 0; i < int(length.(core.Integer)); i++ {
		elt, err := Elt(e, dimensions, core.NewInteger(i))
		if err != nil {
			return nil, err
		}
		if err := ensure(e, core.IntegerClass, elt); err != nil {
			return nil, err
		}
	}
	// set the initial element
	elt := Nil
	if len(initialElement) > 1 {
		return SignalCondition(e, core.NewArityError(e), Nil)
	}
	if len(initialElement) == 1 {
		elt = initialElement[0]
	}
	// general-vector
	if int(length.(core.Integer)) == 1 {
		return createGeneralVector(e, dimensions, elt)
	}
	return createGeneralArrayStar(e, dimensions, elt)
}

func createGeneralVector(e core.Environment, dimensions core.Instance, initialElement core.Instance) (core.Instance, core.Instance) {
	// N-dimensions array
	dimension, err := Car(e, dimensions)
	if err != nil {
		return nil, err
	}
	array := make([]core.Instance, int(dimension.(core.Integer)))
	for i := 0; i < int(dimension.(core.Integer)); i++ {
		array[i] = initialElement
	}
	return core.NewGeneralVector(array), nil
}

func createGeneralArrayStar(e core.Environment, dimensions core.Instance, initialElement core.Instance) (core.Instance, core.Instance) {
	length, err := Length(e, dimensions)
	if err != nil {
		return nil, err
	}
	// 0-dimension array
	if int(length.(core.Integer)) == 0 {
		return core.NewGeneralArrayStar(nil, initialElement), nil
	}
	// N-dimensions array
	dimension, err := Car(e, dimensions)
	if err != nil {
		return nil, err
	}
	array := make([]*core.GeneralArrayStar, int(dimension.(core.Integer)))
	for i := range array {
		cdr, err := Cdr(e, dimensions)
		if err != nil {
			return nil, err
		}
		arr, err := createGeneralArrayStar(e, cdr, initialElement)
		if err != nil {
			return nil, err
		}
		array[i] = arr.(*core.GeneralArrayStar)
	}
	return core.NewGeneralArrayStar(array, nil), nil
}

// Aref returns the object stored in the component of the basic-array specified
// by the sequence of integers z. This sequence must have exactly as many
// elements as there are dimensions in the basic-array, and each one must
// satisfy 0 ≤ zi < di , di the ith dimension and 0 ≤ i < d, d the number of
// dimensions. Arrays are indexed 0 based, so the ith row is accessed via the
// index i − 1. An error shall be signaled if basic-array is not a basic-array
// (error-id. domain-error). An error shall be signaled if any z is not a
// non-negative integer (error-id. domain-error).
func Aref(e core.Environment, basicArray core.Instance, dimensions ...core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.BasicArrayClass, basicArray); err != nil {
		return nil, err
	}
	if err := ensure(e, core.IntegerClass, dimensions...); err != nil {
		return nil, err
	}
	switch {
	case core.InstanceOf(core.StringClass, basicArray):
		if len(dimensions) != 1 {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		index := int(dimensions[0].(core.Integer))
		if len(basicArray.(core.String)) <= index {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		return core.NewCharacter(basicArray.(core.String)[index]), nil
	case core.InstanceOf(core.GeneralVectorClass, basicArray):
		if len(dimensions) != 1 {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		index := int(dimensions[0].(core.Integer))
		if len(basicArray.(core.GeneralVector)) <= index {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		return basicArray.(core.GeneralVector)[index], nil
	default: // General Array*
		return Garef(e, basicArray, dimensions...)
	}
}

// Garef is like aref but an error shall be signaled if its first argument,
// general-array, is not an object of class general-vector or of class
// <general-array*> (error-id. domain-error).
func Garef(e core.Environment, generalArray core.Instance, dimensions ...core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.GeneralArrayStarClass, generalArray); err != nil {
		return nil, err
	}
	if err := ensure(e, core.IntegerClass, dimensions...); err != nil {
		return nil, err
	}
	if len(dimensions) == 0 {
		if core.DeepEqual(generalArray.(*core.GeneralArrayStar).Scalar, nil) {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		return generalArray.(*core.GeneralArrayStar).Scalar, nil
	}
	array := generalArray.(*core.GeneralArrayStar)
	index := int(dimensions[0].(core.Integer))
	if core.DeepEqual(array.Vector, nil) || len(array.Vector) <= index {
		return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
	}
	return Garef(e, array.Vector[index], dimensions[1:]...)
}

// SetAref replaces the object obtainable by aref or garef with obj . The
// returned value is obj. The constraints on the basic-array, the general-array,
// and the sequence of indices z is the same as for aref and garef.
func SetAref(e core.Environment, obj, basicArray core.Instance, dimensions ...core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.BasicArrayClass, basicArray); err != nil {
		return nil, err
	}
	if err := ensure(e, core.IntegerClass, dimensions...); err != nil {
		return nil, err
	}
	switch {
	case core.InstanceOf(core.StringClass, basicArray):
		if err := ensure(e, core.CharacterClass, obj); err != nil {
			return nil, err
		}
		if len(dimensions) != 1 {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		index := int(dimensions[0].(core.Integer))
		if len(basicArray.(core.String)) <= index {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		basicArray.(core.String)[index] = rune(obj.(core.Character))
		return obj, nil
	case core.InstanceOf(core.GeneralVectorClass, basicArray):
		if len(dimensions) != 1 {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		index := int(dimensions[0].(core.Integer))
		if len(basicArray.(core.GeneralVector)) <= index {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		basicArray.(core.GeneralVector)[index] = obj
		return obj, nil
	default: // General Array*
		return SetGaref(e, obj, basicArray, dimensions...)
	}
}

// SetGaref replaces the object obtainable by aref or garef with obj . The
// returned value is obj. The constraints on the basic-array, the general-array,
// and the sequence of indices z is the same as for aref and garef.
func SetGaref(e core.Environment, obj, generalArray core.Instance, dimensions ...core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.GeneralArrayStarClass, generalArray); err != nil {
		return nil, err
	}
	if err := ensure(e, core.IntegerClass, dimensions...); err != nil {
		return nil, err
	}
	if len(dimensions) == 0 {
		if core.DeepEqual(generalArray.(*core.GeneralArrayStar).Scalar, nil) {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		generalArray.(*core.GeneralArrayStar).Scalar = obj
		return obj, nil
	}
	array := generalArray.(*core.GeneralArrayStar)
	index := int(dimensions[0].(core.Integer))
	if core.DeepEqual(array.Vector, nil) || len(array.Vector) <= index {
		return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
	}
	return SetGaref(e, obj, array.Vector[index], dimensions[1:]...)
}

// ArrayDimensions returns a list of the dimensions of a given basic-array. An
// error shall be signaled if basic-array is not a basic-array (error-id.
// domain-error). The consequences are undefined if the returned list is
// modified.
func ArrayDimensions(e core.Environment, basicArray core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.BasicArrayClass, basicArray); err != nil {
		return nil, err
	}
	switch {
	case core.InstanceOf(core.StringClass, basicArray):
		return List(e, core.NewInteger(len(basicArray.(core.String))))
	case core.InstanceOf(core.GeneralVectorClass, basicArray):
		return List(e, core.NewInteger(len(basicArray.(core.GeneralVector))))
	default: // General Array*
		array := basicArray.(*core.GeneralArrayStar)
		dimensions := []core.Instance{}
		for array.Vector != nil {
			dimensions = append(dimensions, core.NewInteger(len(array.Vector)))
			array = array.Vector[0]
		}
		return List(e, dimensions...)
	}
}
