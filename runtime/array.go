// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
)

// BasicArrayP returns t if obj is a basic-array (instance of class
// basic-array); otherwise, returns nil. obj may be any ISLISP object.
func BasicArrayP(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.BasicArrayClass, obj); err != nil {
		return Nil, nil
	}
	return T, nil
}

// BasicArrayStarP returns t if obj is a basic-array* (instance of class
// <basic-array*>); otherwise, returns nil. obj may be any ISLISP object.
func BasicArrayStarP(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.BasicArrayStarClass, obj); err != nil {
		return Nil, nil
	}
	return T, nil
}

// GeneralArrayStarP returns t if obj is a general-array* (instance of class
// <general-array*>); otherwise, returns nil. obj may be any ISLISP object.
func GeneralArrayStarP(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(ilos.GeneralArrayStarClass, obj) {
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
func CreateArray(e ilos.Environment, dimensions ilos.Instance, initialElement ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	length, err := Length(e, dimensions)
	if err != nil {
		return nil, err
	}
	for i := 0; i < int(length.(ilos.Integer)); i++ {
		elt, err := Elt(e, dimensions, ilos.NewInteger(i))
		if err != nil {
			return nil, err
		}
		if err := ensure(e, ilos.IntegerClass, elt); err != nil {
			return nil, err
		}
	}
	// set the initial element
	elt := Nil
	if len(initialElement) > 1 {
		return SignalCondition(e, ilos.NewArityError(e), Nil)
	}
	if len(initialElement) == 1 {
		elt = initialElement[0]
	}
	// general-vector
	if int(length.(ilos.Integer)) == 1 {
		return createGeneralVector(e, dimensions, elt)
	}
	return createGeneralArrayStar(e, dimensions, elt)
}

func createGeneralVector(e ilos.Environment, dimensions ilos.Instance, initialElement ilos.Instance) (ilos.Instance, ilos.Instance) {
	// N-dimensions array
	dimension, err := Car(e, dimensions)
	if err != nil {
		return nil, err
	}
	array := make([]ilos.Instance, int(dimension.(ilos.Integer)))
	for i := 0; i < int(dimension.(ilos.Integer)); i++ {
		array[i] = initialElement
	}
	return ilos.NewGeneralVector(array), nil
}

func createGeneralArrayStar(e ilos.Environment, dimensions ilos.Instance, initialElement ilos.Instance) (ilos.Instance, ilos.Instance) {
	length, err := Length(e, dimensions)
	if err != nil {
		return nil, err
	}
	// 0-dimension array
	if int(length.(ilos.Integer)) == 0 {
		return ilos.NewGeneralArrayStar(nil, initialElement), nil
	}
	// N-dimensions array
	dimension, err := Car(e, dimensions)
	if err != nil {
		return nil, err
	}
	array := make([]*ilos.GeneralArrayStar, int(dimension.(ilos.Integer)))
	for i := range array {
		cdr, err := Cdr(e, dimensions)
		if err != nil {
			return nil, err
		}
		arr, err := createGeneralArrayStar(e, cdr, initialElement)
		if err != nil {
			return nil, err
		}
		array[i] = arr.(*ilos.GeneralArrayStar)
	}
	return ilos.NewGeneralArrayStar(array, nil), nil
}

// Aref returns the object stored in the component of the basic-array specified
// by the sequence of integers z. This sequence must have exactly as many
// elements as there are dimensions in the basic-array, and each one must
// satisfy 0 ≤ zi < di , di the ith dimension and 0 ≤ i < d, d the number of
// dimensions. Arrays are indexed 0 based, so the ith row is accessed via the
// index i − 1. An error shall be signaled if basic-array is not a basic-array
// (error-id. domain-error). An error shall be signaled if any z is not a
// non-negative integer (error-id. domain-error).
func Aref(e ilos.Environment, basicArray ilos.Instance, dimensions ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.BasicArrayClass, basicArray); err != nil {
		return nil, err
	}
	if err := ensure(e, ilos.IntegerClass, dimensions...); err != nil {
		return nil, err
	}
	switch {
	case ilos.InstanceOf(ilos.StringClass, basicArray):
		if len(dimensions) != 1 {
			return SignalCondition(e, ilos.NewArityError(e), Nil)
		}
		index := int(dimensions[0].(ilos.Integer))
		if len(basicArray.(ilos.String)) <= index {
			return SignalCondition(e, ilos.NewIndexOutOfRange(e), Nil)
		}
		return ilos.NewCharacter(basicArray.(ilos.String)[index]), nil
	case ilos.InstanceOf(ilos.GeneralVectorClass, basicArray):
		if len(dimensions) != 1 {
			return SignalCondition(e, ilos.NewArityError(e), Nil)
		}
		index := int(dimensions[0].(ilos.Integer))
		if len(basicArray.(ilos.GeneralVector)) <= index {
			return SignalCondition(e, ilos.NewIndexOutOfRange(e), Nil)
		}
		return basicArray.(ilos.GeneralVector)[index], nil
	default: // General Array*
		return Garef(e, basicArray, dimensions...)
	}
}

// Garef is like aref but an error shall be signaled if its first argument,
// general-array, is not an object of class general-vector or of class
// <general-array*> (error-id. domain-error).
func Garef(e ilos.Environment, generalArray ilos.Instance, dimensions ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.GeneralArrayStarClass, generalArray); err != nil {
		return nil, err
	}
	if err := ensure(e, ilos.IntegerClass, dimensions...); err != nil {
		return nil, err
	}
	if len(dimensions) == 0 {
		if generalArray.(*ilos.GeneralArrayStar).Scalar == nil {
			return SignalCondition(e, ilos.NewIndexOutOfRange(e), Nil)
		}
		return generalArray.(*ilos.GeneralArrayStar).Scalar, nil
	}
	array := generalArray.(*ilos.GeneralArrayStar)
	index := int(dimensions[0].(ilos.Integer))
	if array.Vector == nil || len(array.Vector) <= index {
		return SignalCondition(e, ilos.NewIndexOutOfRange(e), Nil)
	}
	return Garef(e, array.Vector[index], dimensions[1:]...)
}

// SetAref replaces the object obtainable by aref or garef with obj . The
// returned value is obj. The constraints on the basic-array, the general-array,
// and the sequence of indices z is the same as for aref and garef.
func SetAref(e ilos.Environment, obj, basicArray ilos.Instance, dimensions ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.BasicArrayClass, basicArray); err != nil {
		return nil, err
	}
	if err := ensure(e, ilos.IntegerClass, dimensions...); err != nil {
		return nil, err
	}
	switch {
	case ilos.InstanceOf(ilos.StringClass, basicArray):
		if err := ensure(e, ilos.CharacterClass, obj); err != nil {
			return nil, err
		}
		if len(dimensions) != 1 {
			return SignalCondition(e, ilos.NewArityError(e), Nil)
		}
		index := int(dimensions[0].(ilos.Integer))
		if len(basicArray.(ilos.String)) <= index {
			return SignalCondition(e, ilos.NewIndexOutOfRange(e), Nil)
		}
		basicArray.(ilos.String)[index] = rune(obj.(ilos.Character))
		return obj, nil
	case ilos.InstanceOf(ilos.GeneralVectorClass, basicArray):
		if len(dimensions) != 1 {
			return SignalCondition(e, ilos.NewArityError(e), Nil)
		}
		index := int(dimensions[0].(ilos.Integer))
		if len(basicArray.(ilos.GeneralVector)) <= index {
			return SignalCondition(e, ilos.NewIndexOutOfRange(e), Nil)
		}
		basicArray.(ilos.GeneralVector)[index] = obj
		return obj, nil
	default: // General Array*
		return SetGaref(e, obj, basicArray, dimensions...)
	}
}

// SetGaref replaces the object obtainable by aref or garef with obj . The
// returned value is obj. The constraints on the basic-array, the general-array,
// and the sequence of indices z is the same as for aref and garef.
func SetGaref(e ilos.Environment, obj, generalArray ilos.Instance, dimensions ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.GeneralArrayStarClass, generalArray); err != nil {
		return nil, err
	}
	if err := ensure(e, ilos.IntegerClass, dimensions...); err != nil {
		return nil, err
	}
	if len(dimensions) == 0 {
		if generalArray.(*ilos.GeneralArrayStar).Scalar == nil {
			return SignalCondition(e, ilos.NewIndexOutOfRange(e), Nil)
		}
		generalArray.(*ilos.GeneralArrayStar).Scalar = obj
		return obj, nil
	}
	array := generalArray.(*ilos.GeneralArrayStar)
	index := int(dimensions[0].(ilos.Integer))
	if array.Vector == nil || len(array.Vector) <= index {
		return SignalCondition(e, ilos.NewIndexOutOfRange(e), Nil)
	}
	return SetGaref(e, obj, array.Vector[index], dimensions[1:]...)
}

// ArrayDimensions returns a list of the dimensions of a given basic-array. An
// error shall be signaled if basic-array is not a basic-array (error-id.
// domain-error). The consequences are undefined if the returned list is
// modified.
func ArrayDimensions(e ilos.Environment, basicArray ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.BasicArrayClass, basicArray); err != nil {
		return nil, err
	}
	switch {
	case ilos.InstanceOf(ilos.StringClass, basicArray):
		return List(e, ilos.NewInteger(len(basicArray.(ilos.String))))
	case ilos.InstanceOf(ilos.GeneralVectorClass, basicArray):
		return List(e, ilos.NewInteger(len(basicArray.(ilos.GeneralVector))))
	default: // General Array*
		array := basicArray.(*ilos.GeneralArrayStar)
		dimensions := []ilos.Instance{}
		for array.Vector != nil {
			dimensions = append(dimensions, ilos.NewInteger(len(array.Vector)))
			array = array.Vector[0]
		}
		return List(e, dimensions...)
	}
}
