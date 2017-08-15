// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// BasicArrayP returns t if obj is a basic-array (instance of class basic-array);
// otherwise, returns nil. obj may be any ISLISP object.
func BasicArrayP(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.BasicArray, obj) {
		return T, nil
	}
	return Nil, nil
}

// BasicArrayStarP returns t if obj is a basic-array* (instance of class <basic-array*>);
// otherwise, returns nil. obj may be any ISLISP object.
func BasicArrayStarP(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.BasicArrayStar, obj) {
		return T, nil
	}
	return Nil, nil
}

// GeneralArrayStarP returns t if obj is a general-array* (instance of class <general-array*>);
// otherwise, returns nil. obj may be any ISLISP object.
func GeneralArrayStarP(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.GeneralArrayStar, obj) {
		return T, nil
	}
	return Nil, nil
}

// CreateArray creates an array of the given dimensions. The dimensions argument is a list of
// non-negative integers.
//
// The result is of class general-vector if there is only one dimension, or of class
// <general-array*> otherwise.
//
// If initial-element is given, the elements of the new array are initialized with this object,
// otherwise the initialization is implementation defined.
//
// An error shall be signaled if the requested array cannot be allocated
// (error-id. cannot-create-array).
//
// An error shall be signaled if dimensions is not a proper list of non-negative integers
// (error-id. domain-error). initial-element may be any ISLISP object
func CreateArray(_, _ *environment.Environment, dimensions ilos.Instance, initialElement ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.List, dimensions); err != nil {
		return nil, err
	}
	if err := ensure(class.Integer, dimensions.(instance.List).Slice()...); err != nil {
		return nil, err
	}
	dim := dimensions.(instance.List).Slice()
	elt := Nil
	if len(initialElement) > 1 {
		return nil, instance.NewArityError()
	}
	if len(initialElement) == 1 {
		elt = initialElement[0]
	}
	switch len(dim) {
	case 0:
		return instance.NewGeneralArrayStar(nil, elt), nil
	case 1:
		array := make([]*instance.GeneralArrayStar, int(dim[0].(instance.Integer)))
		for i := range array {
			array[i] = instance.NewGeneralArrayStar(nil, elt).(*instance.GeneralArrayStar)
		}
		return instance.NewGeneralArrayStar(array, nil), nil
	default:
		array := make([]*instance.GeneralArrayStar, int(dim[len(dim)-1].(instance.Integer)))
		for i := range array {
			array[i] = instance.NewGeneralArrayStar(nil, elt).(*instance.GeneralArrayStar)
		}
		for i := len(dim) - 2; i >= 0; i-- {
			elt := array
			array := make([]*instance.GeneralArrayStar, int(dim[i].(instance.Integer)))
			for i := range array {
				array[i] = instance.NewGeneralArrayStar(elt, nil).(*instance.GeneralArrayStar)
			}
		}
		return instance.NewGeneralArrayStar(array, nil), nil
	}
}

// Aref returns the object stored in the component of the basic-array specified by the sequence
// of integers z. This sequence must have exactly as many elements as there are dimensions in
// the basic-array, and each one must satisfy 0 ≤ zi < di , di the ith dimension and 0 ≤ i < d,
// d the number of dimensions. Arrays are indexed 0 based, so the ith row is accessed via the
// index i − 1.
//
// An error shall be signaled if basic-array is not a basic-array (error-id. domain-error).
// An error shall be signaled if any z is not a non-negative integer (error-id. domain-error).
func Aref(_, _ *environment.Environment, basicArray ilos.Instance, dimensions ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.BasicArray, basicArray); err != nil {
		return nil, err
	}
	if err := ensure(class.Integer, dimensions...); err != nil {
		return nil, err
	}
	switch {
	case instance.Of(class.String, basicArray):
		if len(dimensions) != 1 {
			return nil, instance.NewArityError()
		}
		index := int(dimensions[0].(instance.Integer))
		if len(basicArray.(instance.String)) <= index {
			return nil, instance.NewIndexOutOfRange()
		}
		return instance.NewCharacter(basicArray.(instance.String)[index]), nil
	case instance.Of(class.GeneralVector, basicArray):
		if len(dimensions) != 1 {
			return nil, instance.NewArityError()
		}
		index := int(dimensions[0].(instance.Integer))
		if len(basicArray.(instance.GeneralVector)) <= index {
			return nil, instance.NewIndexOutOfRange()
		}
		return basicArray.(instance.GeneralVector)[index], nil
	default: // General Array*
		return Garef(nil, nil, basicArray, dimensions...)
	}
}

// Garef is like aref but an error shall be signaled if its first argument, general-array, is
// not an object of class general-vector or of class <general-array*> (error-id. domain-error).
func Garef(_, _ *environment.Environment, generalArray ilos.Instance, dimensions ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.GeneralArrayStar, generalArray); err != nil {
		return nil, err
	}
	if err := ensure(class.Integer, dimensions...); err != nil {
		return nil, err
	}
	var array *instance.GeneralArrayStar
	for _, dim := range dimensions {
		index := int(dim.(instance.Integer))
		if array.Vector == nil || len(array.Vector) <= index {
			return nil, instance.NewIndexOutOfRange()
		}
		array = array.Vector[index]
	}
	if array.Scalar == nil {
		return nil, instance.NewIndexOutOfRange()
	}
	return array.Scalar, nil
}

// SetAref replaces the object obtainable by aref or garef with obj . The returned value is obj.
// The constraints on the basic-array, the general-array, and the sequence of indices z is the
// same as for aref and garef.
func SetAref(_, _ *environment.Environment, obj, basicArray ilos.Instance, dimensions ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.BasicArray, basicArray); err != nil {
		return nil, err
	}
	if err := ensure(class.Integer, dimensions...); err != nil {
		return nil, err
	}
	switch {
	case instance.Of(class.String, basicArray):
		if err := ensure(class.Character, obj); err != nil {
			return nil, err
		}
		if len(dimensions) != 1 {
			return nil, instance.NewArityError()
		}
		index := int(dimensions[0].(instance.Integer))
		if len(basicArray.(instance.String)) <= index {
			return nil, instance.NewIndexOutOfRange()
		}
		basicArray.(instance.String)[index] = rune(obj.(instance.Character))
		return obj, nil
	case instance.Of(class.GeneralVector, basicArray):
		if len(dimensions) != 1 {
			return nil, instance.NewArityError()
		}
		index := int(dimensions[0].(instance.Integer))
		if len(basicArray.(instance.GeneralVector)) <= index {
			return nil, instance.NewIndexOutOfRange()
		}
		basicArray.(instance.GeneralVector)[index] = obj
		return obj, nil
	default: // General Array*
		return SetGaref(nil, nil, obj, basicArray, dimensions...)
	}
}

// SetGaref replaces the object obtainable by aref or garef with obj . The returned value is obj.
// The constraints on the basic-array, the general-array, and the sequence of indices z is the
// same as for aref and garef.
func SetGaref(_, _ *environment.Environment, obj, generalArray ilos.Instance, dimensions ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.GeneralArrayStar, generalArray); err != nil {
		return nil, err
	}
	if err := ensure(class.Integer, dimensions...); err != nil {
		return nil, err
	}
	var array *instance.GeneralArrayStar
	for _, dim := range dimensions {
		index := int(dim.(instance.Integer))
		if array.Vector == nil || len(array.Vector) <= index {
			return nil, instance.NewIndexOutOfRange()
		}
		array = array.Vector[index]
	}
	if array.Scalar == nil {
		return nil, instance.NewIndexOutOfRange()
	}
	array.Scalar = obj
	return obj, nil
}

// ArrayDimensions returns a list of the dimensions of a given basic-array.
// An error shall be signaled if basic-array is not a basic-array (error-id. domain-error).
// The consequences are undefined if the returned list is modified.
func ArrayDimensions(_, _ *environment.Environment, basicArray ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.BasicArray, basicArray); err != nil {
		return nil, err
	}
	switch {
	case instance.Of(class.String, basicArray):
		return List(nil, nil, instance.NewInteger(len(basicArray.(instance.String))))
	case instance.Of(class.GeneralVector, basicArray):
		return List(nil, nil, instance.NewInteger(len(basicArray.(instance.GeneralVector))))
	default: // General Array*
		var array *instance.GeneralArrayStar
		dimensions := []ilos.Instance{}
		for array.Vector != nil {
			dimensions = append(dimensions, instance.NewInteger(len(array.Vector)))
			array = array.Vector[0]
		}
		return List(nil, nil, dimensions...)
	}
}
