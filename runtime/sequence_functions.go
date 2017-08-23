// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"math"

	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// Length returns the length of sequence as an integer greater than or equal to 0.
//
// When sequence is a basic-vector, length returns its dimension.
//
// When sequence is a list, the result is the number of elements in the list; if an element is
// itself a list, the elements within this sublist are not counted. In the case of dotted lists,
// length returns the number of conses at the uppermost level of the list. For example, (length '
// (a b . c)) ⇒ 2, since '(a b . c) ≡ (cons 'a (cons 'b 'c)).
//
// An error shall be signaled if sequence is not a basic-vector or a list
// (error-id. domain-error).
func Length(e env.Environment, sequence ilos.Instance) (ilos.Instance, ilos.Instance) {
	switch {
	case ilos.InstanceOf(class.String, sequence):
		return instance.NewInteger(len(sequence.(instance.String))), nil
	case ilos.InstanceOf(class.GeneralVector, sequence):
		return instance.NewInteger(len(sequence.(instance.GeneralVector))), nil
	case ilos.InstanceOf(class.List, sequence):
		return instance.NewInteger(sequence.(instance.List).Length()), nil
	}
	// TODO: class.Seq
	return nil, instance.NewDomainError(sequence, class.Object)
}

// Elt returns the element of sequence that has index z. Indexing is 0-based; i.e., z = 0
// designates the first element, Given a sequence and an integer z satisfying 0 ≤ z < (length
// sequence). An error shall be signaled if z is an integer outside of the mentioned range
// (error-id. index-out-of-range).
//
// An error shall be signaled if sequence is not a basic-vector or a list or if z is not an
// integer (error-id. domain-error).
func Elt(e env.Environment, sequence, z ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Integer, z); err != nil {
		return nil, err
	}
	switch {
	case ilos.InstanceOf(class.String, sequence):
		seq := sequence.(instance.String)
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return nil, instance.NewIndexOutOfRange()
		}
		return instance.NewCharacter(seq[idx]), nil
	case ilos.InstanceOf(class.GeneralVector, sequence):
		seq := sequence.(instance.GeneralVector)
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return nil, instance.NewIndexOutOfRange()
		}
		return seq[idx], nil
	case ilos.InstanceOf(class.List, sequence):
		seq := sequence.(instance.List).Slice()
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return nil, instance.NewIndexOutOfRange()
		}
		return seq[idx], nil
	}
	return nil, instance.NewDomainError(sequence, class.Object)

}

// SetElt is that these replace the object obtainable by elt with obj. The returned value is obj.
//
// An error shall be signaled if z is an integer outside of the valid range of indices
// (error-id. index-out-of-range). An error shall be signaled if sequence is not a basic-vector
// or a list or if z is not an integer (error-id. domain-error). obj may be any ISLISP object.
func SetElt(e env.Environment, obj, sequence, z ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Integer, z); err != nil {
		return nil, err
	}
	switch {
	case ilos.InstanceOf(class.String, sequence):
		seq := sequence.(instance.String)
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return nil, instance.NewIndexOutOfRange()
		}
		if err := ensure(class.Character, obj); err != nil {
			return nil, err
		}
		seq[idx] = rune(obj.(instance.Character))
		return obj, nil
	case ilos.InstanceOf(class.GeneralVector, sequence):
		seq := sequence.(instance.GeneralVector)
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return nil, instance.NewIndexOutOfRange()
		}
		seq[idx] = obj
		return obj, nil
	case ilos.InstanceOf(class.List, sequence):
		seq := sequence.(instance.List).Slice()
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return nil, instance.NewIndexOutOfRange()
		}
		for idx != 0 && ilos.InstanceOf(class.Cons, sequence) {
			idx--
			sequence = sequence.(*instance.Cons).Cdr
		}
		sequence.(*instance.Cons).Car = obj
		return obj, nil
	}
	return nil, instance.NewDomainError(sequence, class.Object)
}

// Subseq returns the subsequence of length z2 − z1, containing the elements with indices from
// z1 (inclusive) to z2 (exclusive). The subsequence is newly allocated, and has the same class
// as sequence, Given a sequence sequence and two integers z1 and z2 satisfying 0 ≤ z1 ≤ z2 ≤
// (length sequence)
//
// An error shall be signaled if the requested subsequence cannot be allocated (error-id.
// cannot-create-sequence). An error shall be signaled if z1 or z2 are outside of the bounds
// mentioned (error-id. index-out-of-range). An error shall be signaled if sequence is not a
// basic-vector or a list, or if z1 is not an integer, or if z2 is not an integer
// (error-id. domain-error).
func Subseq(e env.Environment, sequence, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Integer, z1, z2); err != nil {
		return nil, err
	}
	start := int(z1.(instance.Integer))
	end := int(z2.(instance.Integer))
	switch {
	case ilos.InstanceOf(class.String, sequence):
		seq := sequence.(instance.String)
		if !(0 <= start && start < len(seq) && 0 <= end && end < len(seq) && start <= end) {
			return nil, instance.NewIndexOutOfRange()
		}
		return seq[start:end], nil
	case ilos.InstanceOf(class.GeneralVector, sequence):
		seq := sequence.(instance.GeneralVector)
		if !(0 <= start && start < len(seq) && 0 <= end && end < len(seq) && start <= end) {
			return nil, instance.NewIndexOutOfRange()
		}
		return seq[start:end], nil
	case ilos.InstanceOf(class.List, sequence):
		seq := sequence.(instance.List).Slice()
		if !(0 < start && start < len(seq) && 0 < end && end < len(seq) && start <= end) {
			return nil, instance.NewIndexOutOfRange()
		}
		return List(e, seq[start:end]...)
	}
	return nil, instance.NewDomainError(sequence, class.Object)
}

// Destructively modifies destination to contain the results of applying function to
// successive elements in the sequences. The destination is returned.
//
// If destination and each element of sequences are not all the same length, the
// iteration terminates when the shortest sequence (of any of the sequences or the
// destination) is exhausted.
//
// The calls to function proceed from left to right, so that if function has
// side-effects, it can rely upon being called first on all of the elements with index
// 0, then on all of those numbered 1, and so on.
//
// An error shall be signaled if destination is not a basic-vector or a list
// (error-id. domain-error).
//
// An error shall be signaled if any sequence is not a basic-vector or a list
// (error-id. domain-error).
func mapInto(e env.Environment, destination, function ilos.Instance, sequences ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.List, append(sequences, destination)...); err != nil {
		if err := ensure(class.BasicVector, append(sequences, destination)...); err != nil {
			return nil, err
		}
	}
	if err := ensure(class.Function, function); err != nil {
		return nil, err
	}
	max := 0.0
	for _, seq := range sequences {
		switch {
		case ilos.InstanceOf(class.String, seq):
			max = math.Max(max, float64(len(seq.(instance.String))))
		case ilos.InstanceOf(class.GeneralVector, seq):
			max = math.Max(max, float64(len(seq.(instance.GeneralVector))))
		case ilos.InstanceOf(class.List, seq):
			max = math.Max(max, float64(len(seq.(instance.List).Slice())))
		}
	}
	for i := 0; i < int(max); i++ {
		arguments := make([]ilos.Instance, int(max))
		for _, seq := range sequences {
			var err ilos.Instance
			arguments[i], err = Elt(e, seq, instance.NewInteger(i))
			if err != nil {
				return nil, err
			}
		}
		ret, err := function.(instance.Applicable).Apply(e, arguments...)
		if err != nil {
			return nil, err
		}
		_, err = SetElt(e, ret, destination, instance.NewInteger(i))
		if err != nil {
			return nil, err
		}
	}
	return destination, nil
}
