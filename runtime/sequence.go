// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/env"
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

// Length returns the length of sequence as an integer greater than or equal to
// 0. When sequence is a basic-vector, length returns its dimension. When
// sequence is a list, the result is the number of elements in the list; if an
// element is itself a list, the elements within this sublist are not counted.
// In the case of dotted lists, length returns the number of conses at the
// uppermost level of the list. For example, (length ' (a b . c)) ⇒ 2, since '(a
// b . c) ≡ (cons 'a (cons 'b 'c)). An error shall be signaled if sequence is
// not a basic-vector or a list (error-id. domain-error).
func Length(e env.Environment, sequence ilos.Instance) (ilos.Instance, ilos.Instance) {
	switch {
	case ilos.InstanceOf(instance.StringClass, sequence):
		return instance.NewInteger(len(sequence.(instance.String))), nil
	case ilos.InstanceOf(instance.GeneralVectorClass, sequence):
		return instance.NewInteger(len(sequence.(instance.GeneralVector))), nil
	case ilos.InstanceOf(instance.ListClass, sequence):
		return instance.NewInteger(sequence.(instance.List).Length()), nil
	}
	// TODO: instance.SeqClass
	return SignalCondition(e, instance.NewDomainError(e, sequence, instance.ObjectClass), Nil)
}

// Elt returns the element of sequence that has index z. Indexing is 0-based;
// i.e., z = 0 designates the first element, Given a sequence and an integer z
// satisfying 0 ≤ z < (length sequence). An error shall be signaled if z is an
// integer outside of the mentioned range (error-id. index-out-of-range). An
// error shall be signaled if sequence is not a basic-vector or a list or if z
// is not an integer (error-id. domain-error).
func Elt(e env.Environment, sequence, z ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, instance.IntegerClass, z); err != nil {
		return nil, err
	}
	switch {
	case ilos.InstanceOf(instance.StringClass, sequence):
		seq := sequence.(instance.String)
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, instance.NewIndexOutOfRange(e), Nil)
		}
		return instance.NewCharacter(seq[idx]), nil
	case ilos.InstanceOf(instance.GeneralVectorClass, sequence):
		seq := sequence.(instance.GeneralVector)
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, instance.NewIndexOutOfRange(e), Nil)
		}
		return seq[idx], nil
	case ilos.InstanceOf(instance.ListClass, sequence):
		seq := sequence.(instance.List).Slice()
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, instance.NewIndexOutOfRange(e), Nil)
		}
		return seq[idx], nil
	}
	return SignalCondition(e, instance.NewDomainError(e, sequence, instance.ObjectClass), Nil)

}

// SetElt is that these replace the object obtainable by elt with obj. The
// returned value is obj. An error shall be signaled if z is an integer outside
// of the valid range of indices (error-id. index-out-of-range). An error shall
// be signaled if sequence is not a basic-vector or a list or if z is not an
// integer (error-id. domain-error). obj may be any ISLISP object.
func SetElt(e env.Environment, obj, sequence, z ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, instance.IntegerClass, z); err != nil {
		return nil, err
	}
	switch {
	case ilos.InstanceOf(instance.StringClass, sequence):
		seq := sequence.(instance.String)
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, instance.NewIndexOutOfRange(e), Nil)
		}
		if err := ensure(e, instance.CharacterClass, obj); err != nil {
			return nil, err
		}
		seq[idx] = rune(obj.(instance.Character))
		return obj, nil
	case ilos.InstanceOf(instance.GeneralVectorClass, sequence):
		seq := sequence.(instance.GeneralVector)
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, instance.NewIndexOutOfRange(e), Nil)
		}
		seq[idx] = obj
		return obj, nil
	case ilos.InstanceOf(instance.ListClass, sequence):
		seq := sequence.(instance.List).Slice()
		idx := int(z.(instance.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, instance.NewIndexOutOfRange(e), Nil)
		}
		for idx != 0 && ilos.InstanceOf(instance.ConsClass, sequence) {
			idx--
			sequence = sequence.(*instance.Cons).Cdr
		}
		sequence.(*instance.Cons).Car = obj
		return obj, nil
	}
	return SignalCondition(e, instance.NewDomainError(e, sequence, instance.ObjectClass), Nil)
}

// Subseq returns the subsequence of length z2 − z1, containing the elements
// with indices from z1 (inclusive) to z2 (exclusive). The subsequence is newly
// allocated, and has the same class as sequence, Given a sequence sequence and
// two integers z1 and z2 satisfying 0 ≤ z1 ≤ z2 ≤ (length sequence) An error
// shall be signaled if the requested subsequence cannot be allocated (error-id.
// cannot-create-sequence). An error shall be signaled if z1 or z2 are outside
// of the bounds mentioned (error-id. index-out-of-range). An error shall be
// signaled if sequence is not a basic-vector or a list, or if z1 is not an
// integer, or if z2 is not an integer (error-id. domain-error).
func Subseq(e env.Environment, sequence, z1, z2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, instance.IntegerClass, z1, z2); err != nil {
		return nil, err
	}
	start := int(z1.(instance.Integer))
	end := int(z2.(instance.Integer))
	switch {
	case ilos.InstanceOf(instance.StringClass, sequence):
		seq := sequence.(instance.String)
		if !(0 <= start && start < len(seq) && 0 <= end && end < len(seq) && start <= end) {
			return SignalCondition(e, instance.NewIndexOutOfRange(e), Nil)
		}
		return seq[start:end], nil
	case ilos.InstanceOf(instance.GeneralVectorClass, sequence):
		seq := sequence.(instance.GeneralVector)
		if !(0 <= start && start < len(seq) && 0 <= end && end < len(seq) && start <= end) {
			return SignalCondition(e, instance.NewIndexOutOfRange(e), Nil)
		}
		return seq[start:end], nil
	case ilos.InstanceOf(instance.ListClass, sequence):
		seq := sequence.(instance.List).Slice()
		if !(0 < start && start < len(seq) && 0 < end && end < len(seq) && start <= end) {
			return SignalCondition(e, instance.NewIndexOutOfRange(e), Nil)
		}
		return List(e, seq[start:end]...)
	}
	return SignalCondition(e, instance.NewDomainError(e, sequence, instance.ObjectClass), Nil)
}

// Destructively modifies destination to contain the results of applying
// function to successive elements in the sequences. The destination is
// returned. If destination and each element of sequences are not all the same
// length, the iteration terminates when the shortest sequence (of any of the
// sequences or the destination) is exhausted. The calls to function proceed
// from left to right, so that if function has side-effects, it can rely upon
// being called first on all of the elements with index 0, then on all of those
// numbered 1, and so on. An error shall be signaled if destination is not a
// basic-vector or a list (error-id. domain-error). An error shall be signaled
// if any sequence is not a basic-vector or a list (error-id. domain-error).
func MapInto(e env.Environment, destination, function ilos.Instance, sequences ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, instance.ListClass, append(sequences, destination)...); err != nil {
		if err := ensure(e, instance.BasicVectorClass, append(sequences, destination)...); err != nil {
			return nil, err
		}
	}
	if err := ensure(e, instance.FunctionClass, function); err != nil {
		return nil, err
	}
	min, err := Length(e, destination)
	if err != nil {
		return nil, err
	}
	for _, seq := range sequences {
		len, err := Length(e, seq)
		if err != nil {
			return nil, err
		}
		min, err = Min(e, min, len)
		if err != nil {
			return nil, err
		}
	}
	for i := 0; i < int(min.(instance.Integer)); i++ {
		arguments := make([]ilos.Instance, len(sequences))
		for j, seq := range sequences {
			var err ilos.Instance
			arguments[j], err = Elt(e, seq, instance.NewInteger(i))
			if err != nil {
				return nil, err
			}
		}
		ret, err := function.(instance.Applicable).Apply(e.NewDynamic(), arguments...)
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
