// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/runtime/core"

// Length returns the length of sequence as an integer greater than or equal to
// 0. When sequence is a basic-vector, length returns its dimension. When
// sequence is a list, the result is the number of elements in the list; if an
// element is itself a list, the elements within this sublist are not counted.
// In the case of dotted lists, length returns the number of conses at the
// uppermost level of the list. For example, (length ' (a b . c)) ⇒ 2, since '(a
// b . c) ≡ (cons 'a (cons 'b 'c)). An error shall be signaled if sequence is
// not a basic-vector or a list (error-id. domain-error).
func Length(e core.Environment, sequence core.Instance) (core.Instance, core.Instance) {
	switch {
	case core.InstanceOf(core.StringClass, sequence):
		return core.NewInteger(len(sequence.(core.String))), nil
	case core.InstanceOf(core.GeneralVectorClass, sequence):
		return core.NewInteger(len(sequence.(core.GeneralVector))), nil
	case core.InstanceOf(core.ListClass, sequence):
		return core.NewInteger(sequence.(core.List).Length()), nil
	}
	// TODO: core.SeqClass
	return SignalCondition(e, core.NewDomainError(e, sequence, core.ObjectClass), Nil)
}

// Elt returns the element of sequence that has index z. Indexing is 0-based;
// i.e., z = 0 designates the first element, Given a sequence and an integer z
// satisfying 0 ≤ z < (length sequence). An error shall be signaled if z is an
// integer outside of the mentioned range (error-id. index-out-of-range). An
// error shall be signaled if sequence is not a basic-vector or a list or if z
// is not an integer (error-id. domain-error).
func Elt(e core.Environment, sequence, z core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.IntegerClass, z); err != nil {
		return nil, err
	}
	switch {
	case core.InstanceOf(core.StringClass, sequence):
		seq := sequence.(core.String)
		idx := int(z.(core.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		return core.NewCharacter(seq[idx]), nil
	case core.InstanceOf(core.GeneralVectorClass, sequence):
		seq := sequence.(core.GeneralVector)
		idx := int(z.(core.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		return seq[idx], nil
	case core.InstanceOf(core.ListClass, sequence):
		seq := sequence.(core.List).Slice()
		idx := int(z.(core.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		return seq[idx], nil
	}
	return SignalCondition(e, core.NewDomainError(e, sequence, core.ObjectClass), Nil)

}

// SetElt is that these replace the object obtainable by elt with obj. The
// returned value is obj. An error shall be signaled if z is an integer outside
// of the valid range of indices (error-id. index-out-of-range). An error shall
// be signaled if sequence is not a basic-vector or a list or if z is not an
// integer (error-id. domain-error). obj may be any ISLISP object.
func SetElt(e core.Environment, obj, sequence, z core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.IntegerClass, z); err != nil {
		return nil, err
	}
	switch {
	case core.InstanceOf(core.StringClass, sequence):
		seq := sequence.(core.String)
		idx := int(z.(core.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		if err := ensure(e, core.CharacterClass, obj); err != nil {
			return nil, err
		}
		seq[idx] = rune(obj.(core.Character))
		return obj, nil
	case core.InstanceOf(core.GeneralVectorClass, sequence):
		seq := sequence.(core.GeneralVector)
		idx := int(z.(core.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		seq[idx] = obj
		return obj, nil
	case core.InstanceOf(core.ListClass, sequence):
		seq := sequence.(core.List).Slice()
		idx := int(z.(core.Integer))
		if idx > 0 && len(seq) <= idx {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		for idx != 0 && core.InstanceOf(core.ConsClass, sequence) {
			idx--
			sequence = sequence.(*core.Cons).Cdr
		}
		sequence.(*core.Cons).Car = obj
		return obj, nil
	}
	return SignalCondition(e, core.NewDomainError(e, sequence, core.ObjectClass), Nil)
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
func Subseq(e core.Environment, sequence, z1, z2 core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.IntegerClass, z1, z2); err != nil {
		return nil, err
	}
	start := int(z1.(core.Integer))
	end := int(z2.(core.Integer))
	switch {
	case core.InstanceOf(core.StringClass, sequence):
		seq := sequence.(core.String)
		if !(0 <= start && start < len(seq) && 0 <= end && end < len(seq) && start <= end) {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		return seq[start:end], nil
	case core.InstanceOf(core.GeneralVectorClass, sequence):
		seq := sequence.(core.GeneralVector)
		if !(0 <= start && start < len(seq) && 0 <= end && end < len(seq) && start <= end) {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		return seq[start:end], nil
	case core.InstanceOf(core.ListClass, sequence):
		seq := sequence.(core.List).Slice()
		if !(0 < start && start < len(seq) && 0 < end && end < len(seq) && start <= end) {
			return SignalCondition(e, core.NewIndexOutOfRange(e), Nil)
		}
		return List(e, seq[start:end]...)
	}
	return SignalCondition(e, core.NewDomainError(e, sequence, core.ObjectClass), Nil)
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
func MapInto(e core.Environment, destination, function core.Instance, sequences ...core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.ListClass, append(sequences, destination)...); err != nil {
		if err := ensure(e, core.BasicVectorClass, append(sequences, destination)...); err != nil {
			return nil, err
		}
	}
	if err := ensure(e, core.FunctionClass, function); err != nil {
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
	for i := 0; i < int(min.(core.Integer)); i++ {
		arguments := make([]core.Instance, len(sequences))
		for j, seq := range sequences {
			var err core.Instance
			arguments[j], err = Elt(e, seq, core.NewInteger(i))
			if err != nil {
				return nil, err
			}
		}
		ret, err := function.(core.Applicable).Apply(e.NewDynamic(), arguments...)
		if err != nil {
			return nil, err
		}
		_, err = SetElt(e, ret, destination, core.NewInteger(i))
		if err != nil {
			return nil, err
		}
	}
	return destination, nil
}
