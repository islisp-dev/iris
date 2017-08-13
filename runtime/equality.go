// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"reflect"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
)

// Eq tests whether obj1 and obj2 are same identical object.
// They return t if the objects are the same; otherwise, they return nil.
// Two objects are the same if there is no operation that could distinguish
// them (without modifying them), and if modifying one would modify the other the same way.
func Eq(_, _ *environment.Environment, obj1, obj2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if obj1 == obj2 {
		return T, nil
	}
	return Nil, nil
}

// Eql tests whether obj1 and obj2 are same identical object.
// They return t if the objects are the same; otherwise, they return nil.
// Two objects are the same if there is no operation that could distinguish
// them (without modifying them), and if modifying one would modify the other the same way.
func Eql(_, _ *environment.Environment, obj1, obj2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if obj1 == obj2 {
		return T, nil
	}
	return Nil, nil
}

// Equal tests whether obj1 and obj2 are isomorphic—i.e., whether obj1 and obj2 denote the same
// structure with equivalent values. equal returns t if the test was satisfied, and nil if not.
// Specifically:
//
// If obj1 and obj2 are direct instances of the same class, equal returns t if they are eql.
func Equal(_, _ *environment.Environment, obj1, obj2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if reflect.DeepEqual(obj1, obj2) {
		return T, nil
	}
	return Nil, nil
}
