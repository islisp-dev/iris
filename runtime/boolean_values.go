// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// The values t and nil are called booleans. t denotes true,
// and nil is the only value denoting false. Predicates,
// also called boolean functions, are functions that return
// t when satisfied and nil otherwise.
//
// Any object other than nil is treated as true (not just t).
// When objects are treated as true or nil this way they are
// called quasi-booleans.
//
// t is an identifier naming the symbol t, and nil is
// an identifier naming the symbol nil (which is also the empty list).
// nil is the unique instance of the null class.
//
// Like boolean functions, the and and or special forms return truth values;
// however, these truth values are nil when the test is not
// satisfied and a non-nil value otherwise.
// The result of and and or are quasi-booleans.
//
// t is a named constant whose value is the symbol t itself.
// nil is a named constant whose value is the symbol nil itself.
var (
	Nil = instance.New(class.Null)
	T   = instance.Symbol("T")
)
