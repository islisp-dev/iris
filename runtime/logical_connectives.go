// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
)

// Not is the logical “not” (or “¬”). It returns t if obj is nil
// and nil otherwise. obj may be any ISLISP object.
func Not(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if obj == Nil {
		return T, nil
	}
	return Nil, nil
}
