// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/env"
	"github.com/islisp-dev/iris/runtime/ilos"
)

// Quote is used to include any object in an ISLisp text. A quoted expression
// denotes a reference to an object.
func Quote(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	return obj, nil
}
