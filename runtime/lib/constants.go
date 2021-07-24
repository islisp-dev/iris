// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/runtime/core"

// Quote is used to include any object in an ISLisp text. A quoted expression
// denotes a reference to an object.
func Quote(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	return obj, nil
}
