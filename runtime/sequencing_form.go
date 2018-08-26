// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/xtaniguchimasaya/iris/runtime/env"
	"github.com/xtaniguchimasaya/iris/runtime/ilos"
)

// Progn allows a series of forms to be evaluated, where normally only one could
// be used. The result of evaluation of the last form of form* is returned. All
// the forms are evaluated from left to right. The values of all the forms but
// the last are discarded, so they are executed only for their side-effects.
// progn without forms returns nil.
func Progn(e env.Environment, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	ret := Nil
	for _, form := range forms {
		ret, err = Eval(e, form)
		if err != nil {
			return nil, err
		}
	}
	return ret, nil
}
