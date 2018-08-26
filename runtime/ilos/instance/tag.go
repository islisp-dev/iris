// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"github.com/xtaniguchimasaya/iris/runtime/env"
	"github.com/xtaniguchimasaya/iris/runtime/ilos"
)

func NewBlockTag(tag, uid, object ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil, nil),
		BlockTagClass,
		NewSymbol("IRIS.TAG"), tag,
		NewSymbol("IRIS.UID"), uid,
		NewSymbol("IRIS.OBJECT"), object)
}
func NewCatchTag(tag, uid, object ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil, nil),
		CatchTagClass,
		NewSymbol("IRIS.TAG"), tag,
		NewSymbol("IRIS.UID"), uid,
		NewSymbol("IRIS.OBJECT"), object)
}
func NewTagbodyTag(tag, uid ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil, nil),
		TagbodyTagClass,
		NewSymbol("IRIS.TAG"), tag,
		NewSymbol("IRIS.UID"), uid)
}
