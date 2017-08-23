// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
)

func NewBlockTag(tag, object ilos.Instance) ilos.Instance {
	return Create(environment.NewEnvironment(nil, nil, nil),
		BlockTagClass,
		NewSymbol("TAG"), tag,
		NewSymbol("OBJECT"), object)
}
func NewCatchTag(tag, object ilos.Instance) ilos.Instance {
	return Create(environment.NewEnvironment(nil, nil, nil),
		CatchTagClass,
		NewSymbol("TAG"), tag,
		NewSymbol("OBJECT"), object)
}
func NewTagbodyTag(tag ilos.Instance) ilos.Instance {
	return Create(environment.NewEnvironment(nil, nil, nil), TagbodyTagClass, NewSymbol("TAG"), tag)
}
