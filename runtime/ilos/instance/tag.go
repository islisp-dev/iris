// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"github.com/ta2gch/iris/runtime/ilos"
)

func NewBlockTag(tag, object ilos.Instance) ilos.Instance {
	return newInstance(BlockTagClass, map[ilos.Instance]ilos.Instance{
		NewSymbol("TAG"):    tag,
		NewSymbol("OBJECT"): object,
	})
}
func NewCatchTag(tag, object ilos.Instance) ilos.Instance {
	return newInstance(CatchTagClass, map[ilos.Instance]ilos.Instance{
		NewSymbol("TAG"):    tag,
		NewSymbol("OBJECT"): object,
	})
}
func NewTagbodyTag(tag ilos.Instance) ilos.Instance {
	return newInstance(TagbodyTagClass, map[ilos.Instance]ilos.Instance{
		NewSymbol("TAG"): tag,
	})
}
