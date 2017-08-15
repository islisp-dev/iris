// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

func NewBlockTag(tag, object ilos.Instance) ilos.Instance {
	return New(class.BlockTag, map[string]ilos.Instance{
		"TAG":    tag,
		"OBJECT": object,
	})
}
func NewCatchTag(tag, object ilos.Instance) ilos.Instance {
	return New(class.CatchTag, map[string]ilos.Instance{
		"TAG":    tag,
		"OBJECT": object,
	})
}
func NewTagbodyTag(tag ilos.Instance) ilos.Instance {
	return New(class.TagbodyTag, map[string]ilos.Instance{
		"TAG": tag,
	})
}
