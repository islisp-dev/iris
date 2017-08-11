// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package class

import "github.com/ta2gch/iris/runtime/ilos"

type builtinclass struct {
	parents []ilos.Class
	slots   []string
	name    string
}

func (*builtinclass) Class() ilos.Class {
	return BuiltInClass
}

func (p *builtinclass) Parents() []ilos.Class {
	return p.parents
}

func (p *builtinclass) Slots() []string {
	return p.slots
}

func (i *builtinclass) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i *builtinclass) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (p *builtinclass) String() string {
	return p.name
}
