// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"github.com/k0kubun/pp"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// General Array *
//

type GeneralArrayStar struct {
	Vector []*GeneralArrayStar
	Scalar ilos.Instance
}

func (GeneralArrayStar) Class() ilos.Class {
	return class.GeneralArrayStar
}

func (i GeneralArrayStar) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i GeneralArrayStar) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (i GeneralArrayStar) String() string {
	return ""
}

//
// General Vector
//

type GeneralVector []ilos.Instance

func (GeneralVector) Class() ilos.Class {
	return class.GeneralVector
}

func (i GeneralVector) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i GeneralVector) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (i GeneralVector) String() string {
	return pp.Sprint([]ilos.Instance(i))
}

//
// String
//

type String []rune

func (String) Class() ilos.Class {
	return class.String
}

func (i String) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i String) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (i String) String() string {
	return "\"" + string(i) + "\""
}
