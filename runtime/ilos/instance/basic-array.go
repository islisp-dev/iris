// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"github.com/k0kubun/pp"
	"github.com/ta2gch/iris/runtime/ilos"
)

//
// General Array *
//

type GeneralArrayStar struct {
	Vector []GeneralArrayStar
	Scalar ilos.Instance
}

func NewGeneralArrayStar(vector []GeneralArrayStar, scalar ilos.Instance) ilos.Instance {
	return GeneralArrayStar{vector, scalar}
}

func (GeneralArrayStar) Class() ilos.Class {
	return GeneralArrayStarClass
}

func (i GeneralArrayStar) String() string {
	return ""
}

//
// General Vector
//

type GeneralVector []ilos.Instance

func NewGeneralVector(v []ilos.Instance) ilos.Instance {
	return GeneralVector(v)
}

func (GeneralVector) Class() ilos.Class {
	return GeneralVectorClass
}

func (i GeneralVector) String() string {
	return pp.Sprint([]ilos.Instance(i))
}

//
// String
//

type String []rune

func NewString(s string) ilos.Instance {
	return String(s)
}

func (String) Class() ilos.Class {
	return StringClass
}

func (i String) String() string {
	return "\"" + string(i) + "\""
}
