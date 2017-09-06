// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
)

// General Array *

type GeneralArrayStar struct {
	Vector []*GeneralArrayStar
	Scalar ilos.Instance
}

func NewGeneralArrayStar(vector []*GeneralArrayStar, scalar ilos.Instance) ilos.Instance {
	return &GeneralArrayStar{vector, scalar}
}

func (*GeneralArrayStar) Class() ilos.Class {
	return GeneralArrayStarClass
}

func (i *GeneralArrayStar) String() string {
	var count func(i *GeneralArrayStar) int
	count = func(i *GeneralArrayStar) int {
		if i.Vector != nil {
			return 1 + count(i.Vector[0])
		}
		return 0
	}
	var stringify func(i *GeneralArrayStar) string
	stringify = func(i *GeneralArrayStar) string {
		if i.Vector != nil {
			str := "("
			for idx, elt := range i.Vector {
				str += stringify(elt)
				if idx != len(i.Vector)-1 {
					str += " "
				}
			}
			str += ")"
			return str
		}
		return i.Scalar.String()
	}
	return fmt.Sprintf("#%vA%v", count(i), stringify(i))
}

// General Vector

type GeneralVector []ilos.Instance

func NewGeneralVector(v []ilos.Instance) ilos.Instance {
	return GeneralVector(v)
}

func (GeneralVector) Class() ilos.Class {
	return GeneralVectorClass
}

func (i GeneralVector) String() string {
	str := fmt.Sprint([]ilos.Instance(i))
	return fmt.Sprintf("#(%v)", str[1:len(str)-1])
}

// String

type String []rune

func NewString(s []rune) ilos.Instance {
	return String(s)
}

func (String) Class() ilos.Class {
	return StringClass
}

func (i String) String() string {
	return "\"" + string(i) + "\""
}
