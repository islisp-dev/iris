// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"fmt"
)

// General Array *

type GeneralArrayStar struct {
	Vector []*GeneralArrayStar
	Scalar Instance
}

func NewGeneralArrayStar(vector []*GeneralArrayStar, scalar Instance) Instance {
	return &GeneralArrayStar{vector, scalar}
}

func (*GeneralArrayStar) Class() Class {
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

type GeneralVector []Instance

func NewGeneralVector(v []Instance) Instance {
	return GeneralVector(v)
}

func (GeneralVector) Class() Class {
	return GeneralVectorClass
}

func (i GeneralVector) String() string {
	str := fmt.Sprint([]Instance(i))
	return fmt.Sprintf("#(%v)", str[1:len(str)-1])
}

// String

type String []rune

func NewString(s []rune) Instance {
	return String(s)
}

func (String) Class() Class {
	return StringClass
}

func (i String) String() string {
	return "\"" + string(i) + "\""
}
