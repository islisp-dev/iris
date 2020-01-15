// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package parser

import (
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/class"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func list2array(dim int, list ilos.Instance) (ilos.Instance, ilos.Instance) {
	if dim == 0 {
		return instance.NewGeneralArrayStar(nil, list), nil
	}
	car, cdr, arrays := instance.Nil, list, []*instance.GeneralArrayStar{}
	for ilos.InstanceOf(class.Cons, cdr) {
		car, cdr = cdr.(*instance.Cons).Car, cdr.(*instance.Cons).Cdr
		array, err := list2array(dim-1, car)
		if err != nil {
			return nil, err
		}
		arrays = append(arrays, array.(*instance.GeneralArrayStar))
	}
	return instance.NewGeneralArrayStar(arrays, nil), nil
}

func list2vector(list ilos.Instance) (ilos.Instance, ilos.Instance) {
	return instance.NewGeneralVector(list.(instance.List).Slice()), nil
}
