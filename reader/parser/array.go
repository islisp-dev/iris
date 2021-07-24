// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package parser

import (
	"github.com/islisp-dev/iris/runtime/ilos"
)

func list2array(dim int, list ilos.Instance) (ilos.Instance, ilos.Instance) {
	if dim == 0 {
		return ilos.NewGeneralArrayStar(nil, list), nil
	}
	car, cdr, arrays := ilos.Nil, list, []*ilos.GeneralArrayStar{}
	for ilos.InstanceOf(ilos.ConsClass, cdr) {
		car, cdr = cdr.(*ilos.Cons).Car, cdr.(*ilos.Cons).Cdr
		array, err := list2array(dim-1, car)
		if err != nil {
			return nil, err
		}
		arrays = append(arrays, array.(*ilos.GeneralArrayStar))
	}
	return ilos.NewGeneralArrayStar(arrays, nil), nil
}

func list2vector(list ilos.Instance) (ilos.Instance, ilos.Instance) {
	return ilos.NewGeneralVector(list.(ilos.List).Slice()), nil
}
