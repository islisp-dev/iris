// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package parser

import "github.com/islisp-dev/iris/core"

func list2array(dim int, list core.Instance) (core.Instance, core.Instance) {
	if dim == 0 {
		return core.NewGeneralArrayStar(nil, list), nil
	}
	car, cdr, arrays := core.Nil, list, []*core.GeneralArrayStar{}
	for core.InstanceOf(core.ConsClass, cdr) {
		car, cdr = cdr.(*core.Cons).Car, cdr.(*core.Cons).Cdr
		array, err := list2array(dim-1, car)
		if err != nil {
			return nil, err
		}
		arrays = append(arrays, array.(*core.GeneralArrayStar))
	}
	return core.NewGeneralArrayStar(arrays, nil), nil
}

func list2vector(list core.Instance) (core.Instance, core.Instance) {
	return core.NewGeneralVector(list.(core.List).Slice()), nil
}
