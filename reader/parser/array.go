// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package parser

import (
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func list2array(dim int, list ilos.Instance) (ilos.Instance, ilos.Instance) {
	if dim == 0 {
		return instance.NewGeneralArrayStar(nil, list), nil
	}
	cdr := list
	arrays := []instance.GeneralArrayStar{}
	for ilos.InstanceOf(class.Cons, cdr) {
		array, err := list2array(dim-1, cdr.(*instance.Cons).Car)
		if err != nil {
			return nil, err
		}
		arrays = append(arrays, array.(instance.GeneralArrayStar))
	}
	return instance.NewGeneralArrayStar(arrays, nil), nil
}

func list2vector(list ilos.Instance) (ilos.Instance, ilos.Instance) {
	return instance.NewGeneralVector(list.(instance.List).Slice()), nil
}
