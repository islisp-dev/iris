// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package parser

import (
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func list2array(dim int, list ilos.Instance) (ilos.Instance, ilos.Instance) {
	if dim == 0 {
		return instance.NewGeneralArrayStar(nil, list), nil
	}
	elements := list.(instance.List).Slice()
	arrays := make([]instance.GeneralArrayStar, len(elements))
	for idx, elt := range elements {
		array, err := list2array(dim-1, elt)
		if err != nil {
			return nil, err
		}
		arrays[idx] = array.(instance.GeneralArrayStar)
	}
	return instance.NewGeneralArrayStar(arrays, nil), nil
}

func list2vector(list ilos.Instance) (ilos.Instance, ilos.Instance) {
	return instance.NewGeneralVector(list.(instance.List).Slice()), nil
}
