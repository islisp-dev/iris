// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func ClassOf(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	return obj.Class(), nil
}

func Instancep(e env.Environment, obj ilos.Instance, class ilos.Class) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(class, obj) {
		return T, nil
	}
	return Nil, nil
}

func Subclassp(e env.Environment, class1, class2 ilos.Class) (ilos.Instance, ilos.Instance) {
	if ilos.SubclassOf(class1, class2) {
		return T, nil
	}
	return Nil, nil
}

func Class(e env.Environment, className ilos.Instance) (ilos.Class, ilos.Instance) {
	if v, ok := TopLevel.Class.Get(className); ok {
		return v.(ilos.Class), nil
	}
	return nil, instance.NewUndefinedClass(className)
}
