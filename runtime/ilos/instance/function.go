// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"
	"reflect"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

type Applicable interface {
	Apply(*environment.Environment, *environment.Environment, ...ilos.Instance) (ilos.Instance, ilos.Instance)
}

type Function struct {
	name     ilos.Instance
	function interface{}
}

func NewFunction(name ilos.Instance, function interface{}) ilos.Instance {
	return Function{name, function}
}

func (Function) Class() ilos.Class {
	return class.Function
}

func (Function) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (Function) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (f Function) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

func (f Function) Apply(local, global *environment.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	fv := reflect.ValueOf(f.function)
	ft := reflect.TypeOf(f.function)
	argv := []reflect.Value{reflect.ValueOf(local), reflect.ValueOf(global)}
	for _, cadr := range arguments {
		argv = append(argv, reflect.ValueOf(cadr))
	}
	if ft.NumIn() != len(argv) && (!ft.IsVariadic() || ft.NumIn()-2 >= len(argv)) {
		return nil, NewArityError()
	}
	rets := fv.Call(argv)
	a, _ := rets[0].Interface().(ilos.Instance)
	b, _ := rets[1].Interface().(ilos.Instance)
	return a, b

}
