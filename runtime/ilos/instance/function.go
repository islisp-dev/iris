package instance

import (
	"fmt"
	"reflect"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

type Applicable interface {
	Apply(*environment.Environment, *environment.Environment, ilos.Instance) (ilos.Instance, ilos.Instance)
}

type Function struct {
	name     Symbol
	function interface{}
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

func (f Function) Apply(local, global *environment.Environment, arguments ilos.Instance) (ilos.Instance, ilos.Instance) {
	fv := reflect.ValueOf(f.function)
	ft := reflect.TypeOf(f.function)
	cdr := arguments
	argv := []reflect.Value{reflect.ValueOf(local), reflect.ValueOf(global)}
	for Of(class.Cons, cdr) {
		cadr := UnsafeCar(cdr)
		cddr := UnsafeCdr(cdr)
		argv = append(argv, reflect.ValueOf(cadr))
		cdr = cddr
	}
	if ft.NumIn() != len(argv) && (!ft.IsVariadic() || ft.NumIn()-2 > len(argv)) {
		return nil, New(class.WrongNumberOfArguments, map[string]ilos.Instance{
			"FORM":      f.name,
			"ARGUMENTS": arguments,
		})
	}
	rets := fv.Call(argv)
	a, _ := rets[0].Interface().(ilos.Instance)
	b, _ := rets[1].Interface().(ilos.Instance)
	return a, b

}
