package instance

import (
	"fmt"
	"reflect"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

type Callable interface {
	Apply(args ilos.Instance, local, global *environment.Environment) (ilos.Instance, ilos.Instance)
}

type Lambda func(*environment.Environment, *environment.Environment, ilos.Instance) (ilos.Instance, ilos.Instance)

func (Lambda) Class() ilos.Class {
	return class.Function
}

func (Lambda) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (Lambda) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (f Lambda) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

func (f Lambda) Apply(args ilos.Instance, local, global *environment.Environment) (ilos.Instance, ilos.Instance) {
	a, b := f(local, global, args)
	return a, b
}

type Function struct {
	fn interface{}
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

func (f Function) Apply(args ilos.Instance, local, global *environment.Environment) (ilos.Instance, ilos.Instance) {
	fv := reflect.ValueOf(f.fn)
	cdr := args
	argv := []reflect.Value{reflect.ValueOf(local), reflect.ValueOf(global)}
	for Of(class.Cons, cdr) {
		cadr := UnsafeCar(cdr)
		cddr := UnsafeCdr(cdr)
		argv = append(argv, reflect.ValueOf(cadr))
		cdr = cddr
	}
	rets := fv.Call(argv)

	a, _ := rets[0].Interface().(ilos.Instance)
	b, _ := rets[1].Interface().(ilos.Instance)
	return a, b

}
