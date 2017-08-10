package instance

import (
	"fmt"
	"reflect"
	"runtime"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

type Applicable interface {
	Apply(*environment.Environment, *environment.Environment, ilos.Instance) (ilos.Instance, ilos.Instance)
}

type NativeFunction struct {
	name     Symbol
	function interface{}
}

func (NativeFunction) Class() ilos.Class {
	return class.Function
}

func (NativeFunction) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (NativeFunction) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (f NativeFunction) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

func (f NativeFunction) Apply(local, global *environment.Environment, args ilos.Instance) (ilos.Instance, ilos.Instance) {
	fv := reflect.ValueOf(f.function)
	ft := reflect.TypeOf(f.function)
	cdr := args
	argv := []reflect.Value{reflect.ValueOf(local), reflect.ValueOf(global)}
	for Of(class.Cons, cdr) {
		cadr := UnsafeCar(cdr)
		cddr := UnsafeCdr(cdr)
		argv = append(argv, reflect.ValueOf(cadr))
		cdr = cddr
	}
	if ft.NumIn() != len(argv) && (!ft.IsVariadic() || ft.NumIn()-2 > len(argv)) {
		return nil, New(class.WrongNumberOfArguments, map[string]ilos.Instance{
			"FORM":      New(class.Symbol, runtime.FuncForPC(fv.Pointer()).Name()),
			"ARGUMENTS": args,
		})
	}
	rets := fv.Call(argv)
	a, _ := rets[0].Interface().(ilos.Instance)
	b, _ := rets[1].Interface().(ilos.Instance)
	return a, b

}

type Function struct {
	name     Symbol
	function interface{}
	min      int
	max      int
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

func (f Function) Apply(local, global *environment.Environment, args ilos.Instance) (ilos.Instance, ilos.Instance) {
	cnt := 0
	cdr := args
	for Of(class.Cons, cdr) {
		cadr := UnsafeCar(cdr)
		cddr := UnsafeCdr(cdr)
		cnt++
		if cadr == New(class.Symbol, ":REST") || cadr == New(class.Symbol, "&REST") {
			break
		}
		cdr = cddr
	}
	if cnt < f.min || f.max < cnt {
		return nil, New(class.WrongNumberOfArguments, map[string]ilos.Instance{
			"FORM":      New(class.Symbol, f.name),
			"ARGUMENTS": args,
		})
	}
	a, b := f.function.(func(*environment.Environment, *environment.Environment, ilos.Instance) (ilos.Instance, ilos.Instance))(local, global, args)
	return a, b
}
