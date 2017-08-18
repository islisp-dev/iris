// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

/*
import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func Defclass(local, global environment.Environment, className, scNames, slotSpecs ilos.Instance, classOpts ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Symbol, className); err != nil {
		return nil, err
	}
	if err := ensure(class.List, scNames, slotSpecs); err != nil {
		return nil, err
	}
	supers := []ilos.Class{}
	specs := slotSpecs.(instance.List).Slice()
	for _, scName := range scNames.(instance.List).Slice() {
		super, ok := global.Class.Get(scName)
		if !ok {
			return nil, nil // TOOD: Undefined Entity
		}
		supers = append(supers, super.(ilos.Class))
	}
	for _, slotSpec := range slotSpecs.(instance.List).Slice() {
		if instance.Of(class.Symbol, slotSpec) {
			slotName := slotSpec
		}
		slotName := slotSpec.(*instance.Cons).Car
		slotOpts := slotSpec.(*instance.Cons).Cdr.(instance.List).Slice()
		var readerFunctionName, writerFunctionName, boundpFunctionName ilos.Instance
		for i := 0; i < len(slotOpts); i += 2 {
			switch slotOpts[i] {
			case instance.NewSymbol(":INITFORM"):
				form := slotOpts[i+1]
			case instance.NewSymbol(":INITARG"):
				initargName := slotOpts[i+1]
			}
		}
	}
	for _, classOpt := range classOpts {
		switch classOpt.(*instance.Cons).Car {
		case instance.NewSymbol(":METACLASS"):
			metaclass := classOpt.(*instance.Cons).Cdr.(*instance.Cons).Car
		case instance.NewSymbol(":ABSTRACTP"):
			switch classOpt.(*instance.Cons).Cdr.(*instance.Cons).Car {
			case T:
			case Nil:
			default:
			}
		default:
		}
	}
}
*/
