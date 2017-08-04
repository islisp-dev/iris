package runtime

import (
	e "github.com/ta2gch/iris/runtime/environment"
	i "github.com/ta2gch/iris/runtime/ilos/instance"
)

func Init() {
	e.TopLevel.DefineMacro(i.NewSymbol("LAMBDA"), i.NewFunction(lambda))
	e.TopLevel.DefineMacro(i.NewSymbol("QUOTE"), i.NewFunction(quote))
	e.TopLevel.DefineFunction(i.NewSymbol("THROW"), i.NewFunction(throw))
	e.TopLevel.DefineMacro(i.NewSymbol("CATCH"), i.NewFunction(catch))
}
