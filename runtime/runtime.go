package runtime

import (
	e "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos/class"
	i "github.com/ta2gch/iris/runtime/ilos/instance"
)

func Init() {
	e.TopLevel.Macro.Define(i.New(class.Symbol, "LAMBDA"), i.New(class.Function, lambda))
	e.TopLevel.Macro.Define(i.New(class.Symbol, "QUOTE"), i.New(class.Function, quote))
	e.TopLevel.Macro.Define(i.New(class.Symbol, "THROW"), i.New(class.Function, throw))
	e.TopLevel.Macro.Define(i.New(class.Symbol, "CATCH"), i.New(class.Function, catch))
	e.TopLevel.Macro.Define(i.New(class.Symbol, "BLOCK"), i.New(class.Function, block))
	e.TopLevel.Macro.Define(i.New(class.Symbol, "RETURN-FROM"), i.New(class.Function, returnFrom))
	e.TopLevel.Macro.Define(i.New(class.Symbol, "TAGBODY"), i.New(class.Function, tagbody))
	e.TopLevel.Macro.Define(i.New(class.Symbol, "GO"), i.New(class.Function, tagbodyGo))
	e.TopLevel.Macro.Define(i.New(class.Symbol, "FUNCTION"), i.New(class.Function, function))
}
