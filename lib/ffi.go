package lib

import (
	"fmt"
	"plugin"
	"strings"
	"github.com/islisp-dev/iris/core"
)

// (import greet :from "greet.so")
func Import(env core.Environment, args ...core.Instance) (core.Instance, core.Instance) {
	if len(args) < 2 {
		return SignalCondition(env, core.Create(env, core.ProgramErrorClass), Nil)
	}
	if !core.DeepEqual(args[len(args)-2], core.NewSymbol(":FROM")) {
		return SignalCondition(env, core.Create(env, core.ProgramErrorClass), Nil)
	}
	if !core.InstanceOf(core.StringClass, args[len(args)-1]) {
		return SignalCondition(env, core.Create(env, core.ProgramErrorClass), Nil)
	}
	for i := 0; i < len(args) - 2; i++ {
		if !core.InstanceOf(core.SymbolClass, args[i]) {
			return SignalCondition(env, core.Create(env, core.ProgramErrorClass), Nil)
		}
	}
	path := args[len(args)-1]
	syms := args[:len(args)-2]
	paths := string([]rune(path.(core.String)))
	p, _ := plugin.Open(paths)
	for _, sym := range syms {
		str := sym.(core.Symbol).String()
		str = strings.ToLower(str)
		str = strings.Replace(str, "-", " ", -1)
		str = strings.Title(str)
		str = strings.Replace(str, " ", "", -1)
		f, err := p.Lookup(str)
		if err != nil {
			fmt.Printf("%v is not exported\n", str)
			return SignalCondition(env, core.Create(env, core.ProgramErrorClass), Nil)
		}
		env.Function[:1].Define(sym, core.NewFunction(sym, f))
	}
	return Nil, nil
}
