package runtime

import (
	"reflect"
	"strings"
	"testing"

	"github.com/ta2gch/iris/reader/parser"
	"github.com/ta2gch/iris/reader/tokenizer"

	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func read(s string) ilos.Instance {
	e, _ := parser.Parse(tokenizer.New(strings.NewReader(s)))
	return e
}

func TestEval(t *testing.T) {
	local := env.New()
	global := env.New()
	local.DefineVariable(instance.NewSymbol("PI"), instance.NewFloat(3.14))
	local.DefineFunction(instance.NewSymbol("INC"), instance.NewFunction(func(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
		car := instance.UnsafeCar(args)
		return instance.NewInteger(int(car.(instance.Integer)) + 1), nil
	}))
	local.DefineMacro(instance.NewSymbol("MINC"), instance.NewFunction(func(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
		ret, err := Eval(instance.NewCons(instance.NewSymbol("INC"), args), local, global)
		return ret, err
	}))
	local.DefineMacro(instance.NewSymbol("LAMBDA"), instance.NewFunction(lambda))
	type args struct {
		obj    ilos.Instance
		local  *env.Environment
		global *env.Environment
	}
	tests := []struct {
		name    string
		args    args
		want    ilos.Instance
		wantErr bool
	}{
		{
			name:    "local variable",
			args:    args{instance.NewSymbol("PI"), local, global},
			want:    instance.NewFloat(3.14),
			wantErr: false,
		},
		{
			name:    "local function",
			args:    args{read("(inc (inc 1))"), local, global},
			want:    instance.NewInteger(3),
			wantErr: false,
		},
		{
			name:    "local macro",
			args:    args{read("(minc (minc 1))"), local, global},
			want:    instance.NewInteger(3),
			wantErr: false,
		},
		{
			name:    "lambda form",
			args:    args{read("((lambda (x)) 1)"), local, global},
			want:    instance.NewNull(),
			wantErr: false,
		},
		{
			name:    "lambda form",
			args:    args{read("((lambda (:rest xs) xs) 1 2)"), local, global},
			want:    read("(1 2)"),
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := Eval(tt.args.obj, tt.args.local, tt.args.global)
			if (err != nil) != tt.wantErr {
				t.Errorf("Eval() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Eval() = %v, want %v", got, tt.want)
			}
		})
	}
}
