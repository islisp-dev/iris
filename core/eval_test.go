package core

import (
	"reflect"
	"strings"
	"testing"

	"github.com/ta2gch/gazelle/core/class"
	"github.com/ta2gch/gazelle/core/class/cons"
	"github.com/ta2gch/gazelle/core/class/function"
	env "github.com/ta2gch/gazelle/core/environment"
	"github.com/ta2gch/gazelle/reader/parser"
	"github.com/ta2gch/gazelle/reader/tokenizer"
)

func read(s string) *class.Instance {
	e, _ := parser.Parse(tokenizer.New(strings.NewReader(s)))
	return e
}

func TestEval(t *testing.T) {
	local := env.New()
	global := env.New()
	local.SetVariable(class.Symbol.New("pi"), class.Float.New(3.14))
	local.SetFunction(class.Symbol.New("inc"), function.New(func(args *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
		car, _ := cons.Car(args)
		return class.Integer.New(car.Value().(int) + 1), nil
	}))
	local.SetMacro(class.Symbol.New("minc"), function.New(func(args *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
		ret, _ := Eval(cons.New(class.Symbol.New("inc"), args), local, global)
		return ret, nil
	}))
	local.SetMacro(class.Symbol.New("lambda"), function.New(func(args *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
		lambdaList, _ := cons.Car(args)
		lambdaBody, _ := cons.Cdr(args)
		return Lambda(lambdaList, lambdaBody, local), nil
	}))
	type args struct {
		obj    *class.Instance
		local  *env.Environment
		global *env.Environment
	}
	tests := []struct {
		name    string
		args    args
		want    *class.Instance
		wantErr bool
	}{
		{
			name:    "local variable",
			args:    args{class.Symbol.New("pi"), local, global},
			want:    class.Float.New(3.14),
			wantErr: false,
		},
		{
			name:    "local function",
			args:    args{read("(inc (inc 1))"), local, global},
			want:    class.Integer.New(3),
			wantErr: false,
		},
		{
			name:    "local macro",
			args:    args{read("(minc (minc 1))"), local, global},
			want:    class.Integer.New(3),
			wantErr: false,
		},
		{
			name:    "lambda",
			args:    args{read("((lambda (x) (minc x)) 1)"), local, global},
			want:    class.Integer.New(2),
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
