package runtime

import (
	"reflect"
	"strings"
	"testing"

	"github.com/ta2gch/iris/reader/parser"
	"github.com/ta2gch/iris/reader/tokenizer"

	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func read(s string) ilos.Instance {
	e, _ := parser.Parse(tokenizer.New(strings.NewReader(s)))
	return e
}

func TestEval(t *testing.T) {
	Init()
	local := env.New()
	global := env.TopLevel
	inc2 := func(local, global *env.Environment, arg ilos.Instance) (ilos.Instance, ilos.Instance) {
		return instance.New(class.Integer, int(arg.(instance.Integer))+1), nil
	}
	local.Variable.Define(instance.New(class.Symbol, "PI"), instance.New(class.Float, 3.14))
	local.Function.Define(instance.New(class.Symbol, "INC"), instance.New(class.Function, inc2))
	local.Macro.Define(instance.New(class.Symbol, "MINC"), instance.New(class.Function, func(local *env.Environment, global *env.Environment, arg ilos.Instance) (ilos.Instance, ilos.Instance) {
		ret, err := Eval(local, global, instance.New(class.Cons, instance.New(class.Symbol, "INC"), instance.New(class.Cons, arg, instance.New(class.Null))))
		return ret, err
	}))
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
			args:    args{instance.New(class.Symbol, "PI"), local, global},
			want:    instance.New(class.Float, 3.14),
			wantErr: false,
		},
		{
			name:    "local function",
			args:    args{read("(inc (inc 1))"), local, global},
			want:    instance.New(class.Integer, 3),
			wantErr: false,
		},
		{
			name:    "local macro",
			args:    args{read("(minc (minc 1))"), local, global},
			want:    instance.New(class.Integer, 3),
			wantErr: false,
		},
		{
			name:    "lambda form",
			args:    args{read("((lambda (x)) 1)"), local, global},
			want:    instance.New(class.Null),
			wantErr: false,
		},
		{
			name:    "lambda form",
			args:    args{read("((lambda (:rest xs) xs) 1 2)"), local, global},
			want:    read("(1 2)"),
			wantErr: false,
		},
		{
			name:    "catch & throw",
			args:    args{read("(catch 'foo 1 (throw 'foo 1))"), local, global},
			want:    read("1"),
			wantErr: false,
		},
		{
			name:    "block & return-from",
			args:    args{read("(block 'foo 1 (return-from'foo 1))"), local, global},
			want:    read("1"),
			wantErr: false,
		},
		{
			name:    "tagbody & go",
			args:    args{read("(catch 'foo (tagbody (go bar) (throw 'foo 1) bar))"), local, global},
			want:    instance.New(class.Null),
			wantErr: false,
		},
		{
			name:    "nested tagbody & go",
			args:    args{read("(catch 'foo (tagbody (tagbody (go bar) (throw 'foo 1) bar (go foobar)) foobar))"), local, global},
			want:    instance.New(class.Null),
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := Eval(tt.args.local, tt.args.global, tt.args.obj)
			if (err != nil) != tt.wantErr {
				t.Errorf("Eval(err, tt.wantErr, ) error = %v, wantErr %v")
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Eval(got, tt.want, ) = %v, want %v")
			}
		})
	}
}
