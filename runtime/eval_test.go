package runtime

import (
	"reflect"
	"testing"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func TestEval(t *testing.T) {
	local := environment.New()
	global := environment.TopLevel
	defun("INC", func(local, global *environment.Environment, arg ilos.Instance) (ilos.Instance, ilos.Instance) {
		return instance.New(class.Integer, int(arg.(instance.Integer))+1), nil
	})
	defglobal("PI", instance.New(class.Float, 3.14))
	defmacro("MINC", func(local *environment.Environment, global *environment.Environment, arg ilos.Instance) (ilos.Instance, ilos.Instance) {
		ret, err := Eval(local, global, instance.New(class.Cons, instance.New(class.Symbol, "INC"), instance.New(class.Cons, arg, instance.New(class.Null))))
		return ret, err
	})
	type arguments struct {
		obj    ilos.Instance
		local  *environment.Environment
		global *environment.Environment
	}
	tests := []struct {
		name    string
		arguments    arguments
		want    ilos.Instance
		wantErr bool
	}{
		{
			name:    "local variable",
			arguments:    arguments{instance.New(class.Symbol, "PI"), local, global},
			want:    instance.New(class.Float, 3.14),
			wantErr: false,
		},
		{
			name:    "local function",
			arguments:    arguments{readFromString("(inc (inc 1))"), local, global},
			want:    instance.New(class.Integer, 3),
			wantErr: false,
		},
		{
			name:    "local macro",
			arguments:    arguments{readFromString("(minc (minc 1))"), local, global},
			want:    instance.New(class.Integer, 3),
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := Eval(tt.arguments.local, tt.arguments.global, tt.arguments.obj)
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
