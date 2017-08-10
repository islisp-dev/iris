package runtime

import (
	"reflect"
	"testing"

	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func TestLambda(t *testing.T) {
	local, global := env.New(), env.TopLevel
	defmacro("LAMBDA", lambda)
	type args struct {
		local  *env.Environment
		global *env.Environment
		obj    ilos.Instance
	}
	tests := []struct {
		name    string
		args    args
		want    ilos.Instance
		wantErr bool
	}{
		{
			name:    "lambda form",
			args:    args{local, global, readFromString("((lambda (x)) 1)")},
			want:    instance.New(class.Null),
			wantErr: false,
		},
		{
			name:    "lambda form",
			args:    args{local, global, readFromString("((lambda (:rest xs) xs) 1 2)")},
			want:    readFromString("(1 2)"),
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := Eval(tt.args.local, tt.args.global, tt.args.obj)
			if (err != nil) != tt.wantErr {
				t.Errorf("Eval() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Eval() = %v, want %v", got, tt.want)
				t.Errorf("Eval() = %v, want %v", got, tt.want)
			}
		})
	}
}
