package runtime

import (
	"reflect"
	"testing"

	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
)

func TestCatch(t *testing.T) {
	local, global := env.New(), env.TopLevel
	defmacro("CATCH", catch)
	defmacro("THROW", throw)
	defmacro("QUOTE", quote)
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
			name:    "catch & throw",
			args:    args{local, global, readFromString("(catch 'foo 1 (throw 'foo 1))")},
			want:    readFromString("1"),
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
