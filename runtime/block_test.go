package runtime

import (
	"reflect"
	"testing"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
)

func TestBlock(t *testing.T) {
	local, global := environment.New(), environment.TopLevel
	defspecial("BLOCK", block)
	defspecial("RETURN-FROM", return_from)
	defspecial("QUOTE", quote)
	type arguments struct {
		local  *environment.Environment
		global *environment.Environment
		obj    ilos.Instance
	}
	tests := []struct {
		name      string
		arguments arguments
		want      ilos.Instance
		wantErr   bool
	}{
		{
			name:      "block & return-from",
			arguments: arguments{local, global, readFromString("(block 'foo 1 (return-from 'foo 1))")},
			want:      readFromString("1"),
			wantErr:   false,
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
				t.Errorf("Eval() = %v, want %v", got, tt.want)
			}
		})
	}
}
