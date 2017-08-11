package runtime

import (
	"reflect"
	"testing"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func TestTagbody(t *testing.T) {
	local, global := environment.New(), environment.TopLevel
	defspecial("TAGBODY", tagbody)
	defspecial("GO", tagbodyGo)
	defspecial("CATCH", catch)
	defspecial("THROW", throw)
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
			name:      "tagbody & go",
			arguments: arguments{local, global, readFromString("(catch 'foo (tagbody (go bar) (throw 'foo 1) bar))")},
			want:      instance.New(class.Null),
			wantErr:   false,
		},
		{
			name:      "nested tagbody & go",
			arguments: arguments{local, global, readFromString("(catch 'foo (tagbody (tagbody (go bar) (throw 'foo 1) bar (go foobar)) foobar))")},
			want:      instance.New(class.Null),
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
