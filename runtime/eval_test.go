package runtime

import (
	"reflect"
	"strings"
	"testing"

	"github.com/ta2gch/gazelle/reader/parser"
	"github.com/ta2gch/gazelle/reader/tokenizer"
	"github.com/ta2gch/gazelle/runtime/class"
	"github.com/ta2gch/gazelle/runtime/object"
)

func read(s string) *object.Object {
	e, _ := parser.Parse(tokenizer.NewTokenReader(strings.NewReader(s)))
	return e
}

func TestEval(t *testing.T) {
	type args struct {
		obj    *object.Object
		local  *Env
		global *Env
	}
	tests := []struct {
		name    string
		args    args
		want    *object.Object
		wantErr bool
	}{
		{
			name:    "local variable",
			args:    args{read("PI"), &Env{nil, map[string]*object.Object{"PI": read("3.14")}}, nil},
			want:    &object.Object{class.Float, 3.14},
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
