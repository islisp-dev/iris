package core

import (
	"reflect"
	"strings"
	"testing"

	"github.com/ta2gch/gazelle/reader/parser"
	"github.com/ta2gch/gazelle/reader/tokenizer"
	"github.com/ta2gch/gazelle/core/class"
)

func read(s string) *class.Instance {
	e, _ := parser.Parse(tokenizer.NewTokenReader(strings.NewReader(s)))
	return e
}

func TestEval(t *testing.T) {
	type args struct {
		obj    *class.Instance
		local  *Env
		global *Env
	}
	tests := []struct {
		name    string
		args    args
		want    *class.Instance
		wantErr bool
	}{
		{
			name:    "local variable",
			args:    args{read("PI"), &Env{nil, map[string]*class.Instance{"PI": read("3.14")}}, nil},
			want:    &class.Instance{class.Float, 3.14},
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
