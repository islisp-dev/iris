package main

import (
	"reflect"
	"strings"
	"testing"
)

func read(s string) *Object {
	e, _ := Parse(NewReader(strings.NewReader(s)))
	return e
}

func TestEval(t *testing.T) {
	testEnv := NewEnv()
	testEnv.Fun["INC"] = &NativeFunction{func(args *Object, local *Env, global *Env) (*Object, error) {
		return &Object{"integer", nil, nil, args.Car.Val.(int) + 1}, nil
	}}
	type args struct {
		obj    *Object
		local  *Env
		global *Env
	}
	tests := []struct {
		name    string
		args    args
		want    *Object
		wantErr bool
	}{
		{
			name:    "local variable",
			args:    args{read("PI"), &Env{nil, nil, nil, map[string]*Object{"PI": read("3.14")}}, nil},
			want:    &Object{"float", nil, nil, 3.14},
			wantErr: false,
		},
		{
			name:    "function call",
			args:    args{read("(inc (inc 1))"), NewEnv(), testEnv},
			want:    &Object{"integer", nil, nil, 3},
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
