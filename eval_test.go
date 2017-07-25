package main

import (
	"reflect"
	"testing"
)

func TestEval(t *testing.T) {
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
			args:    args{&Object{"symbol", nil, nil, "PI"}, &Env{nil, map[string]*Object{"PI": &Object{"float", nil, nil, 3.14}}}, nil},
			want:    &Object{"float", nil, nil, 3.14},
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
