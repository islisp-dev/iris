package main

import (
	"reflect"
	"testing"
)

func Test_parseInteger(t *testing.T) {
	type args struct {
		tok string
	}
	tests := []struct {
		name    string
		args    args
		want    *Object
		wantErr bool
	}{
		{
			name:    "signed",
			args:    args{"-5"},
			want:    &Object{"integer", nil, nil, -5},
			wantErr: false,
		},
		{
			name:    "binary",
			args:    args{"#B00101"},
			want:    &Object{"integer", nil, nil, 5},
			wantErr: false,
		},
		{
			name:    "signed binary",
			args:    args{"#b-00101"},
			want:    &Object{"integer", nil, nil, -5},
			wantErr: false,
		},
		{
			name:    "octal",
			args:    args{"#o00101"},
			want:    &Object{"integer", nil, nil, 65},
			wantErr: false,
		},
		{
			name:    "signed octal",
			args:    args{"#O-00101"},
			want:    &Object{"integer", nil, nil, -65},
			wantErr: false,
		},
		{
			name:    "hexadecimal",
			args:    args{"#X00101"},
			want:    &Object{"integer", nil, nil, 257},
			wantErr: false,
		},
		{
			name:    "signed hexadecimal",
			args:    args{"#x-00101"},
			want:    &Object{"integer", nil, nil, -257},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := parseInteger(tt.args.tok)
			if (err != nil) != tt.wantErr {
				t.Errorf("parseInteger() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("parseInteger() = %v, want %v", got, tt.want)
			}
		})
	}
}
