package parser

import (
	"reflect"
	"testing"

	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func Test_parseAtom(t *testing.T) {
	type args struct {
		tok string
	}
	tests := []struct {
		name    string
		args    args
		want    ilos.Instance
		wantErr bool
	}{
		//
		// Float
		//
		{
			name:    "default",
			args:    args{"3.14"},
			want:    instance.NewFloat(3.14),
			wantErr: false,
		},
		{
			name:    "signed",
			args:    args{"-5.0"},
			want:    instance.NewFloat(-5.0),
			wantErr: false,
		},
		{
			name:    "exponential",
			args:    args{"-5.0E3"},
			want:    instance.NewFloat(-5.0 * 1000),
			wantErr: false,
		},
		{
			name:    "signed exponential",
			args:    args{"5.0E-3"},
			want:    instance.NewFloat(5.0 * 1.0 / 1000.0),
			wantErr: false,
		},
		{
			name:    "without point",
			args:    args{"5E-3"},
			want:    instance.NewFloat(5.0 * 1.0 / 1000.0),
			wantErr: false,
		},
		{
			name:    "invalid case",
			args:    args{"3E-3.0"},
			want:    nil,
			wantErr: true,
		},
		{
			name:    "without point",
			args:    args{"5E-"},
			want:    nil,
			wantErr: true,
		},
		//
		// Integer
		//
		{
			name:    "default",
			args:    args{"5"},
			want:    instance.NewInteger(5),
			wantErr: false,
		},
		{
			name:    "signed",
			args:    args{"-5"},
			want:    instance.NewInteger(-5),
			wantErr: false,
		},
		{
			name:    "binary",
			args:    args{"#B00101"},
			want:    instance.NewInteger(5),
			wantErr: false,
		},
		{
			name:    "signed binary",
			args:    args{"#b+00101"},
			want:    instance.NewInteger(5),
			wantErr: false,
		},
		{
			name:    "octal",
			args:    args{"#o00101"},
			want:    instance.NewInteger(65),
			wantErr: false,
		},
		{
			name:    "signed octal",
			args:    args{"#O-00101"},
			want:    instance.NewInteger(-65),
			wantErr: false,
		},
		{
			name:    "hexadecimal",
			args:    args{"#X00101"},
			want:    instance.NewInteger(257),
			wantErr: false,
		},
		{
			name:    "signed hexadecimal",
			args:    args{"#x-00101"},
			want:    instance.NewInteger(-257),
			wantErr: false,
		},
		{
			name:    "invalid binary",
			args:    args{"-#x00101"},
			want:    nil,
			wantErr: true,
		},
		//
		// Character
		//
		{
			name:    "default",
			args:    args{"#\\a"},
			want:    instance.NewCharacter('a'),
			wantErr: false,
		},
		{
			name:    "newline",
			args:    args{"#\\newline"},
			want:    instance.NewCharacter('\n'),
			wantErr: false,
		},
		{
			name:    "space",
			args:    args{"#\\space"},
			want:    instance.NewCharacter(' '),
			wantErr: false,
		},
		{
			name:    "invalid character name",
			args:    args{"#\\foo"},
			want:    nil,
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := parseAtom(tt.args.tok)
			if (err != nil) != tt.wantErr {
				t.Errorf("parseAtom() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("parseAtom() = %v, want %v", got, tt.want)
			}
		})
	}
}
