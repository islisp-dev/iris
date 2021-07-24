// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package parser

import (
	"reflect"
	"testing"

	"github.com/islisp-dev/iris/reader/tokenizer"
	"github.com/islisp-dev/iris/runtime/ilos"
)

func Test_parseAtom(t *testing.T) {
	type arguments struct {
		tok string
	}
	tests := []struct {
		name      string
		arguments arguments
		want      ilos.Instance
		wantErr   bool
	}{
		//
		// Float
		//
		{
			name:      "default",
			arguments: arguments{"3.14"},
			want:      ilos.NewFloat(3.14),
			wantErr:   false,
		},
		{
			name:      "signed",
			arguments: arguments{"-5.0"},
			want:      ilos.NewFloat(-5.0),
			wantErr:   false,
		},
		{
			name:      "exponential",
			arguments: arguments{"-5.0E3"},
			want:      ilos.NewFloat(-5.0 * 1000),
			wantErr:   false,
		},
		{
			name:      "signed exponential",
			arguments: arguments{"5.0E-3"},
			want:      ilos.NewFloat(5.0 * 1.0 / 1000.0),
			wantErr:   false,
		},
		{
			name:      "without point",
			arguments: arguments{"5E-3"},
			want:      ilos.NewFloat(5.0 * 1.0 / 1000.0),
			wantErr:   false,
		},
		{
			name:      "invalid case",
			arguments: arguments{"3E-3.0"},
			want:      nil,
			wantErr:   true,
		},
		{
			name:      "without point",
			arguments: arguments{"5E-"},
			want:      nil,
			wantErr:   true,
		},
		//
		// Integer
		//
		{
			name:      "default",
			arguments: arguments{"5"},
			want:      ilos.NewInteger(5),
			wantErr:   false,
		},
		{
			name:      "signed",
			arguments: arguments{"-5"},
			want:      ilos.NewInteger(-5),
			wantErr:   false,
		},
		{
			name:      "binary",
			arguments: arguments{"#B00101"},
			want:      ilos.NewInteger(5),
			wantErr:   false,
		},
		{
			name:      "signed binary",
			arguments: arguments{"#b+00101"},
			want:      ilos.NewInteger(5),
			wantErr:   false,
		},
		{
			name:      "octal",
			arguments: arguments{"#o00101"},
			want:      ilos.NewInteger(65),
			wantErr:   false,
		},
		{
			name:      "signed octal",
			arguments: arguments{"#O-00101"},
			want:      ilos.NewInteger(-65),
			wantErr:   false,
		},
		{
			name:      "hexadecimal",
			arguments: arguments{"#X00101"},
			want:      ilos.NewInteger(257),
			wantErr:   false,
		},
		{
			name:      "signed hexadecimal",
			arguments: arguments{"#x-00101"},
			want:      ilos.NewInteger(-257),
			wantErr:   false,
		},
		{
			name:      "invalid binary",
			arguments: arguments{"-#x00101"},
			want:      nil,
			wantErr:   true,
		},
		//
		// Character
		//
		{
			name:      "default",
			arguments: arguments{"#\\a"},
			want:      ilos.NewCharacter('a'),
			wantErr:   false,
		},
		{
			name:      "newline",
			arguments: arguments{"#\\newline"},
			want:      ilos.NewCharacter('\n'),
			wantErr:   false,
		},
		{
			name:      "space",
			arguments: arguments{"#\\space"},
			want:      ilos.NewCharacter(' '),
			wantErr:   false,
		},
		{
			name:      "invalid character name",
			arguments: arguments{"#\\foo"},
			want:      nil,
			wantErr:   true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseAtom(tokenizer.NewToken(tt.arguments.tok, -1, -1))
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
