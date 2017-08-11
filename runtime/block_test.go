// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
			name:      "case1",
			arguments: arguments{local, global, readFromString("(block 'foo 1 (return-from 'foo 1))")},
			want:      readFromString("1"),
			wantErr:   false,
		},
		{
			name:      "case2",
			arguments: arguments{local, global, readFromString("(block)")},
			want:      nil,
			wantErr:   true,
		},
		{
			name:      "case3",
			arguments: arguments{local, global, readFromString("(block 'foo)")},
			want:      Nil,
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
