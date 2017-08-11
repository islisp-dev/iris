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
			want:      Nil,
			wantErr:   false,
		},
		{
			name:      "nested tagbody & go",
			arguments: arguments{local, global, readFromString("(catch 'foo (tagbody (tagbody (go bar) (throw 'foo 1) bar (go foobar)) foobar))")},
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
