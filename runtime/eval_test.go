// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"reflect"
	"testing"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func TestEval(t *testing.T) {
	local := environment.New()
	global := environment.TopLevel
	defun("INC", func(local, global *environment.Environment, arg ilos.Instance) (ilos.Instance, ilos.Instance) {
		return instance.New(class.Integer, int(arg.(instance.Integer))+1), nil
	})
	defglobal("*PI*", Pi)
	defmacro("MINC", func(local, global *environment.Environment, arg ilos.Instance) (ilos.Instance, ilos.Instance) {
		return instance.New(class.Cons, instance.New(class.Symbol, "INC"), instance.New(class.Cons, arg, Nil)), nil
	})
	type arguments struct {
		obj    ilos.Instance
		local  *environment.Environment
		global *environment.Environment
	}
	tests := []struct {
		name      string
		arguments arguments
		want      ilos.Instance
		wantErr   bool
	}{
		{
			name:      "local variable",
			arguments: arguments{readFromString("*pi*"), local, global},
			want:      Pi,
			wantErr:   false,
		},
		{
			name:      "local function",
			arguments: arguments{readFromString("(inc (inc 1))"), local, global},
			want:      instance.New(class.Integer, 3),
			wantErr:   false,
		},
		{
			name:      "local macro",
			arguments: arguments{readFromString("(minc (minc 1))"), local, global},
			want:      instance.New(class.Integer, 3),
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
			}
		})
	}
}
