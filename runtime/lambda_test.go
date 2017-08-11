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

func TestLambda(t *testing.T) {
	local, global := environment.New(), environment.TopLevel
	defspecial("LAMBDA", lambda)
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
			arguments: arguments{local, global, readFromString("((lambda (x)) 1)")},
			want:      instance.New(class.Null),
			wantErr:   false,
		},
		{
			name:      "case2",
			arguments: arguments{local, global, readFromString("((lambda (:rest xs) xs) 1 2)")},
			want:      readFromString("(1 2)"),
			wantErr:   false,
		},
		{
			name:      "case3",
			arguments: arguments{local, global, readFromString("((lambda (:rest xs) xs))")},
			want:      readFromString("nil"),
			wantErr:   false,
		},
		{
			name:      "case4",
			arguments: arguments{local, global, readFromString("((lambda (x) x) 1 2)")},
			want:      nil,
			wantErr:   true,
		},
		{
			name:      "case5",
			arguments: arguments{local, global, readFromString("((lambda (x :rest xs) x))")},
			want:      nil,
			wantErr:   true,
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
