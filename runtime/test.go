// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"os"
	"reflect"
	"regexp"
	"runtime"
	"testing"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

type test struct {
	exp     string
	want    string
	wantErr bool
}

func execTests(t *testing.T, function interface{}, tests []test) {
	name := runtime.FuncForPC(reflect.ValueOf(function).Pointer()).Name()
	name = regexp.MustCompile(`.*\.`).ReplaceAllString(name, "")
	local := environment.NewEnvironment(instance.NewStream(os.Stdin, nil), instance.NewStream(nil, os.Stdout), instance.NewStream(nil, os.Stderr))
	global := TopLevel
	for _, tt := range tests {
		t.Run(tt.exp, func(t *testing.T) {
			got, err := Eval(local, global, readFromString(tt.exp))
			want, _ := Eval(local, global, readFromString(tt.want))
			if !tt.wantErr && !reflect.DeepEqual(got, want) {
				t.Errorf("%v() got = %v, want %v", name, got, want)
			}
			if (err != nil) != tt.wantErr {
				t.Errorf("%v() err = %v, wantErr %v", name, err, tt.wantErr)
			}
		})
	}
}
