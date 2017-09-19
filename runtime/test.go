// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"fmt"
	"reflect"
	"regexp"
	"runtime"
	"testing"
)

type test struct {
	exp     string
	want    string
	wantErr bool
}

func execTests(t *testing.T, function interface{}, tests []test) {
	name := runtime.FuncForPC(reflect.ValueOf(function).Pointer()).Name()
	re := regexp.MustCompile(`\s+`)
	for _, tt := range tests {
		t.Run(re.ReplaceAllString(tt.exp, " "), func(t *testing.T) {
			obj, err1 := readFromString(tt.exp)
			if err1 != nil {
				t.Errorf("ParseError %v, want %v", err1, tt.exp)
				return
			}
			got, err := Eval(TopLevel, obj)
			wantObj, err1 := readFromString(tt.want)
			if err1 != nil {
				t.Errorf("ParseError %v, want %v", err1, tt.want)
				return
			}
			want, _ := Eval(TopLevel, wantObj)
			if !tt.wantErr && !reflect.DeepEqual(got, want) {
				t.Errorf("%v() got = %v, want %v", name, got, want)
			}
			if (err != nil) != tt.wantErr {
				fmt.Println(got, want)
				t.Errorf("%v() err = %v, wantErr %v", name, err, tt.wantErr)
			}
		})
	}
}
