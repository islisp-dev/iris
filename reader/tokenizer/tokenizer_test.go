// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package tokenizer

import (
	"bufio"
	"reflect"
	"strings"
	"testing"

	"github.com/ta2gch/iris/runtime/ilos"
)

func TestTokenizer_Next(t *testing.T) {
	tokenizer := New(strings.NewReader("(default)"))
	type fields struct {
		sc *bufio.Scanner
	}
	tests := []struct {
		name   string
		fields fields
		want   string
		want1  ilos.Instance
	}{
		{
			name:   "start",
			fields: fields{tokenizer.sc},
			want:   "(",
			want1:  nil,
		},
		{
			name:   "default",
			fields: fields{tokenizer.sc},
			want:   "default",
			want1:  nil,
		},
		{
			name:   "end",
			fields: fields{tokenizer.sc},
			want:   ")",
			want1:  nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tok := &Tokenizer{
				sc: tt.fields.sc,
			}
			got, got1 := tok.Next()
			if got != tt.want {
				t.Errorf("Tokenizer.Next() got = %v, want %v", got, tt.want)
			}
			if !reflect.DeepEqual(got1, tt.want1) {
				t.Errorf("Tokenizer.Next() got1 = %v, want %v", got1, tt.want1)
			}
		})
	}
}
