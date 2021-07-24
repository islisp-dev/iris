// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package tokenizer

import (
	"strings"
	"testing"

	"github.com/islisp-dev/iris/runtime/ilos"
)

func TestTokenizer_Next(t *testing.T) {
	tokenizer := NewReader(strings.NewReader(`
	;foo
	("\\""\"foo\"" | foo \| bar | #b101)`))
	tests := []struct {
		name  string
		want  string
		want1 ilos.Instance
	}{
		{
			name: "comment",
			want: ";foo\n",
		},
		{
			name: "start",
			want: "(",
		},
		{
			name: "back slash",
			want: `"\\"`,
		},
		{
			name: `"\"foo\""`,
			want: `"\"foo\""`,
		},
		{
			name: `| foo \| bar |`,
			want: `| foo \| bar |`,
		},
		{
			name: `Binary`,
			want: `#b101`,
		},
		{
			name: "end",
			want: ")",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, _ := tokenizer.Next()
			if got.Str != tt.want {
				t.Errorf("Tokenizer.Next() got = %v, want %v", got, tt.want)
			}
		})
	}
}
