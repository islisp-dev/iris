// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package tokenizer

import (
	"strings"
	"testing"

	"github.com/islisp-dev/iris/util"
)

func TestTokenizer_Next(t *testing.T) {
	tokenizer := NewBufferedTokenReader(util.StringReadCloser{strings.NewReader(`
	#|bar
|#
	;foo
	("\\""\"foo\"" | foo \| bar | #b101)`)})
	tests := []struct {
		name string
		want string
	}{
		{
			name: "comment",
			want: `#|bar
|#`,
		},
		{
			name: "comment",
			want: ";foo",
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
			got, _ := tokenizer.ReadToken()
			if got.Str != tt.want {
				t.Errorf("Tokenizer.Next() got = %v, want %v", got, tt.want)
			}
		})
	}
}
