package lib

import "testing"

func TestStringp(t *testing.T) {
	execTests(t, Stringp, []test{
		{
			exp:     `(stringp "abc")`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(stringp 'abc)`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestCreateString(t *testing.T) {
	execTests(t, CreateString, []test{
		{
			exp:     `(create-string 3 #\a)`,
			want:    `"aaa"`,
			wantErr: false,
		},
		{
			exp:     `(create-string 0 #\a)`,
			want:    `""`,
			wantErr: false,
		},
	})
}

func TestStringEqual(t *testing.T) {
	execTests(t, StringEqual, []test{
		{
			exp:     `(if (string= "abcd" "abcd") t nil)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(if (string= "abcd" "wxyz") t nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(if (string= "abcd" "abcde") t nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(if (string= "abcde" "abcd") t nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(if (string/= "abcde" "abcd") t nil)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(if (string< "abcd" "abcd") t nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(if (string< "abcd" "wxyz") t nil)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(if (string< "abcd" "abcde") t nil)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(if (string< "abcde" "abcd") t nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(if (string<= "abcd" "abcd") t nil)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(if (string<= "abcd" "wxyz") t nil)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(if (string<= "abcd" "abcde") t nil)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(if (string<= "abcde" "abcd") t nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(if (string> "abcd" "wxyz") t nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(if (string>= "abcd" "abcd") t nil)`,
			want:    `t`,
			wantErr: false,
		},
	})
}

func TestCharIndex(t *testing.T) {
	execTests(t, CharIndex, []test{
		{
			exp:     `(char-index #\b "abcab")`,
			want:    `1`,
			wantErr: false,
		},
		{
			exp:     `(char-index #\B "abcab")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(char-index #\b "abcab" 2)`,
			want:    `4`,
			wantErr: false,
		},
		{
			exp:     `(char-index #\d "abcab")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(char-index #\a "abcab" 4)`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestStringIndex(t *testing.T) {
	execTests(t, StringIndex, []test{
		{
			exp:     `(string-index "foo" "foobar")`,
			want:    `0`,
			wantErr: false,
		},
		{
			exp:     `(string-index "bar" "foobar")`,
			want:    `3`,
			wantErr: false,
		},
		{
			exp:     `(string-index "FOO" "foobar")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(string-index "foo" "foobar" 1)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(string-index "bar" "foobar" 1)`,
			want:    `3`,
			wantErr: false,
		},
		{
			exp:     `(string-index "foo" "")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(string-index "" "foo")`,
			want:    `0`,
			wantErr: false,
		},
	})
}

func TestStringAppend(t *testing.T) {
	execTests(t, StringAppend, []test{
		{
			exp:     `(string-append "abc" "def")`,
			want:    `"abcdef"`,
			wantErr: false,
		},
		{
			exp:     `(string-append "abc" "abc")`,
			want:    `"abcabc"`,
			wantErr: false,
		},
		{
			exp:     `(string-append "abc" "")`,
			want:    `"abc"`,
			wantErr: false,
		},
		{
			exp:     `(string-append "" "abc")`,
			want:    `"abc"`,
			wantErr: false,
		},
		{
			exp:     `(string-append "abc" "" "def")`,
			want:    `"abcdef"`,
			wantErr: false,
		},
	})
}
