package lib

import "testing"

func TestFormat(t *testing.T) {
	execTests(t, Format, []test{
		{
			exp:     `(defglobal str (create-string-output-stream))`,
			want:    `'str`,
			wantErr: false,
		},
		{
			exp:     `(progn (format str "test") (get-output-stream-string str))`,
			want:    `"test"`,
			wantErr: false,
		},
		{
			exp:     `(progn (format str "The result is ~A and nothing else." "meningitis") (get-output-stream-string str))`,
			want:    `"The result is meningitis and nothing else."`,
			wantErr: false,
		},
		{
			exp:     `(progn (format str "The result i~C" #\s) (get-output-stream-string str))`,
			want:    `"The result is"`,
			wantErr: false,
		},
		{
			exp:     `(progn (format str "The results are ~S and ~S" 1 #\a) (get-output-stream-string str))`,
			want:    `"The results are 1 and #\a"`,
			wantErr: false,
		},
		{
			exp:     `(progn (format str "Binary code ~B" 150) (get-output-stream-string str))`,
			want:    `"Binary code 10010110"`,
			wantErr: false,
		},
		{
			exp:     `(progn (format str "permission ~O" 493) (get-output-stream-string str))`,
			want:    `"permission 755"`,
			wantErr: false,
		},
		{
			exp:     `(progn (format str "You ~X ~X" 2989 64206) (get-output-stream-string str))`,
			want:    `"You BAD FACE"`,
			wantErr: false,
		},
		{
			// Implementation defined
			exp:     `(progn (format str "~&Name ~10Tincome ~20Ttax~%") (format str "~A ~10T~D ~20T~D" "Grummy" 23000 7500) (get-output-stream-string str))`,
			want:    `(progn (format str "~%Name      income    tax~%Grummy    23000     7500") (get-output-stream-string str))`,
			wantErr: false,
		},
		{
			exp:     `(progn (format str "This is a tilde: ~~") (get-output-stream-string str))`,
			want:    `"This is a tilde: ~"`,
			wantErr: false,
		},
	})
}
