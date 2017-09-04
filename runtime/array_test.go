package runtime

import "testing"

func TestArrayPredicate(t *testing.T) {
	execTests(t, BasicArrayP, []test{
		{
			exp: `
            (mapcar (lambda (x)
                      (list (basic-array-p x)
                            (basic-array*-p x)
                            (general-array*-p x)))
                    '((a b c)
                      "abc"
                      #(a b c)
                      #1a(a b c)
                      #2a((a) (b) (c))))
			`,
			want:    `'((nil nil nil) (t nil nil) (t nil nil) (t nil nil) (t t t))`,
			wantErr: false,
		},
	})
}

func TestCreateArray(t *testing.T) {
	execTests(t, CreateArray, []test{
		{
			exp:     `(create-array '(2 3) 0.0)`,
			want:    `#2a((0.0 0.0 0.0) (0.0 0.0 0.0))`,
			wantErr: false,
		},
		{
			exp:     `(create-array '(2) 0.0)`,
			want:    `#(0.0 0.0)`,
			wantErr: false,
		},
	})
}
