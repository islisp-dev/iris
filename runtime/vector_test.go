package runtime

import "testing"

func TestBasicVectorP(t *testing.T) {
	execTests(t, BasicVectorP, []test{
		{
			exp: `
			(mapcar (lambda (x)
			          (list (basic-vector-p x)
			                (general-vector-p x)))
			        '((a b c)
			          "abc"
			          #(a b c)
			          #1a(a b c)
			          #2a((a) (b) (c))))
			`,
			want:    `'((nil nil) (t nil) (t t) (t t) (nil nil))`,
			wantErr: false,
		},
	})
}

func TestCreateVector(t *testing.T) {
	execTests(t, CreateVector, []test{
		{
			exp:     `(create-vector 3 17)`,
			want:    `#(17 17 17)`,
			wantErr: false,
		},
		{
			exp:     `(create-vector 2 #\a)`,
			want:    `#(#\a #\a)`,
			wantErr: false,
		},
	})
}

func TestVector(t *testing.T) {
	execTests(t, Vector, []test{
		{
			exp:     `(vector 'a 'b 'c)`,
			want:    `#(a b c)`,
			wantErr: false,
		},
		{
			exp:     `(vector)`,
			want:    `#()`,
			wantErr: false,
		},
	})
}
