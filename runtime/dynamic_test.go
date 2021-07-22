package runtime

import "testing"

func TestDynamic(t *testing.T) {
	execTests(t, Dynamic, []test{
		{
			exp: `
			(defun foo (x)
				(dynamic-let ((y x))
				  (bar 1)
				))
			`,
			want:    `'foo`,
			wantErr: false,
		},
		{
			exp: `
				(defun bar (x)
			      (+ x (dynamic y)))
			`,
			want:    `'bar`,
			wantErr: false,
		},
		{
			exp: `
				(foo 2)
			`,
			want:    `3`,
			wantErr: false,
		},
	})
}
