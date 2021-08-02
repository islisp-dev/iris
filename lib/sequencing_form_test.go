package lib

import "testing"

func TestProgn(t *testing.T) {
	execTests(t, Progn, []test{
		{
			exp:     `(progn)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(progn 1 2)`,
			want:    `2`,
			wantErr: false,
		},
	})
}
