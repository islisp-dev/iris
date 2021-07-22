package runtime

import "testing"

func TestIdentity(t *testing.T) {
	execTests(t, Identity, []test{
		{
			exp:     `(identity 1)`,
			want:    `1`,
			wantErr: false,
		},
	})
}
