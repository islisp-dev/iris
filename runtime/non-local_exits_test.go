package runtime

import "testing"

func TestBlock(t *testing.T) {
	tests := []test{
		{
			exp: `(block x (+ 10 (return-from x 6) 22))`,
			want: `6`,
			wantErr: false,
		},
	}
	execTests(t, Block, tests)
}
