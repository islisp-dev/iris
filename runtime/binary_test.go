package runtime

import "testing"

func TestReadByte(t *testing.T) {
	execTests(t, ReadByte, []test{
		{
			exp:     `(defglobal byte-example (open-output-file "__binary"))`,
			want:    `'byte-example`,
			wantErr: false,
		},
	})
}
