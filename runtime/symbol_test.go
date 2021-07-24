package runtime

import "testing"

func TestSymbol(t *testing.T) {
	execTests(t, Symbolp, []test{
		{
			exp:     `(symbolp 'foo)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(symbolp 1)`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestProperty(t *testing.T) {
	execTests(t, Property, []test{
		{
			exp:     `(property 'x 'one)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(property 'x 'one 1)`,
			want:    `1`,
			wantErr: false,
		},
		{
			exp:     `(setf (property 'x 'one) 1)`,
			want:    `1`,
			wantErr: false,
		},
		{
			exp:     `(property 'x 'one)`,
			want:    `1`,
			wantErr: false,
		},
		{
			exp:     `(remove-property 'x 'one)`,
			want:    `1`,
			wantErr: false,
		},
		{
			exp:     `(remove-property 'x 'one)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(property 'x 'one)`,
			want:    `nil`,
			wantErr: false,
		},
	})
}
