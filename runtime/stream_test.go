package runtime

import "testing"

func TestStreamp(t *testing.T) {
	execTests(t, Streamp, []test{
		{
			exp:     `(streamp (standard-input))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(streamp '())`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestWithOpenIoFile(t *testing.T) {
	execTests(t, WithOpenIoFile, []test{
		{
			exp: `
			(with-open-output-file (outstream "__example.dat")
  				(format outstream "hello"))
			`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp: `
			(with-open-input-file (instream "__example.dat")
  				(read instream))
			`,
			want:    `'hello`,
			wantErr: false,
		},
	})
}

