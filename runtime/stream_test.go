package runtime

import "testing"


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

