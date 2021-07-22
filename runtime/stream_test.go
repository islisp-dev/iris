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

func TestInputStreamP(t *testing.T) {
	execTests(t, InputStreamP, []test{
		{
			exp:     `(input-stream-p (standard-input))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(input-stream-p (standard-output))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(streamp '(a b c))`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestOutputStreamP(t *testing.T) {
	execTests(t, OutputStreamP, []test{
		{
			exp:     `(output-stream-p (standard-input))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(output-stream-p (standard-output))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(streamp "hello")`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestWithStandardInput(t *testing.T) {
	execTests(t, WithStandardInput, []test{
		{
			exp: `
			(with-standard-input (create-string-input-stream "this is a string")
 				(list (read) (read)))
			`,
			want:    `'(this is)`,
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

