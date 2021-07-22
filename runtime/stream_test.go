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

func TestCreateStringInputStream(t *testing.T) {
	execTests(t, CreateStringInputStream, []test{
		{
			exp: `
			(let ((str (create-string-input-stream "this is a string")))
				(list (read str) (read str) (read str)))
			`,
			want:    `'(this is a)`,
			wantErr: false,
		},
	})
}

func TestCreateStringOutputStream(t *testing.T) {
	execTests(t, CreateStringOutputStream, []test{
		{
			exp: `
			(let ((str (create-string-output-stream)))
			  (format str "hello")
			  (format str "world")
			  (get-output-stream-string str))
			`,
			want:    `"helloworld"`,
			wantErr: false,
		},
	})
}

func TestGetOutputStreamString(t *testing.T) {
	execTests(t, GetOutputStreamString, []test{
		{
			exp: `
			(let ((out-str (create-string-output-stream)))
			  (format out-str "This is a string")
			  (let ((part1 (get-output-stream-string out-str)))
			    (format out-str "right!")
			    (list part1 (get-output-stream-string out-str))))
			`,
			want:    `'("This is a string" "right!")`,
			wantErr: false,
		},
	})
}

func TestRead(t *testing.T) {
	execTests(t, Read, []test{
		{
			exp:     `(defglobal str (create-string-input-stream "hello #(1 2 3) 123 #\\A"))`,
			want:    `'str`,
			wantErr: false,
		},
		{
			exp:     `(read str)`,
			want:    `'hello`,
			wantErr: false,
		},
		{
			exp:     `(read str)`,
			want:    `#(1 2 3)`,
			wantErr: false,
		},
		{
			exp:     `(read str)`,
			want:    `123`,
			wantErr: false,
		},
		{
			exp:     `(read str)`,
			want:    `#\A`,
			wantErr: false,
		},
	})
}

func TestReadChar(t *testing.T) {
	execTests(t, ReadChar, []test{
		{
			exp:     `(defglobal str (create-string-input-stream "hi"))`,
			want:    `'str`,
			wantErr: false,
		},
		{
			exp:     `(read-char str)`,
			want:    `#\h`,
			wantErr: false,
		},
		{
			exp:     `(read-char str)`,
			want:    `#\i`,
			wantErr: false,
		},
		{
			exp:     `(read str)`,
			want:    `nil`,
			wantErr: true,
		},
	})
}
