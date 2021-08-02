package lib

import "testing"

func TestReadByte(t *testing.T) {
	execTests(t, ReadByte, []test{
		{
			exp:     `(defglobal byte-example (open-output-file "__binary" 8))`,
			want:    `'byte-example`,
			wantErr: false,
		},
		{
			exp:     `(format byte-example "hello")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(close byte-example)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(progn (setq byte-example (open-input-file "__binary" 8)) nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(read-byte byte-example)`,
			want:    `104`,
			wantErr: false,
		},
		{
			exp:     `(read-byte byte-example)`,
			want:    `101`,
			wantErr: false,
		},
		{
			exp:     `(read-byte byte-example)`,
			want:    `108`,
			wantErr: false,
		},
		{
			exp:     `(read-byte byte-example)`,
			want:    `108`,
			wantErr: false,
		},
		{
			exp:     `(read-byte byte-example)`,
			want:    `111`,
			wantErr: false,
		},
	})
}

func TestWriteByte(t *testing.T) {
	execTests(t, WriteByte, []test{
		{
			exp: `
		 (let ((out-str (open-output-file "__binary" 8)))
  			(write-byte #b101 out-str)
  			(close out-str)) 
		 `,
			want:    `nil`,
			wantErr: false,
		},
	})
}
