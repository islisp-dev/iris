package lib

import "testing"

func TestArrayPredicate(t *testing.T) {
	execTests(t, BasicArrayP, []test{
		{
			exp: `
            (mapcar (lambda (x)
                      (list (basic-array-p x)
                            (basic-array*-p x)
                            (general-array*-p x)))
                    '((a b c)
                      "abc"
                      #(a b c)
                      #1a(a b c)
                      #2a((a) (b) (c))))
			`,
			want:    `'((nil nil nil) (t nil nil) (t nil nil) (t nil nil) (t t t))`,
			wantErr: false,
		},
	})
}

func TestCreateArray(t *testing.T) {
	execTests(t, CreateArray, []test{
		{
			exp:     `(create-array '(2 3) 0.0)`,
			want:    `#2a((0.0 0.0 0.0) (0.0 0.0 0.0))`,
			wantErr: false,
		},
		{
			exp:     `(create-array '(2) 0.0)`,
			want:    `#(0.0 0.0)`,
			wantErr: false,
		},
	})
}

func TestAref(t *testing.T) {
	execTests(t, CreateArray, []test{
		{
			exp:     `(defglobal array1 (create-array '(3 3 3) 0))`,
			want:    `'array1`,
			wantErr: false,
		},
		{
			exp: `array1`,
			want: `
			#3a(((0 0 0) (0 0 0) (0 0 0))
			    ((0 0 0) (0 0 0) (0 0 0))
			    ((0 0 0) (0 0 0) (0 0 0)))
			`,
			wantErr: false,
		},
		{
			exp:     `(aref array1 0 1 2)`,
			want:    `0`,
			wantErr: false,
		},
		{
			exp:     `(setf (aref array1 0 1 2) 3.14)`,
			want:    `3.14`,
			wantErr: false,
		},
		{
			exp:     `(aref array1 0 1 2)`,
			want:    `3.14`,
			wantErr: false,
		},
		{
			exp:     `(aref (create-array '(8 8) 6) 1 1)`,
			want:    `6`,
			wantErr: false,
		},
		{
			exp:     `(aref (create-array '() 19))`,
			want:    `19`,
			wantErr: false,
		},
	})
}

func TestSetAref(t *testing.T) {
	execTests(t, CreateArray, []test{
		{
			exp:     `(setf (aref array1 0 1 2) 3.15)`,
			want:    `3.15`,
			wantErr: false,
		},
		{
			exp:     `(set-aref 51.3 array1 0 1 2)`,
			want:    `51.3`,
			wantErr: false,
		},
	})
}

func TestArrayDimensions(t *testing.T) {
	execTests(t, CreateArray, []test{
		{
			exp: `
			(array-dimensions
			  (create-array '(2 2) 0))
			`,
			want:    `'(2 2)`,
			wantErr: false,
		},
		{
			exp:     `(array-dimensions (vector 'a 'b))`,
			want:    `'(2)`,
			wantErr: false,
		},
		{
			exp:     `(array-dimensions "foo")`,
			want:    `'(3)`,
			wantErr: false,
		},
	})
}
