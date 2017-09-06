// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
package runtime

import "testing"

func TestLength(t *testing.T) {
	execTests(t, Length, []test{
		{
			exp:     `(length '(a b c))`,
			want:    `3`,
			wantErr: false,
		},
		{
			exp:     `(length '(a (b) (c d e)))`,
			want:    `3`,
			wantErr: false,
		},
		{
			exp:     `(length '())`,
			want:    `0`,
			wantErr: false,
		},
		{
			exp:     `(length (vector 'a 'b 'c))`,
			want:    `3`,
			wantErr: false,
		},
	})
}

func TestElt(t *testing.T) {
	execTests(t, Elt, []test{
		{
			exp:     `(elt '(a b c) 2)`,
			want:    `'c`,
			wantErr: false,
		},
		{
			exp:     `(elt (vector 'a 'b 'c) 1)`,
			want:    `'b`,
			wantErr: false,
		},
		{
			exp:     `(elt "abc" 0)`,
			want:    `#\a`,
			wantErr: false,
		},
	})
}

func TestSetElt(t *testing.T) {
	execTests(t, SetElt, []test{
		{
			exp: `
			(let ((string (create-string 5 #\x)))
				(setf (elt string 2) #\O)
				string)
			`,
			want:    `"xxOxx"`,
			wantErr: false,
		},
		{
			exp: `
			(let ((string (create-list 5 'x)))
				(setf (elt string 2) 'O)
				string)
			`,
			want:    `'(x x O x x)`,
			wantErr: false,
		},
	})
}

func TestSubseq(t *testing.T) {
	execTests(t, Subseq, []test{
		{
			exp:     `(subseq "abcdef" 1 4)`,
			want:    `"bcd"`,
			wantErr: false,
		},
		{
			exp:     `(subseq '(a b c d e f) 1 4)`,
			want:    `'(b c d)`,
			wantErr: false,
		},
		{
			exp:     `(subseq (vector 'a 'b 'c 'd 'e 'f) 1 4)`,
			want:    `#(b c d)`,
			wantErr: false,
		},
	})
}

func TestMapInto(t *testing.T) {
	execTests(t, MapInto, []test{
		{
			exp:     `(defglobal a nil)`,
			want:    `'a`,
			wantErr: false,
		},
		{
			exp:     `(defglobal b nil)`,
			want:    `'b`,
			wantErr: false,
		},
		{
			exp:     `(defglobal k nil)`,
			want:    `'k`,
			wantErr: false,
		},
		{
			exp:     `(setq a (list 1 2 3 4))`,
			want:    `'(1 2 3 4)`,
			wantErr: false,
		},
		{
			exp:     `(setq a (list 1 2 3 4))`,
			want:    `'(1 2 3 4)`,
			wantErr: false,
		},
		{
			exp:     `(setq b (list 10 10 10 10))`,
			want:    `'(10 10 10 10)`,
			wantErr: false,
		},
		{
			exp:     `(map-into a #'+ a b)`,
			want:    `'(11 12 13 14)`,
			wantErr: false,
		},
		{
			exp:     `a`,
			want:    `'(11 12 13 14)`,
			wantErr: false,
		},
		{
			exp:     `b`,
			want:    `'(10 10 10 10)`,
			wantErr: false,
		},
		{
			exp:     `(setq k '(one two three))`,
			want:    `'(one two three)`,
			wantErr: false,
		},
		{
			exp:     `(map-into a #'cons k a)`,
			want:    `'((one . 11) (two . 12) (three . 13) 14)`,
			wantErr: false,
		},
		{
			exp: `
			(let ((x 0))
			  (map-into a
			    (lambda () (setq x (+ x 2)))))
			`,
			want:    `'(2 4 6 8)`,
			wantErr: false,
		},
		{
			exp:     `a`,
			want:    `'(2 4 6 8)`,
			wantErr: false,
		},
	})
}
