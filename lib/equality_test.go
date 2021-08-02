// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "testing"

func TestEq(t *testing.T) {
	tests := []test{
		{
			exp:     `(eq () ())`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eq '() '())`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eq 'a 'a)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eq 'a 'A)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eq 'a 'b)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eq 'f 'nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eq 2 2)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eq 2 2.0)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eq 100000000 100000000)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eq 10.00000 10.0)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eq (cons 1 2) (cons 1 2))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(let ((x '(a))) (eq x x))`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eq '(a) '(a))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp: `
				(let ((x '(b))
					  (y '(a b)))
					(eq x (cdr y)))
			`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eq '(b) (cdr '(a b)))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp: `
					(let ((p (lambda (x) x)))
						(eq p p))
				`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(let ((x "a")) (eq x x))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(eq "a" "a")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(let ((x "")) (eq x x))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(eq "" "")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eq #\a #\A)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eq #\a #\a)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eq #\space #\Space)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eq #\space #\space)`,
			want:    `T`,
			wantErr: false,
		},
	}
	execTests(t, Eq, tests)
}

func TestEql(t *testing.T) {
	tests := []test{
		{
			exp:     `(eql () ())`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eql '() '())`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eql 'a 'a)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eql 'a 'A)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eql 'a 'b)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eql 'f 'nil)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eql 2 2)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eql 2 2.0)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eql 100000000 100000000)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eql 10.00000 10.0)`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eql (cons 1 2) (cons 1 2))`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(let ((x '(a))) (eql x x))`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eql '(a) '(a))`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp: `
				(let ((x '(b))
					  (y '(a b)))
					(eql x (cdr y)))
			`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp:     `(eql '(b) (cdr '(a b)))`,
			want:    `T`,
			wantErr: false,
		},
		{
			exp: `
					(let ((p (lambda (x) x)))
						(eql p p))
				`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(let ((x "a")) (eql x x))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(eql "a" "a")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(let ((x "")) (eql x x))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(eql "" "")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eql #\a #\A)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(eql #\a #\a)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(eql #\space #\Space)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(eql #\space #\space)`,
			want:    `t`,
			wantErr: false,
		},
	}
	execTests(t, Eql, tests)
}

func TestEqual(t *testing.T) {
	tests := []test{
		{
			exp:     `(equal 'a 'a)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(equal 2 2)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(equal 2 2.0)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(equal '(a) '(a))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(equal '(a (b) c) '(a (b) c))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(equal (cons 1 2) (cons 1 2))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(equal '(a) (list 'a))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(equal "abc" "abc")`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(equal (vector 'a) (vector 'a))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(equal #(a b) #(a b))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(equal #(a b) #(a c))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(equal "a" "A")`,
			want:    `nil`,
			wantErr: false,
		},
	}
	execTests(t, Equal, tests)
}
