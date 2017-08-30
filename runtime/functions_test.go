// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"testing"
)

func TestFunctionp(t *testing.T) {
	tests := []test{
		{
			exp:     `(functionp (function car))`,
			want:    `T`,
			wantErr: false,
		},
	}
	execTests(t, Functionp, tests)
}

func TestFunction(t *testing.T) {
	tests := []test{
		{
			exp:     `(funcall (function -) 3)`,
			want:    `-3`,
			wantErr: false,
		},
		{
			exp:     `(apply #'- '(4 3))`,
			want:    `1`,
			wantErr: false,
		},
	}
	execTests(t, Function, tests)
}

func TestLambda(t *testing.T) {
	tests := []test{
		{
			exp:     `((lambda (x y) (+ (* x x) (* y y))) 3 4)`,
			want:    `25`,
			wantErr: false,
		},
		{
			exp:     `((lambda (x y &rest z) z) 3 4 5 6)`,
			want:    `'(5 6)`,
			wantErr: false,
		},
		{
			exp:     `((lambda (x y :rest z) z) 3 4 5 6)`,
			want:    `'(5 6)`,
			wantErr: false,
		},
		{
			exp:     `(funcall (lambda (x y) (- y (* x y))) 7 3)`,
			want:    `-18`,
			wantErr: false,
		},
	}
	execTests(t, Lambda, tests)
}

func TestLabels(t *testing.T) {
	tests := []test{
		{
			exp: `(labels ((evenp (n)
                              (if (= n 0)
                                  t
                                  (oddp (- n 1))))
                           (oddp (n)
                              (if (= n 0)
                                  nil
                                  (evenp (- n 1)))))
                      (evenp 88))`,
			want:    `T`,
			wantErr: false,
		},
	}
	execTests(t, Labels, tests)
}

func TestFlet(t *testing.T) {
	tests := []test{
		{
			exp:     `(defun f (x) 0)`,
			want:    `'f`,
			wantErr: false,
		},
		{
			exp: `
				(flet ((f (x) (+ x 3)))
					(flet ((f (x) (+ x (f x))))
						(f 7))) 
			`,
			want:    `17`,
			wantErr: false,
		},
	}
	execTests(t, Flet, tests)
}

func TestApply(t *testing.T) {
	tests := []test{
		{
			exp: `
				(apply (if (< 1 2) (function max) (function min))
					   1 2 (list 3 4))
				`,
			want:    `4`,
			wantErr: false,
		},
		{
			exp: `			
				(defun compose (f g)
					(lambda (:rest args)
						(funcall f (apply g args)))))			
	            `,
			want:    `'compose`,
			wantErr: false,
		},
		{
			exp:     `(funcall (compose (function sqrt) (function *)) 12 75)`,
			want:    `30`,
			wantErr: false,
		},
	}
	execTests(t, Apply, tests)
}

func TestFuncall(t *testing.T) {
	tests := []test{
		{
			exp: `
				(let ((x '(1 2 3)))
					(funcall (cond ((listp x) (function car))
								   (t (lambda (x) (cons x 1))))
							 x))	 
			`,
			want:    `1`,
			wantErr: false,
		},
	}
	execTests(t, Funcall, tests)
}
