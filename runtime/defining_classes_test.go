// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import "testing"

func TestDefclass(t *testing.T) {
	defspecial(Defclass)
	defspecial(Defgeneric)
	defspecial(Defmethod)
	defspecial(Quote)
	defun(Sqrt)
	defun2("+", Add)
	defun2("-", Substruct)
	defun2("*", Multiply)
	defspecial(Let)
	defun(List)
	defspecial(defun)
	defun(Create)
	defglobal("NIL", Nil)
	defspecial(Class)
	tests := []test{
		{
			exp:     `(defclass Foo () (a))`,
			want:    `'foo`,
			wantErr: false,
		},
		{
			exp:     `(defclass Bar1 (Foo) (b1))`,
			want:    `'Bar1`,
			wantErr: false,
		},
		{
			exp:     `(defclass Bar2 (Foo Bar1) (b1))`,
			want:    `'Bar2`,
			wantErr: true,
		},
		{
			exp: `
			(defclass <point> ()
				((x :accessor point-x :initform 0.0 :initarg x)
				 (y :accessor point-y :initform 0.0 :initarg y)))			
			`,
			want:    `'<point>`,
			wantErr: false,
		},
		{
			exp: `
			(defclass <point3d> ()
				((x :accessor point-x :initform 0.0 :initarg x)
				 (y :accessor point-y :initform 0.0 :initarg y)
				 (z :accessor point-z :initform 0.0 :initarg z)))
			`,
			want:    `'<point3d>`,
			wantErr: false,
		},
		{
			exp: `
			(defgeneric distance (p1 p2))
			`,
			want:    `'distance`,
			wantErr: false,
		},
		{
			exp: `
			(defmethod distance ((p1 <point>) (p2 <point>))
				(let ((dx (- (point-x p1) (point-x p2)))
					  (dy (- (point-y p1) (point-y p2))))
				  (sqrt (+ (* dx dx) (* dy dy)))))
			`,
			want:    `'distance`,
			wantErr: false,
		},
		{
			exp:     `(distance (create (class <point>) 'x 100) (create (class <point>)  'y 100))`,
			want:    `(sqrt 20000)`,
			wantErr: false,
		},
	}
	execTests(t, Defclass, tests)
}
