// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import "testing"

func TestDefconstant(t *testing.T) {
	defspecial(Defconstant)
	defspecial(Quote)
	defspecial(Defun)
	tests := []test{
		{
			exp:     `(defconstant e 2.7182818284590451)`,
			want:    `'e`,
			wantErr: false,
		},
		{
			exp:     `e`,
			want:    `2.7182818284590451`,
			wantErr: false,
		},
		{
			exp:     `(defun f () e)`,
			want:    `'f`,
			wantErr: false,
		},
		{
			exp:     `(f)`,
			want:    `2.7182818284590451`,
			wantErr: false,
		},
	}
	execTests(t, Defconstant, tests)
}

func TestDefglobal(t *testing.T) {
	defspecial(Defglobal)
	defspecial(Quote)
	defspecial(Defun)
	defspecial(Let)
	tests := []test{
		{
			exp:     `(defglobal today 'wednesday)`,
			want:    `'today`,
			wantErr: false,
		},
		{
			exp:     `today`,
			want:    `'wednesday'`,
			wantErr: false,
		},
		{
			exp:     `(defun what-is-today () today)`,
			want:    `'what-is-today`,
			wantErr: false,
		},
		{
			exp:     `(what-is-today)`,
			want:    `'wednesday`,
			wantErr: false,
		},
		{
			exp: `
			(let ((what-is-today 'tuesday))
			   (what-is-today))
			`,
			want:    `'wednesday`,
			wantErr: false,
		},
		{
			exp:     `(let ((today 'thursday)) (what-is-today))`,
			want:    `'wednesday`,
			wantErr: false,
		},
	}
	execTests(t, Defglobal, tests)
}
func TestDefdynamic(t *testing.T) {
	defspecial(Defdynamic)
	defspecial(Quote)
	defspecial(Defun)
	defspecial(Dynamic)
	defspecial(DynamicLet)
	tests := []test{
		{
			exp:     `(defdynamic *color* 'red)`,
			want:    `'*color*`,
			wantErr: false,
		},
		{
			exp:     `(dynamic *color*)`,
			want:    `'red`,
			wantErr: false,
		},
		{
			exp:     `(defun what-color () (dynamic *color*))`,
			want:    `'what-color`,
			wantErr: false,
		},
		{
			exp:     `(what-color)`,
			want:    `'red'`,
			wantErr: false,
		},
		{
			exp:     `(dynamic-let ((*color* 'green)) (what-color))`,
			want:    `'green'`,
			wantErr: false,
		},
	}
	execTests(t, Defconstant, tests)
}

func TestDefun(t *testing.T) {
	defspecial(Defconstant)
	defspecial(Quote)
	defspecial(Defun)
	tests := []test{
		{
			exp:     `(defun caar (x) (car (car x)))`,
			want:    `'caar`,
			wantErr: false,
		},
	}
	execTests(t, Defconstant, tests)
}
