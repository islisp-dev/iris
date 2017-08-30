// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import "testing"

func TestDefmacro(t *testing.T) {
	tests := []test{
		{
			exp:     "(defmacro caar(x) (list ’car (list ’car x)))",
			want:    "'caar",
			wantErr: false,
		},
	}
	execTests(t, Defmacro, tests)
}

func TestQuasiquote(t *testing.T) {
	tests := []test{
		{
			exp:     "`(list ,(+ 1 2) 4)",
			want:    `'(list 3 4)`,
			wantErr: false,
		},
		{
			exp:     "(let ((name 'a)) `(list name ,name ',name))",
			want:    `'(list name a (quote a))`,
			wantErr: false,
		},
		{
			exp:     "`(a ,(+ 1 2) ,@(create-list 3 'x) b)",
			want:    `'(a 3 x x x b)`,
			wantErr: false,
		},
		{
			exp:     "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))",
			want:    `'((foo 7) . cons)`,
			wantErr: false,
		},
		{
			exp:     "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)",
			want:    "'(a `(b ,(+ 1 2) ,(foo 4 d) e) f)",
			wantErr: false,
		},
		{
			exp:     "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))",
			want:    "`(a `(b ,x ,'y d) e)",
			wantErr: false,
		},
	}
	execTests(t, Quasiquote, tests)
}
