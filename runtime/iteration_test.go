// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import "testing"

func TestWhile(t *testing.T) {
	execTests(t, While, []test{
		{
			exp: `
			(let ((x '())
				  (i 5))
			(while (> i 0)
				(setq x (cons i x))
				(setq i (- i 1)))
			x) 
  			`,
			want:    `'(1 2 3 4 5)`,
			wantErr: false,
		},
	})
}
func TestFor(t *testing.T) {
	execTests(t, For, []test{
		{
			exp: `
			(for ((vec (vector 0 0 0 0 0))
				  (i 0 (+ i 1)))
				 ((= i 5) vec)
			  (setf (elt vec i) i))
  			`,
			want:    `#(0 1 2 3 4)`,
			wantErr: false,
		},
		{
			exp: `
			(let ((x '(1 3 5 7 9)))
				(for ((x x (cdr x))
					  (sum 0 (+ sum (car x))))
					 ((null x) sum))) 
  			`,
			want:    `25`,
			wantErr: false,
		},
	})
}
