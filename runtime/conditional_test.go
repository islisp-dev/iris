// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import "testing"

func TestIf(t *testing.T) {
	execTests(t, If, []test{
		{
			exp:     `(if (> 3 2) 'yes 'no)`,
			want:    `'yes`,
			wantErr: false,
		},
		{
			exp:     `(if (> 2 3) 'yes 'no)`,
			want:    `'no`,
			wantErr: false,
		},
		{
			exp:     `(if (> 2 3) 'yes)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(if (> 3 2) (- 3 2) (+ 3 2))`,
			want:    `1`,
			wantErr: false,
		},
		{
			exp: `
			(let ((x 7))
			  (if (< x 0) x (- x)))
			`,
			want:    `-7`,
			wantErr: false,
		},
	})
}

func TestCond(t *testing.T) {
	execTests(t, Cond, []test{
		{
			exp: `
			(cond ((> 3 2) 'greater)
				  ((< 3 2) 'less)) 
			`,
			want:    `'greater`,
			wantErr: false,
		},
		{
			exp: `
			(cond ((> 3 3) 'greater)
				  ((< 3 3) 'less)) 
			`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp: `
			(cond ((> 3 3) 'greater)
				  ((< 3 3) 'less)
				  (t 'equal))
			`,
			want:    `'equal`,
			wantErr: false,
		},
	})
}

func TestCase(t *testing.T) {
	execTests(t, Case, []test{
		{
			exp: `
			(case (* 2 3)
				((2 3 5 7) 'prime)
				((4 6 8 9) 'composite)) 
			`,
			want:    `'composite`,
			wantErr: false,
		},
		{
			exp: `
			(case (car '(c d))
				((a) 'a)
				((b) 'b)) 
			`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp: `
			(case (car '(c d))
				((a e i o u) 'vowel)
				((y) 'semivowel)
				(t 'consonant)) 
			`,
			want:    `'consonant`,
			wantErr: false,
		},
		{
			exp: `
			(let ((char #\u))
			  (case char
			    ((#\a #\e #\o #\u #\i) 'vowels)
			    (t 'consonants))) 
			`,
			want:    `'vowels`,
			wantErr: false,
		},
	})
}

func TestCaseUsing(t *testing.T) {
	execTests(t, CaseUsing, []test{
		{
			exp: `
			(case-using #'= (+ 1.0 1.0)
			  ((1) 'one)
			  ((2) 'two)
			  (t 'more))
			`,
			want:    `'two`,
			wantErr: false,
		},
		{
			exp: `
			(case-using #'string= "bar"
			  (("foo") 1)
			  (("bar") 2))
			`,
			want:    `'2`,
			wantErr: false,
		},
	})
}
