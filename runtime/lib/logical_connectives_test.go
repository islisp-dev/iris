// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "testing"

func TestNot(t *testing.T) {
	tests := []test{
		{
			exp:     `(not t)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(not '())`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(not 3)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(not t)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(not (list))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(not (list 3))`,
			want:    `nil`,
			wantErr: false,
		},
	}
	execTests(t, Not, tests)
}

func TestAnd(t *testing.T) {
	tests := []test{
		{
			exp:     `(and (= 2 2) (> 2 1))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(and (= 2 2) (< 2 1))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(and (eql 'a 'a) (not (> 1 2)))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp: `
				(let ((x 'a))
					(and x (setq x 'b)))
			`,
			want:    `'b`,
			wantErr: false,
		},
		{
			exp: `
				(let ((x nil))
					(and x (setq x 'b)))
 			`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp: `
				(let ((time 10))
					(if (and (< time 24) (> time 12))
						(- time 12)
						time))			
			`,
			want:    `10`,
			wantErr: false,
		},
		{
			exp: `
				(let ((time 18))
  					(if (and (< time 24) (> time 12))
      					(- time 12)
    					time))			
			`,
			want:    `6`,
			wantErr: false,
		},
	}
	execTests(t, And, tests)
}

func TestOr(t *testing.T) {
	tests := []test{
		{
			exp:     `(or (= 2 2) (> 2 1))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(or (= 2 2) (< 2 1))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp: `
				(let ((x 'a))
  					(or x (setq x 'b)))
			`,
			want:    `'a`,
			wantErr: false,
		},
		{
			exp: `
				(let ((x nil))
  					(or x (setq x 'b)))
			`,
			want:    `'b`,
			wantErr: false,
		},
	}
	execTests(t, And, tests)
}
