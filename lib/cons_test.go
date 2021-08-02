package lib

import "testing"

func TestConsp(t *testing.T) {
	execTests(t, Consp, []test{
		{
			exp:     `(consp '(a . b))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(consp '(a b c))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(consp '())`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(consp #(a b))`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestCons(t *testing.T) {
	execTests(t, Cons, []test{
		{
			exp:     `(cons 'a '())`,
			want:    `'(a)`,
			wantErr: false,
		},
		{
			exp:     `(cons '(a) '(b c d))`,
			want:    `'((a) b c d)`,
			wantErr: false,
		},
		{
			exp:     `(cons "a" '(b c))`,
			want:    `'("a" b c)`,
			wantErr: false,
		},
		{
			exp:     `(cons 'a 3)`,
			want:    `'(a . 3)`,
			wantErr: false,
		},
		{
			exp:     `(cons '(a b) 'c)`,
			want:    `'((a b) . c)`,
			wantErr: false,
		},
	})
}

func TestCar(t *testing.T) {
	execTests(t, Car, []test{
		{
			exp:     `(car '())`,
			want:    `nil`,
			wantErr: true,
		},
		{
			exp:     `(car '(a b c))`,
			want:    `'a`,
			wantErr: false,
		},
		{
			exp:     `(car '((a) b c d))`,
			want:    `'(a)`,
			wantErr: false,
		},
		{
			exp:     `(car '(1 . 2))`,
			want:    `1`,
			wantErr: false,
		},
	})
}

func TestCdr(t *testing.T) {
	execTests(t, Cdr, []test{
		{
			exp:     `(cdr '())`,
			want:    `nil`,
			wantErr: true,
		},
		{
			exp:     `(cdr '((a) b c d))`,
			want:    `'(b c d)`,
			wantErr: false,
		},
		{
			exp:     `(cdr '(1 . 2))`,
			want:    `2`,
			wantErr: false,
		},
	})
}

func TestSetCar(t *testing.T) {
	execTests(t, SetCar, []test{
		{
			exp: `
			(let ((x (list 'apple 'orange)))
			  (list x (car x)
			        (setf (car x) 'banana)
			        x (car x)))
			`,
			want:    `'((banana orange) apple banana (banana orange) banana)`,
			wantErr: false,
		},
	})
}

func TestSetCdr(t *testing.T) {
	execTests(t, SetCdr, []test{
		{
			exp: `
			(let ((x (list 'apple 'orange)))
			  (list x (cdr x)
			        (setf (cdr x) 'banana)
			        x (cdr x)))
			`,
			want:    `'((apple . banana) (orange) banana (apple . banana) banana)`,
			wantErr: false,
		},
	})
}
