package lib

import "testing"

func TestNull(t *testing.T) {
	execTests(t, Null, []test{
		{
			exp:     `(null '(a b c))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(null '())`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(null (list))`,
			want:    `t`,
			wantErr: false,
		},
	})
}

func TestListp(t *testing.T) {
	execTests(t, Listp, []test{
		{
			exp:     `(listp '(a b c))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(listp '())`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(listp '(a . b))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp: `
			(let ((x (list 'a)))
			  (setf (cdr x) x)
			  (listp x))
			`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(listp "abc")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(listp #(1 2))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(listp 'jerome)`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestCreateList(t *testing.T) {
	execTests(t, CreateList, []test{
		{
			exp:     `(create-list 3 17)`,
			want:    `'(17 17 17)`,
			wantErr: false,
		},
		{
			exp:     `(create-list 2 #\a)`,
			want:    `'(#\a #\a)`,
			wantErr: false,
		},
	})
}

func TestList(t *testing.T) {
	execTests(t, List, []test{
		{
			exp:     `(list 'a (+ 3 4) 'c)`,
			want:    `'(a 7 c)`,
			wantErr: false,
		},
		{
			exp:     `(list)`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestReverse(t *testing.T) {
	execTests(t, Reverse, []test{
		{
			exp:     `(reverse '(a b c d e))`,
			want:    `'(e d c b a)`,
			wantErr: false,
		},
		{
			exp:     `(reverse '(a))`,
			want:    `'(a)`,
			wantErr: false,
		},
		{
			exp:     `(reverse '())`,
			want:    `'()`,
			wantErr: false,
		},
		{
			exp: `
			(let* ((x (list 'a 'b))
			       (y (nreverse x)))
			  (equal x y))
			`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestAppend(t *testing.T) {
	execTests(t, Append, []test{
		{
			exp:     `(append '(a b c) '(d e f))`,
			want:    `'(a b c d e f)`,
			wantErr: false,
		},
	})
}

func TestMember(t *testing.T) {
	execTests(t, Member, []test{
		{
			exp:     `(member 'c '(a b c d e f))`,
			want:    `'(c d e f)`,
			wantErr: false,
		},
		{
			exp:     `(member 'g '(a b c d e f))`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(member 'c '(a b c a b c))`,
			want:    `'(c a b c)`,
			wantErr: false,
		},
	})
}

func TestMap(t *testing.T) {
	execTests(t, Mapcar, []test{
		{
			exp:     `(mapcar #'car '((1 a) (2 b) (3 c)))`,
			want:    `'(1 2 3)`,
			wantErr: false,
		},
		{
			exp:     `(mapcar #'abs '(3 -4 2 -5 -6))`,
			want:    `'(3 4 2 5 6)`,
			wantErr: false,
		},
		{
			exp:     `(mapcar #'cons '(a b c) '(1 2 3))`,
			want:    `'((a . 1) (b . 2) (c . 3))`,
			wantErr: false,
		},
		{
			exp: `
			(let ((x 0))
			  (mapc (lambda (v) (setq x (+ x v))) '(3 5))
			    x)
			`,
			want:    `8`,
			wantErr: false,
		},
		{
			exp: `
			(maplist #'append
			  '(1 2 3 4) '(1 2) '(1 2 3))
			`,
			want:    `'((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3))`,
			wantErr: false,
		},
		{
			exp: `
			(maplist (lambda (x) (cons 'foo x))
			  '(a b c d))
			`,
			want:    `'((foo a b c d) (foo b c d) (foo c d) (foo d))`,
			wantErr: false,
		},
		{
			exp: `
			(maplist (lambda (x) (if (member (car x) (cdr x)) 0 1))
			           '(a b a c d b c))
			`,
			want:    `'(0 0 1 0 1 1 1)`,
			wantErr: false,
		},
		{
			exp: `
			(let ((k 0))
			  (mapl (lambda (x)
			          (setq k (+ k (if (member (car x) (cdr x)) 0 1))))
			    '(a b a c d b c))
			  k)
			`,
			want:    `4`,
			wantErr: false,
		},
		{
			exp: `
			(mapcan (lambda (x) (if (> x 0) (list x)))
			  '(-3 4 0 5 -2 7))
			`,
			want:    `'(4 5 7)`,
			wantErr: false,
		},
		{
			exp: `
			(mapcon (lambda (x) (if (member (car x) (cdr x)) (list (car x))))
			  '(a b a c d b c b c))
			`,
			want:    `'(a b c b c)`,
			wantErr: false,
		},
		{
			exp:     `(mapcon #'list '(1 2 3 4))`,
			want:    `'((1 2 3 4) (2 3 4) (3 4) (4))`,
			wantErr: false,
		},
	})
}

func TestAssoc(t *testing.T) {
	execTests(t, Assoc, []test{
		{
			exp:     `(assoc 'a '((a . 1) (b . 2)))`,
			want:    `'(a . 1)`,
			wantErr: false,
		},
		{
			exp:     `(assoc 'c '((a . 1) (a . 2)))`,
			want:    `nil`,
			wantErr: false,
		},
	})
}
