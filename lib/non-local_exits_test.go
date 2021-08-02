package lib

import "testing"

func TestBlock(t *testing.T) {
	tests := []test{
		{
			exp:     `(block x (+ 10 (return-from x 6) 22))`,
			want:    `6`,
			wantErr: false,
		},
		{
			exp: `
				(defun f1 ()
				  (block b
				    (let ((f (lambda () (return-from b 'exit))))
				      ; big computation
				      (f2 f))))
			`,
			want:    `'f1`,
			wantErr: false,
		},
		{
			exp: `
				(defun f2 (g)
				  ; big computation
				  (funcall g))
			`,
			want:    `'f2`,
			wantErr: false,
		},
		{
			exp:     `(f1)`,
			want:    `'exit`,
			wantErr: false,
		},
		{
			exp: `
				(block sum-block
				  (for ((x '(1 a 2 3) (cdr x))
				        (sum 0 (+ sum (car x))))
				       ((null x) sum)
				    (cond ((not (numberp (car x))) (return-from sum-block 0)))))
			`,
			want:    `0`,
			wantErr: false,
		},
		{
			exp: `
				(defun bar (x y)
				  (let ((foo #'car))
				    (let ((result (block bl
				                    (setq foo (lambda () (return-from bl 'first-exit)))
				                    (if x (return-from bl 'second-exit) 'third-exit))))
				      (if y (funcall foo) nil)
				      result)))
			`,
			want:    `'bar`,
			wantErr: false,
		},
		{
			exp:     `(bar t nil)`,
			want:    `'second-exit`,
			wantErr: false,
		},
		{
			exp:     `(bar nil t)`,
			want:    `nil`,
			wantErr: true,
		},
		{
			exp:     `(bar nil nil)`,
			want:    `'third-exit`,
			wantErr: false,
		},
		{
			exp:     `(bar t t)`,
			want:    `nil`,
			wantErr: true,
		},
	}
	execTests(t, Block, tests)
}

func TestCatch(t *testing.T) {
	tests := []test{
		{
			exp: `
				(defun foo (x)
				  (catch 'block-sum (bar x)))
			`,
			want:    `'foo`,
			wantErr: false,
		},
		{
			exp: `
				(defun bar (x)
				  (for ((l x (cdr l))
				        (sum 0 (+ sum (car l))))
				       ((null l) sum)
				    (cond ((not (numberp (car l))) (throw 'block-sum 0)))))
			`,
			want:    `'bar`,
			wantErr: false,
		},
		{
			exp:     `(foo '(1 2 3 4))`,
			want:    `10`,
			wantErr: false,
		},
		{
			exp:     `(foo '(1 2 a 4))`,
			want:    `0`,
			wantErr: false,
		},
	}
	execTests(t, Catch, tests)
}

func TestUnwindProtect(t *testing.T) {
	tests := []test{
		{
			exp: `
				(defun foo (x)
				  (catch 'duplicates
				    (unwind-protect (bar x)
				      (for ((l x (cdr l)))
				           ((null l) 'unused)
				        (remove-property (car l) 'label)))))
			`,
			want:    `'foo`,
			wantErr: false,
		},
		{
			exp: `
				(defun bar (l)
				  (cond ((and (symbolp l) (property l 'label))
				         (throw 'duplicates 'found))
				        ((symbolp l) (setf (property l 'label) t))
				         ((bar (car l)) (bar (cdr l)))
				        (t nil)))
			`,
			want:    `'bar`,
			wantErr: false,
		},
		{
			exp:     `(foo '(a b c))`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(property 'a 'label)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(foo '(a b a c))`,
			want:    `'found`,
			wantErr: false,
		},
		{
			exp:     `(property 'a 'label)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp: `
				(defun test ()
				  (catch 'outer (test2)))
			`,
			want:    `'test`,
			wantErr: false,
		},
		{
			exp: `
				(defun test2 ()
  				  (block inner
    				(test3 (lambda ()
     	        			  (return-from inner 7)))))
			`,
			want:    `'test2`,
			wantErr: false,
		},
		{
			exp: `
				(defun test3 (fun)
  				  (unwind-protect (test4) (funcall fun)))
			`,
			want:    `'test3`,
			wantErr: false,
		},
		{
			exp: `
				(defun test4 ()
				  (throw 'outer 6))
			`,
			want:    `'test4`,
			wantErr: false,
		},
		{
			exp:     `(test)`,
			want:    `nil`,
			wantErr: true,
		},
	}
	execTests(t, UnwindProtect, tests)
}
