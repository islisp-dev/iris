// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
)

// Defmacro defines a named (toplevel) macro. No implicit block with the macro
// name is established when the macro-expansion function is invoked. macro-name
// must be an identifier whose scope is the current toplevel scope in which the
// defmacro form appears. lambda-list is as defined in page 23. The definition
// point of macro-name is the closing parenthesis of the lambda-list.
func Defmacro(e ilos.Environment, macroName, lambdaList ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.SymbolClass, macroName); err != nil {
		return nil, err
	}
	ret, err := newNamedFunction(e, macroName, lambdaList, forms...)
	if err != nil {
		return nil, err
	}
	e.Macro[:1].Define(macroName, ret)
	return macroName, nil
}

// Quasiquote ` or quasiquote constructs a list structure. quasiquote, like
// quote, returns its argument unevaluated if no commas or the syntax ,
// (unquote) or ,@ (unquote-splicing) appear within the form. , (unquote) syntax
// is valid only within ` (quasiquote) expressions. When appearing within a
// quasiquote the form is evaluated and its result is inserted into the
// quasiquote structure instead of the unquote form. ,@ (unquote-splicing) is
// also syntax valid only within ` expressions. When appearing within a
// quasiquote the expression form must evaluate to a list. The elements of the
// list are spliced into the enclosing list in place of the unquote-splicing
// form sequence. Quasiquote forms may be nested. Substitutions are made only
// for unquoted expressions appearing at the same nesting level, which increases
// by one inside each successive quasiquotation and decreases by one inside each
// unquotation.
func Quasiquote(e ilos.Environment, form ilos.Instance) (ilos.Instance, ilos.Instance) {
	return expand(e, form, 0)
}

func expand(e ilos.Environment, form ilos.Instance, level int) (ilos.Instance, ilos.Instance) {
	if !ilos.InstanceOf(ilos.ConsClass, form) {
		return form, nil
	} // If form is a instance of <cons> then,
	exp := []ilos.Instance{}
	cdr := form
	for ilos.InstanceOf(ilos.ConsClass, cdr) {
		cadr := cdr.(*ilos.Cons).Car
		cddr := cdr.(*ilos.Cons).Cdr
		// To expand `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
		if cadr == ilos.NewSymbol("UNQUOTE") && level == 0 {
			caddr := cddr.(*ilos.Cons).Car
			elt, err := Eval(e, caddr)
			if err != nil {
				return nil, err
			}
			exp = append(exp, elt)
			break
		}
		if !ilos.InstanceOf(ilos.ConsClass, cadr) {
			lst, err := List(e, cadr)
			if err != nil {
				return nil, err
			}
			exp = append(exp, lst)
			cdr = cdr.(*ilos.Cons).Cdr
			continue
		} // If cadr is a instance of <cons> then,
		caadr := cadr.(*ilos.Cons).Car
		cdadr := cadr.(*ilos.Cons).Cdr
		if caadr == ilos.NewSymbol("UNQUOTE") {
			cadadr := cdadr.(*ilos.Cons).Car
			var elt, err ilos.Instance
			if level == 0 {
				elt, err = Eval(e, cadadr)
				if err != nil {
					return nil, err
				}
				lst, err := List(e, elt)
				if err != nil {
					return nil, err
				}
				exp = append(exp, lst)
				cdr = cdr.(*ilos.Cons).Cdr
				continue
			} else {
				elt, err = expand(e, cadadr, level-1)
				if err != nil {
					return nil, err
				}
				lst, err := List(e, caadr, elt)
				if err != nil {
					return nil, err
				}
				lstlst, err := List(e, lst)
				if err != nil {
					return nil, err
				}
				exp = append(exp, lstlst)
				cdr = cdr.(*ilos.Cons).Cdr
				continue
			}
		}
		if caadr == ilos.NewSymbol("UNQUOTE-SPLICING") {
			cadadr := cdadr.(*ilos.Cons).Car
			if level == 0 {
				elt, err := Eval(e, cadadr)
				if err != nil {
					return nil, err
				}
				exp = append(exp, elt)
				cdr = cdr.(*ilos.Cons).Cdr
				continue
			} else {
				elt, err := expand(e, cadadr, level-1)
				if err != nil {
					return nil, err
				}
				lst, err := List(e, caadr, elt)
				if err != nil {
					return nil, err
				}
				lstlst, err := List(e, lst)
				if err != nil {
					return nil, err
				}
				exp = append(exp, lstlst)
				cdr = cdr.(*ilos.Cons).Cdr
				continue
			}
		}
		if caadr == ilos.NewSymbol("QUASIQUOTE") {
			cadadr := cdadr.(*ilos.Cons).Car
			elt, err := expand(e, cadadr, level+1)
			if err != nil {
				return nil, err
			}
			lst, err := List(e, caadr, elt)
			if err != nil {
				return nil, err
			}
			lstlst, err := List(e, lst)
			if err != nil {
				return nil, err
			}
			exp = append(exp, lstlst)
			cdr = cdr.(*ilos.Cons).Cdr
			continue
		}
		// If the cadr is not special forms then,
		elt, err := expand(e, cadr, level)
		if err != nil {
			return nil, err
		}
		lst, err := List(e, elt)
		if err != nil {
			return nil, err
		}
		exp = append(exp, lst)
		cdr = cddr
		continue
	}
	if ilos.InstanceOf(ilos.NullClass, cdr) {
		exp = append(exp, Nil)
	}
	lst := exp[len(exp)-1]
	for i := len(exp) - 2; i >= 0; i-- {
		if ilos.InstanceOf(ilos.ListClass, lst) {
			var err ilos.Instance
			lst, err = Append(e, exp[i], lst)
			if err != nil {
				return nil, err
			}
		} else {
			// To expand `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
			// If the last cell of forms is not Nil, run this statements at first

			// the elements of exp is always a instance of <list> because exp isn't appended lists in for-loop
			for j := exp[i].(ilos.List).Length() - 1; j >= 0; j-- {
				lst = ilos.NewCons(exp[i].(ilos.List).Nth(j), lst)
			}
		}
	}
	return lst, nil
}
