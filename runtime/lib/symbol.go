// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import (
	"fmt"

	"github.com/islisp-dev/iris/runtime/core"
)

// Symbolp returns t if obj is a symbol (instance of class symbol); otherwise,
// returns nil. The obj may be any ISLISP object.
func Symbolp(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.SymbolClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// Property returns the value of the property named property-name associated
// with the symbol symbol . If symbol has no property named property-name, obj
// (which defaults to nil) is returned. An error shall be signaled if either
// symbol or property-name is not a symbol (error-id. domain-error). obj may be
// any ISLISP object
func Property(e core.Environment, symbol, propertyName core.Instance, obj ...core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.SymbolClass, symbol); err != nil {
		return nil, err
	}
	if len(obj) > 1 {
		return SignalCondition(e, core.NewArityError(e), Nil)
	}
	ret, ok := e.Property.Get(symbol, propertyName)
	if ok {
		return ret, nil
	}
	if len(obj) == 0 {
		return Nil, nil
	}
	return obj[0], nil
}

// SetProperty causes obj to be the new value of the property named
// property-name asssociated with the symbol symbol . If the property named
// property-name already exists, its corresponding property value is replaced;
// otherwise, a new property is created. obj is returned. An error shall be
// signaled if either symbol or property-name is not a symbol (error-id.
// domain-error). obj may be any ISLISP object
func SetProperty(e core.Environment, obj, symbol, propertyName core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.SymbolClass, symbol); err != nil {
		return nil, err
	}
	e.Property.Set(symbol, propertyName, obj)
	return obj, nil
}

// RemoveProperty removes the property property-name associated with symbol and
// returns the property value of the removed property if there is such a
// property. If there is no such property, nil is returned. An error shall be
// signaled if either symbol or property-name is not a symbol (error-id.
// domain-error).
func RemoveProperty(e core.Environment, symbol, propertyName core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.SymbolClass, symbol); err != nil {
		return nil, err
	}
	if v, ok := e.Property.Delete(symbol, propertyName); ok {
		return v, nil
	}
	return Nil, nil
}

// Gensym returns an unnamed symbol. gensym is useful for writing macros. It is
// impossible for an identifier to name an unnamed symbol.
func Gensym(e core.Environment) (core.Instance, core.Instance) {
	symbol := core.NewSymbol(fmt.Sprintf("#:%v", uniqueInt()))
	return symbol, nil
}
