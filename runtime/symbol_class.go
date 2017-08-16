// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// TODO: setf property

// Symbolp returns t if obj is a symbol (instance of class symbol);
// otherwise, returns nil. The obj may be any ISLISP object.
func Symbolp(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Symbol, obj) {
		return T, nil
	}
	return Nil, nil
}

// Property returns the value of the property named property-name
// associated with the symbol symbol . If symbol has no property named
// property-name, obj (which defaults to nil) is returned.
//
// An error shall be signaled if either symbol or property-name is not a
// symbol (error-id. domain-error). obj may be any ISLISP object
func Property(local, global *environment.Environment, symbol, propertyName ilos.Instance, obj ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Symbol, symbol); err != nil {
		return nil, err
	}
	if len(obj) > 1 {
		return nil, instance.NewArityError()
	}
	ret, ok := global.Property.Get(symbol, propertyName)
	if ok {
		return ret, nil
	}
	return obj[1], nil
}

// SetProperty causes obj to be the new value of the property named
// property-name asssociated with the symbol symbol . If the property
// named property-name already exists, its corresponding property value is
// replaced; otherwise, a new property is created. obj is returned.
//
// An error shall be signaled if either symbol or property-name is not a
// symbol (error-id. domain-error). obj may be any ISLISP object
func SetProperty(local, global *environment.Environment, obj, symbol, propertyName ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Symbol, symbol); err != nil {
		return nil, err
	}
	global.Property.Set(symbol, propertyName, obj)
	return obj, nil
}

// RemoveProperty removes the property property-name associated with
// symbol and returns the property value of the removed property if there
// is such a property. If there is no such property, nil is returned.
//
// An error shall be signaled if either symbol or property-name is not a
// symbol (error-id. domain-error).
func RemoveProperty(local, global *environment.Environment, symbol, propertyName ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Symbol, symbol); err != nil {
		return nil, err
	}
	if v, ok := global.Property.Delete(symbol, propertyName); ok {
		return v, nil
	}
	return Nil, nil
}

// Gensym returns an unnamed symbol. gensym is useful for writing macros.
// It is impossible for an identifier to name an unnamed symbol.
func Gensym(local, global *environment.Environment) (ilos.Instance, ilos.Instance) {
	global.GensymID++
	return instance.NewSymbol(fmt.Sprintf("IRIS/G#%v", global.GensymID)), nil
}