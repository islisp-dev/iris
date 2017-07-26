package main

// Function interaface type is the interface for calling function with arguments
type Function interface {
	Apply(*Object, *Env) (*Object, error)
}

// NativeFunction struct type is the strcut for calling function written in go
type NativeFunction struct {
	raw func(*Object, *Env) (*Object, error)
}

// Apply call function f with args in global
func (f *NativeFunction) Apply(args *Object, global *Env) (*Object, error) {
	a, err := f.raw(args, global)
	return a, err
}

// NormalFunction struct type is the strcut for calling function written by user
type NormalFunction struct {
	Args *Object
	Body *Object
}

// Apply call function f with args in global
func (f *NormalFunction) Apply(args *Object, global *Env) (*Object, error) {
	local := NewEnv()
	v := f.Args
	a := args
	for v != nil && a != nil {
		local.Var[v.Car.Val.(string)] = a.Car
		v = v.Cdr
		a = a.Cdr
	}
	b := f.Body
	var r *Object
	for b != nil {
		b, err := Eval(b.Car, local, global)
		if err != nil {
			return nil, err
		}
		b = b.Cdr
	}
	return r, nil
}
