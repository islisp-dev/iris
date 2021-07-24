package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func ReadByte(e ilos.Environment, args ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	str := e.StandardInput
	if len(args) > 0 {
		str = args[0]
	}
	if ok, _ := InputStreamP(e, str); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, str, instance.StreamClass), Nil)
	}
	eosErrorP := true
	if len(args) > 1 {
		if args[1] == Nil {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(args) > 2 {
		if args[2] == Nil {
			eosValue = args[2]
		}
	}
	if len(args) < 1 || len(args) > 3 {
		return SignalCondition(e, instance.NewArityError(e), Nil)
	}
	buf := make([]byte, 1)
	n, err := str.(instance.Stream).Reader.Read(buf)

	if n != 1 || err != nil {
		if eosErrorP {
			return nil, instance.Create(e, instance.EndOfStreamClass)
		}
		return eosValue, nil
	}
	return instance.NewInteger(int(buf[0])), nil
}

func WriteByte(e ilos.Environment, obj, str ilos.Instance) (ilos.Instance, ilos.Instance) {
	s, ok := str.(instance.Stream)
	if !ok {
		return SignalCondition(e, instance.NewDomainError(e, s, instance.StreamClass), Nil)
	}

	n, ok := obj.(instance.Integer)
	if !ok {
		return SignalCondition(e, instance.NewDomainError(e, s, instance.IntegerClass), Nil)
	}

	b := byte(n)
	if err := s.WriteByte(b); err != nil {
		return SignalCondition(e, instance.NewStreamError(e), Nil)
	}
	return instance.NewInteger(int(b)), nil
}
