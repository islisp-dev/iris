package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
)

func ReadByte(e ilos.Environment, args ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	str := e.StandardInput
	if len(args) > 0 {
		str = args[0]
	}
	if ok, _ := InputStreamP(e, str); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, str, ilos.StreamClass), Nil)
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
		return SignalCondition(e, ilos.NewArityError(e), Nil)
	}
	buf := make([]byte, 1)
	n, err := str.(ilos.Stream).Reader.Read(buf)

	if n != 1 || err != nil {
		if eosErrorP {
			return nil, ilos.Create(e, ilos.EndOfStreamClass)
		}
		return eosValue, nil
	}
	return ilos.NewInteger(int(buf[0])), nil
}

func WriteByte(e ilos.Environment, obj, str ilos.Instance) (ilos.Instance, ilos.Instance) {
	s, ok := str.(ilos.Stream)
	if !ok {
		return SignalCondition(e, ilos.NewDomainError(e, s, ilos.StreamClass), Nil)
	}

	n, ok := obj.(ilos.Integer)
	if !ok {
		return SignalCondition(e, ilos.NewDomainError(e, s, ilos.IntegerClass), Nil)
	}

	b := byte(n)
	if err := s.WriteByte(b); err != nil {
		return SignalCondition(e, ilos.NewStreamError(e), Nil)
	}
	return ilos.NewInteger(int(b)), nil
}
