package lib

import "github.com/islisp-dev/iris/core"

func ReadByte(e core.Environment, args ...core.Instance) (core.Instance, core.Instance) {
	str := e.StandardInput
	if len(args) > 0 {
		str = args[0]
	}
	if ok, _ := InputStreamP(e, str); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, str, core.StreamClass), Nil)
	}
	eosErrorP := true
	if len(args) > 1 {
		if core.DeepEqual(args[1], Nil) {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(args) > 2 {
		if core.DeepEqual(args[2], Nil) {
			eosValue = args[2]
		}
	}
	if len(args) < 1 || len(args) > 3 {
		return SignalCondition(e, core.NewArityError(e), Nil)
	}
	buf := make([]byte, 1)
	n, err := str.(core.Stream).Reader.Read(buf)

	if n != 1 || err != nil {
		if eosErrorP {
			return nil, core.Create(e, core.EndOfStreamClass)
		}
		return eosValue, nil
	}
	return core.NewInteger(int(buf[0])), nil
}

func WriteByte(e core.Environment, obj, str core.Instance) (core.Instance, core.Instance) {
	s, ok := str.(core.Stream)
	if !ok {
		return SignalCondition(e, core.NewDomainError(e, s, core.StreamClass), Nil)
	}

	n, ok := obj.(core.Integer)
	if !ok {
		return SignalCondition(e, core.NewDomainError(e, s, core.IntegerClass), Nil)
	}

	b := byte(n)
	if err := s.WriteByte(b); err != nil {
		return SignalCondition(e, core.NewStreamError(e, str), Nil)
	}
	return core.NewInteger(int(b)), nil
}
