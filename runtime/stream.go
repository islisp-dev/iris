// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"bufio"
	"bytes"
	"os"
	"strings"

	"github.com/islisp-dev/iris/reader/parser"
	"github.com/islisp-dev/iris/runtime/env"
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/class"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func Streamp(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(class.Stream, obj) {
		return T, nil
	}
	return Nil, nil
}

func OpenStreamP(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	return T, nil
}

func InputStreamP(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if s, ok := obj.(instance.Stream); ok && s.Reader != nil {
		return T, nil
	}
	return Nil, nil
}

func OutputStreamP(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if s, ok := obj.(instance.Stream); ok && s.Writer != nil {
		return T, nil
	}
	return Nil, nil
}

func StandardInput(e env.Environment) (ilos.Instance, ilos.Instance) {
	return e.StandardInput, nil
}

func StandardOutput(e env.Environment) (ilos.Instance, ilos.Instance) {
	return e.StandardOutput, nil
}

func ErrorOutput(e env.Environment) (ilos.Instance, ilos.Instance) {
	return e.ErrorOutput, nil
}

func WithStandardInput(e env.Environment, streamForm ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	e.StandardInput, err = Eval(e, streamForm)
	if err != nil {
		return nil, err
	}
	if ok, _ := InputStreamP(e, e.StandardInput); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, e.StandardInput, class.Stream), Nil)
	}
	return Progn(e, forms...)
}

func WithStandardOutput(e env.Environment, streamForm ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	e.StandardOutput, err = Eval(e, streamForm)
	if err != nil {
		return nil, err
	}
	if ok, _ := OutputStreamP(e, e.StandardOutput); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, e.StandardOutput, class.Stream), Nil)
	}
	return Progn(e, forms...)
}

func WithErrorOutput(e env.Environment, streamForm ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	e.ErrorOutput, err = Eval(e, streamForm)
	if err != nil {
		return nil, err
	}
	if ok, _ := OutputStreamP(e, e.ErrorOutput); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, e.ErrorOutput, class.Stream), Nil)
	}
	return Progn(e, forms...)
}

func OpenInputFile(e env.Environment, filename ilos.Instance, elementClass ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: elementClass
	if ok, _ := Stringp(e, filename); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, filename, class.String), Nil)
	}
	file, err := os.Open(string(filename.(instance.String)))
	if err != nil {
		return SignalCondition(e, instance.NewStreamError(e), Nil)
	}
	return instance.NewStream(file, nil), nil
}

func OpenOutputFile(e env.Environment, filename ilos.Instance, elementClass ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: elementClass
	if ok, _ := Stringp(e, filename); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, filename, class.String), Nil)
	}
	rawFilename := string(filename.(instance.String))
	var file *os.File
	if _, err := os.Stat(rawFilename); os.IsNotExist(err) {
		if file, err = os.Create(rawFilename); err != nil {
			return SignalCondition(e, instance.NewStreamError(e), Nil)
		}
	} else {
		if file, err = os.Open(string(filename.(instance.String))); err != nil {
			return SignalCondition(e, instance.NewStreamError(e), Nil)
		}
	}
	return instance.NewStream(nil, file), nil
}

func OpenIoFile(e env.Environment, filename ilos.Instance, elementClass ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: elementClass
	if ok, _ := Stringp(e, filename); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, filename, class.String), Nil)
	}
	file, err := os.Open(string(filename.(instance.String)))
	if err != nil {
		return SignalCondition(e, instance.NewStreamError(e), Nil)
	}
	return instance.NewStream(file, file), nil
}

func WithOpenInputFile(e env.Environment, fileSpec ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := Consp(e, fileSpec); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, fileSpec, class.Cons), Nil)
	}
	car, err := Car(e, fileSpec)
	if err != nil {
		return nil, err
	}
	cdr, err := Cdr(e, fileSpec)
	if err != nil {
		return nil, err
	}
	cadr, err := Car(e, cdr)
	if err != nil {
		return nil, err
	}
	s, err := OpenInputFile(e, cadr)
	if err != nil {
		return nil, err
	}
	e.Variable.Define(car, s)
	r, err := Progn(e, forms...)
	e.Variable.Delete(car)
	if _, err := Close(e, s); err != nil {
		return nil, err
	}
	return r, err
}

func WithOpenOutputFile(e env.Environment, fileSpec ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := Consp(e, fileSpec); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, fileSpec, class.Cons), Nil)
	}
	car, err := Car(e, fileSpec)
	if err != nil {
		return nil, err
	}
	cdr, err := Cdr(e, fileSpec)
	if err != nil {
		return nil, err
	}
	cadr, err := Car(e, cdr)
	if err != nil {
		return nil, err
	}
	s, err := OpenOutputFile(e, cadr)
	if err != nil {
		return nil, err
	}
	e.Variable.Define(car, s)
	r, err := Progn(e, forms...)
	e.Variable.Delete(car)
	if _, err := Close(e, s); err != nil {
		return nil, err
	}
	return r, err
}

func WithOpenIoFile(e env.Environment, fileSpec ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := Consp(e, fileSpec); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, fileSpec, class.Cons), Nil)
	}
	n, err := Car(e, fileSpec)
	if err != nil {
		return nil, err
	}
	s, err := OpenIoFile(e, n)
	if err != nil {
		return nil, err
	}
	e.Variable.Define(n, s)
	r, err := Progn(e, forms...)
	e.Variable.Delete(n)
	if _, err := Close(e, s); err != nil {
		return nil, err
	}
	return r, err
}

func Close(e env.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	// It works on file or std stream.
	if ok, _ := Streamp(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if stream.(instance.Stream).Reader != nil {
		stream.(instance.Stream).Reader.Raw.(*os.File).Close()
	}
	if stream.(instance.Stream).Writer != nil {
		stream.(instance.Stream).Writer.(*os.File).Close()
	}
	return Nil, nil
}

func FlushOutput(e env.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	// It works on file or std stream.
	if ok, _ := Streamp(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if stream.(instance.Stream).Writer != nil {
		// Go does not provide Flush(). We should use bufio
	}
	return Nil, nil
}

func CreateStringInputStream(e env.Environment, str ilos.Instance) (ilos.Instance, ilos.Instance) {
	return instance.NewStream(strings.NewReader(string(str.(instance.String))), nil), nil
}

func CreateStringOutputStream(e env.Environment) (ilos.Instance, ilos.Instance) {
	return instance.NewStream(nil, new(bytes.Buffer)), nil
}

func GetOutputStreamString(e env.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OutputStreamP(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	out := instance.NewString([]rune(stream.(instance.Stream).Writer.(*bytes.Buffer).String()))
	stream.(instance.Stream).Writer.(*bytes.Buffer).Reset()
	return out, nil
}

func Read(e env.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if b, _ := InputStreamP(e, s); b == Nil {
		return SignalCondition(e, instance.NewDomainError(e, s, class.Stream), Nil)
	}
	eosErrorP := true
	if len(options) > 1 {
		if options[1] == Nil {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(options) > 2 {
		if options[2] == Nil {
			eosValue = options[2]
		}
	}
	v, err := parser.Parse(s.(instance.Stream).Reader)
	if err != nil && ilos.InstanceOf(class.EndOfStream, err) {
		if eosErrorP {
			return nil, err
		}
		return eosValue, nil
	}
	return v, nil
}

func ReadChar(e env.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if ok, _ := InputStreamP(e, s); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, s, class.Stream), Nil)
	}
	eosErrorP := true
	if len(options) > 1 {
		if options[1] == Nil {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(options) > 2 {
		if options[2] == Nil {
			eosValue = options[2]
		}
	}
	//v, _, err := bufio.NewReader(s.(instance.Stream).Reader).ReadRune()
	v, _, err := s.(instance.Stream).Reader.ReadRune()
	if err != nil {
		if eosErrorP {
			return nil, instance.Create(e, class.EndOfStream)
		}
		return eosValue, nil
	}
	return instance.NewCharacter(v), nil
}

func ProbeFile(e env.Environment, fs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if len(fs) != 1 {
		return SignalCondition(e, instance.NewArityError(e), Nil)
	}
	if t, err := Stringp(e, fs[0]); err != nil || t == Nil {
		return SignalCondition(e, instance.NewDomainError(e, fs[0], class.String), Nil)
	}
	if _, err := os.Stat(string(fs[0].(instance.String))); os.IsNotExist(err) {
		return Nil, nil
	} else {
		return T, nil
	}
}

func PreviewChar(e env.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if ok, _ := InputStreamP(e, s); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, s, class.Stream), Nil)
	}
	eosErrorP := true
	if len(options) > 1 {
		if options[1] == Nil {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(options) > 2 {
		if options[2] == Nil {
			eosValue = options[2]
		}
	}
	//v, _, err := bufio.NewReader(s.(instance.Stream).Reader).ReadRune()
	v, _, err := s.(instance.Stream).Reader.PeekRune()
	if err != nil {
		if eosErrorP {
			return nil, instance.Create(e, class.EndOfStream)
		}
		return eosValue, nil
	}
	return instance.NewCharacter(v), nil
}

func ReadLine(e env.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if ok, _ := InputStreamP(e, s); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, s, class.Stream), Nil)
	}
	eosErrorP := true
	if len(options) > 1 {
		if options[1] == Nil {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(options) > 2 {
		if options[2] == Nil {
			eosValue = options[2]
		}
	}
	v, _, err := bufio.NewReader(s.(instance.Stream).Reader).ReadLine()
	if err != nil {
		if eosErrorP {
			return nil, instance.Create(e, class.EndOfStream)
		}
		return eosValue, nil
	}
	return instance.NewString([]rune(string(v))), nil
}

func StreamReadyP(e env.Environment, inputStream ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: stream-ready-p
	return T, nil
}
