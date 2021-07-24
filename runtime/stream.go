// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"bytes"
	"os"
	"strings"

	"github.com/islisp-dev/iris/reader/parser"
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func Streamp(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(instance.StreamClass, obj) {
		return T, nil
	}
	return Nil, nil
}

func OpenStreamP(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	return T, nil
}

func InputStreamP(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if s, ok := obj.(instance.Stream); ok && s.Reader.Raw != nil {
		return T, nil
	}
	return Nil, nil
}

func OutputStreamP(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if s, ok := obj.(instance.Stream); ok && s.BufferedWriter.Raw != nil {
		return T, nil
	}
	return Nil, nil
}

func StandardInput(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	return e.StandardInput, nil
}

func StandardOutput(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	return e.StandardOutput, nil
}

func ErrorOutput(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	return e.ErrorOutput, nil
}

func WithStandardInput(e ilos.Environment, streamForm ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	e.StandardInput, err = Eval(e, streamForm)
	if err != nil {
		return nil, err
	}
	if ok, _ := InputStreamP(e, e.StandardInput); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, e.StandardInput, instance.StreamClass), Nil)
	}
	return Progn(e, forms...)
}

func WithStandardOutput(e ilos.Environment, streamForm ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	e.StandardOutput, err = Eval(e, streamForm)
	if err != nil {
		return nil, err
	}
	if ok, _ := OutputStreamP(e, e.StandardOutput); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, e.StandardOutput, instance.StreamClass), Nil)
	}
	return Progn(e, forms...)
}

func WithErrorOutput(e ilos.Environment, streamForm ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	e.ErrorOutput, err = Eval(e, streamForm)
	if err != nil {
		return nil, err
	}
	if ok, _ := OutputStreamP(e, e.ErrorOutput); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, e.ErrorOutput, instance.StreamClass), Nil)
	}
	return Progn(e, forms...)
}

func OpenInputFile(e ilos.Environment, filename ilos.Instance, elementClass ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := Stringp(e, filename); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, filename, instance.StringClass), Nil)
	}
	file, err := os.Open(string(filename.(instance.String)))
	if err != nil {
		return SignalCondition(e, instance.NewStreamError(e), Nil)
	}
	var ec ilos.Instance
	if len(elementClass) == 0 {
		ec = instance.CharacterClass
	}
	if len(elementClass) == 1 {
		ec = elementClass[0]
	}
	return instance.NewStream(file, nil, ec), nil
}

func OpenOutputFile(e ilos.Environment, filename ilos.Instance, elementClass ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := Stringp(e, filename); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, filename, instance.StringClass), Nil)
	}
	rawFilename := string(filename.(instance.String))
	file, err := os.Create(rawFilename)
	if err != nil {
		return SignalCondition(e, instance.NewStreamError(e), Nil)
	}
	var ec ilos.Instance
	if len(elementClass) == 0 {
		ec = instance.CharacterClass
	}
	if len(elementClass) == 1 {
		ec = elementClass[0]
	}
	return instance.NewStream(nil, file, ec), nil
}

func OpenIoFile(e ilos.Environment, filename ilos.Instance, elementClass ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := Stringp(e, filename); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, filename, instance.StringClass), Nil)
	}
	file, err := os.Open(string(filename.(instance.String)))
	if err != nil {
		return SignalCondition(e, instance.NewStreamError(e), Nil)
	}
	var ec ilos.Instance
	if len(elementClass) == 0 {
		ec = instance.CharacterClass
	}
	if len(elementClass) == 1 {
		ec = elementClass[0]
	}
	return instance.NewStream(file, file, ec), nil
}

func WithOpenInputFile(e ilos.Environment, fileSpec ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if !ilos.InstanceOf(instance.ListClass, fileSpec) {
		return SignalCondition(e, instance.NewDomainError(e, fileSpec, instance.ListClass), Nil)
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
	p, err := Eval(e, cadr)
	if err != nil {
		return nil, err
	}
	s, err := OpenInputFile(e, p)
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

func WithOpenOutputFile(e ilos.Environment, fileSpec ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if !ilos.InstanceOf(instance.ListClass, fileSpec) {
		return SignalCondition(e, instance.NewDomainError(e, fileSpec, instance.ListClass), Nil)
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
	p, err := Eval(e, cadr)
	if err != nil {
		return nil, err
	}
	s, err := OpenOutputFile(e, p)
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

func WithOpenIoFile(e ilos.Environment, fileSpec ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if !ilos.InstanceOf(instance.ListClass, fileSpec) {
		return SignalCondition(e, instance.NewDomainError(e, fileSpec, instance.ListClass), Nil)
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
	p, err := Eval(e, cadr)
	if err != nil {
		return nil, err
	}
	s, err := OpenOutputFile(e, p)
	if err != nil {
		return nil, err
	}
	e.Variable.Define(car, cadr)
	r, err := Progn(e, forms...)
	e.Variable.Delete(car)
	if _, err := Close(e, s); err != nil {
		return nil, err
	}
	return r, err
}

func Close(e ilos.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	// It works on file or std stream.
	if ok, _ := Streamp(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, instance.StreamClass), Nil)
	}
	if stream.(instance.Stream).Reader.Raw != nil {
		file, ok := stream.(instance.Stream).Reader.Raw.(*os.File)
		if ok {
			file.Close()
		} else {
			// Close is only for file pointer
			return SignalCondition(e, instance.NewStreamError(e), Nil)
		}
	}
	if stream.(instance.Stream).BufferedWriter.Raw != nil {
		file, ok := stream.(instance.Stream).BufferedWriter.Raw.(*os.File)
		if ok {
			stream.(instance.Stream).Flush()
			file.Close()
		} else {
			// Close is only for file pointer
			return SignalCondition(e, instance.NewStreamError(e), Nil)
		}
	}
	return Nil, nil
}

func FinishOutput(e ilos.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	// It works on file or std stream.
	if ok, _ := Streamp(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, instance.StreamClass), Nil)
	}
	if stream.(instance.Stream).Writer != nil {
		stream.(instance.Stream).Writer.Flush()
	}
	return Nil, nil
}

func CreateStringInputStream(e ilos.Environment, str ilos.Instance) (ilos.Instance, ilos.Instance) {
	return instance.NewStream(strings.NewReader(string(str.(instance.String))), nil, instance.CharacterClass), nil
}

func CreateStringOutputStream(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	return instance.NewStream(nil, new(bytes.Buffer), instance.CharacterClass), nil
}

func GetOutputStreamString(e ilos.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OutputStreamP(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, instance.StreamClass), Nil)
	}
	stream.(instance.Stream).Flush()
	out := instance.NewString([]rune(stream.(instance.Stream).BufferedWriter.Raw.(*bytes.Buffer).String()))
	stream.(instance.Stream).BufferedWriter.Raw.(*bytes.Buffer).Reset()
	return out, nil
}

func Read(e ilos.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if b, _ := InputStreamP(e, s); b == Nil {
		return SignalCondition(e, instance.NewDomainError(e, s, instance.StreamClass), Nil)
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
	if err != nil && ilos.InstanceOf(instance.EndOfStreamClass, err) {
		if eosErrorP {
			return nil, err
		}
		return eosValue, nil
	}
	return v, nil
}

func ReadChar(e ilos.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if ok, _ := InputStreamP(e, s); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, s, instance.StreamClass), Nil)
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
	v, _, err := s.(instance.Stream).ReadRune()
	if err != nil {
		if eosErrorP {
			return nil, instance.Create(e, instance.EndOfStreamClass)
		}
		return eosValue, nil
	}
	return instance.NewCharacter(v), nil
}

func ProbeFile(e ilos.Environment, fs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if len(fs) != 1 {
		return SignalCondition(e, instance.NewArityError(e), Nil)
	}
	if t, err := Stringp(e, fs[0]); err != nil || t == Nil {
		return SignalCondition(e, instance.NewDomainError(e, fs[0], instance.StringClass), Nil)
	}
	if _, err := os.Stat(string(fs[0].(instance.String))); os.IsNotExist(err) {
		return Nil, nil
	} else {
		return T, nil
	}
}

func PreviewChar(e ilos.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if ok, _ := InputStreamP(e, s); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, s, instance.StreamClass), Nil)
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
	bytes, err := s.(instance.Stream).Peek(1)
	if err != nil {
		if eosErrorP {
			return nil, instance.Create(e, instance.EndOfStreamClass)
		}
		return eosValue, nil
	}
	return instance.NewCharacter(rune(bytes[0])), nil
}

func ReadLine(e ilos.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if ok, _ := InputStreamP(e, s); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, s, instance.StreamClass), Nil)
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
	v, _, err := s.(instance.Stream).ReadLine()
	if err != nil {
		if eosErrorP {
			return nil, instance.Create(e, instance.EndOfStreamClass)
		}
		return eosValue, nil
	}
	return instance.NewString([]rune(string(v))), nil
}

func StreamReadyP(e ilos.Environment, inputStream ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: stream-ready-p
	return T, nil
}
