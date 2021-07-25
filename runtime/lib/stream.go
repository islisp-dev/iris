// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import (
	"bytes"
	"os"
	"strings"

	"github.com/islisp-dev/iris/reader/parser"
	"github.com/islisp-dev/iris/runtime/core"
)

func Streamp(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.StreamClass, obj) {
		return T, nil
	}
	return Nil, nil
}

func OpenStreamP(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	return T, nil
}

func InputStreamP(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if s, ok := obj.(core.Stream); ok && s.Reader.Raw != nil {
		return T, nil
	}
	return Nil, nil
}

func OutputStreamP(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if s, ok := obj.(core.Stream); ok && s.BufferedWriter.Raw != nil {
		return T, nil
	}
	return Nil, nil
}

func StandardInput(e core.Environment) (core.Instance, core.Instance) {
	return e.StandardInput, nil
}

func StandardOutput(e core.Environment) (core.Instance, core.Instance) {
	return e.StandardOutput, nil
}

func ErrorOutput(e core.Environment) (core.Instance, core.Instance) {
	return e.ErrorOutput, nil
}

func WithStandardInput(e core.Environment, streamForm core.Instance, forms ...core.Instance) (core.Instance, core.Instance) {
	var err core.Instance
	e.StandardInput, err = Eval(e, streamForm)
	if err != nil {
		return nil, err
	}
	if ok, _ := InputStreamP(e, e.StandardInput); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, e.StandardInput, core.StreamClass), Nil)
	}
	return Progn(e, forms...)
}

func WithStandardOutput(e core.Environment, streamForm core.Instance, forms ...core.Instance) (core.Instance, core.Instance) {
	var err core.Instance
	e.StandardOutput, err = Eval(e, streamForm)
	if err != nil {
		return nil, err
	}
	if ok, _ := OutputStreamP(e, e.StandardOutput); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, e.StandardOutput, core.StreamClass), Nil)
	}
	return Progn(e, forms...)
}

func WithErrorOutput(e core.Environment, streamForm core.Instance, forms ...core.Instance) (core.Instance, core.Instance) {
	var err core.Instance
	e.ErrorOutput, err = Eval(e, streamForm)
	if err != nil {
		return nil, err
	}
	if ok, _ := OutputStreamP(e, e.ErrorOutput); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, e.ErrorOutput, core.StreamClass), Nil)
	}
	return Progn(e, forms...)
}

func OpenInputFile(e core.Environment, filename core.Instance, elementClass ...core.Instance) (core.Instance, core.Instance) {
	if ok, _ := Stringp(e, filename); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, filename, core.StringClass), Nil)
	}
	file, err := os.Open(string(filename.(core.String)))
	if err != nil {
		return SignalCondition(e, core.NewStreamError(e), Nil)
	}
	var ec core.Instance
	if len(elementClass) == 0 {
		ec = core.CharacterClass
	}
	if len(elementClass) == 1 {
		ec = elementClass[0]
	}
	return core.NewStream(file, nil, ec), nil
}

func OpenOutputFile(e core.Environment, filename core.Instance, elementClass ...core.Instance) (core.Instance, core.Instance) {
	if ok, _ := Stringp(e, filename); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, filename, core.StringClass), Nil)
	}
	rawFilename := string(filename.(core.String))
	file, err := os.Create(rawFilename)
	if err != nil {
		return SignalCondition(e, core.NewStreamError(e), Nil)
	}
	var ec core.Instance
	if len(elementClass) == 0 {
		ec = core.CharacterClass
	}
	if len(elementClass) == 1 {
		ec = elementClass[0]
	}
	return core.NewStream(nil, file, ec), nil
}

func OpenIoFile(e core.Environment, filename core.Instance, elementClass ...core.Instance) (core.Instance, core.Instance) {
	if ok, _ := Stringp(e, filename); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, filename, core.StringClass), Nil)
	}
	file, err := os.Open(string(filename.(core.String)))
	if err != nil {
		return SignalCondition(e, core.NewStreamError(e), Nil)
	}
	var ec core.Instance
	if len(elementClass) == 0 {
		ec = core.CharacterClass
	}
	if len(elementClass) == 1 {
		ec = elementClass[0]
	}
	return core.NewStream(file, file, ec), nil
}

func WithOpenInputFile(e core.Environment, fileSpec core.Instance, forms ...core.Instance) (core.Instance, core.Instance) {
	if !core.InstanceOf(core.ListClass, fileSpec) {
		return SignalCondition(e, core.NewDomainError(e, fileSpec, core.ListClass), Nil)
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

func WithOpenOutputFile(e core.Environment, fileSpec core.Instance, forms ...core.Instance) (core.Instance, core.Instance) {
	if !core.InstanceOf(core.ListClass, fileSpec) {
		return SignalCondition(e, core.NewDomainError(e, fileSpec, core.ListClass), Nil)
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

func WithOpenIoFile(e core.Environment, fileSpec core.Instance, forms ...core.Instance) (core.Instance, core.Instance) {
	if !core.InstanceOf(core.ListClass, fileSpec) {
		return SignalCondition(e, core.NewDomainError(e, fileSpec, core.ListClass), Nil)
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

func Close(e core.Environment, stream core.Instance) (core.Instance, core.Instance) {
	// It works on file or std stream.
	if ok, _ := Streamp(e, stream); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, stream, core.StreamClass), Nil)
	}
	if stream.(core.Stream).Reader.Raw != nil {
		file, ok := stream.(core.Stream).Reader.Raw.(*os.File)
		if ok {
			file.Close()
		} else {
			// Close is only for file pointer
			return SignalCondition(e, core.NewStreamError(e), Nil)
		}
	}
	if stream.(core.Stream).BufferedWriter.Raw != nil {
		file, ok := stream.(core.Stream).BufferedWriter.Raw.(*os.File)
		if ok {
			stream.(core.Stream).Flush()
			file.Close()
		} else {
			// Close is only for file pointer
			return SignalCondition(e, core.NewStreamError(e), Nil)
		}
	}
	return Nil, nil
}

func FinishOutput(e core.Environment, stream core.Instance) (core.Instance, core.Instance) {
	// It works on file or std stream.
	if ok, _ := Streamp(e, stream); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, stream, core.StreamClass), Nil)
	}
	if stream.(core.Stream).Writer != nil {
		stream.(core.Stream).Writer.Flush()
	}
	return Nil, nil
}

func CreateStringInputStream(e core.Environment, str core.Instance) (core.Instance, core.Instance) {
	return core.NewStream(strings.NewReader(string(str.(core.String))), nil, core.CharacterClass), nil
}

func CreateStringOutputStream(e core.Environment) (core.Instance, core.Instance) {
	return core.NewStream(nil, new(bytes.Buffer), core.CharacterClass), nil
}

func GetOutputStreamString(e core.Environment, stream core.Instance) (core.Instance, core.Instance) {
	if ok, _ := OutputStreamP(e, stream); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, stream, core.StreamClass), Nil)
	}
	stream.(core.Stream).Flush()
	out := core.NewString([]rune(stream.(core.Stream).BufferedWriter.Raw.(*bytes.Buffer).String()))
	stream.(core.Stream).BufferedWriter.Raw.(*bytes.Buffer).Reset()
	return out, nil
}

func Read(e core.Environment, options ...core.Instance) (core.Instance, core.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if b, _ := InputStreamP(e, s); core.DeepEqual(b, Nil) {
		return SignalCondition(e, core.NewDomainError(e, s, core.StreamClass), Nil)
	}
	eosErrorP := true
	if len(options) > 1 {
		if core.DeepEqual(options[1], Nil) {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(options) > 2 {
		if core.DeepEqual(options[2], Nil) {
			eosValue = options[2]
		}
	}
	v, err := parser.Parse(e, s.(core.Stream).Reader)
	if err != nil && core.InstanceOf(core.EndOfStreamClass, err) {
		if eosErrorP {
			return nil, err
		}
		return eosValue, nil
	}
	return v, nil
}

func ReadChar(e core.Environment, options ...core.Instance) (core.Instance, core.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if ok, _ := InputStreamP(e, s); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, s, core.StreamClass), Nil)
	}
	eosErrorP := true
	if len(options) > 1 {
		if core.DeepEqual(options[1], Nil) {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(options) > 2 {
		if core.DeepEqual(options[2], Nil) {
			eosValue = options[2]
		}
	}
	//v, _, err := bufio.NewReader(s.(core.Stream).Reader).ReadRune()
	v, _, err := s.(core.Stream).ReadRune()
	if err != nil {
		if eosErrorP {
			return nil, core.Create(e, core.EndOfStreamClass)
		}
		return eosValue, nil
	}
	return core.NewCharacter(v), nil
}

func ProbeFile(e core.Environment, fs ...core.Instance) (core.Instance, core.Instance) {
	if len(fs) != 1 {
		return SignalCondition(e, core.NewArityError(e), Nil)
	}
	if t, err := Stringp(e, fs[0]); err != nil || core.DeepEqual(t, Nil) {
		return SignalCondition(e, core.NewDomainError(e, fs[0], core.StringClass), Nil)
	}
	if _, err := os.Stat(string(fs[0].(core.String))); os.IsNotExist(err) {
		return Nil, nil
	} else {
		return T, nil
	}
}

func PreviewChar(e core.Environment, options ...core.Instance) (core.Instance, core.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if ok, _ := InputStreamP(e, s); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, s, core.StreamClass), Nil)
	}
	eosErrorP := true
	if len(options) > 1 {
		if core.DeepEqual(options[1], Nil) {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(options) > 2 {
		if core.DeepEqual(options[2], Nil) {
			eosValue = options[2]
		}
	}
	//v, _, err := bufio.NewReader(s.(core.Stream).Reader).ReadRune()
	bytes, err := s.(core.Stream).Peek(1)
	if err != nil {
		if eosErrorP {
			return nil, core.Create(e, core.EndOfStreamClass)
		}
		return eosValue, nil
	}
	return core.NewCharacter(rune(bytes[0])), nil
}

func ReadLine(e core.Environment, options ...core.Instance) (core.Instance, core.Instance) {
	s := e.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if ok, _ := InputStreamP(e, s); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, s, core.StreamClass), Nil)
	}
	eosErrorP := true
	if len(options) > 1 {
		if core.DeepEqual(options[1], Nil) {
			eosErrorP = false
		}
	}
	eosValue := Nil
	if len(options) > 2 {
		if core.DeepEqual(options[2], Nil) {
			eosValue = options[2]
		}
	}
	v, _, err := s.(core.Stream).ReadLine()
	if err != nil {
		if eosErrorP {
			return nil, core.Create(e, core.EndOfStreamClass)
		}
		return eosValue, nil
	}
	return core.NewString([]rune(string(v))), nil
}

func StreamReadyP(e core.Environment, inputStream core.Instance) (core.Instance, core.Instance) {
	// TODO: stream-ready-p
	return T, nil
}
