// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"regexp"
	"strings"

	"github.com/ta2gch/iris/reader/parser"
	"github.com/ta2gch/iris/reader/tokenizer"
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func Streamp(local, global environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(class.Stream, obj) {
		return T, nil
	}
	return Nil, nil
}

func OpenStreamP(local, global environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	return T, nil
}

func InputStreamp(local, global environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if s, ok := obj.(instance.Stream); ok && s.Reader != nil {
		return T, nil
	}
	return Nil, nil
}

func OutputStreamp(local, global environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if s, ok := obj.(instance.Stream); ok && s.Writer != nil {
		return T, nil
	}
	return Nil, nil
}

func StandardInput(local, global environment.Environment) (ilos.Instance, ilos.Instance) {
	return local.StandardInput, nil
}

func StandardOutput(local, global environment.Environment) (ilos.Instance, ilos.Instance) {
	return local.StandardOutput, nil
}

func ErrorOutput(local, global environment.Environment) (ilos.Instance, ilos.Instance) {
	return local.ErrorOutput, nil
}

func WithStandardInput(local, global environment.Environment, streamForm ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	local.StandardInput, err = Eval(local, global, streamForm)
	if err != nil {
		return nil, err
	}
	if err := ensure(class.Stream, local.ErrorOutput); err != nil {
		return nil, err
	}
	return Progn(local, global, forms...)
}

func WithStandardOutput(local, global environment.Environment, streamForm ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	local.StandardOutput, err = Eval(local, global, streamForm)
	if err != nil {
		return nil, err
	}
	if err := ensure(class.Stream, local.ErrorOutput); err != nil {
		return nil, err
	}
	return Progn(local, global, forms...)
}

func WithErrorOutput(local, global environment.Environment, streamForm ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	local.ErrorOutput, err = Eval(local, global, streamForm)
	if err != nil {
		return nil, err
	}
	if err := ensure(class.Stream, local.ErrorOutput); err != nil {
		return nil, err
	}
	return Progn(local, global, forms...)
}

func OpenInputFile(local, global environment.Environment, filename ilos.Instance, elementClass ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: elementClass
	if err := ensure(class.String, filename); err != nil {
		return nil, err
	}
	file, err := os.Open(string(filename.(instance.String)))
	if err != nil {
		return nil, nil // Error File Not Found
	}
	return instance.NewStream(file, nil), nil
}

func OpenOutputFile(local, global environment.Environment, filename ilos.Instance, elementClass ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: elementClass
	if err := ensure(class.String, filename); err != nil {
		return nil, err
	}
	file, err := os.Open(string(filename.(instance.String)))
	if err != nil {
		return nil, nil // Error File Not Found
	}
	return instance.NewStream(nil, file), nil
}

func OpenIoFile(local, global environment.Environment, filename ilos.Instance, elementClass ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: elementClass
	if err := ensure(class.String, filename); err != nil {
		return nil, err
	}
	file, err := os.Open(string(filename.(instance.String)))
	if err != nil {
		return nil, nil // Error File Not Found
	}
	return instance.NewStream(file, file), nil
}

func WithOpenInputFile(local, global environment.Environment, fileSpec ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Cons, fileSpec); err != nil {
		return nil, err
	}
	n := fileSpec.(*instance.Cons).Car
	s, err := Eval(local, global, instance.NewCons(instance.NewSymbol("OPEN-INPUT-FILE"), fileSpec.(*instance.Cons).Cdr))
	if err != nil {
		return nil, err
	}
	local.Variable.Define(n, s)
	return Progn(local, global, forms...)
}

func WithOpenOutputFile(local, global environment.Environment, fileSpec ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Cons, fileSpec); err != nil {
		return nil, err
	}
	n := fileSpec.(*instance.Cons).Car
	s, err := Eval(local, global, instance.NewCons(instance.NewSymbol("OPEN-OUTPUT-FILE"), fileSpec.(*instance.Cons).Cdr))
	if err != nil {
		return nil, err
	}
	local.Variable.Define(n, s)
	return Progn(local, global, forms...)
}

func WithOpenIoFile(local, global environment.Environment, fileSpec ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Cons, fileSpec); err != nil {
		return nil, err
	}
	n := fileSpec.(*instance.Cons).Car
	s, err := Eval(local, global, instance.NewCons(instance.NewSymbol("OPEN-IO-FILE"), fileSpec.(*instance.Cons).Cdr))
	if err != nil {
		return nil, err
	}
	local.Variable.Define(n, s)
	return Progn(local, global, forms...)
}

func Close(local, global environment.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	// It works on file or std stream.
	if err := ensure(class.Stream, stream); err != nil {
		return nil, err
	}
	if stream.(instance.Stream).Reader != nil {
		stream.(instance.Stream).Reader.(*os.File).Close()
	}
	if stream.(instance.Stream).Writer != nil {
		stream.(instance.Stream).Writer.(*os.File).Close()
	}
	return Nil, nil
}

func FlushOutput(local, global environment.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	// It works on file or std stream.
	if err := ensure(class.Stream, stream); err != nil {
		return nil, err
	}
	if stream.(instance.Stream).Writer != nil {
		stream.(instance.Stream).Writer.(*os.File).Close()
	}
	return Nil, nil
}

func CreateStringInputStream(local, global environment.Environment, str ilos.Instance) (ilos.Instance, ilos.Instance) {
	return instance.NewStream(strings.NewReader(string(str.(instance.String))), nil), nil
}

func CreateStringOutputStream(local, global environment.Environment) (ilos.Instance, ilos.Instance) {
	return instance.NewStream(nil, new(bytes.Buffer)), nil
}

func GetOutputStreamString(local, global environment.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Stream, stream); err != nil {
		return nil, err
	}
	return instance.NewString(stream.(instance.Stream).Writer.(*bytes.Buffer).String()), nil
}

func Read(local, global environment.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := local.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if b, _ := InputStreamp(local, global, s); b == Nil {
		return nil, nil // throw Error
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
	v, err := parser.Parse(tokenizer.New(s.(instance.Stream).Reader))
	if ilos.InstanceOf(class.EndOfStream, err) {
		if eosErrorP {
			return nil, err
		}
		return eosValue, nil
	}
	return v, nil
}

func ReadChar(local, global environment.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := local.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if b, _ := InputStreamp(local, global, s); b == Nil {
		return nil, nil // throw Error
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
	v, _, err := bufio.NewReader(s.(instance.Stream).Reader).ReadRune()
	if err != nil {
		if eosErrorP {
			return nil, instance.Create(local, global, class.EndOfStream)
		}
		return eosValue, nil
	}
	return instance.NewCharacter(v), nil
}

func ReadLine(local, global environment.Environment, options ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	s := local.StandardInput
	if len(options) > 0 {
		s = options[0]
	}
	if b, _ := InputStreamp(local, global, s); b == Nil {
		return nil, nil // throw Error
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
			return nil, instance.Create(local, global, class.EndOfStream)
		}
		return eosValue, nil
	}
	return instance.NewString(string(v)), nil
}

// TODO: preview-char (Hint: Bufio.Rreader)

func StreamReadyP(local, global environment.Environment, inputStream ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO: stream-ready-p
	return T, nil
}

func Format(local, global environment.Environment, stream, formatString ilos.Instance, objs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	f := regexp.MustCompile("([^~])~A").ReplaceAllString(string(formatString.(instance.String)), "%1%v")
	f = regexp.MustCompile(`\`).ReplaceAllString(string(formatString.(instance.String)), `\\`)
	f = regexp.MustCompile("([^~])~%").ReplaceAllString(string(formatString.(instance.String)), "%1\n")
	if b, _ := OutputStreamp(local, global, stream); b == Nil {
		return nil, nil // throw Error
	}
	args := []interface{}{}
	for _, obj := range objs {
		args = append(args, obj)
	}
	fmt.Fprintf(stream.(instance.Stream).Writer, f, args...)
	return Nil, nil
}
