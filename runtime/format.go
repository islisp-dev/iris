package runtime

import (
	"fmt"
	"regexp"
	"strconv"

	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func FormatObject(e env.Environment, stream, object, escapep ilos.Instance) (ilos.Instance, ilos.Instance) {
	ok, err := OpenStreamP(e, stream)
	if err != nil || ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if escapep == T {
		fmt.Fprint(stream.(instance.Stream).Writer, object)
		return Nil, nil
	}
	if ok, _ := Stringp(e, object); ok == T {
		fmt.Fprint(stream.(instance.Stream).Writer, string(object.(instance.String)))
		return Nil, nil
	}
	if ok, _ := Characterp(e, object); ok == T {
		fmt.Fprint(stream.(instance.Stream).Writer, string(object.(instance.Character)))
		return Nil, nil
	}
	fmt.Fprint(stream.(instance.Stream).Writer, object)
	return Nil, nil
}

func FormatChar(e env.Environment, stream, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	ok, err := OpenStreamP(e, stream)
	if err != nil || ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if ok, err := Characterp(e, object); ok == Nil || err != nil {
		return SignalCondition(e, instance.NewDomainError(e, object, class.Character), Nil)
	}
	fmt.Fprint(stream.(instance.Stream).Writer, rune(object.(instance.Character)))
	return Nil, nil
}

func FormatFloat(e env.Environment, stream, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	ok, err := OpenStreamP(e, stream)
	if err != nil || ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if ok, err := Floatp(e, object); ok == Nil || err != nil {
		return SignalCondition(e, instance.NewDomainError(e, object, class.Float), Nil)
	}
	fmt.Fprint(stream.(instance.Stream).Writer, float64(object.(instance.Float)))
	return Nil, nil
}

func FormatInteger(e env.Environment, stream, object, radix ilos.Instance) (ilos.Instance, ilos.Instance) {
	ok, err := OpenStreamP(e, stream)
	if err != nil || ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if ok, err := Integerp(e, object); ok == Nil || err != nil {
		return SignalCondition(e, instance.NewDomainError(e, object, class.Integer), Nil)
	}
	if ok, err := Integerp(e, radix); ok == Nil || err != nil {
		return SignalCondition(e, instance.NewDomainError(e, radix, class.Integer), Nil)
	}
	i := int(object.(instance.Integer))
	r := int(radix.(instance.Integer))
	fmt.Fprint(stream.(instance.Stream).Writer, strconv.FormatInt(int64(i), r))
	return Nil, nil
}

func FormatTab(e env.Environment, stream, num ilos.Instance) (ilos.Instance, ilos.Instance) {
	return FormatChar(e, stream, instance.NewCharacter(' '))
}

func FormatFreshLine(e env.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	return FormatChar(e, stream, instance.NewCharacter(rune("\n"[0])))
}

func Format(env env.Environment, stream, formatString ilos.Instance, formatArguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	str := string(formatString.(instance.String))
	re := regexp.MustCompile(`~(?:[ABCDGOSX%&~]|[0-9]+[RT])`)
	s, e, c := 0, 0, 0
	for loc := re.FindStringIndex(str[s:]); loc != nil; loc = re.FindStringIndex(str[s:]) {
		b := s
		s = b + 0
		e = b + loc[0]
		FormatObject(env, stream, instance.NewString([]rune(str[s:e])), Nil)
		s = b + loc[0]
		e = b + loc[1]
		if str[s:e] == "~A" {
			FormatObject(env, stream, formatArguments[c], Nil)
			c++
			s = e
			continue
		}
		if str[s:e] == "~B" {
			FormatInteger(env, stream, formatArguments[c], instance.NewInteger(2))
			c++
			s = e
			continue
		}
		if str[s:e] == "~C" {
			FormatChar(env, stream, formatArguments[c])
			c++
			s = e
			continue
		}
		if str[s:e] == "~D" {
			FormatInteger(env, stream, formatArguments[c], instance.NewInteger(10))
			c++
			s = e
			continue
		}
		if str[s:e] == "~G" {
			FormatFloat(env, stream, formatArguments[c])
			c++
			s = e
			continue
		}
		if str[s:e] == "~O" {
			FormatInteger(env, stream, formatArguments[c], instance.NewInteger(8))
			c++
			s = e
			continue
		}
		if str[s:e] == "~S" {
			FormatObject(env, stream, formatArguments[c], T)
			c++
			s = e
			continue
		}
		if str[s:e] == "~X" {
			FormatInteger(env, stream, formatArguments[c], instance.NewInteger(16))
			c++
			s = e
			continue
		}
		if str[s:e] == "~%" {
			FormatChar(env, stream, instance.NewCharacter(rune("\n"[0])))
			s = e
			continue
		}
		if str[s:e] == "~&" {
			FormatFreshLine(env, stream)
			s = e
			continue
		}
		if str[s:e] == "~~" {
			FormatChar(env, stream, instance.NewCharacter('~'))
			s = e
			continue
		}
		if str[len(str)-1] == 'R' {
			n, _ := strconv.Atoi(str[1 : len(str)-1])
			FormatInteger(env, stream, formatArguments[c], instance.NewInteger(n))
			c++
			s = e
			continue
		}
		if str[len(str)-1] == 'T' {
			n, _ := strconv.Atoi(str[1 : len(str)-1])
			FormatTab(env, stream, instance.NewInteger(n))
			s = e
			continue
		}
	}
	fmt.Print(str[s:])
	return Nil, nil
}
