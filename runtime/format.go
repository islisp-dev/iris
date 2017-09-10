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

func Format(e env.Environment, stream, formatString ilos.Instance, formatArguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := Stringp(e, formatString); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, formatString, class.String), Nil)
	}
	str := string(formatString.(instance.String))
	re := regexp.MustCompile(`~(?:[ABCDGOSX%&~]|[0-9]+[RT])`)
	start, end, index := 0, 0, 0
	for loc := re.FindStringIndex(str[start:]); loc != nil; loc = re.FindStringIndex(str[start:]) {
		if index == len(formatArguments) {
			return SignalCondition(e, instance.NewArityError(e), Nil)
		}
		base := start
		start = base + 0
		end = base + loc[0]
		FormatObject(e, stream, instance.NewString([]rune(str[start:end])), Nil)
		start = base + loc[0]
		end = base + loc[1]
		var err ilos.Instance
		switch str[start:end] {
		case "~A":
			_, err = FormatObject(e, stream, formatArguments[index], Nil)
			index++
		case "~B":
			_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(2))
			index++
		case "~C":
			_, err = FormatChar(e, stream, formatArguments[index])
			index++
		case "~D":
			_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(10))
			index++
		case "~G":
			_, err = FormatFloat(e, stream, formatArguments[index])
			index++
		case "~O":
			_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(8))
			index++
		case "~S":
			_, err = FormatObject(e, stream, formatArguments[index], T)
			index++
		case "~X":
			_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(16))
			index++
		case "~%":
			_, err = FormatChar(e, stream, instance.NewCharacter(rune("\n"[0])))
		case "~&":
			_, err = FormatFreshLine(e, stream)
		case "~~":
			_, err = FormatChar(e, stream, instance.NewCharacter('~'))
		default:
			if str[len(str)-1] == 'R' {
				n, _ := strconv.Atoi(str[1 : len(str)-1])
				_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(n))
				index++
			}
			if str[len(str)-1] == 'T' {
				n, _ := strconv.Atoi(str[1 : len(str)-1])
				_, err = FormatTab(e, stream, instance.NewInteger(n))
			}
		}
		if err != nil {
			return nil, err
		}
		start = end
	}
	fmt.Print(str[start:])
	return Nil, nil
}
