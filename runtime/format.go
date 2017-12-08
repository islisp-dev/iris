package runtime

import (
	"fmt"
	"regexp"
	"strconv"

	"github.com/asciian/iris/runtime/env"
	"github.com/asciian/iris/runtime/ilos"
	"github.com/asciian/iris/runtime/ilos/class"
	"github.com/asciian/iris/runtime/ilos/instance"
)

func FormatObject(e env.Environment, stream, object, escapep ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OpenStreamP(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if escapep == T {
		fmt.Fprint(stream.(instance.Stream), object)
		return Nil, nil
	}
	if ok, _ := Stringp(e, object); ok == T {
		fmt.Fprint(stream.(instance.Stream), string(object.(instance.String)))
		return Nil, nil
	}
	if ok, _ := Characterp(e, object); ok == T {
		fmt.Fprint(stream.(instance.Stream), string(object.(instance.Character)))
		return Nil, nil
	}
	fmt.Fprint(stream.(instance.Stream), object)
	return Nil, nil
}

func FormatChar(e env.Environment, stream, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OpenStreamP(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if ok, _ := Characterp(e, object); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, object, class.Character), Nil)
	}
	fmt.Fprint(stream.(instance.Stream), string(object.(instance.Character)))
	return Nil, nil
}

func FormatFloat(e env.Environment, stream, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OpenStreamP(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if ok, _ := Floatp(e, object); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, object, class.Float), Nil)
	}
	fmt.Fprint(stream.(instance.Stream), float64(object.(instance.Float)))
	return Nil, nil
}

func FormatInteger(e env.Environment, stream, object, radix ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OpenStreamP(e, stream); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, stream, class.Stream), Nil)
	}
	if ok, _ := Integerp(e, object); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, object, class.Integer), Nil)
	}
	if ok, _ := Integerp(e, radix); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, radix, class.Integer), Nil)
	}
	i := int(object.(instance.Integer))
	r := int(radix.(instance.Integer))
	fmt.Fprint(stream.(instance.Stream), strconv.FormatInt(int64(i), r))
	return Nil, nil
}

func FormatTab(e env.Environment, stream, num ilos.Instance) (ilos.Instance, ilos.Instance) {
	n := int(num.(instance.Integer))
	if *stream.(instance.Stream).Column < n {
		for i := *stream.(instance.Stream).Column; i < n; i++ {
			if _, err := FormatChar(e, stream, instance.NewCharacter(' ')); err != nil {
				return nil, err
			}
		}
		return Nil, nil
	}
	return FormatChar(e, stream, instance.NewCharacter(' '))
}

func FormatFreshLine(e env.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	if *stream.(instance.Stream).Column != 0 {
		return FormatChar(e, stream, instance.NewCharacter('\n'))
	}
	return Nil, nil
}

func Format(e env.Environment, stream, formatString ilos.Instance, formatArguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := Stringp(e, formatString); ok == Nil {
		return SignalCondition(e, instance.NewDomainError(e, formatString, class.String), Nil)
	}
	str := string(formatString.(instance.String))
	re := regexp.MustCompile(`~(?:[0-9]+[RT]|.)`)
	start, end, index := 0, 0, 0
	for loc := re.FindStringIndex(str[start:]); loc != nil; loc = re.FindStringIndex(str[start:]) {
		base := start
		start = base + 0
		end = base + loc[0]
		if _, err := FormatObject(e, stream, instance.NewString([]rune(str[start:end])), Nil); err != nil {
			return nil, err
		}
		start = base + loc[0]
		end = base + loc[1]
		var err ilos.Instance
		switch str[start:end] {
		case "~A":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, instance.NewArityError(e), Nil)
			} else {
				_, err = FormatObject(e, stream, formatArguments[index], Nil)
				index++
			}
		case "~B":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, instance.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(2))
				index++
			}
		case "~C":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, instance.NewArityError(e), Nil)
			} else {
				_, err = FormatChar(e, stream, formatArguments[index])
				index++
			}
		case "~D":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, instance.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(10))
				index++
			}
		case "~G":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, instance.NewArityError(e), Nil)
			} else {
				_, err = FormatFloat(e, stream, formatArguments[index])
				index++
			}
		case "~O":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, instance.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(8))
				index++
			}
		case "~S":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, instance.NewArityError(e), Nil)
			} else {
				_, err = FormatObject(e, stream, formatArguments[index], T)
				index++
			}
		case "~X":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, instance.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(16))
				index++
			}
		case "~%":
			_, err = FormatChar(e, stream, instance.NewCharacter('\n'))
		case "~&":
			_, err = FormatFreshLine(e, stream)
		case "~~":
			_, err = FormatChar(e, stream, instance.NewCharacter('~'))
		default:
			s := str[start:end]
			if len(s) > 2 {
				if s[len(s)-1] == 'R' {
					if index >= len(formatArguments) {
						_, err = SignalCondition(e, instance.NewArityError(e), Nil)
					} else {
						n, _ := strconv.Atoi(s[1 : len(s)-1])
						if n < 2 || 36 < n {
							_, err = SignalCondition(e, instance.NewDomainError(e, instance.NewInteger(n), class.Integer), Nil)
						} else {
							_, err = FormatInteger(e, stream, formatArguments[index], instance.NewInteger(n))
							index++
						}
					}
				}
				if s[len(s)-1] == 'T' {
					n, _ := strconv.Atoi(s[1 : len(s)-1])
					_, err = FormatTab(e, stream, instance.NewInteger(n))
				}
			}
		}
		if err != nil {
			return nil, err
		}
		start = end
	}
	return FormatObject(e, stream, instance.NewString([]rune(str[start:])), Nil)
}
