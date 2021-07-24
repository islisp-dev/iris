package runtime

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/islisp-dev/iris/runtime/ilos"
)

func FormatObject(e ilos.Environment, stream, object, escapep ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OpenStreamP(e, stream); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, stream, ilos.StreamClass), Nil)
	}
	if escapep == T {
		fmt.Fprint(stream.(ilos.Stream), object)
		return Nil, nil
	}
	if ok, _ := Stringp(e, object); ok == T {
		fmt.Fprint(stream.(ilos.Stream), string(object.(ilos.String)))
		return Nil, nil
	}
	if ok, _ := Characterp(e, object); ok == T {
		fmt.Fprint(stream.(ilos.Stream), string(object.(ilos.Character)))
		return Nil, nil
	}
	fmt.Fprint(stream.(ilos.Stream), object)
	return Nil, nil
}

func FormatChar(e ilos.Environment, stream, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OpenStreamP(e, stream); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, stream, ilos.StreamClass), Nil)
	}
	if ok, _ := Characterp(e, object); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, object, ilos.CharacterClass), Nil)
	}
	fmt.Fprint(stream.(ilos.Stream), string(object.(ilos.Character)))
	return Nil, nil
}

func FormatFloat(e ilos.Environment, stream, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OpenStreamP(e, stream); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, stream, ilos.StreamClass), Nil)
	}
	if ok, _ := Floatp(e, object); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, object, ilos.FloatClass), Nil)
	}
	fmt.Fprint(stream.(ilos.Stream), float64(object.(ilos.Float)))
	return Nil, nil
}

func FormatInteger(e ilos.Environment, stream, object, radix ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := OpenStreamP(e, stream); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, stream, ilos.StreamClass), Nil)
	}
	if ok, _ := Integerp(e, object); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, object, ilos.IntegerClass), Nil)
	}
	if ok, _ := Integerp(e, radix); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, radix, ilos.IntegerClass), Nil)
	}
	i := int(object.(ilos.Integer))
	r := int(radix.(ilos.Integer))
	fmt.Fprint(stream.(ilos.Stream), strings.ToUpper(strconv.FormatInt(int64(i), r)))
	return Nil, nil
}

func FormatTab(e ilos.Environment, stream, num ilos.Instance) (ilos.Instance, ilos.Instance) {
	n := int(num.(ilos.Integer))
	if *stream.(ilos.Stream).Column < n {
		for i := *stream.(ilos.Stream).Column; i < n; i++ {
			if _, err := FormatChar(e, stream, ilos.NewCharacter(' ')); err != nil {
				return nil, err
			}
		}
		return Nil, nil
	}
	return FormatChar(e, stream, ilos.NewCharacter(' '))
}

func FormatFreshLine(e ilos.Environment, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	if *stream.(ilos.Stream).Column != 0 {
		return FormatChar(e, stream, ilos.NewCharacter('\n'))
	}
	return Nil, nil
}

func Format(e ilos.Environment, stream, formatString ilos.Instance, formatArguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ok, _ := Stringp(e, formatString); ok == Nil {
		return SignalCondition(e, ilos.NewDomainError(e, formatString, ilos.StringClass), Nil)
	}
	str := string(formatString.(ilos.String))
	re := regexp.MustCompile(`~(?:[0-9]+[RT]|.)`)
	start, end, index := 0, 0, 0
	for loc := re.FindStringIndex(str[start:]); loc != nil; loc = re.FindStringIndex(str[start:]) {
		base := start
		start = base + 0
		end = base + loc[0]
		if _, err := FormatObject(e, stream, ilos.NewString([]rune(str[start:end])), Nil); err != nil {
			return nil, err
		}
		start = base + loc[0]
		end = base + loc[1]
		var err ilos.Instance
		switch str[start:end] {
		case "~A":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, ilos.NewArityError(e), Nil)
			} else {
				_, err = FormatObject(e, stream, formatArguments[index], Nil)
				index++
			}
		case "~B":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, ilos.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], ilos.NewInteger(2))
				index++
			}
		case "~C":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, ilos.NewArityError(e), Nil)
			} else {
				_, err = FormatChar(e, stream, formatArguments[index])
				index++
			}
		case "~D":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, ilos.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], ilos.NewInteger(10))
				index++
			}
		case "~G":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, ilos.NewArityError(e), Nil)
			} else {
				_, err = FormatFloat(e, stream, formatArguments[index])
				index++
			}
		case "~O":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, ilos.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], ilos.NewInteger(8))
				index++
			}
		case "~S":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, ilos.NewArityError(e), Nil)
			} else {
				_, err = FormatObject(e, stream, formatArguments[index], T)
				index++
			}
		case "~X":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, ilos.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], ilos.NewInteger(16))
				index++
			}
		case "~%":
			_, err = FormatChar(e, stream, ilos.NewCharacter('\n'))
		case "~&":
			_, err = FormatFreshLine(e, stream)
		case "~~":
			_, err = FormatChar(e, stream, ilos.NewCharacter('~'))
		default:
			s := str[start:end]
			if len(s) > 2 {
				if s[len(s)-1] == 'R' {
					if index >= len(formatArguments) {
						_, err = SignalCondition(e, ilos.NewArityError(e), Nil)
					} else {
						n, _ := strconv.Atoi(s[1 : len(s)-1])
						if n < 2 || 36 < n {
							_, err = SignalCondition(e, ilos.NewDomainError(e, ilos.NewInteger(n), ilos.IntegerClass), Nil)
						} else {
							_, err = FormatInteger(e, stream, formatArguments[index], ilos.NewInteger(n))
							index++
						}
					}
				}
				if s[len(s)-1] == 'T' {
					n, _ := strconv.Atoi(s[1 : len(s)-1])
					_, err = FormatTab(e, stream, ilos.NewInteger(n))
				}
			}
		}
		if err != nil {
			return nil, err
		}
		start = end
	}
	return FormatObject(e, stream, ilos.NewString([]rune(str[start:])), Nil)
}
