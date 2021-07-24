package lib

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/islisp-dev/iris/runtime/core"
)

func FormatObject(e core.Environment, stream, object, escapep core.Instance) (core.Instance, core.Instance) {
	if ok, _ := OpenStreamP(e, stream); ok == Nil {
		return SignalCondition(e, core.NewDomainError(e, stream, core.StreamClass), Nil)
	}
	if core.DeepEqual(escapep, T) {
		fmt.Fprint(stream.(core.Stream), object)
		return Nil, nil
	}
	if ok, _ := Stringp(e, object); core.DeepEqual(ok, T) {
		fmt.Fprint(stream.(core.Stream), string(object.(core.String)))
		return Nil, nil
	}
	if ok, _ := Characterp(e, object); core.DeepEqual(ok, T) {
		fmt.Fprint(stream.(core.Stream), string(object.(core.Character)))
		return Nil, nil
	}
	fmt.Fprint(stream.(core.Stream), object)
	return Nil, nil
}

func FormatChar(e core.Environment, stream, object core.Instance) (core.Instance, core.Instance) {
	if ok, _ := OpenStreamP(e, stream); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, stream, core.StreamClass), Nil)
	}
	if ok, _ := Characterp(e, object); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, object, core.CharacterClass), Nil)
	}
	fmt.Fprint(stream.(core.Stream), string(object.(core.Character)))
	return Nil, nil
}

func FormatFloat(e core.Environment, stream, object core.Instance) (core.Instance, core.Instance) {
	if ok, _ := OpenStreamP(e, stream); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, stream, core.StreamClass), Nil)
	}
	if ok, _ := Floatp(e, object); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, object, core.FloatClass), Nil)
	}
	fmt.Fprint(stream.(core.Stream), float64(object.(core.Float)))
	return Nil, nil
}

func FormatInteger(e core.Environment, stream, object, radix core.Instance) (core.Instance, core.Instance) {
	if ok, _ := OpenStreamP(e, stream); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, stream, core.StreamClass), Nil)
	}
	if ok, _ := Integerp(e, object); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, object, core.IntegerClass), Nil)
	}
	if ok, _ := Integerp(e, radix); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, radix, core.IntegerClass), Nil)
	}
	i := int(object.(core.Integer))
	r := int(radix.(core.Integer))
	fmt.Fprint(stream.(core.Stream), strings.ToUpper(strconv.FormatInt(int64(i), r)))
	return Nil, nil
}

func FormatTab(e core.Environment, stream, num core.Instance) (core.Instance, core.Instance) {
	n := int(num.(core.Integer))
	if *stream.(core.Stream).Column < n {
		for i := *stream.(core.Stream).Column; i < n; i++ {
			if _, err := FormatChar(e, stream, core.NewCharacter(' ')); err != nil {
				return nil, err
			}
		}
		return Nil, nil
	}
	return FormatChar(e, stream, core.NewCharacter(' '))
}

func FormatFreshLine(e core.Environment, stream core.Instance) (core.Instance, core.Instance) {
	if *stream.(core.Stream).Column != 0 {
		return FormatChar(e, stream, core.NewCharacter('\n'))
	}
	return Nil, nil
}

func Format(e core.Environment, stream, formatString core.Instance, formatArguments ...core.Instance) (core.Instance, core.Instance) {
	if ok, _ := Stringp(e, formatString); core.DeepEqual(ok, Nil) {
		return SignalCondition(e, core.NewDomainError(e, formatString, core.StringClass), Nil)
	}
	str := string(formatString.(core.String))
	re := regexp.MustCompile(`~(?:[0-9]+[RT]|.)`)
	start, end, index := 0, 0, 0
	for loc := re.FindStringIndex(str[start:]); loc != nil; loc = re.FindStringIndex(str[start:]) {
		base := start
		start = base + 0
		end = base + loc[0]
		if _, err := FormatObject(e, stream, core.NewString([]rune(str[start:end])), Nil); err != nil {
			return nil, err
		}
		start = base + loc[0]
		end = base + loc[1]
		var err core.Instance
		switch str[start:end] {
		case "~A":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, core.NewArityError(e), Nil)
			} else {
				_, err = FormatObject(e, stream, formatArguments[index], Nil)
				index++
			}
		case "~B":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, core.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], core.NewInteger(2))
				index++
			}
		case "~C":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, core.NewArityError(e), Nil)
			} else {
				_, err = FormatChar(e, stream, formatArguments[index])
				index++
			}
		case "~D":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, core.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], core.NewInteger(10))
				index++
			}
		case "~G":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, core.NewArityError(e), Nil)
			} else {
				_, err = FormatFloat(e, stream, formatArguments[index])
				index++
			}
		case "~O":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, core.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], core.NewInteger(8))
				index++
			}
		case "~S":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, core.NewArityError(e), Nil)
			} else {
				_, err = FormatObject(e, stream, formatArguments[index], T)
				index++
			}
		case "~X":
			if index >= len(formatArguments) {
				_, err = SignalCondition(e, core.NewArityError(e), Nil)
			} else {
				_, err = FormatInteger(e, stream, formatArguments[index], core.NewInteger(16))
				index++
			}
		case "~%":
			_, err = FormatChar(e, stream, core.NewCharacter('\n'))
		case "~&":
			_, err = FormatFreshLine(e, stream)
		case "~~":
			_, err = FormatChar(e, stream, core.NewCharacter('~'))
		default:
			s := str[start:end]
			if len(s) > 2 {
				if s[len(s)-1] == 'R' {
					if index >= len(formatArguments) {
						_, err = SignalCondition(e, core.NewArityError(e), Nil)
					} else {
						n, _ := strconv.Atoi(s[1 : len(s)-1])
						if n < 2 || 36 < n {
							_, err = SignalCondition(e, core.NewDomainError(e, core.NewInteger(n), core.IntegerClass), Nil)
						} else {
							_, err = FormatInteger(e, stream, formatArguments[index], core.NewInteger(n))
							index++
						}
					}
				}
				if s[len(s)-1] == 'T' {
					n, _ := strconv.Atoi(s[1 : len(s)-1])
					_, err = FormatTab(e, stream, core.NewInteger(n))
				}
			}
		}
		if err != nil {
			return nil, err
		}
		start = end
	}
	r, err := FormatObject(e, stream, core.NewString([]rune(str[start:])), Nil)
	stream.(core.Stream).Flush()
	return r, err
}
