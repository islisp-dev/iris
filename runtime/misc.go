package runtime

import (
	"time"

	"github.com/islisp-dev/iris/runtime/ilos"
)

func GetUniversalTime(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	s := time.Now().Unix()
	return ilos.NewInteger(int(s)), nil
}

func GetInternalRealTime(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	s := time.Now().UnixNano()
	return ilos.NewInteger(int(s)), nil
}

func GetInternalRunTime(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	s := time.Since(Time).Nanoseconds()
	return ilos.NewInteger(int(s)), nil
}

func InternalTimeUnitsPerSecond(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	return ilos.NewInteger(1000000000), nil
}
