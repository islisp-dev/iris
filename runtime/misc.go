package runtime

import (
	"time"

	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func GetUniversalTime(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	s := time.Now().Unix()
	return instance.NewInteger(int(s)), nil
}

func GetInternalRealTime(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	s := time.Now().UnixNano()
	return instance.NewInteger(int(s)), nil
}

func GetInternalRunTime(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	s := time.Since(Time).Nanoseconds()
	return instance.NewInteger(int(s)), nil
}

func InternalTimeUnitsPerSecond(e ilos.Environment) (ilos.Instance, ilos.Instance) {
	return instance.NewInteger(1000000000), nil
}
