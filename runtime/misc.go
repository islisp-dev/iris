package runtime

import (
	"time"

	"github.com/islisp-dev/iris/runtime/env"
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func GetUniversalTime(e env.Environment) (ilos.Instance, ilos.Instance) {
	s := time.Now().Unix()
	return instance.NewInteger(int(s)), nil
}

func GetInternalRealTime(e env.Environment) (ilos.Instance, ilos.Instance) {
	s := time.Now().UnixNano()
	return instance.NewInteger(int(s)), nil
}

func GetInternalRunTime(e env.Environment) (ilos.Instance, ilos.Instance) {
	s := time.Since(Time).Nanoseconds()
	return instance.NewInteger(int(s)), nil
}

func InternalTimeUnitsPerSecond(e env.Environment) (ilos.Instance, ilos.Instance) {
	return instance.NewInteger(1000000000), nil
}
