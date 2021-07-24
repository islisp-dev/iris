package lib

import (
	"time"

	"github.com/islisp-dev/iris/runtime/core"
)

func GetUniversalTime(e core.Environment) (core.Instance, core.Instance) {
	s := time.Now().Unix()
	return core.NewInteger(int(s)), nil
}

func GetInternalRealTime(e core.Environment) (core.Instance, core.Instance) {
	s := time.Now().UnixNano()
	return core.NewInteger(int(s)), nil
}

func GetInternalRunTime(e core.Environment) (core.Instance, core.Instance) {
	s := time.Since(Time).Nanoseconds()
	return core.NewInteger(int(s)), nil
}

func InternalTimeUnitsPerSecond(e core.Environment) (core.Instance, core.Instance) {
	return core.NewInteger(1000000000), nil
}
