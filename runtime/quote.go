package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
)

func quote(local, global *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	return obj, nil
}
