package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
)

func quote(local, global *env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	return obj, nil
}
