package runtime

import (
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func UnsafeEndOfListIsNil(i ilos.Instance) bool {
	cdr := i
	for ilos.InstanceOf(cdr, class.Cons) {
		cdr = instance.UnsafeCdr(cdr) // Checked at the top of this loop
	}
	if ilos.InstanceOf(cdr, class.Null) {
		return true
	}
	return false
}

func UnsafeListLength(i ilos.Instance) int {
	cdr := i
	cnt := 0
	for ilos.InstanceOf(cdr, class.Cons) {
		cdr = instance.UnsafeCdr(cdr) // Checked at the top of this loop
		cnt++
	}
	return cnt
}
