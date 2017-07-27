package class

// Instance struct type is the struct for the internal representations
type Instance struct {
	Class *Class
	Val   interface{}
}

// Cell is a pair of pointers to Object
type Cell struct {
	Car *Instance
	Cdr *Instance
}
