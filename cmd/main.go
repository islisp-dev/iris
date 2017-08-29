// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package main

import (
	"fmt"

	"github.com/ta2gch/iris/runtime"
)

func main() {
	fmt.Print("> ")
	for exp, err := runtime.Read(runtime.TopLevel); err == nil; exp, err = runtime.Read(runtime.TopLevel) {
		ret, err := runtime.Eval(runtime.TopLevel, exp)
		if err != nil {
			fmt.Println(err)
		}
		fmt.Println(ret)
		fmt.Print("> ")
	}
}
