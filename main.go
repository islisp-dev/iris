// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package main

import (
	"flag"
	"fmt"
	"os"
	golang "runtime"

	"github.com/islisp-dev/iris/core"
	"github.com/islisp-dev/iris/lib"
)

var commit string

func repl(quiet bool) {
	if !quiet {
		if commit == "" {
			commit = "HEAD"
		}
		fmt.Printf("Iris ISLisp Interpreter Commit %v on %v\n", commit, golang.Version())
		fmt.Printf("Copyright 2017 islisp-dev All Rights Reserved.\n")
		fmt.Print(">>> ")
	}
	lib.TopLevel.StandardInput = core.NewStream(os.Stdin, nil, core.CharacterClass)
	lib.TopLevel.StandardOutput = core.NewStream(nil, os.Stdout, core.CharacterClass)
	lib.TopLevel.ErrorOutput = core.NewStream(nil, os.Stderr, core.CharacterClass)
	for exp, err := lib.Read(lib.TopLevel); err == nil; exp, err = lib.Read(lib.TopLevel) {
		ret, err := lib.Eval(lib.TopLevel, exp)
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println(ret)
		}
		if !quiet {
			fmt.Print(">>> ")
		}
	}
}

func script(path string) {
	file, err := os.Open(path)
	if err != nil {
		return
	}
	defer file.Close()
	lib.TopLevel.StandardInput = core.NewStream(file, nil, core.CharacterClass)
	lib.TopLevel.StandardOutput = core.NewStream(nil, os.Stdout, core.CharacterClass)
	lib.TopLevel.ErrorOutput = core.NewStream(nil, os.Stderr, core.CharacterClass)
	for {
		exp, err := lib.Read(lib.TopLevel)
		if err != nil {
			if fmt.Sprint(err) != "#<END-OF-STREAM >" {
				fmt.Println(err)
			}
			return
		}
		_, err = lib.Eval(lib.TopLevel, exp)
		if err != nil {
			fmt.Println(err)
			return
		}
	}
}

func main() {
	flag.Parse()
	if flag.NArg() > 0 {
		script(flag.Arg(0))
		return
	}
	info, err := os.Stdin.Stat()
	if err != nil {
		panic(err)
	}
	if (info.Mode() & os.ModeNamedPipe) == 0 {
		repl(false)
		return
	}
	repl(true)
	return
}
