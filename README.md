# iris

Iris is a interpreter of ISLisp implemented with golang

![logo](logo.png)

## Introduction

ISLisp is a member of LISP family and standardized by ISO in 2007.
As you know, Common Lisp is standardized by ANSI in 1994.
Iris is a interpreter of ISLisp implemented with golang.

Iris has the webpage and the online REPL. [islisp.js.org](https://islisp.js.org)

## Usage

### Install

You can install iris with `go get`

```bash
$ go get github.com/islisp-dev/iris
```

### Update

You can update iris with `go get`

```bash
$ go get -u github.com/islisp-dev/iris
```

## Development

### Test

Iris is tested on TravisCI with this command.

```
$ go test ./...
```

## Calling Go function from Iris

```go
// greet.go
package main

import (
    "fmt"
    islisp "github.com/islisp-dev/iris/runtime/core"
)

func SayHello(env islisp.Environment) (islisp.Instance, islisp.Instance) {
    fmt.Fprintf(env.StandardOutput, "Hello there!\n")
    return islisp.Nil, nil
}
```

```
$ go build -buildmode=plugin greet.go
$ iris
>>> (import say-hello :from "greet.so")
Nil
>>> (say-hello)
Hello there!
Nil
```


## License
This software is licensed under the Mozilla Public License v2.0

## Copyright
Copyright (c) 2017 islisp-dev All Rights Reserved.
