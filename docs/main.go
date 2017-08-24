package main

import (
	"fmt"
	"html"
	"strings"

	"github.com/gopherjs/jquery"
	"github.com/ta2gch/iris/reader/parser"
	"github.com/ta2gch/iris/reader/tokenizer"
	"github.com/ta2gch/iris/runtime"
)

var jQuery = jquery.NewJQuery

func main() {
	env := runtime.New()
	jQuery("#repl").Submit(func(e jquery.Event) {
		e.PreventDefault()
		e.StopPropagation()
		input := jQuery("#prompt").Val()
		reader := strings.NewReader(input)
		tokens := tokenizer.New(reader)
		exp, err := parser.Parse(tokens)
		if err != nil {
			jQuery("#history").Append(fmt.Sprintf("<code><pre>&gt; %v</pre></code><code><pre>%v</pre></code>", input, html.EscapeString(err.String())))
		} else {
			output, err := runtime.Eval(env, exp)
			if err != nil {
				jQuery("#history").Append(fmt.Sprintf("<code><pre>&gt; %v</pre></code><code><pre>%v</pre></code>", input, html.EscapeString(err.String())))
			} else {
				jQuery("#history").Append(fmt.Sprintf("<code><pre>&gt; %v</pre></code><code><pre>%v</pre></code>", input, html.EscapeString(output.String())))
			}
		}
	})
}
