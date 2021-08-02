// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

func NewBlockTag(tag, uid, object Instance) Instance {
	return Create(NewEnvironment(nil, nil, nil, DefaultHandler),
		BlockTagClass,
		NewSymbol("IRIS.TAG"), tag,
		NewSymbol("IRIS.UID"), uid,
		NewSymbol("IRIS.OBJECT"), object)
}
func NewCatchTag(tag, uid, object Instance) Instance {
	return Create(NewEnvironment(nil, nil, nil, DefaultHandler),
		CatchTagClass,
		NewSymbol("IRIS.TAG"), tag,
		NewSymbol("IRIS.UID"), uid,
		NewSymbol("IRIS.OBJECT"), object)
}
func NewTagbodyTag(tag, uid Instance) Instance {
	return Create(NewEnvironment(nil, nil, nil, DefaultHandler),
		TagbodyTagClass,
		NewSymbol("IRIS.TAG"), tag,
		NewSymbol("IRIS.UID"), uid)
}
