// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"errors"

	hashstructure "github.com/mitchellh/hashstructure/v2"
	mapset "github.com/deckarep/golang-set"
)

type HashMap struct {
	table map[uint64]interface{}
	keys mapset.Set
}

func NewHashMap() (*HashMap) {
	return &HashMap{map[uint64]interface{}{}, mapset.NewSet()}
}

func (m *HashMap) Set(key, value interface{}) error {
	hash, err := hashstructure.Hash(key, hashstructure.FormatV2, nil)
	if err != nil {
		return err
	}
	m.table[hash] = value
	m.keys.Add(key)
	return nil 
}

func (m *HashMap) Get(key interface{}) (interface{}, error) {
	hash, err := hashstructure.Hash(key, hashstructure.FormatV2, nil)
	if err != nil {
		return nil, err
	}
	value, ok := m.table[hash]
	if !ok {
		return nil, errors.New("Not found")
	}
	return value, nil
}

func (m *HashMap) Delete(key interface{}) error {
	hash, err := hashstructure.Hash(key, hashstructure.FormatV2, nil)
	if err != nil {
		return err
	}
	delete(m.table, hash)
	m.keys.Remove(hash)
	return nil
}

func (m *HashMap) Keys() mapset.Set {
	return m.keys
}

type stack []*HashMap

func NewStack() stack {
	return []*HashMap{NewHashMap()}
}

func (s stack) Get(key Instance) (Instance, bool) {
	for i := len(s) - 1; i >= 0; i-- {
		if v, err := s[i].Get(key); err == nil {
			return v.(Instance), true
		}
	}
	return nil, false
}

func (s stack) Set(key, value Instance) bool {
	for i := len(s) - 1; i >= 0; i-- {
		if _, err := s[i].Get(key); err == nil {
			s[i].Set(key, value)
			return true
		}
	}
	return false
}

func (s stack) Define(key, value Instance) bool {
	if _, err := s[len(s)-1].Get(key); err != nil {
		s[len(s)-1].Set(key, value)
		return true
	}
	s[len(s)-1].Set(key, value)
	return false
}

func (s stack) Delete(key Instance) {
	s[len(s) - 1].Delete(key)
}

func (s stack) Append(t stack) stack {
	u := stack{}
	u = append(u, s...)
	u = append(u, t...)
	return u
}
