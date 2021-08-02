package core

import (
	"errors"
	"fmt"

	mapset "github.com/deckarep/golang-set"
	"github.com/google/go-cmp/cmp"
	hashstructure "github.com/mitchellh/hashstructure/v2"
)

type Map interface {
	Set(key, value interface{}) (err error)
	Get(key interface{}) (value interface{}, err error)
	Delete(key interface{}) (err error)
	Keys() []interface{}
	String() string
}

type HashMap struct {
	table map[uint64]interface{}
	keys  mapset.Set
}

func (m *HashMap) Equal(n interface{}) bool {
	c, ok := n.(*HashMap)
	if !ok {
		return false
	}
	return cmp.Equal(c.table, m.table)
}

func NewHashMap() *HashMap {
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
		return nil, errors.New("not found")
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

func (m *HashMap) Keys() []interface{} {
	return m.keys.ToSlice()
}

func (m *HashMap) String() (str string) {
	str += "{"
	keys := m.keys.ToSlice()
	for idx, key := range keys {
		val, _ := m.Get(key)
		str += fmt.Sprintf("%v: %v", key, val)
		if idx != len(keys)-1 {
			str += ", "
		} else {
			str += "}"
		}
	}
	return
}
