package tokenizer

import (
	"io"
	"strings"
	"testing"
)

func TestReader_ReadToken(t *testing.T) {
	type fields struct {
		err error
		ru  rune
		sz  int
		rr  io.RuneReader
	}
	tests := []struct {
		name    string
		fields  fields
		want    string
		wantErr bool
	}{
		{
			name:    "symbol",
			fields:  fields{nil, 'd', 8, strings.NewReader("efault")},
			want:    "default",
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := &Reader{
				err: tt.fields.err,
				ru:  tt.fields.ru,
				sz:  tt.fields.sz,
				rr:  tt.fields.rr,
			}
			got, err := r.ReadToken()
			if (err != nil) != tt.wantErr {
				t.Errorf("Reader.ReadToken() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.want {
				t.Errorf("Reader.ReadToken() = %v, want %v", got, tt.want)
			}
		})
	}
}
