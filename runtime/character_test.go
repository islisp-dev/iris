package runtime

import "testing"

func TestCharacterp(t *testing.T) {
	execTests(t, Characterp, []test{
		{
			exp:     `(characterp #\a)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(characterp "a")`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(characterp 'a)`,
			want:    `nil`,
			wantErr: false,
		},
	})
}

func TestChar(t *testing.T) {
	execTests(t, CharEqual, []test{
		{
			exp:     `(char= #\a #\a)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(char= #\a #\b)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(char= #\a #\A)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(char/= #\a #\a)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(char< #\a #\a)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(char< #\a #\b)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(char< #\b #\a)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(char< #\a #\A)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(char< #\* #\a)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(char> #\b #\a)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(char<= #\a #\a)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(char<= #\a #\A)`,
			want:    `nil`,
			wantErr: false,
		},
		{
			exp:     `(char>= #\b #\a)`,
			want:    `t`,
			wantErr: false,
		},
		{
			exp:     `(char>= #\a #\a)`,
			want:    `t`,
			wantErr: false,
		},
	})
}
