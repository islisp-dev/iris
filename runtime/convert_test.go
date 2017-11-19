// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import "testing"

func TestConvert(t *testing.T) {
	execTests(t, Convert, []test{
		{
			exp:     `(convert 1.0 <integer>)`,
			want:    `1`,
			wantErr: false,
		},
	})
}
