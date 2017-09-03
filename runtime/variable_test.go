// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import "testing"

func TestSetf(t *testing.T) {
	tests := []test{
		{
			exp:     `(let ((c '(1 2))) (setf (car c) 3) c)`,
			want:    `'(3 2)`,
			wantErr: false,
		},
	}
	execTests(t, Setf, tests)
}
