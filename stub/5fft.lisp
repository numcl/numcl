#|

This file is a part of NUMCL project.
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

NUMCL is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

NUMCL is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
NUMCL.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :numcl.impl)



;; fft(a[, n, axis, norm]) 	Compute the one-dimensional discrete Fourier Transform.
;; fft2(a[, s, axes, norm]) 	Compute the 2-dimensional discrete Fourier Transform
;; fftn(a[, s, axes, norm]) 	Compute the N-dimensional discrete Fourier Transform.
;; ifft(a[, n, axis, norm]) 	Compute the one-dimensional inverse discrete Fourier Transform.
;; ifft2(a[, s, axes, norm]) 	Compute the 2-dimensional inverse discrete Fourier Transform.
;; ifftn(a[, s, axes, norm]) 	Compute the N-dimensional inverse discrete Fourier Transform.
