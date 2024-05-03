name: noisy
link: https://github.com/Kevinpgalligan/noisy
type: desktop
date: 2024-01-02
description: Perlin noise library for Common Lisp.

Perlin noise library for Common Lisp. Supports an arbitrary number of dimensions.

Here it is in 2 dimensions:

<img src="{{ url_for('static', filename='img/noisy/2d-noise.png') }}"
     alt="A comparison of low-resolution (smoother) noise and higher-resolution (more detailed / chaotic) noise"
     class="centered">

Usage:

    CL-USER> (ql:quickload 'noisy)
    CL-USER> (use-package 'noisy)
    CL-USER> (noise-seed 7)
    CL-USER> (noise-detail :lod 2 :falloff 0.4)
    CL-USER> (noise 0.02 0.3)
    0.39439920179571514d0
    CL-USER> (defparameter *N* (make-noise 2 :seed 7 :lod 1 :falloff 1.0))
    CL-USER> (set-noise-detail *N* :lod 2 :falloff 0.3)
    CL-USER> (noise-gen *N* 0.04 0.09)
    0.17510623096285327d0
