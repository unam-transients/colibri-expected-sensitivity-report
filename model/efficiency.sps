;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme file)
        (scheme inexact))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x-min 0.3)
(define x-max 2.0)
(define x-step 0.001)

(define (interpolate old-alist)
  (let loop ((x x-min)
             (x0 (car (car old-alist)))
             (y0 (cdr (car old-alist)))
             (x1 (car (cadr old-alist)))
             (y1 (cdr (cadr old-alist)))
             (old-alist (cddr old-alist))
             (new-alist '()))
    (cond
     ((> x x-max) (reverse new-alist))
     ((< x x0)
      (loop (+ x x-step) x0 y0 x1 y1 old-alist (cons (cons x 0.0) new-alist)))
     ((<= x x1)
      (let* ((f (/ (- x x0) (- x1 x0)))
             (y (+ y0 (* f (- y1 y0)))))
        (loop (+ x x-step) x0 y0 x1 y1 old-alist (cons (cons x y) new-alist))))
     ((null? old-alist)
      (loop x x1 0 x-max 0 '() new-alist))
     (else
      (loop x x1 y1 (caar old-alist) (cdar old-alist) (cdr old-alist) new-alist)))))

(define (alist-* . alists)
  (apply map
         (lambda pairs
           (cons (car (car pairs))
                 (apply * (map cdr pairs))))
         alists))

(define (alist-expt alist y)
  (map
   (lambda (pair)
     (cons (car pair) (expt (cdr pair) y)))
   alist))

(define (alist-write-to-file file-name alist)
  (when (file-exists? file-name)
    (delete-file file-name))
  (with-output-to-file file-name
    (lambda ()
      (for-each
       (lambda (p)
         (let ((x (car p)) (y (cdr p)))
           (when (not (zero? y))
             (display x)
             (display " ")
             (display y)
             (newline))))
       alist))))

(define (alist-mean alist x-low x-high)
  (let loop ((alist alist) (sum 0.0) (n 0))
    (cond
     ((null? alist)
      (/ sum n))
     ((<= x-low (car (car alist)) x-high)
      (loop (cdr alist) (+ sum (cdr (car alist))) (+ n 1)))
     (else
      (loop (cdr alist) sum n)))))

(define (alist-clip-negative old-alist)
  (let loop ((old-alist old-alist) (new-alist '()))
    (cond
     ((null? old-alist) (reverse new-alist))
     ((negative? (cdr (car old-alist)))
      (loop (cdr old-alist) 
            (cons (cons (car (car old-alist)) (- (cdr (car old-alist))))
                        new-alist)))
     (else
      (loop (cdr old-alist) 
            (cons (car old-alist) new-alist))))))

(define pi (* 4 (atan 1)))

;; SI units
(define F_0 3631e-26)
(define h 6.63e-34)
(define A (* 0.25 pi (- (expt 1.3 2) (expt 0.58 2))))

(define (alist-zp alist)
  (let loop ((alist alist) (sum 0.0))
    (cond
     ((null? alist)
      (* (/ (* A F_0) h) (* sum x-step )))
     (else
      (loop (cdr alist)
        (let ((x (car (car alist)))
              (eta (cdr (car alist))))
          (+ sum (/ eta x))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eta-atmosphere-continuum
  (interpolate (with-input-from-file "atmosphere-continuum-48.scm" read)))

(define eta-atmosphere-lines
  (interpolate (with-input-from-file "atmosphere-lines-48-2800.scm" read)))

(define eta-atmosphere
  (alist-* eta-atmosphere-continuum eta-atmosphere-lines))

(alist-write-to-file "atmosphere.dat" eta-atmosphere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Aluminum coating from Boccas et al. (1986).
(define eta-aluminum
  (interpolate '((0.350 . 0.875)
                 (0.400 . 0.885)
                 (0.450 . 0.890)
                 (0.500 . 0.890)
                 (0.550 . 0.885)
                 (0.600 . 0.880)
                 (0.650 . 0.875)
                 (0.700 . 0.870)
                 (0.750 . 0.865)
                 (0.780 . 0.861)
                 (0.800 . 0.860)
                 (0.820 . 0.861)
                 (0.850 . 0.870)
                 (0.900 . 0.890)
                 (0.950 . 0.905)
                 (1.000 . 0.920)
                 (1.100 . 0.932)
                 (1.200 . 0.940)
                 (1.600 . 0.950)
                 (2.000 . 0.955))))

(alist-write-to-file "aluminum.dat" eta-aluminum)

(define eta-geometric
(interpolate 
  (list (cons 0.3 (- 1 (expt (/ 0.58 1.30) 2)))
        (cons 2.0 (- 1 (expt (/ 0.58 1.30) 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modelled in Zemax by Salvador. Email of 2017-01-08.

(define eta-lens-internal-blue
  (interpolate '((0.3600 . 0.814363145)
                 (0.3700 . 0.922489507)
                 (0.3800 . 0.958066482)
                 (0.3900 . 0.977035359)
                 (0.4000 . 0.986110296)
                 (0.4200 . 0.990981435)
                 (0.4400 . 0.991792237)
                 (0.4600 . 0.99237123)
                 (0.4800 . 0.994107283)
                 (0.5200 . 0.996445056)
                 (0.5400 . 0.997047953)
                 (0.5600 . 0.997228788)
                 (0.5800 . 0.997228784)
                 (0.6000 . 0.996882157)
                 (0.6200 . 0.996535477)
                 (0.6400 . 0.996535473)
                 (0.6600 . 0.99653547)
                 (0.6800 . 0.997228769)
                 (0.7000 . 0.997921855)
                 (0.7200 . 0.997844854)
                 (0.7600 . 0.997690844)
                 (0.7800 . 0.997613835)
                 (0.8000 . 0.997536824)
                 (0.8100 . 0.997498307)
                 (0.8200 . 0.9974598)
                 (0.8300 . 0.997421292)
                 (0.8400 . 0.997382783)
                 (0.8500 . 0.997344273))))

(define eta-lens-internal-red
  (interpolate '((0.8000 . 0.997536814)
                 (0.8100 . 0.997498307)
                 (0.8200 . 0.9974598)
                 (0.8300 . 0.997421292)
                 (0.8400 . 0.997382783)
                 (0.8500 . 0.997344273)
                 (0.8600 . 0.997305763)
                 (0.8700 . 0.997267253)
                 (0.8800 . 0.997228741)
                 (0.8900 . 0.997190229)
                 (0.9000 . 0.997151716)
                 (0.9100 . 0.997113203)
                 (0.9200 . 0.997074689)
                 (0.9300 . 0.997036174)
                 (0.9400 . 0.996997659)
                 (0.9500 . 0.996959143)
                 (0.9600 . 0.996920626)
                 (0.9700 . 0.996882109)
                 (0.9800 . 0.996843591)
                 (0.9900 . 0.996805072)
                 (1.0000 . 0.996766553)
                 (1.0200 . 0.996689512)
                 (1.0600 . 0.996535422))))

(define eta-lens-internal-infrared
  (interpolate '((1.0000 . 0.995273223)
                 (1.0200 . 0.995196255)
                 (1.0400 . 0.995119286)
                 (1.0600 . 0.995042315)
                 (1.0800 . 0.994659222)
                 (1.1000 . 0.994276064)
                 (1.1200 . 0.993892841)
                 (1.1400 . 0.993509554)
                 (1.1600 . 0.993126202)
                 (1.1800 . 0.992742784)
                 (1.2000 . 0.992359302)
                 (1.2200 . 0.991529308)
                 (1.2400 . 0.990699645)
                 (1.2600 . 0.989870315)
                 (1.2800 . 0.989041317)
                 (1.3000 . 0.988212653)
                 (1.3200 . 0.987384324)
                 (1.3400 . 0.986556328)
                 (1.3600 . 0.985728667)
                 (1.3800 . 0.98490134)
                 (1.4000 . 0.984074349)
                 (1.4200 . 0.983396045)
                 (1.4400 . 0.982717623)
                 (1.4800 . 0.981361338)
                 (1.5000 . 0.980683476)
                 (1.5200 . 0.980005799)
                 (1.5400 . 0.978231921)
                 (1.5600 . 0.975360627)
                 (1.5800 . 0.972487885)
                 (1.6000 . 0.969613681)
                 (1.6200 . 0.966300564)
                 (1.6400 . 0.962988448)
                 (1.6600 . 0.959677324)
                 (1.6800 . 0.956367182)
                 (1.7000 . 0.953058012)
                 (1.7200 . 0.949749804)
                 (1.7400 . 0.946442547)
                 (1.7500 . 0.944789272)
                 (1.7600 . 0.943136231)
                 (1.7700 . 0.941483423)
                 (1.7800 . 0.939830845)
                 (1.7900 . 0.938178498)
                 (1.8000 . 0.93652638))))

(alist-write-to-file "lens-internal-blue.dat"     eta-lens-internal-blue)
(alist-write-to-file "lens-internal-red.dat"      eta-lens-internal-red)
(alist-write-to-file "lens-internal-infrared.dat" eta-lens-internal-infrared)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eta-lens-coating-common
  (alist-expt (interpolate (with-input-from-file "lens-coating-common.scm" read)) 0.25))

(define eta-lens-coating-blue
  (interpolate '((0.36 . 0.99)
                 (0.84 . 0.99))))

(define eta-lens-coating-red
  (interpolate '((0.80 . 0.99)
                 (1.04 . 0.99))))

(define eta-lens-coating-infrared
  (interpolate '((1.1 . 0.99)
                 (1.8 . 0.99))))

(alist-write-to-file "lens-coating-common.dat"   eta-lens-coating-common)
(alist-write-to-file "lens-coating-blue.dat"     eta-lens-coating-blue)
(alist-write-to-file "lens-coating-red.dat"      eta-lens-coating-red)
(alist-write-to-file "lens-coating-infrared.dat" eta-lens-coating-infrared)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eta-d1-reflection
  (interpolate '((0.360 . 0.90)
                 (1.060 . 0.90)
                 (1.150 . 0.10)
                 (1.800 . 0.10))))

(define eta-d1-transmission
  (interpolate '((0.360 . 0.10)
                 (1.060 . 0.10)
                 (1.150 . 0.90)
                 (1.800 . 0.90))))

(define eta-d2-reflection
  (interpolate '((0.360 . 0.90)
                 (0.805 . 0.90)
                 (0.805 . 0.80)
                 (0.815 . 0.80)
                 (0.825 . 0.20)
                 (0.835 . 0.20)
                 (0.835 . 0.10)
                 (1.060 . 0.10))))

(define eta-d2-transmission
  (interpolate '((0.360 . 0.10)
                 (0.805 . 0.10)
                 (0.805 . 0.20)
                 (0.815 . 0.20)
                 (0.825 . 0.80)
                 (0.835 . 0.80)
                 (0.835 . 0.90)
                 (1.060 . 0.90))))

(alist-write-to-file "d1-reflection.dat" eta-d1-reflection)
(alist-write-to-file "d1-transmission.dat" eta-d1-transmission)
(alist-write-to-file "d2-reflection.dat" eta-d2-reflection)
(alist-write-to-file "d2-transmission.dat" eta-d2-transmission)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CCD BB window coating per surface from email from Charles Slaugher on
;;; 2016-08-10. The email actually only specifies <1% per surface from
;;; 400-800 nm. I'm guessing outside that range.
(define eta-window-coating-blue
  (interpolate '((0.350 . 0.96)
                 (0.400 . 0.99)
                 (0.800 . 0.99)
                 (0.850 . 0.96))))

;;; CCD IR BB coating per surface from email from Charles Slaughter on
;;; 2016-08-22. The email actually only specifies the behaviour in 480-1000
;;; nm. I'm guessing outside that range.
(define eta-window-coating-red
  (interpolate '((0.370 . 0.960)
                 (0.480 . 0.990)
                 (0.600 . 0.995)
                 (0.900 . 0.995)
                 (1.000 . 0.990)
                 (1.050 . 0.960))))

(alist-write-to-file "window-coating-blue.dat" eta-window-coating-blue)
(alist-write-to-file "window-coating-red.dat" eta-window-coating-red)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Gold.
(define eta-gold
  (interpolate '((1.0 . 0.98)
                 (2.0 . 0.99))))

(alist-write-to-file "gold.dat" eta-gold)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Filter.
(define eta-filter
  (interpolate '((0.35 . 0.85)
                 (2.00 . 0.85))))

(alist-write-to-file "filter.dat" eta-filter)

(define eta-filter-B
  (interpolate (alist-clip-negative (with-input-from-file "filter-B.scm" read))))
(alist-write-to-file "filter-B.dat" eta-filter-B)

(define eta-filter-g
  (interpolate '((0.370 . 0.01)
                 (0.390 . 0.10)
                 (0.400 . 0.50)
                 (0.410 . 0.85)
                 (0.540 . 0.85)
                 (0.550 . 0.50)
                 (0.560 . 0.10)
                 (0.580 . 0.01))))
(alist-write-to-file "filter-g.dat" eta-filter-g)

(define eta-filter-r
  (interpolate '((0.520 . 0.01)
                 (0.540 . 0.10)
                 (0.550 . 0.50)
                 (0.560 . 0.85)
                 (0.680 . 0.85)
                 (0.690 . 0.50)
                 (0.700 . 0.10)
                 (0.720 . 0.01))))
(alist-write-to-file "filter-r.dat" eta-filter-r)

(define eta-filter-i
  (interpolate '((0.660 . 0.01)
                 (0.680 . 0.10)
                 (0.690 . 0.50)
                 (0.700 . 0.85)
                 (0.805 . 0.85)
                 (0.820 . 0.10)
                 (0.840 . 0.01))))
(alist-write-to-file "filter-i.dat" eta-filter-i)

(define eta-filter-gri
  (interpolate '((0.370 . 0.01)
                 (0.390 . 0.10)
                 (0.400 . 0.50)
                 (0.410 . 0.85)
                 (0.805 . 0.85)
                 (0.820 . 0.10)
                 (0.840 . 0.01))))
(alist-write-to-file "filter-gri.dat" eta-filter-gri)

(define eta-filter-z
  (interpolate '((0.800 . 0.01)
                 (0.820 . 0.10)
                 (0.835 . 0.85)
                 (0.910 . 0.85)
                 (0.920 . 0.50)
                 (0.930 . 0.10)
                 (0.950 . 0.01))))
(alist-write-to-file "filter-z.dat" eta-filter-z)

(define eta-filter-y
  (interpolate '((0.890 . 0.01)
                 (0.910 . 0.10)
                 (0.920 . 0.50)
                 (0.930 . 0.85)
                 (1.100 . 0.85))))
(alist-write-to-file "filter-y.dat" eta-filter-y)
                
(define eta-filter-zy
  (interpolate '((0.800 . 0.01)
                 (0.820 . 0.10)
                 (0.835 . 0.85)
                 (1.100 . 0.85))))
(alist-write-to-file "filter-zy.dat" eta-filter-zy)

(define eta-filter-J
  (let* ((raw-alist (interpolate (alist-clip-negative (with-input-from-file "filter-J.scm" read))))
         (raw-mean (alist-mean raw-alist 1.19 1.31)))
(display raw-mean)
(newline)
    (map 
      (lambda (pair)
        (cons (car pair) (* (cdr pair) (/ 0.85 raw-mean))))
      raw-alist)))
(alist-write-to-file "filter-J.dat" eta-filter-J)

(define eta-filter-H
  (let* ((raw-alist (interpolate (alist-clip-negative (with-input-from-file "filter-H.scm" read))))
         (raw-mean (alist-mean raw-alist 1.51 1.70)))
    (map 
      (lambda (pair)
        (cons (car pair) (* (cdr pair) (/ 0.85 raw-mean))))
      raw-alist)))
(alist-write-to-file "filter-H.dat" eta-filter-H)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; QE for BI NIMO DD CCD with Astro Multi-2 at -100 C. From e2v Excel
;;; document sent my Charles Slaughter on 2016-11-03.
(define eta-ccd
  (interpolate '((0.250 . 0.0078)
                 (0.260 . 0.0100)
                 (0.270 . 0.0142)
                 (0.280 . 0.0286)
                 (0.290 . 0.0605)
                 (0.300 . 0.1050)
                 (0.310 . 0.1527)
                 (0.320 . 0.1987)
                 (0.330 . 0.2569)
                 (0.340 . 0.3334)
                 (0.350 . 0.4484)
                 (0.360 . 0.5165)
                 (0.370 . 0.6156)
                 (0.380 . 0.7388)
                 (0.390 . 0.8230)
                 (0.400 . 0.8717)
                 (0.410 . 0.9076)
                 (0.420 . 0.9183)
                 (0.430 . 0.9227)
                 (0.440 . 0.9208)
                 (0.450 . 0.9147)
                 (0.460 . 0.9074)
                 (0.470 . 0.9003)
                 (0.480 . 0.8953)
                 (0.490 . 0.8920)
                 (0.500 . 0.8874)
                 (0.510 . 0.8816)
                 (0.520 . 0.8811)
                 (0.530 . 0.8792)
                 (0.540 . 0.8838)
                 (0.550 . 0.8836)
                 (0.560 . 0.8868)
                 (0.570 . 0.8881)
                 (0.580 . 0.8926)
                 (0.590 . 0.8948)
                 (0.600 . 0.8999)
                 (0.610 . 0.9026)
                 (0.620 . 0.9078)
                 (0.630 . 0.9120)
                 (0.640 . 0.9157)
                 (0.650 . 0.9185)
                 (0.660 . 0.9219)
                 (0.670 . 0.9247)
                 (0.680 . 0.9273)
                 (0.690 . 0.9295)
                 (0.700 . 0.9311)
                 (0.710 . 0.9322)
                 (0.720 . 0.9325)
                 (0.730 . 0.9321)
                 (0.740 . 0.9306)
                 (0.750 . 0.9279)
                 (0.760 . 0.9239)
                 (0.770 . 0.9182)
                 (0.780 . 0.9108)
                 (0.790 . 0.9013)
                 (0.800 . 0.8894)
                 (0.810 . 0.8748)
                 (0.820 . 0.8573)
                 (0.830 . 0.8361)
                 (0.840 . 0.8120)
                 (0.850 . 0.7843)
                 (0.860 . 0.7530)
                 (0.870 . 0.7181)
                 (0.880 . 0.6797)
                 (0.890 . 0.6381)
                 (0.900 . 0.5936)
                 (0.910 . 0.5468)
                 (0.920 . 0.4983)
                 (0.930 . 0.4487)
                 (0.940 . 0.3986)
                 (0.950 . 0.3490)
                 (0.960 . 0.3005)
                 (0.970 . 0.2539)
                 (0.980 . 0.2099)
                 (0.990 . 0.1691)
                 (1.000 . 0.1323)
                 (1.010 . 0.0998)
                 (1.020 . 0.0722)
                 (1.030 . 0.0496)
                 (1.040 . 0.0323)
                 (1.050 . 0.0192))))

(define eta-h2rg
  (interpolate '((0.80 . 0.50)
                 (1.00 . 0.50)
                 (1.23 . 0.70)
                 (1.50 . 0.70)
                 (1.70 . 0.70)
                 (1.75 . 0.35)
                 (1.80 . 0.00))))

(alist-write-to-file "ccd.dat" eta-ccd)
(alist-write-to-file "h2rg.dat" eta-h2rg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eta-telescope
  (alist-expt eta-aluminum 3))

(alist-write-to-file "telescope.dat" eta-telescope)

(define eta-instrument-optics-blue
  (alist-* (alist-expt eta-lens-coating-common 4)
           eta-d1-reflection
           eta-d2-reflection
           eta-lens-internal-blue
           (alist-expt eta-lens-coating-blue 2)
           (alist-expt eta-window-coating-blue 2)))

(define eta-instrument-blue
  (alist-* eta-instrument-optics-blue
           eta-ccd))

(alist-write-to-file "instrument-optics-blue.dat"
                     (alist-* eta-instrument-optics-blue))
(alist-write-to-file "instrument-blue.dat"
                     (alist-* eta-instrument-blue))
(alist-write-to-file "hardware-blue.dat"
                     (alist-* eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "system-blue.dat"
                     (alist-* eta-atmosphere eta-geometric eta-telescope eta-instrument-blue))

(define eta-instrument-optics-red
  (alist-* (alist-expt eta-lens-coating-common 4)
           eta-d1-reflection
           eta-d2-transmission
           eta-lens-internal-red
           (alist-expt eta-lens-coating-red 2)
           (alist-expt eta-window-coating-red 2)))

(define eta-instrument-red
  (alist-* eta-instrument-optics-red
           eta-ccd))

(alist-write-to-file "instrument-optics-red.dat"
                     (alist-* eta-instrument-optics-red))
(alist-write-to-file "instrument-red.dat"
                     (alist-* eta-instrument-red))
(alist-write-to-file "hardware-red.dat"
                     (alist-* eta-geometric eta-telescope eta-instrument-red))
(alist-write-to-file "system-red.dat"
                     (alist-* eta-atmosphere eta-geometric eta-telescope eta-instrument-red))

(define eta-instrument-optics-infrared
  (alist-* (alist-expt eta-lens-coating-common 4)
           eta-d1-transmission
           eta-lens-internal-infrared
           (alist-expt eta-lens-coating-infrared 18)
           (alist-expt eta-gold 2)))

(define eta-instrument-infrared
  (alist-* eta-instrument-optics-infrared
           eta-h2rg))

(alist-write-to-file "instrument-optics-infrared.dat"
                     (alist-* eta-instrument-optics-infrared))
(alist-write-to-file "instrument-infrared.dat"
                     (alist-* eta-instrument-infrared))
(alist-write-to-file "hardware-infrared.dat"
                     (alist-* eta-geometric eta-telescope eta-instrument-infrared))
(alist-write-to-file "system-infrared.dat"
                     (alist-* eta-atmosphere eta-geometric eta-telescope eta-instrument-infrared))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "Filters:\n")
(display "g 0.410 0.540 ")
(display (alist-mean eta-filter-g 0.410 0.540))
(newline)
(display "r 0.560 0.680 ")
(display (alist-mean eta-filter-r 0.560 0.680))
(newline)
(display "i 0.700 0.805 ")
(display (alist-mean eta-filter-i 0.700 0.805))
(newline)
(display "z 0.835 0.920 ")
(display (alist-mean eta-filter-z 0.835 0.910))
(newline)
(display "y 0.930 1.020 ")
(display (alist-mean eta-filter-y 0.930 1.020))
(newline)
(display "J 1.17 1.33 ")
(display (alist-mean eta-filter-J 1.170 1.330))
(newline)
(display "H 1.49 1.70 ")
(display (alist-mean eta-filter-H 1.49 1.70))
(newline)


(define (display-means alist-blue alist-red alist-infrared)
  (for-each
   (lambda (name alist x-low x-high)
    (display "eta ")
     (display name)
     (display " ")
     (display x-low)
     (display " ")
     (display x-high)
     (display " ")
     (display (alist-mean alist x-low x-high))
     (newline))
   '(g r i z y J H)
   (list alist-blue alist-blue alist-blue alist-red alist-red alist-infrared alist-infrared)
   '(0.400 0.550 0.690 0.820 0.920 1.17 1.49)
   '(0.550 0.690 0.820 0.920 1.020 1.33 1.70))
  (newline))

(display "Atmosphere:\n")
(display-means eta-atmosphere
               eta-atmosphere
               eta-atmosphere)
(display "Geometric:\n")
(display-means eta-geometric
               eta-geometric
               eta-geometric)
(display "Telescope:\n")
(display-means eta-telescope
               eta-telescope
               eta-telescope)
(display "Instrument:\n")
(display-means (alist-* eta-filter eta-instrument-blue)
               (alist-* eta-filter eta-instrument-red)
               (alist-* eta-filter eta-instrument-infrared))
(display "Telescope+Instrument-Optics:\n")
(display-means (alist-* eta-filter eta-telescope eta-instrument-optics-blue)
               (alist-* eta-filter eta-telescope eta-instrument-optics-red)
               (alist-* eta-filter eta-telescope eta-instrument-optics-infrared))
(display "Telescope+Instrument:\n")
(display-means (alist-* eta-filter eta-telescope eta-instrument-blue)
               (alist-* eta-filter eta-telescope eta-instrument-red)
               (alist-* eta-filter eta-telescope eta-instrument-infrared))
(display "Hardware:\n")
(display-means (alist-* eta-filter eta-telescope eta-geometric eta-instrument-blue)
               (alist-* eta-filter eta-telescope eta-geometric eta-instrument-red)
               (alist-* eta-filter eta-telescope eta-geometric eta-instrument-infrared))
(display "System:\n")
(display-means (alist-* eta-filter eta-atmosphere eta-telescope eta-geometric eta-instrument-blue)
               (alist-* eta-filter eta-atmosphere eta-telescope eta-geometric eta-instrument-red)
               (alist-* eta-filter eta-atmosphere eta-telescope eta-geometric eta-instrument-infrared))
               
(display "Lens Internal:\n")
(display-means eta-lens-internal-blue
               eta-lens-internal-red
               eta-lens-internal-infrared)

(display "Lens/Window Coatings:\n")
(display-means (alist-* (alist-expt eta-lens-coating-common 4) 
											  (alist-expt eta-lens-coating-blue 2)
											  (alist-expt eta-window-coating-blue 2))
               (alist-* (alist-expt eta-lens-coating-common 4) 
											  (alist-expt eta-lens-coating-red 2)
											  (alist-expt eta-window-coating-red 2))
											  (alist-* (alist-expt eta-lens-coating-common 4) 
											  (alist-expt eta-lens-coating-infrared 18)))

(display "Detectors:\n")
(display-means eta-ccd eta-ccd eta-h2rg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alist-write-to-file "instrument-B.dat"
                     (alist-* eta-filter-B eta-instrument-blue))
(alist-write-to-file "instrument-g.dat"
                     (alist-* eta-filter-g eta-instrument-blue))
(alist-write-to-file "instrument-r.dat"
                     (alist-* eta-filter-r eta-instrument-blue))
(alist-write-to-file "instrument-i.dat"
                     (alist-* eta-filter-i eta-instrument-blue))
(alist-write-to-file "instrument-gri.dat"
                     (alist-* eta-filter-gri eta-instrument-blue))
(alist-write-to-file "instrument-z.dat"
                     (alist-* eta-filter-z eta-instrument-red))
(alist-write-to-file "instrument-y.dat"
                     (alist-* eta-filter-y eta-instrument-red))
(alist-write-to-file "instrument-zy.dat"
                     (alist-* eta-filter-zy eta-instrument-red))
(alist-write-to-file "instrument-J.dat"
                     (alist-* eta-filter-J eta-instrument-infrared))
(alist-write-to-file "instrument-H.dat"
                     (alist-* eta-filter-H eta-instrument-infrared))
                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alist-write-to-file "hardware-B.dat"
                     (alist-* eta-filter-B eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "hardware-g.dat"
                     (alist-* eta-filter-g eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "hardware-r.dat"
                     (alist-* eta-filter-r eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "hardware-i.dat"
                     (alist-* eta-filter-i eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "hardware-gri.dat"
                     (alist-* eta-filter-gri eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "hardware-z.dat"
                     (alist-* eta-filter-z eta-geometric eta-telescope eta-instrument-red))
(alist-write-to-file "hardware-y.dat"
                     (alist-* eta-filter-y eta-geometric eta-telescope eta-instrument-red))
(alist-write-to-file "hardware-zy.dat"
                     (alist-* eta-filter-zy eta-geometric eta-telescope eta-instrument-red))
(alist-write-to-file "hardware-J.dat"
                     (alist-* eta-filter-J eta-geometric eta-telescope eta-instrument-infrared))
(alist-write-to-file "hardware-H.dat"
                     (alist-* eta-filter-H eta-geometric eta-telescope eta-instrument-infrared))
                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alist-write-to-file "system-B.dat"
                     (alist-* eta-filter-B eta-atmosphere eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "system-g.dat"
                     (alist-* eta-filter-g eta-atmosphere eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "system-r.dat"
                     (alist-* eta-filter-r eta-atmosphere eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "system-i.dat"
                     (alist-* eta-filter-i eta-atmosphere eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "system-gri.dat"
                     (alist-* eta-filter-gri eta-atmosphere eta-geometric eta-telescope eta-instrument-blue))
(alist-write-to-file "system-z.dat"
                     (alist-* eta-filter-z eta-atmosphere eta-geometric eta-telescope eta-instrument-red))
(alist-write-to-file "system-y.dat"
                     (alist-* eta-filter-y eta-atmosphere eta-geometric eta-telescope eta-instrument-red))
(alist-write-to-file "system-zy.dat"
                     (alist-* eta-filter-zy eta-atmosphere eta-geometric eta-telescope eta-instrument-red))
(alist-write-to-file "system-J.dat"
                     (alist-* eta-filter-J eta-atmosphere eta-geometric eta-telescope eta-instrument-infrared))
(alist-write-to-file "system-H.dat"
                     (alist-* eta-filter-H eta-atmosphere eta-geometric eta-telescope eta-instrument-infrared))
                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for-each
  (lambda (filter eta-filter eta-instrument)
  (display "zp ")
  (display filter)
  (display " ")
  (display (alist-zp (alist-* eta-filter eta-atmosphere eta-telescope eta-instrument)))
  (newline))
  '(B g r i gri z y zy J H)
  (list
    eta-filter-B eta-filter-g eta-filter-r eta-filter-i eta-filter-gri 
    eta-filter-z eta-filter-y eta-filter-zy 
    eta-filter-J eta-filter-H)
  (list
    eta-instrument-blue eta-instrument-blue eta-instrument-blue eta-instrument-blue eta-instrument-blue
    eta-instrument-red eta-instrument-red eta-instrument-red
    eta-instrument-infrared eta-instrument-infrared))
