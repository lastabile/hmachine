
;; Photo hackery

(defun filter-nef-files ()
  (defr
	(defl make-table (file-spec-str)
	  (let ((paths (directory file-spec-str)))
		(let ((h (make-hash-table :test #'equal)))
		  (dolist (path paths)
			(let ((name (pathname-name path)))
			  (let ((key (subseq name 4 (- (length name) 0))))
				(setf (gethash key h) path))))
		  h)))
	(defl get-files ()
	  (let ((nef (make-table "c:/trippics/032/*.NEF")))
		(let ((jpg (make-table "c:/trippics/032/jpg/*.JPG")))
		  (let ((enh (make-table "c:/trippics/032/enh/*.NEF")))
			(let ((rh (make-hash-table :test #'equal)))
			  (maphash (lambda (k v)
						 (let ((key k))
						   (let ((path v))
							 (when (gethash key jpg)
							   (setf (gethash key rh) path)))))
					   nef)
			  (maphash (lambda (k v)
						 (let ((key k))
						   (let ((path v))
							 (setf (gethash key rh) path))))
					   enh)
			  (let ((r nil))
				(maphash (lambda (k v) (setq r (cons v r))) rh)
				r))))))
	(let ((files (get-files)))
	  (let ((argslist (mapcar (lambda (file)
								(list (format nil "~a" file) "c:/trippics/032/select-and-enh-nef/"))
							  files)))
		(block b
		  (dolist (args argslist)
			(print args)
			(run-program "cp" :arguments args)
			;; (return-from b nil)
			))
		nil))))

;; Csv utils

(defun read-csv-file (file &key ignore-chars ignore-high)
  (with-open-file (s file :direction :input)
	(let ((rows nil)
		  (row nil)
		  (token ""))
	  (defr
		(defl append-token-to-row ()
		  (setq row (append row (list token)))
		  (setq token ""))
		(defl append-row-to-rows ()
		  (setq rows (append rows (list row)))
		  (setq row nil))
		(defl append-to-token (c)
		  (setq token (concatenate 'string token (string c))))
		(let ((state 0))
		  (loop
		   (let ((c (read-char s nil nil)))
			 (if (null c)
				 (return rows)
				 (if (or
					  (and ignore-high
						   (> (char-code c) 255))
					  (and ignore-chars
						  (member c ignore-chars :test #'eq)))
					 nil
					 (cond
					  ((= state 0)
					   (cond
						((eq c #\,)
						 (append-token-to-row)
						 (setq state 0))
						((eq c #\Newline)
						 (append-token-to-row)
						 (append-row-to-rows)
						 (setq state 0))
						((eq c #\")
						 (setq state 1))
						(t
						 (append-to-token c)
						 (setq state 0))))
					  ((= state 1)
					   (cond
						((eq c #\")
						 (setq state 0))
						(t
						 (append-to-token c))))))))))))))

(defun write-csv-file (s file)
  (defr
	(defl write-row (f r)
	  (dolist (x r)
		(format f "\"~a\"," x))
	  (format f "~%"))
    (with-open-file (f file :direction :output)
	  (dolist (r s)
		(write-row f r)))))

(defun is-empty-row (r)
  (block b
	(dolist (x r)
	  (when (not (is-empty-string x))
		(return-from b nil)))
	t))

(defun is-empty-string (s)
  (equal s ""))

(defun read-file-into-stream (fin sout)
  (with-open-file (sin fin :direction :input)
	(block b
	  (loop
	   (let ((c (read-char sin nil nil)))
		 (if (null c)
			 (return-from b nil))
		 (write-char c sout))))))

;; substlist is of the form (("varstring1" "subststring1") ...)
;; Variables must be of the form _$<text>$_

(defun subst-string-in-file (fin fout substlist)
  (let ((varchar1 #\_))
	(let ((varchar2 #\$))
	  (let ((state 0))
		(let ((var ""))
		  (with-open-file (sin fin :direction :input)
			(with-open-file (sout fout :direction :output)
			  (defr
				(defl writec (c)
				  (write-char c sout))
				(defl writestr (s)
				  (write-sequence s sout))
				(defl write-var ()
				  (writestr var)
				  (setq var ""))
				(defl append-var (c)
				  (setq var (concatenate 'string var (string c))))
				(defl write-subst ()
				  (dolist (s substlist)
					(when (equal (first s) var)
					  (writestr (second s))))
				  (setq var ""))
				(block b
				  (loop
				   (let ((c (read-char sin nil nil)))
					 (cond
					  ((null c)
					   (return-from b nil))
					  ((= state 0)
					   (cond
						((eq c varchar1)
						 (append-var varchar1)
						 (setq state 1))
						(t
						 (writec c)
						 (setq state 0))))
					  ((= state 1)
					   (cond
						((eq c varchar2)
						 (append-var varchar2)
						 (setq state 2))
						(t
						 (append-var c)
						 (write-var)
						 (setq state 0))))
					  ((= state 2)
					   (cond
						((eq c varchar2)
						 (append-var varchar2)
						 (setq state 3))
						(t
						 (append-var c)
						 (setq state 2))))
					  ((= state 3)
					   (cond
						((eq c varchar1)
						 (append-var varchar1)
						 (write-subst)
						 (setq state 0))
						(t
						 (append-var c)
						 (write-var)
						 (setq state 0))))))))))))))))

(defun old-subst-string-in-file (fin fout substlist)
  (let ((varchar1 #\_))
	(let ((varchar2 #\$))
	  (let ((state 0))
		(let ((var ""))
		  (let ((bufindex 0))
			(let ((buf (make-string 1024)))
			  (with-open-file (sin fin :direction :input)
				(with-open-file (sout fout :direction :output)
				  (defr
					(defl flush ()
					  (write-sequence buf sout :end bufindex)
					  (setq bufindex 0))
					(defl writec (c)
					  (if (= bufindex 1024)
						  (flush)
						  (let ()
							(setf (aref buf bufindex) c)
							(setq bufindex (+ bufindex 1)))))
					(defl writestr (s)
					  (flush)
					  (write-sequence s sout))
					(defl write-var ()
					  (writestr var)
					  (setq var ""))
					(defl append-var (c)
					  (setq var (concatenate 'string var (string c))))
					(defl write-subst ()
					  (dolist (s substlist)
						(when (equal (first s) var)
						  (writestr (second s))))
					  (setq var ""))
					(block b
					  (loop
					   (let ((c (read-char sin nil nil)))
						 (cond
						  ((null c)
						   (flush)
						   (return-from b nil))
						  ((= state 0)
						   (cond
							((eq c varchar1)
							 (append-var varchar1)
							 (setq state 1))
							(t
							 ;; (write-char c sout)
							 (writec c)
							 (setq state 0))))
						  ((= state 1)
						   (cond
							((eq c varchar2)
							 (append-var varchar2)
							 (setq state 2))
							(t
							 (append-var c)
							 (write-var)
							 (setq state 0))))
						  ((= state 2)
						   (cond
							((eq c varchar2)
							 (append-var varchar2)
							 (setq state 3))
							(t
							 (append-var c)
							 (setq state 2))))
						  ((= state 3)
						   (cond
							((eq c varchar1)
							 (append-var varchar1)
							 (write-subst)
							 (setq state 0))
							(t
							 (append-var c)
							 (write-var)
							 (setq state 0))))))))))))))))))



;;;;;; Wave file writers
;; All int writers are little-endian
;; Written in 2's complement 
;; All strings are big-endian
;;
;; https://ccrma.stanford.edu/courses/422/projects/WaveFormat/

(let ()
  (defr
	(defl write-int32 (s n)
	  (let ((m n))
		(dotimes (i 4)
		  (let ((b (logand m 255)))
			(write-byte b s))
		  (setq m (ash m -8)))))
	(defl write-int16 (s n)
	  (let ((m n))
		(dotimes (i 2)
		  (let ((b (logand m 255)))
			(write-byte b s))
		  (setq m (ash m -8)))))
	(defl read-int32 (s)
	  (read-int s 4))
	(defl read-int16 (s)
	  (read-int s 2))
	(defl read-int (s nbytes)
	  (let ((n 0))
		(let ((sign (ash 1 (- (* nbytes 8) 1))))
		  (let ((mod (ash sign 1)))
			(dotimes (i nbytes)
			  (let ((b (read-byte s)))
				(setq n (logior n (ash b (* i 8))))))
			(when (not (= (logand sign n) 0))
			  (setq n (- (- mod n))))
			n))))
	(defl read-str (s len)
	  (let ((str ""))
		(dotimes (i len)
		  (let ((c (code-char (read-byte s))))
			(setq str (concatenate 'string str (string c)))))
		str))
	(defl write-str (s str)
	  (let ((len (length str)))
		(dotimes (i len)
		  (let ((c (char-code (aref str i))))
			(write-byte c s)))
		nil))
	(defl order (s)
	  s)

	;; (write-wave-to-stream s (lambda (n) (returns value-to-write)) where n is the sample number
	;;
	;; return nil for undefined value, ie, "eof" (expected to be called
	;; sequentially, although not required)
	;;
	;; 12/14/17 sampling-rate default was 176400, also used 44100 and 88200 previously

	(defl write-wave-to-stream (s fcn &key sampling-rate)
	  (let ((subchunk1size 16)
			(nchannels 1)
			(bitspersample 16)
			(nsamples 0))
		(write-str s "RIFF")
		(let ((totalsize-file-pos (file-position s)))
		  (write-int32 s 0)													;; totalsize
		  (write-str s "WAVE")
		  (write-str s "fmt ")												;; subchunk1id -- pcm format
		  (write-int32 s 16)												;; subchunk1 size -- constant for pcm format
		  (write-int16 s 1)													;; pcm
		  (write-int16 s nchannels)											;; n channels (= 1)
		  (write-int32 s sampling-rate)										;; sampling rate
		  (write-int32 s (* sampling-rate nchannels (/ bitspersample 8)))	;; byte rate
		  (write-int16 s (* nchannels (/ bitspersample 8)))					;; block align
		  (write-int16 s bitspersample)										;; bitspersample
		  (write-str s "data")												;; subchunk2id -- data
		  (let ((subchunk2size-file-pos (file-position s)))
			(write-int32 s 0)												;; subchunk2 size
			(let ((n 0))
			  (block xxx
				(loop
				 (let ((v (funcall fcn n)))
				   (if (null v)
					   (return-from xxx nil)
					   (progn
						 #|
						 (when (= (mod n 100) 0)
						 (print (list (get-universal-time) n)))
						 |#
						 ;; (print (round v))
						 (write-int16 s (round v)))))
				 (setq n (+ n 1))))
			  (let ((nsamples n))
				(let ((subchunk2size (* nsamples nchannels (/ bitspersample 8))))
				  (file-position s subchunk2size-file-pos)
				  (write-int32 s subchunk2size)
				  (let ((totalsize (+ 4 (+ 8 subchunk1size) (+ 8 subchunk2size))))
					(file-position s totalsize-file-pos)
					(write-int32 s totalsize)))))))))

	(defun write-wave (file fcn &key (sampling-rate 44100))
	  (with-open-file (s file :direction :output :element-type 'unsigned-byte)
		(write-wave-to-stream s fcn :sampling-rate sampling-rate)))


	;; (read-wave s)
	;;
	;; Returns list (sampling-rate per-channel array of sample arrays, [c1[],c2[],...,cn[]])

	(defun read-wave (s)
	  (let ((riff (read-str s 4)))
		(let ((totalsize (read-int32 s)))
		  (let ((wave (read-str s 4)))
			(let ((subchunk1id (read-str s 4)))
			  (let ((subchunk1-size (read-int32 s)))
				(let ((subchunk1-code (read-int16 s)))
				  (let ((nchannels (read-int16 s)))
					(let ((sampling-rate (read-int32 s)))
					  (let ((byte-rate (read-int32 s)))
						(let ((block-align (read-int16 s)))
						  (let ((bits-per-sample (read-int16 s)))
							(when (= subchunk1-size 18)
							  (read-int16 s))
							(let ((subchunk2id (read-str s 4)))
							  (let ((subchunk2-size (read-int32 s)))
								(let ((bytes-per-sample (/ bits-per-sample 8)))
								  (let ((nsamples (/ subchunk2-size bytes-per-sample)))
									(print (list riff totalsize wave subchunk1id subchunk1-size subchunk1-code
												 nchannels sampling-rate byte-rate block-align bits-per-sample bytes-per-sample nsamples))
									(let ((c (make-array nchannels))
										  (n (/ nsamples nchannels)))
									  (dotimes (i nchannels)
										(setf (aref c i) (make-array n)))
									  (dotimes (i n)
										(dotimes (j nchannels)
										  (let ((sample (read-int s bytes-per-sample)))
											(setf (aref (aref c j) i) sample))))
									  (list sampling-rate c))))))))))))))))))


	;; Basic 1/sqrt(n) harmonic amplitude drop, and logn increase in harmonic frequency

	(defun test-wave ()
	  (write-wave "xxx.wav"
				  (lambda (n) 
					(when (= (mod n 1000) 0)
					  (print n))
					(if (>= n 100000)
						nil
						(let ((tee (/ n 10.0)))
						  (let ((f .5))
							(round (* 5000
									  (sum 1 10 (lambda (n)
												  (* (/ 1 (sqrt n)) (sin (* (log n) f tee)))))))))))))


))

