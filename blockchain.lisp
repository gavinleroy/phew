(defpackage :blockchain
  (:use :cl :ironclad))

(in-package :blockchain)

(defun sha256 (data)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array data))))

;; global-prefix-length = 3
(defparameter *global-prefix-length* 3)

;; expected-prefix = string-repeat("0", global-prefix-length)
(defparameter *expected-prefix*
  (make-string *global-prefix-length* :initial-element #\0))

;; data Block:
;;   | block(
;;       ref hash :: String,
;;       prev-hash :: String,
;;       ref datum :: String,
;;       timestamp :: String,
;;       ref nonce :: Number)
;; end
(defstruct cblock
  hash
  prev-hash
  datum
  timestamp
  nonce)

(destructuring-bind (hash prev-hash datum timestamp nonce)
    (make-genesis-block)
  (list hash prev-hash datum timestamp nonce))

;; fun mine-block(datum :: String, prev-hash :: String) -> Block:
;;   timestamp = "fixed for testing"
;;   fun iter(nonce :: Number):
;;     data-to-hash =
;;       prev-hash + datum + timestamp + num-to-string(nonce)
;;     h = sha(data-to-hash)
;;     actual-prefix = string-substring(h, 0, global-prefix-length)
;;     if actual-prefix == expected-prefix:
;;       block(h, prev-hash, datum, timestamp, nonce)
;;     else:
;;       iter(nonce + 1)
;;     end
;;   end
;;   iter(0)
;; end
(defun mine-block (datum prev-hash)
  (let ((timestamp "fixed for testing"))
    (labels ((iter (nonce)
               (let* ((data-to-hash
                        (concatenate 'string prev-hash datum timestamp
                                     (write-to-string nonce)))
                      (h (sha256 data-to-hash))
                      (actual-prefix (subseq h 0 *global-prefix-length*)))
                 (if (string= actual-prefix *expected-prefix*)
                     (make-cblock :hash h :prev-hash prev-hash
                                  :datum datum :timestamp timestamp
                                  :nonce nonce)
                     (iter (1+ nonce))))))
      (iter 0))))

;; fun make-genesis-block() -> Block:
;;   mine-block("genesis block", "0")
;; end
(defun make-genesis-block ()
  (mine-block "genesis block" "0"))

;; data Blockchain:
;;   | blkchn(ref lob :: List<Block>)
;; end
(defstruct blockchain
  lob)

;; fun make-blockchain() -> Blockchain:
;;   first-b = make-genesis-block()
;;   blkchn([list: first-b])
;; end
(defun mk-blockchain ()
  (make-blockchain :lob (list (make-genesis-block))))

;; fun add-data-to-blockchain(bc :: Blockchain, datum :: String):
;;   old-newest-block =
;;     cases (List) bc!lob:
;;       | empty => raise("should never be empty")
;;       | link(f, _) => f
;;     end
;;   new-block = mine-block(datum, old-newest-block!hash)
;;   bc!{lob: link(new-block, bc!lob)}
;; end
(defun add-data-to-blockchain (bc datum)
  (let* ((old-newest-block (first (blockchain-lob bc)))
         (new-block (mine-block datum (cblock-prev-hash old-newest-block))))
    (setf (blockchain-lob bc)
          (cons new-block (blockchain-lob bc)))))

;; fun block-hash-prefix-okay(b :: Block) -> Boolean:
;;   actual-prefix = string-substring(b!hash, 0, global-prefix-length)
;;   expected-prefix == actual-prefix
;; end
(defun block-hash-prefix-okay (b)
  (string= *expected-prefix*
           (subseq (cblock-hash b) 0 *global-prefix-length*)))

;; fun calc-block-hash(b :: Block) -> String:
;;   data-to-hash =
;;     b.prev-hash + b!datum + b.timestamp + num-to-string(b!nonce)
;;   sha(data-to-hash)
;; end
(defun calc-block-hash (b)
  (sha256 (concatenate 'string (cblock-prev-hash b)
                       (cblock-datum b) (cblock-timestamp b)
                       (write-to-string (cblock-nonce b)))))

;; fun verify-blockchain(bc :: Blockchain) -> Boolean:
;;   fun verify-blocks(bs :: List<Block>) -> Boolean:
;;     cases (List) bs:
;;       | empty => raise("should never be empty")
;;       | link(first-block, rest-blocks) =>
;;         (first-block!hash == calc-block-hash(first-block))
;;         and
;;         (block-hash-prefix-okay(first-block))
;;         and
;;         cases (List) rest-blocks:
;;           | empty => true
;;           | link(next-block, _) =>
;;             (first-block.prev-hash == next-block!hash)
;;             and
;;             (verify-blocks(rest-blocks))
;;         end
;;     end
;;   end
;;   verify-blocks(bc!lob)
;; end
(defun verify-blockchain (bc)
  (labels ((verify-blocks (bs)
             (let ((first-block (first bs))
                   (rest-blocks (rest bs)))
               (and (string= (cblock-hash first-block)
                             (calc-block-hash first-block))
                    (block-hash-prefix-okay first-block)
                    (if rest-blocks
                        (let ((next-block (first rest-blocks)))
                          (and (string= (cblock-prev-hash first-block)
                                        (cblock-hash next-block))
                               (verify-blocks rest-blocks)))
                        t)))))
    (verify-blocks (blockchain-lob bc))))

;; fun get-block(bc :: Blockchain, idx :: Number) -> Block:
;;   bc!lob.get(idx)
;; end
(defun get-block (bc idx)
  (nth idx (blockchain-lob bc)))

;; fun re-mine-block(b :: Block) -> Nothing block:

;;   fun set-block-hash():
;;     h = calc-block-hash(b)
;;     b!{hash: h}
;;   end

;;   b!{nonce: 1 + b!nonce}
;;   set-block-hash()
;;   if block-hash-prefix-okay(b) block:
;;     nothing
;;   else:
;;     re-mine-block(b)
;;   end
;; end
(defun re-mine-block (b)
  (labels ((set-block-hash ()
             (let ((h (calc-block-hash b)))
               (setf (cblock-hash b) h)))
           (re-mine-block-iter ()
             (setf (cblock-nonce b) (1+ (cblock-nonce b)))
             (set-block-hash)
             (if (block-hash-prefix-okay b)
                 nil
                 (re-mine-block-iter))))
    (re-mine-block-iter)))

;; fun tamper(bc :: Blockchain, idx :: Number, new-datum :: String,
;;     redo-pow :: Boolean) -> Nothing block:
;;   b = get-block(bc, idx)
;;   b!{datum: new-datum}
;;   when redo-pow:
;;     re-mine-block(b)
;;   end
;; end
(defun tamper (bc idx new-datum redo-pow)
  (let ((b (get-block bc idx)))
    (setf (cblock-datum b) new-datum)
    (when redo-pow
      (re-mine-block b))))

;; bc1 = make-blockchain()
(defconstant bc1
  (let ((tmp (mk-blockchain)))
    ;; add-data-to-blockchain(bc1, "hello")
    (add-data-to-blockchain tmp "hello")
    ;; add-data-to-blockchain(bc1, "world")
    (add-data-to-blockchain tmp "world")
    tmp))

;;   tamper(bc1, 0, "world", false)
;;   bc1 satisfies verify-blockchain

;;   tamper(bc1, 1, "Hello", false)
;;   bc1 violates verify-blockchain

;;   tamper(bc1, 1, "hello", false)
;;   bc1 satisfies verify-blockchain

;;   tamper(bc1, 0, "World", true)
;;   bc1 satisfies verify-blockchain # because it's the *newest* block

;;   tamper(bc1, 0, "world", true)
;;   bc1 satisfies verify-blockchain # because it's the *newest* block

;;   tamper(bc1, 1, "Hello", true)
;;   bc1 violates verify-blockchain

;;   tamper(bc1, 1, "hello", false)
;;   bc1 violates verify-blockchain

;;   tamper(bc1, 1, "hello", true)
;;   bc1 violates verify-blockchain
;; end
