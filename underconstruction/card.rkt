#lang racket

;; A library for poker cards

(provide Spade Heart Diamond Club Ranks Suits Deck
         card card? make-card card-rank card-suit rank-name suit-name card-name
         SA S2 S3 S4 S5 S6 S7 S8 S9 S10 SJ SQ SK
         🂡 🂢 🂣 🂤 🂥 🂦 🂧 🂨 🂩 🂪  🂫 🂭 🂮
         HA H2 H3 H4 H5 H6 H7 H8 H9 H10 HJ HQ HK
         🂱 🂲 🂳 🂴 🂵 🂶 🂷 🂸 🂹 🂺  🂻 🂽 🂾
         DA D2 D3 D4 D5 D6 D7 D8 D9 D10 DJ DQ DK
         🃁 🃂 🃃 🃄 🃅 🃆 🃇 🃈 🃉 🃊  🃋 🃍 🃎
         CA C2 C3 C4 C5 C6 C7 C8 C9 C10 CJ CQ CK
         🃑 🃒 🃓 🃔 🃕 🃖 🃗 🃘 🃙 🃚  🃛 🃝 🃞
         show-card show-cards
         rank=? rank<? rank>?
         card=? card<? card>?
         cards=? cards<a? cards>a? cards<l? cards>l?
         asort-cards dsort-cards
         flush? straight?
         shuffle-cards group-cards deal-hand )

(define Spade "♠")
(define Heart "♥")
(define Diamond "♦")
(define Club "♣")

(define Ranks #("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"))
(define Suits #("♠" "♥" "♦" "♣"))

(define-struct card (rank suit))

;; rank-name string -> string
;; returns the name of a rank
(define (rank-name r)
  (match r
    ["A" "Ace"]
    ["J" "Jack"]
    ["Q" "Queen"]
    ["K" "King"]
    [_   r] ) )

;; suit-name : string -> string
;; returns the name of a suit
(define (suit-name s)
  (match s
    ["♠" "Spade"]
    ["♥" "Heart"]
    ["♦" "Diamond"]
    ["♣" "Club"] ) )

;; card-name : card -> string
;; returns the name of a card
(define (card-name c)
  (match c
    [(struct card (r s))
     (string-append (rank-name r) " of " (suit-name s) "s") ] ) )

(define-syntax-rule (make-cards (r ...)
                                (cs ...)
                                (ch ...)
                                (cd ...)
                                (cc ...) )
  (begin (define cs (make-card r "♠")) ...
         (define ch (make-card r "♥")) ...
         (define cd (make-card r "♦")) ...
         (define cc (make-card r "♣")) ... ) )

(make-cards ("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K")
            ( 🂡  🂢  🂣  🂤  🂥  🂦  🂧  🂨  🂩  🂪   🂫  🂭  🂮)
            ( 🂱  🂲  🂳  🂴  🂵  🂶  🂷  🂸  🂹  🂺   🂻  🂽  🂾)
            ( 🃁  🃂  🃃  🃄  🃅  🃆  🃇  🃈  🃉  🃊   🃋  🃍  🃎)
            ( 🃑  🃒  🃓  🃔  🃕  🃖  🃗  🃘  🃙  🃚   🃛  🃝  🃞) )

#| expanded code

(begin
  (define 🂡 (make-card "A" "♠"))
  (define 🂢 (make-card "2" "♠"))
  (define 🂣 (make-card "3" "♠"))
  (define 🂤 (make-card "4" "♠"))
  (define 🂥 (make-card "5" "♠"))
  (define 🂦 (make-card "6" "♠"))
  (define 🂧 (make-card "7" "♠"))
  (define 🂨 (make-card "8" "♠"))
  (define 🂩 (make-card "9" "♠"))
  (define 🂪 (make-card "10" "♠"))
  (define 🂫 (make-card "J" "♠"))
  (define 🂭 (make-card "Q" "♠"))
  (define 🂮 (make-card "K" "♠"))
  (define 🂱 (make-card "A" "♥"))
  (define 🂲 (make-card "2" "♥"))
  (define 🂳 (make-card "3" "♥"))
  (define 🂴 (make-card "4" "♥"))
  (define 🂵 (make-card "5" "♥"))
  (define 🂶 (make-card "6" "♥"))
  (define 🂷 (make-card "7" "♥"))
  (define 🂸 (make-card "8" "♥"))
  (define 🂹 (make-card "9" "♥"))
  (define 🂺 (make-card "10" "♥"))
  (define 🂻 (make-card "J" "♥"))
  (define 🂽 (make-card "Q" "♥"))
  (define 🂾 (make-card "K" "♥"))
  (define 🃁 (make-card "A" "♦"))
  (define 🃂 (make-card "2" "♦"))
  (define 🃃 (make-card "3" "♦"))
  (define 🃄 (make-card "4" "♦"))
  (define 🃅 (make-card "5" "♦"))
  (define 🃆 (make-card "6" "♦"))
  (define 🃇 (make-card "7" "♦"))
  (define 🃈 (make-card "8" "♦"))
  (define 🃉 (make-card "9" "♦"))
  (define 🃊 (make-card "10" "♦"))
  (define 🃋 (make-card "J" "♦"))
  (define 🃍 (make-card "Q" "♦"))
  (define 🃎 (make-card "K" "♦"))
  (define 🃑 (make-card "A" "♣"))
  (define 🃒 (make-card "2" "♣"))
  (define 🃓 (make-card "3" "♣"))
  (define 🃔 (make-card "4" "♣"))
  (define 🃕 (make-card "5" "♣"))
  (define 🃖 (make-card "6" "♣"))
  (define 🃗 (make-card "7" "♣"))
  (define 🃘 (make-card "8" "♣"))
  (define 🃙 (make-card "9" "♣"))
  (define 🃚 (make-card "10" "♣"))
  (define 🃛 (make-card "J" "♣"))
  (define 🃝 (make-card "Q" "♣"))
  (define 🃞 (make-card "K" "♣")) )
|#

(define Deck
  (list 🂡 🂢 🂣 🂤 🂥 🂦 🂧 🂨 🂩 🂪 🂫 🂭 🂮
        🂱 🂲 🂳 🂴 🂵 🂶 🂷 🂸 🂹 🂺 🂻 🂽 🂾
        🃁 🃂 🃃 🃄 🃅 🃆 🃇 🃈 🃉 🃊 🃋 🃍 🃎
        🃑 🃒 🃓 🃔 🃕 🃖 🃗 🃘 🃙 🃚 🃛 🃝 🃞 ) )

(define-syntax-rule (bind-ids (id1 ...) (id2 ...))
  (begin (define-syntax id1
           (syntax-id-rules ()
             [id1 id2] ) )
         ...) )

(bind-ids (SA  S2  S3  S4  S5  S6  S7  S8  S9  S10  SJ  SQ  SK
           HA  H2  H3  H4  H5  H6  H7  H8  H9  H10  HJ  HQ  HK
           DA  D2  D3  D4  D5  D6  D7  D8  D9  D10  DJ  DQ  DK
           CA  C2  C3  C4  C5  C6  C7  C8  C9  C10  CJ  CQ  CK )
          (🂡  🂢  🂣  🂤  🂥  🂦  🂧  🂨  🂩   🂪  🂫  🂭  🂮
           🂱  🂲  🂳  🂴  🂵  🂶  🂷  🂸  🂹   🂺  🂻  🂽  🂾
           🃁  🃂  🃃  🃄  🃅  🃆  🃇  🃈  🃉   🃊  🃋  🃍  🃎
           🃑  🃒  🃓  🃔  🃕  🃖  🃗  🃘  🃙   🃚  🃛  🃝  🃞 ) )

#| expanded code

(begin
  (define-syntax SA (syntax-id-rules () [SA 🂡]))
  (define-syntax S2 (syntax-id-rules () [S2 🂢]))
  (define-syntax S3 (syntax-id-rules () [S3 🂣]))
  (define-syntax S4 (syntax-id-rules () [S4 🂤]))
  (define-syntax S5 (syntax-id-rules () [S5 🂥]))
  (define-syntax S6 (syntax-id-rules () [S6 🂦]))
  (define-syntax S7 (syntax-id-rules () [S7 🂧]))
  (define-syntax S8 (syntax-id-rules () [S8 🂨]))
  (define-syntax S9 (syntax-id-rules () [S9 🂩]))
  (define-syntax S10 (syntax-id-rules () [S10 🂪]))
  (define-syntax SJ (syntax-id-rules () [SJ 🂫]))
  (define-syntax SQ (syntax-id-rules () [SQ 🂭]))
  (define-syntax SK (syntax-id-rules () [SK 🂮]))
  (define-syntax HA (syntax-id-rules () [HA 🂱]))
  (define-syntax H2 (syntax-id-rules () [H2 🂲]))
  (define-syntax H3 (syntax-id-rules () [H3 🂳]))
  (define-syntax H4 (syntax-id-rules () [H4 🂴]))
  (define-syntax H5 (syntax-id-rules () [H5 🂵]))
  (define-syntax H6 (syntax-id-rules () [H6 🂶]))
  (define-syntax H7 (syntax-id-rules () [H7 🂷]))
  (define-syntax H8 (syntax-id-rules () [H8 🂸]))
  (define-syntax H9 (syntax-id-rules () [H9 🂹]))
  (define-syntax H10 (syntax-id-rules () [H10 🂺]))
  (define-syntax HJ (syntax-id-rules () [HJ 🂻]))
  (define-syntax HQ (syntax-id-rules () [HQ 🂽]))
  (define-syntax HK (syntax-id-rules () [HK 🂾]))
  (define-syntax DA (syntax-id-rules () [DA 🃁]))
  (define-syntax D2 (syntax-id-rules () [D2 🃂]))
  (define-syntax D3 (syntax-id-rules () [D3 🃃]))
  (define-syntax D4 (syntax-id-rules () [D4 🃄]))
  (define-syntax D5 (syntax-id-rules () [D5 🃅]))
  (define-syntax D6 (syntax-id-rules () [D6 🃆]))
  (define-syntax D7 (syntax-id-rules () [D7 🃇]))
  (define-syntax D8 (syntax-id-rules () [D8 🃈]))
  (define-syntax D9 (syntax-id-rules () [D9 🃉]))
  (define-syntax D10 (syntax-id-rules () [D10 🃊]))
  (define-syntax DJ (syntax-id-rules () [DJ 🃋]))
  (define-syntax DQ (syntax-id-rules () [DQ 🃍]))
  (define-syntax DK (syntax-id-rules () [DK 🃎]))
  (define-syntax CA (syntax-id-rules () [CA 🃑]))
  (define-syntax C2 (syntax-id-rules () [C2 🃒]))
  (define-syntax C3 (syntax-id-rules () [C3 🃓]))
  (define-syntax C4 (syntax-id-rules () [C4 🃔]))
  (define-syntax C5 (syntax-id-rules () [C5 🃕]))
  (define-syntax C6 (syntax-id-rules () [C6 🃖]))
  (define-syntax C7 (syntax-id-rules () [C7 🃗]))
  (define-syntax C8 (syntax-id-rules () [C8 🃘]))
  (define-syntax C9 (syntax-id-rules () [C9 🃙]))
  (define-syntax C10 (syntax-id-rules () [C10 🃚]))
  (define-syntax CJ (syntax-id-rules () [CJ 🃛]))
  (define-syntax CQ (syntax-id-rules () [CQ 🃝]))
  (define-syntax CK (syntax-id-rules () [CK 🃞])) )
|#

(define-syntax-rule (match-cards x
                                 (r ...)
                                 (cs ...)
                                 (ch ...)
                                 (cd ...)
                                 (cc ...) )
  (match x
    [(struct card (r "♠")) cs] ...
    [(struct card (r "♥")) ch] ...
    [(struct card (r "♦")) cd] ...
    [(struct card (r "♣")) cc] ... ) )

;; show-card : card -> void
;; shows a card
;; defined as a function thus granted first-class object status.
(define (show-card c)
  (display
   (match-cards c
                ("A"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "J"  "Q"  "K" )
                ("🂡" "🂢" "🂣" "🂤" "🂥" "🂦" "🂧" "🂨" "🂩" "🂪" "🂫" "🂭" "🂮")
                ("🂱" "🂲" "🂳" "🂴" "🂵" "🂶" "🂷" "🂸" "🂹" "🂺" "🂻" "🂽" "🂾")
                ("🃁" "🃂" "🃃" "🃄" "🃅" "🃆" "🃇" "🃈" "🃉" "🃊" "🃋" "🃍" "🃎")
                ("🃑" "🃒" "🃓" "🃔" "🃕" "🃖" "🃗" "🃘" "🃙" "🃚" "🃛" "🃝" "🃞") ) ) )

#| expanded code

(define (show-card c)
  (display
   (match c
     [(struct card ("A" "♠")) "🂡"]
     [(struct card ("2" "♠")) "🂢"]
     [(struct card ("3" "♠")) "🂣"]
     [(struct card ("4" "♠")) "🂤"]
     [(struct card ("5" "♠")) "🂥"]
     [(struct card ("6" "♠")) "🂦"]
     [(struct card ("7" "♠")) "🂧"]
     [(struct card ("8" "♠")) "🂨"]
     [(struct card ("9" "♠")) "🂩"]
     [(struct card ("10" "♠")) "🂪"]
     [(struct card ("J" "♠")) "🂫"]
     [(struct card ("Q" "♠")) "🂭"]
     [(struct card ("K" "♠")) "🂮"]
     [(struct card ("A" "♥")) "🂱"]
     [(struct card ("2" "♥")) "🂲"]
     [(struct card ("3" "♥")) "🂳"]
     [(struct card ("4" "♥")) "🂴"]
     [(struct card ("5" "♥")) "🂵"]
     [(struct card ("6" "♥")) "🂶"]
     [(struct card ("7" "♥")) "🂷"]
     [(struct card ("8" "♥")) "🂸"]
     [(struct card ("9" "♥")) "🂹"]
     [(struct card ("10" "♥")) "🂺"]
     [(struct card ("J" "♥")) "🂻"]
     [(struct card ("Q" "♥")) "🂽"]
     [(struct card ("K" "♥")) "🂾"]
     [(struct card ("A" "♦")) "🃁"]
     [(struct card ("2" "♦")) "🃂"]
     [(struct card ("3" "♦")) "🃃"]
     [(struct card ("4" "♦")) "🃄"]
     [(struct card ("5" "♦")) "🃅"]
     [(struct card ("6" "♦")) "🃆"]
     [(struct card ("7" "♦")) "🃇"]
     [(struct card ("8" "♦")) "🃈"]
     [(struct card ("9" "♦")) "🃉"]
     [(struct card ("10" "♦")) "🃊"]
     [(struct card ("J" "♦")) "🃋"]
     [(struct card ("Q" "♦")) "🃍"]
     [(struct card ("K" "♦")) "🃎"]
     [(struct card ("A" "♣")) "🃑"]
     [(struct card ("2" "♣")) "🃒"]
     [(struct card ("3" "♣")) "🃓"]
     [(struct card ("4" "♣")) "🃔"]
     [(struct card ("5" "♣")) "🃕"]
     [(struct card ("6" "♣")) "🃖"]
     [(struct card ("7" "♣")) "🃗"]
     [(struct card ("8" "♣")) "🃘"]
     [(struct card ("9" "♣")) "🃙"]
     [(struct card ("10" "♣"))"'🃚"]
     [(struct card ("J" "♣")) "🃛"]
     [(struct card ("Q" "♣")) "🃝"]
     [(struct card ("K" "♣")) "🃞"])))
|#

;; show-cards : (listof card) -> void
;; shows a list of cards
(define (show-cards cs)
  (for-each show-card cs) )

;; rank-ordinal : hash -> number
;; returns the ordinal number of a rank
(define (rank-ordinal r)
  (hash-ref
   (hash "2"   2
         "3"   3
         "4"   4
         "5"   5
         "6"   6
         "7"   7
         "8"   8
         "9"   9
         "10" 10
         "J"  11
         "Q"  12
         "K"  13
         "A"  14 )
   r ) )

;; rank=? : string string -> boolean
;; tests if one rank is equal to the other
(define (rank=? r1 r2)
  (= (rank-ordinal r1)
     (rank-ordinal r2) ) )

;; rank<? : string string -> boolean
;; tests if one rank is less than the other
(define (rank<? r1 r2)
  (< (rank-ordinal r1)
     (rank-ordinal r2) ) )

;; rank>? : string string -> boolean
;; tests if one rank is greater than the other
(define (rank>? r1 r2)
  (> (rank-ordinal r1)
     (rank-ordinal r2) ) )

;; card=? : card card -> boolean
;; tests if one card's rank is equal to the other's
(define (card=? c1 c2)
  (rank=? (card-rank c1)
          (card-rank c2) ) )

;; card>? : card card -> boolean
;; tests if one card's rank is less than the other's
(define (card<? c1 c2)
  (rank<? (card-rank c1)
          (card-rank c2) ) )

;; card>? : card card -> boolean
;; tests if one card's rank is greater than the other's
(define (card>? c1 c2)
  (rank>? (card-rank c1)
          (card-rank c2) ) )

;; cards=? : (listof card) (listof card) -> boolean
;; tests if one list of cards is equal to the other
(define (cards=? cs1 cs2)
  (match (cons cs1 cs2)
    [(cons (list) (list)) true]
    [(cons (list c1 cs1 ...)
           (list c2 cs2 ...) )
     (and (card=? c1 c2)
          (cards=? cs1 cs2) ) ]
    [_ false] ) )

;; cards<a? : (listof card) (listof card) -> boolean
;; tests if one list of cards is alphabetically less than the other
(define (cards<a? cs1 cs2)
  (match (cons cs1 cs2)
    [(cons (list) (list c _ ...)) true]
    [(cons (list c1 cs1 ...)
           (list c2 cs2 ...) )
     (cond [(card<? c1 c2) true]
           [(card=? c1 c2) (cards<a? cs1 cs2)]
           [else false] ) ]
    [_ false] ) )

;; cards>a? : (listof card) (listof card) -> boolean
;; tests if one list of cards is alphabetically greater than the other
(define (cards>a? cs1 cs2)
  (match (cons cs1 cs2)
    [(cons (list c _ ...) (list)) true]
    [(cons (list c1 cs1 ...)
           (list c2 cs2 ...) )
     (cond [(card>? c1 c2) true]
           [(card=? c1 c2) (cards>a? cs1 cs2)]
           [else false] ) ]
    [_ false] ) )

;; cards<l? : (listof card) (listof card) -> boolean
;; tests if one list of cards is lexicographically less than the other
(define (cards<l? cs1 cs2)
  (let [(n1 (length cs1))
        (n2 (length cs2)) ]
    (cond [(< n1 n2) true]
          [(> n1 n2) false]
          [else (cards<a? cs1 cs2)] ) ) )

;; cards>? : (listof card) (listof card) -> boolean
;; tests if one list of cards is lexicographically greater than the other
(define (cards>l? cs1 cs2)
  (let [(n1 (length cs1))
        (n2 (length cs2)) ]
    (cond [(< n1 n2) false]
          [(> n1 n2) true]
          [else (cards>a? cs1 cs2)] ) ) )

;; asort-cards : (listof card) -> (listof card)
;; sorts a list of cards in ascending order
(define (asort-cards cs)
  (sort cs card<?) )

;; dsort-cards : (listof card) -> (listof card)
;; sorts a list of cards in descending order
(define (dsort-cards cs)
  (reverse (sort cs card<?) ) )

;; flush? : (listof card) -> boolean
;; tests if a list of cards is flush
(define (flush? cs)
  (match cs
    [(list) true]
    [(list (struct card (_ s)) cs ...)
     (andmap (lambda (c)
               (string=? (card-suit c) s) )
             cs ) ] ) )

;; straight? : (listof card) -> boolean
;; tests if a sorted list of cards is straight
;; treats "A" as "1" when it comes with a straight led by "2"
(define (straight? cs)
  (define (strict-straight? cs)
    (match cs
      [(list _) true]
      [(list c1 c2 cs ...)
       (and (= (- (rank-ordinal (card-rank c2))
                  (rank-ordinal (card-rank c1)) )
               1 )
            (strict-straight? (cons c2 cs)) ) ] ) )
  (match cs
    [(list) true]
    [(list _) true]
    [(list c1 cs ... c2)
     (match (cons c1 c2)
       [(cons (struct card ("2" _))
              (struct card ("A" _)) )
        (strict-straight? (cons c1 cs)) ]
       [_ (strict-straight? (cons c1 (append cs (list c2))))] ) ] ) )

;; group-cards : (listof card) -> (listof (listof card))
;; groups a sorted list of cards into a list of lists of cards of the same rank
(define (group-cards cs)
  (match cs
    [(list) (list empty)]
    [(list c) (list (list c))]
    [(list c1 c2 cs ...)
     (let [(css (group-cards (cons c2 cs)))]
       (cond [(card=? c1 c2) (cons (cons c1 (first css))
                                   (rest css) ) ]
             [else (cons (list c1) css)] ) ) ] ) )

;; shuffle-cards : (listof card) -> (listof card)
;; shuffles cards
(define shuffle-cards shuffle)

;; deal-hand : (listof card) -> (listof card)
;; deals a hand of cards from a list of cards
(define (deal-hand cs)
  (cond [(< (length cs) 5) empty]
        [else (take cs 5)] ) )
