#lang racket

;; A library for playing cards

(provide
 Spade Heart Diamond Club Ranks Suits Deck
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
 shuffle-cards deal-hand )

(define Spade "♠")
(define Heart "♥")
(define Diamond "♦")
(define Club "♣")

(define Ranks #("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"))
(define Suits #("♠" "♥" "♦" "♣"))
(define Deck
  (list 🂡 🂢 🂣 🂤 🂥 🂦 🂧 🂨 🂩 🂪 🂫 🂭 🂮
        🂱 🂲 🂳 🂴 🂵 🂶 🂷 🂸 🂹 🂺 🂻 🂽 🂾
        🃁 🃂 🃃 🃄 🃅 🃆 🃇 🃈 🃉 🃊 🃋 🃍 🃎
        🃑 🃒 🃓 🃔 🃕 🃖 🃗 🃘 🃙 🃚 🃛 🃝 🃞 ) )

(define-struct card (rank suit))

;; rank-name string -> string
;; returns the name of a rank
(define (rank-name r)
  (match r
    ["A" "Ace"]
    ["J" "Jack"]
    ["Q" "Queen"]
    ["K" "King"]
    ["W" "White"]
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

(define-syntax make-cards
  (syntax-rules ()
    [(_ (r ...)
        ((ns ...) (cs ...))
        ((nh ...) (ch ...))
        ((nd ...) (cd ...))
        ((nc ...) (cc ...)) )
     (begin (define cs (make-card r "♠")) ...
            (define ns cs) ...
            (define ch (make-card r "♥")) ...
            (define nh ch) ...
            (define cd (make-card r "♦")) ...
            (define nd cd) ...
            (define cc (make-card r "♣")) ...
            (define nc cc) ... ) ] ) )

(make-cards ( "A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" )
            ((SA  S2  S3  S4  S5  S6  S7  S8  S9  S10  SJ  SQ  SK)
             (🂡  🂢  🂣  🂤  🂥  🂦  🂧  🂨  🂩  🂪   🂫  🂭  🂮) )
            ((HA  H2  H3  H4  H5  H6  H7  H8  H9  H10  HJ  HQ  HK)
             (🂱  🂲  🂳  🂴  🂵  🂶  🂷  🂸  🂹  🂺   🂻  🂽  🂾) )
            ((DA  D2  D3  D4  D5  D6  D7  D8  D9  D10  DJ  DQ  DK)
             (🃁  🃂  🃃  🃄  🃅  🃆  🃇  🃈  🃉  🃊   🃋  🃍  🃎) )
            ((CA  C2  C3  C4  C5  C6  C7  C8  C9  C10  CJ  CQ  CK)
             (🃑  🃒  🃓  🃔  🃕  🃖  🃗  🃘  🃙  🃚   🃛  🃝  🃞) ) )

#|
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
(define SA 🂡)
(define S2 🂢)
(define S3 🂣)
(define S4 🂤)
(define S5 🂥)
(define S6 🂦)
(define S7 🂧)
(define S8 🂨)
(define S9 🂩)
(define S10 🂪)
(define SJ 🂫)
(define SQ 🂭)
(define SK 🂮)

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
(define HA 🂱)
(define H2 🂲)
(define H3 🂳)
(define H4 🂴)
(define H5 🂵)
(define H6 🂶)
(define H7 🂷)
(define H8 🂸)
(define H9 🂹)
(define H10 🂺)
(define HJ 🂻)
(define HQ 🂽)
(define HK 🂾)

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
(define DA 🃁)
(define D2 🃂)
(define D3 🃃)
(define D4 🃄)
(define D5 🃅)
(define D6 🃆)
(define D7 🃇)
(define D8 🃈)
(define D9 🃉)
(define D10 🃊)
(define DJ 🃋)
(define DQ 🃍)
(define DK 🃎)

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
(define 🃞 (make-card "K" "♣"))
(define CA 🃑)
(define C2 🃒)
(define C3 🃓)
(define C4 🃔)
(define C5 🃕)
(define C6 🃖)
(define C7 🃗)
(define C8 🃘)
(define C9 🃙)
(define C10 🃚)
(define CJ 🃛)
(define CQ 🃝)
(define CK 🃞)
|#

(define-syntax match-cards
  (syntax-rules ()
    [(_ x (r ...) (cs ...) (ch ...) (cd ...) (cc ...))
     (match x
       [(struct card (r "♠")) cs] ...
       [(struct card (r "♥")) ch] ...
       [(struct card (r "♦")) cd] ...
       [(struct card (r "♣")) cc] ... ) ] ) )

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

#|
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
    [(cons (list) _) false]
    [(cons _ (list)) false]
    [(cons (list c1 cs1 ...)
           (list c2 cs2 ...) )
     (and (card=? c1 c2)
          (cards=? cs1 cs2) ) ] ) )

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
