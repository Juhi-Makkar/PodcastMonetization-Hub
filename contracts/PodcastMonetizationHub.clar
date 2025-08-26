;; PodcastMonetization Hub Smart Contract
;; A decentralized platform for podcast monetization with listener contributions and sponsor matching

;; Define the fungible token for platform rewards
(define-fungible-token podcast-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-podcast-not-found (err u104))
(define-constant err-already-sponsored (err u105))

;; Data Variables
(define-data-var total-podcasts uint u0)
(define-data-var total-contributions uint u0)

;; Data Maps
;; Podcast registration: podcast-id -> {creator: principal, title: string, contribution-pool: uint, sponsored: bool}
(define-map podcasts uint {
  creator: principal,
  title: (string-ascii 100),
  contribution-pool: uint,
  sponsored: bool
})

;; Listener contributions: {podcast-id, contributor} -> amount
(define-map contributions {podcast-id: uint, contributor: principal} uint)

;; Sponsor matching: {podcast-id, sponsor} -> {amount: uint, matched: bool}
(define-map sponsor-matches {podcast-id: uint, sponsor: principal} {
  amount: uint,
  matched: bool
})

;; Function 1: Contribute to Podcast
;; Allows listeners to contribute STX to their favorite podcasts
(define-public (contribute-to-podcast (podcast-id uint) (amount uint))
  (let (
    (podcast (unwrap! (map-get? podcasts podcast-id) err-podcast-not-found))
    (current-contribution (default-to u0 (map-get? contributions {podcast-id: podcast-id, contributor: tx-sender})))
    (current-pool (get contribution-pool podcast))
  )
    ;; Validate input
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer STX from contributor to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update contributor's total contribution to this podcast
    (map-set contributions 
      {podcast-id: podcast-id, contributor: tx-sender}
      (+ current-contribution amount))
    
    ;; Update podcast contribution pool
    (map-set podcasts podcast-id
      (merge podcast {contribution-pool: (+ current-pool amount)}))
    
    ;; Update total contributions counter
    (var-set total-contributions (+ (var-get total-contributions) amount))
    
    ;; Mint reward tokens to contributor (1:1 ratio for simplicity)
    (try! (ft-mint? podcast-token amount tx-sender))
    
    ;; Print contribution event
    (print {
      event: "contribution",
      podcast-id: podcast-id,
      contributor: tx-sender,
      amount: amount,
      total-pool: (+ current-pool amount)
    })
    
    (ok true)
  )
)

;; Function 2: Sponsor Match Podcast
;; Allows sponsors to match listener contributions with additional funding
(define-public (sponsor-match-podcast (podcast-id uint) (match-amount uint))
  (let (
    (podcast (unwrap! (map-get? podcasts podcast-id) err-podcast-not-found))
    (current-pool (get contribution-pool podcast))
    (existing-match (map-get? sponsor-matches {podcast-id: podcast-id, sponsor: tx-sender}))
  )
    ;; Validate input
    (asserts! (> match-amount u0) err-invalid-amount)
    (asserts! (> current-pool u0) err-insufficient-balance) ;; Must have existing contributions to match
    
    ;; Check if sponsor hasn't already matched this podcast
    (asserts! (is-none existing-match) err-already-sponsored)
    
    ;; Transfer STX from sponsor to contract
    (try! (stx-transfer? match-amount tx-sender (as-contract tx-sender)))
    
    ;; Record sponsor match
    (map-set sponsor-matches 
      {podcast-id: podcast-id, sponsor: tx-sender}
      {amount: match-amount, matched: true})
    
    ;; Update podcast pool with sponsor match
    (map-set podcasts podcast-id
      (merge podcast {
        contribution-pool: (+ current-pool match-amount),
        sponsored: true
      }))
    
    ;; Calculate and distribute rewards
    ;; Sponsor gets 2x reward tokens for matching
    (try! (ft-mint? podcast-token (* match-amount u2) tx-sender))
    
    ;; Podcast creator gets bonus tokens (50% of match amount)
    (try! (ft-mint? podcast-token (/ match-amount u2) (get creator podcast)))
    
    ;; Print sponsor match event
    (print {
      event: "sponsor-match",
      podcast-id: podcast-id,
      sponsor: tx-sender,
      match-amount: match-amount,
      total-pool: (+ current-pool match-amount),
      creator-bonus: (/ match-amount u2)
    })
    
    (ok true)
  )
)

;; Read-only functions for querying data

;; Get podcast information
(define-read-only (get-podcast-info (podcast-id uint))
  (ok (map-get? podcasts podcast-id)))

;; Get contributor's contribution to a specific podcast
(define-read-only (get-contribution (podcast-id uint) (contributor principal))
  (ok (map-get? contributions {podcast-id: podcast-id, contributor: contributor})))

;; Get sponsor match information
(define-read-only (get-sponsor-match (podcast-id uint) (sponsor principal))
  (ok (map-get? sponsor-matches {podcast-id: podcast-id, sponsor: sponsor})))

;; Get user's podcast token balance
(define-read-only (get-token-balance (user principal))
  (ok (ft-get-balance podcast-token user)))

;; Get total platform statistics
(define-read-only (get-platform-stats)
  (ok {
    total-podcasts: (var-get total-podcasts),
    total-contributions: (var-get total-contributions)
  }))

;; Helper function to register a new podcast (owner only for demo)
(define-public (register-podcast (title (string-ascii 100)))
  (let (
    (podcast-id (+ (var-get total-podcasts) u1))
  )
    ;; Only contract owner can register podcasts in this demo
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Register the podcast
    (map-set podcasts podcast-id {
      creator: tx-sender,
      title: title,
      contribution-pool: u0,
      sponsored: false
    })
    
    ;; Increment podcast counter
    (var-set total-podcasts podcast-id)
    
    (print {event: "podcast-registered", podcast-id: podcast-id, title: title})
    (ok podcast-id)
  )
)