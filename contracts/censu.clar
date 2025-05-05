(define-data-var admin principal tx-sender)
(define-data-var total-citizens uint u0)

(define-map citizens 
  { citizen-id: uint }
  {
    name: (string-ascii 64),
    birth-year: uint,
    region: (string-ascii 32),
    registered: uint,
    status: (string-ascii 16)
  }
)

(define-map region-stats
  { region: (string-ascii 32) }
  {
    population: uint,
    avg-age: uint,
    last-updated: uint
  }
)

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-INVALID-DATA (err u102))
(define-constant ERR-NOT-FOUND (err u103))

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (ok (var-set admin new-admin))))

(define-public (register-citizen 
    (name (string-ascii 64))
    (birth-year uint)
    (region (string-ascii 32)))
  (let
    ((citizen-id (+ (var-get total-citizens) u1)))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (> birth-year u1900) ERR-INVALID-DATA)
      (map-set citizens
        { citizen-id: citizen-id }
        {
          name: name,
          birth-year: birth-year,
          region: region,
          registered: stacks-block-height,
          status: "active"
        }))
    (var-set total-citizens citizen-id)
    (update-region-stats region birth-year)
    (ok citizen-id)))

(define-public (update-citizen-status
    (citizen-id uint)
    (new-status (string-ascii 16)))
  (let
    ((citizen (unwrap! (map-get? citizens { citizen-id: citizen-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set citizens
      { citizen-id: citizen-id }
      (merge citizen { status: new-status })))))

(define-read-only (get-citizen (citizen-id uint))
  (map-get? citizens { citizen-id: citizen-id }))

(define-read-only (get-region-stats (region (string-ascii 32)))
  (map-get? region-stats { region: region }))

(define-read-only (get-total-citizens)
  (var-get total-citizens))

(define-private (update-region-stats (region (string-ascii 32)) (birth-year uint))
  (let
    ((stats (default-to
      { population: u0, avg-age: u0, last-updated: stacks-block-height }
      (map-get? region-stats { region: region })))
     (current-year u2024)
     (new-population (+ (get population stats) u1))
     (new-avg-age (/ (+ (* (get avg-age stats) (get population stats)) (- current-year birth-year)) new-population)))
    (map-set region-stats
      { region: region }
      {
        population: new-population,
        avg-age: new-avg-age,
        last-updated: stacks-block-height
      })))



(define-map citizen-migrations
  { citizen-id: uint }
  {
    from-region: (string-ascii 32),
    to-region: (string-ascii 32),
    transfer-height: uint
  }
)

(define-public (transfer-citizen
    (citizen-id uint)
    (new-region (string-ascii 32)))
  (let
    ((citizen (unwrap! (map-get? citizens { citizen-id: citizen-id }) ERR-NOT-FOUND))
     (old-region (get region citizen)))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (map-set citizen-migrations
        { citizen-id: citizen-id }
        {
          from-region: old-region,
          to-region: new-region,
          transfer-height: stacks-block-height
        })
      (map-set citizens
        { citizen-id: citizen-id }
        (merge citizen { region: new-region }))
      (try! (decrease-region-population old-region))
      (update-region-stats new-region (get birth-year citizen))
      (ok true))))
(define-private (decrease-region-population (region (string-ascii 32)))
  (let
    ((stats (unwrap! (map-get? region-stats { region: region }) ERR-NOT-FOUND)))
    (ok (map-set region-stats
      { region: region }
      (merge stats { population: (- (get population stats) u1) })))))




(define-map verifiers
  { address: principal }
  { active: bool }
)

(define-map citizen-attestations
  { citizen-id: uint }
  {
    verifier: principal,
    verification-height: uint,
    verification-type: (string-ascii 16),
    metadata: (string-ascii 128)
  }
)

(define-public (add-verifier (verifier-address principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set verifiers
      { address: verifier-address }
      { active: true }))))

(define-public (remove-verifier (verifier-address principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set verifiers
      { address: verifier-address }
      { active: false }))))

(define-public (attest-citizen
    (citizen-id uint)
    (verification-type (string-ascii 16))
    (metadata (string-ascii 128)))
  (let
    ((verifier-status (unwrap! (map-get? verifiers { address: tx-sender }) ERR-NOT-AUTHORIZED)))
    (begin
      (asserts! (get active verifier-status) ERR-NOT-AUTHORIZED)
      (asserts! (is-some (map-get? citizens { citizen-id: citizen-id })) ERR-NOT-FOUND)
      (ok (map-set citizen-attestations
        { citizen-id: citizen-id }
        {
          verifier: tx-sender,
          verification-height: stacks-block-height,
          verification-type: verification-type,
          metadata: metadata
        })))))

(define-read-only (get-citizen-attestation (citizen-id uint))
  (map-get? citizen-attestations { citizen-id: citizen-id }))