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


(define-constant ERR-VERIFICATION-EXPIRED (err u104))
(define-constant ERR-INSUFFICIENT-VERIFICATION (err u105))
(define-constant ERR-VERIFICATION-PENDING (err u106))

(define-constant VERIFICATION-DOCUMENT u1)
(define-constant VERIFICATION-BIOMETRIC u2)
(define-constant VERIFICATION-WITNESS u3)

(define-constant VERIFICATION-VALIDITY-BLOCKS u52560)

(define-map citizen-verifications
  { citizen-id: uint }
  {
    document-verified: bool,
    biometric-verified: bool,
    witness-verified: bool,
    verification-score: uint,
    last-verification: uint,
    expires-at: uint
  }
)

(define-map verification-requests
  { request-id: uint }
  {
    citizen-id: uint,
    verification-type: uint,
    submitted-at: uint,
    status: (string-ascii 16),
    evidence-hash: (string-ascii 64)
  }
)

(define-data-var next-request-id uint u1)


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


(define-public (submit-verification-request
    (citizen-id uint)
    (verification-type uint)
    (evidence-hash (string-ascii 64)))
  (let
    ((request-id (var-get next-request-id)))
    (begin
      (asserts! (is-some (map-get? citizens { citizen-id: citizen-id })) ERR-NOT-FOUND)
      (asserts! (<= verification-type u3) ERR-INVALID-DATA)
      (asserts! (>= verification-type u1) ERR-INVALID-DATA)
      (map-set verification-requests
        { request-id: request-id }
        {
          citizen-id: citizen-id,
          verification-type: verification-type,
          submitted-at: stacks-block-height,
          status: "pending",
          evidence-hash: evidence-hash
        })
      (var-set next-request-id (+ request-id u1))
      (ok request-id))))

(define-public (approve-verification-request (request-id uint))
  (let
    ((request (unwrap! (map-get? verification-requests { request-id: request-id }) ERR-NOT-FOUND))
     (citizen-id (get citizen-id request))
     (verification-type (get verification-type request))
     (current-verification (default-to
       { document-verified: false, biometric-verified: false, witness-verified: false, verification-score: u0, last-verification: u0, expires-at: u0 }
       (map-get? citizen-verifications { citizen-id: citizen-id }))))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get status request) "pending") ERR-INVALID-DATA)
      (map-set verification-requests
        { request-id: request-id }
        (merge request { status: "approved" }))
      (if (is-eq verification-type VERIFICATION-DOCUMENT)
        (update-citizen-verification citizen-id (merge current-verification { document-verified: true }))
        (if (is-eq verification-type VERIFICATION-BIOMETRIC)
          (update-citizen-verification citizen-id (merge current-verification { biometric-verified: true }))
          (update-citizen-verification citizen-id (merge current-verification { witness-verified: true }))))
      (ok true))))

(define-public (reject-verification-request (request-id uint))
  (let
    ((request (unwrap! (map-get? verification-requests { request-id: request-id }) ERR-NOT-FOUND)))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get status request) "pending") ERR-INVALID-DATA)
      (ok (map-set verification-requests
        { request-id: request-id }
        (merge request { status: "rejected" }))))))

(define-private (update-citizen-verification
    (citizen-id uint)
    (verification-data { document-verified: bool, biometric-verified: bool, witness-verified: bool, verification-score: uint, last-verification: uint, expires-at: uint }))
  (let
    ((new-score (calculate-verification-score verification-data))
     (current-block stacks-block-height)
     (expiry-block (+ current-block VERIFICATION-VALIDITY-BLOCKS)))
    (map-set citizen-verifications
      { citizen-id: citizen-id }
      (merge verification-data {
        verification-score: new-score,
        last-verification: current-block,
        expires-at: expiry-block
      }))))

(define-private (calculate-verification-score
    (verification-data { document-verified: bool, biometric-verified: bool, witness-verified: bool, verification-score: uint, last-verification: uint, expires-at: uint }))
  (let
    ((document-score (if (get document-verified verification-data) u40 u0))
     (biometric-score (if (get biometric-verified verification-data) u40 u0))
     (witness-score (if (get witness-verified verification-data) u20 u0)))
    (+ document-score (+ biometric-score witness-score))))

(define-public (register-verified-citizen
    (name (string-ascii 64))
    (birth-year uint)
    (region (string-ascii 32))
    (min-verification-score uint))
  (let
    ((citizen-id (+ (var-get total-citizens) u1))
     (verification (map-get? citizen-verifications { citizen-id: citizen-id })))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (> birth-year u1900) ERR-INVALID-DATA)
      (if (> min-verification-score u0)
        (begin
          (asserts! (is-some verification) ERR-INSUFFICIENT-VERIFICATION)
          (asserts! (>= (get verification-score (unwrap-panic verification)) min-verification-score) ERR-INSUFFICIENT-VERIFICATION)
          (asserts! (> (get expires-at (unwrap-panic verification)) stacks-block-height) ERR-VERIFICATION-EXPIRED))
        true)
      (map-set citizens
        { citizen-id: citizen-id }
        {
          name: name,
          birth-year: birth-year,
          region: region,
          registered: stacks-block-height,
          status: "verified"
        })
      (var-set total-citizens citizen-id)
      (update-region-stats region birth-year)
      (ok citizen-id))))

(define-read-only (get-citizen-verification (citizen-id uint))
  (map-get? citizen-verifications { citizen-id: citizen-id }))

(define-read-only (get-verification-request (request-id uint))
  (map-get? verification-requests { request-id: request-id }))

(define-read-only (is-citizen-verified (citizen-id uint) (min-score uint))
  (match (map-get? citizen-verifications { citizen-id: citizen-id })
    verification (and 
      (>= (get verification-score verification) min-score)
      (> (get expires-at verification) stacks-block-height))
    false))

(define-read-only (get-verification-status (citizen-id uint))
  (match (map-get? citizen-verifications { citizen-id: citizen-id })
    verification {
      score: (get verification-score verification),
      expired: (<= (get expires-at verification) stacks-block-height),
      document: (get document-verified verification),
      biometric: (get biometric-verified verification),
      witness: (get witness-verified verification)
    }
    { score: u0, expired: true, document: false, biometric: false, witness: false }))