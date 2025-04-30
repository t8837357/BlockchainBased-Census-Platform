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