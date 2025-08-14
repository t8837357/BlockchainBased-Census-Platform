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


(define-constant REWARD-REGISTRATION u50)
(define-constant REWARD-VERIFICATION u100)
(define-constant REWARD-REFERRAL u30)
(define-constant REWARD-DATA-UPDATE u25)
(define-constant REWARD-COMMUNITY-PARTICIPATION u40)

(define-constant LEVEL-BRONZE u100)
(define-constant LEVEL-SILVER u500)
(define-constant LEVEL-GOLD u1000)
(define-constant LEVEL-PLATINUM u2000)

(define-constant ERR-INSUFFICIENT-POINTS (err u107))
(define-constant ERR-INVALID-REWARD-TYPE (err u108))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u109))
(define-constant ERR-COOLDOWN-ACTIVE (err u110))

(define-map citizen-rewards
  { citizen-id: uint }
  {
    total-points: uint,
    level: (string-ascii 16),
    referrals-made: uint,
    last-data-update: uint,
    last-participation: uint,
    achievements: (list 10 (string-ascii 32))
  }
)

(define-map reward-activities
  { citizen-id: uint, activity-type: (string-ascii 32) }
  {
    timestamp: uint,
    points-earned: uint,
    details: (string-ascii 128)
  }
)

(define-map reward-redemptions
  { citizen-id: uint, redemption-id: uint }
  {
    points-spent: uint,
    benefit-type: (string-ascii 32),
    redeemed-at: uint,
    status: (string-ascii 16)
  }
)

(define-map benefit-catalog
  { benefit-id: uint }
  {
    name: (string-ascii 64),
    description: (string-ascii 128),
    cost: uint,
    category: (string-ascii 32),
    available: bool
  }
)

(define-data-var next-redemption-id uint u1)
(define-data-var total-rewards-distributed uint u0)

(define-private (initialize-citizen-rewards (citizen-id uint))
  (map-set citizen-rewards
    { citizen-id: citizen-id }
    {
      total-points: u0,
      level: "bronze",
      referrals-made: u0,
      last-data-update: u0,
      last-participation: u0,
      achievements: (list)
    }))

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


(define-private (award-points (citizen-id uint) (points uint) (activity-type (string-ascii 32)) (details (string-ascii 128)))
  (let
    ((current-rewards (default-to
      { total-points: u0, level: "bronze", referrals-made: u0, last-data-update: u0, last-participation: u0, achievements: (list) }
      (map-get? citizen-rewards { citizen-id: citizen-id })))
     (new-total (+ (get total-points current-rewards) points))
     (new-level (calculate-level new-total)))
    (begin
      (map-set citizen-rewards
        { citizen-id: citizen-id }
        (merge current-rewards {
          total-points: new-total,
          level: new-level
        }))
      (map-set reward-activities
        { citizen-id: citizen-id, activity-type: activity-type }
        {
          timestamp: stacks-block-height,
          points-earned: points,
          details: details
        })
      (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) points))
      new-total)))

(define-private (calculate-level (total-points uint))
  (if (>= total-points LEVEL-PLATINUM)
    "platinum"
    (if (>= total-points LEVEL-GOLD)
      "gold"
      (if (>= total-points LEVEL-SILVER)
        "silver"
        "bronze"))))

(define-public (claim-registration-reward (citizen-id uint))
  (let
    ((citizen (unwrap! (map-get? citizens { citizen-id: citizen-id }) ERR-NOT-FOUND)))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (is-none (map-get? reward-activities { citizen-id: citizen-id, activity-type: "registration" })) ERR-REWARD-ALREADY-CLAIMED)
      (award-points citizen-id REWARD-REGISTRATION "registration" "Initial registration reward")
      (ok true))))

(define-public (claim-verification-reward (citizen-id uint))
  (let
    ((verification (unwrap! (map-get? citizen-verifications { citizen-id: citizen-id }) ERR-NOT-FOUND)))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (>= (get verification-score verification) u60) ERR-INSUFFICIENT-VERIFICATION)
      (asserts! (is-none (map-get? reward-activities { citizen-id: citizen-id, activity-type: "verification" })) ERR-REWARD-ALREADY-CLAIMED)
      (award-points citizen-id REWARD-VERIFICATION "verification" "Successful identity verification")
      (ok true))))

(define-public (claim-referral-reward (referrer-id uint) (referee-id uint))
  (let
    ((referrer (unwrap! (map-get? citizens { citizen-id: referrer-id }) ERR-NOT-FOUND))
     (referee (unwrap! (map-get? citizens { citizen-id: referee-id }) ERR-NOT-FOUND))
     (referrer-rewards (default-to
       { total-points: u0, level: "bronze", referrals-made: u0, last-data-update: u0, last-participation: u0, achievements: (list) }
       (map-get? citizen-rewards { citizen-id: referrer-id }))))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (not (is-eq referrer-id referee-id)) ERR-INVALID-DATA)
      (map-set citizen-rewards
        { citizen-id: referrer-id }
        (merge referrer-rewards { referrals-made: (+ (get referrals-made referrer-rewards) u1) }))
      (award-points referrer-id REWARD-REFERRAL "referral" "Successfully referred new citizen")
      (ok true))))

(define-public (claim-data-update-reward (citizen-id uint))
  (let
    ((citizen (unwrap! (map-get? citizens { citizen-id: citizen-id }) ERR-NOT-FOUND))
     (current-rewards (default-to
       { total-points: u0, level: "bronze", referrals-made: u0, last-data-update: u0, last-participation: u0, achievements: (list) }
       (map-get? citizen-rewards { citizen-id: citizen-id })))
     (cooldown-blocks u1440)
     (current-block stacks-block-height))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (or (is-eq (get last-data-update current-rewards) u0) 
                    (>= (- current-block (get last-data-update current-rewards)) cooldown-blocks)) ERR-COOLDOWN-ACTIVE)
      (map-set citizen-rewards
        { citizen-id: citizen-id }
        (merge current-rewards { last-data-update: current-block }))
      (award-points citizen-id REWARD-DATA-UPDATE "data-update" "Updated census information")
      (ok true))))

(define-public (claim-participation-reward (citizen-id uint) (activity-description (string-ascii 128)))
  (let
    ((citizen (unwrap! (map-get? citizens { citizen-id: citizen-id }) ERR-NOT-FOUND))
     (current-rewards (default-to
       { total-points: u0, level: "bronze", referrals-made: u0, last-data-update: u0, last-participation: u0, achievements: (list) }
       (map-get? citizen-rewards { citizen-id: citizen-id })))
     (cooldown-blocks u2160)
     (current-block stacks-block-height))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (or (is-eq (get last-participation current-rewards) u0) 
                    (>= (- current-block (get last-participation current-rewards)) cooldown-blocks)) ERR-COOLDOWN-ACTIVE)
      (map-set citizen-rewards
        { citizen-id: citizen-id }
        (merge current-rewards { last-participation: current-block }))
      (award-points citizen-id REWARD-COMMUNITY-PARTICIPATION "participation" activity-description)
      (ok true))))

(define-public (setup-benefit (benefit-id uint) (name (string-ascii 64)) (description (string-ascii 128)) (cost uint) (category (string-ascii 32)))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set benefit-catalog
      { benefit-id: benefit-id }
      {
        name: name,
        description: description,
        cost: cost,
        category: category,
        available: true
      }))))

(define-public (redeem-benefit (citizen-id uint) (benefit-id uint))
  (let
    ((citizen (unwrap! (map-get? citizens { citizen-id: citizen-id }) ERR-NOT-FOUND))
     (benefit (unwrap! (map-get? benefit-catalog { benefit-id: benefit-id }) ERR-NOT-FOUND))
     (current-rewards (unwrap! (map-get? citizen-rewards { citizen-id: citizen-id }) ERR-NOT-FOUND))
     (redemption-id (var-get next-redemption-id)))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (get available benefit) ERR-INVALID-DATA)
      (asserts! (>= (get total-points current-rewards) (get cost benefit)) ERR-INSUFFICIENT-POINTS)
      (map-set citizen-rewards
        { citizen-id: citizen-id }
        (merge current-rewards { total-points: (- (get total-points current-rewards) (get cost benefit)) }))
      (map-set reward-redemptions
        { citizen-id: citizen-id, redemption-id: redemption-id }
        {
          points-spent: (get cost benefit),
          benefit-type: (get category benefit),
          redeemed-at: stacks-block-height,
          status: "active"
        })
      (var-set next-redemption-id (+ redemption-id u1))
      (ok redemption-id))))

(define-public (toggle-benefit-availability (benefit-id uint))
  (let
    ((benefit (unwrap! (map-get? benefit-catalog { benefit-id: benefit-id }) ERR-NOT-FOUND)))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (ok (map-set benefit-catalog
        { benefit-id: benefit-id }
        (merge benefit { available: (not (get available benefit)) }))))))

(define-read-only (get-citizen-rewards (citizen-id uint))
  (map-get? citizen-rewards { citizen-id: citizen-id }))

(define-read-only (get-reward-activity (citizen-id uint) (activity-type (string-ascii 32)))
  (map-get? reward-activities { citizen-id: citizen-id, activity-type: activity-type }))

(define-read-only (get-benefit-details (benefit-id uint))
  (map-get? benefit-catalog { benefit-id: benefit-id }))

(define-read-only (get-redemption-details (citizen-id uint) (redemption-id uint))
  (map-get? reward-redemptions { citizen-id: citizen-id, redemption-id: redemption-id }))

(define-read-only (get-total-rewards-distributed)
  (var-get total-rewards-distributed))

(define-read-only (get-citizen-level (citizen-id uint))
  (match (map-get? citizen-rewards { citizen-id: citizen-id })
    rewards (get level rewards)
    "bronze"))

(define-read-only (get-leaderboard-position (citizen-id uint))
  (match (map-get? citizen-rewards { citizen-id: citizen-id })
    rewards (get total-points rewards)
    u0))

;; Census Data Analytics & Reporting System

(define-constant REPORT-DEMOGRAPHIC u1)
(define-constant REPORT-REGIONAL u2)
(define-constant REPORT-TEMPORAL u3)
(define-constant REPORT-QUALITY u4)

(define-constant ACCESS-GOVERNMENT u1)
(define-constant ACCESS-RESEARCHER u2)
(define-constant ACCESS-PUBLIC u3)

(define-constant ERR-INVALID-REPORT-TYPE (err u111))
(define-constant ERR-UNAUTHORIZED-ACCESS (err u112))
(define-constant ERR-REPORT-NOT-FOUND (err u113))
(define-constant ERR-INVALID-TIME-RANGE (err u114))

;; Store generated reports with metadata
(define-map census-reports
  { report-id: uint }
  {
    report-type: uint,
    generated-by: principal,
    generated-at: uint,
    region-filter: (optional (string-ascii 32)),
    time-from: uint,
    time-to: uint,
    access-level: uint,
    data-hash: (string-ascii 64),
    title: (string-ascii 128)
  }
)

;; Track analytical insights and metrics
(define-map demographic-insights
  { region: (string-ascii 32), period: uint }
  {
    total-population: uint,
    average-age: uint,
    age-groups: (list 5 uint), ;; [0-18, 19-35, 36-50, 51-65, 65+]
    growth-rate: int,
    verification-rate: uint,
    data-completeness: uint
  }
)

;; Store stakeholder access permissions
(define-map stakeholder-access
  { stakeholder: principal }
  {
    access-level: uint,
    authorized-regions: (list 10 (string-ascii 32)),
    access-granted-by: principal,
    access-granted-at: uint,
    active: bool
  }
)

;; Track data quality metrics
(define-map quality-metrics
  { region: (string-ascii 32), timestamp: uint }
  {
    completeness-score: uint,
    accuracy-score: uint,
    timeliness-score: uint,
    consistency-score: uint,
    overall-quality: uint,
    issues-detected: uint
  }
)

(define-data-var next-report-id uint u1)
(define-data-var analytics-enabled bool true)

;; Generate demographic report for specified region and time period
(define-public (generate-demographic-report 
    (region (optional (string-ascii 32)))
    (time-from uint)
    (time-to uint)
    (access-level uint)
    (title (string-ascii 128)))
  (let
    ((report-id (var-get next-report-id))
     (current-time stacks-block-height))
    (begin
      (asserts! (var-get analytics-enabled) ERR-NOT-AUTHORIZED)
      (asserts! (<= time-from time-to) ERR-INVALID-TIME-RANGE)
      (asserts! (<= access-level u3) ERR-INVALID-DATA)
      
      ;; Generate insights for the specified parameters
      (unwrap! (if (is-some region)
        (calculate-regional-demographics (unwrap-panic region) time-from time-to)
        (calculate-global-demographics time-from time-to)) ERR-INVALID-DATA)
      
      ;; Store report metadata
      (map-set census-reports
        { report-id: report-id }
        {
          report-type: REPORT-DEMOGRAPHIC,
          generated-by: tx-sender,
          generated-at: current-time,
          region-filter: region,
          time-from: time-from,
          time-to: time-to,
          access-level: access-level,
          data-hash: "demographic-analysis", ;; In real implementation, this would be computed
          title: title
        })
      
      (var-set next-report-id (+ report-id u1))
      (ok report-id))))

;; Generate regional comparison report
(define-public (generate-regional-report
    (regions (list 10 (string-ascii 32)))
    (comparison-metrics (list 5 (string-ascii 16)))
    (time-period uint)
    (title (string-ascii 128)))
  (let
    ((report-id (var-get next-report-id))
     (current-time stacks-block-height))
    (begin
      (asserts! (var-get analytics-enabled) ERR-NOT-AUTHORIZED)
      (asserts! (> (len regions) u0) ERR-INVALID-DATA)
      
      ;; Calculate metrics for each region
      (unwrap! (process-regional-comparisons regions time-period) ERR-INVALID-DATA)
      
      (map-set census-reports
        { report-id: report-id }
        {
          report-type: REPORT-REGIONAL,
          generated-by: tx-sender,
          generated-at: current-time,
          region-filter: none,
          time-from: time-period,
          time-to: current-time,
          access-level: ACCESS-GOVERNMENT,
          data-hash: "regional-comparison",
          title: title
        })
      
      (var-set next-report-id (+ report-id u1))
      (ok report-id))))

;; Generate temporal trend analysis
(define-public (generate-temporal-report
    (region (string-ascii 32))
    (start-period uint)
    (end-period uint)
    (interval-blocks uint)
    (title (string-ascii 128)))
  (let
    ((report-id (var-get next-report-id))
     (current-time stacks-block-height))
    (begin
      (asserts! (var-get analytics-enabled) ERR-NOT-AUTHORIZED)
      (asserts! (< start-period end-period) ERR-INVALID-TIME-RANGE)
      (asserts! (> interval-blocks u0) ERR-INVALID-DATA)
      
      ;; Calculate trends over time periods
      (unwrap! (calculate-temporal-trends region start-period end-period interval-blocks) ERR-INVALID-DATA)
      
      (map-set census-reports
        { report-id: report-id }
        {
          report-type: REPORT-TEMPORAL,
          generated-by: tx-sender,
          generated-at: current-time,
          region-filter: (some region),
          time-from: start-period,
          time-to: end-period,
          access-level: ACCESS-RESEARCHER,
          data-hash: "temporal-trends",
          title: title
        })
      
      (var-set next-report-id (+ report-id u1))
      (ok report-id))))

;; Grant access to stakeholders for specific regions and access levels
(define-public (grant-stakeholder-access
    (stakeholder principal)
    (access-level uint)
    (authorized-regions (list 10 (string-ascii 32))))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (asserts! (<= access-level u3) ERR-INVALID-DATA)
    (asserts! (> access-level u0) ERR-INVALID-DATA)
    
    (ok (map-set stakeholder-access
      { stakeholder: stakeholder }
      {
        access-level: access-level,
        authorized-regions: authorized-regions,
        access-granted-by: tx-sender,
        access-granted-at: stacks-block-height,
        active: true
      }))))

;; Revoke stakeholder access
(define-public (revoke-stakeholder-access (stakeholder principal))
  (let
    ((access-info (unwrap! (map-get? stakeholder-access { stakeholder: stakeholder }) ERR-NOT-FOUND)))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (ok (map-set stakeholder-access
        { stakeholder: stakeholder }
        (merge access-info { active: false }))))))

;; Calculate regional demographics for specified time period
(define-private (calculate-regional-demographics 
    (region (string-ascii 32))
    (time-from uint)
    (time-to uint))
  (let
    ((current-region-stats (unwrap! (map-get? region-stats { region: region }) ERR-NOT-FOUND))
     (current-year u2024)
     (period-key (/ (+ time-from time-to) u2))) ;; Use midpoint as period key
    (begin
      ;; Calculate age distribution (simplified)
      (let
        ((age-groups (calculate-age-distribution region current-year))
         (verification-rate (calculate-verification-rate region))
         (growth-rate (calculate-growth-rate region time-from time-to)))
        
        (map-set demographic-insights
          { region: region, period: period-key }
          {
            total-population: (get population current-region-stats),
            average-age: (get avg-age current-region-stats),
            age-groups: age-groups,
            growth-rate: growth-rate,
            verification-rate: verification-rate,
            data-completeness: u95 ;; Placeholder calculation
          })
        (ok true)))))

;; Calculate global demographics across all regions
(define-private (calculate-global-demographics (time-from uint) (time-to uint))
  (let
    ((global-population (var-get total-citizens))
     (period-key (/ (+ time-from time-to) u2)))
    (begin
      ;; Store global insights using "GLOBAL" as region identifier
      (map-set demographic-insights
        { region: "GLOBAL", period: period-key }
        {
          total-population: global-population,
          average-age: u35, ;; Global average placeholder
          age-groups: (list u20 u25 u20 u15 u10), ;; Age distribution percentages
          growth-rate: 3, ;; 3% growth
          verification-rate: u78,
          data-completeness: u92
        })
      (ok true))))

;; Process regional comparisons for multiple regions
(define-private (process-regional-comparisons 
    (regions (list 10 (string-ascii 32)))
    (time-period uint))
  (begin
    ;; In a full implementation, this would iterate through regions
    ;; For now, we'll simulate processing
    (ok true)))

;; Calculate temporal trends for a region over time
(define-private (calculate-temporal-trends
    (region (string-ascii 32))
    (start-period uint)
    (end-period uint)
    (interval-blocks uint))
  (begin
    ;; Generate trend data for the specified time range
    ;; This would involve analyzing historical data at regular intervals
    (ok true)))

;; Helper function to calculate age distribution
(define-private (calculate-age-distribution 
    (region (string-ascii 32))
    (current-year uint))
  (let
    ((placeholder-distribution (list u25 u30 u20 u15 u10))) ;; Age group percentages
    placeholder-distribution))

;; Helper function to calculate verification rate for a region
(define-private (calculate-verification-rate (region (string-ascii 32)))
  (let
    ((placeholder-rate u82)) ;; 82% verification rate
    placeholder-rate))

;; Helper function to calculate population growth rate
(define-private (calculate-growth-rate 
    (region (string-ascii 32))
    (time-from uint)
    (time-to uint))
  (let
    ((placeholder-growth 2)) ;; 2% growth rate
    placeholder-growth))

;; Record data quality metrics for a region
(define-public (record-quality-metrics
    (region (string-ascii 32))
    (completeness uint)
    (accuracy uint)
    (timeliness uint)
    (consistency uint))
  (let
    ((overall-quality (/ (+ completeness (+ accuracy (+ timeliness consistency))) u4))
     (timestamp stacks-block-height))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
      (asserts! (<= completeness u100) ERR-INVALID-DATA)
      (asserts! (<= accuracy u100) ERR-INVALID-DATA)
      (asserts! (<= timeliness u100) ERR-INVALID-DATA)
      (asserts! (<= consistency u100) ERR-INVALID-DATA)
      
      (ok (map-set quality-metrics
        { region: region, timestamp: timestamp }
        {
          completeness-score: completeness,
          accuracy-score: accuracy,
          timeliness-score: timeliness,
          consistency-score: consistency,
          overall-quality: overall-quality,
          issues-detected: (if (< overall-quality u80) u1 u0)
        })))))

;; Toggle analytics system on/off
(define-public (toggle-analytics (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (ok (var-set analytics-enabled enabled))))

;; Read-only functions for accessing analytics data

(define-read-only (get-report-details (report-id uint))
  (map-get? census-reports { report-id: report-id }))

(define-read-only (get-demographic-insights (region (string-ascii 32)) (period uint))
  (map-get? demographic-insights { region: region, period: period }))

(define-read-only (get-stakeholder-access (stakeholder principal))
  (map-get? stakeholder-access { stakeholder: stakeholder }))

(define-read-only (get-quality-metrics (region (string-ascii 32)) (timestamp uint))
  (map-get? quality-metrics { region: region, timestamp: timestamp }))

(define-read-only (check-report-access (stakeholder principal) (report-id uint))
  (let
    ((report (map-get? census-reports { report-id: report-id }))
     (access-info (map-get? stakeholder-access { stakeholder: stakeholder })))
    (match report
      report-data (match access-info
        access (and
          (get active access)
          (>= (get access-level access) (get access-level report-data)))
        false)
      false)))

(define-read-only (get-analytics-status)
  (var-get analytics-enabled))

(define-read-only (get-next-report-id)
  (var-get next-report-id))







