(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-invalid-time (err u105))

(define-non-fungible-token lab-resource uint)

(define-map lab-resources
  uint 
  {
    name: (string-ascii 50),
    hourly-rate: uint,
    available: bool,
    owner: principal
  }
)

(define-map resource-bookings
  {resource-id: uint, time-slot: uint}
  {
    user: principal,
    duration: uint,
    paid-amount: uint
  }
)

(define-map user-balances principal uint)

(define-data-var next-resource-id uint u1)

(define-public (add-lab-resource (name (string-ascii 50)) (hourly-rate uint))
  (let ((resource-id (var-get next-resource-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (nft-mint? lab-resource resource-id tx-sender))
    (map-set lab-resources
      resource-id
      {
        name: name,
        hourly-rate: hourly-rate,
        available: true,
        owner: tx-sender
      }
    )
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

(define-public (deposit-tokens (amount uint))
  (let ((current-balance (default-to u0 (map-get? user-balances tx-sender))))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-balances
      tx-sender
      (+ current-balance amount)
    )
    (ok amount)
  )
)

(define-public (book-resource (resource-id uint) (time-slot uint) (duration uint))
  (let (
    (resource (unwrap! (map-get? lab-resources resource-id) err-not-found))
    (total-cost (* (get hourly-rate resource) duration))
    (user-balance (default-to u0 (map-get? user-balances tx-sender)))
  )
    (asserts! (get available resource) err-unauthorized)
    (asserts! (>= user-balance total-cost) err-insufficient-balance)
    (asserts! (> duration u0) err-invalid-time)
    
    (map-set resource-bookings
      {resource-id: resource-id, time-slot: time-slot}
      {
        user: tx-sender,
        duration: duration,
        paid-amount: total-cost
      }
    )
    
    (map-set user-balances
      tx-sender
      (- user-balance total-cost)
    )
    
    (map-set lab-resources
      resource-id
      (merge resource {available: false})
    )
    
    (ok true)
  )
)

(define-public (complete-session (resource-id uint) (time-slot uint))
  (let (
    (resource (unwrap! (map-get? lab-resources resource-id) err-not-found))
    (booking (unwrap! (map-get? resource-bookings {resource-id: resource-id, time-slot: time-slot}) err-not-found))
  )
    (asserts! (is-eq (get owner resource) tx-sender) err-owner-only)
    
    (try! (as-contract (stx-transfer? (get paid-amount booking) tx-sender (get owner resource))))
    
    (map-delete resource-bookings {resource-id: resource-id, time-slot: time-slot})
    
    (map-set lab-resources
      resource-id
      (merge resource {available: true})
    )
    
    (ok true)
  )
)

(define-read-only (get-resource (resource-id uint))
  (ok (map-get? lab-resources resource-id))
)

(define-read-only (get-booking (resource-id uint) (time-slot uint))
  (ok (map-get? resource-bookings {resource-id: resource-id, time-slot: time-slot}))
)

(define-read-only (get-user-balance (user principal))
  (ok (default-to u0 (map-get? user-balances user)))
)

(define-map resource-ratings
  {resource-id: uint, user: principal}
  {rating: uint, timestamp: uint}
)

(define-map resource-average-rating
  uint
  {total-ratings: uint, average-score: uint}
)

(define-constant err-invalid-rating (err u106))
(define-constant err-not-booked (err u107))

(define-public (rate-resource (resource-id uint) (rating uint))
  (let (
    (resource (unwrap! (map-get? lab-resources resource-id) err-not-found))
    (current-average (default-to {total-ratings: u0, average-score: u0} 
      (map-get? resource-average-rating resource-id)))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    (map-set resource-ratings
      {resource-id: resource-id, user: tx-sender}
      {rating: rating, timestamp: stacks-block-height}
    )
    (map-set resource-average-rating
      resource-id
      {
        total-ratings: (+ (get total-ratings current-average) u1),
        average-score: (/ (+ (* (get total-ratings current-average) 
                               (get average-score current-average)) 
                            rating)
                         (+ (get total-ratings current-average) u1))
      }
    )
    (ok true)
  )
)

(define-read-only (get-resource-rating (resource-id uint))
  (ok (map-get? resource-average-rating resource-id))
)


(define-map maintenance-schedule
  {resource-id: uint, start-time: uint}
  {duration: uint, reason: (string-ascii 100)}
)

(define-constant err-maintenance-conflict (err u108))

(define-public (schedule-maintenance 
    (resource-id uint) 
    (start-time uint) 
    (duration uint) 
    (reason (string-ascii 100))
  )
  (let ((resource (unwrap! (map-get? lab-resources resource-id) err-not-found)))
    (asserts! (is-eq (get owner resource) tx-sender) err-owner-only)
    (asserts! (> duration u0) err-invalid-time)
    (map-set maintenance-schedule
      {resource-id: resource-id, start-time: start-time}
      {duration: duration, reason: reason}
    )
    (map-set lab-resources
      resource-id
      (merge resource {available: false})
    )
    (ok true)
  )
)

(define-read-only (get-maintenance-schedule (resource-id uint) (start-time uint))
  (ok (map-get? maintenance-schedule {resource-id: resource-id, start-time: start-time}))
)