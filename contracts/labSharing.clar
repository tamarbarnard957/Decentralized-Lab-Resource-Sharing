(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-invalid-time (err u105))

(define-constant err-cancellation-too-late (err u109))
(define-constant err-already-cancelled (err u110))
(define-constant cancellation-window u24)



(define-constant bid-expiry-blocks u144)
(define-constant offer-expiry-blocks u144)
(define-constant err-bid-expired (err u111))
(define-constant err-offer-expired (err u112))
(define-constant err-bid-exists (err u113))
(define-constant err-offer-exists (err u114))
(define-constant err-invalid-bid (err u115))
(define-constant err-invalid-offer (err u116))
(define-constant err-cannot-bid-own-resource (err u117))
(define-constant err-offer-not-found (err u118))
(define-constant err-bid-not-found (err u119))
(define-constant err-offer-too-low (err u120))

(define-map resource-bids
  {resource-id: uint, time-slot: uint, bidder: principal}
  {
    bid-amount: uint,
    duration: uint,
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 20)
  }
)

(define-map resource-offers
  {resource-id: uint, time-slot: uint, offerer: principal}
  {
    offer-amount: uint,
    duration: uint,
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 20)
  }
)

(define-map highest-bid
  {resource-id: uint, time-slot: uint}
  {
    bidder: principal,
    amount: uint,
    duration: uint
  }
)

(define-map lowest-offer
  {resource-id: uint, time-slot: uint}
  {
    offerer: principal,
    amount: uint,
    duration: uint
  }
)

(define-map user-bid-history
  {user: principal, resource-id: uint}
  {
    total-bids: uint,
    successful-bids: uint,
    average-bid: uint
  }
)

(define-map user-offer-history
  {user: principal, resource-id: uint}
  {
    total-offers: uint,
    successful-offers: uint,
    average-offer: uint
  }
)

(define-map cancelled-bookings
  {resource-id: uint, time-slot: uint}
  {
    cancelled-by: principal,
    cancellation-time: uint,
    refund-amount: uint,
    reason: (string-ascii 100)
  }
)

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


(define-public (cancel-maintenance (resource-id uint) (start-time uint))
  (let ((maintenance (unwrap! (map-get? maintenance-schedule {resource-id: resource-id, start-time: start-time}) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete maintenance-schedule {resource-id: resource-id, start-time: start-time})
    (let ((resource (unwrap! (map-get? lab-resources resource-id) err-not-found)))
      (map-set lab-resources
        resource-id
        (merge resource {available: true})
      )
    )
    (ok true)
  )
)



(define-public (cancel-booking-user (resource-id uint) (time-slot uint))
  (let (
    (booking (unwrap! (map-get? resource-bookings {resource-id: resource-id, time-slot: time-slot}) err-not-found))
    (resource (unwrap! (map-get? lab-resources resource-id) err-not-found))
    (current-balance (default-to u0 (map-get? user-balances tx-sender)))
  )
    (asserts! (is-eq (get user booking) tx-sender) err-unauthorized)
    (asserts! (is-none (map-get? cancelled-bookings {resource-id: resource-id, time-slot: time-slot})) err-already-cancelled)
    (asserts! (>= (- time-slot stacks-block-height) cancellation-window) err-cancellation-too-late)
    
    (map-set cancelled-bookings
      {resource-id: resource-id, time-slot: time-slot}
      {
        cancelled-by: tx-sender,
        cancellation-time: stacks-block-height,
        refund-amount: (get paid-amount booking),
        reason: "User cancellation"
      }
    )
    
    (map-set user-balances
      tx-sender
      (+ current-balance (get paid-amount booking))
    )
    
    (map-delete resource-bookings {resource-id: resource-id, time-slot: time-slot})
    
    (map-set lab-resources
      resource-id
      (merge resource {available: true})
    )
    
    (ok (get paid-amount booking))
  )
)

(define-public (cancel-booking-owner (resource-id uint) (time-slot uint) (reason (string-ascii 100)))
  (let (
    (booking (unwrap! (map-get? resource-bookings {resource-id: resource-id, time-slot: time-slot}) err-not-found))
    (resource (unwrap! (map-get? lab-resources resource-id) err-not-found))
    (user-balance (default-to u0 (map-get? user-balances (get user booking))))
  )
    (asserts! (is-eq (get owner resource) tx-sender) err-owner-only)
    (asserts! (is-none (map-get? cancelled-bookings {resource-id: resource-id, time-slot: time-slot})) err-already-cancelled)
    
    (map-set cancelled-bookings
      {resource-id: resource-id, time-slot: time-slot}
      {
        cancelled-by: tx-sender,
        cancellation-time: stacks-block-height,
        refund-amount: (get paid-amount booking),
        reason: reason
      }
    )
    
    (map-set user-balances
      (get user booking)
      (+ user-balance (get paid-amount booking))
    )
    
    (map-delete resource-bookings {resource-id: resource-id, time-slot: time-slot})
    
    (map-set lab-resources
      resource-id
      (merge resource {available: true})
    )
    
    (ok (get paid-amount booking))
  )
)

(define-read-only (get-cancellation-info (resource-id uint) (time-slot uint))
  (ok (map-get? cancelled-bookings {resource-id: resource-id, time-slot: time-slot}))
)

(define-read-only (can-cancel-booking (resource-id uint) (time-slot uint) (user principal))
  (let (
    (booking (map-get? resource-bookings {resource-id: resource-id, time-slot: time-slot}))
    (is-cancelled (is-some (map-get? cancelled-bookings {resource-id: resource-id, time-slot: time-slot})))
  )
    (ok 
      (and 
        (is-some booking)
        (not is-cancelled)
        (is-eq (get user (unwrap-panic booking)) user)
        (>= (- time-slot stacks-block-height) cancellation-window)
      )
    )
  )
)

(define-read-only (get-cancellation-deadline (time-slot uint))
  (ok (- time-slot cancellation-window))
)


(define-public (place-bid (resource-id uint) (time-slot uint) (bid-amount uint) (duration uint))
  (let (
    (resource (unwrap! (map-get? lab-resources resource-id) err-not-found))
    (user-balance (default-to u0 (map-get? user-balances tx-sender)))
    (existing-bid (map-get? resource-bids {resource-id: resource-id, time-slot: time-slot, bidder: tx-sender}))
    (current-highest (map-get? highest-bid {resource-id: resource-id, time-slot: time-slot}))
    (expires-at (+ stacks-block-height bid-expiry-blocks))
    (bid-history (default-to {total-bids: u0, successful-bids: u0, average-bid: u0} 
      (map-get? user-bid-history {user: tx-sender, resource-id: resource-id})))
  )
    (asserts! (not (is-eq (get owner resource) tx-sender)) err-cannot-bid-own-resource)
    (asserts! (> bid-amount u0) err-invalid-bid)
    (asserts! (> duration u0) err-invalid-time)
    (asserts! (>= user-balance bid-amount) err-insufficient-balance)
    (asserts! (is-none existing-bid) err-bid-exists)
    (asserts! (is-none (map-get? resource-bookings {resource-id: resource-id, time-slot: time-slot})) err-already-exists)
    
    (map-set resource-bids
      {resource-id: resource-id, time-slot: time-slot, bidder: tx-sender}
      {
        bid-amount: bid-amount,
        duration: duration,
        created-at: stacks-block-height,
        expires-at: expires-at,
        status: "active"
      }
    )
    
    (map-set user-balances
      tx-sender
      (- user-balance bid-amount)
    )
    
    (if (or (is-none current-highest) (> bid-amount (get amount (unwrap-panic current-highest))))
      (map-set highest-bid
        {resource-id: resource-id, time-slot: time-slot}
        {
          bidder: tx-sender,
          amount: bid-amount,
          duration: duration
        }
      )
      true
    )
    
    (map-set user-bid-history
      {user: tx-sender, resource-id: resource-id}
      {
        total-bids: (+ (get total-bids bid-history) u1),
        successful-bids: (get successful-bids bid-history),
        average-bid: (/ (+ (* (get total-bids bid-history) (get average-bid bid-history)) bid-amount)
                       (+ (get total-bids bid-history) u1))
      }
    )
    
    (ok bid-amount)
  )
)

(define-public (place-offer (resource-id uint) (time-slot uint) (offer-amount uint) (duration uint))
  (let (
    (resource (unwrap! (map-get? lab-resources resource-id) err-not-found))
    (user-balance (default-to u0 (map-get? user-balances tx-sender)))
    (existing-offer (map-get? resource-offers {resource-id: resource-id, time-slot: time-slot, offerer: tx-sender}))
    (current-lowest (map-get? lowest-offer {resource-id: resource-id, time-slot: time-slot}))
    (expires-at (+ stacks-block-height offer-expiry-blocks))
    (offer-history (default-to {total-offers: u0, successful-offers: u0, average-offer: u0} 
      (map-get? user-offer-history {user: tx-sender, resource-id: resource-id})))
  )
    (asserts! (not (is-eq (get owner resource) tx-sender)) err-cannot-bid-own-resource)
    (asserts! (> offer-amount u0) err-invalid-offer)
    (asserts! (> duration u0) err-invalid-time)
    (asserts! (>= user-balance offer-amount) err-insufficient-balance)
    (asserts! (is-none existing-offer) err-offer-exists)
    (asserts! (is-none (map-get? resource-bookings {resource-id: resource-id, time-slot: time-slot})) err-already-exists)
    
    (map-set resource-offers
      {resource-id: resource-id, time-slot: time-slot, offerer: tx-sender}
      {
        offer-amount: offer-amount,
        duration: duration,
        created-at: stacks-block-height,
        expires-at: expires-at,
        status: "active"
      }
    )
    
    (map-set user-balances
      tx-sender
      (- user-balance offer-amount)
    )
    
    (if (or (is-none current-lowest) (< offer-amount (get amount (unwrap-panic current-lowest))))
      (map-set lowest-offer
        {resource-id: resource-id, time-slot: time-slot}
        {
          offerer: tx-sender,
          amount: offer-amount,
          duration: duration
        }
      )
      true
    )
    
    (map-set user-offer-history
      {user: tx-sender, resource-id: resource-id}
      {
        total-offers: (+ (get total-offers offer-history) u1),
        successful-offers: (get successful-offers offer-history),
        average-offer: (/ (+ (* (get total-offers offer-history) (get average-offer offer-history)) offer-amount)
                         (+ (get total-offers offer-history) u1))
      }
    )
    
    (ok offer-amount)
  )
)

(define-public (accept-bid (resource-id uint) (time-slot uint) (bidder principal))
  (let (
    (resource (unwrap! (map-get? lab-resources resource-id) err-not-found))
    (bid (unwrap! (map-get? resource-bids {resource-id: resource-id, time-slot: time-slot, bidder: bidder}) err-bid-not-found))
    (bid-history (default-to {total-bids: u0, successful-bids: u0, average-bid: u0} 
      (map-get? user-bid-history {user: bidder, resource-id: resource-id})))
    (resource-owner-balance (default-to u0 (map-get? user-balances (get owner resource))))
  )
    (asserts! (is-eq (get owner resource) tx-sender) err-owner-only)
    (asserts! (is-eq (get status bid) "active") err-unauthorized)
    (asserts! (>= (get expires-at bid) stacks-block-height) err-bid-expired)
    
    (map-set resource-bookings
      {resource-id: resource-id, time-slot: time-slot}
      {
        user: bidder,
        duration: (get duration bid),
        paid-amount: (get bid-amount bid)
      }
    )
    
    (map-set user-balances
      (get owner resource)
      (+ resource-owner-balance (get bid-amount bid))
    )
    
    (map-set resource-bids
      {resource-id: resource-id, time-slot: time-slot, bidder: bidder}
      (merge bid {status: "accepted"})
    )
    
    (map-set user-bid-history
      {user: bidder, resource-id: resource-id}
      {
        total-bids: (get total-bids bid-history),
        successful-bids: (+ (get successful-bids bid-history) u1),
        average-bid: (get average-bid bid-history)
      }
    )
    
    (map-set lab-resources
      resource-id
      (merge resource {available: false})
    )
    
    (ok (get bid-amount bid))
  )
)

(define-public (accept-offer (resource-id uint) (time-slot uint) (offerer principal))
  (let (
    (resource (unwrap! (map-get? lab-resources resource-id) err-not-found))
    (offer (unwrap! (map-get? resource-offers {resource-id: resource-id, time-slot: time-slot, offerer: offerer}) err-offer-not-found))
    (offer-history (default-to {total-offers: u0, successful-offers: u0, average-offer: u0} 
      (map-get? user-offer-history {user: offerer, resource-id: resource-id})))
    (resource-owner-balance (default-to u0 (map-get? user-balances (get owner resource))))
  )
    (asserts! (is-eq (get owner resource) tx-sender) err-owner-only)
    (asserts! (is-eq (get status offer) "active") err-unauthorized)
    (asserts! (>= (get expires-at offer) stacks-block-height) err-offer-expired)
    
    (map-set resource-bookings
      {resource-id: resource-id, time-slot: time-slot}
      {
        user: offerer,
        duration: (get duration offer),
        paid-amount: (get offer-amount offer)
      }
    )
    
    (map-set user-balances
      (get owner resource)
      (+ resource-owner-balance (get offer-amount offer))
    )
    
    (map-set resource-offers
      {resource-id: resource-id, time-slot: time-slot, offerer: offerer}
      (merge offer {status: "accepted"})
    )
    
    (map-set user-offer-history
      {user: offerer, resource-id: resource-id}
      {
        total-offers: (get total-offers offer-history),
        successful-offers: (+ (get successful-offers offer-history) u1),
        average-offer: (get average-offer offer-history)
      }
    )
    
    (map-set lab-resources
      resource-id
      (merge resource {available: false})
    )
    
    (ok (get offer-amount offer))
  )
)

(define-public (cancel-bid (resource-id uint) (time-slot uint))
  (let (
    (bid (unwrap! (map-get? resource-bids {resource-id: resource-id, time-slot: time-slot, bidder: tx-sender}) err-bid-not-found))
    (user-balance (default-to u0 (map-get? user-balances tx-sender)))
  )
    (asserts! (is-eq (get status bid) "active") err-unauthorized)
    
    (map-set resource-bids
      {resource-id: resource-id, time-slot: time-slot, bidder: tx-sender}
      (merge bid {status: "cancelled"})
    )
    
    (map-set user-balances
      tx-sender
      (+ user-balance (get bid-amount bid))
    )
    
    (ok (get bid-amount bid))
  )
)

(define-public (cancel-offer (resource-id uint) (time-slot uint))
  (let (
    (offer (unwrap! (map-get? resource-offers {resource-id: resource-id, time-slot: time-slot, offerer: tx-sender}) err-offer-not-found))
    (user-balance (default-to u0 (map-get? user-balances tx-sender)))
  )
    (asserts! (is-eq (get status offer) "active") err-unauthorized)
    
    (map-set resource-offers
      {resource-id: resource-id, time-slot: time-slot, offerer: tx-sender}
      (merge offer {status: "cancelled"})
    )
    
    (map-set user-balances
      tx-sender
      (+ user-balance (get offer-amount offer))
    )
    
    (ok (get offer-amount offer))
  )
)

(define-read-only (get-resource-bid (resource-id uint) (time-slot uint) (bidder principal))
  (ok (map-get? resource-bids {resource-id: resource-id, time-slot: time-slot, bidder: bidder}))
)

(define-read-only (get-resource-offer (resource-id uint) (time-slot uint) (offerer principal))
  (ok (map-get? resource-offers {resource-id: resource-id, time-slot: time-slot, offerer: offerer}))
)

(define-read-only (get-highest-bid (resource-id uint) (time-slot uint))
  (ok (map-get? highest-bid {resource-id: resource-id, time-slot: time-slot}))
)

(define-read-only (get-lowest-offer (resource-id uint) (time-slot uint))
  (ok (map-get? lowest-offer {resource-id: resource-id, time-slot: time-slot}))
)

(define-read-only (get-user-bid-history (user principal) (resource-id uint))
  (ok (map-get? user-bid-history {user: user, resource-id: resource-id}))
)

(define-read-only (get-user-offer-history (user principal) (resource-id uint))
  (ok (map-get? user-offer-history {user: user, resource-id: resource-id}))
)

(define-read-only (get-marketplace-stats (resource-id uint) (time-slot uint))
  (let (
    (highest (map-get? highest-bid {resource-id: resource-id, time-slot: time-slot}))
    (lowest (map-get? lowest-offer {resource-id: resource-id, time-slot: time-slot}))
  )
    (ok {
      highest-bid: highest,
      lowest-offer: lowest,
      spread: (if (and (is-some highest) (is-some lowest))
        (some (- (get amount (unwrap-panic lowest)) (get amount (unwrap-panic highest))))
        none
      )
    })
  )
)