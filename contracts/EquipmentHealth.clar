;; Equipment Health Monitoring System
;; Tracks equipment condition, usage patterns, and predicts maintenance needs
;; Integrates with the main lab resource sharing platform for better resource management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-invalid-condition (err u203))
(define-constant err-maintenance-required (err u204))
(define-constant err-already-reported (err u205))

;; Health scoring thresholds
(define-constant excellent-threshold u90)
(define-constant good-threshold u70)
(define-constant fair-threshold u50)
(define-constant poor-threshold u30)

;; Equipment health tracking
(define-map equipment-health
  uint ;; resource-id
  {
    current-condition-score: uint, ;; 0-100 scale
    total-usage-hours: uint,
    last-maintenance: uint,
    next-maintenance-due: uint,
    health-trend: (string-ascii 20), ;; "improving", "stable", "declining"
    status: (string-ascii 20) ;; "operational", "needs-attention", "maintenance-required"
  }
)

;; Usage session tracking for pattern analysis
(define-map usage-sessions
  {resource-id: uint, session-id: uint}
  {
    user: principal,
    start-time: uint,
    duration: uint,
    condition-before: uint,
    condition-after: uint,
    issues-reported: (string-ascii 200)
  }
)

;; Condition reports from users
(define-map condition-reports
  {resource-id: uint, reporter: principal, timestamp: uint}
  {
    condition-score: uint,
    issues-found: (string-ascii 150),
    severity: (string-ascii 20), ;; "minor", "moderate", "severe", "critical"
    requires-immediate-attention: bool
  }
)

;; Maintenance predictions and alerts
(define-map maintenance-predictions
  uint ;; resource-id
  {
    predicted-failure-date: uint,
    confidence-level: uint, ;; 0-100
    recommended-actions: (string-ascii 200),
    cost-estimate: uint,
    urgency: (string-ascii 20) ;; "low", "medium", "high", "critical"
  }
)

;; Equipment usage patterns for analytics
(define-map usage-patterns
  uint ;; resource-id
  {
    peak-usage-hours: (string-ascii 50),
    average-session-duration: uint,
    heavy-usage-days: uint,
    reliability-score: uint,
    user-satisfaction: uint
  }
)

;; Counter for tracking sessions
(define-data-var next-session-id uint u1)

;; Private helper functions
(define-private (calculate-status-from-score (score uint))
  (if (>= score excellent-threshold)
    "operational"
    (if (>= score good-threshold)
      "operational"
      (if (>= score fair-threshold)
        "needs-attention"
        "maintenance-required"
      )
    )
  )
)

(define-private (calculate-prediction-confidence (condition uint) (usage-hours uint))
  (let (
    (condition-factor (/ condition u2))
    (usage-factor (if (> usage-hours u100) u40 (/ usage-hours u3)))
  )
    (+ condition-factor usage-factor u10)
  )
)

(define-private (calculate-urgency (condition uint))
  (if (< condition poor-threshold)
    "critical"
    (if (< condition fair-threshold)
      "high"
      (if (< condition good-threshold)
        "medium"
        "low"
      )
    )
  )
)

(define-private (get-maintenance-recommendations (condition uint))
  (if (< condition poor-threshold)
    "Immediate inspection and repair required"
    (if (< condition fair-threshold)
      "Schedule maintenance within 1 week"
      "Routine maintenance recommended"
    )
  )
)

(define-private (calculate-maintenance-cost (condition uint) (usage-hours uint))
  (let (
    (base-cost u100)
    (condition-multiplier (/ (- u100 condition) u20))
    (usage-multiplier (/ usage-hours u200))
  )
    (* base-cost (+ u1 condition-multiplier usage-multiplier))
  )
)

;; Initialize equipment health monitoring
(define-public (initialize-equipment-health (resource-id uint) (initial-condition uint))
  (let ((current-block stacks-block-height))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= initial-condition u0) (<= initial-condition u100)) err-invalid-condition)
    
    (map-set equipment-health
      resource-id
      {
        current-condition-score: initial-condition,
        total-usage-hours: u0,
        last-maintenance: current-block,
        next-maintenance-due: (+ current-block u2016), ;; ~2 weeks
        health-trend: "stable",
        status: (calculate-status-from-score initial-condition)
      }
    )
    
    (map-set usage-patterns
      resource-id
      {
        peak-usage-hours: "09:00-17:00",
        average-session-duration: u0,
        heavy-usage-days: u0,
        reliability-score: initial-condition,
        user-satisfaction: u80
      }
    )
    
    (ok true)
  )
)

;; Record usage session with before/after condition
(define-public (record-usage-session 
  (resource-id uint) 
  (duration uint) 
  (condition-before uint) 
  (condition-after uint)
  (issues-reported (string-ascii 200))
)
  (let (
    (session-id (var-get next-session-id))
    (equipment (unwrap! (map-get? equipment-health resource-id) err-not-found))
    (patterns (unwrap! (map-get? usage-patterns resource-id) err-not-found))
  )
    (asserts! (and (>= condition-before u0) (<= condition-before u100)) err-invalid-condition)
    (asserts! (and (>= condition-after u0) (<= condition-after u100)) err-invalid-condition)
    
    ;; Record the session
    (map-set usage-sessions
      {resource-id: resource-id, session-id: session-id}
      {
        user: tx-sender,
        start-time: stacks-block-height,
        duration: duration,
        condition-before: condition-before,
        condition-after: condition-after,
        issues-reported: issues-reported
      }
    )
    
    ;; Update equipment health
    (let (
      (new-total-hours (+ (get total-usage-hours equipment) duration))
      (condition-change (if (> condition-after condition-before) "improving" "declining"))
      (new-trend (if (is-eq condition-before condition-after) "stable" condition-change))
    )
      (map-set equipment-health
        resource-id
        (merge equipment {
          current-condition-score: condition-after,
          total-usage-hours: new-total-hours,
          health-trend: new-trend,
          status: (calculate-status-from-score condition-after)
        })
      )
    )
    
    ;; Update usage patterns
    (let (
      (current-avg-duration (get average-session-duration patterns))
      (session-count (if (is-eq current-avg-duration u0) u1 (+ (/ (get total-usage-hours equipment) current-avg-duration) u1)))
      (new-avg-duration (/ (+ (* current-avg-duration (- session-count u1)) duration) session-count))
    )
      (map-set usage-patterns
        resource-id
        (merge patterns {
          average-session-duration: new-avg-duration,
          reliability-score: condition-after
        })
      )
    )
    
    (var-set next-session-id (+ session-id u1))
    
    ;; Generate maintenance prediction if condition is declining
    (if (< condition-after condition-before)
      (update-maintenance-prediction resource-id)
      (ok true)
    )
  )
)

;; Submit condition report from user
(define-public (submit-condition-report 
  (resource-id uint) 
  (condition-score uint) 
  (issues-found (string-ascii 150))
  (severity (string-ascii 20))
  (requires-immediate-attention bool)
)
  (let (
    (current-time stacks-block-height)
    (report-key {resource-id: resource-id, reporter: tx-sender, timestamp: current-time})
  )
    (asserts! (and (>= condition-score u0) (<= condition-score u100)) err-invalid-condition)
    (asserts! (is-none (map-get? condition-reports report-key)) err-already-reported)
    
    (map-set condition-reports
      report-key
      {
        condition-score: condition-score,
        issues-found: issues-found,
        severity: severity,
        requires-immediate-attention: requires-immediate-attention
      }
    )
    
    ;; Update equipment health if this is a concerning report
    (if (or requires-immediate-attention (< condition-score u50))
      (begin
        (let ((equipment (unwrap! (map-get? equipment-health resource-id) err-not-found)))
          (map-set equipment-health
            resource-id
            (merge equipment {
              current-condition-score: condition-score,
              status: (if requires-immediate-attention "maintenance-required" "needs-attention")
            })
          )
        )
        (update-maintenance-prediction resource-id)
      )
      (ok true)
    )
  )
)

;; Update maintenance prediction based on current data
(define-public (update-maintenance-prediction (resource-id uint))
  (let (
    (equipment (unwrap! (map-get? equipment-health resource-id) err-not-found))
    (patterns (unwrap! (map-get? usage-patterns resource-id) err-not-found))
    (current-condition (get current-condition-score equipment))
    (usage-hours (get total-usage-hours equipment))
    (last-maintenance (get last-maintenance equipment))
  )
    (let (
      (usage-factor (/ usage-hours u100)) ;; Higher usage = more frequent maintenance
      (condition-factor (- u100 current-condition)) ;; Lower condition = sooner maintenance
      (time-factor (/ (- stacks-block-height last-maintenance) u100))
      (prediction-blocks (- u1008 (* (+ usage-factor condition-factor time-factor) u10))) ;; ~1 week baseline
      (confidence (calculate-prediction-confidence current-condition usage-hours))
      (urgency (calculate-urgency current-condition))
    )
      (map-set maintenance-predictions
        resource-id
        {
          predicted-failure-date: (+ stacks-block-height prediction-blocks),
          confidence-level: confidence,
          recommended-actions: (get-maintenance-recommendations current-condition),
          cost-estimate: (calculate-maintenance-cost current-condition usage-hours),
          urgency: urgency
        }
      )
      (ok true)
    )
  )
)

;; Record maintenance completion
(define-public (record-maintenance-completion (resource-id uint) (new-condition-score uint))
  (let ((equipment (unwrap! (map-get? equipment-health resource-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= new-condition-score u80) (<= new-condition-score u100)) err-invalid-condition)
    
    (map-set equipment-health
      resource-id
      (merge equipment {
        current-condition-score: new-condition-score,
        last-maintenance: stacks-block-height,
        next-maintenance-due: (+ stacks-block-height u4032), ;; ~4 weeks
        health-trend: "improving",
        status: "operational"
      })
    )
    
    ;; Clear maintenance prediction after completion
    (map-delete maintenance-predictions resource-id)
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-equipment-health (resource-id uint))
  (map-get? equipment-health resource-id)
)

(define-read-only (get-maintenance-prediction (resource-id uint))
  (map-get? maintenance-predictions resource-id)
)

(define-read-only (get-usage-patterns (resource-id uint))
  (map-get? usage-patterns resource-id)
)

(define-read-only (get-usage-session (resource-id uint) (session-id uint))
  (map-get? usage-sessions {resource-id: resource-id, session-id: session-id})
)

(define-read-only (get-condition-report (resource-id uint) (reporter principal) (timestamp uint))
  (map-get? condition-reports {resource-id: resource-id, reporter: reporter, timestamp: timestamp})
)

(define-read-only (is-equipment-operational (resource-id uint))
  (match (map-get? equipment-health resource-id)
    health (is-eq (get status health) "operational")
    false
  )
)

(define-read-only (get-equipment-health-summary (resource-id uint))
  (match (map-get? equipment-health resource-id)
    health (some {
      condition-score: (get current-condition-score health),
      status: (get status health),
      trend: (get health-trend health),
      maintenance-due: (get next-maintenance-due health)
    })
    none
  )
)
