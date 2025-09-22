;; Community Tech Support Network
;; A decentralized platform for local technical assistance with reputation tracking

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_USER_NOT_FOUND (err u101))
(define-constant ERR_REQUEST_NOT_FOUND (err u102))
(define-constant ERR_INVALID_STATUS (err u103))
(define-constant ERR_CANNOT_RATE_OWN_REQUEST (err u104))
(define-constant ERR_ALREADY_RATED (err u105))
(define-constant ERR_REQUEST_NOT_COMPLETED (err u106))
(define-constant ERR_INSUFFICIENT_FUNDS (err u107))
(define-constant ERR_INVALID_RATING (err u108))

;; Define data variables
(define-data-var next-request-id uint u1)
(define-data-var platform-fee uint u50) ;; 0.5% fee (50/10000)

;; Define data maps
(define-map users principal {
  username: (string-ascii 50),
  reputation-score: uint,
  total-requests-helped: uint,
  total-requests-made: uint,
  location: (string-ascii 100),
  skills: (list 10 (string-ascii 50)),
  is-active: bool
})

(define-map support-requests uint {
  requester: principal,
  helper: (optional principal),
  title: (string-ascii 100),
  description: (string-ascii 500),
  category: (string-ascii 50),
  location: (string-ascii 100),
  reward: uint,
  status: (string-ascii 20), ;; "open", "assigned", "completed", "cancelled"
  created-at: uint,
  completed-at: (optional uint)
})

(define-map request-applications uint (list 20 principal))

(define-map ratings uint {
  requester-rating: (optional uint),
  helper-rating: (optional uint),
  requester-feedback: (optional (string-ascii 200)),
  helper-feedback: (optional (string-ascii 200))
})

;; User registration and management
(define-public (register-user (username (string-ascii 50)) (location (string-ascii 100)) (skills (list 10 (string-ascii 50))))
  (begin
    (map-set users tx-sender {
      username: username,
      reputation-score: u100, ;; Starting reputation
      total-requests-helped: u0,
      total-requests-made: u0,
      location: location,
      skills: skills,
      is-active: true
    })
    (ok true)
  )
)

(define-public (update-user-profile (username (string-ascii 50)) (location (string-ascii 100)) (skills (list 10 (string-ascii 50))))
  (let ((user (unwrap! (map-get? users tx-sender) ERR_USER_NOT_FOUND)))
    (map-set users tx-sender (merge user {
      username: username,
      location: location,
      skills: skills
    }))
    (ok true)
  )
)

(define-public (deactivate-user)
  (let ((user (unwrap! (map-get? users tx-sender) ERR_USER_NOT_FOUND)))
    (map-set users tx-sender (merge user {
      is-active: false
    }))
    (ok true)
  )
)

;; Support request management
(define-public (create-support-request 
  (title (string-ascii 100)) 
  (description (string-ascii 500)) 
  (category (string-ascii 50)) 
  (location (string-ascii 100)) 
  (reward uint))
  (let 
    ((request-id (var-get next-request-id))
     (user (unwrap! (map-get? users tx-sender) ERR_USER_NOT_FOUND)))
    
    ;; Check if user has sufficient funds for reward
    (asserts! (>= (stx-get-balance tx-sender) reward) ERR_INSUFFICIENT_FUNDS)
    
    ;; Transfer reward to contract
    (try! (stx-transfer? reward tx-sender (as-contract tx-sender)))
    
    ;; Create the request
    (map-set support-requests request-id {
      requester: tx-sender,
      helper: none,
      title: title,
      description: description,
      category: category,
      location: location,
      reward: reward,
      status: "open",
      created-at: block-height,
      completed-at: none
    })
    
    ;; Update user stats
    (map-set users tx-sender (merge user {
      total-requests-made: (+ (get total-requests-made user) u1)
    }))
    
    ;; Increment request ID counter
    (var-set next-request-id (+ request-id u1))
    
    (ok request-id)
  )
)

(define-public (apply-to-help (request-id uint))
  (let 
    ((request (unwrap! (map-get? support-requests request-id) ERR_REQUEST_NOT_FOUND))
     (current-applications (default-to (list) (map-get? request-applications request-id)))
     (user (unwrap! (map-get? users tx-sender) ERR_USER_NOT_FOUND)))
    
    ;; Verify request is open and user is not the requester
    (asserts! (is-eq (get status request) "open") ERR_INVALID_STATUS)
    (asserts! (not (is-eq tx-sender (get requester request))) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active user) ERR_NOT_AUTHORIZED)
    
    ;; Add application
    (map-set request-applications request-id 
      (unwrap! (as-max-len? (append current-applications tx-sender) u20) ERR_NOT_AUTHORIZED))
    
    (ok true)
  )
)

(define-public (assign-helper (request-id uint) (helper principal))
  (let 
    ((request (unwrap! (map-get? support-requests request-id) ERR_REQUEST_NOT_FOUND))
     (helper-user (unwrap! (map-get? users helper) ERR_USER_NOT_FOUND)))
    
    ;; Only requester can assign helper
    (asserts! (is-eq tx-sender (get requester request)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status request) "open") ERR_INVALID_STATUS)
    (asserts! (get is-active helper-user) ERR_NOT_AUTHORIZED)
    
    ;; Update request status
    (map-set support-requests request-id (merge request {
      helper: (some helper),
      status: "assigned"
    }))
    
    (ok true)
  )
)

(define-public (mark-request-completed (request-id uint))
  (let ((request (unwrap! (map-get? support-requests request-id) ERR_REQUEST_NOT_FOUND)))
    
    ;; Only assigned helper can mark as completed
    (asserts! (is-eq (some tx-sender) (get helper request)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status request) "assigned") ERR_INVALID_STATUS)
    
    ;; Update request status
    (map-set support-requests request-id (merge request {
      status: "completed",
      completed-at: (some block-height)
    }))
    
    (ok true)
  )
)

(define-public (cancel-request (request-id uint))
  (let 
    ((request (unwrap! (map-get? support-requests request-id) ERR_REQUEST_NOT_FOUND)))
    
    ;; Only requester can cancel, and only if not assigned
    (asserts! (is-eq tx-sender (get requester request)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status request) "open") ERR_INVALID_STATUS)
    
    ;; Update status and refund
    (map-set support-requests request-id (merge request {
      status: "cancelled"
    }))
    
    ;; Refund the reward to requester
    (try! (as-contract (stx-transfer? (get reward request) tx-sender (get requester request))))
    
    (ok true)
  )
)

;; Rating and payment system
(define-public (rate-and-pay (request-id uint) (helper-rating uint) (feedback (string-ascii 200)))
  (let 
    ((request (unwrap! (map-get? support-requests request-id) ERR_REQUEST_NOT_FOUND))
     (helper (unwrap! (get helper request) ERR_REQUEST_NOT_FOUND))
     (current-rating (default-to {
       requester-rating: none,
       helper-rating: none,
       requester-feedback: none,
       helper-feedback: none
     } (map-get? ratings request-id)))
     (helper-user (unwrap! (map-get? users helper) ERR_USER_NOT_FOUND)))
    
    ;; Verify conditions
    (asserts! (is-eq tx-sender (get requester request)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status request) "completed") ERR_REQUEST_NOT_COMPLETED)
    (asserts! (and (>= helper-rating u1) (<= helper-rating u5)) ERR_INVALID_RATING)
    (asserts! (is-none (get requester-rating current-rating)) ERR_ALREADY_RATED)
    
    ;; Calculate payment amounts
    (let ((reward (get reward request))
          (fee (/ (* reward (var-get platform-fee)) u10000))
          (helper-payment (- reward fee)))
      
      ;; Transfer payment to helper
      (try! (as-contract (stx-transfer? helper-payment tx-sender helper)))
      
      ;; Update helper's reputation and stats
      (let ((new-reputation (+ (get reputation-score helper-user) (* helper-rating u10))))
        (map-set users helper (merge helper-user {
          reputation-score: new-reputation,
          total-requests-helped: (+ (get total-requests-helped helper-user) u1)
        }))
      )
      
      ;; Store rating
      (map-set ratings request-id (merge current-rating {
        requester-rating: (some helper-rating),
        requester-feedback: (some feedback)
      }))
      
      (ok true)
    )
  )
)

(define-public (rate-requester (request-id uint) (requester-rating uint) (feedback (string-ascii 200)))
  (let 
    ((request (unwrap! (map-get? support-requests request-id) ERR_REQUEST_NOT_FOUND))
     (current-rating (default-to {
       requester-rating: none,
       helper-rating: none,
       requester-feedback: none,
       helper-feedback: none
     } (map-get? ratings request-id))))
    
    ;; Verify conditions
    (asserts! (is-eq (some tx-sender) (get helper request)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status request) "completed") ERR_REQUEST_NOT_COMPLETED)
    (asserts! (and (>= requester-rating u1) (<= requester-rating u5)) ERR_INVALID_RATING)
    (asserts! (is-none (get helper-rating current-rating)) ERR_ALREADY_RATED)
    
    ;; Store rating
    (map-set ratings request-id (merge current-rating {
      helper-rating: (some requester-rating),
      helper-feedback: (some feedback)
    }))
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-user (user principal))
  (map-get? users user)
)

(define-read-only (get-support-request (request-id uint))
  (map-get? support-requests request-id)
)

(define-read-only (get-request-applications (request-id uint))
  (map-get? request-applications request-id)
)

(define-read-only (get-rating (request-id uint))
  (map-get? ratings request-id)
)

(define-read-only (get-next-request-id)
  (var-get next-request-id)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

;; Admin functions
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u1000) ERR_INVALID_RATING) ;; Max 10% fee
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-public (withdraw-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (ok true)
  )
)