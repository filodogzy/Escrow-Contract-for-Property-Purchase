(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-already-initialized (err u102))
(define-constant err-not-found (err u103))
(define-constant err-wrong-status (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-insufficient-approvals (err u106))
(define-constant err-already-approved (err u107))
(define-constant err-not-approver (err u108))

(define-data-var escrow-fee uint u2)
(define-data-var minimum-deposit uint u1000)
(define-data-var approval-threshold uint u2)

(define-map Properties
    { property-id: uint }
    {
        seller: principal,
        buyer: principal,
        price: uint,
        deposit: uint,
        status: (string-ascii 20),
        inspection-passed: bool,
        title-cleared: bool,
        mortgage-approved: bool,
        deadline: uint,
    }
)

(define-map Balances
    { user: principal }
    { amount: uint }
)

(define-map PropertyApprovers
    { property-id: uint }
    { approvers: (list 10 principal) }
)

(define-map ApprovalStatus
    {
        property-id: uint,
        approver: principal,
        milestone: (string-ascii 20),
    }
    { approved: bool }
)

(define-data-var transaction-counter uint u0)

(define-map PropertyHistory
    {
        property-id: uint,
        transaction-id: uint,
    }
    {
        from-owner: (optional principal),
        to-owner: principal,
        price: uint,
        transaction-type: (string-ascii 20),
        block-height: uint,
        timestamp: uint,
    }
)

(define-map PropertyTransactionCount
    { property-id: uint }
    { count: uint }
)

(define-public (create-escrow
        (property-id uint)
        (buyer principal)
        (price uint)
        (deposit uint)
        (deadline uint)
        (approvers (list 10 principal))
    )
    (let ((current-height burn-block-height))
        (asserts! (>= deposit (var-get minimum-deposit)) err-insufficient-funds)
        (asserts! (> deadline current-height) err-wrong-status)
        (try! (stx-transfer? deposit tx-sender (as-contract tx-sender)))
        (map-set Properties { property-id: property-id } {
            seller: tx-sender,
            buyer: buyer,
            price: price,
            deposit: deposit,
            status: "pending",
            inspection-passed: false,
            title-cleared: false,
            mortgage-approved: false,
            deadline: deadline,
        })
        (map-set PropertyApprovers { property-id: property-id } { approvers: approvers })
        (unwrap-panic (record-property-transaction property-id none tx-sender deposit
            "escrow-created"
        ))
        (ok true)
    )
)

(define-public (update-inspection-status
        (property-id uint)
        (status bool)
    )
    (let ((property (unwrap! (map-get? Properties { property-id: property-id }) err-not-found)))
        (asserts!
            (or
                (is-eq tx-sender (get buyer property))
                (is-eq tx-sender contract-owner)
                (has-sufficient-approvals property-id "inspection")
            )
            err-not-authorized
        )
        (map-set Properties { property-id: property-id }
            (merge property { inspection-passed: status })
        )
        (ok true)
    )
)

(define-public (update-title-status
        (property-id uint)
        (status bool)
    )
    (let ((property (unwrap! (map-get? Properties { property-id: property-id }) err-not-found)))
        (asserts!
            (or
                (is-eq tx-sender (get seller property))
                (is-eq tx-sender contract-owner)
                (has-sufficient-approvals property-id "title")
            )
            err-not-authorized
        )
        (map-set Properties { property-id: property-id }
            (merge property { title-cleared: status })
        )
        (ok true)
    )
)

(define-public (update-mortgage-status
        (property-id uint)
        (status bool)
    )
    (let ((property (unwrap! (map-get? Properties { property-id: property-id }) err-not-found)))
        (asserts!
            (or
                (is-eq tx-sender (get buyer property))
                (is-eq tx-sender contract-owner)
                (has-sufficient-approvals property-id "mortgage")
            )
            err-not-authorized
        )
        (map-set Properties { property-id: property-id }
            (merge property { mortgage-approved: status })
        )
        (ok true)
    )
)
(define-public (complete-purchase (property-id uint))
    (let (
            (property (unwrap! (map-get? Properties { property-id: property-id })
                err-not-found
            ))
            (current-height stacks-block-height)
            (fee-amount (/ (* (get price property) (var-get escrow-fee)) u100))
        )
        (asserts! (is-eq (get buyer property) tx-sender) err-not-authorized)
        (asserts! (get inspection-passed property) err-wrong-status)
        (asserts! (get title-cleared property) err-wrong-status)
        (asserts! (get mortgage-approved property) err-wrong-status)
        (asserts! (<= current-height (get deadline property)) err-wrong-status)
        (try! (stx-transfer? (- (get price property) fee-amount) tx-sender
            (get seller property)
        ))
        (try! (stx-transfer? fee-amount tx-sender contract-owner))
        (map-set Properties { property-id: property-id }
            (merge property { status: "completed" })
        )
        (unwrap-panic (record-property-transaction property-id (some (get seller property))
            (get buyer property) (get price property) "purchase-completed"
        ))
        (ok true)
    )
)

(define-public (cancel-escrow (property-id uint))
    (let (
            (property (unwrap! (map-get? Properties { property-id: property-id })
                err-not-found
            ))
            (current-height stacks-block-height)
        )
        (asserts!
            (or
                (is-eq tx-sender (get seller property))
                (is-eq tx-sender (get buyer property))
            )
            err-not-authorized
        )
        (asserts! (> current-height (get deadline property)) err-wrong-status)
        (try! (stx-transfer? (get deposit property) (as-contract tx-sender)
            (get buyer property)
        ))
        (map-set Properties { property-id: property-id }
            (merge property { status: "cancelled" })
        )
        (unwrap-panic (record-property-transaction property-id (some (get seller property))
            (get buyer property) (get deposit property) "escrow-cancelled"
        ))
        (ok true)
    )
)

(define-public (update-escrow-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set escrow-fee new-fee)
        (ok true)
    )
)

(define-public (update-minimum-deposit (new-minimum uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set minimum-deposit new-minimum)
        (ok true)
    )
)

(define-read-only (get-property-details (property-id uint))
    (ok (unwrap! (map-get? Properties { property-id: property-id }) err-not-found))
)

(define-read-only (get-escrow-fee)
    (ok (var-get escrow-fee))
)

(define-read-only (get-minimum-deposit)
    (ok (var-get minimum-deposit))
)

(define-public (submit-approval
        (property-id uint)
        (milestone (string-ascii 20))
    )
    (let ((approvers-data (unwrap! (map-get? PropertyApprovers { property-id: property-id })
            err-not-found
        )))
        (asserts! (is-some (index-of (get approvers approvers-data) tx-sender))
            err-not-approver
        )
        (asserts!
            (is-none (map-get? ApprovalStatus {
                property-id: property-id,
                approver: tx-sender,
                milestone: milestone,
            }))
            err-already-approved
        )
        (map-set ApprovalStatus {
            property-id: property-id,
            approver: tx-sender,
            milestone: milestone,
        } { approved: true }
        )
        (ok true)
    )
)

(define-private (count-approvals
        (property-id uint)
        (milestone (string-ascii 20))
    )
    (let ((approvers-data (default-to { approvers: (list) }
            (map-get? PropertyApprovers { property-id: property-id })
        )))
        (fold count-approval-fold (get approvers approvers-data) {
            property-id: property-id,
            milestone: milestone,
            count: u0,
        })
    )
)

(define-private (count-approval-fold
        (approver principal)
        (acc {
            property-id: uint,
            milestone: (string-ascii 20),
            count: uint,
        })
    )
    (let ((approval-status (map-get? ApprovalStatus {
            property-id: (get property-id acc),
            approver: approver,
            milestone: (get milestone acc),
        })))
        (if (and (is-some approval-status) (get approved (unwrap-panic approval-status)))
            (merge acc { count: (+ (get count acc) u1) })
            acc
        )
    )
)

(define-private (has-sufficient-approvals
        (property-id uint)
        (milestone (string-ascii 20))
    )
    (let ((approval-count (get count (count-approvals property-id milestone))))
        (>= approval-count (var-get approval-threshold))
    )
)

(define-public (update-approval-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set approval-threshold new-threshold)
        (ok true)
    )
)

(define-read-only (get-approval-threshold)
    (ok (var-get approval-threshold))
)

(define-read-only (get-property-approvers (property-id uint))
    (ok (unwrap! (map-get? PropertyApprovers { property-id: property-id })
        err-not-found
    ))
)

(define-read-only (get-approval-status
        (property-id uint)
        (approver principal)
        (milestone (string-ascii 20))
    )
    (ok (map-get? ApprovalStatus {
        property-id: property-id,
        approver: approver,
        milestone: milestone,
    }))
)

(define-private (record-property-transaction
        (property-id uint)
        (from-owner (optional principal))
        (to-owner principal)
        (price uint)
        (transaction-type (string-ascii 20))
    )
    (let (
            (current-count (default-to u0
                (get count
                    (map-get? PropertyTransactionCount { property-id: property-id })
                )))
            (new-transaction-id (+ (var-get transaction-counter) u1))
        )
        (map-set PropertyHistory {
            property-id: property-id,
            transaction-id: new-transaction-id,
        } {
            from-owner: from-owner,
            to-owner: to-owner,
            price: price,
            transaction-type: transaction-type,
            block-height: burn-block-height,
            timestamp: burn-block-height,
        })
        (map-set PropertyTransactionCount { property-id: property-id } { count: (+ current-count u1) })
        (var-set transaction-counter new-transaction-id)
        (ok true)
    )
)

(define-read-only (get-property-history (property-id uint))
    (let ((transaction-count (default-to u0
            (get count
                (map-get? PropertyTransactionCount { property-id: property-id })
            ))))
        (ok (map get-transaction-by-id (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))
    )
)

(define-private (get-transaction-by-id (transaction-id uint))
    (map-get? PropertyHistory {
        property-id: u0,
        transaction-id: transaction-id,
    })
)

(define-read-only (get-property-transaction
        (property-id uint)
        (transaction-id uint)
    )
    (ok (map-get? PropertyHistory {
        property-id: property-id,
        transaction-id: transaction-id,
    }))
)

(define-read-only (get-property-transaction-count (property-id uint))
    (ok (default-to u0
        (get count
            (map-get? PropertyTransactionCount { property-id: property-id })
        )))
)
