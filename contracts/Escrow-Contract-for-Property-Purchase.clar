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
(define-constant err-dispute-not-found (err u109))
(define-constant err-dispute-already-exists (err u110))
(define-constant err-not-arbitrator (err u111))
(define-constant err-already-voted (err u112))
(define-constant err-dispute-not-active (err u113))
(define-constant err-milestone-not-completed (err u114))
(define-constant err-payment-already-made (err u115))
(define-constant err-invalid-payment-amount (err u116))
(define-constant err-payment-schedule-not-found (err u117))

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
(define-data-var dispute-counter uint u0)
(define-data-var arbitrator-threshold uint u3)

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

(define-map PropertyDisputes
    { dispute-id: uint }
    {
        property-id: uint,
        milestone: (string-ascii 20),
        raised-by: principal,
        status: (string-ascii 20),
        resolution: (string-ascii 50),
        arbitrators: (list 10 principal),
        votes-for: uint,
        votes-against: uint,
        created-at: uint,
    }
)

(define-map ArbitratorVotes
    {
        dispute-id: uint,
        arbitrator: principal,
    }
    { vote: bool }
)

(define-map PaymentSchedules
    { property-id: uint }
    {
        inspection-amount: uint,
        title-amount: uint,
        mortgage-amount: uint,
        final-amount: uint,
        inspection-paid: bool,
        title-paid: bool,
        mortgage-paid: bool,
        final-paid: bool,
        total-paid: uint,
    }
)

(define-map EscrowTemplates
    { template-id: uint }
    {
        seller: principal,
        default-price: uint,
        default-deposit: uint,
        default-deadline-offset: uint,
        approvers: (list 10 principal),
    }
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
            (current-height burn-block-height)
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
            (current-height burn-block-height)
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
        (ok (list
            (get-transaction-by-id property-id u1)
            (get-transaction-by-id property-id u2)
            (get-transaction-by-id property-id u3)
            (get-transaction-by-id property-id u4)
            (get-transaction-by-id property-id u5)
        ))
    )
)

(define-private (get-transaction-by-id
        (property-id uint)
        (transaction-id uint)
    )
    (map-get? PropertyHistory {
        property-id: property-id,
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

(define-public (raise-dispute
        (property-id uint)
        (milestone (string-ascii 20))
        (arbitrators (list 10 principal))
    )
    (let (
            (property (unwrap! (map-get? Properties { property-id: property-id })
                err-not-found
            ))
            (new-dispute-id (+ (var-get dispute-counter) u1))
        )
        (asserts!
            (or
                (is-eq tx-sender (get buyer property))
                (is-eq tx-sender (get seller property))
            )
            err-not-authorized
        )
        (asserts! (is-none (get-active-dispute property-id milestone))
            err-dispute-already-exists
        )
        (map-set PropertyDisputes { dispute-id: new-dispute-id } {
            property-id: property-id,
            milestone: milestone,
            raised-by: tx-sender,
            status: "active",
            resolution: "",
            arbitrators: arbitrators,
            votes-for: u0,
            votes-against: u0,
            created-at: burn-block-height,
        })
        (var-set dispute-counter new-dispute-id)
        (ok new-dispute-id)
    )
)

(define-public (arbitrator-vote
        (dispute-id uint)
        (vote bool)
    )
    (let (
            (dispute (unwrap! (map-get? PropertyDisputes { dispute-id: dispute-id })
                err-dispute-not-found
            ))
            (arbitrators (get arbitrators dispute))
        )
        (asserts! (is-some (index-of arbitrators tx-sender)) err-not-arbitrator)
        (asserts! (is-eq (get status dispute) "active") err-dispute-not-active)
        (asserts!
            (is-none (map-get? ArbitratorVotes {
                dispute-id: dispute-id,
                arbitrator: tx-sender,
            }))
            err-already-voted
        )
        (map-set ArbitratorVotes {
            dispute-id: dispute-id,
            arbitrator: tx-sender,
        } { vote: vote }
        )
        (let (
                (new-votes-for (if vote
                    (+ (get votes-for dispute) u1)
                    (get votes-for dispute)
                ))
                (new-votes-against (if vote
                    (get votes-against dispute)
                    (+ (get votes-against dispute) u1)
                ))
                (total-votes (+ new-votes-for new-votes-against))
            )
            (map-set PropertyDisputes { dispute-id: dispute-id }
                (merge dispute {
                    votes-for: new-votes-for,
                    votes-against: new-votes-against,
                })
            )
            (if (>= total-votes (var-get arbitrator-threshold))
                (resolve-dispute dispute-id)
                (ok true)
            )
        )
    )
)

(define-private (resolve-dispute (dispute-id uint))
    (let (
            (dispute (unwrap! (map-get? PropertyDisputes { dispute-id: dispute-id })
                err-dispute-not-found
            ))
            (votes-for (get votes-for dispute))
            (votes-against (get votes-against dispute))
            (property-id (get property-id dispute))
            (milestone (get milestone dispute))
        )
        (if (> votes-for votes-against)
            (begin
                (map-set PropertyDisputes { dispute-id: dispute-id }
                    (merge dispute {
                        status: "resolved",
                        resolution: "milestone-approved",
                    })
                )
                (try! (force-milestone-approval property-id milestone))
            )
            (begin
                (map-set PropertyDisputes { dispute-id: dispute-id }
                    (merge dispute {
                        status: "resolved",
                        resolution: "milestone-rejected",
                    })
                )
                (try! (force-milestone-rejection property-id milestone))
            )
        )
        (ok true)
    )
)

(define-private (force-milestone-approval
        (property-id uint)
        (milestone (string-ascii 20))
    )
    (let ((property (unwrap! (map-get? Properties { property-id: property-id }) err-not-found)))
        (if (is-eq milestone "inspection")
            (map-set Properties { property-id: property-id }
                (merge property { inspection-passed: true })
            )
            (if (is-eq milestone "title")
                (map-set Properties { property-id: property-id }
                    (merge property { title-cleared: true })
                )
                (if (is-eq milestone "mortgage")
                    (map-set Properties { property-id: property-id }
                        (merge property { mortgage-approved: true })
                    )
                    false
                )
            )
        )
        (ok true)
    )
)

(define-private (force-milestone-rejection
        (property-id uint)
        (milestone (string-ascii 20))
    )
    (let ((property (unwrap! (map-get? Properties { property-id: property-id }) err-not-found)))
        (if (is-eq milestone "inspection")
            (map-set Properties { property-id: property-id }
                (merge property { inspection-passed: false })
            )
            (if (is-eq milestone "title")
                (map-set Properties { property-id: property-id }
                    (merge property { title-cleared: false })
                )
                (if (is-eq milestone "mortgage")
                    (map-set Properties { property-id: property-id }
                        (merge property { mortgage-approved: false })
                    )
                    false
                )
            )
        )
        (ok true)
    )
)

(define-private (get-active-dispute
        (property-id uint)
        (milestone (string-ascii 20))
    )
    (fold check-active-dispute (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) none)
)

(define-private (check-active-dispute
        (dispute-id uint)
        (acc (optional uint))
    )
    (if (is-some acc)
        acc
        (match (map-get? PropertyDisputes { dispute-id: dispute-id })
            dispute (if (is-eq (get status dispute) "active")
                (some dispute-id)
                none
            )
            none
        )
    )
)

(define-read-only (get-dispute (dispute-id uint))
    (ok (map-get? PropertyDisputes { dispute-id: dispute-id }))
)

(define-read-only (get-arbitrator-vote
        (dispute-id uint)
        (arbitrator principal)
    )
    (ok (map-get? ArbitratorVotes {
        dispute-id: dispute-id,
        arbitrator: arbitrator,
    }))
)

(define-public (update-arbitrator-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set arbitrator-threshold new-threshold)
        (ok true)
    )
)

(define-read-only (get-arbitrator-threshold)
    (ok (var-get arbitrator-threshold))
)

(define-public (setup-payment-schedule
        (property-id uint)
        (inspection-amount uint)
        (title-amount uint)
        (mortgage-amount uint)
        (final-amount uint)
    )
    (let ((property (unwrap! (map-get? Properties { property-id: property-id }) err-not-found)))
        (asserts! (is-eq tx-sender (get seller property)) err-not-authorized)
        (asserts!
            (is-eq
                (+ (+ (+ inspection-amount title-amount) mortgage-amount)
                    final-amount
                )
                (get price property)
            )
            err-invalid-payment-amount
        )
        (map-set PaymentSchedules { property-id: property-id } {
            inspection-amount: inspection-amount,
            title-amount: title-amount,
            mortgage-amount: mortgage-amount,
            final-amount: final-amount,
            inspection-paid: false,
            title-paid: false,
            mortgage-paid: false,
            final-paid: false,
            total-paid: u0,
        })
        (ok true)
    )
)

(define-public (pay-milestone
        (property-id uint)
        (milestone (string-ascii 20))
    )
    (let (
            (property (unwrap! (map-get? Properties { property-id: property-id })
                err-not-found
            ))
            (schedule (unwrap! (map-get? PaymentSchedules { property-id: property-id })
                err-payment-schedule-not-found
            ))
        )
        (asserts! (is-eq tx-sender (get buyer property)) err-not-authorized)
        (if (is-eq milestone "inspection")
            (begin
                (asserts! (get inspection-passed property)
                    err-milestone-not-completed
                )
                (asserts! (not (get inspection-paid schedule))
                    err-payment-already-made
                )
                (try! (stx-transfer? (get inspection-amount schedule) tx-sender
                    (get seller property)
                ))
                (map-set PaymentSchedules { property-id: property-id }
                    (merge schedule {
                        inspection-paid: true,
                        total-paid: (+ (get total-paid schedule)
                            (get inspection-amount schedule)
                        ),
                    })
                )
                (ok true)
            )
            (if (is-eq milestone "title")
                (begin
                    (asserts! (get title-cleared property)
                        err-milestone-not-completed
                    )
                    (asserts! (not (get title-paid schedule))
                        err-payment-already-made
                    )
                    (try! (stx-transfer? (get title-amount schedule) tx-sender
                        (get seller property)
                    ))
                    (map-set PaymentSchedules { property-id: property-id }
                        (merge schedule {
                            title-paid: true,
                            total-paid: (+ (get total-paid schedule)
                                (get title-amount schedule)
                            ),
                        })
                    )
                    (ok true)
                )
                (if (is-eq milestone "mortgage")
                    (begin
                        (asserts! (get mortgage-approved property)
                            err-milestone-not-completed
                        )
                        (asserts! (not (get mortgage-paid schedule))
                            err-payment-already-made
                        )
                        (try! (stx-transfer? (get mortgage-amount schedule) tx-sender
                            (get seller property)
                        ))
                        (map-set PaymentSchedules { property-id: property-id }
                            (merge schedule {
                                mortgage-paid: true,
                                total-paid: (+ (get total-paid schedule)
                                    (get mortgage-amount schedule)
                                ),
                            })
                        )
                        (ok true)
                    )
                    (if (is-eq milestone "final")
                        (begin
                            (asserts! (get inspection-paid schedule)
                                err-milestone-not-completed
                            )
                            (asserts! (get title-paid schedule)
                                err-milestone-not-completed
                            )
                            (asserts! (get mortgage-paid schedule)
                                err-milestone-not-completed
                            )
                            (asserts! (not (get final-paid schedule))
                                err-payment-already-made
                            )
                            (try! (stx-transfer? (get final-amount schedule) tx-sender
                                (get seller property)
                            ))
                            (map-set PaymentSchedules { property-id: property-id }
                                (merge schedule {
                                    final-paid: true,
                                    total-paid: (+ (get total-paid schedule)
                                        (get final-amount schedule)
                                    ),
                                })
                            )
                            (map-set Properties { property-id: property-id }
                                (merge property { status: "completed" })
                            )
                            (ok true)
                        )
                        err-not-found
                    )
                )
            )
        )
    )
)

(define-read-only (get-payment-schedule (property-id uint))
    (ok (map-get? PaymentSchedules { property-id: property-id }))
)

(define-read-only (get-payment-progress (property-id uint))
    (let ((schedule (map-get? PaymentSchedules { property-id: property-id })))
        (ok (if (is-some schedule)
            (some {
                total-paid: (get total-paid (unwrap-panic schedule)),
                inspection-status: (get inspection-paid (unwrap-panic schedule)),
                title-status: (get title-paid (unwrap-panic schedule)),
                mortgage-status: (get mortgage-paid (unwrap-panic schedule)),
                final-status: (get final-paid (unwrap-panic schedule)),
            })
            none
        ))
    )
)

(define-public (create-escrow-template
        (template-id uint)
        (default-price uint)
        (default-deposit uint)
        (default-deadline-offset uint)
        (approvers (list 10 principal))
    )
    (begin
        (asserts!
            (is-none (map-get? EscrowTemplates { template-id: template-id }))
            err-already-initialized
        )
        (asserts! (> default-deadline-offset u0) err-wrong-status)
        (asserts! (>= default-deposit (var-get minimum-deposit))
            err-insufficient-funds
        )
        (map-set EscrowTemplates { template-id: template-id } {
            seller: tx-sender,
            default-price: default-price,
            default-deposit: default-deposit,
            default-deadline-offset: default-deadline-offset,
            approvers: approvers,
        })
        (ok true)
    )
)

(define-read-only (get-escrow-template (template-id uint))
    (ok (unwrap! (map-get? EscrowTemplates { template-id: template-id })
        err-not-found
    ))
)

(define-public (create-escrow-from-template
        (property-id uint)
        (buyer principal)
        (template-id uint)
    )
    (let (
            (current-height burn-block-height)
            (template (unwrap! (map-get? EscrowTemplates { template-id: template-id })
                err-not-found
            ))
            (deadline (+ current-height (get default-deadline-offset template)))
            (deposit (get default-deposit template))
            (price (get default-price template))
        )
        (asserts! (is-eq tx-sender (get seller template)) err-not-authorized)
        (asserts! (> deadline current-height) err-wrong-status)
        (asserts! (>= deposit (var-get minimum-deposit)) err-insufficient-funds)
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
        (map-set PropertyApprovers { property-id: property-id } { approvers: (get approvers template) })
        (unwrap-panic (record-property-transaction property-id none tx-sender deposit
            "escrow-created"
        ))
        (ok true)
    )
)
