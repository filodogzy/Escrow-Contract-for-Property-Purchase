(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-already-initialized (err u102))
(define-constant err-not-found (err u103))
(define-constant err-wrong-status (err u104))
(define-constant err-insufficient-funds (err u105))

(define-data-var escrow-fee uint u2)
(define-data-var minimum-deposit uint u1000)

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

(define-public (create-escrow
        (property-id uint)
        (buyer principal)
        (price uint)
        (deposit uint)
        (deadline uint)
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
