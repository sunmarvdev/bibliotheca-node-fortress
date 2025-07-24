;; Bibliotheca-Node-Fortress - Distributed library management blockchain: Digital Antiquarian Text Authentication System
;; Immutable ledger protocol for scholarly document verification and custody management

;; Protocol Authority Configuration
(define-constant supreme-administrator tx-sender)

;; System Response Codes for Transaction Results
(define-constant forbidden-access-violation (err u300))
(define-constant document-absence-error (err u301))
(define-constant duplicate-entry-conflict (err u302))
(define-constant invalid-title-format (err u303))
(define-constant page-count-boundary-exceeded (err u304))
(define-constant ownership-mismatch-error (err u305))
(define-constant access-denied-insufficient-privilege (err u306))
(define-constant authentication-process-failed (err u307))
(define-constant metadata-validation-failure (err u308))

;; Global Document Indexing System
(define-data-var next-document-identifier uint u0)

;; Primary Data Storage Structures

;; Main registry for authenticated scholarly documents
(define-map nexus-document-repository
  { document-id: uint }
  {
    title-inscription: (string-ascii 64),
    current-guardian: principal,
    page-count-total: uint,
    creation-block-stamp: uint,
    historical-context: (string-ascii 128),
    category-labels: (list 10 (string-ascii 32))
  }
)

;; Scholar access permissions tracking system
(define-map research-access-registry
  { document-id: uint, scholar-address: principal }
  { access-granted: bool }
)

;; Internal Validation Functions

;; Verifies document existence in the repository
(define-private (document-exists-in-vault? (doc-id uint))
  (is-some (map-get? nexus-document-repository { document-id: doc-id }))
)

;; Confirms guardian authorization over specific document
(define-private (verify-guardian-authority? (doc-id uint) (potential-guardian principal))
  (match (map-get? nexus-document-repository { document-id: doc-id })
    doc-record (is-eq (get current-guardian doc-record) potential-guardian)
    false
  )
)

;; Retrieves total page count for specified document
(define-private (get-document-page-total (doc-id uint))
  (default-to u0
    (get page-count-total
      (map-get? nexus-document-repository { document-id: doc-id })
    )
  )
)

;; Validates individual category label format
(define-private (validate-category-label (label (string-ascii 32)))
  (and
    (> (len label) u0)
    (< (len label) u33)
  )
)

;; Ensures category label list integrity
(define-private (validate-category-list (labels (list 10 (string-ascii 32))))
  (and
    (> (len labels) u0)
    (<= (len labels) u10)
    (is-eq (len (filter validate-category-label labels)) (len labels))
  )
)

;; Public Interface Functions

;; Registers new document into the vault system
(define-public (register-scholarly-document 
  (document-title (string-ascii 64)) 
  (total-pages uint) 
  (context-description (string-ascii 128)) 
  (category-tags (list 10 (string-ascii 32)))
)
  (let
    (
      (new-doc-id (+ (var-get next-document-identifier) u1))
    )
    ;; Input validation checks
    (asserts! (> (len document-title) u0) invalid-title-format)
    (asserts! (< (len document-title) u65) invalid-title-format)
    (asserts! (> total-pages u0) page-count-boundary-exceeded)
    (asserts! (< total-pages u1000000000) page-count-boundary-exceeded)
    (asserts! (> (len context-description) u0) invalid-title-format)
    (asserts! (< (len context-description) u129) invalid-title-format)
    (asserts! (validate-category-list category-tags) metadata-validation-failure)

    ;; Store document record in main repository
    (map-insert nexus-document-repository
      { document-id: new-doc-id }
      {
        title-inscription: document-title,
        current-guardian: tx-sender,
        page-count-total: total-pages,
        creation-block-stamp: block-height,
        historical-context: context-description,
        category-labels: category-tags
      }
    )

    ;; Grant initial access privileges to creator
    (map-insert research-access-registry
      { document-id: new-doc-id, scholar-address: tx-sender }
      { access-granted: true }
    )

    ;; Update global document counter
    (var-set next-document-identifier new-doc-id)
    (ok new-doc-id)
  )
)

;; Transfers document guardianship to new principal
(define-public (transfer-document-custody (doc-id uint) (new-guardian principal))
  (let
    (
      (doc-record (unwrap! (map-get? nexus-document-repository { document-id: doc-id }) document-absence-error))
    )
    ;; Verify document exists and caller has authority
    (asserts! (document-exists-in-vault? doc-id) document-absence-error)
    (asserts! (is-eq (get current-guardian doc-record) tx-sender) ownership-mismatch-error)

    ;; Execute guardianship transfer
    (map-set nexus-document-repository
      { document-id: doc-id }
      (merge doc-record { current-guardian: new-guardian })
    )
    (ok true)
  )
)

;; Updates document metadata with revised information
(define-public (modify-document-metadata 
  (doc-id uint) 
  (revised-title (string-ascii 64)) 
  (revised-pages uint) 
  (revised-context (string-ascii 128)) 
  (revised-categories (list 10 (string-ascii 32)))
)
  (let
    (
      (doc-record (unwrap! (map-get? nexus-document-repository { document-id: doc-id }) document-absence-error))
    )
    ;; Verify document existence and guardian authority
    (asserts! (document-exists-in-vault? doc-id) document-absence-error)
    (asserts! (is-eq (get current-guardian doc-record) tx-sender) ownership-mismatch-error)

    ;; Validate revised metadata inputs
    (asserts! (> (len revised-title) u0) invalid-title-format)
    (asserts! (< (len revised-title) u65) invalid-title-format)
    (asserts! (> revised-pages u0) page-count-boundary-exceeded)
    (asserts! (< revised-pages u1000000000) page-count-boundary-exceeded)
    (asserts! (> (len revised-context) u0) invalid-title-format)
    (asserts! (< (len revised-context) u129) invalid-title-format)
    (asserts! (validate-category-list revised-categories) metadata-validation-failure)

    ;; Apply metadata updates to document record
    (map-set nexus-document-repository
      { document-id: doc-id }
      (merge doc-record { 
        title-inscription: revised-title, 
        page-count-total: revised-pages, 
        historical-context: revised-context, 
        category-labels: revised-categories 
      })
    )
    (ok true)
  )
)

;; Removes document permanently from vault system
(define-public (archive-document-permanently (doc-id uint))
  (let
    (
      (doc-record (unwrap! (map-get? nexus-document-repository { document-id: doc-id }) document-absence-error))
    )
    ;; Verify document existence and guardian authority
    (asserts! (document-exists-in-vault? doc-id) document-absence-error)
    (asserts! (is-eq (get current-guardian doc-record) tx-sender) ownership-mismatch-error)

    ;; Remove document from repository
    (map-delete nexus-document-repository { document-id: doc-id })
    (ok true)
  )
)

;; Expands category classification for existing document
(define-public (enhance-category-classification (doc-id uint) (additional-tags (list 10 (string-ascii 32))))
  (let
    (
      (doc-record (unwrap! (map-get? nexus-document-repository { document-id: doc-id }) document-absence-error))
      (current-tags (get category-labels doc-record))
      (merged-tags (unwrap! (as-max-len? (concat current-tags additional-tags) u10) metadata-validation-failure))
    )
    ;; Verify document existence and guardian authority
    (asserts! (document-exists-in-vault? doc-id) document-absence-error)
    (asserts! (is-eq (get current-guardian doc-record) tx-sender) ownership-mismatch-error)

    ;; Validate additional category tags
    (asserts! (validate-category-list additional-tags) metadata-validation-failure)

    ;; Update document with enhanced categorization
    (map-set nexus-document-repository
      { document-id: doc-id }
      (merge doc-record { category-labels: merged-tags })
    )
    (ok merged-tags)
  )
)

;; Revokes research access for specified scholar
(define-public (revoke-scholar-access (doc-id uint) (target-scholar principal))
  (let
    (
      (doc-record (unwrap! (map-get? nexus-document-repository { document-id: doc-id }) document-absence-error))
    )
    ;; Verify document existence and guardian authority
    (asserts! (document-exists-in-vault? doc-id) document-absence-error)
    (asserts! (is-eq (get current-guardian doc-record) tx-sender) ownership-mismatch-error)
    (asserts! (not (is-eq target-scholar tx-sender)) forbidden-access-violation)

    ;; Remove scholar access privileges
    (map-delete research-access-registry { document-id: doc-id, scholar-address: target-scholar })
    (ok true)
  )
)

;; Applies conservation status to protect document integrity
(define-public (apply-conservation-status (doc-id uint))
  (let
    (
      (doc-record (unwrap! (map-get? nexus-document-repository { document-id: doc-id }) document-absence-error))
      (conservation-tag "CONSERVATION-STATUS")
      (current-tags (get category-labels doc-record))
    )
    ;; Verify document existence and authorized intervention
    (asserts! (document-exists-in-vault? doc-id) document-absence-error)
    (asserts! 
      (or 
        (is-eq tx-sender supreme-administrator)
        (is-eq (get current-guardian doc-record) tx-sender)
      ) 
      forbidden-access-violation
    )

    (ok true)
  )
)

;; Performs comprehensive document authenticity verification
(define-public (authenticate-document-ownership (doc-id uint) (claimed-guardian principal))
  (let
    (
      (doc-record (unwrap! (map-get? nexus-document-repository { document-id: doc-id }) document-absence-error))
      (actual-guardian (get current-guardian doc-record))
      (creation-timestamp (get creation-block-stamp doc-record))
      (scholar-has-access (default-to 
        false 
        (get access-granted 
          (map-get? research-access-registry { document-id: doc-id, scholar-address: tx-sender })
        )
      ))
    )
    ;; Verify document existence and access privileges
    (asserts! (document-exists-in-vault? doc-id) document-absence-error)
    (asserts! 
      (or 
        (is-eq tx-sender actual-guardian)
        scholar-has-access
        (is-eq tx-sender supreme-administrator)
      ) 
      access-denied-insufficient-privilege
    )

    ;; Generate comprehensive authentication report
    (if (is-eq actual-guardian claimed-guardian)
      ;; Return successful authentication with metadata
      (ok {
        authentication-successful: true,
        current-block-height: block-height,
        document-age: (- block-height creation-timestamp),
        ownership-verified: true
      })
      ;; Return authentication failure report
      (ok {
        authentication-successful: false,
        current-block-height: block-height,
        document-age: (- block-height creation-timestamp),
        ownership-verified: false
      })
    )
  )
)

