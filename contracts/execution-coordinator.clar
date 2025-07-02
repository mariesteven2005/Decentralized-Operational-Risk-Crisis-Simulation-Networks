;; Execution Coordination System
;; Coordinates simulation execution and participant management

(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-SIMULATION-NOT-FOUND (err u301))
(define-constant ERR-INVALID-STATUS (err u302))
(define-constant ERR-PARTICIPANT-EXISTS (err u303))
(define-constant ERR-PARTICIPANT-NOT-FOUND (err u304))

(define-map simulation-executions
  { simulation-id: uint }
  {
    coordinator: principal,
    status: (string-ascii 16),
    start-time: (optional uint),
    end-time: (optional uint),
    participant-count: uint,
    completed-tasks: uint,
    total-tasks: uint
  }
)

(define-map simulation-participants
  { simulation-id: uint, participant: principal }
  {
    role: (string-ascii 32),
    joined-at: uint,
    status: (string-ascii 16),
    performance-score: uint,
    tasks-completed: uint
  }
)

(define-map simulation-tasks
  { simulation-id: uint, task-id: uint }
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    assigned-to: (optional principal),
    status: (string-ascii 16),
    created-at: uint,
    completed-at: (optional uint),
    difficulty: uint
  }
)

(define-data-var next-task-id uint u1)

(define-public (initialize-execution (simulation-id uint) (total-tasks uint))
  (begin
    (map-set simulation-executions
      { simulation-id: simulation-id }
      {
        coordinator: tx-sender,
        status: "initialized",
        start-time: none,
        end-time: none,
        participant-count: u0,
        completed-tasks: u0,
        total-tasks: total-tasks
      }
    )
    (print { event: "execution-initialized", simulation-id: simulation-id, coordinator: tx-sender })
    (ok true)
  )
)

(define-public (join-simulation (simulation-id uint) (role (string-ascii 32)))
  (match (map-get? simulation-executions { simulation-id: simulation-id })
    execution-info
    (begin
      (asserts! (is-none (map-get? simulation-participants { simulation-id: simulation-id, participant: tx-sender }))
                ERR-PARTICIPANT-EXISTS)
      (map-set simulation-participants
        { simulation-id: simulation-id, participant: tx-sender }
        {
          role: role,
          joined-at: block-height,
          status: "active",
          performance-score: u0,
          tasks-completed: u0
        }
      )
      (map-set simulation-executions
        { simulation-id: simulation-id }
        (merge execution-info {
          participant-count: (+ (get participant-count execution-info) u1)
        })
      )
      (print { event: "participant-joined", simulation-id: simulation-id, participant: tx-sender, role: role })
      (ok true)
    )
    ERR-SIMULATION-NOT-FOUND
  )
)

(define-public (start-simulation (simulation-id uint))
  (match (map-get? simulation-executions { simulation-id: simulation-id })
    execution-info
    (begin
      (asserts! (is-eq (get coordinator execution-info) tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get status execution-info) "initialized") ERR-INVALID-STATUS)
      (map-set simulation-executions
        { simulation-id: simulation-id }
        (merge execution-info {
          status: "running",
          start-time: (some block-height)
        })
      )
      (print { event: "simulation-started", simulation-id: simulation-id })
      (ok true)
    )
    ERR-SIMULATION-NOT-FOUND
  )
)

(define-public (create-task
  (simulation-id uint)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (difficulty uint))
  (let ((task-id (var-get next-task-id)))
    (match (map-get? simulation-executions { simulation-id: simulation-id })
      execution-info
      (begin
        (asserts! (is-eq (get coordinator execution-info) tx-sender) ERR-NOT-AUTHORIZED)
        (map-set simulation-tasks
          { simulation-id: simulation-id, task-id: task-id }
          {
            name: name,
            description: description,
            assigned-to: none,
            status: "pending",
            created-at: block-height,
            completed-at: none,
            difficulty: difficulty
          }
        )
        (var-set next-task-id (+ task-id u1))
        (print { event: "task-created", simulation-id: simulation-id, task-id: task-id })
        (ok task-id)
      )
      ERR-SIMULATION-NOT-FOUND
    )
  )
)

(define-public (complete-task (simulation-id uint) (task-id uint))
  (match (map-get? simulation-tasks { simulation-id: simulation-id, task-id: task-id })
    task-info
    (begin
      (asserts! (is-eq (get assigned-to task-info) (some tx-sender)) ERR-NOT-AUTHORIZED)
      (map-set simulation-tasks
        { simulation-id: simulation-id, task-id: task-id }
        (merge task-info {
          status: "completed",
          completed-at: (some block-height)
        })
      )
      (match (map-get? simulation-participants { simulation-id: simulation-id, participant: tx-sender })
        participant-info
        (map-set simulation-participants
          { simulation-id: simulation-id, participant: tx-sender }
          (merge participant-info {
            tasks-completed: (+ (get tasks-completed participant-info) u1),
            performance-score: (+ (get performance-score participant-info) (get difficulty task-info))
          })
        )
        false
      )
      (print { event: "task-completed", simulation-id: simulation-id, task-id: task-id, participant: tx-sender })
      (ok true)
    )
    ERR-SIMULATION-NOT-FOUND
  )
)

(define-read-only (get-execution (simulation-id uint))
  (map-get? simulation-executions { simulation-id: simulation-id })
)

(define-read-only (get-participant (simulation-id uint) (participant principal))
  (map-get? simulation-participants { simulation-id: simulation-id, participant: participant })
)

(define-read-only (get-task (simulation-id uint) (task-id uint))
  (map-get? simulation-tasks { simulation-id: simulation-id, task-id: task-id })
)
