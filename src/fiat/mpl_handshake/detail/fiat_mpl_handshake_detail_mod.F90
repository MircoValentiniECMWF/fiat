!> @file
!!
!! @brief Internal collective protocol for named MPL communicators.
!!
!! This detail module supports `FIAT_MPL_HANDSHAKE_MOD`. Application code must
!! use the public facade rather than importing this module directly.
!!

MODULE FIAT_MPL_HANDSHAKE_DETAIL_MOD

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM

IMPLICIT NONE

  ! Default visibility
  PRIVATE

  ! Public name limit and internal protocol version
  INTEGER(KIND=JPIM), PARAMETER, PUBLIC :: MAX_GROUPNAME_LEN = 64_JPIM
  INTEGER(KIND=JPIM), PARAMETER :: HANDSHAKE_VERSION = 1_JPIM

  ! Symbol used only by the public facade
  PUBLIC :: HANDSHAKE

CONTAINS


!> @brief Build communicators from group names announced by each rank.
!!
!! All ranks in `COMM` must call this collective procedure. In each protocol
!! iteration, the lowest rank with an unprocessed local name broadcasts that
!! name and all ranks collectively split the parent communicator. Ranks may
!! announce different sets of names.
!!
!! The caller owns every non-null communicator returned in `GROUP_COMMS`.
!!
!! @param [in]     COMM        Parent MPL communicator.
!! @param [in]     GROUP_NAMES Group names announced by the calling rank.
!! @param [in,out] GROUP_COMMS Communicators corresponding to local names.
!!
SUBROUTINE HANDSHAKE(COMM, GROUP_NAMES, GROUP_COMMS)

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM
  USE :: MPL_MODULE, ONLY: MPL_ABORT
  USE :: MPL_MODULE, ONLY: MPL_ALLREDUCE
  USE :: MPL_MODULE, ONLY: MPL_BROADCAST
  USE :: MPL_MODULE, ONLY: MPL_COMM_SPLIT
  USE :: MPL_MODULE, ONLY: MPL_MYRANK
  USE :: MPL_MODULE, ONLY: MPL_NPROC
  USE :: MPL_MPIF, ONLY: MPI_COMM_NULL
  USE :: YOMHOOK, ONLY: DR_HOOK
  USE :: YOMHOOK, ONLY: JPHOOK
  USE :: YOMHOOK, ONLY: LHOOK

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIM), INTENT(IN) :: COMM
  CHARACTER(LEN=MAX_GROUPNAME_LEN), INTENT(IN) :: GROUP_NAMES(:)
  INTEGER(KIND=JPIM), INTENT(INOUT) :: GROUP_COMMS(SIZE(GROUP_NAMES, KIND=JPIM))

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: GROUP_NAME
  INTEGER(KIND=JPIM) :: BROADCAST_RANK
  INTEGER(KIND=JPIM) :: COLOR
  INTEGER(KIND=JPIM) :: ERROR
  INTEGER(KIND=JPIM) :: GROUP_INDEX
  INTEGER(KIND=JPIM) :: GROUP_NAME_LENGTH
  INTEGER(KIND=JPIM) :: I
  INTEGER(KIND=JPIM) :: MATCH_INDEX
  INTEGER(KIND=JPIM) :: NEW_COMM
  INTEGER(KIND=JPIM) :: NUM_PROCS
  INTEGER(KIND=JPIM) :: RANK
  INTEGER(KIND=JPIM) :: VERSION
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  ! Trace begin of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 0_JPIM, ZHOOK_HANDLE)

  ! Verify the handshake protocol version collectively
  VERSION = HANDSHAKE_VERSION
  CALL MPL_ALLREDUCE(VERSION, 'MIN', KCOMM=COMM, KERROR=ERROR)
  IF (ERROR /= 0_JPIM) THEN
    IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 1_JPIM, ZHOOK_HANDLE)
    CALL MPL_ABORT('FIAT:HANDSHAKE: version exchange failed')
  ENDIF
  IF (VERSION /= HANDSHAKE_VERSION) THEN
    IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 1_JPIM, ZHOOK_HANDLE)
    CALL MPL_ABORT('FIAT:HANDSHAKE: incompatible protocol version')
  ENDIF

  ! Initialize rank information and local communicator results
  RANK = MPL_MYRANK(COMM) - 1_JPIM
  NUM_PROCS = MPL_NPROC(COMM)
  GROUP_COMMS = MPI_COMM_NULL

  ! Process one globally selected group name per iteration
  DO

    ! Find the next unprocessed local group name
    GROUP_INDEX = 0_JPIM
    DO I = 1_JPIM, SIZE(GROUP_NAMES, KIND=JPIM)
      IF (GROUP_COMMS(I) == MPI_COMM_NULL) THEN
        GROUP_INDEX = I
        EXIT
      ENDIF
    ENDDO

    ! Elect the lowest rank that still has an unprocessed group
    IF (GROUP_INDEX > 0_JPIM) THEN
      BROADCAST_RANK = RANK
    ELSE
      BROADCAST_RANK = NUM_PROCS
    ENDIF
    CALL MPL_ALLREDUCE(BROADCAST_RANK, 'MIN', KCOMM=COMM, KERROR=ERROR)
    IF (ERROR /= 0_JPIM) THEN
      IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 1_JPIM, ZHOOK_HANDLE)
      CALL MPL_ABORT('FIAT:HANDSHAKE: broadcaster selection failed')
    ENDIF

    ! Finish when all ranks have processed their local groups
    IF (BROADCAST_RANK == NUM_PROCS) EXIT

    ! Broadcast and validate the selected group-name length
    GROUP_NAME_LENGTH = 0_JPIM
    IF (RANK == BROADCAST_RANK) THEN
      GROUP_NAME_LENGTH = LEN_TRIM(GROUP_NAMES(GROUP_INDEX), KIND=JPIM)
    ENDIF
    CALL MPL_BROADCAST(GROUP_NAME_LENGTH, KTAG=1_JPIM, KROOT=BROADCAST_RANK+1_JPIM, &
      KCOMM=COMM, KERROR=ERROR)
    IF (ERROR /= 0_JPIM) THEN
      IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 1_JPIM, ZHOOK_HANDLE)
      CALL MPL_ABORT('FIAT:HANDSHAKE: group-name length broadcast failed')
    ENDIF
    IF (GROUP_NAME_LENGTH < 0_JPIM .OR. GROUP_NAME_LENGTH > MAX_GROUPNAME_LEN) THEN
      IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 1_JPIM, ZHOOK_HANDLE)
      CALL MPL_ABORT('FIAT:HANDSHAKE: invalid group-name length')
    ENDIF

    ! Broadcast the selected group name without trailing padding
    ALLOCATE(CHARACTER(LEN=GROUP_NAME_LENGTH) :: GROUP_NAME)
    IF (RANK == BROADCAST_RANK) THEN
      GROUP_NAME = GROUP_NAMES(GROUP_INDEX)(1_JPIM:GROUP_NAME_LENGTH)
    ENDIF
    CALL MPL_BROADCAST(GROUP_NAME, KTAG=2_JPIM, KROOT=BROADCAST_RANK+1_JPIM, &
      KCOMM=COMM, KERROR=ERROR)
    IF (ERROR /= 0_JPIM) THEN
      DEALLOCATE(GROUP_NAME)
      IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 1_JPIM, ZHOOK_HANDLE)
      CALL MPL_ABORT('FIAT:HANDSHAKE: group-name broadcast failed')
    ENDIF

    ! Match the selected name and reject duplicate local names
    MATCH_INDEX = 0_JPIM
    DO I = 1_JPIM, SIZE(GROUP_NAMES, KIND=JPIM)
      IF (LEN_TRIM(GROUP_NAMES(I), KIND=JPIM) == GROUP_NAME_LENGTH) THEN
        IF (TRIM(GROUP_NAMES(I)) == GROUP_NAME) THEN
          IF (GROUP_COMMS(I) /= MPI_COMM_NULL .OR. MATCH_INDEX /= 0_JPIM) THEN
            DEALLOCATE(GROUP_NAME)
            IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 1_JPIM, ZHOOK_HANDLE)
            CALL MPL_ABORT('FIAT:HANDSHAKE: duplicate local group name')
          ENDIF
          MATCH_INDEX = I
        ENDIF
      ENDIF
    ENDDO
    DEALLOCATE(GROUP_NAME)

    ! Split collectively for group members and non-members
    IF (MATCH_INDEX > 0_JPIM) THEN
      COLOR = 0_JPIM
    ELSE
      COLOR = -1_JPIM
    ENDIF
    CALL MPL_COMM_SPLIT(COMM, COLOR, RANK, NEW_COMM, ERROR, CDSTRING='FIAT:HANDSHAKE')
    IF (ERROR /= 0_JPIM) THEN
      IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 1_JPIM, ZHOOK_HANDLE)
      CALL MPL_ABORT('FIAT:HANDSHAKE: communicator split failed')
    ENDIF

    ! Store the communicator for local group members
    IF (MATCH_INDEX > 0_JPIM) GROUP_COMMS(MATCH_INDEX) = NEW_COMM
  ENDDO

  ! Trace end of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:HANDSHAKE', 1_JPIM, ZHOOK_HANDLE)

END SUBROUTINE HANDSHAKE

END MODULE FIAT_MPL_HANDSHAKE_DETAIL_MOD
