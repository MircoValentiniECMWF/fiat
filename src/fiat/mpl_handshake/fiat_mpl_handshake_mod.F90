!> @file
!!
!! @brief Public interface for name-driven MPL communicator construction.
!!
!! This facade builds and owns communicators for ranks that announce the same
!! group names. Collective protocol and map storage are hidden in detail
!! modules.
!!

MODULE FIAT_MPL_HANDSHAKE_MOD

  ! Symbols imported from handshake detail modules.
  USE :: FIAT_COMMUNICATORS_MAP_DETAIL_MOD, ONLY: COMMUNICATORS_MAP_T
  USE :: FIAT_MPL_HANDSHAKE_DETAIL_MOD, ONLY: MAX_GROUPNAME_LEN

IMPLICIT NONE

  ! Default visibility
  PRIVATE

  ! Whitelist of public symbols
  PUBLIC :: MAX_GROUPNAME_LEN
  PUBLIC :: COMMUNICATORS_MAP_T
  PUBLIC :: BUILD_COMMUNICATORS

CONTAINS


!> @brief Build an owning map of named communicators.
!!
!! All ranks in `COMM` must call this collective procedure. Ranks may announce
!! different sets of names. The resulting map owns every local communicator
!! and must be released with its `FREE` operation before MPL finalization.
!!
!! @param [in]  NAMES Group names announced by the calling rank.
!! @param [out] COMMS Owning map populated with the local communicators.
!! @param [in]  COMM  Parent MPL communicator.
!!
SUBROUTINE BUILD_COMMUNICATORS(NAMES, COMMS, COMM)

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM
  USE :: MPL_MPIF, ONLY: MPI_COMM_NULL
  USE :: YOMHOOK, ONLY: DR_HOOK
  USE :: YOMHOOK, ONLY: JPHOOK
  USE :: YOMHOOK, ONLY: LHOOK

  ! Symbols imported from handshake detail modules.
  USE :: FIAT_COMMUNICATORS_MAP_DETAIL_MOD, ONLY: COMMUNICATORS_MAP_T
  USE :: FIAT_COMMUNICATORS_MAP_DETAIL_MOD, ONLY: INITIALIZE_COMMUNICATORS_MAP
  USE :: FIAT_MPL_HANDSHAKE_DETAIL_MOD, ONLY: HANDSHAKE
  USE :: FIAT_MPL_HANDSHAKE_DETAIL_MOD, ONLY: MAX_GROUPNAME_LEN

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=MAX_GROUPNAME_LEN), INTENT(IN) :: NAMES(:)
  TYPE(COMMUNICATORS_MAP_T), INTENT(OUT) :: COMMS
  INTEGER(KIND=JPIM), INTENT(IN) :: COMM

  ! Local variables
  INTEGER(KIND=JPIM), ALLOCATABLE :: GROUP_COMMS(:)
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  ! Trace begin of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:BUILD_COMMUNICATORS', 0_JPIM, ZHOOK_HANDLE)

  ! Build communicators in the same order as the supplied names
  ALLOCATE(GROUP_COMMS(SIZE(NAMES, KIND=JPIM)))
  GROUP_COMMS = MPI_COMM_NULL
  CALL HANDSHAKE(COMM, NAMES, GROUP_COMMS)

  ! Transfer communicator ownership to the map
  CALL INITIALIZE_COMMUNICATORS_MAP(COMMS, NAMES, GROUP_COMMS)
  DEALLOCATE(GROUP_COMMS)

  ! Trace end of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:BUILD_COMMUNICATORS', 1_JPIM, ZHOOK_HANDLE)

END SUBROUTINE BUILD_COMMUNICATORS

END MODULE FIAT_MPL_HANDSHAKE_MOD
