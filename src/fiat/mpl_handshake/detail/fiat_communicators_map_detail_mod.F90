!> @file
!!
!! @brief Internal owning map for named MPL communicators.
!!
!! This detail module supports `FIAT_MPL_HANDSHAKE_MOD`. Application code must
!! use the public facade rather than importing this module directly.
!!

MODULE FIAT_COMMUNICATORS_MAP_DETAIL_MOD

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM

IMPLICIT NONE

  ! Default visibility
  PRIVATE

  ! Internal name-to-communicator entry
  TYPE :: COMMUNICATOR_T
    CHARACTER(LEN=:), ALLOCATABLE :: NAME
    INTEGER(KIND=JPIM) :: COMM
  END TYPE COMMUNICATOR_T

  !> @brief Owning map of named communicators.
  !!
  !! Communicators stored in this map must be released with `FREE` before MPL
  !! finalization. A lookup for an unknown name returns `MPI_COMM_NULL`.
  TYPE, PUBLIC :: COMMUNICATORS_MAP_T
    PRIVATE

    ! Owned communicator entries
    TYPE(COMMUNICATOR_T), ALLOCATABLE :: COMMS_(:)

  CONTAINS

    ! Communicator lookup and management
    PROCEDURE :: GET => COMMUNICATORS_MAP_GET
    PROCEDURE :: FREE => COMMUNICATORS_MAP_FREE

  END TYPE COMMUNICATORS_MAP_T

  ! Symbol used only by the public facade
  PUBLIC :: INITIALIZE_COMMUNICATORS_MAP

CONTAINS


!> @brief Initialize a map and take ownership of communicators.
!!
!! @param [out] COMMS       Map receiving the named communicators.
!! @param [in]  NAMES       Names associated with the communicators.
!! @param [in]  GROUP_COMMS Communicators whose ownership is transferred.
!!
SUBROUTINE INITIALIZE_COMMUNICATORS_MAP(COMMS, NAMES, GROUP_COMMS)

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMUNICATORS_MAP_T), INTENT(OUT) :: COMMS
  CHARACTER(LEN=*), INTENT(IN) :: NAMES(:)
  INTEGER(KIND=JPIM), INTENT(IN) :: GROUP_COMMS(SIZE(NAMES, KIND=JPIM))

  ! Local variables
  INTEGER(KIND=JPIM) :: I

  ! Allocate the map entries
  ALLOCATE(COMMS%COMMS_(SIZE(NAMES, KIND=JPIM)))

  ! Store names and transfer communicator ownership
  DO I = 1_JPIM, SIZE(NAMES, KIND=JPIM)
    COMMS%COMMS_(I)%NAME = NAMES(I)
    COMMS%COMMS_(I)%COMM = GROUP_COMMS(I)
  ENDDO

END SUBROUTINE INITIALIZE_COMMUNICATORS_MAP


!> @brief Return the communicator associated with a name.
!!
!! @param [in] COMMS Communicator map to search.
!! @param [in] NAME  Group name to find.
!!
!! @return Associated communicator, or `MPI_COMM_NULL` when absent.
!!
FUNCTION COMMUNICATORS_MAP_GET(COMMS, NAME) RESULT(COMM)

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM
  USE :: MPL_MPIF, ONLY: MPI_COMM_NULL
  USE :: YOMHOOK, ONLY: DR_HOOK
  USE :: YOMHOOK, ONLY: JPHOOK
  USE :: YOMHOOK, ONLY: LHOOK

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMUNICATORS_MAP_T), INTENT(IN) :: COMMS
  CHARACTER(LEN=*), INTENT(IN) :: NAME

  ! Function result
  INTEGER(KIND=JPIM) :: COMM

  ! Local variables
  INTEGER(KIND=JPIM) :: I
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  ! Trace begin of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:COMMUNICATORS_MAP_GET', 0_JPIM, ZHOOK_HANDLE)

  ! Initialize the result for an absent name
  COMM = MPI_COMM_NULL

  ! Return when the map has no entries
  IF (.NOT. ALLOCATED(COMMS%COMMS_)) THEN
    IF (LHOOK) CALL DR_HOOK('FIAT:COMMUNICATORS_MAP_GET', 1_JPIM, ZHOOK_HANDLE)
    RETURN
  ENDIF

  ! Search for the requested name
  DO I = 1_JPIM, SIZE(COMMS%COMMS_, KIND=JPIM)
    IF (TRIM(COMMS%COMMS_(I)%NAME) == TRIM(NAME)) THEN
      COMM = COMMS%COMMS_(I)%COMM
      IF (LHOOK) CALL DR_HOOK('FIAT:COMMUNICATORS_MAP_GET', 1_JPIM, ZHOOK_HANDLE)
      RETURN
    ENDIF
  ENDDO

  ! Trace end of procedure for an absent name
  IF (LHOOK) CALL DR_HOOK('FIAT:COMMUNICATORS_MAP_GET', 1_JPIM, ZHOOK_HANDLE)

END FUNCTION COMMUNICATORS_MAP_GET


!> @brief Release all communicators owned by a map.
!!
!! The operation is safe when the map is empty or has already been freed.
!!
!! @param [in,out] COMMS Communicator map to release.
!!
SUBROUTINE COMMUNICATORS_MAP_FREE(COMMS)

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM
  USE :: MPL_MODULE, ONLY: MPL_ABORT
  USE :: MPL_MODULE, ONLY: MPL_COMM_FREE
  USE :: MPL_MPIF, ONLY: MPI_COMM_NULL
  USE :: YOMHOOK, ONLY: DR_HOOK
  USE :: YOMHOOK, ONLY: JPHOOK
  USE :: YOMHOOK, ONLY: LHOOK

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMUNICATORS_MAP_T), INTENT(INOUT) :: COMMS

  ! Local variables
  INTEGER(KIND=JPIM) :: ERROR
  INTEGER(KIND=JPIM) :: I
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  ! Trace begin of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:COMMUNICATORS_MAP_FREE', 0_JPIM, ZHOOK_HANDLE)

  ! Return when the map has no entries
  IF (.NOT. ALLOCATED(COMMS%COMMS_)) THEN
    IF (LHOOK) CALL DR_HOOK('FIAT:COMMUNICATORS_MAP_FREE', 1_JPIM, ZHOOK_HANDLE)
    RETURN
  ENDIF

  ! Release every communicator owned by the map
  DO I = 1_JPIM, SIZE(COMMS%COMMS_, KIND=JPIM)
    IF (COMMS%COMMS_(I)%COMM /= MPI_COMM_NULL) THEN
      CALL MPL_COMM_FREE(COMMS%COMMS_(I)%COMM, ERROR, CDSTRING='FIAT:COMMUNICATORS_MAP_FREE')
      IF (ERROR /= 0_JPIM) THEN
        IF (LHOOK) CALL DR_HOOK('FIAT:COMMUNICATORS_MAP_FREE', 1_JPIM, ZHOOK_HANDLE)
        CALL MPL_ABORT('FIAT:COMMUNICATORS_MAP_FREE: communicator free failed')
      ENDIF
      COMMS%COMMS_(I)%COMM = MPI_COMM_NULL
    ENDIF
  ENDDO

  ! Release the map storage
  DEALLOCATE(COMMS%COMMS_)

  ! Trace end of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:COMMUNICATORS_MAP_FREE', 1_JPIM, ZHOOK_HANDLE)

END SUBROUTINE COMMUNICATORS_MAP_FREE

END MODULE FIAT_COMMUNICATORS_MAP_DETAIL_MOD
