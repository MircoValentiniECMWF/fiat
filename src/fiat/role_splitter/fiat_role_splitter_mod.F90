!> @file
!!
!! @brief Definition of the role-splitter factory.
!!
!! This module is the public entry point for configurable role assignment.
!! Concrete role-splitter implementations are hidden in detail modules.
!!

MODULE FIAT_ROLE_SPLITTER_MOD

  ! Symbols imported from role-splitter detail modules.
  USE :: FIAT_ROLE_SPLITTER_DETAIL_MOD, ONLY: ROLE_SPLITTER_T
  USE :: FIAT_ROLE_SPLITTER_DETAIL_MOD, ONLY: ROLE_T

IMPLICIT NONE

  ! Default visibility
  PRIVATE

  ! Whitelist of public symbols
  PUBLIC :: ROLE_T
  PUBLIC :: ROLE_SPLITTER_T
  PUBLIC :: CREATE_ROLE_SPLITTER
  PUBLIC :: FREE_ROLE_SPLITTER

CONTAINS


!> @brief Create and configure the requested role splitter.
!!
!! The factory reads `NAM_ROLE_CONFIGURATION`, allocates the selected concrete
!! implementation, and delegates implementation-specific configuration through
!! the abstract `READ_CONFIGURATION` operation.
!!
!! @param [in]  KUNIT    Unit containing the role configuration namelists.
!! @param [out] SPLITTER Allocated and configured role splitter.
!!
SUBROUTINE CREATE_ROLE_SPLITTER(KUNIT, SPLITTER)

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM
  USE :: MPL_MODULE, ONLY: MPL_ABORT
  USE :: NAMELIST_MOD, ONLY: POSNAME
  USE :: YOMHOOK, ONLY: DR_HOOK
  USE :: YOMHOOK, ONLY: JPHOOK
  USE :: YOMHOOK, ONLY: LHOOK

  ! Symbols imported from role-splitter detail modules.
  USE :: FIAT_RANGE_ROLE_SPLITTER_DETAIL_MOD, ONLY: ALLOCATE_RANGE_ROLE_SPLITTER
  USE :: FIAT_ROLE_SPLITTER_DETAIL_MOD, ONLY: ROLE_SPLITTER_T

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIM), INTENT(IN) :: KUNIT
  CLASS(ROLE_SPLITTER_T), POINTER, INTENT(OUT) :: SPLITTER

  ! Local variables
  CHARACTER(LEN=32) :: SPLITTER_TYPE
  INTEGER(KIND=JPIM) :: ERROR
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  ! Namelist definition
  NAMELIST / NAM_ROLE_CONFIGURATION / SPLITTER_TYPE

  ! Trace begin of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:CREATE_ROLE_SPLITTER', 0_JPIM, ZHOOK_HANDLE)

  ! Initialization
  NULLIFY(SPLITTER)
  SPLITTER_TYPE = ''

  ! Locate the role-splitter configuration
  CALL POSNAME(KUNIT, 'NAM_ROLE_CONFIGURATION', ERROR)
  IF (ERROR /= 0_JPIM) THEN
    IF (LHOOK) CALL DR_HOOK('FIAT:CREATE_ROLE_SPLITTER', 1_JPIM, ZHOOK_HANDLE)
    CALL MPL_ABORT('FIAT:CREATE_ROLE_SPLITTER: NAM_ROLE_CONFIGURATION is missing')
  ENDIF

  ! Read the requested role-splitter type
  READ(KUNIT, NML=NAM_ROLE_CONFIGURATION, IOSTAT=ERROR)
  IF (ERROR /= 0_JPIM) THEN
    IF (LHOOK) CALL DR_HOOK('FIAT:CREATE_ROLE_SPLITTER', 1_JPIM, ZHOOK_HANDLE)
    CALL MPL_ABORT('FIAT:CREATE_ROLE_SPLITTER: cannot read NAM_ROLE_CONFIGURATION')
  ENDIF

  ! Allocate the requested concrete implementation
  SELECT CASE (TRIM(ADJUSTL(SPLITTER_TYPE)))
  CASE ('range', 'RANGE')
    CALL ALLOCATE_RANGE_ROLE_SPLITTER(SPLITTER)
  CASE DEFAULT
    IF (LHOOK) CALL DR_HOOK('FIAT:CREATE_ROLE_SPLITTER', 1_JPIM, ZHOOK_HANDLE)
    CALL MPL_ABORT('FIAT:CREATE_ROLE_SPLITTER: unknown SPLITTER_TYPE')
  END SELECT

  ! Read implementation-specific configuration
  CALL SPLITTER%READ_CONFIGURATION(KUNIT)

  ! Trace end of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:CREATE_ROLE_SPLITTER', 1_JPIM, ZHOOK_HANDLE)

END SUBROUTINE CREATE_ROLE_SPLITTER


!> @brief Release a role splitter allocated by the factory.
!!
!! @param [in,out] SPLITTER Role splitter to release and nullify.
!!
SUBROUTINE FREE_ROLE_SPLITTER(SPLITTER)

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM
  USE :: YOMHOOK, ONLY: DR_HOOK
  USE :: YOMHOOK, ONLY: JPHOOK
  USE :: YOMHOOK, ONLY: LHOOK

  ! Symbols imported from role-splitter detail modules.
  USE :: FIAT_ROLE_SPLITTER_DETAIL_MOD, ONLY: ROLE_SPLITTER_T

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ROLE_SPLITTER_T), POINTER, INTENT(INOUT) :: SPLITTER

  ! Local variables
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  ! Trace begin of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:FREE_ROLE_SPLITTER', 0_JPIM, ZHOOK_HANDLE)

  ! Return when the splitter is not allocated
  IF (.NOT. ASSOCIATED(SPLITTER)) THEN
    IF (LHOOK) CALL DR_HOOK('FIAT:FREE_ROLE_SPLITTER', 1_JPIM, ZHOOK_HANDLE)
    RETURN
  ENDIF

  ! Release the concrete implementation
  DEALLOCATE(SPLITTER)
  NULLIFY(SPLITTER)

  ! Trace end of procedure
  IF (LHOOK) CALL DR_HOOK('FIAT:FREE_ROLE_SPLITTER', 1_JPIM, ZHOOK_HANDLE)

END SUBROUTINE FREE_ROLE_SPLITTER

END MODULE FIAT_ROLE_SPLITTER_MOD
