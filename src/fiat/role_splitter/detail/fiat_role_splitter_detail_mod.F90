!> @file
!!
!! @brief Internal abstract role-splitter definitions.
!!
!! This detail module supports `FIAT_ROLE_SPLITTER_MOD` and its concrete
!! implementations. Application code must use the public factory module.
!!

MODULE FIAT_ROLE_SPLITTER_DETAIL_MOD

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM

IMPLICIT NONE

  ! Default visibility
  PRIVATE

  ! Internal limits
  INTEGER(KIND=JPIM), PARAMETER :: MAX_ROLE_NAME_LEN = 64_JPIM

  !> @brief Role assigned to a rank.
  TYPE, PUBLIC :: ROLE_T
    PRIVATE

    ! Assigned role name
    CHARACTER(LEN=MAX_ROLE_NAME_LEN) :: ROLE_NAME_ = ''

  CONTAINS

    ! Role management
    PROCEDURE :: MY_ROLE => ROLE_MY_ROLE
    PROCEDURE :: FREE => ROLE_FREE

  END TYPE ROLE_T

  !> @brief Abstract interface for configurable role splitters.
  TYPE, ABSTRACT, PUBLIC :: ROLE_SPLITTER_T
  CONTAINS

    ! Role assignment and implementation-specific configuration
    PROCEDURE(ROLE_SPLITTER_ASSIGN), DEFERRED :: ASSIGN
    PROCEDURE(ROLE_SPLITTER_READ_CONFIGURATION), DEFERRED :: READ_CONFIGURATION

  END TYPE ROLE_SPLITTER_T

  ! Deferred role-splitter interfaces
  ABSTRACT INTERFACE

    SUBROUTINE ROLE_SPLITTER_ASSIGN(SPLITTER, COMM, ROLE)
      USE :: EC_PARKIND, ONLY: JPIM

      IMPORT :: ROLE_SPLITTER_T
      IMPORT :: ROLE_T

    IMPLICIT NONE

      CLASS(ROLE_SPLITTER_T), INTENT(IN) :: SPLITTER
      INTEGER(KIND=JPIM), INTENT(IN) :: COMM
      TYPE(ROLE_T), INTENT(OUT) :: ROLE

    END SUBROUTINE ROLE_SPLITTER_ASSIGN


    SUBROUTINE ROLE_SPLITTER_READ_CONFIGURATION(SPLITTER, KUNIT)
      USE :: EC_PARKIND, ONLY: JPIM

      IMPORT :: ROLE_SPLITTER_T

    IMPLICIT NONE

      CLASS(ROLE_SPLITTER_T), INTENT(INOUT) :: SPLITTER
      INTEGER(KIND=JPIM), INTENT(IN) :: KUNIT

    END SUBROUTINE ROLE_SPLITTER_READ_CONFIGURATION

  END INTERFACE

  ! Symbols used only by concrete detail implementations
  PUBLIC :: MAX_ROLE_NAME_LEN
  PUBLIC :: SET_ROLE_NAME

CONTAINS


!> @brief Set the role name during concrete role assignment.
!!
!! @param [out] ROLE      Role receiving the assigned name.
!! @param [in]  ROLE_NAME Assigned role name.
!!
SUBROUTINE SET_ROLE_NAME(ROLE, ROLE_NAME)

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ROLE_T), INTENT(OUT) :: ROLE
  CHARACTER(LEN=*), INTENT(IN) :: ROLE_NAME

  ! Store the assigned role name
  ROLE%ROLE_NAME_ = ROLE_NAME

END SUBROUTINE SET_ROLE_NAME


!> @brief Return the role assigned to a rank.
!!
!! @param [in] ROLE Assigned role object.
!!
!! @return Assigned role name.
!!
FUNCTION ROLE_MY_ROLE(ROLE) RESULT(ROLE_NAME)

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM
  USE :: YOMHOOK, ONLY: DR_HOOK
  USE :: YOMHOOK, ONLY: JPHOOK
  USE :: YOMHOOK, ONLY: LHOOK

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ROLE_T), INTENT(IN) :: ROLE

  ! Function result
  CHARACTER(LEN=MAX_ROLE_NAME_LEN) :: ROLE_NAME

  ! Local variables
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  ! Retrieve the assigned role name
  IF (LHOOK) CALL DR_HOOK('FIAT:ROLE_MY_ROLE', 0_JPIM, ZHOOK_HANDLE)
  ROLE_NAME = ROLE%ROLE_NAME_
  IF (LHOOK) CALL DR_HOOK('FIAT:ROLE_MY_ROLE', 1_JPIM, ZHOOK_HANDLE)

END FUNCTION ROLE_MY_ROLE


!> @brief Clear the assigned role.
!!
!! @param [in,out] ROLE Role object to clear.
!!
SUBROUTINE ROLE_FREE(ROLE)

  ! Symbols imported from other modules within the project.
  USE :: EC_PARKIND, ONLY: JPIM
  USE :: YOMHOOK, ONLY: DR_HOOK
  USE :: YOMHOOK, ONLY: JPHOOK
  USE :: YOMHOOK, ONLY: LHOOK

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ROLE_T), INTENT(INOUT) :: ROLE

  ! Local variables
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

  ! Clear the assigned role name
  IF (LHOOK) CALL DR_HOOK('FIAT:ROLE_FREE', 0_JPIM, ZHOOK_HANDLE)
  ROLE%ROLE_NAME_ = ''
  IF (LHOOK) CALL DR_HOOK('FIAT:ROLE_FREE', 1_JPIM, ZHOOK_HANDLE)

END SUBROUTINE ROLE_FREE

END MODULE FIAT_ROLE_SPLITTER_DETAIL_MOD
