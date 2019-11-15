MODULE vars_mod

  !-----------------------------------------------------------------------------
  !   This module declares 'global' variables.
  !
  !   CVS:$Id$
  !-----------------------------------------------------------------------------

  USE kinds_mod

  !*****************************************************************************

  IMPLICIT NONE
  SAVE

  !*****************************************************************************

  CHARACTER(LEN=char_len) :: src_file
  CHARACTER(LEN=char_len) :: src_var

  INTEGER(KIND=int_kind)  :: dst_imt, dst_jmt, dst_km
  CHARACTER(LEN=char_len) :: dst_horiz_grid_file
  CHARACTER(LEN=char_len) :: dst_vert_grid_file
  CHARACTER(LEN=char_len) :: dst_topography_file
  CHARACTER(LEN=char_len) :: dst_region_mask_filename

  CHARACTER(LEN=char_len) :: dst_file
  LOGICAL(KIND=log_kind)  :: dst_lclobber
  CHARACTER(LEN=char_len) :: dst_var

  !*****************************************************************************

END MODULE vars_mod
