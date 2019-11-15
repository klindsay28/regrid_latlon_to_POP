PROGRAM main

  !-----------------------------------------------------------------------------
  !   This program regrids rectilinear (lat-lon) data to POP style grid.
  !
  !   Uses trilinear (linear in x, y, z) interpolation.
  !   Source data is first extended with Laplacian smooting.
  !
  !   CVS:$Id$
  !-----------------------------------------------------------------------------

  USE kinds_mod
  USE vars_mod
  USE regrid

  !*****************************************************************************

  IMPLICIT NONE

  !*****************************************************************************

  NAMELIST /regrid_nml/ &
       src_file, src_var, &
       dst_imt, dst_jmt, dst_km, &
       dst_horiz_grid_file, dst_vert_grid_file, dst_topography_file, &
       dst_region_mask_filename, &
       dst_file, dst_lclobber, dst_var

  INTEGER(KIND=int_kind), PARAMETER :: stdin = 5

  INTEGER(KIND=int_kind) :: ios

  !*****************************************************************************

  src_file = 'unknown'
  src_var  = 'unknown'

  dst_imt = 0
  dst_jmt = 0
  dst_km  = 0
  dst_horiz_grid_file      = 'unknown'
  dst_vert_grid_file       = 'unknown'
  dst_topography_file      = 'unknown'
  dst_region_mask_filename = 'unknown'

  dst_file     = 'unknown'
  dst_lclobber = .TRUE.

  DO
     dst_var = 'unknown'
     READ(UNIT=stdin, NML=regrid_nml, IOSTAT=ios, ERR=10, END=20)
     IF (dst_var == 'unknown') dst_var = src_var
     PRINT *, 'src_file = ', TRIM(src_file)
     PRINT *, 'src_var  = ', TRIM(src_var)
     CALL regrid_driver
     CYCLE
10   PRINT *, 'error reading namelist, iostat = ', ios
20   EXIT
  END DO

END PROGRAM main
