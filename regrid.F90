MODULE regrid

  !-----------------------------------------------------------------------------
  !   CVS:$Id$
  !-----------------------------------------------------------------------------

  USE kinds_mod
  USE vars_mod
  USE msg_mod
  USE char_case
  USE const_mod
  USE nf_wrap
  USE nf_tools

  IMPLICIT NONE
  SAVE

  !*****************************************************************************

  INTEGER(KIND=int_kind) src_ncid, src_varid
  INTEGER(KIND=int_kind) src_imt, src_jmt, src_km, timelen

  CHARACTER(LEN=char_len) :: src_lon_name, src_lat_name, &
       src_depth_name, src_depth_edges_name, src_time_name

  REAL(KIND=real_kind), DIMENSION(:), ALLOCATABLE :: &
       src_lon, src_lat, src_depth, src_depth_edges, time

  REAL(KIND=real_kind), DIMENSION(:), ALLOCATABLE :: &
       dst_depth, dst_depth_edges

  INTEGER(KIND=int_kind) dst_kmin, dst_kmax

  INTEGER(KIND=int_kind), DIMENSION(:,:), ALLOCATABLE :: &
       dst_kmt, REGION_MASK

  REAL(KIND=real_kind), DIMENSION(:,:), ALLOCATABLE :: &
       ULAT, ULONG, TLAT, TLONG, TAREA

  INTEGER(KIND=int_kind) dst_ncid, dst_varid

  LOGICAL(KIND=log_kind) :: dst_needs_time, &
       dst_needs_depth, dst_needs_depth_edges, &
       dst_needs_X, dst_needs_Y, &
       dst_needs_ULAT, dst_needs_ULONG, &
       dst_needs_TLAT, dst_needs_TLONG, &
       dst_needs_TAREA, dst_needs_REGION_MASK, &
       dst_needs_KMT

  INTEGER(KIND=int_kind) :: dst_time_dimid, &
       dst_depth_dimid, dst_depth_edges_varid, &
       dst_X_dimid, dst_Y_dimid, &
       dst_ULAT_varid, dst_ULONG_varid, &
       dst_TLAT_varid, dst_TLONG_varid, &
       dst_TAREA_varid, dst_REGION_MASK_varid, &
       dst_KMT_varid

  CHARACTER(LEN=char_len) :: dst_time_name, &
       dst_depth_name, dst_depth_edges_name, &
       dst_X_name, dst_Y_name, &
       dst_ULAT_name, dst_ULONG_name, &
       dst_TLAT_name, dst_TLONG_name, &
       dst_TAREA_name, dst_REGION_MASK_name, &
       dst_KMT_name

  REAL(KIND=real_kind), PARAMETER :: default_msv = -1.0E+34

  !*****************************************************************************

CONTAINS

  !*****************************************************************************

  SUBROUTINE regrid_driver

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    CHARACTER(LEN=*), PARAMETER :: sub_name = 'regrid_driver'
    LOGICAL(KIND=log_kind) :: lstat_ok
    INTEGER(KIND=int_kind) :: l

    CALL read_src_file_info
    CALL read_dst_grid_info
    CALL dst_depth_select(lstat_ok)
    IF (lstat_ok) THEN
       CALL create_dst_file
       CALL def_dst_file
       CALL nf_enddef_wrap(dst_ncid)
       CALL put_aux_vars
       DO l=1,MAX(1,timelen)
          CALL msg_write(sub_name, 'regridding l = ', l)
          CALL regrid_all_z_levs(l)
       END DO
       CALL nf_close_wrap(dst_ncid)
    END IF
    CALL nf_close_wrap(src_ncid)
    CALL free_vars

  END SUBROUTINE regrid_driver

  !*****************************************************************************

  SUBROUTINE read_src_file_info

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    CHARACTER(LEN=*), PARAMETER :: sub_name = 'read_src_file_info'

    INTEGER(KIND=int_kind) :: ndims
    INTEGER(KIND=int_kind), DIMENSION(4) :: dimids

    !---------------------------------------------------------------------------

    CALL nf_open_wrap(src_file, 0, src_ncid)

    CALL nf_inq_varid_wrap(src_ncid, src_var, src_varid)

    CALL nf_inq_varndims_wrap(src_ncid, src_varid, ndims)

    IF (ndims > 4 .OR. ndims < 2) THEN
       CALL msg_write(sub_name, 'illegal ndims ', ndims)
       STOP
    END IF

    CALL nf_inq_vardimid_wrap(src_ncid, src_varid, dimids)

    CALL nf_inq_dim_wrap(src_ncid, dimids(1), src_lon_name, src_imt)

    IF ((INDEX(upper(src_lon_name), 'LON') /= 1) .AND. &
         (INDEX(upper(src_lon_name), 'X') /= 1)) THEN
       CALL msg_write(sub_name, 'unexpected src_lon_name ', &
            TRIM(src_lon_name))
       STOP
    END IF

    CALL nf_inq_dim_wrap(src_ncid, dimids(2), src_lat_name, src_jmt)

    IF ((INDEX(upper(src_lat_name), 'LAT') /= 1) .AND. &
         (INDEX(upper(src_lat_name), 'Y') /= 1)) THEN
       CALL msg_write(sub_name, 'unexpected src_lat_name ', &
            TRIM(src_lat_name))
       STOP
    END IF

    src_km = 0
    timelen = 0

    IF (ndims > 2) THEN
       CALL nf_inq_dim_wrap(src_ncid, dimids(3), src_depth_name, src_km)

       IF ((INDEX(upper(src_depth_name), 'DEPTH') /= 1) .AND. &
            (INDEX(upper(src_depth_name), 'Z') /= 1)) THEN
          IF (((INDEX(upper(src_depth_name), 'TIME') == 1) .OR. &
               (INDEX(upper(src_depth_name), 'DATE') == 1) .OR. &
               (INDEX(upper(src_depth_name), 'T') == 1) .OR. &
               (INDEX(upper(src_depth_name), 'MONTH') == 1)) .AND. &
               (ndims == 3)) THEN
             src_time_name = src_depth_name
             src_depth_name = 'unknown'
             timelen = src_km
             src_km = 0
          ELSE
             CALL msg_write(sub_name, 'unexpected src_depth_name ', &
                  TRIM(src_depth_name))
             STOP
          END IF
       END IF

       IF (ndims > 3) THEN
          CALL nf_inq_dim_wrap(src_ncid, dimids(4), src_time_name, timelen)

          IF ((INDEX(upper(src_time_name), 'TIME') /= 1) .AND. &
               (INDEX(upper(src_time_name), 'DATE') /= 1) .AND. &
               (INDEX(upper(src_time_name), 'T') /= 1) .AND. &
               (INDEX(upper(src_time_name), 'MONTH') /= 1)) THEN
             CALL msg_write(sub_name, 'unexpected src_time_name ', &
                  TRIM(src_time_name))
             STOP
          END IF
       END IF

    END IF

    ALLOCATE(src_lon(src_imt), src_lat(src_jmt))

    CALL nf_read_dim(src_ncid, dimids(1), src_lon)
    CALL nf_read_dim(src_ncid, dimids(2), src_lat)

    IF (src_km > 0) THEN
       ALLOCATE(src_depth(src_km), src_depth_edges(src_km+1))
       CALL nf_read_dim(src_ncid, dimids(3), src_depth)
       CALL nf_read_dim_edges(src_ncid, dimids(3), src_depth_edges_name, &
            src_depth_edges)
    END IF

    IF (timelen > 0) THEN
       ALLOCATE(time(timelen))
       IF (src_km > 0) THEN
          CALL nf_read_dim(src_ncid, dimids(4), time)
       ELSE
          CALL nf_read_dim(src_ncid, dimids(3), time)
       END IF
    END IF

  END SUBROUTINE read_src_file_info

  !*****************************************************************************

  SUBROUTINE nf_read_dim(ncid, dimid, dim_vals)

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

    INTEGER(KIND=int_kind), INTENT(IN) :: ncid, dimid
    REAL(KIND=real_kind), DIMENSION(:), INTENT(OUT) :: dim_vals

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    CHARACTER(LEN=*), PARAMETER :: sub_name = 'nf_read_dim'

    INTEGER(KIND=int_kind) :: varid, ndims
    INTEGER(KIND=int_kind), DIMENSION(1) :: vardims
    CHARACTER(LEN=char_len) :: var_name

    !---------------------------------------------------------------------------

    CALL nf_inq_dimname_wrap(ncid, dimid, var_name)
    CALL nf_inq_varid_wrap(ncid, var_name, varid)

    CALL nf_inq_varndims_wrap(ncid, varid, ndims)
    IF (ndims /= 1) THEN
       CALL msg_write(sub_name, 'illegal ndims for ', var_name, ndims)
       STOP
    END IF

    CALL nf_inq_vardimid_wrap(ncid, varid, vardims)
    IF (vardims(1) /= dimid) THEN
       CALL msg_write(sub_name, 'illegal dimid for ', var_name, vardims(1))
       STOP
    END IF

    CALL nf_get_var_wrap(ncid, varid, dim_vals)

  END SUBROUTINE nf_read_dim

  !*****************************************************************************

  SUBROUTINE nf_read_dim_edges(ncid, dimid, dim_edge_name, dim_edge_vals)

    include 'netcdf.inc'

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

    INTEGER(KIND=int_kind), INTENT(IN) :: ncid, dimid
    REAL(KIND=real_kind), DIMENSION(:), INTENT(OUT) :: dim_edge_vals
    CHARACTER(len=*), INTENT(OUT) :: dim_edge_name

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    CHARACTER(LEN=*), PARAMETER :: sub_name = 'nf_read_dim_edges'

    INTEGER(KIND=int_kind) :: stat, varid
    INTEGER(KIND=int_kind) :: ndims
    INTEGER(KIND=int_kind), DIMENSION(1) :: vardims
    INTEGER(KIND=int_kind) :: dimlen, dimlenp1, i
    REAL(KIND=real_kind), DIMENSION(:), ALLOCATABLE :: dim_vals
    REAL(KIND=real_kind) :: delta

    CHARACTER(LEN=char_len) :: var_name

    !---------------------------------------------------------------------------

    CALL nf_inq_dim_wrap(ncid, dimid, var_name, dimlen)
    CALL nf_inq_varid_wrap(ncid, var_name, varid)

    CALL nf_inq_varndims_wrap(ncid, varid, ndims)
    IF (ndims /= 1) THEN
       CALL msg_write(sub_name, 'illegal ndims for ', var_name, ndims)
       STOP
    END IF

    CALL nf_inq_vardimid_wrap(ncid, varid, vardims)
    IF (vardims(1) /= dimid) THEN
       CALL msg_write(sub_name, 'illegal dimid for ', var_name, vardims(1))
       STOP
    END IF

    dim_edge_name = ''
    CALL nf_get_att_wrap(ncid, varid, 'edges', dim_edge_name, &
         ALLOW=NF_ENOTATT, stat_out=stat)

    !---------------------------------------------------------------------------
    !   If no edge attribute exists for the variable corresponding to
    !   the dimension, generate edges.
    !---------------------------------------------------------------------------

    IF (stat == NF_ENOTATT) THEN
       dim_edge_name = TRIM(var_name) // '_edges'

       ALLOCATE(dim_vals(1:dimlen))
       CALL nf_get_var_wrap(ncid, varid, dim_vals)

       IF (dimlen == 1) THEN
          dim_edge_vals(1) = dim_vals(1)
          dim_edge_vals(2) = dim_vals(1)
       ELSE
          delta = dim_vals(2) - dim_vals(1)
          dim_edge_vals(1) = dim_vals(1) - p5 * delta
          DO i = 1,dimlen
             delta = dim_vals(i) - dim_edge_vals(i)
             dim_edge_vals(i+1) = dim_edge_vals(i) + c2 * delta
          END DO
       END IF
       DEALLOCATE(dim_vals)
    ELSE
       CALL nf_inq_varid_wrap(ncid, dim_edge_name, varid)

       CALL nf_inq_varndims_wrap(ncid, varid, ndims)
       IF (ndims /= 1) THEN
          CALL msg_write(sub_name, 'illegal ndims for ', dim_edge_name, ndims)
          STOP
       END IF

       CALL nf_inq_vardimid_wrap(ncid, varid, vardims)
       CALL nf_inq_dimlen_wrap(ncid, vardims(1), dimlenp1)
       IF (dimlenp1 /= dimlen + 1) THEN
          CALL msg_write(sub_name, 'invalid size for dimension ', &
               dim_edge_name, dimlenp1)
          STOP
       END IF

       CALL nf_get_var_wrap(ncid, varid, dim_edge_vals)
    END IF

  END SUBROUTINE nf_read_dim_edges

  !*****************************************************************************

  SUBROUTINE read_dst_grid_info

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    INTEGER(KIND=int_kind), PARAMETER :: unit_in  = 21
    INTEGER(KIND=int_kind) :: recl_dbl, recl_int

    REAL(KIND=dbl_kind) :: WORK(dst_imt,dst_jmt)
    REAL(KIND=real_kind) :: dz, dlat, dlon, min_lat
    INTEGER(KIND=int_kind) :: k, i, j, im1, jm1, n

    REAL(KIND=dbl_kind) :: recl_dbl_tmp
    INTEGER(KIND=int_kind) :: recl_int_tmp

    REAL(KIND=real_kind), DIMENSION(:,:,:), ALLOCATABLE :: &
         TLAT_corner, TLONG_corner

    REAL(KIND=dbl_kind), DIMENSION(:,:), ALLOCATABLE :: &
         HTN, HTE, DXT, DYT

    !---------------------------------------------------------------------------
    !   generate recl_dbl, recl_int
    !---------------------------------------------------------------------------

    INQUIRE(IOLENGTH=recl_dbl) recl_dbl_tmp
    INQUIRE(IOLENGTH=recl_int) recl_int_tmp

    !---------------------------------------------------------------------------
    !   allocate local allocatable vars
    !---------------------------------------------------------------------------

    ALLOCATE(TLAT_corner(4,dst_imt,dst_jmt), TLONG_corner(4,dst_imt,dst_jmt))
    ALLOCATE(HTN(dst_imt,dst_jmt), HTE(dst_imt,dst_jmt), &
         DXT(dst_imt,dst_jmt), DYT(dst_imt,dst_jmt))

    !---------------------------------------------------------------------------
    !   if necessary, read depth grid info
    !---------------------------------------------------------------------------

    IF (src_km > 0) THEN
       ALLOCATE(dst_depth(dst_km), dst_depth_edges(dst_km+1))

       dst_depth_edges(1) = c0
       OPEN(unit_in, file=dst_vert_grid_file, status='old', form='formatted')
       DO k = 1,dst_km
          READ(unit_in,*) dz
          dz = dz * p01
          dst_depth_edges(k+1) = dst_depth_edges(k) + dz
          dst_depth(k) = dst_depth_edges(k) + &
               p5*(dst_depth_edges(k+1)-dst_depth_edges(k))
       END DO
       CLOSE(unit_in)
    END IF

    ALLOCATE(dst_kmt(dst_imt,dst_jmt))

    OPEN(unit_in, file=dst_topography_file, status='old', &
         form='unformatted', access='direct', &
         recl=dst_imt*dst_jmt*recl_int)
    READ(unit_in,rec=1) dst_kmt
    CLOSE(unit_in)

    ALLOCATE(ULAT(dst_imt,dst_jmt), ULONG(dst_imt,dst_jmt), &
         TLAT(dst_imt,dst_jmt), TLONG(dst_imt,dst_jmt), &
         TAREA(dst_imt,dst_jmt))

    OPEN(unit_in, file=dst_horiz_grid_file, status='old', &
         form='unformatted', access='direct', &
         recl=dst_imt*dst_jmt*recl_dbl)
    READ(unit_in,rec=1) WORK
    ULAT = WORK
    READ(unit_in,rec=2) WORK
    ULONG = WORK
    READ(unit_in,rec=3) WORK
    HTN = WORK
    READ(unit_in,rec=4) WORK
    HTE = WORK
    CLOSE(unit_in)

    DO j = 1,dst_jmt
       DO i = 1,dst_imt
          IF (i /= 1) THEN
             im1 = i - 1
          ELSE
             im1 = dst_imt
          END IF

          TLAT_corner(3,i,j) = ULAT(i,j)
          TLONG_corner(3,i,j) = ULONG(i,j)

          TLAT_corner(4,i,j) = ULAT(im1,j)
          TLONG_corner(4,i,j) = ULONG(im1,j)
       END DO
    END DO

    DO j = 2,dst_jmt
       DO i = 1,dst_imt
          jm1 = j - 1

          TLAT_corner(2,i,j) = TLAT_corner(3,i,jm1)
          TLAT_corner(1,i,j) = TLAT_corner(4,i,jm1)

          TLONG_corner(2,i,j) = TLONG_corner(3,i,jm1)
          TLONG_corner(1,i,j) = TLONG_corner(4,i,jm1)
       END DO
    END DO

    min_lat = -pih + tiny

    DO i = 1,dst_imt
       dlat = TLAT_corner(1,i,3) - TLAT_corner(1,i,2)
       TLAT_corner(1,i,1) = MAX(TLAT_corner(1,i,2) - dlat, min_lat)

       dlat = TLAT_corner(2,i,3) - TLAT_corner(2,i,2)
       TLAT_corner(2,i,1) = MAX(TLAT_corner(2,i,2) - dlat, min_lat)

       TLONG_corner(1,i,1) = TLONG_corner(4,i,1)
       TLONG_corner(2,i,1) = TLONG_corner(3,i,1)
    END DO

    DO j = 1,dst_jmt
       DO i = 1,dst_imt
          IF (TLONG_corner(1,i,j) > pi2) &
               TLONG_corner(1,i,j) = TLONG_corner(1,i,j) - pi2
          IF (TLONG_corner(1,i,j) < c0) &
               TLONG_corner(1,i,j) = TLONG_corner(1,i,j) + pi2
          DO n = 2,4
             dlon = TLONG_corner(n,i,j) - TLONG_corner(n-1,i,j)
             IF (dlon < -c3*pih) &
                  TLONG_corner(n,i,j) = TLONG_corner(n,i,j) + pi2
             IF (dlon >  c3*pih) &
                  TLONG_corner(n,i,j) = TLONG_corner(n,i,j) - pi2
          END DO
       END DO
    END DO

    DO j = 1,dst_jmt
       DO i = 1,dst_imt
          TLAT(i,j) = (TLAT_corner(1,i,j) + TLAT_corner(2,i,j) + &
               TLAT_corner(3,i,j) + TLAT_corner(4,i,j)) / c4
          TLAT(i,j) = TLAT(i,j) * c90 / pih

          TLONG(i,j) = (TLONG_corner(1,i,j) + TLONG_corner(2,i,j) + &
               TLONG_corner(3,i,j) + TLONG_corner(4,i,j)) / c4
          IF (TLONG(i,j) > pi2)  TLONG(i,j) = TLONG(i,j) - pi2
          IF (TLONG(i,j) < c0) TLONG(i,j) = TLONG(i,j) + pi2
          TLONG(i,j) = TLONG(i,j) * c90 / pih
       END DO
    END DO

    ULAT = ULAT * c90 / pih
    ULONG = ULONG * c90 / pih

    !---------------------------------------------------------------------------
    !   compute TAREA the same way that POP does
    !---------------------------------------------------------------------------

    ! call s_shift(WORK, HTN)
    DO j = 1,dst_jmt
       IF (j /= 1) THEN
          jm1 = j - 1
       ELSE
          jm1 = dst_jmt
       END IF
       WORK(:,j) = HTN(:,jm1)
    END DO
    DXT = p5 * (HTN + WORK)

    ! call w_shift(WORK, HTE)
    DO i = 1,dst_imt
       IF (i /= 1) THEN
          im1 = i - 1
       ELSE
          im1 = dst_imt
       END IF
       WORK(i,:) = HTE(im1,:)
    END DO
    DYT = p5 * (HTE + WORK)

    TAREA = DXT * DYT

    !---------------------------------------------------------------------------
    !   read in REGION_MASK
    !---------------------------------------------------------------------------

    IF (dst_region_mask_filename /= 'unknown') THEN
       ALLOCATE(REGION_MASK(dst_imt,dst_jmt))

       OPEN(unit_in, file=dst_region_mask_filename, status='old', &
            form='unformatted', access='direct', &
            recl=dst_imt*dst_jmt*recl_int)
       READ(unit_in,rec=1) REGION_MASK
       CLOSE(unit_in)
    END IF

    !---------------------------------------------------------------------------
    !   deallocate local allocatable vars
    !---------------------------------------------------------------------------

    DEALLOCATE(TLAT_corner, TLONG_corner)
    DEALLOCATE(HTN, HTE, DXT, DYT)

  END SUBROUTINE read_dst_grid_info

  !*****************************************************************************

  SUBROUTINE dst_depth_select(lstat_ok)

    !---------------------------------------------------------------------------
    !   argument
    !---------------------------------------------------------------------------

    LOGICAL(KIND=log_kind), INTENT(OUT) :: lstat_ok

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    CHARACTER(LEN=*), PARAMETER :: sub_name = 'dst_depth_select'
    REAL(KIND=real_kind) :: ztmp

    !---------------------------------------------------------------------------
    !   If there is no depth information in src, do not put any in dst.
    !---------------------------------------------------------------------------

    IF (src_km == 0) THEN
       dst_kmin = 0
       dst_kmax = 0
       lstat_ok = .TRUE.
       RETURN
    END IF

    !---------------------------------------------------------------------------
    !   Non-overlapping vertical grids is an error.
    !---------------------------------------------------------------------------

    IF (MAXVAL(dst_depth) < MINVAL(src_depth)) THEN
       CALL msg_write(sub_name, 'dst_depth < src_depth')
       lstat_ok = .FALSE.
       RETURN
    END IF

    IF (MINVAL(dst_depth) > MAXVAL(src_depth)) THEN
       CALL msg_write(sub_name, 'dst_depth > src_depth')
       lstat_ok = .FALSE.
       RETURN
    END IF

    !---------------------------------------------------------------------------
    !   If src_depth is a single z surface, use closest dst_depth, otherwise
    !   use dst_depth values that are bracketed by src_depth.
    !---------------------------------------------------------------------------

    IF (src_km == 1) THEN
       dst_kmin = MINLOC(ABS(dst_depth-src_depth(1)), DIM=1)
       dst_kmax = dst_kmin
    ELSE
       ztmp = MINVAL(src_depth)
       dst_kmin = MINLOC(dst_depth, DIM=1, MASK=(dst_depth >= ztmp))

       ztmp = MAXVAL(src_depth)
       dst_kmax = MAXLOC(dst_depth, DIM=1, MASK=(dst_depth <= ztmp))

       IF (dst_kmax < dst_kmin) THEN
          CALL msg_write(sub_name, 'internal error : dst_kmax < dst_kmin')
          lstat_ok = .FALSE.
          RETURN
       END IF
    END IF

    lstat_ok = .TRUE.

  END SUBROUTINE dst_depth_select

  !*****************************************************************************

  SUBROUTINE create_dst_file

    include 'netcdf.inc'

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    LOGICAL(KIND=log_kind) :: dst_exists, lcreate
    REAL(KIND=real_kind), DIMENSION(dst_imt) :: X
    REAL(KIND=real_kind), DIMENSION(dst_jmt) :: Y
    INTEGER(KIND=int_kind) :: i, j

    !---------------------------------------------------------------------------

    dst_needs_time        = timelen > 0
    dst_needs_depth       = dst_kmin > 0
    dst_needs_depth_edges = dst_kmin > 0
    dst_needs_X           = .true.
    dst_needs_Y           = .true.
    dst_needs_ULAT        = .true.
    dst_needs_ULONG       = .true.
    dst_needs_TLAT        = .true.
    dst_needs_TLONG       = .true.
    dst_needs_TAREA       = .true.
    dst_needs_REGION_MASK = dst_region_mask_filename /= 'unknown'
    dst_needs_KMT         = .true.

    inquire(file=dst_file, exist=dst_exists)
    lcreate = dst_lclobber .OR. .NOT. dst_exists

    IF (lcreate) THEN
       CALL nf_create_wrap(dst_file, NF_64BIT_OFFSET, dst_ncid)
    ELSE
       CALL nf_open_wrap(dst_file, NF_WRITE, dst_ncid)

       !------------------------------------------------------------------------
       !   Determine which dimensions & variables are already in the file.
       !------------------------------------------------------------------------

       IF (dst_needs_time) THEN
          CALL nf_dimlookup(dst_ncid, time, dst_time_name, dst_time_dimid)
          IF (dst_time_dimid > 0) dst_needs_time = .FALSE.
       END IF

       IF (dst_needs_depth) THEN
          CALL nf_dimlookup(dst_ncid, dst_depth(dst_kmin:dst_kmax), &
               dst_depth_name, dst_depth_dimid)
          IF (dst_depth_dimid > 0) dst_needs_depth = .FALSE.
       END IF

       IF (dst_needs_depth_edges) THEN
          CALL nf_varlookup(dst_ncid, dst_depth_edges(dst_kmin:dst_kmax+1), &
               dst_depth_edges_name, dst_depth_edges_varid)
          IF (dst_depth_edges_varid > 0) dst_needs_depth_edges = .FALSE.
       END IF

       DO i = 1,dst_imt
          X(i) = REAL(i,real_kind)
       END DO
       CALL nf_dimlookup(dst_ncid, X, dst_X_name, dst_X_dimid)
       IF (dst_X_dimid > 0) dst_needs_X = .FALSE.

       DO j = 1,dst_jmt
          Y(j) = REAL(j,real_kind)
       END DO
       CALL nf_dimlookup(dst_ncid, Y, dst_Y_name, dst_Y_dimid)
       IF (dst_Y_dimid > 0) dst_needs_Y = .FALSE.

       CALL nf_varlookup(dst_ncid, ULAT, dst_ULAT_name, dst_ULAT_varid)
       IF (dst_ULAT_varid > 0) dst_needs_ULAT = .FALSE.

       CALL nf_varlookup(dst_ncid, ULONG, dst_ULONG_name, dst_ULONG_varid)
       IF (dst_ULONG_varid > 0) dst_needs_ULONG = .FALSE.

       CALL nf_varlookup(dst_ncid, TLAT, dst_TLAT_name, dst_TLAT_varid)
       IF (dst_TLAT_varid > 0) dst_needs_TLAT = .FALSE.

       CALL nf_varlookup(dst_ncid, TLONG, dst_TLONG_name, dst_TLONG_varid)
       IF (dst_TLONG_varid > 0) dst_needs_TLONG = .FALSE.

       CALL nf_varlookup(dst_ncid, TAREA, dst_TAREA_name, dst_TAREA_varid)
       IF (dst_TAREA_varid > 0) dst_needs_TAREA = .FALSE.

       IF (dst_needs_REGION_MASK) THEN
          CALL nf_varlookup(dst_ncid, REAL(REGION_MASK, real_kind), &
               dst_REGION_MASK_name, dst_REGION_MASK_varid)
          IF (dst_REGION_MASK_varid > 0) dst_needs_REGION_MASK = .FALSE.
       END IF

       CALL nf_varlookup(dst_ncid, REAL(dst_kmt, real_kind), &
            dst_KMT_name, dst_KMT_varid)
       IF (dst_KMT_varid > 0) dst_needs_KMT = .FALSE.

       CALL nf_redef_wrap(dst_ncid)
    END IF

  END SUBROUTINE create_dst_file

  !*****************************************************************************

  SUBROUTINE def_dst_file

    !---------------------------------------------------------------------------
    !   define dimensions, variables and attributes
    !---------------------------------------------------------------------------

    INCLUDE 'netcdf.inc'

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    INTEGER(KIND=int_kind) :: tmp_varid, src_timeid, natts, attnum, tmp_dimid
    CHARACTER(LEN=char_len) :: att_name, att_string
    INTEGER(KIND=int_kind) :: stat
    REAL(KIND=real_kind) :: msv

    !---------------------------------------------------------------------------

    IF (dst_needs_time) THEN
       dst_time_name = src_time_name
       CALL nf_def_dim_wrap(dst_ncid, dst_time_name, timelen, dst_time_dimid)

       CALL nf_def_var_wrap(dst_ncid, dst_time_name, NF_FLOAT, 1, &
            (/ dst_time_dimid /), tmp_varid)

       !------------------------------------------------------------------------
       !   copy attributes for src_time
       !------------------------------------------------------------------------

       CALL nf_inq_varid_wrap(src_ncid, src_time_name, src_timeid)
       CALL nf_inq_varnatts_wrap(src_ncid, src_timeid, natts)
       DO attnum = 1,natts
          CALL nf_inq_attname_wrap(src_ncid, src_timeid, attnum, att_name)
          CALL nf_copy_att_wrap(src_ncid, src_timeid, att_name, dst_ncid, &
               tmp_varid)
       END DO
    END IF

    IF (dst_needs_depth) THEN
       dst_depth_name = src_depth_name
       CALL nf_def_dim_wrap(dst_ncid, dst_depth_name, dst_kmax-dst_kmin+1, &
            dst_depth_dimid)

       CALL nf_def_var_wrap(dst_ncid, dst_depth_name, NF_FLOAT, 1, &
            (/ dst_depth_dimid /), tmp_varid)

       att_name = 'units'
       att_string = 'meters'
       CALL nf_put_att_wrap(dst_ncid, tmp_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'positive'
       att_string = 'down'
       CALL nf_put_att_wrap(dst_ncid, tmp_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'edges'
       att_string = src_depth_edges_name
       CALL nf_put_att_wrap(dst_ncid, tmp_varid, att_name, &
            LEN_TRIM(att_string), att_string)
    END IF

    IF (dst_needs_depth_edges) THEN
       dst_depth_edges_name = src_depth_edges_name
       CALL nf_def_dim_wrap(dst_ncid, dst_depth_edges_name, &
            dst_kmax-dst_kmin+2, tmp_dimid)

       CALL nf_def_var_wrap(dst_ncid, dst_depth_edges_name, NF_FLOAT, 1, &
            (/ tmp_dimid /), tmp_varid)
    END IF

    IF (dst_needs_X) THEN
       dst_X_name = 'X'
       CALL nf_def_dim_wrap(dst_ncid, dst_X_name, dst_imt, dst_X_dimid)

       CALL nf_def_var_wrap(dst_ncid, dst_X_name, NF_FLOAT, 1, &
            (/ dst_X_dimid /), tmp_varid)
    END IF

    IF (dst_needs_Y) THEN
       dst_Y_name = 'Y'
       CALL nf_def_dim_wrap(dst_ncid, dst_Y_name, dst_jmt, dst_Y_dimid)

       CALL nf_def_var_wrap(dst_ncid, dst_Y_name, NF_FLOAT, 1, &
            (/ dst_Y_dimid /), tmp_varid)
    END IF

    IF (dst_needs_ULAT) THEN
       dst_ULAT_name = 'ULAT'
       CALL nf_def_var_wrap(dst_ncid, dst_ULAT_name, NF_FLOAT, 2, &
            (/ dst_X_dimid, dst_Y_dimid /), dst_ULAT_varid)

       att_name = 'units'
       att_string = 'degrees_north'
       CALL nf_put_att_wrap(dst_ncid, dst_ULAT_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'long_name'
       att_string = 'Latitude (U grid)'
       CALL nf_put_att_wrap(dst_ncid, dst_ULAT_varid, att_name, &
            LEN_TRIM(att_string), att_string)
    END IF

    IF (dst_needs_ULONG) THEN
       dst_ULONG_name = 'ULONG'
       CALL nf_def_var_wrap(dst_ncid, dst_ULONG_name, NF_FLOAT, 2, &
            (/ dst_X_dimid, dst_Y_dimid /), dst_ULONG_varid)

       att_name = 'units'
       att_string = 'degrees_east'
       CALL nf_put_att_wrap(dst_ncid, dst_ULONG_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'long_name'
       att_string = 'Longitude (U grid)'
       CALL nf_put_att_wrap(dst_ncid, dst_ULONG_varid, att_name, &
            LEN_TRIM(att_string), att_string)
    END IF

    IF (dst_needs_TLAT) THEN
       dst_TLAT_name = 'TLAT'
       CALL nf_def_var_wrap(dst_ncid, dst_TLAT_name, NF_FLOAT, 2, &
            (/ dst_X_dimid, dst_Y_dimid /), dst_TLAT_varid)

       att_name = 'units'
       att_string = 'degrees_north'
       CALL nf_put_att_wrap(dst_ncid, dst_TLAT_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'long_name'
       att_string = 'Latitude (T grid)'
       CALL nf_put_att_wrap(dst_ncid, dst_TLAT_varid, att_name, &
            LEN_TRIM(att_string), att_string)
    END IF

    IF (dst_needs_TLONG) THEN
       dst_TLONG_name = 'TLONG'
       CALL nf_def_var_wrap(dst_ncid, dst_TLONG_name, NF_FLOAT, 2, &
            (/ dst_X_dimid, dst_Y_dimid /), dst_TLONG_varid)

       att_name = 'units'
       att_string = 'degrees_east'
       CALL nf_put_att_wrap(dst_ncid, dst_TLONG_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'long_name'
       att_string = 'Longitude (T grid)'
       CALL nf_put_att_wrap(dst_ncid, dst_TLONG_varid, att_name, &
            LEN_TRIM(att_string), att_string)
    END IF

    IF (dst_needs_TAREA) THEN
       dst_TAREA_name = 'TAREA'
       CALL nf_def_var_wrap(dst_ncid, dst_TAREA_name, NF_FLOAT, 2, &
            (/ dst_X_dimid, dst_Y_dimid /), dst_TAREA_varid)

       att_name = 'units'
       att_string = 'centimeter^2'
       CALL nf_put_att_wrap(dst_ncid, dst_TAREA_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'long_name'
       att_string = 'area of T cells'
       CALL nf_put_att_wrap(dst_ncid, dst_TAREA_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'coordinates'
       att_string = TRIM(dst_TLONG_name) // ' ' // TRIM(dst_TLAT_name)
       CALL nf_put_att_wrap(dst_ncid, dst_TAREA_varid, att_name, &
            LEN_TRIM(att_string), att_string)
    END IF

    IF (dst_needs_REGION_MASK) THEN
       dst_REGION_MASK_name = 'REGION_MASK'
       CALL nf_def_var_wrap(dst_ncid, dst_REGION_MASK_name, NF_INT, 2, &
            (/ dst_X_dimid, dst_Y_dimid /), dst_REGION_MASK_varid)

       att_name = 'units'
       att_string = 'Basin Index'
       CALL nf_put_att_wrap(dst_ncid, dst_REGION_MASK_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'long_name'
       att_string = 'basin index number (signed integers)'
       CALL nf_put_att_wrap(dst_ncid, dst_REGION_MASK_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'coordinates'
       att_string = TRIM(dst_TLONG_name) // ' ' // TRIM(dst_TLAT_name)
       CALL nf_put_att_wrap(dst_ncid, dst_REGION_MASK_varid, att_name, &
            LEN_TRIM(att_string), att_string)
    END IF

    IF (dst_needs_KMT) THEN
       dst_KMT_name = 'KMT'
       CALL nf_def_var_wrap(dst_ncid, dst_KMT_name, NF_INT, 2, &
            (/ dst_X_dimid, dst_Y_dimid /), dst_KMT_varid)

       att_name = 'units'
       att_string = 'unitless'
       CALL nf_put_att_wrap(dst_ncid, dst_KMT_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'long_name'
       att_string = 'k Index of Deepest Grid Cell on T Grid'
       CALL nf_put_att_wrap(dst_ncid, dst_KMT_varid, att_name, &
            LEN_TRIM(att_string), att_string)

       att_name = 'coordinates'
       att_string = TRIM(dst_TLONG_name) // ' ' // TRIM(dst_TLAT_name)
       CALL nf_put_att_wrap(dst_ncid, dst_KMT_varid, att_name, &
            LEN_TRIM(att_string), att_string)
    END IF

    !------------------------------------------------------------------------
    !   define dst_var
    !------------------------------------------------------------------------

    IF (dst_kmin == 0 .AND. timelen == 0) THEN
       CALL nf_def_var_wrap(dst_ncid, dst_var, NF_FLOAT, 2, &
            (/ dst_X_dimid, dst_Y_dimid /), dst_varid)
    ELSE IF (dst_kmin > 0 .AND. timelen == 0) THEN
       CALL nf_def_var_wrap(dst_ncid, dst_var, NF_FLOAT, 3, &
            (/ dst_X_dimid, dst_Y_dimid, dst_depth_dimid /), dst_varid)
    ELSE IF (dst_kmin == 0 .AND. timelen > 0) THEN
       CALL nf_def_var_wrap(dst_ncid, dst_var, NF_FLOAT, 3, &
            (/ dst_X_dimid, dst_Y_dimid, dst_time_dimid /), dst_varid)
    ELSE
       CALL nf_def_var_wrap(dst_ncid, dst_var, NF_FLOAT, 4, &
            (/ dst_X_dimid, dst_Y_dimid, dst_depth_dimid, dst_time_dimid /),&
            dst_varid)
    END IF

    !------------------------------------------------------------------------
    !   copy attributes for src_varid
    !------------------------------------------------------------------------

    CALL nf_inq_varnatts_wrap(src_ncid, src_varid, natts)
    DO attnum = 1,natts
       CALL nf_inq_attname_wrap(src_ncid, src_varid, attnum, att_name)
       CALL nf_copy_att_wrap(src_ncid, src_varid, att_name, dst_ncid, dst_varid)
    END DO

    att_name = 'coordinates'
    IF (dst_kmin == 0 .AND. timelen == 0) THEN
       att_string = TRIM(dst_TLONG_name) // ' ' // TRIM(dst_TLAT_name)
    ELSE IF (dst_kmin > 0 .AND. timelen == 0) THEN
       att_string = TRIM(dst_TLONG_name) // ' ' // TRIM(dst_TLAT_name) &
            // ' ' // TRIM(dst_depth_name)
    ELSE IF (dst_kmin == 0 .AND. timelen > 0) THEN
       att_string = TRIM(dst_TLONG_name) // ' ' // TRIM(dst_TLAT_name) &
            // ' ' // TRIM(dst_time_name)
    ELSE
       att_string = TRIM(dst_TLONG_name) // ' ' // TRIM(dst_TLAT_name) &
            // ' ' // TRIM(dst_depth_name) // ' ' // TRIM(dst_time_name)
    END IF
    CALL nf_put_att_wrap(dst_ncid, dst_varid, att_name, &
         LEN_TRIM(att_string), att_string)

    !------------------------------------------------------------------------
    !   add missing_value attribute if it is not already present
    !      use _FillValue value if it is present, otherwise use default_msv
    !------------------------------------------------------------------------

    CALL nf_get_att_wrap(dst_ncid, dst_varid, 'missing_value', msv, &
         ALLOW=NF_ENOTATT, stat_out=stat)
    IF (stat == NF_ENOTATT) THEN
       CALL nf_get_att_wrap(dst_ncid, dst_varid, '_FillValue', msv, &
            ALLOW=NF_ENOTATT, stat_out=stat)
       IF (stat /= NF_ENOTATT) THEN
          att_name = 'missing_value'
          CALL nf_put_att_wrap(dst_ncid, dst_varid, att_name, NF_FLOAT, 1, &
               msv)
       ELSE
          att_name = 'missing_value'
          CALL nf_put_att_wrap(dst_ncid, dst_varid, att_name, NF_FLOAT, 1, &
               default_msv)
          att_name = '_FillValue'
          CALL nf_put_att_wrap(dst_ncid, dst_varid, att_name, NF_FLOAT, 1, &
               default_msv)
       END IF
    END IF

  END SUBROUTINE def_dst_file

  !*****************************************************************************

  SUBROUTINE put_aux_vars

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    REAL(KIND=real_kind), DIMENSION(dst_imt) :: X
    REAL(KIND=real_kind), DIMENSION(dst_jmt) ::Y
    INTEGER(KIND=int_kind) :: varid, i, j

    !---------------------------------------------------------------------------

    IF (timelen > 0) THEN
       CALL nf_inq_varid_wrap(dst_ncid, dst_time_name, varid)
       CALL nf_put_var_wrap(dst_ncid, varid, time)
    END IF

    IF (dst_kmin > 0 .AND. dst_needs_depth) THEN
       CALL nf_inq_varid_wrap(dst_ncid, dst_depth_name, varid)
       CALL nf_put_var_wrap(dst_ncid, varid, dst_depth(dst_kmin:dst_kmax))
    END IF

    IF (dst_kmin > 0 .AND. dst_needs_depth_edges) THEN
       CALL nf_inq_varid_wrap(dst_ncid, dst_depth_edges_name, varid)
       CALL nf_put_var_wrap(dst_ncid, varid, &
            dst_depth_edges(dst_kmin:dst_kmax+1))
    END IF

    IF (dst_needs_X) THEN
       DO i = 1,dst_imt
          X(i) = REAL(i,real_kind)
       END DO
       CALL nf_inq_varid_wrap(dst_ncid, dst_X_name, varid)
       CALL nf_put_var_wrap(dst_ncid, varid, X)
    END IF

    IF (dst_needs_Y) THEN
       DO j = 1,dst_jmt
          Y(j) = REAL(j,real_kind)
       END DO
       CALL nf_inq_varid_wrap(dst_ncid, dst_Y_name, varid)
       CALL nf_put_var_wrap(dst_ncid, varid, Y)
    END IF

    IF (dst_needs_ULAT) THEN
       CALL nf_put_var_wrap(dst_ncid, dst_ULAT_varid, ULAT)
    END IF

    IF (dst_needs_ULONG) THEN
       CALL nf_put_var_wrap(dst_ncid, dst_ULONG_varid, ULONG)
    END IF

    IF (dst_needs_TLAT) THEN
       CALL nf_put_var_wrap(dst_ncid, dst_TLAT_varid, TLAT)
    END IF

    IF (dst_needs_TLONG) THEN
       CALL nf_put_var_wrap(dst_ncid, dst_TLONG_varid, TLONG)
    END IF

    IF (dst_needs_TAREA) THEN
       CALL nf_put_var_wrap(dst_ncid, dst_TAREA_varid, TAREA)
    END IF

    IF (dst_needs_REGION_MASK) THEN
       CALL nf_put_var_wrap(dst_ncid, dst_REGION_MASK_varid, REGION_MASK)
    END IF

    IF (dst_needs_KMT) THEN
       CALL nf_put_var_wrap(dst_ncid, dst_KMT_varid, dst_kmt)
    END IF

  END SUBROUTINE put_aux_vars

  !*****************************************************************************

  SUBROUTINE regrid_all_z_levs(l)

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

    INTEGER(KIND=int_kind), INTENT(IN) :: l ! time level

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    CHARACTER(LEN=*), PARAMETER :: sub_name = 'regrid_all_z_levs'
    INTEGER(KIND=int_kind) :: stat
    REAL(KIND=real_kind) :: msv
    REAL(KIND=real_kind), DIMENSION(:, :), ALLOCATABLE :: src_data
    REAL(KIND=real_kind), DIMENSION(:, :), ALLOCATABLE :: &
       src_data_above_on_dst, src_data_below_on_dst, dst_data, dst_data_prev
    INTEGER(KIND=int_kind) :: dst_lev, src_lev, i, j
    REAL(KIND=real_kind) :: dst_z, src_z, src_z_above, src_z_below, wz, &
       num, denom
    LOGICAL(KIND=log_kind) :: needs_fill

    !---------------------------------------------------------------------------
    !   allocate local allocatable vars
    !---------------------------------------------------------------------------

    ALLOCATE(src_data(src_imt, src_jmt))
    ALLOCATE(src_data_above_on_dst(dst_imt, dst_jmt), src_data_below_on_dst(dst_imt, dst_jmt), &
         dst_data(dst_imt, dst_jmt), dst_data_prev(dst_imt, dst_jmt))

    !---------------------------------------------------------------------------
    !   use unlikely value if variable has no missing_value attribute
    !---------------------------------------------------------------------------

    CALL nf_get_att_wrap(dst_ncid, dst_varid, 'missing_value', msv)

    !---------------------------------------------------------------------------
    !   handle case where there is no vertical axis, use surface mask
    !---------------------------------------------------------------------------

    IF (dst_kmin == 0) THEN
       CALL nf_get_vara_wrap(src_ncid, src_varid, (/ 1, 1, l /), &
            (/ src_imt, src_jmt, 1 /), src_data)
       CALL regrid_level(msv, src_data, dst_data, dst_kmt > 0)
       CALL fill_level(msv, dst_data, dst_kmt > 0, needs_fill)
       IF (needs_fill) CALL msg_write(sub_name, 'some ocean points not filled')
       CALL nf_put_vara_wrap(dst_ncid, dst_varid, (/ 1, 1, l /), &
            (/ dst_imt, dst_jmt, 1 /), dst_data)
       RETURN
    END IF

    !---------------------------------------------------------------------------
    !   handle case where src vertical axis has only 1 level
    !---------------------------------------------------------------------------

    IF (src_km == 1) THEN
       CALL nf_get_vara_wrap(src_ncid, src_varid, (/ 1, 1, 1, l /), &
            (/ src_imt, src_jmt, 1, 1 /), src_data)
       CALL regrid_level(msv, src_data, dst_data, dst_kmt >= dst_kmin)
       CALL fill_level(msv, dst_data, dst_kmt >= dst_kmin, needs_fill)
       IF (needs_fill) CALL msg_write(sub_name, 'some ocean points not filled')
       CALL nf_put_vara_wrap(dst_ncid, dst_varid, (/ 1, 1, 1, l /), &
            (/ dst_imt, dst_jmt, 1, 1 /), dst_data)
       RETURN
    END IF

    !---------------------------------------------------------------------------
    !   handle the general case w/ vertical interpolation
    !---------------------------------------------------------------------------

    DO dst_lev = dst_kmin,dst_kmax
       CALL msg_write(sub_name, 'regridding k = ', dst_lev)
       dst_z = dst_depth(dst_lev)

       !------------------------------------------------------------------------
       !   if a level exists whose depth is nearly identical to dst_z,
       !   use that level with no vertical interpolation
       !------------------------------------------------------------------------

       src_lev = MINLOC(ABS(src_depth-dst_z), DIM=1)
       src_z = src_depth(src_lev)

       IF (equal(src_z, dst_z, RELTOL=1.0e-4)) THEN
          CALL nf_get_vara_wrap(src_ncid, src_varid, (/ 1, 1, src_lev, l /), &
               (/ src_imt, src_jmt, 1, 1 /), src_data)
          CALL regrid_level(msv, src_data, src_data_above_on_dst, &
               dst_kmt >= dst_lev)
          CALL fill_level(msv, src_data_above_on_dst, dst_kmt >= dst_lev, &
               needs_fill)
          src_data_below_on_dst = src_data_above_on_dst
          wz = c0
       ELSE
          src_lev = MAXLOC(src_depth, DIM=1, MASK=(src_depth<=dst_z))
          src_z_above = src_depth(src_lev)

          CALL nf_get_vara_wrap(src_ncid, src_varid, (/ 1, 1, src_lev, l /), &
               (/ src_imt, src_jmt, 1, 1 /), src_data)
          CALL regrid_level(msv, src_data, src_data_above_on_dst, &
               dst_kmt >= dst_lev)
          CALL fill_level(msv, src_data_above_on_dst, dst_kmt >= dst_lev, &
               needs_fill)

          src_lev = MINLOC(src_depth, DIM=1, MASK=(src_depth>dst_z))
          src_z_below = src_depth(src_lev)

          CALL nf_get_vara_wrap(src_ncid, src_varid, (/ 1, 1, src_lev, l /), &
               (/ src_imt, src_jmt, 1, 1 /), src_data)
          CALL regrid_level(msv, src_data, src_data_below_on_dst, &
               dst_kmt >= dst_lev)
          CALL fill_level(msv, src_data_below_on_dst, dst_kmt >= dst_lev, &
               needs_fill)

          wz = (dst_z - src_z_above) / (src_z_below - src_z_above)
       END IF

       DO j = 1,dst_jmt
          DO i = 1,dst_imt
             IF (dst_kmt(i,j) >= dst_lev) THEN
                num = 0.0
                denom = 0.0

                IF (src_data_above_on_dst(i,j) /= msv) THEN
                   num = num + (1.0 - wz) * src_data_above_on_dst(i,j)
                   denom = denom + (1.0 - wz)
                END IF

                IF (src_data_below_on_dst(i,j) /= msv) THEN
                   num = num + wz * src_data_below_on_dst(i,j)
                   denom = denom + wz
                END IF

                IF (denom > 0) THEN
                   dst_data(i,j) = num / denom
                ELSE
                   dst_data(i,j) = msv
                END IF
             ELSE
                dst_data(i,j) = msv
             END IF
          END DO
       END DO

       CALL fill_level(msv, dst_data, dst_kmt >= dst_lev, needs_fill)

       !------------------------------------------------------------------------
       !   if point are still unfilled, attempt to copy from previous level
       !------------------------------------------------------------------------

       IF (needs_fill) THEN
          If (dst_lev > dst_kmin) THEN
             CALL msg_write(sub_name, 'filling points from previous level ', &
                  TRIM(dst_var), ' dst_lev=', dst_lev)
             needs_fill = .FALSE.
             DO j = 1,dst_jmt
                DO i = 1,dst_imt
                   IF (dst_kmt(i,j) >= dst_lev .AND. dst_data(i,j) == msv) THEN
                      IF (dst_data_prev(i,j) == msv) THEN
                         needs_fill = .TRUE.
                      ELSE
                         dst_data(i,j) = dst_data_prev(i,j)
                      END IF
                   END IF
                END DO
             END DO
          END IF
          IF (needs_fill) CALL msg_write(sub_name, &
               'some ocean points not filled ', TRIM(dst_var), &
               ' dst_lev=', dst_lev)
       END IF

       CALL nf_put_vara_wrap(dst_ncid, dst_varid, &
            (/ 1, 1, dst_lev-dst_kmin+1, l /), (/ dst_imt, dst_jmt, 1, 1 /), &
            dst_data)

       dst_data_prev = dst_data
    END DO

    !---------------------------------------------------------------------------
    !   deallocate local allocatable vars
    !---------------------------------------------------------------------------

    DEALLOCATE(src_data)
    DEALLOCATE(src_data_above_on_dst, src_data_below_on_dst, &
         dst_data, dst_data_prev)

  END SUBROUTINE regrid_all_z_levs

  !*****************************************************************************

  SUBROUTINE regrid_level(msv, src_data, dst_data, dst_mask)

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

    REAL(KIND=real_kind), INTENT(IN) :: msv
    REAL(KIND=real_kind), DIMENSION(src_imt, src_jmt), INTENT(IN) :: src_data
    REAL(KIND=real_kind), DIMENSION(dst_imt, dst_jmt), INTENT(OUT) :: dst_data
    LOGICAL(KIND=log_kind), DIMENSION(dst_imt, dst_jmt), INTENT(IN) :: dst_mask

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    INTEGER(KIND=int_kind) :: src_i, src_ip1, src_j, src_jp1, dst_i, dst_j
    REAL(KIND=real_kind) :: dst_lat, dst_lon, src_dlon, src_dlat, wx, wy
    REAL(KIND=real_kind), DIMENSION(2,2) :: weight, vals
    LOGICAL(KIND=log_kind), DIMENSION(2,2) :: interp_mask

    DO dst_j = 1,dst_jmt
       DO dst_i = 1,dst_imt
          IF (.NOT. dst_mask(dst_i, dst_j)) THEN
             dst_data(dst_i, dst_j) = msv
             CYCLE
          END IF

          dst_lat = TLAT(dst_i, dst_j)
          dst_lon = TLONG(dst_i, dst_j)
          IF (dst_lon > src_lon(src_imt)) dst_lon = dst_lon - 360.0
          IF (dst_lon < src_lon(1)) dst_lon = dst_lon + 360.0

          src_i = find_index(dst_lon, src_lon)
          IF (src_i < src_imt) THEN
             src_ip1 = src_i + 1
             src_dlon = src_lon(src_ip1) - src_lon(src_i)
          ELSE
             src_ip1 = 1
             src_dlon = 360.0 + src_lon(src_ip1) - src_lon(src_i)
          END IF

          wx = (dst_lon - src_lon(src_i)) / src_dlon

          src_j = find_index(dst_lat, src_lat)
          IF (src_j < src_jmt) THEN
             src_jp1 = src_j + 1
             src_dlat = src_lat(src_jp1) - src_lat(src_j)
             wy = (dst_lat - src_lat(src_j)) / src_dlat
          ELSE
             src_jp1 = src_j
             wy = 0.0
          END IF

          weight = RESHAPE( (/ (1.0-wx)*(1.0-wy), wx*(1.0-wy), &
               (1.0-wx)*wy, wx*wy /), (/ 2, 2 /) )

          vals = src_data( (/ src_i, src_ip1 /), (/ src_j, src_jp1 /) )

          interp_mask = (vals /= msv) .AND. (weight > 0.0)

          !---------------------------------------------------------------------
          !   no valid data to interpolate from
          !---------------------------------------------------------------------

          IF (COUNT(interp_mask) == 0) THEN
             dst_data(dst_i, dst_j) = msv
             CYCLE
          END IF

          !---------------------------------------------------------------------
          !   do not interpolate if valid data is only at opposite corners
          !   copy value from appropriate quadrant
          !---------------------------------------------------------------------

          IF ((COUNT(interp_mask) == 2) .AND. &
               (interp_mask(1,2) .EQV. interp_mask(2,1))) THEN

             IF (wx <= 0.5) THEN
                IF (wy <= 0.5) THEN
                   dst_data(dst_i, dst_j) = src_data(src_i,src_j)
                ELSE
                   dst_data(dst_i, dst_j) = src_data(src_i,src_jp1)
                END IF
             ELSE
                IF (wy <= 0.5) THEN
                   dst_data(dst_i, dst_j) = src_data(src_ip1,src_j)
                ELSE
                   dst_data(dst_i, dst_j) = src_data(src_ip1,src_jp1)
                END IF
             END IF

             CYCLE
          END IF

          !---------------------------------------------------------------------
          !   compute masked weighted average
          !---------------------------------------------------------------------

          dst_data(dst_i, dst_j) = SUM(weight * vals, MASK=interp_mask) / &
               SUM(weight, MASK=interp_mask)

       END DO
    END DO

  END SUBROUTINE regrid_level

  !*****************************************************************************

  SUBROUTINE fill_level(msv, data, mask, needs_fill)

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

    REAL(KIND=real_kind), INTENT(IN) :: msv
    REAL(KIND=real_kind), DIMENSION(:,:), INTENT(INOUT) :: data
    LOGICAL(KIND=log_kind), DIMENSION(:,:), INTENT(IN) :: mask
    LOGICAL(KIND=log_kind), INTENT(OUT) :: needs_fill

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    INTEGER(KIND=int_kind) :: imt, jmt, i, j, ip1, im1, jp1, jm1
    REAL(KIND=real_kind), DIMENSION(:, :), ALLOCATABLE :: work
    LOGICAL(KIND=log_kind) :: filled_a_cell

    REAL(KIND=real_kind), DIMENSION(4) :: vals
    LOGICAL(KIND=log_kind), DIMENSION(4) :: interp_mask

    imt = SIZE(data,1)
    jmt = SIZE(data,2)

    !---------------------------------------------------------------------------
    !   allocate local allocatable vars
    !---------------------------------------------------------------------------

    ALLOCATE(work(imt, jmt))

    filled_a_cell = .TRUE.

    DO WHILE (filled_a_cell)
       filled_a_cell = .FALSE.
       needs_fill = .FALSE.
       work = data
       DO j = 1,jmt

          jm1 = MAX(j - 1, 1)
          jp1 = MIN(j + 1, jmt)

          DO i = 1,imt

             im1 = i - 1
             IF (i == 1) im1 = imt
             ip1 = i + 1
             IF (i == imt) ip1 = 1

             IF (mask(i,j) .AND. data(i,j) == msv) THEN

                vals = (/ work(ip1,j), work(i,jp1), work(im1,j), work(i,jm1) /)

                !---------------------------------------------------------------
                !   fill only from unmasked points w/ valid data
                !---------------------------------------------------------------

                interp_mask = &
                     (/ mask(ip1,j), mask(i,jp1), mask(im1,j), mask(i,jm1) /)
                interp_mask = interp_mask .AND. (vals /= msv)

                IF (ANY(interp_mask)) THEN
                   filled_a_cell = .TRUE.
                   data(i,j) = SUM(vals, MASK=interp_mask) / COUNT(interp_mask)
                ELSE
                   needs_fill = .TRUE.
                END IF

             END IF
          END DO
       END DO
    END DO

    !---------------------------------------------------------------------------
    !   deallocate local allocatable vars
    !---------------------------------------------------------------------------

    DEALLOCATE(work)

  END SUBROUTINE fill_level

  !*****************************************************************************

  INTEGER FUNCTION find_index(x, sorted_array)

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

    REAL(KIND=real_kind) :: x
    REAL(KIND=real_kind), DIMENSION(:) :: sorted_array

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

    INTEGER(KIND=int_kind) :: len, i

    len = SIZE(sorted_array)

    IF (x < sorted_array(1)) THEN
       find_index = 0
       RETURN
    END IF

    DO i = 1,len-1
       IF (x < sorted_array(i+1)) THEN
          find_index = i
          RETURN
       END IF
    END DO

    find_index = len

  END FUNCTION find_index

  !*****************************************************************************

  SUBROUTINE free_vars

    IF (ALLOCATED(src_lon))         DEALLOCATE(src_lon)
    IF (ALLOCATED(src_lat))         DEALLOCATE(src_lat)
    IF (ALLOCATED(src_depth))       DEALLOCATE(src_depth)
    IF (ALLOCATED(src_depth_edges)) DEALLOCATE(src_depth_edges)
    IF (ALLOCATED(dst_depth))       DEALLOCATE(dst_depth)
    IF (ALLOCATED(dst_depth_edges)) DEALLOCATE(dst_depth_edges)
    IF (ALLOCATED(dst_kmt))         DEALLOCATE(dst_kmt)
    IF (ALLOCATED(ULAT))            DEALLOCATE(ULAT)
    IF (ALLOCATED(ULONG))           DEALLOCATE(ULONG)
    IF (ALLOCATED(TLAT))            DEALLOCATE(TLAT)
    IF (ALLOCATED(TLONG))           DEALLOCATE(TLONG)
    IF (ALLOCATED(TAREA))           DEALLOCATE(TAREA)
    IF (ALLOCATED(REGION_MASK))     DEALLOCATE(REGION_MASK)
    IF (ALLOCATED(time))            DEALLOCATE(time)

  END SUBROUTINE free_vars

  !*****************************************************************************

END MODULE regrid
