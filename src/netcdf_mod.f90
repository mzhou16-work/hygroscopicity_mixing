!$id: netcdf_mod.f90, v1.0 10/21/2015, unlvrtm v1.4x, xxu
MODULE netCDF_MOD
!
 IMPLICIT NONE
!
!Make everthing PRIVATE,
 PRIVATE 
!
!except:

! i/o create, open/close, and defines 
 PUBLIC :: netCDF_Create
 PUBLIC :: netCDF_Close
 PUBLIC :: netCDF_Open_Write
 PUBLIC :: netCDF_Open_Read
 PUBLIC :: netCDF_Begin_Def
 PUBLIC :: netCDF_End_Def
 PUBLIC :: netCDF_Def_Dim
 PUBLIC :: netCDF_Def_Var

! checks
 PUBLIC :: netCDF_uDim_Exist
 PUBLIC :: netCDF_Dim_Exist
 PUBLIC :: netCDF_Var_Exist
 PUBLIC :: netCDF_Attr_Exist 

! get dims
 PUBLIC :: netCDF_Get_Dim
 PUBLIC :: netCDF_Get_uDim

! others
 PUBLIC :: netCDF_Def_Attr
 INTERFACE netCDF_Def_Attr
    MODULE PROCEDURE ncDef_0D_Attr_c
    MODULE PROCEDURE ncDef_0D_Attr_i
    MODULE PROCEDURE ncDef_0D_Attr_r4
    MODULE PROCEDURE ncDef_0D_Attr_r8
    MODULE PROCEDURE ncDef_1D_Attr_i
    MODULE PROCEDURE ncDef_1D_Attr_r4
    MODULE PROCEDURE ncDef_1D_Attr_r8
 END INTERFACE netCDF_Def_Attr

 PUBLIC :: netCDF_Write_Var
 INTERFACE netCDF_Write_Var
    MODULE PROCEDURE ncWrite_0D_Var_Text
    MODULE PROCEDURE ncWrite_0D_Var_i
    MODULE PROCEDURE ncWrite_0D_Var_r4
    MODULE PROCEDURE ncWrite_0D_Var_r8
    MODULE PROCEDURE ncWrite_0D_Var_c
    MODULE PROCEDURE ncWrite_1D_Var_i
    MODULE PROCEDURE ncWrite_1D_Var_r4
    MODULE PROCEDURE ncWrite_1D_Var_r8
    MODULE PROCEDURE ncWrite_1D_Var_c
    MODULE PROCEDURE ncWrite_2D_Var_i
    MODULE PROCEDURE ncWrite_2D_Var_r4
    MODULE PROCEDURE ncWrite_2D_Var_r8
    MODULE PROCEDURE ncWrite_3D_Var_i
    MODULE PROCEDURE ncWrite_3D_Var_r4
    MODULE PROCEDURE ncWrite_3D_Var_r8
    MODULE PROCEDURE ncWrite_4D_Var_i
    MODULE PROCEDURE ncWrite_4D_Var_r4
    MODULE PROCEDURE ncWrite_4D_Var_r8
    MODULE PROCEDURE ncWrite_5D_Var_r4
    MODULE PROCEDURE ncWrite_5D_Var_r8
    MODULE PROCEDURE ncWrite_6D_Var_r4
    MODULE PROCEDURE ncWrite_6D_Var_r8
 END INTERFACE netCDF_Write_Var

 PUBLIC :: netCDF_Get_Attr
 INTERFACE netCDF_Get_Attr
    MODULE PROCEDURE ncGet_0D_Attr_c
    MODULE PROCEDURE ncGet_0D_Attr_i
    MODULE PROCEDURE ncGet_0D_Attr_r4
    MODULE PROCEDURE ncGet_0D_Attr_r8
    MODULE PROCEDURE ncGet_1D_Attr_i
    MODULE PROCEDURE ncGet_1D_Attr_r4
    MODULE PROCEDURE ncGet_1D_Attr_r8
 END INTERFACE netCDF_Get_Attr

 PUBLIC :: netCDF_Read_Var
 INTERFACE netCDF_Read_Var
    MODULE PROCEDURE ncRead_0D_Var_i
    MODULE PROCEDURE ncRead_0D_Var_r4
    MODULE PROCEDURE ncRead_0D_Var_r8
    MODULE PROCEDURE ncRead_0D_Var_c
    MODULE PROCEDURE ncRead_1D_Var_i
    MODULE PROCEDURE ncRead_1D_Var_r4
    MODULE PROCEDURE ncRead_1D_Var_r8
    MODULE PROCEDURE ncRead_1D_Var_c
    MODULE PROCEDURE ncRead_2D_Var_i
    MODULE PROCEDURE ncRead_2D_Var_r4
    MODULE PROCEDURE ncRead_2D_Var_r8
    MODULE PROCEDURE ncRead_3D_Var_i
    MODULE PROCEDURE ncRead_3D_Var_r4
    MODULE PROCEDURE ncRead_3D_Var_r8
    MODULE PROCEDURE ncRead_4D_Var_i
    MODULE PROCEDURE ncRead_4D_Var_r4
    MODULE PROCEDURE ncRead_4D_Var_r8
    MODULE PROCEDURE ncRead_5D_Var_r4
    MODULE PROCEDURE ncRead_5D_Var_r8
    MODULE PROCEDURE ncRead_6D_Var_r4
    MODULE PROCEDURE ncRead_6D_Var_r8
 END INTERFACE netCDF_Read_Var

! All subroutines follow CONTAINS statement:
CONTAINS
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_Create (ncid, filename, Write_NC4 )
! 
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments
 INTEGER,           INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: filename
 LOGICAL, OPTIONAL, INTENT(IN) :: Write_NC4
!
!DESCRIPTION: 
!  Creates a netCDF file for writing with error checking 
!
!REMARKS: 
! If the netCDF4 library is used, then the NF_CLOBBER flag will write
! a classic (i.e. netCDF3) file.  Use NF_64_BIT_OFFSET to create a
! netCDF 4 file. (bmy, 11/7/11)
!
!REVISION HISTROY:
! [10/21/2015, xxu]
!   Intial code was adapted from GEOS-Chem NcdfUtil codes.

!Local Variables
 LOGICAL                       :: Tmp_NC4
 CHARACTER (LEN=128)           :: Msg

! Save the value of the optional WRITE_NC4 variable in
! a local shadow variable (bmy, 11/7/11) 
 IF ( PRESENT( Write_NC4 ) ) THEN
    Tmp_NC4 = Write_NC4
 ELSE
    Tmp_NC4 = .FALSE.
 ENDIF

! Now open a netCDF file
 IF ( Tmp_NC4 ) THEN 
    ! in netCDF4 format
    Msg = 'netCDF_Create: cannot create netCDF4: ' // TRIM( FILENAME )
    CALL Check ( Nf_Create (filename, NF_64BIT_OFFSET, ncid), 0, Msg )
 ELSE
    ! in netCDF3 format
    Msg = 'netCDF_Create: cannot create netCDF3: ' // TRIM( FILENAME )
    CALL Check ( Nf_Create (filename, NF_CLOBBER, ncid), 0, Msg )
 ENDIF

!
 RETURN
!
 END SUBROUTINE netCDF_Create
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_Open_Write ( ncid, filename )
! 
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments
 INTEGER,           INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: filename

! Local variable
 CHARACTER (LEN=128)           :: Msg

! Error message
 Msg = 'netCDF_Open_Write: cannot open:' // TRIM( FILENAME )
 
 CALL Check( Nf_Open(filename, NF_WRITE, ncid), 1, Msg )

 RETURN
!
 END SUBROUTINE netCDF_Open_Write
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_Open_Read ( ncid, filename )
! 
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments
 INTEGER,           INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: filename

! Local variable
 CHARACTER (LEN=128)           :: Msg

! Error message
 Msg = 'netCDF_Open_Read: cannot open:' // TRIM( FILENAME )

 CALL Check( Nf_Open(filename, NF_NOWRITE, ncid), 1, Msg )

 RETURN
!
 END SUBROUTINE netCDF_Open_Read
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_Close ( ncid )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments
 INTEGER, INTENT(IN) :: ncid
!
!DESCRIPTION:
! Closes a netCDF file (with file id ncid)
!
 CALL Check( Nf_Close(ncid), 99, 'netCDF_Close: connot close file' )
!
 RETURN
!
 END SUBROUTINE netCDF_Close
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_Begin_Def ( ncid )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments
 INTEGER, INTENT(IN) :: ncid
!
!DESCRIPTION:
! Opens (or re-opens) netCDF define mode, where variables
! and attributes can be defined.

 CALL Check ( Nf_Redef (ncid), 10, 'netCDF_Begin_Def' )

!
 RETURN
!
 END SUBROUTINE netCDF_Begin_Def
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_End_Def ( ncid )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments
 INTEGER, INTENT(IN) :: ncid
!
!DESCRIPTION:
! Ends definitions of variables and their attributes.

 CALL Check ( Nf_Enddef (ncid), 19, 'netCDF_End_Def' )

!
 RETURN
!
 END SUBROUTINE netCDF_End_Def
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_Def_Dim ( ncid, name, len, dimid )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments
 INTEGER,           INTENT(IN)  :: ncid
 CHARACTER (LEN=*), INTENT(IN)  :: name
 INTEGER,           INTENT(IN)  :: len
 INTEGER,           INTENT(OUT) :: dimid
!
!DESCRIPTION:
! Defines dimension.
!   ncid  : netCDF file id
!   name  : dimension name
!   len   : dimension number
!   dimid : dimension id
!
!Local Variables
 CHARACTER (LEN=128)            :: Msg

! Error message
 Msg = 'netCDF_Def_Dim: cannot define dimension: '// TRIM( name )

! Call netcdf function
 CALL Check( Nf_Def_Dim (ncid, name, len, dimid), 11,  Msg)

!
 RETURN
!
 END SUBROUTINE netCDF_Def_Dim
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_Def_Var ( ncid, name, type, ndims, dims, varid )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments
 INTEGER,           INTENT(IN)  :: ncid
 CHARACTER (LEN=*), INTENT(IN)  :: name
 INTEGER,           INTENT(IN)  :: type
 INTEGER,           INTENT(IN)  :: ndims
 INTEGER,           INTENT(IN)  :: dims(ndims)
 INTEGER,           INTENT(IN)  :: varid
!
!DESCRIPTION:
! Defines a netCDF variable.
!
!Local variables
 CHARACTER (LEN=128)            :: Msg

! Error message
 Msg = 'netCDF_Def_Var: cannot define variable: ' // TRIM( name )
 
 CALL Check( Nf_Def_Var( ncid, name, type, ndims, dims, varid ), 12, Msg )
!
 RETURN
!
 END SUBROUTINE netCDF_Def_Var
!
!------------------------------------------------------------------------------
! Subroutines for a generic interface: netCDF_Def_Var_Attr
!------------------------------------------------------------------------------
! 
 SUBROUTINE ncDef_0D_Attr_c ( ncid, varid, attr_val, attr_name )
!
 IMPLICIT NONE
!
!USE:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN)  :: ncid
 INTEGER          , INTENT(IN)  :: varid
 CHARACTER (LEN=*), INTENT(IN)  :: attr_name
 CHARACTER (LEN=*), INTENT(IN)  :: attr_val
!
!Local variables
 INTEGER                        :: attr_len
 INTEGER                        :: vid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncDef_0D_Attr_c: ' // TRIM( attr_name )

! Determine if it is for Global or Variable attribute
 vid = varid
 IF ( vid == -999 ) vid = NF_GLOBAL

! Define the arrtribute
 attr_len = LEN( attr_val )
 CALL Check( Nf_Put_Att_Text(ncid, vid, attr_name, attr_len, attr_val), &
             21, Msg )

 RETURN
!
 END SUBROUTINE ncDef_0D_Attr_c
!
!-----------------------------------------------------------------------------
! 
 SUBROUTINE ncDef_0D_Attr_i ( ncid, varid, attr_val, attr_name )
!
 IMPLICIT NONE
!
!USE:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN)  :: ncid
 INTEGER          , INTENT(IN)  :: varid
 CHARACTER (LEN=*), INTENT(IN)  :: attr_name
 INTEGER          , INTENT(IN)  :: attr_val
!
!Local variables
 INTEGER                        :: attr_len
 INTEGER                        :: vid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncDef_0D_Attr_i: ' // TRIM( attr_name )

! Determine if it is for Global or Variable attribute
 vid = varid
 IF ( vid == -999 ) vid = NF_GLOBAL

! Define the arrtribute
 attr_len = 1
 CALL Check( Nf_Put_Att_Real(ncid, vid, attr_name, NF_INT, &
             attr_len, attr_val), 21, Msg )

 RETURN
!
 END SUBROUTINE ncDef_0D_Attr_i
!
!-----------------------------------------------------------------------------
! 
 SUBROUTINE ncDef_0D_Attr_r4 ( ncid, varid, attr_val, attr_name )
!
 IMPLICIT NONE
!
!USE:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN)  :: ncid
 INTEGER          , INTENT(IN)  :: varid
 CHARACTER (LEN=*), INTENT(IN)  :: attr_name
 REAL*4           , INTENT(IN)  :: attr_val
!
!Local variables
 INTEGER                        :: attr_len
 INTEGER                        :: vid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncDef_0D_Attr_r4: ' // TRIM( attr_name )

! Determine if it is for Global or Variable attribute
 vid = varid
 IF ( vid == -999 ) vid = NF_GLOBAL

! Define the arrtribute
 attr_len = 1
 CALL Check( Nf_Put_Att_Real(ncid, vid, attr_name, NF_FLOAT, &
             attr_len, attr_val), 21, Msg )

 RETURN
!
 END SUBROUTINE ncDef_0D_Attr_r4
!
!-----------------------------------------------------------------------------
! 
 SUBROUTINE ncDef_0D_Attr_r8 ( ncid, varid, attr_val, attr_name )
!
 IMPLICIT NONE
!
!USE:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN)  :: ncid
 INTEGER          , INTENT(IN)  :: varid
 CHARACTER (LEN=*), INTENT(IN)  :: attr_name
 REAL*8           , INTENT(IN)  :: attr_val
!
!Local variables
 INTEGER                        :: attr_len
 INTEGER                        :: vid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncDef_0D_Attr_r8: ' // TRIM( attr_name )

! Determine if it is for Global or Variable attribute
 vid = varid
 IF ( vid == -999 ) vid = NF_GLOBAL

! Define the arrtribute
 attr_len = 1
 CALL Check( Nf_Put_Att_Double(ncid, vid, attr_name, NF_DOUBLE, &
             attr_len, attr_val), 21, Msg )

 RETURN
!
 END SUBROUTINE ncDef_0D_Attr_r8
!
!-----------------------------------------------------------------------------
!
 SUBROUTINE ncDef_1D_Attr_i ( ncid, varid, attr_val, attr_name )
!
 IMPLICIT NONE
!
!USE:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN)  :: ncid
 INTEGER          , INTENT(IN)  :: varid
 CHARACTER (LEN=*), INTENT(IN)  :: attr_name
 INTEGER          , INTENT(IN)  :: attr_val(:)

! DESCRIPTION: 
!Defines a netCDF variable attribute of type: CHARACTER.
!
!Local variables
 INTEGER                        :: attr_len
 INTEGER                        :: vid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncDef_1D_Attr_i: ' // TRIM( attr_name )

! Determine if it is for Global or Variable attribute
 vid = varid
 IF ( vid == -999 ) vid = NF_GLOBAL

! Define the arrtribute
 attr_len = SIZE( attr_val )
 CALL Check( Nf_Put_Att_Real( ncid, vid, attr_name, NF_INT, &
             attr_len, attr_val ), 22, Msg )

 RETURN
!
 END SUBROUTINE ncDef_1D_Attr_i
!
!-----------------------------------------------------------------------------
!
 SUBROUTINE ncDef_1D_Attr_r4 ( ncid, varid, attr_val, attr_name )
!
 IMPLICIT NONE
!
!USE:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN)  :: ncid
 INTEGER          , INTENT(IN)  :: varid
 CHARACTER (LEN=*), INTENT(IN)  :: attr_name
 REAL*4           , INTENT(IN)  :: attr_val(:)

! DESCRIPTION: 
!Defines a netCDF variable attribute of type: CHARACTER.
!
!Local variables
 INTEGER                        :: attr_len
 INTEGER                        :: vid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncDef_1D_Attr_r4: ' // TRIM( attr_name )

! Determine if it is for Global or Variable attribute
 vid = varid
 IF ( vid == -999 ) vid = NF_GLOBAL

! Define the arrtribute
 attr_len = SIZE( attr_val )
 CALL Check( Nf_Put_Att_Real( ncid, vid, attr_name, NF_FLOAT, &
             attr_len, attr_val ), 22, Msg )
 
 RETURN
!
 END SUBROUTINE ncDef_1D_Attr_r4
!
!-----------------------------------------------------------------------------
!
 SUBROUTINE ncDef_1D_Attr_r8 ( ncid, varid, attr_val, attr_name )
!
 IMPLICIT NONE
!
!USE:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN)  :: ncid
 INTEGER          , INTENT(IN)  :: varid
 CHARACTER (LEN=*), INTENT(IN)  :: attr_name
 REAL*8           , INTENT(IN)  :: attr_val(:)

! DESCRIPTION: 
!Defines a netCDF variable attribute of type: CHARACTER.
!
!Local variables
 INTEGER                        :: attr_len
 INTEGER                        :: vid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncDef_1D_Attr_r8: ' // TRIM( attr_name )

! Determine if it is for Global or Variable attribute
 vid = varid
 IF ( vid == -999 ) vid = NF_GLOBAL

! Define the arrtribute
 attr_len = SIZE( attr_val )
 CALL Check( Nf_Put_Att_Double( ncid, vid, attr_name, NF_DOUBLE, &
             attr_len, attr_val ), 22, Msg )

 RETURN
!
 END SUBROUTINE ncDef_1D_Attr_r8 
!
!------------------------------------------------------------------------------
! End Subroutines for a generic interface: netCDF_Def_Var_Attr
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
! Subroutines for a generic interface: netCDF_Write_Var
!------------------------------------------------------------------------------
!
 SUBROUTINE ncWrite_0D_Var_Text ( ncid, var, varname )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 CHARACTER (LEN=*), INTENT(IN) :: var
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_0D_Var_Text: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Var_Text( ncid, varid, var ), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_0D_Var_Text
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncWrite_0D_Var_i ( ncid, var, varname )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: var
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_0D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Var_Int( ncid, varid, var ), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_0D_Var_i
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncWrite_0D_Var_r4 ( ncid, var, varname )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 REAL*4           , INTENT(IN) :: var
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_0D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Var_Real( ncid, varid, var ), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_0D_Var_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncWrite_0D_Var_r8 ( ncid, var, varname )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 REAL*8           , INTENT(IN) :: var
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_0D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Var_Double( ncid, varid, var ), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_0D_Var_r8
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_0D_Var_c ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start
 INTEGER          , INTENT(IN) :: count
 CHARACTER (LEN=*), INTENT(IN) :: var
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_1D_Var_c: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Text(ncid, varid, start, count, var), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_0D_Var_c
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_1D_Var_i ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start
 INTEGER          , INTENT(IN) :: count
 INTEGER          , INTENT(IN) :: var(count)
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_1D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Int(ncid, varid, start, count, var), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_1D_Var_i
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_1D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start
 INTEGER          , INTENT(IN) :: count
 REAL*4           , INTENT(IN) :: var(count)
!
!Local variables    
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_1D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Real(ncid, varid, start, count, var), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_1D_Var_r4
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_1D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE               
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start
 INTEGER          , INTENT(IN) :: count
 REAL*8           , INTENT(IN) :: var(count)
!
!Local variables    
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_1D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Double(ncid, varid, start, count, var), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_1D_Var_r8
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_1D_Var_c ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(2)
 INTEGER          , INTENT(IN) :: count(2)
 CHARACTER (LEN=*), INTENT(IN) :: var(count(2))
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_2D_Var_c: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Text(ncid, varid, start, count, var), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_1D_Var_c
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_2D_Var_i ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(2)
 INTEGER          , INTENT(IN) :: count(2)
 INTEGER          , INTENT(IN) :: var(count(1),count(2))
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_2D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Int(ncid, varid, start, count, var), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_2D_Var_i
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_2D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE               
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(2)
 INTEGER          , INTENT(IN) :: count(2)
 REAL*4           , INTENT(IN) :: var(count(1),count(2))
!
!Local variables  
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_2D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Real(ncid, varid, start, count, var), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_2D_Var_r4
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_2D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE               
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(2)
 INTEGER          , INTENT(IN) :: count(2)
 REAL*8           , INTENT(IN) :: var(count(1),count(2))
!
!Local variables  
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_2D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Double(ncid, varid, start, count, var), 31, Msg )

 RETURN
!
 END SUBROUTINE ncWrite_2D_Var_r8
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_3D_Var_i ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(3)
 INTEGER          , INTENT(IN) :: count(3)
 INTEGER          , INTENT(IN) :: var(count(1),count(2),count(3))
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_3D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Int(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_3D_Var_i
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_3D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(3)
 INTEGER          , INTENT(IN) :: count(3)
 REAL*4           , INTENT(IN) :: var(count(1),count(2),count(3))
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_3D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Real(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_3D_Var_r4
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_3D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE           
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(3)
 INTEGER          , INTENT(IN) :: count(3)
 REAL*8           , INTENT(IN) :: var(count(1),count(2),count(3))
!
!Local variables  
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_3D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Double(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_3D_Var_r8

!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_4D_Var_i ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(4)
 INTEGER          , INTENT(IN) :: count(4)
 INTEGER          , INTENT(IN) :: var(count(1),count(2),count(3),count(4))
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_4D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Int(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_4D_Var_i
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_4D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(4)
 INTEGER          , INTENT(IN) :: count(4)
 REAL*4           , INTENT(IN) :: var(count(1),count(2),count(3),count(4))
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_4D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Real(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_4D_Var_r4
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_4D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(4)
 INTEGER          , INTENT(IN) :: count(4)
 REAL*8           , INTENT(IN) :: var(count(1),count(2),count(3),count(4))
!
!Local variables    
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_4D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Double(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_4D_Var_r8
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_5D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(5)
 INTEGER          , INTENT(IN) :: count(5)
 REAL*4           , INTENT(IN) :: var &
                    (count(1),count(2),count(3),count(4),count(5))
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_5D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Real(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_5D_Var_r4
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_5D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(5)
 INTEGER          , INTENT(IN) :: count(5)
 REAL*8           , INTENT(IN) :: var &
                    (count(1),count(2),count(3),count(4),count(5))
!
!Local variables    
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_5D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Double(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_5D_Var_r8
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_6D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(6)
 INTEGER          , INTENT(IN) :: count(6)
 REAL*4           , INTENT(IN) :: var &
                    (count(1),count(2),count(3),count(4),count(5),count(6))
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_6D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Real(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_6D_Var_r4
!
!----------------------------------------------------------------------
!
 SUBROUTINE ncWrite_6D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN) :: ncid
 CHARACTER (LEN=*), INTENT(IN) :: varname
 INTEGER          , INTENT(IN) :: start(6)
 INTEGER          , INTENT(IN) :: count(6)
 REAL*8           , INTENT(IN) :: var &
                    (count(1),count(2),count(3),count(4),count(5),count(6))
!
!Local variables    
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncWrite_6D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 30, Msg )

! Write the variable into the file
 CALL Check( Nf_Put_Vara_Double(ncid, varid, start, count, var), 31, Msg )
!
 RETURN
 END SUBROUTINE ncWrite_6D_Var_r8
!
!------------------------------------------------------------------------------
! End Subroutines for a generic interface: netCDF_Write_Var
!------------------------------------------------------------------------------
!
 FUNCTION netCDF_uDim_Exist( ncid ) RESULT( ItExist )
!
 IMPLICIT NONE
!
 include "netcdf.inc"
!
!Arguments 
 INTEGER, INTENT(IN) :: ncid 
!
!Return value
 LOGICAL             :: ItExist
!
!Description:
! Checks a given netCDF file to see if it contains an unlimited dimension.
!
!Local variables
 INTEGER             :: iErr
 INTEGER             :: UdimID
!
 iErr = Nf_Inq_Unlimdim( ncid,  UdimID )

 IF ( iErr == NF_NOERR ) THEN
    ItExist = .True.
 ELSE
    ItExist = .False.
 ENDIF
 
!Return to the calling routine
 RETURN

 END FUNCTION netCDF_uDim_Exist
!
!------------------------------------------------------------------------------
!
 FUNCTION netCDF_Dim_Exist( ncid, dimname ) RESULT( ItExist )
!
 IMPLICIT NONE
!
 include "netcdf.inc"
!
!Arguments 
 INTEGER         , INTENT(IN) :: ncid
 CHARACTER(LEN=*), INTENT(IN) :: dimname
!
!Return value
 LOGICAL                      :: ItExist
!
!Description:
! Checks a given netCDF file to see if a given netCDF dimension exists in it.
!
!Local variables
 INTEGER                      :: iErr
 INTEGER                      :: DimID
!
 iErr = Nf_Inq_Dimid( ncid, Dimname, DimId )

 IF ( iErr == NF_NOERR ) THEN
    ItExist = .True.
 ELSE
    ItExist = .False.
 ENDIF

!Return to the calling routine
 RETURN

 END FUNCTION netCDF_Dim_Exist
!
!------------------------------------------------------------------------------
!
 FUNCTION netCDF_Var_Exist( ncid, varname ) RESULT( ItExist )
!
 IMPLICIT NONE
!
 include "netcdf.inc"
!
!Arguments 
 INTEGER         , INTENT(IN) :: ncid
 CHARACTER(LEN=*), INTENT(IN) :: varname
!
!Return value
 LOGICAL                      :: ItExist
!
!Description:
! Checks a given netCDF file to see if a given netCDF variable exists in it.
!
!Local variables
 INTEGER                      :: iErr
 INTEGER                      :: VarID
!
 iErr = Nf_Inq_Varid( ncid, Varname, VarId )

 IF ( iErr == NF_NOERR ) THEN
    ItExist = .True.
 ELSE
    ItExist = .False.
 ENDIF

!Return to the calling routine
 RETURN

 END FUNCTION netCDF_Var_Exist
!
!------------------------------------------------------------------------------
!
 FUNCTION netCDF_Attr_Exist( ncid, varname, attname, attType ) RESULT( ItExist )
!
 IMPLICIT NONE
!
 include "netcdf.inc"
!
!Arguments 
 INTEGER         , INTENT(IN) :: ncid
 CHARACTER(LEN=*), INTENT(IN) :: varname
 CHARACTER(LEN=*), INTENT(IN) :: attname
!
!Output Parameter:
! attType  : Attribute type.  This value is will be set to one of the
! following: NF_BYTE, NF_CHAR, NF_SHORT, NF_INT, NF_FLOAT, or NF_DOUBLE.
 INTEGER         , INTENT(OUT):: attType
!
!Return value
 LOGICAL                      :: ItExist
!
!Description:
! Checks a given netCDF file to see if a given netCDF attribute exists for a
! given netCDF variable.
!
!Local variables
 INTEGER                      :: iErr
 INTEGER                      :: VarID
 INTEGER                      :: AttLen
!
!Intialize
 ItExist = .False.
 attType = -1

! First check the variable
 iErr = Nf_Inq_Varid( ncid, Varname, VarId )

! Check the attribute if variable was found
 IF ( iErr == NF_NOERR ) THEN
 
    iErr = Nf_Inq_Att( ncid, VarId, attName, attType, attLen )
    IF ( iErr == NF_NOERR ) ItExist = .True.

 ENDIF

!Return to the calling routine
 RETURN

 END FUNCTION netCDF_Attr_Exist
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_Get_Dim ( ncid, DimName, DimLen )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments:
 INTEGER         , INTENT(IN ) :: ncid
 CHARACTER(LEN=*), INTENT(IN ) :: DimName
 INTEGER         , INTENT(OUT) :: DimLen
!
!Description;
!  Read the length of a given netCDF dimension from a given netCDF file.
!
!Local variables
 INTEGER                       :: DimId
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'netCDF_Get_Dim: ' // TRIM( DimName )

! First get the dimension ID
 CALL Check( Nf_Inq_Dimid( ncid, DimName, DimId ), 50, Msg )

! Then read the dimension length
 CALL Check( Nf_Inq_Dimlen( ncid, DimID, DimLen ), 51, Msg )

! Return to invoking routine
 RETURN
!
 END SUBROUTINE netCDF_Get_Dim
!
!------------------------------------------------------------------------------
!
 SUBROUTINE netCDF_Get_uDim ( ncid, uDimLen )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments:
 INTEGER         , INTENT(IN ) :: ncid
 INTEGER         , INTENT(OUT) :: uDimLen
!
!Description;
!  Read the length of a netCDF unlimited dimension from a given netCDF file.
!
!Local variables
 INTEGER                       :: DimId
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'netCDF_Get_uDim'
!
! First get the dimension ID
 CALL Check( Nf_Inq_Unlimdim( ncid, DimId ), 50, Msg )

! Then read the dimension length
 CALL Check( Nf_Inq_Dimlen( ncid, DimID, uDimLen ), 51, Msg )

! Return to invoking routine
 RETURN
!
 END SUBROUTINE netCDF_Get_uDim
!
!------------------------------------------------------------------------------
! Begin Subroutines for a generic interface: netCDF_Get_Attr
!------------------------------------------------------------------------------
!
 SUBROUTINE ncGet_0D_Attr_c ( ncid, VarName, AttName, AttValue )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments:
 INTEGER         , INTENT(IN ) :: ncid
 CHARACTER(LEN=*), INTENT(IN ) :: VarName
 CHARACTER(LEN=*), INTENT(IN ) :: AttName
 CHARACTER(LEN=*), INTENT(OUT) :: AttValue
!
!Description:
! Reads a variable attribute(text type) from a netCDF file. 
! VarName = '' will read a global attribute of the netCDF file.
!
!Local variables
 INTEGER                       :: VarID
 CHARACTER (LEN=128)           :: Msg

! Initialize
 AttValue = ''
 Msg = 'ncGet_0D_Attr_c:' // TRIM( VarName ) // ': ' // Trim( AttName )

! Read a global attribute if "VarName" is a blank string
 IF ( TRIM( VarName ) == '' ) THEN 

    CALL Check( Nf_Get_Att_Text( ncid, NF_GLOBAL, AttName, AttValue ), 52, Msg )

! Otherwise read variable attribute
 ELSE
  
    ! First check if the variable exist
    CALL Check( Nf_Inq_Varid( ncid, VarName, VarID ), 53, Msg )

    ! Then get the attribute
    CALL Check( Nf_Get_Att_Text( ncid, VarID, AttName, AttValue ), 54, Msg ) 

 ENDIF
!
 RETURN
!
 END SUBROUTINE ncGet_0D_Attr_c
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncGet_0D_Attr_i ( ncid, VarName, AttName, AttValue )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments:
 INTEGER         , INTENT(IN ) :: ncid
 CHARACTER(LEN=*), INTENT(IN ) :: VarName
 CHARACTER(LEN=*), INTENT(IN ) :: AttName
 INTEGER         , INTENT(OUT) :: AttValue
!
!Description:
! Reads a variable attribute(INT type) from a netCDF file. 
! VarName = '' will read a global attribute of the netCDF file.
!
!Local variables
 INTEGER                       :: VarID
 CHARACTER (LEN=128)           :: Msg

! Initialize
 AttValue = ''
 Msg = 'ncGet_0D_Attr_i:' // TRIM( VarName ) // ': ' // Trim( AttName )

! Read a global attribute if "VarName" is a blank string
 IF ( TRIM( VarName ) == '' ) THEN

    CALL Check( Nf_Get_Att_Int( ncid, NF_GLOBAL, AttName, AttValue ), 52, Msg )

! Otherwise read variable attribute
 ELSE

    ! First check if the variable exist
    CALL Check( Nf_Inq_Varid( ncid, VarName, VarID ), 53, Msg )

    ! Then get the attribute
    CALL Check( Nf_Get_Att_Int( ncid, VarID, AttName, AttValue ), 54, Msg )

 ENDIF
!
 RETURN
!
 END SUBROUTINE ncGet_0D_Attr_i
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncGet_0D_Attr_r4 ( ncid, VarName, AttName, AttValue )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments:
 INTEGER         , INTENT(IN ) :: ncid
 CHARACTER(LEN=*), INTENT(IN ) :: VarName
 CHARACTER(LEN=*), INTENT(IN ) :: AttName
 REAL*4          , INTENT(OUT) :: AttValue
!
!Description:
! Reads a variable attribute(REAL4 type) from a netCDF file. 
! VarName = '' will read a global attribute of the netCDF file.
!
!Local variables
 INTEGER                       :: VarID
 CHARACTER (LEN=128)           :: Msg

! Initialize
 AttValue = ''
 Msg = 'ncGet_0D_Attr_r4:' // TRIM( VarName ) // ': ' // Trim( AttName )

! Read a global attribute if "VarName" is a blank string
 IF ( TRIM( VarName ) == '' ) THEN

    CALL Check( Nf_Get_Att_Real( ncid, NF_GLOBAL, AttName, AttValue ), 52, Msg )

! Otherwise read variable attribute
 ELSE

    ! First check if the variable exist
    CALL Check( Nf_Inq_Varid( ncid, VarName, VarID ), 53, Msg )

    ! Then get the attribute
    CALL Check( Nf_Get_Att_Real( ncid, VarID, AttName, AttValue ), 54, Msg )

 ENDIF
!
 RETURN
!
 END SUBROUTINE ncGet_0D_Attr_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncGet_0D_Attr_r8 ( ncid, VarName, AttName, AttValue )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments:
 INTEGER         , INTENT(IN ) :: ncid
 CHARACTER(LEN=*), INTENT(IN ) :: VarName
 CHARACTER(LEN=*), INTENT(IN ) :: AttName
 REAL*8          , INTENT(OUT) :: AttValue
!
!Description:
! Reads a variable attribute(REAL8 type) from a netCDF file. 
! VarName = '' will read a global attribute of the netCDF file.
!
!Local variables
 INTEGER                       :: VarID
 CHARACTER (LEN=128)           :: Msg

! Initialize
 AttValue = ''
 Msg = 'ncGet_0D_Attr_r8:' // TRIM( VarName ) // ': ' // Trim( AttName )

! Read a global attribute if "VarName" is a blank string
 IF ( TRIM( VarName ) == '' ) THEN

    CALL Check( Nf_Get_Att_Double( ncid, NF_GLOBAL, AttName, AttValue ), 52, Msg )

! Otherwise read variable attribute
 ELSE

    ! First check if the variable exist
    CALL Check( Nf_Inq_Varid( ncid, VarName, VarID ), 53, Msg )

    ! Then get the attribute
    CALL Check( Nf_Get_Att_Double( ncid, VarID, AttName, AttValue ), 54, Msg )

 ENDIF
!
 RETURN
!
 END SUBROUTINE ncGet_0D_Attr_r8
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncGet_1D_Attr_i ( ncid, VarName, AttName, AttValue )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments:
 INTEGER         , INTENT(IN ) :: ncid
 CHARACTER(LEN=*), INTENT(IN ) :: VarName
 CHARACTER(LEN=*), INTENT(IN ) :: AttName
 INTEGER         , INTENT(OUT) :: AttValue(:)
!
!Description:
! Reads a variable attribute(1D INT type) from a netCDF file. 
! VarName = '' will read a global attribute of the netCDF file.
!
!Local variables
 INTEGER                       :: VarID
 CHARACTER (LEN=128)           :: Msg

! Initialize
 AttValue = ''
 Msg = 'ncGet_1D_Attr_i:' // TRIM( VarName ) // ': ' // Trim( AttName )

! Read a global attribute if "VarName" is a blank string
 IF ( TRIM( VarName ) == '' ) THEN

    CALL Check( Nf_Get_Att_Int( ncid, NF_GLOBAL, AttName, AttValue ), 52, Msg )

! Otherwise read variable attribute
 ELSE

    ! First check if the variable exist
    CALL Check( Nf_Inq_Varid( ncid, VarName, VarID ), 53, Msg )

    ! Then get the attribute
    CALL Check( Nf_Get_Att_Int( ncid, VarID, AttName, AttValue ), 54, Msg )

 ENDIF
!
 RETURN
!
 END SUBROUTINE ncGet_1D_Attr_i
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncGet_1D_Attr_r4 ( ncid, VarName, AttName, AttValue )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments:
 INTEGER         , INTENT(IN ) :: ncid
 CHARACTER(LEN=*), INTENT(IN ) :: VarName
 CHARACTER(LEN=*), INTENT(IN ) :: AttName
 REAL*4          , INTENT(OUT) :: AttValue(:)
!
!Description:
! Reads a variable attribute(1D REAL4 type) from a netCDF file. 
! VarName = '' will read a global attribute of the netCDF file.
!
!Local variables
 INTEGER                       :: VarID
 CHARACTER (LEN=128)           :: Msg

! Initialize
 AttValue = ''
 Msg = 'ncGet_1D_Attr_r4:' // TRIM( VarName ) // ': ' // Trim( AttName )

! Read a global attribute if "VarName" is a blank string
 IF ( TRIM( VarName ) == '' ) THEN

    CALL Check( Nf_Get_Att_Real( ncid, NF_GLOBAL, AttName, AttValue ), 52, Msg )

! Otherwise read variable attribute
 ELSE

    ! First check if the variable exist
    CALL Check( Nf_Inq_Varid( ncid, VarName, VarID ), 53, Msg )

    ! Then get the attribute
    CALL Check( Nf_Get_Att_Real( ncid, VarID, AttName, AttValue ), 54, Msg )

 ENDIF
!
 RETURN
!
 END SUBROUTINE ncGet_1D_Attr_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncGet_1D_Attr_r8 ( ncid, VarName, AttName, AttValue )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments:
 INTEGER         , INTENT(IN ) :: ncid
 CHARACTER(LEN=*), INTENT(IN ) :: VarName
 CHARACTER(LEN=*), INTENT(IN ) :: AttName
 REAL*8          , INTENT(OUT) :: AttValue(:)
!
!Description:
! Reads a variable attribute(1D REAL8 type) from a netCDF file. 
! VarName = '' will read a global attribute of the netCDF file.
!
!Local variables
 INTEGER                       :: VarID
 CHARACTER (LEN=128)           :: Msg

! Initialize
 AttValue = ''
 Msg = 'ncGet_1D_Attr_r8:' // TRIM( VarName ) // ': ' // Trim( AttName )

! Read a global attribute if "VarName" is a blank string
 IF ( TRIM( VarName ) == '' ) THEN

    CALL Check( Nf_Get_Att_Double( ncid, NF_GLOBAL, AttName, AttValue ), 52, Msg )

! Otherwise read variable attribute
 ELSE

    ! First check if the variable exist
    CALL Check( Nf_Inq_Varid( ncid, VarName, VarID ), 53, Msg )

    ! Then get the attribute
    CALL Check( Nf_Get_Att_Double( ncid, VarID, AttName, AttValue ), 54, Msg )

 ENDIF
!
 RETURN
!
 END SUBROUTINE ncGet_1D_Attr_r8
!
!------------------------------------------------------------------------------
! End Subroutines for a generic interface: netCDF_Get_Attr
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
! Begin Subroutines for a generic interface: netCDF_Read_Var
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_0D_Var_i ( ncid, var, varname )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(OUT) :: var
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_0D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Var_Int( ncid, varid, var ), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_0D_Var_i
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_0D_Var_r4 ( ncid, var, varname )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 REAL*4           , INTENT(OUT) :: var
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_0D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Var_Real( ncid, varid, var ), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_0D_Var_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_0D_Var_r8 ( ncid, var, varname )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 REAL*8           , INTENT(OUT) :: var
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_0D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Var_Double( ncid, varid, var ), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_0D_Var_r8
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_0D_Var_c ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start
 INTEGER          , INTENT(IN ) :: count
 CHARACTER (LEN=*), INTENT(OUT) :: var
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_1D_Var_c: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Text(ncid, varid, start, count, var), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_0D_Var_c
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_1D_Var_i ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start
 INTEGER          , INTENT(IN ) :: count
 INTEGER          , INTENT(OUT) :: var(count)
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_1D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Int(ncid, varid, start, count, var), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_1D_Var_i
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_1D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start
 INTEGER          , INTENT(IN ) :: count
 REAL*4           , INTENT(OUT) :: var(count)
!
!Local variables    
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_1D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Real(ncid, varid, start, count, var), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_1D_Var_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_1D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start
 INTEGER          , INTENT(IN ) :: count
 REAL*8           , INTENT(OUT) :: var(count)
!
!Local variables    
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_1D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Double(ncid, varid, start, count, var), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_1D_Var_r8
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_1D_Var_c ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(2)
 INTEGER          , INTENT(IN ) :: count(2)
 CHARACTER (LEN=*), INTENT(OUT) :: var(count(2))
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_2D_Var_c: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Text(ncid, varid, start, count, var), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_1D_Var_c
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_2D_Var_i ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(2)
 INTEGER          , INTENT(IN ) :: count(2)
 INTEGER          , INTENT(OUT) :: var(count(1),count(2))
!
!Local variables
 INTEGER                       :: varid
 CHARACTER (LEN=128)           :: Msg

! Msg
 Msg = 'ncRead_2D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Int(ncid, varid, start, count, var), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_2D_Var_i
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_2D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(2)
 INTEGER          , INTENT(IN ) :: count(2)
 REAL*4           , INTENT(OUT) :: var(count(1),count(2))
!
!Local variables  
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_2D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Real(ncid, varid, start, count, var), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_2D_Var_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_2D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(2)
 INTEGER          , INTENT(IN ) :: count(2)
 REAL*8           , INTENT(OUT) :: var(count(1),count(2))
!
!Local variables  
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_2D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Double(ncid, varid, start, count, var), 61, Msg )

 RETURN
!
 END SUBROUTINE ncRead_2D_Var_r8
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_3D_Var_i ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(3)
 INTEGER          , INTENT(IN ) :: count(3)
 INTEGER          , INTENT(OUT) :: var(count(1),count(2),count(3))
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_3D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Int(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_3D_Var_i
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_3D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(3)
 INTEGER          , INTENT(IN ) :: count(3)
 REAL*4           , INTENT(OUT) :: var(count(1),count(2),count(3))
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_3D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Real(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_3D_Var_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_3D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(3)
 INTEGER          , INTENT(IN ) :: count(3)
 REAL*8           , INTENT(OUT) :: var(count(1),count(2),count(3))
!
!Local variables  
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_3D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Double(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_3D_Var_r8
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_4D_Var_i ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(4)
 INTEGER          , INTENT(IN ) :: count(4)
 INTEGER          , INTENT(OUT) :: var(count(1),count(2),count(3),count(4))
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_4D_Var_i: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Int(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_4D_Var_i
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_4D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(4)
 INTEGER          , INTENT(IN ) :: count(4)
 REAL*4           , INTENT(OUT) :: var(count(1),count(2),count(3),count(4))
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_4D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Real(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_4D_Var_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_4D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(4)
 INTEGER          , INTENT(IN ) :: count(4)
 REAL*8           , INTENT(OUT) :: var(count(1),count(2),count(3),count(4))
!
!Local variables    
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_4D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Double(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_4D_Var_r8
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_5D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(5)
 INTEGER          , INTENT(IN ) :: count(5)
 REAL*4           , INTENT(OUT) :: var &
                    (count(1),count(2),count(3),count(4),count(5))
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_5D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Real(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_5D_Var_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_5D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(5)
 INTEGER          , INTENT(IN ) :: count(5)
 REAL*8           , INTENT(OUT) :: var &
                    (count(1),count(2),count(3),count(4),count(5))
!
!Local variables    
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_5D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Double(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_5D_Var_r8
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_6D_Var_r4 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(6)
 INTEGER          , INTENT(IN ) :: count(6)
 REAL*4           , INTENT(OUT) :: var &
                    (count(1),count(2),count(3),count(4),count(5),count(6))
!
!Local variables
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_6D_Var_r4: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Real(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_6D_Var_r4
!
!------------------------------------------------------------------------------
!
 SUBROUTINE ncRead_6D_Var_r8 ( ncid, var, varname, start, count )
!
 IMPLICIT NONE
!                            
!USES:
 include 'netcdf.inc'
!
!Arguments
 INTEGER          , INTENT(IN ) :: ncid
 CHARACTER (LEN=*), INTENT(IN ) :: varname
 INTEGER          , INTENT(IN ) :: start(6)
 INTEGER          , INTENT(IN ) :: count(6)
 REAL*8           , INTENT(OUT) :: var &
                    (count(1),count(2),count(3),count(4),count(5),count(6))
!
!Local variables    
 INTEGER                        :: varid
 CHARACTER (LEN=128)            :: Msg

! Msg
 Msg = 'ncRead_6D_Var_r8: ' // TRIM( varname )

! Get the variable ID given the variable name
 CALL Check( Nf_Inq_Varid( ncid, varname, varid ), 60, Msg )

! Read the variable from the file
 CALL Check( Nf_Get_Vara_Double(ncid, varid, start, count, var), 61, Msg )
!
 RETURN
 END SUBROUTINE ncRead_6D_Var_r8
!
!------------------------------------------------------------------------------
! End Subroutines for a generic interface: netCDF_Read_Var
!------------------------------------------------------------------------------
!
 SUBROUTINE Check ( Status, Location, Msg )
!
 IMPLICIT NONE
!
!USE: 
 include 'netcdf.inc'
!
!Arguments
 INTEGER,           INTENT(IN) :: Status
 INTEGER,           INTENT(IN) :: Location
 CHARACTER (LEN=*), INTENT(IN) :: Msg

!DESCRIPTION:
! checks the status of calling netCDF library routines
! 

 IF ( STATUS /= NF_NOERR ) THEN

    WRITE(*,*) TRIM( NF_STRERROR( STATUS ) )
    WRITE(*,*) ' At location = ', LOCATION, ', ', TRIM( Msg )
    !CALL ERROR_STOP('netCDF error', 'diag_mod.f')
    STOP

 ENDIF

 RETURN
! 
 END SUBROUTINE Check
!
!------------------------------------------------------------------------------
!
 SUBROUTINE Check_Optional_Arguments ( NArg, Arg_Present, iPresent, Msg )
!
 IMPLICIT NONE
!
!Arguments
 INTEGER,           INTENT(IN) :: NArg
 LOGICAL,           INTENT(IN) :: Arg_Present(NArg)
 INTEGER,           INTENT(IN) :: iPresent
 CHARACTER (LEN=*), INTENT(IN) :: Msg

! Local variable
 INTEGER                       :: Num_Present
 INTEGER                       :: I

! Only iPresent of these optional arguments can be present
 Num_Present = 0

 DO I = 1, NArg
    IF ( Arg_Present(I) ) Num_Present = Num_Present + 1
 ENDDO

 IF ( num_present == 0 ) THEN
    WRITE(*,*) 'Warnning: num_present=0 for ' // TRIM(Msg)
 ELSE IF ( num_present /= iPresent ) THEN
    WRITE(*,*) 'Num_Present =', num_present
    WRITE(*,*) 'Num_Present for '//TRIM(Msg)//' is not equal to', iPresent
    STOP
 ENDIF

 RETURN
!
 END SUBROUTINE Check_Optional_Arguments
!
!------------------------------------------------------------------------------
!
END MODULE netCDF_MOD
