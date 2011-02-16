!------------------------------------------------------------------------------
SUBROUTINE XdmfWriter(Model, Solver, dt, TransientSimulation)
!------------------------------------------------------------------------------
! EXPERIMENTAL
!
! Writes results in xdmf/hdf5 format.
!
! Only linear elements and scalar functions are supprted at the moment.
!
! See README.txt within this directory for more information.
!
! Written by: Mikko Lyly, 11 Feb 2011
!------------------------------------------------------------------------------
  USE HDF5
  USE DefUtils
  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
  INTEGER, PARAMETER :: MaxScalarFields = 1000
  INTEGER :: NofScalarFields
  CHARACTER(LEN=MAX_NAME_LEN) :: ScalarFieldNames(MaxScalarFields)
  CHARACTER(LEN=MAX_NAME_LEN) :: BaseFileName
  TYPE(Mesh_t), POINTER :: Mesh
  INTEGER :: PEs, MyPE, i, ierr
  INTEGER, ALLOCATABLE :: NofNodes(:), NofElements(:), NofStorage(:), itmp(:)
  INTEGER(HID_T) :: file_id, plist_id
  CHARACTER(LEN=MAX_NAME_LEN) :: Str
  REAL(KIND=dp) :: RealTime, StartTime, TotalTime
  LOGICAL :: Found
  INTEGER :: Counter = 1
  SAVE BaseFileName, NofScalarFields, ScalarFieldNames, Counter
!------------------------------------------------------------------------------
  StartTime = RealTime()
  Mesh => GetMesh()
  PEs = ParEnv % PEs
  MyPE = ParEnv % MyPE + 1

  ! Determine file names and fields variables:
  !--------------------------------------------
  IF(Counter == 1) THEN
     BaseFileName = ListGetString(Solver % Values, 'base file name', Found)
     IF(.NOT.Found) BaseFileName = 'results'
     CALL INFO('XdmfWriter', 'Base file name = '//TRIM(BaseFileName))

     CALL FindScalarFields(NofScalarFields, ScalarFieldNames)
     DO i = 1, NofScalarFields
        CALL INFO('Xdmfwriter', 'Scalar field: '//TRIM(ScalarFieldNames(i)))
     END DO
  END IF

  ! Determine Nof nodes, Nof elements and mixed xdmf storage size for all PEs:
  !----------------------------------------------------------------------------
  ALLOCATE(itmp(PEs))

  ALLOCATE(NofNodes(PEs), NofElements(PEs), NofStorage(PEs))

  NofNodes = 0; itmp = 0;  itmp(MyPE) = Mesh % NumberOfNodes
  CALL MPI_ALLREDUCE(itmp, NofNodes, PEs, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)

  NofElements = 0; itmp = 0; itmp(MyPE) = GetNofActive()
  CALL MPI_ALLREDUCE(itmp, NofElements, PEs, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)

  NofStorage = 0; itmp = 0; itmp(MyPE) = GetElementStorageSize()
  CALL MPI_ALLREDUCE(itmp, NofStorage, PEs, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)

  DEALLOCATE(itmp)  

  ! Initialize and create the hdf5 file collectively:
  !---------------------------------------------------
  CALL h5open_f(ierr)
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
  CALL h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL, ierr)

  IF(Counter == 1) THEN
     CALL h5fcreate_f(TRIM(BaseFileName)//'.h5', H5F_ACC_TRUNC_F, file_id, ierr, access_prp = plist_id)
  ELSE
     CALL h5fopen_f(TRIM(BaseFileName)//'.h5', H5F_ACC_RDWR_F, file_id, ierr, access_prp = plist_id)
  END IF

  CALL h5pclose_f(plist_id, ierr)

  ! Write nodes, elements and data:
  !---------------------------------
  IF(Counter == 1) THEN
     CALL WriteNodes(file_id, PEs, MyPE, NofNodes)
     CALL WriteElements(file_id, PEs, MyPE, NofStorage)
     CALL WriteParts(file_id, PEs, MyPE, NofNodes)
  END IF

  DO i = 1, NofScalarFields
     CALL WriteScalars(file_id, PEs, MyPE, NofNodes, ScalarFieldNames(i))
  END DO

  IF(MyPE == 1) CALL WriteXdmfFile(PEs, NofNodes, NofElements, &
       NofStorage, NofScalarFields, ScalarFieldNames, BaseFileName)

  ! Finalize:
  !-----------
  CALL h5fclose_f(file_id, ierr)
  DEALLOCATE(NofElements, NofNodes, NofStorage)
  TotalTime = RealTime() - StartTime
  WRITE(Str, *) TotalTime
  CALL INFO('XdmfWriter', 'Total write time (REAL) = '//TRIM(ADJUSTL(Str)))
  Counter = Counter + 1

CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE FindScalarFields(NofScalarFields, ScalarFieldNames)
!------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: NofScalarFields
    CHARACTER(LEN=MAX_NAME_LEN), INTENT(OUT) :: ScalarFieldNames(:)

    INTEGER :: id
    LOGICAL :: Found
    CHARACTER(LEN=MAX_NAME_LEN) :: LHS, RHS, Tmp1
    TYPE(Variable_t), POINTER :: Variable

    NofScalarFields = 0

    DO id = 1, SIZE(ScalarFieldNames)
       WRITE(Tmp1, *) id

       WRITE(LHS, '(A)') 'scalar field '//TRIM(ADJUSTL(Tmp1))

       RHS = ListGetString(Solver % Values, TRIM(LHS), Found)

       IF(.NOT.Found) CYCLE

       Variable => VariableGet(Solver % Mesh % Variables, TRIM(RHS))

       IF(.NOT.ASSOCIATED(Variable)) THEN
          CALL INFO('Xdmfwriter', 'Bad scalar field: '//TRIM(RHS))
          CYCLE
       END IF

       NofScalarFields = NofScalarFields + 1
       ScalarFieldNames(NofScalarFields) = TRIM(RHS)
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE FindScalarFields
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  INTEGER FUNCTION GetXdmfCode(ElementCode) RESULT(XdmfCode)
!------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: ElementCode, XdmfCode
    
    SELECT CASE(ElementCode)
    CASE(202)
       XdmfCode = 2 ! polyline
    CASE(303)
       XdmfCode = 4 ! linear triangle
    CASE(404)
       XdmfCode = 5 ! linear quadrilateral
    CASE(504)
       XdmfCode = 6 ! linear tetrahedron
    CASE(808)
       XdmfCode = 9 ! linear hexahedron
    CASE DEFAULT
       XdmfCode = -1 ! not supported, yet
    END SELECT

!------------------------------------------------------------------------------
  END FUNCTION GetXdmfCode
!------------------------------------------------------------------------------
  
!------------------------------------------------------------------------------
  INTEGER FUNCTION GetElementStorageSize() RESULT(StorageSize)
!------------------------------------------------------------------------------
    INTEGER :: i, StorageSize
    TYPE(Element_t), POINTER :: Element
    INTEGER :: XdmfCode

    StorageSize = 0
    DO i = 1, GetNofActive()
       Element => GetActiveElement(i)
       IF(.NOT.ASSOCIATED(Element)) CYCLE
       XdmfCode = GetXdmfCode(Element % Type % ElementCode)
       IF(XdmfCode < 0) CYCLE ! unknown: skip this element
       StorageSize = StorageSize + 1
       IF(XdmfCode == 2) StorageSize = StorageSize + 1 ! polyline
       StorageSize = StorageSize + GetElementNofNodes()
    END DO

!------------------------------------------------------------------------------
  END FUNCTION GetElementStorageSize
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE WriteNodes(file_id, PEs, MyPE, NofNodes)
!------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: file_id, PEs, MyPE, NofNodes(:)

    INTEGER :: i, ierr
    INTEGER(HSIZE_T) :: dims(2)
    INTEGER(HID_T) :: dset_id(PEs), filespace, memspace, plist_id
    REAL(KIND=dp) :: data(3, NofNodes(MyPE))
    CHARACTER(LEN=MAX_NAME_LEN) :: Str

    ! Create the datasets collectively:
    !-----------------------------------
    DO i = 1, PEs
       dims(1) = SIZE(data, 1)
       dims(2) = NofNodes(i)

       WRITE(Str, *) i
       WRITE(Str, '(A)') 'nodes_'//TRIM(ADJUSTL(Str))

       CALL h5screate_simple_f(2, dims, filespace, ierr)
       CALL h5dcreate_f(file_id, TRIM(ADJUSTL(Str)), H5T_NATIVE_DOUBLE, filespace, dset_id(i), ierr)
       CALL h5sclose_f(filespace, ierr)
    END DO

    ! Write the data independently:
    !-------------------------------
    dims(1) = SIZE(data, 1)
    dims(2) = SIZE(data, 2)

    DO i = 1, dims(2)
       data(1, i) = Mesh % Nodes % x(i)
       data(2, i) = Mesh % Nodes % y(i)
       data(3, i) = Mesh % Nodes % z(i)
    END DO
    
    CALL h5screate_simple_f(2, dims, memspace, ierr)
    CALL h5dget_space_f(dset_id(MyPE), filespace, ierr)
    
    CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
    CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)
    
    CALL h5dwrite_f(dset_id(MyPE), H5T_NATIVE_DOUBLE, data, dims, ierr, &
         file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)
    
    ! Finalize:
    !-----------
    CALL h5pclose_f(plist_id, ierr)
    CALL h5sclose_f(filespace, ierr)
    CALL h5sclose_f(memspace, ierr)
   
    DO i = 1, PEs
       CALL h5dclose_f(dset_id(i), ierr)
    END DO
    
!------------------------------------------------------------------------------
  END SUBROUTINE WriteNodes
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE WriteElements(file_id, PEs, MyPE, NofStorage)
!------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: file_id, PEs, MyPE, NofStorage(:)

    INTEGER :: i, j, k, ierr, XdmfCode
    INTEGER(HSIZE_T) :: dims(2)
    INTEGER(HID_T) :: dset_id(PEs), filespace, memspace, plist_id
    INTEGER :: data(1, NofStorage(MyPE))
    CHARACTER(LEN=MAX_NAME_LEN) :: Str
    TYPE(Element_t), POINTER :: Element

    ! Create the datasets collectively:
    !-----------------------------------
    DO i = 1, PEs
       dims(1) = SIZE(data, 1)
       dims(2) = NofStorage(i)

       WRITE(Str, *) i
       WRITE(Str, '(A)') 'elements_'//TRIM(ADJUSTL(Str))

       CALL h5screate_simple_f(2, dims, filespace, ierr)
       CALL h5dcreate_f(file_id, TRIM(ADJUSTL(Str)), H5T_NATIVE_INTEGER, filespace, dset_id(i), ierr)
       CALL h5sclose_f(filespace, ierr)
    END DO

    ! Write the data independently:
    !-------------------------------
    dims(1) = SIZE(data, 1)
    dims(2) = SIZE(data, 2)

    j = 0
    DO i = 1, GetNofActive()
       Element => GetActiveElement(i)
       IF(.NOT.ASSOCIATED(Element)) CYCLE
       XdmfCode = GetXdmfCode(Element % Type % ElementCode)
       IF(XdmfCode < 0) CYCLE ! unknown: skip this element

       j = j + 1
       data(1, j) = XdmfCode

       IF(XdmfCode == 2) THEN
          j = j + 1 ! polyline: nof nodes
          data(1, j) = GetElementNofNodes()
       END IF

       DO k = 1, GetElementNofNodes()
          j = j + 1
          data(1, j) = Element % NodeIndexes(k) - 1 ! C-style numbering
       END DO
    END DO

    IF(j /= NofStorage(MyPE)) CALL Fatal('XdmfWriter', 'Element numbering failed')
    
    CALL h5screate_simple_f(2, dims, memspace, ierr)
    CALL h5dget_space_f(dset_id(MyPE), filespace, ierr)
    
    CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
    CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)

    CALL h5dwrite_f(dset_id(MyPE), H5T_NATIVE_INTEGER, data, dims, ierr, &
         file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)

    ! Finalize:
    !-----------
    CALL h5pclose_f(plist_id, ierr)
    CALL h5sclose_f(filespace, ierr)
    CALL h5sclose_f(memspace, ierr)
   
    DO i = 1, PEs
       CALL h5dclose_f(dset_id(i), ierr)
    END DO
    
!------------------------------------------------------------------------------
  END SUBROUTINE WriteElements
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE WriteParts(file_id, PEs, MyPE, NofNodes)
!------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: file_id, PEs, MyPE, NofNodes(:)

    INTEGER :: i, ierr
    INTEGER(HSIZE_T) :: dims(2)
    INTEGER(HID_T) :: dset_id(PEs), filespace, memspace, plist_id
    REAL(KIND=dp) :: data(1, NofNodes(MyPE))
    CHARACTER(LEN=MAX_NAME_LEN) :: Str

    ! Create the datasets collectively:
    !-----------------------------------
    DO i = 1, PEs
       dims(1) = SIZE(data, 1)
       dims(2) = NofNodes(i)

       WRITE(Str, *) i
       WRITE(Str, '(A)') 'part_number_'//TRIM(ADJUSTL(Str))

       CALL h5screate_simple_f(2, dims, filespace, ierr)
       CALL h5dcreate_f(file_id, TRIM(ADJUSTL(Str)), H5T_NATIVE_DOUBLE, filespace, dset_id(i), ierr)
       CALL h5sclose_f(filespace, ierr)
    END DO

    ! Write the data independently:
    !-------------------------------
    dims(1) = SIZE(data, 1)
    dims(2) = SIZE(data, 2)

    data = MyPE
    
    CALL h5screate_simple_f(2, dims, memspace, ierr)
    CALL h5dget_space_f(dset_id(MyPE), filespace, ierr)
    
    CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
    CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)
    
    CALL h5dwrite_f(dset_id(MyPE), H5T_NATIVE_DOUBLE, data, dims, ierr, &
         file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)
    
    ! Finalize:
    !-----------
    CALL h5pclose_f(plist_id, ierr)
    CALL h5sclose_f(filespace, ierr)
    CALL h5sclose_f(memspace, ierr)
   
    DO i = 1, PEs
       CALL h5dclose_f(dset_id(i), ierr)
    END DO
    
!------------------------------------------------------------------------------
  END SUBROUTINE WriteParts
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  SUBROUTINE WriteScalars(file_id, PEs, MyPE, NofNodes, ScalarFieldName)
!------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: file_id, PEs, MyPE, NofNodes(:)
    CHARACTER(LEN=MAX_NAME_LEN) :: ScalarFieldName

    INTEGER :: i, j, ierr
    INTEGER(HSIZE_T) :: dims(2)
    INTEGER(HID_T) :: dset_id(PEs), filespace, memspace, plist_id
    REAL(KIND=dp) :: data(1, NofNodes(MyPE))
    CHARACTER(LEN=MAX_NAME_LEN) :: Str, Tmp1, Tmp2
    TYPE(Variable_t), POINTER :: Var

    ! Create the datasets collectively:
    !-----------------------------------
    DO i = 1, PEs
       dims(1) = SIZE(data, 1)
       dims(2) = NofNodes(i)
       
       WRITE(Tmp1, *) i
       WRITE(Tmp2, *) Counter
       
       WRITE(Str, '(A)') TRIM(ScalarFieldName)//'_'//TRIM(ADJUSTL(Tmp2))//'_'//TRIM(ADJUSTL(Tmp1))
       
       CALL h5screate_simple_f(2, dims, filespace, ierr)
       CALL h5dcreate_f(file_id, TRIM(ADJUSTL(Str)), H5T_NATIVE_DOUBLE, filespace, dset_id(i), ierr)
       CALL h5sclose_f(filespace, ierr)
    END DO
    
    ! Write the data independently:
    !-------------------------------
    dims(1) = SIZE(data, 1)
    dims(2) = SIZE(data, 2)
    
    Var => VariableGet(Solver % Mesh % Variables, TRIM(ScalarFieldName))
    IF(.NOT.ASSOCIATED(Var)) CALL INFO('XdmfWriter', 'Scalar not found')
    
    DO i = 1, dims(2)
       j = Var % Perm(i)
       data(1, i) = Var % Values(j)
    END DO
    
    CALL h5screate_simple_f(2, dims, memspace, ierr)
    CALL h5dget_space_f(dset_id(MyPE), filespace, ierr)
    
    CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
    CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)
    
    CALL h5dwrite_f(dset_id(MyPE), H5T_NATIVE_DOUBLE, data, dims, ierr, &
         file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)
    
    ! Finalize:
    !-----------
    CALL h5pclose_f(plist_id, ierr)
    CALL h5sclose_f(filespace, ierr)
    CALL h5sclose_f(memspace, ierr)
    
    DO i = 1, PEs
       CALL h5dclose_f(dset_id(i), ierr)
    END DO
    
!------------------------------------------------------------------------------
  END SUBROUTINE WriteScalars
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  LOGICAL FUNCTION Dmp(fid, indent, str) RESULT(ok)
!------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: fid, indent
    CHARACTER(LEN=*) :: str
    LOGICAL :: ok

    INTEGER :: i
    CHARACTER(LEN=MAX_NAME_LEN) :: line

    WRITE(line, '(A)') REPEAT(' ', indent)//TRIM(ADJUSTL(str))//CHAR(10)
    
    WRITE(fid) TRIM(line)

    ok = .TRUE.
!------------------------------------------------------------------------------
  END FUNCTION Dmp
!------------------------------------------------------------------------------
  
!------------------------------------------------------------------------------
  SUBROUTINE WriteXdmfFile(PEs, NofNodes, NofElements, &
       NofStorage, NofScalarFields, ScalarFieldNames, BaseFileName)
!------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: PEs, NofScalarFields
    INTEGER :: NofElements(:), NofNodes(:), NofStorage(:)
    CHARACTER(LEN=MAX_NAME_LEN) :: ScalarFieldNames(:), BaseFileName

    CHARACTER(LEN=MAX_NAME_LEN) :: Line, Tmp1, Tmp2, Tmp3, Tmp4, Tmp5
    CHARACTER(LEN=MAX_NAME_LEN) :: FileName
    CHARACTER(LEN=MAX_NAME_LEN) :: H5FileName
    CHARACTER(LEN=MAX_NAME_LEN) :: ScalarName
    INTEGER :: i, j, k, ScalarId
    LOGICAL :: ok

    ! Initialize:
    !-------------
    FileName = TRIM(ADJUSTL(BaseFileName))//'.xmf'
    H5FileName = TRIM(ADJUSTL(BaseFileName))//'.h5'

    OPEN(UNIT=10, FILE=TRIM(FileName), FORM='unformatted', ACCESS='stream', STATUS='unknown')

    ok = Dmp(10, 0, '<?xml version="1.0" ?>')
    ok = Dmp(10, 0, '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>')
    ok = Dmp(10, 0, '')
    ok = Dmp(10, 0, '<Xdmf>')
    ok = Dmp(10, 2, '<Domain>')

    ok = Dmp(10, 4, '<Grid Name="mesh" GridType="Collection" CollectionType="Temporal">')

    DO j = 1, Counter
       WRITE(Tmp1, *) j; WRITE(Tmp1, '(A)') TRIM(ADJUSTL(Tmp1))

       ok = Dmp(10, 6, '<Grid Name="mesh_'//TRIM(Tmp1)//'" GridType="Collection" CollectionType="Spatial">')
       ok = Dmp(10, 8, '<Time Value="'//TRIM(Tmp1)//'" />')

       DO i = 1, PEs
          WRITE(Tmp1, *) i;              WRITE(Tmp1, '(A)') TRIM(ADJUSTL(Tmp1))
          WRITE(Tmp2, *) NofElements(i); WRITE(Tmp2, '(A)') TRIM(ADJUSTL(Tmp2))
          WRITE(Tmp3, *) NofStorage(i);  WRITE(Tmp3, '(A)') TRIM(ADJUSTL(Tmp3))
          WRITE(Tmp4, *) NofNodes(i);    WRITE(Tmp4, '(A)') TRIM(ADJUSTL(Tmp4))
          WRITE(Tmp5, *) j;              WRITE(Tmp5, '(A)') TRIM(ADJUSTL(Tmp5))
          
          ! Init part:
          !------------
          ok = Dmp(10, 8, '<Grid Name="mesh_'//TRIM(Tmp5)//'_'//TRIM(Tmp1)//'">')
          ok = Dmp(10, 10, '<Time Value="'//TRIM(Tmp5)//'" />')
          
          ! Write elements:
          !-----------------
          ok = Dmp(10, 10, '<Topology Type="Mixed" NumberOfElements="'//TRIM(Tmp2)//'">')
          ok = Dmp(10, 12, '<DataItem Format="HDF" DataType="Int" Dimensions="'//TRIM(Tmp3)//'">')
          ok = Dmp(10, 14, TRIM(H5FileName)//':/elements_'//TRIM(Tmp1))
          ok = Dmp(10, 12, '</DataItem>')
          ok = Dmp(10, 10, '</Topology>')
          
          ! Write nodes:
          !--------------
          ok = Dmp(10, 10, '<Geometry Type="XYZ">')
          ok = Dmp(10, 12, '<DataItem Format="HDF" DataType="Float" Precision="8" Dimensions="'//TRIM(Tmp4)//' 3">')
          ok = Dmp(10, 14, TRIM(H5FileName)//':/nodes_'//TRIM(Tmp1))
          ok = Dmp(10, 12, '</DataItem>')
          ok = Dmp(10, 10, '</Geometry>')
          
          ! Write part number:
          !--------------------
          ok = Dmp(10, 10, ' <Attribute Name="part_number" Center="Node">')
          ok = Dmp(10, 12, ' <DataItem Format="HDF" DataType="Float" Precision="8" Dimensions="'//TRIM(Tmp4)//' 1">')
          ok = Dmp(10, 14, TRIM(H5FileName)//':/part_number_'//TRIM(Tmp1))
          ok = Dmp(10, 12, '</DataItem>')
          ok = Dmp(10, 10, '</Attribute>')
          
          ! Write scalar fields:
          !----------------------
          DO k = 1, NofScalarFields
             ok = Dmp(10, 10, ' <Attribute Name="'//TRIM(ScalarFieldNames(k))//'" Center="Node">')
             ok = Dmp(10, 12, ' <DataItem Format="HDF" DataType="Float" Precision="8" Dimensions="'//TRIM(Tmp4)//' 1">')
             ok = Dmp(10, 14, TRIM(H5FileName)//':/'//TRIM(ScalarFieldNames(k))//'_'//TRIM(Tmp5)//'_'//TRIM(Tmp1))
             ok = Dmp(10, 12, '</DataItem>')
             ok = Dmp(10, 10, '</Attribute>')
          END DO
          
          ! Finalize part:
          !----------------
          ok = Dmp(10, 8, '</Grid>') ! part
       END DO

    ok = Dmp(10, 6, '</Grid>') ! spatial collection

    END DO

    ok = Dmp(10, 4, '</Grid>') ! temporal collection
    ok = Dmp(10, 2, '</Domain>')
    ok = Dmp(10, 0, '</Xdmf>')

    CLOSE(10)

!------------------------------------------------------------------------------
  END SUBROUTINE WriteXdmfFile
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END SUBROUTINE XdmfWriter
!------------------------------------------------------------------------------
