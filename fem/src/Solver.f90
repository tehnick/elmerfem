PROGRAM Solver
   USE Types
   USE GeneralUtils

   CHARACTER(LEN=MAX_NAME_LEN) :: DateStr
   REAL(KIND=dp) :: CT, RT, CPUTime, RealTime

   CT = CPUtime()
   RT = RealTime()
   DateStr = FormatDate()
   WRITE( *,'(A,A)' ) 'ELMER SOLVER STARTED AT: ', TRIM(DateStr)
   CALL ElmerSolver
   WRITE( *,'(a,F12.2,F12.2)' ) 'SOLVER TOTAL TIME(CPU,REAL): ', &
               CPUTime()-CT, RealTime()-RT
   DateStr = FormatDate()
   WRITE( *,'(A,A)' ) 'ELMER SOLVER FINISHED AT: ', TRIM(DateStr)
END PROGRAM Solver
