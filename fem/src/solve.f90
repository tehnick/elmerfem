	SUBROUTINE SOLVEF( N,M,A,x )

	INTEGER  N,M,IPIV(N)
	REAL(KIND=dp) :: A(m,n),x(n)

	IF ( N .LE. 0 ) RETURN

	CALL DGETRF( N,N,A,N,IPIV,INFO )
	IF ( info .NE. 0 ) PRINT*,'DGETRF: ', info

	CALL DGETRS( 'N',N,M,A,N,IPIV,X,N,INFO )
	IF ( info .NE. 0 ) PRINT*,'DGETRS: ', info

	END
