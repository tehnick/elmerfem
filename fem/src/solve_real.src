	SUBROUTINE SolveLapack( N,A,x )

	INTEGER  N,IPIV(N)
	DOUBLE PRECISION  A(n*n),x(n)

	IF ( N .LE. 0 ) RETURN
	CALL DGETRF( N,N,A,N,IPIV,INFO )
	IF ( info .NE. 0 ) PRINT*,'DGETRF: ', info

	CALL DGETRS( 'N',N,1,A,N,IPIV,X,N,INFO )
	IF ( info .NE. 0 ) PRINT*,'DGETRS: ', info

	END
