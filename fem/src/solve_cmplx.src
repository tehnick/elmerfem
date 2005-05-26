	SUBROUTINE SolveLapack_cmplx( N,A,x )

	INTEGER  N, IPIV(N)
	DOUBLE COMPLEX A(n*n), x(n)

	IF ( N .LE. 0 ) RETURN
	CALL ZGETRF( N,N,A,N,IPIV,INFO )
	IF ( info .NE. 0 ) PRINT*,'ZGETRF: ', info

	CALL ZGETRS( 'N',N,1,A,N,IPIV,X,N,INFO )
	IF ( info .NE. 0 ) PRINT*,'ZGETRS: ', info

	END
