subroutine about(numH, numSP, numR, totalRr, numARi, error)

implicit none
integer                                                             :: i, j, k, ii, jj, kk, h
integer                                                             :: totalnumR
integer, intent(in)                                                 :: numH, numSP
integer, intent(out)                                                :: error
integer, dimension(numH)                                            :: numAR
integer, dimension(numH), intent(out)                               :: numARi
integer, allocatable, dimension(:)                                  :: comb
integer, allocatable, dimension(:,:)                                :: ARline
integer, dimension(numH*2), intent(in)                              :: numR

double precision, allocatable, dimension(:,:)                       :: AA
double precision, allocatable, dimension(:,:)                       :: IRr

double precision                                                    :: temp, rec
double precision, dimension(sum(numR),numSP+1)                      :: totalRr
double precision, allocatable, dimension(:,:,:,:)                   :: ARr


error=0
numAR=0
numARi=0 ! number of range constraints
totalnumR=sum(numR)

allocate (ARline(numH,totalnumR),ARr(numH,totalnumR,totalnumR,numSP+1))

do h = 1,numH ! numH means the number of hypotheses under consideration.

allocate (IRr(numR(2*h),numSP+1),AA(numR(2*h),numR(2*h)))

do i = 1, numR(2*h)
IRr(i,1:(numSP+1))=totalRr(sum(numR(1:(2*h-1)))+i,1:(numSP+1))
end do

numAR(h)=brank(IRr,numR(2*h),numSP+1)-brank(IRr(1:numR(2*h),1:numSP),numR(2*h),numSP)

if (numAR(h)>0) then ! range constraints or invalid constraints
AA=0
do i = 2, numR(2*h)
allocate(comb(i))
call combine(1,numR(2*h)-i+1,1)
deallocate(comb)
end do

if (numARi(h)<numAR(h)) then
error=1
go to 999
end if

do ii = 1, numARi(h)  ! number of range constraints
jj=1
do i = 1, ARline(h,ii) ! the ii th range constraints in hypothesis h
do j =1, ARline(h,ii)
rec=0
do k = 1, numSP
if (ARr(h,ii,i,k)+ARr(h,ii,j,k)==0 .or. ARr(h,ii,i,k)==0) then
rec=rec+1
end if
end do

if (rec==numSP .and. jj.LE.ARline(h,ii)) then
do k=1,(numSP+1)
temp=ARr(h,ii,jj,k)
ARr(h,ii,jj,k)=ARr(h,ii,i,k)
ARr(h,ii,i,k)=temp
end do
jj=jj+ARline(h,ii)-1
end if
end do
end do


kk=0
do j=0,numR(2*h)-1
if(kk<2) then
if(all(totalRr(sum(numR(1:(2*h)))-j,1:numSP+1)==ARr(h,ii,1,1:numSP+1)) .or.  &
   all(totalRr(sum(numR(1:(2*h)))-j,1:numSP+1)== &
       ARr(h,ii,ARline(h,ii),1:numSP+1))) then
totalRr(sum(numR(1:(2*h)))-j,numSP+1)=totalRr(sum(numR(1:(2*h)))-j,numSP+1)- &
  sum(ARr(h,ii,1:ARline(h,ii),numSP+1))/2
kk=kk+1
end if
end if
end do
end do

end if
deallocate(IRr,AA)
end do


ii=0

999 deallocate(ARline,ARr)


contains

recursive subroutine combine(a,b,c)
integer,intent(in) :: a,b,c
integer :: j,ii,jj,kk,rec
integer, dimension(:), allocatable   :: vectcount
allocate(vectcount(numSP))

do j=a,b
comb(c)=j
if(c.LT.i) then
call combine(j+1,b+1,c+1)
end if
if(c.EQ.i) then
vectcount=0
do k = 1, i
vectcount=vectcount+int(IRr(comb(k),1:numSP))
end do

kk=0
if (numARi(h)>1) then
do ii = 1, numARi(h)-1
rec=0
do k = 1,i
do jj = 1, ARline(h,ii)

if(comb(k)==AA(ii,jj)) then
rec=rec+1
end if
end do
end do
if (rec==ARline(h,ii)) then
kk=1
end if
end do
end if

if (sum(abs(vectcount))==0 .and. kk.NE.1) then
numARi(h)=numARi(h)+1
ARline(h,numARi(h))=i
do k = 1, i
ARr(h,numARi(h),k,1:numSP+1)=IRr(comb(k),1:numSP+1)  ! record lines
AA(numARi(h),k)=comb(k)                              ! record combinations
end do
end if
end if
end do
end subroutine

function brank(M,nrow,ncol)

implicit none
integer                                                        :: i, j, k
integer                                                        :: nrow, ncol
integer                                                        :: brank

double precision                                               :: temp
double precision, dimension(nrow,ncol), intent(in)             :: M
double precision, dimension(nrow,ncol)                         :: A

brank=nrow
A=M

do j=1,(ncol-1)
do k=(j+1),ncol
if(all(A(1:nrow,j).EQ.0).and.any(A(1:nrow,k).NE.0)) then
A(1:nrow,j)=A(1:nrow,k)
A(1:nrow,k)=0
end if
end do
end do

do i=1,min(nrow,ncol)
do k=i+1,nrow
if (A(i,i).EQ.0 .and. A(k,i).NE.0) then
do j=1,ncol
temp=A(i,j)
A(i,j)=A(k,j)
A(k,j)=temp
end do
end if
end do

if (A(i,i).NE.0) then
temp=A(i,i)
do j=1,ncol
A(i,j)=A(i,j)/temp
end do
do k=1,nrow
if (k.NE.i) then
temp=A(k,i)
do j=1,ncol
A(k,j)=A(k,j)-A(i,j)*temp
end do
end if
end do
end if
end do

do i=1,nrow
if (sum(abs(A(i,1:ncol))).EQ.0) then
brank=brank-1
end if
end do
return
end function


end subroutine




subroutine mrank(numIR, numSP, rowrank, IRr, transR, constant, transcon)

implicit none
integer                                                        :: i, j, k, ii
integer, allocatable, dimension(:)                             :: rownumber
integer, intent(in)                                            :: numIR, numSP
integer, intent(out)                                           :: rowrank

double precision                                               :: temp
double precision, dimension(numIR), intent(in)                 :: constant
double precision, dimension(numIR), intent(out)                :: transcon
double precision, allocatable, dimension(:,:)                  :: A, Ainv
double precision, dimension(numIR,numSP+1), intent(inout)      :: IRr
double precision, dimension(numIR,numIR), intent(out)          :: transR


if (numIR>numSP) then
allocate (A(numIR,numIR), Ainv(numIR,numIR), rownumber(numIR))
else
  allocate (A(numIR,numSP), Ainv(numIR,numSP), rownumber(numIR))
end if

A=0
Ainv=0
rownumber=1
ii=1
do i = 1, numSP
if (sum(abs(IRr(1:numIR,i))).NE.0) then
A(1:numIR,ii)=IRr(1:numIR,i)
ii=ii+1
end if
end do

do i = 1,numIR
Ainv(i,i)=1
if (i>1) then
rownumber(i)=rownumber(i-1)+1
end if
end do
rowrank=numIR
do i=1,numIR
do k=i+1,numIR
if (A(i,i).EQ.0 .and. A(k,i).NE.0) then
do j=1,max(numIR,numSP)
temp=A(i,j)
A(i,j)=A(k,j)
A(k,j)=temp
temp=Ainv(i,j)
Ainv(i,j)=Ainv(k,j)
Ainv(k,j)=temp
end do
do j=1,(numSP+1)
temp=IRr(i,j)
IRr(i,j)=IRr(k,j)
IRr(k,j)=temp
end do
temp=rownumber(i)
rownumber(i)=rownumber(k)
rownumber(k)=int(temp)
end if
end do
if (A(i,i).NE.0) then
temp=A(i,i)
do j=1,max(numIR,numSP)
A(i,j)=A(i,j)/temp
Ainv(i,j)=Ainv(i,j)/temp
end do
do k=1,numIR
if (k.NE.i) then
temp=A(k,i)
do j=1,max(numIR,numSP)
A(k,j)=A(k,j)-A(i,j)*temp
Ainv(k,j)=Ainv(k,j)-Ainv(i,j)*temp
end do
end if
end do
end if
end do



do i=1,numIR
if (sum(abs(A(i,1:numSP))).EQ.0) then
rowrank=rowrank-1
end if
end do

do i=1,numIR
do k=i+1,numIR
if (sum(abs(A(i,1:numSP))).EQ.0 .and. sum(abs(A(k,1:numSP))).NE.0) then
do j=1,max(numIR,numSP)
temp=A(i,j)
A(i,j)=A(k,j)
A(k,j)=temp
temp=Ainv(i,j)
Ainv(i,j)=Ainv(k,j)
Ainv(k,j)=temp
end do
do j=1,(numSP+1)
temp=IRr(i,j)
IRr(i,j)=IRr(k,j)
IRr(k,j)=temp
end do
temp=rownumber(i)
rownumber(i)=rownumber(k)
rownumber(k)=int(temp)
end if
end do
end do

do i=1,numIR
do j=1,numIR
transR(i,j)=Ainv(i,rownumber(j))
end do

transcon(i)=sum(Ainv(i,1:numIR)*constant(1:numIR))
end do

deallocate(A,Ainv,rownumber)
end subroutine







subroutine forc(numER,numIR,rowrank,bet,transcon,invbetadiag,B,transR,f_or_c,Numfc,seed)

implicit none

integer                                                                :: N, i, j, k, time, nn
integer, intent(in)                                                    :: seed
integer, allocatable, dimension(:)                                     :: iseed
integer, intent(in)                                                    :: numER, numIR, rowrank
integer, dimension(numIR)                                              :: Num
integer, intent(out)                                                   :: Numfc

double precision                                                       :: lower, upper
double precision                                                       :: large, small
double precision                                                       :: temp, p, GG
double precision, dimension(numER+rowrank), intent(in)                 :: bet, invbetadiag
double precision, dimension(numIR), intent(in)                         :: transcon
double precision, dimension(numER+rowrank)                             :: gibbsample, conmeans
double precision, dimension(numIR)                                     :: df_or_dc
double precision, dimension(numER+rowrank,numER+rowrank), intent(in)   :: B
double precision, dimension(numIR,numIR), intent(in)                   :: transR
double precision, intent(out)                                          :: f_or_c



call RANDOM_SEED(size=nn)
allocate(iseed(nn))
iseed(:)=seed
call RANDOM_SEED(put=iseed)

do k = 1,numIR

if (k.LE.rowrank) then
N=3000   !initial sample size
777  df_or_dc(k)=0
gibbsample=0 ! Initial value of the gibbs sampling
do time=1,N+100 ! "time" denotes the iteration index in the gibbs sampling, 100 denotes the burn-in period.
do i=1,(numER+rowrank)  ! numR betas need to be sampled

! "conmeans" is the mean of the conditional posterior normal distribution
conmeans(i)=bet(i)+sum(B(i,1:numER+rowrank)*(gibbsample(1:numER+rowrank)-bet(1:numER+rowrank))) &
  -B(i,i)*(gibbsample(i)-bet(i))

if (i.LE.numER) then
gibbsample(i)=0
else if (i<(k+numER)) then ! When computing the probability of beta_{k}>0, beta_{i},i=1,...,k-1 have to be constrained (larger than 0)

! lower bound is 0, but it has to be standardized.
lower=(0-conmeans(i))/sqrt(1/invbetadiag(i))
call random_number(GG)! This function generates a pseudorandom number from a uniform (0,1) distribution

! "cumnor" returns a probability that a normal random variable takes a value less than or equal to "lower"
p=cumnor(lower)+GG*(1-cumnor(lower))

if (p>0.9999999) then
p=0.9999999  ! to avoid infinity
end if

! "dinvnr" evaluates the inverse of the strandard normal distribution function.
temp=dinvnr(p)
! Gibbs sample of beta_{k} is then obtained
gibbsample(i)=temp*sqrt(1/invbetadiag(i))+conmeans(i)
else ! if i>k, sample is unconstrained.
gibbsample(i)=rnormal()*sqrt(1/invbetadiag(i))+conmeans(i)
end if
end do   ! one iteration of gibbs sample ends.

! Compute Pr(beta_{k}>0 | (beta_{1},...,beta_{k-1})>0)
if (time>100) then ! discard first 100 sample
temp=conmeans(numER+k)/sqrt(1/invbetadiag(numER+k))
! ANORDF(temp) is the probability Pr(beta_{k}>0|beta_{i}^{t}>0,i<k)
df_or_dc(k)=df_or_dc(k)+cumnor(temp)/real(N) ! We sum Pr(beta_{k}>0)/N and obtain the mean of Pr(beta_{k}>0).
end if
end do


! Reset the number of iterations (sample size T) based on the value of the estimation.
if (df_or_dc(k)<0.166 .AND. N==3000) then
N=8000
go to 777
else if (df_or_dc(k)<0.0417 .AND. N==8000) then
N=40000
go to 777
else if (df_or_dc(k)<0.00833 .AND. N==40000) then
N=96000
go to 777
else if (df_or_dc(k)<0.00139 .AND. N==96000) then
N=224000
go to 777
else if (df_or_dc(k)<0.000198 .AND. N==224000) then
N=512000
go to 777
else if (df_or_dc(k)<0.0000248 .AND. N==512000) then
N=1000000
go to 777
end if
Num(k)=N ! final sample size in "k" constraints

else

  N=3000   !initial sample size
888  df_or_dc(k)=0
gibbsample=0 ! Initial value of the gibbs sampling
do time=1,N+100 ! "time" denotes the iteration index in the gibbs sampling, 100 denotes the burn-in period.
do i=1,(numER+rowrank)  ! numR betas need to be sampled

! "conmeans" is the mean of the conditional posterior normal distribution
conmeans(i)=bet(i)+sum(B(i,1:rowrank)*(gibbsample(1:rowrank)-bet(1:rowrank))) &
  -B(i,i)*(gibbsample(i)-bet(i))
if (i.LE.numER) then
gibbsample(i)=0
else
  large=0
small=100000000

if (k>(numER+rowrank+1)) then
do j=(rowrank+1),(k-1)
if (transR(j,i)<0) then
large=max(large,(transR(j,i-numER)*gibbsample(i)  &
                   -sum(transR(j,1:rowrank)*gibbsample(numER+1:numER+rowrank))-transcon(j))/transR(j,i-numER))
end if
if (transR(j,i)>0) then
small=min(small,(transR(j,i-numER)*gibbsample(i)  &
                   -sum(transR(j,1:rowrank)*gibbsample(numER+1:numER+rowrank))-transcon(j))/transR(j,i-numER))
end if
end do
end if
lower=(large-conmeans(i))/sqrt(1/invbetadiag(i))
upper=(small-conmeans(i))/sqrt(1/invbetadiag(i))
call random_number(GG) ! This function generates a pseudorandom number from a uniform (0,1) distribution
! "cumnor" returns a probability that a normal random variable takes a value less than or equal to "lower"
p=cumnor(lower)+GG*(cumnor(upper)-cumnor(lower))

if (p>0.9999999) then
p=0.9999999  ! to avoid infinity
end if

if (p<0.0000001) then
p=0.0000001  ! to avoid -infinity
end if

! "dinvnr" evaluates the inverse of the strandard normal distribution function.
temp=dinvnr(p)
! Gibbs sample of beta_{k} is then obtained
gibbsample(i)=temp*sqrt(1/invbetadiag(i))+conmeans(i)
end if
end do   ! one iteration of gibbs sample ends.

! Compute Pr(beta_{k}>0 | (beta_{1},...,beta_{k-1})>0)
if (time>100) then ! discard first 100 sample
if (-sum(transR(k,1:rowrank)*gibbsample(numER+1:numER+rowrank))>transcon(k)) then
df_or_dc(k)=df_or_dc(k)+1/real(N)
end if
end if
end do

if (df_or_dc(k)<0.166 .AND. N==3000) then
N=9600
go to 888
else if (df_or_dc(k)<0.042 .AND. N==9600) then
N=120000
go to 888
else if (df_or_dc(k)<0.008 .AND. N==120000) then
N=360000
go to 888
else if (df_or_dc(k)<0.0014 .AND. N==360000) then
N=2520000
go to 888
else if (df_or_dc(k)<0.0002 .AND. N==2520000) then
N=10000000
go to 888
end if
Num(k)=N
end if

f_or_c=product(df_or_dc)
Numfc=sum(Num)

end do


contains

function rnormal ()

!*****************************************************************************80
!
  !! RNORMAL returns a unit pseudonormal R8.
!
  !  Discussion:
  !
  !    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license.
!
  !  Modified:
  !
  !    06 August 2013
!
  !  Author:
  !
  !    John Burkardt
!
  !  Parameters:
  !
  !
  !    Output, real ( kind = 8 ) RNORMAL, a normally distributed
!    random value.
!
  implicit none

real ( kind = 8 ) r1
real ( kind = 8 ) r2
real ( kind = 8 ) rnormal
real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
real ( kind = 8 ) GG
real ( kind = 8 ) x

call random_number(GG)
r1 = GG
call random_number(GG)
r2 = GG
x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * pi * r2 )

rnormal = x

return
end function



function cumnor ( arg )

!*****************************************************************************80
! The original code was modified
!
  !
  !! CUMNOR computes the cumulative normal distribution.
!
  !  Discussion:
  !
  !    This function evaluates the normal distribution function:
  !
  !                              / x
!                     1       |       -t*t/2
!          P(x) = ----------- |      e       dt
!                 sqrt(2 pi)  |
  !                             /-oo
!
  !    This transportable program uses rational functions that
!    theoretically approximate the normal distribution function to
!    at least 18 significant decimal digits.  The accuracy achieved
!    depends on the arithmetic system, the compiler, the intrinsic
!    functions, and proper selection of the machine dependent
!    constants.
!
  !  Author:
  !
  !    William Cody
!    Mathematics and Computer Science Division
!    Argonne National Laboratory
!    Argonne, IL 60439
!
  !  Reference:
  !
  !    William Cody,
!    Rational Chebyshev approximations for the error function,
!    Mathematics of Computation,
!    1969, pages 631-637.
!
  !    William Cody,
!    Algorithm 715:
  !    SPECFUN - A Portable FORTRAN Package of Special Function Routines
!    and Test Drivers,
!    ACM Transactions on Mathematical Software,
!    Volume 19, Number 1, 1993, pages 22-32.
!
  !  Parameters:
  !
  !    Input, real ( kind = 8 ) ARG, the upper limit of integration.
!
  !    Output, real ( kind = 8 ) the Normal density CDF.
!
  !  Local Parameters:
  !
  !    Local, real ( kind = 8 ) EPS, the argument below which anorm(x)
!    may be represented by 0.5 and above which  x*x  will not underflow.
!    A conservative value is the largest machine number X
!    such that   1.0D+00 + X = 1.0D+00   to machine precision.
!
  implicit none

real ( kind = 8 ), parameter, dimension ( 5 ) :: a = (/ &
                                                        2.2352520354606839287D+00, &
                                                        1.6102823106855587881D+02, &
                                                        1.0676894854603709582D+03, &
                                                        1.8154981253343561249D+04, &
                                                        6.5682337918207449113D-02 /)
real ( kind = 8 ) arg
real ( kind = 8 ), parameter, dimension ( 4 ) :: b = (/ &
                                                        4.7202581904688241870D+01, &
                                                        9.7609855173777669322D+02, &
                                                        1.0260932208618978205D+04, &
                                                        4.5507789335026729956D+04 /)
real ( kind = 8 ), parameter, dimension ( 9 ) :: c = (/ &
                                                        3.9894151208813466764D-01, &
                                                        8.8831497943883759412D+00, &
                                                        9.3506656132177855979D+01, &
                                                        5.9727027639480026226D+02, &
                                                        2.4945375852903726711D+03, &
                                                        6.8481904505362823326D+03, &
                                                        1.1602651437647350124D+04, &
                                                        9.8427148383839780218D+03, &
                                                        1.0765576773720192317D-08 /)
real ( kind = 8 ) cumnor
real ( kind = 8 ), parameter, dimension ( 8 ) :: d = (/ &
                                                        2.2266688044328115691D+01, &
                                                        2.3538790178262499861D+02, &
                                                        1.5193775994075548050D+03, &
                                                        6.4855582982667607550D+03, &
                                                        1.8615571640885098091D+04, &
                                                        3.4900952721145977266D+04, &
                                                        3.8912003286093271411D+04, &
                                                        1.9685429676859990727D+04 /)
real ( kind = 8 ) del
real ( kind = 8 ) eps
integer ( kind = 4 ) i
real ( kind = 8 ), parameter, dimension ( 6 ) :: p = (/ &
                                                        2.1589853405795699D-01, &
                                                        1.274011611602473639D-01, &
                                                        2.2235277870649807D-02, &
                                                        1.421619193227893466D-03, &
                                                        2.9112874951168792D-05, &
                                                        2.307344176494017303D-02 /)
real ( kind = 8 ), parameter, dimension ( 5 ) :: q = (/ &
                                                        1.28426009614491121D+00, &
                                                        4.68238212480865118D-01, &
                                                        6.59881378689285515D-02, &
                                                        3.78239633202758244D-03, &
                                                        7.29751555083966205D-05 /)
real ( kind = 8 ), parameter :: root32 = 5.656854248D+00
real ( kind = 8 ), parameter :: sixten = 16.0D+00
real ( kind = 8 ), parameter :: sqrpi = 3.9894228040143267794D-01
real ( kind = 8 ), parameter :: thrsh = 0.66291D+00
real ( kind = 8 ) x
real ( kind = 8 ) xden
real ( kind = 8 ) xnum
real ( kind = 8 ) y
real ( kind = 8 ) xsq
!
  !  Machine dependent constants
!
  eps = epsilon ( 1.0D+00 ) * 0.5D+00

x = arg
y = abs ( x )

if ( y <= thrsh ) then
!
  !  Evaluate  anorm  for  |X| <= 0.66291
!
  if ( eps < y ) then
xsq = x * x
else
  xsq = 0.0D+00
end if

xnum = a(5) * xsq
xden = xsq
do i = 1, 3
xnum = ( xnum + a(i) ) * xsq
xden = ( xden + b(i) ) * xsq
end do
cumnor = x * ( xnum + a(4) ) / ( xden + b(4) )
cumnor = 0.5D+00 + cumnor
!
  !  Evaluate ANORM for 0.66291 <= |X| <= sqrt(32)
!
  else if ( y <= root32 ) then

xnum = c(9) * y
xden = y
do i = 1, 7
xnum = ( xnum + c(i) ) * y
xden = ( xden + d(i) ) * y
end do
cumnor = ( xnum + c(8) ) / ( xden + d(8) )
xsq = aint ( y * sixten ) / sixten
del = ( y - xsq ) * ( y + xsq )
cumnor = exp ( - xsq * xsq * 0.5D+00 ) * exp ( -del * 0.5D+00 ) * cumnor

if ( 0.0D+00 < x ) then
cumnor = 1D+00 - cumnor
end if
!
  !  Evaluate ANORM for sqrt(32) < |X|.
!
  else

    cumnor = 0.0D+00
xsq = 1.0D+00 / ( x * x )
xnum = p(6) * xsq
xden = xsq
do i = 1, 4
xnum = ( xnum + p(i) ) * xsq
xden = ( xden + q(i) ) * xsq
end do

cumnor = xsq * ( xnum + p(5) ) / ( xden + q(5) )
cumnor = ( sqrpi - cumnor ) / y
xsq = aint ( x * sixten ) / sixten
del = ( x - xsq ) * ( x + xsq )
cumnor = exp ( - xsq * xsq * 0.5D+00 ) &
  * exp ( - del * 0.5D+00 ) * cumnor

if ( 0.0D+00 < x ) then
cumnor = 1D+00 - cumnor
end if

end if

if ( cumnor < tiny ( cumnor ) ) then
cumnor = 0.0D+00
end if

return
end function

function dinvnr ( p )

!*****************************************************************************80
!
  !! DINVNR computes the inverse of the normal distribution.
!
  !  Discussion:
  !
  !    This routine returns X such that
!
  !      CUMNOR(X) = P,
!
  !    that is, so that
!
  !      P = integral ( -oo <= T <= X ) exp(-U*U/2)/sqrt(2*PI) dU
!
  !    The rational function is used as a
!    starting value for the Newton method of finding roots.
!
  !  Reference:
  !
  !    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980,
!    QA276.4 K46
!
  !  Parameters:
  !
  !    Input, real ( kind = 8 ) P.
!
  !    Output, real ( kind = 8 ) DINVNR, the argument X for which the
!    Normal CDF has the value P.
!
  implicit none

real ( kind = 8 ) cum
real ( kind = 8 ) dinvnr
real ( kind = 8 ) dx
real ( kind = 8 ), parameter :: eps = 1.0D-13
integer ( kind = 4 ) i
integer ( kind = 4 ), parameter :: maxit = 100
real ( kind = 8 ) p
real ( kind = 8 ) pp
real ( kind = 8 ), parameter :: r2pi = 0.3989422804014326D+00
real ( kind = 8 ) strtx
! real ( kind = 8 ) stvaln
real ( kind = 8 ) xcur

pp = min ( p, 1-p )
strtx = stvaln ( pp )
xcur = strtx
!
  !  Newton iterations.
!
  do i = 1, maxit

cum = cumnor ( xcur )
dx = ( cum - pp ) / ( r2pi * exp ( -0.5D+00 * xcur * xcur ) )
xcur = xcur - dx

if ( abs ( dx / xcur ) < eps ) then
if ( p <= 1-p ) then
dinvnr = xcur
else
  dinvnr = -xcur
end if
return
end if

end do

if ( p <= 1-p ) then
dinvnr = strtx
else
  dinvnr = -strtx
end if

return
end function

function stvaln ( p )

!*****************************************************************************80
!
  !! STVALN provides starting values for the inverse of the normal distribution.
!
  !  Discussion:
  !
  !    The routine returns an X for which it is approximately true that
!      P = CUMNOR(X),
!    that is,
!      P = Integral ( -infinity < U <= X ) exp(-U*U/2)/sqrt(2*PI) dU.
!
  !  Reference:
  !
  !    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980, page 95,
!    QA276.4 K46
!
  !  Parameters:
  !
  !    Input, real ( kind = 8 ) P, the probability whose normal deviate
!    is sought.
!
  !    Output, real ( kind = 8 ) STVALN, the normal deviate whose probability
!    is approximately P.
!
  implicit none

!  real ( kind = 8 ) eval_pol
real ( kind = 8 ) p
real ( kind = 8 ) sgn
real ( kind = 8 ) stvaln
real ( kind = 8 ), parameter, dimension(0:4) :: xden = (/ &
                                                          0.993484626060D-01, &
                                                          0.588581570495D+00, &
                                                          0.531103462366D+00, &
                                                          0.103537752850D+00, &
                                                          0.38560700634D-02 /)
real ( kind = 8 ), parameter, dimension(0:4) :: xnum = (/ &
                                                          -0.322232431088D+00, &
                                                          -1.000000000000D+00, &
                                                          -0.342242088547D+00, &
                                                          -0.204231210245D-01, &
                                                          -0.453642210148D-04 /)
real ( kind = 8 ) y
real ( kind = 8 ) z

if ( p <= 0.5D+00 ) then

sgn = -1.0D+00
z = p

else

  sgn = 1.0D+00
z = 1.0D+00 - p

end if

y = sqrt ( -2.0D+00 * log ( z ) )
stvaln = y + eval_pol ( xnum, 4, y ) / eval_pol ( xden, 4, y )
stvaln = sgn * stvaln

return
end function

function eval_pol ( a, n, x )

!*****************************************************************************80
!
  !! EVAL_POL evaluates a polynomial at X.
!
  !  Discussion:
  !
  !    EVAL_POL = A(0) + A(1)*X + ... + A(N)*X**N
!
  !  Modified:
  !
  !    15 December 1999
!
  !  Parameters:
  !
  !    Input, real ( kind = 8 ) A(0:N), coefficients of the polynomial.
!
  !    Input, integer ( kind = 4 ) N, length of A.
!
  !    Input, real ( kind = 8 ) X, the point at which the polynomial
!    is to be evaluated.
!
  !    Output, real ( kind = 8 ) EVAL_POL, the value of the polynomial at X.
!
  implicit none

integer ( kind = 4 ) n

real ( kind = 8 ) a(0:n)
real ( kind = 8 ) eval_pol
integer ( kind = 4 ) i
real ( kind = 8 ) term
real ( kind = 8 ) x

term = a(n)
do i = n - 1, 0, -1
term = term * x + a(i)
end do

eval_pol = term

return
end function


end subroutine
