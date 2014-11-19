subroutine get_sim_length(sim_length)
  use nrtype
  use snow17_sac, only: forcing_name, start_year,start_day,start_month, &
                        end_year,end_month,end_day

  !output variables
  integer(I4B),intent(out)	:: sim_length

  !local variables
  integer(I4B)			:: ios
  integer(I4B)			:: dum_int
  integer(I4B)			:: year,month,day
  integer(I4B)			:: read_flag

  character(len=64), parameter		:: read_format = "(I4.4, 3(1x,I2.2),1x,7(F10.2))"
  character(len = 1024)			:: dum_str
  real(dp)		:: dum_real

  

!code
  read_flag = 0
  sim_length = 0

  open (UNIT=50,file=trim(forcing_name),form='formatted',status='old')

  read (UNIT=50,FMT='(F7.2)') dum_real
  read (UNIT=50,FMT='(F7.2)') dum_real
  read (UNIT=50,FMT='(F11.0)') dum_real
  read (UNIT=50,FMT='(80A)') dum_str


  do while(ios .ge. 0)
    read (UNIT=50,FMT=read_format,IOSTAT=ios) year,month,day,dum_int,&
				dum_real,dum_real,dum_real,dum_real,dum_real,&
				dum_real,dum_real

    if(year .eq. start_year .and. month .eq. start_month .and. day .eq. start_day) then
      read_flag = 1
    end if

    if(read_flag .eq. 1) then
      sim_length = sim_length + 1
    end if

    if(year .eq. end_year .and. month .eq. end_month .and. day .eq. end_day) then
      read_flag = 0
    end if
  end do

  close(unit=50)

end subroutine get_sim_length

!ccccccccccccccccccccccccccccccc

subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,vpd,dayl,swdown,precip)
  use nrtype
  use snow17_sac, only: forcing_name, start_year,start_day,start_month, &
                        end_year,end_month,end_day,lat,area_basin,elev

  implicit none

!output variables
  integer(I4B),dimension(:),intent(out)	:: year
  integer(I4B),dimension(:),intent(out)	:: month
  integer(I4B),dimension(:),intent(out)	:: day
  integer(I4B),dimension(:),intent(out)	:: hour
  real(dp),dimension(:),intent(out)	:: tmin
  real(dp),dimension(:),intent(out)	:: tmax
  real(dp),dimension(:),intent(out)	:: vpd
  real(dp),dimension(:),intent(out)	:: dayl
  real(dp),dimension(:),intent(out)	:: swdown
  real(dp),dimension(:),intent(out)	:: precip

!local variables
  integer(I4B)				:: i,ios
  integer(I4B)				:: yr,mnth,dy,hr
  integer(I4B)				:: read_flag

  character(len=64), parameter		:: read_format = "(I4.4, 3(1x,I2.2),1x,7(F10.2))"
  character(len = 1024)			:: dum_str
  real(DP)				:: swe
  real(DP)				:: dum_real

  real(DP)				:: dl,pcp,sw,tma,tmn,vp




!code
  i = 1
!read met file
  open (UNIT=50,file=trim(forcing_name),form='formatted',status='old')

  read (UNIT=50,FMT='(F7.2)') lat
  read (UNIT=50,FMT='(F7.2)') elev
  read (UNIT=50,FMT='(F11.0)') area_basin
  read (UNIT=50,FMT='(80A)') dum_str
  !read the rest of the input file
  !forcing_offset is the first day of the first complete water year in corresponding observed streamflow
  !this is the point at which we want to keep the forcing data
  !keep through the end of the sim_len or end of file (if validation period)
  !need to do this because forcing starts 01 Jan 1980
  !observed streamflow varies in its start date by gauge

  do while(ios .ge. 0)
    read (UNIT=50,FMT=read_format,IOSTAT=ios) yr,mnth,dy,hr,&
			    dl,pcp,sw,swe,tma,&
			    tmn,vp

    if(yr .eq. start_year .and. mnth .eq. start_month .and. dy .eq. start_day) then
      read_flag = 1
    end if

    if(read_flag .eq. 1) then
      year(i)	= yr
      month(i)	= mnth
      day(i)	= dy
      hour(i)	= hr
      dayl(i)	= dl
      precip(i)	= pcp
      swdown(i) = sw
      tmax(i)	= tma
      tmin(i)	= tmn
      vpd(i)	= vp
      i = i + 1

    end if

    if(yr .eq. end_year .and. mnth .eq. end_month .and. dy .eq. end_day) then
      read_flag = 0
    end if

  end do

  close(unit=50)

  return

end subroutine read_areal_forcing

!cccccccccccccccccccccccccccccccccccccccccc
!change & simplify
subroutine read_streamflow(streamflow)
  use nrtype
  use constants, only: sec_day,cfs_cms
  use snow17_sac, only: stream_name,area_basin,start_year,start_day,start_month, &
			end_year,end_month,end_day

  implicit none

!output variables
  real(dp),dimension(:),intent(out)	:: streamflow

!local variables
  integer(I4B) :: i
  integer(I4B) :: yr,mn,dy,gauge,ios,error
  real(dp)    :: flow

  logical	:: valid

!this subroutine assumes streamflow is daily data of the following format:
  character(len=64), parameter :: read_format = "(I8.8,1x,I4.4, 2(1x,I2.2),1x,1(F8.2))"


!code
  valid = .false.
  i = 1

! open streamflow file
  open (UNIT=50,file=stream_name,form='formatted',status='old')


  do while(ios .ge. 0)
    read (UNIT=50,FMT=read_format,IOSTAT=ios) gauge,yr,mn,dy,flow

    if(yr .eq. start_year .and. mn .eq. start_month .and. dy .eq. start_day) then
      valid = .true.
    end if

    if(valid) then
      streamflow(i)	= flow

      !convert flow to mm/day
      !convert streamflow (cfs) to cms
      streamflow(i) = streamflow(i)*cfs_cms  !now in cubic meters per second

      !need to convert to mm/day
      streamflow(i) = streamflow(i)*sec_day !now in cubic meters per day m^3 day^-1
                                           !m^3/day

      !1 cubic meter per day is 1000 mm per square m -> mm*m^2/day
      streamflow(i) = streamflow(i)*1000./area_basin  !now in mm/day
      
      i = i + 1
    end if

    if(yr .eq. end_year .and. mn .eq. end_month .and. dy .eq. end_day) then
      valid = .false.
    end if

  end do

  close(unit=50)

  return

end subroutine read_streamflow
