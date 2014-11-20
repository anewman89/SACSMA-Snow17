module gauge_calib

  interface 

    subroutine julian_day(year,month,day,jday)
      use nrtype
      !input variables
      integer(I4B),dimension(:),intent(in) :: year
      integer(I4B),dimension(:),intent(in) :: month
      integer(I4B),dimension(:),intent(in) :: day

      !output variables
      integer(I4B),dimension(:),intent(out) :: jday

    end subroutine julian_day


    subroutine julianday_scalar(iyear,imonth,iday,jday_scalar)
      use nrtype

      integer(I4B), intent(in) :: iyear
      integer(I4B), intent(in) :: imonth
      integer(I4B), intent(in) :: iday

      integer(I4B),intent(out) :: jday_scalar
    end subroutine julianday_scalar


    subroutine read_namelist(namelist_name)
      use nrtype
      !input variable
      character(:),intent(in)	:: namelist_name

    end subroutine read_namelist

    subroutine spin_up_first_year(a, params_in,spinup_crit, in_tair,in_precip,in_pet, &
                                  in_raim,uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc)
      use nrtype

      real(dp), intent(in)		:: spinup_crit
      real(dp), dimension(36500), intent(in):: in_tair,in_precip,in_pet,in_raim
      real(sp),dimension(30),intent(in)	:: params_in


      real(sp), dimension(30), intent(inout):: a
 
      real(dp), intent(out)		:: uztwc,uzfwc,lztwc
      real(dp), intent(out)		:: lzfsc,lzfpc,adimc
    end subroutine spin_up_first_year

    subroutine sfc_pressure(elev, pres)
      use nrtype
      use constants, only: sfc_pres_a,sfc_pres_b,sfc_pres_c,&
                       sfc_pres_d,sfc_pres_e

      real(dp), intent(in)		:: elev

      real(dp), intent(out)		:: pres
    end subroutine sfc_pressure


    subroutine calc_pet_pt(jday,tmax,tmin,tair,vpd,swdown,dayl,pet)
      use constants
      use nrtype
      use snow17_sac, only: elev,lat,pet_coef

      !input variable
      integer(I4B), dimension(:), intent(in) 	:: jday
      real(dp), dimension(:), intent(in) 	:: tair
      real(dp), dimension(:), intent(in) 	:: tmax
      real(dp), dimension(:), intent(in) 	:: tmin
      real(dp), dimension(:), intent(in) 	:: vpd
      real(dp), dimension(:), intent(in) 	:: swdown
      real(dp), dimension(:), intent(in) 	:: dayl

      !output variable
      real(dp),dimension(:),intent(out)	:: pet
    end subroutine calc_pet_pt

    subroutine get_sim_length(sim_length)
      use nrtype
      use snow17_sac, only: forcing_name, start_year,start_day,start_month, &
                        end_year,end_month,end_day

      integer(I4B), intent(out)	:: sim_length	
    end subroutine get_sim_length


    subroutine read_streamflow(streamflow)
      use nrtype
      use constants,  only: cfs_cms, sec_day
      use snow17_sac, only: stream_name,area_basin

      !output variables
      real(dp),dimension(:),intent(out)	:: streamflow
    end subroutine read_streamflow


    subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,vpd,dayl,swdown,precip)
      use nrtype
      use snow17_sac, only: forcing_name, start_year,start_day,start_month, &
                        end_year,end_month,end_day,lat,area_basin,elev

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
    end subroutine read_areal_forcing



  end interface

end module gauge_calib