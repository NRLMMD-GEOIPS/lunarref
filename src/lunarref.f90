subroutine lunarref(in_dnb_rad, solar_zenith, lunar_zenith, yyyymmddhh, minute, lunar_phase, &
                          lunar_ref, lines, samples, in_dat_path)!, in_city_lights)

    !***************************************************************
    !
    ! 8/14/2012 Jeremy Solbrig (jeremys.solbrig@nrlmry.navy.mil)
    !
    ! To compile for Python:
    ! f2py --fcompiler=gfortran -c -m lunarref lunarref.f90
    !
    ! To compile for Fortran:
    ! gfortran lunarref.f90
    ! gfortran viirs_lunar_reflectance.f90 lunarref.o -o viirs_lunar_reflectance
    !
    ! Ported from Steve Miller's code to work with f2py.
    ! This allows the routine to be compiled as a python module.
    ! Also added another fortran program as a wrapper to this routine
    ! which should allow it to be called the same as the original fortran
    ! program in order to create similarity between CIRA and NRL code for
    ! easier porting in the future.
    !
    ! Inputs:
    !   in_dnb_rad     - Array of DNB radiances (lines, samples)
    !   solar_zenith   - Array of solar zenith angles (lines, samples)
    !   lunar_zenith   - Array of lunar zenith angles (lines, samples)
    !   yyyymmddhh     - Date and hour of granule
    !   minute         - Minute of granule (float)
    !   lunar_phase    - Moon phase in degrees where 0 is full and 180 is new
    !                    (float)
    !   samples        - Numer of samples in arrays (integer) 
    !                      optional if called from python
    !   lines          - Number of lines in arrays (integer)
    !                      optional if called from python
    !   in_dat_path    - Path to ancillary binary files
    !                    Defaults to <ancildat_path>/src/lunarref/dat
    !                      if called from Python
    !                    Must be supplied if called outside generic processor or by
    !                      Fortran
    !   in_city_lights - Array of city light flags (lines, samples)
    !                    Optional.  Defaults to all zeros.
    !                    Much of the city light code has yet to be implimented.
    !
    ! 5/31/2012 Steve Miller (Steven.Miller@colostate.edu)
    ! 
    ! Computes lunar %-reflectance [0,100] at each pixel for a passed-in dataset
    ! of VIIRS Day/Night band radiances.
    !
    ! OUTPUT: Moonlight %-Reflectance [0.0,100.0] for valid portions of the scene 
    !         Pixels with TOA irradiance below minimum threshold, contaminated, or missing 
    !	  values are reported as -999.0
    !
    ! UNITS:
    ! DNB measurements are in W/cm^2-sr (must multiply by 1.e+04 to convert to  W/m^2-sr)
    ! Results of lunar_irrad are in mW/m^2-um (must divide by 1.e+03 to convert to W/m^2-um)
    ! 	and multiply by 0.329539 (= the integrated SRF, in um) to convert to W/m^2
    !
    ! Reflectance product is a dimensionless quantity, reported here in percent (%): [0,100]
    !
    ! NOTES:
    ! Conditions for this calculation are:
    !  Lunar Irradiance * cos(Lunar Zenith) greater than minimum threshold
    !  Solar zenith angle below astronomical dark threshold (-19 degrees)
    !  Avoid city lights (not yet implemented)
    !
    ! MODIFY PATH TO ANCILLARY DATA
    !
    !***************************************************************
    !***************************************************************

    !***************************************************************
    ! 1. Declare Variables
    !***************************************************************

    use config
    implicit none

    !***************************************************************
    ! inputs
    !***************************************************************
    ! Dataset dimentions
    integer, intent(in)     :: lines
    integer, intent(in)     :: samples
    ! Date, time, and astronomical state
    double precision, intent(in)    :: yyyymmddhh
    real, intent(in)                :: minute
    real, intent(in)                :: lunar_phase     ! Degrees
    ! Input Datasets
    real, dimension(lines, samples), intent(in)     :: in_dnb_rad
    real, dimension(lines, samples), intent(in)     :: solar_zenith
    real, dimension(lines, samples), intent(in)     :: lunar_zenith
    !integer, dimension(lines, samples), optional, intent(in) :: in_city_lights
    real, dimension(lines, samples)                 :: dnb_rad
    !integer, dimension(lines, samples)              :: city_lights
    ! Optional Path to Binary Files
    character(len=256), optional, intent(in) :: in_dat_path

    !***************************************************************
    ! ANCILLARY DATA PATHS AND VARIABLES
    !***************************************************************
    ! Paths to Binary Files
    character(len=256) :: lunar_irrad_file
    character(len=256) :: dist_phase_file
    ! Dataset dimensions
    integer, parameter :: num_irrad_tabvals=181
    integer, parameter :: num_dist_tabvals=184080
    ! Ancillary Datasets
    real, dimension(2, num_irrad_tabvals) :: lunar_irrad_lut
    double precision, dimension(4, num_dist_tabvals) :: dist_phase_lut
    double precision, dimension(num_irrad_tabvals) :: phase_array

    !***************************************************************
    ! INTERNAL PARAMETERS
    !***************************************************************
    ! Datatype sizes in bytes
    integer, parameter :: double_size=8
    integer, parameter :: float_size=4
    integer, parameter :: byte_size=1
    ! Constants
    real, parameter :: badval = -999.9
    real, parameter :: pi = 3.14159
    real, parameter :: dtor = pi/180.0
    real, parameter :: srf_integ = 0.329539 ! Integral of dnb sensor response function

    real, parameter :: earth_radius_km = 6378.140
    double precision, parameter :: mean_earthsun_dist = 149598022.6071
    double precision, parameter :: mean_earthmoon_dist = 384400.0
    real, parameter :: astro_dark_thresh = 109.0
    real, parameter :: min_lunar_irrad_dnb = 1.0E-06 ! W/m^2 = 1.0E-09 W/cm^2
                                                     ! Threshold for doing calcs.
                                                     ! Maybe play with this to
                                                     ! deal with spekling

    !***************************************************************
    ! INTERNAL VARIABLES
    !***************************************************************
    ! Counters
    integer :: lind
    integer :: sind
    logical :: good_angles
    ! Indices returned from locate()
    integer :: dtg_index
    integer :: irrad_index
    ! Calculated Values
    real :: hour_fraction
    real :: phase_fraction
    double precision :: curr_phase_angle
    double precision :: curr_earthmoon_dist
    double precision :: curr_earthsun_dist
    double precision :: curr_mean_irrad
    double precision :: cos_phase_angle
    real :: lunar_irrad_dnb
    double precision :: cos_weighted_irrad
    ! Denormalization Parameters
    double precision :: denorm1
    double precision :: denorm2
    double precision :: denorm3
    double precision :: denorm_factor
    ! Paths
    character(len=256) :: dat_path

    !***************************************************************
    ! OUTPUTS
    !***************************************************************
    real, dimension(lines, samples), intent(out)    :: lunar_ref

    !***************************************************************
    ! f2py Signature Information
    !***************************************************************
    !f2py integer, intent(in) :: lines
    !f2py ingeger, intent(in) :: samples
    !f2py double precision, intent(in) :: yyyymmddhh
    !f2py real, intent(in) :: minute
    !f2py real, intent(in) :: lunar_phase
    !f2py real, dimension(lines, samples), intent(in) :: in_dnb_rad
    !f2py real, dimension(lines, samples), intent(in) :: solar_zenith
    !f2py real, dimension(lines, samples), intent(in) :: lunar_zenith
    !f2py real, dimension(lines, samples), intent(in), optional :: in_city_lights
    !f2py real, dimension(lines, samples), intent(out) :: lunar_ref
    !f2py character*256, intent(in), optional :: in_dat_path = 'null'

    !***************************************************************
    ! Initialize paths and city_lights
    !***************************************************************
    print *, '\nEntered lunarref routine.'

    ! Paths to binary files
    if (present(in_dat_path).and.(.not.(in_dat_path.eq.'null'))) then
        dat_path = in_dat_path
    else
        dat_path = trim(ancildat_path)//'/lunarref'
    endif
    print *, 'Binary Path: ', dat_path
    lunar_irrad_file = trim(dat_path)//'/lunar_irrad_Mean_DNB.bin'
    dist_phase_file = trim(dat_path)//'/DIST_2010-2030_double.bin'
    print *, 'Lunar Irrad File: ', lunar_irrad_file
    print *, 'Dist Phase File:  ', dist_phase_file

    !! Initialize city_lights if present, otherwise set to zero
    !if (present(in_city_lights)) then
    !    print *, 'Found City Lights!', in_city_lights
    !    city_lights = in_city_lights
    !else
    !    city_lights(:,:) = 0
    !endif

    !***************************************************************
    ! 2. Print/Check input data
    !***************************************************************
    print *, '\nInput Data:'
    print *, 'Lines x Samples: ', lines, 'x', samples
    print *, 'YYYYMMDDHH:      ', yyyymmddhh
    print *, 'Minute:          ', minute
    print *, 'Lunar Phase:     ', lunar_phase

    print *, '\nSolar Zenith Angles:'
    print *, 'solar_zenith(1:10,1), degrees = ', solar_zenith(1:10, 1)
    print *, 'solar_zenith(1:10,2), degrees = ', solar_zenith(1:10, 2)

    print *, '\nLunar Zenith Angles:'
    print *, 'lunar_zenith(1:10,1), degrees = ', lunar_zenith(1:10, 1)
    print *, 'lunar_zenith(1:10,2), degrees = ', lunar_zenith(1:10, 2)

    print *, '\nDNB Radiance:'
    ! Convert from W/cm^2-sr to W/m^2-sr
    dnb_rad = in_dnb_rad * 1.0E+04
    print *, 'dnb_rad(1:10,1), W/(m^2 sr) = ', dnb_rad(1:10, 1)
    print *, 'dnb_rad(1:10,2), W/(m^2 sr) = ', dnb_rad(1:10, 2)


    !***************************************************************
    ! 3. Read ancillary data files
    !***************************************************************
    print *, '\nReading Lunar Irradiance LUT...'
    open(unit=1, file=lunar_irrad_file, status='old', action='read', &
         access='direct', form='unformatted', &
         recl=float_size*2*num_irrad_tabvals)
    read(unit=1, rec=1) lunar_irrad_lut
    close(1)

    print *, '\nReading ancillary lunar phase and distance data file...'
    open(unit=1, file=dist_phase_file, status='old', action='read',  &
         access='direct', form='unformatted', &
         recl=double_size*4*num_dist_tabvals)
    read(unit=1, rec=1) dist_phase_lut
    close(1)

    print *, '\nAncillary Data:'
    print *, 'Lunar Reflectance:'
    print *, 'lunar_irrad_lut(:,1) = ', lunar_irrad_lut(:,1)
    print *, 'Distance/Phase Table Values:'
    print *, 'dist_phase_lut(:,1) = ', dist_phase_lut(:,1)
    print *, 'dist_phase_lut(:, num_dist_tabvals) = ', dist_phase_lut(:, num_dist_tabvals)

    !***************************************************************
    ! 4. Compute TOA downwelling lunar irradiance (lunar_irrad_dnb) for current
    ! date/time
    !***************************************************************
    print *, '\nComputing downwelling TOA lunar irradiance for current time.'
    
    ! a) Interpolate dist_phase_lut to get current phase/dist parameters
    hour_fraction = minute/60.0
    print *, 'minute, hour_fraction = ', minute, hour_fraction

    !Locate the nearest datetime group from the distance and phase lookup table
    call locate(dist_phase_lut(1,:), num_dist_tabvals, yyyymmddhh, dtg_index)
    dtg_index = min(num_dist_tabvals, dtg_index)
    dtg_index = max(1, dtg_index)

    print *, '\ndtg_index = ', dtg_index
    print *, 'dist_phase_lut(:, dtg_index) = ', dist_phase_lut(:, dtg_index)
    print *, 'yyyymmddhh - dist_phase_lut(1, dtg_index) = ', &
             yyyymmddhh-dist_phase_lut(1, dtg_index)

    !Use lookup table index to determine distances and moon phase
    curr_phase_angle    = dist_phase_lut(2, dtg_index) + &
                          hour_fraction*(dist_phase_lut(2, dtg_index+1)-dist_phase_lut(2, dtg_index))
    curr_earthsun_dist  = dist_phase_lut(3, dtg_index) + &
                          hour_fraction*(dist_phase_lut(3, dtg_index+1)-dist_phase_lut(3, dtg_index))
    curr_earthmoon_dist = dist_phase_lut(4, dtg_index) + &
                          hour_fraction*(dist_phase_lut(4, dtg_index+1)-dist_phase_lut(4, dtg_index))

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! COMPARE THESE TWO VALUES -- SHOULD BE ABOUT THE SAME
    print *, '\nCOMPARE THESE TWO VALUES.  SHOULD BE ABOUT THE SAME!'
    print *, 'VIIRS Granule Lunar Phase         = ', lunar_phase
    print *, 'Calculated Lunar Phase            =    ', curr_phase_angle
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    print *, '\ndist_phase_lut(:, dtg_index)    = ', dist_phase_lut(:, dtg_index)
    print *, 'dist_phase_lut(:, dtg_index+1)    = ', dist_phase_lut(:, dtg_index+1)
    print *, 'hour_fraction                     = ', hour_fraction
    print *, 'curr_phase_angle                  = ', curr_phase_angle
    print *, 'curr_earthsun_dist                = ', curr_earthsun_dist
    print *, 'curr_earthmoon_dist               = ', curr_earthmoon_dist

    ! b) Interpolate lunar_irrad_lut to get current mean-geometry lunar
    ! irradiance pre-colvonved to DNB SRF
    phase_fraction = curr_phase_angle - int(curr_phase_angle)
    phase_array = lunar_irrad_lut(1, :)
    call locate(phase_array, num_irrad_tabvals, curr_phase_angle, irrad_index)
    curr_mean_irrad = lunar_irrad_lut(2, irrad_index) + &
                      phase_fraction*(lunar_irrad_lut(2, irrad_index+1)-lunar_irrad_lut(2, irrad_index))

    print *, '\nphase_fraction                  = ', phase_fraction
    print *, 'irrad_index                       = ', irrad_index
    print *, 'lunar_irrad_lut(:, irrad_index)   = ', lunar_irrad_lut(:, irrad_index)
    print *, 'lunar_irrad_lut(:, irrad_index+1) = ', lunar_irrad_lut(:, irrad_index+1)
    print *, 'curr_mean_irrad                   = ', curr_mean_irrad

    ! c) Define denormalization parameters to scale irradiance to current
    ! Sun/Earth/Moon geometry
    cos_phase_angle = cos(curr_phase_angle*dtor)
    denorm1 = mean_earthsun_dist**2 + mean_earthmoon_dist**2 + &
              2.0 * mean_earthmoon_dist * mean_earthsun_dist * cos_phase_angle
    denorm2 = curr_earthsun_dist**2 + curr_earthmoon_dist**2 + &
              2.0 * curr_earthmoon_dist * curr_earthsun_dist * cos_phase_angle
    denorm3 = ((mean_earthmoon_dist - earth_radius_km) / &
               (curr_earthmoon_dist - earth_radius_km)) ** 2.0
    denorm_factor = (denorm1 / denorm2) * denorm3

    print *, '\nDenormalization Parameters:'
    print *, 'DP 1      = ', denorm1
    print *, 'DP 2      = ', denorm2
    print *, 'DP 3      = ', denorm3
    print *, 'DP Factor = ', denorm_factor

    ! d) Denormalize mean-geometry irradiance to current geometry
    ! Also convert from mW/m^2-um to W/m^2 (divide by 1000 and multiply by srf_integ)
    lunar_irrad_dnb = curr_mean_irrad * denorm_factor * (srf_integ * 1.0E-03)

    print *, 'curr_mean_irrad (mW/m^2-micron)= ', curr_mean_irrad
    print *, '--> DNB Band-Integrated Lunar Irradiance (W/m^2) = ', lunar_irrad_dnb

    !***************************************************************
    ! 4. Loop over image to compute reflectances at each valid pixel
    ! Uncomputed mased values are assigned values of -999.0
    !***************************************************************
    
    print *, '\nLooping over image to compute lunar reflectance for valid pixels...'
    print *, 'lines, samples= ', lines, samples
    do lind=1, lines
        do sind=1, samples
            ! Check if city light and skip if flag set
            if (solar_zenith(lind, sind) >= astro_dark_thresh .and. lunar_zenith(lind, sind) < 70.0) &
                then
                good_angles = .TRUE.
                !print *, 'lunar_zenith= ', lunar_zenith(lind, sind),lind,sind
                !stop
            else
                good_angles = .FALSE.
            endif
            if (dnb_rad(lind, sind) >= 0.0 .and. good_angles .eqv. .TRUE.) &
                !city_lights(lind, sind) == 0) &
                then
                cos_weighted_irrad = cos(lunar_zenith(lind, sind) * dtor) * lunar_irrad_dnb
                if (cos_weighted_irrad > min_lunar_irrad_dnb) then
                    lunar_ref(lind, sind) = 100.0 * (pi * dnb_rad(lind, sind)) / &
                                            (cos_weighted_irrad)
                    if (lunar_ref(lind, sind) > 150.0) then
                        ! CHECK FOR POSSIBLE CITY LIGHT or other source (e.g. Lightning,
                        ! Fire)
                        !print *, '\nHigh Reflectance Encountered.'
                        !print *, 'dnb_rad(lind, sind) = ', dnb_rad(lind, sind)
                        !print *, '(lind, sind) = ', lind, sind
                        !print *,'dnb_rad(lind, sind) = ',dnb_rad(lind, sind)
                        !print *,'cos_weighted_irrad = ',cos_weighted_irrad
                        !print *,'--> % lunar_ref(lind, sind) = ',lunar_ref(lind, sind)
                        !print *,'MAX(dnb_rad) = ',maxval(dnb_rad)
                        !print *,'MAX(lunar_irrad_LUT(2,:)) = ',maxval(lunar_irrad_LUT(2,:))
                        !print *,'STOP DEBUG'
                        !STOP
                        lunar_ref(lind, sind) = 150.0
                    endif
                else
                    lunar_ref(lind, sind) = badval
                endif
            elseif (dnb_rad(lind, sind) > -1.0 .and. good_angles .eqv. .TRUE.) then
                lunar_ref(lind, sind) = 0.0
            else
                lunar_ref(lind, sind) = badval
            endif
        enddo !sind
    enddo !lind

    !***************************************************************
    contains
    !***************************************************************
    subroutine locate(interp_arr, arr_size, input_val, interp_index)
        ! Retrieve the index in interp_arr where the value most closely matches
        ! the value in input_val.  Return that index.
        ! Simple midpoint search
        integer, intent(in) :: arr_size ! Size of array being interpolated to
        integer, intent(out) :: interp_index ! Index of closest match
        double precision, intent(in) :: input_val ! Value to search for
        ! Array in which to locate closest match to input_val
        double precision, dimension(:), intent(in) :: interp_arr
        integer :: loop_ind, lower, mid, upper
        lower = 0
        upper = arr_size+1

        do loop_ind=1, 2*arr_size
            if (upper-lower <= 1) then
                exit
            endif
            mid = (upper + lower)/2
            if ((interp_arr(arr_size) >= interp_arr(1)) .eqv. (input_val >= interp_arr(mid))) then
                lower = mid
            else
                upper = mid
            endif
        enddo

        if (input_val == interp_arr(1)) then
            interp_index = 1
        elseif (input_val == interp_arr(arr_size)) then
            interp_index = arr_size-1
        else
            interp_index = lower
        endif
    end subroutine locate

end subroutine lunarref
