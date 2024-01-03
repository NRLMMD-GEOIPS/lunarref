PROGRAM viirs_dnb_lunar_reflectance

    !***********************************************************
    !
    ! 09/05/2012 Jeremy Solbrig (jeremy.solbrig@nrlmry.navy.mil)
    !
    ! This program is intended to interface with Steve Miller's software
    !   while providing an example of how to utilize the lunar reflectance
    !   code in fortran.
    !
    ! Requires compiled version of lunarref.f90
    ! To compile both this and lunarref do the following:
    !
    !   gfortran -c lunarref.f90
    !   gfortran viirs_dnb_lunar_reflectance.f90 lunarref.o -o viirs_dnb_lunar_reflectance
    !
    ! This program wraps lunarref to do the following:
    !
    ! COPIED FROM STEVE MILLER's viirs_dnb_lunar_reflectance.f90 program:
    !
    ! Computes lunar %-reflectance [0,100] at each pixel for a passed-in dataset
    ! of VIIRS Day/Night band radiances.
    !
    ! INPUTS: 
    !
    !  1. Metafile containing:
    !  number of lines, number of samples, yyyymmddhh, minute
    !  lunar_phase 
    !  NOTE: for 85 sec granule it is okay to assume that lunar phase is constant
    !      360 deg/orbit / 29.54 day/orbit 
    !       = 12.191 deg/day / 86400 s/day 
    !       = 1.411e-4 deg/sec * 85 sec/gran 
    !       = 0.012 deg/gran
    !  process ID number (just used for tracking purposes)
    !
    !  2. Flat files containing:
    !  VIIRS/DNB radiances (W/cm^2-sr)
    !  Lunar zenith angles (to make sure the moon is up)
    !  Solar zenith angles (to make sure the sun is down)
    !  City lights mask (Not Yet Included)
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

    IMPLICIT NONE

    !***************************************************************
    ! Data read from input metadate file
    !***************************************************************
    ! Metadata file name
    CHARACTER(LEN=16)   :: metafile = 'metafile_DNB.dat'
    ! Dataset dimensions
    INTEGER(KIND=4)     :: lines
    INTEGER(KIND=4)     :: samples
    ! Granule time information
    DOUBLE PRECISION    :: yyyymmddhh
    REAL                :: minute
    ! Lunar phase angle in degrees
    REAL                :: lunar_phase
    ! Process ID
    !INTEGER(KIND=4)     :: pid

    !***************************************************************
    ! Input data from flat files
    !***************************************************************
    ! Flat Data Filenames
    CHARACTER(LEN=17) :: dnb_rad_file = 'viirs_dnb_rad.bin'
    CHARACTER(LEN=26) :: lun_zen_file = 'viirs_dnb_lunar_zenith.bin'
    CHARACTER(LEN=26) :: sol_zen_file = 'viirs_dnb_solar_zenith.bin'
    ! DNB Radiances (W/cm^2)
    REAL, DIMENSION(:, :), ALLOCATABLE :: dnb_rad
    ! Lunar Zenith Angle (degrees)
    REAL, DIMENSION(:, :), ALLOCATABLE :: lunar_zenith
    ! City-lights Mask (boolian)
    !REAL, DIMENSION(:, :), ALLOCATABLE :: city_lights
    ! Solar Zenith Angle in Degrees
    REAL, DIMENSION(:, :), ALLOCATABLE :: solar_zenith

    !***************************************************************
    ! Internal
    !***************************************************************
    INTEGER :: AllocateStatus
    CHARACTER(LEN=256) :: in_bin_path
    CHARACTER(LEN=256) :: GENPROCSDIR

    !***************************************************************
    ! Output Variable
    !***************************************************************
    REAL, DIMENSION(:, :), ALLOCATABLE :: lunar_ref

    CALL GET_ENVIRONMENT_VARIABLE('GENPROCSDIR', GENPROCSDIR)
    in_bin_path = TRIM(GENPROCSDIR)//'/new_scheme/genutils/corrections/lunarref'

    print *, ''
    print *, 'Entered viirs_dnb_lunar_reflectance calculation routine.'
    print *, ''

    !***************************************************************
    ! 2. Read Datasets and Allocate Memory
    !***************************************************************
    print *, ''
    print *, 'Reading data and allocating arrays'
    print *, ''

    ! Read the metadata file (line, sample, yyyymmddhh, minute)
    CALL read_metadata(metafile, lines, samples, yyyymmddhh, &
                       minute, lunar_phase & !, pid &
                      )

    print *, 'Allocating arrays...'
    ALLOCATE(dnb_rad(samples, lines), &
             lunar_zenith(samples, lines), &
             solar_zenith(samples, lines), &
             !city_lights(samples, lines), &
             lunar_ref(samples, lines), &
             STAT=AllocateStatus &
            )
    if (AllocateStatus /= 0) STOP '*** Not Enough Memory ***'

    print *, 'Reading DNB Radiance Data (W.cm^-2.sr^-2)...'
    CALL read_flatfile(dnb_rad_file, lines, samples, dnb_rad)
    print *,'dnb_rad(1:10,1), W.cm^-2.sr^-1 = ',dnb_rad(1:10,1)
    print *,'dnb_rad(1:10,2), W.cm^-2.sr^-1 = ',dnb_rad(1:10,2)

    print *, 'Reading Lunar Zenith Angle...'
    CALL read_flatfile(lun_zen_file, lines, samples, lunar_zenith)

    print *, 'Reading Solar Zenith Angle...'
    CALL read_flatfile(sol_zen_file, lines, samples, solar_zenith)

    CALL lunarref(dnb_rad, solar_zenith, lunar_zenith, &
                        yyyymmddhh, minute, lunar_phase, lunar_ref, &
                        samples, lines, in_bin_path)!, in_city_lights)

    print *, ''
    print *,'lunar_ref(1:10,1), W.cm^-2.sr^-1 = ',lunar_ref(1:10,1)
    print *,'lunar_ref(1:10,2), W.cm^-2.sr^-1 = ',lunar_ref(1:10,2)
    print *, ''
    print *, 'Writing lunar reflectance data to output file.'
    print *, ''

    OPEN (UNIT=16, FILE='dnb_lunar_reflectance.bin', STATUS='NEW', &
          ACTION='WRITE', ACCESS='DIRECT', FORM='UNFORMATTED', &
          RECL=8*lines*samples)
    WRITE (UNIT=16, REC=1) lunar_ref
    CLOSE (16)

    !***************************************************************
    CONTAINS
    !***************************************************************
    SUBROUTINE read_metadata(metafile, lines, samples, yyyymmddhh, &
                             minute, lunar_phase & !, pid &
                            )
        CHARACTER(LEN=*), INTENT(IN) :: metafile

        INTEGER(KIND=4), INTENT(OUT)    :: lines
        INTEGER(KIND=4), INTENT(OUT)    :: samples
        DOUBLE PRECISION, INTENT(OUT)   :: yyyymmddhh
        REAL, INTENT(OUT)               :: minute
        REAL, INTENT(OUT)               :: lunar_phase
        !REAL, INTENT(OUT)               :: pid

        print *, 'Reading metadata text file...'
        OPEN (UNIT=1, FILE=metafile, STATUS='OLD', ACTION='READ')
        READ (1, *) lines, samples, yyyymmddhh, minute
        READ (1, *) lunar_phase
        !READ (1, *) pid
        CLOSE (UNIT=1)

        print *, ''
        print *, '      lines = ', lines
        print *, '    samples = ', samples
        print *, ' yyyymmddhh = ', yyyymmddhh
        print *, '     minute = ', minute
        print *, 'lunar_phase = ', lunar_phase
        !print *, '        pid = ', pid
        print *, ''

    END SUBROUTINE read_metadata

    SUBROUTINE read_flatfile(flatfile, lines, samples, outdata)
        INTEGER, PARAMETER :: dtype_size = 4
        CHARACTER(LEN=*), INTENT(IN) :: flatfile
        INTEGER, INTENT(IN) :: lines
        INTEGER, INTENT(IN) :: samples
        REAL(KIND=dtype_size), DIMENSION(lines, samples), INTENT(OUT) :: outdata

        print *, 'Reading '//flatfile
        OPEN (UNIT=1, FILE=flatfile, STATUS='OLD', ACTION='READ', &
              ACCESS='DIRECT', FORM='UNFORMATTED', RECL=dtype_size*lines*samples)
        READ (1, REC=1) outdata
        CLOSE (1)
    END SUBROUTINE read_flatfile

END PROGRAM viirs_dnb_lunar_reflectance
