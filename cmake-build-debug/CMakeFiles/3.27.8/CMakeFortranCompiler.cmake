set(CMAKE_Fortran_COMPILER "/opt/intel/oneapi/compiler/2024.0/bin/ifx")
set(CMAKE_Fortran_COMPILER_ARG1 "")
set(CMAKE_Fortran_COMPILER_ID "IntelLLVM")
set(CMAKE_Fortran_COMPILER_VERSION "2024.0.2")
set(CMAKE_Fortran_COMPILER_WRAPPER "")
set(CMAKE_Fortran_PLATFORM_ID "Linux")
set(CMAKE_Fortran_SIMULATE_ID "")
set(CMAKE_Fortran_COMPILER_FRONTEND_VARIANT "GNU")
set(CMAKE_Fortran_SIMULATE_VERSION "")




set(CMAKE_AR "/usr/bin/ar")
set(CMAKE_Fortran_COMPILER_AR "CMAKE_Fortran_COMPILER_AR-NOTFOUND")
set(CMAKE_RANLIB "/usr/bin/ranlib")
set(CMAKE_TAPI "CMAKE_TAPI-NOTFOUND")
set(CMAKE_Fortran_COMPILER_RANLIB "CMAKE_Fortran_COMPILER_RANLIB-NOTFOUND")
set(CMAKE_COMPILER_IS_GNUG77 )
set(CMAKE_Fortran_COMPILER_LOADED 1)
set(CMAKE_Fortran_COMPILER_WORKS TRUE)
set(CMAKE_Fortran_ABI_COMPILED TRUE)

set(CMAKE_Fortran_COMPILER_ENV_VAR "FC")

set(CMAKE_Fortran_COMPILER_SUPPORTS_F90 1)

set(CMAKE_Fortran_COMPILER_ID_RUN 1)
set(CMAKE_Fortran_SOURCE_FILE_EXTENSIONS f;F;fpp;FPP;f77;F77;f90;F90;for;For;FOR;f95;F95;f03;F03;f08;F08)
set(CMAKE_Fortran_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_Fortran_LINKER_PREFERENCE 20)
set(CMAKE_Fortran_LINKER_DEPFILE_SUPPORTED )
if(UNIX)
  set(CMAKE_Fortran_OUTPUT_EXTENSION .o)
else()
  set(CMAKE_Fortran_OUTPUT_EXTENSION .obj)
endif()

# Save compiler ABI information.
set(CMAKE_Fortran_SIZEOF_DATA_PTR "8")
set(CMAKE_Fortran_COMPILER_ABI "ELF")
set(CMAKE_Fortran_LIBRARY_ARCHITECTURE "x86_64-linux-gnu")

if(CMAKE_Fortran_SIZEOF_DATA_PTR AND NOT CMAKE_SIZEOF_VOID_P)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_Fortran_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_Fortran_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_Fortran_COMPILER_ABI}")
endif()

if(CMAKE_Fortran_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "x86_64-linux-gnu")
endif()





set(CMAKE_Fortran_IMPLICIT_INCLUDE_DIRECTORIES "/opt/intel/oneapi/tbb/2021.11/include;/opt/intel/oneapi/mpi/2021.11/include;/opt/intel/oneapi/mkl/2024.0/include;/opt/intel/oneapi/ippcp/2021.9/include;/opt/intel/oneapi/ipp/2021.10/include;/opt/intel/oneapi/dpl/2022.3/include;/opt/intel/oneapi/dpcpp-ct/2024.0/include;/opt/intel/oneapi/dnnl/2024.0/include;/opt/intel/oneapi/dev-utilities/2024.0/include;/opt/intel/oneapi/dal/2024.0/include/dal;/opt/intel/oneapi/compiler/2024.0/opt/oclfpga/include;/opt/intel/oneapi/ccl/2021.11/include;/opt/intel/oneapi/compiler/2024.0/opt/compiler/include;/opt/intel/oneapi/compiler/2024.0/include;/usr/local/include;/opt/intel/oneapi/compiler/2024.0/lib/clang/17/include;/usr/include;/usr/include/x86_64-linux-gnu")
set(CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "ifport;ifcoremt;imf;svml;m;ipgo;irc;pthread;svml;c;gcc;gcc_s;irc_s;dl;c")
set(CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES "/opt/intel/oneapi/tbb/2021.11/lib/intel64/gcc4.8;/opt/intel/oneapi/mpi/2021.11/lib;/opt/intel/oneapi/mkl/2024.0/lib;/opt/intel/oneapi/ippcp/2021.9/lib;/opt/intel/oneapi/ipp/2021.10/lib;/opt/intel/oneapi/dpl/2022.3/lib;/opt/intel/oneapi/dnnl/2024.0/lib;/opt/intel/oneapi/dal/2024.0/lib;/opt/intel/oneapi/compiler/2024.0/lib;/opt/intel/oneapi/ccl/2021.11/lib;/opt/intel/oneapi/compiler/2024.0/lib/clang/17/lib/x86_64-unknown-linux-gnu;/opt/intel/oneapi/tbb/2021.11/lib/intel64/lib;/usr/lib/gcc/x86_64-linux-gnu/11;/usr/lib/x86_64-linux-gnu;/usr/lib64;/usr/lib;/lib/x86_64-linux-gnu;/lib64;/lib;/usr/lib/i386-linux-gnu")
set(CMAKE_Fortran_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")