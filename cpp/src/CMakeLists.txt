# Minimum cmake version
cmake_minimum_required (VERSION 3.0.0)

# Name of project and that it is C++ only.
project (bionitio-cpp CXX)
set(BIONITIO_LIB_SRC options.cpp fasta_stats.cpp error.cpp)  

add_library(bionitio-lib STATIC ${BIONITIO_LIB_SRC})

# ----------------------------------------------------------------------------
# Dependencies
# ----------------------------------------------------------------------------

# Search for zlib as a dependency for SeqAn.
find_package (ZLIB)

# Load the SeqAn module and fail if not found.
find_package (SeqAn REQUIRED)

# Load the required BOOST modules and fail if not found.
ADD_DEFINITIONS(-DBOOST_LOG_DYN_LINK=1)
find_package(Boost COMPONENTS program_options log log_setup thread filesystem system)

add_library(Catch INTERFACE)
target_include_directories(Catch INTERFACE ${CMAKE_SOURCE_DIR}/catch/)
add_executable(bionitio-test test.cpp)
target_link_libraries (bionitio-test bionitio-lib Catch ${SEQAN_LIBRARIES} ${Boost_LIBRARIES})

# ----------------------------------------------------------------------------
# Build Setup
# ----------------------------------------------------------------------------

# Add include directories.
include_directories (${SEQAN_INCLUDE_DIRS})

# Add definitions set by find_package (SeqAn).
add_definitions (${SEQAN_DEFINITIONS})

if(Boost_FOUND)
    include_directories(${Boost_INCLUDE_DIRS})
endif()

# Add CXX flags found by find_package (SeqAn).
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SEQAN_CXX_FLAGS}")

# Add executable and link against SeqAn dependencies.
add_executable (bionitio-cpp bionitio.cpp)
target_link_libraries (bionitio-cpp bionitio-lib ${SEQAN_LIBRARIES} ${Boost_LIBRARIES})

enable_testing()
add_test(NAME bionitio-unit-tests COMMAND bionitio-test)
