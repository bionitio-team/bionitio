# Minimum cmake version
cmake_minimum_required (VERSION 3.0.0)

# Name of project and that it is C++ only.
project (biotool-cpp CXX)
set(BIOTOOL_LIB_SRC options.cpp fasta_stats.cpp error.cpp)  

add_library(biotool-lib STATIC ${BIOTOOL_LIB_SRC})

# ----------------------------------------------------------------------------
# Dependencies
# ----------------------------------------------------------------------------

# Search for zlib as a dependency for SeqAn.
find_package (ZLIB)

# Load the SeqAn module and fail if not found.
find_package (SeqAn REQUIRED)

# Load the SeqAn module and fail if not found.
find_package(Boost COMPONENTS program_options REQUIRED)

add_library(Catch INTERFACE)
target_include_directories(Catch INTERFACE ${CMAKE_SOURCE_DIR}/catch/)
add_executable(biotool-test test.cpp)
target_link_libraries (biotool-test biotool-lib Catch ${SEQAN_LIBRARIES})

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
add_executable (biotool-cpp biotool.cpp)
target_link_libraries (biotool-cpp biotool-lib ${SEQAN_LIBRARIES} ${Boost_LIBRARIES})

enable_testing()
add_test(NAME biotool-unit-tests COMMAND biotool-test)
