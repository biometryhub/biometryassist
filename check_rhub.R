# Check on Rhub platforms

# Build for Mac OS
rhub::check(platform = "macos-highsierra-release-cran")

# Build on Ubuntu
rhub::check(platform = "ubuntu-gcc-release")

rhub::check_for_cran()
