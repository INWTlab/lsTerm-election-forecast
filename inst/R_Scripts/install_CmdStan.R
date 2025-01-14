# install cmdstanr with GPU support
# It's already installed but must be installed again to support the GPU
library(cmdstanr)
check_cmdstan_toolchain()

# Use the next steps on vml3 to enable GPU support:
# path_to_opencl_lib <- "/usr/local/cuda/targets/x86_64-linux/lib"
# cpp_options = list(paste0("LDFLAGS+= -L", path_to_opencl_lib, " -lOpenCL"))
# install_cmdstan(cpp_options = cpp_options)

# without GPU:
install_cmdstan()
cmdstan_path()
cmdstan_version()
