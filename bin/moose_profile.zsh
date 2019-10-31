export MODULE_PATH="$MODULE_PATH:/opt/moose/Modules/3.2.10/modulefiles"
source /usr/local/Cellar/modules/4.3.0/init/zsh
export MOOSE_JOBS=`/usr/sbin/sysctl -n hw.ncpu`

module load moose-dev-clang ccache icecream
