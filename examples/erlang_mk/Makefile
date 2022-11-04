PROJECT = erlang_mk
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 1.1.5

BUILD_DEPS = vsn_transform
dep_vsn_transform = git https://github.com/rlipscombe/vsn_transform.git

ERLC_OPTS += +'{parse_transform, vsn_transform}' +'{vsn, "${PROJECT_VERSION}"}'

include erlang.mk
