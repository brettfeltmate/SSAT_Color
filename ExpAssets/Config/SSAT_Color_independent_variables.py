from klibs.KLIndependentVariable import IndependentVariableSet

SSAT_Color_ind_vars = IndependentVariableSet()

SSAT_Color_ind_vars.add_variable("set_size", int, [8,12,16])
SSAT_Color_ind_vars.add_variable("present_absent", str, ['present', 'absent'])