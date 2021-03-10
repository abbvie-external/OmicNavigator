library("OmicNavigator")

study <- OmicNavigator:::testStudy("test")
plots <- OmicNavigator:::testPlots()
study <- addPlots(study, plots = plots)

installStudy(study)
