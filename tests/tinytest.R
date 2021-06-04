# Test files in inst/tinytest/
if (requireNamespace("tinytest", quietly = TRUE)) {
  suppressMessages(tinytest::test_package("OmicNavigator"))
}
