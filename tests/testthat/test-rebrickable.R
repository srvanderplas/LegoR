
test_that("rebrickable_save_credentials works",
          {
            rebrickable_save_credentials("testing", profile_save = NULL, sys_env_var = "rbkey", global_var = ".rbkey")
            expect_equal(.rbkey, "testing")
            expect_equal(Sys.getenv("rbkey"), "testing")
            expect_true(exists(".rbkey"))
            
            rebrickable_save_credentials("testing", profile_save = "user", sys_env_var = "rbkey2", global_var = ".rbkey2")
            expect_equal(clipr::read_clip(allow_non_interactive = T), ".rbkey2 = 'testing'")
            
            Sys.unsetenv("rbkey")
            Sys.unsetenv("rbkey2")
            suppressWarnings(rm(.rbkey, .rbkey2))
          })

