# can print empty environments [plain]

    Code
      hermod_environment_show("default", root)
    Message <cliMessage>
      
      -- hermod environment 'default' ------------------------------------------------
      * packages: (none)
      * sources: (none)

# can print empty environments [ansi]

    Code
      hermod_environment_show("default", root)
    Message <cliMessage>
      
      [36m--[39m [1mhermod environment 'default'[22m [36m------------------------------------------------[39m
      * packages: [3m(none)[23m
      * sources: [3m(none)[23m

# can print empty environments [unicode]

    Code
      hermod_environment_show("default", root)
    Message <cliMessage>
      
      ── hermod environment 'default' ────────────────────────────────────────────────
      • packages: (none)
      • sources: (none)

# can print empty environments [fancy]

    Code
      hermod_environment_show("default", root)
    Message <cliMessage>
      
      [36m──[39m [1mhermod environment 'default'[22m [36m────────────────────────────────────────────────[39m
      • packages: [3m(none)[23m
      • sources: [3m(none)[23m

# can print nontrivial environments [plain]

    Code
      hermod_environment_show("foo", root)
    Message <cliMessage>
      
      -- hermod environment 'foo' ----------------------------------------------------
      * packages: x, y, z
      * sources: a.R, b.R

# can print nontrivial environments [ansi]

    Code
      hermod_environment_show("foo", root)
    Message <cliMessage>
      
      [36m--[39m [1mhermod environment 'foo'[22m [36m----------------------------------------------------[39m
      * packages: [1mx[22m, [1my[22m, [1mz[22m
      * sources: [1ma.R[22m, [1mb.R[22m

# can print nontrivial environments [unicode]

    Code
      hermod_environment_show("foo", root)
    Message <cliMessage>
      
      ── hermod environment 'foo' ────────────────────────────────────────────────────
      • packages: x, y, z
      • sources: a.R, b.R

# can print nontrivial environments [fancy]

    Code
      hermod_environment_show("foo", root)
    Message <cliMessage>
      
      [36m──[39m [1mhermod environment 'foo'[22m [36m────────────────────────────────────────────────────[39m
      • packages: [1mx[22m, [1my[22m, [1mz[22m
      • sources: [1ma.R[22m, [1mb.R[22m

