# Using secrets on the cluster

## Using `cyphr`

We assume that you have followed the [“data encryption”
vignette](https://docs.ropensci.org/cyphr/articles/data.html) in `cyphr`
and that you are running from the same directory that it is set up for.
(You may or may not be using orderly with this, the procedure is the
same either way).

We’ll quickly set up the same basic structure in this vignette.

First, we configure hipercow at this directory:

``` r
library(hipercow)
hipercow_init()
#> ✔ Initialised hipercow at '.' (/home/rfitzjoh/net/home/cluster/hipercow-vignette/hv-20250912-187f90285932a2)
#> ℹ Next, call 'hipercow_configure()'
hipercow_configure(driver = "dide-windows")
#> ✔ Configured hipercow to use 'dide-windows'
```

We assume that you have ssh keys that work for **your machine** from
your previous use of `cyphr` and the vignette already has these working.
These will have a password, but for the purposes of this vignette you
will not see the password prompt here.

``` r
cyphr::data_admin_init(".")
#> Generating data key
#> Authorising ourselves
#> Adding key 78:59:4c:6c:61:a1:9e:d8:c8:02:22:49:33:c9:8e:db:b3:d1:c3:65:9b:1d:76:5b:50:e5:36:2c:da:52:1e:f8
#>   user: rfitzjoh
#>   host: wpia-dide300
#>   date: 2025-09-12 13:41:53.50033
#> Verifying
```

We can locally work with this key just fine to encrypt some small secret
data set, `x`

``` r
x <- runif(10)
```

``` r
key <- cyphr::data_key()
cyphr::encrypt(saveRDS(x, "data.rds"), key)
```

What we want to do is to write a cluster job that can use this secret
data.

Firstly, we need to install `cyphr` into the library that will run on
the cluster. You could also do this by adding `cyphr` to your
`pkgdepends.txt`:

``` r
hipercow_provision(method = "pkgdepends", refs = "cyphr")
#> ℹ Looking for active tasks before installation
#> ✔ No tasks running
#> /`-'\  _______  ___  ___ ____
#> \,T./ / __/ _ \/ _ \/ _ `/ _ \
#>   |   \__/\___/_//_/\_,_/_//_/
#>   |   ---- THE  LIBRARIAN ----
#> 
#> Bootstrapping from: I:/bootstrap-windows/4.4.2
#> Installing into library: hipercow/lib/windows/4.4.2
#> Using method pkgdepends
#> Running in path: V:/cluster/hipercow-vignette/hv-20250912-187f90285932a2
#> Library paths:
#>   - V:/cluster/hipercow-vignette/hv-20250912-187f90285932a2/hipercow/lib/windows/4.4.2
#>   - C:/Program Files/R/R-4.4.2/library
#> id: 20250912124156
#> Logs from pkgdepends follow:
#> 
#> -------------------------------------------------------------------------------
#> 
#> 
#> ── repos 
#> • https://cloud.r-project.org
#> 
#> ── refs 
#> • cyphr
#> ✔ Updated metadata database: 7.83 MB in 6 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> + askpass      1.2.1  [dl]
#> + cyphr        1.1.7  [dl]
#> + getPass      0.2-4  [dl]
#> + openssl      2.3.3  [dl]
#> + rstudioapi   0.17.1 [dl]
#> + sodium       1.4.0  [dl]
#> + sys          3.4.3  [dl]
#> ℹ Getting 7 pkgs with unknown sizes
#> ✔ Got askpass 1.2.1 (x86_64-w64-mingw32) (74.80 kB)
#> ✔ Got rstudioapi 0.17.1 (i386+x86_64-w64-mingw32) (346.85 kB)
#> ✔ Got sys 3.4.3 (x86_64-w64-mingw32) (47.98 kB)
#> ✔ Got openssl 2.3.3 (x86_64-w64-mingw32) (3.47 MB)
#> ✔ Got sodium 1.4.0 (x86_64-w64-mingw32) (660.91 kB)
#> ✔ Got cyphr 1.1.7 (i386+x86_64-w64-mingw32) (400.78 kB)
#> ✔ Got getPass 0.2-4 (x86_64-w64-mingw32) (493.58 kB)
#> ✔ Installed askpass 1.2.1  (582ms)
#> ✔ Installed cyphr 1.1.7  (647ms)
#> ✔ Installed getPass 0.2-4  (755ms)
#> ✔ Installed sys 3.4.3  (707ms)
#> ✔ Installed sodium 1.4.0  (894ms)
#> ✔ Installed rstudioapi 0.17.1  (1.1s)
#> ✔ Installed openssl 2.3.3  (1.3s)
#> ✔ Summary:   7 new  in 5.9s
#> 
#> -------------------------------------------------------------------------------
#> Writing library description to 'hipercow/lib/windows/4.4.2/.conan/20250912124156'
#> Done!
#> ✔ Installation script finished successfully in 20.39 secs
```

We might then naively try and run a task that reads the encrypted data:

``` r
t <- task_create_expr(
  cyphr::decrypt(readRDS("data.rds"), cyphr::data_key()))
#> ✔ Submitted task '20bee48990a9104d3cd497a02b80cdf2' using 'dide-windows'
```

This will fail:

``` r
task_result(t)
#> <simpleError: Did not find default ssh public key at '~/.ssh/id_rsa.pub'
#> You can create a key here by running
#>   cyphr::ssh_keygen("~/.ssh/id_rsa.pub")>
```

The reason why it fails is because the cluster cannot see the key that
we are using to work with cyphr; the `~/.ssh/id_rsa.pub` referred to
here would be on the cluster node that ran the job and not the key that
exists on your machine.

What we can do is to create another key that will be found in your
network home directory:

``` r
dide_generate_keypair()
#> ✔ Created new private key at '/home/rfitzjoh/net/home/.hipercow/key'
#> ✔ Created new public key at '/home/rfitzjoh/net/home/.hipercow/key.pub'
#> ✔ Saved public key into your keychain
```

We need to authorise this key with cyphr, which we can do by running a
short cluster job:

``` r
t <- task_create_expr(cyphr::data_request_access())
#> ✔ Submitted task 'd4163d6ff3e2dafafd98a26116bd8352' using 'dide-windows'
```

Once that has run, we will see the request for access, which is actually
us but from the cluster:

``` r
cyphr::data_admin_list_requests()
#> 1 key:
#>   51:40:32:58:11:4a:05:df:c3:7a:27:2b:25:17:6d:53:66:41:f0:f0:48:d6:2b:d6:23:b1:39:7a:f9:5f:58:66
#>     user: rfitzjoh
#>     host: WPIA-054
#>     date: 2025-09-12 13:42:21.019979
```

We can authorise ourselves:

``` r
cyphr::data_admin_authorise(yes = TRUE)
#> There is 1 request for access
#> Adding key 51:40:32:58:11:4a:05:df:c3:7a:27:2b:25:17:6d:53:66:41:f0:f0:48:d6:2b:d6:23:b1:39:7a:f9:5f:58:66
#>   user: rfitzjoh
#>   host: WPIA-054
#>   date: 2025-09-12 13:42:21.019979
#> Added 1 key
#> If you are using git, you will need to commit and push:
#> 
#>     git add .cyphr
#>     git commit -m "Authorised rfitzjoh"
#>     git push
```

And now we can run our jobs that require encrypted data

``` r
t <- task_create_expr(
  cyphr::decrypt(readRDS("data.rds"), cyphr::data_key()))
#> ✔ Submitted task 'da8170adcb81ff1b69af55ecc146b74e' using 'dide-windows'
```

This time we can read the data!

``` r
task_result(t)
#>  [1] 0.05838293 0.41034069 0.22518724 0.11884682 0.28322052 0.82149523
#>  [7] 0.41899393 0.16678808 0.79077363 0.81735793
```
