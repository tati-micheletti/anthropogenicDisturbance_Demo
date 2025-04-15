
#### setLibPaths start #### New File:TRUE # DO NOT EDIT BETWEEN THESE LINES
### DELETE THESE LINES BELOW TO RESTORE STANDARD R Package LIBRARY
### Previous .libPaths: /home/tmichele/R/x86_64-pc-linux-gnu-library/4.4, /opt/R/4.4.1/lib/R/library
._libPaths <- c('/home/tmichele/.local/share/R/anthropogenicDisturbance_Demo/packages/x86_64-pc-linux-gnu/4.4')
._standAlone <- TRUE
{
    .oldLibPaths <- .libPaths()
    if (!dir.exists(._libPaths[1])) dir.create(._libPaths[1], recursive = TRUE)
    gte4.1 <- isTRUE(getRversion() >= "4.1")
    if (gte4.1) {
        do.call(.libPaths, list(new = ._libPaths, if (gte4.1) include.site <- !._standAlone))
    }
    .shim_fun <- .libPaths
    .shim_env <- new.env(parent = environment(.shim_fun))
    if (isTRUE(._standAlone)) {
        .shim_env$.Library <- utils::tail(.libPaths(), 1)
    }
    else {
        .shim_env$.Library <- .libPaths()
    }
    .shim_env$.Library.site <- character()
    environment(.shim_fun) <- .shim_env
    .shim_fun(unique(._libPaths))
    message(".libPaths() is now: ", paste(.libPaths(), collapse = ", "))
}
message("To reset libPaths to previous state, run: Require::setupOff() (or delete section in .Rprofile file)") 
#### setLibPaths end
