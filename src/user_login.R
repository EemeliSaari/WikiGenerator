# Scripts to read user data for WikiMedia

require(tcltk)


read_user_data <- function(user_data) {
    if (!is.null(user_data) && !file.exists(user_data)) {
        print('Provide user data for this method in .json format')
        return(NULL)
    }
    read_user(user_data)
}


read_user_login <- function() {
    "Based on the solution proposed in the following blog post:
    https://magesblog.com/post/2014-07-15-simple-user-interface-in-r-to-get-login/"

    tt <- tktoplevel()
    tkwm.title(tt, "Get login details")
    user_name <- tclVar("Login ID")
    password <- tclVar("Password")

    entry_name <- tkentry(tt, width= "20", textvariable = user_name)
    entry_password <- tkentry(tt, width = "20", show = "*", textvariable = password)

    tkgrid(tklabel(tt, text = "Login to WikiMedia bot."))
    tkgrid(entry_name)
    tkgrid(entry_password)
    
    ok_ok <- function() { 
        tkdestroy(tt) 
    }
    ok_but <-tkbutton(tt, text="OK", command=ok_ok)
    tkbind(entry_password, "<Return>", ok_ok)

    tkgrid(ok_but)
    tkraise(tt)
    
    tkfocus(tt)
    tkwait.window(tt)
    
    return(invisible(c(name = tclvalue(user_name), password = tclvalue(password))))
}
