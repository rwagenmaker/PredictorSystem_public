# =====================================================================================================================
# =====================================================================================================================
# ====================================    CLIENT.R   ============================================
#
#   > This file holds the methods for the remote execution from the client standpoint. 
#   > Used for the connection with de Sandbox designed by Francisco Banha
#
# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================


source("source.R")

remote_handler <- function(host_ip = "localhost", port_number = 6311) {
    connection = NULL
    open_connection = function() {
        connection <<- RS.connect(host = host_ip, port = port_number)
    }
    close_connection = function() {
        RS.close(connection)
    }
    load_libraries = function(installed_packages) {
        for (package in installed_packages) {
            RS.server.eval(connection, paste("library(", package, ")"))
        }
        close_connection()
    }
    assign_variables = function() {
        open_connection()
        vars <- ls(.GlobalEnv, all.names = TRUE)

        print(vars)
        for (var in vars) {
            RS.assign(connection, var, get(var))
        }
    }
    read_file = function(filename) {
        conn <- file(filename, open = "r")
        lines <- readLines(conn)
        close(conn)
        return(lines)
    }
    evaluate = function(expressions, inputExpression) {
        RS.assign(connection, "inputExpression", inputExpression)
        RS.assign(connection, "tmp____________________", expressions)
        RS.eval(connection, eval(parse(text = tmp____________________)))
        RS.eval(connection, remove("tmp____________________"))

        #RS.assign(connection, "tmp____________________", expressions)
        #RS.eval(connection, eval(parse(text = tmp____________________)))
        #RS.eval(connection, remove("tmp____________________"))
    }
    update_variables = function() {
        vars <- RS.eval(connection, ls(.GlobalEnv, all.names = TRUE))
        for (var in vars) {
            tmp <- as.call(list(quote(get), var))
            assign(var, RS.eval(connection, tmp, lazy = FALSE), envir = .GlobalEnv)
        }
    }
    remote_eval = function(value, inputExpression = "" ,is_file = FALSE) {
        installed_packages <- (.packages())
        library(RSclient)
        open_connection()
        load_libraries(installed_packages)
        assign_variables()
        expressions <- if(is_file) read_file(value) else value
        tryCatch(
            {
                evaluate(expressions,inputExpression)
                update_variables()
            },
            error = function(condition) {
                message(condition)
                return(NA)
            },
            finally = {
                close_connection()
                if (!("RSclient" %in% installed_packages)) detach("package:RSclient", unload=TRUE)
            }
        )
    }
    return(remote_eval)
}

f<-remote_handler()
