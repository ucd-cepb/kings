#DEV edits by Github users panoptikum (Felix Idelberger, Data Scientist) and jehoonchae (Je Hoon Chae, PhD student, UCLA)
# #https://github.com/bstewart/stm/issues/200   
estimateEffectDEV <- function (formula, stmobj, metadata = NULL, uncertainty = c("Global","Local", "None"), group=FALSE, documents = NULL, nsims = 25, prior = NULL) 
{
   origcall <- match.call()
   thetatype <- match.arg(uncertainty)
   if (thetatype == "None") 
      nsims <- 1
   if (!is.null(documents)) {
      args <- asSTMCorpus(documents, data = metadata)
      documents <- args$documents
      metadata <- args$data
   }
   if (!inherits(formula, "formula")) 
      stop("formula must be a formula object.")
   if (!is.null(metadata) & !is.data.frame(metadata)) 
      metadata <- as.data.frame(metadata)
   termobj <- terms(formula, data = metadata)
   if (attr(termobj, "response") == 1) {
      response <- as.character(formula)[2]
      if (group==TRUE) {
         grouped_topics <- trimws(strsplit(response, "&")[[1]])
         K <- 1:length(grouped_topics)
      } else {
         K <- eval(parse(text = response))
         if (!(posint(K) && max(K) <= stmobj$settings$dim$K))
            stop("Topics specified as response in formula must be a set of positive integers equal to or less than the number of topics in the model.")
      }
      formula <- as.formula(as.character(formula)[c(1, 3)])
      termobj <- terms(formula, data = metadata)
   } else {
      K <- 1:stmobj$settings$dim$K
   }
   mf <- model.frame(termobj, data = metadata)
   xmat <- model.matrix(termobj, data = metadata)
   varlist <- all.vars(termobj)
   if (!is.null(metadata)) {
      data <- metadata[, varlist, drop = FALSE]
   }
   else {
      templist <- list()
      for (i in 1:length(varlist)) {
         templist[[i]] <- get(varlist[i])
      }
      data <- data.frame(templist)
      names(data) <- varlist
      rm(templist)
   }
   metadata <- data
   rm(data)
   if (!is.null(prior)) {
      if (!is.matrix(prior)) {
         prior <- diag(prior, nrow = ncol(xmat))
      }
      if (ncol(prior) != ncol(xmat)) 
         stop("number of columns in prior does not match columns in design matrix")
      prior.pseudo <- chol(prior)
      xmat <- rbind(xmat, prior.pseudo)
   }
   qx <- qr(xmat)
   if (qx$rank < ncol(xmat)) {
      prior <- diag(1e-05, nrow = ncol(xmat))
      prior.pseudo <- chol(prior)
      xmat <- rbind(xmat, prior.pseudo)
      qx <- qr(xmat)
      warning("Covariate matrix is singular.  See the details of ?estimateEffect() for some common causes.\n             Adding a small prior 1e-5 for numerical stability.")
   }
   storage <- vector(mode = "list", length = length(K))
   K_grouped <- c()
   for (i in 1:nsims) {
      if (thetatype == "None") 
         thetasims <- stmobj$theta
      else {
         thetasims <- thetaPosterior(stmobj, nsims = 1, type = thetatype, 
                                     documents = documents)
         thetasims <- do.call(rbind, thetasims)
      }
      for (k in K) {
         if (group==TRUE) {
            topics_to_group <- eval(parse(text = grouped_topics[k]))
            grouped_thetasims <- apply(thetasims[, topics_to_group], 1, sum)
            lm.mod <- qr.lm(grouped_thetasims, qx)
         } else {
            lm.mod <- qr.lm(thetasims[, k], qx)
         }
         storage[[which(k == K)]][[i]] <- summary.qr.lm(lm.mod)
         if (group==TRUE & i==1)
            K_grouped[k] <- paste0(c(k, ": ", paste0(topics_to_group, collapse = "+")), collapse = "")
      }
   }
   toreturn <- list(parameters = storage, topics = K_grouped, call = origcall, 
                    uncertainty = thetatype, formula = formula, data = metadata, 
                    modelframe = mf, varlist = varlist)
   class(toreturn) <- "estimateEffect"
   return(toreturn)
}


qr.lm <- function(y, qx) {
   if(length(y)!=nrow(qx$qr)) {
      #probably don't match because of a prior
      if(length(y)!=(nrow(qx$qr)-ncol(qx$qr))) stop("number of covariate observations does not match number of docs")
      #if it passes this check its the prior. thus
      y <- c(y,rep(0, ncol(qx$qr)))
   }
   beta <- solve.qr(qx, y)
   residuals <- qr.resid(qx,y)
   fitted.values <- qr.fitted(qx,y)
   df.residual <- length(fitted.values) - qx$rank
   out <- list(coefficients=beta, residuals=residuals, 
               fitted.values=fitted.values, 
               df.residual=df.residual, rank=qx$rank, qr=qx)
   out 
}
#this function rewrites the summary.lm() function
# to calculate from our reduced regression
summary.qr.lm <- function (object) {
   z <- object
   p <- z$rank
   rdf <- z$df.residual
   
   Qr <- object$qr
   n <- nrow(Qr$qr)
   p1 <- 1L:p
   r <- z$residuals
   f <- z$fitted.values
   
   mss <- ifelse(attr(z$terms, "intercept"), sum((f - mean(f))^2), sum(f^2)) 
   rss <- sum(r^2)
   
   resvar <- rss/rdf
   R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
   se <- sqrt(diag(R) * resvar)
   est <- z$coefficients[Qr$pivot[p1]]
   sigma <- sqrt(resvar)
   list(est=est, vcov=(sigma^2 * R))
}

rmvnorm<-function(n,mu,Sigma,chol.Sigma=chol(Sigma)) {
   E<-matrix(rnorm(n*length(mu)),n,length(mu))
   t(  t(E%*%chol.Sigma) +c(mu))
}

summary.estimateEffect <- function(object, topics=NULL, nsim=500, ...) {
   if(is.null(topics)) topics <- object$topics
   if(any(!(topics %in% object$topics))) {
      stop("Some topics specified with the topics argument are not available in this estimateEffect object.")
   }
   tables <- vector(mode="list", length=length(topics))
   for(i in seq_along(topics)) {
      topic = as.numeric(strsplit(topics[i], ":")[[1]][1])
      sims <- lapply(object$parameters[[topic]], function(x) rmvnorm(nsim, x$est, x$vcov))
      sims <- do.call(rbind,sims)
      est<- colMeans(sims)
      se <- sqrt(apply(sims,2, stats::var))
      tval <- est/se
      rdf <- nrow(object$data) - length(est)
      p <- 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE)
      
      coefficients <- cbind(est, se, tval, p)
      rownames(coefficients) <- attr(object$parameters[[1]][[1]]$est, "names")
      colnames(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      tables[[i]] <- coefficients
   }
   out <- list(call=object$call, topics=topics, tables=tables)
   class(out) <- "summary.estimateEffect"
   return(out)
}

print.summary.estimateEffect <- function(x, digits = max(3L, getOption("digits") - 3L), 
                                         signif.stars = getOption("show.signif.stars"), ...) {
   cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
       "\n\n", sep = "")
   
   for(i in 1:length(x$tables)) {
      cat(sprintf("\nTopic %s:\n", x$topics[i]))
      cat("\nCoefficients:\n")
      coefs <- x$tables[[i]]
      stats::printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
                          na.print = "NA", ...)
      cat("\n")
   }
   invisible(x)
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
   abs(x - round(x)) < tol
} 

posint <- function(x) {
   all(is.wholenumber(x)) & all(x>0)
}

nonnegint <- function(x) {
   all(is.wholenumber(x)) & all(x>=0)
}

extract.estimateEffect <- function (x, covariate, model = NULL, topics = x$topics, method = "pointestimate", 
                                    cov.value1 = NULL, cov.value2 = NULL, moderator = NULL, moderator.value = NULL, 
                                    npoints = 100, nsims = 100, ci.level = 0.95, custom.labels = NULL, 
                                    labeltype = "numbers", n = 7, frexw = 0.5) 
{
   cthis <- stm:::produce_cmatrix(prep = x, covariate = covariate, 
                                  method = method, cov.value1 = cov.value1, cov.value2 = cov.value2, 
                                  moderator = moderator, moderator.value = moderator.value)
   simbetas <- stm:::simBetas(parameters = x$parameters, nsims = nsims)
   uvals <- cthis$cdata[[covariate]]
   offset <- (1 - ci.level)/2
   labels <- stm:::createLabels(labeltype = labeltype, covariate = covariate, 
                                method = method, cdata = cthis$cdata, cov.value1 = cov.value1, 
                                cov.value2 = cov.value2, model = model, n = n, topics = x$topics, 
                                custom.labels = custom.labels, frexw = frexw)
   out <- lapply(topics, function(i) {
      sims <- cthis$cmatrix %*% t(simbetas[[which(x$topics == 
                                                     i)]])
      if (method == "difference") {
         diff <- sims[1, ] - sims[2, ]
         out_inner <- data.frame(method = method, topic = i, 
                                 covariate = covariate, covariate.value = paste0(cov.value1, 
                                                                                 "-", cov.value2), estimate = mean(diff), std.error = sd(diff), 
                                 ci.level = ci.level, ci.lower = quantile(diff, 
                                                                          offset), ci.upper = quantile(diff, 1 - offset), 
                                 label = labels[which(x$topics == i)])
      }
      else {
         #browser()
         out_inner <- data.frame(method = method, topic = i, 
                                 covariate = covariate, covariate.value = uvals, 
                                 estimate = rowMeans(sims), std.error = sqrt(apply(sims, 
                                                                                   1, stats::var)), ci.level = ci.level, ci.lower = apply(sims, 
                                                                                                                                          1, quantile, probs = offset), ci.upper = apply(sims, 
                                                                                                                                                                                         1, quantile, probs = (1 - offset)), label = labels[which(x$topics == 
                                                                                                                                                                                                                                                     i)])
         out_inner["tval"] <- out_inner["estimate"]/out_inner["std.error"]
         rdf <- nrow(x$data) - length(out_inner["estimate"])
         out_inner["p"] <- 2 * stats::pt(abs(out_inner[["tval"]]), rdf, lower.tail = FALSE)
      }
      if (!is.null(moderator)) {
         out_inner$moderator <- moderator
         out_inner$moderator.value <- moderator.value
      }
      rownames(out_inner) <- NULL
      return(out_inner)
   })
   out <- do.call("rbind", out)
   return(out)
}


# 
# estimateEffectDEV <- function (formula, stmobj, metadata = NULL, uncertainty = c("Global", 
#                                                                                  "Local", "None"), group=FALSE, documents = NULL, nsims = 25, prior = NULL) 
# {
#    #Section 1 ####
#    origcall <- match.call()
#    thetatype <- match.arg(uncertainty)
#    if (thetatype == "None") 
#       nsims <- 1
#    if (!is.null(documents)) {
#       args <- asSTMCorpus(documents, data = metadata)
#       documents <- args$documents
#       metadata <- args$data
#    }
#    if (!inherits(formula, "formula")) 
#       stop("formula must be a formula object.")
#    if (!is.null(metadata) & !is.data.frame(metadata)) 
#       metadata <- as.data.frame(metadata)
#    termobj <- terms(formula, data = metadata)
#    if (attr(termobj, "response") == 1) {
#       response <- as.character(formula)[2]
#       if (group==TRUE) {
#          grouped_topics <- trimws(strsplit(response, "&")[[1]])
#          K <- 1:length(grouped_topics)
#       } else {
#          K <- eval(parse(text = response))
#          if (!(posint(K) && max(K) <= stmobj$settings$dim$K))
#             stop("Topics specified as response in formula must be a set of positive integers equal to or less than the number of topics in the model.")
#       }
#       formula <- formula(paste(as.character(formula)[c(1,3)],collapse=" "))
#       termobj <- terms(formula, data = metadata)
#    } else {
#       K <- 1:stmobj$settings$dim$K
#    }
#    mf <- model.frame(termobj, data = metadata)
#    xmat <- model.matrix(termobj, data = metadata)
#    varlist <- all.vars(termobj)
#    if (!is.null(metadata)) {
#       data <- metadata[, varlist, drop = FALSE]
#    }
#    else {
#       templist <- list()
#       for (i in 1:length(varlist)) {
#          templist[[i]] <- get(varlist[i])
#       }
#       data <- data.frame(templist)
#       names(data) <- varlist
#       rm(templist)
#    }
#    metadata <- data
#    rm(data)
#    if (!is.null(prior)) {
#       if (!is.matrix(prior)) {
#          prior <- diag(prior, nrow = ncol(xmat))
#       }
#       if (ncol(prior) != ncol(xmat)) 
#          stop("number of columns in prior does not match columns in design matrix")
#       prior.pseudo <- chol(prior)
#       xmat <- rbind(xmat, prior.pseudo)
#    }
#    qx <- qr(xmat)
#    if (qx$rank < ncol(xmat)) {
#       prior <- diag(1e-05, nrow = ncol(xmat))
#       prior.pseudo <- chol(prior)
#       xmat <- rbind(xmat, prior.pseudo)
#       qx <- qr(xmat)
#       warning("Covariate matrix is singular.  See the details of ?estimateEffect() for some common causes.\n             Adding a small prior 1e-5 for numerical stability.")
#    }
#    storage <- vector(mode = "list", length = length(K))
#    K_grouped <- c()
#    for (i in 1:nsims) {
#       if (thetatype == "None") 
#          thetasims <- stmobj$theta
#       else {
#          thetasims <- thetaPosterior(stmobj, nsims = 1, type = thetatype, 
#                                      documents = documents)
#          thetasims <- do.call(rbind, thetasims)
#       }
#       for (k in K) {
#          if (group==TRUE) {
#             topics_to_group <- eval(parse(text = grouped_topics[k]))
#             grouped_thetasims <- apply(thetasims[, topics_to_group], 1, sum)
#             lm.mod <- qr.lm(grouped_thetasims, qx)
#          } else {
#             lm.mod <- qr.lm(thetasims[, k], qx)
#          }
#          storage[[which(k == K)]][[i]] <- summary.qr.lm(lm.mod)
#          if (group==TRUE & i==1)
#             K_grouped[k] <- paste0(c(k, ": ", paste0(topics_to_group, collapse = "+")), collapse = "")
#       }
#    }
#    toreturn <- list(parameters = storage, topics = K_grouped, call = origcall, 
#                     uncertainty = thetatype, formula = formula, data = metadata, 
#                     modelframe = mf, varlist = varlist)
#    class(toreturn) <- "estimateEffect"
#    return(toreturn)
# }
# 
# 
# qr.lm <- function(y, qx) {
#    if(length(y)!=nrow(qx$qr)) {
#       #probably don't match because of a prior
#       if(length(y)!=(nrow(qx$qr)-ncol(qx$qr))) stop("number of covariate observations does not match number of docs")
#       #if it passes this check its the prior. thus
#       y <- c(y,rep(0, ncol(qx$qr)))
#    }
#    beta <- solve.qr(qx, y)
#    residuals <- qr.resid(qx,y)
#    fitted.values <- qr.fitted(qx,y)
#    df.residual <- length(fitted.values) - qx$rank
#    out <- list(coefficients=beta, residuals=residuals, 
#                fitted.values=fitted.values, 
#                df.residual=df.residual, rank=qx$rank, qr=qx)
#    out 
# }
# #this function rewrites the summary.lm() function
# # to calculate from our reduced regression
# summary.qr.lm <- function (object) {
#    z <- object
#    p <- z$rank
#    rdf <- z$df.residual
#    
#    Qr <- object$qr
#    n <- nrow(Qr$qr)
#    p1 <- 1L:p
#    r <- z$residuals
#    f <- z$fitted.values
#    
#    mss <- ifelse(attr(z$terms, "intercept"), sum((f - mean(f))^2), sum(f^2)) 
#    rss <- sum(r^2)
#    
#    resvar <- rss/rdf
#    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
#    se <- sqrt(diag(R) * resvar)
#    est <- z$coefficients[Qr$pivot[p1]]
#    sigma <- sqrt(resvar)
#    list(est=est, vcov=(sigma^2 * R))
# }
# 
# #Section 2 ####
# 
# rmvnorm<-function(n,mu,Sigma,chol.Sigma=chol(Sigma)) {
#    E<-matrix(rnorm(n*length(mu)),n,length(mu))
#    t(  t(E%*%chol.Sigma) +c(mu))
# }
# 
# summary.estimateEffectDEV <- function(object, topics=NULL, nsim=500, ...) {
#    if(is.null(topics)) topics <- object$topics
#    if(any(!(topics %in% object$topics))) {
#       stop("Some topics specified with the topics argument are not available in this estimateEffect object.")
#    }
#    tables <- vector(mode="list", length=length(topics))
#    for(i in seq_along(topics)) {
#       topic = as.numeric(strsplit(topics[i], ":")[[1]][1])
#       sims <- lapply(object$parameters[[topic]], function(x) rmvnorm(nsim, x$est, x$vcov))
#       sims <- do.call(rbind,sims)
#       est<- colMeans(sims)
#       se <- sqrt(apply(sims,2, stats::var))
#       tval <- est/se
#       rdf <- nrow(object$data) - length(est)
#       p <- 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE)
#       
#       coefficients <- cbind(est, se, tval, p)
#       rownames(coefficients) <- attr(object$parameters[[1]][[1]]$est, "names")
#       colnames(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
#       tables[[i]] <- coefficients
#    }
#    out <- list(call=object$call, topics=topics, tables=tables)
#    class(out) <- "summary.estimateEffect"
#    return(out)
# }
# # Section 3 ####
# 
# 
# print.summary.estimateEffectDEV <- function(x, digits = max(3L, getOption("digits") - 3L), 
#                                          signif.stars = getOption("show.signif.stars"), ...) {
#    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
#        "\n\n", sep = "")
#    
#    for(i in 1:length(x$tables)) {
#    #   cat(sprintf("\nTopic %s:\n", x$topics[i]))
#       cat(sprintf("\nTopic \n", x$topics[i]))
#       cat("\nCoefficients:\n")
#       coefs <- x$tables[[i]]
#       stats::printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
#                           na.print = "NA", ...)
#       cat("\n")
#    }
#    invisible(x)
# }
# 
# is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
#    abs(x - round(x)) < tol
# } 
# 
# posint <- function(x) {
#    all(is.wholenumber(x)) & all(x>0)
# }
# 
# nonnegint <- function(x) {
#    all(is.wholenumber(x)) & all(x>=0)
# }
# 
# extract.estimateEffectDEV <- function (x, covariate, model = NULL, topics = x$topics, method = "pointestimate", 
#                                     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL, moderator.value = NULL, 
#                                     npoints = 100, nsims = 100, ci.level = 0.95, custom.labels = NULL, 
#                                     labeltype = "numbers", n = 7, frexw = 0.5) 
# {
#    cthis <- stm:::produce_cmatrix(prep = x, covariate = covariate, 
#                                   method = method, cov.value1 = cov.value1, cov.value2 = cov.value2, 
#                                   moderator = moderator, moderator.value = moderator.value)
#    simbetas <- stm:::simBetas(parameters = x$parameters, nsims = nsims)
#    uvals <- cthis$cdata[[covariate]]
#    offset <- (1 - ci.level)/2
#    labels <- stm:::createLabels(labeltype = labeltype, covariate = covariate, 
#                                 method = method, cdata = cthis$cdata, cov.value1 = cov.value1, 
#                                 cov.value2 = cov.value2, model = model, n = n, topics = x$topics, 
#                                 custom.labels = custom.labels, frexw = frexw)
#    out <- lapply(topics, function(i) {
#       sims <- cthis$cmatrix %*% t(simbetas[[which(x$topics == 
#                                                      i)]])
#       if (method == "difference") {
#          diff <- sims[1, ] - sims[2, ]
#          out_inner <- data.frame(method = method, topic = i, 
#                                  covariate = covariate, covariate.value = paste0(cov.value1, 
#                                                                                  "-", cov.value2), estimate = mean(diff), std.error = sd(diff), 
#                                  ci.level = ci.level, ci.lower = quantile(diff, 
#                                                                           offset), ci.upper = quantile(diff, 1 - offset), 
#                                  label = labels[which(x$topics == i)])
#       }
#       else {
#          #browser()
#          out_inner <- data.frame(method = method, topic = i, 
#                                  covariate = covariate, covariate.value = uvals, 
#                                  estimate = rowMeans(sims), std.error = sqrt(apply(sims, 
#                                                                                    1, stats::var)), ci.level = ci.level, ci.lower = apply(sims, 
#                                                                                                                                           1, quantile, probs = offset), ci.upper = apply(sims, 
#                                                                                                                                                                                          1, quantile, probs = (1 - offset)), label = labels[which(x$topics == 
#                                                                                                                                                                                                                                                      i)])
#          out_inner["tval"] <- out_inner["estimate"]/out_inner["std.error"]
#          rdf <- nrow(x$data) - length(out_inner["estimate"])
#          out_inner["p"] <- 2 * stats::pt(abs(out_inner[["tval"]]), rdf, lower.tail = FALSE)
#       }
#       if (!is.null(moderator)) {
#          out_inner$moderator <- moderator
#          out_inner$moderator.value <- moderator.value
#       }
#       rownames(out_inner) <- NULL
#       return(out_inner)
#    })
#    out <- do.call("rbind", out)
#    return(out)
# }