dateMatch <- function(x, table, how=c("NA", "before", "after", "nearest", "interp"), error.how=c("NA", "drop", "nearest"), nomatch=NA, offset=NULL,value=FALSE){
    if (is.null(how))
        how <- "NA"
    else
        how <- match.arg(how)
    if (is.null(error.how))
        error.how <- "NA"
    else
        error.how <- match.arg(error.how)
    
    # return indices of x in table
    if (!inherits(x, 'Date'))
        x <- dateParse(x)
    if (!inherits(table, "Date"))
        table <- dateParse(table)
    # NA's in table confuse align() below, so remove them
    table.ok <- which(!is.na(table))
    need.na.adj <- FALSE
    if (length(table.ok) != length(table)) {
        table <- table[table.ok]
        need.na.adj <- TRUE
    }

    if (length(table) && length(x)) {
        if (any(diff(table) <= 0))
            stop("table must contain strictly increasing dates")

		# Date vectors can use match() for exact matches with NA's for no match
        idx <- match(x,table)
		idxNA <- is.na(idx)
		N <- length(table)

		if (how=="NA"){
			if (any(idxNA)){
				if (error.how=='NA' || error.how=='nearest') {
					idx[idxNA] <- nomatch
				} else if (error.how=='drop'){	
					idx <- idx[!idxNA]
				}
			}
		} else if (how=="before"){
			if (any(idxNA)){
				# findInterval() returns the left side of the interval (i.e the "before" index),
				# 0 if x[i] <- min(table),
				# and N if x[i] > table[N]
				idx[idxNA] <- findInterval(x[idxNA],table)

				if (error.how=='NA') {
					idx[idx==0] <- nomatch
				} else if (error.how=='drop'){	
					idx <- idx[idx>0]
				} else if (error.how=='nearest') {
					idx[idx==0] <- 1
				}
			}
		} else if (how=="after"){
			if (any(idxNA)){
				idx[idxNA] <- findInterval(x[idxNA],table) + 1

				if (error.how=='NA') {
					idx[idx>N] <- nomatch
				} else if (error.how=='drop'){	
					idx <- idx[idx<=N]
				} else if (error.how=='nearest') {
					idx[idx>N] <- N
				}
			}
		} else if (how=="nearest"){
			if (any(idxNA)){
				x_idxNA <- x[idxNA]
				y <- findInterval(x_idxNA,table)

				# Not efficient! But what's the alternative look like?
				for (i in 1:length(y)){
					if (y[i] == 0){
						y[i] <- 1
					} else if (y[i] == N){
						d2 <- table[N] - x_idxNA[i]
						d1 <- x_idxNA[i] - table[N-1]
						if (d2 < d1) y[i] <- N
						else y[i] <- N-1
					} else {
						d2 <- table[y[i]+1] - x_idxNA[i]
						d1 <- x_idxNA[i] - table[y[i]]
						if (d2 < d1) y[i] <- y[i]+1
						else y[i] <- y[i]
					}
				}
				idx[idxNA] <- y
			}
		} else if (how=="interp"){
			if (any(idxNA)){
				x_idxNA <- x[idxNA]
				y <- findInterval(x_idxNA,table)

				# Not efficient! But what's the alternative look like?
				for (i in 1:length(y)){
					if (y[i] == 0){
						next
					} else if (y[i] == N){
						# -1 signifies an upper bound error condition
						if (x_idxNA[i] > table[N]){
							 y[i] <- -1
						} else {
							d2 <- table[N] - table[N-1]
							d1 <- x_idxNA[i] - table[N-1]
							y[i] <- y[i] + unclass(d1)/unclass(d2)
						}
					} else {
						d2 <- table[y[i]+1] - table[y[i]]
						d1 <- x_idxNA[i] - table[y[i]]
						y[i] <- y[i] + unclass(d1)/unclass(d2)
					}
				}

				idx[idxNA] <- y

				if (error.how=='NA') {
					idx[idx==0|idx==-1] <- nomatch
				} else if (error.how=='drop'){	
					idx <- idx[idx>0]
				} else if (error.how=='nearest') {
					idx[idx==-1] <- N
					idx[idx==0] <- 1
				}
			}
		}

        if (!is.null(offset)) {
            if (abs(as.integer(offset) - offset)>0){
                stop("offset must be an integer")
            } else {
                idx <- idx + offset
                if (any(i <- which(idx < 1)))
                    idx[i] <- NA
                if (any(i <- which(idx > length(table))))
                    idx[i] <- NA
            }
        }
    } else {
        if (error.how=="drop")
            idx <- numeric(0)
        else
            idx <- rep(as.numeric(NA), length(x))
    }
    if (any(is.na(idx)) && !is.null(nomatch) && !is.na(nomatch))
        idx[which(is.na(idx))] <- nomatch
    # need to adjust idx to account for NA's in original table?
    if (need.na.adj)
        idx <- table.ok[idx]

    if (value)
        return(table[idx])
    else
        return(idx)
}
