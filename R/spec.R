#' Checks whether a Specification is Encoded
#'
#' Checks whether a specification is encoded with respect to the guide column.
#' @param x spec
#' @param column character: items in \code{column} to check
#' @param ... passed arguments
#' @return logical with same length as \code{column} argument
#' @import encode
#' @export
#' @keywords internal
encoded.spec <- function(x,column=x$column,...)encoded(x$guide[x$column %in% column])

extract <- function(x, pattern, group = 0, invert=FALSE,...){
  y <- regexec(pattern,x)
  scale <- sapply(y,length)
  group <- rep(group, length.out= length(y))
  group <- group + 1
  start <- sapply(seq_along(y), function(i){
    y <- y[[i]]
    group <- group[[i]]
    if(group > length(y)) return(0)
    y[[group]]
  })
  start[is.na(start)] <- 0
  start[start < 0] <- 0
  len <- sapply(seq_along(y), function(i){
    y <- y[[i]]
    group <- group[[i]]
    if(group > length(y)) return(0)
    attr(y,'match.length')[[group]]
  })
  len[is.na(len)] <- 0
  len[len < 0] <- 0
  stop <- start + len - 1
  stop[stop < 0] <- 0
  indices <- lapply(seq_along(y),function(i)start[[i]]:stop[[i]])
  indices[len == 0] <- 0
  splits <- strsplit(x,NULL)
  welds <- sapply(seq_along(splits), function(i){
    z <- splits[[i]]
    index <- indices[[i]]
    if(invert){
      if(len[i] > 0) z <- z[-index]
    } else z <- z[index]
    z <- paste(z,collapse='')
    z
   })
  welds[is.na(x)] <- NA
  welds
}

#' Extract Text from Guide
#'
#' Extracts text from guide.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
guidetext <- function(x,...)UseMethod('guidetext')


#' Extract Text from Guide Column of Specification
#'
#' Extracts text from guide column of specification, ignoring number ranges if any.
#' @param x object
#' @param column character: items in \code{column} to check
#' @param ... passed arguments
#' @return character
#' @export
#' @keywords internal
guidetext.spec <- function(x,column=x$column,...){
  x <- x[x$column %in% column,]
  pattern <- '((\\(|\\[) *([-+eE.0-9]*) *[,:] *([-+eE.0-9]*) *(\\)|\\])) *$'
  y <- extract(x$guide,pattern=pattern,invert=TRUE)
  y <- sub('^\\s','',y)
  y <- sub('\\s$','',y)
  y[encoded(y)] <- NA
  y[y == ''] <- NA
  y[x$type == 'datetime'] <- NA
  y
}

#' Extract Codes from Specification
#'
#' Extracts codes from specification guide column.
#' @param x spec
#' @param column character: items in \code{column} to check
#' @param ... passed arguments
#' @return list
#' @export
#' @keywords internal
codes.spec <- function(x,column=x$column,...)codes(x$guide[x$column %in% column])

#' Extract Decodes from Specification
#'
#' Extracts decodes from specification guide column.
#' @param x spec
#' @param column character: items in \code{column} to check
#' @param ... passed arguments
#' @return list
#' @export
#' @keywords internal
decodes.spec <- function(x,column=x$column,...)decodes(x$guide[x$column %in% column])

#' Extract Labels from Specification
#'
#' Extracts labels from specification label column.
#' @param object spec
#' @param column character: items in \code{column} to check
#' @param ... passed arguments
#' @return character
#' @export
#' @keywords internal
labels.spec <- function(object,column=object$column,...)object$label[object$column %in% column]

#' Coerce to Spec
#'
#' Coerces to class \code{spec}, a specification object
#' @param x object
#' @param ... passed arguments
#' @export
as.spec <- function(x, ...)UseMethod('as.spec')

#' Coerce to Spec from Data Frame
#'
#' Coerces to spec from data.frame already having basic properties.
#' @param x data.frame
#' @param ... passed arguments
#' @return spec
#' @export
as.spec.data.frame <- function(x, ...){
  expected <- c('column','label','type','guide','required','comment')
  found <- names(x)
  missing <- setdiff(expected, found)
  extra <- setdiff(found,expected)
  if(length(missing))warning('missing expected column(s) ',paste(missing,collapse=', '))
  if(length(extra))message('found unexpected column(s) ',paste(extra,collapse=', '))
  #x <- as.keyed(x,'column')
  class(x) <- union('spec', class(x))
  x
}

.nibble <- function(x,...){
  # strip outer spaces
  x <- sub('^ *','',x)
  x <- sub(' *$','',x)
  # identify balanced double quotes
  leading <- grepl('^"',x)
  trailing <- grepl('"$',x)
  i <- leading & trailing
  # strip balanced double quotes
  x[i] <- sub('^"','',x[i])
  x[i] <- sub('"$','',x[i])
  # strip outer spaces
  x <- sub('^ *','',x)
  x <- sub(' *$','',x)
  x
}

#' Read Specification from File
#'
#' Reads specification from file.  If first line contains tab characters, assumes format is tab-delimited text.  Otherwise, assumes format is comma-separated variable (csv).
#' @param x character (file path)
#' @param clean whether to strip balanced double quotes and outer white space from character values
#' @param ... passed arguments (ignored)
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @importFrom utils write.table
#' @return spec
#' @export
read.spec <- function(x, clean = TRUE, ...){
  h <- readLines(x,n=1)
  if(any(grepl('\t',h))) {
    x <- read.table(x,header=TRUE,as.is=TRUE,na.strings=c('','.','NA'), quote='',sep='\t')
  }else{
    x %<>% as.csv
  }
  chars <- sapply(x, inherits, 'character')
  if(clean) x[chars] <- lapply(x[chars], .nibble)
  x <- as.spec(x)
  x
}

#' Coerce to Specification from Character
#'
#' Coerces to specification from character (length-one filepath).
#' @param x character path to spec-formatted file
#' @param ... passed arguments
#' @return spec
#' @export
#' @import csv
#' @import magrittr
as.spec.character <- function(x,...){
  stopifnot(length(x) == 1 && file.exists(x))
  y <- read.spec(x,...)
  y
}

#' Write Specification to Storage
#'
#' Writes specification to storage in tab-delimited format.  Use as.csv() for CSV format.
#' @param x spec
#' @param file character filepath for storage location
#' @param ... passed arguments
write.spec <- function(x,file,...)write.table(x,file=file, row.names=FALSE,quote=FALSE,na='.',sep='\t',...)

#' Make a Specification
#'
#' Makes a specification.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
specification <- function(x,...)UseMethod('specification')

#' Make a Specification by Default
#'
#' Makes a specification by default method, i.e. returns input unchanged.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
specification.default <- function(x, ...)x

#' Make a Specification for a Comment
#'
#' Makes a specification for a comment
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
specification.comment <- function(x,...)factor(x, levels=c(TRUE,FALSE), labels=c('C','.'))

#' Make Guide
#'
#' Makes Guide
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
.guide <- function(x,...)UseMethod('.guide')
#' Make Default Guide
#'
#' Makes default guide.
#' @param x object
#' @param tol integer
#' @param ... dots
#' @export
#' @keywords internal
.guide.default <- function(x,tol=10,...){
  x <- as.character(x)
  codes <- unique(x)
  codes <- codes[!is.na(codes)]
  if(length(codes) <= tol) return(encode(codes,labels=codes))
  return(as.character(NA))
}
#' Make Numeric Guide
#'
#' Makes numeric guide.
#' @param x object
#' @param digits integer
#' @param ... dots
#' @export
#' @keywords internal
.guide.numeric <- function(x,digits=20,...){
  if(all(x == round(x),na.rm=TRUE)) .guide(as.integer(x),...)
  else paste0('[',signif(digits=digits,min(x,na.rm=TRUE)),':',signif(digits=digits,max(x,na.rm=TRUE)),']')
}
#' Make Integer Guide
#'
#' Makes integer guide.
#' @param x object
#' @param tol integer
#' @param ... dots
#' @export
#' @keywords internal
.guide.integer <- function(x,tol=10,...){
  if(length(unique(x)) <= tol) .guide(as.factor(x),...)
  else paste0('[',min(x,na.rm=TRUE),':',max(x,na.rm=TRUE),']')
}
.guide.factor <- function(x,...){
  codes <- levels(x)
  encode(codes,labels=codes)
}
# .guide.mDateTime <- function(x,...)'%Y-%m-%d %H:%M'
# .guide.mDate <- function(x,...)'%Y-%m-%d'
# .guide.mTime <- function(x,...)'%H:%M'

#' Make Type
#'
#' Makes type
#' @export
#' @keywords internal
.type <- function(x,...)UseMethod('.type')
#' Make Default Type
#'
#' Makes default type.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
.type.default <- function(x,...) 'character'
#' Make Timepoint Type
#'
#' Makes timepoint type.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
.type.timepoint <- function(x,...)'datetime'
#' Make Numeric Type
#'
#' Makes numeric type.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
.type.numeric <- function(x,...)if(all(x==round(x),na.rm=TRUE)) 'integer' else 'numeric'
#' Make Label
#'
#' Makes label.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
.label <- function(x,...)UseMethod('.label')
#' Make Default Label
#'
#' Makes default label.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
.label.default <- function(x,...){
  lab <- attr(x,which='label')
  if(is.null(lab)) lab <- as.character(NA)
  lab
}
#' Make Required
#'
#' Makes required.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
.required <- function(x,...)UseMethod('.required')
#' Make Default Required
#'
#' Makes default required.
#' @param x object
#' @param ... dots
#' @export
#' @keywords internal
.required.default <- function(x,...)as.integer(all(!is.na(x)))

#' Make a Specification for a Data Frame
#'
#' Makes a specification for data.frame.  Creates a template based on the data.frame. Uses column names for labels where columns do not have a label attribute. Factors will be encoded. numerics will be rounded to \code{digits} and like integers will be expressed as ranges in \code{guide} column. Integers and character with less than or exactly \code{tol} unique values will be encoded.
#' @param x object
#' @param tol integer
#' @param digits integer
#' @param ... passed arguments
#' @export
specification.data.frame <- function(x,tol=10,digits=20,...){
  x[] <- lapply(x,specification)
  y <- data.frame(
    stringsAsFactors=FALSE,
    column=names(x),
    label=sapply(x,.label,...),
    type=sapply(x,.type,...),
    guide=sapply(x,.guide,tol=tol,digits=digits,...),
    required=sapply(x,.required,...),
    comment=NA
  )
  y$label[is.na(y$label)] <- y$column[is.na(y$label)]
  rownames(y) <- NULL
  y <- as.spec(y)
  y
}

#' Check Whether x and y Match
#'
#' Checks Whether x an y match.
#' @param x object
#' @param y object
#' @param ... passed arguments
#' @export
#' @keywords internal
`%matches%` <- function(x, y)UseMethod("%matches%")

#' Check Whether Spec matches Dataset
#'
#' Checks whether spec matches dataset by checking whether dataset matches spec.
#' @param x spec
#' @param y object
#' @param ... passed arguments
#' @export
#' @keywords internal
`%matches%.spec` <- function(x,y, ...) y %matches% x

#' Check Whether Character matches y
#'
#' Checks whether character matches y, treating x as filepath.
#' @param x character
#' @param y object
#' @param ... passed arguments
#' @export
`%matches%.character` <- function(x, y, ...){
  stopifnot(length(x) == 1)
  if(! file.exists(x))stop(x,' not found')
  x <- read.csv(x,as.is=TRUE,na.strings=c('','.','NA'))
  x %matches% y
}
#`%matches%.data.frame` <- function(x, y, ...)as.keyed(x) %matches% y

#' Coerce to Vector from Spec.
#'
#' Coerces to vector from spec by returning \code{column}.
#' @param x spec
#' @param mode to match generic
#' @export
#' @keywords internal
as.vector.spec <- function(x,mode='any')x$column

#' Check Whether Data Frame matches Spec
#'
#' Checks whether data.frame matches spec.
#' @param x spec
#' @param y coerced to spec (spec object or filepath for spec file).
#' @param ... passed arguments
#' @return logical
#' @export
#' @examples
#' specification(Theoph) \%matches\% Theoph
`%matches%.data.frame` <- function(x, y, ...){ # was as.keyed method
  y <- as.spec(y)
  x[] <- lapply(x,specification)
  unspecified <- setdiff(names(x), as.vector(y))
  if(length(unspecified)){
    message('unspecified: ',paste(unspecified,collapse=', '))
    return(FALSE)
  }
  unimplemented <- setdiff(as.vector(y), names(x))
  if(length(unimplemented)){
    message('unimplemented: ',paste(unimplemented,collapse=', '))
    return(FALSE)
  }
  if(any(names(x) != as.vector(y))){
    message('column order differs, e.g. ',as.vector(x)[as.vector(x) != as.vector(y)][[1]])
    return(FALSE)
  }
  for(col in as.vector(y)[y$type == 'integer'])if(any(na.rm=TRUE, x[[col]] != as.integer(x[[col]]))){
    message(col,' not integer')
    return(FALSE)
  }
  for(col in as.vector(y)[y$type == 'numeric'])if(!is.numeric(x[[col]])){
    message(col,' not numeric')
    return(FALSE)
  }
  # for(col in y$column[y$type == 'datetime']){
  #   format <- y$guide[y$column == col]
  #   if(any(
  #     !is.na(x[[col]]) & is.na(as.mDateTime(format=format,x[[col]]))
  #   )){
  #     message(col, ' not (coercible to) ', format)
  #     return(FALSE)
  #   }
  # }
  z <- y[encoded(y$guide),]
  allgoodcodes <- TRUE
  for(col in z$column){
    codes <- codes(z$guide[z$column==col])
    vals <- unique(x[,col])
    vals <- vals[!is.na(vals)]
    bad <- setdiff(vals, codes)
    if(length(bad)){
      message('For ',col,': expecting only NA or ',paste(codes, collapse=', '),
      ' but found ',paste(bad, collapse=', '))
      allgoodcodes <- FALSE
    }
  }
  if(!allgoodcodes)return(FALSE)
  allrequired <- TRUE
  for(col in y$column){
    condition <- y$required[y$column == col]
    required <- as.logical(eval(parse(text=condition), envir=x))
    required[is.na(required)] <- TRUE
    missing <- is.na(x[,col])
    exceptions <- sum(required & missing)
    if(exceptions){
      message('found ',exceptions,' NA in ',col,' for condition ',condition)
      allrequired <- FALSE
    }
    if(!allrequired)return(FALSE)
  }
  pattern <- '((\\(|\\[) *([-+eE.0-9]*) *, *([-+eE.0-9]*) *(\\)|\\])) *$'
  y$lo <- extract(y$guide,pattern,group=3)
  y$hi <- extract(y$guide,pattern,group=4)
  y$lo <- as.numeric(y$lo)
  y$hi <- as.numeric(y$hi)
  y$oplo <- extract(y$guide,pattern,group=2)
  y$ophi <- extract(y$guide,pattern,group=5)
  y$lo[y$lo == ''] <- NA
  y$hi[y$hi == ''] <- NA
  y$oplo[y$oplo == ''] <- NA
  y$ophi[y$ophi == ''] <- NA
  y$mn <- sapply(y$column,function(col)if(is.numeric(x[[col]]))min(x[[col]],na.rm=TRUE) else NA)
  y$mx <- sapply(y$column,function(col)if(is.numeric(x[[col]]))max(x[[col]],na.rm=TRUE) else NA)
  y$goodlo <- TRUE
  y$goodhi <- TRUE
  y$goodlo[with(y, !is.na(lo) & !is.na(oplo) & !is.na(mn) & oplo=='(' & mn <= lo)] <- FALSE
  y$goodlo[with(y, !is.na(lo) & !is.na(oplo) & !is.na(mn) & oplo=='[' & mn <  lo)] <- FALSE
  y$goodhi[with(y, !is.na(hi) & !is.na(ophi) & !is.na(mx) & ophi==')' & mx >= hi)] <- FALSE
  y$goodhi[with(y, !is.na(hi) & !is.na(ophi) & !is.na(mx) & ophi==']' & mx >  hi)] <- FALSE
  y$interval <- extract(y$guide,pattern, group=1)
  y$msg <- with(y, paste(column,'range',mn,',',mx,'outside of interval',interval,'\n'))
  y$failed <- !y$goodlo | !y$goodhi
  if(any(y$failed)){
    message(y$msg[y$failed])
    return(FALSE)
  }
  TRUE
}

#' Respecify Something
#'
#' Respecify something.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
respecify <- function(x,...)UseMethod('respecify')

#' Respecify Character
#'
#' Respecify specification supplied as filepath. Updates numeric ranges.  Useful if these have changed and spec no longer matches.
#' @param x character filepath for a spec file (*.spec)
#' @param data character filepath for a dataset
#' @param file where to write the result (over-write source, by default)
#' @param ... passed arguments
#' @export
respecify.character <- function(
  x,
  data=sub('spec$','csv',x),
  file=x,
  ...
)respecify(read.spec(x,...),data=data,file=file,...)

#' Respecify Specification
#'
#' Respecify specification. Updates numeric ranges.  Useful if these have changed and spec no longer matches.
#' @param x spec
#' @param data a data.frame or path to csv file
#' @param file where to write the result (default: do not write)
#' @param ... passed arguments
#' @export

respecify.spec <- function(x, data, file=NULL, ...){
  if (inherits(data,'character')) data %<>% as.csv(...)
  # get as many ranges as possible
  y <- specification(data,tol=0, ...)
  # only where originals and replacements exist
  xrange <- !encoded(x) & grepl('[])]$',x$guide)
  yrange <- !encoded(x) & grepl('[])]$',y$guide)
  text <- guidetext(x) # won't be present in y
  text[is.na(text)] <- '' # paste0s nicely
  # trailing space where applicable
  text[text != ''] <- paste0(text[text != ''], ' ')
  if(any(xrange & ! yrange)) warning('some ranges not updated')
  here <- xrange & yrange
  x$guide[here] <- paste0(text[here], y$guide[here])
  if (is.null(file))
    return(x)
  else {
    write.spec(x, file=file, ...)
    invisible(x)
  }
}


