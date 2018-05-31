fv.tr=function(CF,TR,ValDate=NULL) {  
#
# DESCRIPTION:
# a function that calculates future value with time varying interest rates
# 
# ARGUMENTS:
# CF -- a zoo object of cash flows
# TR -- a zoo object which is a total return index reflecting the variable interest rate, 
#       there must be an entry in TR for every date in CF and for the ValDate
# ValDate -- the valuation date or dates, defaults to last date in cash flow if none provided,
#             if earlier than last date in cash flows, then cash flows after this date are ignored,
#             if earlier than first date in cash flows, returns NA
#             defaults to the last date in the cash flow,
#             must be in the same date format as CF and TR
#
# RESULT:
# a zoo object of the same length as ValDate which is the value of the cash flows as if invested in the index
# as of each valuation date
#
  if(is.null(ValDate)) ValDate=tail(time(CF),1)
  if(!is.zoo(CF)) stop("CF not a zoo object")
  if(!is.zoo(TR)) stop("TR not a zoo object")
  if(any(!(ValDate%in%time(TR)))) stop("No TR for ValDate")
  if(any(!(time(CF)%in%time(TR)))) stop("No TR for CF")
  value=vector()
  for (i in 1:length(ValDate)) {
    if(ValDate[i]<min(time(CF))) value[i]=NA
    CF.i=CF[time(CF)<=ValDate[i]]
    TR.i=TR[time(TR)<=ValDate[i]]
    TR.ind=TR.i/tail(TR.i,1)
    value[i]=sum(TR.ind*CF.i)
  }
  return(zoo(value,ValDate))
}

