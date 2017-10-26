extentsOverlap <- function(e1, e2)
# test whether two extent objects overlap
# returns a TRUE or FALSE (overlaps or not)
# for overlap in the x domain:
  # (xmin(e1) <= xmin(e2) & xmin(e2) < xmax(e1)) | v.v.
# for overlap in the y domain:
  # (ymin(e1) <= ymin(e2) & ymin(e2) < ymax(e1)) | v.v.
# both conditions must be true
# args:
  # e1, e2: two extent objects to be compared
{
  xOverlap <- 
    (xmin(e1) <= xmin(e2) & xmin(e2) < xmax(e1)) | 
    (xmin(e2) <= xmin(e1) & xmin(e1) < xmax(e2))
  
  yOverlap <- 
    (ymin(e1) <= ymin(e2) & ymin(e2) < ymax(e1)) | 
    (ymin(e2) <= ymin(e1) & ymin(e1) < ymax(e2))
  
  overlap <- xOverlap & yOverlap
  
  return(overlap)
}