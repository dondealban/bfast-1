ndvi <-
function(R, NIR, ...)
{
    ndvi <- overlay(R, NIR, fun=function(x,y) (y-x)/(y+x), ...)

    return(ndvi)
}
