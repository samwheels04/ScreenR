print("this is the file that'll run tests")



test.QQ <- function(y, x) {
    cor <- cor.test(x, y)
    return(cor$p.value)
}
