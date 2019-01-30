k=iris
l=best_vector(iris,5,3,100,0)
#expect_output(best_vector(iris,5,3,100,0), c(1,2,3))
a=MeanAccuracy(iris,l,5)
b=WorstAccuracy(iris,l,5)
expect_equal(a, 1)
expect_equal(b,1)
