#Load

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/ClosedTriangles.RData")
prop.triangle.close.All=prop.triangle.close.All[!is.na(prop.triangle.close.All$iter),]
closure.triangle.All=prop.triangle.close.All[!is.na(closure.triangle.All$iter),]