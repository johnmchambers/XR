require(XR)
ev <- XRPython::RPython()
po = ev$proxyObject
x1 = ev$Convert(1:3)
ll = list(x=1, y = matrix(1:12,3,4))
ll$z <- x1
asServerObject(ll, po)
