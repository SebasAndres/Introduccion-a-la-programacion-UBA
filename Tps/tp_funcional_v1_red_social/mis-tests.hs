-- particion 1
u1 = (1,"alice")
a = (2, "juan")
b = (3, "sebas")
c = (4, "messi")
d = (5, "teo")
e = (6, "monica")
u2 = (7, "bob")

-- particion 2
f = (8, "f")
g = (9, "g")
h = (10, "h")

relaciones = [(u1,a),(u1,b),(u1,c),(a,d),(b,e),(e,u2),(f,g),(g,h)]
usuarios = [u1,a,b,c,d,e,f,g,u2]
posts = []
redF = (usuarios, relaciones, posts)

