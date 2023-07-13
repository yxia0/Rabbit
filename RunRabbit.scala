import Assignment3.RabbitEDSL.Assignment3Embedded.RabbitDSLImpl._

def anim = moveXY(((pure({x_0_2: Int => {y_1_3: Int => (x_0_2) * (y_1_3)}})) <*> (time)) <*> (pure(20)), pure(100), read("turtle")) 
saveToFile(anim, 20, "test.gif")
