object Map {
    
    implicit class ExtendedList[T](val value: List[T]) extends AnyVal {
        
        def myMap(pred: (T => T)) = {
            def map(current: List[T], modified: List[T]): List[T] = {
                if(current.isEmpty) {
                    modified.reverse
                } else {
                    val head = current.head
                    map(current.tail, pred(head) :: modified)
                }
            }
            
            map(value, List())
        }
        
    }
    
    def main(args: Array[String]) {
        println(List(1,2,3,4,5,6,7,8,9,10).myMap((x:Int) => x*x))
        println(List("Jori", "joni", "Jani", "joona", "Joonas", "jaakko").myMap((x:String) => x.toUpperCase))
    }
}