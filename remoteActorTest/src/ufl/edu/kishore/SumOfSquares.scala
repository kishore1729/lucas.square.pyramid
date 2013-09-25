package ufl.edu.kishore
import scala.actors.Actor
import scala.actors.Actor._

case class params(first:Int, last: Int, k: Int){}

class project1_boss(N:Int,k:Int) extends Actor {	
	val numActors = 6
	def act() {	  
		var count = 1
		var first = 1
		var split = Math.ceil(N.toDouble/numActors).toInt;
		var last = split;

		while(count <= numActors){
			if(count == numActors) last = N
			count +=1
			val workerParams = new params(first,last,k)
			first = last + 1
			last = last+split
			val w = new stwork
			w.workers ! workerParams		  
		}
		count = 1
		var finalOut = List[Int]()
		while(count <= numActors){
			count += 1
			receive{
				case l:List[Int]=> finalOut :::= l //append results into finalOut
			}
		}		

		//Sorting and printing
		var out:Array[Int] = finalOut.toArray
		util.Sorting.quickSort(out)
		for(elem <- out) {print(elem);print(", ")}
	}

}

class stwork {
	def workers: Actor = {
		val worker = actor{
			react{
		case myParams:params =>	
		var sqsum = 0.0
	    var sqroot = 0.0
		val N = myParams.last
		val k = myParams.k
		val x = myParams.first
		var solutions = List[Int]()
		for (i <- x to N){
			sqsum = 0.0
			for(j <- i to (i+k-1))sqsum += j.toDouble*j.toDouble
			sqroot = Math.sqrt(sqsum)
			if(!((sqroot%1)>0))solutions ::= i
		}
		sender ! solutions
			}
		}
		worker
	}
}

object SumOfSquares {
	def main(args: Array[String]){
	  if(args.length != 2){
	    println("Incorrect input parameters")
	  }
	  else {
		val boss = new project1_boss(args(0).toInt,args(1).toInt)
		boss.start
	  }
	}
}