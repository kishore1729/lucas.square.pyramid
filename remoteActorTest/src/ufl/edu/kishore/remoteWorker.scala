import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote.Node
import scala.actors.remote.RemoteActor._

class remWorkerUnit extends Actor {
	  def act() {
	  println("Remote Worker firsted")

	  val localhost = java.net.InetAddress.getLocalHost.getHostName
	  var hostPart = localhost.split("-")(1)
	  val port = hostPart.slice(0,2)
	  println("Port : "+(9000 + (port.toInt))+" name : "+"remoteActor"+port);
	  alive(9000 + (port.toInt))
	  register(Symbol("remWorker"+port),self)

	  receive{
	    case "myOwnComplexStop" => 
	      println("Stopping worker")
	      exit()

	    case cmdStr : String =>
	      	println("Command received from Server")
	      	println(cmdStr)
			var splitStr = cmdStr.split(",")
			var first = splitStr(0).toInt
			val N = splitStr(1).toInt
			val k = splitStr(2).toInt
			val numActors = splitStr(3).toInt

			//Divide the work among Actors and spawn them
			var count = 1
			var split = Math.ceil((N - first).toDouble/numActors).toInt;
			var last = first + split;
			while(count <= numActors){
			  if(count == numActors)
			    last = N;
			  count = count +1
			  val params = List(first,last,k)
			  first = last + 1;
			  last = last + split;
			  val w = new work
			  w.workers ! params		  
			}
			count = 1
			var out = List[Int]()
			while(count <= numActors){
			  count = count + 1
			  receive{
			    case l:List[Int]=> out :::=l
			  }
			}
			var finalStr = ""
			for(elem <- out)finalStr+=(elem.toString + ", ")
			println("Slasting this to the main boss "+finalStr)
			sender ! finalStr
	}
	}

}

class work{
  	def workers: Actor = {
	  val worker = actor{
	    react{
	      	case params:List[Int] =>
	      	  var sum = 0.0
			  var sq = 0.0
			  val st = params(0)
			  val N = params(1)
			  val k = params(2)
			  var out = List[Int]()
			  for (i <- st to N){
			    sum = 0.0
			    for(j <- i until (i+k)){
			      sum += j.toDouble*j.toDouble;
			    }
			    sq = Math.sqrt(sum)
			    if(!((sq%1)>0)){
			      out ::= i
			    }
		   }  
	sender ! out
	    }
	  }
	worker
	}
}
object remoteWorker  {
 def main(args: Array[String]){
  val remWorker = new remWorkerUnit
  remWorker.start
  }
}
