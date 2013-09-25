import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote.Node
import scala.actors.remote.RemoteActor._


class ServerMain(N:Int, k:Int, nA:Int) extends Actor {
	val remWorkers = Array("00","01","02","03","04")		//lin114-01 = "01"
	var port = 0
	var remMsg:String = ""
	var first = 1
	var split = Math.ceil(N.toDouble/remWorkers.length).toInt;
	var last = split;
	def act(){
		println("Server Running")
		for(i <- 0 until remWorkers.length){
			port = 9000+remWorkers(i).toInt
			var remWorker = select(Node("lin114-"+remWorkers(i)+".cise.ufl.edu", port), Symbol("remWorker"+remWorkers(i)))
			if(i == remWorkers.length) last = N;
			remMsg = first + "," + last + "," + k + "," +nA
			first = last + 1; last = last + split
			remWorker ! remMsg
		}
		println("Receving from Remote Workers")
		for(i <- 0 until remWorkers.length){
			receive{
			case n:String=>print(n)
			}
		}
		//TO DO: should handle socket close --> implement timeout
	}
}

object Server {
	def main(args: Array[String]){
		if(args.length != 3){
			println("Incorrect input parameters")
		}
		else {
			val smain = new ServerMain(args(0).toInt,args(1).toInt,args(2).toInt)// N, k, number of actors in remote machine
			smain.start
		}
	}
}