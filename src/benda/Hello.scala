package benda

import java.io.File
import scala.io.Codec
import scala.io.Source
import java.nio.charset.CodingErrorAction
import scala.xml.{Elem, XML}
import scala.xml.factory.XMLLoader
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import scala.collection.immutable.ListMap



object Hello extends App {
	
  object TagSoupXmlLoader {	    
	    private val factory = new SAXFactoryImpl()	  
	    
	    def get(): XMLLoader[Elem] = {
	      XML.withSAXParser(factory.newSAXParser())
	    }
	}
  
  override def main(args: Array[String]) {
	
	def dir1 = "C:/scala/eclipse/workspace/Benda/benda"
	def dir2 = "C:/scala/eclipse/workspace/Benda/bible"
	  
	implicit val codec = Codec("UTF-8")
		codec.onMalformedInput(CodingErrorAction.REPLACE)
		codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
			
	/**
	 * Read all files in a given directory and concat them to a single string.
	 */
	def readDirAsString(dir: String) : String = {
		val lHtml = (for {
				file <- new File(dir).listFiles.toIterator if file.isFile						
			} yield Source.fromFile(file).mkString).toList				

		val lText = (for {
		  fileStr <- lHtml		 
		} yield  TagSoupXmlLoader.get().loadString(fileStr) \\ "body" text).mkString(" ")		
		lText
	}
				
	/**
	 * @return a list of all single words (1-grams) in the given string.
	 */
	def words (text : String) : List[String] = text.filterNot((1 to 9).toSet contains _ ).filterNot(Consts.NIKUD contains _ ).filterNot(".,;:!/?\"".toSet).filter(_.toInt < Consts.MAX).replaceAll("-"," ").replaceAll("\n"," ").replaceAll("\r"," ").replaceAll("\\n"," ").replaceAll("\\r"," ").replaceAll("\t"," ").split(" ").toList.map(_.trim).filterNot(_ == "" ).filterNot(_(0).toInt == 160 )
  	
	//val x = words(readDirAsString(dir1))  	
	//x.zipWithIndex foreach {case (value,index) => println( index + "---" + value + "---")}
	
	/**
	 * @return a map from each word to the number of times it occurs in the input list.
	 */
	def occurences (list : List[String]) = list.foldLeft(Map[String, Int]() withDefaultValue 0)  { (m, x) => m + (x -> (m(x) + 1)) }
//	def occurencesList (list : List[String]) = {list.map(x => (x, list.count(_ == x)))}
	
//	val occ1 = (occurences(words(readDirAsString(dir1))).toList sortBy {_._2}).toMap
//	val occ2 = (occurences(words(readDirAsString(dir2))).toList sortBy {_._2}).toMap
	
	/**
	 * @return (word,diff), where word is identical to the input word, 
	 * and diff is the number of times it occurs in occA
	 * minus the number of times it occurs in occB
	 */
	def diff (occA : Map[String, Int], occB: Map[String, Int], word : String) : (String, Int) = {
	  if (occB.contains(word))
	    (word, occA(word) - occB(word))
	  else
	    (word,occA(word))
	}
	
	/**
	 * @return  non-null if the input word occurs only in occA.
	 */
	def onlyInOne (occA : Map[String, Int], occB: Map[String, Int], word : String) : (String, Int) = {
	  if (occB.contains(word))
	    null
	  else
	    (word,occA(word))
	}
	
//	val l = (for {(aword, aval) <- occ1} yield diff(occ1, occ2, aword)).toList  sortBy {_._2}
//	val o = (for {
//	  (aword, aval) <- occ1
//	  if (onlyInOne(occ1, occ2, aword) != null)
//	  } yield (aword, aval)).toList  sortBy {_._2} reverse
	
	
	//println(o.filter(_._2 > 0) mkString("...\n"))
	//println(l.filter(_._2 > 150) mkString("\n"))
	//println(occ1.filter(_._2 > 150) mkString("\n"))
	//println(occ2.filter(_._2 > 6) mkString("\n"))
	//println(occ1.reverse.mkString("\n"))
		
	//def ngrams (n: Int, w: List[String]) = (for( i <- n to n) yield w.sliding(i).map(p => p.toList)).flatMap(x => x)
	
	//val occ = (words(readDirAsString(dir1))).toList
	
	//println(ngrams(2, occ).take(1000).takeRight(500))
	
	//val trgt = (words(readDirAsString(dir2))).toList
	//println("trgt " + trgt.take(1000))
	
	/**
	 * subroutine of nGramList
	 */
	def ngrams2(n: Int, words: List[String]) = {
    // exclude 1-grams
    (n to n).map { i => words.toArray.sliding(i).toStream }
      .foldLeft(Stream[Array[String]]()) {
        (a, b) => a #::: b
      }
  }
	
	/**
	 * @return the list of all n-grams in the input string.
	 */
	def nGramList (n:Int, dir: String) : List[String]= {
		val vec = ngrams2(n, words(readDirAsString(dir)))
		(for (l <- vec) yield (l.mkString(" "))).toList
		}
	
	//println("4-grams  " + nGramList(4,dir1).take(100))
	
	//val occs = occurences(nGramList(4,dir1)).toList.toSet.toList
	val occs = /*occurencesList*/(nGramList(4,dir1)).toList
	
	val trgs = occurences(nGramList(4,dir2)).toList.toSet.toList
	val trgs3 = occurences(nGramList(3,dir2)).toList.toSet.toList
	val trgs2 = occurences(nGramList(2,dir2)).toList.toSet.toList
	def filterOccs(l : List[(String, Int)], min: Int, max: Int) : List[(String, Int)] = {
			l.filter(_._2 >= min).filter(_._2 <= max)  		
		}
	
	
	val foccs = occs//filterOccs(occs, 1, 1000)
	//println("foccs " + foccs.sortBy ( _._2 ).reverse.take(100))
	val ftrgs4 = filterOccs(trgs, 1, 100).toSet.toList
	val ftrgs3 = filterOccs(trgs3, 1, 100).toSet.toList
	val ftrgs2 = filterOccs(trgs2, 1, 5).toSet.toList
	val ftrgts1 = ftrgs4.map(_._1)
	//println("bible " + ftrgs.sortBy ( _._2 ).reverse.take(100))
	//println("targets: " + ftrgts1.takeRight(900).take(100))
	//println(foccs.take(1000).take(900))
	
	def inTarget (tr4 : List[String], tr3 : List[String], tr2 : List[String], word : String) : String = {
	  if (tr4.contains(word))
	    word
	  else
	  {
	    val w3 = word.split(" ").dropRight(1).mkString(" ")	    
	    if (tr3.contains(w3))
	    {	      
	    	w3
	    }
	    else
	    {
	    	val w2 = w3.split(" ").dropRight(1).mkString(" ")	    
	    			if (tr2.contains(w2))
	    			{	      
	    				w2
	    			}
	    			else
	    				null
	    }
	  }
	}
	
	
	
	def inTargetScore (tr4 : List[String], tr3 : List[String], tr2 : List[String], word : String) : (Int, String) = {
	  if (tr4.contains(word))
	    (4, word)
	  else
	  {
	    val w3 = word.split(" ").dropRight(1).mkString(" ")	    
	    if (tr3.contains(w3))
	    {	      
	    	(3,w3)
	    }
	    else
	    {
	    	val w2 = w3.split(" ").dropRight(1).mkString(" ")	    
	    			if (tr2.contains(w2))
	    			{	      
	    				(2,w2)
	    			}
	    			else
	    				(0, null)
	    }
	  }
	}
	
	
	def calc( a:Int, b: Int, c:Int, d: Int) : Int = {
	  if( d >= 4)
	    d
	    else
	      if (c >= 3)
	        max(a,c)
	        else
	          if (b >= 2)
	            max(a,b)
	            else
	              a	        
	}
	
	//val both = (for {(aword) <- foccs} yield inTarget(ftrgs4.map(_._1),ftrgs3.map(_._1),ftrgs2.map(_._1), aword)).toList.toSet.toList
	//println("both: " + both.length)
	//println(both.reverse.mkString("\n"))
	//println(foccs.take(20))
	val bothScore = (for {(aword) <- foccs} yield (aword.split(" ").take(1).mkString, inTargetScore(ftrgs4.map(_._1),ftrgs3.map(_._1),ftrgs2.map(_._1), aword))).toList
	val fixScore = for (i <- bothScore.length -1 to 3 by -1) yield (bothScore(i)._1, calc(bothScore(i)._2._1, bothScore(i-1)._2._1, bothScore(i-2)._2._1, bothScore(i-3)._2._1), bothScore(i)._2._2)
	//println("both: " + fixScore.length)
	//println(fixScore.reverse.mkString("\n"))
	println(htmlr(fixScore.toList.reverse))
	
	def htmlr(xs : List[(String, Int, String)]) : String = {
	  val a = htmlr2(xs)
	  "<html><head><META http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"><style> .s2 {background-color:aliceblue;}	.s3 {background-color:cyan;}	.s4 {background-color:cornflowerblue;}</style></head><body dir='rtl' width='600'>" + a.mkString + "</body></html>"	    
	}
	
	def htmlr2(xs : List[(String, Int, String)]) : String = 
	xs match {
      case Nil => ""
      case x :: Nil =>  tag(x._1,x._2,x._3, null)
      case x :: tail => {
        tag(x._1,x._2,x._3, tail.head._3) + " " + htmlr2(tail)        
      }
	}
	    
	def tag(x: String, y: Int ,z: String, nextZ: String) : String = {
	  if (y == 0)
	    x + " "
	    else
	  if (z == null)
		  ""//"<div style='display: inline' class='s" + y + "'>" + x + "</div>&nbsp;"
		  else
		  {		    
		    val zToNext = 
		      (if (nextZ == null)
		    	  z
		    	  else
		    	  {
		    	   val nextX = nextZ.split(" ").toList.take(1).mkString		    	   
		    	   val index = z.indexOf(nextX)
		    	   //println("nextX " + nextX + "*" + index)
		    	   z.take(index)
		    	  } )		      
		    
		    //println(zToNext)
		    "<div style='display: inline' class='s" + y + "'><a target='_blank' href='http://sparks.simania.co.il/bibleSearch.php?query=%22" + z + "%22'>" + zToNext + "</a></div>"
		  }
		}
	
	
    }                 
	
   
	def levenshtein(str1: String, str2: String): Int = {
    val lenStr1 = str1.length
    val lenStr2 = str2.length
 
    val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)
 
    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j
 
    for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
      val cost = if (str1(i - 1) == str2(j-1)) 0 else 1
 
      d(i)(j) = min(
        d(i-1)(j  ) + 1,     // deletion
        d(i  )(j-1) + 1,     // insertion
        d(i-1)(j-1) + cost   // substitution
      )
    }
 
    d(lenStr1)(lenStr2)
  }
 
  def min(nums: Int*): Int = nums.min
  def max(nums: Int*): Int = nums.max
		
	
}	