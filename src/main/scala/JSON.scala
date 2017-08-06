package JSON

import netscape.javascript.JSObject

object JSON {
  def main(args:Array[String]): Unit ={

    val code1=JsString("#FF0000")
    val code2=JsString("#F0FFF")
    val code3=JsString("#CD853F")

    val code4=JsNumber(123)
    val code5=JsNumber(124)
    val code6=JsNumber(125)

    val predicate1=JsBoolean(true)
    val predicate2=JsBoolean(false)

    val colors1 = JsObject(Map("red" -> code1, "azure" -> code2, "peru" -> code3))
    val colors2 = JsObject(Map("blue" -> code4, "green" -> code5, "pink" -> code6))

    val people=JsArray(List(predicate1, predicate2 ))

    val objects=JsArray(List(colors1, colors2,people))

    print(createJsonString(objects))



  }

  trait JsAbstract
  case class JsObject(fields:Map[String,JsAbstract]) extends JsAbstract
  case class JsArray(elements:List[JsAbstract]) extends JsAbstract
  case class JsBoolean(value:Boolean) extends JsAbstract
  case class JsString(value:String) extends JsAbstract
  case class JsNumber(value:Double) extends JsAbstract
  def createJsonString(jsAbstract: JsAbstract):String={

    jsAbstract match{
      case o: JsObject=> {
        var answer="{"
        var it=o.fields.iterator;
        var current=it.next()
        answer+="\""+current._1+"\":\""
        answer+=createJsonString(current._2)
        answer+="\""
        while(it.hasNext){
          answer+=","
          current=it.next()
          answer+="\"" + current._1 + "\":"
          answer+=createJsonString(current._2)
        }
        answer+="}"
        return answer
      }
      case o:JsArray =>{
        var answer="["
        for(el <- o.elements){
          if(el!=o.elements.head) answer+=","
          answer+=createJsonString(el)
        }
        answer+="]"
        return answer
      }
      case o:JsBoolean=> return o.value.toString
      case o:JsString=>return "\""+o.value+"\""
      case o:JsNumber=>return o.value.toString
    }

  }
}
