/**
  * Created by SouNourS on 20/12/2016.
  */

import net.psforever.packet._
import scodec.bits._
import scala.io.Source
import java.io.FileWriter
import util.control.Breaks._
import java.io.File

object Xtools {

  def main(args: Array[String]): Unit = {
    for (file <- new File("D:\\decode_capture\\").listFiles) {
      val FileToRead = file.toString
      val FileToWrite = "F:\\_" + FileToRead.drop(3).dropRight(FileToRead.toString.length - FileToRead.toString.indexOf(".txt")) + "_decoded.txt"
      val fw = new FileWriter(FileToWrite, true)

      for (line <- Source.fromFile(FileToRead).getLines()) {

        val linetest = line substring(0, 2)
        val linetest2 = line substring(0, 17)
        if (linetest != "IF") {

          fw.write(System.getProperty("line.separator") + "#" + line + System.getProperty("line.separator"))
//          println(System.getProperty("line.separator") + "#" + line)
          var isSlotted = -1
          var isMultiPacketEx = -1
          var isMultiPacket = -1
          var isMultiPacketExSlot = -1
          var isHandleGamePacket = -1
          var AfterDecode = Fdecode(line.drop(line.lastIndexOf(' ')))
          var AfterDecode2 = ""
          var AfterDecode3 = ""
          var AfterDecode4 = ""
          var AfterDecode5 = ""

          isMultiPacket = AfterDecode.indexOf("Successful(MultiPacket(")
          isSlotted = AfterDecode.indexOf("Successful(SlottedMetaPacket(")
          isMultiPacketEx = AfterDecode.indexOf("Successful(MultiPacketEx(")

          if (isSlotted != 0 && isMultiPacket == -1 && isMultiPacketEx == -1) {
            fw.write(AfterDecode + System.getProperty("line.separator"))
            //        println(AfterDecode )
          }

          if (isMultiPacket != -1) {
            fw.write(AfterDecode + System.getProperty("line.separator"))
            //        println(AfterDecode)
            var xindex1 = 1
            var zindex1 = 0
            var boucle1 = 0
            while (boucle1 != -1) {
              AfterDecode2 = Fdecode(AfterDecode.drop(AfterDecode.indexOf(" 0x", xindex1) + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")", zindex1 + 1)))
              xindex1 = AfterDecode.indexOf(" 0x", xindex1) + 1
              boucle1 = AfterDecode.indexOf(" 0x", xindex1)
              zindex1 = AfterDecode.indexOf(")", zindex1) + 1
              isSlotted = AfterDecode2.indexOf("Successful(SlottedMetaPacket(")
              if (isSlotted == 0) {
                fw.write("> " + AfterDecode2 + System.getProperty("line.separator"))
                //            println("> " + AfterDecode2)
                AfterDecode3 = Fdecode(AfterDecode2.drop(AfterDecode2.lastIndexOf(" 0x") + 3).dropRight(AfterDecode2.length - AfterDecode2.indexOf(")")))
                isMultiPacketExSlot = AfterDecode3.indexOf("Successful(MultiPacketEx(")
                if (isMultiPacketExSlot != -1) {
                  fw.write("-> " + AfterDecode3 + System.getProperty("line.separator"))
                  //                println("-> " + AfterDecode3)
                  var xindex2 = 1
                  var zindex2 = 0
                  var boucle2 = 0
                  while (boucle2 != -1) {
                    AfterDecode4 = Fdecode(AfterDecode3.drop(AfterDecode3.indexOf(" 0x", xindex2) + 3).dropRight(AfterDecode3.length - AfterDecode3.indexOf(")", zindex2 + 1)))
                    xindex2 = AfterDecode3.indexOf(" 0x", xindex2) + 1
                    boucle2 = AfterDecode3.indexOf(" 0x", xindex2)
                    zindex2 = AfterDecode3.indexOf(")", zindex2) + 1
                    fw.write("--> " + AfterDecode4 + System.getProperty("line.separator"))
                    //                println("--> " + AfterDecode4 )
                  }
                  isMultiPacketEx = -1
                  isMultiPacketExSlot = -1
                } else {
                  fw.write("-> " + AfterDecode3 + System.getProperty("line.separator"))
                  //                println("-> " + AfterDecode3 )
                }
              } else {
                fw.write("> " + AfterDecode2 + System.getProperty("line.separator"))
                //              println("> " + AfterDecode2 )
              }
            }
          }
          if (isSlotted == 0 && isMultiPacket == -1) {
            fw.write(AfterDecode + System.getProperty("line.separator"))
            //        println(AfterDecode)
            AfterDecode = Fdecode(AfterDecode.drop(AfterDecode.lastIndexOf(" 0x") + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")")))
            isMultiPacketExSlot = AfterDecode.indexOf("Successful(MultiPacketEx(")
            isHandleGamePacket = AfterDecode.indexOf("Successful(HandleGamePacket(")
            if (isHandleGamePacket != -1) {
              fw.write("> " + AfterDecode + System.getProperty("line.separator"))
//              println("> " + AfterDecode )
              if(AfterDecode.lastIndexOf(" 0x") != -1){
              AfterDecode5 = Fdecode(AfterDecode.drop(AfterDecode.lastIndexOf(" 0x") + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")")))
              fw.write("-> " + AfterDecode5 + System.getProperty("line.separator"))
//              println("-> " + AfterDecode5 )
              }
            }
            if (isMultiPacketExSlot == -1 && isHandleGamePacket == -1) {
              fw.write("> " + AfterDecode + System.getProperty("line.separator"))
              //          println("> " + AfterDecode )
            }
            if (isMultiPacketExSlot != -1 && isHandleGamePacket == -1) {
              fw.write("> " + AfterDecode + System.getProperty("line.separator"))
              //          println("> " + AfterDecode )
              var xindex3 = 1
              var zindex3 = 0
              var boucle3 = 0
              while (boucle3 != -1) {
                AfterDecode2 = Fdecode(AfterDecode.drop(AfterDecode.indexOf(" 0x", xindex3) + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")", zindex3 + 1)))
                fw.write("-> " + AfterDecode2 + System.getProperty("line.separator"))
                //            println("-> " + AfterDecode2)
                xindex3 = AfterDecode.indexOf(" 0x", xindex3) + 1
                boucle3 = AfterDecode.indexOf(" 0x", xindex3)
                zindex3 = AfterDecode.indexOf(")", zindex3) + 1
              }
            }
          }
          if ((isMultiPacketEx != -1 || isMultiPacketExSlot != -1) && isSlotted != 0) {
            fw.write(AfterDecode + System.getProperty("line.separator"))
            //        println( AfterDecode )
            var xindex = 1
            var zindex = 0
            var boucle = 0
            while (boucle != -1) {
              AfterDecode2 = Fdecode(AfterDecode.drop(AfterDecode.indexOf(" 0x", xindex) + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")", zindex + 1)))
              fw.write("> " + AfterDecode2 + System.getProperty("line.separator"))
              //          println("> " + AfterDecode2)
              xindex = AfterDecode.indexOf(" 0x", xindex) + 1
              boucle = AfterDecode.indexOf(" 0x", xindex)
              zindex = AfterDecode.indexOf(")", zindex) + 1
            }
          }

        }

      }
      fw.close()
    }
  }

  def Fdecode(toto: String): String = {
    val ADecode = PacketCoding.DecodePacket(ByteVector.fromValidHex(toto)).toString;
    return ADecode
  }

}