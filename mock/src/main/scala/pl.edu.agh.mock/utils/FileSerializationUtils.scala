package pl.edu.agh.mock.utlis

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

object FileSerializationUtils {
  def serializeInFile[Type](obj: Type, fileName: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fileName))
    oos.writeObject(obj)
    oos.close()
  }

  def deserializeFromFile[Type](fileName: String): Type = {
    val ois = new ObjectInputStream(new FileInputStream(fileName))
    val obj = ois.readObject.asInstanceOf[Type]
    ois.close()
    obj
  }
}
