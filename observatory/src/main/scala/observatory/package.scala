import org.apache.spark.sql.SparkSession
import org.apache.spark.{SparkConf, SparkContext}

package object observatory {

  @transient lazy val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Capstone")
      .config("spark.master", "local")
      .getOrCreate()

  @transient lazy val sc: SparkContext = spark.sparkContext


  def homeFile(relativePath: String): java.io.File = new java.io.File(s"${sys.env("HOME")}/$relativePath")

}
