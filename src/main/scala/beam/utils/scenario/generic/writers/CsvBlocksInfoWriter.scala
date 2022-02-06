package beam.utils.scenario.generic.writers

import beam.utils.csv.CsvWriter
import beam.utils.scenario.BlockInfo
import com.typesafe.scalalogging.LazyLogging

trait CsvBlocksInfoWriter {
  def write(path: String, xs: Iterable[BlockInfo]): Unit
}

object CsvBlocksInfoWriter extends CsvBlocksInfoWriter with LazyLogging {
  private val headers: Array[String] = Array("blockId", "x", "y")

  override def write(path: String, xs: Iterable[BlockInfo]): Unit = {
    val csvWriter = new CsvWriter(path, headers)
    try {
      xs.foreach { household =>
        csvWriter.write(
          household.blockId.id,
          household.x,
          household.y
        )
      }
      logger.info(s"Wrote blocks information to $path")
    } finally {
      csvWriter.close()
    }
  }
}
