package io.joern.clang2cpg.parser.clang
import io.joern.clang2cpg.parser.ClangParser
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** Lossless completed-capture storage. Failed captures remain unpacked for diagnosis. */
object ClangArchive {
  def complete(directory: Path): Path = {
    val archive = directory.resolveSibling(directory.getFileName.toString + ".zip")
    val temporary = archive.resolveSibling(archive.getFileName.toString + ".partial")
    require(!Files.exists(archive) && !Files.exists(temporary), s"Evidence archive already exists: $archive")
    val listing = Files.list(directory)
    val files = try listing.iterator().asScala.toVector.sortBy(_.getFileName.toString) finally listing.close()
    require(files.nonEmpty && files.forall(p => Files.isRegularFile(p) && !Files.isSymbolicLink(p)), s"Invalid evidence directory: $directory")
    val expected = files.map(p => p.getFileName.toString -> ClangParser.digest(p)).toMap
    val out = new ZipOutputStream(Files.newOutputStream(temporary))
    try files.foreach { file =>
      out.putNextEntry(new ZipEntry(file.getFileName.toString))
      Files.copy(file, out)
      out.closeEntry()
    } finally out.close()
    val zip = new ZipFile(temporary.toFile)
    try {
      require(zip.size() == files.size, "Evidence archive entry count changed")
      expected.foreach { case (name, digest) =>
        val input = zip.getInputStream(zip.getEntry(name))
        val actual = MessageDigest.getInstance("SHA-256")
        try {
          val buffer = new Array[Byte](65536)
          var count = input.read(buffer)
          while (count != -1) { actual.update(buffer, 0, count); count = input.read(buffer) }
        } finally input.close()
        require(actual.digest().map(v => f"${v & 0xff}%02x").mkString == digest, s"Evidence archive verification failed: $name")
      }
    } finally zip.close()
    Files.move(temporary, archive, StandardCopyOption.ATOMIC_MOVE)
    files.foreach(Files.delete)
    Files.delete(directory)
    archive
  }
}
