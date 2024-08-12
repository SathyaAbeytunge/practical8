object Q1{

  def encryptCaesar(plaintext: String, shift: Int): String = {
    plaintext.map { char =>
      if (char.isLetter) {
        val shiftAmount = shift % 26
        val base = if (char.isLower) 'a' else 'A'
        ((char - base + shiftAmount) % 26 + base).toChar
      } else {
        char
      }
    }
  }

  def decryptCaesar(ciphertext: String, shift: Int): String = {
    ciphertext.map { char =>
      if (char.isLetter) {
        val shiftAmount = shift % 26
        val base = if (char.isLower) 'a' else 'A'
        ((char - base - shiftAmount + 26) % 26 + base).toChar
      } else {
        char
      }
    }
  }

  def caesarCipher(text: String, shift: Int, operation: String): String = {
    operation.toLowerCase match {
      case "encrypt" => encryptCaesar(text, shift)
      case "decrypt" => decryptCaesar(text, shift)
    }
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Hello, World!"
    val shift = 3

    val encryptedText = caesarCipher(plaintext, shift, "encrypt")
    println(s"Encrypted: $encryptedText")

    val decryptedText = caesarCipher(encryptedText, shift, "decrypt")
    println(s"Decrypted: $decryptedText")
  }
}

