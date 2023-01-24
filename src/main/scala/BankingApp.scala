import java.io.{File, PrintWriter}
import java.util.Calendar
import scala.io.StdIn.{readDouble, readInt, readLine}


object BankingApp {
  var loginCredential : Map[String,String] = Map("Rin" -> "Rin123", "Swet" -> "Swet123", "Cindy" -> "Cindy123")
  var bankBalance : Map[String,Double] = Map("Rin" -> 50.0, "Swet" -> 50.0, "Cindy" -> 100.0)
  var check = false
  var currUser = ""
  def main(args: Array[String]): Unit = {
    try{
      println("Welcome to our banking app. Please select  a valid option from our menu. \n\t 1) Login \n\t 2) Register\n")
      print("Enter your choice here: ")
      var userInput = readInt()
      if (userInput == 1) {
        print("Enter your Username here: ")
        val userName = readLine()
        currUser = userName
        print("Enter your Password here: ")
        val password = readLine()
        login(userName, password)
        if(check == false){
          println("\nInvalid Username/Password. Please try again\n")
          main(args)
        }
      }
      else if(userInput == 2)
      {
        registerUser()
        println("\nProceeding to Login Page\n")
        print("Enter your Username here: ")
        val userName = readLine()
        currUser = userName
        print("Enter your Password here: ")
        val password = readLine()
        login(userName, password)
        loginCredential.keys.foreach { key=>
          println("Username " + key)
          println("Password " + loginCredential(key))

        }
      }
      else{
        var args = Array[String]()
        main(args)
        println("Invalid input. Please try again.")
      }

    }catch{
      case x: NumberFormatException =>
        {
          main(args)
        }
    }

    if (check) {
     homePage()
    }
  }

  def login(Username: String, Password: String): Unit = {
    if (loginCredential.contains(Username) && Password == loginCredential(Username)) {
      println("")
      check = true
    }
  }

  def registerUser(): Unit = {
      print("Please enter a Username: ")
      var tempInput1 = readLine()
      print("Please enter a Password: ")
      var tempInput2 = readLine()
      loginCredential = loginCredential + (tempInput1 -> tempInput2)
      bankBalance = bankBalance + (tempInput1 -> 0)
  }

  def receipt(userName: String, transferAmount: Double, transfer: Int): Unit = {
    print("Do you want a receipt for your transaction? \n\t1) Yes  \n\t2) No\n")
    var userInput = readInt()
    if (userInput == 1) {
      if(transfer == 0){
        val r = scala.util.Random
        val now = Calendar.getInstance().getTime()
        val pw = new PrintWriter(new File("Receipt.txt"))
        pw.write("\t\t The Banking App\n\t" + now + "\n\nTransaction Type is: Transfer Money\n" +
          "\n" + currUser + " Transferred: $" + transferAmount + " into the " + userName + "'s bank account.\n\nNow you have $" +
          bankBalance(currUser) + " in your account.\n\nYour receipt number is: " + r.nextInt().abs)
        pw.close()
      }else if (transfer == 1) {
        val r = scala.util.Random
        val now = Calendar.getInstance().getTime()
        val pw = new PrintWriter(new File("Receipt.txt" ))
        pw.write("\t\t The Banking App\n\t" + now + "\n\nTransaction Type is: Withdraw\n" +
          "\n" + currUser + " Withdrawn: $" + transferAmount + " from the bank account.\n\nNow you have $" +
          bankBalance(currUser) + " in your account.\n\nYour receipt number is: " + r.nextInt().abs)
        pw.close()
      }else if (transfer == 2){
        val r = scala.util.Random
        val now = Calendar.getInstance().getTime()
        val pw = new PrintWriter(new File("Receipt.txt" ))
        pw.write("\t\t The Banking App\n\t" + now + "\n\nTransaction Type is: Deposit\n" +
          "\nYou deposited: $" + transferAmount + " into the bank account.\n\nNow you have $" +
          bankBalance(currUser) + " in your account.\n\nYour receipt number is: " + r.nextInt().abs)
        pw.close()
      }

    }
  }

  def homePage(): Unit ={
    try{
      print("Go to home page\n")
      print("\t1) Transfer\n\t2) Deposit\n\t3) Withdraw\n\t4) Change Password\n\n")
      print("Enter your choice here: ")
      var userInput = readInt()
      if (userInput == 1) {
        println("Transfer Page\n")
        print("Who would you like to transfer to?\n")
        bankBalance.keys.foreach { key =>
          println("\t" + key)

        }
        print("Enter the username: ")
        var userName = readLine()
        try{
          if(currUser == userName){
            println("\nCannot transfer money to yourself\n")
            homePage()
          }
          if(loginCredential.contains(userName)){
            print("How much money do you want to transfer? ")
            var transferAmount = readDouble()
            if (bankBalance(currUser) - transferAmount < 0){
              println("No sufficient fund, please deposit money first\n")
              homePage()
            }
            var currentBalance = bankBalance(userName) + transferAmount
            var currentUserBalance = bankBalance(currUser) - transferAmount
            bankBalance = bankBalance + (currUser -> currentUserBalance)
            println("Money has been successfully transferred to " + userName)
            println("Your account balance now is: " + bankBalance(currUser))
            bankBalance = bankBalance + (userName -> currentBalance)
            println( userName + "'s balance is " + bankBalance(userName))
            try{
              receipt(userName,transferAmount, 0)

            }catch{
              case x: NumberFormatException =>
              {
                println("Please choose a valid Option")
                receipt(userName,transferAmount, 0)
              }
            }

          } else {
            println("\nPerson does not exist\n")
            homePage()
          }

        }catch{
          case x: NoSuchElementException => {
            homePage()
          }
          }
        }
      if (userInput == 2){
        println("Deposit Page")
        print("How much money do you want to deposit? ")
        var userMoney = readDouble()
        var currBalance = bankBalance(currUser) + userMoney
        bankBalance = bankBalance + (currUser -> currBalance)
        println("Now you have $" + bankBalance(currUser) + " in your account")
        try{
          receipt(currUser,userMoney, 2)

        }catch{
          case x: NumberFormatException =>
          {
            println("Please choose a valid Option")
            receipt(currUser,userMoney, 2)
          }
        }

      }
      if (userInput == 3){
        println("Withdraw Page")
        print("How much money do you want to withdraw? ")
        var userMoney = readDouble()
        if (bankBalance(currUser) - userMoney < 0){
          println("No sufficient fund, please deposit money first\n")
          homePage()
        }
        var currBalance = bankBalance(currUser) - userMoney
        bankBalance = bankBalance + (currUser -> currBalance)
        println("Now you have $" + bankBalance(currUser) + " in your account")

        try{
          receipt(currUser,userMoney, 1)

        }catch{
          case x: NumberFormatException =>
          {
            println("Please choose a valid Option")
            receipt(currUser,userMoney, 1)
          }
        }

        }

      if (userInput == 4){
        println("Change Password Page")
        var confirm = false
        print("Enter your old password: ")
        val userOldPass = readLine()
        while (!confirm) {
          if (userOldPass == loginCredential(currUser)) {
            print("Now enter your new password: ")
            val userNewPass = readLine()
            print("Please confirm your new password: ")
            val userNewPass2 = readLine()
            if (userNewPass2 != userNewPass) {
              println("The Password entered do not match the new password")
            } else {
              loginCredential = loginCredential + (currUser -> userNewPass)
              confirm = true
            }

          } else if (userOldPass != loginCredential(currUser)) {
            println("Invalid password please try again")
            homePage()
          }
        }
      }


      var confirm = false
      while(!confirm){
        print("Please select an action: \n\t1) Continue\n\t2) Logout \n\t3) Exit the App\n\n")
        print("Enter your choice here: ")
        userInput = readInt()
        if (userInput <= 3 && userInput >= 0){
          if (userInput == 1){
            homePage()
          }else if (userInput == 2){
            println("Logging out of the Application\n\n")
            var args = Array[String]()
            main(args)
          }
          else if (userInput == 3) {
            println("Exiting the Application\n")
            System.exit(0)
          }
          confirm = true

        }else{
          confirm = false
        }
      }

    }catch{
      case x: NumberFormatException =>
      {
        println("An error has occurred please try again.")
        homePage()
      }
    }

  }
}
