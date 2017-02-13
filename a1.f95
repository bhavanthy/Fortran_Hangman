!      game of hangman by dave ahl, digital
!      based on a basic program written by ken aupperle
!            half hallow hills h.s. dix hills ny
!      converted to fortran 77 by m.wirth, april 2012
!
!      Edited by: Bhavanthy Modchalingam (0896901)
       
       program hangman 
       character, dimension(12,12) :: board
       character, dimension(20) :: dashes
       character, dimension(26) :: n
       character(len = 20) :: a, b
       character :: guess, ans
       integer, dimension(50) :: used
       integer :: q, missed, i, w, turnCount, r, l, counter=1, endGameFlag=0, repeat, endGuessFlag, skip
       real :: rnd 

       character(len=20), dimension(50):: dict = [character(len=20):: "gum","sin",&
       "for","cry","lug","bye","fly","ugly","each","from","work","talk","with","self",&
              "pizza","thing","feign","fiend","elbow","fault",&
              "dirty","budget","spirit","quaint","maiden",&
              "escort","pickax","example","tension","quinine",&
              "kidney","replica","sleeper","triangle",&
              "kangaroo","mahogany","sergeant","sequence",&
              "moustache","dangerous","scientist","different",&
              "quiescent","magistrate","erroneously",&
              "loudspeaker","phytotoxic","matrimonial",&
              "parasympathomimetic","thigmotropism"]


       write (*,*) "THE GAME OF HANGMAN"
       do while (endGameFlag == 0)
           call clear_board(board, dashes, n, used)
           w=50
           missed=0 
           endGuessFlag = 0
           if(counter==49) then
               endGameflag = 1
           end if

           !get a word to guess
           rnd=rand()
           q=ceiling(rnd*50)
           do while(used(q) == 1)
               rnd=rand()
               q=ceiling(rnd*50)
           end do
           used(q) = 1; counter=counter+1; turnCount=0
           a = dict(q)
           l = len_trim(a) 
           write (*,*) dashes(1:l)

           do while(endGuessFlag == 0)
             !print previously used letters
               write (*,*) "Here are the letters you used: "
               i = 1
               do while(i < 27)
                   if (n(i) == ' ') then
                     exit
                   end if
                   write (*,'(aa$)') n(i),","
                   i = i+1
               end do

               repeat = 0
               do while (repeat == 0)
                 !check for invaild input
                   write (*,*) " "
                   write (*,*) "What is your guess? "; r=0
                   read (*,*) guess
                   i = 1 
                   do while(i < 27)
                       if (n(i) == " ") then
                           repeat = 1
                           exit 
                       else if (n(i) == guess) then
                           write (*,*) "You guessed that letter before";
                           exit
                       end if
                       i = i+1
                   end do 
                   if(repeat /= 1) then
                       write (*,*) "Invalid character"
                   end if
               end do
               n(i)=guess; turnCount=turnCount+1

               !changes dashes to guessed letter
               i = 1
               do while (i < l+1)
                   if (a(i:i) == guess) then
                       dashes(i) = guess; r=r+1
                   end if
                   i = i+1
               end do

               if (r == 0) then
               !guessed the wrong letter
                   missed=missed+1
                   write (*,*) "Sorry, that letter isn't in the word."
                   call print_man (missed, board)
                   if (missed == 10) then
                       write (*,*) "Sorry, you loose. The word was ", a
                       write (*,*) "You missed that one."
                       endGuessFlag = 1
                       exit
                   end if
               else 
                   !guess was right 
                   skip = 0
                   i = 1
                   do while(i < l+1)
                       if (dashes(i) == "-") then
                           write (*,*) dashes(1:l)
                           write (*,*) "What is your guess for the word? "
                           read (*,*) b
                           if (a == b) then
                               write (*,*) "Right! It took you ",turnCount," guesses"
                               endGuessFlag = 1
                               skip = 1
                               exit
                           else 
                               write (*,*) "Wrong. Try another letter"
                               skip = 1
                               exit
                           end if
                       end if
                       i = i+1
                   end do
                   !got all the letters
                   if (skip == 0) then
                       write (*,*) "You found the word."
                       endGuessFlag = 1
                   end if
               end if
           end do
         if (counter == 50) then
             write (*,*) "You did all the words"
             exit
         end if
         write (*,*) "Do you want another word? (Press Y to continue) "
         read (*,*) ans
         if (ans /= "Y") then
             endGameFlag = 1
         end if
       end do
       write (*,*) "It's been fun! Bye for now."
       write (*,*) "Ending..."
       
       end


       subroutine clear_board (p, d, n, u)
           character, intent(out),dimension(12,12) :: p
           character, intent(out), dimension(20) :: d
           character, intent(out), dimension(26) :: n
           integer, intent (out), dimension(50) :: u
           integer :: i, j

           i = 1
           do while (i < 13)
               j = 1
               do while (j < 13)
                   P(I,J) = " "
                   j = j+1
               end do
               i = i+1
           end do
           i = 1
           do while (i < 21)
               D(I) = "-"
               i = i+1
           end do
           i = 1
           do while (i < 27)
               N(I) = " "
               i = i+1
           end do
           i = 1
           do while (i < 51)
               U(I) = 0
                i = i+1
           end do
           i = 1
           do while (i < 13)
               P(I,1) = "X"
               i = i+1
           end do
           j = 1
           do while (j < 8)
               P(1,J) = "X"
               j = j+1
           end do
           P(2,7) = "X"
         end subroutine clear_board

         subroutine print_man (m, p)
             character, intent(out),dimension(12,12) :: p
             integer, intent(in) :: m

             select case (m)
                 case(1)
                     write (*,*) "First we draw a head."
                     p(3,6) = "-"; p(3,7) = "-"; p(3,8) = "-"; p(4,5) = "("; 
                     p(4,6) = "."; 
                     p(4,8) = "."; p(4,9) = ")"; p(5,6) = "-"; p(5,7) = "-"; 
                     p(5,8) = "-";
                 case(2)
                     write (*,*) "Now we draw a body."
                     i = 6
                     do while (i < 10)
                         p(i,7) = "x" 
                         i = i+1
                     end do
                 case(3)
                     write (*,*) "Next we draw an arm."
                     i = 4 
                     do while (i < 8)
                         p(i,i-1) = "\" 
                         i = i+1
                     end do
                 case(4)
                     write (*,*) "This time it's the other arm."
                     p(4,11) = "/"; p(5,10) = "/"; p(6,9) = "/"; p(7,8) = "/"
                 case(5)
                     write (*,*) "Now, let's draw the right leg."
                     p(10,6) = "/"; p(11,5) = "/";
                 case(6)
                     write (*,*) "This time we draw the left leg."
                     p(10,8) = "\"; p(11,9) = "\"
                 case(7)
                     write (*,*) "Now we put up a hand."
                     p(3,11) = "\"
                 case(8)
                     write (*,*) "Next the other hand."
                     p(3,3) = "/"
                 case(9)
                     write (*,*) "Now we draw one foot."
                     p(12,10) = "\"; p(12,11) = "-"
                 case(10)
                     write (*,*) "Here's the other foot -- You're hung!!."
                     p(12,3) = "-"; p(12,4) = "/" 
                 case default
                     write (*,*) "skip"
             end select   
             i = 1
             do while (i < 13)
                 write (*,*) (p(i,j),j=1,12)
                 i = i+1
             end do 

         end subroutine print_man
