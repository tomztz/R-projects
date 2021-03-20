n_stay <- 0              # n_stay=0
n_switch <- 0            #n_switch=0
n_winCounter<- 0         #n_winCounter=0
n_loseCounter<-0        #n_lose
for ( i in 1:100) {      #repeat the game for 100 times
  door <- c(1,2,3)        #set three doors door1,door2,door3
  cardoor <- sample(door,1) #chose a random door from the list door, and make the car in that door
  choice <- sample(door,1) #chose another random door to be the players position
  goatdoors <- setdiff(door, cardoor) #new vector that stores the door with goats only,
                                      #i.e. the door containing the car is removed
  reveal_options <- setdiff(goatdoors, choice) #stores the door which disinclude the door which the person is standing behind 
  if (choice == cardoor) { #if the door the person is standing behind is the car
    reveal <- sample(reveal_options,1)  }  #reveal any of the two doors listed in reveal_options
  else {                                #else
    reveal <- reveal_options            #reveal the only door which remains in the list reveal_options 
  }
  remaining_doors <-setdiff(door, reveal)#disinclude the revealed door from the list of doors, and store in remaining_doors
  newchoice <- setdiff(remaining_doors, choice)   #disinclude the door which the player is standing,from the list remaining_doors
                                                  #i.e. move to the other door store in newchoice
  list<-c(choice,newchoice)                     #create a list with the door which the player is standing,and the other door
  player<-sample(list,1)                      #make the player randomly chose a door from list store in player
  if(player==cardoor){                      #if the door contains the car
    n_winCounter<- n_winCounter+1           #n_winCounter++
    
  }
  else{                                   #else
    n_loseCounter<-n_loseCounter+1        #n_loseCounter++
  }
  if (choice == cardoor) {                #if the door the player is standing contains the car
    n_stay <- n_stay + 1                  #n_stay++
  }
  
  if (newchoice == cardoor) {           #if the door the player moves to contains the car
    n_switch <- n_switch + 1            #n_switch++
  }
}
print(n_stay/100)                       #print(n_stay/100)
print(n_switch/100)                     #print(n_switch/100) 
print(n_winCounter)                     #print(n_winCounter)
print(n_loseCounter)                    #print(n_loseCounter)
