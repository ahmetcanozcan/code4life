# Code4Life Strategy Report

> 18th in silver division

Codingame Username : ahmetcanozcan


## Main Strategy

In first round, i take 2 sample just like enemy bot to  be as fast as the enemy bot and then I go to diagnosis for diagnosing sample in my hand and then I go molecules to collect needed molecules. if i can collect enough molecules to serve a sample, i go lab otherwise, I go diagnosis to get rid of a  sample or I go the samples if i have  available space in my hand. After collecting  enough molecules to serve one or more samples to lab, I go lab and serve them and then if I'm in late of the game and i have one or more sample in my hand, I go molecules to make that samples done. Otherwise,  I go the samples to get new samples


## Side Strategies

  -  #### Getting Sample From The Samples
      a player get any rank of sample from the samples building. High ranked samples comes with more profit but need more molecules to be done. In early of the game, Getting high ranked molecules can block your progress. I calculate a parameters depends on player expertise and player molecules then I get samples depends on that parameter.

  - #### Picking Best Sample
      Player picks best sample by sample score that is calculated by a basic formula, ` ADVANTAGE(GAIN) + SAMPLE.health / 3 - ERROR(Need)`  

      - ADVANTAGE(GAIN) : how many samples in inventory can use this sample's gain 

      - ERROR(Need) : Sum of square of needed molecul count

          ```
            Sample A :
              Needed Molecules : 2A 2B 
                  ERROR : 2**2 + 2**2 = 8

            Sample B : 
              Needed Molecules : 3C
                  ERROR : 3**2 = 9 
                    
                picks the first sample  even though the second one requires fewer molecules   


          ```          
          
          
  




