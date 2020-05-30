import math._
import scala.util._
import scala.io.StdIn._
import scala.collection.mutable.ListBuffer



class Sample (var id:Int,var carriedBy:Int,var  rank:Int,var  health:Int,var  gain:String, var costs: Array[Int] ) {
    var isDone = false
}


class Player (var target:String, var molecules: Array[Int], var expertises:Array[Int],var  inventory: List[Sample]) {
    var remainMolecules : Array[Int] = Array[Int](molecules: _ *)
    
}



/**
 * Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine!
 **/
object Player extends App {
    val projectCount = readLine.toInt
    for(i <- 0 until projectCount) {
        val Array(a, b, c, d, e) = (readLine split " ").map (_.toInt)
    }
    var round = 0
    val players = new Array[Player](2)
    var totalSampleLimit = 2
    var isFirstRound = true
    var isFull = false;
    // game loop
    while(true) {
        round = round + 1;
        for(i <- 0 until 2) {
            val Array(target, _eta, _score, _storageA, _storageB, _storageC, _storageD, _storageE, _expertiseA, _expertiseB, _expertiseC, _expertiseD, _expertiseE) = readLine split " "
            val eta = _eta.toInt
            val score = _score.toInt
            val storageA = _storageA.toInt
            val storageB = _storageB.toInt
            val storageC = _storageC.toInt
            val storageD = _storageD.toInt
            val storageE = _storageE.toInt
            val expertiseA = _expertiseA.toInt
            val expertiseB = _expertiseB.toInt
            val expertiseC = _expertiseC.toInt
            val expertiseD = _expertiseD.toInt
            val expertiseE = _expertiseE.toInt
            /*
             * Instantiate players. 0 : me , 1 : Enemy
             */
            players(i) = new Player(target,
                Array(storageA,storageB,storageC,storageD,storageE),
                Array(expertiseA,expertiseB,expertiseC,expertiseD,expertiseE),
                List[Sample]()
                );

        }
        val Array(availableA, availableB, availableC, availableD, availableE) = (readLine split " ").map (_.toInt)
        val sampleCount = readLine.toInt
        var cloudInventory : List[Sample]  =List()
        val samples =new  Array[Sample](sampleCount)
        val availables = Array(availableA,availableB,availableC,availableD,availableE) 
        for(i <- 0 until sampleCount) {
            val Array(_sampleId, _carriedBy, _rank, expertiseGain, _health, _costA, _costB, _costC, _costD, _costE) = readLine split " "
            val sampleId = _sampleId.toInt
            val carriedBy = _carriedBy.toInt
            val rank = _rank.toInt
            val health = _health.toInt
            val costA = _costA.toInt
            val costB = _costB.toInt
            val costC = _costC.toInt
            val costD = _costD.toInt
            val costE = _costE.toInt
            samples(i) = new Sample(sampleId,carriedBy,rank,health,expertiseGain,Array(costA,costB,costC,costD,costE))
            carriedBy match {
                case 1 =>  players(1).inventory  =  samples(i) :: players(1).inventory 
                case 0 => players(0).inventory  =  samples(i) :: players(0).inventory 
                case _ =>  cloudInventory=  samples(i) :: cloudInventory 

            }

        }
     
     
        // Goes diagnosis if I'm not in diagnosis
        // otherwise resolve a sample 
        def goDiagnosis(playerTarget : String ,sampleID : Int) : Unit = {
            if (playerTarget == "DIAGNOSIS"){
                println("CONNECT "+sampleID)
            } else {
                println("GOTO DIAGNOSIS")
            }
        }

        // Transform molecul id to molecul name 
        def moleculIDToMoleculName(id:Int): String = {
            id match {
                case 0 => return "A"
                case 1 => return "B"
                case 2 => return "C"
                case 3 => return "D"
                case 4 => return "E"
                case _ => return "DEFAULT"
                
            }
        } 

        def moleculNameToMoleculID(name : String) : Int = {
            name match {
                case "A" => return 0
                case "B" => return 1
                case "C" => return 2
                case "D" => return 3
                case "E" => return 4
                case  _  => return -1
            }
        }


        def calculateTotalExperties(player : Player) : Int  = {
            var i = 0
            var acc = 0
            for (i <- 0 until player.expertises.size) {
                acc += player.expertises(i)
            }
            return acc
        }

        // Returns a sample which have  -1 health.
        def getSampleForDiagnosis(samples : List[Sample]) : Sample = {
            var i = 0
            for (i <- 0 until samples.size) {
                val s = samples(i)
                if(s.health == -1) {
                    return s
                }
            }
            return samples(0)
        }

        // Goes molecules if I'm not here,
        // otherwise, collects given molecule 
        def goMolecules(playerTarget : String, moleculID: Int) : Unit = {
            val moleculName : String = moleculIDToMoleculName(moleculID);
            if (playerTarget == "MOLECULES") {
                println("CONNECT "+moleculName)
            } else {
                println("GOTO MOLECULES")
            }
        }

        // Goes laboratory, if Player0 is not in Lab
        // otherwise, serve given sample to lab
        def goLaboratory(playerTarget: String, sampleID: Int) : Unit =  {
            if (playerTarget == "LABORATORY") {
                println("CONNECT "+ sampleID)
            } else {
                println("GOTO LABORATORY")
            }

        }
        // Goes samples, if Player0 is not in Samples
        // otherwise, gets a sample given rank
        def goSamples(playerTarget : String, rank : Int) : Unit = {
            if (playerTarget == "SAMPLES") {
                println("CONNECT "+ rank)
            } else {
                println("GOTO SAMPLES")
            }

        }

        // Calculate needed molecule
        // if there is no needed molecule returns -1
        def getNeededMolecule( costs : Array[Int] , availables : Array[Int], player : Player,molecules : Array[Int]) : Int = {
            
            var min = 9
            var minX = -1
            // Get samples that's gonna be given to lab
            val doneSamples = getDoneSamples(player)
            var gainMolecules = Array[Int](0,0,0,0,0)

            var y = 0
            // Sum all gain of this sample
            for (y <- 0 until doneSamples.size) {
                gainMolecules(moleculNameToMoleculID( doneSamples(y).gain)) += 1 
            }
            

            var x  = 0
            for ( x <- 0 to 4) {
                // calculate needing of molecule x
                val need  = costs(x) - player.expertises(x) - molecules(x) - gainMolecules(x)
                // if need is bigger than 0
                if(  need > 0  ) {
                    // Compare availability of molecule x to get less
                    // available molecule.
                    if ( availables(x) < min  && availables(x) > 0 ) {
                        min = availables(x)
                        minX = x
                    }
                }
            }
            return minX
        }


        // Calculate player's total molecule
        def getPlayerTotalMolecule(player: Player) : Int =  {
            var x = 0
            var acc = 0
            for (x <- 0 until 5) {
                acc = acc + player.molecules(x)
            }
            return acc
        }

        def getSampleTotalMoleculeCost (sample : Sample, player: Player) : Int = {
            var x  = 0
            var acc = 0
           
            for ( x <- 0 until sample.costs.size) {
                acc += max(0, sample.costs(x) - player.expertises(x)- player.molecules(x))
            }
            return acc 
        }

        def checkSampleMakeable (player: Player,availables : Array[Int],sample : Sample,molecules : Array[Int]) : Boolean = {
            // Get player's total molecule
            val ptotal = getPlayerTotalMolecule(player)
            // Get sample's total molecule cost
            var stotal  = getSampleTotalMoleculeCost(sample,player)
            // if sum of player molecules and sample molecule cost bigger than 10,
            //  returns false
            // Because it's gonna exceed inventory limit
            if(ptotal + stotal > 10) {
                return false
            }

            var gain = Array[Int](0,0,0,0,0)
            // Get samples that have allocated mollecules 
            var doneSamples = getDoneSamples(player)
            var i = 0
            // Sum all gain of this samples
            for (i <- 0 until doneSamples.size) {
                val gv = moleculNameToMoleculID(doneSamples(i).gain)
                if(gv > -1){
                      gain(gv) += 1
                }
            }


            var x = 0
            // if Player0 can not satisfy any molecules  cost of the sample,
            //  return false
            for (  x <- 0 until 5){
                if( sample.costs(x) - molecules(x) - availables(x) - player.expertises(x) - gain(x) > 0){
                    return false
                }
            }
            // otherwise return true
            return true
        }

        /*
          * This function is deprecated.
          */
        def pickBestSample(player : Player , availables: Array[Int]) : Sample = {
            var x = 0
            var minC = 999999
            var inv = getMakeableSamples(player,player.inventory,availables)
            var t = 0
            for (t <- 0 until inv.size) {
            }
            if (inv.size == 0 ) {
                return null
            }

            var result = inv(0)
            for ( x <- 0 until inv.size) {
                val temp = getSampleTotalMoleculeCost(inv(x),player)
                if ( temp < minC && !inv(x).isDone ) {
                    result = inv(x)
                    minC = temp
                }    
            }
            return result
        }




        def pickBestSampleV2(player : Player , availables: Array[Int]) : Sample = {
            var x = 0
            var maxScore = -1000  
            
            // Get makeable samples
            var inv = getMakeableSamples(player,player.inventory,availables)

            // if player has not a sample, return null
            if (inv.size == 0 ) {
                return null
            }

            var result = inv(0)
            // pick sample which have highest sample score
            for ( x <- 0 until inv.size) {
                // Calculate sample score. 
                val score = calculateSampleScore(inv(x) , player, players(1), availables)
                if ( score > maxScore && !inv(x).isDone ) {
                    result = inv(x)
                    maxScore = temp
                }    
            }

            return result
        }
        
        /*
         * Needed error is one of the parameters of the sample score formula.
         * 
         *  sample X needs 2A 2B and sample Y needs 3A then
         *  sample Y more need error.
         *  ERR(X) = 2**2 + 2**2 = 8
         *  ERR(Y) = 3**2 = 9
         */
        def calculateNeedError(sample:Sample,player : Player) : Int = {
            var x = 0
            var error = 0
            for (x <- 0 until 5) {
                // calculate needing of molecule x
                val a = sample.costs(x) - player.expertises(x) - player.molecules(x)
                // if needing is bigger than 0
                // add square of this needing to error
                if ( a  > 0) {
                    error += a*a
                }
                
            }
            return error
        }


        /*
         * Gain advantage is one of the parameters of the sample score formula
         * That express how much useful gain of a sample
         */
        def calculateGainAdvantage(sample : Sample, player: Player) : Int = {
            // Get gain id of sample
            val gainID = moleculNameToMoleculID(sample.gain)
            // If gaind id is -1, this sample is not activated
            if ( gainID == -1) return 0

            var advantage = 0
            var x = 0
            for(x <- 0 until player.inventory.size) {
                // For each sample in player inventory
                val s = player.inventory(x)
                // Count sample that can uses this sample's gain 
                if( s.id != sample.id) {
                    if ( s.costs(gainID) > player.expertises(gainID) ){
                        advantage += 1
                    }
                }
            }
        
            return advantage
        }


        /*
         * Sample score express a sample's priority
         *  SS = ADVANTAGE(GAIN) + SAMPLE.health / 3 - ERROR(Need); 
         */
        def calculateSampleScore( sample : Sample, player: Player , enemyPlayer : Player, availables: Array[Int] ) : Int = {
           
           
            val needError =  calculateNeedError(sample,player) 

            val gainAdvantage = calculateGainAdvantage(sample,player) 

            return 10*gainAdvantage + ( sample.health/3 ).toInt - needError

        } 

        // Return samples that can be makeable 
        def getMakeableSamples(player : Player, samples : List[Sample] , availables : Array[Int], useRemain : Boolean = false) : List[Sample] = {
            var x =  0
            var molecules = player.molecules
            
            var result : List[Sample] = List()
            for ( x <- 0 until player.inventory.size) {
                // if sample is makeable, add this sample to the list
                if(checkSampleMakeable(player, availables, samples(x),molecules)){
                    result = samples(x) :: result
                }
            }
         
            return result
        } 

        /*
         * DEPRECATED : old version of pickBestSample
         */
        def getBestSample(samples: List[Sample]) : Sample = {
          
            if (samples.size == 0) {
                return null 
            }
            var maxHealth = 0
            var maxSample = samples(0)
            var i = 0
            for( i <- 0 until samples.size) {
                val s = samples(i)
                if (s.health > maxHealth) {
                    maxHealth = s.health
                    maxSample = s
                }
            }
            return maxSample
        }
        
        
        def getReadyToServeSample(player : Player, availables : Array[Int]) : Sample = {
            var x = 0
            for (x <- 0 until player.inventory.size) {
                val nm = getNeededMolecule(player.inventory(x).costs,availables,player,player.molecules)
                if(nm == -1) {
                    return player.inventory(x)
                }
            }
            return null
        }

        def updateRemainMols(player: Player, sample : Sample) : Unit =  {
            var x = 0 
            for ( x <- 0 until 5) {
                val v= max(0,player.remainMolecules(x) + player.expertises(x)  - sample.costs(x))
                if ( v > 0 && player.expertises(x) < sample.costs(x)) {
                    player.remainMolecules(x) = v 
                }
            }
        }

        def checkSampleDone(player : Player , sample: Sample, molecules : Array[Int]) : Boolean = {
            var x = 0
            for ( x <- 0 until 5) {
                if ( sample.costs(x) - molecules(x) -  player.expertises(x) > 0  ) {
                    return false
                }
            }
            return true
        }

        def getDoneSamples(player : Player) : List[Sample] = {
            var x = 0
            var res  : List[Sample] = List()
            for ( x <- 0 until player.inventory.size) {
                var s = player.inventory(x)
                if(s.isDone) {
                    res = s :: res
                }
            }
            return res
        }

        def getRemainingMolecules(player : Player) : Array[Int] = {
            var res  = Array[Int](player.molecules: _ *)
            var x = 0 
            for ( x <- 0 until player.inventory.size) {
                var y  = 0
                val sample = player.inventory(x)
                if(sample.isDone) {
                    for ( y <- 0 until 5) {
                        var va = res(y) + player.expertises(y)  + moleculNameToMoleculID(sample.gain)  - sample.costs(y)  

                        if ( va > 0 && player.expertises(y) < sample.costs(y)) {
                            res(y) = va
                        }
                    }   
                }
        }
            return res
        }

        
        def setSamplesDone(player: Player,availables : Array[Int] ) : Unit = {
            var x = 0
            // For all sample in player inventory
            for ( x <- 0 until player.inventory.size) {
                // Get best sample
                val s = pickBestSample(player,availables)
                
                val remain = getRemainingMolecules(player)
                //if this sample is done, set its isDone variable true
                if(s != null && checkSampleDone(player,s,remain) ) {
                    var y = 0
                    for (y <- 0 until player.inventory.size ) {
                        if( s.id == player.inventory(y).id) {
                            var xx = 0
                            player.inventory(y).isDone = true
                        }
                    }
                }    

            }
        }
        
        // Return a sample that done not using gain of any sample
        def getDoneSampleMakeableWithoutGain(player:Player,availables: Array[Int]) : Sample = {
            var x = 0
            var breakFlag = true
            var res  : Sample = null
            // for every sample in inventory
             while ( ( x < player.inventory.size ) && breakFlag) {
                var s = player.inventory(x)
                var mkb = true
                var ii = 0
                // Check sample is makeable or not
                for ( ii <- 0 until 5) {
                    if (  ( s.costs(ii) - player.expertises(ii) - player.molecules(ii) ) > 0  ) {
                        mkb = false
                    }
                }
                // if makeable return it
                if(s.isDone && mkb) {
                   res = s
                   breakFlag = false
                }
                x+=1
             }
            return res

        }

        setSamplesDone(players(0), availables)
        var doneSamples = getDoneSamples(players(0))
        

        //if it is first round take 2 sample instead of 3
        if(isFirstRound) {
            totalSampleLimit = 2
        } else {
            totalSampleLimit = 3
        }

        // Check our hand is full
        if (  players(0).inventory.size == totalSampleLimit ){
            isFull = true
        } else if (  players(0).inventory.size == 0 ){
            isFull = false
        }

        
        
        Console.err.println("dsize " + doneSamples.size)
         Console.err.print("Remain mols ")
            var ix = 0
            for (ix <- 0 until 5) {
                Console.err.print( players(0).remainMolecules(ix) + "  ")
            }
        Console.err.println()

        val doneSampleWithoutGain = getDoneSampleMakeableWithoutGain(players(0),availables)
        Console.err.println("DS "+ doneSampleWithoutGain)
        // if player0 has a sample that's done anot using any sample's gain. 
        // Serve it
        if( (doneSampleWithoutGain != null) && (doneSamples.size > 0 )&& players(0).target == "LABORATORY" ) {
            goLaboratory(players(0).target,doneSampleWithoutGain.id)
        } else {
            // if inventory is not full
            if (!isFull){
                // take sample by any rank by cof value. 
                val cof = calculateTotalExperties(players(0)) + players(0).inventory.size
                if( cof < 8) {
                    goSamples(players(0).target,1)
                } else if (cof <13) {
                    goSamples(players(0).target,2)
                } else {
                    goSamples(players(0).target,3)
                }
            } else {
                // if it is first round false it
                if(isFirstRound) {
                    isFirstRound = false
                }
                // Get sample
                val smp = getSampleForDiagnosis(players(0).inventory)
                // if sample health is not -1,
                // it a recognized sample.
                if(smp.health != -1) {
                    // pick a sample
                    val smpOnWork = pickBestSample(players(0),availables)
                    // if it's null, means there is no sample 
                    if (smpOnWork == null) {
                        // if player's hand is not full
                        if (players(0).inventory.size < 3 ) {
                            isFull = false
                            // then go sample station for getting new sample 
                            goSamples(players(0).target,1)
                        } else {
                            // otherwise, go diagnosis to get rid of a sample
                            goDiagnosis(players(0).target,players(0).inventory(0).id)
                        }
                        // if sample is not null
                    } else {
                        Console.err.println("Best Sample " + smpOnWork.id )
                        // Get Needed molecule type 
                        val nm = getNeededMolecule(smpOnWork.costs,availables,players(0),getRemainingMolecules(players(0)))
                        // get a molecule type.
                        val remm = getRemainingMolecules(players(0))
                        Console.err.print("Remaining Molecules ")
                        var xx = 0
                        for ( xx <- 0 until 5) {
                            Console.err.print( remm(xx) + "  ")
                        }
                        Console.err.println()
                        Console.err.println("Needed Molecule "+ moleculIDToMoleculName(nm))
                        // if sample does not need a molecule 
                        if ( nm != -1) {

                            if (!checkSampleMakeable(players(0),availables,smpOnWork,getRemainingMolecules(players(0)))
                              || getPlayerTotalMolecule(players(0)) >= 10  ) {
                                // if player has any sample that can be served
                                // go lab for serving samples
                                if ( doneSamples.size > 0) {
                                    goLaboratory(players(0).target, doneSamples(0).id)
                                // otherwise go diagnosis to get rid of a sample
                                } else {
                                    goDiagnosis(players(0).target,smpOnWork.id)
                                }
                            
                            } else {
                                if ( players(0).target == "LABORATORY") {
                                    /*
                                     * In late of the game, if player has one or more sample in hand
                                     * goes molecules to done this sample instead of going samples for
                                     * getting new sample.
                                     */
                                    if ((round > 120 && players(0).inventory.size > 1) || round > 160 ) {
                               Console.err.println(" Log3-> " +nm +"  round " + round )

                                        goMolecules(players(0).target,nm)
                                    } else {
                                        isFull = false
                                        goSamples(players(0).target,2)
                                    }
                                // if player is not in lab, go molecules to make some sample done
                                } else {
                                 Console.err.println(" Log4-> " +nm +"  round " + round )
                                    goMolecules(players(0).target,nm)
                                }
                            }
                        
                        } else {
                        
                           if ( doneSamples.size > 0) {
                               goLaboratory(players(0).target,doneSamples(0).id)
                           } else {
                                if ( players(0).target == "LABORATORY") {
                                    Console.err.println(" Log5-> " +nm +"  round " + round )
                                if ((round > 120 && players(0).inventory.size > 1) || round > 160 ) {
                                    goMolecules(players(0).target,nm)
                                } else {
                                    isFull = false
                                    goSamples(players(0).target,2)
                                }
                                } else {
 Console.err.println(" Log6-> " +nm +"  round " + round )
                                    goMolecules(players(0).target,nm)
                                }
                           }
                        }
                    }
                    
                } else {
                    goDiagnosis(players(0).target,smp.id)
                }
            }
        }



        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        
       
    }
}