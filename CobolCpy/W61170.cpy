000100 01  W61170-CTX.                                                          
000200*                                 LISTA TILL NEDERLÄNDSK TULL             
000300*                                                                         
000400*                                 SAMMANLAGT VÄRDE OCH SAMMANLGD          
000500*                                 VIKT PER ARTIKELNUMMER FÖR AR-          
000600*                                 TIKLAR INLEVERADE PÅ C2 SENASTE         
000700*                                 MÅNADEN.                                
000800*                                                                         
000900*                                 LIST TO THE DUTCH CUSTOM                
001000*                                 AUTHORITIES.                            
001100*                                 TOTAL VALUE AND TOTAL WEIGHT            
001200*                                 PER PART NO. FOR PARTS RECEIVED         
001300*                                 AT THE SUPPLY TERMINAL IN BORN          
001400*                                 THE LAST MONTH.                         
001500*                                                                         
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900     03 IDSTATNR             PIC S9(9)           COMP-3.                  
002000*                                 STATISTISKT NUMMER                      
002100*                                 1 = NORSKT                              
002200*                                 2 = ENGELSKT                            
002300*                                 3 = BELGISKT                            
002400*                                 4 = PERUANSKT                           
002500*                                 5 = SVENSKT                             
002600*                                 6 =                                     
002700*                                 STATISTICAL NO.                         
002800     03 KDARTURS             PIC X(2).                                    
002900*                                 ARTIKELURSPRUNGSKOD                     
003000*                                 COUNTRY OF ORIGIN                       
003100     03 KVANTMOT             PIC S9(7)           COMP-3.                  
003200*                                 ANTAL MOTTAGET                          
003300*                                 QUANTITY RECEIVED                       
003400     03 VKARTTOT             PIC S9(17)          COMP-3.                  
003500*                                 TOTAL VIKT LEVERERAT TILL LAGER         
003600*                                  (IX)                                   
003700     03 SUARTBES             PIC S9(9)V9(2)      COMP-3.                  
003800*                                 SUMMA RADVÄRDE BEST.PRIS                
003900*                                 SUM LINEVALUE ORDERPRICE                
004000     03 TIAAMMDD-GAELL       PIC S9(7)           COMP-3.                  
004100*                                 GÄLLANDE DATUM                          
004200*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
