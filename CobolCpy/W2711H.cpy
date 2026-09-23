000100 01  W2711H-CTX.                                                          
000200*                                 COPYTEXT FÖR FILEN W2711H               
000300*                                 EXTRAKTFIL AV                           
000400*                                 REFILLFÖRSLAG                           
000500     03 IDPERSON-BUY         PIC Z(2)9.                                   
000600*                                 PERSONKOD REFILLANSVARIG                
000700     03 DELIMITER-01         PIC X.                                       
000800     03 KDREFTYP             PIC X.                                       
000900*                                 TYP AV REFILLORDER                      
001000     03 DELIMITER-02         PIC X.                                       
001100     03 IDARTNR              PIC Z(7)9.                                   
001200*                                 ARTIKELNUMMER                           
001300     03 DELIMITER-03         PIC X.                                       
001400     03 BEART                PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600     03 DELIMITER-04         PIC X.                                       
001700     03 KVBEART              PIC Z(5)9.                                   
001800*                                 BESTÄLLT ANTAL STYCKEN                  
001900     03 DELIMITER-05         PIC X.                                       
002000     03 TEREFMED-COMM1       PIC X(36).                                   
002100     03 DELIMITER-06         PIC X.                                       
002200     03 TEREFMED-COMM2       PIC X(36).                                   
002300     03 DELIMITER-07         PIC X.                                       
002400     03 IDSTATNR             PIC Z(8)9.                                   
002500*                                 STATISTISKT NUMMER                      
002600*                                 1 = NORSKT                              
002700*                                 2 = ENGELSKT                            
002800*                                 3 = BELGISKT                            
002900*                                 4 = PERUANSKT                           
003000*                                 5 = SVENSKT                             
003100*                                 6 =                                     
003200     03 DELIMITER-08         PIC X.                                       
003300     03 FLFLYG               PIC X.                                       
003400*                                 FLYGARTIKEL                             
003500     03 DELIMITER-09         PIC X.                                       
003600     03 KDARTURS             PIC X(2).                                    
003700*                                 ARTIKELURSPRUNGSKOD                     
003800     03 DELIMITER-10         PIC X.                                       
003900     03 VKART                PIC Z(6)9.                                   
004000*                                 ARTIKELVIKT (G)                         
004100     03 DELIMITER-11         PIC X.                                       
004200     03 VLARTNTO             PIC X(10).                                   
004300*                                 ARTIKELVOLYM (CM3)                      
004400*** END OF VILMAII-COPY LENGTH= 155 BYTES                                 
