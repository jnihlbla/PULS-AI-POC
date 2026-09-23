000100 01  MOD-W2O12700.                                                        
000200*                                 MOD-COPYTEXT FÖR W2012700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDANSK-DOLD      PIC Z(2)9.                                   
001200*                                 ANSKAFFARNUMMER                         
001300     03 MOD-TISENBEK-DAG-ENTER                                            
001400                             PIC 9(6).                                    
001500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001600     03 MOD-TISENBEK-KL-ENTER                                             
001700                             PIC 9(6).                                    
001800*                                 TIM - MIN - SEK   (HHMMSS)              
001900     03 MOD-KDLARM-ENTER     PIC Z(2)9.                                   
002000*                                 LARMORSAKSKOD                           
002100     03 MOD-TISENBEK-DAG-NEXT                                             
002200                             PIC 9(6).                                    
002300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002400     03 MOD-TISENBEK-KL-NEXT PIC 9(6).                                    
002500*                                 TIM - MIN - SEK   (HHMMSS)              
002600     03 MOD-KDLARM-NEXT      PIC Z(2)9.                                   
002700*                                 LARMORSAKSKOD                           
002800     03 MOD-IDANSK           PIC Z(2)9.                                   
002900*                                 ANSKAFFARNUMMER                         
003000     03 MOD-IDLEVNR          PIC X(5).                                    
003100*                                 LEVERANTÖRNUMMER                        
003200     03 MOD-IDARTNR          PIC Z(8)9.                                   
003300*                                 ARTIKELNUMMER                           
003400     03 MOD-IDDISTR          PIC Z(3)9.                                   
003500*                                 DISTRIKTNUMMER                          
003600     03 MOD-IDKUNDNR         PIC Z(5)9.                                   
003700*                                 KUNDNUMMER                              
003800     03 MOD-KDCLAGER         PIC 9.                                       
003900*                                 CENTRALLAGERKOD                         
004000     03 MOD-IDORDNR7         PIC Z(6)9.                                   
004100*                                 ORDERNUMMER                             
004200     03 MOD-TIFORDAT         PIC 9(6).                                    
004300*                                 FÖRFALLODATUM                           
004400     03 MOD-INFO-RAD         OCCURS 3 TIMES.                              
004500*                                 RADINFORMATION                          
004600        05 MOD-KVART-UT      PIC Z(6)9.                                   
004700*                                 ANTAL ARTNR PER BRYTBEGREPP             
004800        05 MOD-KVVECKOR-TPO5-UT                                           
004900                             PIC Z9.                                      
005000*                                 ANTAL VECKOR FÖR TPO5                   
005100        05 MOD-KVART-IN-ATTR PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-KVART-IN      PIC Z(6)9.                                   
005400*                                 ANTAL ARTNR PER BRYTBEGREPP             
005500        05 MOD-KVVECKOR-TPO5-IN-ATTR                                      
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-KVVECKOR-TPO5-IN                                           
005900                             PIC Z9.                                      
006000*                                 ANTAL VECKOR FÖR TPO5                   
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*** END OF VILMAII-COPY LENGTH= 257 BYTES                                 
