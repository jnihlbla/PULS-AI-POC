000100 01  MOD-W2O12601.                                                        
000200*                                 MOD-COPYTEXT F÷R W2012600               
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
001500*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001600     03 MOD-TISENBEK-KL-ENTER                                             
001700                             PIC 9(6).                                    
001800*                                 TIM - MIN - SEK   (HHMMSS)              
001900     03 MOD-KDLARM-ENTER     PIC Z(2)9.                                   
002000*                                 LARMORSAKSKOD                           
002100     03 MOD-TISENBEK-DAG-NEXT                                             
002200                             PIC 9(6).                                    
002300*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002400     03 MOD-TISENBEK-KL-NEXT PIC 9(6).                                    
002500*                                 TIM - MIN - SEK   (HHMMSS)              
002600     03 MOD-KDLARM-NEXT      PIC Z(2)9.                                   
002700*                                 LARMORSAKSKOD                           
002800     03 MOD-IDANSK           PIC Z(2)9.                                   
002900*                                 ANSKAFFARNUMMER                         
003000     03 MOD-IDLEVNR          PIC X(5).                                    
003100*                                 LEVERANT÷RNUMMER                        
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
004200     03 MOD-KVART            PIC Z(6)9.                                   
004300*                                 ANTAL ARTNR PER BRYTBEGREPP             
004400     03 MOD-TITPO            PIC 9(5).                                    
004500*                                 ≈R - VECKA - DAG   (≈≈VVD)              
004600     03 MOD-KDTPOTYP         PIC 9.                                       
004700*                                 TYP AV TIDPLANERAD ORDER                
004800     03 MOD-INFO-RAD         OCCURS 6 TIMES.                              
004900*                                 RADINFORMATION                          
005000        05 MOD-KVART-IN-ATTR PIC X(2).                                    
005100*                                 MFS ATTRIBUTFƒLT                        
005200        05 MOD-KVART-IN      PIC Z(6)9.                                   
005300*                                 ANTAL ARTNR PER BRYTBEGREPP             
005400        05 MOD-TITPO-IN-ATTR PIC X(2).                                    
005500*                                 MFS ATTRIBUTFƒLT                        
005600        05 MOD-TITPO-IN      PIC 9(5).                                    
005700*                                 ≈R - VECKA - DAG   (≈≈VVD)              
005800        05 MOD-KVART-UT      PIC Z(6)9.                                   
005900*                                 ANTAL ARTNR PER BRYTBEGREPP             
006000        05 MOD-TITPO-UT      PIC 9(5).                                    
006100*                                 ≈R - VECKA - DAG   (≈≈VVD)              
006200     03 MOD-TEMFSINF         PIC X(55).                                   
006300*                                 INFORMATIONSMEDDELANDE                  
006400*** END OF VILMAII-COPY LENGTH= 366 BYTES                                 
