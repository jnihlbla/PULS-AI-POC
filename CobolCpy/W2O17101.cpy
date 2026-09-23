000100 01  MOD-W2O17101.                                                        
000200*                                 MOD-COPYTEXT FÖR W2017100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDANSK-IN        PIC X(3).                                    
001200*                                 ANSKAFFARNUMMER                         
001300     03 MOD-IDANSK-UT        PIC X(3).                                    
001400*                                 ANSKAFFARNUMMER                         
001500     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900     03 MOD-KDLARM-IN        PIC X(3).                                    
002000*                                 LARMORSAKSKOD                           
002100     03 MOD-KDLARM-UT        PIC X(3).                                    
002200*                                 LARMORSAKSKOD                           
002300     03 MOD-KDOTFREK-IN      PIC X.                                       
002400*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
002500     03 MOD-KDOTFREK-UT      PIC X.                                       
002600*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
002700     03 MOD-TISENBEK-DAG-ENTER                                            
002800                             PIC 9(6).                                    
002900*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003000     03 MOD-TISENBEK-KL-ENTER                                             
003100                             PIC 9(6).                                    
003200*                                 TIM - MIN - SEK   (HHMMSS)              
003300     03 MOD-KDLARM-ENTER     PIC X(3).                                    
003400*                                 LARMORSAKSKOD                           
003500     03 MOD-TISENBEK-DAG-NEXT                                             
003600                             PIC 9(6).                                    
003700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003800     03 MOD-TISENBEK-KL-NEXT PIC 9(6).                                    
003900*                                 TIM - MIN - SEK   (HHMMSS)              
004000     03 MOD-KDLARM-NEXT      PIC X(3).                                    
004100*                                 LARMORSAKSKOD                           
004200     03 MOD-INFO-RAD         OCCURS 14 TIMES.                             
004300*                                 RADINFORMATION                          
004400        05 MOD-SELECT-ARTIKEL-ATTR                                        
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-SELECT-ARTIKEL                                             
004800                             PIC X(2).                                    
004900*                                 MFS BEHANDLING AV INPUTFÄLT             
005000        05 MOD-FLNYLARM-ATTR PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-FLNYLARM      PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400        05 MOD-KDLARM        PIC Z(2)9.                                   
005500*                                 LARMORSAKSKOD                           
005600        05 MOD-IDARTNR       PIC Z(8)9.                                   
005700*                                 ARTIKELNUMMER                           
005800        05 MOD-IDLEVNR       PIC X(5).                                    
005900*                                 LEVERANTÖRNUMMER                        
006000        05 MOD-TISENBEK-DAG-UT-ATTR                                       
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-TISENBEK-DAG-UT                                            
006400                             PIC 9(5).                                    
006500*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
006600        05 MOD-TISENBEK-KL-UT-ATTR                                        
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-TISENBEK-KL-UT                                             
007000                             PIC 9(6).                                    
007100*                                 TIM - MIN - SEK   (HHMMSS)              
007200        05 MOD-IDDISTR       PIC Z(3)9.                                   
007300*                                 DISTRIKTNUMMER                          
007400        05 MOD-TEORSLRM      PIC X(24).                                   
007500        05 MOD-KDOTFREK      PIC X.                                       
007600*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
007700        05 MOD-TIREGDAT      PIC 9(6).                                    
007800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007900     03 MOD-TEMFSINF         PIC X(55).                                   
008000*                                 INFORMATIONSMEDDELANDE                  
008100*** END OF VILMAII-COPY LENGTH= 1221 BYTES                                
