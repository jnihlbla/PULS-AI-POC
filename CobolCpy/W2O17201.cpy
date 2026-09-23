000100 01  MOD-W2O17201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2017200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDANSK-IN        PIC X(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MOD-IDANSK-UT        PIC X(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 MOD-KDLARM-IN        PIC X(3).                                    
001600*                                 LARMORSAKSKOD                           
001700     03 MOD-KDLARM-UT        PIC X(3).                                    
001800*                                 LARMORSAKSKOD                           
001900     03 MOD-IDARTNR-IN       PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-KDOTFREK-IN      PIC X.                                       
002400*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
002500     03 MOD-KDOTFREK-UT      PIC X.                                       
002600*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
002700     03 MOD-INFO-RAD         OCCURS 15 TIMES.                             
002800*                                 RADINFORMATION                          
002900        05 MOD-SELECT-ARTIKEL-ATTR                                        
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-SELECT-ARTIKEL                                             
003300                             PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500        05 MOD-FLNYLARM      PIC X.                                       
003600*                                 ANGER OM ARTIKELLARMET ÄR NYTT          
003700        05 MOD-IDARTNR       PIC Z(8)9.                                   
003800*                                 ARTIKELNUMMER                           
003900        05 MOD-IDLEVNR       PIC X(5).                                    
004000*                                 LEVERANTÖRNUMMER                        
004100        05 MOD-TEORSLRM      PIC X(25).                                   
004200        05 MOD-KDOTFREK      PIC X.                                       
004300*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
004400        05 MOD-TIREGDAT      PIC 9(6).                                    
004500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004600        05 MOD-TIPLANDAT     PIC 9(6).                                    
004700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004800        05 MOD-KVAVIS        PIC Z(6)9.                                   
004900*                                 AVISERAT ANTAL                          
005000        05 MOD-KVAVROP       PIC Z(6)9.                                   
005100*                                 AVROPSKVANTITET                         
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 1206 BYTES                                
