000100 01  MOD-W2O47201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2047200               
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
002300     03 MOD-STRECK           PIC X.                                       
002400     03 MOD-REKSIFFR         PIC 9.                                       
002500*                                 KONTROLLSIFFRA                          
002600     03 MOD-IDDC-IN          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 MOD-IDDC-UT          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MOD-INFO-RAD         OCCURS 15 TIMES.                             
003100*                                 RADINFORMATION                          
003200        05 MOD-SELECT-ARTIKEL-ATTR                                        
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-SELECT-ARTIKEL                                             
003600                             PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800        05 MOD-IDDC          PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000        05 MOD-FLNYLARM      PIC X.                                       
004100*                                 ANGER OM ARTIKELLARMET ÄR NYTT          
004200        05 MOD-IDARTNR       PIC X(8).                                    
004300*                                 ARTIKELNUMMER                           
004400        05 MOD-IDLEVNR       PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER                        
004600        05 MOD-KDLARM        PIC Z(2)9.                                   
004700*                                 LARMORSAKSKOD                           
004800        05 MOD-TEORSLRM      PIC X(20).                                   
004900        05 MOD-TIREGDAT      PIC 9(6).                                    
005000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005100        05 MOD-TIPLANDAT     PIC 9(6).                                    
005200*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005300        05 MOD-KVAVIS        PIC Z(6)9.                                   
005400*                                 AVISERAT ANTAL                          
005500        05 MOD-KVAVROP       PIC Z(6)9.                                   
005600*                                 AVROPSKVANTITET                         
005700     03 MOD-TEMFSINF         PIC X(55).                                   
005800*                                 INFORMATIONSMEDDELANDE                  
005900*** END OF VILMAII-COPY LENGTH= 1180 BYTES                                
