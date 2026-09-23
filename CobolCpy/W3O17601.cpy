000100 01  MOD-W3O17601.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 REG AV OBJEKTSRETUR, BYTES              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDFAKT-IN        PIC X(7).                                    
001700*                                 FAKTURANUMMER                           
001800     03 MOD-IDFAKT-UT        PIC X(7).                                    
001900*                                 FAKTURANUMMER                           
002000     03 MOD-IDBYTRAP-IN      PIC X(7).                                    
002100*                                 RAPPORTNUMMER  BYTES                    
002200     03 MOD-IDBYTRAP-UT      PIC X(7).                                    
002300*                                 RAPPORTNUMMER  BYTES                    
002400     03 MOD-RAD-GRUPP        OCCURS 27 TIMES.                             
002500        05 MOD-IDARTNR-RAD-ATTR                                           
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-IDARTNR-RAD   PIC Z(7)9.                                   
002900*                                 ARTIKELNUMMER                           
003000        05 MOD-KVRETUR-RAD-ATTR                                           
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-KVRETUR-RAD   PIC -(6)9.                                   
003400*                                 ANTAL I RETUR                           
003500        05 MOD-IDTABNR-RAD-ATTR                                           
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-IDTABNR-RAD   PIC 9(3).                                    
003900*                                 TABELLNUMMER                            
004000     03 MOD-FLKLAR-ATTR      PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-FLKLAR           PIC X.                                       
004300*                                 AVSLUTNINGSMARKERING                    
004400     03 MOD-TEMFSINF         PIC X(55).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*** END OF VILMAII-COPY LENGTH= 798 BYTES                                 
