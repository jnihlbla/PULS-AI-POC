000100 01  MOD-W1O21501.                                                        
000200*                                 MOD-COPYTEXT FÖR W1021500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-STR-IN   PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-STR-UT   PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-KDSTRRAD-ENTER   PIC X.                                       
001200*                                 TYP AV STRUKTURRAD                      
001300     03 MOD-IDRADNR-ENTER    PIC 9(5).                                    
001400*                                 RADNUMMER                               
001500     03 MOD-KDSTRRAD-NEXT    PIC X.                                       
001600*                                 TYP AV STRUKTURRAD                      
001700     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
001800*                                 RADNUMMER                               
001900     03 MOD-BEART            PIC X(25).                                   
002000*                                 ARTIKELBENÄMNING                        
002100     03 MOD-IDLEVNR          PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 MOD-FLEXFORP-UT      PIC X.                                       
002400*                                 FLAGGA FÖR EXTERNFÖRPACKNING            
002500     03 MOD-FLEXFORP-IN-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-FLEXFORP-IN      PIC X.                                       
002800*                                 FLAGGA FÖR EXTERNFÖRPACKNING            
002900     03 MOD-OUTPUT           OCCURS 3 TIMES.                              
003000*                                 RADINFORMATION                          
003100        05 MOD-IDRADNR       PIC X(5).                                    
003200*                                 RADNUMMER                               
003300        05 MOD-IDARTNR-UT    PIC Z(9).                                    
003400*                                 ARTIKELNUMMER                           
003500        05 MOD-BEART-SVE-UT  PIC X(25).                                   
003600*                                 SVENSK ARTIKELBENÄMNING                 
003700        05 MOD-REANTPSA-UT   PIC Z9.9(3).                                 
003800*                                 ANTAL PER SATS                          
003900        05 MOD-TISTADAT-UT   PIC 9(4).                                    
004000*                                 ÅR - VECKA  (ÅÅVV)                      
004100        05 MOD-TISTODAT-UT   PIC 9(4).                                    
004200*                                 ÅR - VECKA  (ÅÅVV)                      
004300        05 MOD-BORT-ATTR     PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-BORT          PIC X.                                       
004600        05 MOD-IDARTNR-IN-ATTR                                            
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-IDARTNR-IN    PIC X(9).                                    
005000*                                 ARTIKELNUMMER                           
005100        05 MOD-REANTPSA-IN-ATTR                                           
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-REANTPSA-IN   PIC X(2).                                    
005500*                                 MFS BEHANDLING AV INPUTFÄLT             
005600        05 MOD-TISTADAT-IN-ATTR                                           
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-TISTADAT-IN   PIC X(2).                                    
006000*                                 MFS BEHANDLING AV INPUTFÄLT             
006100        05 MOD-TISTODAT-IN-ATTR                                           
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-TISTODAT-IN   PIC X(2).                                    
006500*                                 MFS BEHANDLING AV INPUTFÄLT             
006600        05 MOD-TESTRNOT-ATTR PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-TESTRNOT      PIC X(70).                                   
006900*                                 STRUKTURNOTERING                        
007000     03 MOD-KLAR-ATTR        PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-KLAR             PIC X.                                       
007300     03 MOD-TEMFSINF         PIC X(55).                                   
007400*                                 INFORMATIONSMEDDELANDE                  
007500*** END OF VILMAII-COPY LENGTH= 612 BYTES                                 
