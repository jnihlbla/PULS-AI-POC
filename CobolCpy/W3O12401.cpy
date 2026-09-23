000100 01  MOD-W3O12401.                                                        
000200*                                 MOD-COPYTEXT FÖR W30124                 
000300*                                 EXCHANGE EXTENDED LEAD-TIME             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDARTNR-UT       PIC X(8).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-CMD-ATTR         PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-CMD              PIC X.                                       
001500     03 MOD-IDARTNR-UPD-ATTR PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDARTNR-UPD      PIC Z(7)9.                                   
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-KVVECKOR-UPD-ATTR                                             
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-KVVECKOR-UPD     PIC Z9.                                      
002300*                                 ANTAL VECKOR                            
002400     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
002500*                                 GRUPP MED TABELL RADER                  
002600        05 MOD-IDARTNR       PIC Z(7)9.                                   
002700*                                 ARTIKELNUMMER                           
002800        05 MOD-BEART         PIC X(25).                                   
002900*                                 ARTIKELBENÄMNING                        
003000        05 MOD-KVVECKOR      PIC Z9.                                      
003100*                                 ANTAL VECKOR                            
003200        05 MOD-IDUSER        PIC X(8).                                    
003300*                                 ANVÄNDARENS SÄKERHETS ID                
003400        05 MOD-DAREGDAT      PIC 9(8).                                    
003500*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003600     03 MOD-TEMFSINF         PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*** END OF VILMAII-COPY LENGTH= 738 BYTES                                 
