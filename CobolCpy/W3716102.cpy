000100 01  W3716102-CTX.                                                        
000200*                                 RAPPORT-POST FÖR BYTES                  
000300*                                 EJ INKOMNA RETURER                      
000400*                                                                         
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 REKSIFFR             PIC S9              COMP-3.                  
000800*                                 KONTROLLSIFFRA                          
000900     03 BEART                PIC X(25).                                   
001000*                                 ARTIKELBENÄMNING                        
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 IDORDNR              PIC S9(5)           COMP-3.                  
001400*                                 ORDERNUMMER                             
001500     03 TIAAMMDD-FAKT        PIC S9(7)           COMP-3.                  
001600*                                 SENASTE FAKTURERINGSDATUM               
001700     03 KVANTAL-REST         PIC S9(7)           COMP-3.                  
001800*                                 DATAELEMENT                             
001900     03 KVANTAL-FAKT         PIC S9(7)           COMP-3.                  
002000*                                 FAKTURERAT ANTAL                        
002100*                                                                         
002200     03 TIAAMMDD-TDEB        PIC S9(7)           COMP-3.                  
002300*                                 TILLÄGGSDEBITERINGS-DATUM               
002400     03 IDTABNR              PIC S9(3)           COMP-3.                  
002500*                                 TABELLNUMMER                            
002600*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
