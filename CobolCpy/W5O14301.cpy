000100 01  MOD-W5O14301.                                                        
000200*                                 MOD-COPYTEXT FÖR W5O143                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-EMB-IN   PIC X(9).                                    
000900*                                 EMBALLAGE-ARTIKELNUMMER                 
001000     03 MOD-IDARTNR-EMB-UT   PIC X(9).                                    
001100*                                 EMBALLAGE-ARTIKELNUMMER                 
001200     03 MOD-IDARTNR-EMB-SPAR PIC X(9).                                    
001300*                                 EMBALLAGE-ARTIKELNUMMER                 
001400     03 MOD-IDARTNR-EMB-RAD1 PIC X(9).                                    
001500*                                 EMBALLAGE-ARTIKELNUMMER                 
001600     03 MOD-RAD              OCCURS 12 TIMES                              
001700                             INDEXED MOD-IX.                              
001800        05 MOD-IDARTNR-EMB-ATTR                                           
001900                             PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100        05 MOD-IDARTNR-EMB-RAD                                            
002200                             PIC Z(8)9.                                   
002300*                                 ARTIKELNUMMER                           
002400        05 MOD-PRDMTRL-ATTR  PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-PRDMTRL-RAD   PIC Z(5)9.9(3).                              
002700*                                 DIREKT MATERIAL                         
002800     03 MOD-IDARTNR-NY-ATTR  PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-IDARTNR-EMB      PIC X(9).                                    
003100*                                 EMBALLAGE-ARTIKELNUMMER                 
003200     03 MOD-PRDMTRL-NY-ATTR  PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-PRDMTRL          PIC X(10).                                   
003500*                                 DIREKT MATERIAL                         
003600     03 MOD-KDCMD-ATTR       PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KDCMD            PIC X.                                       
003900*                                 RAD-UPPDATERINGSKOMMANDO                
004000     03 MOD-TEMFSINF         PIC X(55).                                   
004100*                                 INFORMATIONSMEDDELANDE                  
004200*** END OF VILMAII-COPY LENGTH= 437 BYTES                                 
