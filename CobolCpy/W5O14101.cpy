000100 01  MOD-W5O14101.                                                        
000200*                                 MOD-COPYTEXT FÖR W5O141                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-BEFT-IN          PIC X(2).                                    
000900*                                 FÖRPACKNINGSTYP                         
001000     03 MOD-BEFT-UT          PIC X(2).                                    
001100*                                 FÖRPACKNINGSTYP                         
001200     03 MOD-BEFT-SPAR        PIC X(2).                                    
001300*                                 FÖRPACKNINGSTYP                         
001400     03 MOD-BEFT-SPAR-RAD1   PIC X(2).                                    
001500*                                 FÖRPACKNINGSTYP                         
001600     03 MOD-RAD              OCCURS 12 TIMES                              
001700                             INDEXED MOD-IX-1.                            
001800*                                  TABELL-RADER                           
001900*                                                                         
002000        05 MOD-BEFT-ATTR     PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200        05 MOD-BEFT-RAD      PIC Z(2)9.                                   
002300*                                 FÖRPACKNINGSTYP                         
002400        05 MOD-PRDIRLON-ATTR PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-PRDIRLON-RAD  PIC Z(3)9.9(3).                              
002700*                                 DIREKT LÖN                              
002800        05 MOD-PRDMTRL-ATTR  PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-PRDMTRL-RAD   PIC Z(5)9.9(3).                              
003100*                                 DIREKT MATERIAL                         
003200        05 MOD-PROVRPAL-ATTR PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-PROVRPAL-RAD  PIC Z(3)9.9(3).                              
003500*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
003600     03 MOD-BEFT-ATTR-UPP    PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-BEFT-UPP         PIC X(2).                                    
003900*                                 FÖRPACKNINGSTYP                         
004000     03 MOD-PRDIRLON-ATTR-UPP                                             
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-PRDIRLON-UPP     PIC X(8).                                    
004400*                                 DIREKT LÖN                              
004500     03 MOD-PRDMTRL-ATTR-UPP PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-PRDMTRL-UPP      PIC X(10).                                   
004800*                                 DIREKT MATERIAL                         
004900     03 MOD-PROVRPAL-ATTR-UPP                                             
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-PROVRPAL-UPP     PIC X(8).                                    
005300*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
005400     03 MOD-KDCMD-ATTR       PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KDCMD            PIC X.                                       
005700*                                 RAD-UPPDATERINGSKOMMANDO                
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END OF VILMAII-COPY LENGTH= 590 BYTES                                 
