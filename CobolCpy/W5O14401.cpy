000100 01  MOD-W5O14401.                                                        
000200*                                 MOD-COPYTEXT FÖR W5O144                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDFORPGP-IN      PIC X(2).                                    
000900*                                 FÖRPACKNINGSGRUPP                       
001000     03 MOD-KDFORPGP-UT      PIC X(2).                                    
001100*                                 FÖRPACKNINGSGRUPP                       
001200     03 MOD-KDFORPGP-SPAR    PIC X(2).                                    
001300*                                 FÖRPACKNINGSGRUPP                       
001400     03 MOD-KDFORPGP-SPAR-RAD1                                            
001500                             PIC X(2).                                    
001600*                                 FÖRPACKNINGSGRUPP                       
001700     03 MOD-RAD              OCCURS 12 TIMES                              
001800                             INDEXED MOD-IX-1.                            
001900*                                  TABELL-RADER                           
002000*                                                                         
002100        05 MOD-KDFORPGP-ATTR PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-KDFORPGP-RAD  PIC Z9.                                      
002400*                                 FÖRPACKNINGSGRUPP                       
002500        05 MOD-PRDIRLON-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-PRDIRLON-RAD  PIC Z(3)9.9(3).                              
002800*                                 DIREKT LÖN                              
002900        05 MOD-PRDMTRL-ATTR  PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-PRDMTRL-RAD   PIC Z(5)9.9(3).                              
003200*                                 DIREKT MATERIAL                         
003300        05 MOD-PROVRPAL-ATTR PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-PROVRPAL-RAD  PIC Z(3)9.9(3).                              
003600*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
003700     03 MOD-KDFORPGP-ATTR-UPP                                             
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-KDFORPGP-UPP     PIC X(2).                                    
004100*                                 FÖRPACKNINGSGRUPP                       
004200     03 MOD-PRDIRLON-ATTR-UPP                                             
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-PRDIRLON-UPP     PIC X(8).                                    
004600*                                 DIREKT LÖN                              
004700     03 MOD-PRDMTRL-ATTR-UPP PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-PRDMTRL-UPP      PIC X(10).                                   
005000*                                 DIREKT MATERIAL                         
005100     03 MOD-PROVRPAL-ATTR-UPP                                             
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-PROVRPAL-UPP     PIC X(8).                                    
005500*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
005600     03 MOD-KDCMD-ATTR       PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-KDCMD            PIC X.                                       
005900*                                 RAD-UPPDATERINGSKOMMANDO                
006000     03 MOD-TEMFSINF         PIC X(55).                                   
006100*                                 INFORMATIONSMEDDELANDE                  
006200*** END OF VILMAII-COPY LENGTH= 578 BYTES                                 
