000100 01  W4O73201.                                                            
000200*                                 MODCOPYTEXT TILL W40732.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDSNDNNR-IN          PIC X(6).                                    
000800     03 IDKOLLI-IN           PIC X(5).                                    
000900*                                 KOLLINUMMER                             
001000     03 IDSNDNNR-UT          PIC X(6).                                    
001100     03 IDKOLLI-UT           PIC X(5).                                    
001200*                                 KOLLINUMMER                             
001300     03 IDFRASED             PIC X(15).                                   
001400*                                 FRAKTSEDELSNUMMER                       
001500     03 RADER                OCCURS 13 TIMES.                             
001600*                                                                         
001700        05 KDCMD-ATTR        PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 KDCMD             PIC X(4).                                    
002000        05 ADINLOMR-UPD-ATTR PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200        05 ADINLOMR-UPD      PIC X(2).                                    
002300*                                 MFS BEHANDLING AV INPUTFÄLT             
002400        05 ADINLOMR          PIC X(4).                                    
002500*                                 INLEVERANSOMRÅDE                        
002600        05 IDKOLLI           PIC Z(4)9.                                   
002700*                                 KOLLINUMMER                             
002800        05 FLFARLIG          PIC X.                                       
002900*                                 FARLIGT GODS-FLAGGA                     
003000        05 BESTATUS          PIC X(4).                                    
003100        05 FLBUYBAC          PIC X.                                       
003200*                                 FLAGGA BUYBACK J/N                      
003300     03 TEMFSINF             PIC X(55).                                   
003400*                                 INFORMATIONSMEDDELANDE                  
003500*** END OF VILMAII-COPY LENGTH= 461 BYTES                                 
