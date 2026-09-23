000100 01  W4O66601.                                                            
000200*                                 MODCOPYTEXT TILL W40666.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDDC-IN              PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDTRPTNR-IN          PIC X(3).                                    
001000*                                 TRANSPORTIDENTITET                      
001100     03 IDTRPTNR-UT          PIC X(3).                                    
001200*                                 TRANSPORTIDENTITET                      
001300     03 IDLBBET-UT           PIC X(12).                                   
001400*                                 LASTBÄRARBETECKNING                     
001500     03 FLFARLIG-UT          PIC X.                                       
001600*                                 FARLIGT GODS-FLAGGA                     
001700*                                                                         
001800     03 IDDC-UT              PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 IDTRPTNR-ENTER       PIC X(3).                                    
002100*                                 TRANSPORTIDENTITET                      
002200     03 IDLBBET-ENTER        PIC X(12).                                   
002300*                                 LASTBÄRARBETECKNING                     
002400     03 IDTRPTNR-NEXT        PIC X(3).                                    
002500*                                 TRANSPORTIDENTITET                      
002600     03 IDLBBET-NEXT         PIC X(12).                                   
002700*                                 LASTBÄRARBETECKNING                     
002800     03 RADER                OCCURS 14 TIMES.                             
002900*                                                                         
003000        05 IDTRPTNR          PIC Z(2)9.                                   
003100*                                 TRANSPORTIDENTITET                      
003200        05 IDLBBET           PIC X(12).                                   
003300*                                 LASTBÄRARBETECKNING                     
003400     03 TEMFSINF             PIC X(55).                                   
003500*                                 INFORMATIONSMEDDELANDE                  
003600*** END COPY W4O66601    LENGTH=362                                       
