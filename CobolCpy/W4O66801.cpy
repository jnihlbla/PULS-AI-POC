000100 01  MOD-W4O66801.                                                        
000200*                                 MODCOPYTEXT TILL W40668.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-TIDATUM-IN       PIC X(6).                                    
000800*                                 DATUM ENLIGT KDDATFORM                  
000900     03 MOD-TIDATUM-UT       PIC X(6).                                    
001000*                                 DATUM ENLIGT KDDATFORM                  
001100     03 MOD-RADER            OCCURS 42 TIMES.                             
001200*                                                                         
001300        05 MOD-KDSVAR-ATTR   PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500        05 MOD-KDSVAR        PIC X.                                       
001600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001700        05 MOD-IDTRPTNR      PIC Z(2)9.                                   
001800*                                 TRANSPORTIDENTITET                      
001900        05 MOD-IDLBBET       PIC X(12).                                   
002000*                                 LASTBÄRARBETECKNING                     
002100     03 MOD-TEMFSINF         PIC X(55).                                   
002200*                                 INFORMATIONSMEDDELANDE                  
002300*** END OF VILMAII-COPY LENGTH= 867 BYTES                                 
