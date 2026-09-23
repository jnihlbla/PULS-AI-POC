000100 01  MOD-W0O81201.                                                        
000200*                                 MOD-COPYTEXT TILL W00812                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDRAPRIO-ENTER   PIC X(3).                                    
000800*                                 PRIORITETSKOD PÅ RADEN                  
000900     03 MOD-KDRAPRIO-PF8     PIC X(3).                                    
001000*                                 PRIORITETSKOD PÅ RADEN                  
001100     03 MOD-RAD              OCCURS 14 TIMES.                             
001200        05 MOD-KDRAPRIO      PIC Z(2)9.                                   
001300*                                 PRIORITETSKOD PÅ RADEN                  
001400        05 MOD-BERAPRIO      PIC X(10).                                   
001500*                                 PRIORITETSBENÄMNING                     
001600        05 MOD-FLPRIO        PIC X.                                       
001700*                                 PRIORITERAD                             
001800        05 MOD-REROFORD      PIC Z(2)9.                                   
001900*                                 RESTORDERFÖRDELNINGSFAKTOR              
002000        05 MOD-KVVECKOR-TECK PIC Z9.                                      
002100*                                 ANTAL VECKOR FÖR HEL ROTÄCKNING         
002200        05 MOD-RELEVFOR      PIC 9.9(2).                                  
002300*                                 RELATIONSKOEFFICIENT RESTORDER          
002400     03 MOD-TEMFSINF         PIC X(61).                                   
002500*                                 INFORMATIONSMEDDELANDE                  
002600*** END COPY W0O81201C0  LENGTH=433                                       
