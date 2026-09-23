000100 01  RESP-WL0178O1.                                                       
000200*                                 RESPONS FROM PGM WL0178                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDLISTNR-KEY    PIC 9(6).                                    
000600*                                 LISTNUMMER                              
000700     03 RESP-IDUSER-KEY      PIC X(8).                                    
000800*                                 ANVÄNDARENS SÄKERHETS ID                
000900     03 RESP-FLDIFF          PIC X.                                       
001000*                                 JA/NEJ-FLAGGA                           
001100     03 RESP-KVRADER         PIC Z(4)9.                                   
001200*                                 ANTAL RADER                             
001300     03 RESP-TABELLRAD       OCCURS 10 TIMES.                             
001400*                                 GRUPP MED TABELL RADER                  
001500        05 RESP-IDARTNR      PIC Z(7)9.                                   
001600*                                 ARTIKELNUMMER                           
001700        05 RESP-BEART-SVE    PIC X(25).                                   
001800        05 RESP-KVAKS        PIC Z(6)9-.                                  
001900*                                 ANKOMSTSALDO                            
002000        05 RESP-KVLS         PIC Z(6)9-.                                  
002100*                                 LAGERSALDO                              
002200        05 RESP-KVEFRS       PIC Z(6)9-.                                  
002300*                                 EJ FAKTURERAT ANTAL STYCK               
002400        05 RESP-ANTAL-IN     PIC Z(6).                                    
002500*                                 ANTAL                                   
002600        05 RESP-TECKEN-IN    PIC X.                                       
002700        05 RESP-DIFFERANS-IN PIC Z(6).                                    
002800*                                 ANTAL                                   
002900        05 RESP-FLOMINV-IN   PIC X.                                       
003000        05 RESP-IDMSG-ERROR-LINE                                          
003100                             PIC X(3).                                    
003200*                                 FELMEDDELANDE ID                        
003300*** END OF VILMAII-COPY LENGTH= 762 BYTES                                 
