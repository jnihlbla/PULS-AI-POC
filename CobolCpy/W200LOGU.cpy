000100 01  LOGU-W200LOGU.                                                       
000200*                                 LINK AREA FOR W200LOGU                  
000300     03 LOGU-IDSKYLT         PIC X(3).                                    
000400*                                 NATIONALITETSTECKEN                     
000500*                                 SPRÅKIDENTIFIKATION                     
000600     03 LOGU-IDDOKTYP        PIC X(8).                                    
000700*                                 DOKUMENTATIONSTYP                       
000800     03 LOGU-IDDOK           PIC X(8).                                    
000900*                                 DOKUMENTATIONSIDENTITET                 
001000     03 LOGU-IDUSER          PIC X(8).                                    
001100*                                 ANVÄNDARENS SÄKERHETS ID                
001200     03 LOGU-TEINFO          PIC X(1200).                                 
001300*                                 ALLMÄN TEXT INFO                        
001400     03 LOGU-KDSVAR          PIC X.                                       
001500      88 LOGU-KDSVAR-OK      VALUE ' '.                                   
001600      88 LOGU-KDSVAR-FEL     VALUE 'F'.                                   
001700*                                                       KDSVAR-88         
001800*                                 SVARSKOD FRÅN SUBPROGRAM                
001900     03 LOGU-IDMSG-ERROR     PIC X(3).                                    
002000*                                 FELMEDDELANDE ID                        
002100     03 LOGU-IDELMT-ERROR    PIC X(16).                                   
002200*                                 DATAELEMENTIDENTITET                    
002300     03 LOGU-FEL-TEXT        PIC X(25).                                   
002400*** END OF VILMAII-COPY LENGTH= 1272 BYTES                                
