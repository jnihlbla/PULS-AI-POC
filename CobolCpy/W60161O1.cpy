000100 01  RESP-W60161O1.                                                       
000200*                                 UPPDATERING HANTERINGSKOD               
000300     03 RESP-KVRADER         PIC 9(5).                                    
000400*                                 ANTAL RADER                             
000500     03 RESP-OUTPUT-LINES    OCCURS 10 TIMES.                             
000600        05 RESP-IDARTNR-ATTR PIC X(2).                                    
000700*                                 MFS ATTRIBUTFÄLT                        
000800        05 RESP-IDARTNR      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000        05 RESP-KDARTHNT-V-ATTR                                           
001100                             PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300        05 RESP-KDARTHNT-V   PIC X(2).                                    
001400*                                 MFS BEHANDLING AV INPUTFÄLT             
001500        05 RESP-KDARTHNT-H-ATTR                                           
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800        05 RESP-KDARTHNT-H   PIC X(2).                                    
001900*                                 MFS BEHANDLING AV INPUTFÄLT             
002000        05 RESP-KDARTURS-ATTR                                             
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 RESP-KDARTURS     PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500*** END OF VILMAII-COPY LENGTH= 165 BYTES                                 
