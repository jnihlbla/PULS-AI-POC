000100 01  RAPP-W6L111.                                                         
000200*                                 UPPFÖLJINGNSREGISTER                    
000300*                                 KONTROLLRAPPORT                         
000400*                                 FYSISK NYCKEL: IDKR                     
000500     03 RAPP-IDKR            PIC 9(5).                                    
000600*                                 KONTROLLRAPPORT NUMMER                  
000700*                                 INSPECTION REPORT NUMBER                
000800     03 RAPP-BEKRFEL         OCCURS 3 TIMES                               
000900                             PIC X(70).                                   
001000*                                 BESKRIVNING FELKOD KONTR.RAPPOR         
001100*                                 T                                       
001200*                                 DESCRIPTION OF ERRORCODE INSP.R         
001300*                                 EPORT                                   
001400     03 RAPP-KDKVASTA-PRI    PIC X.                                       
001500*                                 STATUS PRIMÄRKONTROLL                   
001600*                                 STATUS PRIMARY INSPECTION               
001700     03 RAPP-TEKRFEL         PIC X(70).                                   
001800*                                 FELBESKRIVNING I FRI TEXT               
001900*                                 DESCRIP.ERROR IN OWN WORDS              
002000     03 RAPP-IDUSER          PIC X(8).                                    
002100*                                 ANVÄNDARENS SÄKERHETS ID                
002200*                                 USER SECURITY-IDENTITY                  
002300*** END OF VILMAII-COPY LENGTH= 294 BYTES                                 
