000100 01  RESP-WZ0420O1.                                                       
000200*                                 RESPONSE-COPYTEXT FOR WZ0420            
000300*                                 WHEN IS USES PGM WZ110UTW               
000400*                                 (OUTPUT-METHOD WEB)                     
000500*                                                                         
000600     03 RESP-IDOUTTYPE-KEY   PIC X(15).                                   
000700*                                 OUTPUTTYP                               
000800*                                 OUTPUT TYPE                             
000900     03 RESP-IDOUTREC-KEY    PIC X(30).                                   
001000*                                 OUTPUTMOTTAGARE                         
001100*                                 OUTPUT RECEIVER                         
001200     03 RESP-IDLIST-KEY      PIC X(10).                                   
001300*                                 LISTIDENTITET                           
001400*                                 LIST IDENTITY                           
001500     03 RESP-TIREGDAT-KEY    PIC 9(6).                                    
001600*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001700*                                 REGISTRATION DATE (YYMMDD)              
001800     03 RESP-TIKLOCK-KEY     PIC X(8).                                    
001900*                                 KLOCKSLAG (TTMMSSTH)                    
002000*                                 TIME OF DAY (HHMMSSTH)                  
002100     03 RESP-IDLOPNR-KEY     PIC 9(3).                                    
002200*                                 L÷PNUMMER                               
002300*                                 SEQUENCE NUMBER                         
002400*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
