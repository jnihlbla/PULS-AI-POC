000100 01  REQU-WZ0414I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WZ0414             
000300*                                 RETRIEVE RESTART DATA                   
000400*                                                                         
000500     03 REQU-IDOUTTYPE-KEY   PIC X(15).                                   
000600*                                 OUTPUTTYP                               
000700*                                 OUTPUT TYPE                             
000800     03 REQU-IDOUTREC-KEY    PIC X(30).                                   
000900*                                 OUTPUTMOTTAGARE                         
001000*                                 OUTPUT RECEIVER                         
001100     03 REQU-IDLIST-KEY      PIC X(10).                                   
001200*                                 LISTIDENTITET                           
001300*                                 LIST IDENTITY                           
001400     03 REQU-TIREGDAT-KEY    PIC 9(6).                                    
001500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001600*                                 REGISTRATION DATE (YYMMDD)              
001700     03 REQU-TIKLOCK-KEY     PIC 9(8).                                    
001800*                                 KLOCKSLAG (TTMMSSTH)                    
001900*                                 TIME OF DAY (HHMMSSTH)                  
002000     03 REQU-IDLOPNR-KEY     PIC 9(3).                                    
002100*                                 LÖPNUMMER                               
002200*                                 SEQUENCE NUMBER                         
002300     03 REQU-KVPOST-LAST-KEY PIC 9(7).                                    
002400*                                 RÄKNARE, ANTAL POSTER                   
002500*                                 RECORD COUNTER                          
002600     03 REQU-KDPRTVIEW       PIC X.                                       
002700*                                 VISA(V) ELLER SKRIV UT(P) ?             
002800*                                 VIEW(V) OR PRINT(P) ?                   
002900*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
