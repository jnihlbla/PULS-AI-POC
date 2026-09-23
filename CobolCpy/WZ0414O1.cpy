000100 01  RESP-WZ0414O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WZ0411         
000300*                                 RETRIEVE RESTART DATA                   
000400*                                                                         
000500     03 RESP-IDOUTTYPE-KEY   PIC X(15).                                   
000600*                                 OUTPUTTYP                               
000700*                                 OUTPUT TYPE                             
000800     03 RESP-IDOUTREC-KEY    PIC X(30).                                   
000900*                                 OUTPUTMOTTAGARE                         
001000*                                 OUTPUT RECEIVER                         
001100     03 RESP-IDLIST-KEY      PIC X(10).                                   
001200*                                 LISTIDENTITET                           
001300*                                 LIST IDENTITY                           
001400     03 RESP-TIREGDAT-KEY    PIC 9(6).                                    
001500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001600*                                 REGISTRATION DATE (YYMMDD)              
001700     03 RESP-TIKLOCK-KEY     PIC X(8).                                    
001800*                                 KLOCKSLAG (TTMMSSTH)                    
001900*                                 TIME OF DAY (HHMMSSTH)                  
002000     03 RESP-IDLOPNR-KEY     PIC 9(3).                                    
002100*                                 LÖPNUMMER                               
002200*                                 SEQUENCE NUMBER                         
002300     03 RESP-KVPOST-LAST-KEY PIC Z(6)9.                                   
002400*                                 RÄKNARE, ANTAL POSTER                   
002500*                                 RECORD COUNTER                          
002600     03 RESP-KVRADER         PIC Z(4)9.                                   
002700*                                 ANTAL RADER                             
002800*                                 NUMBER OF LINES                         
002900     03 RESP-OUTDATA-GROUP   OCCURS 20 TIMES.                             
003000*                                 GRUPP AV OUTDATA-POSTER                 
003100        05 RESP-TEOUTDATA-L  PIC 9(5).                                    
003200*                                 LÄNGD PÅ D&P-OUTPUTDATA                 
003300*                                 LENGTH OF D&P DATA                      
003400        05 RESP-TEOUTDATA-D  PIC X(3000).                                 
003500*                                 OUTPUTDATA                              
003600*                                 OUTPUT DATA                             
003700*** END OF VILMAII-COPY LENGTH= 60184 BYTES                               
