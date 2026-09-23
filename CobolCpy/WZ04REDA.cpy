000100 01  REDA-WZ04REDA.                                                       
000200*                                 COPYTEXT FOR PGM WZ04REDA               
000300*                                 RETRIEVE RESTART DATA                   
000400     03 REDA-IDOUTTYPE-KEY   PIC X(15).                                   
000500*                                 OUTPUTTYP                               
000600*                                 OUTPUT TYPE                             
000700     03 REDA-IDOUTREC-KEY    PIC X(30).                                   
000800*                                 OUTPUTMOTTAGARE                         
000900*                                 OUTPUT RECEIVER                         
001000     03 REDA-IDLIST-KEY      PIC X(10).                                   
001100*                                 LISTIDENTITET                           
001200*                                 LIST IDENTITY                           
001300     03 REDA-TIREGDAT-KEY    PIC S9(7)           COMP-3.                  
001400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001500*                                 REGISTRATION DATE (YYMMDD)              
001600     03 REDA-TIKLOCK-KEY     PIC S9(9)           COMP-3.                  
001700*                                 KLOCKSLAG (TTMMSSTH)                    
001800*                                 TIME OF DAY (HHMMSSTH)                  
001900     03 REDA-IDLOPNR-KEY     PIC S9(3)           COMP-3.                  
002000*                                 LÖPNUMMER                               
002100*                                 SEQUENCE NUMBER                         
002200     03 REDA-KVPOST-LAST     PIC S9(7)           COMP-3.                  
002300*                                 POST ELLER RADRÄKNARE                   
002400*                                 RECORD OR LINE COUNTER                  
002500     03 REDA-IDMSG           PIC X(3).                                    
002600*                                 MEDDELANDE NUMMER                       
002700*                                 MESSAGE NUMBER                          
002800*                                                                         
002900     03 REDA-IDELMT-ERROR    PIC X(16).                                   
003000*                                 DATAELEMENTIDENTITET                    
003100*                                 DATA ITEM NAME                          
003200     03 REDA-KVRADER         PIC 9(5).                                    
003300*                                 ANTAL RADER                             
003400*                                 NUMBER OF LINES                         
003500     03 REDA-OUTDATA-GROUP   OCCURS 20 TIMES.                             
003600*                                 GROUP OF OUTDATA RECORDS                
003700        49 REDA-TEOUTDATA-LEN                                             
003800                             PIC S9(4)           COMP SYNC.               
003900*                                 LÄNGD PÅ D&P-OUTPUTDATA                 
004000*                                 LENGTH OF D&P DATA                      
004100        49 REDA-TEOUTDATA-DATA                                            
004200                             PIC X(3000).                                 
004300*                                 OUTPUTDATA                              
004400*                                 OUTPUT DATA                             
004500*** END OF VILMAII-COPY LENGTH= 60174 BYTES                               
