000100 01  REKY-WZ04REKY.                                                       
000200*                                 COPYTEXT FOR PGM WZ04REKY               
000300*                                 RETRIEVE RESTART KEYS                   
000400     03 REKY-IDOUTTYPE-KEY   PIC X(15).                                   
000500*                                 OUTPUTTYP                               
000600*                                 OUTPUT TYPE                             
000700     03 REKY-IDOUTREC-KEY    PIC X(30).                                   
000800*                                 OUTPUTMOTTAGARE                         
000900*                                 OUTPUT RECEIVER                         
001000     03 REKY-IDLIST-KEY      PIC X(10).                                   
001100*                                 LISTIDENTITET                           
001200*                                 LIST IDENTITY                           
001300     03 REKY-TIREGDAT-MIN-KEY                                             
001400                             PIC S9(7)           COMP-3.                  
001500*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001600*                                 REGISTRATION DATE (YYMMDD)              
001700     03 REKY-TIREGDAT-MAX-KEY                                             
001800                             PIC S9(7)           COMP-3.                  
001900*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002000*                                 REGISTRATION DATE (YYMMDD)              
002100     03 REKY-TIKLOCK-MIN-KEY PIC S9(9)           COMP-3.                  
002200*                                 KLOCKSLAG (TTMMSSTH)                    
002300*                                 TIME OF DAY (HHMMSSTH)                  
002400     03 REKY-TIKLOCK-MAX-KEY PIC S9(9)           COMP-3.                  
002500*                                 KLOCKSLAG (TTMMSSTH)                    
002600*                                 TIME OF DAY (HHMMSSTH)                  
002700     03 REKY-IDLOPNR-MIN-KEY PIC S9(3)           COMP-3.                  
002800*                                 L÷PNUMMER                               
002900*                                 SEQUENCE NUMBER                         
003000     03 REKY-IDLOPNR-MAX-KEY PIC S9(3)           COMP-3.                  
003100*                                 L÷PNUMMER                               
003200*                                 SEQUENCE NUMBER                         
003300     03 REKY-IDMSG           PIC X(3).                                    
003400*                                 MEDDELANDE NUMMER                       
003500*                                 MESSAGE NUMBER                          
003600*                                                                         
003700     03 REKY-IDELMT-ERROR    PIC X(16).                                   
003800*                                 DATAELEMENTIDENTITET                    
003900*                                 DATA ITEM NAME                          
004000     03 REKY-KVRADER         PIC 9(5).                                    
004100*                                 ANTAL RADER                             
004200*                                 NUMBER OF LINES                         
004300     03 REKY-IDOUTTYPE       OCCURS 20 TIMES                              
004400                             PIC X(15).                                   
004500*                                 OUTPUTTYP                               
004600*                                 OUTPUT TYPE                             
004700     03 REKY-IDOUTREC        OCCURS 20 TIMES                              
004800                             PIC X(30).                                   
004900*                                 OUTPUTMOTTAGARE                         
005000*                                 OUTPUT RECEIVER                         
005100     03 REKY-IDLIST          OCCURS 20 TIMES                              
005200                             PIC X(10).                                   
005300*                                 LISTIDENTITET                           
005400*                                 LIST IDENTITY                           
005500     03 REKY-TIREGDAT        OCCURS 20 TIMES                              
005600                             PIC S9(7)           COMP-3.                  
005700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
005800*                                 REGISTRATION DATE (YYMMDD)              
005900     03 REKY-TIKLOCK         OCCURS 20 TIMES                              
006000                             PIC S9(9)           COMP-3.                  
006100*                                 KLOCKSLAG (TTMMSSTH)                    
006200*                                 TIME OF DAY (HHMMSSTH)                  
006300     03 REKY-IDLOPNR         OCCURS 20 TIMES                              
006400                             PIC S9(3)           COMP-3.                  
006500*                                 L÷PNUMMER                               
006600*                                 SEQUENCE NUMBER                         
006700*** END OF VILMAII-COPY LENGTH= 1421 BYTES                                
