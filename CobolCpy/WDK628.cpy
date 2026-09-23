000100 01  BYT-WDK628.                                                          
000200*                                 EXTRA RETURVECKOR                       
000300*                                 FYSISK NYCKEL IDARTNR                   
000400     03 BYT-IDARTNR          PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 BYT-DAREGDAT         PIC 9(8).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
000900*                                 REGISTRATION DATE (YYYYMMDD)            
001000     03 BYT-FLLARM-ACT       PIC X.                                       
001100*                                 OBJEKT LARMRAPPORT UTFÄRDAD             
001200*                                 ACTIVE CORE ALARM                       
001300     03 BYT-IDUSER           PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500*                                 USER SECURITY-IDENTITY                  
001600     03 BYT-KVVECKOR         PIC S9(3)           COMP-3.                  
001700*                                 ANTAL VECKOR                            
001800     03 BYT-RERETUR          PIC S9(3)           COMP-3.                  
001900*                                 VISAR PROCENT FÖR RETURER               
002000*                                 SHOWS PERCENTAGE RETURNS                
002100     03 BYT-REREUSE          PIC S9(3)           COMP-3.                  
002200*                                 VISAR PROCENT FÖR ÅTERANV.              
002300*                                 SHOWS PERCENTAGE REUSES                 
002400     03 BYT-RELARM-FAC       PIC S9V9(2)         COMP-3.                  
002500*                                 SHOWS ALARM LIMIT FOR EXCH-PART         
002600*                                 S                                       
002700     03 BYT-RELARM-PER       PIC 9(2).                                    
002800*                                 PERCENTAGE THAT INDICATES WHEN          
002900*                                 A DEVIATION BETWEEN                     
003000*                                 OUTBOUND QTY AND RETURNED QTY I         
003100*                                 S TO BE ALARMED                         
003200     03 BYT-TIKLOCK          PIC S9(9)           COMP-3.                  
003300*                                 KLOCKSLAG (TTMMSSTH)                    
003400*                                 TIME OF DAY (HHMMSSTH)                  
003500*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
