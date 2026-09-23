000100 01  UPPF-W6L201.                                                         
000200*                                 UPPFÖLJINGNSREGISTER                    
000300*                                 INLEVERANS HÄNDELSER                    
000400*                                 FYSISK NYCKEL: W6L101KY                 
000500*                                 (IDLOPNRM, IDRADNR,                     
000600*                                 DAREGDAT, TIKLOCK                       
000700     03 UPPF-IDLOPNRM        PIC S9(9)           COMP-3.                  
000800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000900*                                 (0VVDLLLLK)                             
001000*                                 SERIAL NO RECEIVING REPORT              
001100*                                 (0WWDLLLLC)                             
001200     03 UPPF-IDRADNR         PIC S9(5)           COMP-3.                  
001300*                                 RADNUMMER                               
001400*                                 LINE NO                                 
001500     03 UPPF-DAREGDAT        PIC 9(8).                                    
001600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001700*                                 REGISTRATION DATE (YYYYMMDD)            
001800     03 UPPF-TIKLOCK         PIC S9(9)           COMP-3.                  
001900*                                 KLOCKSLAG (TTMMSSTH)                    
002000*                                 TIME OF DAY (HHMMSSTH)                  
002100     03 UPPF-KDINLSTA        PIC X(3).                                    
002200*                                 SYSTEMSTATUS INLEVERANS                 
002300*                                 SYSTEM STATUS RECEIVING                 
002400     03 UPPF-ADINLOMR        PIC X(4).                                    
002500*                                 INLEVERANSOMRÅDE                        
002600*                                 RECEIVING AREA                          
002700     03 UPPF-ADINLOMR-NXT    PIC X(4).                                    
002800*                                 INLEVERANSOMRÅDE NÄSTA                  
002900*                                 RECEIVING AREA NEXT                     
003000     03 UPPF-KVINLART        PIC S9(7)           COMP-3.                  
003100*                                 ANTAL I PARTIRAD                        
003200*                                 QTY/LINE IN A LOT                       
003300     03 UPPF-IDUSER          PIC X(8).                                    
003400*                                 ANVÄNDARENS SÄKERHETS ID                
003500*                                 USER SECURITY-IDENTITY                  
003600*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
