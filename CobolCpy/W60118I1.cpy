000100 01  REQU-W60118I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W6011810              
000300     03 REQU-IDLOPNRM-KEY    PIC X(8).                                    
000400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000500*                                 (0VVDLLLLK)                             
000600*                                 SERIAL NO RECEIVING REPORT              
000700*                                 (0WWDLLLLC)                             
000800     03 REQU-IDDC-KEY        PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 REQU-IDSPRAK         PIC X(2).                                    
001200*                                 2-STÄLLIG ISO SPRÅKKOD                  
001300*                                 2-LETTER ISO LANGUAGE CODE              
001400     03 REQU-IDRADNR-START   PIC 9(4).                                    
001500*                                 RADNUMMER                               
001600*                                 LINE NO                                 
001700     03 REQU-KVRADER         PIC 9(5).                                    
001800*                                 ANTAL RADER                             
001900*                                 NUMBER OF LINES                         
002000     03 REQU-INPUT           OCCURS 50 TIMES.                             
002100*                                 INDATA FÖR UPPDATERING                  
002200        05 REQU-ADINLOMR-UPD-LINE                                         
002300                             PIC X(4).                                    
002400*                                 INLEVERANSOMRÅDE                        
002500*                                 RECEIVING AREA                          
002600        05 REQU-IDRADNR-LINE PIC 9(4).                                    
002700*                                 RADNUMMER                               
002800*                                 LINE NO                                 
002900        05 REQU-KDINLSTA-LINE                                             
003000                             PIC X(3).                                    
003100*                                 SYSTEMSTATUS INLEVERANS                 
003200*                                 SYSTEM STATUS RECEIVING                 
003300*** END OF VILMAII-COPY LENGTH= 571 BYTES                                 
