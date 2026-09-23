000100 01  REQU-W60148I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W60148                
000300*                                                                         
000400     03 REQU-IDLOPNRM-KEY    PIC X(8).                                    
000500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000600*                                 (0VVDLLLLK)                             
000700*                                 SERIAL NO RECEIVING REPORT              
000800*                                 (0WWDLLLLC)                             
000900     03 REQU-IDLEVNR-KEY     PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 REQU-IDOKOLLI-KEY    PIC X(9).                                    
001300*                                 ODETTE KOLLINUMMER                      
001400*                                 ODETTE CASE NUMBER                      
001500     03 REQU-IDDC-KEY        PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 REQU-IDSPRAK         PIC X(2).                                    
001900*                                 2-STÄLLIG ISO SPRÅKKOD                  
002000*                                 2-LETTER ISO LANGUAGE CODE              
002100     03 REQU-IDRADNR-START   PIC 9(4).                                    
002200*                                 RADNUMMER                               
002300*                                 LINE NO                                 
002400     03 REQU-KVRADER         PIC 9(5).                                    
002500*                                 ANTAL RADER                             
002600*                                 NUMBER OF LINES                         
002700     03 REQU-INPUT.                                                       
002800*                                 INDATA FÖR UPPDATERING                  
002900        05 REQU-KDCMDVAL-LINE                                             
003000                             OCCURS 50 TIMES                              
003100                             PIC X(3).                                    
003200*                                 GENERELL KOMMANDOKOD                    
003300*                                 GENERAL COMMAND-CODE                    
003400     03 REQU-IDRADNR-LINE    OCCURS 50 TIMES                              
003500                             PIC 9(4).                                    
003600*                                 RADNUMMER                               
003700*                                 LINE NO                                 
003800*** END OF VILMAII-COPY LENGTH= 385 BYTES                                 
