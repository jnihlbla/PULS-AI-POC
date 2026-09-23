000100 01  REQU-W60144I1.                                                       
000200*                                 COPYTEXT FÖR REQU TILL W6014410         
000300*                                 W60144I1                                
000400     03 REQU-IDLEVNR-KOLLI-KEY                                            
000500                             PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000800     03 REQU-IDOKOLLI-KEY    PIC X(9).                                    
000900*                                 ODETTE KOLLINUMMER                      
001000*                                 ODETTE CASE NUMBER                      
001100     03 REQU-IDLOPNRM-KEY    PIC X(9).                                    
001200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001300*                                 (0VVDLLLLK)                             
001400*                                 SERIAL NO RECEIVING REPORT              
001500*                                 (0WWDLLLLC)                             
001600     03 REQU-IDDC-KEY        PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 REQU-IDSPRAK         PIC X(2).                                    
002000*                                 2-STÄLLIG ISO SPRÅKKOD                  
002100*                                 2-LETTER ISO LANGUAGE CODE              
002200     03 REQU-IDRADNR-START   PIC 9(5).                                    
002300*                                 RADNUMMER                               
002400*                                 LINE NO                                 
002500     03 REQU-KVRADER         PIC 9(5).                                    
002600*                                 ANTAL RADER                             
002700*                                 NUMBER OF LINES                         
002800     03 REQU-INPUT.                                                       
002900        05 REQU-INPUT-RAD.                                                
003000           07 REQU-IDRADNR-LINE                                           
003100                             OCCURS 50 TIMES                              
003200                             PIC X(3).                                    
003300*                                 RADNUMMER                               
003400*                                 LINE NO                                 
003500        05 REQU-INPUT-UPD.                                                
003600           07 REQU-IDANSTNR  PIC X(5).                                    
003700*                                 ANSTÄLLNINGSNUMMER                      
003800*                                 IDENTIFICATION NO EMPLOYEE              
003900           07 REQU-INPUT-LINE                                             
004000                             OCCURS 50 TIMES.                             
004100              09 REQU-KDCMDVAL-LINE                                       
004200                             PIC X(3).                                    
004300*                                 GENERELL KOMMANDOKOD                    
004400*                                 GENERAL COMMAND-CODE                    
004500              09 REQU-KVINLART-UPD-LINE                                   
004600                             PIC X(6).                                    
004700*                                 ANTAL I PARTIRAD                        
004800*                                 QTY/LINE IN A LOT                       
004900              09 REQU-ADINLOMR-NXT-UPD-LINE                               
005000                             PIC X(4).                                    
005100*                                 INLEVERANSOMRÅDE NÄSTA                  
005200*                                 RECEIVING AREA NEXT                     
005300*** END OF VILMAII-COPY LENGTH= 842 BYTES                                 
