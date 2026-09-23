000100 01  MID-W6I14401.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I14501                                
000400     03 MID-IDLEVNR-KOLLI-IN PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000700     03 MID-IDOKOLLI-IN      PIC X(9).                                    
000800*                                 ODETTE KOLLINUMMER                      
000900*                                 ODETTE CASE NUMBER                      
001000     03 MID-IDLOPNRM-IN      PIC X(9).                                    
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300*                                 SERIAL NO RECEIVING REPORT              
001400*                                 (0WWDLLLLC)                             
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 MID-INPUT.                                                        
001900        05 MID-IDANSTNR      PIC X(5).                                    
002000*                                 ANSTÄLLNINGSNUMMER                      
002100*                                 IDENTIFICATION NO EMPLOYEE              
002200        05 MID-KDCMDVAL-RAD  OCCURS 8 TIMES                               
002300                             PIC X(3).                                    
002400*                                 GENERELL KOMMANDOKOD                    
002500*                                 GENERAL COMMAND-CODE                    
002600        05 MID-KVINLART-UPD  OCCURS 8 TIMES                               
002700                             PIC X(6).                                    
002800*                                 ANTAL I PARTIRAD                        
002900*                                 QTY/LINE IN A LOT                       
003000        05 MID-ADINLOMR-NXT-UPD                                           
003100                             OCCURS 8 TIMES                               
003200                             PIC X(4).                                    
003300*                                 INLEVERANSOMRÅDE NÄSTA                  
003400*                                 RECEIVING AREA NEXT                     
003500     03 MID-IDRADNR-RAD      OCCURS 8 TIMES                               
003600                             PIC X(3).                                    
003700*                                 RADNUMMER                               
003800*                                 LINE NO                                 
003900*** END OF VILMAII-COPY LENGTH= 158 BYTES                                 
