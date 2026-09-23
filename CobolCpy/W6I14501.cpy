000100 01  MID-W6I14501.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I14501                                
000400     03 MID-IDLEVNR-KOLLI-IN PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600*                                 SUPPLIER NUMBER                         
000700     03 MID-IDLEVNR-KOLLI-UT PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900*                                 SUPPLIER NUMBER                         
001000     03 MID-IDOKOLLI-IN      PIC X(9).                                    
001100*                                 ODETTE KOLLINUMMER                      
001200*                                 ODETTE CASE NUMBER                      
001300     03 MID-IDOKOLLI-UT      PIC X(9).                                    
001400*                                 ODETTE KOLLINUMMER                      
001500*                                 ODETTE CASE NUMBER                      
001600     03 MID-IDLOPNRM-IN      PIC X(9).                                    
001700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900*                                 SERIAL NO RECEIVING REPORT              
002000*                                 (0WWDLLLLC)                             
002100     03 MID-IDLOPNRM-UT      PIC X(9).                                    
002200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002300*                                 (0VVDLLLLK)                             
002400*                                 SERIAL NO RECEIVING REPORT              
002500*                                 (0WWDLLLLC)                             
002600     03 MID-IDDC-IN          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800*                                 WAREHOUSE IDENTIFIER                    
002900     03 MID-IDDC-UT          PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100*                                 WAREHOUSE IDENTIFIER                    
003200     03 MID-INPUT.                                                        
003300        05 MID-FLKLAR        PIC X.                                       
003400*                                 AVSLUTNINGSMARKERING                    
003500*                                 FINISHED FLAG                           
003600        05 MID-IDANSTNR      PIC X(5).                                    
003700*                                 ANSTÄLLNINGSNUMMER                      
003800*                                 IDENTIFICATION NO EMPLOYEE              
003900        05 MID-KDCMDVAL-RAD  OCCURS 12 TIMES                              
004000                             PIC X(3).                                    
004100*                                 GENERELL KOMMANDOKOD                    
004200*                                 GENERAL COMMAND-CODE                    
004300        05 MID-KVINLART-UPD  OCCURS 12 TIMES                              
004400                             PIC X(6).                                    
004500*                                 ANTAL I PARTIRAD                        
004600*                                 QTY/LINE IN A LOT                       
004700        05 MID-ADINLOMR-NXT-UPD                                           
004800                             OCCURS 12 TIMES                              
004900                             PIC X(4).                                    
005000*                                 INLEVERANSOMRÅDE NÄSTA                  
005100*                                 RECEIVING AREA NEXT                     
005200     03 MID-IDLOPNRM-RAD     OCCURS 12 TIMES                              
005300                             PIC X(9).                                    
005400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005500*                                 (0VVDLLLLK)                             
005600*                                 SERIAL NO RECEIVING REPORT              
005700*                                 (0WWDLLLLC)                             
005800     03 MID-IDRADNR-RAD      OCCURS 12 TIMES                              
005900                             PIC X(3).                                    
006000*                                 RADNUMMER                               
006100*                                 LINE NO                                 
