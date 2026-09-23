000100 01  002-W6113002.                                                        
000200*                                 URVAL FRÅN W6D1                         
000300*                                 POSTER TILL AK-BELÄGGNINGSLISTA         
000400*                                 POSTTYP 002 RADINFORMATION              
000500*                                 /                                       
000600*                                 EXTRACTION FROM W6D1                    
000700*                                 RECORDS TO AK WORK LOAD LISTING         
000800*                                 RECTYPE 002 = LINE INFO                 
000900*                                                             .           
001000     03 002-IDPTYP           PIC X(3).                                    
001100*                                 POSTTYP                                 
001200*                                 RECORD TYPE                             
001300     03 002-IDDC             PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 002-ADINLOMR         PIC X(4).                                    
001700*                                 INLEVERANSOMRÅDE                        
001800*                                 RECEIVING AREA                          
001900     03 002-IDLOPNRM         PIC 9(8).                                    
002000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002100*                                 (0VVDLLLLK)                             
002200*                                 SERIAL NO RECEIVING REPORT              
002300*                                 (0WWDLLLLC)                             
002400     03 002-IDRADNR          PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600*                                 LINE NO                                 
002700     03 002-IDOKOLLI         PIC 9(9).                                    
002800*                                 ODETTE KOLLINUMMER                      
002900*                                 ODETTE CASE NUMBER                      
003000     03 002-KVINLART         PIC 9(6).                                    
003100*                                 ANTAL I PARTIRAD                        
003200*                                 QTY/LINE IN A LOT                       
003300     03 002-PRARTSTD         PIC 9(7)V9(2).                               
003400*                                 ARTIKELSTANDARDPRIS                     
003500*                                 STANDARD PRICE                          
003600     03 002-KDINLPRIO        PIC 9(2).                                    
003700*                                 PRIORITETSGRUPP                         
003800*                                 PRIORITY GROUP                          
003900*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
