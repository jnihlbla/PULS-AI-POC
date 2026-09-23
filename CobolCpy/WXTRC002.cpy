000100 01  002-WXTRC002.                                                        
000200*                                 URVAL FRÅN W6D1                         
000300*                                 POSTER TILL AK-BELÄGGNINGSLISTA         
000400*                                 POSTTYP 002 RADINFORMATION              
000500*                                 /                                       
000600*                                 EXTRACTION FROM W6D1                    
000700*                                 RECORDS TO AK WORK LOAD LISTING         
000800*                                 RECTYPE 002 = LINE INFO                 
000900     03 002-IDPTYP           PIC X(3).                                    
001000*                                 POSTTYP                                 
001100*                                 RECORD TYPE                             
001200     03 002-IDDC             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 002-ADINLOMR         PIC X(4).                                    
001600*                                 INLEVERANSOMRÅDE                        
001700*                                 RECEIVING AREA                          
001800     03 002-IDLOPNRM         PIC 9(8).                                    
001900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002000*                                 (0VVDLLLLK)                             
002100*                                 SERIAL NO RECEIVING REPORT              
002200*                                 (0WWDLLLLC)                             
002300     03 002-IDRADNR          PIC 9(4).                                    
002400*                                 RADNUMMER                               
002500*                                 LINE NO                                 
002600     03 002-IDOKOLLI         PIC 9(9).                                    
002700*                                 ODETTE KOLLINUMMER                      
002800*                                 ODETTE CASE NUMBER                      
002900     03 002-KVINLART         PIC 9(6).                                    
003000*                                 ANTAL I PARTIRAD                        
003100*                                 QTY/LINE IN A LOT                       
003200     03 002-PRARTSTD         PIC 9(7)V9(2).                               
003300*                                 ARTIKELSTANDARDPRIS                     
003400*                                 STANDARD PRICE                          
003500     03 002-KDINLPRIO        PIC 9(2).                                    
003600*                                 PRIORITETSGRUPP                         
003700*                                 PRIORITY GROUP                          
003800     03 002-KDINLOMR         PIC X(3).                                    
003900*                                 TYP AV INLEVERANSOMRÅDE                 
004000*                                 TYPE OF RECEIVING AREA                  
004100*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
