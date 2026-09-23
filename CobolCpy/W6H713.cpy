000100 01  JUST-W6H713.                                                         
000200*                                 KVALITET                                
000300*                                 KONTROLLRAPPORT - JUSTERING             
000400*                                 NYCKEL KDSEGKEY = 1                     
000500     03 JUST-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 JUST-KDKRJUST        PIC X.                                       
000900*                                 JUSTERINGSKOD                           
001000*                                 ADJUSTMENT CODE                         
001100     03 JUST-KDKRSTA         PIC X.                                       
001200*                                 KONTROLLRAPPORT STATUS                  
001300*                                 INSPECTION REPORT STATUS                
001400     03 JUST-KVARBTID        PIC S9(2)V9(1)      COMP-3.                  
001500*                                 ANTAL MANTIMMAR                         
001600*                                 NUMBER OF MAN HOURS                     
001700     03 JUST-KVART-RET       PIC S9(7)           COMP-3.                  
001800*                                 ANTAL ARTIKLAR I RETUR                  
001900*                                 QUANTITY INSPECTED PARTS                
002000     03 JUST-KVART-SJUST     PIC S9(7)           COMP-3.                  
002100*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
002200*                                 QUANTITY INSPECTED PARTS                
002300     03 JUST-KVART-SKROT     PIC S9(7)           COMP-3.                  
002400*                                 ANTAL SKROTADE ARTIKLAR                 
002500*                                 QUANTITY INSPECTED PARTS                
002600     03 JUST-SUMAT           PIC S9(7)V9(2)      COMP-3.                  
002700*                                 MATERIALKOSTNAD                         
002800     03 JUST-SUOMK           PIC S9(7)           COMP-3.                  
002900*                                 BELOPP SOM SKALL DEBITERAS              
003000*                                 KUND                                    
003100*                                 AMOUNT TO BE PAID BY CUSTOMER           
003200     03 JUST-TIREGDAT        PIC S9(7)           COMP-3.                  
003300*                                 REGISTRERINGSDATUM (≈≈MMDD)             
003400*                                 REGISTRATION DATE (YYMMDD)              
003500*** END COPY W6H713      LENGTH=30                                        
