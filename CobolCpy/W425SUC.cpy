000100 01  W425SUC-CTX.                                                         
000200*                                 INFO OM ORDERKLASSFÖRÄNDRING            
000300*                                 PÅ RADDATABAS                           
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDISTR              PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 IDSUPPL              PIC S9(5)           COMP-3.                  
001200*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001300     03 IDORDNR-002          PIC S9(7)           COMP-3.                  
001400*                                 ORDERNR             IDORDNR-002         
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 REKSIFFR             PIC S9              COMP-3.                  
001800*                                 KONTROLLSIFFRA                          
001900     03 KVBEART              PIC S9(7)           COMP-3.                  
002000*                                 BESTÄLLT ANTAL STYCKEN                  
002100     03 KDRO                 PIC S9              COMP-3.                  
002200*                                 RESTORDERKOD PÅ INFORMATION             
002300*                                 TILL VR                                 
002400     03 KDORDER              PIC S9              COMP-3.                  
002500*                                 ORDERKOD                                
002600     03 KDORDER-NY           PIC S9              COMP-3.                  
002700*                                 ORDERKOD                                
002800     03 KDVRINFO             PIC S9              COMP-3.                  
002900*                                 PÅVERKAN I VR/DSP SYSTEM                
003000     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
003100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003200     03 TIKLOCK              PIC S9(9)           COMP-3.                  
003300*                                 KLOCKSLAG (TTMMSSTH)                    
003400     03 FILLERX71            PIC X(71).                                   
003500*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
