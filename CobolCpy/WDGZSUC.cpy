000100 01  SUC-WDGZSUC.                                                         
000200*                                 SUC                                     
000300*                                 SKAPAS FÖR RO-RADER PÅ VR-              
000400*                                 DISTRIKT VID ÄNDRING AV ORDER-          
000500*                                 KLASS PÅ BILD 4572 O 4573.              
000600*                                 ANVÄNDS VID TRANSAKTIONSSKA-            
000700*                                 PANDE TILL ÖVRIGA SYSTEM.               
000800     03 SUC-IDPTYP           PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 SUC-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200     03 SUC-IDKUNDNR         PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 SUC-IDDC             PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 SUC-IDORDNR          PIC S9(7)           COMP-3.                  
001700*                                 ORDERNR             IDORDNR-002         
001800     03 SUC-IDARTNR          PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 SUC-REKSIFFR         PIC S9              COMP-3.                  
002100*                                 KONTROLLSIFFRA                          
002200     03 SUC-KVBEART          PIC S9(7)           COMP-3.                  
002300*                                 BESTÄLLT ANTAL STYCKEN                  
002400     03 SUC-KDRO             PIC S9              COMP-3.                  
002500*                                 RESTORDERKOD PÅ INFORMATION             
002600*                                 TILL VR                                 
002700     03 SUC-KDORDER          PIC S9              COMP-3.                  
002800*                                 ORDERKOD                                
002900     03 SUC-KDORDER-NY       PIC S9              COMP-3.                  
003000*                                 ORDERKOD                                
003100     03 SUC-KDVRINFO         PIC S9              COMP-3.                  
003200*                                 PÅVERKAN I VR/DSP SYSTEM                
003300*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
