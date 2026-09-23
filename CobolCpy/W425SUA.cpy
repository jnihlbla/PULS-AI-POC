000100 01  W425SUA-CTX.                                                         
000200*                                 INFO OM BIPACKAD RESTORDER              
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDSUPPL              PIC S9(5)           COMP-3.                  
001100*                                 LEVERANSNR TILL ÅTERFÖRSÄLJARE          
001200     03 IDORDNR-002          PIC S9(7)           COMP-3.                  
001300*                                 ORDERNR             IDORDNR-002         
001400     03 IDRONR-002           PIC S9(7)           COMP-3.                  
001500*                                 RESTORDERNUMMER      IDRONR-002         
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 REKSIFFR             PIC S9              COMP-3.                  
001900*                                 KONTROLLSIFFRA                          
002000     03 KVLEVART             PIC S9(7)           COMP-3.                  
002100*                                 LEVERERAT ANTAL STYCK                   
002200     03 KDRESTR              PIC S9(3)           COMP-3.                  
002300*                                 RESTRIKTIONSKOD                         
002400     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
002500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002600     03 TIKLOCK              PIC S9(9)           COMP-3.                  
002700*                                 KLOCKSLAG (TTMMSSTH)                    
002800     03 FILLERX69            PIC X(69).                                   
002900*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
