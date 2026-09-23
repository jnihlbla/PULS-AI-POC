000100 01  SHIST-W61256.                                                        
000200*                                 R32:ER OCH 310:OR                       
000300*                                                                         
000400     03 SHIST-IDDC           PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 SHIST-IDDISTR        PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 SHIST-KDFRAKT        PIC S9(3)           COMP-3.                  
000900*                                 FRAKTSÄTT DC TILL KUND                  
001000     03 SHIST-IDARTNR        PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 SHIST-PRARTNTO       PIC S9(7)V9(2)      COMP-3.                  
001300*                                 ARTIKELPRIS NETTO                       
001400     03 SHIST-PRKURS         PIC S9(6)V9(5)      COMP-3.                  
001500*                                 VALUTAKURS                              
001600     03 SHIST-KDVALISO       PIC X(3).                                    
001700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001800     03 SHIST-KVANTMOT       PIC S9(7)           COMP-3.                  
001900*                                 ANTAL MOTTAGET                          
002000     03 SHIST-FLPRIO         PIC X.                                       
002100*                                 PRIORITERAD                             
002200     03 SHIST-KVROS          PIC S9(7)           COMP-3.                  
002300*                                 RESTORDERSALDO                          
002400     03 SHIST-PRAVCOST       PIC S9(7)V9(2)      COMP-3.                  
002500*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
002600     03 SHIST-KVAVIS         PIC S9(7)           COMP-3.                  
002700*                                 AVISERAT ANTAL                          
002800     03 SHIST-IDPTYP         PIC X(3).                                    
002900*                                 POSTTYP                                 
003000     03 SHIST-TIINLMOT       PIC S9(7)           COMP-3.                  
003100*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
003200     03 SHIST-TIINLMTI       PIC 9(4).                                    
003300*                                 MOTTAGNINGSTID     (TTMM)               
003400     03 SHIST-TIINLINL       PIC S9(7)           COMP-3.                  
003500*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003600     03 SHIST-TIINLITI       PIC 9(4).                                    
003700*                                 RAPPORTERINGSTID   INLAGD (R32)         
003800*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  
