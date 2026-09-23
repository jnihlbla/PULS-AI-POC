000100 01  SHIST-W61253.                                                        
000200*                                 INLEVERANS HISTORIK SDC                 
000300*                                 UPPFÖLJNING                             
000400*                                 DE FLESTA R30 & R32                     
000500     03 SHIST-IDPTYP         PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 SHIST-IDDC           PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 SHIST-IDDISTR        PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 SHIST-IDFAKT         PIC S9(7)           COMP-3.                  
001200*                                 FAKTURANUMMER                           
001300     03 SHIST-IDKOLLI        PIC S9(5)           COMP-3.                  
001400*                                 KOLLINUMMER                             
001500     03 SHIST-IDKUNDNR       PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700     03 SHIST-IDARTNR        PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900     03 SHIST-PRARTNTO       PIC S9(7)V9(2)      COMP-3.                  
002000*                                 ARTIKELPRIS NETTO                       
002100     03 SHIST-PRKURS         PIC S9(6)V9(5)      COMP-3.                  
002200*                                 VALUTAKURS                              
002300     03 SHIST-KDVALISO       PIC X(3).                                    
002400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002500     03 SHIST-KVAVIS         PIC S9(7)           COMP-3.                  
002600*                                 AVISERAT ANTAL                          
002700     03 SHIST-KVANTMOT       PIC S9(7)           COMP-3.                  
002800*                                 ANTAL MOTTAGET                          
002900     03 SHIST-TIBERANK       PIC 9(6).                                    
003000*                                 BERÄKNAD ANKOMSTDATUM                   
003100     03 SHIST-FLPRIO         PIC X.                                       
003200*                                 PRIORITERAD                             
003300     03 SHIST-KDFRAKT        PIC S9(3)           COMP-3.                  
003400*                                 FRAKTSÄTT DC TILL KUND                  
003500     03 SHIST-IDORDNR5       PIC 9(5).                                    
003600*                                 ORDERNUMMER                             
003700     03 SHIST-TIINLMOT       PIC S9(7)           COMP-3.                  
003800*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
003900     03 SHIST-TIINLMTI       PIC 9(4).                                    
004000*                                 MOTTAGNINGSTID     (TTMM)               
004100     03 SHIST-TIINLINL       PIC S9(7)           COMP-3.                  
004200*                                 RAPPORTERINGSDATUM INLAGD (R32)         
004300     03 SHIST-TIINLITI       PIC 9(4).                                    
004400*                                 RAPPORTERINGSTID   INLAGD (R32)         
004500*** END OF VILMAII-COPY LENGTH= 76 BYTES                                  
