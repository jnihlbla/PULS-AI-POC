000100 01  W61265.                                                              
000200*                                 INLAGT GODS UNDER VECKAN                
000300*                                 FÖR AK-UPPFÖLJNING                      
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDPTYP               PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 IDDISTR              PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 FLPRIO               PIC X.                                       
001400*                                 PRIORITERAD                             
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
001800*                                 ARTIKELPRIS NETTO                       
001900     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
002000*                                 VALUTAKURS                              
002100     03 KDVALISO             PIC X(3).                                    
002200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002300     03 KVANTMOT             PIC S9(7)           COMP-3.                  
002400*                                 ANTAL MOTTAGET                          
002500     03 TIINLMOT             PIC S9(7)           COMP-3.                  
002600*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
002700     03 TIINLMTI             PIC 9(4).                                    
002800*                                 MOTTAGNINGSTID     (TTMM)               
002900     03 TIINLINL             PIC S9(7)           COMP-3.                  
003000*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003100     03 TIINLITI             PIC 9(4).                                    
003200*                                 RAPPORTERINGSTID   INLAGD (R32)         
003300*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
