000100 01  W236L001.                                                            
000200*                                 COPYTEXT FÖR LÄSNING AV                 
000300*                                 ART-INFO OCH LEV-INFO.                  
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-ART-INFO        VALUE +101.                                  
000700      88 LAS-LEV-INFO        VALUE +102.                                  
000800     03 KDSVAR               PIC X.                                       
000900      88 ART-INFO-FINNS      VALUE ' '.                                   
001000      88 LEV-INFO-FINNS      VALUE ' '.                                   
001100*                                 SVAR FRÅN SUBPROGRAM                    
001200     03 IO-AREA.                                                          
001300*                                 ART-AREA  WDK6                          
001400        05 IDARTNR           PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
001700*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
001800        05 IDANSK            PIC S9(3)           COMP-3.                  
001900*                                 ANSKAFFARNUMMER                         
002000        05 KDPRODSL          PIC S9(3)           COMP-3.                  
002100*                                 PRODUKTSLAG                             
002200        05 KDHF              PIC S9(3)           COMP-3.                  
002300*                                 HUVUDFÖRRÅDSMÄRKNING   KDHF-002         
002400        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
002500*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
002600        05 IDAVINR-SEN       PIC S9(7)           COMP-3.                  
002700*                                 AVINUMMER SENASTE INLEVERANS            
002800        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
002900*                                 SENAST AVISERAT ANTAL                   
003000        05 IDLEVNR-SEN       PIC X(5).                                    
003100*                                 SENASTE LEVERANTÖR                      
003200        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
003300*                                 ARTIKELSTANDARDPRIS                     
003400        05 FLTOPP            PIC X.                                       
003500*                                 TOPP-200-ARTIKEL                        
003600        05 KDGK              PIC S9              COMP-3.                  
003700*                                 GODSMOTTAGAREKOD                        
003800        05 KVAKS-E           PIC S9(7)           COMP-3.                  
003900*                                 DEL AV EFR TILL ANDRA CLAGRET           
004000        05 KVAKS             PIC S9(7)           COMP-3.                  
004100*                                 ANKOMSTSALDO                            
004200        05 KVRESS            PIC S9(7)           COMP-3.                  
004300*                                 RESERVERAT ANTAL ARTIKLAR               
004400        05 KVLS              PIC S9(7)           COMP-3.                  
004500*                                 LAGERSALDO                              
004600        05 KVROS             PIC S9(7)           COMP-3.                  
004700*                                 RESTORDERSALDO                          
004800        05 KVSLAGER          PIC S9(7)           COMP-3.                  
004900*                                 SÄKERHETSLAGER                          
005000        05 IDLEVNR           PIC X(5).                                    
005100*                                 LEVERANTÖRNUMMER                        
005200        05 KVBR              PIC S9(7)           COMP-3.                  
005300*                                 BESTÄLLNINGSREST                        
005400*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
