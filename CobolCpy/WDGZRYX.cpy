000100 01  RYX-WDGZRYX.                                                         
000200*                                 RYX                                     
000300*                                 SKAPAS VID UTSKRIFT AV                  
000400*                                 EN ORDERRAD BEKR KOD 90 91              
000500*                                 FRÅN W411DEAV.                          
000600*                                 ANVÄNDS FÖR FIL TILL LOGISTIK.          
000700     03 RYX-IDPTYP           PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 RYX-IDDISTR-IN       PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 RYX-IDKUNDNR-IN      PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300     03 RYX-IDORDNR5-IN      PIC 9(5).                                    
001400*                                 ORDERNUMMER                             
001500     03 RYX-IDARTNR-IN       PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 RYX-KDORDKL-IN       PIC S9              COMP-3.                  
001800*                                 ORDERKLASS                              
001900     03 RYX-IDKAMPRF-IN      PIC S9(7)           COMP-3.                  
002000*                                 KAMPANJREFERENS                         
002100     03 RYX-TIRODAT-IN       PIC S9(7)           COMP-3.                  
002200*                                 RESTORDERDATUM         (ÅÅMMDD)         
002300     03 RYX-ADLAGOMR-IN      PIC S9(3)           COMP-3.                  
002400*                                 LAGEROMRÅDE                             
002500     03 RYX-IDDC-RO-IN       PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 RYX-KVAKS-CDC-IN     PIC S9(7)           COMP-3.                  
002800*                                 DEL AV AK SOM LIGGER I CDC              
002900     03 RYX-KVAKS-PAV-IN     PIC S9(7)           COMP-3.                  
003000*                                 DEL AV AK PÅ VÄG                        
003100     03 RYX-KVLS-IN          PIC S9(7)           COMP-3.                  
003200*                                 LAGERSALDO                              
003300     03 RYX-KVRESS-IN        PIC S9(7)           COMP-3.                  
003400*                                 RESERVERAT ANTAL ARTIKLAR               
003500     03 RYX-KVSPANT-IN       PIC S9(7)           COMP-3.                  
003600*                                 SPÄRRAT ANTAL                           
003700     03 RYX-KVUTRS-IN        PIC S9(7)           COMP-3.                  
003800*                                 UTREDNINGSSALDO                         
003900     03 RYX-RERF-ART-IN      PIC S9V9(4)         COMP-3.                  
004000*                                 RANSONERINGSFAKTOR ARTIKEL              
004100     03 RYX-RERF-RAD-NY-IN   PIC S9V9(4)         COMP-3.                  
004200*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
004300     03 RYX-KDSORT-IN        PIC X(2).                                    
004400*                                 SORT-KOD                                
004500     03 RYX-KDPRODSL-IN      PIC S9(3)           COMP-3.                  
004600*                                 PRODUKTSLAG                             
004700     03 RYX-KDLEVSP-IN       PIC S9(3)           COMP-3.                  
004800*                                 SPÄRRKOD LEVERANS                       
004900     03 RYX-FLAKPLOC-IN      PIC X.                                       
005000*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
005100     03 RYX-KVBEART-Q-IN     PIC S9(7)           COMP-3.                  
005200*                                 BESTÄLLT KVANTANPASSAT ANTAL            
005300     03 RYX-KVPREAVB-IN      PIC S9(7)           COMP-3.                  
005400*                                 PREL-AVB KVANT                          
005500     03 RYX-KVPRERO-IN       PIC S9(7)           COMP-3.                  
005600*                                 PRELIMINÄR RO-KVANT                     
005700     03 RYX-RERF-RAD-IN      PIC S9V9(4)         COMP-3.                  
005800*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
005900     03 RYX-FLRESTN-IN       PIC X.                                       
006000*                                 RESTNOTERING ?                          
006100     03 RYX-KVSPARR-KVAL-IN  PIC S9(7)           COMP-3.                  
006200*                                 SPÄRRAT ANTAL KVALITETSFEL              
006300*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
