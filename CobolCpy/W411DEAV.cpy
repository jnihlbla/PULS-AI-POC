000100 01  DEAV-W411DEAV.                                                       
000200*                                 LÄNKAREA TILL W411DEAV -                
000300*                                 DEFINITIVAVBOKNING                      
000400     03 DEAV-IN-AREA.                                                     
000500        05 DEAV-IDKAMPRF-IN  PIC S9(7)           COMP-3.                  
000600*                                 KAMPANJREFERENS                         
000700        05 DEAV-FLFORBI-IN   PIC X.                                       
000800*                                 FÖRBIORDERFLAGGA                        
000900        05 DEAV-IDARTNR-IN   PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100        05 DEAV-IDANSK-IN    PIC S9(3)           COMP-3.                  
001200*                                 ANSKAFFARNUMMER                         
001300        05 DEAV-TIRODAT-IN   PIC S9(7)           COMP-3.                  
001400*                                 RESTORDERDATUM         (ÅÅMMDD)         
001500        05 DEAV-ADLAGOMR-IN  PIC S9(3)           COMP-3.                  
001600*                                 LAGEROMRÅDE                             
001700        05 DEAV-KDORDKL-IN   PIC S9              COMP-3.                  
001800*                                 ORDERKLASS                              
001900        05 DEAV-IDDC-IN      PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100        05 DEAV-IDDC-RO-IN   PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300        05 DEAV-IDDISTR-IN   PIC S9(5)           COMP-3.                  
002400*                                 DISTRIKTNUMMER                          
002500        05 DEAV-KVAKS-CDC-IN PIC S9(7)           COMP-3.                  
002600*                                 DEL AV AK SOM LIGGER I CDC              
002700        05 DEAV-KVAKS-PAV-IN PIC S9(7)           COMP-3.                  
002800*                                 DEL AV AK PÅ VÄG                        
002900        05 DEAV-KVLS-IN      PIC S9(7)           COMP-3.                  
003000*                                 LAGERSALDO                              
003100        05 DEAV-KVRESS-IN    PIC S9(7)           COMP-3.                  
003200*                                 RESERVERAT ANTAL ARTIKLAR               
003300        05 DEAV-KVSPANT-IN   PIC S9(7)           COMP-3.                  
003400*                                 SPÄRRAT ANTAL                           
003500        05 DEAV-KVUTRS-IN    PIC S9(7)           COMP-3.                  
003600*                                 UTREDNINGSSALDO                         
003700        05 DEAV-RERF-ART-IN  PIC S9V9(4)         COMP-3.                  
003800*                                 RANSONERINGSFAKTOR ARTIKEL              
003900        05 DEAV-RERF-RAD-NY-IN                                            
004000                             PIC S9V9(4)         COMP-3.                  
004100*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
004200        05 DEAV-KVQPACK-0-IN PIC S9(5)           COMP-3.                  
004300*                                 ANTAL I Q0 FÖRPACKNING                  
004400        05 DEAV-KVQPACK-1-IN PIC S9(5)           COMP-3.                  
004500*                                 ANTAL I Q1 FÖRPACKNING                  
004600        05 DEAV-IDFKNGRP-IN  PIC S9(5)           COMP-3.                  
004700*                                 FUNKTIONSGRUPP                          
004800        05 DEAV-KDSORT-IN    PIC X(2).                                    
004900*                                 SORT-KOD                                
005000        05 DEAV-KDPRODSL-IN  PIC S9(3)           COMP-3.                  
005100*                                 PRODUKTSLAG                             
005200        05 DEAV-KDKVBRYT-IN  PIC S9              COMP-3.                  
005300*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005400        05 DEAV-KDLEVSP-IN   PIC S9(3)           COMP-3.                  
005500*                                 SPÄRRKOD LEVERANS                       
005600        05 DEAV-FLAKPLOC-IN  PIC X.                                       
005700*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
005800        05 DEAV-FLORDSPE-IN  PIC X.                                       
005900*                                 SPECIALORDERFLAGGA                      
006000        05 DEAV-FLOVRLEV-IN  PIC X.                                       
006100*                                 ÖVERLEVERANS                            
006200        05 DEAV-KVBEART-Q-IN PIC S9(7)           COMP-3.                  
006300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
006400        05 DEAV-KVPREAVB-IN  PIC S9(7)           COMP-3.                  
006500*                                 PREL-AVB KVANT                          
006600        05 DEAV-KVPRERO-IN   PIC S9(7)           COMP-3.                  
006700*                                 PRELIMINÄR RO-KVANT                     
006800        05 DEAV-IDKUNDRF-RO-IN                                            
006900                             PIC X(10).                                   
007000*                                 KUND REF PÅ RO                          
007100        05 DEAV-RERF-RAD-IN  PIC S9V9(4)         COMP-3.                  
007200*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
007300        05 DEAV-IDLEVNR-IN   PIC X(5).                                    
007400*                                 LEVERANTÖRNUMMER                        
007500        05 DEAV-FLRESTN-IN   PIC X.                                       
007600*                                 RESTNOTERING ?                          
007700        05 DEAV-KDTPOTYP-IN  PIC S9              COMP-3.                  
007800*                                 TYP AV TIDPLANERAD ORDER                
007900        05 DEAV-IDSYSTEM-IN  PIC X(4).                                    
008000*                                 VOLVO VCCS SYSTEMNUMMER                 
008100        05 DEAV-KVSPARR-KVAL-IN                                           
008200                             PIC S9(7)           COMP-3.                  
008300*                                 SPÄRRAT ANTAL KVALITETSFEL              
008400        05 DEAV-IDKUNDNR-IN  PIC S9(7)           COMP-3.                  
008500*                                 KUNDNUMMER                              
008600        05 DEAV-IDORDNR5-IN  PIC 9(5).                                    
008700*                                 ORDERNUMMER                             
008800        05 DEAV-IDPRODNR-IN  PIC S9(7)           COMP-3.                  
008900*                                 PRODUKTIONSNUMMER                       
009000        05 DEAV-IDPLKLST-IN  PIC S9(3)           COMP-3.                  
009100*                                 PLOCKLISTNUMMER                         
009200        05 DEAV-BERADREF-IN  PIC X(10).                                   
009300*                                 KUNDENS RADREFERENS                     
009400        05 DEAV-FLSDCLEV-IN  PIC X.                                       
009500*                                 LEVERANSSTYRNING SDC                    
009600        05 DEAV-IDUSER-SPKVAL-IN                                          
009700                             PIC X(8).                                    
009800*                                 ANVÄNDAR-ID KVALITETSPÄRR               
009900        05 DEAV-KDFDKRAV-IN  PIC S9(3)           COMP-3.                  
010000*                                 TRANSPORTFÖRPACKNINGSKOD                
009900        05 DEAV-FILLER       PIC X.                                       
010000     03 DEAV-UT-AREA.                                                     
010100        05 DEAV-FLAKPLOC-UT  PIC X.                                       
010200*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
010300        05 DEAV-KVAVBART-UT  PIC S9(7)           COMP-3.                  
010400*                                 AVBOKAT ANTAL ARTIKLAR                  
010500        05 DEAV-KDORDBEK-UT  PIC 9(2).                                    
010600*                                 ORDERBEKRÄFTELSEKOD                     
010700        05 DEAV-RERF-RAD-UT  PIC S9V9(4)         COMP-3.                  
010800*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
010900        05 DEAV-KVEFRS-UT    PIC S9(7)           COMP-3.                  
011000*                                 EJ FAKTURERAT ANTAL STYCK               
011100        05 DEAV-KVLS-UT      PIC S9(7)           COMP-3.                  
011200*                                 LAGERSALDO                              
011300        05 DEAV-KVRESS-UT    PIC S9(7)           COMP-3.                  
011400*                                 RESERVERAT ANTAL ARTIKLAR               
011500        05 DEAV-KVROS-UT     PIC S9(7)           COMP-3.                  
011600*                                 RESTORDERSALDO                          
011700        05 DEAV-KDROO-UT     PIC S9              COMP-3.                  
011800*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
012100*** END OF VILMAII-COPY LENGTH= 179 BYTES                                 
