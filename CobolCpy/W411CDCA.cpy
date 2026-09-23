000100 01  CDCA-W411CDCA.                                                       
000200*                                 LÄNKAREA TILL W411CDCA -                
000300*                                 KONTROLL AV PRELIMINÄRBOKNING           
000400*                                 AV EN CDC-RAD                           
000500     03 CDCA-IN-AREA.                                                     
000600        05 CDCA-FLFINLV-IN   PIC X.                                       
000700        05 CDCA-FLFORBI-IN   PIC X.                                       
000800*                                 FÖRBIORDERFLAGGA                        
000900        05 CDCA-FLORDSPE-IN  PIC X.                                       
001000*                                 SPECIALORDERFLAGGA                      
001100        05 CDCA-FLOVRLEV-IN  PIC X.                                       
001200*                                 ÖVERLEVERANS                            
001300        05 CDCA-FLPRELRO-IN  PIC X.                                       
001400*                                 PRELIMINÄR RESTORDERFLAGGA              
001500        05 CDCA-FLRESTN-IN   PIC X.                                       
001600*                                 RESTNOTERING ?                          
001700        05 CDCA-FLSLATT-IN   PIC X.                                       
001800*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
001900*                                 LL BERÄKNAS ELLER EJ                    
002000*                                 OM FLRESTN = J OCH FLSLATT = J,         
002100*                                  DÅ BERÄKNAS KVSLATT                    
002200        05 CDCA-IDARTNR-IN   PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400        05 CDCA-IDDC-IN      PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600        05 CDCA-IDDISTR-IN   PIC S9(5)           COMP-3.                  
002700*                                 DISTRIKTNUMMER                          
002800        05 CDCA-IDKAMPRF-IN  PIC S9(7)           COMP-3.                  
002900*                                 KAMPANJREFERENS                         
003000        05 CDCA-IDKUNDRF-RO-IN                                            
003100                             PIC X(10).                                   
003200*                                 KUND REF PÅ RO                          
003300        05 CDCA-IDSYSTEM-IN  PIC X(4).                                    
003400*                                 VOLVO VCCS SYSTEMNUMMER                 
003500        05 CDCA-IDLEVNR-IN   PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700        05 CDCA-IDFKNGRP-IN  PIC S9(5)           COMP-3.                  
003800*                                 FUNKTIONSGRUPP                          
003900        05 CDCA-KDERS-IN     PIC S9(3)           COMP-3.                  
004000*                                 ERSÄTTNINGSKOD                          
004100        05 CDCA-KDKVBRYT-IN  PIC S9              COMP-3.                  
004200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004300        05 CDCA-KDORDKL-IN   PIC S9              COMP-3.                  
004400*                                 ORDERKLASS                              
004500        05 CDCA-KDPRODSL-IN  PIC S9(3)           COMP-3.                  
004600*                                 PRODUKTSLAG                             
004700        05 CDCA-KDPROTYP-IN  PIC X.                                       
004800*                                 TYP AV PROFORMA                         
004900        05 CDCA-KDSORT-IN    PIC X(2).                                    
005000*                                 SORT-KOD                                
005100        05 CDCA-KDTPOTYP-IN  PIC S9              COMP-3.                  
005200*                                 TYP AV TIDPLANERAD ORDER                
005300        05 CDCA-KDUART-IN    PIC X.                                       
005400*                                 UNDANTAGSARTIKEL                        
005500        05 CDCA-KVBEART-IN   PIC S9(7)           COMP-3.                  
005600*                                 BESTÄLLT ANTAL STYCKEN                  
005700        05 CDCA-KVBEART-Q-IN PIC S9(7)           COMP-3.                  
005800*                                 BESTÄLLT KVANTANPASSAT ANTAL            
005900        05 CDCA-KVQPACK-0-IN PIC S9(5)           COMP-3.                  
006000*                                 ANTAL I Q0 FÖRPACKNING                  
006100        05 CDCA-KVQPACK-1-IN PIC S9(5)           COMP-3.                  
006200*                                 ANTAL I Q1 FÖRPACKNING                  
006300        05 CDCA-RERF-ART-IN  PIC S9V9(4)         COMP-3.                  
006400*                                 RANSONERINGSFAKTOR ARTIKEL              
006500        05 CDCA-RERF-RAD-IN  PIC S9V9(4)         COMP-3.                  
006600*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
006700        05 CDCA-RESLATT-IN   PIC S9(3)           COMP-3.                  
006800*                                 SLATTGRÄNS                              
006900        05 CDCA-KVAKS-CDC-IN PIC S9(7)           COMP-3.                  
007000*                                 DEL AV AK SOM LIGGER I CDC              
007100        05 CDCA-KVAKS-PAV-IN PIC S9(7)           COMP-3.                  
007200*                                 DEL AV AK PÅ VÄG                        
007300        05 CDCA-KVLS-IN      PIC S9(7)           COMP-3.                  
007400*                                 LAGERSALDO                              
007500        05 CDCA-KVRESS-IN    PIC S9(7)           COMP-3.                  
007600*                                 RESERVERAT ANTAL ARTIKLAR               
007700        05 CDCA-KVSPANT-IN   PIC S9(7)           COMP-3.                  
007800*                                 SPÄRRAT ANTAL                           
007900        05 CDCA-KVUTRS-IN    PIC S9(7)           COMP-3.                  
008000*                                 UTREDNINGSSALDO                         
008100        05 CDCA-KVSPARR-KVAL-IN                                           
008200                             PIC S9(7)           COMP-3.                  
008300*                                 SPÄRRAT ANTAL KVALITETSFEL              
008400        05 CDCA-IDKUNDNR-IN  PIC S9(7)           COMP-3.                  
008500*                                 KUNDNUMMER                              
008600        05 CDCA-BERADREF-IN  PIC X(10).                                   
008700*                                 KUNDENS RADREFERENS                     
008800        05 CDCA-KDCALL       PIC S9(3)           COMP-3.                  
008900*                                 ANROPSTYP                               
009000     03 CDCA-UT-AREA.                                                     
009100        05 CDCA-FLAKPLOC-UT  PIC X.                                       
009200*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
009300        05 CDCA-KDORDBEK-UT  PIC 9(2).                                    
009400*                                 ORDERBEKRÄFTELSEKOD                     
009500        05 CDCA-KVANNANT-UT  PIC S9(7)           COMP-3.                  
009600*                                 ANNULLERAT ANTAL ARTIKLAR               
009700        05 CDCA-KVBEART-UT   PIC S9(7)           COMP-3.                  
009800*                                 BESTÄLLT ANTAL STYCKEN                  
009900        05 CDCA-KVBEART-Q-UT PIC S9(7)           COMP-3.                  
010000*                                 BESTÄLLT KVANTANPASSAT ANTAL            
010100        05 CDCA-KVPREAVB-UT  PIC S9(7)           COMP-3.                  
010200*                                 PREL-AVB KVANT                          
010300        05 CDCA-KVPRERO-UT   PIC S9(7)           COMP-3.                  
010400*                                 PRELIMINÄR RO-KVANT                     
010500        05 CDCA-KVSLATT-UT   PIC S9(7)           COMP-3.                  
010600*                                 BERÄKNAD SLATTGRÄNS                     
010700        05 CDCA-RERF-RAD-UT  PIC S9V9(4)         COMP-3.                  
010800*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
010900*** END OF VILMAII-COPY LENGTH= 150 BYTES                                 
