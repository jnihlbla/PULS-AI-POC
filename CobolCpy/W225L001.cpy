000100 01  W225L001.                                                            
000200*                                 LÄNKAREA I W225 MOT                     
000300*                                 ARTIKELREGISTER                         
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-ARTIKELINFO     VALUE +1.                                    
000700      88 LAS-LEVERANSINFO    VALUE +2.                                    
000800*                                            KDCALL-W222-W228-002         
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 FLJANEJ-IDARTNR      PIC X.                                       
001200      88 ANROP-OK            VALUE 'J'.                                   
001300      88 ANROP-FEL           VALUE 'N'.                                   
001400*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001500     03 KDCLPOST             PIC S9              COMP-3.                  
001600*                                 CENTRALLAGERPOST                        
001700     03 C1-INFO.                                                          
001800        05 FLTOPP            PIC X.                                       
001900*                                 TOPP-200-ARTIKEL                        
002000        05 KVLS              PIC S9(7)           COMP-3.                  
002100*                                 LAGERSALDO                              
002200        05 KVRESS            PIC S9(7)           COMP-3.                  
002300*                                 RESERVERAT ANTAL ARTIKLAR               
002400        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
002500*                                 ORDERKÖSALDO, KLASS 2-4                 
002600        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
002700*                                 ORDERKÖSALDO, KLASS 1                   
002800        05 KVOKS-VOR         PIC S9(7)           COMP-3.                  
002900*                                 ORDERKÖSALDO, VOR                       
003000        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
003100*                                 DEL AV AK SOM LIGGER I CDC              
003200        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
003300*                                 DEL AV AK PÅ VÄG                        
003400        05 KVAKS-T           PIC S9(7)           COMP-3.                  
003500*                                 DEL AV AK I EN TERMINAL                 
003600        05 KVUTRS            PIC S9(7)           COMP-3.                  
003700*                                 UTREDNINGSSALDO                         
003800        05 KVSPANT           PIC S9(7)           COMP-3.                  
003900*                                 SPÄRRAT ANTAL                           
004000        05 KVSLAGER          PIC S9(7)           COMP-3.                  
004100*                                 SÄKERHETSLAGER                          
004200        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
004300*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
004400        05 KVAVIS            PIC S9(7)           COMP-3.                  
004500*                                 AVISERAT ANTAL                          
004600        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
004700*                                 SEPARAT PERIODBEHOV                     
004800        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
004900*                                 SATS-PERIODBEHOV                        
005000        05 KDERS             PIC S9(3)           COMP-3.                  
005100*                                 ERSÄTTNINGSKOD                          
005200        05 TIINVDAT          PIC S9(5)           COMP-3.                  
005300*                                 INVENTERINGSDATUM                       
005400        05 TIRODAT           PIC S9(5)           COMP-3.                  
005500*                                 RESTORDERDATUM      TIRODAT-002         
005600*                                 (AAVVD)                                 
005700        05 TIPBDAT           PIC S9(7)           COMP-3.                  
005800*                                 PROGNOSDATUM ÅÅMMDD TIPBDAT-002         
005900     03 GEMENSAM-INFO.                                                    
006000        05 KDPROD            PIC S9(3)           COMP-3.                  
006100*                                 PRODUKTIONSKOD                          
006200        05 KDHF              PIC S9              COMP-3.                  
006300*                                 HUVUDFÖRRÅDSMÄRKNING                    
006400        05 KDGK              PIC S9              COMP-3.                  
006500*                                 GODSMOTTAGAREKOD                        
006600        05 KDPRODSL          PIC S9(3)           COMP-3.                  
006700*                                 PRODUKTSLAG                             
006800        05 TIFINLV           PIC S9(5)           COMP-3.                  
006900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
007000        05 KDLTK             PIC S9              COMP-3.                  
007100*                                 LAGERTILLHÖRIGHETSKOD                   
007200        05 FLLTKSP           PIC X.                                       
007300*                                 SPÄRR UTLEVERANS C2-LAGER               
007400        05 KDLEVSP           PIC S9(3)           COMP-3.                  
007500*                                 SPÄRRKOD LEVERANS                       
007600        05 FLLSRDEL          PIC X.                                       
007700*                                 LEVERERAS SOM RESDEL                    
007800        05 KDUART            PIC X.                                       
007900*                                 UNDANTAGSARTIKEL                        
008000        05 BEART-SVE         PIC X(25).                                   
008100*                                 SVENSK ARTIKELBENÄMNING                 
008200        05 IDANSK            PIC S9(3)           COMP-3.                  
008300*                                 ANSKAFFARNUMMER                         
008400        05 IDLEVNR           PIC X(5).                                    
008500*                                 LEVERANTÖRNUMMER                        
008600        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
008700*                                 FUNKTIONSGRUPP                          
008800        05 IDLKTO            PIC S9(7)           COMP-3.                  
008900*                                 LAGERKONTO (FFHHHUU)                    
009000        05 KDVVKL            PIC S9              COMP-3.                  
009100*                                 VOLYMVÄRDESKLASS                        
009200        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
009300*                                 ARTIKELSTANDARDPRIS                     
009400        05 IDPROJ            PIC X(4).                                    
009500*                                 PARTS PROJEKTIDENTITET                  
009600*** END OF VILMAII-COPY LENGTH= 146 BYTES                                 
