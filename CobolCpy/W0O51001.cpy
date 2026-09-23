000100 01  W0O51001.                                                            
000200*                                 COPYTEXT FÖR MOD W0051100               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDDC-IN              PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 IDARTNR-UT           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 IDDC-UT              PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 IDDC-ENTER           PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 IDDC-NEXT            PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 CDC-INFO.                                                         
002000        05 KVLS-CDC          PIC -(6)9.                                   
002100*                                 LAGERSALDO                              
002200        05 KVDISP-CDC        PIC -(6)9.                                   
002300*                                 DISPONIBELT LAGER                       
002400        05 KVOKS-CDC         PIC -(6)9.                                   
002500*                                 ORDERKÖSALDO                            
002600        05 KVROS-CDC         PIC -(6)9.                                   
002700*                                 RESTORDERSALDO                          
002800        05 KVRESS-CDC        PIC -(6)9.                                   
002900*                                 RESERVERAT ANTAL ARTIKLAR               
003000        05 KVEFRS-CDC        PIC -(6)9.                                   
003100*                                 EJ FAKTURERAT ANTAL STYCK               
003200        05 KVSPANT           PIC -(6)9.                                   
003300*                                 SPÄRRAT ANTAL                           
003400        05 KVSPARR-KVAL-CDC  PIC Z(6)9.                                   
003500*                                 SPÄRRAT ANTAL KVALITETSFEL              
003600        05 KVUTRS-CDC        PIC -(6)9.                                   
003700*                                 UTREDNINGSSALDO                         
003800        05 KVAKS-CDC         PIC Z(6)9.                                   
003900*                                 DEL AV AK SOM LIGGER I CDC              
004000        05 KVAKS-PAV-CDC     PIC Z(6)9.                                   
004100*                                 DEL AV AK PÅ VÄG                        
004200     03 WKD9-AREA.                                                        
004300        05 KVOKS-VOR         PIC -(6)9.                                   
004400*                                 ORDERKÖSALDO, VOR                       
004500        05 KVOKS-DAG         PIC -(6)9.                                   
004600*                                 ORDERKÖSALDO, KLASS 1                   
004700        05 KVOKS-BULK        PIC -(6)9.                                   
004800*                                 ORDERKÖSALDO, KLASS 2-4                 
004900        05 KVPREAVB-VOR      PIC -(6)9.                                   
005000*                                 PREL-AVB KVANT, VOR                     
005100        05 KVPREAVB-DAG      PIC -(6)9.                                   
005200*                                 PREL-AVB KVANT, KLASS 1                 
005300        05 KVPREAVB-BULK     PIC -(6)9.                                   
005400*                                 PREL-AVB KVANT, KLASS 2-4               
005500        05 KVPRERO-DAG       PIC -(6)9.                                   
005600*                                 PRELIMINÄR RO-KVANT, KLASS 1            
005700        05 KVPRERO-BULK      PIC -(6)9.                                   
005800*                                 PRELIMINÄR RO-KVANT, KLASS 2-4          
005900        05 RERF-ART          PIC 9.9(3).                                  
006000*                                 RANSONERINGSFAKTOR ARTIKEL              
006100        05 SUTPO-TOT         PIC -(6)9.                                   
006200*                                 TPO-KVANTITET, TOTAL                    
006300        05 KVOFFERT          PIC Z(5)9.                                   
006400*                                 OFFERTSALDO                             
006500     03 SDC-INFO             OCCURS 12 TIMES.                             
006600        05 IDDC-SDC          PIC X(2).                                    
006700*                                 IDENTIFIERARE LAGER                     
006800        05 KVLS-SDC          PIC -(6)9.                                   
006900*                                 LAGERSALDO                              
007000        05 KVDISP-SDC        PIC -(6)9.                                   
007100*                                 DISPONIBELT LAGER                       
007200        05 KVOKS-DAG-SDC     PIC -(6)9.                                   
007300*                                 ORDERKÖSALDO, KLASS 1                   
007400        05 KVOKS-BULK-SDC    PIC -(6)9.                                   
007500*                                 ORDERKÖSALDO, KLASS 2-4                 
007600        05 KVROS-DAG         PIC -(6)9.                                   
007700*                                 RESTORDERSALDO, KLASS 1                 
007800        05 KVROS-BULK        PIC -(6)9.                                   
007900*                                 RESTORDERSALDO, KLASS 2-4               
008000        05 KVRESS-SDC        PIC -(6)9.                                   
008100*                                 RESERVERAT ANTAL ARTIKLAR               
008200        05 KVEFRS-SDC        PIC -(6)9.                                   
008300*                                 EJ FAKTURERAT ANTAL STYCK               
008400        05 KVSPARR-KVAL-SDC  PIC Z(6)9.                                   
008500*                                 SPÄRRAT ANTAL KVALITETSFEL              
008600        05 KVUTRS-SDC        PIC -(6)9.                                   
008700*                                 UTREDNINGSSALDO                         
008800        05 KVAKS-SDC         PIC Z(6)9.                                   
008900*                                 DEL AV AK SOM LIGGER I SDC              
009000     03 RUBKOL04             PIC X(5).                                    
009100     03 RUBKOL05             PIC X(5).                                    
009200     03 TEMFSINF             PIC X(55).                                   
009300*                                 INFORMATIONSMEDDELANDE                  
009400*** END OF VILMAII-COPY LENGTH= 1234 BYTES                                
