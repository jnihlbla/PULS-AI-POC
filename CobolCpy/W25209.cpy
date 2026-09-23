000100 01  W25209-CTX.                                                          
000200*                                 ÄT-TOT LISTA                            
000300     03 BEART-ENG            PIC X(25).                                   
000400*                                 ENGELSK ARTIKELBENÄMNING                
000500     03 BEFT                 PIC S9(3)           COMP-3.                  
000600*                                 FÖRPACKNINGSTYP                         
000700     03 BEUPPDSU             PIC X(35).                                   
000800*                                 SU-UPPDRAG BENÄMNING                    
000900     03 DADATUM              PIC X(10).                                   
001000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001100     03 DAPUBL-CN            PIC 9(8).                                    
001200*                                 PUBLICERINGSDATUM PER ART/LAND          
001300     03 DAPUBL-US            PIC 9(8).                                    
001400*                                 PUBLICERINGSDATUM PER ART/LAND          
001500     03 FLERS                PIC X.                                       
001600*                                 TILLKOMMANDE ARTIKEL ?                  
001700     03 FLPISK               PIC X.                                       
001800*                                 PISK ARTIKEL                            
001900     03 FLUPG                PIC X.                                       
002000*                                 FLAGGA UTFALLSPROV GODKÄNT              
002100     03 IDANSK               PIC S9(3)           COMP-3.                  
002200*                                 ANSKAFFARNUMMER                         
002300     03 IDNAMN-FORP          PIC X(40).                                   
002400*                                 NAMN                                    
002500     03 IDNAMN-IDANSK        PIC X(40).                                   
002600*                                 NAMN                                    
002700     03 IDNAMN-IDINK         PIC X(40).                                   
002800*                                 NAMN                                    
002900     03 IDAO                 PIC X(10).                                   
003000*                                 ÄNDRINGSORDERNUMMER                     
003100     03 IDARTNR              PIC S9(9)           COMP-3.                  
003200*                                 ARTIKELNUMMER                           
003300     03 IDARTNR-EMBQ0        PIC S9(9)           COMP-3.                  
003400*                                 EMBALLAGEARTIKELNR FÖR Q0               
003500     03 IDARTNR-TILLK        PIC S9(9)           COMP-3.                  
003600*                                 TILLKOMMANDE ARTIKELNUMMER              
003700     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
003800*                                 FUNKTIONSGRUPP                          
003900     03 IDLEVNR              PIC X(5).                                    
004000*                                 LEVERANTÖRNUMMER                        
004100     03 IDINK                PIC X(4).                                    
004200*                                 INKÖPARNUMMER                           
004300     03 IDPROJ               PIC X(4).                                    
004400*                                 PARTS PROJEKTIDENTITET                  
004500     03 IDPROJUP             PIC X(8).                                    
004600*                                 PROJEKTUPPDRAG                          
004700     03 IDUPPDKU             PIC X(8).                                    
004800*                                 KU-UPPDRAGSNUMMER TIKO                  
004900     03 IDUPPDSU             PIC X(8).                                    
005000*                                 SU-UPPDRAGSNUMMER TIKO                  
005100     03 KDARTURS             PIC X(2).                                    
005200*                                 ARTIKELURSPRUNGSKOD                     
005300     03 KDAVT                PIC S9              COMP-3.                  
005400*                                 AVTALSMÄRKNING                          
005500     03 KDEMBKOD-2           PIC S9(3)           COMP-3.                  
005600*                                 EMBALLAGEKOD 2                          
005700     03 KDERS                PIC S9(3)           COMP-3.                  
005800*                                 ERSÄTTNINGSKOD                          
005900     03 KDFARLIG             PIC S9              COMP-3.                  
006000*                                 KOD FÖR FARLIGT GODS                    
006100     03 KDFPKPRI             PIC X.                                       
006200*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
006300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006400*                                 PRODUKTSLAG                             
006500     03 KDTPD                PIC X.                                       
006600*                                 KOD TILLFÄLLIGT UTFALLSPROV             
006700     03 KDUART               PIC X.                                       
006800*                                 UNDANTAGSARTIKEL                        
006900     03 KVOKS-BULK           PIC S9(7)           COMP-3.                  
007000*                                 ORDERKÖSALDO, KLASS 2-4                 
007100     03 KVOKS-DAG            PIC S9(7)           COMP-3.                  
007200*                                 ORDERKÖSALDO, KLASS 1                   
007300     03 KVOKS-VOR            PIC S9(7)           COMP-3.                  
007400*                                 ORDERKÖSALDO, VOR                       
007500     03 KVAKS-CDC            PIC S9(7)           COMP-3.                  
007600*                                 DEL AV AK SOM LIGGER I CDC              
007700     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
007800*                                 DEL AV AK PÅ VÄG                        
007900     03 KVAKS-T              PIC S9(7)           COMP-3.                  
008000*                                 DEL AV AK I EN TERMINAL                 
008100     03 KVLS                 PIC S9(7)           COMP-3.                  
008200*                                 LAGERSALDO                              
008300     03 KVLS-EMBQ0           PIC S9(7)           COMP-3.                  
008400*                                 LAGERSALDO                              
008500     03 KVLS-SS              PIC S9(7)           COMP-3.                  
008600*                                 LAGERSALDO                              
008700     03 KVRADER-CDC          PIC S9(5)           COMP-3.                  
008800*                                 ANTAL RADER                             
008900     03 KVRADER-SLAG         PIC S9(5)           COMP-3.                  
009000*                                 ANTAL RADER                             
009100     03 KVRESS               PIC S9(7)           COMP-3.                  
009200*                                 RESERVERAT ANTAL ARTIKLAR               
009300     03 KVRESS-EMBQ0         PIC S9(7)           COMP-3.                  
009400*                                 RESERVERAT ANTAL ARTIKLAR               
009500     03 KVRESS-SS            PIC S9(7)           COMP-3.                  
009600*                                 RESERVERAT ANTAL ARTIKLAR               
009700     03 KVROS                PIC S9(7)           COMP-3.                  
009800*                                 RESTORDERSALDO                          
009900     03 KVROS-EMBQ0          PIC S9(7)           COMP-3.                  
010000*                                 RESTORDERSALDO                          
010100     03 KVROS-SS             PIC S9(7)           COMP-3.                  
010200*                                 RESTORDERSALDO                          
010300     03 KVVORKO              PIC S9(7)           COMP-3.                  
010400*                                 VOR-KÖ KVANT                            
010500     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
010600*                                 ARTIKELSTANDARDPRIS                     
010700     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
010800*                                 DIREKTLEVERANSANDEL                     
010900     03 SUTPO-TOT            PIC S9(7)           COMP-3.                  
011000*                                 TPO-KVANTITET, TOTAL                    
011100     03 TELEVBSK             PIC X(80).                                   
011200*                                 LEVERANSBESKEDSINFORMATION              
011300     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
011400*                                 AVISERINGSDATUM (YYMMDD)                
011500     03 TIAVTAL              PIC S9(7)           COMP-3.                  
011600*                                 AVTALSDATUM  (ÅÅMMDD)                   
011700     03 TIDISPIN             PIC S9(7)           COMP-3.                  
011800*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
011900     03 TIFINLV              PIC S9(5)           COMP-3.                  
012000*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
012100     03 TILEVBSK-DISP        PIC S9(7)           COMP-3.                  
012200*                                 LEV. BESK. DISPONIBEL(ÅÅMMDD)           
012300     03 TIMOTSI              PIC S9(7)           COMP-3.                  
012400*                                 DATUM KVITTO KÖPANMODAN FR SI+          
012500     03 TIREGDAT             PIC S9(7)           COMP-3.                  
012600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
012700     03 TISOP                PIC S9(5)           COMP-3.                  
012800*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
012900     03 TIUPPDAT-BTO         PIC S9(7)           COMP-3.                  
013000*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
013100     03 TIUPPDAT-EMB         PIC S9(7)           COMP-3.                  
013200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
013300     03 W25209-001-GRP       OCCURS 6 TIMES.                              
013400*                                                                         
013500*                                                                         
013600        05 DAPRLIST-SLAG     PIC 9(8).                                    
013700*                                 PRISLISTEDATUM (AAAAMMDD)               
013800        05 IDANSK-SLAG       PIC S9(3)           COMP-3.                  
013900*                                 ANSKAFFARNUMMER                         
014000        05 IDARTNR-EMBQ0-SLAG                                             
014100                             PIC S9(9)           COMP-3.                  
014200*                                 EMBALLAGEARTIKELNR FÖR Q0               
014300        05 IDDC-SLAG         PIC X(2).                                    
014400*                                 IDENTIFIERARE LAGER                     
014500        05 IDINK-SLAG        PIC X(4).                                    
014600*                                 INKÖPARNUMMER                           
014700        05 IDLEVNR-SLAG      PIC X(5).                                    
014800*                                 LEVERANTÖRNUMMER                        
014900        05 IDNAMN-IDANSK-SLAG                                             
015000                             PIC X(40).                                   
015100*                                 NAMN                                    
015200        05 IDNAMN-IDINK-SLAG PIC X(40).                                   
015300*                                 NAMN                                    
015400        05 KDAVT-SLAG        PIC S9              COMP-3.                  
015500*                                 AVTALSMÄRKNING                          
015600        05 KDARTURS-SLAG     PIC X(2).                                    
015700*                                 ARTIKELURSPRUNGSKOD                     
015800        05 KDMATRPR-SLAG     PIC X.                                       
015900*                                 KOD OM MAT.PRIS TIPPAT/KÖP              
016000        05 KVAKS-SDC-SLAG    PIC S9(7)           COMP-3.                  
016100*                                 DEL AV AK SOM LIGGER I SDC              
016200        05 KVAKS-SDC-EMBQ0-SLAG                                           
016300                             PIC S9(7)           COMP-3.                  
016400*                                 DEL AV AK SOM LIGGER I SDC              
016500        05 KVAKS-SDC-SS-SLAG PIC S9(7)           COMP-3.                  
016600*                                 DEL AV AK SOM LIGGER I SDC              
016700        05 KVBEART-SLAG      PIC S9(7)           COMP-3.                  
016800*                                 BESTÄLLT ANTAL STYCKEN                  
016900        05 KVLS-SLAG         PIC S9(7)           COMP-3.                  
017000*                                 LAGERSALDO                              
017100        05 KVLS-EMBQ0-SLAG   PIC S9(7)           COMP-3.                  
017200*                                 LAGERSALDO                              
017300        05 KVLS-SS-SLAG      PIC S9(7)           COMP-3.                  
017400*                                 LAGERSALDO                              
017500        05 KVOKS-BULK-SLAG   PIC S9(7)           COMP-3.                  
017600*                                 ORDERKÖSALDO, KLASS 2-4                 
017700        05 KVOKS-BULK-EMBQ0-SLAG                                          
017800                             PIC S9(7)           COMP-3.                  
017900*                                 ORDERKÖSALDO, KLASS 2-4                 
018000        05 KVOKS-BULK-SS-SLAG                                             
018100                             PIC S9(7)           COMP-3.                  
018200*                                 ORDERKÖSALDO, KLASS 2-4                 
018300        05 KVOKS-DAG-SLAG    PIC S9(7)           COMP-3.                  
018400*                                 ORDERKÖSALDO, KLASS 1                   
018500        05 KVOKS-DAG-EMBQ0-SLAG                                           
018600                             PIC S9(7)           COMP-3.                  
018700*                                 ORDERKÖSALDO, KLASS 1                   
018800        05 KVOKS-DAG-SS-SLAG PIC S9(7)           COMP-3.                  
018900*                                 ORDERKÖSALDO, KLASS 1                   
019000        05 KVRESS-SLAG       PIC S9(7)           COMP-3.                  
019100*                                 RESERVERAT ANTAL ARTIKLAR               
019200        05 KVRESS-EMBQ0-SLAG PIC S9(7)           COMP-3.                  
019300*                                 RESERVERAT ANTAL ARTIKLAR               
019400        05 KVRESS-SS-SLAG    PIC S9(7)           COMP-3.                  
019500*                                 RESERVERAT ANTAL ARTIKLAR               
019600        05 KVROS-BULK-SLAG   PIC S9(7)           COMP-3.                  
019700*                                 RESTORDERSALDO, KLASS 2-4               
019800        05 KVROS-BULK-EMBQ0-SLAG                                          
019900                             PIC S9(7)           COMP-3.                  
020000*                                 RESTORDERSALDO, KLASS 2-4               
020100        05 KVROS-BULK-SS-SLAG                                             
020200                             PIC S9(7)           COMP-3.                  
020300*                                 RESTORDERSALDO, KLASS 2-4               
020400        05 KVROS-DAG-SLAG    PIC S9(7)           COMP-3.                  
020500*                                 RESTORDERSALDO, KLASS 1                 
020600        05 KVROS-DAG-EMBQ0-SLAG                                           
020700                             PIC S9(7)           COMP-3.                  
020800*                                 RESTORDERSALDO, KLASS 1                 
020900        05 KVROS-DAG-SS-SLAG PIC S9(7)           COMP-3.                  
021000*                                 RESTORDERSALDO, KLASS 1                 
021100        05 TELEVBSK-SLAG     PIC X(80).                                   
021200*                                 LEVERANSBESKEDSINFORMATION              
021300        05 TIAVTAL-SLAG      PIC S9(7)           COMP-3.                  
021400*                                 AVTALSDATUM  (ÅÅMMDD)                   
021500        05 TIBERANK-SLAG     PIC 9(6).                                    
021600*                                 BERÄKNAD ANKOMSTDATUM                   
021700        05 TIINLMOT-SLAG     PIC S9(7)           COMP-3.                  
021800*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
021900        05 TIMOTSI-SLAG      PIC S9(7)           COMP-3.                  
022000*                                 DATUM KVITTO KÖPANMODAN FR SI+          
022100        05 TILEVBSK-DISP-SLAG                                             
022200                             PIC S9(7)           COMP-3.                  
022300*                                 LEV. BESK. DISPONIBEL(ÅÅMMDD)           
022400*** END OF VILMAII-COPY LENGTH= 2290 BYTES                                
