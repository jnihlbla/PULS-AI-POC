000100 01  W271ERS-W271ERS.                                                     
000200*                                 LÄNKCOPYTEXT MELLAN W27110 OCH          
000300*                                 W271ERS                                 
000400*                                 SUBPROGRAMMET W271ERS ANVÄNDS           
000500*                                 FÖR ATT BEHANDLA REFILL FÖR AR-         
000600*                                 TIKLAR MED ERSÄTTNINGSKOD 01-09         
000700*                                                                         
000800*                                 KDSVAR FRÅN SUBPROGRAMMET ANGER         
000900*                                 1 *KVBEART-IN-MAINPGM*                  
001000*                                     HUVUDPROGRAMMET SKALL BERÄ-         
001100*                                     NA KVBEART                          
001200*                                                                         
001300*                                 2 *KVBEART-FROM-SUBPGM*                 
001400*                                     SUBPROGRAMMET HAR BERÄKNAT          
001500*                                     KVBEART                             
001600*                                                                         
001700*                                 3 *NO-ORDER*                            
001800*                                     ARTIKELN SKALL EJ BEHANDLAS         
001900*                                     VIDARE                              
002000*                                                                         
002100*                                                                         
002200     03 W271ERS-IDARTNR      PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500     03 W271ERS-AKTUELLT-IDDC                                             
002600                             PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800*                                 WAREHOUSE IDENTIFIER                    
002900     03 W271ERS-W271ERS-CDC-INFO.                                         
003000*                                 CDC-UPPGIFTER FÖR ERSATTA               
003100*                                 ARTIKLAR SOM SKALL REFILL-              
003200*                                 BEVAKAS                                 
003300        05 W271ERS-CLAG-KDERS                                             
003400                             PIC S9(3)           COMP-3.                  
003500*                                 ERSÄTTNINGSKOD                          
003600*                                 SUPERSESSION CODE                       
003700        05 W271ERS-CLAG-KVLS PIC S9(7)           COMP-3.                  
003800*                                 LAGERSALDO                              
003900*                                 STOCK BALANCE                           
004000        05 W271ERS-CLAG-KVRESS                                            
004100                             PIC S9(7)           COMP-3.                  
004200*                                 RESERVERAT ANTAL ARTIKLAR               
004300*                                 QUANTITY RESERVED ITEMS                 
004400        05 W271ERS-CLAG-KVSPANT                                           
004500                             PIC S9(7)           COMP-3.                  
004600*                                 SPÄRRAT ANTAL                           
004700*                                 BLOCKED QTY                             
004800        05 W271ERS-CLAG-KVAKS-CDC                                         
004900                             PIC S9(7)           COMP-3.                  
005000*                                 DEL AV AK SOM LIGGER I CDC              
005100*                                 PART OF AK IN THE CDC                   
005200        05 W271ERS-CLAG-KVAKS-PAV                                         
005300                             PIC S9(7)           COMP-3.                  
005400*                                 DEL AV AK PÅ VÄG                        
005500*                                 PART OF AK ON ITS WAY                   
005600        05 W271ERS-CLAG-KVAKS-T                                           
005700                             PIC S9(7)           COMP-3.                  
005800*                                 DEL AV AK I EN TERMINAL                 
005900*                                 PART OF AK IN A TERMINAL                
006000        05 W271ERS-CLAG-KVROS                                             
006100                             PIC S9(7)           COMP-3.                  
006200*                                 RESTORDERSALDO                          
006300*                                 BACKORDER QTY                           
006400        05 W271ERS-CLAG-KVPB-SEP                                          
006500                             PIC S9(6)V9(1)      COMP-3.                  
006600*                                 SEPARAT PERIODBEHOV                     
006700*                                 SEPARATE PERIOD REQUIREMENTS            
006800        05 W271ERS-CLAG-KVPB-SATS                                         
006900                             PIC S9(6)V9(1)      COMP-3.                  
007000*                                 SATS-PERIODBEHOV                        
007100*                                 KIT PERIOD REQUIREMENTS                 
007200        05 W271ERS-CLAG-KVPB-TPO                                          
007300                             PIC S9(6)V9(1)      COMP-3.                  
007400*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
007500*                                 PERIODICAL DEMAND TPO1 AND TPO2         
007600*                                                                         
007700        05 W271ERS-CLAG-KVQPACK-1                                         
007800                             PIC S9(5)           COMP-3.                  
007900*                                 ANTAL I Q1 FÖRPACKNING                  
008000*                                 QUANTITY IN BULK PACK Q1                
008100        05 W271ERS-CLAG-PRARTSTD                                          
008200                             PIC S9(7)V9(2)      COMP-3.                  
008300*                                 ARTIKELSTANDARDPRIS                     
008400*                                 STANDARD PRICE                          
008500        05 W271ERS-CLAG-REDIRLEV                                          
008600                             PIC S9V9(2)         COMP-3.                  
008700*                                 DIREKTLEVERANSANDEL                     
008800        05 W271ERS-ERSA-TIERSDAT-PREL-C1                                  
008900                             PIC S9(5)           COMP-3.                  
009000*                                 PREL ERSÄTTNINGSDATUM C1  ÅÅVVD         
009100        05 W271ERS-SUM-KVBR  PIC S9(7)           COMP-3.                  
009200*                                 BESTÄLLNINGSREST                        
009300*                                 ON ORDER BALANCE                        
009400        05 W271ERS-ARTM-KVOKS-BULK                                        
009500                             PIC S9(7)           COMP-3.                  
009600*                                 ORDERKÖSALDO, KLASS 2-4                 
009700*                                 ORDER QUEUE BALANCE, CLASS 2-4          
009800        05 W271ERS-ARTM-KVOKS-DAG                                         
009900                             PIC S9(7)           COMP-3.                  
010000*                                 ORDERKÖSALDO, KLASS 1                   
010100*                                 ORDER QUEUE BALANCE, CLASS 1            
010200        05 W271ERS-ARTM-KVOKS-VOR                                         
010300                             PIC S9(7)           COMP-3.                  
010400*                                 ORDERKÖSALDO, VOR                       
010500*                                 ORDER QUEUE BALANCE, VOR                
010600        05 W271ERS-SUM-KVAVROP                                            
010700                             PIC S9(7)           COMP-3.                  
010800*                                 AVROPSKVANTITET                         
010900        05 W271ERS-ART-TIERSDAT                                           
011000                             PIC S9(5)           COMP-3.                  
011100*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
011200*                                 DATE OF SUPERSESSION (YYWWD)            
011300        05 W271ERS-ART-FLIART                                             
011400                             PIC X.                                       
011500*                                 ARTIKELN INGÅR I SATS                   
011600*                                 PART IN KIT                             
011700        05 W271ERS-REFILL-CDC                                             
011800                             PIC X.                                       
011900     03 W271ERS-W271ERS-SDC-INFO.                                         
012000*                                 UPPGIFTER OM ARTIKLAR MED ERS-          
012100*                                 KOD, AKTUELLT SDC                       
012200        05 W271ERS-SLAG-KVLS PIC S9(7)           COMP-3.                  
012300*                                 LAGERSALDO                              
012400*                                 STOCK BALANCE                           
012500        05 W271ERS-SLAG-KVAKS-SDC                                         
012600                             PIC S9(7)           COMP-3.                  
012700*                                 DEL AV AK SOM LIGGER I SDC              
012800*                                 PART OF AK IN THE SDC                   
012900        05 W271ERS-SLAG-KVAKS-PAV                                         
013000                             PIC S9(7)           COMP-3.                  
013100*                                 DEL AV AK PÅ VÄG                        
013200*                                 PART OF AK ON ITS WAY                   
013300        05 W271ERS-SLAG-KVBEART                                           
013400                             PIC S9(7)           COMP-3.                  
013500*                                 BESTÄLLT ANTAL STYCKEN                  
013600*                                 ORDERED QUANTITY                        
013700        05 W271ERS-SLAG-KVOKS-DAG                                         
013800                             PIC S9(7)           COMP-3.                  
013900*                                 ORDERKÖSALDO, KLASS 1                   
014000*                                 ORDER QUEUE BALANCE, CLASS 1            
014100        05 W271ERS-SLAG-KVOKS-BULK                                        
014200                             PIC S9(7)           COMP-3.                  
014300*                                 ORDERKÖSALDO, KLASS 2-4                 
014400*                                 ORDER QUEUE BALANCE, CLASS 2-4          
014500        05 W271ERS-SLAG-KVPB-REF                                          
014600                             PIC S9(6)V9(1)      COMP-3.                  
014700*                                 PERIODBEHOV REFILLING                   
014800*                                 FORECAST REFILLING                      
014900        05 W271ERS-SLAG-KVREFPKT                                          
015000                             PIC S9(7)           COMP-3.                  
015100*                                 BERÄKNAD PÅFYLLNADSPUNKT                
015200*                                 CALCULATED REFILLING POINT              
015300        05 W271ERS-SLAG-KVROS-DAG                                         
015400                             PIC S9(7)           COMP-3.                  
015500*                                 RESTORDERSALDO, KLASS 1                 
015600*                                 BACK ORDER BALANCE, CLASS 1             
015700        05 W271ERS-SLAG-KVROS-BULK                                        
015800                             PIC S9(7)           COMP-3.                  
015900*                                 RESTORDERSALDO, KLASS 2-4               
016000*                                 BACK ORDER BALANCE, CLASS 2-4           
016100        05 W271ERS-SLAG-KVSPARR-KVAL                                      
016200                             PIC S9(7)           COMP-3.                  
016300*                                 SPÄRRAT ANTAL KVALITETSFEL              
016400*                                 BLOCKED QUANTITY QUALITY ERROR          
016500        05 W271ERS-SLAG-KVREFBER                                          
016600                             PIC S9(7)           COMP-3.                  
016700*                                 BERÄKNAD REFILLINGKVANTITET             
016800*                                 CALCULATED REFILLING QUANTITY           
016900        05 W271ERS-SLAG-FLREFBEO                                          
017000                             PIC X.                                       
017100*                                 AUTOMATISK REFILL BEORDRING?            
017200*                                 AUTOMATIC REFILL ORDERING?              
017300        05 W271ERS-SLAG-FLPB-FLYTT                                        
017400                             PIC X.                                       
017500*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
017600*                                  REDA PÅ KOPIERING AV PROGNOS           
017700*                                 FLAG                                    
017800     03 W271ERS-W271ERS-RESULTAT.                                         
017900*                                 RESULTAT FRÅN REFILLBEVAKNING           
018000*                                 AV ERSATTA ARTIKLAR                     
018100        05 W271ERS-ANTAL     PIC S9(7)           COMP-3.                  
018200*                                 BESTÄLLT ANTAL STYCKEN                  
018300*                                 ORDERED QUANTITY                        
018400        05 W271ERS-KDREFTYP  PIC X.                                       
018500*                                 TYP AV REFILLORDER                      
018600*                                 TYPE OF REFILLINGORDER                  
018700        05 W271ERS-KDREFSTA  PIC X.                                       
018800*                                 STATUS REFILLARTIKEL                    
018900*                                 STATUS REFILLPART                       
019000        05 W271ERS-KDREFORS  PIC X.                                       
019100*                                 REFILL ORDER STATUSKOD                  
019200*                                 REFILL ORDER STATUS CODE                
019300        05 W271ERS-FLREFBEO  PIC X.                                       
019400*                                 AUTOMATISK REFILL BEORDRING?            
019500*                                 AUTOMATIC REFILL ORDERING?              
019600        05 W271ERS-FLPB-FLYTT                                             
019700                             PIC X.                                       
019800*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
019900*                                  REDA PÅ KOPIERING AV PROGNOS           
020000*                                 FLAG                                    
020100        05 W271ERS-KDSVAR    PIC X.                                       
020200         88 W271ERS-MAINPGM  VALUE '1'.                                   
020300         88 W271ERS-ORDER    VALUE '2'.                                   
020400         88 W271ERS-NO-ORDER VALUE '3'.                                   
020500*                                 SVAR FRÅN SUBPROGRAM                    
020600*** END OF VILMAII-COPY LENGTH= 147 BYTES                                 
