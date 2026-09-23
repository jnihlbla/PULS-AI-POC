000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2317800.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   94/08/16.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SKAPAR UTFIL W23178 AV INFILER.                       
001000*        EN POST/ ARTIKEL SKAPAS MED                                      
001100*        -  GRUNDUPPGIFTER FÖR BERÄKNINGAR FÖR ANALYS-LISTOR              
001200*        -  KATEGORI, PRISKLASS OCH FREKVENSKLASS SÄTTS                   
001300*        -  SUMMERINGAR AV KVOKS (BULK+DAG+VOR)                           
001400*                                ORDERKÖSALDO                             
001500*                          KVOI CDC (DIVERSE+SATS+PROGNOSPAV)             
001600*                                ORDERINGÅNG                              
001700*                          KVINORD CDC (ORDERINGÅNG RADER)                
001800*                          KVAVBRAD CDC (AVBOKADE ORDERRADER)             
001900*                          KVFYSAVV CDC (FYSIKS AVVIK RADER)              
002000*        -  UTRÄKNING AV SNITT-DISP-LAGER/DAG I VECKAN.                   
002100*                                                                         
002200*        SAMTLIGA ARTIKLAR VLB MED KDERS < 20 BEHANDLAS.                  
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- VLB                                                        
003200     SELECT VLB                        ASSIGN TO W23178D1.                
003300     SKIP2                                                                
003400*          --- ARTIKELUPPGIFTER                                           
003500     SELECT W91042                     ASSIGN TO W23178D2.                
003600     SKIP2                                                                
003700*          --- ORDERINGÅNG                                                
003800     SELECT W23123                     ASSIGN TO W23178D3.                
003900     SKIP2                                                                
004000*          --- SERVICEGRAD                                                
004100     SELECT W22509                     ASSIGN TO W23178D4.                
004200     SKIP2                                                                
004300*          --- KVOKS                                                      
004400     SELECT W23177                     ASSIGN TO W23178D5.                
004500     SKIP2                                                                
004600*          --- LAGERSALDO                                                 
004700     SELECT W23180                     ASSIGN TO W23178D6.                
004800     SKIP2                                                                
004900*          --- GRUNDFIL FÖR ANALYS-LISTOR                                 
005000     SELECT W23178                     ASSIGN TO W23178D7.                
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300     SKIP3                                                                
005400 FILE SECTION.                                                            
005500     SKIP3                                                                
005600 FD  VLB                                                                  
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  -COPY W011100      -L.                                               
006100     SKIP3                                                                
006200 FD  W91042                                                               
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500                                                                          
006600*01  -COPY W91042       -L.                                               
006700     SKIP3                                                                
006800 FD  W22509                                                               
006900     RECORDING       F                                                    
007000     BLOCK CONTAINS  0.                                                   
007100                                                                          
007200*01  -COPY W225P233      -L.                                              
007300     SKIP3                                                                
007400 FD  W23123                                                               
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS  0.                                                   
007700                                                                          
007800*01  -COPY W231212    -L.                                                 
007900     SKIP3                                                                
008000 FD  W23177                                                               
008100     RECORDING       F                                                    
008200     BLOCK CONTAINS  0.                                                   
008300                                                                          
008400*01  -COPY W231771A    -L.                                                
008500     SKIP3                                                                
008600 FD  W23180                                                               
008700     RECORDING       F                                                    
008800     BLOCK CONTAINS  0.                                                   
008900                                                                          
009000*01  -COPY W231801A    -L.                                                
009100     SKIP3                                                                
009200 FD  W23178                                                               
009300     RECORDING       F                                                    
009400     BLOCK CONTAINS  0.                                                   
009500                                                                          
009600*01  POST  -COPY W231781A   -PRE UT-  -L.                                 
009700     EJECT                                                                
009800 WORKING-STORAGE SECTION.                                                 
009900                                                                          
010000*    -COPY WY2000W2                                                       
010100     SKIP3                                                                
010200 77  IDPGM                       PIC X(8)    VALUE 'W2317800'.            
010300 77  JA                          PIC X       VALUE 'J'.                   
010400 77  NEJ                         PIC X       VALUE 'N'.                   
010500 77  PER-IX                      PIC S9(3)   VALUE ZERO.                  
010600 77  PER-IX-MAX                  PIC S9(3)   VALUE +12.                   
010700 77  W91042-IX                   PIC S9(3)   VALUE ZERO.                  
010800 77  W91042-IX-MAX               PIC S9(3)   VALUE +6.                    
010900 77  WS-AKTUELL-ART              PIC X       VALUE 'N'.                   
011000 77  WS-KVOKS-TOT                PIC S9(7)   VALUE ZERO COMP-3.           
011100 77  WS-KVDISP                   PIC S9(7)   VALUE ZERO COMP-3.           
011200 77  WS-KVLS                     PIC S9(7)   VALUE ZERO COMP-3.           
011300 77  WS-KVRESS-KVOKS             PIC S9(7)   VALUE ZERO COMP-3.           
011400 77  WS-KVOI                     PIC S9(7)   VALUE ZERO COMP-3.           
011500 77  WS-KVOI-TOT                 PIC S9(7)   VALUE ZERO COMP-3.           
011600 77  WS-KVFYSAVV-TOT             PIC S9(5)V9(2) VALUE ZERO COMP-3.        
011700 77  WS-KVINORD-TOT              PIC S9(7)   VALUE ZERO COMP-3.           
011800 77  WS-KVAVBRAD-TOT             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011900 77  WS-KVPB                     PIC S9(6)V9(1) VALUE ZERO COMP-3.        
012000 77  WS-KVPB-TPO                 PIC S9(6)V9(1) VALUE ZERO COMP-3.        
012100 77  WS-KVPB-TOT                 PIC S9(6)V9(1) VALUE ZERO COMP-3.        
012200 77  WS-FREKVENS                 PIC S9(6)V9(1) VALUE ZERO COMP-3.        
012300                                                                          
012400 01  VLB-ID.                                                              
012500     05  VLB-IDIDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012600 01  W91042-ID.                                                           
012700     05  W91042-IDIDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
012800 01  W22509-ID.                                                           
012900     05  W22509-IDIDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
013000 01  W23123-ID.                                                           
013100     05  W23123-IDIDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
013200 01  W23177-ID.                                                           
013300     05  W23177-IDIDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
013400 01  W23180-ID.                                                           
013500     05  W23180-IDIDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
013600                                                                          
013700 01  KORNINGS-DATUM              PIC 9(6).                                
013800 01  FILLER REDEFINES KORNINGS-DATUM.                                     
013900     03  KORNING-AAA             PIC 9(3).                                
014000     03  KORNING-VV              PIC 9(2).                                
014100     03  KORNING-D               PIC 9(1).                                
014200                                                                          
014300 01  TIFINLV-AAAVVD              PIC 9(6).                                
014400 01  FILLER REDEFINES TIFINLV-AAAVVD.                                     
014500     03  TIFINLV-AAA             PIC 9(3).                                
014600     03  TIFINLV-VV              PIC 9(2).                                
014700     03  TIFINLV-D               PIC 9(1).                                
014800     EJECT                                                                
014900                                                                          
015000*01  -COPY WWPRODSL                                                       
015100                                                                          
015200 01  DYNAMISKA-SUBPROGRAM.                                                
015300*                                                                         
015400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015600     SKIP3                                                                
015700*    --- PARAMETRAR TILL DATKORT                                          
015800*                                                                         
015900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23178'.              
016000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
016100                                                                          
016200*01  -COPY WDATKORT                                                       
016300     EJECT                                                                
016400*    --- PARAMETRAR TILL POSTSUM                                          
016500*                                                                         
016600*01  -COPY W0005   -PRE  POSTSUM-                                         
016700     EJECT                                                                
016800 01  VLB-AREA-START              PIC X(24)   VALUE                        
016900                                 'VLB-AREA-START  '.                      
017000*01  AREA -COPY W011100     -PRE VLB-                                     
017100     EJECT                                                                
017200 01  W91042-AREA-START           PIC X(24)   VALUE                        
017300                                 'W91042-AREA-START'.                     
017400*01  AREA   -COPY W91042     -PRE W91042-                                 
017500     EJECT                                                                
017600 01  W23123-AREA-START           PIC X(24)   VALUE                        
017700                                 'W23123-AREA-START  '.                   
017800                                                                          
017900*01  AREA  -COPY W231212     -PRE W23123-                                 
018000     EJECT                                                                
018100 01  W22509-AREA-START           PIC X(24)   VALUE                        
018200                                 'W22509-AREA-START  '.                   
018300                                                                          
018400*01  AREA -COPY W225P233     -PRE W22509-                                 
018500     EJECT                                                                
018600 01  W23177-AREA-START           PIC X(24)   VALUE                        
018700                                 'W23177-AREA-START  '.                   
018800                                                                          
018900*01  AREA -COPY W231771A  -PRE W23177-                                    
019000     EJECT                                                                
019100 01  W23180-AREA-START           PIC X(24)   VALUE                        
019200                                 'W23180-AREA-START  '.                   
019300                                                                          
019400*01  AREA -COPY W231801A  -PRE W23180-                                    
019500     EJECT                                                                
019600 01  UT-AREA-START               PIC X(24)   VALUE                        
019700                                 'UT-AREA-START  '.                       
019800                                                                          
019900*01  AREA -COPY W231781A  -PRE UT-                                        
020000     EJECT                                                                
020100 PROCEDURE DIVISION.                                                      
020200                                                                          
020300     PERFORM A-INIT                                                       
020400     PERFORM B-SKAPA-GRUNDFIL                                             
020500     PERFORM Z-FINIT                                                      
020600                                                                          
020700     MOVE ZERO TO RETURN-CODE                                             
020800     GOBACK                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 A-INIT SECTION.                                                          
021200                                                                          
021300     OPEN INPUT  VLB                                                      
021400                 W91042                                                   
021500                 W22509                                                   
021600                 W23123                                                   
021700                 W23177                                                   
021800                 W23180                                                   
021900     OPEN OUTPUT W23178                                                   
022000                                                                          
022100     MOVE LOW-VALUE TO VLB-ID                                             
022200                       W91042-ID                                          
022300                       W22509-ID                                          
022400                       W23123-ID                                          
022500                       W23177-ID                                          
022600                       W23180-ID                                          
022700                                                                          
022800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
022900     MOVE D-AAR     TO  KORNING-AAA                                       
023000     MOVE D-VECKA   TO  KORNING-VV                                        
023100     MOVE D-DAG     TO  KORNING-D                                         
023200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023300     .                                                                    
023400     EJECT                                                                
023500 B-SKAPA-GRUNDFIL SECTION.                                                
023600                                                                          
023700     PERFORM S01-LAS-VLB                                                  
023800                                                                          
023900     PERFORM UNTIL VLB-ID = HIGH-VALUE                                    
024000        PERFORM S07-NOLLSTALL-UTAREA                                      
024100        PERFORM BA-BEHANDLA-VLB                                           
024200        MOVE VLB-KDPRODSL   TO TEST-KDPRODSL                              
024300        IF WS-AKTUELL-ART = JA AND KDPRODSL-VOLVO-BIMA                    
024400           PERFORM BB-LAS-W23177                                          
024500           PERFORM BC-LAS-W22509                                          
024600           PERFORM BD-LAS-W23123                                          
024700           PERFORM BE-LAS-W23180                                          
024800           PERFORM BF-LAS-W91042                                          
024900                                                                          
025000           PERFORM S08-SKRIV-W23178                                       
025100        END-IF                                                            
025200        PERFORM S01-LAS-VLB                                               
025300     END-PERFORM                                                          
025400     .                                                                    
025500     EJECT                                                                
025600 BA-BEHANDLA-VLB SECTION.                                                 
025700                                                                          
025800     MOVE NEJ TO WS-AKTUELL-ART                                           
025900     IF VLB-KDERS < 20                                                    
026000        MOVE JA             TO WS-AKTUELL-ART                             
026100        MOVE VLB-FLTOPP     TO UT-FLTOPP                                  
026200        MOVE VLB-IDARTNR    TO UT-IDARTNR                                 
026300        MOVE VLB-IDLEVNR    TO UT-IDLEVNR                                 
026400        MOVE VLB-IDANSK     TO UT-IDANSK                                  
026500*****   COMPUTE UT-KVPB-TOT = VLB-KVPB-SATS + VLB-KVPB-SEP                
026600        MOVE VLB-KDPRODSL   TO UT-KDPRODSL                                
026700        MOVE VLB-KDVVKL     TO UT-KDVVKL                                  
026800        MOVE VLB-KVLS       TO UT-KVLS                                    
026900        MOVE VLB-KVRESS     TO UT-KVRESS                                  
027000        MOVE VLB-KVSLAGER   TO UT-KVSLAGER                                
027100        MOVE VLB-KVQ        TO UT-KVQ                                     
027200        MOVE VLB-PRARTSTD   TO UT-PRARTSTD                                
027300        MOVE VLB-TIFINLV    TO TIFINLV-AAAVVD                             
027400                                                                          
027500        PERFORM BAA-SAETT-KATEGORI                                        
027600*****   PERFORM BAB-SAETT-PRISKLASS                                       
027700     END-IF                                                               
027800     .                                                                    
027900     EJECT                                                                
028000 BAA-SAETT-KATEGORI SECTION.                                              
028100******************************************************************        
028200*   KATEGORI SÄTTS BEROENDE PÅ OM ARTIKEL ÄR                     *        
028300*      - ÄLDRE ÄN 1 ÅR MEN EJ SLUTKÖPT  KATEGORI 1               *        
028400*      - SLUTKÖPT                                2               *        
028500*      - YNGRE ÄN 1 ÅR MEN EJ SLUTKÖPT           3               *        
028600******************************************************************        
028700                                                                          
028800     ADD +1 TO TIFINLV-AAA                                                
028900     MOVE KORNINGS-DATUM   TO TMP1-YYWWD                                  
029000     MOVE TIFINLV-AAAVVD   TO TMP2-YYWWD                                  
029100     PERFORM WY2000P2                                                     
029200     IF TMP1-YYWWD > TMP2-YYWWD                                           
029300        IF VLB-KVSLUTKP = ZERO                                            
029400           MOVE '1'              TO UT-KATEGORI                           
029500        ELSE                                                              
029600           MOVE '2'              TO UT-KATEGORI                           
029700        END-IF                                                            
029800     ELSE                                                                 
029900        IF VLB-KVSLUTKP = ZERO                                            
030000           MOVE '3'              TO UT-KATEGORI                           
030100        ELSE                                                              
030200           MOVE '2'              TO UT-KATEGORI                           
030300        END-IF                                                            
030400     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 BAB-SAETT-PRISKLASS SECTION.                                             
030800******************************************************************        
030900*   PRISKLASS SÄTTS BEROENDE PÅ STANDARDPRIS                     *        
031000******************************************************************        
031100     EVALUATE TRUE                                                        
031200     WHEN VLB-PRARTSTD <= 1.00                                            
031300          MOVE '1'   TO UT-KDPRISKL                                       
031400     WHEN VLB-PRARTSTD > 1.00 AND <= 3.00                                 
031500          MOVE '2'   TO UT-KDPRISKL                                       
031600     WHEN VLB-PRARTSTD > 3.00 AND <= 10.00                                
031700          MOVE '3'   TO UT-KDPRISKL                                       
031800     WHEN VLB-PRARTSTD > 10.00 AND <= 30.00                               
031900          MOVE '4'   TO UT-KDPRISKL                                       
032000     WHEN VLB-PRARTSTD > 30.00 AND <= 100.00                              
032100          MOVE '5'   TO UT-KDPRISKL                                       
032200     WHEN VLB-PRARTSTD > 100.00 AND <= 300.00                             
032300          MOVE '6'   TO UT-KDPRISKL                                       
032400     WHEN VLB-PRARTSTD > 300.00 AND <= 1000.00                            
032500          MOVE '7'   TO UT-KDPRISKL                                       
032600     WHEN VLB-PRARTSTD > 1000.00 AND <= 3000.00                           
032700          MOVE '8'   TO UT-KDPRISKL                                       
032800     WHEN VLB-PRARTSTD > 3000.00                                          
032900          MOVE '9'   TO UT-KDPRISKL                                       
033000     WHEN OTHER                                                           
033100          CONTINUE                                                        
033200     END-EVALUATE                                                         
033300     .                                                                    
033400     EJECT                                                                
033500 BB-LAS-W23177 SECTION.                                                   
033600                                                                          
033700     PERFORM UNTIL W23177-ID NOT <  VLB-ID                                
033800        PERFORM S02-LAS-W23177                                            
033900     END-PERFORM                                                          
034000                                                                          
034100     IF W23177-ID = VLB-ID                                                
034200        MOVE ZERO TO WS-KVOKS-TOT                                         
034300        MOVE W23177-KVOKS-C1 TO WS-KVOKS-TOT                              
034400        MOVE WS-KVOKS-TOT TO UT-KVOKS-TOT                                 
034500                                                                          
034600     END-IF                                                               
034700     .                                                                    
034800     EJECT                                                                
034900 BC-LAS-W22509 SECTION.                                                   
035000                                                                          
035100     PERFORM UNTIL W22509-ID NOT <  VLB-ID                                
035200        PERFORM S04-LAS-W22509                                            
035300     END-PERFORM                                                          
035400                                                                          
035500     IF W22509-ID = VLB-ID                                                
035600                                                                          
035700        MOVE ZERO TO WS-KVAVBRAD-TOT                                      
035800                     WS-KVFYSAVV-TOT                                      
035900                     WS-KVINORD-TOT                                       
036000                                                                          
036100        COMPUTE WS-KVAVBRAD-TOT = W22509-KVAVBRAD-CDC-1-2(2) +            
036200                                  W22509-KVAVBRAD-CDC-3-4(2)              
036300        MOVE WS-KVAVBRAD-TOT TO UT-KVAVBRAD-TOT                           
036400                                                                          
036500        COMPUTE WS-KVFYSAVV-TOT = W22509-KVFYSAVV-CDC-1-2(2) +            
036600                                  W22509-KVFYSAVV-CDC-3-4(2)              
036700        MOVE WS-KVFYSAVV-TOT TO UT-KVFYSAVV-TOT                           
036800                                                                          
036900        COMPUTE WS-KVINORD-TOT  = W22509-KVINORD-CDC-1-2(2) +             
037000                                  W22509-KVINORD-CDC-3-4(2)               
037100        MOVE WS-KVINORD-TOT TO UT-KVINORD-TOT                             
037200     END-IF                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 BD-LAS-W23123 SECTION.                                                   
037600                                                                          
037700     PERFORM UNTIL W23123-ID NOT <  VLB-ID                                
037800        PERFORM S03-LAS-W23123                                            
037900     END-PERFORM                                                          
038000                                                                          
038100     IF W23123-ID = VLB-ID                                                
038200       MOVE ZERO TO WS-KVOI-TOT                                           
038300                    WS-KVOI                                               
038400       MOVE +1 TO PER-IX                                                  
038500       PERFORM UNTIL PER-IX > PER-IX-MAX                                  
038600         COMPUTE WS-KVOI = W23123-KVOI-PROG   (PER-IX) +                  
038700                           W23123-KVOI-DIV    (PER-IX) +                  
038800                           W23123-KVOI-SATS   (PER-IX) +                  
038900                           W23123-KVOI-REFILL (PER-IX)                    
039000         ADD WS-KVOI TO WS-KVOI-TOT                                       
039100         ADD +1 TO PER-IX                                                 
039200       END-PERFORM                                                        
039300       MOVE WS-KVOI-TOT TO UT-KVOI-TOT                                    
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700 BE-LAS-W23180 SECTION.                                                   
039800                                                                          
039900     PERFORM UNTIL W23180-ID NOT <  VLB-ID                                
040000        PERFORM S05-LAS-W23180                                            
040100     END-PERFORM                                                          
040200                                                                          
040300     IF W23180-ID = VLB-ID                                                
040400        MOVE ZERO TO WS-KVLS                                              
040500                     WS-KVRESS-KVOKS                                      
040600                                                                          
040700        COMPUTE WS-KVRESS-KVOKS = W23180-KVRESS-TOT +                     
040800                                  W23180-KVOKS-TOT                        
040900        COMPUTE WS-KVLS = W23180-KVLS-TOT - WS-KVRESS-KVOKS               
041000                                                                          
041100        IF WS-KVLS = ZERO                                                 
041200           MOVE ZERO TO WS-KVDISP                                         
041300        ELSE                                                              
041400           COMPUTE WS-KVDISP = WS-KVLS / W23180-KVANTAL                   
041500        END-IF                                                            
041600                                                                          
041700        MOVE WS-KVDISP TO UT-KVDISP-SNITT-VECKA                           
041800     END-IF                                                               
041900     .                                                                    
042000     EJECT                                                                
042100 BF-LAS-W91042 SECTION.                                                   
042200                                                                          
042300     PERFORM UNTIL W91042-ID NOT <  VLB-ID                                
042400        PERFORM S06-LAS-W91042                                            
042500     END-PERFORM                                                          
042600                                                                          
042700     IF W91042-ID = VLB-ID                                                
042800        MOVE ZERO TO WS-KVPB-TPO                                          
042900        COMPUTE WS-KVPB-TPO = W91042-KVPB-TPO                             
043000        MOVE WS-KVPB-TPO TO UT-KVPB-TPO                                   
043100*****   PERFORM BFA-SAETT-FREKVENSKLASS                                   
043200                                                                          
043300        MOVE ZERO TO WS-KVPB-TOT                                          
043400        MOVE +1 TO W91042-IX                                              
043500        PERFORM UNTIL W91042-IX > W91042-IX-MAX                           
043600           ADD W91042-KVPB-SEP(W91042-IX) TO WS-KVPB-TOT                  
043700           ADD +1 TO W91042-IX                                            
043800        END-PERFORM                                                       
043900        ADD W91042-KVPB-SATS   TO WS-KVPB-TOT                             
044000        MOVE WS-KVPB-TOT       TO UT-KVPB-TOT                             
044100                                                                          
044200        MOVE W91042-KDFREKKL TO UT-KDFREKKL                               
044300        MOVE W91042-KDPRISKL TO UT-KDPRISKL                               
044400     END-IF                                                               
044500     .                                                                    
044600     EJECT                                                                
044700 BFA-SAETT-FREKVENSKLASS SECTION.                                         
044800******************************************************************        
044900*   FREKVENSKLASS SÄTTS BEROENDE PÅ PROGNOSTICERAD               *        
045000*   STYCKFÖRSÄLJNING/ ÅR                                         *        
045100******************************************************************        
045200                                                                          
045300     MOVE ZERO TO WS-KVPB                                                 
045400     COMPUTE WS-KVPB = WS-KVPB-TPO + VLB-KVPB-SATS + VLB-KVPB-SEP         
045500     MULTIPLY WS-KVPB BY 12 GIVING WS-FREKVENS ROUNDED                    
045600                                                                          
045700     EVALUATE TRUE                                                        
045800     WHEN WS-FREKVENS  <= 87.0                                            
045900          MOVE 'A'   TO UT-KDFREKKL                                       
046000     WHEN WS-FREKVENS  > 87.0 AND <= 260.0                                
046100          MOVE 'B'   TO UT-KDFREKKL                                       
046200     WHEN WS-FREKVENS  > 260.0 AND <= 867.0                               
046300          MOVE 'C'   TO UT-KDFREKKL                                       
046400     WHEN WS-FREKVENS  > 867.0 AND <= 2600.0                              
046500          MOVE 'D'   TO UT-KDFREKKL                                       
046600     WHEN WS-FREKVENS  > 2600.0 AND <= 8667.0                             
046700          MOVE 'E'   TO UT-KDFREKKL                                       
046800     WHEN WS-FREKVENS  > 8667.0 AND <= 26000.0                            
046900          MOVE 'F'   TO UT-KDFREKKL                                       
047000     WHEN WS-FREKVENS  > 26000.0                                          
047100          MOVE 'G'   TO UT-KDFREKKL                                       
047200     WHEN OTHER                                                           
047300          CONTINUE                                                        
047400     END-EVALUATE                                                         
047500     .                                                                    
047600     EJECT                                                                
047700 Z-FINIT SECTION.                                                         
047800                                                                          
047900     CLOSE VLB                                                            
048000           W91042                                                         
048100           W22509                                                         
048200           W23123                                                         
048300           W23177                                                         
048400           W23180                                                         
048500           W23178                                                         
048600                                                                          
048700     MOVE 'S' TO POSTSUM-OPKOD                                            
048800     CALL POSTSUM USING POSTSUM-PARM                                      
048900     .                                                                    
049000     EJECT                                                                
049100 S01-LAS-VLB SECTION.                                                     
049200                                                                          
049300     READ VLB INTO VLB-AREA END                                           
049400        MOVE HIGH-VALUE TO VLB-ID                                         
049500     END-READ                                                             
049600                                                                          
049700     IF VLB-ID NOT = HIGH-VALUE                                           
049800        MOVE VLB-IDARTNR TO VLB-IDIDARTNR                                 
049900        MOVE 'VLB'       TO POSTSUM-FDNAMN                                
050000        MOVE 'W23178D1'  TO POSTSUM-DDNAMN2                               
050100        MOVE SPACE       TO POSTSUM-TRANSTYP                              
050200        CALL POSTSUM USING POSTSUM-PARM                                   
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 S02-LAS-W23177 SECTION.                                                  
050700                                                                          
050800     READ W23177 INTO W23177-AREA END                                     
050900        MOVE HIGH-VALUE TO W23177-ID                                      
051000     END-READ                                                             
051100                                                                          
051200     IF W23177-ID NOT = HIGH-VALUE                                        
051300        MOVE W23177-IDARTNR  TO W23177-IDIDARTNR                          
051400        MOVE 'W23177'        TO POSTSUM-FDNAMN                            
051500        MOVE 'W23178D5'      TO POSTSUM-DDNAMN2                           
051600        MOVE SPACE           TO POSTSUM-TRANSTYP                          
051700        CALL POSTSUM USING POSTSUM-PARM                                   
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100 S03-LAS-W23123 SECTION.                                                  
052200                                                                          
052300     READ W23123 INTO W23123-AREA END                                     
052400        MOVE HIGH-VALUE TO W23123-ID                                      
052500     END-READ                                                             
052600                                                                          
052700     IF W23123-ID NOT = HIGH-VALUE                                        
052800        MOVE W23123-IDARTNR   TO W23123-IDIDARTNR                         
052900        MOVE 'W23123'         TO POSTSUM-FDNAMN                           
053000        MOVE 'W23178D3'       TO POSTSUM-DDNAMN2                          
053100        MOVE SPACE            TO POSTSUM-TRANSTYP                         
053200        CALL POSTSUM USING POSTSUM-PARM                                   
053300     END-IF                                                               
053400     .                                                                    
053500     EJECT                                                                
053600 S04-LAS-W22509 SECTION.                                                  
053700                                                                          
053800     READ W22509 INTO W22509-AREA END                                     
053900        MOVE HIGH-VALUE TO W22509-ID                                      
054000     END-READ                                                             
054100                                                                          
054200     IF W22509-ID NOT = HIGH-VALUE                                        
054300        MOVE W22509-IDARTNR TO W22509-IDIDARTNR                           
054400        MOVE 'W22509'       TO POSTSUM-FDNAMN                             
054500        MOVE 'W23178D4'     TO POSTSUM-DDNAMN2                            
054600        MOVE SPACE          TO POSTSUM-TRANSTYP                           
054700        CALL POSTSUM USING POSTSUM-PARM                                   
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100 S05-LAS-W23180 SECTION.                                                  
055200                                                                          
055300     READ W23180 INTO W23180-AREA END                                     
055400        MOVE HIGH-VALUE TO W23180-ID                                      
055500     END-READ                                                             
055600                                                                          
055700     IF W23180-ID NOT = HIGH-VALUE                                        
055800        MOVE W23180-IDARTNR TO W23180-IDIDARTNR                           
055900        MOVE 'W23180'       TO POSTSUM-FDNAMN                             
056000        MOVE 'W23178D6'     TO POSTSUM-DDNAMN2                            
056100        MOVE SPACE          TO POSTSUM-TRANSTYP                           
056200        CALL POSTSUM USING POSTSUM-PARM                                   
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600 S06-LAS-W91042 SECTION.                                                  
056700                                                                          
056800     READ W91042 INTO W91042-AREA END                                     
056900        MOVE HIGH-VALUE TO W91042-ID                                      
057000     END-READ                                                             
057100                                                                          
057200     IF W91042-ID NOT = HIGH-VALUE                                        
057300        MOVE W91042-IDARTNR TO W91042-IDIDARTNR                           
057400        MOVE 'W91042'       TO POSTSUM-FDNAMN                             
057500        MOVE 'W23178D2'     TO POSTSUM-DDNAMN2                            
057600        MOVE SPACE          TO POSTSUM-TRANSTYP                           
057700        CALL POSTSUM USING POSTSUM-PARM                                   
057800     END-IF                                                               
057900     .                                                                    
058000     EJECT                                                                
058100 S07-NOLLSTALL-UTAREA SECTION.                                            
058200                                                                          
058300     MOVE SPACE TO UT-FLTOPP                                              
058400                   UT-KATEGORI                                            
058500                   UT-KDPRISKL                                            
058600                   UT-KDFREKKL                                            
058700     MOVE SPACE TO UT-IDLEVNR                                             
058800     MOVE ZERO  TO UT-IDANSK                                              
058900                   UT-IDARTNR                                             
059000                   UT-KVPB-TOT                                            
059100                   UT-KVPB-TPO                                            
059200                   UT-KDPRODSL                                            
059300                   UT-KDVVKL                                              
059400                   UT-KVDISP-SNITT-VECKA                                  
059500                   UT-KVSLAGER                                            
059600                   UT-KVLS                                                
059700                   UT-KVRESS                                              
059800                   UT-KVOKS-TOT                                           
059900                   UT-KVQ                                                 
060000                   UT-KVINORD-TOT                                         
060100                   UT-KVFYSAVV-TOT                                        
060200                   UT-KVAVBRAD-TOT                                        
060300                   UT-KVOI-TOT                                            
060400                   UT-PRARTSTD                                            
060500     .                                                                    
060600     EJECT                                                                
060700 S08-SKRIV-W23178 SECTION.                                                
060800                                                                          
060900     WRITE UT-POST FROM UT-AREA                                           
061000                                                                          
061100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
061200     MOVE 'W23178'   TO POSTSUM-FDNAMN                                    
061300     MOVE 'W23178D7' TO POSTSUM-DDNAMN2                                   
061400     CALL POSTSUM USING POSTSUM-PARM                                      
061500     .                                                                    
061600     EJECT                                                                
061700     EJECT                                                                
061800*    -COPY WY2000P2                                                       
