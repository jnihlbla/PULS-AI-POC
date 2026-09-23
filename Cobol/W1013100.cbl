000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1013100.                                                
000300*AUTHOR.         BODIL LINDAHL.                                           
000400*DATE-WRITTEN.   MARS 1987.                                               
000500*    FUNKTION.                                                            
000600*                BYTE BENÄMNINGSNUMMER FÖR SAMTLIGA                       
000700*                ARTIKLAR TILL NYTT BENÄMNINGSNUMMER.                     
000800*                                                                         
000900*    ÄNDRINGAR:                                                           
001000*        2003-01-09 UPPDAT AV BENA-BEN-FLAENDR VID FÖRÄNDRING /CE         
001100*                                                                         
001200*    ÄNDRINGAR:                                                           
001300*        2005-10-18 UPPDAT AV BENA-BEN-FLAENDR ENDAST VID                 
001400*                   FÖRÄNDRING SOM BERÖR NEVIS-ARTIKLAR,                  
001500*                   D.V.S. KDPRODSL MELLAN 11 OCH 29.                     
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W1T131                                              
001900*                     W1T131U                                             
002000*        MID:         W1I13101                                            
002100**   UTDATA.                                                              
002200*        MOD:         W1O13101                                            
002300*    SUBPROGRAM.                                                          
002400*        FELLOG                                                           
002500*        CBLTDLI                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP3                                                                
002900 DATA DIVISION.                                                           
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W1013100'.            
003500 77  JA                          PIC X(1)    VALUE 'J'.                   
003600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003700 77  INPUT-RETT                  PIC X(1)    VALUE 'J'.                   
003800 77  LAS-VIDARE                  PIC X(1)    VALUE 'J'.                   
003900 77  NEVIS-ART                   PIC X(1)    VALUE 'N'.                   
004000 77  WS-RAKNARE                  PIC S9(3)   COMP-3 VALUE ZERO.           
004100 77  MAX-RAD-PLUS-1              PIC S9(3)   VALUE +73  COMP-3.           
004200 77  WS-MAX-PLUS-1               PIC S9(3)   VALUE +100 COMP-3.           
004300 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77  SPR-IX                      PIC S9(3)   VALUE +0   COMP SYNC.        
004500 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +833  COMP SYNC.        
004700                                                                          
004800*01  -COPY WWPRODSL                                                       
004900     SKIP3                                                                
005000                                                                          
005100 01  DYNAMISKA-SUBPROGRAM.                                                
005200   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005300   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005400                                                                          
005500 01  WS-IDBENNR                             PIC X(7).                     
005600 01  IDBENNR-WS REDEFINES WS-IDBENNR        PIC 9(7).                     
005700     SKIP2                                                                
005800 01  WS-IDBENNR-NYTT                                 PIC X(7).            
005900 01  IDBENNR-NYTT-WS REDEFINES WS-IDBENNR-NYTT       PIC 9(7).            
006000     SKIP2                                                                
006100 01  WS-IDARTNR-SPAR                                 PIC 9(9).            
006200     EJECT                                                                
006300 01  NYCKLAR-TILL-DLI.                                                    
006400     03  W-IDARTNR-X.                                                     
006500         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
006600     03  W-IDBENNR-X.                                                     
006700         05  W-IDBENNR            PIC S9(7) COMP-3 VALUE ZERO.            
006800     03  W-IDSKYLT-X.                                                     
006900         05  W-IDSKYLT            PIC X(3)  VALUE 'S  '.                  
007000     EJECT                                                                
007100 01  MEDDELANDE.                                                          
007200                                                                          
007300     03  FEL-01.                                                          
007400         05  FILLER              PIC X(29)  VALUE                         
007500            'BENÄMNINGSNUMMER EJ NUMERISKT'.                              
007600         05  FILLER              PIC X(29)  VALUE                         
007700            'DESCR. NUMBER NOT NUMERIC    '.                              
007800     03  FILLER REDEFINES FEL-01.                                         
007900         05  FEL-1 OCCURS 2      PIC X(29).                               
008000*                                                                         
008100     03  FEL-02.                                                          
008200         05  FILLER              PIC X(24) VALUE                          
008300            'BENÄMNINGSNUMMER SAKNAS '.                                   
008400         05  FILLER              PIC X(24) VALUE                          
008500            'DESCR. NUMBER IS MISSING'.                                   
008600     03  FILLER REDEFINES FEL-02.                                         
008700         05  FEL-2 OCCURS 2      PIC X(24).                               
008800*                                                                         
008900                                                                          
009000     03  FEL-03.                                                          
009100         05  FILLER              PIC X(20) VALUE                          
009200            'ARTIKLAR SAKNAS     '.                                       
009300         05  FILLER              PIC X(20) VALUE                          
009400            'NO PARTS REGISTERED '.                                       
009500     03  FILLER REDEFINES FEL-03.                                         
009600         05  FEL-3 OCCURS 2      PIC X(20).                               
009700*                                                                         
009800                                                                          
009900     03  FEL-04.                                                          
010000         05  FILLER              PIC X(26) VALUE                          
010100            'ANGE NYTT BENÄMNINGSNUMMER'.                                 
010200         05  FILLER              PIC X(26) VALUE                          
010300            'RESPECIFY DESCR. NUMBER   '.                                 
010400     03  FILLER REDEFINES FEL-04.                                         
010500         05  FEL-4 OCCURS 2      PIC X(26).                               
010600*                                                                         
010700                                                                          
010800     03  MED-01.                                                          
010900         05   FILLER             PIC X(20) VALUE                          
011000            'UPPDATERING GJORD  '.                                        
011100         05   FILLER             PIC X(20) VALUE                          
011200            'DATABASE IS UPDATED'.                                        
011300     03  FILLER REDEFINES MED-01.                                         
011400         05  MED-1 OCCURS 2      PIC X(20).                               
011500*                                                                         
011600                                                                          
011700     03  MED-02.                                                          
011800         05   FILLER             PIC X(16) VALUE                          
011900            'FLER RADER FINNS'.                                           
012000         05   FILLER             PIC X(16) VALUE                          
012100            'MORE LINES      '.                                           
012200     03  FILLER REDEFINES MED-02.                                         
012300         05  MED-2 OCCURS 2      PIC X(16).                               
012400*                                                                         
012500     EJECT                                                                
012600*                        ****    MFS OCH SKÄRMHANTERING                   
012700 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
012800*01  MID -COPY W1I13101                                                   
012900     EJECT                                                                
013000 01  FILLER              PIC X(16)  VALUE 'MOD-MID-AREA'.                 
013100 01  W-PROG-TO-PROG-SW.                                                   
013200     03   M-SW-LL        PIC S9(4)  VALUE +41 COMP SYNC.                  
013300     03   M-SW-Z1-Z2     PIC X(2)   VALUE LOW-VALUE.                      
013400     03   M-SW-KDTRANS   PIC X(8)   VALUE 'W1T131U '.                     
013500     03   M-SW-IDTRANS   PIC X(4)   VALUE '1131'.                         
013600     03   M-SW-KDFORM    PIC X(1)   VALUE '1'.                            
013700*    03   -COPY W1I13101  -PRE MOD-.                                      
013800     EJECT                                                                
013900*01  -COPY WMSGAREA                                                       
014000     EJECT                                                                
014100*03  MOD -COPY W1O13101  -RED MSG-AREA.                                   
014200     EJECT                                                                
014300*01  -COPY WMFSAREA                                                       
014400     EJECT                                                                
014500******************************************************************        
014600*****                                                                     
014700*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014800*****                                                                     
014900 01  IMS-WS.                                                              
015000     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
015100     SKIP3                                                                
015200*****                    **** STATUS-KOD FRÅN IMS                         
015300     03  STATUS-WS               PIC X(2).                                
015400         88  SEGMENT-FINNS                   VALUE '  '.                  
015500         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
015600     SKIP3                                                                
015700     03  GODK-STATUSKODER.                                                
015800         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
015900     SKIP3                                                                
016000 01  SSA1                        PIC X(64).                               
016100 01  SSA2                        PIC X(64).                               
016200     EJECT                                                                
016300*                            IMS FUNKTIONSKODER                           
016400*01  -COPY W0003                                                          
016500     EJECT                                                                
016600*                            DLI INPUT-OUTPUT AREA                        
016700 01  DLI-IO-AREA.                                                         
016800     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
016900     SKIP3                                                                
017000*    03  WLBENA  -COPY WDD301   -PRE BENA-  -RED IO-AREA.                 
017100     EJECT                                                                
017200*    03  WLBENA  -COPY WDD311   -PRE BENA-  -RED IO-AREA.                 
017300     EJECT                                                                
017400*    03  WLBENA  -COPY WDD312   -PRE BENA-  -RED IO-AREA.                 
017500     EJECT                                                                
017600 01  DLI-IO-AREA-2.                                                       
017700     03  IO-AREA-2               PIC X(110)  VALUE SPACE.                 
017800*    03  WDK6    -COPY WDK601   -PRE WDK6-  -RED IO-AREA-2.               
017900     EJECT                                                                
018000 LINKAGE SECTION.                                                         
018100     SKIP2                                                                
018200*01  -COPY W0009     -PRE MSG-                                            
018300     EJECT                                                                
018400*01  -COPY W0008     -PRE ALT-                                            
018500         05  FILLER              PIC X.                                   
018600     EJECT                                                                
018700*01  -COPY W0008     -PRE BEN1-                                           
018800         05  FILLER              PIC X.                                   
018900     EJECT                                                                
019000*01  -COPY W0008     -PRE BEN2-                                           
019100         05  FILLER              PIC X.                                   
019200     EJECT                                                                
019300*01  -COPY W0008     -PRE WDK6-                                           
019400         05  FILLER              PIC X.                                   
019500     EJECT                                                                
019600 PROCEDURE DIVISION USING MSG-PCB ALT-PCB BEN1-PCB BEN2-PCB               
019700                                                   WDK6-PCB.              
019800                                                                          
019900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB BEN1-PCB BEN2-PCB              
020000                                                   WDK6-PCB.              
020100     SKIP2                                                                
020200     PERFORM IMS-GET-MSG                                                  
020300     IF SEGMENT-FINNS                                                     
020400       PERFORM A-INIT-SPARA-INPUT                                         
020500       IF WS-IDBENNR NUMERIC                                              
020600         IF MFS-UPDATE                                                    
020700           PERFORM E-ROER-EJ-FAELT                                        
020800           PERFORM D-KOLLA-SKAERMEN                                       
020900           IF INPUT-RETT = JA                                             
021000             PERFORM B-UPPDATERA                                          
021100           ELSE                                                           
021200             MOVE WS-IDBENNR TO MOD-IDBENNR-UT                            
021300             INSPECT MOD-IDBENNR-UT REPLACING                             
021400                LEADING ZERO BY SPACE                                     
021500             MOVE MAX-MOD-LAENGD TO MSG-KVLL                              
021600             PERFORM IMS-INSERT-MSG                                       
021700           END-IF                                                         
021800         ELSE                                                             
021900           PERFORM C-LAS-BASEN                                            
022000           MOVE MFS-RENSA-FAELT TO MOD-IDBENNR-NYTT                       
022100           MOVE WS-IDBENNR TO MOD-IDBENNR-UT                              
022200           INSPECT MOD-IDBENNR-UT REPLACING                               
022300              LEADING ZERO BY SPACE                                       
022400           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
022500           PERFORM IMS-INSERT-MSG                                         
022600         END-IF                                                           
022700       ELSE                                                               
022800         MOVE FEL-1(SPR-IX) TO MOD-TEMFSFEL                               
022900         MOVE WS-IDBENNR TO MOD-IDBENNR-UT                                
023000         INSPECT MOD-IDBENNR-UT                                           
023100               REPLACING LEADING ZERO BY SPACE                            
023200         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
023300         PERFORM IMS-INSERT-MSG                                           
023400       END-IF                                                             
023500     END-IF                                                               
023600     MOVE ZERO TO RETURN-CODE                                             
023700     GOBACK                                                               
023800     CONTINUE.                                                            
023900     EJECT                                                                
024000 A-INIT-SPARA-INPUT SECTION.                                              
024100     SKIP2                                                                
024200     IF MSG-DUBBLA-TRANSKODER                                             
024300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I13101-CTX             
024400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
024500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024600     ELSE                                                                 
024700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I13101-CTX              
024800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
024900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025000     END-IF                                                               
025100     IF MID-IDARTNR-SPAR NUMERIC                                          
025200       MOVE MID-IDARTNR-SPAR TO WS-IDARTNR-SPAR                           
025300     ELSE                                                                 
025400       MOVE ZERO TO WS-IDARTNR-SPAR                                       
025500     END-IF                                                               
025600     IF MFS-IDTRANS = '1131'                                              
025700       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
025800     ELSE                                                                 
025900       MOVE SPACE TO MFS-KDTRTYP                                          
026000       MOVE ZERO TO WS-IDARTNR-SPAR                                       
026100     END-IF                                                               
026200     IF MID-IDBENNR-IN = ALL '+' OR SPACE                                 
026300       INSPECT MID-IDBENNR-UT REPLACING LEADING SPACE BY ZERO             
026400       MOVE MID-IDBENNR-UT TO WS-IDBENNR                                  
026500     ELSE                                                                 
026600       MOVE MID-IDBENNR-IN TO WS-IDBENNR                                  
026700       MOVE SPACE TO MFS-KDTRTYP                                          
026800       MOVE ZERO TO WS-IDARTNR-SPAR                                       
026900     END-IF                                                               
027000     MOVE LOW-VALUE TO MOD-W1O13101-CTX                                   
027100     MOVE 'W1O131N1' TO MFS-IDMOD                                         
027200     MOVE '1131' TO MOD-IDTRANS                                           
027300                                                                          
027400     IF ENGLISH-TEXT                                                      
027500       MOVE +2 TO SPR-IX                                                  
027600     ELSE                                                                 
027700       MOVE +1 TO SPR-IX                                                  
027800     END-IF                                                               
027900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
028000                             MOD-TEMFSINF                                 
028100                             MOD-IDBENNR-IN                               
028200     CONTINUE.                                                            
028300     EJECT                                                                
028400 B-UPPDATERA SECTION.                                                     
028500     SKIP2                                                                
028600     MOVE WS-IDBENNR TO MOD-IDBENNR-UT                                    
028700     INSPECT MOD-IDBENNR-UT REPLACING LEADING ZERO BY SPACE               
028800     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
028900                                                                          
029000     MOVE IDBENNR-WS TO W-IDBENNR                                         
029100     PERFORM IMS-GU-BENA01                                                
029200     IF SEGMENT-SAKNAS                                                    
029300       MOVE FEL-2(SPR-IX) TO MOD-TEMFSFEL                                 
029400       PERFORM IMS-INSERT-MSG                                             
029500     ELSE                                                                 
029600       MOVE IDBENNR-NYTT-WS TO W-IDBENNR                                  
029700       PERFORM IMS-GU-BENA01-NY                                           
029800       IF SEGMENT-SAKNAS                                                  
029900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDBENNR-NYTT-ATTR                  
030000         MOVE FEL-2(SPR-IX) TO MOD-TEMFSFEL                               
030100         PERFORM IMS-INSERT-MSG                                           
030200       ELSE                                                               
030300         PERFORM IMS-GHNP-BENA12                                          
030400         IF SEGMENT-SAKNAS                                                
030500           MOVE FEL-3(SPR-IX) TO MOD-TEMFSFEL                             
030600           PERFORM IMS-INSERT-MSG                                         
030700         ELSE                                                             
030800           MOVE NEJ TO NEVIS-ART                                          
030900           MOVE ZERO TO WS-RAKNARE                                        
031000           PERFORM UNTIL SEGMENT-SAKNAS                                   
031100                      OR WS-RAKNARE >= WS-MAX-PLUS-1                      
031200                                                                          
031300             MOVE BENA-ART-IDARTNR TO W-IDARTNR                           
031400             PERFORM IMS-GU-WDK601                                        
031500             MOVE WDK6-ART-KDPRODSL TO TEST-KDPRODSL                      
031600             IF KDPRODSL-VOLVO-ALL                                        
031700               MOVE JA TO NEVIS-ART                                       
031800             END-IF                                                       
031900*            -- TAG BORT ARTIKELN PÅ DET GAMLA IDBENNR (BEN1-PCB)         
032000             PERFORM IMS-DLET-BENA12                                      
032100             MOVE NEJ TO BENA-ART-FLFELHOMO                               
032200                                                                          
032300*            -- LÄGG TILL ARTIKELN PÅ DET NYA IDBENNR (BEN2-PCB)          
032400             PERFORM IMS-ISRT-BENA12                                      
032500             ADD +1 TO WS-RAKNARE                                         
032600             PERFORM IMS-GHNP-BENA12                                      
032700           END-PERFORM                                                    
032800           IF SEGMENT-SAKNAS                                              
032900             IF NEVIS-ART = JA                                            
033000               MOVE IDBENNR-WS TO W-IDBENNR                               
033100               PERFORM IMS-GHU-BENA01-BEN2                                
033200               MOVE JA TO BENA-BEN-FLAENDR                                
033300               PERFORM IMS-REPL-BENA01-BEN2                               
033400                                                                          
033500               MOVE IDBENNR-NYTT-WS TO W-IDBENNR                          
033600               PERFORM IMS-GHU-BENA01-BEN2                                
033700               MOVE JA TO BENA-BEN-FLAENDR                                
033800               PERFORM IMS-REPL-BENA01-BEN2                               
033900             END-IF                                                       
034000                                                                          
034100             PERFORM IMS-GU-BENA11                                        
034200             MOVE BENA-TEXT-BEART TO MOD-BEART-NYTT                       
034300             MOVE MED-1(SPR-IX) TO MOD-TEMFSINF                           
034400             PERFORM IMS-INSERT-MSG                                       
034500           ELSE                                                           
034600             MOVE MID-W1I13101-CTX TO MOD-MID-W1I13101-CTX                
034700             MOVE ZERO TO WS-RAKNARE                                      
034800             PERFORM IMS-INSERT-ALT-MSG                                   
034900           END-IF                                                         
035000         END-IF                                                           
035100       END-IF                                                             
035200     END-IF                                                               
035300     CONTINUE.                                                            
035400     EJECT                                                                
035500 C-LAS-BASEN SECTION.                                                     
035600     SKIP2                                                                
035700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-SPAR                             
035800     MOVE +1 TO RAD-IX                                                    
035900     MOVE JA TO LAS-VIDARE                                                
036000                                                                          
036100     MOVE IDBENNR-WS TO W-IDBENNR                                         
036200     PERFORM IMS-GU-BENA01                                                
036300     IF SEGMENT-FINNS                                                     
036400       PERFORM IMS-GET-BENA11                                             
036500       MOVE BENA-TEXT-BEART TO MOD-BEART                                  
036600                                                                          
036700       PERFORM IMS-GU-BENA01                                              
036800       MOVE WS-IDARTNR-SPAR TO W-IDARTNR                                  
036900       PERFORM IMS-GNP-BENA12                                             
037000       IF SEGMENT-FINNS                                                   
037100         PERFORM UNTIL SEGMENT-SAKNAS OR LAS-VIDARE = NEJ                 
037200           IF RAD-IX < MAX-RAD-PLUS-1                                     
037300             MOVE BENA-ART-IDARTNR TO MOD-IDARTNR(RAD-IX)                 
037400             ADD +1 TO RAD-IX                                             
037500             PERFORM IMS-GNP-BENA12                                       
037600           ELSE                                                           
037700             MOVE BENA-ART-IDARTNR TO MOD-IDARTNR-SPAR                    
037800             MOVE NEJ TO LAS-VIDARE                                       
037900             MOVE MED-2(SPR-IX) TO MOD-TEMFSINF                           
038000           END-IF                                                         
038100         END-PERFORM                                                      
038200       ELSE                                                               
038300         MOVE FEL-3(SPR-IX) TO MOD-TEMFSFEL                               
038400       END-IF                                                             
038500     ELSE                                                                 
038600       MOVE FEL-2(SPR-IX) TO MOD-TEMFSFEL                                 
038700     END-IF                                                               
038800     PERFORM UNTIL RAD-IX >= MAX-RAD-PLUS-1                               
038900       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(RAD-IX)                        
039000       ADD +1 TO RAD-IX                                                   
039100     END-PERFORM                                                          
039200     CONTINUE.                                                            
039300     EJECT                                                                
039400 D-KOLLA-SKAERMEN SECTION.                                                
039500     SKIP2                                                                
039600     MOVE JA TO INPUT-RETT                                                
039700                                                                          
039800     IF MID-IDBENNR-NYTT NUMERIC                                          
039900       MOVE MID-IDBENNR-NYTT TO WS-IDBENNR-NYTT                           
040000       IF WS-IDBENNR-NYTT = WS-IDBENNR                                    
040100         MOVE FEL-4(SPR-IX) TO MOD-TEMFSFEL                               
040200         MOVE MFS-NUM-FAELT-FEL TO MOD-IDBENNR-NYTT-ATTR                  
040300         MOVE NEJ TO INPUT-RETT                                           
040400       ELSE                                                               
040500         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBENNR-NYTT-ATTR                
040600       END-IF                                                             
040700     ELSE                                                                 
040800       MOVE MFS-NUM-FAELT-FEL TO MOD-IDBENNR-NYTT-ATTR                    
040900       MOVE FEL-1(SPR-IX) TO MOD-TEMFSFEL                                 
041000       MOVE NEJ TO INPUT-RETT                                             
041100     END-IF                                                               
041200     CONTINUE.                                                            
041300     EJECT                                                                
041400 E-ROER-EJ-FAELT SECTION.                                                 
041500     SKIP2                                                                
041600     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
041700                               MOD-IDBENNR-NYTT                           
041800                                                                          
041900     MOVE 1 TO RAD-IX                                                     
042000     PERFORM UNTIL                                                        
042100      NOT ( RAD-IX < MAX-RAD-PLUS-1 )                                     
042200       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(RAD-IX)                      
042300       ADD +1 TO RAD-IX                                                   
042400     END-PERFORM                                                          
042500     CONTINUE.                                                            
042600     EJECT                                                                
042700* IMS SEKTIONER                                                           
042800     SKIP3                                                                
042900 IMS-GET-MSG SECTION.                                                     
043000     SKIP2                                                                
043100     MOVE '  QC' TO GODK-STATUSKODER                                      
043200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
043300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043400     PERFORM IMS-STATUS-KONTROLL                                          
043500     CONTINUE.                                                            
043600     SKIP3                                                                
043700 IMS-INSERT-MSG SECTION.                                                  
043800     SKIP2                                                                
043900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
044000     IF NOT ENGLISH-TEXT                                                  
044100       MOVE '0' TO MFS-KDHUVOMR                                           
044200     END-IF                                                               
044300     MOVE SPACE TO GODK-STATUSKODER                                       
044400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
044500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044600     PERFORM IMS-STATUS-KONTROLL                                          
044700     CONTINUE.                                                            
044800     SKIP3                                                                
044900 IMS-INSERT-ALT-MSG SECTION.                                              
045000     SKIP2                                                                
045100     MOVE SPACE TO GODK-STATUSKODER                                       
045200     IF ENGLISH-TEXT                                                      
045300       MOVE '2' TO M-SW-KDFORM                                            
045400     END-IF                                                               
045500     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
045600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
045700     PERFORM IMS-STATUS-KONTROLL                                          
045800     CONTINUE.                                                            
045900     EJECT                                                                
046000 IMS-GU-BENA01 SECTION.                                                   
046100     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
046200             DELIMITED BY SIZE INTO SSA1                                  
046300     MOVE '  GE' TO GODK-STATUSKODER                                      
046400     CALL CBLTDLI USING GU BEN1-PCB DLI-IO-AREA SSA1                      
046500     MOVE BEN1-STATUS-CODE TO STATUS-WS                                   
046600     PERFORM IMS-STATUS-KONTROLL                                          
046700     CONTINUE.                                                            
046800     SKIP3                                                                
046900 IMS-GU-BENA01-NY SECTION.                                                
047000     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
047100             DELIMITED BY SIZE INTO SSA1                                  
047200     MOVE '  GE' TO GODK-STATUSKODER                                      
047300     CALL CBLTDLI USING GU BEN2-PCB DLI-IO-AREA SSA1                      
047400     MOVE BEN2-STATUS-CODE TO STATUS-WS                                   
047500     PERFORM IMS-STATUS-KONTROLL                                          
047600     CONTINUE.                                                            
047700     SKIP3                                                                
047800 IMS-GHU-BENA01-BEN2 SECTION.                                             
047900     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
048000             DELIMITED BY SIZE INTO SSA1                                  
048100     MOVE '  GE' TO GODK-STATUSKODER                                      
048200     CALL CBLTDLI USING GHU BEN2-PCB DLI-IO-AREA SSA1                     
048300     MOVE BEN2-STATUS-CODE TO STATUS-WS                                   
048400     PERFORM IMS-STATUS-KONTROLL                                          
048500     CONTINUE.                                                            
048600     SKIP3                                                                
048700 IMS-REPL-BENA01-BEN2 SECTION.                                            
048800     MOVE '  ' TO GODK-STATUSKODER                                        
048900     CALL CBLTDLI USING REPL BEN2-PCB DLI-IO-AREA                         
049000     MOVE BEN2-STATUS-CODE TO STATUS-WS                                   
049100     PERFORM IMS-STATUS-KONTROLL                                          
049200     CONTINUE.                                                            
049300     SKIP3                                                                
049400 IMS-GET-BENA11 SECTION.                                                  
049500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
049600              DELIMITED BY SIZE INTO SSA1                                 
049700     MOVE '  ' TO GODK-STATUSKODER                                        
049800     CALL CBLTDLI USING GU BEN1-PCB DLI-IO-AREA SSA1                      
049900     MOVE BEN1-STATUS-CODE TO STATUS-WS                                   
050000     CONTINUE.                                                            
050100     SKIP3                                                                
050200 IMS-GU-BENA11 SECTION.                                                   
050300     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
050400             DELIMITED BY SIZE INTO SSA1                                  
050500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
050600              DELIMITED BY SIZE INTO SSA2                                 
050700     MOVE '  ' TO GODK-STATUSKODER                                        
050800     CALL CBLTDLI USING GU BEN1-PCB DLI-IO-AREA SSA1 SSA2                 
050900     MOVE BEN1-STATUS-CODE TO STATUS-WS                                   
051000     PERFORM IMS-STATUS-KONTROLL                                          
051100     CONTINUE.                                                            
051200     EJECT                                                                
051300 IMS-GNP-BENA12 SECTION.                                                  
051400     STRING 'WLBENA12(IDARTNR >=' W-IDARTNR-X ')'                         
051500             DELIMITED BY SIZE INTO SSA1                                  
051600     MOVE '  GE' TO GODK-STATUSKODER                                      
051700     CALL CBLTDLI USING GNP BEN1-PCB DLI-IO-AREA SSA1                     
051800     MOVE BEN1-STATUS-CODE TO STATUS-WS                                   
051900     PERFORM IMS-STATUS-KONTROLL                                          
052000     CONTINUE.                                                            
052100     SKIP3                                                                
052200 IMS-GHNP-BENA12 SECTION.                                                 
052300     STRING 'WLBENA12(IDARTNR >=' W-IDARTNR-X ')'                         
052400             DELIMITED BY SIZE INTO SSA1                                  
052500     MOVE '  GE' TO GODK-STATUSKODER                                      
052600     CALL CBLTDLI USING GHNP BEN1-PCB DLI-IO-AREA SSA1                    
052700     MOVE BEN1-STATUS-CODE TO STATUS-WS                                   
052800     PERFORM IMS-STATUS-KONTROLL                                          
052900     CONTINUE.                                                            
053000     SKIP3                                                                
053100 IMS-DLET-BENA12 SECTION.                                                 
053200     MOVE '  ' TO GODK-STATUSKODER                                        
053300     CALL CBLTDLI USING DLET BEN1-PCB DLI-IO-AREA                         
053400     MOVE BEN1-STATUS-CODE TO STATUS-WS                                   
053500     PERFORM IMS-STATUS-KONTROLL                                          
053600     CONTINUE.                                                            
053700     SKIP3                                                                
053800 IMS-ISRT-BENA12 SECTION.                                                 
053900     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
054000              DELIMITED BY SIZE INTO SSA1                                 
054100     MOVE 'WLBENA12 ' TO SSA2                                             
054200     MOVE '  ' TO GODK-STATUSKODER                                        
054300     CALL CBLTDLI USING ISRT BEN2-PCB DLI-IO-AREA SSA1 SSA2               
054400     MOVE BEN2-STATUS-CODE TO STATUS-WS                                   
054500     PERFORM IMS-STATUS-KONTROLL                                          
054600     CONTINUE.                                                            
054700     SKIP3                                                                
054800 IMS-GU-WDK601 SECTION.                                                   
054900                                                                          
055000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
055100          DELIMITED BY SIZE INTO SSA1                                     
055200     MOVE '  ' TO GODK-STATUSKODER                                        
055300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-2 SSA1                    
055400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
055500     PERFORM IMS-STATUS-KONTROLL                                          
055600     .                                                                    
055700     EJECT                                                                
055800 IMS-STATUS-KONTROLL SECTION.                                             
055900     SET STATUS-IX TO 1                                                   
056000     SEARCH GODK-STATUS                                                   
056100       AT END                                                             
056200         CALL FELLOG                                                      
056300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
056400     END-SEARCH                                                           
056500     CONTINUE                                                             
056600            CONTINUE.                                                     
056700 IMS-STATUS-KONTROLL-EXIT. EXIT.                                          
056800     CONTINUE.                                                            
