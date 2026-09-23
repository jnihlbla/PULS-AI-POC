000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411SDCA.                                                
000500 AUTHOR.         LASSI OLGRENER.                                          
000600 DATE-WRITTEN.   NOV  -94.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                       
001100*                                                                         
001200*    FUNKTION.                                                            
001300*      - PRELIMINÄR AVBOKNING AV EN RAD MOT ETT SUPORTLAGER               
001400*        AVBOKNING GÖRS ENBART OM HELA RADEN FINNS DISPONIBELT            
001500*        PÅ RESPEKTIVE SDC.                                               
001600*        I ANNAT FALL CLEARAS RADEN I SIN HELHET TILL CDC:ET              
001700*                                                                         
001800*        OM KDCALL = 2 (=FRÅGA SALDO) GÖRS INGEN UPDATERING               
001900*                                                                         
002000*        PROGRAMMET UPPDAT     WLARTS (WDK7)  SDC-ARTIKELREG.             
002100*                                                                         
002200*        LÄNKAREA: W411SDCA                                               
002300*                                                                         
002400*        STORY 2375089 ADD IDSYSTEM VOUI, ECOM                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W411SDCA'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600 77  W-DISP                      PIC S9(8)   VALUE ZERO.                  
003700 77  W-DISP-KVANT                PIC S9(8).                               
003800 77  W-KVAKS-SDC                 PIC 9(7)    VALUE ZERO.                  
003900 77  W-KVOKS-DAG                 PIC 9(7)    VALUE ZERO.                  
004000 77  W-KVOKS-BULK                PIC 9(7)    VALUE ZERO.                  
004100 77  W-KVDAGAR-POKS              PIC 9(3)    VALUE ZERO COMP-3.           
004200 77  W-KVDAGAR-PP                PIC 9(3)    VALUE ZERO COMP-3.           
004300 77  W-TIBUFF                    PIC 9(6)    VALUE ZERO.                  
004400 77  W-KVBEART-BUFF              PIC S9(7)   VALUE ZERO.                  
004500 77  W-KVLS-AVAILABLE            PIC S9(7)   VALUE ZERO.                  
004600 77  W-KVLS-ON-HAND              PIC S9(7)   VALUE ZERO.                  
004700 77  W-KVDISP-REF                PIC S9(7)   VALUE ZERO.                  
004800 77  W-HELP-DATE                 PIC 9(6)    VALUE ZERO.                  
004900       EJECT                                                              
005000 77  AKTUELLT-LAND               PIC X       VALUE 'N'.                   
005100     88  AKTUELLT-LAND-KINA                  VALUE 'J'.                   
005200     88  AKTUELLT-LAND-EJ-KINA               VALUE 'N'.                   
005300                                                                          
005400 77  PREPLANED-SW                PIC X       VALUE 'N'.                   
005500     88  PREPLANED                           VALUE 'J'.                   
005600                                                                          
005700 01  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005800 01  CURRENT-TIME                PIC 9(8)    VALUE ZERO.                  
005900 01  FILLER REDEFINES CURRENT-TIME.                                       
006000     03  CURRENT-TTHHMM          PIC 9(6).                                
006100     03  FILLER                  PIC 9(2).                                
006200                                                                          
006300 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
006400 01  FILLER REDEFINES TEST-IDDISTR.                                       
006500*    03   -COPY WWDIST18.                                                 
006600     EJECT                                                                
006700 01  FILLER REDEFINES TEST-IDDISTR.                                       
006800*    03   -COPY WWDIST35.                                                 
006900     EJECT                                                                
007000*01  -COPY WWDC99                                                         
007100     EJECT                                                                
007200*01  -COPY WWPRODSL                                                       
007300     EJECT                                                                
007400 01  GENERELLA-SUBPROGRAM.                                                
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
007800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
007900     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
008000     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
008100                                                                          
008200 01  WZ20DAYS                    PIC X(8) VALUE 'WZ20DAYS'.               
008300*    -COPY WZ20DAYS                                                       
008400*01  -COPY WORKAREA                                                       
008500*01  -COPY WDAGAREA                                                       
008600 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
008700*    -COPY W005WDK7                                                       
008800                                                                          
008900 01  DATUM-AAMMDD                PIC 9(6).                                
009000                                                                          
009100                                                                          
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  NYCKLAR-TILL-DLI.                                                    
009500     03  W-IDARTNR-X.                                                     
009600         05  W-IDARTNR           PIC S9(9)   VALUE +0  COMP-3.            
009700                                                                          
009800     03  W-KDSEGKEY-X.                                                    
009900         05  W-KDSEGKE           PIC X(1)    VALUE '1'.                   
010000                                                                          
010100     03  W-IDDC-X.                                                        
010200         05 W-IDDC               PIC  X(2)   VALUE SPACE.                 
010300                                                                          
010400     03  W-IDDC-B6-X.                                                     
010500         05 W-IDDC-B6                  PIC X(2).                          
010600                                                                          
010700     03  W-IDDC-REF-X.                                                    
010800         05 W-IDDC-REF                 PIC X(2).                          
010900                                                                          
011000     03  W-IDLEVNR-1441-X.                                                
011100         05  W-IDLEVNR-1441      PIC X(5)    VALUE '1441 '.               
011200                                                                          
011300     03  W-WDQ4B1KY-MIN-X.                                                
011400         05  W-IDARTNR-Q4-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
011500         05  FILLER              PIC  X(32)  VALUE LOW-VALUE.             
011600                                                                          
011700     03  W-WDQ4B1KY-MAX-X.                                                
011800         05  W-IDARTNR-Q4-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
011900         05  FILLER              PIC  X(32)  VALUE HIGH-VALUE.            
012000                                                                          
012100     03  W-IDDC-Q4-X.                                                     
012200         05  W-IDDC-Q4           PIC  X(2)   VALUE SPACE.                 
012300                                                                          
012400     03  W-IDORDER-X.                                                     
012500         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
012600                                                                          
012700     03  W-WDQ401KY-X.                                                    
012800         05  W-ORAD-IDORDER      PIC S9(7)   VALUE ZERO COMP-3.           
012900         05  W-ORAD-IDDC         PIC  X(2)   VALUE ZERO.                  
013000         05  W-ORAD-ADLAGOMR     PIC S9(3)   VALUE ZERO COMP-3.           
013100         05  W-ORAD-ADGANG       PIC S9(3)   VALUE ZERO COMP-3.           
013200         05  W-ORAD-ADPLATS      PIC S9(5)   VALUE ZERO COMP-3.           
013300         05  W-ORAD-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
013400         05  W-ORAD-IDLOPNR      PIC S9(3)   VALUE ZERO COMP-3.           
013500     SKIP3                                                                
013600*    --- STATUS-KOD FRÅN IMS                                              
013700 01  STATUS-WS                   PIC XX.                                  
013800     88  SEGMENT-FINNS                       VALUE '  '.                  
013900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014000     88  BASEN-SLUT                          VALUE 'GB'.                  
014100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014200     SKIP2                                                                
014300 01  GODK-STATUSKODER.                                                    
014400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014500     SKIP3                                                                
014600 01  SSA1                        PIC X(144).                              
014700 01  SSA2                        PIC X(96).                               
014800     EJECT                                                                
014900*    --- IMS FUNKTIONSKODER                                               
015000*01  -COPY W0003                                                          
015100     EJECT                                                                
015200*    ---  DLI INPUT-OUTPUT AREA                                           
015300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015400     SKIP3                                                                
015500 01  DLI-IO-AREA.                                                         
015600*    03   -COPY WDK711                                                    
015700                                                                          
015800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
015900 01   DLI-IO-AREA-B601.                                                   
016000*     03  -COPY WDB601                                                    
016100                                                                          
016200 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
016300 01   DLI-IO-AREA-B616.                                                   
016400*     03  -COPY WDB616                                                    
016500                                                                          
016600                                                                          
016700 01  FILLER               PIC X(16)   VALUE 'WDK901 AREA'.                
016800 01   DLI-IO-AREA-K901.                                                   
016900*     03  -COPY WDK901                                                    
017000                                                                          
017100 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
017200 01   DLI-IO-AREA-R601.                                                   
017300*     03  -COPY WDR601                                                    
017400*     05  -COPY W414LOGG      -RED FIL-WDR601-DATA                        
017500                                                                          
017600 01  FILLER               PIC X(16)   VALUE 'WDK611 AREA'.                
017700 01   DLI-IO-AREA-K611.                                                   
017800*     03  -COPY WDK611                                                    
017900                                                                          
018000                                                                          
018100 01  FILLER               PIC X(16)   VALUE 'WDQ4B1 AREA'.                
018200 01   DLI-IO-AREA-Q4B1.                                                   
018300*     03  -COPY WDQ4B1                                                    
018400                                                                          
018500                                                                          
018600 01  FILLER               PIC X(16)   VALUE 'WDQ201 AREA'.                
018700 01   DLI-IO-AREA-Q201.                                                   
018800*     03  -COPY WDQ201                                                    
018900                                                                          
019000                                                                          
019100 01  FILLER               PIC X(16)   VALUE 'WDQ401 AREA'.                
019200 01   DLI-IO-AREA-Q401.                                                   
019300*     03  -COPY WDQ401                                                    
019400                                                                          
019500 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK711'.              
019600 01   DLI-IO-WDK711.                                                      
019700*     03  -COPY WDK711 -PRE K7-                                           
019800                                                                          
019900 LINKAGE SECTION.                                                         
020000*   -COPY W411SDCA                                                        
020100     EJECT                                                                
020200*01  -COPY W0008      -PRE ARTS-                                          
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500*01  -COPY W0008      -PRE WDB6-                                          
020600     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
020800*01  -COPY W0008      -PRE WDK9-                                          
020900     05  FILLER                  PIC X.                                   
021000     EJECT                                                                
021100*01  -COPY W0008      -PRE WDR6-                                          
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400*01  -COPY W0008      -PRE WDK6-                                          
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021700*01  -COPY W0008      -PRE WDQ4B-                                         
021800     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
022000*01  -COPY W0008      -PRE WDQ2-                                          
022100     05  FILLER                  PIC X.                                   
022200     EJECT                                                                
022300*01  -COPY W0008      -PRE WDQ4-                                          
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600*01  -COPY W0008      -PRE WDB6-2-                                        
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008      -PRE WDK6-2-                                        
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008      -PRE WDK7-2-                                        
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023200*01  -COPY W0008      -PRE WDK7-3-                                        
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500 PROCEDURE DIVISION  USING SDCA-W411SDCA ARTS-PCB WDB6-PCB                
023600                           WDK9-PCB WDR6-PCB WDK6-PCB                     
023700                           WDQ4B-PCB WDQ2-PCB WDQ4-PCB                    
023800                           WDB6-2-PCB WDK6-2-PCB WDK7-2-PCB               
023800                           WDK7-3-PCB.                                    
023900                                                                          
024000     MOVE ZERO              TO SDCA-KDORDBEK                              
024100     MOVE SPACE             TO SDCA-KDOI                                  
024200     MOVE SDCA-IDDC         TO W-IDDC-B6                                  
024300     PERFORM IMS-GU-WDB601                                                
024400     IF DCS-CHINA                                                         
024500        SET AKTUELLT-LAND-KINA    TO TRUE                                 
024600     ELSE                                                                 
024700        SET AKTUELLT-LAND-EJ-KINA TO TRUE                                 
024800     END-IF                                                               
024900     MOVE DCS-KVDAGAR-POKS  TO W-KVDAGAR-POKS                             
025000     MOVE DCS-KVDAGAR-PP    TO W-KVDAGAR-PP                               
025100                                                                          
025200     IF SDCA-IXDCCLEAR = 1                                                
025300       MOVE SPACE           TO SDCA-CLEARGROUP                            
025400     END-IF                                                               
025500*-------------------------DIRLEV, STYR ALLTID TILL DC11                   
025600     MOVE SDCA-IDDC-TVS     TO W-IDDC-B6                                  
025700     PERFORM IMS-GU-WDB601                                                
025800                                                                          
025900     IF SDCA-IDLEVNR NOT = SPACE                                          
026000       IF DCS-SDC OR DCS-NDC                                              
026100          MOVE 53           TO SDCA-KDORDBEK                              
026200          MOVE 'CD'         TO SDCA-KDOI                                  
026300       ELSE                                                               
026400          MOVE 15           TO SDCA-KDORDBEK                              
026500          MOVE 'CD'         TO SDCA-KDOI                                  
026600       END-IF                                                             
026700     ELSE                                                                 
026800       IF SDCA-KDORDING < 3                                               
026900         MOVE 'SD'          TO SDCA-KDOI                                  
027000         PERFORM S03-CHECK-PRE-PLANNED                                    
027100         IF PREPLANED AND SDCA-KDORDBEK = ZERO                            
027200            MOVE 'PP'       TO SDCA-KDOI                                  
027300         ELSE                                                             
027400            MOVE 'XX'       TO SDCA-KDOI                                  
027500         END-IF                                                           
027600         MOVE SDCA-IDDC     TO SDCA-IDDC-CLEAR(SDCA-IXDCCLEAR)            
027700       END-IF                                                             
027800                                                                          
027900       MOVE SDCA-IDARTNR    TO W-IDARTNR                                  
028000       MOVE SDCA-IDDC       TO W-IDDC                                     
028100                                                                          
028200       PERFORM IMS-GHU-ARTS11                                             
028300       IF SEGMENT-FINNS                                                   
028400         IF SDCA-FLORDSPE = JA OR                                         
028500            SDCA-IDSYSTEM = 'W216'                                        
028600*           IF SDCA-KDOI = 'XX'                                           
028700            IF SDCA-KDOI = 'XX' OR 'PP'                                   
028800               MOVE JA  TO SDCA-FLLF(SDCA-IXDCCLEAR)                      
028900               MOVE NEJ TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)                   
029000            END-IF                                                        
029100            IF SDCA-FLORDSPE = JA AND                                     
029200*              (SDCA-IDSYSTEM = 'LDC ' OR 'LDCH' OR 'TACD')               
029300              ((SDCA-IDSYSTEM = 'LDC ' OR 'TACD') OR                      
029400               (SDCA-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR           
029500                                     'TAD' OR 'ACC' OR 'APA' OR           
029600                                     'APB' OR 'APC' OR 'APD' OR           
029700                                     'APE' OR 'APF' OR 'APG' OR           
029800                                     'APH' OR 'API' OR 'APJ' ))           
029900              IF SDCA-KDORDKL > +1                                        
030000                 IF SDCA-TIREPDAT > ZERO                                  
030100                    PERFORM S02-CHECK-PRE-OKS                             
030200                    IF SDCA-KVOKS-PREL = ZERO                             
030300                       ADD SDCA-KVBEART-Q TO SLAG-KVOKS-BULK              
030400                    END-IF                                                
030500                 ELSE                                                     
030600                    ADD SDCA-KVBEART-Q TO SLAG-KVOKS-BULK                 
030700                 END-IF                                                   
030800              ELSE                                                        
030900                ADD SDCA-KVBEART-Q TO SLAG-KVOKS-DAG                      
031000              END-IF                                                      
031100              PERFORM S01-REPL-ARTS                                       
031200            ELSE                                                          
031300              CONTINUE                                                    
031400            END-IF                                                        
031500         ELSE                                                             
031600           PERFORM A-KOLLA-SALDOT                                         
031700         END-IF                                                           
031800                                                                          
031900*        IF SDCA-KDORDBEK = ZERO OR 15                                    
032000         IF SDCA-KDORDBEK = ZERO                                          
032100           MOVE SLAG-ADLAGOMR    TO SDCA-ADLAGOMR                         
032200           MOVE SLAG-ADGANG      TO SDCA-ADGANG                           
032300           MOVE SLAG-ADPLATS     TO SDCA-ADPLATS                          
032400         END-IF                                                           
032500       ELSE                                                               
032600*        IF SDCA-KDOI = 'XX'                                              
032700         IF SDCA-KDOI = 'XX' OR 'PP'                                      
032800            MOVE NEJ TO SDCA-FLLF(SDCA-IXDCCLEAR)                         
032900         END-IF                                                           
033000         PERFORM B-SAETT-KDORDBEK                                         
033100*        IF SDCA-KDORDING < 3                                             
033200*          MOVE 'C2'             TO SDCA-KDOI                             
033300*        END-IF                                                           
033400***CHECK FOR INSERTION OF DC NOT EQUAL TO 11 ***                          
033500         IF (((DCS-FLARTADD  = 'Y' OR 'J') AND                            
033600               SDCA-IDDC NOT = '11') AND                                  
033700             ((SDCA-KDORDKL  = 0 AND SDCA-FLFORBI NOT = JA) OR            
033800              (SDCA-KDORDKL  = 1     OR                                   
033900               SDCA-KDORDKL  = 2     OR                                   
034000               SDCA-KDORDKL  = 3     OR                                   
034100               SDCA-KDORDKL  = 4)))                                       
034200            MOVE SDCA-IDARTNR    TO W-IDARTNR                             
034300            MOVE SDCA-IDDC       TO W-IDDC                                
034400            PERFORM IMS-GU-WDK711                                         
034500         END-IF                                                           
034600       END-IF                                                             
034700     END-IF                                                               
034800                                                                          
034900*    IF SDCA-KDOI = 'XX'                                                  
035000     IF SDCA-KDOI = 'XX' OR 'PP'                                          
035100        PERFORM C-KOLLA-KDOI                                              
035200     END-IF                                                               
035300                                                                          
035400     GOBACK                                                               
035500                                                                          
035600     .                                                                    
035700     EJECT                                                                
035800 A-KOLLA-SALDOT SECTION.                                                  
035900                                                                          
036000     IF SLAG-KVAKS-SDC < +0                                               
036100       MOVE +0             TO W-KVAKS-SDC                                 
036200     ELSE                                                                 
036300       MOVE SLAG-KVAKS-SDC TO W-KVAKS-SDC                                 
036400     END-IF                                                               
036500     IF SLAG-KVOKS-DAG < +0                                               
036600       MOVE +0             TO W-KVOKS-DAG                                 
036700     ELSE                                                                 
036800       MOVE SLAG-KVOKS-DAG TO W-KVOKS-DAG                                 
036900     END-IF                                                               
037000     IF SLAG-KVOKS-BULK < +0                                              
037100       MOVE +0              TO W-KVOKS-BULK                               
037200     ELSE                                                                 
037300       MOVE SLAG-KVOKS-BULK TO W-KVOKS-BULK                               
037400     END-IF                                                               
037500     COMPUTE W-DISP = SLAG-KVLS    +                                      
037600                      W-KVAKS-SDC  -                                      
037700                      W-KVOKS-DAG  -                                      
037800                      W-KVOKS-BULK                                        
037900     IF W-DISP > ZERO                                                     
038000       COMPUTE W-DISP = W-DISP       -                                    
038100                        SLAG-KVUTRS -                                     
038200                        SLAG-KVSPARR-KVAL                                 
038300     END-IF                                                               
038400                                                                          
038500*GK  IF SDCA-TIREPDAT > ZERO AND                                          
038600*       SDCA-IDSYSTEM NOT = 'LDCH' AND                                    
038700*       PREPLANED                                                         
038800*    EN LITEN FIX FÖR ATT INTE KOLLA SALDOT                               
038900*    DETTA SKALL GÖRAS I HANGING ORDER LINE RUTINEN                       
039000*    NÄR VI KOMMER FRÅN HANGING ORDER LINE HAR VI 'LDCH' I IDSYSTE        
039100*                                                                         
039200*       MOVE 99999999 TO W-DISP                                           
039300*GK  END-IF                                                               
039400                                                                          
039500     IF SDCA-KDSORT    = 'L ' AND                                         
039600        SDCA-KVQPACK-1 > ZERO                                             
039700        PERFORM AA-ANPASSA-DISP-TILL-KVANT                                
039800     END-IF                                                               
039900                                                                          
040000     IF SDCA-KVBEART-Q > W-DISP AND                                       
040100        SDCA-KDORDKL    = 1       AND                                     
040200        SDCA-IDLEVNR    = SPACE   AND                                     
040300        SDCA-KDCALL     = 1                                               
040400        PERFORM AB-CREATE-LOG                                             
040500*                                                                         
040600*  FROM THE BEGINNG THE LOG WAS CREATED JUST TO SEE                       
040700*  HOW MANY CLASS 1 ORDER WITH NO DISP COULD USE                          
040800*  RESERVED QUANTITY FOR PREPLANNED ORDERS.                               
040900*  AFTER DECISION TO USE THE RESERVED QUANTITY FOR                        
041000*  THESE ORDERS WE JUST CHANGE W-DISP TO SDCA-KVBEART-Q.                  
041100*  LOG-FLLEVOK WILL TELL US IF WE CAN OR NOT.                             
041200*  AND SO THE ORDER LINE WILL STAY AT THE LDC.                            
041300*                                                                         
041400        IF LOG-FLLEVOK = JA                                               
041500           MOVE SDCA-KVBEART-Q TO W-DISP                                  
041600        END-IF                                                            
041700     END-IF                                                               
041800     IF SDCA-KVBEART-Q > W-DISP OR                                        
041900             (SDCA-FLFORBI = NEJ AND SLAG-KDLEVSP > +0)                   
042000                                                                          
042100       IF SDCA-FLSDCLEV = JA                                              
042200           MOVE 80 TO SDCA-KDORDBEK                                       
042300*          IF SDCA-KDOI = 'XX'                                            
042400           IF SDCA-KDOI = 'XX' OR 'PP'                                    
042500              MOVE JA  TO SDCA-FLLF(SDCA-IXDCCLEAR)                       
042600              MOVE NEJ TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)                    
042700           END-IF                                                         
042800       ELSE                                                               
042900         MOVE SDCA-KDPRODSL      TO TEST-KDPRODSL                         
043000         IF SDCA-IDDC-TVS NOT = SPACE OR                                  
043100            (KDPRODSL-LOCAL AND AKTUELLT-LAND-EJ-KINA)                    
043200*          IF SDCA-KDOI = 'XX'                                            
043300           IF SDCA-KDOI = 'XX' OR 'PP'                                    
043400              MOVE JA  TO SDCA-FLLF(SDCA-IXDCCLEAR)                       
043500              MOVE NEJ TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)                    
043600           END-IF                                                         
043700           IF SDCA-KDORDKL = +0                                           
043800             MOVE 92           TO SDCA-KDORDBEK                           
043900             ADD SDCA-KVBEART-Q TO SLAG-KVOKS-DAG                         
044000             PERFORM S01-REPL-ARTS                                        
044100           ELSE                                                           
044200             MOVE SDCA-IDDISTR TO TEST-IDDISTR                            
044300             IF (SLAG-KDLEVSP > 19                                        
044400              OR SLAG-KVSPARR-KVAL > 0)                                   
044500             AND (DIST18-SKROT                                            
044600              OR DIST35-RETUR-Q)                                          
044700               IF SDCA-KDORDKL > +1                                       
044800                  IF SDCA-TIREPDAT > ZERO                                 
044900                     PERFORM S02-CHECK-PRE-OKS                            
045000                     IF SDCA-KVOKS-PREL = ZERO                            
045100                        ADD SDCA-KVBEART-Q TO SLAG-KVOKS-BULK             
045200                     END-IF                                               
045300                  ELSE                                                    
045400                     ADD SDCA-KVBEART-Q TO SLAG-KVOKS-BULK                
045500                  END-IF                                                  
045600               ELSE                                                       
045700                 ADD SDCA-KVBEART-Q TO SLAG-KVOKS-DAG                     
045800               END-IF                                                     
045900               PERFORM S01-REPL-ARTS                                      
046000             ELSE                                                         
046100               MOVE 80         TO SDCA-KDORDBEK                           
046200             END-IF                                                       
046300           END-IF                                                         
046400         ELSE                                                             
046500           MOVE 15             TO SDCA-KDORDBEK                           
046600           IF SDCA-KDOI = 'PP'                                            
046700              MOVE 'XX' TO SDCA-KDOI                                      
046800           END-IF                                                         
046900           IF SDCA-KDORDING < 3                                           
047000*            IF SDCA-KDOI = 'XX'                                          
047100             IF SDCA-KDOI = 'XX' OR 'PP'                                  
047200               IF SDCA-FLREFILL = JA AND SDCA-REDIRLEV < 1                
047300                  IF SLAG-FLREFILL = NEJ                                  
047400                     MOVE NEJ TO SDCA-FLLF(SDCA-IXDCCLEAR)                
047500                     MOVE JA  TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)             
047600                  ELSE                                                    
047700                     MOVE JA  TO SDCA-FLLF(SDCA-IXDCCLEAR)                
047800                     MOVE JA  TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)             
047900                  END-IF                                                  
048000               ELSE                                                       
048100                  MOVE NEJ      TO SDCA-FLLF(SDCA-IXDCCLEAR)              
048200                  MOVE JA       TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)           
048300               END-IF                                                     
048400             END-IF                                                       
048500           END-IF                                                         
048600         END-IF                                                           
048700       END-IF                                                             
048800     ELSE                                                                 
048900       IF SDCA-KDORDKL > +1                                               
049000         IF SDCA-TIREPDAT > ZERO                                          
049100            PERFORM S02-CHECK-PRE-OKS                                     
049200            IF SDCA-KVOKS-PREL = ZERO                                     
049300               ADD SDCA-KVBEART-Q TO SLAG-KVOKS-BULK                      
049400            END-IF                                                        
049500         ELSE                                                             
049600            ADD SDCA-KVBEART-Q TO SLAG-KVOKS-BULK                         
049700         END-IF                                                           
049800       ELSE                                                               
049900         ADD SDCA-KVBEART-Q    TO SLAG-KVOKS-DAG                          
050000       END-IF                                                             
050100       PERFORM S01-REPL-ARTS                                              
050200*      IF SDCA-KDORDING < 3                                               
050300*        IF SDCA-FLREFILL = JA AND SDCA-REDIRLEV < 1                      
050400*          MOVE 'S1'           TO SDCA-KDOI                               
050500*        ELSE                                                             
050600*          MOVE 'SD'           TO SDCA-KDOI                               
050700*        END-IF                                                           
050800*      END-IF                                                             
050900*      IF SDCA-KDOI = 'XX'                                                
051000       IF SDCA-KDOI = 'XX' OR 'PP'                                        
051100          MOVE JA  TO SDCA-FLLF(SDCA-IXDCCLEAR)                           
051200          MOVE NEJ TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)                        
051300       END-IF                                                             
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700 AA-ANPASSA-DISP-TILL-KVANT SECTION.                                      
051800                                                                          
051900       IF W-DISP < SDCA-KVBEART-Q                                         
052000          COMPUTE W-DISP-KVANT =                                          
052100                  W-DISP / SDCA-KVQPACK-1                                 
052200          COMPUTE W-DISP-KVANT =                                          
052300                  W-DISP-KVANT * SDCA-KVQPACK-1                           
052400          MOVE W-DISP-KVANT TO W-DISP                                     
052500       END-IF                                                             
052600     .                                                                    
052700     EJECT                                                                
052800                                                                          
052900 AB-CREATE-LOG   SECTION.                                                 
053000                                                                          
053100*    MOVE SDCA-IDARTNR      TO W-IDARTNR                                  
053200*    MOVE SDCA-IDDC         TO W-IDDC                                     
053300*    PERFORM IMS-GU-ARTS11                                                
053400*    IF SEGMENT-FINNS                                                     
053500                                                                          
053600     MOVE IDPGM             TO FIL-IDPGM                                  
053700     ACCEPT FIL-TIREGDAT    FROM DATE                                     
053800     ACCEPT FIL-TIKLOCK     FROM TIME                                     
053900     MOVE 1                 TO FIL-IDSEKVNR                               
054000     MOVE 'W414'            TO FIL-CT-IDSYSTEM                            
054100     MOVE 'A'               TO FIL-CT-IDVTYP                              
054200     MOVE 'LOG'             TO FIL-CT-IDPTYP                              
054300                                                                          
054400     MOVE FIL-TIREGDAT      TO LOG-TIREGDAT                               
054500     ACCEPT CURRENT-TIME    FROM TIME                                     
054600     MOVE CURRENT-TTHHMM    TO LOG-TIREGTID                               
054700     MOVE SDCA-IDARTNR      TO LOG-IDARTNR                                
054800     MOVE SDCA-KVBEART-Q    TO LOG-KVBEART-Q                              
054900     MOVE SDCA-IDDC         TO LOG-IDDC                                   
055000     MOVE SLAG-KVLS         TO LOG-KVLS                                   
055100     MOVE W-DISP            TO LOG-KVDISP                                 
055200     MOVE SLAG-KVAKS-SDC    TO LOG-KVAKS-SDC                              
055300     MOVE SLAG-KVAKS-PAV    TO LOG-KVAKS-PAV                              
055400     MOVE SLAG-KVOKS-BULK   TO LOG-KVOKS-BULK                             
055500     MOVE SLAG-KVOKS-DAG    TO LOG-KVOKS-DAG                              
055600     MOVE ZERO              TO LOG-KVOKS-BULK-REF                         
055700                               LOG-KVOKS-DAG-REF                          
055800                               LOG-KVDISP-REF                             
055900                               W-KVDISP-REF                               
056000                                                                          
056100     PERFORM ABA-COMPUTE-BUFFER-DATE                                      
056200     MOVE W-TIBUFF          TO LOG-TIBUFF                                 
056300     PERFORM ABB-COMPUTE-KVLS-BUFF                                        
056400     COMPUTE LOG-KVBEART-BUFF = W-KVBEART-BUFF + SLAG-KVOKS-DAG           
056500                                                                          
056600     COMPUTE W-KVLS-AVAILABLE = SLAG-KVOKS-BULK - W-KVBEART-BUFF          
056700     MOVE W-KVLS-AVAILABLE  TO LOG-KVLS-AVAILABLE                         
056800     COMPUTE W-KVLS-ON-HAND     = SLAG-KVLS - W-KVBEART-BUFF              
056900                                - SLAG-KVSPARR-KVAL                       
057000                                - SLAG-KVUTRS                             
057100     MOVE W-KVLS-ON-HAND    TO LOG-KVLS-ON-HAND                           
057200     MOVE DCS-IDDC-REF      TO LOG-IDDC-REF                               
057300                               W-IDDC-REF                                 
057400     PERFORM IMS-GU-WDK901                                                
057500     IF SEGMENT-FINNS                                                     
057600        MOVE ART-KVOKS-BULK TO LOG-KVOKS-BULK-REF                         
057700        MOVE ART-KVOKS-DAG  TO LOG-KVOKS-DAG-REF                          
057800     ELSE                                                                 
057900        MOVE ZERO TO ART-KVOKS-BULK                                       
058000                     ART-KVOKS-DAG                                        
058100                     ART-KVOKS-VOR                                        
058200     END-IF                                                               
058300                                                                          
058400     PERFORM IMS-GU-WDK611                                                
058500     IF SEGMENT-FINNS                                                     
058600        COMPUTE W-KVDISP-REF  = CLAG-KVLS                                 
058700                              - ART-KVOKS-BULK                            
058800                              - ART-KVOKS-DAG                             
058900                              - ART-KVOKS-VOR                             
059000                              - CLAG-KVRESS                               
059100        IF SLAG-KVOKS-DAG > ZERO                                          
059200* FIX FOR NEGATIVE OKS                                                    
059300           COMPUTE W-KVDISP-REF = W-KVDISP-REF - SLAG-KVOKS-DAG           
059400        END-IF                                                            
059500        MOVE W-KVDISP-REF   TO LOG-KVDISP-REF                             
059600     END-IF                                                               
059700                                                                          
059800                                                                          
059900*    IF  LOG-KVBEART-Q  NOT > LOG-KVLS-AVAILABLE                          
060000*    AND LOG-KVBEART-Q  NOT > LOG-KVDISP-REF                              
060100*    IF  LOG-KVBEART-Q  <= W-KVLS-ON-HAND                                 
060200     IF  LOG-KVBEART-Q  <= W-KVLS-ON-HAND - SLAG-KVOKS-DAG                
060300     AND LOG-KVBEART-Q  <= W-KVDISP-REF                                   
060400        MOVE JA             TO LOG-FLLEVOK                                
060500     ELSE                                                                 
060600        MOVE NEJ            TO LOG-FLLEVOK                                
060700     END-IF                                                               
060800                                                                          
060900                                                                          
061000*    MOVE LOG-W414LOGG  TO FIL-WDR601-DATA                                
061100     PERFORM IMS-ISRT-WDR601                                              
061200     IF SEGMENT-FINNS-REDAN                                               
061300       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
061400         ADD 1 TO FIL-IDSEKVNR                                            
061500         PERFORM IMS-ISRT-WDR601                                          
061600       END-PERFORM                                                        
061700     END-IF                                                               
061800*    END-IF                                                               
061900     .                                                                    
062000     EJECT                                                                
062100                                                                          
062200 ABA-COMPUTE-BUFFER-DATE SECTION.                                         
062300                                                                          
062400     MOVE ZERO         TO W-TIBUFF                                        
062500     MOVE DCS-IDDC     TO W-IDDC-B6                                       
062600     MOVE DCS-IDDC-REF TO W-IDDC-REF                                      
062700     PERFORM IMS-GU-WDB616                                                
062800     IF SEGMENT-FINNS                                                     
062900                                                                          
063000        MOVE 2                      TO WORK-KDCALL                        
063100        MOVE W-IDDC-REF             TO WORK-IDDC                          
063200        MOVE FIL-TIREGDAT           TO WORK-TIAAMMDD-FOM                  
063300*    PACKTID BÅT  (ARBETSTID)                                             
063400*    HAMN TILL GRIND     (ARBETSTID)                                      
063500*    INLÄGGNINGSTID BÅT  (ARBETSTID)                                      
063600*    BUFFER DAYS         (ARBETSTID)                                      
063700        COMPUTE WORK-KVWORKD = REF-KVDLTID-BOATPAC +                      
063800                REF-KVDLTID-BOAT2DC + REF-KVDLTID-BOATINS +               
063900                REF-KVDLTID-BUFF                                          
064000        ADD +3                      TO WORK-KVWORKD                       
064100*       JUST TO MAKE IT MATCH 4408                                        
064200                                                                          
064300        CALL WORKDAY USING WORK-KDCALL                                    
064400                           WORK-DATE-AREA WORK-KDSVAR                     
064500        IF WORK-KDSVAR-OK                                                 
064600           MOVE WORK-TIAAMMDD-TOM   TO DAG-TIAAMMDD-FOM                   
064700           MOVE 20                  TO DAG-TISEKEL-FOM                    
064800*                                                                         
064900*    TRANSPORTTID BÅT  (KALENDERTID)                                      
065000           MOVE 2                   TO DAG-KDCALL                         
065100           MOVE REF-KVDLTID-BOATTRP TO DAG-KVKALDAG                       
065200           ADD +1                   TO DAG-KVKALDAG                       
065300*                                                                         
065400                                                                          
065500           CALL WDAGKONV USING DAG-KDCALL                                 
065600                               DAG-DATUM-AREA DAG-KDSVAR                  
065700           IF WORK-KDSVAR-OK                                              
065800              MOVE DAG-TIAAMMDD-TOM TO W-TIBUFF                           
065900           END-IF                                                         
066000        END-IF                                                            
066100     END-IF                                                               
066200     .                                                                    
066300     EJECT                                                                
066400                                                                          
066500 ABB-COMPUTE-KVLS-BUFF   SECTION.                                         
066600                                                                          
066700     MOVE ZERO TO W-KVBEART-BUFF                                          
066800     MOVE SDCA-IDARTNR TO W-IDARTNR-Q4-MIN                                
066900                          W-IDARTNR-Q4-MAX                                
067000     MOVE SDCA-IDDC    TO W-IDDC-Q4                                       
067100                                                                          
067200     PERFORM IMS-GU-WDQ4B                                                 
067300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                        
067400                   SEQB-IDARTNR NOT = SDCA-IDARTNR OR                     
067500                   SEQB-IDDC    NOT = SDCA-IDDC                           
067600                                                                          
067700        MOVE SEQB-IDORDER TO W-IDORDER-X                                  
067800        PERFORM IMS-GU-WDQ201                                             
067900        IF OHUV-TIREPDAT = ZERO OR                                        
068000          (OHUV-TIREPDAT NOT < FIL-TIREGDAT AND                           
068100           OHUV-TIREPDAT NOT > W-TIBUFF)                                  
068200           MOVE SEQB-IDORDER     TO W-ORAD-IDORDER                        
068300           MOVE SEQB-IDDC        TO W-ORAD-IDDC                           
068400           MOVE SEQB-ADLAGOMR    TO W-ORAD-ADLAGOMR                       
068500           MOVE SEQB-ADGANG      TO W-ORAD-ADGANG                         
068600           MOVE SEQB-ADPLATS     TO W-ORAD-ADPLATS                        
068700           MOVE SEQB-IDARTNR     TO W-ORAD-IDARTNR                        
068800           MOVE SEQB-IDLOPNR     TO W-ORAD-IDLOPNR                        
068900                                                                          
069000           PERFORM IMS-GU-WDQ401                                          
069100           ADD ORAD-KVBEART-Q TO W-KVBEART-BUFF                           
069200        END-IF                                                            
069300                                                                          
069400        PERFORM IMS-GN-WDQ4B                                              
069500     END-PERFORM                                                          
069600     .                                                                    
069700     EJECT                                                                
069800                                                                          
069900 B-SAETT-KDORDBEK SECTION.                                                
070000                                                                          
070100     MOVE SDCA-KDPRODSL          TO TEST-KDPRODSL                         
070200     IF KDPRODSL-LOCAL AND AKTUELLT-LAND-EJ-KINA                          
070300       MOVE 55                 TO SDCA-KDORDBEK                           
070400*      IF SDCA-KDOI = 'XX'                                                
070500       IF SDCA-KDOI = 'XX' OR 'PP'                                        
070600          MOVE NEJ             TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)            
070700       END-IF                                                             
070800     ELSE                                                                 
070900       IF DCS-SDC OR DCS-NDC                                              
071000         MOVE 53               TO SDCA-KDORDBEK                           
071100*        IF SDCA-KDOI = 'XX'                                              
071200         IF SDCA-KDOI = 'XX' OR 'PP'                                      
071300            MOVE NEJ           TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)            
071400         END-IF                                                           
071500       ELSE                                                               
071600         MOVE 15               TO SDCA-KDORDBEK                           
071700*        IF SDCA-KDOI = 'XX'                                              
071800         IF SDCA-KDOI = 'XX' OR 'PP'                                      
071900            MOVE JA            TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)            
072000         END-IF                                                           
072100       END-IF                                                             
072200     END-IF                                                               
072300     .                                                                    
072400     EJECT                                                                
072500                                                                          
072600 C-KOLLA-KDOI   SECTION.                                                  
072700                                                                          
072800     MOVE SDCA-IDARTNR    TO W-IDARTNR                                    
072900     IF DCS-JAPAN OR DCS-AUSTRALIA OR DCS-CANADA                          
073000                                                                          
073100        MOVE DCS-IDDC        TO W-IDDC                                    
073200        PERFORM IMS-GU-ARTS11                                             
073300        IF SEGMENT-FINNS                                                  
073400           IF SLAG-IDLEVNR NOT = 1441                                     
073500              MOVE 'LO'      TO SDCA-KDOI                                 
073600           END-IF                                                         
073700        END-IF                                                            
073800     ELSE                                                                 
073900        IF DCS-USA                                                        
074000           MOVE 'LO'         TO SDCA-KDOI                                 
074100                                                                          
074200           PERFORM IMS-GU-ARTS01                                          
074300           IF SEGMENT-FINNS                                               
074400              PERFORM IMS-GNP-ARTS11-1441                                 
074500              PERFORM UNTIL SEGMENT-SAKNAS                                
074600*                  OR SDCA-KDOI = 'XX'                                    
074700                   OR SDCA-KDOI = 'PP'                                    
074800                 MOVE SLAG-IDDC  TO WS-IDDC                               
074900                 IF NDC-US                                                
075000                    PERFORM S03-CHECK-PRE-PLANNED                         
075100                    IF PREPLANED AND SDCA-KDORDBEK = ZERO                 
075200                       MOVE 'PP' TO SDCA-KDOI                             
075300                    ELSE                                                  
075400                       MOVE 'XX' TO SDCA-KDOI                             
075500                    END-IF                                                
075600                 ELSE                                                     
075700                    PERFORM IMS-GNP-ARTS11-1441                           
075800                 END-IF                                                   
075900              END-PERFORM                                                 
076000           END-IF                                                         
076100        ELSE                                                              
076200           IF DCS-CHINA                                                   
076300              MOVE 'LO'         TO SDCA-KDOI                              
076400              PERFORM IMS-GU-ARTS01                                       
076500              IF SEGMENT-FINNS                                            
076600                 PERFORM IMS-GNP-ARTS11-1441                              
076700                 PERFORM UNTIL SEGMENT-SAKNAS                             
076800                      OR SDCA-KDOI = 'XX'                                 
076900                      OR SDCA-KDOI = 'PP'                                 
077000                    MOVE SLAG-IDDC TO WS-IDDC                             
077100                    IF NDC-CN                                             
077200                       PERFORM S03-CHECK-PRE-PLANNED                      
077300                       IF PREPLANED AND SDCA-KDORDBEK = ZERO              
077400                          MOVE 'PP' TO SDCA-KDOI                          
077500                       ELSE                                               
077600                          MOVE 'XX' TO SDCA-KDOI                          
077700                       END-IF                                             
077800                    ELSE                                                  
077900                       PERFORM IMS-GNP-ARTS11-1441                        
078000                    END-IF                                                
078100                 END-PERFORM                                              
078200              END-IF                                                      
078300          END-IF                                                          
078400        END-IF                                                            
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800                                                                          
078900 S01-REPL-ARTS  SECTION.                                                  
079000     IF SDCA-KDCALL = +1                                                  
079100        PERFORM IMS-REPL-ARTS                                             
079200     END-IF                                                               
079300     .                                                                    
079400     EJECT                                                                
079500                                                                          
079600 S02-CHECK-PRE-OKS SECTION.                                               
079700                                                                          
079800**   MOVE DCS-IDDC TO WS-IDDC                                             
079900*    VI GÖR UNDANTAG FÖR DC 21 TILLS VI KAN STYRA                         
080000*    RADEN TILL DIREKTLEVEERANTÖ ISF DC11 I HANGING OKS                   
080100                                                                          
080200     IF NOT SDC-NL                                                        
080300                                                                          
080400     ACCEPT DATUM-AAMMDD FROM DATE                                        
080500     MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                          
080600     MOVE DATUM-AAMMDD         TO DAYS-TIDATE1                            
080700     MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                          
080800     MOVE SDCA-TIREPDAT        TO W-HELP-DATE                             
080900     MOVE W-HELP-DATE          TO DAYS-TIDATE2                            
081000     MOVE ZERO                 TO DAYS-KVDAYS                             
081100     MOVE SPACE                TO DAYS-IDCALEND                           
081200                                                                          
081300     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
081400                                                                          
081500     IF DAYS-KDRC = ZERO                                                  
081600        IF DAYS-KVDAYS > W-KVDAGAR-POKS                                   
081700           MOVE SDCA-KVBEART-Q TO SDCA-KVOKS-PREL                         
081800        END-IF                                                            
081900     ELSE                                                                 
082000        CALL FELLOG                                                       
082100     END-IF                                                               
082200     END-IF                                                               
082300     .                                                                    
082400                                                                          
082500     EJECT                                                                
082600 S03-CHECK-PRE-PLANNED SECTION.                                           
082700                                                                          
082800     MOVE NEJ TO PREPLANED-SW                                             
082900                                                                          
083000     IF SDCA-KDORDKL = 3 AND SDCA-TIREPDAT > ZERO                         
083100        ACCEPT DATUM-AAMMDD FROM DATE                                     
083200        MOVE 'YYMMDD'          TO DAYS-KDDATFMT1                          
083300        MOVE DATUM-AAMMDD      TO DAYS-TIDATE1                            
083400        MOVE 'YYMMDD'          TO DAYS-KDDATFMT2                          
083500        MOVE SDCA-TIREPDAT     TO W-HELP-DATE                             
083600        MOVE W-HELP-DATE       TO DAYS-TIDATE2                            
083700        MOVE ZERO              TO DAYS-KVDAYS                             
083800        MOVE SPACE             TO DAYS-IDCALEND                           
083900                                                                          
084000        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
084100                                                                          
084200        IF DAYS-KDRC = ZERO                                               
084300           IF DAYS-KVDAYS > W-KVDAGAR-PP                                  
084400              MOVE JA          TO PREPLANED-SW                            
084500           END-IF                                                         
084600        ELSE                                                              
084700           CALL FELLOG                                                    
084800        END-IF                                                            
084900     END-IF                                                               
085000     .                                                                    
085100                                                                          
085200     EJECT                                                                
085300 IMS-GU-ARTS01 SECTION.                                                   
085400                                                                          
085500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
085600          DELIMITED BY SIZE INTO SSA1                                     
085700     MOVE '  GE' TO GODK-STATUSKODER                                      
085800     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1                      
085900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
086000     PERFORM IMS-STATUSKONTROLL                                           
086100     .                                                                    
086200                                                                          
086300                                                                          
086400 IMS-GNP-ARTS11-1441 SECTION.                                             
086500                                                                          
086600     STRING 'WLARTS11(IDDC     =' W-IDDC-X                                
086700                    '&IDLEVNR  =' W-IDLEVNR-1441-X ')'                    
086800          DELIMITED BY SIZE INTO SSA1                                     
086900     MOVE '  GE' TO GODK-STATUSKODER                                      
087000     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA SSA1                     
087100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
087200     PERFORM IMS-STATUSKONTROLL                                           
087300     .                                                                    
087400                                                                          
087500                                                                          
087600 IMS-GHU-ARTS11 SECTION.                                                  
087700                                                                          
087800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
087900            DELIMITED BY SIZE INTO SSA1                                   
088000     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
088100            DELIMITED BY SIZE INTO SSA2                                   
088200     MOVE '  GE' TO GODK-STATUSKODER                                      
088300     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA SSA1 SSA2                
088400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
088500     PERFORM IMS-STATUSKONTROLL                                           
088600     .                                                                    
088700     SKIP2                                                                
088800 IMS-GU-ARTS11 SECTION.                                                   
088900                                                                          
089000     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
089100            DELIMITED BY SIZE INTO SSA1                                   
089200     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
089300            DELIMITED BY SIZE INTO SSA2                                   
089400     MOVE '  GE' TO GODK-STATUSKODER                                      
089500     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1 SSA2                 
089600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     .                                                                    
089900     SKIP2                                                                
090000 IMS-REPL-ARTS SECTION.                                                   
090100                                                                          
090200     MOVE '    ' TO GODK-STATUSKODER                                      
090300     CALL CBLTDLI USING REPL  ARTS-PCB DLI-IO-AREA                        
090400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
090500     PERFORM IMS-STATUSKONTROLL                                           
090600     .                                                                    
090700                                                                          
090800 IMS-GU-WDB601    SECTION.                                                
090900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
091000          DELIMITED BY SIZE INTO SSA1                                     
091100     MOVE '  GE' TO GODK-STATUSKODER                                      
091200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
091300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     IF SEGMENT-SAKNAS                                                    
091600        MOVE SPACE TO DCS-KDDC                                            
091700     END-IF                                                               
091800     .                                                                    
091900                                                                          
092000 IMS-GU-WDB616 SECTION.                                                   
092100                                                                          
092200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
092300          DELIMITED BY SIZE INTO SSA1                                     
092400     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
092500          DELIMITED BY SIZE INTO SSA2                                     
092600     MOVE '  GE' TO GODK-STATUSKODER                                      
092700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
092800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
092900     PERFORM IMS-STATUSKONTROLL                                           
093000     .                                                                    
093100                                                                          
093200 IMS-GU-WDK901                 SECTION.                                   
093300     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
093400            DELIMITED BY SIZE INTO SSA1                                   
093500     MOVE '  GE' TO GODK-STATUSKODER                                      
093600     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-AREA-K901 SSA1                 
093700     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
093800     PERFORM IMS-STATUSKONTROLL                                           
093900     .                                                                    
094000                                                                          
094100 IMS-ISRT-WDR601 SECTION.                                                 
094200                                                                          
094300     MOVE 'WDR601' TO SSA1                                                
094400     MOVE '  II' TO GODK-STATUSKODER                                      
094500     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
094600     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
094700     PERFORM IMS-STATUSKONTROLL                                           
094800     .                                                                    
094900                                                                          
095000 IMS-GU-WDK611 SECTION.                                                   
095100                                                                          
095200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
095300          DELIMITED BY SIZE INTO SSA1                                     
095400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
095500          DELIMITED BY SIZE INTO SSA2                                     
095600     MOVE '  GE' TO GODK-STATUSKODER                                      
095700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K611 SSA1 SSA2            
095800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100                                                                          
096200 IMS-GU-WDQ4B SECTION.                                                    
096300                                                                          
096400     STRING 'WDQ4B1  (WDQ4B1KY=>' W-WDQ4B1KY-MIN-X                        
096500                    '&WDQ4B1KY=<' W-WDQ4B1KY-MAX-X                        
096600                    '&IDDC    = ' W-IDDC-Q4-X ')'                         
096700          DELIMITED BY SIZE INTO SSA1                                     
096800     MOVE '  GE'           TO GODK-STATUSKODER                            
096900     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-AREA-Q4B1 SSA1                
097000     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
097100     PERFORM IMS-STATUSKONTROLL                                           
097200     .                                                                    
097300                                                                          
097400                                                                          
097500 IMS-GN-WDQ4B SECTION.                                                    
097600                                                                          
097700     STRING 'WDQ4B1  (WDQ4B1KY=>' W-WDQ4B1KY-MIN-X                        
097800                    '&WDQ4B1KY=<' W-WDQ4B1KY-MAX-X                        
097900                    '&IDDC    = ' W-IDDC-Q4-X ')'                         
098000          DELIMITED BY SIZE INTO SSA1                                     
098100     MOVE '  GEGB'         TO GODK-STATUSKODER                            
098200     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-AREA-Q4B1 SSA1                
098300     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
098400     PERFORM IMS-STATUSKONTROLL                                           
098500     .                                                                    
098600                                                                          
098700                                                                          
098800 IMS-GU-WDQ201 SECTION.                                                   
098900                                                                          
099000     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
099100          DELIMITED BY SIZE INTO SSA1                                     
099200     MOVE '    ' TO GODK-STATUSKODER                                      
099300     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-Q201 SSA1                 
099400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
099500     PERFORM IMS-STATUSKONTROLL                                           
099600     .                                                                    
099700                                                                          
099800                                                                          
099900 IMS-GU-WDQ401 SECTION.                                                   
100000                                                                          
100100     STRING 'WDQ401  (WDQ401KY =' W-WDQ401KY-X ')'                        
100200          DELIMITED BY SIZE INTO SSA1                                     
100300     MOVE '    ' TO GODK-STATUSKODER                                      
100400     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-AREA-Q401 SSA1                 
100500     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
100600     PERFORM IMS-STATUSKONTROLL                                           
100700     .                                                                    
100800                                                                          
100900                                                                          
101000     SKIP2                                                                
101100***INSERT FIRST OCCURRENCE OF DC <> 11 ON WDK711 ***                      
101200***FOR ORDER CLASS 0-4 ONLY,WHEN AUTO INSERT FLAG ***                     
101300***IS 'YES' ON THE W4N403 (DC INFORMATION 2) SCREEN ***                   
101400 IMS-GU-WDK711 SECTION.                                                   
101500     MOVE 'IMS-GU-WDK711   '  TO CURRENT-IMS-SECTION                      
101600                                                                          
101700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
101800          DELIMITED BY SIZE INTO SSA1                                     
101900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
102000          DELIMITED BY SIZE INTO SSA2                                     
102100     MOVE '  GE'              TO GODK-STATUSKODER                         
102200     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-WDK711 SSA1 SSA2             
102300     MOVE WDK7-2-STATUS-CODE  TO STATUS-WS                                
102400     PERFORM IMS-STATUSKONTROLL                                           
102500     .                                                                    
102600                                                                          
102700     IF SEGMENT-FINNS                                                     
102800        CONTINUE                                                          
102900     ELSE                                                                 
103000        MOVE ALL '+'          TO WDK7-W005WDK7                            
103100        MOVE 'WDK711'         TO WDK7-IDSEGM                              
103200        MOVE W-IDARTNR        TO WDK7-IDARTNR-KFB                         
103300        MOVE W-IDDC           TO WDK7-IDDC-KFB                            
103400                                 WDK7-IDDC                                
103500        CALL W005WDK7 USING WDK7-W005WDK7                                 
103600                            WDB6-2-PCB                                    
103700                            WDK6-2-PCB                                    
103800                            WDK7-2-PCB                                    
103900        MOVE ALL '+'        TO WDK7-W005WDK7                              
103900        MOVE 'WDK721'       TO WDK7-IDSEGM                                
103900        MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                           
103900        MOVE W-IDDC         TO WDK7-IDDC-KFB                              
103900        MOVE '1'            TO WDK7-KDSEGKEY                              
103900                            IN WDK7-WDK721                                
103900        MOVE FUNCTION CURRENT-DATE(1:8)                                   
103900                            TO WDK7-DAORDSP-EJRO                          
103900        MOVE 'AUTOINS'      TO WDK7-IDUSER-ORDSP-EJRO                     
103900        CALL W005WDK7 USING WDK7-W005WDK7 WDB6-2-PCB                      
103900                                          WDK6-2-PCB                      
104000                                          WDK7-3-PCB                      
104000     .                                                                    
104100     EJECT                                                                
104200                                                                          
104300 IMS-STATUSKONTROLL SECTION.                                              
104400                                                                          
104500     SET STATUS-IX TO 1                                                   
104600     SEARCH GODK-STATUS AT END CALL FELLOG                                
104700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
104800     END-SEARCH                                                           
104900     .                                                                    
