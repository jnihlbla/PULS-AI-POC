000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W218ETA.                                                 
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   96/11/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SUB-PGM ESTIMATED TIME OF ARRIVAL                                
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001100*        PROGRAMMET LÄSER              WDK7                               
001200*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001300*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
001400*                                                                         
001500* 2011-10-13 E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1                 
001600*                                                                         
001700*                                                                         
001800     SKIP2                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000*    -- NOT CHECKED BY WY2000, HANDLES Y2000 FROM THE START               
003100                                                                          
003200 77  IDPGM                       PIC X(8)    VALUE 'W218ETA '.            
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  W-KVAVIS                    PIC S9(7)   COMP-3.                      
003600 77  WS-ANTAL-D9                  PIC S9(7)   COMP-3.                     
003700 77  TRAEFF-SW                   PIC X   VALUE 'N'.                       
003800     88  EJ-TRAEFF                       VALUE 'N'.                       
003900     88  TRAEFF                          VALUE 'J'.                       
004000                                                                          
004100 01  FELTEXT.                                                             
004200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004400                                                                          
004500 01 KODER.                                                                
004600      03 ORDER-AIR            PIC 9(3)    VALUE 601.                      
004700      03 ORDER-BOAT           PIC 9(3)    VALUE 602.                      
004800      03 INVOICING-AIR        PIC 9(3)    VALUE 603.                      
004900      03 INVOICING-BOAT       PIC 9(3)    VALUE 604.                      
005000      03 GOODS-REC-C          PIC 9(3)    VALUE 605.                      
005100      03 GOODS-REC-A          PIC 9(3)    VALUE 606.                      
005200      03 GOODS-REC-I          PIC 9(3)    VALUE 607.                      
005300      03 GOODS-REC-R          PIC 9(3)    VALUE 608.                      
005400      03 BACKORDER-AIR        PIC 9(3)    VALUE 609.                      
005500      03 BACKORDER-BOAT       PIC 9(3)    VALUE 610.                      
005600      03 ORDER-EXT-SUPPL      PIC 9(3)    VALUE 611.                      
005700      03 ORDER-CONFIRM        PIC 9(3)    VALUE 612.                      
005800      03 GOODS-REC-R-AIR      PIC 9(3)    VALUE 613.                      
005900                                                                          
006000 01  RKOD                    PIC S9(4)  VALUE +0     COMP SYNC.           
006100     EJECT                                                                
006200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES DAGENS-DATUM.                                       
006400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006700 01  DAGENS-TISEKEL              PIC 9(2).                                
006800     SKIP2                                                                
006900 01  ARBETS-AREOR.                                                        
007000     03  WINL-TIBERANK-SSAAMMDD      PIC 9(8).                            
007100     03  FILLER  REDEFINES WINL-TIBERANK-SSAAMMDD.                        
007200         05  WINL-TIBERANK-SS        PIC 9(2).                            
007300         05  WINL-TIBERANK-AAMMDD    PIC 9(6).                            
007400         05  FILLER REDEFINES WINL-TIBERANK-AAMMDD.                       
007500             07  WINL-TIBERANK-AA    PIC 9(2).                            
007600             07  FILLER              PIC 9(4).                            
007700     03  W-TIBERANK-SSAAMMDD         PIC 9(8).                            
007800     03  FILLER  REDEFINES W-TIBERANK-SSAAMMDD.                           
007900         05  W-TIBERANK-SS           PIC 9(2).                            
008000         05  W-TIBERANK-AAMMDD       PIC 9(6).                            
008100         05  FILLER REDEFINES W-TIBERANK-AAMMDD.                          
008200             07  W-TIBERANK-AA       PIC 9(2).                            
008300             07  FILLER              PIC 9(4).                            
008400     03  WS-TIBERANK-AAMMDD          PIC 9(6) VALUE ZERO.                 
008500     03  W-TILEVBSK-AAMMDD           PIC 9(6) VALUE ZERO.                 
008600     03  W-LAGSTA-DATUM              PIC 9(6) VALUE ZERO.                 
008700     03  W-TIAVRDAT-AAMMDD           PIC 9(6) VALUE ZERO.                 
008800     03  WS-FROM-DATE                PIC 9(6) VALUE ZERO.                 
008900     03  WS-KVDAGAR-INLEV            PIC S9(3) VALUE ZERO COMP-3.         
009000     EJECT                                                                
009100*      --- VALID IDDC CODES                                               
009200*                                                                         
009300*01    -COPY WWDCKONS                                                     
009400*01    -COPY WWDC99                                                       
009500*01    -COPY WWDC99 -PRE REC-                                             
009600       EJECT                                                              
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800                                                                          
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
010200     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
010300     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
010400     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
010500                                                                          
010600 01  DATUM-AAVV              PIC S9(5)               COMP-3.              
010700 01  ANTAL                   PIC S9(3)               COMP-3.              
010800     EJECT                                                                
010900*01  -COPY WORKAREA                                                       
011000     EJECT                                                                
011100*01  -COPY WDAGAREA                                                       
011200     EJECT                                                                
011300*    --- PARAMETERS FOR WZ20DAYS SUBPROGRAM                               
011400*01  -COPY WZ20DAYS                                                       
011500*                                                                         
011600*    --- TABELLER                                                         
011700 01  W-IX.                                                                
011800     03  IX                  PIC S9(3) COMP-3.                            
011900     EJECT                                                                
012000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012300     SKIP3                                                                
012400 01  NYCKLAR-TILL-DLI.                                                    
012500     03  W-IDARTNR-X.                                                     
012600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012700     03  W-KDSEGKEY-X.                                                    
012800         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
012900     03  W-IDDC-X.                                                        
013000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013100     03  W-DAINLEV-X.                                                     
013200         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
013300     03  W-WDL612KY-X.                                                    
013400         05  W-WDL612KY          PIC X(8)    VALUE SPACE.                 
013500     03  W-IDLEVNR-X.                                                     
013600         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
013700     03  W-IDLEVNR-MIN-X.                                                 
013800         05  W-IDLEVNR-MIN       PIC  X(5)   VALUE LOW-VALUE.             
013900     03  W-IDLEVNR-MAX-X.                                                 
014000         05  W-IDLEVNR-MAX       PIC  X(5)   VALUE HIGH-VALUE.            
014100     03  W-IDDC-B601-X.                                                   
014200         05  W-IDDC-B601         PIC X(2)   VALUE SPACE.                  
014300     03  W-IDDC-B616-X.                                                   
014400         05  W-IDDC-B616         PIC X(2)   VALUE SPACE.                  
014500     03  W-IDLAND-X.                                                      
014600         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
014700     03  W-KDAVROP-X.                                                     
014800         05  W-KDAVROP           PIC S9      VALUE 2    COMP-3.           
014900     03  W-WDD901KY-X.                                                    
015000         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
015100         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
015200                                                                          
015300     SKIP2                                                                
015400*    --- STATUS-KOD FRÅN IMS                                              
015500 01  STATUS-WS                   PIC XX.                                  
015600     88  SEGMENT-FINNS                       VALUE '  '.                  
015700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     SKIP2                                                                
016000 01  GODK-STATUSKODER.                                                    
016100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200     SKIP3                                                                
016300 01  SSA1                        PIC X(64).                               
016400 01  SSA2                        PIC X(64).                               
016500     EJECT                                                                
016600*    --- IMS FUNKTIONSKODER                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900*    ---  DLI INPUT-OUTPUT AREA                                           
017000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
017100 01  DLI-IO-WLARTC01.                                                     
017200*    03  -COPY WDK601                                                     
017300     EJECT                                                                
017400                                                                          
017500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
017600 01  DLI-IO-WLARTC11.                                                     
017700*    03  -COPY WDK611                                                     
017800     EJECT                                                                
017900                                                                          
018000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
018100 01  DLI-IO-WDK701.                                                       
018200*    03  -COPY WDK701                                                     
018300     EJECT                                                                
018400                                                                          
018500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
018600 01  DLI-IO-WDK711.                                                       
018700*    03  -COPY WDK711                                                     
018800     EJECT                                                                
018900                                                                          
019000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712  '.                    
019100 01  DLI-IO-WDK712.                                                       
019200*    03  -COPY WDK712                                                     
019300     EJECT                                                                
019400                                                                          
019500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC01'.                    
019600 01  DLI-IO-WLINLC01.                                                     
019700*    03  -COPY WDL601                                                     
019800     EJECT                                                                
019900                                                                          
020000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC11'.                    
020100 01  DLI-IO-WLINLC11.                                                     
020200*    03  -COPY WDL611                                                     
020300     EJECT                                                                
020400                                                                          
020500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC12'.                    
020600 01  DLI-IO-WLINLC12.                                                     
020700*    03  -COPY WDL612                                                     
020800     EJECT                                                                
020900                                                                          
021000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLLEVA01'.                    
021100 01  DLI-IO-WLLEVA01.                                                     
021200*    03  -COPY WDF101                                                     
021300     EJECT                                                                
021400                                                                          
021500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLLEVA16'.                    
021600 01  DLI-IO-WLLEVA16.                                                     
021700*    03  -COPY WDF116                                                     
021800     EJECT                                                                
021900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB616'.                      
022000 01  DLI-IO-WDB616.                                                       
022100*    03  -COPY WDB616                                                     
022200     EJECT                                                                
022300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
022400 01  DLI-IO-WDD901.                                                       
022500*    03  -COPY WDD901                                                     
022600     SKIP3                                                                
022700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
022800 01  DLI-IO-WDD902.                                                       
022900*    03  -COPY WDD902                                                     
023000     EJECT                                                                
023100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
023200 01  DLI-IO-WDD905.                                                       
023300*    03  -COPY WDD905                                                     
023400     EJECT                                                                
023500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
023600 01  DLI-IO-WDD924.                                                       
023700*    03  -COPY WDD924                                                     
023800     EJECT                                                                
023900 LINKAGE SECTION.                                                         
024000                                                                          
024100*01  -COPY W218LETA  -PRE LINK-                                           
024200     EJECT                                                                
024300*01  -COPY W0008  -PRE ARTC-                                              
024400     05  FILLER                  PIC X.                                   
024500     EJECT                                                                
024600*01  -COPY W0008  -PRE WDK7-                                              
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900*01  -COPY W0008  -PRE INLC-                                              
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025200*01  -COPY W0008  -PRE LEVA-                                              
025300     05  FILLER                  PIC X.                                   
025400     EJECT                                                                
025500*01  -COPY W0008  -PRE WDB6-                                              
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025800*01  -COPY W0008  -PRE WDD9-                                              
025900     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026100 PROCEDURE DIVISION  USING LINK-W218LETA ARTC-PCB WDK7-PCB                
026200                                         INLC-PCB LEVA-PCB                
026300                                         WDB6-PCB WDD9-PCB.               
026400 MAIN SECTION.                                                            
026500                                                                          
026600     PERFORM A-INIT                                                       
026700                                                                          
026800     EVALUATE LINK-KDCALL                                                 
026900         WHEN ORDER-AIR                                                   
027000             PERFORM B-ORDER-FLYG                                         
027100         WHEN ORDER-BOAT                                                  
027200             PERFORM C-ORDER-OVRIGA                                       
027300         WHEN INVOICING-AIR                                               
027400             PERFORM D-FAKTURERING-FLYG                                   
027500         WHEN INVOICING-BOAT                                              
027600             PERFORM E-FAKTURERING-OVRIGA                                 
027700         WHEN GOODS-REC-C                                                 
027800             PERFORM F-GOODS-REC-KOD-C                                    
027900         WHEN GOODS-REC-A                                                 
028000             PERFORM G-GOODS-REC-KOD-A                                    
028100         WHEN GOODS-REC-I                                                 
028200             PERFORM H-GOODS-REC-KOD-I                                    
028300         WHEN GOODS-REC-R                                                 
028400             PERFORM J-GOODS-REC-KOD-R                                    
028500         WHEN BACKORDER-AIR                                               
028600             PERFORM K-RESTORDER-FLYG                                     
028700         WHEN BACKORDER-BOAT                                              
028800             PERFORM L-RESTORDER-OVRIGA                                   
028900         WHEN ORDER-EXT-SUPPL                                             
029000             PERFORM M-ORDER-EXT-LEV                                      
029100         WHEN ORDER-CONFIRM                                               
029200             PERFORM N-ORDER-BEKRAFTELSE                                  
029300         WHEN GOODS-REC-R-AIR                                             
029400             PERFORM O-GOODS-REC-KOD-R-AIR                                
029500                                                                          
029600         WHEN OTHER                                                       
029700             MOVE NEJ TO LINK-SVAR-OK                                     
029800             MOVE ' W218ETA , KDCALL-VÄRDE FEL' TO FELTEXT-STR            
029900     END-EVALUATE                                                         
030000                                                                          
030100     PERFORM Z-FINIT                                                      
030200                                                                          
030300     MOVE ZERO TO RETURN-CODE                                             
030400     GOBACK                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 A-INIT SECTION.                                                          
030800                                                                          
030900     ACCEPT DAGENS-DATUM  FROM DATE                                       
031000     IF DAGENS-DATUM-AAR > 50                                             
031100        MOVE 19 TO DAGENS-TISEKEL                                         
031200     ELSE                                                                 
031300        MOVE 20 TO DAGENS-TISEKEL                                         
031400     END-IF                                                               
031500                                                                          
031600     MOVE JA    TO LINK-SVAR-OK                                           
031700     MOVE SPACE TO DAG-KDSVAR                                             
031800                                                                          
031900     MOVE ZERO  TO LINK-TIAAMMDD-SVAR                                     
032000                   LINK-TISEKEL-SVAR                                      
032100                                                                          
032200     IF LINK-TISEKEL-ANROP NOT NUMERIC OR                                 
032300       (LINK-TISEKEL-ANROP NOT = 19 AND                                   
032400        LINK-TISEKEL-ANROP NOT = 20)                                      
032500        IF LINK-TIAAMMDD-ANROP > 900000                                   
032600           MOVE 19 TO LINK-TISEKEL-ANROP                                  
032700        ELSE                                                              
032800           MOVE 20 TO LINK-TISEKEL-ANROP                                  
032900        END-IF                                                            
033000     END-IF                                                               
033100                                                                          
033200     IF LINK-KDCALL > 608 AND < 613                                       
033300        IF LINK-IDARTNR NOT NUMERIC OR                                    
033400           LINK-IDARTNR NOT > ZERO                                        
033500           MOVE NEJ TO LINK-SVAR-OK                                       
033600        END-IF                                                            
033700     END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 B-ORDER-FLYG    SECTION.                                                 
034100                                                                          
034200***  KDCALL = 601   ***                                                   
034300                                                                          
034400     PERFORM S-HAEMTA-LEDTIDER                                            
034500                                                                          
034600*    PACKTID FLYG (ARBETSTID)                  SKALL EJ MED, T.V.         
034700     MOVE LINK-IDDC-SEND        TO WS-IDDC                                
034800     IF NDC                                                               
034900        MOVE LINK-IDDC-SEND     TO WORK-IDDC                              
035000     ELSE                                                                 
035100        IF LINK-IDDC-SEND NUMERIC                                         
035200           MOVE LINK-IDDC-SEND  TO WORK-IDDC                              
035300        ELSE                                                              
035400           MOVE WC-CDC-SE       TO WORK-IDDC                              
035500        END-IF                                                            
035600     END-IF                                                               
035700     IF NDC OR CDC-SE                                                     
035800       MOVE 2                   TO WORK-KDCALL                            
035900       MOVE LINK-TIAAMMDD-ANROP TO WORK-TIAAMMDD-FOM                      
036000       MOVE REF-KVDLTID-AIRPAC  TO WORK-KVWORKD                           
036100       ADD +1                   TO WORK-KVWORKD                           
036200                                                                          
036300       CALL WORKDAY USING WORK-KDCALL                                     
036400                 WORK-DATE-AREA WORK-KDSVAR                               
036500                                                                          
036600       IF WORK-KDSVAR-OK                                                  
036700         MOVE WORK-TIAAMMDD-TOM  TO LINK-TIAAMMDD-SVAR                    
036800         MOVE LINK-TISEKEL-ANROP TO LINK-TISEKEL-SVAR                     
036900       ELSE                                                               
037000         MOVE NEJ TO LINK-SVAR-OK                                         
037100       END-IF                                                             
037200* *    FÖRBEREDANDE FLYTT FÖR NÄSTA ANROP                                 
037300       MOVE LINK-TISEKEL-SVAR  TO DAG-TISEKEL-FOM                         
037400       MOVE LINK-TIAAMMDD-SVAR TO DAG-TIAAMMDD-FOM                        
037500     ELSE                                                                 
037600       MOVE LINK-TISEKEL-ANROP TO DAG-TISEKEL-FOM                         
037700       MOVE LINK-TIAAMMDD-ANROP TO DAG-TIAAMMDD-FOM                       
037800     END-IF                                                               
037900*                                                                         
038000*    TRANSPORTTID FLYG (KALENDERTID)                                      
038100     MOVE 2                  TO DAG-KDCALL                                
038200     MOVE REF-KVDLTID-AIRTRP TO DAG-KVKALDAG                              
038300     ADD +1                  TO DAG-KVKALDAG                              
038400*                                                                         
038500                                                                          
038600     CALL WDAGKONV USING DAG-KDCALL                                       
038700               DAG-DATUM-AREA DAG-KDSVAR                                  
038800                                                                          
038900     IF DAG-KDSVAR = SPACE                                                
039000       MOVE DAG-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                        
039100       MOVE DAG-TISEKEL-TOM  TO LINK-TISEKEL-SVAR                         
039200     ELSE                                                                 
039300       MOVE NEJ TO LINK-SVAR-OK                                           
039400     END-IF                                                               
039500                                                                          
039600*    INLÄGGNINGSTID FLYG (ARBETSTID)                                      
039700     MOVE 2                  TO WORK-KDCALL                               
039800     MOVE LINK-IDDC-REC      TO WORK-IDDC                                 
039900     MOVE LINK-TIAAMMDD-SVAR TO WORK-TIAAMMDD-FOM                         
040000     MOVE REF-KVDLTID-AIRINS TO WORK-KVWORKD                              
040100     ADD +1                  TO WORK-KVWORKD                              
040200                                                                          
040300     CALL WORKDAY USING WORK-KDCALL                                       
040400               WORK-DATE-AREA WORK-KDSVAR                                 
040500                                                                          
040600     IF WORK-KDSVAR-OK                                                    
040700       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
040800     ELSE                                                                 
040900       MOVE NEJ TO LINK-SVAR-OK                                           
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 C-ORDER-OVRIGA  SECTION.                                                 
041400                                                                          
041500***  KDCALL = 602   ***                                                   
041600                                                                          
041700     PERFORM S-HAEMTA-LEDTIDER                                            
041800                                                                          
041900*    PACKTID BÅT  (ARBETSTID)                                             
042000     MOVE 2                   TO WORK-KDCALL                              
042100     MOVE LINK-IDDC-SEND      TO WS-IDDC                                  
042200     IF NDC                                                               
042300        MOVE LINK-IDDC-SEND   TO WORK-IDDC                                
042400     ELSE                                                                 
042500        IF LINK-IDDC-SEND NUMERIC                                         
042600           MOVE LINK-IDDC-SEND                                            
042700                              TO WORK-IDDC                                
042800        ELSE                                                              
042900           MOVE WC-CDC-SE     TO WORK-IDDC                                
043000        END-IF                                                            
043100     END-IF                                                               
043200     MOVE LINK-TIAAMMDD-ANROP TO WORK-TIAAMMDD-FOM                        
043300     MOVE REF-KVDLTID-BOATPAC TO WORK-KVWORKD                             
043400     ADD +1                   TO WORK-KVWORKD                             
043500                                                                          
043600     CALL WORKDAY USING WORK-KDCALL                                       
043700               WORK-DATE-AREA WORK-KDSVAR                                 
043800                                                                          
043900     IF WORK-KDSVAR-OK                                                    
044000       MOVE WORK-TIAAMMDD-TOM  TO LINK-TIAAMMDD-SVAR                      
044100       MOVE LINK-TISEKEL-ANROP TO LINK-TISEKEL-SVAR                       
044200     ELSE                                                                 
044300       MOVE NEJ TO LINK-SVAR-OK                                           
044400     END-IF                                                               
044500*                                                                         
044600*    TRANSPORTTID BÅT  (KALENDERTID)                                      
044700     MOVE 2                    TO DAG-KDCALL                              
044800     MOVE LINK-TISEKEL-SVAR    TO DAG-TISEKEL-FOM                         
044900     MOVE LINK-TIAAMMDD-SVAR   TO DAG-TIAAMMDD-FOM                        
045000     MOVE REF-KVDLTID-BOATTRP  TO DAG-KVKALDAG                            
045100     ADD +1                    TO DAG-KVKALDAG                            
045200                                                                          
045300     CALL WDAGKONV USING DAG-KDCALL                                       
045400               DAG-DATUM-AREA DAG-KDSVAR                                  
045500                                                                          
045600     IF DAG-KDSVAR = SPACE                                                
045700       MOVE DAG-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                        
045800       MOVE DAG-TISEKEL-TOM  TO LINK-TISEKEL-SVAR                         
045900     ELSE                                                                 
046000       MOVE NEJ TO LINK-SVAR-OK                                           
046100     END-IF                                                               
046200                                                                          
046300*    HAMN TILL GRIND     (ARBETSTID)                                      
046400     MOVE 2                   TO WORK-KDCALL                              
046500     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
046600     MOVE LINK-TIAAMMDD-SVAR  TO WORK-TIAAMMDD-FOM                        
046700     MOVE REF-KVDLTID-BOAT2DC TO WORK-KVWORKD                             
046800     ADD +1                   TO WORK-KVWORKD                             
046900                                                                          
047000     CALL WORKDAY USING WORK-KDCALL                                       
047100               WORK-DATE-AREA WORK-KDSVAR                                 
047200                                                                          
047300     IF WORK-KDSVAR-OK                                                    
047400       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
047500     ELSE                                                                 
047600       MOVE NEJ TO LINK-SVAR-OK                                           
047700     END-IF                                                               
047800                                                                          
047900*    INLÄGGNINGSTID BÅT  (ARBETSTID)                                      
048000     MOVE 2                   TO WORK-KDCALL                              
048100     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
048200     MOVE LINK-TIAAMMDD-SVAR  TO WORK-TIAAMMDD-FOM                        
048300     MOVE REF-KVDLTID-BOATINS TO WORK-KVWORKD                             
048400     ADD +1                   TO WORK-KVWORKD                             
048500                                                                          
048600     CALL WORKDAY USING WORK-KDCALL                                       
048700               WORK-DATE-AREA WORK-KDSVAR                                 
048800                                                                          
048900     IF WORK-KDSVAR-OK                                                    
049000       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
049100     ELSE                                                                 
049200       MOVE NEJ TO LINK-SVAR-OK                                           
049300     END-IF                                                               
049400     .                                                                    
049500     EJECT                                                                
049600 D-FAKTURERING-FLYG  SECTION.                                             
049700                                                                          
049800***  KDCALL = 603   ***                                                   
049900                                                                          
050000     PERFORM S-HAEMTA-LEDTIDER                                            
050100                                                                          
050200*    TRANSPORTTID FLYG (KALENDERTID)                                      
050300     MOVE 2                   TO DAG-KDCALL                               
050400     MOVE LINK-TISEKEL-ANROP  TO DAG-TISEKEL-FOM                          
050500     MOVE LINK-TIAAMMDD-ANROP TO DAG-TIAAMMDD-FOM                         
050600     MOVE REF-KVDLTID-AIRTRP  TO DAG-KVKALDAG                             
050700     ADD +1                   TO DAG-KVKALDAG                             
050800                                                                          
050900*                                                                         
051000     MOVE LINK-IDDC-REC       TO WS-IDDC                                  
051100                                                                          
051200     CALL WDAGKONV USING DAG-KDCALL                                       
051300               DAG-DATUM-AREA DAG-KDSVAR                                  
051400                                                                          
051500     IF DAG-KDSVAR = SPACE                                                
051600       MOVE DAG-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                        
051700       MOVE DAG-TISEKEL-TOM  TO LINK-TISEKEL-SVAR                         
051800     ELSE                                                                 
051900       MOVE NEJ TO LINK-SVAR-OK                                           
052000     END-IF                                                               
052100                                                                          
052200*    INLÄGGNINGSTID FLYG (ARBETSTID)                                      
052300     MOVE 2                  TO WORK-KDCALL                               
052400     MOVE LINK-IDDC-REC      TO WORK-IDDC                                 
052500     MOVE LINK-TIAAMMDD-SVAR TO WORK-TIAAMMDD-FOM                         
052600     MOVE REF-KVDLTID-AIRINS TO WORK-KVWORKD                              
052700     ADD +1                  TO WORK-KVWORKD                              
052800*                                                                         
052900     MOVE LINK-IDDC-REC      TO WS-IDDC                                   
053000                                                                          
053100     CALL WORKDAY USING WORK-KDCALL                                       
053200               WORK-DATE-AREA WORK-KDSVAR                                 
053300                                                                          
053400     IF WORK-KDSVAR-OK                                                    
053500       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
053600     ELSE                                                                 
053700       MOVE NEJ TO LINK-SVAR-OK                                           
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 E-FAKTURERING-OVRIGA SECTION.                                            
054200                                                                          
054300***  KDCALL = 604   ***                                                   
054400                                                                          
054500     PERFORM S-HAEMTA-LEDTIDER                                            
054600                                                                          
054700*    TRANSPORTTID BÅT  (KALENDERTID)                                      
054800     MOVE 2                   TO DAG-KDCALL                               
054900     MOVE LINK-TISEKEL-ANROP  TO DAG-TISEKEL-FOM                          
055000     MOVE LINK-TIAAMMDD-ANROP TO DAG-TIAAMMDD-FOM                         
055100     MOVE REF-KVDLTID-BOATTRP TO DAG-KVKALDAG                             
055200     ADD +1                   TO DAG-KVKALDAG                             
055300                                                                          
055400     CALL WDAGKONV USING DAG-KDCALL                                       
055500               DAG-DATUM-AREA DAG-KDSVAR                                  
055600                                                                          
055700     IF DAG-KDSVAR = SPACE                                                
055800       MOVE DAG-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                        
055900       MOVE DAG-TISEKEL-TOM  TO LINK-TISEKEL-SVAR                         
056000     ELSE                                                                 
056100       MOVE NEJ TO LINK-SVAR-OK                                           
056200     END-IF                                                               
056300                                                                          
056400*    HAMN TILL GRIND     (ARBETSTID)                                      
056500     MOVE 2                   TO WORK-KDCALL                              
056600     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
056700     MOVE LINK-TIAAMMDD-SVAR  TO WORK-TIAAMMDD-FOM                        
056800     MOVE REF-KVDLTID-BOAT2DC TO WORK-KVWORKD                             
056900     ADD +1                   TO WORK-KVWORKD                             
057000                                                                          
057100     CALL WORKDAY USING WORK-KDCALL                                       
057200               WORK-DATE-AREA WORK-KDSVAR                                 
057300                                                                          
057400     IF WORK-KDSVAR-OK                                                    
057500       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
057600     ELSE                                                                 
057700       MOVE NEJ TO LINK-SVAR-OK                                           
057800     END-IF                                                               
057900                                                                          
058000*    INLÄGGNINGSTID BÅT  (ARBETSTID)                                      
058100     MOVE 2                   TO WORK-KDCALL                              
058200     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
058300     MOVE LINK-TIAAMMDD-SVAR  TO WORK-TIAAMMDD-FOM                        
058400     MOVE REF-KVDLTID-BOATINS TO WORK-KVWORKD                             
058500     ADD +1                   TO WORK-KVWORKD                             
058600                                                                          
058700     CALL WORKDAY USING WORK-KDCALL                                       
058800               WORK-DATE-AREA WORK-KDSVAR                                 
058900                                                                          
059000     IF WORK-KDSVAR-OK                                                    
059100       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
059200     ELSE                                                                 
059300       MOVE NEJ TO LINK-SVAR-OK                                           
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 F-GOODS-REC-KOD-C   SECTION.                                             
059800                                                                          
059900***  KDCALL = 605   ***                                                   
060000                                                                          
060100     PERFORM S-HAEMTA-LEDTIDER                                            
060200                                                                          
060300*    TULLTID             (ARBETSTID)                                      
060400     MOVE   2                 TO WORK-KDCALL                              
060500     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
060600     MOVE LINK-TIAAMMDD-ANROP TO WORK-TIAAMMDD-FOM                        
060700     MOVE REF-KVDLTID-CUST    TO WORK-KVWORKD                             
060800     ADD +1                   TO WORK-KVWORKD                             
060900                                                                          
061000     CALL WORKDAY USING WORK-KDCALL                                       
061100               WORK-DATE-AREA WORK-KDSVAR                                 
061200                                                                          
061300     IF WORK-KDSVAR-OK                                                    
061400       MOVE WORK-TIAAMMDD-TOM   TO LINK-TIAAMMDD-SVAR                     
061500       MOVE LINK-TISEKEL-ANROP  TO LINK-TISEKEL-SVAR                      
061600     ELSE                                                                 
061700       MOVE NEJ TO LINK-SVAR-OK                                           
061800     END-IF                                                               
061900                                                                          
062000*    LIGGTID I TULL      (ARBETSTID)                                      
062100     MOVE   2                  TO WORK-KDCALL                             
062200     MOVE LINK-IDDC-REC        TO WORK-IDDC                               
062300     MOVE LINK-TIAAMMDD-SVAR   TO WORK-TIAAMMDD-FOM                       
062400     MOVE REF-KVDLTID-CUSTWAIT TO WORK-KVWORKD                            
062500     ADD +1                    TO WORK-KVWORKD                            
062600                                                                          
062700     CALL WORKDAY USING WORK-KDCALL                                       
062800               WORK-DATE-AREA WORK-KDSVAR                                 
062900                                                                          
063000     IF WORK-KDSVAR-OK                                                    
063100       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
063200     ELSE                                                                 
063300       MOVE NEJ TO LINK-SVAR-OK                                           
063400     END-IF                                                               
063500     EJECT                                                                
063600*    TULL TILL GRIND     (ARBETSTID)                                      
063700     MOVE   2                 TO WORK-KDCALL                              
063800     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
063900     MOVE LINK-TIAAMMDD-SVAR  TO WORK-TIAAMMDD-FOM                        
064000     MOVE REF-KVDLTID-CUST2DC TO WORK-KVWORKD                             
064100     ADD +1                   TO WORK-KVWORKD                             
064200                                                                          
064300     CALL WORKDAY USING WORK-KDCALL                                       
064400               WORK-DATE-AREA WORK-KDSVAR                                 
064500                                                                          
064600     IF WORK-KDSVAR-OK                                                    
064700       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
064800     ELSE                                                                 
064900       MOVE NEJ TO LINK-SVAR-OK                                           
065000     END-IF                                                               
065100                                                                          
065200*    INLÄGGNINGSTID BÅT  (ARBETSTID)                                      
065300     MOVE   2                 TO WORK-KDCALL                              
065400     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
065500     MOVE LINK-TIAAMMDD-SVAR  TO WORK-TIAAMMDD-FOM                        
065600     MOVE REF-KVDLTID-BOATINS TO WORK-KVWORKD                             
065700     ADD +1                   TO WORK-KVWORKD                             
065800                                                                          
065900     CALL WORKDAY USING WORK-KDCALL                                       
066000               WORK-DATE-AREA WORK-KDSVAR                                 
066100                                                                          
066200     IF WORK-KDSVAR-OK                                                    
066300       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
066400     ELSE                                                                 
066500       MOVE NEJ TO LINK-SVAR-OK                                           
066600     END-IF                                                               
066700     .                                                                    
066800     EJECT                                                                
066900 G-GOODS-REC-KOD-A   SECTION.                                             
067000                                                                          
067100***  KDCALL = 606   ***                                                   
067200                                                                          
067300     PERFORM S-HAEMTA-LEDTIDER                                            
067400                                                                          
067500*    LIGGTID I TULL      (ARBETSTID)                                      
067600     MOVE   2                  TO WORK-KDCALL                             
067700     MOVE LINK-IDDC-REC        TO WORK-IDDC                               
067800     MOVE LINK-TIAAMMDD-ANROP  TO WORK-TIAAMMDD-FOM                       
067900     MOVE REF-KVDLTID-CUSTWAIT TO WORK-KVWORKD                            
068000     ADD +1                    TO WORK-KVWORKD                            
068100                                                                          
068200     CALL WORKDAY USING WORK-KDCALL                                       
068300               WORK-DATE-AREA WORK-KDSVAR                                 
068400                                                                          
068500     IF WORK-KDSVAR-OK                                                    
068600       MOVE WORK-TIAAMMDD-TOM   TO LINK-TIAAMMDD-SVAR                     
068700       MOVE LINK-TISEKEL-ANROP  TO LINK-TISEKEL-SVAR                      
068800     ELSE                                                                 
068900       MOVE NEJ TO LINK-SVAR-OK                                           
069000     END-IF                                                               
069100                                                                          
069200*    TULL TILL GRIND     (ARBETSTID)                                      
069300     MOVE   2                 TO WORK-KDCALL                              
069400     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
069500     MOVE LINK-TIAAMMDD-SVAR  TO WORK-TIAAMMDD-FOM                        
069600     MOVE REF-KVDLTID-CUST2DC TO WORK-KVWORKD                             
069700     ADD +1                   TO WORK-KVWORKD                             
069800                                                                          
069900     CALL WORKDAY USING WORK-KDCALL                                       
070000               WORK-DATE-AREA WORK-KDSVAR                                 
070100                                                                          
070200     IF WORK-KDSVAR-OK                                                    
070300       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
070400     ELSE                                                                 
070500       MOVE NEJ TO LINK-SVAR-OK                                           
070600     END-IF                                                               
070700     EJECT                                                                
070800*    INLÄGGNINGSTID BÅT  (ARBETSTID)                                      
070900     MOVE   2                 TO WORK-KDCALL                              
071000     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
071100     MOVE LINK-TIAAMMDD-SVAR  TO WORK-TIAAMMDD-FOM                        
071200     MOVE REF-KVDLTID-BOATINS TO WORK-KVWORKD                             
071300     ADD +1                   TO WORK-KVWORKD                             
071400                                                                          
071500     CALL WORKDAY USING WORK-KDCALL                                       
071600               WORK-DATE-AREA WORK-KDSVAR                                 
071700                                                                          
071800     IF WORK-KDSVAR-OK                                                    
071900       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
072000     ELSE                                                                 
072100       MOVE NEJ TO LINK-SVAR-OK                                           
072200     END-IF                                                               
072300     .                                                                    
072400     EJECT                                                                
072500 H-GOODS-REC-KOD-I   SECTION.                                             
072600                                                                          
072700***  KDCALL = 607   ***                                                   
072800                                                                          
072900     PERFORM S-HAEMTA-LEDTIDER                                            
073000                                                                          
073100*    TULL TILL GRIND     (ARBETSTID)                                      
073200     MOVE   2                 TO WORK-KDCALL                              
073300     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
073400     MOVE LINK-TIAAMMDD-ANROP TO WORK-TIAAMMDD-FOM                        
073500     MOVE REF-KVDLTID-CUST2DC TO WORK-KVWORKD                             
073600     ADD +1                   TO WORK-KVWORKD                             
073700                                                                          
073800     CALL WORKDAY USING WORK-KDCALL                                       
073900               WORK-DATE-AREA WORK-KDSVAR                                 
074000                                                                          
074100     IF WORK-KDSVAR-OK                                                    
074200       MOVE WORK-TIAAMMDD-TOM  TO LINK-TIAAMMDD-SVAR                      
074300       MOVE LINK-TISEKEL-ANROP TO LINK-TISEKEL-SVAR                       
074400     ELSE                                                                 
074500       MOVE NEJ TO LINK-SVAR-OK                                           
074600     END-IF                                                               
074700                                                                          
074800*    INLÄGGNINGSTID BÅT  (ARBETSTID)                                      
074900     MOVE   2                 TO WORK-KDCALL                              
075000     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
075100     MOVE LINK-TIAAMMDD-SVAR  TO WORK-TIAAMMDD-FOM                        
075200     MOVE REF-KVDLTID-BOATINS TO WORK-KVWORKD                             
075300     ADD +1                   TO WORK-KVWORKD                             
075400                                                                          
075500     CALL WORKDAY USING WORK-KDCALL                                       
075600               WORK-DATE-AREA WORK-KDSVAR                                 
075700                                                                          
075800     IF WORK-KDSVAR-OK                                                    
075900       MOVE WORK-TIAAMMDD-TOM TO LINK-TIAAMMDD-SVAR                       
076000     ELSE                                                                 
076100       MOVE NEJ TO LINK-SVAR-OK                                           
076200     END-IF                                                               
076300     .                                                                    
076400     EJECT                                                                
076500 J-GOODS-REC-KOD-R   SECTION.                                             
076600                                                                          
076700***  KDCALL = 608   ***                                                   
076800                                                                          
076900     MOVE LINK-IDDC-REC TO WS-IDDC                                        
077000     IF (LINK-IDDC-SEND = SPACE) AND NDC-CN                               
077100       MOVE LINK-IDARTNR TO W-IDARTNR                                     
077200       MOVE 'CN'         TO W-IDLAND                                      
077300       PERFORM IMS-GU-WDK712                                              
077400       IF SEGMENT-FINNS                                                   
077500         MOVE LART-KVDAGAR-INLEV  TO WS-KVDAGAR-INLEV                     
077600       ELSE                                                               
077700         MOVE ZERO                TO WS-KVDAGAR-INLEV                     
077800       END-IF                                                             
077900     ELSE                                                                 
078000       PERFORM S-HAEMTA-LEDTIDER                                          
078100       MOVE REF-KVDLTID-BOATINS   TO WS-KVDAGAR-INLEV                     
078200     END-IF                                                               
078300                                                                          
078400*    INLÄGGNINGSTID BÅT  (ARBETSTID)                                      
078500     MOVE   2                 TO WORK-KDCALL                              
078600     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
078700     MOVE LINK-TIAAMMDD-ANROP TO WORK-TIAAMMDD-FOM                        
078800     MOVE WS-KVDAGAR-INLEV    TO WORK-KVWORKD                             
078900     ADD +1                   TO WORK-KVWORKD                             
079000                                                                          
079100     CALL WORKDAY USING WORK-KDCALL                                       
079200               WORK-DATE-AREA WORK-KDSVAR                                 
079300                                                                          
079400     IF WORK-KDSVAR-OK                                                    
079500       MOVE WORK-TIAAMMDD-TOM  TO LINK-TIAAMMDD-SVAR                      
079600       MOVE LINK-TISEKEL-ANROP TO LINK-TISEKEL-SVAR                       
079700     ELSE                                                                 
079800       MOVE NEJ TO LINK-SVAR-OK                                           
079900     END-IF                                                               
080000     .                                                                    
080100     EJECT                                                                
080200 K-RESTORDER-FLYG    SECTION.                                             
080300                                                                          
080400***  KDCALL = 609   ***                                                   
080500                                                                          
080600     MOVE LINK-IDARTNR TO W-IDARTNR                                       
080700     PERFORM IMS-GU-ARTC-CLAG                                             
080800     IF SEGMENT-FINNS                                                     
080900        MOVE CLAG-TIDISPIN TO LINK-TIAAMMDD-ANROP                         
081000        IF CLAG-TIDISPIN > 900000                                         
081100           MOVE 19 TO LINK-TISEKEL-ANROP                                  
081200        ELSE                                                              
081300           MOVE 20 TO LINK-TISEKEL-ANROP                                  
081400        END-IF                                                            
081500        IF CLAG-TIDISPIN = ZERO                                           
081600           MOVE ZERO TO LINK-TISEKEL-ANROP                                
081700        END-IF                                                            
081800        IF LINK-TISEKEL-ANROP   < DAGENS-TISEKEL  OR                      
081900          (LINK-TISEKEL-ANROP   = DAGENS-TISEKEL  AND                     
082000           LINK-TIAAMMDD-ANROP  < DAGENS-DATUM)                           
082100           MOVE DAGENS-DATUM   TO LINK-TIAAMMDD-ANROP                     
082200           MOVE DAGENS-TISEKEL TO LINK-TISEKEL-ANROP                      
082300        END-IF                                                            
082400     ELSE                                                                 
082500        MOVE NEJ TO LINK-SVAR-OK                                          
082600     END-IF                                                               
082700                                                                          
082800     IF LINK-SVAR-OK = JA                                                 
082900        PERFORM B-ORDER-FLYG                                              
083000     END-IF                                                               
083100     .                                                                    
083200     EJECT                                                                
083300 L-RESTORDER-OVRIGA  SECTION.                                             
083400                                                                          
083500***  KDCALL = 610   ***                                                   
083600                                                                          
083700     MOVE LINK-IDARTNR TO W-IDARTNR                                       
083800     PERFORM IMS-GU-ARTC-CLAG                                             
083900     IF SEGMENT-FINNS                                                     
084000        MOVE CLAG-TIDISPIN TO LINK-TIAAMMDD-ANROP                         
084100        IF CLAG-TIDISPIN > 900000                                         
084200           MOVE 19 TO LINK-TISEKEL-ANROP                                  
084300        ELSE                                                              
084400           MOVE 20 TO LINK-TISEKEL-ANROP                                  
084500        END-IF                                                            
084600        IF CLAG-TIDISPIN = ZERO                                           
084700           MOVE ZERO TO LINK-TISEKEL-ANROP                                
084800        END-IF                                                            
084900        IF LINK-TISEKEL-ANROP   < DAGENS-TISEKEL  OR                      
085000          (LINK-TISEKEL-ANROP   = DAGENS-TISEKEL  AND                     
085100           LINK-TIAAMMDD-ANROP  < DAGENS-DATUM)                           
085200           MOVE DAGENS-DATUM   TO LINK-TIAAMMDD-ANROP                     
085300           MOVE DAGENS-TISEKEL TO LINK-TISEKEL-ANROP                      
085400        END-IF                                                            
085500     ELSE                                                                 
085600        MOVE NEJ TO LINK-SVAR-OK                                          
085700     END-IF                                                               
085800                                                                          
085900     IF LINK-SVAR-OK = JA                                                 
086000        PERFORM C-ORDER-OVRIGA                                            
086100     END-IF                                                               
086200     .                                                                    
086300     EJECT                                                                
086400 M-ORDER-EXT-LEV     SECTION.                                             
086500                                                                          
086600***  KDCALL = 611   ***                                                   
086700                                                                          
086800     MOVE LINK-IDARTNR  TO W-IDARTNR                                      
086900     MOVE LINK-IDDC-REC TO W-IDDC                                         
087000     MOVE LINK-TIAAMMDD-ANROP  TO WS-FROM-DATE                            
087100     PERFORM IMS-GU-WDK711                                                
087200     IF SEGMENT-FINNS                                                     
087300        MOVE SLAG-IDLEVNR TO W-IDLEVNR                                    
087400        IF SLAG-KVDAGAR-MANLT > ZERO                                      
087500          MOVE SLAG-KVDAGAR-MANLT    TO DAYS-KVDAYS                       
087600          MOVE 'YYMMDD'              TO DAYS-KDDATFMT1                    
087700          MOVE WS-FROM-DATE          TO DAYS-TIDATE1                      
087800                                                                          
087900          MOVE 'YYMMDD'              TO DAYS-KDDATFMT2                    
088000          MOVE SPACE                 TO DAYS-TIDATE2                      
088100                                                                          
088200          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
088300*                                                                         
088400          IF DAYS-KDRC = +0                                               
088500            MOVE 20                  TO LINK-TISEKEL-SVAR                 
088600            MOVE DAYS-TIDATE2 (1:6)  TO LINK-TIAAMMDD-SVAR                
088700          ELSE                                                            
088800            MOVE NEJ TO LINK-SVAR-OK                                      
088900          END-IF                                                          
089000        ELSE                                                              
089100          PERFORM IMS-GU-LEVA-NDC                                         
089200          IF SEGMENT-FINNS                                                
089300*   HEMTAGNING TILL MOT. DC   (ARBETSTID)                                 
089400             MOVE NDC-KVDAGAR-TBT     TO DAYS-KVDAYS                      
089500             MOVE 'YYMMDD'           TO DAYS-KDDATFMT1                    
089600             MOVE WS-FROM-DATE        TO DAYS-TIDATE1                     
089700                                                                          
089800             MOVE 'YYMMDD'           TO DAYS-KDDATFMT2                    
089900             MOVE SPACE              TO DAYS-TIDATE2                      
090000                                                                          
090100             CALL WZ20DAYS USING DAYS-WZ20DAYS                            
090200*                                                                         
090300             IF DAYS-KDRC = +0                                            
090400               MOVE 20                 TO LINK-TISEKEL-SVAR               
090500               MOVE DAYS-TIDATE2 (1:6) TO LINK-TIAAMMDD-SVAR              
090600             ELSE                                                         
090700               MOVE NEJ TO LINK-SVAR-OK                                   
090800             END-IF                                                       
090900          ELSE                                                            
091000******       MOVE NEJ TO LINK-SVAR-OK                                     
091100             MOVE 999999              TO LINK-TIAAMMDD-SVAR               
091200             MOVE 99                  TO LINK-TISEKEL-SVAR                
091300          END-IF                                                          
091400        END-IF                                                            
091500     ELSE                                                                 
091600       MOVE NEJ TO LINK-SVAR-OK                                           
091700     END-IF                                                               
091800     .                                                                    
091900     EJECT                                                                
092000 N-ORDER-BEKRAFTELSE SECTION.                                             
092100                                                                          
092200***  KDCALL = 612   ***                                                   
092300                                                                          
092400     MOVE LINK-IDDC-REC TO WS-IDDC                                        
092500                                                                          
092600     IF NDC-CN                                                            
092700       PERFORM NC-ORDER-BEKR-NDC-CN                                       
092800     ELSE                                                                 
092900       MOVE 99999999            TO W-TIBERANK-SSAAMMDD                    
093000       MOVE +0                  TO W-KVAVIS                               
093100       MOVE LINK-IDARTNR        TO W-IDARTNR                              
093200       PERFORM IMS-GU-INLC-ROT                                            
093300       IF SEGMENT-FINNS                                                   
093400         PERFORM NA-SOEK-TIBERANK-WDL611                                  
093500         IF W-TIBERANK-SSAAMMDD NOT = 99999999                            
093600           MOVE W-TIBERANK-AAMMDD TO LINK-TIAAMMDD-SVAR                   
093700           MOVE W-TIBERANK-SS   TO LINK-TISEKEL-SVAR                      
093800           MOVE W-KVAVIS        TO LINK-KVAVIS-ETA                        
093900         ELSE                                                             
094000           IF LINK-IDLEVNR = SPACE                                        
094100             MOVE LINK-IDARTNR TO W-IDARTNR                               
094200             MOVE LINK-IDDC-REC TO W-IDDC                                 
094300             PERFORM IMS-GU-WDK711                                        
094400             IF SEGMENT-FINNS                                             
094500               PERFORM NB-LEVNR-EJ-1441                                   
094600             ELSE                                                         
094700               MOVE NEJ           TO LINK-SVAR-OK                         
094800             END-IF                                                       
094900           ELSE                                                           
095000             IF LINK-IDLEVNR NOT = '1441 ' OR 'BP2TW'                     
095100               PERFORM NB-LEVNR-EJ-1441                                   
095200             ELSE                                                         
095300               MOVE NEJ           TO LINK-SVAR-OK                         
095400             END-IF                                                       
095500           END-IF                                                         
095600         END-IF                                                           
095700       ELSE                                                               
095800         MOVE NEJ               TO LINK-SVAR-OK                           
095900       END-IF                                                             
096000     END-IF                                                               
096100     .                                                                    
096200     EJECT                                                                
096300 NA-SOEK-TIBERANK-WDL611 SECTION.                                         
096400                                                                          
096500     MOVE LINK-IDDC-REC                  TO W-IDDC                        
096600     PERFORM IMS-GNP-INLC-INL                                             
096700     PERFORM UNTIL SEGMENT-SAKNAS                                         
096800       IF INL-IDPTYP = 'R30' OR '310'                                     
096900         MOVE INL-TIBERANK               TO WINL-TIBERANK-AAMMDD          
097000         IF WINL-TIBERANK-AA > 50                                         
097100           MOVE 19                       TO WINL-TIBERANK-SS              
097200         ELSE                                                             
097300           MOVE 20                       TO WINL-TIBERANK-SS              
097400         END-IF                                                           
097500         IF (WINL-TIBERANK-AAMMDD >= LINK-TIAAMMDD-ANROP AND              
097600             WINL-TIBERANK-SS      = LINK-TISEKEL-ANROP)  OR              
097700            (WINL-TIBERANK-SS     >  LINK-TISEKEL-ANROP)                  
097800           IF WINL-TIBERANK-SSAAMMDD <  W-TIBERANK-SSAAMMDD               
097900             MOVE WINL-TIBERANK-SSAAMMDD TO W-TIBERANK-SSAAMMDD           
098000             MOVE  INL-KVAVIS            TO W-KVAVIS                      
098100           END-IF                                                         
098200         END-IF                                                           
098300       END-IF                                                             
098400       PERFORM IMS-GNP-INLC-INL                                           
098500     END-PERFORM                                                          
098600     .                                                                    
098700     EJECT                                                                
098800 NB-LEVNR-EJ-1441      SECTION.                                           
098900                                                                          
099000     PERFORM IMS-GNP-INLC-ORD                                             
099100     PERFORM UNTIL SEGMENT-SAKNAS                                         
099200       IF ORD-IDDC     = LINK-IDDC-REC AND                                
099300          ORD-TIBERANK > ZERO                                             
099400         MOVE ORD-TIBERANK               TO WINL-TIBERANK-AAMMDD          
099500         IF WINL-TIBERANK-AA > 50                                         
099600           MOVE 19                       TO WINL-TIBERANK-SS              
099700         ELSE                                                             
099800           MOVE 20                       TO WINL-TIBERANK-SS              
099900         END-IF                                                           
100000         IF (WINL-TIBERANK-AAMMDD >= LINK-TIAAMMDD-ANROP AND              
100100             WINL-TIBERANK-SS      = LINK-TISEKEL-ANROP) OR               
100200            (WINL-TIBERANK-SS     >  LINK-TISEKEL-ANROP)                  
100300           IF WINL-TIBERANK-SSAAMMDD < W-TIBERANK-SSAAMMDD                
100400             MOVE WINL-TIBERANK-SSAAMMDD TO W-TIBERANK-SSAAMMDD           
100500             MOVE ORD-KVAVIS             TO W-KVAVIS                      
100600           END-IF                                                         
100700         END-IF                                                           
100800       END-IF                                                             
100900       PERFORM IMS-GNP-INLC-ORD                                           
101000     END-PERFORM                                                          
101100                                                                          
101200     IF W-TIBERANK-SSAAMMDD NOT = 99999999                                
101300       MOVE W-TIBERANK-AAMMDD TO LINK-TIAAMMDD-SVAR                       
101400       MOVE W-TIBERANK-SS     TO LINK-TISEKEL-SVAR                        
101500       MOVE W-KVAVIS          TO LINK-KVAVIS-ETA                          
101600     ELSE                                                                 
101700       MOVE NEJ               TO LINK-SVAR-OK                             
101800     END-IF                                                               
101900     .                                                                    
102000     EJECT                                                                
102100                                                                          
102200 NC-ORDER-BEKR-NDC-CN    SECTION.                                         
102300                                                                          
102400     MOVE LINK-IDARTNR TO W-IDARTNR                                       
102500                          W-IDARTNR-D9                                    
102600     MOVE LINK-IDDC-REC TO W-IDDC                                         
102700                           W-IDDC-D9                                      
102800     PERFORM IMS-GU-WDK711                                                
102900     MOVE NEJ TO TRAEFF-SW                                                
103000     IF SEGMENT-FINNS                                                     
103100       PERFORM NCA-SOEK-TIBERANK-WDL611                                   
103200       PERFORM NCB-SOEK-LAGSTA-WDD9                                       
103300       IF EJ-TRAEFF                                                       
103400         MOVE NEJ               TO LINK-SVAR-OK                           
103500       ELSE                                                               
103600         IF WS-TIBERANK-AAMMDD < W-LAGSTA-DATUM                           
103700           MOVE WS-TIBERANK-AAMMDD TO LINK-TIAAMMDD-SVAR                  
103800           MOVE W-KVAVIS         TO LINK-KVAVIS-ETA                       
103900         ELSE                                                             
104000           MOVE W-LAGSTA-DATUM   TO LINK-TIAAMMDD-SVAR                    
104100           MOVE WS-ANTAL-D9      TO LINK-KVAVIS-ETA                       
104200         END-IF                                                           
104300         MOVE 20   TO LINK-TISEKEL-SVAR                                   
104400       END-IF                                                             
104500     ELSE                                                                 
104600       MOVE NEJ               TO LINK-SVAR-OK                             
104700     END-IF                                                               
104800     .                                                                    
104900     EJECT                                                                
105000 NCA-SOEK-TIBERANK-WDL611 SECTION.                                        
105100                                                                          
105200     MOVE 999999     TO WS-TIBERANK-AAMMDD                                
105300     MOVE LINK-IDARTNR          TO W-IDARTNR                              
105400     PERFORM IMS-GU-INLC-ROT                                              
105500     IF SEGMENT-FINNS                                                     
105600       PERFORM IMS-GNP-INLC-INL                                           
105700       PERFORM UNTIL SEGMENT-SAKNAS                                       
105800         IF INL-IDPTYP = 'R30' OR '310' OR 'R31'                          
105900           IF INL-TIBERANK <  WS-TIBERANK-AAMMDD                          
106000             MOVE INL-TIBERANK           TO WS-TIBERANK-AAMMDD            
106100             MOVE  INL-KVAVIS            TO W-KVAVIS                      
106200             MOVE JA TO TRAEFF-SW                                         
106300           END-IF                                                         
106400         END-IF                                                           
106500         PERFORM IMS-GNP-INLC-INL                                         
106600       END-PERFORM                                                        
106700     END-IF                                                               
106800     .                                                                    
106900     EJECT                                                                
107000                                                                          
107100 NCB-SOEK-LAGSTA-WDD9 SECTION.                                            
107200                                                                          
107300     PERFORM IMS-GU-WDD901                                                
107400     MOVE 999999   TO W-LAGSTA-DATUM                                      
107500     IF SEGMENT-FINNS                                                     
107600       PERFORM IMS-GNP-WDD924                                             
107700       IF SEGMENT-FINNS                                                   
107800         PERFORM UNTIL SEGMENT-SAKNAS                                     
107900           MOVE LEV-TILEVBSK-DISP TO W-TILEVBSK-AAMMDD                    
108000           IF W-TILEVBSK-AAMMDD < W-LAGSTA-DATUM                          
108100             MOVE LEV-KVAVIS-BSKKVAR TO WS-ANTAL-D9                       
108200             MOVE LEV-TILEVBSK-DISP TO W-LAGSTA-DATUM                     
108300             MOVE JA TO TRAEFF-SW                                         
108400           END-IF                                                         
108500           PERFORM IMS-GNP-WDD924                                         
108600         END-PERFORM                                                      
108700       ELSE                                                               
108800         PERFORM IMS-GNP-WDD905-FIRST                                     
108900         PERFORM UNTIL SEGMENT-SAKNAS                                     
109000           MOVE TIAVRDAT-DISP TO W-TIAVRDAT-AAMMDD                        
109100           IF W-TIAVRDAT-AAMMDD < W-LAGSTA-DATUM                          
109200             MOVE KVAVROP             TO WS-ANTAL-D9                      
109300             MOVE TIAVRDAT-DISP       TO W-LAGSTA-DATUM                   
109400             MOVE JA TO TRAEFF-SW                                         
109500           END-IF                                                         
109600           PERFORM IMS-GNP-WDD905                                         
109700         END-PERFORM                                                      
109800       END-IF                                                             
109900     END-IF                                                               
110000     .                                                                    
110100     EJECT                                                                
110200                                                                          
110300                                                                          
110400 O-GOODS-REC-KOD-R-AIR   SECTION.                                         
110500                                                                          
110600***  KDCALL = 613   ***                                                   
110700                                                                          
110800     PERFORM S-HAEMTA-LEDTIDER                                            
110900                                                                          
111000*    INLÄGGNINGSTID FLYG (ARBETSTID)                                      
111100     MOVE   2                 TO WORK-KDCALL                              
111200     MOVE LINK-IDDC-REC       TO WORK-IDDC                                
111300     MOVE LINK-TIAAMMDD-ANROP TO WORK-TIAAMMDD-FOM                        
111400     MOVE REF-KVDLTID-AIRINS  TO WORK-KVWORKD                             
111500     ADD +1                   TO WORK-KVWORKD                             
111600                                                                          
111700*                                                                         
111800     MOVE LINK-IDDC-REC       TO WS-IDDC                                  
111900                                                                          
112000     CALL WORKDAY USING WORK-KDCALL                                       
112100               WORK-DATE-AREA WORK-KDSVAR                                 
112200                                                                          
112300     IF WORK-KDSVAR-OK                                                    
112400       MOVE WORK-TIAAMMDD-TOM  TO LINK-TIAAMMDD-SVAR                      
112500       MOVE LINK-TISEKEL-ANROP TO LINK-TISEKEL-SVAR                       
112600     ELSE                                                                 
112700       MOVE NEJ TO LINK-SVAR-OK                                           
112800     END-IF                                                               
112900     .                                                                    
113000     EJECT                                                                
113100 S-HAEMTA-LEDTIDER SECTION.                                               
113200                                                                          
113300     MOVE LINK-IDDC-SEND        TO WS-IDDC                                
113400     IF NDC                                                               
113500        MOVE LINK-IDDC-SEND     TO W-IDDC-B616                            
113600     ELSE                                                                 
113700        IF LINK-IDDC-SEND NUMERIC                                         
113800           MOVE LINK-IDDC-SEND  TO W-IDDC-B616                            
113900        ELSE                                                              
114000           MOVE WC-CDC-SE       TO W-IDDC-B616                            
114100        END-IF                                                            
114200     END-IF                                                               
114300     MOVE LINK-IDDC-REC     TO W-IDDC-B601                                
114400                                                                          
114500     PERFORM IMS-GU-WDB616                                                
114600     IF SEGMENT-SAKNAS                                                    
114700        MOVE NEJ            TO LINK-SVAR-OK                               
114800     END-IF                                                               
114900     .                                                                    
115000     EJECT                                                                
115100 Z-FINIT SECTION.                                                         
115200                                                                          
115300     IF LINK-SVAR-OK = NEJ                                                
115400        MOVE 999999 TO LINK-TIAAMMDD-SVAR                                 
115500        MOVE 99     TO LINK-TISEKEL-SVAR                                  
115600     END-IF                                                               
115700     .                                                                    
115800     EJECT                                                                
115900* --- IMS SEKTIONER ---                                                   
116000                                                                          
116100 IMS-GU-ARTC-CLAG SECTION.                                                
116200                                                                          
116300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
116400          DELIMITED BY SIZE INTO SSA1                                     
116500     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
116600          DELIMITED BY SIZE INTO SSA2                                     
116700     MOVE '  GE' TO GODK-STATUSKODER                                      
116800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2             
116900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
117000     PERFORM IMS-STATUSKONTROLL                                           
117100     .                                                                    
117200     EJECT                                                                
117300 IMS-GU-WDK711 SECTION.                                                   
117400                                                                          
117500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
117600          DELIMITED BY SIZE INTO SSA1                                     
117700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
117800          DELIMITED BY SIZE INTO SSA2                                     
117900     MOVE '  GE' TO GODK-STATUSKODER                                      
118000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
118100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
118200     PERFORM IMS-STATUSKONTROLL                                           
118300     .                                                                    
118400     EJECT                                                                
118500 IMS-GU-INLC-ROT SECTION.                                                 
118600                                                                          
118700     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
118800          DELIMITED BY SIZE INTO SSA1                                     
118900     MOVE '  GE' TO GODK-STATUSKODER                                      
119000     CALL CBLTDLI USING GU INLC-PCB DLI-IO-WLINLC01 SSA1                  
119100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
119200     PERFORM IMS-STATUSKONTROLL                                           
119300     .                                                                    
119400     EJECT                                                                
119500 IMS-GNP-INLC-INL SECTION.                                                
119600                                                                          
119700     STRING 'WLINLC11(IDDC     =' W-IDDC-X ')'                            
119800          DELIMITED BY SIZE INTO SSA1                                     
119900     MOVE '  GE' TO GODK-STATUSKODER                                      
120000     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-WLINLC11 SSA1                 
120100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
120200     PERFORM IMS-STATUSKONTROLL                                           
120300     .                                                                    
120400     EJECT                                                                
120500 IMS-GNP-INLC-ORD SECTION.                                                
120600                                                                          
120700     STRING 'WLINLC12    '                                                
120800          DELIMITED BY SIZE INTO SSA1                                     
120900     MOVE '  GE' TO GODK-STATUSKODER                                      
121000     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-WLINLC12 SSA1                 
121100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
121200     PERFORM IMS-STATUSKONTROLL                                           
121300     .                                                                    
121400     EJECT                                                                
121500 IMS-GU-LEVA-NDC SECTION.                                                 
121600                                                                          
121700     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
121800          DELIMITED BY SIZE INTO SSA1                                     
121900     STRING 'WLLEVA16(IDDC     =' W-IDDC-X ')'                            
122000          DELIMITED BY SIZE INTO SSA2                                     
122100     MOVE '  GE' TO GODK-STATUSKODER                                      
122200     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-WLLEVA16 SSA1 SSA2             
122300     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
122400     PERFORM IMS-STATUSKONTROLL                                           
122500     .                                                                    
122600     EJECT                                                                
122700                                                                          
122800 IMS-GU-WDB616    SECTION.                                                
122900     STRING 'WDB601  (IDDC     =' W-IDDC-B601-X ')'                       
123000          DELIMITED BY SIZE INTO SSA1                                     
123100     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
123200          DELIMITED BY SIZE INTO SSA2                                     
123300     MOVE '  GE' TO GODK-STATUSKODER                                      
123400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
123500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
123600     PERFORM IMS-STATUSKONTROLL                                           
123700     .                                                                    
123800     EJECT                                                                
123900                                                                          
124000 IMS-GU-WDK712   SECTION.                                                 
124100                                                                          
124200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
124300          DELIMITED BY SIZE INTO SSA1                                     
124400     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
124500          DELIMITED BY SIZE INTO SSA2                                     
124600     MOVE '  GE' TO GODK-STATUSKODER                                      
124700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712                         
124800          SSA1 SSA2                                                       
124900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
125000     PERFORM IMS-STATUSKONTROLL                                           
125100     .                                                                    
125200     EJECT                                                                
125300 IMS-GU-WDD901 SECTION.                                                   
125400                                                                          
125500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
125600          DELIMITED BY SIZE INTO SSA1                                     
125700     MOVE '  GE'              TO GODK-STATUSKODER                         
125800     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
125900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
126000     PERFORM IMS-STATUSKONTROLL                                           
126100     .                                                                    
126200     SKIP2                                                                
126300 IMS-GNP-WDD924 SECTION.                                                  
126400                                                                          
126500     MOVE 'WDD924' TO SSA1                                                
126600     MOVE '  GE' TO GODK-STATUSKODER                                      
126700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
126800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
126900     PERFORM IMS-STATUSKONTROLL                                           
127000     .                                                                    
127100     EJECT                                                                
127200                                                                          
127300 IMS-GNP-WDD905-FIRST SECTION.                                            
127400                                                                          
127500     STRING 'WDD905  *F(KDAVROP  =' W-KDAVROP-X ')'                       
127600          DELIMITED BY SIZE INTO SSA1                                     
127700     MOVE '  GE'              TO GODK-STATUSKODER                         
127800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
127900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
128000     PERFORM IMS-STATUSKONTROLL                                           
128100     .                                                                    
128200     SKIP2                                                                
128300 IMS-GNP-WDD905 SECTION.                                                  
128400                                                                          
128500     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
128600          DELIMITED BY SIZE INTO SSA1                                     
128700     MOVE '  GE'              TO GODK-STATUSKODER                         
128800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
128900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
129000     PERFORM IMS-STATUSKONTROLL                                           
129100     .                                                                    
129200     SKIP2                                                                
129300 IMS-STATUSKONTROLL SECTION.                                              
129400                                                                          
129500     SET STATUS-IX TO 1                                                   
129600     SEARCH GODK-STATUS                                                   
129700       AT END                                                             
129800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
129900           DELIMITED BY SIZE INTO FELTEXT-STR                             
130000         CALL FELLOG                                                      
130100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
130200         CONTINUE                                                         
130300     END-SEARCH                                                           
130400     .                                                                    
