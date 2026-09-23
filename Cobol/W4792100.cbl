000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4792100.                                                
000300 AUTHOR.         SUSANNE OLSSON.                                          
000400 DATE-WRITTEN.   00/06/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SKAPAR RENSNINGSPOSTER FRÅN WDR4                                 
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDR4                                       
001100*                                                                         
001200     SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*          --- RENSNINGSPOSTER 4014                                       
002000     SELECT WDR44014                   ASSIGN TO W47921D1.                
002100     SKIP2                                                                
002200*          --- RENSNINGSPOSTER 4102                                       
002300     SELECT WDR44102                   ASSIGN TO W47921D2.                
002400     SKIP2                                                                
002500*          --- RENSNINGSPOSTER 4319                                       
002600     SELECT WDR44319                   ASSIGN TO W47921D3.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  WDR44014                                                             
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY WDR44014 -PRE UT1- -L.                                    
003700     SKIP3                                                                
003800 FD  WDR44102                                                             
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY WDR44102 -PRE UT2- -L.                                    
004300     SKIP3                                                                
004400 FD  WDR44319                                                             
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST -COPY WDR44319 -PRE UT3- -L.                                    
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005200*    -COPY WY2000W1                                                       
005300     SKIP3                                                                
005400 77  IDPGM                       PIC X(8)    VALUE 'W4792100'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  WS-IDHTYP                   PIC X(4)    VALUE SPACE.                 
005800 77  WS-IDDC-SPAR                PIC X(2)    VALUE SPACE.                 
005900 77  WS-IDDISTR                  PIC S9(5)   COMP-3 VALUE +0.             
006000 77  WS-IDGMTREF                 PIC X(17)   VALUE SPACE.                 
006100 77  DATE-100-TIAAMMDD           PIC 9(6)    VALUE ZERO.                  
006200*                                                                         
006300 01  FILLER                      PIC X(24)   VALUE 'SWITCHAR'.            
006400                                                                          
006500 77  FIRST-4016-SW               PIC X   VALUE 'J'.                       
006600     88  FIRST-4016                      VALUE 'J'.                       
006700                                                                          
006800 77  BORT-SW                     PIC X   VALUE 'N'.                       
006900     88  TA-BORT                         VALUE 'J'.                       
007000*      --- VALID IDDC CODES                                               
007100*01  -COPY WWDCKONS                                                       
007200*                                                                         
007300*01  -COPY WWDIST40                                                       
007400     EJECT                                                                
007500                                                                          
007600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700 01  FILLER REDEFINES DAGENS-DATUM.                                       
007800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008100                                                                          
008200 01  DD-MINUS-7D-NDC-US           PIC  9(8) VALUE ZERO.                   
008300 01  FILLER REDEFINES DD-MINUS-7D-NDC-US.                                 
008400     03 DD-MINUS-7D-NDC-US-SS     PIC 9(2).                               
008500     03 DD-MINUS-7D-NDC-US-AAMMDD PIC 9(6).                               
008600                                                                          
008700 01  DD-MINUS-7D-NDC-CA           PIC  9(8) VALUE ZERO.                   
008800 01  FILLER REDEFINES DD-MINUS-7D-NDC-CA.                                 
008900     03 DD-MINUS-7D-NDC-CA-SS     PIC  9(2).                              
009000     03 DD-MINUS-7D-NDC-CA-AAMMDD PIC  9(6).                              
009100                                                                          
009200 01  DD-MINUS-7D-NDC-JP           PIC  9(8) VALUE ZERO.                   
009300 01  FILLER REDEFINES DD-MINUS-7D-NDC-JP.                                 
009400     03 DD-MINUS-7D-NDC-JP-SS     PIC  9(2).                              
009500     03 DD-MINUS-7D-NDC-JP-AAMMDD PIC  9(6).                              
009600                                                                          
009700 01  DD-MINUS-7D-NDC-AU           PIC  9(8) VALUE ZERO.                   
009800 01  FILLER REDEFINES DD-MINUS-7D-NDC-AU.                                 
009900     03 DD-MINUS-7D-NDC-AU-SS     PIC  9(2).                              
010000     03 DD-MINUS-7D-NDC-AU-AAMMDD PIC  9(6).                              
010100                                                                          
010200 01  DD-MINUS-7D-NDC-CN           PIC  9(8) VALUE ZERO.                   
010300 01  FILLER REDEFINES DD-MINUS-7D-NDC-CN.                                 
010400     03 DD-MINUS-7D-NDC-CN-SS     PIC  9(2).                              
010500     03 DD-MINUS-7D-NDC-CN-AAMMDD PIC  9(6).                              
010600     EJECT                                                                
010700                                                                          
010800 01  DYNAMISKA-SUBPROGRAM.                                                
010900*                                                                         
011000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011400     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
011500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
011600     SKIP2                                                                
011700*    --- PARAMETRAR TILL ABEND                                            
011800                                                                          
011900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012200     SKIP2                                                                
012300 01  FELTEXT.                                                             
012400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012600     EJECT                                                                
012700 01  FILLER  PIC X(16) VALUE 'POSTSUM-PARAM'.                             
012800*                                                                         
012900*01  -COPY W0005   -PRE  POSTSUM-                                         
013000     EJECT                                                                
013100 01  FILLER  PIC X(16) VALUE 'DAGKONV-PARAM'.                             
013200*                                                                         
013300*01  -COPY WDAGAREA                                                       
013400     EJECT                                                                
013500 01  FILLER  PIC X(16) VALUE 'WORKDAY-PARAM'.                             
013600*                                                                         
013700*01  -COPY WORKAREA                                                       
013800     EJECT                                                                
013900 01  UT1-AREA-START               PIC X(16)   VALUE                       
014000                                 'UT1-AREA-START'.                        
014100     SKIP2                                                                
014200                                                                          
014300*01  AREA -COPY WDR44014   -PRE UT1-                                      
014400     EJECT                                                                
014500 01  UT2-AREA-START               PIC X(16)   VALUE                       
014600                                 'UT2-AREA-START'.                        
014700     SKIP2                                                                
014800                                                                          
014900*01  AREA -COPY WDR44102   -PRE UT2-                                      
015000     EJECT                                                                
015100 01  UT3-AREA-START               PIC X(16)   VALUE                       
015200                                 'UT3-AREA-START'.                        
015300     SKIP2                                                                
015400                                                                          
015500*01  AREA -COPY WDR44319   -PRE UT3-                                      
015600     EJECT                                                                
015700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015800*                                                                         
015900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016000     SKIP3                                                                
016100*    --- STATUS-KOD FRÅN IMS                                              
016200 01  STATUS-WS                   PIC XX.                                  
016300     88  SEGMENT-FINNS                       VALUE '  '.                  
016400     88  BASEN-SLUT                          VALUE 'GB'.                  
016500     SKIP2                                                                
016600 01  GODK-STATUSKODER.                                                    
016700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016800     SKIP3                                                                
016900 01  SSA1                        PIC X(64).                               
017000     EJECT                                                                
017100*    --- IMS FUNKTIONSKODER                                               
017200*01  -COPY W0003                                                          
017300     EJECT                                                                
017400*    ---  DLI INPUT-OUTPUT AREA                                           
017500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
017600 01  DLI-IO-AREA.                                                         
017700     03  IO-AREA                 PIC X(1400).                             
017800*    03  -COPY WDGX01   -RED IO-AREA.                                     
017900*    03  -COPY WDGX4014 -RED IO-AREA.                                     
018000*    03  -COPY WDGX4016 -RED IO-AREA.                                     
018100*    03  -COPY WDGX4101 -RED IO-AREA.                                     
018200*    03  -COPY WDGX4102 -RED IO-AREA.                                     
018300*    03  -COPY WDGX4319 -RED IO-AREA.                                     
018400     EJECT                                                                
018500 LINKAGE SECTION.                                                         
018600*01  -COPY W0008  -PRE WDR4-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900 PROCEDURE DIVISION  USING WDR4-PCB.                                      
019000 MAIN SECTION.                                                            
019100     ENTRY 'DLITCBL' USING WDR4-PCB.                                      
019200                                                                          
019300     PERFORM A-INIT                                                       
019400                                                                          
019500     PERFORM IMS-GN-WDR4                                                  
019600     PERFORM UNTIL BASEN-SLUT                                             
019700       EVALUATE WDR4-SEG-NAME-FB                                          
019800         WHEN 'WDR401'                                                    
019900           MOVE IDHTYP          TO WS-IDHTYP                              
020000           IF WS-IDHTYP = '4101'                                          
020100             MOVE 4101-IDDC     TO WS-IDDC-SPAR                           
020200             MOVE 4101-IDDISTR  TO WS-IDDISTR                             
020300           END-IF                                                         
020400           IF WS-IDHTYP = '4319'                                          
020500             PERFORM B-KOLLA-RENSNING-4319                                
020600           END-IF                                                         
020700         WHEN 'WDGX4014'                                                  
020800           MOVE 4014-IDDISTR    TO DIST40-IDDISTR                         
020900           MOVE 4014-IDGMTREF   TO WS-IDGMTREF                            
021000           MOVE JA              TO FIRST-4016-SW                          
021100         WHEN 'WDGX4016'                                                  
021200           IF FIRST-4016                                                  
021300             PERFORM C-KOLLA-RENSNING-4014                                
021400             MOVE NEJ             TO FIRST-4016-SW                        
021500           END-IF                                                         
021600         WHEN 'WDGX4102'                                                  
021700           PERFORM D-KOLLA-RENSNING-4102                                  
021800       END-EVALUATE                                                       
021900       PERFORM IMS-GN-WDR4                                                
022000     END-PERFORM                                                          
022100                                                                          
022200     PERFORM Z-FINIT                                                      
022300                                                                          
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900                                                                          
023000     OPEN OUTPUT WDR44014                                                 
023100                 WDR44102                                                 
023200                 WDR44319                                                 
023300                                                                          
023400     ACCEPT DAGENS-DATUM  FROM DATE                                       
023500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023600                                                                          
023700     PERFORM AA-KOLLA-100-DAGAR                                           
023800     PERFORM AB-KOLLA-007-DAGAR-NDC                                       
023900     .                                                                    
024000     EJECT                                                                
024100 AA-KOLLA-100-DAGAR             SECTION.                                  
024200                                                                          
024300     MOVE DAGENS-DATUM   TO DAG-TIAAMMDD-TOM                              
024400     MOVE 100            TO DAG-KVKALDAG                                  
024500     MOVE 003            TO DAG-KDCALL                                    
024600                                                                          
024700     CALL WDAGKONV  USING DAG-KDCALL                                      
024800                          DAG-DATUM-AREA                                  
024900                          DAG-KDSVAR                                      
025000                                                                          
025100     IF DAG-KDSVAR = SPACE                                                
025200       MOVE DAG-TIAAMMDD-FOM    TO DATE-100-TIAAMMDD                      
025300     ELSE                                                                 
025400       MOVE 'ERROR IN CONVERTING DATE-100' TO FELTEXT-STR                 
025500       DISPLAY FELTEXT                                                    
025600       PERFORM S99-ABEND                                                  
025700     END-IF                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 AB-KOLLA-007-DAGAR-NDC SECTION.                                          
026100                                                                          
026200     MOVE 003                 TO WORK-KDCALL                              
026300     MOVE WC-NDC-US-RU        TO WORK-IDDC                                
026400     MOVE 7                   TO WORK-KVWORKD                             
026500     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                        
026600     CALL WORKDAY             USING WORK-KDCALL                           
026700                                    WORK-DATE-AREA                        
026800                                    WORK-KDSVAR                           
026900     IF WORK-KDSVAR-OK                                                    
027000        MOVE WORK-TIAAMMDD-FOM TO DAG-TIAAMMDD-FOM                        
027100        MOVE 1                 TO DAG-KVKALDAG                            
027200        MOVE 002               TO DAG-KDCALL                              
027300        CALL WDAGKONV          USING DAG-KDCALL                           
027400                                     DAG-DATUM-AREA                       
027500                                     DAG-KDSVAR                           
027600        IF DAG-KDSVAR = SPACE                                             
027700           MOVE DAG-TIAAMMDD-TOM TO DD-MINUS-7D-NDC-US-AAMMDD             
027800           MOVE DAG-TISEKEL-TOM  TO DD-MINUS-7D-NDC-US-SS                 
027900        ELSE                                                              
028000          DISPLAY 'FEL FRÅN DAGKONV 1 AB SECTION.'                        
028100          CALL FELLOG                                                     
028200        END-IF                                                            
028300                                                                          
028400     ELSE                                                                 
028500       DISPLAY 'FEL FRÅN WORKDAY 1 AB SECTION.'                           
028600       CALL FELLOG                                                        
028700     END-IF                                                               
028800                                                                          
028900                                                                          
029000     MOVE 003                 TO WORK-KDCALL                              
029100     MOVE WC-NDC-CA           TO WORK-IDDC                                
029200     MOVE 7                   TO WORK-KVWORKD                             
029300     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                        
029400     CALL WORKDAY             USING WORK-KDCALL                           
029500                                    WORK-DATE-AREA                        
029600                                    WORK-KDSVAR                           
029700     IF WORK-KDSVAR-OK                                                    
029800        MOVE WORK-TIAAMMDD-FOM TO DAG-TIAAMMDD-FOM                        
029900        MOVE 1                 TO DAG-KVKALDAG                            
030000        MOVE 002               TO DAG-KDCALL                              
030100        CALL WDAGKONV          USING DAG-KDCALL                           
030200                                     DAG-DATUM-AREA                       
030300                                     DAG-KDSVAR                           
030400        IF DAG-KDSVAR = SPACE                                             
030500           MOVE DAG-TIAAMMDD-TOM TO DD-MINUS-7D-NDC-CA-AAMMDD             
030600           MOVE DAG-TISEKEL-TOM  TO DD-MINUS-7D-NDC-CA-SS                 
030700        ELSE                                                              
030800          DISPLAY 'FEL FRÅN DAGKONV 2 AB SECTION.'                        
030900          CALL FELLOG                                                     
031000        END-IF                                                            
031100                                                                          
031200     ELSE                                                                 
031300       DISPLAY 'FEL FRÅN WORKDAY 2 AB SECTION.'                           
031400       CALL FELLOG                                                        
031500     END-IF                                                               
031600                                                                          
031700     MOVE 003                 TO WORK-KDCALL                              
031800     MOVE WC-NDC-JP-61        TO WORK-IDDC                                
031900     MOVE 7                   TO WORK-KVWORKD                             
032000     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                        
032100     CALL WORKDAY             USING WORK-KDCALL                           
032200                                    WORK-DATE-AREA                        
032300                                    WORK-KDSVAR                           
032400     IF WORK-KDSVAR-OK                                                    
032500        MOVE WORK-TIAAMMDD-FOM TO DAG-TIAAMMDD-FOM                        
032600        MOVE 1                 TO DAG-KVKALDAG                            
032700        MOVE 002               TO DAG-KDCALL                              
032800        CALL WDAGKONV          USING DAG-KDCALL                           
032900                                     DAG-DATUM-AREA                       
033000                                     DAG-KDSVAR                           
033100        IF DAG-KDSVAR = SPACE                                             
033200           MOVE DAG-TIAAMMDD-TOM TO DD-MINUS-7D-NDC-JP-AAMMDD             
033300           MOVE DAG-TISEKEL-TOM  TO DD-MINUS-7D-NDC-JP-SS                 
033400        ELSE                                                              
033500          DISPLAY 'FEL FRÅN DAGKONV 3 AB SECTION.'                        
033600          CALL FELLOG                                                     
033700        END-IF                                                            
033800                                                                          
033900     ELSE                                                                 
034000       DISPLAY 'FEL FRÅN WORKDAY 3 AB SECTION.'                           
034100       CALL FELLOG                                                        
034200     END-IF                                                               
034300                                                                          
034400     MOVE 003                 TO WORK-KDCALL                              
034500     MOVE WC-NDC-AU           TO WORK-IDDC                                
034600     MOVE 7                   TO WORK-KVWORKD                             
034700     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                        
034800     CALL WORKDAY             USING WORK-KDCALL                           
034900                                    WORK-DATE-AREA                        
035000                                    WORK-KDSVAR                           
035100     IF WORK-KDSVAR-OK                                                    
035200        MOVE WORK-TIAAMMDD-FOM TO DAG-TIAAMMDD-FOM                        
035300        MOVE 1                 TO DAG-KVKALDAG                            
035400        MOVE 002               TO DAG-KDCALL                              
035500        CALL WDAGKONV          USING DAG-KDCALL                           
035600                                     DAG-DATUM-AREA                       
035700                                     DAG-KDSVAR                           
035800        IF DAG-KDSVAR = SPACE                                             
035900           MOVE DAG-TIAAMMDD-TOM TO DD-MINUS-7D-NDC-AU-AAMMDD             
036000           MOVE DAG-TISEKEL-TOM  TO DD-MINUS-7D-NDC-AU-SS                 
036100        ELSE                                                              
036200          DISPLAY 'FEL FRÅN DAGKONV 4 AB SECTION.'                        
036300          CALL FELLOG                                                     
036400        END-IF                                                            
036500                                                                          
036600     ELSE                                                                 
036700       DISPLAY 'FEL FRÅN WORKDAY 4 AB SECTION.'                           
036800       CALL FELLOG                                                        
036900     END-IF                                                               
037000                                                                          
037010     MOVE 003                 TO WORK-KDCALL                              
037020     MOVE WC-NDC-CN-71        TO WORK-IDDC                                
037030     MOVE 7                   TO WORK-KVWORKD                             
037040     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                        
037050     CALL WORKDAY             USING WORK-KDCALL                           
037060                                    WORK-DATE-AREA                        
037070                                    WORK-KDSVAR                           
037080     IF WORK-KDSVAR-OK                                                    
037090        MOVE WORK-TIAAMMDD-FOM TO DAG-TIAAMMDD-FOM                        
037091        MOVE 1                 TO DAG-KVKALDAG                            
037092        MOVE 002               TO DAG-KDCALL                              
037093        CALL WDAGKONV          USING DAG-KDCALL                           
037094                                     DAG-DATUM-AREA                       
037095                                     DAG-KDSVAR                           
037096        IF DAG-KDSVAR = SPACE                                             
037097           MOVE DAG-TIAAMMDD-TOM TO DD-MINUS-7D-NDC-CN-AAMMDD             
037098           MOVE DAG-TISEKEL-TOM  TO DD-MINUS-7D-NDC-CN-SS                 
037099        ELSE                                                              
037100          DISPLAY 'FEL FRÅN DAGKONV 5 AB SECTION.'                        
037101          CALL FELLOG                                                     
037102        END-IF                                                            
037103                                                                          
037104     ELSE                                                                 
037105       DISPLAY 'FEL FRÅN WORKDAY 5 AB SECTION.'                           
037106       CALL FELLOG                                                        
037107     END-IF                                                               
037110     .                                                                    
037200     EJECT                                                                
037300 B-KOLLA-RENSNING-4319 SECTION.                                           
037400                                                                          
037500     MOVE 001              TO WORK-KDCALL                                 
037600     MOVE '11'             TO WORK-IDDC                                   
037700     MOVE 4319-TIREGDAT    TO WORK-TIAAMMDD-FOM                           
037800     MOVE DAGENS-DATUM     TO WORK-TIAAMMDD-TOM                           
037900     CALL WORKDAY USING WORK-KDCALL                                       
038000                        WORK-DATE-AREA                                    
038100                        WORK-KDSVAR                                       
038200     IF WORK-KDSVAR-OK                                                    
038300       CONTINUE                                                           
038400     ELSE                                                                 
038500       DISPLAY 'FEL FRÅN WORKDAY '                                        
038600       CALL FELLOG                                                        
038700     END-IF                                                               
038800                                                                          
038900     IF WORK-KVWORKD > 21                                                 
039000       MOVE WS-IDHTYP         TO UT3-IDHTYP                               
039100       MOVE 4319-TIREGDAT     TO UT3-TIREGDAT                             
039200       PERFORM S13-SKRIV-WDR44319                                         
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600 C-KOLLA-RENSNING-4014 SECTION.                                           
039700                                                                          
039800     IF DIST40-NDC-USA                                                    
039900        IF 4016-DAREGDAT < DD-MINUS-7D-NDC-US                             
040000           MOVE JA TO BORT-SW                                             
040100        END-IF                                                            
040200     END-IF                                                               
040300     IF DIST40-NDC-CAN                                                    
040400        IF 4016-DAREGDAT < DD-MINUS-7D-NDC-CA                             
040500           MOVE JA TO BORT-SW                                             
040600        END-IF                                                            
040700     END-IF                                                               
040800     IF DIST40-NDC-JAP                                                    
040900        IF 4016-DAREGDAT < DD-MINUS-7D-NDC-JP                             
041000           MOVE JA TO BORT-SW                                             
041100        END-IF                                                            
041200     END-IF                                                               
041300     IF DIST40-NDC-AU                                                     
041400        IF 4016-DAREGDAT < DD-MINUS-7D-NDC-AU                             
041500           MOVE JA TO BORT-SW                                             
041600        END-IF                                                            
041700     END-IF                                                               
041710     IF DIST40-NDC-CN                                                     
041720        IF 4016-DAREGDAT < DD-MINUS-7D-NDC-CN                             
041730           MOVE JA TO BORT-SW                                             
041740        END-IF                                                            
041750     END-IF                                                               
041800                                                                          
041900     IF TA-BORT                                                           
042000       MOVE WS-IDHTYP    TO UT1-IDHTYP                                    
042100       MOVE WS-IDGMTREF  TO UT1-IDGMTREF                                  
042200       PERFORM S11-SKRIV-WDR44014                                         
042300     END-IF                                                               
042400     MOVE NEJ       TO BORT-SW                                            
042500     .                                                                    
042600     EJECT                                                                
042700 D-KOLLA-RENSNING-4102 SECTION.                                           
042800                                                                          
042900*- RADER ÄLDRE ÄN 100 DAGAR SKALL RENSAS BORT.                            
043000                                                                          
043100     IF 4102-DARFSDAT = ZERO                                              
043200       CONTINUE                                                           
043300     ELSE                                                                 
043400       MOVE 4102-DARFSDAT (3:6)   TO TMP1-YYMMDD                          
043500       MOVE DATE-100-TIAAMMDD     TO TMP2-YYMMDD                          
043600       PERFORM WY2000P1                                                   
043700                                                                          
043800       IF TMP1-YYMMDD < TMP2-YYMMDD                                       
043900                                                                          
044000         MOVE WS-IDHTYP         TO UT2-IDHTYP                             
044100         MOVE WS-IDDC-SPAR      TO UT2-IDDC                               
044200         MOVE WS-IDDISTR        TO UT2-IDDISTR                            
044300         MOVE 4102-IDKUNDNR     TO UT2-IDKUNDNR                           
044400         MOVE 4102-IDRAPP       TO UT2-IDRAPP                             
044500         MOVE 4102-IDKOLLI      TO UT2-IDKOLLI                            
044600         MOVE 4102-IDARTNR      TO UT2-IDARTNR                            
044700         MOVE 4102-DARFSDAT     TO UT2-DARFSDAT                           
044800                                                                          
044900         PERFORM S12-SKRIV-WDR44102                                       
045000       END-IF                                                             
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 Z-FINIT SECTION.                                                         
045500     CLOSE WDR44014                                                       
045600           WDR44102                                                       
045700           WDR44319                                                       
045800     SKIP2                                                                
045900     MOVE 'S' TO POSTSUM-OPKOD                                            
046000     CALL POSTSUM USING POSTSUM-PARM                                      
046100     .                                                                    
046200     EJECT                                                                
046300 S11-SKRIV-WDR44014 SECTION.                                              
046400                                                                          
046500     WRITE UT1-POST FROM UT1-AREA                                         
046600                                                                          
046700     MOVE '4014'     TO POSTSUM-TRANSTYP                                  
046800     MOVE 'WDR44014' TO POSTSUM-FDNAMN                                    
046900     MOVE 'W47921D1' TO POSTSUM-DDNAMN2                                   
047000     CALL POSTSUM USING POSTSUM-PARM                                      
047100     .                                                                    
047200     EJECT                                                                
047300 S12-SKRIV-WDR44102 SECTION.                                              
047400                                                                          
047500     WRITE UT2-POST FROM UT2-AREA                                         
047600                                                                          
047700     MOVE '4102'     TO POSTSUM-TRANSTYP                                  
047800     MOVE 'WDR44102' TO POSTSUM-FDNAMN                                    
047900     MOVE 'W47921D2' TO POSTSUM-DDNAMN2                                   
048000     CALL POSTSUM USING POSTSUM-PARM                                      
048100     .                                                                    
048200     EJECT                                                                
048300 S13-SKRIV-WDR44319 SECTION.                                              
048400                                                                          
048500     WRITE UT3-POST FROM UT3-AREA                                         
048600                                                                          
048700     MOVE '4319'     TO POSTSUM-TRANSTYP                                  
048800     MOVE 'WDR44319' TO POSTSUM-FDNAMN                                    
048900     MOVE 'W47921D3' TO POSTSUM-DDNAMN2                                   
049000     CALL POSTSUM USING POSTSUM-PARM                                      
049100     .                                                                    
049200     EJECT                                                                
049300 S99-ABEND SECTION.                                                       
049400                                                                          
049500     SKIP2                                                                
049600     MOVE 'S' TO POSTSUM-OPKOD                                            
049700     CALL POSTSUM USING POSTSUM-PARM                                      
049800     CALL ABEND USING RKOD-ABEND                                          
049900     .                                                                    
050000     EJECT                                                                
050100* --- IMS SEKTIONER ---                                                   
050200                                                                          
050300 IMS-GN-WDR4   SECTION.                                                   
050400                                                                          
050500     CALL CBLTDLI USING GN WDR4-PCB DLI-IO-AREA                           
050600     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
050700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
050800     PERFORM IMS-STATUSKONTROLL                                           
050900     .                                                                    
051000     EJECT                                                                
051100 IMS-STATUSKONTROLL SECTION.                                              
051200                                                                          
051300     SET STATUS-IX TO 1                                                   
051400     SEARCH GODK-STATUS                                                   
051500       AT END                                                             
051600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
051700           DELIMITED BY SIZE INTO FELTEXT                                 
051800         DISPLAY FELTEXT                                                  
051900         CALL FELLOG                                                      
052000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
052100         CONTINUE                                                         
052200     END-SEARCH                                                           
052300     .                                                                    
052400     EJECT                                                                
052500*    -COPY WY2000P1                                                       
