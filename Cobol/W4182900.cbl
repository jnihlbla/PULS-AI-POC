000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4182900.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   07/03/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER FIL MED GODKÄNDA ARTIKLAR                                  
001000*        HÄMTAR PRIS IFRÅN W355PRIS                                       
001100*        LETAR UPP LEDIGT LISTNR PÅ WDA2                                  
001200*        SKAPA NYA POSTER TILL WDA4                                       
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDA2                                       
001500*        PROGRAMMET UPPDATERAR WDA4                                       
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INFIL MED GODKÄNDA ARTIKLAR                                
002600     SELECT W41828                     ASSIGN TO W41829D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W41828                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W4182802      -L.                                              
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W4182900'.            
004100 01  CHKP-VAR.                                                            
004200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004700     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  W-DAGENS-DATUM            PIC 9(6)      VALUE ZERO.                  
005100 77  WS-REFOBNET               PIC 9(3)      VALUE ZERO.                  
005200 77  WS-SUMINVD                PIC 9(3)      VALUE ZERO.                  
005300 77  WS-PRARTNTO               PIC S9(7)V9(2) VALUE ZERO COMP-3.          
005400 77  WS-PRARTNTO-NEW           PIC S9(7)V9(2) VALUE ZERO COMP-3.          
005500 77  WS-PRARTNTO-TOT           PIC S9(7)V9(2) VALUE ZERO COMP-3.          
005600 77  WS-IDDISTR                PIC S9(5)      VALUE ZERO COMP-3.          
005700 77  W-W41828-KVPOST-IN        PIC 9(6)       VALUE ZERO.                 
005800 77  W-IMS-SECTION             PIC X(40)      VALUE ZERO.                 
005900 77  W-SECTION                 PIC X(40)      VALUE ZERO.                 
006000 77  WS-MAIL-TEXT              PIC X(80)      VALUE ZERO.                 
006100 77  KDRC-DISPLAY              PIC Z(5).                                  
006200 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
006300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)  COMP VALUE +16.               
006400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  COMP VALUE +1000.             
006500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
006600     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007000                                                                          
007100 77  W41828-EOF-SW               PIC X       VALUE 'N'.                   
007200     88  END-OF-W41828                       VALUE 'J'.                   
007300     EJECT                                                                
007400 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
007500 01  FILLER REDEFINES DAGENS-DATUM.                                       
007600     03  DAGENS-DATUM-SS         PIC 9(2).                                
007700     03  DAGENS-DATUM-AAMMDD     PIC 9(6).                                
007800                                                                          
007900     EJECT                                                                
008000 01  DAGENS-TID                  PIC 9(9)    VALUE ZERO.                  
008100 01  FILLER REDEFINES DAGENS-TID.                                         
008200     03  DAGENS-TID-1-6          PIC 9(6).                                
008300     03  DAGENS-TID-7-9          PIC 9(3).                                
008400 01  WS-IDRAPPNR                 PIC 9(7)    VALUE ZERO.                  
008500 01  FILLER REDEFINES WS-IDRAPPNR.                                        
008600     03  WS-IDDISTR-4            PIC 9(4).                                
008700     03  WS-IDRAPPNR-LOP         PIC 9(3).                                
008800     EJECT                                                                
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000*                                                                         
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009500     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
009600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010100*01  -COPY WZ01SUB                                                        
010200     EJECT                                                                
010300                                                                          
010400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
010500*01  -COPY WZ01SEND                                                       
010600                                                                          
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
010900 01  HDR-AREA.                                                            
011000*    03  -COPY WZ01REQU  -PRE HDR-                                        
011100*    03  -COPY WZ04HDR                                                    
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
011400 01  DOC-AREA.                                                            
011500     03  DOC-LINE                PIC X(80).                               
011600                                                                          
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL DATKORT                                          
011900*                                                                         
012000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W41829'.              
012100     SKIP2                                                                
012200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012300     SKIP2                                                                
012400*01  -COPY WDATKORT                                                       
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL POSTSUM                                          
012700*                                                                         
012800*01  -COPY W0005   -PRE  POSTSUM-                                         
012900     EJECT                                                                
013000*01  -COPY W335PRIS                                                       
013100     EJECT                                                                
013200 01  TEST-IDDISTR           PIC  9(5) COMP-3 VALUE ZERO.                  
013300*01  FILLER  -COPY WWDIST79    -RED TEST-IDDISTR.                         
013400     SKIP2                                                                
013500 01  IN-AREA-START               PIC X(24)   VALUE                        
013600                                             'IN-AREA-START'.             
013700     SKIP2                                                                
013800                                                                          
013900*01  AREA -COPY W4182802     -PRE IN-                                     
014000*                                                                         
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014300     SKIP3                                                                
014400 01  NYCKLAR-TILL-DLI.                                                    
014500     03 W-IDLEVANM-X.                                                     
014600        05 W-IDDISTR-WDA2        PIC S9(5)    COMP-3.                     
014700        05 W-IDKUNDNR-WDA2       PIC S9(7)    COMP-3.                     
014800        05 W-IDRAPPNR-WDA2       PIC  9(7)    VALUE ZERO.                 
014900                                                                          
015000     03  W-IDDISTR-X.                                                     
015100         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
015200     03  W-IDRAPPNR-X.                                                    
015300         05  W-IDRAPPNR          PIC 9(7)    VALUE ZERO.                  
015400     03  W-IDKUNDNR-X.                                                    
015500         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
015600     03  W-IDARTNR-X.                                                     
015700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015800     03   W-IDSKYLT-X.                                                    
015900         05 W-IDSKYLT            PIC X(3).                                
016000     03   W-WDA401KY-X.                                                   
016100         05 W-IDDISTR-WDA4      PIC S9(5)   VALUE ZERO COMP-3.            
016200         05 W-IDRAPPNR-WDA4     PIC 9(7)    VALUE ZERO.                   
016300                                                                          
016400     SKIP2                                                                
016500*    --- STATUS-KOD FRÅN IMS                                              
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-FINNS                       VALUE '  '.                  
016800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017100     88  IMS-EJ-OK                           VALUE 'XD'.                  
017200     SKIP2                                                                
017300 01  GODK-STATUSKODER.                                                    
017400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  SSA1                        PIC X(64).                               
017700 01  SSA2                        PIC X(64).                               
017800     EJECT                                                                
017900*    --- IMS FUNKTIONSKODER                                               
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300                                                                          
018400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
018500 01  DLI-IO-WDA201.                                                       
018600*    03  -COPY WDA201                                                     
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA401'.                      
018800 01  DLI-IO-WDA401.                                                       
018900*    03  -COPY WDA401                                                     
019000     EJECT                                                                
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA411'.                      
019200 01  DLI-IO-WDA411.                                                       
019300*    03  -COPY WDA411                                                     
019400 01  DLI-IO-WLBENA11.                                                     
019500*    03  -COPY WDD311 -PRE BEN-                                           
019600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
019700 01  DLI-IO-WDK601.                                                       
019800*    03  -COPY WDK601                                                     
019900     EJECT                                                                
020000                                                                          
020100     EJECT                                                                
020200 LINKAGE SECTION.                                                         
020300                                                                          
020400*01  -COPY W0009   -PRE MSG-                                              
020500 01  DISTRDOC-PCB                PIC X.                                   
020600     EJECT                                                                
020700                                                                          
020800*01  -COPY W0008  -PRE WDA2-                                              
020900     05  FILLER                  PIC X.                                   
021000                                                                          
021100*01  -COPY W0008  -PRE WDA4-                                              
021200     05  FILLER                  PIC X.                                   
021300*01  -COPY W0008  -PRE ARTC-                                              
021400     05  FILLER                  PIC X.                                   
021500     EJECT                                                                
021510*01  -COPY W0008  -PRE WDK7-                                              
021520     05  FILLER                  PIC X.                                   
021530     EJECT                                                                
021600*01  -COPY W0008  -PRE GMTA-                                              
021700     05  FILLER                  PIC X.                                   
021800     EJECT                                                                
021900*01  -COPY W0008  -PRE BETA-                                              
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008  -PRE GPRIA-                                             
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500*01  -COPY W0008  -PRE GPRIB-                                             
022600     05  FILLER                  PIC X.                                   
022700     EJECT                                                                
022800 01  PRIS-COST-WDK6-PCB          PIC X.                                   
022900 01  PRIS-COST-WDK7-PCB          PIC X.                                   
023000 01  PRIS-COST-WDF1-PCB          PIC X.                                   
023100 01  PRIS-COST-9305-PCB          PIC X.                                   
023110 01  PRIS-COST-WDK72-PCB         PIC X.                                   
023120 01  PRIS-COST-WDB6-PCB          PIC X.                                   
023200*01  -COPY W0008  -PRE BENA-                                              
023300     05  FILLER                  PIC X.                                   
023400*01  -COPY W0008  -PRE WDK6-                                              
023500     05  FILLER                  PIC X.                                   
023600 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB                           
023700     WDA2-PCB WDA4-PCB                                                    
023800     ARTC-PCB WDK7-PCB  GMTA-PCB                                          
023900     BETA-PCB GPRIA-PCB GPRIB-PCB                                         
024000     PRIS-COST-WDK6-PCB                                                   
024100     PRIS-COST-WDK7-PCB                                                   
024200     PRIS-COST-WDF1-PCB                                                   
024300     PRIS-COST-9305-PCB                                                   
024301     PRIS-COST-WDK72-PCB                                                  
024302     PRIS-COST-WDB6-PCB                                                   
024310     BENA-PCB WDK6-PCB.                                                   
024400                                                                          
024500 MAIN SECTION.                                                            
024600     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB                           
024700     WDA2-PCB WDA4-PCB                                                    
024800     ARTC-PCB WDK7-PCB  GMTA-PCB                                          
024900     BETA-PCB GPRIA-PCB GPRIB-PCB                                         
025000     PRIS-COST-WDK6-PCB                                                   
025100     PRIS-COST-WDK7-PCB                                                   
025200     PRIS-COST-WDF1-PCB                                                   
025300     PRIS-COST-9305-PCB                                                   
025301     PRIS-COST-WDK72-PCB                                                  
025302     PRIS-COST-WDB6-PCB                                                   
025310     BENA-PCB WDK6-PCB.                                                   
025400                                                                          
025500     SKIP2                                                                
025600     PERFORM A-INIT                                                       
025700     PERFORM S01-LAES-W41828                                              
025800                                                                          
025900     IF NOT END-OF-W41828                                                 
026000       PERFORM B-ORDNA-IDRAPPNR                                           
026100     ELSE                                                                 
026200       MOVE '   EMPTY FILE, NOTING UPPDATED' TO WS-MAIL-TEXT              
026300       PERFORM S10-SKAPA-MAIL                                             
026400     END-IF                                                               
026500                                                                          
026600     PERFORM UNTIL END-OF-W41828                                          
026700       IF CHKP-ANT > CHKP-MAX                                             
026800         PERFORM X-TAG-CHECKPOINT                                         
026900       END-IF                                                             
027000                                                                          
027100       PERFORM C-PRISTILLAEMPA                                            
027200       PERFORM D-SKAPA-WDA411-POST                                        
027300       PERFORM S01-LAES-W41828                                            
027400                                                                          
027500     END-PERFORM                                                          
027600                                                                          
027700                                                                          
027800     PERFORM Z-FINIT                                                      
027900                                                                          
028000     MOVE ZERO TO RETURN-CODE                                             
028100     GOBACK                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 A-INIT SECTION.                                                          
028500     SKIP2                                                                
028600                                                                          
028700     PERFORM IMS-RESTART                                                  
028800                                                                          
028900     OPEN INPUT W41828                                                    
029000                                                                          
029100                                                                          
029200                                                                          
029300*    CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
029400*    MOVE D-AAR            TO DAGENS-DATUM-AAR                            
029500*    MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
029600*    MOVE D-DAG            TO DAGENS-DATUM-DAG                            
029700     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
029800     ACCEPT DAGENS-TID FROM TIME                                          
029900                                                                          
030000     MOVE 'GB '            TO W-IDSKYLT                                   
030100     MOVE IDPGM            TO POSTSUM-PROGNAMN                            
030200     .                                                                    
030300     EJECT                                                                
030400 B-ORDNA-IDRAPPNR SECTION.                                                
030500     MOVE 'B-ORDNA-IDRAPP    ' TO W-SECTION                               
030600     MOVE IN-IDDISTR       TO W-IDDISTR                                   
030700                              W-IDDISTR-WDA2                              
030800                              W-IDDISTR-WDA4                              
030900                              WS-IDDISTR-4                                
031000                              WS-IDDISTR                                  
031100                              TEST-IDDISTR                                
031200     MOVE IN-IDKUNDNR      TO W-IDKUNDNR                                  
031300                              W-IDKUNDNR-WDA2                             
031400     MOVE WS-IDRAPPNR      TO W-IDRAPPNR                                  
031500                              W-IDRAPPNR-WDA2                             
031600                              W-IDRAPPNR-WDA4                             
031700     MOVE IN-REFOBNET      TO WS-REFOBNET                                 
031800     MOVE IN-SUMINVD       TO WS-SUMINVD                                  
031900                                                                          
032000                                                                          
032100     PERFORM IMS-GET-WDA201                                               
032200     IF SEGMENT-FINNS                                                     
032300       PERFORM UNTIL SEGMENT-SAKNAS                                       
032400         ADD +1 TO W-IDRAPPNR-WDA2                                        
032500         ADD +1 TO W-IDRAPPNR-WDA4                                        
032600         PERFORM IMS-GET-WDA201                                           
032700                                                                          
032800       END-PERFORM                                                        
032900     END-IF                                                               
033000**** SE OM IDRAPPNR FINNS PÅ WDA4                                         
033100                                                                          
033200     PERFORM IMS-GET-WDA401                                               
033300     IF SEGMENT-FINNS                                                     
033400       PERFORM UNTIL SEGMENT-SAKNAS                                       
033500         ADD +1 TO W-IDRAPPNR-WDA4                                        
033600         ADD +1 TO W-IDRAPPNR-WDA2                                        
033700                                                                          
033800         PERFORM IMS-GET-WDA401                                           
033900         IF SEGMENT-SAKNAS                                                
034000*** KOLLA OM RAPPARTEN FINNS PÅ WDA2 ANNARS +1 RAPPNR                     
034100           PERFORM IMS-GET-WDA201                                         
034200         END-IF                                                           
034300       END-PERFORM                                                        
034400     END-IF                                                               
034500**** LÄGG TILL WDA4 POST *****                                            
034600                                                                          
034700     MOVE W-IDDISTR           TO BUYB-IDDISTR                             
034800     MOVE W-IDKUNDNR          TO BUYB-IDKUNDNR                            
034900     MOVE W-IDRAPPNR-WDA4     TO BUYB-IDRAPPNR                            
035000     MOVE 'N'                 TO BUYB-FLPRINT                             
035100     MOVE 'N'                 TO BUYB-FLPERMIT                            
035200     MOVE WS-REFOBNET         TO BUYB-REFOBNET                            
035300     MOVE WS-SUMINVD          TO BUYB-SUMINVD                             
035400     MOVE DAGENS-DATUM-AAMMDD TO BUYB-TIREGDAT                            
035500     MOVE DAGENS-TID-1-6      TO BUYB-TIREGTID                            
035600                                                                          
035700     PERFORM IMS-ISRT-WDA401                                              
035800     .                                                                    
035900     EJECT                                                                
036000 C-PRISTILLAEMPA SECTION.                                                 
036100     SKIP2                                                                
036200     MOVE 'C-PRISTILLAM      ' TO W-SECTION                               
036300                                                                          
036400     MOVE 1                      TO PRIS-KDCALL                           
036410     MOVE IDPGM                  TO PRIS-IDPGM                            
036500     MOVE IN-IDARTNR             TO PRIS-IDARTNR                          
036600     MOVE WS-IDDISTR             TO PRIS-IDDISTR                          
036700                                    TEST-IDDISTR                          
036800     MOVE W-IDKUNDNR             TO PRIS-IDKUNDNR                         
036900     MOVE '11'                   TO PRIS-IDDC                             
037000     MOVE +4                     TO PRIS-KDORDKL                          
037100*    MOVE IN-KVANTAL             TO PRIS-KVBEART                          
037200     MOVE 1                      TO PRIS-KVBEART                          
037300     MOVE SPACE                  TO PRIS-FLINVEST                         
037400                                                                          
037500     CALL W335PRIS USING PRIS-W335PRIS                                    
037600                              ARTC-PCB                                    
037610                              WDK7-PCB                                    
037700                              GMTA-PCB                                    
037800                              BETA-PCB                                    
037900                              GPRIA-PCB                                   
038000                              GPRIB-PCB                                   
038100                     PRIS-COST-WDK6-PCB                                   
038200                     PRIS-COST-WDK7-PCB                                   
038300                     PRIS-COST-WDF1-PCB                                   
038400                     PRIS-COST-9305-PCB                                   
038410                     PRIS-COST-WDK72-PCB                                  
038420                     PRIS-COST-WDB6-PCB                                   
038500                                                                          
038600     IF PRIS-KDSVAR = '1'                                                 
038700        DISPLAY ' ARTIKEL SAKNAS PÅ ARTREG ' IN-IDDISTR                   
038800                 IN-IDKUNDNR IN-IDARTNR                                   
038900     END-IF                                                               
039000     IF PRIS-KDSVAR = '2'                                                 
039100        DISPLAY ' SEGMENT SAKNAS PÅ KUNDREG ' IN-IDDISTR                  
039200                 IN-IDKUNDNR IN-IDARTNR                                   
039300     END-IF                                                               
039400                                                                          
039500     .                                                                    
039600     EJECT                                                                
039700 D-SKAPA-WDA411-POST SECTION.                                             
039800     MOVE 'D-SKAPA-WDA4      ' TO W-SECTION                               
039900     SKIP2                                                                
040000     MOVE IN-IDARTNR             TO W-IDARTNR                             
040100                                                                          
040200     PERFORM IMS-GET-BENA                                                 
040300                                                                          
040400     IF SEGMENT-FINNS                                                     
040500       MOVE BEN-TEXT-BEART       TO BART-BEART                            
040600     ELSE                                                                 
040700       MOVE SPACE                TO BART-BEART                            
040800     END-IF                                                               
040900                                                                          
041000     PERFORM IMS-GET-WDK601                                               
041100     IF SEGMENT-FINNS                                                     
041200       MOVE ART-REKSIFFR         TO BART-REKSIFFR                         
041300     ELSE                                                                 
041400        MOVE ZERO                TO BART-REKSIFFR                         
041500     END-IF                                                               
041600                                                                          
041700     MOVE IN-IDARTNR             TO BART-IDARTNR                          
041800     MOVE IN-FLMATCH             TO BART-FLMATCH                          
041900     MOVE IN-KVANTAL             TO BART-KVANTAL                          
042000**   MOVE '68'                   TO BART-                                 
042100                                                                          
042200     IF DIST79-DEALER-PRICE OR                                            
042220        DIST79-ECOM-PRICE                                                 
042300*      COMPUTE WS-PRARTNTO        = PRIS-PRARTNTO / IN-KVANTAL            
042400       COMPUTE WS-PRARTNTO        = PRIS-PRARTNTO                         
042500       MOVE WS-PRARTNTO          TO BART-PRARTNTO-LOC                     
042600       MOVE PRIS-KDVALISO        TO BART-KDVALISO                         
042700       MOVE ZERO                 TO BART-PRARTNTO                         
042800     ELSE                                                                 
042900*      COMPUTE WS-PRARTNTO        = PRIS-PRARTNTO / IN-KVANTAL            
043000       COMPUTE WS-PRARTNTO        = PRIS-PRARTNTO                         
043100       MOVE WS-PRARTNTO          TO BART-PRARTNTO                         
043200       MOVE PRIS-KDVALISO        TO BART-KDVALISO                         
043300       MOVE ZERO                 TO BART-PRARTNTO-LOC                     
043400     END-IF                                                               
043500                                                                          
043600     COMPUTE WS-PRARTNTO-NEW = WS-PRARTNTO * (IN-REFOBNET / 100)          
043700     COMPUTE WS-PRARTNTO-TOT = WS-PRARTNTO-NEW * IN-KVANTAL               
043800                                                                          
043900     MOVE WS-PRARTNTO-NEW        TO BART-PRARTNTO-NEW                     
044000     MOVE WS-PRARTNTO-TOT        TO BART-PRARTNTO-TOT                     
044100     IF WS-PRARTNTO-TOT > WS-SUMINVD                                      
044200       MOVE 'J'                  TO BART-FLPRGRNS                         
044300     ELSE                                                                 
044400       MOVE 'N'                  TO BART-FLPRGRNS                         
044500     END-IF                                                               
044600                                                                          
044700     PERFORM IMS-ISRT-WDA411                                              
044800     ADD +1 TO CHKP-ANT                                                   
044900     .                                                                    
045000 Z-FINIT SECTION.                                                         
045100                                                                          
045200                                                                          
045300     CLOSE W41828                                                         
045400     SKIP2                                                                
045500     MOVE 'S' TO POSTSUM-OPKOD                                            
045600     CALL POSTSUM USING POSTSUM-PARM                                      
045700     .                                                                    
045800     EJECT                                                                
045900 S01-LAES-W41828  SECTION.                                                
046000     SKIP2                                                                
046100     READ W41828 INTO IN-AREA                                             
046200     AT END                                                               
046300*       MOVE HIGH-VALUE TO IN-ID                                          
046400        SET END-OF-W41828 TO TRUE                                         
046500                                                                          
046600     NOT AT END                                                           
046700        MOVE 'W41828'   TO POSTSUM-FDNAMN                                 
046800        MOVE 'W41829D1' TO POSTSUM-DDNAMN2                                
046900        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
047000        CALL POSTSUM USING POSTSUM-PARM                                   
047100                                                                          
047200        ADD 1 TO W-W41828-KVPOST-IN                                       
047300     END-READ                                                             
047400     .                                                                    
047500     EJECT                                                                
047600 S10-SKAPA-MAIL  SECTION.                                                 
047700      PERFORM S90-OPEN-DAP-SEND                                           
047800      MOVE 001             TO HDR-REQU-IDMSGVER                           
047900      MOVE SPACE           TO HDR-REQU-KDPGMACT                           
048000      MOVE 'W41829'        TO HDR-REQU-IDUSER                             
048100                                                                          
048200      MOVE 'BUYBACK-FIL-FEL'   TO HDR-IDOUTTYPE                           
048300      MOVE SPACE               TO HDR-IDOUTREC                            
048400      MOVE 'W41829'            TO HDR-IDOUTREC                            
048500      MOVE DAGENS-DATUM        TO HDR-IDLIST                              
048600      PERFORM S90-PUT-DAP-HEADER                                          
048700      MOVE WS-MAIL-TEXT        TO DOC-LINE                                
048800                                                                          
048900      PERFORM S90-PUT-DOC                                                 
049000      PERFORM S90-CLOSE-DAP-SEND                                          
049100     .                                                                    
049200     EJECT                                                                
049300 S90-OPEN-DAP-SEND SECTION.                                               
049400*    MOVE 'S90-OPEN-DAP-S' TO CURR-SECTION                                
049500                                                                          
049600     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
049700     MOVE 'OPEN'                     TO SEND-KDFUNC                       
049800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
049900                         SEND-OPEN-AREA                                   
050000     IF SEND-KDRC > ZERO                                                  
050100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
050200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
050300       DELIMITED BY SIZE INTO FELTEXT                                     
050400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050500     END-IF                                                               
050600     .                                                                    
050700                                                                          
050800 S90-CLOSE-DAP-SEND SECTION.                                              
050900*    MOVE 'S90-CLOSE-DAP-' TO CURR-SECTION                                
051000                                                                          
051100     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
051200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
051300                                                                          
051400     IF SEND-KDRC > 0                                                     
051500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
051600       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
051700       DELIMITED BY SIZE INTO FELTEXT                                     
051800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
051900     END-IF                                                               
052000     .                                                                    
052100                                                                          
052200 S90-PUT-DAP-HEADER SECTION.                                              
052300*    MOVE 'S90-PUT-DAP-HE' TO CURR-SECTION                                
052400                                                                          
052500     MOVE 'PUT'                           TO SEND-KDFUNC                  
052600     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
052700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
052800                         SEND-KVDLEN                                      
052900                         HDR-AREA                                         
053000     IF SEND-KDRC > ZERO                                                  
053100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
053200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
053300       DELIMITED BY SIZE INTO FELTEXT                                     
053400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
053500     END-IF                                                               
053600     .                                                                    
053700 S90-PUT-DOC      SECTION.                                                
053800*    MOVE 'S90-PUT-DOC ' TO CURR-SECTION                                  
053900                                                                          
054000     MOVE 'PUT'                           TO SEND-KDFUNC                  
054100     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
054200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
054300                         SEND-KVDLEN                                      
054400                         DOC-AREA                                         
054500     IF SEND-KDRC > ZERO                                                  
054600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
054700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
054800       DELIMITED BY SIZE INTO FELTEXT                                     
054900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
055000     END-IF                                                               
055100     .                                                                    
055200 X-TAG-CHECKPOINT   SECTION.                                              
055300                                                                          
055400* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
055500* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
055600     PERFORM IMS-CHECKPOINT                                               
055700     MOVE ZERO TO CHKP-ANT                                                
055800* --- LÄS OM DATABAS OM DET BEHÖVS                                        
055900     .                                                                    
056000     EJECT                                                                
056100* --- IMS SEKTIONER ---                                                   
056200                                                                          
056300     EJECT                                                                
056400*                                                                         
056500*IMS-GET-WDA201 SECTION.                                                  
056600*                                                                         
056700*    STRING 'WDA201  (IDDISTR  =' W-IDDISTR-X ')'                         
056800*         DELIMITED BY SIZE INTO SSA1                                     
056900*    MOVE '  GE' TO GODK-STATUSKODER                                      
057000*    CALL CBLTDLI USING GU WDA2-PCB DLI-IO-WDA201 SSA1                    
057100*    MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
057200*    PERFORM IMS-STATUSKONTROLL                                           
057300*    .                                                                    
057400*    EJECT                                                                
057500 IMS-GET-WDA201 SECTION.                                                  
057600     MOVE 'IMS-GET-WDA201     ' TO W-IMS-SECTION                          
057700     MOVE '*** IMS-GET-WDA201   *** '                                     
057800                              TO FELTEXT                                  
057900     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
058000            DELIMITED BY SIZE INTO SSA1                                   
058100     MOVE '  GE' TO GODK-STATUSKODER                                      
058200     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-WDA201 SSA1                    
058300     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
058400     PERFORM IMS-STATUSKONTROLL                                           
058500     .                                                                    
058600     EJECT                                                                
058700     SKIP3                                                                
058800 IMS-GET-WDA401 SECTION.                                                  
058900     MOVE 'IMS-GET-WDA401     ' TO W-IMS-SECTION                          
059000     MOVE '*** IMS-GET-WDA401   *** '                                     
059100                              TO FELTEXT                                  
059200     STRING 'WDA401  (WDA401KY =' W-WDA401KY-X ')'                        
059300            DELIMITED BY SIZE INTO SSA1                                   
059400     MOVE '  GE' TO GODK-STATUSKODER                                      
059500     CALL CBLTDLI USING GU WDA4-PCB DLI-IO-WDA401 SSA1                    
059600     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     .                                                                    
059900     EJECT                                                                
060000     SKIP3                                                                
060100 IMS-ISRT-WDA401 SECTION.                                                 
060200     MOVE 'IMS-ISRT-WDA401     ' TO W-IMS-SECTION                         
060300                                                                          
060400     MOVE 'WDA401 ' TO SSA1                                               
060500     MOVE '  II' TO GODK-STATUSKODER                                      
060600     CALL CBLTDLI USING ISRT WDA4-PCB DLI-IO-WDA401 SSA1                  
060700     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
060800     PERFORM IMS-STATUSKONTROLL                                           
060900     .                                                                    
061000     SKIP3                                                                
061100 IMS-REPL-WDA401 SECTION.                                                 
061200                                                                          
061300     MOVE '  ' TO GODK-STATUSKODER                                        
061400     CALL CBLTDLI USING REPL WDA4-PCB DLI-IO-WDA401                       
061500     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
061600     PERFORM IMS-STATUSKONTROLL                                           
061700     .                                                                    
061800     SKIP3                                                                
061900 IMS-DLET-WDA401 SECTION.                                                 
062000                                                                          
062100     MOVE '  ' TO GODK-STATUSKODER                                        
062200     CALL CBLTDLI USING DLET WDA4-PCB DLI-IO-WDA401                       
062300     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
062400     PERFORM IMS-STATUSKONTROLL                                           
062500     .                                                                    
062600     EJECT                                                                
062700 IMS-GET-WDA411 SECTION.                                                  
062800                                                                          
062900     STRING 'WDA411  (IDARTNR  =' W-IDARTNR-X ')'                         
063000          DELIMITED BY SIZE INTO SSA1                                     
063100     MOVE '  GE' TO GODK-STATUSKODER                                      
063200     CALL CBLTDLI USING GHNP WDA4-PCB DLI-IO-WDA411 SSA1                  
063300     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
063400     PERFORM IMS-STATUSKONTROLL                                           
063500     .                                                                    
063600     SKIP3                                                                
063700 IMS-ISRT-WDA411 SECTION.                                                 
063800     MOVE 'IMS-ISRT-WDA411     ' TO W-IMS-SECTION                         
063900                                                                          
064000     STRING 'WDA401  (WDA401KY =' W-WDA401KY-X ')'                        
064100          DELIMITED BY SIZE INTO SSA1                                     
064200     MOVE 'WDA411 ' TO SSA2                                               
064300     MOVE '  II' TO GODK-STATUSKODER                                      
064400     CALL CBLTDLI USING ISRT WDA4-PCB DLI-IO-WDA411 SSA1 SSA2             
064500     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     .                                                                    
064800     SKIP3                                                                
064900 IMS-REPL-WDA411 SECTION.                                                 
065000                                                                          
065100     MOVE '  ' TO GODK-STATUSKODER                                        
065200     CALL CBLTDLI USING REPL WDA4-PCB DLI-IO-WDA411                       
065300     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     .                                                                    
065600     SKIP3                                                                
065700 IMS-DLET-WDA411 SECTION.                                                 
065800                                                                          
065900     MOVE '  ' TO GODK-STATUSKODER                                        
066000     CALL CBLTDLI USING DLET WDA4-PCB DLI-IO-WDA411                       
066100     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
066200     PERFORM IMS-STATUSKONTROLL                                           
066300     .                                                                    
066400     EJECT                                                                
066500 IMS-RESTART SECTION.                                                     
066600     SKIP2                                                                
066700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
066800     MOVE '  ' TO GODK-STATUSKODER                                        
066900     CALL CBLTDLI USING XRST MSG-PCB                                      
067000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
067100                        CHKP-AREA-LENGTH CHKP-AREA                        
067200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067300     PERFORM IMS-STATUSKONTROLL                                           
067400     .                                                                    
067500     SKIP3                                                                
067600 IMS-CHECKPOINT SECTION.                                                  
067700     SKIP2                                                                
067800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
067900     MOVE '  XD' TO GODK-STATUSKODER                                      
068000     CALL CBLTDLI USING CHKP MSG-PCB                                      
068100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
068200                        CHKP-AREA-LENGTH CHKP-AREA                        
068300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068400     PERFORM IMS-STATUSKONTROLL                                           
068500                                                                          
068600     IF IMS-EJ-OK                                                         
068700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
068800       DISPLAY FELTEXT                                                    
068900       CALL FELLOG                                                        
069000     END-IF                                                               
069100     .                                                                    
069200     EJECT                                                                
069300 IMS-GET-BENA SECTION.                                                    
069400     MOVE 'IMS-GET-BENA        ' TO W-IMS-SECTION                         
069500                                                                          
069600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
069700     DELIMITED BY SIZE INTO SSA1                                          
069800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
069900     DELIMITED BY SIZE INTO SSA2                                          
070000     MOVE '  GE' TO GODK-STATUSKODER                                      
070100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
070200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
070300     PERFORM IMS-STATUSKONTROLL                                           
070400     .                                                                    
070500     EJECT                                                                
070600********************** WDK6 ARTIKELREGISTER CDC ******************        
070700                                                                          
070800 IMS-GET-WDK601     SECTION.                                              
070900     MOVE 'IMS-GET-WDK6        ' TO W-IMS-SECTION                         
071000     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
071100            DELIMITED BY SIZE INTO SSA1                                   
071200     MOVE '  GE' TO GODK-STATUSKODER                                      
071300     CALL  CBLTDLI  USING GHU WDK6-PCB DLI-IO-WDK601   SSA1               
071400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     .                                                                    
071700     SKIP3                                                                
071800 IMS-STATUSKONTROLL SECTION.                                              
071900     SKIP2                                                                
072000     SET STATUS-IX TO 1                                                   
072100     SEARCH GODK-STATUS                                                   
072200       AT END                                                             
072300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
072400           DELIMITED BY SIZE INTO FELTEXT                                 
072500         DISPLAY FELTEXT                                                  
072600         CALL FELLOG                                                      
072700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
072800         CONTINUE                                                         
072900     END-SEARCH                                                           
073000     .                                                                    
