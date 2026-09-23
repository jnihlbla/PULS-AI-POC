000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3352000.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   93/10/14.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001000*        PRISGRUPPSINFORMATION                                            
001100*        UPPDATERAR KUNDREGISTRET MED EN FIL FRÅN RESP                    
001200*        MARKNAD.                                                         
001300*        FILEN INNEHÅLLER KAMPANJRABATTER.                                
001400*                                                                         
001410*                                                                         
001500*        PROGRAMMET UPPDATERAR WLPRIB (WDC2)                              
001600*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- NYA RABATTSTRUKTURER                                       
003000     SELECT W33506                     ASSIGN TO W33520D1.                
003100     SKIP2                                                                
003200*          --- RABATTSTRUKTURER SOM SAKNAR PRIS-OMR                       
003300     SELECT W33521                     ASSIGN TO W33520D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W33506                                                               
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300*01  -COPY W335201B  -L.                                                  
004400     SKIP2                                                                
004500 FD  W33521                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800     SKIP2                                                                
004900*01  POST   -COPY W335201B  -PRE FEL-   -L.                               
005000 WORKING-STORAGE SECTION.                                                 
005010*    -COPY WY2000W1                                                       
005100                                                                          
005200 01  CHKP-VAR.                                                            
005300 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005400 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005500 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005600 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005700 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005800 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005900 77  IDPGM                       PIC X(8)    VALUE 'W3352000'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  UPPDATERA                   PIC X       VALUE 'J'.                   
006300 77  IX                          PIC 9(2)    COMP SYNC.                   
006500 77  SPAR-TISTODAT               PIC 9(7)    VALUE ZERO.                  
006600 77  SPAR-IDPROMR                PIC X(3)    VALUE SPACE.                 
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007100                                                                          
007200 77  W33506-EOF-SW               PIC X       VALUE 'N'.                   
007300     88  END-OF-W33506                       VALUE 'J'.                   
007400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007500                                                                          
007600 77  NY-TABELL-SW                PIC X       VALUE 'N'.                   
007700     88  NY-TABELL                           VALUE 'J'.                   
007800     EJECT                                                                
007900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007910     EJECT                                                                
008500 01  DYNAMISKA-SUBPROGRAM.                                                
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600 01  AMC-AREA-START              PIC X(24)   VALUE                        
009700                                             'AMC-AREA-START'.            
009800     SKIP2                                                                
010000*01  AREA -COPY W335201B   -PRE AMC-                                      
010100*                                                                         
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500 01  NYCKLAR-TILL-DLI.                                                    
010600     03  W-IDPROMR-X.                                                     
010700         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
010800     03  W-WDC212KY-X.                                                    
010900         05  W-KDARTKAM          PIC 9(5)   VALUE ZERO.                   
011000         05  W-DASTADAT          PIC 9(8)   VALUE ZERO.                   
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011800     88  IMS-EJ-OK                           VALUE 'XD'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013100                                                                          
013200 01  DLI-IO-PRIB01.                                                       
013600*  03  -COPY WDC201  -PRE PRIB-                                           
013700     EJECT                                                                
013710 01  DLI-IO-PRIB12.                                                       
013900*  03  -COPY WDC212  -PRE KAMP-                                           
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014300*01  -COPY W0009   -PRE MSG-                                              
014400     EJECT                                                                
014500*01  -COPY W0008  -PRE PRIB-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION  USING MSG-PCB  PRIB-PCB.                             
014810 MAIN SECTION.                                                            
014900     ENTRY 'DLITCBL' USING MSG-PCB  PRIB-PCB.                             
015000                                                                          
015200     PERFORM A-INIT                                                       
015300     PERFORM S01-LAES-W33506                                              
015400     PERFORM UNTIL END-OF-W33506                                          
015500       IF CHKP-ANT > CHKP-MAX                                             
015600         PERFORM X-TAG-CHECKPOINT                                         
015700       END-IF                                                             
015800       MOVE AMC-IDPROMR TO W-IDPROMR-X                                    
015900       MOVE AMC-IDPROMR TO SPAR-IDPROMR                                   
016000       PERFORM B-UPPDATERA-PRIB-KAMP                                      
016100     END-PERFORM                                                          
016200                                                                          
016300                                                                          
016400     PERFORM Z-FINIT                                                      
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017200                                                                          
017300     PERFORM IMS-RESTART                                                  
017400     ACCEPT DAGENS-DATUM FROM DATE                                        
017500     OPEN INPUT W33506                                                    
017600     OPEN OUTPUT W33521                                                   
017700                                                                          
017900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018000     .                                                                    
018100     EJECT                                                                
018300 B-UPPDATERA-PRIB-KAMP SECTION.                                           
018500                                                                          
018600     PERFORM IMS-GET-PRIB-PRO                                             
018700     IF SEGMENT-FINNS                                                     
018800       PERFORM UNTIL END-OF-W33506 OR                                     
018900         AMC-IDPROMR NOT = SPAR-IDPROMR                                   
019000         IF AMC-KDARTKAM > ZERO                                           
019100                                                                          
019200           PERFORM IMS-GHNP-KAMP-FIRST                                    
019300           IF SEGMENT-FINNS                                               
019400             PERFORM BA-KOLLA-KOD-DATUM                                   
019500             PERFORM BB-UPPDATERA                                         
019600           ELSE                                                           
019610             MOVE DAGENS-DATUM  TO TMP1-YYMMDD                            
019620             MOVE AMC-TISTODAT  TO TMP2-YYMMDD                            
019630             PERFORM WY2000P1                                             
019640                                                                          
019700             IF TMP1-YYMMDD <= TMP2-YYMMDD                                
019800               MOVE AMC-TISTADAT      TO KAMP-KAM-DASTADAT                
019810*---Y2K-FIX*******                                                        
019820               IF AMC-TISTADAT NOT = ZERO                                 
019830                 IF AMC-TISTADAT < 500000                                 
019840                   MOVE 20          TO KAMP-KAM-DASTADAT(1:2)             
019850                 ELSE                                                     
019860                   IF AMC-TISTADAT < 999999                               
019870                     MOVE 19        TO KAMP-KAM-DASTADAT(1:2)             
019880                   ELSE                                                   
019890                     MOVE 99999999  TO KAMP-KAM-DASTADAT                  
019891                   END-IF                                                 
019892                 END-IF                                                   
019893               END-IF                                                     
019900               MOVE AMC-TISTODAT      TO KAMP-KAM-TISTODAT                
020000               MOVE AMC-KDARTKAM      TO KAMP-KAM-KDARTKAM                
020100               MOVE AMC-REARTRAB-DO   TO KAMP-KAM-REARTRAB-DO             
020200               MOVE AMC-REARTRAB-BULK TO KAMP-KAM-REARTRAB-BULK           
020300               PERFORM IMS-ISRT-PRIB-KAMP                                 
020400               ADD +1 TO CHKP-ANT                                         
020500             END-IF                                                       
020600           END-IF                                                         
020700         ELSE                                                             
020800           PERFORM BC-SKRIV-FELFIL                                        
020900         END-IF                                                           
021000         PERFORM S01-LAES-W33506                                          
021100       END-PERFORM                                                        
021200     ELSE                                                                 
021300       PERFORM BC-SKRIV-FELFIL                                            
021400       PERFORM S01-LAES-W33506                                            
021500     END-IF                                                               
021600     .                                                                    
021700     EJECT                                                                
021800 BA-KOLLA-KOD-DATUM SECTION.                                              
021900                                                                          
022000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
022100       IF AMC-KDARTKAM = KAMP-KAM-KDARTKAM                                
022110       MOVE AMC-TISTADAT           TO TMP1-YYMMDD                         
022120       MOVE KAMP-KAM-DASTADAT(3:6) TO TMP2-YYMMDD                         
022130       MOVE AMC-TISTODAT           TO TMP3-YYMMDD                         
022131       MOVE KAMP-KAM-TISTODAT      TO TMP4-YYMMDD                         
022140       PERFORM WY2000Q1                                                   
022200         IF TMP1-YYMMDD  <= TMP2-YYMMDD                                   
022300           AND TMP3-YYMMDD  >= TMP2-YYMMDD                                
022400           PERFORM IMS-DLET-KAMP                                          
022500           ADD +1 TO CHKP-ANT                                             
022600         ELSE                                                             
022700                                                                          
022800           IF TMP1-YYMMDD  >= TMP2-YYMMDD                                 
022900              AND TMP1-YYMMDD  <= TMP4-YYMMDD                             
023000             PERFORM IMS-DLET-KAMP                                        
023100             ADD +1 TO CHKP-ANT                                           
023200           END-IF                                                         
023300         END-IF                                                           
023400       END-IF                                                             
023500       PERFORM IMS-GHNP-KAMP-SEG                                          
023600     END-PERFORM                                                          
023700     .                                                                    
023800     EJECT                                                                
023900 BB-UPPDATERA SECTION.                                                    
024000                                                                          
024200     MOVE AMC-TISTADAT      TO KAMP-KAM-DASTADAT                          
024210*---Y2K-FIX*******                                                        
024220     IF AMC-TISTADAT NOT = ZERO                                           
024230       IF AMC-TISTADAT < 500000                                           
024240         MOVE 20          TO KAMP-KAM-DASTADAT(1:2)                       
024250       ELSE                                                               
024260         IF AMC-TISTADAT < 999999                                         
024270           MOVE 19        TO KAMP-KAM-DASTADAT(1:2)                       
024280         ELSE                                                             
024290           MOVE 99999999  TO KAMP-KAM-DASTADAT                            
024291         END-IF                                                           
024292       END-IF                                                             
024293     END-IF                                                               
024300     MOVE AMC-TISTODAT      TO KAMP-KAM-TISTODAT                          
024400     MOVE AMC-KDARTKAM      TO KAMP-KAM-KDARTKAM                          
024500     MOVE AMC-REARTRAB-DO   TO KAMP-KAM-REARTRAB-DO                       
024600     MOVE AMC-REARTRAB-BULK TO KAMP-KAM-REARTRAB-BULK                     
024700     PERFORM IMS-ISRT-PRIB-KAMP                                           
024800     ADD +1 TO CHKP-ANT                                                   
024900     IF SEGMENT-FINNS-REDAN                                               
025000        PERFORM BBA-BYT-TILL-NYTT                                         
025100     END-IF                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 BBA-BYT-TILL-NYTT SECTION.                                               
025600*--- BYT GAMMALT SEGMENT MOT NYTT.                                        
025700                                                                          
025800     MOVE AMC-TISTADAT      TO W-DASTADAT                                 
025810*---Y2K-FIX*******                                                        
025820     IF AMC-TISTADAT NOT = ZERO                                           
025830       IF AMC-TISTADAT < 500000                                           
025840         MOVE 20          TO W-DASTADAT(1:2)                              
025850       ELSE                                                               
025860         IF AMC-TISTADAT < 999999                                         
025870           MOVE 19        TO W-DASTADAT(1:2)                              
025880         ELSE                                                             
025890           MOVE 99999999  TO W-DASTADAT                                   
025891         END-IF                                                           
025892       END-IF                                                             
025893     END-IF                                                               
025900     MOVE AMC-KDARTKAM      TO W-KDARTKAM                                 
026000     PERFORM IMS-GHU-KAMP-SEG                                             
026100     IF SEGMENT-FINNS                                                     
026200       MOVE AMC-TISTODAT      TO KAMP-KAM-TISTODAT                        
026300       MOVE AMC-REARTRAB-DO   TO KAMP-KAM-REARTRAB-DO                     
026400       MOVE AMC-REARTRAB-BULK TO KAMP-KAM-REARTRAB-BULK                   
026500       PERFORM IMS-REPL-KAMP-SEG                                          
026600       ADD +1 TO CHKP-ANT                                                 
026700     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 BC-SKRIV-FELFIL SECTION.                                                 
027200                                                                          
027300     WRITE FEL-POST FROM AMC-AREA                                         
027400     MOVE SPACE TO POSTSUM-TRANSTYP                                       
027500     MOVE 'W33521' TO POSTSUM-FDNAMN                                      
027600     MOVE 'W33020D2' TO POSTSUM-DDNAMN2                                   
027700     CALL POSTSUM USING POSTSUM-PARM                                      
027800     .                                                                    
027900     EJECT                                                                
028000 Z-FINIT SECTION.                                                         
028100                                                                          
028300     CLOSE W33506                                                         
028400           W33521                                                         
028500                                                                          
028600     MOVE 'S' TO POSTSUM-OPKOD                                            
028700     CALL POSTSUM USING POSTSUM-PARM                                      
028800     .                                                                    
028900     EJECT                                                                
029000 S01-LAES-W33506  SECTION.                                                
029100                                                                          
029200     READ W33506 INTO AMC-AREA                                            
029300     AT END                                                               
029400     MOVE JA TO W33506-EOF-SW                                             
029500                                                                          
029600     NOT AT END                                                           
029700        MOVE 'W33506' TO POSTSUM-FDNAMN                                   
029800        MOVE 'W33520D1' TO POSTSUM-DDNAMN2                                
029900        MOVE 'AMC'      TO POSTSUM-TRANSTYP                               
030000        CALL POSTSUM USING POSTSUM-PARM                                   
030100     END-READ                                                             
030200     .                                                                    
030300     EJECT                                                                
030400 X-TAG-CHECKPOINT   SECTION.                                              
030500                                                                          
030800     PERFORM IMS-CHECKPOINT                                               
030900     MOVE ZERO TO CHKP-ANT                                                
031100     .                                                                    
031200     EJECT                                                                
031300* --- IMS SEKTIONER ---                                                   
031400                                                                          
031500 IMS-GET-PRIB-PRO SECTION.                                                
031600     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
031700          DELIMITED BY SIZE INTO SSA1                                     
031800     MOVE '  GE' TO GODK-STATUSKODER                                      
031900     CALL CBLTDLI USING GHU PRIB-PCB DLI-IO-PRIB01 SSA1                   
032000     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
032100     PERFORM IMS-STATUSKONTROLL                                           
032200     .                                                                    
032400                                                                          
032410     SKIP3                                                                
032500 IMS-GHNP-KAMP-FIRST SECTION.                                             
032600                                                                          
032700     MOVE 'WLPRIB12*F '  TO SSA1                                          
032800     MOVE '  GE' TO GODK-STATUSKODER                                      
032900     CALL CBLTDLI USING GHNP PRIB-PCB DLI-IO-PRIB12 SSA1                  
033000     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     SKIP3                                                                
033300     .                                                                    
033400 IMS-GHNP-KAMP-SEG SECTION.                                               
033500                                                                          
033600     MOVE 'WLPRIB12 '  TO SSA1                                            
033700     MOVE '  GE' TO GODK-STATUSKODER                                      
033800     CALL CBLTDLI USING GHNP PRIB-PCB DLI-IO-PRIB12 SSA1                  
033900     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
034000     PERFORM IMS-STATUSKONTROLL                                           
034100     .                                                                    
034200     EJECT                                                                
034300 IMS-ISRT-PRIB-KAMP SECTION.                                              
034400                                                                          
034500     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
034600          DELIMITED BY SIZE INTO SSA1                                     
034700     MOVE 'WLPRIB12 ' TO SSA2                                             
034800     MOVE '  II' TO GODK-STATUSKODER                                      
034900     CALL CBLTDLI USING ISRT PRIB-PCB DLI-IO-PRIB12 SSA1 SSA2             
035000     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
035100     PERFORM IMS-STATUSKONTROLL                                           
035300     .                                                                    
035310     SKIP2                                                                
035400 IMS-GHU-KAMP-SEG   SECTION.                                              
035500     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
035600          DELIMITED BY SIZE INTO SSA1                                     
035700     STRING 'WLPRIB12(WDC212KY =' W-WDC212KY-X ')'                        
035800          DELIMITED BY SIZE INTO SSA2                                     
035900     MOVE '  ' TO GODK-STATUSKODER                                        
036000     CALL CBLTDLI USING GHU PRIB-PCB DLI-IO-PRIB12 SSA1 SSA2              
036100     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
036200     PERFORM IMS-STATUSKONTROLL                                           
036400     .                                                                    
036410                                                                          
036500     EJECT                                                                
036600 IMS-DLET-KAMP  SECTION.                                                  
036700     MOVE '  ' TO GODK-STATUSKODER                                        
036800     CALL CBLTDLI USING DLET PRIB-PCB DLI-IO-PRIB12                       
036900     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
037000     PERFORM IMS-STATUSKONTROLL                                           
037200     .                                                                    
037210     SKIP2                                                                
037300 IMS-REPL-KAMP-SEG  SECTION.                                              
037400     MOVE '  ' TO GODK-STATUSKODER                                        
037500     CALL CBLTDLI USING REPL PRIB-PCB DLI-IO-PRIB12                       
037600     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
037700     PERFORM IMS-STATUSKONTROLL                                           
037900     .                                                                    
038000                                                                          
038010     EJECT                                                                
038100 IMS-RESTART SECTION.                                                     
038200                                                                          
038300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
038400     MOVE '  ' TO GODK-STATUSKODER                                        
038500     CALL CBLTDLI USING XRST MSG-PCB                                      
038600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
038700                        CHKP-AREA-LENGTH CHKP-AREA                        
038800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038900     PERFORM IMS-STATUSKONTROLL                                           
039000     .                                                                    
039100     SKIP3                                                                
039200 IMS-CHECKPOINT SECTION.                                                  
039300                                                                          
039400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
039500     MOVE '  XD' TO GODK-STATUSKODER                                      
039600     CALL CBLTDLI USING CHKP MSG-PCB                                      
039700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
039800                        CHKP-AREA-LENGTH CHKP-AREA                        
039900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040000     PERFORM IMS-STATUSKONTROLL                                           
040100                                                                          
040200     IF IMS-EJ-OK                                                         
040300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
040400       DISPLAY FELTEXT                                                    
040500       CALL FELLOG                                                        
040600     END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900 IMS-STATUSKONTROLL SECTION.                                              
041000                                                                          
041100     SET STATUS-IX TO 1                                                   
041200     SEARCH GODK-STATUS                                                   
041300       AT END                                                             
041400         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
041500         DISPLAY FELTEXT                                                  
041600         CALL FELLOG                                                      
041700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041800         CONTINUE                                                         
041900     END-SEARCH                                                           
042000     .                                                                    
042100*    -COPY WY2000Q1                                                       
042110*    -COPY WY2000P1                                                       
042200                                                                          
