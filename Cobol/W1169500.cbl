000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1169500.                                                
000400 AUTHOR.         HÅKAN BOHLIN.                                            
000500 DATE-WRITTEN.   18/01/11.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001000*        UPPDATERAR SÄLJBOLAGS SUGGESTED RETAIL PRISER.                   
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDC3                                       
001300*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- FIL MED PRISER FRÅN PRICE SC                               
002700     SELECT W11695                     ASSIGN TO W11695D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W11695                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600     SKIP2                                                                
003700*01  -COPY W11695      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE '11695100'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 01  CHKP-VAR.                                                            
004500 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004600 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004700 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004800 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004900 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005000 03  CHKP-MAX                    PIC S9(3)   VALUE +200.                  
008100     SKIP2                                                                
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008500                                                                          
008600 77  W11695-EOF-SW               PIC X       VALUE 'N'.                   
008700     88  END-OF-W11695                       VALUE 'J'.                   
008800     EJECT                                                                
008900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009000 01  FILLER REDEFINES DAGENS-DATUM.                                       
009100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009400     EJECT                                                                
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL ABEND                                            
010300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800     EJECT                                                                
010900 01  IN-AREA-START               PIC X(24)   VALUE                        
011000                                             'IN-AREA-START'.             
011100     SKIP2                                                                
011200                                                                          
011300*01  AREA -COPY W11695    -PRE IN-                                        
011400*                                                                         
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
011800 01  NYCKLAR-TILL-DLI.                                                    
011900     03  W-WDC301KY-X.                                                    
012100         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
012110     03  W-WDC311KY-X.                                                    
012120         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
012200     SKIP2                                                                
012300*    --- STATUS-KOD FRÅN IMS                                              
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FINNS                       VALUE '  '.                  
012600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012900     88  IMS-EJ-OK                           VALUE 'XD'.                  
013000     SKIP2                                                                
013100 01  GODK-STATUSKODER.                                                    
013200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013300     SKIP3                                                                
013400 01  SSA1                        PIC X(64).                               
013500 01  SSA2                        PIC X(64).                               
013600     EJECT                                                                
013700*    --- IMS FUNKTIONSKODER                                               
013800*01  -COPY W0003                                                          
013900     EJECT                                                                
014000*    ---  DLI INPUT-OUTPUT AREA                                           
014100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDC301'.         
014300 01  DLI-IO-WDC301.                                                       
014700*    05  -COPY WDC301                                                     
014701 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDC311'.         
014710 01  DLI-IO-WDC311.                                                       
014720*    05  -COPY WDC311                                                     
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015100*01  -COPY W0009  -PRE MSG-                                               
015200     EJECT                                                                
015300*01  -COPY W0008  -PRE WDC3-                                              
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600 PROCEDURE DIVISION  USING MSG-PCB WDC3-PCB.                              
015610 MAIN SECTION.                                                            
015700     ENTRY 'DLITCBL' USING MSG-PCB WDC3-PCB.                              
015800                                                                          
016000     PERFORM A-INIT                                                       
016100     PERFORM S01-LAES-W11695                                              
016200     PERFORM UNTIL END-OF-W11695                                          
016300       IF CHKP-ANT > CHKP-MAX                                             
016400         PERFORM X-TAG-CHECKPOINT                                         
016500       END-IF                                                             
016600       IF IN-PRARTBTO-SC > ZERO                                           
016700         PERFORM B-BEHANDLA-INPOST                                        
             END-IF                                                             
017300       PERFORM S01-LAES-W11695                                            
017400     END-PERFORM                                                          
017500     PERFORM Z-FINIT                                                      
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018200                                                                          
018300     PERFORM IMS-RESTART                                                  
018400     OPEN INPUT W11695                                                    
018500                                                                          
018700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018720     ACCEPT DAGENS-DATUM FROM DATE                                        
018800     .                                                                    
018900     EJECT                                                                
019000 B-BEHANDLA-INPOST SECTION.                                               
019100                                                                          
019200     MOVE IN-IDARTNR   TO W-IDARTNR                                       
019400     PERFORM IMS-GU-WDC301                                                
019500     IF SEGMENT-FINNS                                                     
019600        PERFORM BA-JUSTERA-PRIS                                           
019700     ELSE                                                                 
              MOVE W-IDARTNR TO RART-IDARTNR                                    
              PERFORM IMS-ISRT-WDC301                                           
              ADD  +1 TO CHKP-ANT                                               
019800        PERFORM BB-NYTT-PRIS                                              
019900     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 BA-JUSTERA-PRIS SECTION.                                                 
020300                                                                          
021100     MOVE IN-IDLANDX2    TO W-IDLANDX2                                    
           PERFORM IMS-GHNP-WDC311                                              
           IF SEGMENT-FINNS                                                     
             IF IN-PRARTBTO-SC = SRP-PRARTBTO-SC                                
               CONTINUE                                                         
             ELSE                                                               
021200         MOVE IN-KDVALISO    TO SRP-KDVALISO                              
021300         MOVE IN-PRARTBTO-SC TO SRP-PRARTBTO-SC                           
021510         MOVE DAGENS-DATUM   TO SRP-TIUPPDAT                              
020700         PERFORM IMS-REPL-WDC311                                          
               ADD  +1 TO CHKP-ANT                                              
             END-IF                                                             
           ELSE                                                                 
021100       MOVE IN-IDLANDX2    TO SRP-IDLANDX2                                
021200       MOVE IN-KDVALISO    TO SRP-KDVALISO                                
021300       MOVE IN-PRARTBTO-SC TO SRP-PRARTBTO-SC                             
021510       MOVE DAGENS-DATUM   TO SRP-TIUPPDAT                                
020700       PERFORM IMS-ISRT-WDC311                                            
             ADD  +1 TO CHKP-ANT                                                
           END-IF                                                               
020800     .                                                                    
020900 BB-NYTT-PRIS SECTION.                                                    
021000                                                                          
021100     MOVE IN-IDLANDX2    TO SRP-IDLANDX2                                  
021200     MOVE IN-KDVALISO    TO SRP-KDVALISO                                  
021300     MOVE IN-PRARTBTO-SC TO SRP-PRARTBTO-SC                               
021510     MOVE DAGENS-DATUM   TO SRP-TIUPPDAT                                  
021600     PERFORM IMS-ISRT-WDC311                                              
           ADD  +1 TO CHKP-ANT                                                  
021700     .                                                                    
021800     EJECT                                                                
021900 X-TAG-CHECKPOINT   SECTION.                                              
022000                                                                          
022300     PERFORM IMS-CHECKPOINT                                               
022400     MOVE ZERO TO CHKP-ANT                                                
022600     .                                                                    
022700     EJECT                                                                
022800* --- IMS SEKTIONER ---                                                   
023100 Z-FINIT SECTION.                                                         
023200                                                                          
023400     CLOSE W11695                                                         
023500                                                                          
023600     MOVE 'S' TO POSTSUM-OPKOD                                            
023700     CALL POSTSUM USING POSTSUM-PARM                                      
023800     .                                                                    
023900     EJECT                                                                
024000 S01-LAES-W11695  SECTION.                                                
024100                                                                          
024200     READ W11695 INTO IN-AREA                                             
024300     AT END                                                               
024400        SET END-OF-W11695 TO TRUE                                         
024500                                                                          
024600     NOT AT END                                                           
024700        MOVE 'W11695' TO POSTSUM-FDNAMN                                   
024800        MOVE 'W11695D1' TO POSTSUM-DDNAMN2                                
024900        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
025000        CALL POSTSUM USING POSTSUM-PARM                                   
025100     END-READ                                                             
025200     .                                                                    
025300 S99-ABEND SECTION.                                                       
025400                                                                          
025500     SKIP2                                                                
025600     MOVE 'S' TO POSTSUM-OPKOD                                            
025700     CALL POSTSUM USING POSTSUM-PARM                                      
025800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
025900     .                                                                    
026000     EJECT                                                                
026100* --- IMS SEKTIONER ---                                                   
026300                                                                          
026400 IMS-GU-WDC301 SECTION.                                                   
026500     STRING 'WDC301  (IDARTNR  =' W-WDC301KY-X ')'                        
026600          DELIMITED BY SIZE INTO SSA1                                     
026700     MOVE '  GE' TO GODK-STATUSKODER                                      
026800     CALL CBLTDLI USING GU WDC3-PCB DLI-IO-WDC301 SSA1                    
026900     MOVE WDC3-STATUS-CODE TO STATUS-WS                                   
027000     PERFORM IMS-STATUSKONTROLL                                           
027100     .                                                                    
026300                                                                          
026400 IMS-GHNP-WDC311 SECTION.                                                 
026500     STRING 'WDC311  (IDLANDX2 =' W-WDC311KY-X ')'                        
026600          DELIMITED BY SIZE INTO SSA1                                     
026700     MOVE '  GE' TO GODK-STATUSKODER                                      
026800     CALL CBLTDLI USING GHNP WDC3-PCB DLI-IO-WDC311 SSA1                  
026900     MOVE WDC3-STATUS-CODE TO STATUS-WS                                   
027000     PERFORM IMS-STATUSKONTROLL                                           
027100     .                                                                    
027200     SKIP3                                                                
027300 IMS-ISRT-WDC301 SECTION.                                                 
027500     MOVE 'WDC301   ' TO SSA1                                             
027600     MOVE '  II' TO GODK-STATUSKODER                                      
027700     CALL CBLTDLI USING ISRT WDC3-PCB DLI-IO-WDC301 SSA1                  
027800     MOVE WDC3-STATUS-CODE TO STATUS-WS                                   
027900     PERFORM IMS-STATUSKONTROLL                                           
028000     .                                                                    
027200     SKIP3                                                                
027300 IMS-ISRT-WDC311 SECTION.                                                 
026500     STRING 'WDC301  (IDARTNR  =' W-WDC301KY-X ')'                        
026600          DELIMITED BY SIZE INTO SSA1                                     
027500     MOVE 'WDC311   ' TO SSA2                                             
027600     MOVE '  II' TO GODK-STATUSKODER                                      
027700     CALL CBLTDLI USING ISRT WDC3-PCB DLI-IO-WDC311 SSA1 SSA2             
027800     MOVE WDC3-STATUS-CODE TO STATUS-WS                                   
027900     PERFORM IMS-STATUSKONTROLL                                           
028000     .                                                                    
028100     SKIP3                                                                
028200 IMS-REPL-WDC311 SECTION.                                                 
028300                                                                          
028500     MOVE '  ' TO GODK-STATUSKODER                                        
028600     CALL CBLTDLI USING REPL WDC3-PCB DLI-IO-WDC311                       
028700     MOVE WDC3-STATUS-CODE TO STATUS-WS                                   
028800     PERFORM IMS-STATUSKONTROLL                                           
028900     .                                                                    
029000     EJECT                                                                
029010 IMS-RESTART SECTION.                                                     
029020                                                                          
029030     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
029040     MOVE '  ' TO GODK-STATUSKODER                                        
029050     CALL CBLTDLI USING XRST MSG-PCB                                      
029060                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
029070                        CHKP-AREA-LENGTH CHKP-AREA                        
029080     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029090     PERFORM IMS-STATUSKONTROLL                                           
029091     .                                                                    
029092     SKIP3                                                                
029093 IMS-CHECKPOINT SECTION.                                                  
029094                                                                          
029095     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
029096     MOVE '  XD' TO GODK-STATUSKODER                                      
029097     CALL CBLTDLI USING CHKP MSG-PCB                                      
029098                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
029099                        CHKP-AREA-LENGTH CHKP-AREA                        
029100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029101     PERFORM IMS-STATUSKONTROLL                                           
029102                                                                          
029103     IF IMS-EJ-OK                                                         
029104       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
029105       DISPLAY FELTEXT                                                    
029106       CALL FELLOG                                                        
029107     END-IF                                                               
029108     .                                                                    
029109     EJECT                                                                
029110 IMS-STATUSKONTROLL SECTION.                                              
029200                                                                          
029300     SET STATUS-IX TO 1                                                   
029400     SEARCH GODK-STATUS                                                   
029500       AT END                                                             
029600         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
029700         DISPLAY FELTEXT                                                  
029800         CALL FELLOG                                                      
029900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030000         CONTINUE                                                         
030100     END-SEARCH                                                           
030200     .                                                                    
