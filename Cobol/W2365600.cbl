000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2365600.                                                
000300 AUTHOR.         KENT JEBSEN.                                             
000400 DATE-WRITTEN.   04/11/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER ORDERING/ORDERTRÄFFAR P1-P12 FRÅN WDL7                     
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDL7                                       
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- NEDLÄST KVOI/KVOT 12 RULLANDE                              
002500     SELECT W23656                     ASSIGN TO W23656D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W23656                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  POST -COPY W2365601 -PRE  UT-  -L.                                   
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W2365600'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  IX                          PIC 9(2)    VALUE ZERO.                  
004300 77  HELP-IX                     PIC 9(2)    VALUE ZERO.                  
004400     EJECT                                                                
004500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004600 01  FILLER REDEFINES DAGENS-DATUM.                                       
004700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005000                                                                          
005100 01  WS-AKTUELL-RP               PIC 9(4)    VALUE ZERO.                  
005200 01  FILLER REDEFINES WS-AKTUELL-RP.                                      
005300     03  WS-AKTUELL-RP-1-2       PIC 9(2).                                
005400     03  WS-AKTUELL-RP-3-4       PIC 9(2).                                
005500                                                                          
005600 01  FILLER.                                                              
005700   02  FILLER               OCCURS 12.                                    
005800     03  START-VV                 PIC 9(2).                               
005900     03  STOPP-VV                 PIC 9(2).                               
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300*01  -COPY WDATAREA                                                       
008400     EJECT                                                                
008500 01  UT-AREA-START               PIC X(24)   VALUE                        
008600                                 'UT-AREA-START  '.                       
008700     SKIP2                                                                
008800                                                                          
008900*01  AREA -COPY W2365601     -PRE UT-                                     
009000     EJECT                                                                
009100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009200*                                                                         
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500     SKIP3                                                                
009600 01  NYCKLAR-TILL-DLI.                                                    
009700     03  W-IDARTNR-X.                                                     
009800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009900     03  W-IDDC-X.                                                        
010000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
010600     SKIP2                                                                
010700 01  GODK-STATUSKODER.                                                    
010800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(64).                               
011100 01  SSA2                        PIC X(64).                               
011200     EJECT                                                                
011300*    --- IMS FUNKTIONSKODER                                               
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600*    ---  DLI INPUT-OUTPUT AREA                                           
011700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL7'.                        
011800 01  DLI-IO-WDL7    PIC X(2000).                                          
011900*01  WDL701      -COPY WDL701               -RED DLI-IO-WDL7.             
012000     EJECT                                                                
012100*01  WDL711      -COPY WDL711               -RED DLI-IO-WDL7.             
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500                                                                          
012600*01  -COPY W0008  -PRE WDL7-                                              
012700     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012900 PROCEDURE DIVISION  USING WDL7-PCB.                                      
013000 MAIN SECTION.                                                            
013100     ENTRY 'DLITCBL' USING WDL7-PCB.                                      
013200                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013500     PERFORM IMS-GET-WDL7                                                 
013600     PERFORM UNTIL SEGMENT-SAKNAS                                         
013700       EVALUATE WDL7-SEG-NAME-FB                                          
013800         WHEN 'WDL701'                                                    
013900           IF UT-IDARTNR > 0                                              
014000             PERFORM S11-SKRIV-W23656                                     
014100           END-IF                                                         
014200           MOVE 0 TO UT-KVOI-12-RULL                                      
014300                     UT-KVOI-P1                                           
014400                     UT-KVOI-P2                                           
014500                     UT-KVOI-P3                                           
014600                     UT-KVOI-P4                                           
014700                     UT-KVOI-P5                                           
014800                     UT-KVOI-P6                                           
014900                     UT-KVOI-P7                                           
015000                     UT-KVOI-P8                                           
015100                     UT-KVOI-P9                                           
015200                     UT-KVOI-P10                                          
015300                     UT-KVOI-P11                                          
015400                     UT-KVOI-P12                                          
015500                     UT-KVOT-12-RULL                                      
015600                     UT-KVOT-P1                                           
015700                     UT-KVOT-P2                                           
015800                     UT-KVOT-P3                                           
015900                     UT-KVOT-P4                                           
016000                     UT-KVOT-P5                                           
016100                     UT-KVOT-P6                                           
016200                     UT-KVOT-P7                                           
016300                     UT-KVOT-P8                                           
016400                     UT-KVOT-P9                                           
016500                     UT-KVOT-P10                                          
016600                     UT-KVOT-P11                                          
016700                     UT-KVOT-P12                                          
016800                                                                          
016900           MOVE ART-IDARTNR TO UT-IDARTNR                                 
017000                                                                          
017100         WHEN 'WDL711'                                                    
017200           PERFORM B-BERAKNA-PERIOD                                       
017300                                                                          
017400       END-EVALUATE                                                       
017500       PERFORM IMS-GET-WDL7                                               
017600     END-PERFORM                                                          
017700     PERFORM Z-FINIT                                                      
017800                                                                          
017900     MOVE ZERO TO RETURN-CODE                                             
018000     GOBACK                                                               
018100     .                                                                    
018200     EJECT                                                                
018300 A-INIT SECTION.                                                          
018400                                                                          
018500     OPEN OUTPUT W23656                                                   
018600                                                                          
018700     MOVE ZERO TO UT-IDARTNR                                              
018800                     UT-KVOI-12-RULL                                      
018900                     UT-KVOI-P1                                           
019000                     UT-KVOI-P2                                           
019100                     UT-KVOI-P3                                           
019200                     UT-KVOI-P4                                           
019300                     UT-KVOI-P5                                           
019400                     UT-KVOI-P6                                           
019500                     UT-KVOI-P7                                           
019600                     UT-KVOI-P8                                           
019700                     UT-KVOI-P9                                           
019800                     UT-KVOI-P10                                          
019900                     UT-KVOI-P11                                          
020000                     UT-KVOI-P12                                          
020100                     UT-KVOT-12-RULL                                      
020200                     UT-KVOT-P1                                           
020300                     UT-KVOT-P2                                           
020400                     UT-KVOT-P3                                           
020500                     UT-KVOT-P4                                           
020600                     UT-KVOT-P5                                           
020700                     UT-KVOT-P6                                           
020800                     UT-KVOT-P7                                           
020900                     UT-KVOT-P8                                           
021000                     UT-KVOT-P9                                           
021100                     UT-KVOT-P10                                          
021200                     UT-KVOT-P11                                          
021300                     UT-KVOT-P12                                          
021400                                                                          
021500     ACCEPT DAGENS-DATUM  FROM DATE                                       
021600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021700     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
021800     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
021900                                                                          
022000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
022100                     DAT-O-TIDATUM DAT-KDSVAR                             
022200                                                                          
022300     IF DAT-KDSVAR-OK                                                     
022400       MOVE DAT-TIAARP TO WS-AKTUELL-RP                                   
022500       MOVE DAT-TIAARP TO DAT-I-TIDATUM                                   
022600       MOVE 'AARP  '      TO DAT-KDDATFORM                                
022700       CALL WDATKONV  USING  DAT-KDDATFORM                                
022800                             DAT-I-TIDATUM                                
022900                             DAT-O-TIDATUM                                
023000                             DAT-KDSVAR                                   
023100       IF DAT-KDSVAR-OK                                                   
023200         MOVE DAT-TIVV TO START-VV(12)                                    
023300         IF START-VV(12) = 01                                             
023400           MOVE 53 TO STOPP-VV(11)                                        
023500         ELSE                                                             
023600           COMPUTE STOPP-VV(11) = START-VV(12) - 1                        
023700         END-IF                                                           
023800       ELSE                                                               
023900         STRING ' FEL FRÅN DATUMRUTIN WDATKONV/ANROP2' STATUS-WS          
024000         DELIMITED BY SIZE INTO FELTEXT                                   
024100         CALL FELLOG                                                      
024200       END-IF                                                             
024300                                                                          
024400       MOVE 11 TO IX                                                      
024500       PERFORM UNTIL IX < 1                                               
024600         IF WS-AKTUELL-RP-3-4 = 01                                        
024700           SUBTRACT 1 FROM WS-AKTUELL-RP-1-2                              
024800           MOVE 12 TO WS-AKTUELL-RP-3-4                                   
024900         ELSE                                                             
025000           SUBTRACT 1 FROM WS-AKTUELL-RP                                  
025100         END-IF                                                           
025200                                                                          
025300         MOVE WS-AKTUELL-RP TO DAT-I-TIDATUM                              
025400         MOVE 'AARP  '      TO DAT-KDDATFORM                              
025500         CALL WDATKONV  USING  DAT-KDDATFORM                              
025600                               DAT-I-TIDATUM                              
025700                               DAT-O-TIDATUM                              
025800                               DAT-KDSVAR                                 
025900         IF DAT-KDSVAR-OK                                                 
026000           MOVE DAT-TIVV TO START-VV(IX)                                  
026100           IF IX > 1                                                      
026200             COMPUTE HELP-IX = IX - 1                                     
026300             IF START-VV(IX) = 01                                         
026400               MOVE 53 TO STOPP-VV(HELP-IX)                               
026500             ELSE                                                         
026600               COMPUTE STOPP-VV(HELP-IX) = START-VV(IX) - 1               
026700             END-IF                                                       
026800           END-IF                                                         
026900         ELSE                                                             
027000           STRING ' FEL FRÅN DATUMRUTIN WDATKONV/ANROP3' STATUS-WS        
027100           DELIMITED BY SIZE INTO FELTEXT                                 
027200           CALL FELLOG                                                    
027300         END-IF                                                           
027400         SUBTRACT 1 FROM IX                                               
027500       END-PERFORM                                                        
027600     ELSE                                                                 
027700       STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                  
027800       DELIMITED BY SIZE INTO FELTEXT                                     
027900       CALL FELLOG                                                        
028000     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 B-BERAKNA-PERIOD SECTION.                                                
028400     MOVE 1 TO IX                                                         
028500     PERFORM UNTIL IX > 53                                                
028600       COMPUTE UT-KVOI-12-RULL = UT-KVOI-12-RULL +                        
028700         DC-KVOI-RULL(IX)                                                 
028800                                                                          
028900       COMPUTE UT-KVOT-12-RULL = UT-KVOT-12-RULL +                        
029000         DC-KVOT-RULL(IX)                                                 
029100                                                                          
029200       IF (IX > START-VV(1) OR IX = START-VV(1)) AND                      
029300          (IX < STOPP-VV(1) OR IX = STOPP-VV(1))                          
029400         COMPUTE UT-KVOI-P1 = UT-KVOI-P1 + DC-KVOI-RULL(IX)               
029500         COMPUTE UT-KVOT-P1 = UT-KVOT-P1 + DC-KVOT-RULL(IX)               
029600       ELSE                                                               
029700         IF (IX > START-VV(2) OR IX = START-VV(2)) AND                    
029800            (IX < STOPP-VV(2) OR IX = STOPP-VV(2))                        
029900           COMPUTE UT-KVOI-P2 = UT-KVOI-P2 + DC-KVOI-RULL(IX)             
030000           COMPUTE UT-KVOT-P2 = UT-KVOT-P2 + DC-KVOT-RULL(IX)             
030100         ELSE                                                             
030200           IF (IX > START-VV(3) OR IX = START-VV(3)) AND                  
030300              (IX < STOPP-VV(3) OR IX = STOPP-VV(3))                      
030400            COMPUTE UT-KVOI-P3 = UT-KVOI-P3 + DC-KVOI-RULL(IX)            
030500            COMPUTE UT-KVOT-P3 = UT-KVOT-P3 + DC-KVOT-RULL(IX)            
030600           ELSE                                                           
030700             IF (IX > START-VV(4) OR IX = START-VV(4)) AND                
030800                (IX < STOPP-VV(4) OR IX = STOPP-VV(4))                    
030900               COMPUTE UT-KVOI-P4 = UT-KVOI-P4 +                          
031000                                              DC-KVOI-RULL(IX)            
031100               COMPUTE UT-KVOT-P4 = UT-KVOT-P4 +                          
031200                                              DC-KVOT-RULL(IX)            
031300             ELSE                                                         
031400               IF (IX > START-VV(5) OR IX = START-VV(5)) AND              
031500                  (IX < STOPP-VV(5) OR IX = STOPP-VV(5))                  
031600                 COMPUTE UT-KVOI-P5 = UT-KVOI-P5 +                        
031700                                              DC-KVOI-RULL(IX)            
031800                 COMPUTE UT-KVOT-P5 = UT-KVOT-P5 +                        
031900                                              DC-KVOT-RULL(IX)            
032000               ELSE                                                       
032100                 IF (IX > START-VV(6) OR IX = START-VV(6)) AND            
032200                    (IX < STOPP-VV(6) OR IX = STOPP-VV(6))                
032300                   COMPUTE UT-KVOI-P6 = UT-KVOI-P6 +                      
032400                                              DC-KVOI-RULL(IX)            
032500                   COMPUTE UT-KVOT-P6 = UT-KVOT-P6 +                      
032600                                              DC-KVOT-RULL(IX)            
032700                 ELSE                                                     
032800                   IF (IX > START-VV(7) OR IX = START-VV(7)) AND          
032900                      (IX < STOPP-VV(7) OR IX = STOPP-VV(7))              
033000                     COMPUTE UT-KVOI-P7 = UT-KVOI-P7 +                    
033100                                              DC-KVOI-RULL(IX)            
033200                     COMPUTE UT-KVOT-P7 = UT-KVOT-P7 +                    
033300                                              DC-KVOT-RULL(IX)            
033400                   ELSE                                                   
033500                     IF (IX > START-VV(8) OR IX = START-VV(8)) AND        
033600                        (IX < STOPP-VV(8) OR IX = STOPP-VV(8))            
033700                       COMPUTE UT-KVOI-P8 = UT-KVOI-P8 +                  
033800                                              DC-KVOI-RULL(IX)            
033900                       COMPUTE UT-KVOT-P8 = UT-KVOT-P8 +                  
034000                                              DC-KVOT-RULL(IX)            
034100                     ELSE                                                 
034200                       IF (IX > START-VV(9) OR IX = START-VV(9))          
034300                        AND (IX < STOPP-VV(9) OR IX = STOPP-VV(9))        
034400                         COMPUTE UT-KVOI-P9 = UT-KVOI-P9 +                
034500                                              DC-KVOI-RULL(IX)            
034600                         COMPUTE UT-KVOT-P9 = UT-KVOT-P9 +                
034700                                              DC-KVOT-RULL(IX)            
034800                       ELSE                                               
034900                         IF (IX > START-VV(10) OR                         
035000                            IX = START-VV(10)) AND                        
035100                           (IX < STOPP-VV(10)  OR                         
035200                            IX = STOPP-VV(10))                            
035300                           COMPUTE UT-KVOI-P10 = UT-KVOI-P10 +            
035400                                              DC-KVOI-RULL(IX)            
035500                           COMPUTE UT-KVOT-P10 = UT-KVOT-P10 +            
035600                                              DC-KVOT-RULL(IX)            
035700                         ELSE                                             
035800                           IF (IX > START-VV(11) OR                       
035900                              IX = START-VV(11)) AND                      
036000                             (IX < STOPP-VV(11)  OR                       
036100                              IX = STOPP-VV(11))                          
036200                             COMPUTE UT-KVOI-P11 = UT-KVOI-P11 +          
036300                                              DC-KVOI-RULL(IX)            
036400                             COMPUTE UT-KVOT-P11 = UT-KVOT-P11 +          
036500                                              DC-KVOT-RULL(IX)            
036600                           ELSE                                           
036700                             COMPUTE UT-KVOI-P12 = UT-KVOI-P12 +          
036800                                              DC-KVOI-RULL(IX)            
036900                             COMPUTE UT-KVOT-P12 = UT-KVOT-P12 +          
037000                                              DC-KVOT-RULL(IX)            
037100                           END-IF                                         
037200                         END-IF                                           
037300                       END-IF                                             
037400                     END-IF                                               
037500                   END-IF                                                 
037600                 END-IF                                                   
037700               END-IF                                                     
037800             END-IF                                                       
037900           END-IF                                                         
038000         END-IF                                                           
038100       END-IF                                                             
038200       ADD 1 TO IX                                                        
038300     END-PERFORM                                                          
038400     .                                                                    
038500     EJECT                                                                
038600 Z-FINIT SECTION.                                                         
038700     IF UT-IDARTNR > 0                                                    
038710       PERFORM S11-SKRIV-W23656                                           
038720     END-IF                                                               
038800     CLOSE W23656                                                         
038900     SKIP2                                                                
039000     MOVE 'S' TO POSTSUM-OPKOD                                            
039100     CALL POSTSUM USING POSTSUM-PARM                                      
039200     .                                                                    
039300     EJECT                                                                
039400 S11-SKRIV-W23656 SECTION.                                                
039500                                                                          
039600     WRITE UT-POST FROM UT-AREA                                           
039700                                                                          
039800     MOVE '711 '    TO POSTSUM-TRANSTYP                                   
039900     MOVE 'W23656' TO POSTSUM-FDNAMN                                      
040000     MOVE 'W23656D1' TO POSTSUM-DDNAMN2                                   
040100     CALL POSTSUM USING POSTSUM-PARM                                      
040200     .                                                                    
040300     EJECT                                                                
040400* --- IMS SEKTIONER ---                                                   
040500                                                                          
040600 IMS-GET-WDL7 SECTION.                                                    
040700                                                                          
040800     CALL CBLTDLI USING GN WDL7-PCB DLI-IO-WDL7                           
040900     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
041000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
041100     PERFORM IMS-STATUSKONTROLL                                           
041200     .                                                                    
041300     EJECT                                                                
041400 IMS-STATUSKONTROLL SECTION.                                              
041500                                                                          
041600     SET STATUS-IX TO 1                                                   
041700     SEARCH GODK-STATUS                                                   
041800       AT END                                                             
041900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
042000           DELIMITED BY SIZE INTO FELTEXT                                 
042100         DISPLAY FELTEXT                                                  
042200         CALL FELLOG                                                      
042300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
042400         CONTINUE                                                         
042500     END-SEARCH                                                           
042600     .                                                                    
