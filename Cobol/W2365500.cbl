000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2365500.                                                
000300 AUTHOR.         KENT JEBSEN.                                             
000400 DATE-WRITTEN.   04/11/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER UT KVOI OCH KVOT                                           
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDL8                                       
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
002400*          --- SELEKTERAT DATA                                            
002500     SELECT W23655                     ASSIGN TO W23655D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W23655                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  POST -COPY W2365501 -PRE  UT-  -L.                                   
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W2365500'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  INDX                        PIC 9(2)    VALUE ZERO.                  
004300     EJECT                                                                
004400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES DAGENS-DATUM.                                       
004600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004900                                                                          
005000 01  HELP-DATE                   PIC 9(6).                                
005100 01  FILLER REDEFINES HELP-DATE.                                          
005200     03  HELP-AAAA               PIC 9(4).                                
005300     03  IX                      PIC 9(2).                                
005400                                                                          
005500 01  WS-AKTUELL-RP               PIC 9(4)    VALUE ZERO.                  
005600 01  FILLER REDEFINES WS-AKTUELL-RP.                                      
005700     03  WS-AKTUELL-RP-1-2       PIC 9(2).                                
005800     03  WS-AKTUELL-RP-3-4       PIC 9(2).                                
005900                                                                          
006000 01  FILLER.                                                              
006100   02  FILLER               OCCURS 12.                                    
006200     03  START-TID                PIC 9(6).                               
006300     03  FILLER REDEFINES START-TID.                                      
006400        05  START-SEKEL           PIC 9(2).                               
006500        05  START-AAVV            PIC 9(4).                               
006600     EJECT                                                                
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800*                                                                         
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007310     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007400     SKIP2                                                                
007500*    --- PARAMETRAR TILL ABEND                                            
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     SKIP2                                                                
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400     EJECT                                                                
008410*    --- PARAMETRAR TILL DATKORT                                          
008420*                                                                         
008430 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23655'.              
008440     SKIP2                                                                
008450 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
008460     SKIP2                                                                
008470*01  -COPY WDATKORT                                                       
008480     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900*01  -COPY WDATAREA                                                       
009000     EJECT                                                                
009100 01  UT-AREA-START               PIC X(24)   VALUE                        
009200                                 'UT-AREA-START  '.                       
009300     SKIP2                                                                
009400                                                                          
009500*01  AREA -COPY W2365501     -PRE UT-                                     
009600     EJECT                                                                
009700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009800*                                                                         
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010100     SKIP3                                                                
010200 01  NYCKLAR-TILL-DLI.                                                    
010300     03  W-IDARTNR-X.                                                     
010400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010500     03  W-TIAAA-X.                                                       
010600         05  W-TIAAA             PIC S9(4)   VALUE ZERO COMP-3.           
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL8'.                        
012400 01  DLI-IO-WDL8    PIC X(2000).                                          
012500*01  WDL801      -COPY WDL801               -RED DLI-IO-WDL8.             
012600     EJECT                                                                
012700*01  WDL811      -COPY WDL811               -RED DLI-IO-WDL8.             
012800     EJECT                                                                
012900 LINKAGE SECTION.                                                         
013000*01  -COPY W0008  -PRE WDL8-                                              
013100     05  FILLER                  PIC X.                                   
013200     EJECT                                                                
013300 PROCEDURE DIVISION  USING WDL8-PCB.                                      
013400 MAIN SECTION.                                                            
013500     ENTRY 'DLITCBL' USING WDL8-PCB.                                      
013600                                                                          
013700     PERFORM A-INIT                                                       
013800                                                                          
013900     PERFORM IMS-GET-WDL8                                                 
014000     PERFORM UNTIL SEGMENT-SAKNAS                                         
014100       EVALUATE WDL8-SEG-NAME-FB                                          
014200                                                                          
014300         WHEN 'WDL801'                                                    
014400           IF UT-IDARTNR > 0                                              
014500             PERFORM S11-SKRIV-W23655                                     
014600           END-IF                                                         
014700           MOVE 0 TO UT-KVOI-12-RULL                                      
014800                     UT-KVOI-P1                                           
014900                     UT-KVOI-P2                                           
015000                     UT-KVOI-P3                                           
015100                     UT-KVOI-P4                                           
015200                     UT-KVOI-P5                                           
015300                     UT-KVOI-P6                                           
015400                     UT-KVOI-P7                                           
015500                     UT-KVOI-P8                                           
015600                     UT-KVOI-P9                                           
015700                     UT-KVOI-P10                                          
015800                     UT-KVOI-P11                                          
015900                     UT-KVOI-P12                                          
016000                     UT-KVOT-12-RULL                                      
016100                     UT-KVOT-P1                                           
016200                     UT-KVOT-P2                                           
016300                     UT-KVOT-P3                                           
016400                     UT-KVOT-P4                                           
016500                     UT-KVOT-P5                                           
016600                     UT-KVOT-P6                                           
016700                     UT-KVOT-P7                                           
016800                     UT-KVOT-P8                                           
016900                     UT-KVOT-P9                                           
017000                     UT-KVOT-P10                                          
017100                     UT-KVOT-P11                                          
017200                     UT-KVOT-P12                                          
017300                     UT-KVOI-12-RULL-REF                                  
017400                     UT-KVOI-P1-REF                                       
017500                     UT-KVOI-P2-REF                                       
017600                     UT-KVOI-P3-REF                                       
017700                     UT-KVOI-P4-REF                                       
017800                     UT-KVOI-P5-REF                                       
017900                     UT-KVOI-P6-REF                                       
018000                     UT-KVOI-P7-REF                                       
018100                     UT-KVOI-P8-REF                                       
018200                     UT-KVOI-P9-REF                                       
018300                     UT-KVOI-P10-REF                                      
018400                     UT-KVOI-P11-REF                                      
018500                     UT-KVOI-P12-REF                                      
018600                     UT-KVOT-12-RULL-REF                                  
018700                     UT-KVOT-P1-REF                                       
018800                     UT-KVOT-P2-REF                                       
018900                     UT-KVOT-P3-REF                                       
019000                     UT-KVOT-P4-REF                                       
019100                     UT-KVOT-P5-REF                                       
019200                     UT-KVOT-P6-REF                                       
019300                     UT-KVOT-P7-REF                                       
019400                     UT-KVOT-P8-REF                                       
019500                     UT-KVOT-P9-REF                                       
019600                     UT-KVOT-P10-REF                                      
019700                     UT-KVOT-P11-REF                                      
019800                     UT-KVOT-P12-REF                                      
019900                                                                          
020000           MOVE ART-IDARTNR TO UT-IDARTNR                                 
020100                                                                          
020200         WHEN 'WDL811'                                                    
020300           MOVE AAR-TIAAAA TO HELP-AAAA                                   
020400           PERFORM B-BERAKNA-PERIOD                                       
020500                                                                          
020600       END-EVALUATE                                                       
020700       PERFORM IMS-GET-WDL8                                               
020800     END-PERFORM                                                          
020900     PERFORM Z-FINIT                                                      
021000                                                                          
021100     MOVE ZERO TO RETURN-CODE                                             
021200     GOBACK                                                               
021300     .                                                                    
021400     EJECT                                                                
021500 A-INIT SECTION.                                                          
021600                                                                          
021700     OPEN OUTPUT W23655                                                   
021800                                                                          
021900     MOVE ZERO TO UT-IDARTNR                                              
022000                     UT-KVOI-12-RULL                                      
022100                     UT-KVOI-P1                                           
022200                     UT-KVOI-P2                                           
022300                     UT-KVOI-P3                                           
022400                     UT-KVOI-P4                                           
022500                     UT-KVOI-P5                                           
022600                     UT-KVOI-P6                                           
022700                     UT-KVOI-P7                                           
022800                     UT-KVOI-P8                                           
022900                     UT-KVOI-P9                                           
023000                     UT-KVOI-P10                                          
023100                     UT-KVOI-P11                                          
023200                     UT-KVOI-P12                                          
023300                     UT-KVOT-12-RULL                                      
023400                     UT-KVOT-P1                                           
023500                     UT-KVOT-P2                                           
023600                     UT-KVOT-P3                                           
023700                     UT-KVOT-P4                                           
023800                     UT-KVOT-P5                                           
023900                     UT-KVOT-P6                                           
024000                     UT-KVOT-P7                                           
024100                     UT-KVOT-P8                                           
024200                     UT-KVOT-P9                                           
024300                     UT-KVOT-P10                                          
024400                     UT-KVOT-P11                                          
024500                     UT-KVOT-P12                                          
024600                     UT-KVOI-12-RULL-REF                                  
024700                     UT-KVOI-P1-REF                                       
024800                     UT-KVOI-P2-REF                                       
024900                     UT-KVOI-P3-REF                                       
025000                     UT-KVOI-P4-REF                                       
025100                     UT-KVOI-P5-REF                                       
025200                     UT-KVOI-P6-REF                                       
025300                     UT-KVOI-P7-REF                                       
025400                     UT-KVOI-P8-REF                                       
025500                     UT-KVOI-P9-REF                                       
025600                     UT-KVOI-P10-REF                                      
025700                     UT-KVOI-P11-REF                                      
025800                     UT-KVOI-P12-REF                                      
025900                     UT-KVOT-12-RULL-REF                                  
026000                     UT-KVOT-P1-REF                                       
026100                     UT-KVOT-P2-REF                                       
026200                     UT-KVOT-P3-REF                                       
026300                     UT-KVOT-P4-REF                                       
026400                     UT-KVOT-P5-REF                                       
026500                     UT-KVOT-P6-REF                                       
026600                     UT-KVOT-P7-REF                                       
026700                     UT-KVOT-P8-REF                                       
026800                     UT-KVOT-P9-REF                                       
026900                     UT-KVOT-P10-REF                                      
027000                     UT-KVOT-P11-REF                                      
027100                     UT-KVOT-P12-REF                                      
027200                                                                          
027300     MOVE 1 TO INDX                                                       
027400     PERFORM UNTIL INDX > 12                                              
027500       MOVE 20 TO START-SEKEL(INDX)                                       
027600       ADD 1 TO INDX                                                      
027700     END-PERFORM                                                          
027800                                                                          
027900*    ACCEPT DAGENS-DATUM  FROM DATE                                       
028000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028100     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
028200*    MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
028210     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
028220     MOVE D-AAR        TO DAGENS-DATUM-AAR                                
028230     MOVE D-MAANAD     TO DAGENS-DATUM-MAANAD                             
028240     MOVE D-DAG        TO DAGENS-DATUM-DAG                                
028250     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
028300                                                                          
028400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
028500                     DAT-O-TIDATUM DAT-KDSVAR                             
028600                                                                          
028700     IF DAT-KDSVAR-OK                                                     
028800       MOVE DAT-TIAARP TO WS-AKTUELL-RP                                   
028900       MOVE DAT-TIAARP TO DAT-I-TIDATUM                                   
029000       MOVE 'AARP  '      TO DAT-KDDATFORM                                
029100       CALL WDATKONV  USING  DAT-KDDATFORM                                
029200                             DAT-I-TIDATUM                                
029300                             DAT-O-TIDATUM                                
029400                             DAT-KDSVAR                                   
029500       IF DAT-KDSVAR-OK                                                   
029600         MOVE DAT-TIAAVV-GRP TO START-AAVV(12)                            
029700       ELSE                                                               
029800         STRING ' FEL FRÅN DATUMRUTIN WDATKONV/ANROP2' STATUS-WS          
029900         DELIMITED BY SIZE INTO FELTEXT                                   
030000         CALL FELLOG                                                      
030100       END-IF                                                             
030200                                                                          
030300       MOVE 11 TO INDX                                                    
030400       PERFORM UNTIL INDX < 1                                             
030500         IF WS-AKTUELL-RP-3-4 = 01                                        
030600           SUBTRACT 1 FROM WS-AKTUELL-RP-1-2                              
030700           MOVE 12 TO WS-AKTUELL-RP-3-4                                   
030800         ELSE                                                             
030900           SUBTRACT 1 FROM WS-AKTUELL-RP                                  
031000         END-IF                                                           
031100                                                                          
031200         MOVE WS-AKTUELL-RP TO DAT-I-TIDATUM                              
031300         MOVE 'AARP  '      TO DAT-KDDATFORM                              
031400         CALL WDATKONV  USING  DAT-KDDATFORM                              
031500                               DAT-I-TIDATUM                              
031600                               DAT-O-TIDATUM                              
031700                               DAT-KDSVAR                                 
031800         IF DAT-KDSVAR-OK                                                 
031900           MOVE DAT-TIAAVV-GRP TO START-AAVV(INDX)                        
032000         ELSE                                                             
032100           STRING ' FEL FRÅN DATUMRUTIN WDATKONV/ANROP3' STATUS-WS        
032200           DELIMITED BY SIZE INTO FELTEXT                                 
032300           CALL FELLOG                                                    
032400         END-IF                                                           
032500         SUBTRACT 1 FROM INDX                                             
032600       END-PERFORM                                                        
032700     ELSE                                                                 
032800       STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                  
032900       DELIMITED BY SIZE INTO FELTEXT                                     
033000       CALL FELLOG                                                        
033100     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400                                                                          
033500 B-BERAKNA-PERIOD SECTION.                                                
033600     MOVE 1 TO IX                                                         
033700     PERFORM UNTIL IX > 53                                                
033800       IF HELP-DATE = START-TID(1) OR HELP-DATE > START-TID(1)            
033900         COMPUTE UT-KVOI-12-RULL = UT-KVOI-12-RULL +                      
034000         AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +                           
034100         AAR-KVOI-SATS(IX)                                                
034200                                                                          
034300         COMPUTE UT-KVOT-12-RULL = UT-KVOT-12-RULL +                      
034400         AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +                           
034500         AAR-KVOT-SATS(IX)                                                
034600                                                                          
034700         COMPUTE UT-KVOI-12-RULL-REF = UT-KVOI-12-RULL-REF +              
034800         AAR-KVOI-REFILL(IX)                                              
034900                                                                          
035000         COMPUTE UT-KVOT-12-RULL-REF = UT-KVOT-12-RULL-REF +              
035100                                       AAR-KVOT-REFILL(IX)                
035200                                                                          
035300         IF HELP-DATE < START-TID(2)                                      
035400           COMPUTE UT-KVOI-P1 = UT-KVOI-P1 +                              
035500           AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +                         
035600           AAR-KVOI-SATS(IX)                                              
035700                                                                          
035800           COMPUTE UT-KVOT-P1 = UT-KVOT-P1 +                              
035900           AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +                         
036000           AAR-KVOT-SATS(IX)                                              
036100                                                                          
036200           COMPUTE UT-KVOI-P1-REF = UT-KVOI-P1-REF +                      
036300                                    AAR-KVOI-REFILL(IX)                   
036400                                                                          
036500           COMPUTE UT-KVOT-P1 = UT-KVOT-P1-REF  +                         
036600                                AAR-KVOT-REFILL(IX)                       
036700         ELSE                                                             
036800           IF HELP-DATE < START-TID(3)                                    
036900             COMPUTE UT-KVOI-P2 = UT-KVOI-P2 +                            
037000             AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +                       
037100             AAR-KVOI-SATS(IX)                                            
037200                                                                          
037300             COMPUTE UT-KVOT-P2 = UT-KVOT-P2 +                            
037400             AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +                       
037500             AAR-KVOT-SATS(IX)                                            
037600                                                                          
037700             COMPUTE UT-KVOI-P2-REF = UT-KVOI-P2-REF +                    
037800                                           AAR-KVOI-REFILL(IX)            
037900                                                                          
038000             COMPUTE UT-KVOT-P2-REF = UT-KVOT-P2-REF +                    
038100                                     AAR-KVOT-REFILL(IX)                  
038200                                                                          
038300           ELSE                                                           
038400             IF HELP-DATE < START-TID(4)                                  
038500               COMPUTE UT-KVOI-P3 = UT-KVOI-P3 +                          
038600               AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +                     
038700               AAR-KVOI-SATS(IX)                                          
038800                                                                          
038900               COMPUTE UT-KVOT-P3 = UT-KVOT-P3 +                          
039000               AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +                     
039100               AAR-KVOT-SATS(IX)                                          
039200                                                                          
039300               COMPUTE UT-KVOI-P3-REF = UT-KVOI-P3-REF +                  
039400                                           AAR-KVOI-REFILL(IX)            
039500                                                                          
039600               COMPUTE UT-KVOT-P3-REF = UT-KVOT-P3-REF +                  
039700                                           AAR-KVOT-REFILL(IX)            
039800                                                                          
039900             ELSE                                                         
040000               IF HELP-DATE < START-TID(5)                                
040100                 COMPUTE UT-KVOI-P4 = UT-KVOI-P4 +                        
040200                 AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +                   
040300                 AAR-KVOI-SATS(IX)                                        
040400                                                                          
040500                 COMPUTE UT-KVOT-P4 = UT-KVOT-P4 +                        
040600                 AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +                   
040700                 AAR-KVOT-SATS(IX)                                        
040800                                                                          
040900                 COMPUTE UT-KVOI-P4-REF = UT-KVOI-P4-REF +                
041000                                             AAR-KVOI-REFILL(IX)          
041100                                                                          
041200                 COMPUTE UT-KVOT-P4-REF = UT-KVOT-P4-REF +                
041300                                              AAR-KVOT-REFILL(IX)         
041400               ELSE                                                       
041500                 IF HELP-DATE < START-TID(6)                              
041600                   COMPUTE UT-KVOI-P5 = UT-KVOI-P5 +                      
041700                   AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +                 
041800                   AAR-KVOI-SATS(IX)                                      
041900                                                                          
042000                   COMPUTE UT-KVOT-P5 = UT-KVOT-P5 +                      
042100                   AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +                 
042200                   AAR-KVOT-SATS(IX)                                      
042300                                                                          
042400                   COMPUTE UT-KVOI-P5-REF = UT-KVOI-P5-REF +              
042500                                              AAR-KVOI-REFILL(IX)         
042600                                                                          
042700                   COMPUTE UT-KVOT-P5-REF = UT-KVOT-P5-REF +              
042800                                            AAR-KVOT-REFILL(IX)           
042900                 ELSE                                                     
043000                   IF HELP-DATE < START-TID(7)                            
043100                     COMPUTE UT-KVOI-P6 = UT-KVOI-P6 +                    
043200                     AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +               
043300                     AAR-KVOI-SATS(IX)                                    
043400                                                                          
043500                     COMPUTE UT-KVOT-P6 = UT-KVOT-P6 +                    
043600                     AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +               
043700                     AAR-KVOT-SATS(IX)                                    
043800                                                                          
043900                     COMPUTE UT-KVOI-P6-REF = UT-KVOI-P6-REF +            
044000                                               AAR-KVOI-REFILL(IX)        
044100                                                                          
044200                     COMPUTE UT-KVOT-P6-REF = UT-KVOT-P6-REF +            
044300                                              AAR-KVOT-REFILL(IX)         
044400                   ELSE                                                   
044500                     IF HELP-DATE < START-TID(8)                          
044600                       COMPUTE UT-KVOI-P7 = UT-KVOI-P7 +                  
044700                       AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +             
044800                       AAR-KVOI-SATS(IX)                                  
044900                                                                          
045000                       COMPUTE UT-KVOT-P7 = UT-KVOT-P7 +                  
045100                       AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +             
045200                       AAR-KVOT-SATS(IX)                                  
045300                                                                          
045400                       COMPUTE UT-KVOI-P7-REF = UT-KVOI-P7-REF +          
045500                                               AAR-KVOI-REFILL(IX)        
045600                                                                          
045700                       COMPUTE UT-KVOT-P7-REF = UT-KVOT-P7-REF +          
045800                                               AAR-KVOT-REFILL(IX)        
045900                     ELSE                                                 
046000                       IF HELP-DATE < START-TID(9)                        
046100                         COMPUTE UT-KVOI-P8 = UT-KVOI-P8 +                
046200                         AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +           
046300                         AAR-KVOI-SATS(IX)                                
046400                                                                          
046500                         COMPUTE UT-KVOT-P8 = UT-KVOT-P8 +                
046600                         AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +           
046700                         AAR-KVOT-SATS(IX)                                
046800                                                                          
046900                         COMPUTE UT-KVOI-P8-REF = UT-KVOI-P8-REF +        
047000                                               AAR-KVOI-REFILL(IX)        
047100                                                                          
047200                         COMPUTE UT-KVOT-P8-REF = UT-KVOT-P8-REF +        
047300                                               AAR-KVOT-REFILL(IX)        
047400                       ELSE                                               
047500                         IF HELP-DATE < START-TID(10)                     
047600                           COMPUTE UT-KVOI-P9 = UT-KVOI-P9 +              
047700                           AAR-KVOI-DIV(IX) + AAR-KVOI-PROG(IX) +         
047800                           AAR-KVOI-SATS(IX)                              
047900                                                                          
048000                           COMPUTE UT-KVOT-P9 = UT-KVOT-P9 +              
048100                           AAR-KVOT-DIV(IX) + AAR-KVOT-PROG(IX) +         
048200                           AAR-KVOT-SATS(IX)                              
048300                                                                          
048400                           COMPUTE UT-KVOI-P9-REF =                       
048500                              UT-KVOI-P9-REF + AAR-KVOI-REFILL(IX)        
048600                                                                          
048700                           COMPUTE UT-KVOT-P9-REF =                       
048800                              UT-KVOT-P9-REF + AAR-KVOT-REFILL(IX)        
048900                         ELSE                                             
049000                           IF HELP-DATE < START-TID(11)                   
049100                             COMPUTE UT-KVOI-P10 = UT-KVOI-P10 +          
049200                             AAR-KVOI-DIV(IX) +                           
049300                             AAR-KVOI-PROG(IX) +                          
049400                             AAR-KVOI-SATS(IX)                            
049500                                                                          
049600                             COMPUTE UT-KVOT-P10 = UT-KVOT-P10 +          
049700                             AAR-KVOT-DIV(IX) +                           
049800                             AAR-KVOT-PROG(IX) +                          
049900                             AAR-KVOT-SATS(IX)                            
050000                                                                          
050100                             COMPUTE UT-KVOI-P10-REF =                    
050200                             UT-KVOI-P10-REF +                            
050300                             AAR-KVOI-REFILL(IX)                          
050400                                                                          
050500                             COMPUTE UT-KVOT-P10-REF =                    
050600                             UT-KVOT-P10-REF +                            
050700                             AAR-KVOT-REFILL(IX)                          
050800                                                                          
050900                           ELSE                                           
051000                             IF HELP-DATE < START-TID(12)                 
051100                               COMPUTE UT-KVOI-P11 = UT-KVOI-P11 +        
051200                               AAR-KVOI-DIV(IX) +                         
051300                               AAR-KVOI-PROG(IX) +                        
051400                               AAR-KVOI-SATS(IX)                          
051500                                                                          
051600                               COMPUTE UT-KVOT-P11 = UT-KVOT-P11 +        
051700                               AAR-KVOT-DIV(IX) +                         
051800                               AAR-KVOT-PROG(IX) +                        
051900                               AAR-KVOT-SATS(IX)                          
052000                                                                          
052100                               COMPUTE UT-KVOI-P11-REF =                  
052200                                             UT-KVOI-P11-REF +            
052300                                             AAR-KVOI-REFILL(IX)          
052400                                                                          
052500                               COMPUTE UT-KVOT-P11-REF =                  
052600                                             UT-KVOT-P11-REF +            
052700                                             AAR-KVOT-REFILL(IX)          
052800                                                                          
052900                             ELSE                                         
053000                               COMPUTE UT-KVOI-P12 = UT-KVOI-P12 +        
053100                               AAR-KVOI-DIV(IX) +                         
053200                               AAR-KVOI-PROG(IX) +                        
053300                               AAR-KVOI-SATS(IX)                          
053400                                                                          
053500                               COMPUTE UT-KVOT-P12 = UT-KVOT-P12 +        
053600                               AAR-KVOT-DIV(IX) +                         
053700                               AAR-KVOT-PROG(IX) +                        
053800                               AAR-KVOT-SATS(IX)                          
053900                                                                          
054000                               COMPUTE UT-KVOI-P12-REF =                  
054100                                              UT-KVOI-P12-REF +           
054200                                              AAR-KVOI-REFILL(IX)         
054300                                                                          
054400                               COMPUTE UT-KVOT-P12-REF =                  
054500                                              UT-KVOT-P12-REF +           
054600                                              AAR-KVOT-REFILL(IX)         
054700                             END-IF                                       
054800                           END-IF                                         
054900                         END-IF                                           
055000                       END-IF                                             
055100                     END-IF                                               
055200                   END-IF                                                 
055300                 END-IF                                                   
055400               END-IF                                                     
055500             END-IF                                                       
055600           END-IF                                                         
055700         END-IF                                                           
055800       END-IF                                                             
055900       ADD 1 TO IX                                                        
056000     END-PERFORM                                                          
056100     .                                                                    
056200     EJECT                                                                
056300 Z-FINIT SECTION.                                                         
056310                                                                          
056400     IF UT-IDARTNR > 0                                                    
056410       PERFORM S11-SKRIV-W23655                                           
056420     END-IF                                                               
056500     CLOSE W23655                                                         
056600     SKIP2                                                                
056700     MOVE 'S' TO POSTSUM-OPKOD                                            
056800     CALL POSTSUM USING POSTSUM-PARM                                      
056900     .                                                                    
057000     EJECT                                                                
057100 S11-SKRIV-W23655 SECTION.                                                
057200                                                                          
057300     WRITE UT-POST FROM UT-AREA                                           
057400                                                                          
057500     MOVE '811 '    TO POSTSUM-TRANSTYP                                   
057600     MOVE 'W23655' TO POSTSUM-FDNAMN                                      
057700     MOVE 'W23655D1' TO POSTSUM-DDNAMN2                                   
057800     CALL POSTSUM USING POSTSUM-PARM                                      
057900     .                                                                    
058000     EJECT                                                                
058100* --- IMS SEKTIONER ---                                                   
058200                                                                          
058300 IMS-GET-WDL8 SECTION.                                                    
058400     CALL CBLTDLI USING GN WDL8-PCB DLI-IO-WDL8                           
058500     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
058600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
058700     PERFORM IMS-STATUSKONTROLL                                           
058800     .                                                                    
058900     EJECT                                                                
059000 IMS-STATUSKONTROLL SECTION.                                              
059100     SET STATUS-IX TO 1                                                   
059200     SEARCH GODK-STATUS                                                   
059300       AT END                                                             
059400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059500           DELIMITED BY SIZE INTO FELTEXT                                 
059600         DISPLAY FELTEXT                                                  
059700         CALL FELLOG                                                      
059800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
059900         CONTINUE                                                         
060000     END-SEARCH                                                           
060100     .                                                                    
