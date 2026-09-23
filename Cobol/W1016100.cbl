000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1016100.                                                
000400 AUTHOR.         JANNE MELANDER                                           
000500 DATE-WRITTEN.   MARS. 87.                                                
000800***************************************************************           
000900*    FUNKTION.                                                *           
001000*                                                             *           
001100*        PROGRAMMET ÄR EN KÖ-BILD                             *           
001200*                                                             *           
001300*        BEREDAREN ANGER PROJ  (IDPROJ)   OCH DCN-NO(IDAO).   *           
001400*        HAN FÅR DÅ DE ARTIKLAR SOM LIGGER UNDER DE/DEN       *           
001500*        ANGIVNA NYCKELN.                                     *           
001600*        FÖR ATT KOMMA TILL NÄSTA BILD SÅ VÄLJER HAN UT EN    *           
001700*        ARTIKEL MED ETT 'S' OCH TRYCKER PFK9.                *           
001800*                                                             *           
002700***************************************************************           
002800     INDATA.                                                              
002900         TRANSAKTION: W1T161                                              
003000         MID:         W1I16101                                            
003100                                                                          
003200     UTDATA.                                                              
003300         MOD:         W1O16101                                            
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP3                                                                
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W1016100'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  COL-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77  DIST-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  MAX-IX-PLUS-1               PIC S9(9)   VALUE +15  COMP SYNC.        
004900 77  MAX-MOD-LAENGD              PIC S9(4)                                
005000                                      VALUE +1015 COMP SYNC.              
005100                                                                          
005200 77  WS-IDAO                     PIC X(10)   VALUE SPACE.                 
005300 77  WS-IDPROJ                   PIC X(4)    VALUE SPACE.                 
005400                                                                          
005420 01  DYNAMISKA-SUBPROGRAM.                                                
005430     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005440     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005450                                                                          
005500 01  INDATA-SW                   PIC X       VALUE 'N'.                   
005600    88 INDATA-OK                             VALUE 'J'.                   
005700     SKIP3                                                                
005800 01  NYCKLAR-TILL-DLI.                                                    
005900   03  W-WDD2C1KY-MIN.                                                    
006000     05  W-IDAO-MIN           PIC X(10).                                  
006100     05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.                
006300                                                                          
006400   03  W-WDD2C1KY-MAX.                                                    
006500     05  W-IDAO-MAX           PIC X(10).                                  
006600     05  FILLER               PIC S9(9) COMP-3 VALUE +999999999.          
006800                                                                          
006900                                                                          
007000   03  W-IDPROJ-X.                                                        
007100     05  W-IDPROJ                PIC X(4)   VALUE SPACE.                  
007200     SKIP3                                                                
007300   03  W-IDARTNR-X.                                                       
007400     05  WX-IDARTNR               PIC S9(9)  COMP-3 VALUE ZERO.           
007500                                                                          
007600     EJECT                                                                
007700 01  MEDDELANDE.                                                          
007800   03  FEL1.                                                              
007900     05 FILLER                   PIC X(40)                                
008000          VALUE 'UPPLYSTA FÄLT FEL'.                                      
008100     05 FILLER                   PIC X(40)                                
008200          VALUE 'CORRECT HIGHLIGHTED FIELDS'.                             
008300   03  FILLER REDEFINES FEL1.                                             
008400     05  FEL-1                   PIC X(40)   OCCURS 2.                    
008500                                                                          
008600   03  FEL2.                                                              
008700     05 FILLER                   PIC X(40)                                
008800          VALUE 'SÖKT ÄONUMMER    SAKNAS I BASEN'.                        
008900     05 FILLER                   PIC X(40)                                
009000          VALUE 'DCN-NUMBER IS MISSING     '.                             
009100   03  FILLER REDEFINES FEL2.                                             
009200     05  FEL-2                   PIC X(40)   OCCURS 2.                    
009300                                                                          
009400   03  FEL3.                                                              
009500     05 FILLER                   PIC X(40)                                
009600          VALUE 'ARTIKLAR SAKNAS PÅ ANGIVEN NYCKEL '.                     
009700     05 FILLER                   PIC X(40)                                
009800          VALUE 'PARTS MISSING                     '.                     
009900   03  FILLER REDEFINES FEL3.                                             
010000     05  FEL-3                   PIC X(40)   OCCURS 2.                    
010100                                                                          
010200   03  FEL4.                                                              
010300     05 FILLER                   PIC X(40)                                
010400          VALUE 'ANGE NYCKLAR FÖR SÖKNING      '.                         
010500     05 FILLER                   PIC X(40)                                
010600          VALUE 'PLAESE PUT IN KEYS      '.                               
010700   03  FILLER REDEFINES FEL4.                                             
010800     05  FEL-4                   PIC X(40)   OCCURS 2.                    
010900                                                                          
011000   03  MED1.                                                              
011100     05 FILLER                   PIC X(40)                                
011200          VALUE 'UPPDATERING GJORD        '.                              
011300     05 FILLER                   PIC X(40)                                
011400          VALUE 'UPDATED                     '.                           
011500   03  FILLER REDEFINES MED1.                                             
011600     05  MED-1                   PIC X(40)   OCCURS 2.                    
011700                                                                          
011800   03  MED2.                                                              
011900     05 FILLER                   PIC X(40)                                
012000          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
012100     05 FILLER                   PIC X(40)                                
012200          VALUE 'PRESS PF8 FOR MORE LINES'.                               
012300   03  FILLER REDEFINES MED2.                                             
012400     05  MED-2                   PIC X(40)   OCCURS 2.                    
012500                                                                          
012600   03  MED3.                                                              
012700     05 FILLER                   PIC X(40)                                
012800          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
012900     05 FILLER                   PIC X(40)                                
013000          VALUE 'THIS IS THE FIRST PAGE'.                                 
013100   03  FILLER REDEFINES MED3.                                             
013200     05  MED-3                   PIC X(40)   OCCURS 2.                    
013300                                                                          
013400   03  MED4.                                                              
013500     05 FILLER                   PIC X(40)                                
013600          VALUE 'DETTA ÄR SISTA SIDAN'.                                   
013700     05 FILLER                   PIC X(40)                                
013800          VALUE 'THIS IS THE LAST PAGE'.                                  
013900   03  FILLER REDEFINES MED4.                                             
014000     05  MED-4                   PIC X(40)   OCCURS 2.                    
014100     EJECT                                                                
014200******************************************************************        
014300*                                                                         
014400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
014700     SKIP3                                                                
014800*01  MID -COPY W1I16101                                                   
015000     EJECT                                                                
015100*01  MID -COPY W1I11401  -PRE 1114-                                       
015300     EJECT                                                                
015400*01  -COPY WMSGAREA                                                       
015600     EJECT                                                                
015700*  03  MOD -COPY W1O16101           -RED MSG-AREA.                        
015900     EJECT                                                                
016000*01  -COPY WMFSAREA                                                       
016200     EJECT                                                                
016300******************************************************************        
016400*                                                                         
016500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016600*                                                                         
016700 01  IMS-WS.                                                              
016800   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
016900     SKIP3                                                                
017000*                        **** STATUS-KOD FRÅN IMS                         
017100   03  STATUS-WS                 PIC XX.                                  
017200     88  SEGMENT-FINNS                       VALUE '  '.                  
017300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017400     SKIP3                                                                
017500   03  GODK-STATUSKODER.                                                  
017600     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017700     SKIP3                                                                
017800 01    SSA1                      PIC X(96).                               
017900 01    SSA2                      PIC X(64).                               
018000     EJECT                                                                
018100*                            IMS FUNKTIONSKODER                           
018200*01    -COPY W0003                                                        
018400     EJECT                                                                
018500*                            DLI INPUT-OUTPUT AREA                        
018600 01  DLI-IO-AREA.                                                         
018700   03  IO-AREA                   PIC X(600)  VALUE SPACE.                 
018800     SKIP3                                                                
018900*  03  WLARTG01 -COPY WDD201 -PRE ARTG-  -RED IO-AREA.                    
019100     EJECT                                                                
019200*  03  WLARTJ01 -COPY WDD2C1 -PRE ARTJ-  -RED IO-AREA.                    
019400     EJECT                                                                
019500 LINKAGE SECTION.                                                         
019600*01  -COPY W0009     -PRE MSG-                                            
019800     EJECT                                                                
019900*01  -COPY W0008     -PRE ARTG-                                           
020100     05  FILLER                  PIC X.                                   
020200     EJECT                                                                
020300*01  -COPY W0008     -PRE ARTJ-                                           
020500     05  FILLER                  PIC X.                                   
020600     EJECT                                                                
020700 PROCEDURE DIVISION USING MSG-PCB  ARTG-PCB ARTJ-PCB.                     
020800     ENTRY 'DLITCBL' USING MSG-PCB ARTG-PCB ARTJ-PCB.                     
020900                                                                          
021000     PERFORM IMS-GET-MSG                                                  
021100     IF SEGMENT-FINNS                                                     
021200       PERFORM A-INIT-SPARA-INPUT                                         
021300       IF WS-IDPROJ = SPACE AND WS-IDAO = SPACE                           
021500          MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL                           
021600          PERFORM S3-RENSA-FAELT-MOD                                      
021700       ELSE                                                               
021800          IF WS-IDAO = SPACE                                              
021900             MOVE LOW-VALUE  TO W-IDAO-MIN                                
022000             MOVE HIGH-VALUE TO W-IDAO-MAX                                
022100          ELSE                                                            
022200             MOVE WS-IDAO    TO W-IDAO-MIN                                
022300                                W-IDAO-MAX                                
022400          END-IF                                                          
022500          IF WS-IDPROJ = SPACE                                            
022600             CONTINUE                                                     
022700          ELSE                                                            
022800             MOVE WS-IDPROJ  TO W-IDPROJ                                  
022900          END-IF                                                          
023000          IF MFS-IDPFK = '7'                                              
023100             MOVE ZERO TO W-IDARTNR                                       
023200          ELSE                                                            
023300             IF MFS-IDPFK = '8'                                           
023700                MOVE MID-IDARTNR-PF8 TO W-IDARTNR                         
024100                IF MID-IDAO-PF8 = SPACE                                   
024200                   CONTINUE                                               
024300                ELSE                                                      
024400                   MOVE MID-IDAO-PF8 TO W-IDAO-MIN   WS-IDAO              
024600                END-IF                                                    
024700             ELSE                                                         
025100                MOVE MID-IDARTNR-ENTER TO W-IDARTNR                       
025500             END-IF                                                       
025600          END-IF                                                          
025700          PERFORM B-LAES-VISA-INFO                                        
025800       END-IF                                                             
025900       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
026000       PERFORM IMS-INSERT-MSG                                             
026100     END-IF                                                               
026300     MOVE ZERO TO RETURN-CODE                                             
026400     GOBACK.                                                              
026500     EJECT                                                                
026600 A-INIT-SPARA-INPUT SECTION.                                              
026700                                                                          
026800     IF MSG-DUBBLA-TRANSKODER                                             
026900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I16101                 
027000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
027100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027200       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
027300       MOVE MSG-IDPFK TO MFS-IDPFK                                        
027400     ELSE                                                                 
027500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I16101                  
027600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
027700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027800       MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                         
027900     END-IF                                                               
028000                                                                          
028100     IF MFS-IDTRANS NOT = '1161'                                          
028200       MOVE SPACE TO MFS-KDTRTYP                                          
028300       MOVE '7'   TO MFS-IDPFK                                            
028400       IF MFS-IDTRANS = '1114'                                            
028500          MOVE MID-W1I16101        TO 1114-MID-W1I11401                   
028600          MOVE 1114-MID-IDPROJ-KEY TO MID-IDPROJ-IN                       
028700          MOVE SPACE               TO MID-IDAO-IN                         
028800                                      MID-IDAO-PF8                        
028810       ELSE                                                               
028820          MOVE SPACE               TO MID-IDAO-IN                         
028830                                      MID-IDPROJ-IN                       
028900       END-IF                                                             
029000     END-IF                                                               
029100                                                                          
029200     IF MID-IDAO-IN = ALL '+'                                             
029300        MOVE MID-IDAO-UT TO WS-IDAO                                       
029400     ELSE                                                                 
029500        MOVE MID-IDAO-IN TO WS-IDAO                                       
029600        MOVE ' '             TO MFS-KDTRTYP                               
029700        MOVE '7'             TO MFS-IDPFK                                 
029800     END-IF                                                               
029900                                                                          
030000     IF MID-IDPROJ-IN = ALL '+'                                           
030100        MOVE MID-IDPROJ-UT TO WS-IDPROJ                                   
030200     ELSE                                                                 
030300        MOVE MID-IDPROJ-IN TO WS-IDPROJ                                   
030400        MOVE ' '             TO MFS-KDTRTYP                               
030500        MOVE '7'             TO MFS-IDPFK                                 
030600     END-IF                                                               
030700                                                                          
030800     IF MFS-KDMFSFOR = '2'                                                
030900       MOVE +2 TO SPRAK-IX                                                
031000     ELSE                                                                 
031100       MOVE +1 TO SPRAK-IX                                                
031200     END-IF                                                               
031300                                                                          
031400     MOVE LOW-VALUE TO MSG-AREA                                           
031500     MOVE 'W1O161N1' TO MFS-IDMOD                                         
031600     MOVE '1161' TO MOD-IDTRANS                                           
031700     MOVE WS-IDPROJ TO MOD-IDPROJ-UT                                      
031800     MOVE WS-IDAO   TO MOD-IDAO-UT                                        
031900                                                                          
032000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
032100                             MOD-IDPROJ-IN MOD-IDAO-IN                    
032200     .                                                                    
032300     EJECT                                                                
032400 B-LAES-VISA-INFO SECTION.                                                
032600***********************************************                           
032800*  HÄR BÖRJAR DEN NYA LÖSNINGEN                                           
032900*  LÖSNINGEN BYGGER PÅ 42 RADER OCH INGA KOLUMNER.                        
033100***********************************************                           
033400     PERFORM BA-LAES-NAESTA-ARTIKEL                                       
033410                                                                          
033610     IF SEGMENT-FINNS                                                     
033630        MOVE ARTG-ART-IDARTNR TO MOD-IDARTNR-ENTER                        
033640        MOVE ARTG-ART-IDAO    TO MOD-IDAO-ENTER                           
033641     ELSE                                                                 
033642        MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                              
033643        MOVE ZERO  TO MOD-IDARTNR-ENTER                                   
033644        MOVE SPACE TO MOD-IDAO-ENTER                                      
033650     END-IF                                                               
033700                                                                          
033710     MOVE +1 TO DIST-INDX                                                 
033720     MOVE +1 TO RAD-INDX                                                  
033800     PERFORM UNTIL RAD-INDX > 42                                          
033900        IF SEGMENT-FINNS                                                  
034400           MOVE ARTG-ART-IDARTNR TO MOD-IDARTNR(RAD-INDX)                 
034700           MOVE ARTG-ART-IDAO    TO MOD-IDAO(RAD-INDX)                    
034800           PERFORM BA-LAES-NAESTA-ARTIKEL                                 
035400        ELSE                                                              
035800           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(RAD-INDX)                  
035900                                   MOD-IDAO(RAD-INDX)                     
036000        END-IF                                                            
036100        MOVE MFS-RENSA-FAELT    TO MOD-AFFECT(RAD-INDX)                   
036200        ADD +1 TO RAD-INDX                                                
036300     END-PERFORM                                                          
036310     IF SEGMENT-FINNS                                                     
036320        MOVE ARTG-ART-IDARTNR TO MOD-IDARTNR-PF8                          
036330        MOVE ARTG-ART-IDAO    TO MOD-IDAO-PF8                             
036340        MOVE MED-2 (SPRAK-IX) TO MOD-TEMFSFEL                             
036341     ELSE                                                                 
036342        MOVE ZERO  TO MOD-IDARTNR-PF8                                     
036343        MOVE SPACE TO MOD-IDAO-PF8                                        
036350     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 BA-LAES-NAESTA-ARTIKEL  SECTION.                                         
036700                                                                          
036800     IF WS-IDAO = SPACE                                                   
036900        PERFORM IMS-GET-ARTJ-IDPROJ                                       
037000     ELSE                                                                 
037100        IF WS-IDPROJ = SPACE                                              
037200           PERFORM IMS-GET-ARTJ-IDAO                                      
037300        ELSE                                                              
037400           PERFORM IMS-GET-ARTJ-IDAO-IDPROJ                               
037500        END-IF                                                            
037600     END-IF                                                               
037700     IF SEGMENT-FINNS                                                     
037800        MOVE ARTJ-SEQC-IDARTNR TO WX-IDARTNR                              
037900        PERFORM IMS-GET-ARTG-ARTIKEL-UNIK                                 
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300                                                                          
038400 S3-RENSA-FAELT-MOD SECTION.                                              
038500     MOVE +1                TO INDX                                       
038600                                                                          
038700     PERFORM UNTIL INDX > 42                                              
038800        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(INDX)                         
038900                                MOD-IDAO(INDX)                            
039000                                MOD-AFFECT(INDX)                          
039100        ADD +1 TO INDX                                                    
039200     END-PERFORM                                                          
039300     .                                                                    
039400     EJECT                                                                
039500* IMS SEKTIONER                                                           
039600     SKIP3                                                                
039700 IMS-GET-MSG SECTION.                                                     
039800                                                                          
039900     MOVE '  QC' TO GODK-STATUSKODER                                      
040000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
040100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040200     PERFORM IMS-STATUSKONTROLL                                           
040300     .                                                                    
040400     SKIP3                                                                
040500 IMS-INSERT-MSG SECTION.                                                  
040600                                                                          
040700     IF NOT ENGLISH-TEXT                                                  
040800       MOVE '0' TO MFS-KDHUVOMR                                           
040900     END-IF                                                               
041000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
041100     MOVE SPACE TO GODK-STATUSKODER                                       
041200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
041300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041400     PERFORM IMS-STATUSKONTROLL                                           
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800 IMS-GET-ARTG-ARTIKEL-UNIK SECTION.                                       
041900                                                                          
042000     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
042100            DELIMITED BY SIZE INTO SSA1                                   
042200     MOVE '  GE' TO GODK-STATUSKODER                                      
042300     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-AREA SSA1                      
042400     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
042500     PERFORM IMS-STATUSKONTROLL                                           
042600     .                                                                    
042700     SKIP3                                                                
042800 IMS-GET-ARTJ-IDAO     SECTION.                                           
042900                                                                          
043000     STRING 'WLARTJ01(WDD2C1KY=>' W-WDD2C1KY-MIN                          
043100                    '&WDD2C1KY<=' W-WDD2C1KY-MAX ')'                      
043200            DELIMITED BY SIZE INTO SSA1                                   
043300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
043400     CALL CBLTDLI USING GN ARTJ-PCB DLI-IO-AREA SSA1                      
043500     MOVE ARTJ-STATUS-CODE TO STATUS-WS                                   
043600     PERFORM IMS-STATUSKONTROLL                                           
043700     .                                                                    
043800     SKIP3                                                                
043900 IMS-GET-ARTJ-IDPROJ SECTION.                                             
044000                                                                          
044100     STRING 'WLARTJ01(IDPROJ   =' W-IDPROJ-X ')'                          
044200            DELIMITED BY SIZE INTO SSA1                                   
044300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
044400     CALL CBLTDLI USING GN ARTJ-PCB DLI-IO-AREA SSA1                      
044500     MOVE ARTJ-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     .                                                                    
044800     SKIP3                                                                
044900 IMS-GET-ARTJ-IDAO-IDPROJ SECTION.                                        
045000                                                                          
045100     STRING 'WLARTJ01(WDD2C1KY=>' W-WDD2C1KY-MIN                          
045200                    '&WDD2C1KY<=' W-WDD2C1KY-MAX                          
045300                    '&IDPROJ   =' W-IDPROJ-X ')'                          
045400            DELIMITED BY SIZE INTO SSA1                                   
045500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
045600     CALL CBLTDLI USING GN ARTJ-PCB DLI-IO-AREA SSA1                      
045700     MOVE ARTJ-STATUS-CODE TO STATUS-WS                                   
045800     PERFORM IMS-STATUSKONTROLL                                           
045900     .                                                                    
046000     SKIP3                                                                
046100 IMS-STATUSKONTROLL SECTION.                                              
046200                                                                          
046300     SET STATUS-IX TO 1                                                   
046400     SEARCH GODK-STATUS                                                   
046410       AT END                                                             
046420         CALL FELLOG                                                      
046500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
046600     END-SEARCH                                                           
046700     .                                                                    
