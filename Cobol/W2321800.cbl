000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2321800.                                                
000400 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500 DATE-WRITTEN.   96/08/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        UPPDATERAR WDL8:AS ROT VAD GÄLLER DE SENASTE                     
001100*        TRE VECKORNAS KVOI, NY TOM VECKA SKAPAS                          
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WLOIGB (WDL8)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200     SKIP2                                                                
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W2321800'.            
003400 01  CHKP-VAR.                                                            
003500 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
003600 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
003700 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
003800 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
003900 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004000 03  CHKP-MAX                    PIC S9(3)   VALUE +200.                  
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300     SKIP2                                                                
004400 01  FELTEXT.                                                             
004500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004700     SKIP2                                                                
004800 01  ARB-AREOR.                                                           
004900     03  IX-G                    PIC S9(3)  COMP-3.                       
005000     03  IX-N                    PIC S9(3)  COMP-3.                       
005100     EJECT                                                                
005200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005300 01  FILLER REDEFINES DAGENS-DATUM.                                       
005400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005700                                                                          
005800 01  MASK-TIAAVVD                PIC 9(5)    VALUE ZERO.                  
005900 01  FILLER REDEFINES MASK-TIAAVVD.                                       
006000     03  MASK-AAR                PIC 9(2).                                
006100     03  MASK-VECKA              PIC 9(2).                                
006200     03  MASK-DAG                PIC 9(1).                                
006300 01  FILLER REDEFINES MASK-TIAAVVD.                                       
006400     03  MASK-TIAAVV             PIC 9(4).                                
006500     03  FILLER                  PIC 9(1).                                
006600                                                                          
006700 01  NYSTART-TIAAVVD             PIC 9(5)    VALUE ZERO.                  
006800 01  FILLER REDEFINES NYSTART-TIAAVVD.                                    
006900     03  NYSTART-AAR             PIC 9(2).                                
007000     03  NYSTART-VECKA           PIC 9(2).                                
007100     03  NYSTART-DAG             PIC 9(1).                                
007200 01  FILLER REDEFINES NYSTART-TIAAVVD.                                    
007300     03  NYSTART-TIAAVV          PIC 9(4).                                
007400     03  FILLER                  PIC 9(1).                                
007500 01  FILLER REDEFINES NYSTART-TIAAVVD.                                    
007600     03  FILLER                  PIC 9(2).                                
007700     03  NYSTART-TIVVD           PIC 9(3).                                
007800                                                                          
007900 01  NYSTART-TIAAVVD-1           PIC 9(5)    VALUE ZERO.                  
008000 01  FILLER REDEFINES NYSTART-TIAAVVD-1.                                  
008100     03  NYSTART-AAR-1           PIC 9(2).                                
008200     03  NYSTART-VECKA-1         PIC 9(2).                                
008300     03  NYSTART-DAG-1           PIC 9(1).                                
008400 01  FILLER REDEFINES NYSTART-TIAAVVD-1.                                  
008500     03  NYSTART-TIAAVV-1        PIC 9(4).                                
008600     03  FILLER                  PIC 9(1).                                
008700 01  FILLER REDEFINES NYSTART-TIAAVVD-1.                                  
008800     03  FILLER                  PIC 9(2).                                
008900     03  NYSTART-TIVVD-1         PIC 9(3).                                
009000                                                                          
009100 01  WS-TIVVD                    PIC 9(3)    VALUE ZERO.                  
009200     EJECT                                                                
009300 01  DYNAMISKA-SUBPROGRAM.                                                
009400*                                                                         
009500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009900     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL POSTSUM                                          
010400*                                                                         
010500*01  -COPY W0005   -PRE  POSTSUM-                                         
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL DATKORT                                          
010800*                                                                         
010900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23218'.              
011000     SKIP2                                                                
011100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011200     SKIP2                                                                
011300*01  -COPY WDATKORT                                                       
011400     EJECT                                                                
011500*01  -COPY WDATAREA                                                       
011600     EJECT                                                                
011700* VARIABLER TILL SUBPROGRAM W009VADD                                      
011800 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
011900 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012500     SKIP3                                                                
012600 01  NYCKLAR-TILL-DLI.                                                    
012700     03  W-IDARTNR-X.                                                     
012800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012900     SKIP2                                                                
013000*    --- STATUS-KOD FRÅN IMS                                              
013100 01  STATUS-WS                   PIC XX.                                  
013200     88  SEGMENT-FINNS                       VALUE '  '.                  
013300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013600     88  IMS-EJ-OK                           VALUE 'XD'.                  
013700     SKIP2                                                                
013800 01  GODK-STATUSKODER.                                                    
013900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014000     SKIP3                                                                
014100 01  SSA1                        PIC X(64).                               
014200 01  SSA2                        PIC X(64).                               
014300     EJECT                                                                
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014600     EJECT                                                                
014700*    ---  DLI INPUT-OUTPUT AREA                                           
014800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014900     SKIP3                                                                
015000 01  DLI-IO-AREA.                                                         
015300     03  WLOIGB01.                                                        
015400*        05  -COPY WDL801                                                 
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700                                                                          
015800*01  -COPY W0009   -PRE MSG-                                              
015900     EJECT                                                                
016000*01  -COPY W0008  -PRE OIGB-                                              
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300 PROCEDURE DIVISION  USING MSG-PCB OIGB-PCB.                              
016400 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING MSG-PCB OIGB-PCB.                              
016600                                                                          
016700                                                                          
016800     PERFORM A-INIT                                                       
016900     PERFORM IMS-GHN-OIGB-ROT                                             
017000                                                                          
017100     PERFORM UNTIL SEGMENT-SLUT                                           
017200       IF CHKP-ANT > CHKP-MAX                                             
017300         PERFORM X-TAG-CHECKPOINT                                         
017400       END-IF                                                             
017500                                                                          
017600       IF ART-TIVVD (15) NOT = NYSTART-TIVVD                              
017700          PERFORM B-FLYTTA-VECKA                                          
017800                                                                          
017900          PERFORM IMS-REPL-OIGB                                           
018000       END-IF                                                             
018100                                                                          
018200       PERFORM IMS-GHN-OIGB-ROT                                           
018300                                                                          
018400     END-PERFORM                                                          
018500                                                                          
018600                                                                          
018700     PERFORM Z-FINIT                                                      
018800                                                                          
018900     MOVE ZERO TO RETURN-CODE                                             
019000     GOBACK                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 A-INIT SECTION.                                                          
019400     SKIP2                                                                
019500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019600                                                                          
019700     PERFORM IMS-RESTART                                                  
019800                                                                          
019900*    CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
020000*    MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
020100*    MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
020200*    MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
020300                                                                          
020400     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
020500***  MOVE AAMMDD      TO DAT-I-TIDATUM                                    
020600                                                                          
020700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
020800                     DAT-O-TIDATUM DAT-KDSVAR                             
020900                                                                          
021000     IF DAT-KDSVAR-OK                                                     
021100       MOVE DAT-TIAAVVD  TO MASK-TIAAVVD                                  
021200     ELSE                                                                 
021300       MOVE '******** FEL I DATKONV *******' TO FELTEXT-STR               
021400       DISPLAY FELTEXT                                                    
021500       CALL FELLOG                                                        
021600     END-IF                                                               
021700                                                                          
021800     IF MASK-DAG < 4                                                      
021900        MOVE 1           TO NYSTART-DAG                                   
022000        MOVE MASK-VECKA  TO NYSTART-VECKA                                 
022100        MOVE MASK-AAR    TO NYSTART-AAR                                   
022200     ELSE                                                                 
022300        MOVE 1           TO ANTAL-VECKOR                                  
022400        MOVE MASK-TIAAVV TO DATUM-AAVV                                    
022500        CALL W009VADD USING DATUM-AAVV ANTAL-VECKOR                       
022600        MOVE DATUM-AAVV  TO NYSTART-TIAAVV                                
022700        MOVE 1           TO NYSTART-DAG                                   
022800     END-IF                                                               
022810     DISPLAY ' ***  STARTPUNKT VVD ' NYSTART-TIVVD                        
022900     EJECT                                                                
023000*    FÖR KONTROLL AV VECKONR PÅ SISTA VECKAN PÅ BASEN                     
023010                                                                          
023100     MOVE -1             TO ANTAL-VECKOR                                  
023200     MOVE NYSTART-TIAAVV TO DATUM-AAVV                                    
023300     CALL W009VADD USING DATUM-AAVV ANTAL-VECKOR                          
023400     MOVE DATUM-AAVV     TO NYSTART-TIAAVV-1                              
023500     MOVE 1              TO NYSTART-DAG-1                                 
023510     DISPLAY ' ***  STARTPUNKT VVD - 1 ' NYSTART-TIVVD-1                  
023600     .                                                                    
023700     EJECT                                                                
023800 B-FLYTTA-VECKA SECTION.                                                  
023900     SKIP2                                                                
024000*    IF NYSTART-TIVVD-1 NOT = ART-TIVVD (15)                              
024100*       MOVE ' * FEL: EJ VECKA - 1 ' TO FELTEXT-STR                       
024200*       DISPLAY FELTEXT                                                   
024300*    END-IF                                                               
024400                                                                          
024500     MOVE 8       TO IX-G                                                 
024600     MOVE 1       TO IX-N                                                 
024700     PERFORM UNTIL IX-N > 14                                              
024800        MOVE ART-DAGLIG (IX-G) TO ART-DAGLIG (IX-N)                       
024900        ADD +1    TO IX-G IX-N                                            
025000     END-PERFORM                                                          
025100                                                                          
025200*    NU ÄR IX-N = 15                                                      
025300     MOVE NYSTART-TIVVD TO WS-TIVVD                                       
025400     PERFORM UNTIL IX-N > 21                                              
025500        MOVE WS-TIVVD       TO ART-TIVVD      (IX-N)                      
025600        MOVE ZERO           TO ART-KVOI-DIV   (IX-N)                      
025700        MOVE ZERO           TO ART-KVOI-NDC   (IX-N)                      
025800        MOVE ZERO           TO ART-KVOI-PROG  (IX-N)                      
025900        MOVE ZERO           TO ART-KVOI-REFILL(IX-N)                      
026000        MOVE ZERO           TO ART-KVOI-SATS  (IX-N)                      
026100        MOVE ZERO           TO ART-KVOI-SDC   (IX-N)                      
026110        MOVE ZERO           TO ART-KVOI-LEDTID (IX-N)                     
026200        ADD +1              TO WS-TIVVD                                   
026300        ADD +1              TO IX-N                                       
026400     END-PERFORM                                                          
026500     .                                                                    
026600     EJECT                                                                
026700 Z-FINIT SECTION.                                                         
026800                                                                          
026900     MOVE 'S' TO POSTSUM-OPKOD                                            
027000     CALL POSTSUM USING POSTSUM-PARM                                      
027100     .                                                                    
027200     EJECT                                                                
027300 X-TAG-CHECKPOINT   SECTION.                                              
027400                                                                          
027500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
027600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
027700     MOVE ART-IDARTNR TO W-IDARTNR                                        
027800                                                                          
027900     PERFORM IMS-CHECKPOINT                                               
028000                                                                          
028100     MOVE ZERO TO CHKP-ANT                                                
028200* --- LÄS OM DATABAS OM DET BEHÖVS                                        
028300     PERFORM IMS-GET-OIGB-ROT                                             
028400     .                                                                    
028500     EJECT                                                                
028600* --- IMS SEKTIONER ---                                                   
028700     SKIP3                                                                
028800 IMS-GET-OIGB-ROT SECTION.                                                
028900                                                                          
029000     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
029100          DELIMITED BY SIZE INTO SSA1                                     
029200     MOVE '  GE' TO GODK-STATUSKODER                                      
029300     CALL CBLTDLI USING GHU OIGB-PCB DLI-IO-AREA SSA1                     
029400     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
029500     PERFORM IMS-STATUSKONTROLL                                           
029600     .                                                                    
029700     SKIP3                                                                
029800 IMS-GHN-OIGB-ROT SECTION.                                                
029900                                                                          
030000     STRING 'WLOIGB01     '                                               
030100          DELIMITED BY SIZE INTO SSA1                                     
030200     MOVE '  GB' TO GODK-STATUSKODER                                      
030300     CALL CBLTDLI USING GHN OIGB-PCB DLI-IO-AREA SSA1                     
030400     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
030500     PERFORM IMS-STATUSKONTROLL                                           
030600     .                                                                    
030700     EJECT                                                                
030800 IMS-REPL-OIGB SECTION.                                                   
030900                                                                          
031000     MOVE '  ' TO GODK-STATUSKODER                                        
031100     CALL CBLTDLI USING REPL OIGB-PCB DLI-IO-AREA                         
031200     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
031300     PERFORM IMS-STATUSKONTROLL                                           
031400                                                                          
031500     ADD +1              TO CHKP-ANT                                      
031600                                                                          
031700     MOVE 'WDL801'       TO POSTSUM-FDNAMN                                
031800     MOVE 'WLOIGB01'     TO POSTSUM-DDNAMN2                               
031900     MOVE 'REPL'         TO POSTSUM-TRANSTYP                              
032000     CALL POSTSUM USING POSTSUM-PARM                                      
032100     .                                                                    
032200     EJECT                                                                
032300 IMS-RESTART SECTION.                                                     
032400     SKIP2                                                                
032500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
032600     MOVE '  ' TO GODK-STATUSKODER                                        
032700     CALL CBLTDLI USING XRST MSG-PCB                                      
032800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
032900                        CHKP-AREA-LENGTH CHKP-AREA                        
033000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     .                                                                    
033300     EJECT                                                                
033400 IMS-CHECKPOINT SECTION.                                                  
033500     SKIP2                                                                
033600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
033700     MOVE '  XD' TO GODK-STATUSKODER                                      
033800     CALL CBLTDLI USING CHKP MSG-PCB                                      
033900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
034000                        CHKP-AREA-LENGTH CHKP-AREA                        
034100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034200     PERFORM IMS-STATUSKONTROLL                                           
034300                                                                          
034400     IF IMS-EJ-OK                                                         
034500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
034600       DISPLAY FELTEXT                                                    
034700       CALL FELLOG                                                        
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 IMS-STATUSKONTROLL SECTION.                                              
035200     SKIP2                                                                
035300     SET STATUS-IX TO 1                                                   
035400     SEARCH GODK-STATUS                                                   
035500       AT END                                                             
035600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035700           DELIMITED BY SIZE INTO FELTEXT                                 
035800         DISPLAY FELTEXT                                                  
035900         CALL FELLOG                                                      
036000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036100         CONTINUE                                                         
036200     END-SEARCH                                                           
036300     .                                                                    
