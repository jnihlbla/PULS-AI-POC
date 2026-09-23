000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1142800.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   05/02/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR WDD2 MED ARTIKEL INFO FRÅN FLIT                       
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDD2                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- ARTINFO FRÅN FLIT                                          
002200     SELECT W11427                     ASSIGN TO W11428D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W11427                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W11426      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W1142800'.            
003700 01  CHKP-VAR.                                                            
003800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004300     03 CHKP-MAX                 PIC S9(3)   VALUE +50  COMP-3.           
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
004700 77  IX-MAX                      PIC S9(3)   VALUE +15 COMP-3.            
004800 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
004900 77  SW-UPPDATERA                PIC X       VALUE SPACE.                 
005000 77  SW-UPG                      PIC X       VALUE 'N'.                   
005100 77  SW-TPD                      PIC X       VALUE 'N'.                   
005110 77  NEW-DAAAVV               PIC 9(6)    VALUE ZERO.                     
005120 77  SW-DATUM-OK              PIC X       VALUE 'N'.                      
005200                                                                          
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700 77  W11427-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W11427                       VALUE 'J'.                   
005900     EJECT                                                                
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500                                                                          
006510 01  WS-AAAAVV.                                                           
006520     03 WS-SEKEL                 PIC 9(2).                                
006530     03 WS-AAR                   PIC 9(2).                                
006540     03 WS-VECKA                 PIC 9(2).                                
006550                                                                          
006560 01  NY-AAVVD                    PIC 9(5).                                
006570 01  FILLER REDEFINES NY-AAVVD.                                           
006580     03 NY-AAR                   PIC 9(2).                                
006590     03 NY-VECKA                 PIC 9(2).                                
006591     03 NY-DAG                   PIC 9(1).                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007110     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL DATKORT                                          
007400*                                                                         
007500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W11428'.              
007600     SKIP2                                                                
007700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007800     SKIP2                                                                
007900*01  -COPY WDATKORT                                                       
008000     EJECT                                                                
008020*01  -COPY WDATAREA                                                       
008030     EJECT                                                                
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500 01  IN-AREA-START               PIC X(24)   VALUE                        
008600                                             'IN-AREA-START'.             
008700*01  AREA -COPY W11426     -PRE IN-                                       
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009000     SKIP3                                                                
009100 01  NYCKLAR-TILL-DLI.                                                    
009200     03  W-IDARTNR-X.                                                     
009300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009400     SKIP2                                                                
009500*    --- STATUS-KOD FRÅN IMS                                              
009600 01  STATUS-WS                   PIC XX.                                  
009700     88  SEGMENT-FINNS                       VALUE '  '.                  
009800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010100     88  IMS-EJ-OK                           VALUE 'XD'.                  
010200     SKIP2                                                                
010300 01  GODK-STATUSKODER.                                                    
010400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010500     SKIP3                                                                
010600 01  SSA1                        PIC X(64).                               
010700 01  SSA2                        PIC X(64).                               
010800     EJECT                                                                
010900*    --- IMS FUNKTIONSKODER                                               
011000*01  -COPY W0003                                                          
011100     EJECT                                                                
011200*    ---  DLI INPUT-OUTPUT AREA                                           
011300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
011400 01  DLI-IO-WDK601.                                                       
011500*    03  -COPY WDK601 -PRE ARTC-                                          
011600     EJECT                                                                
011700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
011800 01  DLI-IO-WDD201.                                                       
011900*    03  -COPY WDD201 -PRE ARTG-                                          
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*01  -COPY W0009   -PRE MSG-                                              
012400     EJECT                                                                
012500*01  -COPY W0008   -PRE WDD2-                                             
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800*01  -COPY W0008   -PRE WDK6-                                             
012900     05  FILLER                  PIC X.                                   
013000     EJECT                                                                
013100 PROCEDURE DIVISION  USING MSG-PCB WDD2-PCB WDK6-PCB.                     
013200 MAIN SECTION.                                                            
013300     ENTRY 'DLITCBL' USING MSG-PCB WDD2-PCB WDK6-PCB.                     
013400                                                                          
013500     PERFORM A-INIT                                                       
013600     PERFORM S01-LAES-W11427                                              
013700     PERFORM UNTIL END-OF-W11427                                          
013710        MOVE IN-IDARTNR TO W-IDARTNR                                      
013720        PERFORM IMS-GET-WDD201                                            
013730        IF SEGMENT-FINNS                                                  
013800          PERFORM B-KOLLA-UPPDATERA                                       
013900          IF CHKP-ANT > CHKP-MAX                                          
014000             PERFORM X-TAG-CHECKPOINT                                     
014100          END-IF                                                          
014130        END-IF                                                            
014200        PERFORM S01-LAES-W11427                                           
014300     END-PERFORM                                                          
014400                                                                          
014500     PERFORM Z-FINIT                                                      
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     PERFORM IMS-RESTART                                                  
015300     MOVE ZERO TO CHKP-ANT                                                
015400     OPEN INPUT W11427                                                    
015500                                                                          
015600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015700     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
015800     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
015900     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
016000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016100     .                                                                    
016200     EJECT                                                                
016300 B-KOLLA-UPPDATERA SECTION.                                               
016400                                                                          
017320     MOVE IN-IDARTNR TO W-IDARTNR                                         
017330     MOVE +1 TO IX                                                        
017340     PERFORM UNTIL IX > IX-MAX                                            
017350        MOVE IN-IDLEVNR(IX) TO WS-IDLEVNR                                 
017360        INSPECT WS-IDLEVNR REPLACING ALL SPACE BY ZERO                    
017370        IF WS-IDLEVNR NUMERIC                                             
017380        OR IN-IDLEVNR(IX) = SPACE                                         
017390           CONTINUE                                                       
017391        ELSE                                                              
017400           PERFORM IMS-GET-WDK601                                         
017500           IF SEGMENT-FINNS                                               
017600              IF ARTC-ART-IDLEVNR = IN-IDLEVNR(IX)                        
017700                 PERFORM BA-UPPDATERA-WDD2                                
017710                 MOVE IX-MAX TO IX                                        
017800              END-IF                                                      
019300           END-IF                                                         
019400        END-IF                                                            
019500        ADD +1 TO IX                                                      
019600     END-PERFORM                                                          
019700     .                                                                    
019800     EJECT                                                                
019900 BA-UPPDATERA-WDD2 SECTION.                                               
020000                                                                          
020100     MOVE NEJ TO SW-UPPDATERA                                             
020200                 SW-TPD                                                   
020300                 SW-UPG                                                   
020310                 SW-DATUM-OK                                              
020400                                                                          
020500     PERFORM IMS-GET-WDD201                                               
020600     IF SEGMENT-FINNS                                                     
020700        IF IN-IDSTEKN(IX) NOT = SPACE                                     
020800           IF IN-IDSTEKN(IX) NOT = ARTG-ART-IDSTEKN                       
020900              MOVE IN-IDSTEKN(IX) TO ARTG-ART-IDSTEKN                     
021000              MOVE JA TO SW-UPPDATERA                                     
021300           END-IF                                                         
021400        END-IF                                                            
021500                                                                          
021600        IF IN-DAAAVV-TPD(IX) NUMERIC                                      
021700        AND IN-DAAAVV-TPD(IX) NOT = ZERO                                  
021710           MOVE IN-DAAAVV-TPD(IX) TO WS-AAAAVV                            
021800           PERFORM S12-DATKONV                                            
021810           IF SW-DATUM-OK = JA                                            
021900              MOVE JA TO SW-TPD                                           
022000           ELSE                                                           
022010              MOVE NEJ TO SW-TPD                                          
022020           END-IF                                                         
022100        END-IF                                                            
022110                                                                          
022200        IF SW-TPD = JA                                                    
022300           IF IN-KDTPD(IX) = 'D' OR 'S' OR 'P' OR 'R' OR 'A'              
022400              IF IN-KDTPD(IX) NOT = ARTG-ART-KDTPD                        
022500                 MOVE IN-KDTPD(IX) TO ARTG-ART-KDTPD                      
022600                 MOVE DAT-TIAAMMDD TO ARTG-ART-TITPD                      
022700                 MOVE JA TO SW-UPPDATERA                                  
022710              END-IF                                                      
022900           END-IF                                                         
023200           MOVE NEJ TO SW-TPD                                             
023300        END-IF                                                            
023400                                                                          
023500        IF IN-DAAAVV-UPG(IX) NUMERIC                                      
023600           IF IN-DAAAVV-UPG(IX) = ZERO AND IN-FLUPG(IX) = 'N'             
023800              IF IN-FLUPG (IX) NOT = ARTG-ART-FLUPG                       
023900                 MOVE IN-FLUPG(IX) TO ARTG-ART-FLUPG                      
024000                 MOVE ZERO TO ARTG-ART-TIUPG                              
024100                 MOVE JA TO SW-UPPDATERA                                  
024300              END-IF                                                      
024400           ELSE                                                           
024500              IF IN-DAAAVV-UPG(IX) = ZERO                                 
024600                 MOVE NEJ TO SW-UPG                                       
024700              ELSE                                                        
024800                 MOVE IN-DAAAVV-UPG(IX) TO WS-AAAAVV                      
024810                 PERFORM S12-DATKONV                                      
024820                 IF SW-DATUM-OK = JA                                      
024830                   MOVE JA TO SW-UPG                                      
024840                 ELSE                                                     
024850                   MOVE NEJ TO SW-UPG                                     
024860                 END-IF                                                   
024900              END-IF                                                      
025000           END-IF                                                         
025100        END-IF                                                            
025200                                                                          
025300        IF SW-UPG = JA                                                    
025310           IF IN-FLUPG(IX) = SPACE AND ARTG-ART-FLUPG = SPACE             
025320              MOVE IN-FLUPG(IX) TO ARTG-ART-FLUPG                         
025330              MOVE DAT-TIAAMMDD TO ARTG-ART-TIUPG                         
025340              MOVE JA TO SW-UPPDATERA                                     
025400           ELSE                                                           
025500              IF IN-FLUPG(IX) = 'A' OR 'R' OR 'I' OR 'Z'                  
025510                 IF IN-FLUPG (IX) NOT = ARTG-ART-FLUPG                    
025520                    MOVE IN-FLUPG(IX) TO ARTG-ART-FLUPG                   
025521                    MOVE DAT-TIAAMMDD TO ARTG-ART-TIUPG                   
025540                    MOVE JA TO SW-UPPDATERA                               
026200                 END-IF                                                   
026300              END-IF                                                      
026310           END-IF                                                         
026400           MOVE NEJ TO SW-UPG                                             
026500        END-IF                                                            
026600                                                                          
026610        IF IN-DAAAVV-TPD(IX) NUMERIC                                      
026620        AND IN-DAAAVV-TPD(IX) = ZERO                                      
026630        AND IN-KDTPD(IX) = SPACE                                          
026631           IF ARTG-ART-KDTPD = SPACE                                      
026632           AND ARTG-ART-TITPD = ZERO                                      
026633             CONTINUE                                                     
026634           ELSE                                                           
026640              MOVE SPACE TO ARTG-ART-KDTPD                                
026650              MOVE ZERO TO ARTG-ART-TITPD                                 
026651              MOVE JA TO SW-UPPDATERA                                     
026654           END-IF                                                         
026660        END-IF                                                            
026661                                                                          
026670        IF IN-DAAAVV-UPG(IX) NUMERIC                                      
026680        AND IN-DAAAVV-UPG(IX) = ZERO                                      
026681        AND IN-FLUPG(IX) = SPACE                                          
026682            IF ARTG-ART-FLUPG = SPACE                                     
026683            AND ARTG-ART-TIUPG = ZERO                                     
026684               CONTINUE                                                   
026685            ELSE                                                          
026686               MOVE SPACE TO ARTG-ART-FLUPG                               
026687               MOVE ZERO TO ARTG-ART-TIUPG                                
026688               MOVE JA TO SW-UPPDATERA                                    
026691            END-IF                                                        
026692        END-IF                                                            
026693                                                                          
026700        IF SW-UPPDATERA = JA                                              
026800           PERFORM IMS-REPL-WDD201                                        
026900           ADD +1 TO CHKP-ANT                                             
027100        END-IF                                                            
027200                                                                          
027500     END-IF                                                               
027600     .                                                                    
027700     EJECT                                                                
027800 Z-FINIT SECTION.                                                         
027900                                                                          
028000     CLOSE W11427                                                         
028100     MOVE 'S' TO POSTSUM-OPKOD                                            
028200     CALL POSTSUM USING POSTSUM-PARM                                      
028300     .                                                                    
028400     EJECT                                                                
028500 S01-LAES-W11427  SECTION.                                                
028600                                                                          
028700     READ W11427 INTO IN-AREA                                             
028800     AT END                                                               
028900        SET END-OF-W11427 TO TRUE                                         
029000     NOT AT END                                                           
029100        MOVE 'W11427'   TO POSTSUM-FDNAMN                                 
029200        MOVE 'W11428D1' TO POSTSUM-DDNAMN2                                
029300        MOVE SPACE      TO POSTSUM-TRANSTYP                               
029400        CALL POSTSUM USING POSTSUM-PARM                                   
029500     END-READ                                                             
029600     .                                                                    
029700     EJECT                                                                
029710 S12-DATKONV SECTION.                                                     
029720                                                                          
029780     MOVE WS-AAR     TO NY-AAR                                            
029790     MOVE WS-VECKA   TO NY-VECKA                                          
029791     MOVE 1          TO NY-DAG                                            
029792     MOVE 'AAVVD  '  TO DAT-KDDATFORM                                     
029793     MOVE NY-AAVVD   TO DAT-I-TIDATUM                                     
029794     CALL WDATKONV USING DAT-KDDATFORM                                    
029795                            DAT-I-TIDATUM                                 
029796                            DAT-O-TIDATUM                                 
029797                            DAT-KDSVAR                                    
029798     IF DAT-KDSVAR-OK                                                     
029799        MOVE JA TO SW-DATUM-OK                                            
029800     ELSE                                                                 
029801        MOVE NEJ TO SW-DATUM-OK                                           
029802     END-IF                                                               
029803     .                                                                    
029804     EJECT                                                                
029810 X-TAG-CHECKPOINT   SECTION.                                              
029900                                                                          
030000     PERFORM IMS-CHECKPOINT                                               
030100     MOVE ZERO TO CHKP-ANT                                                
030200     .                                                                    
030300     EJECT                                                                
030400* --- IMS SEKTIONER ---                                                   
030500                                                                          
030600 IMS-GET-WDK601 SECTION.                                                  
030700                                                                          
030800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
030900          DELIMITED BY SIZE INTO SSA1                                     
031000     MOVE '  GE' TO GODK-STATUSKODER                                      
031100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
031200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031500     SKIP3                                                                
031600 IMS-GET-WDD201 SECTION.                                                  
031700                                                                          
031800     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
031900          DELIMITED BY SIZE INTO SSA1                                     
032000     MOVE '  GE' TO GODK-STATUSKODER                                      
032100     CALL CBLTDLI USING GHU WDD2-PCB DLI-IO-WDD201 SSA1                   
032200     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
032300     PERFORM IMS-STATUSKONTROLL                                           
032400     .                                                                    
032500     SKIP3                                                                
032600 IMS-REPL-WDD201 SECTION.                                                 
032700                                                                          
032800     MOVE '  ' TO GODK-STATUSKODER                                        
032900     CALL CBLTDLI USING REPL WDD2-PCB DLI-IO-WDD201                       
033000     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033200                                                                          
033300     MOVE 'W11428D2' TO POSTSUM-DDNAMN2                                   
033400     MOVE 'W11428'   TO POSTSUM-FDNAMN                                    
033500     .                                                                    
033600     EJECT                                                                
033700 IMS-RESTART SECTION.                                                     
033800                                                                          
033900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
034000     MOVE '  ' TO GODK-STATUSKODER                                        
034100     CALL CBLTDLI USING XRST MSG-PCB                                      
034200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
034300                        CHKP-AREA-LENGTH CHKP-AREA                        
034400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034500     PERFORM IMS-STATUSKONTROLL                                           
034600     .                                                                    
034700     SKIP3                                                                
034800 IMS-CHECKPOINT SECTION.                                                  
034900                                                                          
035000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
035100     MOVE '  XD' TO GODK-STATUSKODER                                      
035200     CALL CBLTDLI USING CHKP MSG-PCB                                      
035300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
035400                        CHKP-AREA-LENGTH CHKP-AREA                        
035500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035600     PERFORM IMS-STATUSKONTROLL                                           
035700                                                                          
035800     IF IMS-EJ-OK                                                         
035900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
036000       DISPLAY FELTEXT                                                    
036100       CALL FELLOG                                                        
036200     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500 IMS-STATUSKONTROLL SECTION.                                              
036600     SKIP2                                                                
036700     SET STATUS-IX TO 1                                                   
036800     SEARCH GODK-STATUS                                                   
036900       AT END                                                             
037000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
037100           DELIMITED BY SIZE INTO FELTEXT                                 
037200         DISPLAY FELTEXT                                                  
037300         CALL FELLOG                                                      
037400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037500         CONTINUE                                                         
037600     END-SEARCH                                                           
037700     .                                                                    
