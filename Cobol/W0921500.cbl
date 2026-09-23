000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0921500.                                                
000400 AUTHOR.         LARS THELL      (MG).                                    
000500 DATE-WRITTEN.   90/10/17.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER INFIL MED INFORMATION OM VILKA                  
001100*        TRANSAKTIONER SOM SKALL RENSAS PÅ WDG6-BASEN.                    
001200*                                                                         
001300*        SAKNAS SEGMENTET LÄSAR MAN NÄSTA POST PÅ INFILEN.                
001400*        DETTA FÖR ATT MAN LÄTT SKALL KUNNA ÅTERSTARTA MED                
001500*        MED SAMMA INFIL VID EN EVENTUELL ABEND.                          
001600*                                                                         
001700*        PROGRAMMET UPPATERAR WLZZAC (WDG6)                               
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- WDG6 TRANSAR SOM SKALL RENSAS                              
003200     SELECT W092X9                     ASSIGN TO W09215D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W092X9                                                               
003900     LABEL RECORD    STANDARD                                             
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300*01  -COPY W092X9          -L.                                            
004400                                                                          
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004701                                                                          
004710*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                       PIC X(8)    VALUE 'W0921500'.            
004900 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
005000 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
005100 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005200 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005300 77  CHKP-ANT                    PIC S9(3)   VALUE +0     COMP-3.         
005400 77  CHKP-MAX                    PIC S9(3)   VALUE +500   COMP-3.         
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700                                                                          
005800 77  W092X9-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W092X9                       VALUE 'J'.                   
006000     EJECT                                                                
006100 01  DAGENS-DATUM.                                                        
006200     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
006300     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
006400     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL POSTSUM                                          
007300*                                                                         
007400*01  -COPY W0005      -PRE  POSTSUM-                                      
007500     EJECT                                                                
007600 01  W092X9-AREA-START           PIC X(24)   VALUE                        
007700                                             'W092X9-AREA-START'.         
007800     SKIP2                                                                
007900                                                                          
008000*01  AREA -COPY W092X9     -PRE W092X9-                                   
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI.                                                    
008500     03  W-WDG6KEY-X.                                                     
008600         05  W-TIAAMMDD          PIC 9(6)    VALUE ZERO.                  
008700         05  W-TIKLOCK           PIC 9(8)    VALUE ZERO.                  
008800         05  W-IDLOGLOP          PIC 9(1)    VALUE ZERO.                  
008900         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
009000     SKIP2                                                                
009100*    --- STATUS-KOD FRÅN IMS                                              
009200 01  STATUS-WS                   PIC XX.                                  
009300     88  SEGMENT-FINNS                       VALUE '  '.                  
009400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009700     88  IMS-EJ-OK                           VALUE 'XD'.                  
009800     SKIP2                                                                
009900 01  GODK-STATUSKODER.                                                    
010000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010100     SKIP3                                                                
010200 01  SSA1                        PIC X(64).                               
010300 01  SSA2                        PIC X(64).                               
010400     EJECT                                                                
010500*    --- IMS FUNKTIONSKODER                                               
010600*01  -COPY W0003                                                          
010700     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREA                                           
010900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011000     SKIP3                                                                
011100 01  DLI-IO-AREA.                                                         
011200     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011300     SKIP3                                                                
011400     03  WLZZAC01 REDEFINES IO-AREA.                                      
011500*        05  -COPY WDG601     -PRE ZZAC-                                  
011600     EJECT                                                                
011700 LINKAGE SECTION.                                                         
011800                                                                          
011900*01  -COPY W0009      -PRE MSG-                                           
012000     EJECT                                                                
012100*01  -COPY W0008      -PRE ZZAC-                                          
012200     05  FILLER                  PIC X.                                   
012300     EJECT                                                                
012400 PROCEDURE DIVISION  USING MSG-PCB ZZAC-PCB.                              
012500     ENTRY 'DLITCBL' USING MSG-PCB ZZAC-PCB.                              
012600                                                                          
012700     PERFORM A-INIT                                                       
012800     PERFORM S01-LAES-W092X9                                              
012900                                                                          
013000     PERFORM UNTIL END-OF-W092X9                                          
013100       IF CHKP-ANT             > CHKP-MAX                                 
013200         PERFORM X-TAG-CHECKPOINT                                         
013300       END-IF                                                             
013400       PERFORM B-BEHANDLA-INPOST                                          
013500                                                                          
013600       PERFORM S01-LAES-W092X9                                            
013700     END-PERFORM                                                          
013800                                                                          
013900     PERFORM Z-FINIT                                                      
014000                                                                          
014100     MOVE ZERO                 TO RETURN-CODE                             
014200     GOBACK                                                               
014300     .                                                                    
014400     EJECT                                                                
014500 A-INIT SECTION.                                                          
014600                                                                          
014700     PERFORM IMS-RESTART                                                  
014800                                                                          
014900     OPEN INPUT W092X9                                                    
015000                                                                          
015100     ACCEPT DAGENS-DATUM       FROM DATE                                  
015200                                                                          
015300     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
015400     MOVE +0                   TO  CHKP-ANT                               
015500     .                                                                    
015600     EJECT                                                                
015700                                                                          
015800 B-BEHANDLA-INPOST      SECTION.                                          
015900                                                                          
016000     MOVE W092X9-TIAAMMDD      TO  W-TIAAMMDD                             
016100     MOVE W092X9-TIKLOCK       TO  W-TIKLOCK                              
016200     MOVE W092X9-IDLOGLOP      TO  W-IDLOGLOP                             
016300     MOVE W092X9-IDPTYP        TO  W-IDPTYP                               
016400                                                                          
016500     PERFORM IMS-GET-ZZAC01                                               
016600     IF SEGMENT-FINNS                                                     
016700         ADD +1                TO  CHKP-ANT                               
016800         PERFORM IMS-DLET-ZZAC01                                          
016900     END-IF                                                               
017000                                                                          
017100     .                                                                    
017200     EJECT                                                                
017300 Z-FINIT SECTION.                                                         
017400                                                                          
017500     CLOSE W092X9                                                         
017600                                                                          
017700     MOVE 'S'                  TO POSTSUM-OPKOD                           
017800     CALL POSTSUM              USING POSTSUM-PARM                         
017900     .                                                                    
018000     EJECT                                                                
018100 S01-LAES-W092X9  SECTION.                                                
018200                                                                          
018300     READ W092X9 INTO W092X9-AREA                                         
018400     AT END                                                               
018500        SET END-OF-W092X9      TO TRUE                                    
018600                                                                          
018700     NOT AT END                                                           
018800        MOVE 'W092X9'          TO POSTSUM-FDNAMN                          
018900        MOVE 'W09215D1'        TO POSTSUM-DDNAMN2                         
019000        MOVE W092X9-IDPTYP     TO POSTSUM-TRANSTYP                        
019100        CALL POSTSUM           USING POSTSUM-PARM                         
019200                                                                          
019300*       ADD 1                  TO W-W092X9-KVPOST-IN                      
019400     END-READ                                                             
019500     .                                                                    
019600     EJECT                                                                
019700 X-TAG-CHECKPOINT   SECTION.                                              
019800                                                                          
019900     PERFORM IMS-CHECKPOINT                                               
020000     MOVE +0                   TO  CHKP-ANT                               
020100     .                                                                    
020200     EJECT                                                                
020300* --- IMS SEKTIONER ---                                                   
020400     SKIP3                                                                
020500 IMS-GET-ZZAC01 SECTION.                                                  
020600     STRING 'WLZZAC01(WDG6KEY  =' W-WDG6KEY-X ')'                         
020700          DELIMITED BY SIZE INTO SSA1                                     
020800     MOVE '  GE'               TO GODK-STATUSKODER                        
020900     CALL CBLTDLI USING GHU ZZAC-PCB DLI-IO-AREA SSA1                     
021000     MOVE ZZAC-STATUS-CODE     TO STATUS-WS                               
021100     PERFORM IMS-STATUS-KONTROLL                                          
021200     .                                                                    
021300     SKIP3                                                                
021400 IMS-DLET-ZZAC01 SECTION.                                                 
021500                                                                          
021600     MOVE '  '                 TO GODK-STATUSKODER                        
021700     CALL CBLTDLI USING DLET ZZAC-PCB DLI-IO-AREA                         
021800     MOVE ZZAC-STATUS-CODE     TO STATUS-WS                               
021900     PERFORM IMS-STATUS-KONTROLL                                          
022000     .                                                                    
022100     EJECT                                                                
022200 IMS-RESTART SECTION.                                                     
022300     SKIP2                                                                
022400     MOVE SPACE                TO MSG-IO-AREA                             
022500     MOVE '  '                 TO GODK-STATUSKODER                        
022600     CALL CBLTDLI USING        XRST MSG-PCB                               
022700                               MSG-IO-AREA-LENGTH MSG-IO-AREA             
022800                               CHKP-AREA-LENGTH CHKP-AREA                 
022900     MOVE MSG-STATUS-CODE      TO STATUS-WS                               
023000     PERFORM IMS-STATUS-KONTROLL                                          
023100     .                                                                    
023200     EJECT                                                                
023300 IMS-CHECKPOINT SECTION.                                                  
023400     SKIP2                                                                
023500     MOVE SPACE TO MSG-IO-AREA                                            
023600     MOVE '  XD' TO GODK-STATUSKODER                                      
023700     CALL CBLTDLI USING CHKP MSG-PCB                                      
023800                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
023900                        CHKP-AREA-LENGTH CHKP-AREA                        
024000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024100     PERFORM IMS-STATUS-KONTROLL                                          
024200                                                                          
024300     IF IMS-EJ-OK                                                         
024400       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
024500       CALL FELLOG                                                        
024600     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 IMS-STATUS-KONTROLL SECTION.                                             
025000     SKIP2                                                                
025100     SET STATUS-IX TO 1                                                   
025200     SEARCH GODK-STATUS                                                   
025300       AT END CALL FELLOG                                                 
025400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
025500     END-SEARCH                                                           
025600     .                                                                    
