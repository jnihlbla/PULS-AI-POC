000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1146600.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   05/12/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER ÄNDRADE INKÖPARE FRÅN SI+                                  
001000*        UPPDAT WDK6, WDD2                                                
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDK6                                       
001300*        PROGRAMMET UPPDATERAR WDD2                                       
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- FIL FRÅN SI PLUS MED IDINK                                 
002400     SELECT W11465                     ASSIGN TO W11466D1.                
002500     SKIP2                                                                
002600*          --- REGISTER EJ UPPNÅDDA IDINK                                 
002700     SELECT W11467                     ASSIGN TO W11466D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W11465                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY PI75R7H1      -L.                                              
003800     SKIP3                                                                
003900 FD  W11467                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  POST -COPY PI75R7H1 -PRE  UTREG-  -L.                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W1146600'.            
004800 01  CHKP-VAR.                                                            
004900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005400     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700     SKIP2                                                                
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006100                                                                          
006200 77  W11465-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W11465                       VALUE 'J'.                   
006400                                                                          
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100 01  DADATUM.                                                             
007200     03  FILLER                  PIC 9(2)    VALUE 20.                    
007300     03  DADATUM-AAMMDD          PIC 9(6).                                
007400     SKIP3                                                                
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     SKIP3                                                                
008100 01  ARBAREOR.                                                            
008200     03  WS-IDPITEM.                                                      
008300         05  FILLER              PIC X(11)   VALUE SPACE.                 
008400         05  WS-IDARTNR-X        PIC X(09)   VALUE SPACE.                 
008500     03  FILLER   REDEFINES  WS-IDPITEM.                                  
008600         05  FILLER              PIC X(11).                               
008700         05  WS-IDARTNR          PIC 9(09).                               
008710                                                                          
008720     03  WS-IDHANDLR             PIC 9(4)    VALUE ZERO.                  
008730     03  WS-IDINK                PIC X(4)    VALUE SPACE.                 
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL POSTSUM                                          
009000*                                                                         
009100*01  -COPY W0005   -PRE  POSTSUM-                                         
009200     EJECT                                                                
009300 01  IN-AREA-START               PIC X(24)   VALUE                        
009400                                             'IN-AREA-START'.             
009500     SKIP2                                                                
009600                                                                          
009700*01  AREA -COPY PI75R7H1     -PRE IN-                                     
009800     EJECT                                                                
009900 01  UTREG-AREA-START            PIC X(24)   VALUE                        
010000                                             'UTREG-AREA-START'.          
010100     SKIP2                                                                
010200                                                                          
010300*01  AREA -COPY PI75R7H1     -PRE UTREG-                                  
010400*                                                                         
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700     SKIP3                                                                
010800 01  NYCKLAR-TILL-DLI.                                                    
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011100     03  W-KDSEGKEY-X.                                                    
011200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011300     SKIP2                                                                
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012000     88  IMS-EJ-OK                           VALUE 'XD'.                  
012100     SKIP2                                                                
012200 01  GODK-STATUSKODER.                                                    
012300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400     SKIP3                                                                
012500 01  SSA1                        PIC X(64).                               
012600 01  SSA2                        PIC X(64).                               
012700     EJECT                                                                
012800*    --- IMS FUNKTIONSKODER                                               
012900*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200                                                                          
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
013400 01  DLI-IO-WDK601.                                                       
013500*    03  -COPY WDK601                                                     
013600     EJECT                                                                
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
013800 01  DLI-IO-WDK611.                                                       
013900*    03  -COPY WDK611                                                     
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
014100 01  DLI-IO-WDD201.                                                       
014200*    03  -COPY WDD201                                                     
014300                                                                          
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700*01  -COPY W0009   -PRE MSG-                                              
014800                                                                          
014900*01  -COPY W0008  -PRE WDK6-                                              
015000     05  FILLER                  PIC X.                                   
015100                                                                          
015200*01  -COPY W0008  -PRE WDD2-                                              
015300     05  FILLER                  PIC X.                                   
015400     EJECT                                                                
015500 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDD2-PCB.                     
015600 MAIN SECTION.                                                            
015700     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDD2-PCB.                     
015800                                                                          
015900     SKIP2                                                                
016000     PERFORM A-INIT                                                       
016100     PERFORM S01-LAES-W11465                                              
016200     PERFORM UNTIL END-OF-W11465                                          
016300       IF CHKP-ANT > CHKP-MAX                                             
016400         PERFORM X-TAG-CHECKPOINT                                         
016500       END-IF                                                             
016600       IF DADATUM < IN-TICHANGE-YYMMDD                                    
016700**        SKRIV REGUT                                                     
016800          MOVE IN-AREA TO UTREG-AREA                                      
016900          PERFORM S11-SKRIV-W11467                                        
017000       ELSE                                                               
017010**        FIXA IDARTNR                                                    
017100          MOVE IN-IDPITEM TO WS-IDPITEM                                   
017200          INSPECT WS-IDARTNR-X REPLACING LEADING SPACE BY ZERO            
017210          MOVE WS-IDARTNR TO W-IDARTNR                                    
017220**        FIXA IDINK                                                      
017310          MOVE IN-IDHANDLR               TO WS-IDHANDLR                   
017320          IF WS-IDHANDLR (2:1) > ZERO                                     
017321             MOVE WS-IDHANDLR (2:3)      TO WS-IDINK                      
017322          ELSE                                                            
017323             IF WS-IDHANDLR (3:1) > ZERO                                  
017324                MOVE WS-IDHANDLR (3:2)   TO WS-IDINK                      
017325             ELSE                                                         
017326                IF WS-IDHANDLR (4:1) > ZERO                               
017327                   MOVE WS-IDHANDLR (4:1) TO WS-IDINK                     
017328                ELSE                                                      
017329                   MOVE SPACE            TO WS-IDINK                      
017330                END-IF                                                    
017331             END-IF                                                       
017332          END-IF                                                          
017340**        UPPDATERA IDINK PÅ NYPON OCH ARTIKELREGISTRET                   
017400          PERFORM B-UPPDAT-WDD2                                           
017500          PERFORM C-UPPDAT-WDK6                                           
017600       END-IF                                                             
017700       PERFORM S01-LAES-W11465                                            
017800     END-PERFORM                                                          
017900                                                                          
018000                                                                          
018100     PERFORM Z-FINIT                                                      
018200                                                                          
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 A-INIT SECTION.                                                          
018800     SKIP2                                                                
018900                                                                          
019000     PERFORM IMS-RESTART                                                  
019100                                                                          
019200     OPEN INPUT  W11465                                                   
019300                                                                          
019400     OPEN OUTPUT W11467                                                   
019500                                                                          
019600     ACCEPT DAGENS-DATUM  FROM DATE                                       
019700     MOVE DAGENS-DATUM TO DADATUM-AAMMDD                                  
019800                                                                          
019900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020000     .                                                                    
020100     EJECT                                                                
020200 B-UPPDAT-WDD2 SECTION.                                                   
020300     SKIP2                                                                
020400     PERFORM IMS-GET-WDD201                                               
020410     IF SEGMENT-FINNS                                                     
020430        MOVE WS-IDINK TO ART-IDINK                                        
020440        PERFORM IMS-REPL-WDD201                                           
020450     END-IF                                                               
020500     .                                                                    
020600     EJECT                                                                
020700 C-UPPDAT-WDK6 SECTION.                                                   
020800     SKIP2                                                                
020810     PERFORM IMS-GET-WDK611                                               
020820     IF SEGMENT-FINNS                                                     
020830        MOVE WS-IDINK TO CLAG-IDINK                                       
020840        PERFORM IMS-REPL-WDK611                                           
020850     END-IF                                                               
020900                                                                          
021000     .                                                                    
021100     EJECT                                                                
021200 Z-FINIT SECTION.                                                         
021300                                                                          
021400     CLOSE W11465                                                         
021500                                                                          
021600           W11467                                                         
021700     SKIP2                                                                
021800     MOVE 'S' TO POSTSUM-OPKOD                                            
021900     CALL POSTSUM USING POSTSUM-PARM                                      
022000     .                                                                    
022100     EJECT                                                                
022200 S01-LAES-W11465  SECTION.                                                
022300     SKIP2                                                                
022400     READ W11465 INTO IN-AREA                                             
022500     AT END                                                               
022600        SET END-OF-W11465 TO TRUE                                         
022700                                                                          
022800     NOT AT END                                                           
022900        MOVE 'W11465'   TO POSTSUM-FDNAMN                                 
023000        MOVE 'W11466D1' TO POSTSUM-DDNAMN2                                
023100        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
023200        CALL POSTSUM USING POSTSUM-PARM                                   
023300                                                                          
023500     END-READ                                                             
023600     .                                                                    
023700     EJECT                                                                
023800 S11-SKRIV-W11467 SECTION.                                                
023900     SKIP2                                                                
024000     WRITE UTREG-POST FROM UTREG-AREA                                     
024100                                                                          
024200     MOVE 'REG'      TO POSTSUM-TRANSTYP                                  
024300     MOVE 'W11467 '  TO POSTSUM-FDNAMN                                    
024400     MOVE 'W11466D2' TO POSTSUM-DDNAMN2                                   
024500     CALL POSTSUM USING POSTSUM-PARM                                      
024600     .                                                                    
024700     EJECT                                                                
024800 X-TAG-CHECKPOINT   SECTION.                                              
024900                                                                          
025000* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
025100* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
025200     PERFORM IMS-CHECKPOINT                                               
025300     MOVE ZERO TO CHKP-ANT                                                
025400* --- LÄS OM DATABAS OM DET BEHÖVS                                        
025500     .                                                                    
025900     EJECT                                                                
025910* --- IMS SEKTIONER ---                                                   
025920                                                                          
026000 IMS-GET-WDK611 SECTION.                                                  
026100                                                                          
026200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
026300          DELIMITED BY SIZE INTO SSA1                                     
026400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
026500          DELIMITED BY SIZE INTO SSA2                                     
026600     MOVE '  GE' TO GODK-STATUSKODER                                      
026700     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
026800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100     SKIP3                                                                
027200 IMS-REPL-WDK611 SECTION.                                                 
027300                                                                          
027400     MOVE '  ' TO GODK-STATUSKODER                                        
027500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
027600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027700     PERFORM IMS-STATUSKONTROLL                                           
027710     ADD +1 TO CHKP-ANT                                                   
027800     .                                                                    
027900     EJECT                                                                
028000 IMS-GET-WDD201 SECTION.                                                  
028100                                                                          
028200     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
028300          DELIMITED BY SIZE INTO SSA1                                     
028400     MOVE '  GE' TO GODK-STATUSKODER                                      
028500     CALL CBLTDLI USING GHU WDD2-PCB DLI-IO-WDD201 SSA1                   
028600     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
028700     PERFORM IMS-STATUSKONTROLL                                           
028800     .                                                                    
028900     SKIP3                                                                
029000 IMS-REPL-WDD201 SECTION.                                                 
029100                                                                          
029200     MOVE '  ' TO GODK-STATUSKODER                                        
029300     CALL CBLTDLI USING REPL WDD2-PCB DLI-IO-WDD201                       
029400     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
029500     PERFORM IMS-STATUSKONTROLL                                           
029510     ADD +1 TO CHKP-ANT                                                   
029600     .                                                                    
029700     EJECT                                                                
029800 IMS-RESTART SECTION.                                                     
029900     SKIP2                                                                
030000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
030100     MOVE '  ' TO GODK-STATUSKODER                                        
030200     CALL CBLTDLI USING XRST MSG-PCB                                      
030300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
030400                        CHKP-AREA-LENGTH CHKP-AREA                        
030500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030600     PERFORM IMS-STATUSKONTROLL                                           
030700     .                                                                    
030800     SKIP3                                                                
030900 IMS-CHECKPOINT SECTION.                                                  
031000     SKIP2                                                                
031100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031200     MOVE '  XD' TO GODK-STATUSKODER                                      
031300     CALL CBLTDLI USING CHKP MSG-PCB                                      
031400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031500                        CHKP-AREA-LENGTH CHKP-AREA                        
031600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031700     PERFORM IMS-STATUSKONTROLL                                           
031800                                                                          
031900     IF IMS-EJ-OK                                                         
032000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
032100       DISPLAY FELTEXT                                                    
032200       CALL FELLOG                                                        
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 IMS-STATUSKONTROLL SECTION.                                              
032700     SKIP2                                                                
032800     SET STATUS-IX TO 1                                                   
032900     SEARCH GODK-STATUS                                                   
033000       AT END                                                             
033100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033200           DELIMITED BY SIZE INTO FELTEXT                                 
033300         DISPLAY FELTEXT                                                  
033400         CALL FELLOG                                                      
033500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033600         CONTINUE                                                         
033700     END-SEARCH                                                           
033800     .                                                                    
