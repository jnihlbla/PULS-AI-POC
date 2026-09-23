000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4182A00.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   07/04/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        STARTAS AV WEBBEN                                                
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDA4                                       
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
002400*          --- UTFIL                                                      
002500     SELECT W80301                     ASSIGN TO W4182AD2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W80301                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  POST -COPY W418R14 -PRE  UT-  -L.                                    
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W4182A00'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  INIT-SW                PIC X              VALUE ' '.                 
004300     88  INIT-OK                               VALUE 'J'.                 
004400     88  INIT-ERROR                            VALUE 'N'.                 
004500                                                                          
004600     EJECT                                                                
004700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004800 01  FILLER REDEFINES DAGENS-DATUM.                                       
004900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005200     EJECT                                                                
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400*                                                                         
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005900     SKIP2                                                                
006000*    --- PARAMETRAR TILL ABEND                                            
006100                                                                          
006200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006500     SKIP2                                                                
006600 01  FELTEXT.                                                             
006700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006900                                                                          
007000*    ******  NYCKLAR SOM LÄSES FRÅN SYSIN                                 
007100 01  PARM-SYSIN               PIC X(80).                                  
007200 01  PARM-IDDISTR             PIC X(5)  JUST RIGHT.                       
007300 01  PARM-IDRAPPNR            PIC X(7).                                   
007400                                                                          
007500                                                                          
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100 01  UT-AREA-START               PIC X(24)   VALUE                        
008200                                 'UT-AREA-START  '.                       
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W418R14     -PRE UT-                                      
008600     EJECT                                                                
008700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008800*                                                                         
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100     SKIP3                                                                
009200 01  NYCKLAR-TILL-DLI.                                                    
009300     03  W-WDA401-X.                                                      
009400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
009500         05  W-IDRAPPNR          PIC 9(7)    VALUE ZERO.                  
009600     03  W-IDARTNR-X.                                                     
009700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009800     SKIP2                                                                
009900*    --- STATUS-KOD FRÅN IMS                                              
010000 01  STATUS-WS                   PIC XX.                                  
010100     88  SEGMENT-FINNS                       VALUE '  '.                  
010200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010400     SKIP2                                                                
010500 01  GODK-STATUSKODER.                                                    
010600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010700     SKIP3                                                                
010800 01  SSA1                        PIC X(64).                               
010900 01  SSA2                        PIC X(64).                               
011000     EJECT                                                                
011100*    --- IMS FUNKTIONSKODER                                               
011200*01  -COPY W0003                                                          
011300     EJECT                                                                
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA401'.                      
011600 01  DLI-IO-WDA401.                                                       
011700*    03  -COPY WDA401                                                     
011800     EJECT                                                                
011900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA411'.                      
012000 01  DLI-IO-WDA411.                                                       
012100*    03  -COPY WDA411                                                     
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500                                                                          
012600*01  -COPY W0008  -PRE WDA4-                                              
012700     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012900 PROCEDURE DIVISION  USING WDA4-PCB.                                      
013000 MAIN SECTION.                                                            
013100     ENTRY 'DLITCBL' USING WDA4-PCB.                                      
013200                                                                          
013300                                                                          
013400     PERFORM A-INIT                                                       
013500     IF INIT-OK                                                           
013600       MOVE PARM-IDDISTR    TO W-IDDISTR                                  
013700       MOVE PARM-IDRAPPNR   TO W-IDRAPPNR                                 
013800       DISPLAY 'IDDISTR:' W-IDDISTR                                       
013900       DISPLAY 'IDRAPPN:' W-IDRAPPNR                                      
014000                                                                          
014100       PERFORM IMS-GET-WDA401                                             
014200       IF SEGMENT-FINNS                                                   
014300         PERFORM IMS-GET-WDA411                                           
014400         IF SEGMENT-FINNS                                                 
014500           DISPLAY 'FLMATCH:' BART-FLMATCH                                
014600           DISPLAY 'FLPRGRN:' BART-FLPRGRNS                               
014700                                                                          
014800           IF BART-FLMATCH = 'J' AND BART-FLPRGRNS = 'J'                  
014900             PERFORM UNTIL SEGMENT-SAKNAS OR BART-FLMATCH = 'N'           
015000                     OR BART-FLPRGRNS = 'N'                               
015100               PERFORM B-SKAPA-UTPOST                                     
015200               PERFORM IMS-GET-WDA411                                     
015300                                                                          
015400             END-PERFORM                                                  
015500           END-IF                                                         
015600         ELSE                                                             
015700           DISPLAY 'WDA411 SAKNAS'                                        
015800         END-IF                                                           
015900       ELSE                                                               
016000         DISPLAY ' WDA401 SAKNAS '                                        
016100       END-IF                                                             
016200     END-IF                                                               
016300                                                                          
016400     PERFORM Z-FINIT                                                      
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100     SET INIT-OK TO TRUE                                                  
017200     OPEN OUTPUT W80301                                                   
017300                                                                          
017400     ACCEPT DAGENS-DATUM  FROM DATE                                       
017500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017600                                                                          
017700     ACCEPT PARM-SYSIN  FROM SYSIN                                        
017800     UNSTRING PARM-SYSIN DELIMITED BY ','                                 
017900       INTO PARM-IDDISTR                                                  
018000            PARM-IDRAPPNR                                                 
018100                                                                          
018200     INSPECT PARM-IDDISTR REPLACING ALL SPACE BY ZERO                     
018300     IF PARM-IDDISTR NOT NUMERIC                                          
018400       STRING 'FELAKTIGT DISTRNR    ' PARM-IDDISTR                        
018500          DELIMITED BY SIZE  INTO FELTEXT-STR                             
018600       DISPLAY 'FEL:' FELTEXT-STR                                         
018700       SET INIT-ERROR TO TRUE                                             
018800     END-IF                                                               
018900     IF PARM-IDRAPPNR NOT NUMERIC                                         
019000       STRING 'FELAKTIGT RAPPORTNR  ' PARM-IDRAPPNR                       
019100          DELIMITED BY SIZE  INTO FELTEXT-STR                             
019200       DISPLAY 'FEL:' FELTEXT-STR                                         
019300       SET INIT-ERROR TO TRUE                                             
019400     END-IF                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 B-SKAPA-UTPOST SECTION.                                                  
019800*    OPEN OUTPUT W80301                                                   
019900     MOVE 'R14'               TO UT-IDPTYP                                
020000     MOVE BUYB-IDDISTR        TO UT-IDDISTR                               
020100     MOVE BUYB-IDKUNDNR       TO UT-IDKUNDNR                              
020200     MOVE 1                   TO UT-KDCLAGER                              
020300     MOVE BUYB-IDRAPPNR       TO UT-IDLEVANM                              
020400     MOVE 68                  TO UT-KDFRAKT                               
020500     MOVE 0                   TO UT-IDORDNR                               
020600     MOVE BART-IDARTNR        TO UT-IDARTNR                               
020700     MOVE 0                   TO UT-IDRADNR                               
020800     MOVE DAGENS-DATUM        TO UT-TIM-LEVANM                            
020900     MOVE 0                   TO UT-IDKOLLI                               
021000     MOVE 0                   TO UT-REKSIFFR                              
021100     MOVE BART-KVANTAL        TO UT-KVLEVANM                              
021200     MOVE 98                  TO UT-KDANMORS                              
021300     MOVE 0                   TO UT-KDEMBLEV                              
021400     IF BART-PRARTNTO-NEW > 0                                             
021500       MOVE BART-PRARTNTO-NEW TO UT-PRARTBTO                              
021600       MOVE 0                 TO UT-PRARTBTO-LOC                          
021700     ELSE                                                                 
021710       IF BART-PRARTNTO   > 0                                             
021720         MOVE BART-PRARTNTO   TO UT-PRARTBTO                              
021730         MOVE 0               TO UT-PRARTBTO-LOC                          
021740       END-IF                                                             
021750     END-IF                                                               
021800     IF BART-PRARTNTO-LOC > 0                                             
021900       MOVE BART-PRARTNTO-LOC TO UT-PRARTBTO-LOC                          
022000       MOVE 0                 TO UT-PRARTBTO                              
022100     END-IF                                                               
022200     MOVE ' '                 TO UT-KDFAKTYP                              
022300     MOVE 0                   TO UT-IDFAKT                                
022400     MOVE 0                   TO UT-TIFAKT                                
022500     MOVE 0                   TO UT-FLDIRLEV                              
022600     MOVE 0                   TO UT-KDSPEKTO                              
022700     MOVE 0                   TO UT-KDFTG                                 
022800     MOVE 0                   TO UT-IDKONTO-RAD                           
022900     MOVE 0                   TO UT-FLSTRET                               
023000     MOVE 0                   TO UT-FLSKROT                               
023100     MOVE 0                   TO UT-KDNIVAA4                              
023200     MOVE BART-KDVALISO       TO UT-KDVALISO                              
023300     PERFORM S11-SKRIV-W80301                                             
023400     .                                                                    
023500     EJECT                                                                
023600 Z-FINIT SECTION.                                                         
023700     CLOSE W80301                                                         
023800     SKIP2                                                                
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     .                                                                    
024200     EJECT                                                                
024300 S11-SKRIV-W80301 SECTION.                                                
024400                                                                          
024500     WRITE UT-POST FROM UT-AREA                                           
024600                                                                          
024700     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
024800     MOVE 'W80301' TO POSTSUM-FDNAMN                                      
024900     MOVE 'W4182AD2' TO POSTSUM-DDNAMN2                                   
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200     EJECT                                                                
025300 S99-ABEND SECTION.                                                       
025400                                                                          
025500     SKIP2                                                                
025600     MOVE 'S' TO POSTSUM-OPKOD                                            
025700     CALL POSTSUM USING POSTSUM-PARM                                      
025800     CALL ABEND USING RKOD-ABEND                                          
025900     .                                                                    
026000     EJECT                                                                
026100* --- IMS SEKTIONER ---                                                   
026200                                                                          
026300     EJECT                                                                
026400 IMS-GET-WDA401 SECTION.                                                  
026500*    DISPLAY 'IMS-GET-WDA401 SECTION'                                     
026600     STRING 'WDA401  (WDA401KY =' W-WDA401-X ')'                          
026700          DELIMITED BY SIZE INTO SSA1                                     
026800     MOVE '  GE' TO GODK-STATUSKODER                                      
026900     CALL CBLTDLI USING GHU WDA4-PCB DLI-IO-WDA401 SSA1                   
027000     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
027100     PERFORM IMS-STATUSKONTROLL                                           
027200     .                                                                    
027300     SKIP3                                                                
027400 IMS-GET-WDA411 SECTION.                                                  
027500*    DISPLAY 'IMS-GET-WDA411 SECTION'                                     
027600                                                                          
027700*    STRING 'WDA411  (IDARTNR  =' W-IDARTNR-X ')'                         
027800*         DELIMITED BY SIZE INTO SSA1                                     
027900     MOVE 'WDA411   '  TO SSA1                                            
028000     MOVE '  GE' TO GODK-STATUSKODER                                      
028100     CALL CBLTDLI USING GHNP WDA4-PCB DLI-IO-WDA411 SSA1                  
028200     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
028300     PERFORM IMS-STATUSKONTROLL                                           
028400     .                                                                    
028500     SKIP3                                                                
028600 IMS-STATUSKONTROLL SECTION.                                              
028700                                                                          
028800     SET STATUS-IX TO 1                                                   
028900     SEARCH GODK-STATUS                                                   
029000       AT END                                                             
029100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
029200           DELIMITED BY SIZE INTO FELTEXT                                 
029300         DISPLAY FELTEXT                                                  
029400         CALL FELLOG                                                      
029500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
029600         CONTINUE                                                         
029700     END-SEARCH                                                           
029800     .                                                                    
