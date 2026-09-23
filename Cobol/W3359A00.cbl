000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3359A00.                                                
000300 AUTHOR.         STINA MOGREN.                                            
000400 DATE-WRITTEN.   03/12/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*      - KOPIERAR PRISFIL, KOMPL. MED PROJEKT OSV                         
000900*                                                                         
001000*                                                                         
001100*    ÄNDRING:                                                             
001200*        FÖR KOMPLETTERING AV UTFILEN W3359A MED 'KDFAM'.                 
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*       --- INFIL                                                         
002300     SELECT W33597                     ASSIGN TO W3359AD1.                
002400*       --- UTFIL                                                         
002500     SELECT W3359A                     ASSIGN TO W3359AD2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W33597                                                               
003200     RECORDING V                                                          
003300     BLOCK CONTAINS 0.                                                    
003400                                                                          
003500*01  -COPY  W33597A       -L.                                             
003600*01  -COPY  W33597        -L.                                             
003700                                                                          
003800 FD  W3359A                                                               
003900     RECORDING       V                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W33597A  -PRE  UTA-  -L.                                  
004300*01  POST -COPY W3359A   -PRE  UT-  -L.                                   
004400     SKIP2                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)      VALUE 'W3359A00'.          
004800 77  JA                          PIC X         VALUE 'J'.                 
004900 77  NEJ                         PIC X         VALUE 'N'.                 
005000 77  IX                          PIC S9(9)   VALUE ZERO COMP-3.           
005100                                                                          
005200 01  W-TIURPROD                  PIC 9(5)      VALUE ZERO.                
005300                                                                          
005400 77  W33597-EOF-SW               PIC X         VALUE 'N'.                 
005500     88  END-OF-W33597                         VALUE 'J'.                 
005600                                                                          
005700 01  FILLER                      PIC X(16) VALUE 'WS-SEKTION'.            
005800 01  WS-SEKTION                  PIC X(40)     VALUE SPACE.               
005900                                                                          
006000 01  DAGENS-DATUM                PIC 9(8)      VALUE ZERO.                
006100     EJECT                                                                
006200 01  WS-AAAAMMDD.                                                         
006300     03  WS-SEKEL                       PIC 9(2).                         
006400     03  WS-AAMMDD                      PIC 9(6).                         
006500 01  WS-TIFINLV REDEFINES WS-AAAAMMDD   PIC 9(8).                         
006600*                                                                         
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     SKIP2                                                                
007200                                                                          
007300 01  FELTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100*01  -COPY WWPRODSL                                                       
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
008400*01 -COPY WDATAREA                                                        
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16) VALUE 'BYTES   '.              
008700*01 -COPY WWBYT01                                                         
008800     EJECT                                                                
008900                                                                          
009000 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
009100 01  IN-AREA                     PIC X(100).                              
009200*01  FILLER -COPY W33597A      -PRE INA-  -RED  IN-AREA                   
009300*01  FILLER -COPY W33597       -PRE IN-   -RED  IN-AREA                   
009400     EJECT                                                                
009500                                                                          
009600                                                                          
009700 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
009800 01  UT-AREA.                                                             
009900     03  FILLER                  PIC X(100).                              
010000*01  FILLER -COPY W33597A      -PRE UTA-  -RED  UT-AREA                   
010100*01  FILLER -COPY W3359A       -PRE UT-   -RED  UT-AREA                   
010200     EJECT                                                                
010300                                                                          
010400*    --- ARBETS-AREOR TILL  IMS-SEKTIONERNA                               
010500 01  FILLER                      PIC X(24)   VALUE 'IMS-WS '.             
010600 01  NYCKLAR-TILL-DLI.                                                    
010700     03   W-IDARTNR-X.                                                    
010800         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
010900                                                                          
011000*    --- STATUS-KOD FRÅN IMS                                              
011100 01  STATUS-WS                   PIC XX.                                  
011200     88  SEGMENT-FINNS                       VALUE '  '.                  
011300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011500     88  BASEN-SLUT                          VALUE 'GB'.                  
011600     SKIP2                                                                
011700 01  GODK-STATUSKODER.                                                    
011800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011900     SKIP3                                                                
012000 01  SSA1                        PIC X(64).                               
012100 01  SSA2                        PIC X(64).                               
012200     EJECT                                                                
012300*    --- IMS FUNKTIONSKODER                                               
012400*01  -COPY W0003                                                          
012500     EJECT                                                                
012600 01  FILLER                  PIC X(16)   VALUE 'WDK601-11    '.           
012700 01  DLI-IO-WDK601-11.                                                    
012800     03  DLI-IO-WDK601.                                                   
012900*        05 -COPY WDK601  -PRE K601-                                      
013000     03  DLI-IO-WDK611.                                                   
013100*        05 -COPY WDK611                                                  
013200     EJECT                                                                
013300 01  DLI-IO-WDD201.                                                       
013400*    03  WDD201    -COPY WDD201                                           
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700*01  -COPY W0008      -PRE WDK6-                                          
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000*01  -COPY W0008      -PRE WDD2-                                          
014100     05  FILLER                  PIC X.                                   
014200     EJECT                                                                
014300 PROCEDURE DIVISION   USING  WDK6-PCB WDD2-PCB.                           
014400                                                                          
014500 MAIN SECTION.                                                            
014600     ENTRY 'DLITCBL'  USING  WDK6-PCB WDD2-PCB.                           
014700                                                                          
014800     PERFORM A-INIT                                                       
014900                                                                          
015000     PERFORM S01-LAES-W33597                                              
015100     MOVE INA-DAAAMM         TO UTA-DAAAMM                                
015200     WRITE UTA-POST          FROM UT-AREA                                 
015300                                                                          
015400     MOVE 'W3359A'          TO POSTSUM-FDNAMN                             
015500     MOVE 'W3359AD2'        TO POSTSUM-DDNAMN2                            
015600     CALL POSTSUM           USING POSTSUM-PARM                            
015700                                                                          
015800     PERFORM S01-LAES-W33597                                              
015900                                                                          
016000     PERFORM UNTIL END-OF-W33597                                          
016100         PERFORM B-FLYTTA-KOMPL-SKRIV-UTPOST                              
016200         PERFORM S01-LAES-W33597                                          
016300     END-PERFORM                                                          
016400                                                                          
016500     PERFORM Z-FINIT                                                      
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     OPEN INPUT  W33597                                                   
017400          OUTPUT W3359A                                                   
017500                                                                          
017600                                                                          
017700     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
017800                                                                          
017900     .                                                                    
018000     EJECT                                                                
018100 B-FLYTTA-KOMPL-SKRIV-UTPOST SECTION.                                     
018200                                                                          
018300     MOVE IN-AREA          TO UT-AREA                                     
018400                                                                          
018500     PERFORM BA-HAMTA-KOMPLETTERA-INFO                                    
018600                                                                          
018700     MOVE IN-KDPRODSL       TO  TEST-KDPRODSL                             
018800*    IF KDPRODSL-LYNK                                                     
018900*      CONTINUE                                                           
019000*    ELSE                                                                 
019100       PERFORM S11-SKRIV-W3359A                                           
019200*    END-IF                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 BA-HAMTA-KOMPLETTERA-INFO  SECTION.                                      
019600     SKIP2                                                                
019700                                                                          
019800* WDK6-INFO                                                               
019900     MOVE UT-IDARTNR        TO  W-IDARTNR                                 
020000                                                                          
020100     PERFORM IMS-GU-WDK611-PATH                                           
020200                                                                          
020300     IF SEGMENT-FINNS                                                     
020400       MOVE CLAG-IDPROJUP   TO  UT-IDPROJUP                               
020500       MOVE K601-ART-TIURPROD   TO  W-TIURPROD                            
020600       MOVE W-TIURPROD(2:4) TO  UT-TIURPROD                               
020700     ELSE                                                                 
020800       MOVE SPACE           TO  UT-IDPROJUP                               
020900       MOVE ZERO            TO  UT-TIURPROD                               
021000     END-IF                                                               
021100                                                                          
021200     MOVE SPACE             TO  UT-FLBYTES                                
021300                                                                          
021400* WDD2-INFO                                                               
021500     PERFORM IMS-GU-WDD201                                                
021600     IF SEGMENT-FINNS                                                     
021700       MOVE ART-IDPROJK     TO  UT-IDPROJK                                
021800**?*   MOVE ART-FLBYTES     TO  UT-FLBYTES                                
021900     ELSE                                                                 
022000       MOVE SPACE           TO  UT-IDPROJK                                
022100     END-IF                                                               
022200                                                                          
022300     MOVE W-IDARTNR         TO  BYT01-IDARTNR                             
022400     IF BYT01-BYTES                                                       
022500       MOVE 'J'             TO  UT-FLBYTES                                
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 Z-FINIT SECTION.                                                         
023000                                                                          
023100                                                                          
023200     CLOSE W33597                                                         
023300           W3359A                                                         
023400                                                                          
023500     MOVE 'S'           TO POSTSUM-OPKOD                                  
023600     CALL POSTSUM    USING POSTSUM-PARM                                   
023700     .                                                                    
023800     EJECT                                                                
023900 S01-LAES-W33597  SECTION.                                                
024000                                                                          
024100     READ W33597 INTO IN-AREA                                             
024200     AT END                                                               
024300        SET END-OF-W33597 TO TRUE                                         
024400     NOT AT END                                                           
024500        MOVE 'W33597'   TO POSTSUM-FDNAMN                                 
024600        MOVE 'W3359AD1' TO POSTSUM-DDNAMN2                                
024700        CALL POSTSUM USING POSTSUM-PARM                                   
024800     END-READ                                                             
024900     .                                                                    
025000     EJECT                                                                
025100 S11-SKRIV-W3359A SECTION.                                                
025200     WRITE UT-POST          FROM UT-AREA                                  
025300                                                                          
025400     MOVE 'W3359A'          TO POSTSUM-FDNAMN                             
025500     MOVE 'W3359AD2'        TO POSTSUM-DDNAMN2                            
025600     CALL POSTSUM           USING POSTSUM-PARM                            
025700     .                                                                    
025800     EJECT                                                                
025900********************************                                          
026000 IMS-GU-WDK611-PATH SECTION.                                              
026100     MOVE 'IMS-GU-WDK611'     TO WS-SEKTION                               
026200                                                                          
026300     STRING 'WDK601  *D(IDARTNR  =' W-IDARTNR-X ')'                       
026400          DELIMITED BY SIZE INTO SSA1                                     
026500     MOVE 'WDK611'       TO SSA2                                          
026600     MOVE '  GE' TO GODK-STATUSKODER                                      
026700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601-11 SSA1 SSA2            
026800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100     EJECT                                                                
027200 IMS-GU-WDD201 SECTION.                                                   
027300     SKIP2                                                                
027400     MOVE 'IMS-GU-WDD201'     TO WS-SEKTION                               
027500                                                                          
027600     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
027700          DELIMITED BY SIZE INTO SSA1                                     
027800     MOVE '  GE' TO GODK-STATUSKODER                                      
027900     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-WDD201 SSA1                    
028000     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
028100     PERFORM IMS-STATUSKONTROLL                                           
028200     .                                                                    
028300     EJECT                                                                
028400                                                                          
028500 IMS-STATUSKONTROLL SECTION.                                              
028600                                                                          
028700     SET STATUS-IX TO 1                                                   
028800     SEARCH GODK-STATUS                                                   
028900       AT END CALL FELLOG                                                 
029000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
029100     END-SEARCH                                                           
029200     .                                                                    
029300     EJECT                                                                
