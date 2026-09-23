000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1117N00.                                                
000400*AUTHOR.         CONNY EGHOLT.                                            
000500*DATE-WRITTEN.   DECEMBER 2001.                                           
000600*                                                                         
000700*        REMARKS.                                                         
000800*            -  PGM SKAPAR EN OPACKAD FIL TILL NEVIS BASELINE             
000900*               AV W11170, SOM NYSS SKAPATS I W1117000.                   
001000*               TAR ENDAST MED SVE-BEN OCH KOMPLETTERAR MED BENNR         
001100*                                                                         
001200*        FUNKTION:                                                        
001300*            -  LÄSER  FIL W11170                                         
001400*            -  SKAPAR FIL W1117N.                                        
001500*                                                                         
001600*            PROGRAMMET LÄSER       WDD301 MED BSEQ                       
001601*                                                                         
001610*        ÄNDRINGAR:                                                       
001620*            -  IDBENNR MÅSTE FÖRLÄNGAS TILL 7 BYTE                       
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100                                                                          
002200     SELECT  W11170   ASSIGN TO W1117ND1.                                 
002300     SELECT  W1117N   ASSIGN TO W1117ND2.                                 
002400                                                                          
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700                                                                          
002800 FILE SECTION.                                                            
002900                                                                          
003000 FD  W11170                                                               
003100     RECORDING       V                                                    
003200     BLOCK CONTAINS  0.                                                   
003300     SKIP2                                                                
003400*01  -COPY W111702A      -L.                                              
003500*01  -COPY W111701A      -L.                                              
003600     SKIP3                                                                
003700 FD  W1117N                                                               
003800     RECORDING V                                                          
003900     BLOCK CONTAINS 0 RECORDS.                                            
004000     SKIP3                                                                
004100*01  POST   -COPY W1117N1A   -PRE UTS01-    -L                            
004200     SKIP3                                                                
004300*01  POST   -COPY W1117N2A   -PRE UTS2-     -L                            
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600*                                                                         
004700 77  IDPGM                  PIC X(8)   VALUE 'W1117N00'.                  
004800 77  JA                     PIC X      VALUE 'J'.                         
004900 77  NEJ                    PIC X      VALUE 'N'.                         
005000 77  W-IDLOPNR              PIC 9(5)   VALUE  ZERO   COMP-3.              
005100 77  IX                     PIC S9(9)  VALUE  +1     COMP SYNC.           
005200                                                                          
005300*                                                                         
005400 77  W11170-EOF-SW              PIC X       VALUE 'N'.                    
005500     88  END-OF-W11170                      VALUE 'J'.                    
005600     EJECT                                                                
005700 01  SUBPROGRAM.                                                          
005800     03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                  
005900     03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                  
006000     03    POSTSUM         PIC X(8)    VALUE 'POSTSUM '.                  
006100     EJECT                                                                
006200*    PARAMETRAR TILL POSTSUM                                              
006300*01   -COPY W0005 -PRE POSTSUM-                                           
006400     EJECT                                                                
006500                                                                          
006600 01  IN-AREA-START           PIC X(24)   VALUE 'IN-AREA START'.           
006700     SKIP2                                                                
006800 01  IN-AREA.                                                             
006900     03 IN-IDPTYP            PIC X(3).                                    
007000     03 FILLER               PIC X(157).                                  
007100*01  AREA   -COPY W111701A      -PRE IN701- -RED IN-AREA                  
007200     EJECT                                                                
007300*01  AREA   -COPY W111702A      -PRE IN702- -RED IN-AREA                  
007400     EJECT                                                                
007500                                                                          
007600 01  FILLER                  PIC X(24)   VALUE 'UT-AREA START'.           
007700 01  UT-AREA.                                                             
007800     03 UT-IDPTYP            PIC X(3).                                    
007900     03 FILLER               PIC X(97).                                   
008000     SKIP3                                                                
008100*01  AREA   -COPY W1117N1A      -PRE UTS01- -RED UT-AREA                  
008200     EJECT                                                                
008300*01  AREA   -COPY W1117N2A      -PRE UTS2- -RED UT-AREA                   
008400     EJECT                                                                
008500                                                                          
008600*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
008700*                                                                         
008800 01  IMS-WS.                                                              
008900     03     FILLER         PIC X(8)    VALUE 'IMS-WS  '.                  
009000*                                                                         
009100 01  NYCKLAR.                                                             
009200     03  W-IDARTNR-X.                                                     
009300       05  W-IDARTNR         PIC S9(9)     COMP-3.                        
009400     EJECT                                                                
009500*                            *** STATUSKOD FRÅN IMS                       
009600     03  STATUS-WS         PIC XX.                                        
009700         88  SEGMENT-FINNS             VALUE '  '.                        
009800         88  SEGMENT-SAKNAS            VALUE 'GE'.                        
009900*                                                                         
010000     SKIP3                                                                
010100     03    SSA1            PIC X(64).                                     
010200     03    SSA2            PIC X(64).                                     
010300     SKIP3                                                                
010400     03    GODK-STATUSKODER.                                              
010500         05    GODK-STATUS OCCURS 3  INDEXED BY STATUS-IX PIC XX.         
010600     EJECT                                                                
010700*01      -COPY W0003                                                      
010800     EJECT                                                                
010900 01  DLI-IO-AREA.                                                         
011000     03  IO-AREA     PIC X(200)  VALUE SPACE.                             
011100     SKIP3                                                                
011200    03  WDD301  REDEFINES IO-AREA.                                        
011300*       05 -COPY WDD301                                                   
011400     EJECT                                                                
011500 LINKAGE SECTION.                                                         
011600     SKIP3                                                                
011700*01      -COPY W0008     -PRE WDD3-                                       
011800         05  FILLER       PIC X.                                          
011900     EJECT                                                                
012000 PROCEDURE DIVISION  USING  WDD3-PCB .                                    
012100     ENTRY 'DLITCBL' USING  WDD3-PCB .                                    
012200                                                                          
012300     PERFORM A-INIT                                                       
012400*                                                                         
012500*--- LÄSER IGENOM SAMTLIGA POSTER PÅ W11170 OCH SKRIVER UT                
012600*    DEM OPACKADE PÅ FIL W1117N MED KOMPLETTERING AV IDBENNR              
012700*                                                                         
012800     PERFORM S01-LAES-W11170                                              
012900     PERFORM UNTIL END-OF-W11170                                          
013000       IF IN-IDPTYP = '701'                                               
013100         PERFORM B-KONVERTERA-701-POST                                    
013200       ELSE                                                               
013300         PERFORM C-KONVERTERA-702-POST                                    
013400       END-IF                                                             
013500       PERFORM S01-LAES-W11170                                            
013600     END-PERFORM                                                          
013700*                                                                         
013800     PERFORM Z-FINIT                                                      
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014300 A-INIT SECTION.                                                          
014400                                                                          
014500     OPEN INPUT  W11170                                                   
014600     OPEN OUTPUT W1117N                                                   
014700                                                                          
014800     MOVE SPACE TO UT-AREA                                                
014900     .                                                                    
015000     EJECT                                                                
015100 B-KONVERTERA-701-POST SECTION.                                           
015200     SKIP2                                                                
015300     MOVE 'S01'              TO UTS01-IDPTYP                              
015400     MOVE IN701-IDARTNR-ERS  TO W-IDARTNR                                 
015500                                UTS01-IDARTNR-ERS                         
015600     MOVE IN701-IDLOPNR      TO UTS01-IDLOPNR                             
015700     MOVE IN701-TIAAMMDD     TO UTS01-TIAAMMDD                            
015800     MOVE IN701-REKSIFFR-ERS TO UTS01-REKSIFFR-ERS                        
015900     MOVE IN701-KDERS-OLD    TO UTS01-KDERS-OLD                           
016000     MOVE IN701-KDERS-NEW    TO UTS01-KDERS-NEW                           
016100     MOVE IN701-TIERSDAT     TO UTS01-TIERSDAT                            
016200     MOVE IN701-DIERS-ERS    TO UTS01-DIERS-ERS                           
016300     MOVE IN701-BEART-SVE    TO UTS01-BEART-SVE                           
016400                                                                          
016500     PERFORM IMS-GET-WDD301                                               
016600     IF SEGMENT-FINNS                                                     
016700       MOVE BEN-IDBENNR      TO UTS01-IDBENNR                             
016800     END-IF                                                               
016900                                                                          
017000     PERFORM S11-SKRIV-UTPOST                                             
017100     .                                                                    
017200     EJECT                                                                
017300 C-KONVERTERA-702-POST SECTION.                                           
017400     SKIP2                                                                
017500     MOVE IN702-IDARTNR-ERS       TO W-IDARTNR                            
017600                                     UTS2-IDARTNR-ERS                     
017700     MOVE IN702-IDLOPNR           TO UTS2-IDLOPNR                         
017800     MOVE IN702-TIAAMMDD          TO UTS2-TIAAMMDD                        
017900     MOVE IN702-IDKORTNR          TO UTS2-IDKORTNR                        
018000     MOVE IN702-FLTEXT            TO UTS2-FLTEXT                          
018100     IF IN702-FLTEXT = 'J'                                                
018200       MOVE IN702-BEERS           TO UTS2-BEERS                           
018300       MOVE 'S2B'                 TO UTS2-IDPTYP                          
018400     ELSE                                                                 
018500       MOVE 'S2A'                 TO UTS2-IDPTYP                          
018600       MOVE IN702-IDARTNR-TILLK   TO W-IDARTNR                            
018700                                     UTS2-IDARTNR-TILLK                   
018800       MOVE IN702-REKSIFFR-TILLK  TO UTS2-REKSIFFR-TILLK                  
018900       MOVE IN702-DIERS-TILLK     TO UTS2-DIERS-TILLK                     
019000       MOVE IN702-BEART-SVE-TILLK TO UTS2-BEART-SVE-TILLK                 
019100                                                                          
019200       PERFORM IMS-GET-WDD301                                             
019300       IF SEGMENT-FINNS                                                   
019400         MOVE BEN-IDBENNR         TO UTS2-IDBENNR                         
019500       END-IF                                                             
019600     END-IF                                                               
019700     PERFORM S11-SKRIV-UTPOST                                             
019800     .                                                                    
019900     EJECT                                                                
020000 Z-FINIT SECTION.                                                         
020100                                                                          
020200     CLOSE W11170                                                         
020300           W1117N                                                         
020400                                                                          
020500     MOVE 'S' TO POSTSUM-OPKOD                                            
020600     CALL POSTSUM USING POSTSUM-PARM                                      
020700     .                                                                    
020800     EJECT                                                                
020900 S01-LAES-W11170  SECTION.                                                
021000     SKIP2                                                                
021100     READ W11170 INTO IN-AREA                                             
021200     AT END                                                               
021300        SET END-OF-W11170 TO TRUE                                         
021400                                                                          
021500     NOT AT END                                                           
021600       IF IN-IDPTYP = '701'                                               
021700         MOVE 'I01'      TO POSTSUM-TRANSTYP                              
021800       ELSE                                                               
021900         MOVE 'I02'      TO POSTSUM-TRANSTYP                              
022000       END-IF                                                             
022100       MOVE 'W11170'   TO POSTSUM-FDNAMN                                  
022200       MOVE 'W1117ND1' TO POSTSUM-DDNAMN2                                 
022300       CALL POSTSUM USING POSTSUM-PARM                                    
022400     END-READ                                                             
022500     .                                                                    
022600     EJECT                                                                
022700 S11-SKRIV-UTPOST SECTION.                                                
022800                                                                          
022900     IF UT-IDPTYP = 'S01'                                                 
023000        WRITE UTS01-POST FROM UTS01-AREA                                  
023100        MOVE 'S01'         TO POSTSUM-TRANSTYP                            
023200     ELSE                                                                 
023300        IF UT-IDPTYP = 'S2A'                                              
023400          MOVE 'S2A'       TO POSTSUM-TRANSTYP                            
023500        ELSE                                                              
023600          MOVE 'S2B'       TO POSTSUM-TRANSTYP                            
023700        END-IF                                                            
023800        WRITE UTS2-POST FROM UTS2-AREA                                    
023900     END-IF                                                               
024000     MOVE SPACE TO UT-AREA                                                
024100                                                                          
024200     MOVE 'W1117N'      TO POSTSUM-FDNAMN                                 
024300     MOVE 'W1117ND2'    TO POSTSUM-DDNAMN2                                
024400     CALL POSTSUM USING POSTSUM-PARM                                      
024500     .                                                                    
024600     EJECT                                                                
024700*   IMS SEKTIONER                                                         
024800     SKIP2                                                                
024900 IMS-GET-WDD301 SECTION.                                                  
025000                                                                          
025100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
025200            DELIMITED BY SIZE INTO SSA1                                   
025300     MOVE '  GE' TO GODK-STATUSKODER                                      
025400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA SSA1                      
025500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
025600     PERFORM IMS-STATUSKONTROLL                                           
025700     .                                                                    
025800     SKIP3                                                                
025900 IMS-STATUSKONTROLL SECTION.                                              
026000                                                                          
026100     SET STATUS-IX TO 1                                                   
026200     SEARCH GODK-STATUS                                                   
026300        AT END                                                            
026400           CALL FELLOG                                                    
026500        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
026600           CONTINUE                                                       
026700     END-SEARCH                                                           
026800     .                                                                    
