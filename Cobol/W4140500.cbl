000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4140500.                                                
000300 AUTHOR.         BO SVENSSON.                                             
000400 DATE-WRITTEN.   95/10/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGREMMET KOMPLETTERAR PRISFELSPOSTER MED MARKNADS-             
000900*        BOLAGSKOD.                                                       
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001200*        PROGRAMMET LÄSER      WDB101                                     
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- ORDERRADER MED PRISFEL                                     
002700     SELECT W4140J                     ASSIGN TO W41405D1.                
002800     SKIP2                                                                
002900*          --- PRISFELSPOSTER KOMPLETTERADE MED MARKNADSBOLAGSKOD         
003000     SELECT W4140K                     ASSIGN TO W41405D2.                
003100     SKIP2                                                                
003200*          --- SORTERINGSFIL                                              
003300     SELECT SORTFIL                    ASSIGN TO W41405DS.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W4140J                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W414009      -L.                                               
004400     SKIP3                                                                
004500 FD  W4140K                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  POST -COPY W414009 -PRE  UT-  -L.                                    
005000     SKIP2                                                                
005100 SD  SORTFIL.                                                             
005200                                                                          
005300*01  POST -COPY W414009      -PRE SORT-                                   
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700                                                                          
005800*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM                       PIC X(8)    VALUE 'W4140500'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200                                                                          
006300 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
006400     88  END-OF-SORTFIL                      VALUE 'J'.                   
006500     SKIP2                                                                
006600 01  OLD-IDDISTR                 PIC S9(5)    COMP-3 VALUE ZERO.          
006700 01  OLD-IDKUNDNR                PIC S9(7)    COMP-3 VALUE ZERO.          
006800                                                                          
006900 01  WS-IDMARKBO                 PIC X        VALUE SPACE.                
007000     EJECT                                                                
007100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES DAGENS-DATUM.                                       
007300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007600     EJECT                                                                
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800*                                                                         
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     SKIP2                                                                
008400*    --- PARAMETRAR TILL ABEND                                            
008500                                                                          
008600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008800     SKIP2                                                                
008900 01  FELTEXT.                                                             
009000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009200     EJECT                                                                
009300*    --- PARAMETRAR TILL POSTSUM                                          
009400*                                                                         
009500*01  -COPY W0005   -PRE  POSTSUM-                                         
009600     EJECT                                                                
009700 01  IN-AREA-START               PIC X(24)   VALUE                        
009800                                 'IN-AREA-START  '.                       
009900     SKIP2                                                                
010000                                                                          
010100*01  AREA -COPY W414009     -PRE IN-                                      
010200     EJECT                                                                
010300 01  UT-AREA-START               PIC X(24)   VALUE                        
010400                                 'UT-AREA-START  '.                       
010500     SKIP2                                                                
010600                                                                          
010700*01  AREA -COPY W414009     -PRE UT-                                      
010800     EJECT                                                                
010900 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
011000                                  'SORTWS-AREA-START  '.                  
011100     SKIP2                                                                
011200                                                                          
011300*01  AREA -COPY W414009      -PRE SORTWS-                                 
011400 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
011500     EJECT                                                                
011600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011700*                                                                         
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012000     SKIP3                                                                
012100 01  NYCKLAR-TILL-DLI.                                                    
012200     03  W-IDGMT-X.                                                       
012300         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
012400         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
012500     03  W-WDB101KY-X.                                                    
012600         05  W-WDB1-IDPARTNR     PIC X(9)         VALUE SPACE.            
012700         05  W-WDB1-IDFTG        PIC 9(2)         VALUE ZERO.             
012800     SKIP2                                                                
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                       VALUE '  '.                  
013200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013400     SKIP2                                                                
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000     EJECT                                                                
014100*    --- IMS FUNKTIONSKODER                                               
014200*01  -COPY W0003                                                          
014300     EJECT                                                                
014400*    ---  DLI INPUT-OUTPUT AREA                                           
014500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014600                                                                          
014700 01  DLI-IO-AREA.                                                         
014800*  03  -COPY WDB201  -PRE GMTA-                                           
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-ARE2'.         
015100                                                                          
015200 01  DLI-IO-ARE2.                                                         
015300*  03  -COPY WDB101  -PRE WDB1-                                           
015400     EJECT                                                                
015500 LINKAGE SECTION.                                                         
015600*01  -COPY W0008  -PRE GMTA-                                              
015700     05  FILLER                  PIC X.                                   
015800                                                                          
015900*01  -COPY W0008  -PRE WDB1-                                              
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200 PROCEDURE DIVISION  USING GMTA-PCB WDB1-PCB.                             
016300 MAIN SECTION.                                                            
016400     ENTRY 'DLITCBL' USING GMTA-PCB WDB1-PCB.                             
016500                                                                          
016600     PERFORM A-INIT                                                       
016700                                                                          
016800     SORT SORTFIL ASCENDING KEY SORT-AMC-IDDISTR                          
016900                                SORT-AMC-IDKUNDNR                         
017000                  USING W4140J                                            
017100                  OUTPUT PROCEDURE B-SORT-OUTPUT                          
017200                                                                          
017300     IF SORT-RETURN NOT = 0                                               
017400       MOVE SORT-RETURN TO SORT-RETURN-X                                  
017500       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
017600           DELIMITED BY SIZE                                              
017700           INTO FELTEXT-STR                                               
017800       DISPLAY FELTEXT                                                    
017900       PERFORM S99-ABEND                                                  
018000     ELSE                                                                 
018100       PERFORM Z-FINIT                                                    
018200                                                                          
018300       MOVE ZERO TO RETURN-CODE                                           
018400       GOBACK                                                             
018500     END-IF                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 A-INIT SECTION.                                                          
018900                                                                          
019000     OPEN OUTPUT W4140K                                                   
019100                                                                          
019200     ACCEPT DAGENS-DATUM  FROM DATE                                       
019300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019400     .                                                                    
019500     EJECT                                                                
019600 B-SORT-OUTPUT SECTION.                                                   
019700                                                                          
019800     PERFORM S31-SORT-RETURN                                              
019900     PERFORM UNTIL END-OF-SORTFIL                                         
020000       MOVE SORTWS-AREA TO IN-AREA                                        
020100                                                                          
020200       IF IN-AMC-IDDISTR NOT = OLD-IDDISTR                                
020300       OR IN-AMC-IDKUNDNR NOT = OLD-IDKUNDNR                              
020400         PERFORM CA-LAS-FRAM-MARKNADSBOLAG                                
020500         MOVE IN-AMC-IDDISTR  TO OLD-IDDISTR                              
020600         MOVE IN-AMC-IDKUNDNR TO OLD-IDKUNDNR                             
020700       END-IF                                                             
020800                                                                          
020900       MOVE IN-AREA     TO UT-AREA                                        
021000       MOVE WS-IDMARKBO TO UT-AMC-IDMARKBO                                
021100                                                                          
021200       PERFORM S11-SKRIV-W4140K                                           
021300                                                                          
021400       PERFORM S31-SORT-RETURN                                            
021500     END-PERFORM                                                          
021600     .                                                                    
021700     EJECT                                                                
021800 CA-LAS-FRAM-MARKNADSBOLAG SECTION.                                       
021900                                                                          
022000     MOVE IN-AMC-IDDISTR  TO W-IDDISTR                                    
022100     MOVE IN-AMC-IDKUNDNR TO W-IDKUNDNR                                   
022200                                                                          
022300     PERFORM IMS-GU-WDB201                                                
022400                                                                          
022500     MOVE GMTA-GMT-IDPARTNR TO W-WDB1-IDPARTNR                            
022600     MOVE GMTA-GMT-IDFTG    TO W-WDB1-IDFTG                               
022700     PERFORM IMS-GET-WDB101                                               
022800                                                                          
022900     IF  SEGMENT-FINNS                                                    
023000       MOVE WDB1-BET-IDMARKBO TO WS-IDMARKBO                              
023100     ELSE                                                                 
023200       MOVE SPACE             TO WS-IDMARKBO                              
023300     END-IF                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 Z-FINIT SECTION.                                                         
023700     CLOSE W4140K                                                         
023800     SKIP2                                                                
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     .                                                                    
024200     EJECT                                                                
024300 S11-SKRIV-W4140K SECTION.                                                
024400                                                                          
024500     WRITE UT-POST FROM UT-AREA                                           
024600                                                                          
024700     MOVE UT-AMC-IDPTYP TO POSTSUM-TRANSTYP                               
024800     MOVE 'W4140K' TO POSTSUM-FDNAMN                                      
024900     MOVE 'W41405D2' TO POSTSUM-DDNAMN2                                   
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200     EJECT                                                                
025300 S31-SORT-RETURN  SECTION.                                                
025400                                                                          
025500     RETURN SORTFIL INTO SORTWS-AREA                                      
025600     AT END                                                               
025700         SET END-OF-SORTFIL TO TRUE                                       
025800     .                                                                    
025900     EJECT                                                                
026000 S99-ABEND SECTION.                                                       
026100                                                                          
026200     SKIP2                                                                
026300     MOVE 'S' TO POSTSUM-OPKOD                                            
026400     CALL POSTSUM USING POSTSUM-PARM                                      
026500     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
026600     .                                                                    
026700     EJECT                                                                
026800* --- IMS SEKTIONER ---                                                   
026900     SKIP3                                                                
027000     EJECT                                                                
027100 IMS-GU-WDB201 SECTION.                                                   
027200                                                                          
027300     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
027400          DELIMITED BY SIZE INTO SSA1                                     
027500     MOVE '  GE' TO GODK-STATUSKODER                                      
027600     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA SSA1                      
027700     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
027800     PERFORM IMS-STATUSKONTROLL                                           
027900     .                                                                    
028000     EJECT                                                                
028100 IMS-GET-WDB101 SECTION.                                                  
028200                                                                          
028300     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
028400          DELIMITED BY SIZE INTO SSA1                                     
028500     MOVE '  GE'  TO GODK-STATUSKODER                                     
028600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-ARE2 SSA1                      
028700     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
028800     PERFORM IMS-STATUSKONTROLL                                           
028900     .                                                                    
029000     EJECT                                                                
029100 IMS-STATUSKONTROLL SECTION.                                              
029200                                                                          
029300     SET STATUS-IX TO 1                                                   
029400     SEARCH GODK-STATUS                                                   
029500       AT END                                                             
029600         STRING ' FELAKTIG RETURKOD FRÅN IMS: ' STATUS-WS                 
029700           DELIMITED BY SIZE INTO FELTEXT-STR                             
029800         DISPLAY FELTEXT                                                  
029900         CALL FELLOG                                                      
030000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030100         CONTINUE                                                         
030200     END-SEARCH                                                           
030300     .                                                                    
