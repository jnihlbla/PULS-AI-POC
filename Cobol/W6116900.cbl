000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6116900.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   98/09/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        POSTER FÖR FRAMSTÄLLNING AV INTRASTAT                            
000900*        KOMPLETTERAS MED LANDKOD                                         
001000*        EU-POSTER GÅR VIDARE                                             
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
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
002600*          --- POSTER TILL INTRASTAT                                      
002700     SELECT W61169                     ASSIGN TO W61169D1.                
002800     SKIP2                                                                
002900*          --- EU-POSTER TILL INTRASTAT                                   
003000     SELECT W61171                     ASSIGN TO W61169D2.                
003100     SKIP2                                                                
003200*          --- SORTERINGSFIL                                              
003300     SELECT SORTFIL                    ASSIGN TO W61169DS.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W61169                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W61169      -L.                                                
004400     SKIP3                                                                
004500 FD  W61171                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  POST -COPY W61171 -PRE  UT-  -L.                                     
005000     SKIP2                                                                
005100 SD  SORTFIL.                                                             
005200                                                                          
005300*01  POST -COPY W61169      -PRE SORT-                                    
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700                                                                          
005800*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM                       PIC X(8)    VALUE 'W6116900'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200                                                                          
006300 77  W61169-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W61169                       VALUE 'J'.                   
006500                                                                          
006600 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
006700     88  END-OF-SORTFIL                      VALUE 'J'.                   
006800                                                                          
006900 01  ARBAREOR.                                                            
007000     03  OLD-IDLEVNR             PIC  X(5)   VALUE SPACE.                 
007100     EJECT                                                                
007200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007300 01  FILLER REDEFINES DAGENS-DATUM.                                       
007400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007700     EJECT                                                                
007800*01  -COPY WWLEVEU                                                        
007900     EJECT                                                                
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008600     SKIP2                                                                
008700*    --- PARAMETRAR TILL ABEND                                            
008800                                                                          
008900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009200     SKIP2                                                                
009300 01  FELTEXT.                                                             
009400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL POSTSUM                                          
009800*                                                                         
009900*01  -COPY W0005   -PRE  POSTSUM-                                         
010000     EJECT                                                                
010100 01  IN-AREA-START               PIC X(24)   VALUE                        
010200                                 'IN-AREA-START  '.                       
010300     SKIP2                                                                
010400                                                                          
010500*01  AREA -COPY W61169     -PRE IN-                                       
010600     EJECT                                                                
010700 01  UT-AREA-START               PIC X(24)   VALUE                        
010800                                 'UT-AREA-START  '.                       
010900     SKIP2                                                                
011000                                                                          
011100*01  AREA -COPY W61171     -PRE UT-                                       
011200     EJECT                                                                
011300 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
011400                                  'SORTWS-AREA-START  '.                  
011500     SKIP2                                                                
011600                                                                          
011700*01  AREA -COPY W61169      -PRE SORTWS-                                  
011800 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
011900     EJECT                                                                
012000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012100*                                                                         
012200     EJECT                                                                
012300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012400     SKIP3                                                                
012500 01  NYCKLAR-TILL-DLI.                                                    
012600     03  W-IDLEVNR-X.                                                     
012700         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
012800     03  W-IDLEVSUF-X.                                                    
012900         05  W-IDLEVSUF          PIC S9(1)   VALUE 1    COMP-3.           
013000     SKIP2                                                                
013100*    --- STATUS-KOD FRÅN IMS                                              
013200 01  STATUS-WS                   PIC XX.                                  
013300     88  SEGMENT-FINNS                       VALUE '  '.                  
013400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013600     SKIP2                                                                
013700 01  GODK-STATUSKODER.                                                    
013800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01  SSA1                        PIC X(64).                               
014100 01  SSA2                        PIC X(64).                               
014200     EJECT                                                                
014300*    --- IMS FUNKTIONSKODER                                               
014400*01  -COPY W0003                                                          
014500     EJECT                                                                
014600*    ---  DLI INPUT-OUTPUT AREA                                           
014700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA01'.                    
014800 01  DLI-IO-WLLEVA01.                                                     
014900*    03  -COPY WDF101                                                     
015000     EJECT                                                                
015100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA14'.                    
015200 01  DLI-IO-WLLEVA14.                                                     
015300*    03  -COPY WDF106                                                     
015400     EJECT                                                                
015500 LINKAGE SECTION.                                                         
015600                                                                          
015700                                                                          
015800*01  -COPY W0008  -PRE LEVA-                                              
015900     05  FILLER                  PIC X.                                   
016000     EJECT                                                                
016100 PROCEDURE DIVISION  USING LEVA-PCB.                                      
016200 MAIN SECTION.                                                            
016300     ENTRY 'DLITCBL' USING LEVA-PCB.                                      
016400                                                                          
016500                                                                          
016600     PERFORM A-INIT                                                       
016700                                                                          
016800     SORT SORTFIL ASCENDING KEY SORT-IDLEVNR                              
016900                                SORT-KDRT                                 
017000                  INPUT PROCEDURE B-SORT-INPUT                            
017100                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
017200                                                                          
017300     IF SORT-RETURN NOT = 0                                               
017400       MOVE SORT-RETURN TO SORT-RETURN-X                                  
017500       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
017600           DELIMITED BY SIZE                                              
017700           INTO FELTEXT-STR                                               
017800       DISPLAY FELTEXT                                                    
017900       MOVE RKOD-ABEND-UTAN-DUMP  TO RKOD-ABEND                           
018000       PERFORM S99-ABEND                                                  
018100     ELSE                                                                 
018200       PERFORM Z-FINIT                                                    
018300                                                                          
018400       MOVE ZERO TO RETURN-CODE                                           
018500       GOBACK                                                             
018600     END-IF                                                               
018700                                                                          
018800     .                                                                    
018900     EJECT                                                                
019000 A-INIT SECTION.                                                          
019100                                                                          
019200     OPEN INPUT  W61169                                                   
019300                                                                          
019400     OPEN OUTPUT W61171                                                   
019500                                                                          
019600     ACCEPT DAGENS-DATUM  FROM DATE                                       
019700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019800     .                                                                    
019900     EJECT                                                                
020000 B-SORT-INPUT  SECTION.                                                   
020100                                                                          
020200     PERFORM S01-LAES-W61169                                              
020300     PERFORM UNTIL END-OF-W61169                                          
020400       MOVE IN-AREA TO SORTWS-AREA                                        
020500       PERFORM S31-SORT-RELEASE                                           
020600       PERFORM S01-LAES-W61169                                            
020700     END-PERFORM                                                          
020800     .                                                                    
020900     EJECT                                                                
021000 C-SORT-OUTPUT SECTION.                                                   
021100                                                                          
021200     PERFORM S32-SORT-RETURN                                              
021300     PERFORM UNTIL END-OF-SORTFIL                                         
021400       IF SORTWS-KDRT = 08                                                
021500          MOVE 'NL' TO UT-IDLANDX2                                        
021600          PERFORM CA-SKRIV-UTAREA                                         
021700       END-IF                                                             
021800                                                                          
021900       IF SORTWS-KDRT = 00                                                
022000          IF SORTWS-IDLEVNR NOT = OLD-IDLEVNR                             
022100             MOVE SORTWS-IDLEVNR TO W-IDLEVNR                             
022200             PERFORM IMS-GET-WDF106                                       
022300             IF SEGMENT-FINNS                                             
022400               MOVE ADR-IDLANDX2 TO LEVEU-IDLANDX2                        
022500               IF LEVEU-IDLANDX2-EU                                       
022600                  MOVE ADR-IDLANDX2 TO UT-IDLANDX2                        
022700                  PERFORM CA-SKRIV-UTAREA                                 
022800                  MOVE SORTWS-IDLEVNR TO OLD-IDLEVNR                      
022900               END-IF                                                     
023000             ELSE                                                         
023100               DISPLAY 'IDLEVNR SAKNAS '  SORTWS-IDLEVNR                  
023200             END-IF                                                       
023300          ELSE                                                            
023301             IF LEVEU-IDLANDX2-EU                                         
023302                MOVE ADR-IDLANDX2 TO UT-IDLANDX2                          
023303                PERFORM CA-SKRIV-UTAREA                                   
023304             END-IF                                                       
023400          END-IF                                                          
023500       END-IF                                                             
023600       PERFORM S32-SORT-RETURN                                            
023700     END-PERFORM                                                          
023800     .                                                                    
023900     EJECT                                                                
024000 CA-SKRIV-UTAREA SECTION.                                                 
024100                                                                          
024200     MOVE SORTWS-IDARTNR           TO UT-IDARTNR                          
024300     MOVE SORTWS-KVANTMOT          TO UT-KVANTMOT                         
024400     MOVE SORTWS-SUARTBES          TO UT-SUARTBES                         
024500     MOVE SORTWS-KDSORT            TO UT-KDSORT                           
024600     MOVE SORTWS-TIAAMMDD-GAELL    TO UT-TIAAMMDD-GAELL                   
024610                                                                          
024620     PERFORM S11-SKRIV-W61171                                             
024700     .                                                                    
024800     EJECT                                                                
024900 Z-FINIT SECTION.                                                         
025000     CLOSE W61169                                                         
025100           W61171                                                         
025200     SKIP2                                                                
025300     MOVE 'S' TO POSTSUM-OPKOD                                            
025400     CALL POSTSUM USING POSTSUM-PARM                                      
025500     .                                                                    
025600     EJECT                                                                
025700 S01-LAES-W61169  SECTION.                                                
025800     READ W61169 INTO IN-AREA                                             
025900     AT END                                                               
026000        SET END-OF-W61169 TO TRUE                                         
026100                                                                          
026200     NOT AT END                                                           
026300        MOVE 'W61169'   TO POSTSUM-FDNAMN                                 
026400        MOVE 'W61169D1' TO POSTSUM-DDNAMN2                                
026500        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
026600        CALL POSTSUM USING POSTSUM-PARM                                   
026700     END-READ                                                             
026800     .                                                                    
026900     EJECT                                                                
027000 S11-SKRIV-W61171 SECTION.                                                
027100                                                                          
027200     WRITE UT-POST FROM UT-AREA                                           
027300                                                                          
027400     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
027500     MOVE 'W61171'   TO POSTSUM-FDNAMN                                    
027600     MOVE 'W61169D2' TO POSTSUM-DDNAMN2                                   
027700     CALL POSTSUM USING POSTSUM-PARM                                      
027800     .                                                                    
027900     EJECT                                                                
028000 S31-SORT-RELEASE  SECTION.                                               
028100                                                                          
028200     RELEASE SORT-POST FROM SORTWS-AREA                                   
028300     .                                                                    
028400     SKIP3                                                                
028500 S32-SORT-RETURN  SECTION.                                                
028600                                                                          
028700     RETURN SORTFIL INTO SORTWS-AREA                                      
028800     AT END                                                               
028900         SET END-OF-SORTFIL TO TRUE                                       
029000     .                                                                    
029100     EJECT                                                                
029200 S99-ABEND SECTION.                                                       
029300                                                                          
029400     SKIP2                                                                
029500     MOVE 'S' TO POSTSUM-OPKOD                                            
029600     CALL POSTSUM USING POSTSUM-PARM                                      
029700     CALL ABEND USING RKOD-ABEND                                          
029800     .                                                                    
029900     EJECT                                                                
030000* --- IMS SEKTIONER ---                                                   
030100                                                                          
030200 IMS-GET-WDF106 SECTION.                                                  
030300                                                                          
030400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
030500          DELIMITED BY SIZE INTO SSA1                                     
030600     STRING 'WLLEVA14(IDLEVSUF =' W-IDLEVSUF-X ')'                        
030700          DELIMITED BY SIZE INTO SSA2                                     
030800     MOVE '  GE' TO GODK-STATUSKODER                                      
030900     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-WLLEVA14 SSA1 SSA2             
031000     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300     SKIP3                                                                
031400 IMS-STATUSKONTROLL SECTION.                                              
031500                                                                          
031600     SET STATUS-IX TO 1                                                   
031700     SEARCH GODK-STATUS                                                   
031800       AT END                                                             
031900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032000           DELIMITED BY SIZE INTO FELTEXT                                 
032100         DISPLAY FELTEXT                                                  
032200         CALL FELLOG                                                      
032300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032400         CONTINUE                                                         
032500     END-SEARCH                                                           
032600     .                                                                    
