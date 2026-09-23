000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0116600.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   94/10/25.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER WDD2 MED SB.                                               
001000*        SKAPAR FIL MED URVAL AV UPPGIFTER .                              
001100*        W01166   URVAL FRÅN WDD201                                       
001200*                                                                         
001210*        SKAPAR FIL TILL CPAM. URVAL KDRESBED > SPACE                     
001230*                                                                         
001300*        PROGRAMMET LÄSER      WLARTG (WDD2)                              
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
002600     SKIP2                                                                
002700*          --- UTDRAG UR WDD201                                           
002800     SELECT W01166                     ASSIGN TO W01166D1.                
002900*          --- UTDRAG UR WDD201 FÖR CPAM                                  
003000     SELECT W0116602                   ASSIGN TO W01166D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W01166                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W01166 -PRE  W01166-  -L.                                 
004100     EJECT                                                                
004200 FD  W0116602                                                             
004300     RECORDING       V                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W0116602 -PRE  UT-CPAM-        -L.                        
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W0116600'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005410 77  WS-IDARTNR-NUM8            PIC S9(08).                               
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  FELTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  W01166-AREA-START           PIC X(24)   VALUE                        
008300                                 'W01166-AREA-START  '.                   
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W01166     -PRE W01166-                                   
008700     EJECT                                                                
008710 01  W0116602-AREA-START         PIC X(24)   VALUE                        
008720                                 'W0116602-AREA-START  '.                 
008730     SKIP2                                                                
008740                                                                          
008750*01  AREA -COPY W0116602   -PRE UT-CPAM-                                  
008760     EJECT                                                                
008800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008900*                                                                         
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009200     SKIP3                                                                
009300 01  NYCKLAR-TILL-DLI.                                                    
009400     03  W-IDARTNR-X.                                                     
009500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009600     SKIP2                                                                
009700*    --- STATUS-KOD FRÅN IMS                                              
009800 01  STATUS-WS                   PIC XX.                                  
009900     88  SEGMENT-FINNS                       VALUE '  '.                  
010000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011300     SKIP3                                                                
011400 01  DLI-IO-AREA.                                                         
011500     03  IO-AREA                 PIC X(700)  VALUE SPACE.                 
011600     SKIP3                                                                
011700     03  WDD201 REDEFINES IO-AREA.                                        
011800*        05  -COPY WDD201                                                 
011900     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100                                                                          
012200     EJECT                                                                
012300*01  -COPY W0008  -PRE WDD2-                                              
012400     05  FILLER                  PIC X.                                   
012500     EJECT                                                                
012600 PROCEDURE DIVISION  USING WDD2-PCB.                                      
012700     ENTRY 'DLITCBL' USING WDD2-PCB.                                      
012800                                                                          
012900     PERFORM A-INIT                                                       
013000     PERFORM IMS-GET-WDD2                                                 
013100     PERFORM UNTIL SEGMENT-SLUT                                           
013200        EVALUATE WDD2-SEG-NAME-FB                                         
013300           WHEN 'WDD201  '                                                
013400              IF ART-IDARTNR-MOTSV > ZERO                                 
013500              OR ART-TEORSAK > SPACE                                      
013600              OR ART-KDANSKQ = '1'                                        
013700                 PERFORM B-FLYTTA-WDD201                                  
013800                 PERFORM S11-SKRIV-W01166                                 
013900              END-IF                                                      
013910                                                                          
014000              IF ART-KDRESBED NOT = SPACE                                 
014030                 PERFORM C-FLYTTA-WDD201-CPAM                             
014040                 PERFORM S12-SKRIV-W0116602                               
014050              END-IF                                                      
014100         END-EVALUATE                                                     
014200         PERFORM IMS-GET-WDD2                                             
014300     END-PERFORM                                                          
014400     PERFORM Z-FINIT                                                      
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014700     .                                                                    
014800     EJECT                                                                
014900                                                                          
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     OPEN OUTPUT W01166                                                   
015210                 W0116602                                                 
015300                                                                          
015400     ACCEPT DAGENS-DATUM  FROM DATE                                       
015500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015700     EJECT                                                                
015800                                                                          
015900                                                                          
016000 B-FLYTTA-WDD201 SECTION.                                                 
016100                                                                          
016200     MOVE ART-IDARTNR        TO W01166-ART-IDARTNR                        
016300     MOVE ART-IDARTNR-MOTSV  TO                                           
016400               W01166-ART-IDARTNR-MOTSV                                   
016500     MOVE ART-TEORSAK        TO W01166-ART-TEORSAK                        
016600     MOVE ART-KDANSKQ        TO W01166-ART-KDANSKQ                        
016700     MOVE ART-IDANSK         TO W01166-ART-IDANSK                         
016800     MOVE ART-DAFINLEV       TO W01166-ART-DAFINLEV                       
016900     MOVE ART-TIREGDAT       TO W01166-ART-TIREGDAT                       
017000     .                                                                    
017100     EJECT                                                                
017200                                                                          
017300                                                                          
017310 C-FLYTTA-WDD201-CPAM SECTION.                                            
017320                                                                          
017321     MOVE 'SV1'              TO UT-CPAM-IDPTYP                            
017330     MOVE ART-IDARTNR        TO WS-IDARTNR-NUM8                           
017340     MOVE WS-IDARTNR-NUM8    TO UT-CPAM-IDARTNR                           
017391     MOVE ART-KDRESBED       TO UT-CPAM-KDRESBED                          
017394     .                                                                    
017395     EJECT                                                                
017396                                                                          
017397                                                                          
017400 Z-FINIT SECTION.                                                         
017500     CLOSE W01166                                                         
017510           W0116602                                                       
017600     SKIP2                                                                
017700     MOVE 'S' TO POSTSUM-OPKOD                                            
017800     CALL POSTSUM USING POSTSUM-PARM                                      
017900     .                                                                    
018000     EJECT                                                                
018100                                                                          
018200                                                                          
018300 S11-SKRIV-W01166 SECTION.                                                
018400                                                                          
018500     WRITE W01166-POST FROM W01166-AREA                                   
018600                                                                          
018700     MOVE 'W01166' TO POSTSUM-FDNAMN                                      
018800     MOVE 'W01166D1' TO POSTSUM-DDNAMN2                                   
018900     CALL POSTSUM USING POSTSUM-PARM                                      
019000     .                                                                    
019100     EJECT                                                                
019200                                                                          
019210 S12-SKRIV-W0116602 SECTION.                                              
019220                                                                          
019230     WRITE UT-CPAM-POST FROM UT-CPAM-AREA                                 
019240                                                                          
019250     MOVE 'W0116602' TO POSTSUM-FDNAMN                                    
019260     MOVE 'W01166D2' TO POSTSUM-DDNAMN2                                   
019270     CALL POSTSUM USING POSTSUM-PARM                                      
019280     .                                                                    
019290     EJECT                                                                
019291                                                                          
019300                                                                          
019400* --- IMS SEKTIONER ---                                                   
019500     SKIP3                                                                
019600     EJECT                                                                
019700 IMS-GET-WDD2   SECTION.                                                  
019800                                                                          
019900     CALL CBLTDLI USING GN WDD2-PCB DLI-IO-AREA                           
020000     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
020100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
020200     PERFORM IMS-STATUSKONTROLL                                           
020300     .                                                                    
020400     EJECT                                                                
020500 IMS-STATUSKONTROLL SECTION.                                              
020600                                                                          
020700     SET STATUS-IX TO 1                                                   
020800     SEARCH GODK-STATUS                                                   
020900       AT END                                                             
021000         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
021100         DISPLAY FELTEXT                                                  
021200         CALL FELLOG                                                      
021300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
021400         CONTINUE                                                         
021500     END-SEARCH                                                           
021600     .                                                                    
