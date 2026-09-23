000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6114600.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   98/05/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        TEST                                                             
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLETIA (WDK3)                              
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
002400*          --- BORTTAG OCH NYUPPLÄGG                                      
002500     SELECT W61144                     ASSIGN TO W61146D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W61144                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY W61144       -L.                                               
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W6114600'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  W-CHKP-RAKNARE              PIC S9(5)   VALUE +0    COMP-3.          
004500 77  W-CHKP-MAX                  PIC S9(5)   VALUE +800  COMP-3.          
004600 77  CHKP-ID                     PIC X(8)    VALUE 'W6114600'.            
004700 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
004800 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
004900 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
005000 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
005100                                                                          
005200 77  W61144-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W61144                       VALUE 'J'.                   
005400     EJECT                                                                
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     SKIP2                                                                
006800*    --- PARAMETRAR TILL ABEND                                            
006900                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
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
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                 'IN-AREA-START  '.                       
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W61144      -PRE IN-                                      
008700     EJECT                                                                
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
010000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
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
011300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK301'.                      
011400 01  DLI-IO-WDK301.                                                       
011500*    03  -COPY WDK301                                                     
011600     EJECT                                                                
011700 LINKAGE SECTION.                                                         
011800                                                                          
011900*01  -COPY W0009  -PRE MSG-                                               
012000                                                                          
012100*01  -COPY W0008  -PRE WDK3-                                              
012200     05  FILLER                  PIC X.                                   
012300     EJECT                                                                
012400 PROCEDURE DIVISION  USING MSG-PCB WDK3-PCB.                              
012500 MAIN SECTION.                                                            
012600     ENTRY 'DLITCBL' USING MSG-PCB WDK3-PCB.                              
012700                                                                          
012800                                                                          
012900     PERFORM A-INIT                                                       
013000                                                                          
013100     PERFORM S01-LAES-W61144                                              
013200     PERFORM UNTIL END-OF-W61144                                          
013300       EVALUATE IN-IDPTYP                                                 
013400          WHEN 'NEW'                                                      
013500            MOVE IN-IDARTNR    TO  ETI-IDARTNR                            
013600            MOVE FUNCTION CURRENT-DATE(1:8) TO ETI-DAREGDAT               
013700            MOVE ZERO          TO  ETI-IDARTNR-ETIK                       
013800            MOVE SPACE         TO  ETI-IDLAYOUT                           
013900            MOVE 'VO '         TO  ETI-IDSORTIM                           
014000            MOVE 'W612V2'      TO  ETI-IDUSER                             
014100            MOVE SPACE         TO  ETI-TEETIK-INT                         
014200            MOVE ZEROES        TO  ETI-TIUPPDAT                           
014300                                                                          
014400            PERFORM IMS-ISRT-WDK301                                       
014500          WHEN 'DEL'                                                      
014600            MOVE IN-IDARTNR    TO  W-IDARTNR                              
014700            PERFORM IMS-GHU-WDK301                                        
014710            IF SEGMENT-FINNS                                              
014711               PERFORM IMS-DLET-WDK3                                      
014720            END-IF                                                        
014900       END-EVALUATE                                                       
015000                                                                          
015010       IF W-CHKP-RAKNARE              >  W-CHKP-MAX                       
015020          PERFORM IMS-CHECKPOINT                                          
015030          MOVE ZERO                   TO W-CHKP-RAKNARE                   
015040       END-IF                                                             
015050                                                                          
015100       PERFORM S01-LAES-W61144                                            
015200     END-PERFORM                                                          
015300                                                                          
015400     PERFORM Z-FINIT                                                      
015500                                                                          
015600     MOVE ZERO TO RETURN-CODE                                             
015700     GOBACK                                                               
015800     .                                                                    
015900     EJECT                                                                
016000 A-INIT SECTION.                                                          
016100                                                                          
016200     OPEN INPUT  W61144                                                   
016300                                                                          
016310     PERFORM IMS-RESTART                                                  
016320     MOVE ZERO       TO W-CHKP-RAKNARE                                    
016330                                                                          
016400     ACCEPT DAGENS-DATUM  FROM DATE                                       
016500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016600     .                                                                    
016700     EJECT                                                                
016800 Z-FINIT SECTION.                                                         
016900     CLOSE W61144                                                         
017000     SKIP2                                                                
017100     MOVE 'S' TO POSTSUM-OPKOD                                            
017200     CALL POSTSUM USING POSTSUM-PARM                                      
017300     .                                                                    
017400     EJECT                                                                
017500 S01-LAES-W61144  SECTION.                                                
017600     READ W61144 INTO IN-AREA                                             
017700     AT END                                                               
017800        MOVE HIGH-VALUE TO IN-AREA                                        
017900        SET END-OF-W61144 TO TRUE                                         
018000                                                                          
018100     NOT AT END                                                           
018200        MOVE 'W61144' TO POSTSUM-FDNAMN                                   
018300        MOVE 'W61146D1' TO POSTSUM-DDNAMN2                                
018400        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
018500        CALL POSTSUM USING POSTSUM-PARM                                   
018600     END-READ                                                             
018700     .                                                                    
018800     EJECT                                                                
018900 S99-ABEND SECTION.                                                       
019000                                                                          
019100     SKIP2                                                                
019200     MOVE 'S' TO POSTSUM-OPKOD                                            
019300     CALL POSTSUM USING POSTSUM-PARM                                      
019400     CALL ABEND USING RKOD-ABEND                                          
019500     .                                                                    
019600     EJECT                                                                
019700* --- IMS SEKTIONER ---                                                   
019800                                                                          
019900     EJECT                                                                
019910 IMS-RESTART           SECTION.                                           
019920                                                                          
019930     MOVE SPACE TO MSG-IO-AREA-1                                          
019940     MOVE '  ' TO GODK-STATUSKODER                                        
019950     CALL CBLTDLI USING XRST MSG-PCB                                      
019960                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
019970                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
019980     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019990     PERFORM IMS-STATUSKONTROLL                                           
019991     .                                                                    
019992                                                                          
019993     SKIP3                                                                
019994 IMS-CHECKPOINT        SECTION.                                           
019995                                                                          
019996     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
019997     MOVE '  XD' TO GODK-STATUSKODER                                      
019998     CALL CBLTDLI USING CHKP MSG-PCB                                      
019999                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
020000                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
020001     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020002     PERFORM IMS-STATUSKONTROLL                                           
020003     .                                                                    
020004     EJECT                                                                
020010 IMS-GHU-WDK301   SECTION.                                                
020100                                                                          
020200     STRING 'WDK301  (IDARTNR  =' W-IDARTNR-X ')'                         
020300          DELIMITED BY SIZE INTO SSA1                                     
020400     MOVE '  ' TO GODK-STATUSKODER                                        
020500     CALL CBLTDLI USING GHU WDK3-PCB DLI-IO-WDK301 SSA1                   
020600     MOVE WDK3-STATUS-CODE TO STATUS-WS                                   
020700     PERFORM IMS-STATUSKONTROLL                                           
020800     .                                                                    
020900     SKIP3                                                                
021000 IMS-ISRT-WDK301   SECTION.                                               
021100                                                                          
021200     MOVE 'WDK301   ' TO SSA1                                             
021300     MOVE '  ' TO GODK-STATUSKODER                                        
021400     CALL CBLTDLI USING ISRT WDK3-PCB DLI-IO-WDK301 SSA1                  
021500     MOVE WDK3-STATUS-CODE TO STATUS-WS                                   
021600     PERFORM IMS-STATUSKONTROLL                                           
021610     ADD +1    TO W-CHKP-RAKNARE                                          
021700     .                                                                    
021800     SKIP3                                                                
021900 IMS-DLET-WDK3     SECTION.                                               
022000                                                                          
022100     MOVE '  ' TO GODK-STATUSKODER                                        
022200     CALL CBLTDLI USING DLET WDK3-PCB DLI-IO-WDK301                       
022300     MOVE WDK3-STATUS-CODE TO STATUS-WS                                   
022400     PERFORM IMS-STATUSKONTROLL                                           
022410     ADD +1    TO W-CHKP-RAKNARE                                          
022500     .                                                                    
022600     EJECT                                                                
022700 IMS-STATUSKONTROLL SECTION.                                              
022800                                                                          
022900     SET STATUS-IX TO 1                                                   
023000     SEARCH GODK-STATUS                                                   
023100       AT END                                                             
023200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023300           DELIMITED BY SIZE INTO FELTEXT                                 
023400         DISPLAY FELTEXT                                                  
023500         CALL FELLOG                                                      
023600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023700         CONTINUE                                                         
023800     END-SEARCH                                                           
023900     .                                                                    
