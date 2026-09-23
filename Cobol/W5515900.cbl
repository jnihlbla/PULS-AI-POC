000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W5515900.                                        
000400 AUTHOR.                 KARL JOHAN HANSSON.                              
000500 DATE-WRITTEN.           AUGUSTI 1994.                                    
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR ETT LADDPROGRAM FÖR SPIS BASEN WDC6                
001100*                                                                         
001200*        - W55158, INFIL MED LADDTRANSAKTIONER.                           
001300*                                                                         
001400*                                                                         
001500*       W55158-FILEN LÄSES OCH OCH BASEN WDC6 LADDAS MED DESSA            
001600*                                                                         
001900     SKIP2                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*    ---- INFIL MED LADD POSTER:                                          
002700     SELECT  W55159        ASSIGN  W55159D1.                              
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W55159                                                               
003800     LABEL RECORD STANDARD                                                
003900     RECORDING  F                                                         
004000     BLOCK CONTAINS 0.                                                    
004100                                                                          
004200*    -COPY W55122      -L.                                                
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                   PIC X(8) VALUE 'W5515900'.                   
004800                                                                          
004900 77  JA                      PIC X       VALUE 'J'.                       
005000 77  NEJ                     PIC X       VALUE 'N'.                       
005100                                                                          
005200 77  INFIL-EOF               PIC X       VALUE 'N'.                       
005300                                                                          
005400 01  WS-1000-ISRT-OK         PIC S9(7)   VALUE ZERO    COMP-3.            
005500 01  WS-ANT-ISRT-OK          PIC S9(7)   VALUE ZERO    COMP-3.            
005600 01  WS-ANT-LADDTRANSAR      PIC S9(7)   VALUE ZERO    COMP-3.            
005700                                                                          
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
006000   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006100   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006300                                                                          
006400 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
006500     EJECT                                                                
006600 01  FILLER                  PIC X(24) VALUE 'IN-AREA-START'.             
006700                                                                          
006800*01  AREA -COPY W55122    -PRE  ART-.                                     
006900     EJECT                                                                
007000 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
007100     SKIP3                                                                
007200*    ---- STATUSKOD FRÅN IMS                                              
007300                                                                          
007400 01  STATUS-WS               PIC XX.                                      
007500     88  INSERT-OK                        VALUE '  '.                     
007600     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
007700     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
007800     SKIP3                                                                
007900 01  GODK-STATUSKODER.                                                    
008000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
008100     SKIP3                                                                
008200 01  SSA1                    PIC X(32).                                   
008300     EJECT                                                                
008400*01  -COPY W0003                                                          
008500     EJECT                                                                
008600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
008700     SKIP3                                                                
008800 01  DLI-IO-AREA.                                                         
008900*  03  IO-AREA -COPY WDC601    .                                          
009000     EJECT                                                                
009100 LINKAGE SECTION.                                                         
009200     SKIP2                                                                
009300*01  -COPY W0008      -PRE  WDC6-                                         
009400       05  FILLER                PIC X.                                   
009500     EJECT                                                                
009600 PROCEDURE DIVISION  USING  WDC6-PCB.                                     
009700     ENTRY 'DLITCBL' USING  WDC6-PCB.                                     
009800                                                                          
009900     OPEN INPUT  W55159                                                   
010000                                                                          
010100                                                                          
010200     PERFORM S01-LAS-INPOST                                               
010300     PERFORM UNTIL INFIL-EOF = JA                                         
010400                                                                          
010500         MOVE CORR ART-W55122 TO ART-WDC601                               
010600         PERFORM IMS-INSERT-WDC6-SEGMENT                                  
010700                                                                          
010800         IF INSERT-OK                                                     
010900           ADD +1        TO WS-ANT-ISRT-OK                                
011000                            WS-1000-ISRT-OK                               
011100           IF WS-1000-ISRT-OK > 999                                       
011200             DISPLAY ' ***====> 1000 NYA SEGMENT LADDADE,'                
011300                     ' TOTALT LADDADE: ' WS-ANT-ISRT-OK                   
011400             MOVE ZERO TO WS-1000-ISRT-OK                                 
011500           END-IF                                                         
011600         END-IF                                                           
011700         PERFORM S01-LAS-INPOST                                           
011800     END-PERFORM                                                          
012000                                                                          
012100     CLOSE  W55159                                                        
012200                                                                          
012300     DISPLAY '**** ANTAL LADDNINGSTRANSAR ' WS-ANT-LADDTRANSAR            
012400     DISPLAY '**** ANTAL LADDADE SEGMENT  ' WS-ANT-ISRT-OK                
012500                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
015100     EJECT                                                                
015200 S01-LAS-INPOST     SECTION.                                              
015300                                                                          
015400     READ W55159 INTO ART-AREA                                            
015500     AT END                                                               
015600        MOVE JA       TO INFIL-EOF                                        
015700     NOT AT END                                                           
015800        ADD +1        TO WS-ANT-LADDTRANSAR                               
015900     END-READ                                                             
016000     .                                                                    
016100     EJECT                                                                
016200*    ---- IMS SEKTIONER                                                   
016300                                                                          
016400 IMS-INSERT-WDC6-SEGMENT SECTION.                                         
016500                                                                          
016600     MOVE 'WDC601   '           TO SSA1                                   
016700     MOVE '  IILB'              TO GODK-STATUSKODER                       
016800     CALL CBLTDLI USING ISRT    WDC6-PCB DLI-IO-AREA SSA1                 
016900     MOVE WDC6-STATUS-CODE      TO STATUS-WS                              
017000     PERFORM IMS-STATUSKONTROLL                                           
017100     .                                                                    
017200     SKIP3                                                                
017300 IMS-STATUSKONTROLL SECTION.                                              
017400                                                                          
017500     SET STATUS-IX TO 1                                                   
017600     SEARCH GODK-STATUS                                                   
017700       AT END CALL FELLOG                                                 
017800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
017900     END-SEARCH                                                           
018000     .                                                                    
