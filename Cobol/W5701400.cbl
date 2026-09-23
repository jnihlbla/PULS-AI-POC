000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5701400.                                                
000300 AUTHOR.         BARSHARANI BISHOYE.                                      
000400 DATE-WRITTEN.   22/09/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        RÄKNAR ANTALET VERIFIKAT SOM SKICKATS TILL SAP IDAG              
001000*                                                                         
001100*                                                                         
001200*    ABENDCODES:                                                          
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
002400*          --- INFILE FROM ALL MARKET                                     
002500     SELECT W57014                     ASSIGN TO W57014D1.                
002600     SKIP2                                                                
002700*          --- COMBINED OUTPUT FOR ALL MARKET                             
002800     SELECT W57014A                    ASSIGN TO W57014D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W57014                                                               
003500     RECORDING       V                                                    
003600     BLOCK CONTAINS  0.                                                   
003610 01  INPOST                      PIC X(751).                              
003700     SKIP3                                                                
003800 FD  W57014A                                                              
003900     RECORDING       V                                                    
004000     BLOCK CONTAINS  0.                                                   
004100 01  UTPOST                      PIC X(64).                               
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W5701400'.            
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800 77  IN-EOF                      PIC X       VALUE 'N'.                   
004900 77  WS-COUNTER                  PIC 9(5)    VALUE ZERO.                  
004901 77  WS-COUNTER-TOT              PIC 9(5)    VALUE ZERO.                  
004910 77  WS-RUBRIK                   PIC X(34)   VALUE                        
005000                       ' VC  , NUMBER OF VERIFICAT TO SAP '.              
005001 77  WS-COUNTRY                  PIC X(4).                                
005002 77  WS-PREV-RECORD              PIC X(4).                                
005003 77  FIRST-READ-SW               PIC X       VALUE 'Y'.                   
005010 01  W001-DAP.                                                            
005020     03  FILLER                  PIC X(165)  VALUE SPACE.                 
005200 77  END-OF-W57014               PIC X       VALUE 'N'.                   
005300     EJECT                                                                
005400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES TODAYS-DATE.                                        
005600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005800     03  TODAYS-DATE-DAY         PIC 9(2).                                
005900     EJECT                                                                
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100*                                                                         
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     SKIP2                                                                
006500*    --- PARAMETERS TO ABEND                                              
006600                                                                          
006700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007000     SKIP2                                                                
007100 01  ERROR-TEXT.                                                          
007200     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL POSTSUM                                          
007600*                                                                         
007700*01  -COPY W0005   -PRE  POSTSUM-                                         
007800     EJECT                                                                
007900 01  IN1-AREA-START              PIC X(24)  VALUE                         
008000                                 'IN1-AREA-START  '.                      
008100     SKIP2                                                                
008200 01  IN1-AREA.                                                            
008301     03  IN1-IDPTYP              PIC X(3).                                
008310     03  IN1-KDTRADP             PIC X(4).                                
008320     03  IN1-HEAD                PIC X(10).                               
008330     03  FILLER                  PIC X(734).                              
008400     EJECT                                                                
011000                                                                          
011100 01  UT-AREA-START               PIC X(24)   VALUE                        
011200                                 'UT-AREA-START  '.                       
011300     SKIP2                                                                
011400 01  UT-AREA.                                                             
011500     03  UT-RUBRIK               PIC X(34).                               
011600     03  UT-DATUM                PIC X(8).                                
011700     03  UT-ANTAL                PIC X(5).                                
011710     03  FILLER                  PIC X.                                   
011800     03  UT-ST                   PIC X(3).                                
011900     EJECT                                                                
012000 PROCEDURE DIVISION.                                                      
012100 MAIN SECTION.                                                            
012200     SKIP2                                                                
012300                                                                          
012400     PERFORM A-INIT                                                       
012500     PERFORM S01-READ-W57014                                              
012600     PERFORM UNTIL END-OF-W57014 = YES                                    
012602       IF IN1-HEAD = SPACE                                                
012700*        IF IN1-IDPTYP = '200' OR '300' OR '600'                          
012710           IF IN1-KDTRADP    NOT = WS-PREV-RECORD                         
012720           AND FIRST-READ-SW  = 'N'                                       
012722             MOVE WS-COUNTER  TO WS-COUNTER-TOT                           
012740             MOVE 1           TO WS-COUNTER                               
012750           ELSE                                                           
012800             COMPUTE WS-COUNTER  = WS-COUNTER + 1                         
012810           END-IF                                                         
012900           MOVE TODAYS-DATE TO UT-DATUM                                   
013020           MOVE WS-PREV-RECORD(1:2)   TO WS-RUBRIK(4:2)                   
013050           MOVE WS-RUBRIK TO UT-RUBRIK                                    
013100           MOVE WS-COUNTER-TOT TO UT-ANTAL                                
013200           MOVE 'QTY'  TO  UT-ST                                          
013201           IF FIRST-READ-SW  = 'Y'                                        
013202             MOVE IN1-KDTRADP TO WS-PREV-RECORD                           
013203             MOVE 'N'  TO   FIRST-READ-SW                                 
013206           END-IF                                                         
013210           IF IN1-KDTRADP    NOT = WS-PREV-RECORD                         
013220           AND IN1-KDTRADP(1:2) NOT = WS-PREV-RECORD(1:2)                 
013221             PERFORM S10-WRITE-DAP                                        
013230             MOVE IN1-KDTRADP TO WS-PREV-RECORD                           
013250             PERFORM S11-WRITE-W57014A                                    
013300           END-IF                                                         
013311*        END-IF                                                           
013410       END-IF                                                             
013420       PERFORM S01-READ-W57014                                            
013430       IF END-OF-W57014 = YES                                             
013440         MOVE WS-COUNTER     TO UT-ANTAL                                  
013450         MOVE IN1-KDTRADP  TO WS-PREV-RECORD                              
013451         MOVE TODAYS-DATE TO UT-DATUM                                     
013452         MOVE WS-PREV-RECORD(1:2)   TO WS-RUBRIK(4:2)                     
013453         MOVE WS-RUBRIK TO UT-RUBRIK                                      
013460         PERFORM S10-WRITE-DAP                                            
013470         PERFORM S11-WRITE-W57014A                                        
013480       END-IF                                                             
013610     END-PERFORM                                                          
013700                                                                          
013800                                                                          
013900     PERFORM Z-FINIT                                                      
014000                                                                          
014100     MOVE ZERO TO RETURN-CODE                                             
014200     GOBACK                                                               
014300     .                                                                    
014400     EJECT                                                                
014500 A-INIT SECTION.                                                          
014600                                                                          
014700     OPEN INPUT  W57014                                                   
014800                                                                          
014900     OPEN OUTPUT W57014A                                                  
015000     SKIP2                                                                
015100     ACCEPT TODAYS-DATE  FROM DATE                                        
015300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015400     .                                                                    
015500     EJECT                                                                
015600 Z-FINIT SECTION.                                                         
015700     CLOSE W57014                                                         
015800           W57014A                                                        
015900     SKIP2                                                                
016000     MOVE 'S' TO POSTSUM-OPKOD                                            
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016200     .                                                                    
016300     EJECT                                                                
016400 S01-READ-W57014  SECTION.                                                
016500     READ W57014 INTO IN1-AREA                                            
016600       AT END MOVE YES TO END-OF-W57014                                   
016700     END-READ                                                             
016800     .                                                                    
016900     EJECT                                                                
017000 S11-WRITE-W57014A SECTION.                                               
017100                                                                          
017200     WRITE UTPOST  FROM UT-AREA                                           
017300                                                                          
017400     MOVE 'W57014A'  TO POSTSUM-FDNAMN                                    
017500     MOVE 'W57014D2' TO POSTSUM-DDNAMN2                                   
017600     CALL POSTSUM USING POSTSUM-PARM                                      
017700     .                                                                    
017800     EJECT                                                                
017810 S10-WRITE-DAP SECTION.                                                   
017820                                                                          
017830     MOVE ' ¤DAPW57014-001' TO W001-DAP                                   
017840     WRITE UTPOST     FROM W001-DAP                                       
017850                                                                          
017860     MOVE SPACES            TO W001-DAP                                   
017870                                                                          
017880     STRING ' ¤DAP' WS-PREV-RECORD                                        
017890            DELIMITED BY SIZE INTO W001-DAP                               
017891     WRITE UTPOST     FROM W001-DAP                                       
017892     MOVE SPACES            TO W001-DAP                                   
017893     .                                                                    
017894     EJECT                                                                
017900 S99-ABEND SECTION.                                                       
018000                                                                          
018100     SKIP2                                                                
018200     MOVE 'S' TO POSTSUM-OPKOD                                            
018300     CALL POSTSUM USING POSTSUM-PARM                                      
018400     CALL ABEND USING RKOD-ABEND                                          
018500     .                                                                    
