000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.             W1144000.                                        
000400 AUTHOR.                 P.DAHLÖF.                                        
000500     DATE-WRITTEN.       DECEMBER 1988.                                   
000600*                                                                         
000700     REMARKS.                                                             
000800                                                                          
000900     FUNKTION:                                                            
001000     IMS BMP-PROGRAM I NYPON-SYSTEMET.                                    
001100     INFILEN BESTÅR AV MAX 200 ARTIKLAR                                   
001200     PROGRAMMET UPPDATERAR WLARTG01 SAMT WLARTC01                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*                  FIL FRÅN W11438-PROGRAMMET RUTIN W114D1                
002100     SELECT  W11438-INFIL             ASSIGN TO    W11440D1.              
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP2                                                                
002500 FILE SECTION.                                                            
002600     SKIP2                                                                
002700 FD  W11438-INFIL                                                         
002800     LABEL RECORD STANDARD                                                
002900     RECORDING      F                                                     
003000     BLOCK CONTAINS 0.                                                    
003100     SKIP2                                                                
003200*01  POST -COPY W11438 -PRE W11438- -L                                    
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP2                                                                
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1144000'.               
003800     SKIP2                                                                
003900*    ---- GENERELLA KONSTANTER ---------------------------------          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200*    ---- FLAGGOR / SWITCHAR -----------------------------------          
004300 77  W11438-EOF                  PIC X       VALUE 'N'.                   
004400*    ---- NYCKLAR TILL DLI                                                
004500 01  W-IDARTNR-X.                                                         
004600     03  W-IDARTNR               PIC S9(9) COMP-3 VALUE ZERO.             
004700     EJECT                                                                
004800*--------------------------------------------------------------*          
004900*    SUBPROGRAM OCH PARAMETERAREOR                                        
005000*--------------------------------------------------------------*          
005100     SKIP2                                                                
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
005400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005600     EJECT                                                                
005700*    ---- PARAMETRAR TILL POSTSUM                                         
005800 01  FILLER               PIC X(16)   VALUE  'POSTSUM-AREA'.              
005900*01  -COPY W0005 -PRE POSTSUM-.                                           
006100     EJECT                                                                
006200*--------------------------------------------------------------*          
006300*    W11438-AREA                                               *          
006400*--------------------------------------------------------------*          
006500 01  FILLER             PIC X(16)       VALUE 'W11404-AREA'.              
006600*01  AREA -COPY W11438 -PRE IN-                                           
006800     EJECT                                                                
006900     EJECT                                                                
007000* *********************ARBETSAREOR IMS                                    
007100 01  IMS-WS.                                                              
007200   03  FILLER                    PIC X(8)   VALUE 'IMS-WS'.               
007300   03  STATUS-WS                 PIC X(2).                                
007400     88  SEGMENT-FINNS                      VALUE '  '.                   
007500     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
007600   03  SSA1                      PIC X(64).                               
007700   03  GODK-STATUSKODER.                                                  
007800     05  GODK-STATUS     OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
007900*01  -COPY W0003                                                          
008100     EJECT                                                                
008200*--------------------------------------------------------------*          
008300*    DLI-IO-AREA / IO-AREOR 1, 2, 3 ,4                                    
008400*--------------------------------------------------------------*          
008500 01  DLI-IO-AREA.                                                         
008600   03 IO-AREA-1                  PIC X(600)   VALUE SPACE.                
008700     SKIP2                                                                
008800*  03  WLARTC01 -COPY WDK601              -RED IO-AREA-1.                 
009000     EJECT                                                                
009100     SKIP2                                                                
009200*  03  WLARTG01 -COPY WDD201 -PRE ARTG01- -RED IO-AREA-1.                 
009400     EJECT                                                                
009500 LINKAGE SECTION.                                                         
009600* - - - - - - - - -LOGISK TERMINAL PCB-COPYTEXT FÖR BMP                   
009700*01      -COPY W0008     -PRE MSG-                                        
009900      05 FILLER          PIC X(4).                                        
010000     SKIP2                                                                
010100*01      -COPY W0008     -PRE ARTC-                                       
010300      05 FILLER          PIC X(4).                                        
010400     SKIP2                                                                
010500*01      -COPY W0008     -PRE ARTG-                                       
010700      05 FILLER          PIC X(4).                                        
010800     EJECT                                                                
010900 PROCEDURE DIVISION USING MSG-PCB ARTC-PCB  ARTG-PCB.                     
011000     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB  ARTG-PCB.                    
011100     SKIP2                                                                
011200     PERFORM A-INIT                                                       
011300     PERFORM S01-LAES-W11438-INFIL                                        
011400     PERFORM UNTIL W11438-EOF = JA                                        
011500        MOVE IN-IDARTNR                   TO W-IDARTNR                    
011600        PERFORM IMS-GHU-ARTG01                                            
011700        IF SEGMENT-FINNS                                                  
011800           MOVE IN-TIFINLEV-AAMMDD        TO ARTG01-ART-DAFINLEV          
011801           IF IN-TIFINLEV-AAMMDD NOT = ZERO                               
011810              IF IN-TIFINLEV-AAMMDD < 500000                              
011820                 MOVE 20 TO ARTG01-ART-DAFINLEV (1:2)                     
011830              ELSE                                                        
011831                 IF IN-TIFINLEV-AAMMDD < 999999                           
011840                    MOVE 19 TO ARTG01-ART-DAFINLEV (1:2)                  
011841                 ELSE                                                     
011842                    MOVE 99999999 TO ARTG01-ART-DAFINLEV                  
011843                 END-IF                                                   
011850              END-IF                                                      
011860           END-IF                                                         
011900           PERFORM IMS-REPL-ARTG                                          
012000        END-IF                                                            
012100        PERFORM IMS-GHU-ARTC01                                            
012200        IF SEGMENT-FINNS                                                  
012300           MOVE IN-TIFINLEV-AAVVD         TO ART-TIFINLV                  
012400           PERFORM IMS-REPL-ARTC                                          
012500        END-IF                                                            
012600        PERFORM S01-LAES-W11438-INFIL                                     
012700     END-PERFORM                                                          
012800     PERFORM Z-FINIT                                                      
012900     MOVE ZERO                            TO RETURN-CODE                  
013000     GOBACK                                                               
013100     .                                                                    
013200 A-INIT   SECTION.                                                        
013300     SKIP2                                                                
013400     OPEN INPUT W11438-INFIL                                              
013500     .                                                                    
013600     EJECT                                                                
013700 S01-LAES-W11438-INFIL SECTION.                                           
013800     SKIP2                                                                
013900     READ W11438-INFIL INTO IN-AREA                                       
014000        AT END MOVE JA TO W11438-EOF                                      
014100     END-READ                                                             
014200     IF W11438-EOF = NEJ                                                  
014300        MOVE 'W11440D1'  TO  POSTSUM-DDNAMN2                              
014400        MOVE 'W11440'    TO  POSTSUM-FDNAMN                               
014500        MOVE 'IN-'       TO  POSTSUM-TRANSTYP                             
014600        CALL POSTSUM USING POSTSUM-PARM                                   
014700     END-IF                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 Z-FINIT   SECTION.                                                       
015100     SKIP2                                                                
015200     CLOSE   W11438-INFIL                                                 
015300     MOVE 'S' TO POSTSUM-OPKOD                                            
015400     CALL POSTSUM USING POSTSUM-PARM                                      
015500     .                                                                    
015600     EJECT                                                                
015700* IMS SECTIONER                                                           
015800     SKIP2                                                                
015900 IMS-GHU-ARTC01   SECTION.                                                
016000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
016100            DELIMITED BY SIZE INTO SSA1                                   
016200     MOVE '  GE' TO GODK-STATUSKODER                                      
016300     CALL CBLTDLI USING GHU ARTC-PCB IO-AREA-1 SSA1                       
016400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016500     PERFORM IMS-STATUSKONTROLL                                           
016600     .                                                                    
016700     SKIP2                                                                
016800 IMS-REPL-ARTC   SECTION.                                                 
016900     MOVE   '  '  TO GODK-STATUSKODER                                     
017000     CALL CBLTDLI USING REPL ARTC-PCB IO-AREA-1                           
017100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
017200     PERFORM IMS-STATUSKONTROLL                                           
017300     .                                                                    
017400     EJECT                                                                
017500 IMS-GHU-ARTG01 SECTION.                                                  
017600     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
017700            DELIMITED BY SIZE INTO SSA1                                   
017800     MOVE '  GE' TO GODK-STATUSKODER                                      
017900     CALL CBLTDLI USING GHU ARTG-PCB IO-AREA-1 SSA1                       
018000     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
018100     PERFORM IMS-STATUSKONTROLL                                           
018200     .                                                                    
018300     EJECT                                                                
018400 IMS-REPL-ARTG   SECTION.                                                 
018500     MOVE   '  '  TO GODK-STATUSKODER                                     
018600     CALL CBLTDLI USING REPL ARTG-PCB IO-AREA-1                           
018700     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
018800     PERFORM IMS-STATUSKONTROLL                                           
018900     .                                                                    
019000     EJECT                                                                
019100 IMS-STATUSKONTROLL SECTION.                                              
019200     SET STATUS-IX TO 1                                                   
019300     SEARCH GODK-STATUS                                                   
019400       AT END CALL FELLOG                                                 
019500       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
019600     END-SEARCH                                                           
019700     .                                                                    
