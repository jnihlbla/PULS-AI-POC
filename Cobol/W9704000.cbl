000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9704000.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   07/10/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        COMPUTE DATES FOR PREVIOUS OR SPECIFIED WEEK                     
001000*        AND PREPARE SYMBOLIC PARAMETERS FOR SOP                          
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- DATE PARAMETERS FOR SOP                                    
002100     SELECT W97040                     ASSIGN TO W97040D1.                
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W97040                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100 01  OPARM-POST                  PIC X(80).                               
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W9704000'.            
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 01  W-DATE                      PIC X(6).                                
004000 01  W-YYWW1.                                                             
004100     03  W-YYWW                  PIC X(4).                                
004200     03  FILLER                  PIC X       VALUE '1'.                   
004300                                                                          
004400 01  IX                          PIC S9(4)   BINARY.                      
004500 01  IX-DISPL                    PIC 9.                                   
004600                                                                          
004700 01  GENERAL-SUBPROGRAMS.                                                 
004800*                                                                         
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005000     03  WZ20DATE                PIC X(8)    VALUE 'WZ20DATE'.            
005100     SKIP2                                                                
005200*    --- PARAMETERS TO ABEND                                              
005300                                                                          
005400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005700                                                                          
005800     EJECT                                                                
005900*01  -COPY WZ20DATE                                                       
006000                                                                          
006100     EJECT                                                                
006200 01  OPARM-AREA-START            PIC X(24)   VALUE                        
006300                                 'OPARM-AREA-START  '.                    
006400     SKIP2                                                                
006500                                                                          
006600 01  OPARM-AREA                  PIC X(80).                               
006700     EJECT                                                                
006800 LINKAGE SECTION.                                                         
006900                                                                          
007000 01  PARM.                                                                
007100     03  PARM-LENGTH             PIC S9(4)   COMP SYNC.                   
007200     03  PARM-YYWW               PIC X(4).                                
007300     EJECT                                                                
007400 PROCEDURE DIVISION USING PARM.                                           
007500 MAIN SECTION.                                                            
007600                                                                          
007700     PERFORM A-INIT                                                       
007800                                                                          
007900*    -- DATE-TILILDAT IS FIRST DAY OF THE WEEK                            
008000*    -- COMPUTE DATES FOR ALL WEEK                                        
008100     MOVE 1 TO IX                                                         
008200     PERFORM UNTIL IX > 7                                                 
008300                                                                          
008400       MOVE SPACE      TO DATE-TIDATE                                     
008500       MOVE 'YYYYMMDD' TO DATE-KDDATFMT                                   
008600       CALL WZ20DATE USING DATE-WZ20DATE                                  
008700                                                                          
008800*      -- SET YEAR (YYYY) AND DAY (MMDD) AS SYMBOLIC PARMS                
008900       MOVE SPACE TO OPARM-AREA                                           
009000       MOVE IX TO IX-DISPL                                                
009100       STRING 'YYYY' IX-DISPL '(' DATE-TIDATE(1:4) ')'                    
009200             ' MMDD' IX-DISPL '(' DATE-TIDATE(5:4) ')'                    
009300             DELIMITED BY SIZE                                            
009400             INTO OPARM-AREA                                              
009500       WRITE OPARM-POST FROM OPARM-AREA                                   
009600                                                                          
009700       ADD 1 TO IX                                                        
009800       ADD 1 TO DATE-TILILDAT                                             
009900     END-PERFORM                                                          
010000                                                                          
010100*    -- ALSO SET WEEK NUMBER (YYWW) AS SYMBOLIC PARM                      
010200     MOVE SPACE TO OPARM-AREA                                             
010300     STRING 'YYWW(' W-YYWW ')'                                            
010400           DELIMITED BY SIZE                                              
010500           INTO OPARM-AREA                                                
010600     WRITE OPARM-POST FROM OPARM-AREA                                     
010700                                                                          
010800     PERFORM Z-FINIT                                                      
010900                                                                          
011000     MOVE ZERO TO RETURN-CODE                                             
011100     GOBACK                                                               
011200     .                                                                    
011300     EJECT                                                                
011400 A-INIT SECTION.                                                          
011500                                                                          
011600     OPEN OUTPUT W97040                                                   
011700                                                                          
011800     EVALUATE PARM-LENGTH                                                 
011900     WHEN 0                                                               
012000*      -- COMPUTE WEEK NUMBER FOR LAST WEEK                               
012100       ACCEPT W-DATE     FROM DATE                                        
012200       MOVE W-DATE       TO DATE-TIDATE                                   
012300       MOVE 'YYMMDD'     TO DATE-KDDATFMT                                 
012400       CALL WZ20DATE USING DATE-WZ20DATE                                  
012500                                                                          
012600       SUBTRACT 7 FROM DATE-TILILDAT                                      
012700       MOVE SPACE        TO DATE-TIDATE                                   
012800       MOVE 'YYWW'      TO DATE-KDDATFMT                                  
012900       CALL WZ20DATE USING DATE-WZ20DATE                                  
013000                                                                          
013100*      -- COMPUTE FIRST DAY OF THAT WEEK                                  
013200       MOVE DATE-TIDATE TO W-YYWW                                         
013300       MOVE W-YYWW1    TO DATE-TIDATE                                     
013400       MOVE 'YYWWD'    TO DATE-KDDATFMT                                   
013500       CALL WZ20DATE USING DATE-WZ20DATE                                  
013600       MOVE DATE-TIDATE TO W-YYWW                                         
013700                                                                          
013800     WHEN 4                                                               
013900       IF PARM-YYWW  IS NUMERIC                                           
014000*        -- COMPUTE LILIAN DATE FOR FIRST DAY OF SPECIFIED WEEK           
014100         MOVE PARM-YYWW  TO W-YYWW                                        
014200         MOVE W-YYWW1    TO DATE-TIDATE                                   
014300         MOVE 'YYWWD'    TO DATE-KDDATFMT                                 
014400         CALL WZ20DATE USING DATE-WZ20DATE                                
014500         IF DATE-KDRC > 0                                                 
014600            DISPLAY 'WEEK IN EXEC PARM MUST BE IN FORMAT YYWW'            
014700            CALL ABEND USING RKOD-ABEND-NO-DUMP                           
014800         END-IF                                                           
014900       ELSE                                                               
015000          DISPLAY 'WEEK GIVEN IN EXEC PARM IS NOT NUMERIC'                
015100          CALL ABEND USING RKOD-ABEND-NO-DUMP                             
015200       END-IF                                                             
015300     WHEN OTHER                                                           
015400        DISPLAY 'WEEK IN EXEC PARM MUST BE IN FORMAT YYWW'                
015500        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
015600     END-EVALUATE                                                         
015700     .                                                                    
015800                                                                          
015900     EJECT                                                                
016000 Z-FINIT SECTION.                                                         
016100                                                                          
016200     CLOSE W97040                                                         
016300     .                                                                    
