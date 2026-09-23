000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4794800.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   15/05/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM SELECTS RECORDS FOR PREVIOUS MONTH AND              
001000*        WRITES INTO OUTPUT FILE                                          
001100*                                                                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INPUT FILE                                                 
002600     SELECT W47955                     ASSIGN TO W47948D1.                
002700     SKIP2                                                                
002800*          --- OUTPUT FILE WITH RECORDS FROM PREV MONTH                   
002900     SELECT W47948                     ASSIGN TO W47948D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W47955                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W479055      -L.                                               
004000     SKIP3                                                                
004100 FD  W47948                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W47948 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W4794800'.            
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  WS-TIFAKT-BILLIT            PIC 9(6)    VALUE ZEROS.                 
005210 77  WS-IDDC-LEV                 PIC X(2)    VALUE SPACES.                
005220 77  WS-IDDISTR                  PIC 9(5)    VALUE ZEROS.                 
005230 77  WS-INV-VAL                  PIC S9(11)V9(2) COMP-3                   
005231                                             VALUE ZEROS.                 
005232 77  WS-SUM-INV-VAL              PIC S9(11)V9(2) COMP-3                   
005233                                             VALUE ZEROS.                 
005300                                                                          
005400 77  W47955-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W47955                       VALUE 'J'.                   
005510 77  WS-PREV-MTH-REC-SW          PIC X       VALUE 'N'.                   
005520     88  WS-PREV-MTH-REC                     VALUE 'J'.                   
005600     EJECT                                                                
005700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES TODAYS-DATE.                                        
005900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006100     03  TODAYS-DATE-DAY         PIC 9(2).                                
006200     EJECT                                                                
006210 01  WS-PREV-DATE                PIC 9(4)    VALUE ZERO.                  
006220 01  FILLER REDEFINES WS-PREV-DATE.                                       
006400     03  WS-PREV-YEAR            PIC 9(2).                                
006500     03  WS-PREV-MONTH           PIC 9(2).                                
006600     EJECT                                                                
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800*                                                                         
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006910     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
007000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     SKIP2                                                                
007300*    --- PARAMETERS TO ABEND                                              
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007800     SKIP2                                                                
007900 01  ERROR-TEXT.                                                          
008000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
008100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL POSTSUM                                          
008400*                                                                         
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600     EJECT                                                                
008700*01  -COPY WDATAREA                                                       
008800     EJECT                                                                
008900 01  IN-AREA-START               PIC X(24)   VALUE                        
009000                                 'IN-AREA-START  '.                       
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W479055     -PRE IN-                                      
009400     EJECT                                                                
009500 01  UT-AREA-START               PIC X(24)   VALUE                        
009600                                 'UT-AREA-START  '.                       
009700     SKIP2                                                                
009800                                                                          
009900*01  AREA -COPY W47948      -PRE UT-                                      
010000     EJECT                                                                
010100 PROCEDURE DIVISION.                                                      
010200 MAIN SECTION.                                                            
010300                                                                          
010400     PERFORM A-INIT                                                       
010500     PERFORM S01-READ-W47955                                              
010700     PERFORM B-CALC-INV-VAL                                               
011000                                                                          
011100     PERFORM Z-FINIT                                                      
011200                                                                          
011300     MOVE ZERO TO RETURN-CODE                                             
011400     GOBACK                                                               
011500     .                                                                    
011600     EJECT                                                                
011700 A-INIT SECTION.                                                          
011800                                                                          
011900     OPEN INPUT  W47955                                                   
012000                                                                          
012100     OPEN OUTPUT W47948                                                   
012200                                                                          
012300     ACCEPT TODAYS-DATE  FROM DATE                                        
012400     IF TODAYS-DATE-MONTH = 01                                            
012500        COMPUTE WS-PREV-YEAR  = TODAYS-DATE-YEAR - 1                      
012600        COMPUTE WS-PREV-MONTH = 12                                        
012700     ELSE                                                                 
012800        COMPUTE WS-PREV-YEAR  = TODAYS-DATE-YEAR                          
012900        COMPUTE WS-PREV-MONTH = TODAYS-DATE-MONTH - 1                     
013000     END-IF                                                               
013100                                                                          
013200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013300     .                                                                    
013400     EJECT                                                                
013500 B-CALC-INV-VAL SECTION.                                                  
013600                                                                          
013601* FIND THE FIRST RECORD FOR THE PREVIOUS MONTH. CONTINUE READING          
013602* THE INPUT FILE UNTIL FOUND                                              
013604     PERFORM UNTIL WS-PREV-MTH-REC OR END-OF-W47955                       
013605       MOVE IN-TIFAKT-BILLIT      TO WS-TIFAKT-BILLIT                     
013606       IF WS-TIFAKT-BILLIT(1:4) = WS-PREV-DATE                            
013607          MOVE IN-IDDC-LEV        TO WS-IDDC-LEV                          
013608          MOVE IN-IDDISTR         TO WS-IDDISTR                           
013609          COMPUTE WS-INV-VAL = IN-KVLEVART2 * IN-PRARTSTD                 
013610          MOVE WS-INV-VAL         TO WS-SUM-INV-VAL                       
013611          MOVE YES                TO WS-PREV-MTH-REC-SW                   
013612* WHEN FOUND, READ THE NEXT RECORD FROM THE FILE                          
013613          PERFORM S01-READ-W47955                                         
013614       ELSE                                                               
013615          PERFORM S01-READ-W47955                                         
013616       END-IF                                                             
013617     END-PERFORM                                                          
013618                                                                          
013619     PERFORM UNTIL END-OF-W47955                                          
013700       MOVE IN-TIFAKT-BILLIT    TO WS-TIFAKT-BILLIT                       
013900       IF WS-TIFAKT-BILLIT(1:4) = WS-PREV-DATE                            
013901* ROLL UP THE INVOICE VALUE FOR SAME DISTR/DC                             
013910          IF IN-IDDC-LEV = WS-IDDC-LEV                                    
013920          AND IN-IDDISTR = WS-IDDISTR                                     
013921             COMPUTE WS-INV-VAL = IN-KVLEVART2 * IN-PRARTSTD              
013930             COMPUTE WS-SUM-INV-VAL = WS-SUM-INV-VAL +                    
013940                                      WS-INV-VAL                          
013950          ELSE                                                            
013960* WHEN DISTR AND DC CHANGES, WRITE THE ACCUMULATED INVOICE VALUE          
013970* TO OUTPUT FILE                                                          
013992             PERFORM BA-MOVE-UT-FIELDS                                    
013993* START CALCULATING THE INVOICE VALUE FOR THE NEW DISTR AND DC            
014000             MOVE IN-IDDC-LEV   TO WS-IDDC-LEV                            
014100             MOVE IN-IDDISTR    TO WS-IDDISTR                             
014110             MOVE ZERO          TO WS-INV-VAL                             
014120                                   WS-SUM-INV-VAL                         
014200             COMPUTE WS-INV-VAL = IN-KVLEVART2 * IN-PRARTSTD              
014201             MOVE WS-INV-VAL    TO WS-SUM-INV-VAL                         
014210          END-IF                                                          
014500       END-IF                                                             
014531       PERFORM S01-READ-W47955                                            
014540     END-PERFORM                                                          
014541                                                                          
014542* WRITE THE LAST SET OF RECORDS READ BEFORE END OF FILE                   
014543     IF END-OF-W47955 AND WS-SUM-INV-VAL > 0                              
014550        PERFORM BA-MOVE-UT-FIELDS                                         
014560     END-IF                                                               
014600     .                                                                    
014700     EJECT                                                                
014701 BA-MOVE-UT-FIELDS SECTION.                                               
014702                                                                          
014703     MOVE '20'                 TO UT-TIDATUM(1:2)                         
014704     MOVE WS-PREV-DATE         TO UT-TIDATUM(3:4)                         
014710     MOVE WS-IDDC-LEV          TO UT-IDDC-LEV                             
014720     MOVE WS-IDDISTR           TO UT-IDDISTR                              
014730     MOVE WS-SUM-INV-VAL       TO UT-SUARTSTD                             
014740     PERFORM S11-WRITE-W47948                                             
014750     .                                                                    
014760     EJECT                                                                
014800 Z-FINIT SECTION.                                                         
014900     CLOSE W47955                                                         
015000           W47948                                                         
015100     SKIP2                                                                
015200     MOVE 'S' TO POSTSUM-OPKOD                                            
015300     CALL POSTSUM USING POSTSUM-PARM                                      
015400     .                                                                    
015500     EJECT                                                                
015600 S01-READ-W47955  SECTION.                                                
015700     READ W47955 INTO IN-AREA                                             
015800     AT END                                                               
015900        SET END-OF-W47955 TO TRUE                                         
016000                                                                          
016100     NOT AT END                                                           
016200        MOVE 'W47955' TO POSTSUM-FDNAMN                                   
016300        MOVE 'W47948D1' TO POSTSUM-DDNAMN2                                
016400        CALL POSTSUM USING POSTSUM-PARM                                   
016500     END-READ                                                             
016600     .                                                                    
016700     EJECT                                                                
016800 S11-WRITE-W47948 SECTION.                                                
016900                                                                          
017000     WRITE UT-POST FROM UT-AREA                                           
017100                                                                          
017200     MOVE 'W47948' TO POSTSUM-FDNAMN                                      
017300     MOVE 'W47948D2' TO POSTSUM-DDNAMN2                                   
017400     CALL POSTSUM USING POSTSUM-PARM                                      
017500     .                                                                    
017600     EJECT                                                                
017700 S99-ABEND SECTION.                                                       
017800                                                                          
017900     SKIP2                                                                
018000     MOVE 'S' TO POSTSUM-OPKOD                                            
018100     CALL POSTSUM USING POSTSUM-PARM                                      
018200     CALL ABEND USING RKOD-ABEND                                          
018300     .                                                                    
