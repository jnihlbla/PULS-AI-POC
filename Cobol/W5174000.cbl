000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    W5174000.                                                 
000400 AUTHOR.        JAN PETTERSSON                                            
000500 DATE-WRITTEN.  NOV 1987.                                                 
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*     VECKORAPPORT FÖR VOLVO CAR PARTS                                    
001200*     SÄTTER MARKNADSKOD PÅ UTFIL (96 MARKNADER)                          
001300*            MED HJÄLP AV SUBPROGRAM W510MARK.                            
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600                                                                          
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*--- INFIL:                                                               
002200                                                                          
002300     SELECT W51717                       ASSIGN TO UT-S-W51740D1.         
002400                                                                          
002500*--- UTFILER:                                                             
002600                                                                          
002700     SELECT W51741                       ASSIGN TO UT-S-W51740D2.         
002800                                                                          
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100                                                                          
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W51717                                                               
003500     LABEL RECORD   STANDARD                                              
003600     RECORDING      V                                                     
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900*    -COPY  W517RW1   -L.                                                 
004000*    -COPY  W517RW2   -L.                                                 
004100*    -COPY  W517RW9   -L.                                                 
004200     SKIP3                                                                
004300 FD  W51741                                                               
004400     LABEL RECORD   STANDARD                                              
004500     RECORDING      V                                                     
004600     BLOCK CONTAINS 0.                                                    
004700                                                                          
004800*01  UT1-W517RW1 -COPY  W517RW1 -L.                                       
004900*01  UT1-W517RW2 -COPY  W517RW2 -L.                                       
005000*01  UT1-W517RW9 -COPY  W517RW9 -L.                                       
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400*    -- CHECKED BY WY2000                                                 
005500 77  IDPGM                       PIC X(8)    VALUE 'W5174000'.            
005600 77  RETURKOD                    PIC S9(3)   VALUE ZERO COMP-3.           
005700                                                                          
005800 01  GENERELLA-KONSTANTER.                                                
005900     03  JA                      PIC X(1)    VALUE 'J'.                   
006000     03  NEJ                     PIC X(1)    VALUE 'N'.                   
006100                                                                          
006200 01  END-OF-FILE-SWITCHAR.                                                
006300                                                                          
006400     03  REGPOST-FINNS-SW        PIC X(1)    VALUE 'J'.                   
006500         88  REGPOST-FINNS                   VALUE 'J'.                   
006600                                                                          
006700*01  -COPY WWPRODSL                                                       
006800                                                                          
006900 01  GENERELLA-SUBPROGRAM.                                                
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007200     03  W510MARK                PIC X(8)    VALUE 'W510MARK'.            
007300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007400     EJECT                                                                
007500 01  FILLER                          PIC X(16) VALUE 'W510MARK '.         
007600*                                                                         
007700*01  -COPY W510MARK                                                       
007800     EJECT                                                                
007900 01  WDATUM                          PIC X(6)  VALUE 'WDATUM'.            
008000                                                                          
008100*01  -COPY WDATKORT                                                       
008200     EJECT                                                                
008300 01  FILLER                          PIC X(16) VALUE 'REG-AREA'.          
008400                                                                          
008500 01  REG-AREA.                                                            
008600     03  REGAREA.                                                         
008700        05  REG-IDPTYP              PIC X(3)    VALUE SPACE.              
008800        05  REG-KDWRTYP             PIC 9(4).                             
008900        05  REG-IDDC                PIC X(2).                             
009000        05  REG-IDDISTR             PIC S9(5)   COMP-3.                   
009100        05  REG-KDPRODSL            PIC S9(3)   COMP-3.                   
009200        05  REG-DAVVREG             PIC 9(6).                             
009300        05  FILLER                  PIC X(34)   VALUE SPACE.              
009400        EJECT                                                             
009500*    03 AREA    -PRE RW9-  -COPY W517RW9  -RED REGAREA.                   
009600        EJECT                                                             
009700*    03 AREA    -PRE RW1-  -COPY W517RW1  -RED REGAREA.                   
009800        EJECT                                                             
009900*    03 AREA    -PRE RW2-  -COPY W517RW2  -RED REGAREA.                   
010000        EJECT                                                             
010100*                                                                         
010200*--- PARAMETRAR TILL POSTSUM                                              
010300*                                                                         
010400*01  -COPY  W0005        -PRE  POSTSUM-.                                  
010500     EJECT                                                                
010600 PROCEDURE DIVISION.                                                      
010700                                                                          
010800     OPEN INPUT  W51717                                                   
010900          OUTPUT W51741                                                   
011000                                                                          
011100     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
011200                                                                          
011300     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
011400                                                                          
011500     PERFORM S01-LAS-W51717                                               
011600                                                                          
011700     PERFORM UNTIL NOT REGPOST-FINNS                                      
011800       MOVE RW1-KDPRODSL         TO TEST-KDPRODSL                         
011900       IF KDPRODSL-VOLVO-BIMA                                             
012000         IF REG-KDWRTYP = '0110' OR                                       
012100                          '0310' OR                                       
012200                          '0350' OR                                       
012300                          '0550' OR                                       
012400                          '0421' OR                                       
012500                          '0422' OR                                       
012600                          '1110' OR                                       
012700                          '0999' OR                                       
012800                          '1999'                                          
012900           PERFORM B-SKAPA-W51741                                         
013000         END-IF                                                           
013100       END-IF                                                             
013200       PERFORM S01-LAS-W51717                                             
013300     END-PERFORM                                                          
013400                                                                          
013500                                                                          
013600     PERFORM Z-FINIT                                                      
013700                                                                          
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 B-SKAPA-W51741 SECTION.                                                  
014300                                                                          
014400     IF REG-IDPTYP = 'RW1'                                                
014500         IF REG-KDWRTYP NOT = '1110'                                      
014600***     BUDGETTRANSAR HAR REDAN MARKNADSKOD      *****                    
014700           MOVE +0               TO MARK-KDCALL                           
014800           MOVE REG-IDDISTR      TO MARK-IDDISTR                          
014900           CALL W510MARK USING MARK-W510MARK                              
015000           MOVE MARK-KDMARK-BUDG TO RW1-KDMARK                            
015100         END-IF                                                           
015200                                                                          
015300         WRITE UT1-W517RW1 FROM RW1-AREA                                  
015400     ELSE                                                                 
015500        IF REG-IDPTYP = 'RW2'                                             
015600           WRITE UT1-W517RW2 FROM RW2-AREA                                
015700        ELSE                                                              
015800           WRITE UT1-W517RW9 FROM RW9-AREA                                
015900        END-IF                                                            
016000     END-IF                                                               
016100                                                                          
016200     MOVE 'W51741' TO POSTSUM-FDNAMN                                      
016300     MOVE 'W51740D2' TO POSTSUM-DDNAMN2                                   
016400     MOVE REG-IDPTYP TO POSTSUM-TRANSTYP                                  
016500     CALL POSTSUM USING POSTSUM-PARM                                      
016600     .                                                                    
016700     EJECT                                                                
016800 Z-FINIT SECTION.                                                         
016900                                                                          
017000     CLOSE W51717                                                         
017100           W51741                                                         
017200                                                                          
017300     MOVE 'S' TO POSTSUM-OPKOD                                            
017400     CALL POSTSUM USING POSTSUM-PARM                                      
017500     .                                                                    
017600     SKIP3                                                                
017700 S01-LAS-W51717 SECTION.                                                  
017800                                                                          
017900     READ W51717 INTO REG-AREA                                            
018000     AT END                                                               
018100        MOVE NEJ TO REGPOST-FINNS-SW                                      
018200     NOT AT END                                                           
018300        MOVE 'W51717' TO POSTSUM-FDNAMN                                   
018400        MOVE 'W51740D1' TO POSTSUM-DDNAMN2                                
018500        MOVE REG-IDPTYP TO POSTSUM-TRANSTYP                               
018600        CALL POSTSUM USING POSTSUM-PARM                                   
018700     END-READ                                                             
018800     .                                                                    
