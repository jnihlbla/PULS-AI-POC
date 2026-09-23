000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4763700.                                                
000300 AUTHOR.         MOGREN STINA.                                            
000400 DATE-WRITTEN.   04/10/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER W47635 , SORTERADE POSTER                       
000900*        PLOCKAR AV SORTERINGS-BEGREPP                                    
001000*        SKRIVS PÅ UTFIL                                                  
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*     --- INPOSTER FRÅN W4762500 SOM SKALL KLÄS AV                        
002000     SELECT W47635                     ASSIGN TO W47637D1.                
002100     SKIP2                                                                
002200*     --- RENSAD UTFIL                                                    
002300     SELECT W47637                     ASSIGN TO W47637D2.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W47635                                                               
003000     RECORDING       V                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W4763501     -L.                                               
003400     SKIP3                                                                
003500 FD  W47637                                                               
003600     RECORDING       V                                                    
003700     BLOCK CONTAINS  0.                                                   
003800*01  POST   -COPY W47584H  -PRE BRH- -L.                                  
003900*01  POST   -COPY W47584O  -PRE BRO- -L.                                  
004000*01  POST   -COPY W47584P  -PRE BRP- -L.                                  
004100*01  POST   -COPY W47584T  -PRE BRT- -L.                                  
004200*01  POST   -COPY W47584DU -PRE BRD- -L.                                  
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W4763700'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  W47635-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W47635                       VALUE 'J'.                   
005200                                                                          
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005700                                                                          
005800*    --- PARAMETRAR TILL ABEND                                            
005900                                                                          
006000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006300     EJECT                                                                
006400 01  FILLER                     PIC X(16)  VALUE 'NYCKLAR'.               
006500 01  W-IDFAKT                   PIC S9(7)  VALUE ZERO  COMP-3.            
006600 01  W-IDDISTR                  PIC S9(5)  VALUE ZERO  COMP-3.            
006700 01  W-IDKUNDNR                 PIC S9(7)  VALUE ZERO  COMP-3.            
006800 01  W-IDORDER                  PIC S9(7)  VALUE ZERO  COMP-3.            
006900 01  W-IDPRODNR                 PIC S9(7)  VALUE ZERO  COMP-3.            
007000 01  W-IDPURAD                  PIC S9(5)  VALUE ZERO  COMP-3.            
007100 01  W-IDKOLLI                  PIC 9(5)   VALUE ZERO.                    
007200 01  W-IDPTYP                   PIC X(3)   VALUE SPACE.                   
007300                                                                          
007400 01  TEST-IDDISTR               PIC 9(5)                COMP-3.           
007500*01  FILLER  -COPY   WWDIST07   -RED TEST-IDDISTR.                        
007600     EJECT                                                                
007700*01  FILLER  -COPY   WWDIST35   -RED TEST-IDDISTR.                        
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16) VALUE 'IN-AREA'.               
008400                                                                          
008500 01  IN-AREA.                                                             
008600*    03  -COPY W4763501                                                   
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16) VALUE 'UT-AREA'.               
008900                                                                          
009000 01  FILLER.                                                              
009100     03  UT-AREA-USA             PIC X(250) VALUE SPACE.                  
009200                                                                          
009300*    03  -COPY W47584H    -PRE BRHU-  -RED UT-AREA-USA                    
009400                                                                          
009500*    03  -COPY W47584O    -PRE BROU-  -RED UT-AREA-USA                    
009600                                                                          
009700*    03  -COPY W47584P    -PRE BRPU-  -RED UT-AREA-USA                    
009800                                                                          
009900*    03  -COPY W47584T    -PRE BRTU-  -RED UT-AREA-USA                    
010000                                                                          
010100*    03  -COPY W47584DU   -PRE BRDU-  -RED UT-AREA-USA                    
010200                                                                          
010300     03  UT-AREA                 PIC X(250) VALUE SPACE.                  
010400                                                                          
010500*    03  -COPY W47584H    -PRE BRH-  -RED UT-AREA                         
010600                                                                          
010700*    03  -COPY W47584O    -PRE BRO-  -RED UT-AREA                         
010800                                                                          
010900*    03  -COPY W47584P    -PRE BRP-  -RED UT-AREA                         
011000                                                                          
011100*    03  -COPY W47584T    -PRE BRT-  -RED UT-AREA                         
011200                                                                          
011300*    03  -COPY W47584D    -PRE BRD-  -RED UT-AREA                         
011400     EJECT                                                                
011500 PROCEDURE DIVISION.                                                      
011600 MAIN SECTION.                                                            
011700                                                                          
011800     PERFORM A-INIT                                                       
011900                                                                          
012000     PERFORM S01-LAES-W47635                                              
012100     PERFORM UNTIL END-OF-W47635                                          
012200                                                                          
012300       EVALUATE BR-IDPTYP                                                 
012400        WHEN '1  '                                                        
012500           MOVE BR-FILLER         TO BRH-W47584H                          
012600        WHEN '2  '                                                        
012700           MOVE BR-FILLER         TO BRO-W47584O                          
012800        WHEN '3  '                                                        
012900           MOVE BR-FILLER         TO BRP-W47584P                          
013000        WHEN '4  '                                                        
013100           MOVE BR-FILLER         TO BRT-W47584T                          
013200        WHEN '5  '                                                        
013420           MOVE BR-FILLER         TO BRD-W47584D                          
013430           MOVE BR-FILLER         TO BRDU-W47584D                         
013500       END-EVALUATE                                                       
013600                                                                          
013700       PERFORM S11-SKRIV-W47637                                           
013800       PERFORM S01-LAES-W47635                                            
013900                                                                          
014000     END-PERFORM                                                          
014100                                                                          
014200     PERFORM Z-FINIT                                                      
014300                                                                          
014400     MOVE ZERO TO RETURN-CODE                                             
014500     GOBACK                                                               
014600     .                                                                    
014700     EJECT                                                                
014800 A-INIT SECTION.                                                          
014900                                                                          
015000     OPEN INPUT  W47635                                                   
015100     OPEN OUTPUT W47637                                                   
015200                                                                          
015300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015400     .                                                                    
015500     EJECT                                                                
015600 Z-FINIT SECTION.                                                         
015700                                                                          
015800     CLOSE W47635                                                         
015900           W47637                                                         
016000                                                                          
016100     MOVE 'S' TO POSTSUM-OPKOD                                            
016200     CALL POSTSUM USING POSTSUM-PARM                                      
016300     .                                                                    
016400     EJECT                                                                
016500 S01-LAES-W47635  SECTION.                                                
016600                                                                          
016700     READ W47635 INTO IN-AREA                                             
016800     AT END                                                               
016900        SET END-OF-W47635 TO TRUE                                         
017000     NOT AT END                                                           
017100        MOVE 'W47635'       TO POSTSUM-FDNAMN                             
017200        MOVE 'W47637D1'     TO POSTSUM-DDNAMN2                            
017300        MOVE BR-IDPTYP      TO POSTSUM-TRANSTYP                           
017400        CALL POSTSUM USING POSTSUM-PARM                                   
017500     END-READ                                                             
017600     .                                                                    
017700     EJECT                                                                
017800 S11-SKRIV-W47637 SECTION.                                                
017900                                                                          
018000     EVALUATE BR-IDPTYP                                                   
018100       WHEN '1  '                                                         
018200          WRITE BRH-POST FROM BRH-W47584H                                 
018300       WHEN '2  '                                                         
018400          WRITE BRO-POST FROM BRO-W47584O                                 
018500       WHEN '3  '                                                         
018600          WRITE BRP-POST FROM BRP-W47584P                                 
018700       WHEN '4  '                                                         
018800          WRITE BRT-POST FROM BRT-W47584T                                 
018900       WHEN '5  '                                                         
019000          MOVE BRD-IDDISTR TO TEST-IDDISTR                                
019010          IF DIST07-USA-CUSTOMERS    OR                                   
019020             DIST35-CDC-NDC41-REFILL OR                                   
019022             DIST35-CDC-NDC43-REFILL OR                                   
019023             DIST35-CDC-NDC44-REFILL OR                                   
019024             DIST35-CDC-NDC45-REFILL OR                                   
019025             DIST35-CDC-NDC46-REFILL OR                                   
019026             DIST35-CDC-NDC47-REFILL                                      
019030            WRITE BRD-POST FROM BRDU-W47584D                              
019031          ELSE                                                            
019040            WRITE BRD-POST FROM BRD-W47584D                               
019050          END-IF                                                          
019100     END-EVALUATE                                                         
019200                                                                          
019300     MOVE BR-IDPTYP         TO POSTSUM-TRANSTYP                           
019400     MOVE 'W47637'          TO POSTSUM-FDNAMN                             
019500     MOVE 'W47637D2'        TO POSTSUM-DDNAMN2                            
019600     CALL POSTSUM USING POSTSUM-PARM                                      
019700     .                                                                    
