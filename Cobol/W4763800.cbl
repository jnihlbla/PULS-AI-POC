000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4763800.                                                
000300 AUTHOR.         MOGREN STINA.                                            
000400 DATE-WRITTEN.   06/10/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER W47635 , SORTERADE POSTER                       
000900*        PLOCKAR AV SORTERINGS-BEGREPP                                    
001100*        SKRIVS PÅ UTFIL W47637..                                         
001200*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*     --- INPOSTER FRÅN W4762500 SOM SKALL KLÄS AV                        
002400     SELECT W47635                     ASSIGN TO W47638D1.                
002500     SKIP2                                                                
002600*     --- RENSAD UTFIL                                                    
002700     SELECT W47637                     ASSIGN TO W47638D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W47635                                                               
003400     RECORDING       V                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W4763501     -L.                                               
003800     SKIP3                                                                
003900 FD  W47637                                                               
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200*01  POST   -COPY W4756GH  -PRE BRH- -L.                                  
004600*01  POST   -COPY W4756GD  -PRE BRD- -L.                                  
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000 77  IDPGM                       PIC X(8)    VALUE 'W4763800'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300                                                                          
005400 77  W47635-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W47635                       VALUE 'J'.                   
005600                                                                          
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100                                                                          
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     EJECT                                                                
006800 01  FILLER                     PIC X(16)  VALUE 'NYCKLAR'.               
006900 01  W-IDFAKT                   PIC S9(7)  VALUE ZERO  COMP-3.            
007000 01  W-IDDISTR                  PIC S9(5)  VALUE ZERO  COMP-3.            
007100 01  W-IDKUNDNR                 PIC S9(7)  VALUE ZERO  COMP-3.            
007200 01  W-IDORDER                  PIC S9(7)  VALUE ZERO  COMP-3.            
007300 01  W-IDPRODNR                 PIC S9(7)  VALUE ZERO  COMP-3.            
007400 01  W-IDPURAD                  PIC S9(5)  VALUE ZERO  COMP-3.            
007500 01  W-IDKOLLI                  PIC 9(5)   VALUE ZERO.                    
007600 01  W-IDPTYP                   PIC X(3)   VALUE SPACE.                   
007700                                                                          
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16) VALUE 'IN-AREA'.               
008300                                                                          
008400 01  IN-AREA.                                                             
008500*    03  -COPY W4763501                                                   
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16) VALUE 'UT-AREA'.               
008800                                                                          
008900 01  FILLER.                                                              
008910     03  UT-AREA                 PIC X(250) VALUE SPACE.                  
009000                                                                          
009100*    03  -COPY W4756GH    -PRE BRH-  -RED UT-AREA                         
009200                                                                          
009900*    03  -COPY W4756GD    -PRE BRD-  -RED UT-AREA                         
010000     EJECT                                                                
010100 PROCEDURE DIVISION.                                                      
010200 MAIN SECTION.                                                            
010300                                                                          
010400     PERFORM A-INIT                                                       
010500                                                                          
010600     PERFORM S01-LAES-W47635                                              
011200     PERFORM UNTIL END-OF-W47635                                          
011300                                                                          
011400       EVALUATE BR-IDPTYP                                                 
011500        WHEN '1  '                                                        
011501           MOVE BR-FILLER         TO BRH-BROKJPH-W4756GH                  
011508        WHEN '5  '                                                        
011509           MOVE BR-FILLER         TO BRD-BROKJPD-W4756GD                  
011510       END-EVALUATE                                                       
011511***    MOVE BR-FILLER             TO UT-AREA                              
011520       PERFORM S11-SKRIV-W47637                                           
011600       PERFORM S01-LAES-W47635                                            
011610                                                                          
011700     END-PERFORM                                                          
011800                                                                          
011900     PERFORM Z-FINIT                                                      
012000                                                                          
012100     MOVE ZERO TO RETURN-CODE                                             
012200     GOBACK                                                               
012300     .                                                                    
012400     EJECT                                                                
012500 A-INIT SECTION.                                                          
012600                                                                          
012700     OPEN INPUT  W47635                                                   
012800     OPEN OUTPUT W47637                                                   
012900                                                                          
013000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013100     .                                                                    
013200     EJECT                                                                
015200 Z-FINIT SECTION.                                                         
015300                                                                          
015400     CLOSE W47635                                                         
015500           W47637                                                         
015600                                                                          
015700     MOVE 'S' TO POSTSUM-OPKOD                                            
015800     CALL POSTSUM USING POSTSUM-PARM                                      
015900     .                                                                    
016000     EJECT                                                                
016100 S01-LAES-W47635  SECTION.                                                
016200                                                                          
016300     READ W47635 INTO IN-AREA                                             
016400     AT END                                                               
016500        SET END-OF-W47635 TO TRUE                                         
016600     NOT AT END                                                           
016700        MOVE 'W47635'       TO POSTSUM-FDNAMN                             
016800        MOVE 'W47638D1'     TO POSTSUM-DDNAMN2                            
016900        MOVE BR-IDPTYP      TO POSTSUM-TRANSTYP                           
017000        CALL POSTSUM USING POSTSUM-PARM                                   
017100     END-READ                                                             
017200     .                                                                    
017300     EJECT                                                                
018600 S11-SKRIV-W47637 SECTION.                                                
018700                                                                          
018800     EVALUATE BR-IDPTYP                                                   
018900       WHEN '1  '                                                         
019000          WRITE BRH-POST FROM BRH-BROKJPH-W4756GH                         
019700       WHEN '5  '                                                         
019800          WRITE BRD-POST FROM BRD-BROKJPD-W4756GD                         
019900     END-EVALUATE                                                         
020000                                                                          
020100     MOVE BR-IDPTYP         TO POSTSUM-TRANSTYP                           
020200     MOVE 'W47638'          TO POSTSUM-FDNAMN                             
020300     MOVE 'W47638D2'        TO POSTSUM-DDNAMN2                            
020400     CALL POSTSUM USING POSTSUM-PARM                                      
020500     .                                                                    
