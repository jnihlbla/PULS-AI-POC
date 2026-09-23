000400 ID  DIVISION.                                                            
000600 PROGRAM-ID.    W3718300.                                                 
001000 AUTHOR.        JAN PETTERSSON                                            
001100 DATE-WRITTEN.  SEPTEMBER 1987.                                           
001110 DATE-COMPILED.                                                           
001200                                                                          
001500*    FUNKTION:                                                            
001600*                                                                         
001700*           PROGRAMMET LÄSER REGISTER W37181 (KVITTNINGS-                 
001800*           TABELLER OCH SKAPAR FIL TILL VIPS                             
001900*                                                                         
002000                                                                          
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*- - - - - - - - - - - - INFIL:                                           
002900*                        - -  FIL MED KVITT-POSTER                        
003000*                                                                         
003100     SELECT W37181                       ASSIGN TO W37183D1.              
003200*- - - - - - - - - - - - UTFIL:                                           
003300*                        - -  FIL MED POSTER TILL VIPS                    
003400     SELECT W37183                       ASSIGN TO W37183D2.              
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W37181                                                               
004100     RECORDING      F                                                     
004200     BLOCK CONTAINS 0.                                                    
004300     SKIP2                                                                
004310*01  POST  -COPY W37181       -L.                                         
004600     SKIP2                                                                
004700 FD  W37183                                                               
004800     RECORDING      V                                                     
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP2                                                                
005100*01  POST     -COPY W37183      -PRE UT-  -L.                             
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005410                                                                          
005500*    -- CHECKED BY WY2000                                                 
006100 77  IDPGM                       PIC X(8)    VALUE 'W3718300'.            
006300                                                                          
006400*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
006500                                                                          
006600 77  JA                          PIC X(1)    VALUE 'J'.                   
006700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006800     SKIP2                                                                
006900*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007000                                                                          
007100 77  W37181-EOF                  PIC X(1)    VALUE 'N'.                   
007200     SKIP2                                                                
007300                                                                          
007400     EJECT                                                                
007500 01  DAGENS-DATUM                PIC 9(6).                                
007600 01  RED-DATUM                   REDEFINES DAGENS-DATUM.                  
007700   03  DAGENS-DATUM-AR           PIC 9(2).                                
007800   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
007900   03  DAGENS-DATUM-DAG          PIC 9(2).                                
008000                                                                          
008100                                                                          
008200 01  KONTROLL-SIFFRA.                                                     
008300   03  REK-IDARTNR               PIC 9(9)    VALUE ZERO.                  
008400   03  REK-LENGD                 PIC 9(1)    VALUE 9.                     
008500   03  REK-REKSIFFR              PIC 9(1)    VALUE ZERO.                  
008600                                                                          
008700     EJECT                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
009000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
009200   03  W009KSIF                  PIC X(8)    VALUE 'W009KSIF'.            
009300     SKIP3                                                                
009400*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
009500                                                                          
009600 01  RETURKODER.                                                          
009700   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
009800   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
009900   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
010000     EJECT                                                                
010100*- - - - - - - - - - - - - -  VALID IDDC CODES                            
010101*01  -COPY  WWDCKONS                                                      
010110*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
010200                                                                          
010300 01  FILLER                   PIC X(16) VALUE 'POSTSUM********'.          
010400*01  -COPY W0005       -PRE  POSTSUM-.                                    
010600     EJECT                                                                
010700 01  FILLER                   PIC X(16) VALUE '***IN-AREA*****'.          
010800*01  AREA  -PRE IN-  -COPY W37181                                         
011000     EJECT                                                                
011100 01  FILLER                   PIC X(16) VALUE '***UT-AREA*****'.          
011200*01  AREA  -PRE UT-    -COPY W37183                                       
011400     EJECT                                                                
011500 PROCEDURE DIVISION.                                                      
011600 MAIN SECTION.                                                            
011700     PERFORM A-INIT                                                       
011800                                                                          
011900     PERFORM  B-BEARBETNING                                               
012000                                                                          
012100     PERFORM Z-FINIT                                                      
012200     MOVE ZERO TO RETURN-CODE                                             
012300     GOBACK                                                               
012400     .                                                                    
012500                                                                          
012600     EJECT                                                                
012700 A-INIT SECTION.                                                          
012800     SKIP2                                                                
012900     OPEN  INPUT W37181                                                   
013000     OPEN OUTPUT W37183                                                   
013100     MOVE SPACE TO UT-AREA                                                
013200     MOVE ZERO  TO UT-SOR0-IDLOPNR                                        
013300     SKIP2                                                                
013400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013500     .                                                                    
013600     SKIP2                                                                
013700     EJECT                                                                
013800 B-BEARBETNING SECTION.                                                   
013900     SKIP2                                                                
014000     PERFORM S01-LAS-REGISTER                                             
014100     PERFORM UNTIL                                                        
014200      NOT ( W37181-EOF = NEJ )                                            
014300       PERFORM BA-SKAPA-SKRIV-POST                                        
014400       PERFORM S01-LAS-REGISTER                                           
014500     END-PERFORM                                                          
014600     .                                                                    
014700     EJECT                                                                
014800 BA-SKAPA-SKRIV-POST SECTION.                                             
014900     SKIP2                                                                
015000     MOVE   ZERO  TO UT-SOR0-IDDISTR                                      
015100                     UT-SOR0-IDKUNDNR                                     
015200                     UT-SOR0-IDRONR                                       
015300                     UT-SOR0-TIRODAT                                      
015400     MOVE  '025'  TO UT-SOR0-IDPTYP                                       
015500     ADD   +1     TO UT-SOR0-IDLOPNR                                      
015600                                                                          
015700     MOVE 'RKF' TO UT-IDPTYP                                              
015800     MOVE WC-CDC-SE TO UT-IDDC                                            
015900     MOVE IN-IDARTNR TO UT-IDARTNR                                        
016000     MOVE IN-IDTABNR TO UT-IDTABNR                                        
016100     MOVE IN-IDARTNR TO REK-IDARTNR                                       
016200     CALL W009KSIF USING REK-IDARTNR                                      
016300                         REK-LENGD                                        
016400                         REK-REKSIFFR                                     
016500     MOVE REK-REKSIFFR TO UT-REKSIFFR                                     
016600     WRITE UT-POST FROM UT-W37183                                         
016700                                                                          
016800     MOVE 'W37183'            TO POSTSUM-FDNAMN                           
016900     MOVE 'W37183D2'          TO POSTSUM-DDNAMN2                          
017000     MOVE 'RFK'               TO POSTSUM-TRANSTYP                         
017100     CALL POSTSUM   USING POSTSUM-PARM                                    
017200     .                                                                    
017300                                                                          
017400     EJECT                                                                
017500 S01-LAS-REGISTER        SECTION.                                         
017600     SKIP2                                                                
017700     READ W37181    INTO    IN-AREA                                       
017800                      AT END MOVE JA TO W37181-EOF                        
017900     END-READ                                                             
018000                                                                          
018100     IF W37181-EOF = NEJ                                                  
018200       MOVE 'W37181'            TO POSTSUM-FDNAMN                         
018300       MOVE 'W37183D1'          TO POSTSUM-DDNAMN2                        
018400       MOVE 'REG'               TO POSTSUM-TRANSTYP                       
018500       CALL POSTSUM   USING POSTSUM-PARM                                  
018600                                                                          
018700     END-IF                                                               
018800     .                                                                    
018900     EJECT                                                                
019000 Z-FINIT SECTION.                                                         
019100     SKIP2                                                                
019200     CLOSE W37181                                                         
019300           W37183                                                         
019400*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
019500*                                    SKRIVNA POSTER                       
019600                                                                          
019700     MOVE 'S' TO POSTSUM-OPKOD                                            
019800     CALL POSTSUM USING POSTSUM-PARM                                      
019900     .                                                                    
