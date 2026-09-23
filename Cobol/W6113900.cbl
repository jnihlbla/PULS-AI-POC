000010                                                                          
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6113900.                                                
000400 AUTHOR.         KENT JEBSEN.                                             
000500 DATE-WRITTEN.   03/03/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        KONVERTERAR DISTRIKT TILL DC PÅ VECKANS REFILLORDRAR.            
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
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
002401     SKIP2                                                                
002402*          --- VECKANS REFILLORDRAR                                       
002403     SELECT W27112                     ASSIGN TO W61139D1.                
002404     SKIP2                                                                
002405*          --- ÄNDRAT DISTRIKT TILL DC                                    
002410     SELECT W61139                     ASSIGN TO W61139D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W27112                                                               
003003     RECORDING       V                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006 01  FILLER  PIC X(160).                                                  
003007                                                                          
003008*01  -COPY W412RX3      -L.                                               
003009     SKIP3                                                                
003010 FD  W61139                                                               
003011     RECORDING       F                                                    
003012     BLOCK CONTAINS  0.                                                   
003013                                                                          
003020*01  POST -COPY W6113901 -PRE  UT-  -L.                                   
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W6113900'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  IX1                         PIC S9(4)   VALUE +0   COMP SYNC.        
003710 77  IX2                         PIC S9(4)   VALUE +0   COMP SYNC.        
003720 77  TRAEFF                      PIC X       VALUE 'N'.                   
003800                                                                          
003801                                                                          
003802 77  W27112-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W27112                       VALUE 'J'.                   
003900     EJECT                                                                
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500     EJECT                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     SKIP2                                                                
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006203 01  FILLER                      PIC X(16)   VALUE 'DIST-DC-TAB'.         
006204     -COPY WWDIST57                                                       
006205     EJECT                                                                
006206 01  IN-AREA-START               PIC X(24)   VALUE                        
006207                                 'IN-AREA-START  '.                       
006208     SKIP2                                                                
006209 01  IN-AREA.                                                             
006210     03  IN-AREA-0.                                                       
006211       05  IN-IDPTYP             PIC X(3).                                
006213       05  FILLER                PIC X(400).                              
006214*   03  FILLER -COPY W412RX3  -PRE IN-  -RED  IN-AREA-0                   
006215     EJECT                                                                
006216 01  UT-AREA-START               PIC X(24)   VALUE                        
006217                                 'UT-AREA-START  '.                       
006218     SKIP2                                                                
006219                                                                          
006220*01  AREA -COPY W6113901     -PRE UT-                                     
006300     EJECT                                                                
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007010     PERFORM S01-LAES-W27112                                              
007100     PERFORM UNTIL END-OF-W27112                                          
007200                                                                          
007201       SEARCH ALL DIST57-REFILL-DC                                        
007202         AT END                                                           
007206           MOVE NEJ TO TRAEFF                                             
007207         WHEN DIST57-SOK-IDDISTR(DIST57-IX) = IN-IDDISTR                  
007208           MOVE DIST57-REFILL-TO-DC(DIST57-IX)                            
007210                             TO UT-IDDC                                   
007211           MOVE JA  TO TRAEFF                                             
007212       END-SEARCH                                                         
007213                                                                          
007214*                                                                         
007215*      MOVE +1 TO IX1                                                     
007216*      MOVE NEJ TO TRAEFF                                                 
007220*      PERFORM UNTIL TRAEFF = JA OR IX1 > DC-MAX                          
007240*         MOVE +1 TO IX2                                                  
007241*         PERFORM UNTIL TRAEFF = JA OR IX2 > DC-DISTRIKT-MAX              
007243*           IF IN-IDDISTR = IDDISTR(IX1, IX2)                             
007245*             MOVE JA TO TRAEFF                                           
007246*           END-IF                                                        
007247*           ADD 1 TO IX2                                                  
007248*         END-PERFORM                                                     
007249*         ADD 1 TO IX1                                                    
007260*      END-PERFORM                                                        
007270       IF TRAEFF = NEJ                                                    
007280         CONTINUE                                                         
007290       ELSE                                                               
007291         MOVE IN-IDARTNR   TO UT-IDARTNR                                  
007292*        MOVE IDDC(IX1) TO UT-IDDC                                        
007293         IF IN-ADLAGOMR-CD > 0                                            
007294           MOVE ZERO       TO UT-KVBEART                                  
007295           MOVE IN-KVBEART TO UT-KVBEART-CD                               
007296         ELSE                                                             
007298           MOVE IN-KVBEART TO UT-KVBEART                                  
007299           MOVE ZERO       TO UT-KVBEART-CD                               
007300         END-IF                                                           
007301         PERFORM S11-SKRIV-W61139                                         
007302       END-IF                                                             
007700                                                                          
007810       PERFORM S01-LAES-W27112                                            
007900     END-PERFORM                                                          
008000                                                                          
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008910     OPEN INPUT  W27112                                                   
009001                                                                          
009010     OPEN OUTPUT W61139                                                   
009100     SKIP2                                                                
009200     ACCEPT DAGENS-DATUM  FROM DATE                                       
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W27112                                                         
009710           W61139                                                         
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-LAES-W27112  SECTION.                                                
010003     READ W27112 INTO IN-AREA                                             
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO IN-AREA                                        
010006        SET END-OF-W27112 TO TRUE                                         
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'W27112'   TO POSTSUM-FDNAMN                                 
010010        MOVE 'W61139D1' TO POSTSUM-DDNAMN2                                
010013        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010020     .                                                                    
010101     EJECT                                                                
010102 S11-SKRIV-W61139 SECTION.                                                
010103                                                                          
010104     WRITE UT-POST FROM UT-AREA                                           
010105                                                                          
010106     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
010107     MOVE 'W61139'   TO POSTSUM-FDNAMN                                    
010108     MOVE 'W61139D2' TO POSTSUM-DDNAMN2                                   
010109     CALL POSTSUM USING POSTSUM-PARM                                      
010110     .                                                                    
