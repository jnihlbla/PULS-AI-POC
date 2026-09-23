000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2710700.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   JUNE 2005.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                PROGRAMMET LÄSER RESTORDER OCH SUMMERAR                  
001000*                ORDERRADER PER DC/ARTIKEL FÖR ATT SENARE                 
001100*                SKRIVA LISTAN 'TOPP 100 RESTORDER' PER DC                
001200*                I ÄLDSTAORDNING                                          
001300*                ENBART ÄLDSTA RESTORDERN PER ARTIKEL VISAS               
001400*                                                                         
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800     SELECT W27103                     ASSIGN TO W27107D1.                
002900*                                                                         
003000     SELECT W27107                     ASSIGN TO W27107D2.                
003100     SKIP2                                                                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700                                                                          
003800 FD  W27103                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100*01  POST -COPY W27105 -PRE  IN-  -L.                                     
004200     EJECT                                                                
004300                                                                          
004400     SKIP3                                                                
004500                                                                          
004600 FD  W27107                                                               
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS  0.                                                   
004900*01  POST -COPY W27107 -PRE  UT-  -L.                                     
005000     EJECT                                                                
005100*                                                                         
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400*    -- CHECKED BY WY2000                                                 
005500 77  IDPGM                       PIC X(8)    VALUE 'W2710700'.            
005600 77  DC-IX                       PIC S9(4)   VALUE +0  COMP SYNC.         
005700 77  MAX-TAB-IX                  PIC S9(4)   VALUE +10 COMP SYNC.         
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  IX                          PIC 9(4)    VALUE 1.                     
006100     SKIP2                                                                
006200 01  WS.                                                                  
006300     03  WS-IDDC                 PIC X(2)    VALUE SPACE.                 
006400     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
006500 01  W-IDDCTEXT-MSGI.                                                     
006600     03  FILLER              PIC X(5)   VALUE 'WIDDC'.                    
006700     03  W-IDDC-MSGI         PIC X(2).                                    
006800     EJECT                                                                
006900 01  POST-ANT                    PIC S9(7)   VALUE ZERO COMP-3.           
007000                                                                          
007100 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
007200 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
007300                                                                          
007400     EJECT                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800                                                                          
007900 77  W27103-EOF-SW               PIC X       VALUE 'N'.                   
008000     88  END-OF-W27103                       VALUE 'J'.                   
008100     EJECT                                                                
008200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008300 01  FILLER REDEFINES DAGENS-DATUM.                                       
008400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008700     SKIP3                                                                
008800 01  WS-AAMMDD                   PIC 9(6).                                
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
009100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009200 01  RUBRIK-RAD.                                                          
009300     03  FILLER                 PIC X(5)    VALUE 'BUYER'.                
009400     03  FILLER                 PIC X(1)    VALUE X'05'.                  
009500     03  FILLER                 PIC X(9)    VALUE 'PART NO. '.            
009600     03  FILLER                 PIC X(1)    VALUE X'05'.                  
009700     03  FILLER                 PIC X(12)   VALUE 'TOTAL AMOUNT'.         
009800     03  FILLER                 PIC X(1)    VALUE X'05'.                  
009900     03  FILLER                 PIC X(10)   VALUE '   PRICE  '.           
010000     03  FILLER                 PIC X(1)    VALUE X'05'.                  
010100     03  FILLER                 PIC X(8)    VALUE 'FORECAST'.             
010200     03  FILLER                 PIC X(1)    VALUE X'05'.                  
010300     03  FILLER                 PIC X(8)    VALUE 'AVAILAB.'.             
010400     03  FILLER                 PIC X(1)    VALUE X'05'.                  
010500     03  FILLER                 PIC X(2)    VALUE 'DC'.                   
010600     03  FILLER                 PIC X(1)    VALUE X'05'.                  
010700                                                                          
010800     EJECT                                                                
010900 01  IN-AREA-START               PIC X(24)   VALUE                        
011000                                             'IN-AREA-START'.             
011100     SKIP2                                                                
011200*01  AREA -COPY W27105     -PRE IN-                                       
011300                                                                          
011400     EJECT                                                                
011500 01  UT-AREA-START              PIC X(24)   VALUE                         
011600                                 'UT-AREA-START  '.                       
011700     SKIP2                                                                
011800*01  AREA -COPY W27107     -PRE UT-                                       
011900                                                                          
012000                                                                          
012100     EJECT                                                                
012200 PROCEDURE DIVISION.                                                      
012300                                                                          
012400     PERFORM A-INIT                                                       
012500                                                                          
012600     PERFORM S01-LAES-W27103                                              
012700                                                                          
012800     PERFORM UNTIL END-OF-W27103                                          
012900                                                                          
013000       IF ((IN-IDARTNR = WS-IDARTNR)                                      
013100       AND (IN-IDDC     = WS-IDDC))                                       
013200          CONTINUE                                                        
013300       ELSE                                                               
013400          IF IX = 1                                                       
013500            WRITE UT-POST FROM RUBRIK-RAD                                 
013600          END-IF                                                          
013700          ADD +1 TO IX                                                    
013800          MOVE IN-IDARTNR      TO WS-IDARTNR                              
013900                                UT-IDARTNR                                
014000          MOVE IN-IDDC         TO WS-IDDC                                 
014100                                UT-IDDC                                   
014200          MOVE IN-IDPERSON     TO UT-IDPERSON                             
014300          MOVE IN-TOTALSUM     TO UT-TOTALSUM                             
014400          IF IN-IDDC (1:1) = '7' OR '4' OR '5'                            
014410            MOVE IN-PRMATRL    TO UT-PRARTSTD                             
014510          ELSE                                                            
014511            MOVE IN-PRARTSTD   TO UT-PRARTSTD                             
014520          END-IF                                                          
014600          MOVE IN-AVAILABLE    TO UT-AVAILABLE                            
014700          MOVE IN-KVPB-REF     TO UT-KVPB-REF                             
014800          PERFORM S02-SKRIV-UTFIL                                         
014900       END-IF                                                             
015000       PERFORM S01-LAES-W27103                                            
015100     END-PERFORM                                                          
015200                                                                          
015300                                                                          
015400                                                                          
015500     PERFORM Z-FINIT                                                      
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200     SKIP2                                                                
016300                                                                          
016400     OPEN INPUT  W27103                                                   
016500     OPEN OUTPUT W27107                                                   
016600     .                                                                    
016700     EJECT                                                                
016800                                                                          
016900 Z-FINIT SECTION.                                                         
017000                                                                          
017100                                                                          
017200     CLOSE W27103                                                         
017300           W27107                                                         
017400     .                                                                    
017500     EJECT                                                                
017600 S01-LAES-W27103   SECTION.                                               
017700     SKIP2                                                                
017800     READ W27103             INTO IN-AREA                                 
017900     AT END                                                               
018000        MOVE JA TO W27103-EOF-SW                                          
018100                                                                          
018200     END-READ                                                             
018300     .                                                                    
018400     EJECT                                                                
018500 S02-SKRIV-UTFIL SECTION.                                                 
018600     SKIP2                                                                
018700     WRITE UT-POST                    FROM UT-AREA                        
018800     .                                                                    
