000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2713900.                                                
000400*AUTHOR.         ARUP DATTA.                                              
000500*DATE-WRITTEN.   MAY 2020.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAM READS ONORMAL FORECAST ALARM                             
001100*                                                                         
001200*        ALSO CAN BE EXECUTED TO READ INPUT FILE WITH FORECAST            
001300*        ALARMS FOR SLOW MOVING REFILL PARTS                              
001310*                                                                         
001400*        THE DATA IS PROCESSED AND WRITTEN TO OUTPUT IN                   
001500*        REPORT FORMAT                                                    
001600*                                                                         
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600                                                                          
002700     SELECT W27139IN                   ASSIGN TO W27139D1.                
002800                                                                          
002900     SELECT W27139-001                 ASSIGN TO W27139D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500                                                                          
003600 FD  W27139IN                                                             
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST  -COPY W27138    -PRE IN-    -L.                                
004100                                                                          
004200     SKIP3                                                                
004300                                                                          
004400 FD  W27139-001                                                           
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800 01  LISTPOST            PIC X(75).                                       
004900                                                                          
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200     SKIP2                                                                
005300                                                                          
005400*    -- CHECKED BY WY2000                                                 
005500 77  IDPGM                       PIC X(8)    VALUE 'W2713900'.            
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800     EJECT                                                                
005900                                                                          
006000 77  W27139IN-EOF-SW             PIC X       VALUE 'N'.                   
006100     88  END-OF-W27139IN                     VALUE 'J'.                   
006200                                                                          
006300 77  FIRST-LINE-SW               PIC X       VALUE 'J'.                   
006400     EJECT                                                                
006500                                                                          
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200                                                                          
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     SKIP2                                                                
007800*    --- PARAMETRAR TILL ABEND                                            
007900                                                                          
008000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008200     SKIP2                                                                
008300 01  FELTEXT.                                                             
008400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009100 01  IN-AREA-START              PIC X(24)   VALUE                         
009200                                 'IN-AREA-START  '.                       
009300     SKIP2                                                                
009400                                                                          
009500*01  AREA -COPY W27138     -PRE IN-                                       
009600     EJECT                                                                
009700 01  LIST-AREA-START              PIC X(24)   VALUE                       
009800                                 'LIST-AREA-START  '.                     
009900     SKIP2                                                                
010000                                                                          
010100 01  LIST-AREA                  PIC X(70).                                
010200     EJECT                                                                
010300 01  W001-REPORT-AREA           PIC X(24)   VALUE                         
010400                                 'W001-REPORT-AREA '.                     
010500     SKIP2                                                                
010600 01  W001-HDR-LINE.                                                       
010700     05 W-HDR-IDPERSON-BUY      PIC X(05) VALUE 'BUYER'.                  
010800     05 FILLER                  PIC X(01) VALUE ';'.                      
010900     05 W-HDR-IDARTNR           PIC X(07) VALUE 'PART NO'.                
011000     05 FILLER                  PIC X(01) VALUE ';'.                      
011100     05 W-HDR-IDDC              PIC X(02) VALUE 'DC'.                     
011200     05 FILLER                  PIC X(01) VALUE ';'.                      
011300     05 W-HDR-BEART             PIC X(16) VALUE                           
011400                                 'PART DESCRIPTION'.                      
011500     05 FILLER                  PIC X(01) VALUE ';'.                      
011600     05 W-HDR-KVPB-REF          PIC X(10) VALUE 'CURRENT PB'.             
011700     05 FILLER                  PIC X(01) VALUE ';'.                      
011800     05 W-HDR-NY-KVPB-REF       PIC X(06) VALUE 'NEW PB'.                 
011900     05 FILLER                  PIC X(01) VALUE ';'.                      
012000     EJECT                                                                
012100 01  W001-DTL-LINE.                                                       
012200     05 W001-IDPERSON-BUY       PIC Z(2)9 VALUE ZERO.                     
012300     05 FILLER                  PIC X(01) VALUE ';'.                      
012400     05 W001-IDARTNR            PIC Z(8)9 VALUE ZERO.                     
012500     05 FILLER                  PIC X(01) VALUE ';'.                      
012600     05 W001-IDDC               PIC X(02) VALUE SPACES.                   
012700     05 FILLER                  PIC X(01) VALUE ';'.                      
012800     05 W001-BEART              PIC X(25) VALUE SPACES.                   
012900     05 FILLER                  PIC X(01) VALUE ';'.                      
013000     05 W001-KVPB-REF           PIC Z(5)9.9-.                             
013100     05 FILLER                  PIC X(01) VALUE ';'.                      
013200     05 W001-NY-KVPB-REF        PIC Z(5)9.9-.                             
013300     05 FILLER                  PIC X(01) VALUE ';'.                      
013400     EJECT                                                                
013500 PROCEDURE DIVISION.                                                      
013600                                                                          
013700     PERFORM A-INIT                                                       
013800                                                                          
013900     PERFORM S01-LAES-W27139IN                                            
014000                                                                          
014100     PERFORM UNTIL END-OF-W27139IN                                        
014200                                                                          
014300       IF FIRST-LINE-SW = JA                                              
014400          MOVE W001-HDR-LINE    TO LIST-AREA                              
014500          PERFORM S02-SKRIV-LISTPOST                                      
014600          MOVE NEJ              TO FIRST-LINE-SW                          
014700       END-IF                                                             
014800                                                                          
014900       MOVE IN-IDPERSON-BUY     TO W001-IDPERSON-BUY                      
015000       MOVE IN-IDARTNR          TO W001-IDARTNR                           
015100       MOVE IN-IDDC             TO W001-IDDC                              
015200       MOVE IN-BEART            TO W001-BEART                             
015300       MOVE IN-KVPB-REF         TO W001-KVPB-REF                          
015400       MOVE IN-NY-KVPB-REF      TO W001-NY-KVPB-REF                       
015500                                                                          
015600       MOVE W001-DTL-LINE       TO LIST-AREA                              
015700       PERFORM S02-SKRIV-LISTPOST                                         
015800                                                                          
015900       PERFORM S01-LAES-W27139IN                                          
016000                                                                          
016100     END-PERFORM                                                          
016200                                                                          
016300     PERFORM Z-FINIT                                                      
016400                                                                          
016500     MOVE ZERO TO RETURN-CODE                                             
016600     GOBACK                                                               
016700     .                                                                    
016800     EJECT                                                                
016900 A-INIT SECTION.                                                          
017000                                                                          
017100     OPEN INPUT  W27139IN                                                 
017200     OPEN OUTPUT W27139-001                                               
017300     .                                                                    
017400     EJECT                                                                
017500 Z-FINIT SECTION.                                                         
017600                                                                          
017700     CLOSE W27139IN                                                       
017800           W27139-001                                                     
017900                                                                          
018000     MOVE 'S' TO POSTSUM-OPKOD                                            
018100     CALL POSTSUM USING POSTSUM-PARM                                      
018200     .                                                                    
018300     EJECT                                                                
018400 S01-LAES-W27139IN SECTION.                                               
018500     SKIP2                                                                
018600     READ W27139IN           INTO IN-AREA                                 
018700     AT END                                                               
018800        MOVE JA TO W27139IN-EOF-SW                                        
018900                                                                          
019000     END-READ                                                             
019100     .                                                                    
019200     EJECT                                                                
019300 S02-SKRIV-LISTPOST  SECTION.                                             
019400                                                                          
019500     WRITE LISTPOST  FROM LIST-AREA                                       
019600                                                                          
019700     MOVE 'W27139'   TO POSTSUM-FDNAMN                                    
019800     MOVE 'W27139D2' TO POSTSUM-DDNAMN2                                   
019900     CALL POSTSUM USING POSTSUM-PARM                                      
020000     .                                                                    
020100     EJECT                                                                
