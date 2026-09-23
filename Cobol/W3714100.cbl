000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3714100.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   96/09/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR REGISTER ÖVER GODKÄNDA BYTESRETURER.                      
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
002400     SKIP2                                                                
002500*          --- INFIL MED NYA GODKÄNDA RAPPORTER                           
002600     SELECT W37138                     ASSIGN TO W37141D1.                
002700     SKIP2                                                                
002800*          --- REGISTRET IN                                               
002900     SELECT W37148-IN                  ASSIGN TO W37141D2.                
003000     SKIP2                                                                
003100*          --- REGISTRET UT                                               
003200     SELECT W37148-UT                  ASSIGN TO W37141D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W37138                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W37138      -L.                                                
004300     SKIP3                                                                
004400 FD  W37148-IN                                                            
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY W37138      -L.                                                
004900     SKIP3                                                                
005000 FD  W37148-UT                                                            
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W37138 -PRE  REGUT-  -L.                                  
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005701*    -COPY WY2000W1                                                       
005710     SKIP3                                                                
005800 77  IDPGM                       PIC X(8)    VALUE 'W3714100'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100                                                                          
006200 77  W37138-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W37138                       VALUE 'J'.                   
006400                                                                          
006500 77  W37148-EOF-SW               PIC X       VALUE 'N'.                   
006600     88  END-OF-W37148                       VALUE 'J'.                   
006700     EJECT                                                                
006800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES DAGENS-DATUM.                                       
007000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007300 01  W-TEST-DATUM                PIC 9(6)    VALUE ZERO.                  
007400     EJECT                                                                
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008000     SKIP2                                                                
008100*    --- PARAMETRAR TILL ABEND                                            
008200                                                                          
008300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008500     SKIP2                                                                
008600 01  FELTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL POSTSUM                                          
009100*                                                                         
009200*01  -COPY W0005   -PRE  POSTSUM-                                         
009300     EJECT                                                                
009400*01  -COPY WDATAREA                                                       
009500     EJECT                                                                
009600 01  TRANS-AREA-START            PIC X(24)   VALUE                        
009700                                 'TRANS-AREA-START  '.                    
009800     SKIP2                                                                
009900                                                                          
010000*01  AREA -COPY W37138     -PRE TRANS-                                    
010100     EJECT                                                                
010200 01  REGIN-AREA-START            PIC X(24)   VALUE                        
010300                                 'REGIN-AREA-START  '.                    
010400     SKIP2                                                                
010500                                                                          
010600*01  AREA -COPY W37138     -PRE REGIN-                                    
010700     EJECT                                                                
010800 01  REGUT-AREA-START            PIC X(24)   VALUE                        
010900                                 'REGUT-AREA-START  '.                    
011000     SKIP2                                                                
011100                                                                          
011200*01  AREA -COPY W37138     -PRE REGUT-                                    
011300     EJECT                                                                
011400 PROCEDURE DIVISION.                                                      
011500 MAIN SECTION.                                                            
011600     SKIP2                                                                
011700                                                                          
011800     PERFORM A-INIT                                                       
011900     PERFORM S02-LAES-W37148-IN                                           
012000     PERFORM UNTIL END-OF-W37148                                          
012001       MOVE W-TEST-DATUM          TO TMP1-YYMMDD                          
012010       MOVE REGIN-TIREGDAT-GODK   TO TMP2-YYMMDD                          
012100       PERFORM WY2000P1                                                   
012200       IF TMP1-YYMMDD < TMP2-YYMMDD                                       
012300         PERFORM C-FLYTTA-TILL-UTREG                                      
012400         PERFORM S11-SKRIV-W37148-UT                                      
012500       END-IF                                                             
012600       PERFORM S02-LAES-W37148-IN                                         
012700     END-PERFORM                                                          
012800     PERFORM S01-LAES-W37138                                              
012900     PERFORM UNTIL END-OF-W37138                                          
013000       PERFORM D-FLYTTA-NYA-TILL-UTREG                                    
013100       PERFORM S11-SKRIV-W37148-UT                                        
013200       PERFORM S01-LAES-W37138                                            
013300     END-PERFORM                                                          
013400                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200                                                                          
014300     OPEN INPUT  W37138                                                   
014400                 W37148-IN                                                
014500                                                                          
014600     OPEN OUTPUT W37148-UT                                                
014700     SKIP2                                                                
014800     ACCEPT DAGENS-DATUM  FROM DATE                                       
014900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015000                                                                          
015020     IF DAGENS-DATUM-AAR = 00                                             
015030       MOVE 98 TO DAGENS-DATUM-AAR                                        
015040     ELSE                                                                 
015050       IF DAGENS-DATUM-AAR = 01                                           
015060         MOVE 99 TO DAGENS-DATUM-AAR                                      
015070       ELSE                                                               
015100         SUBTRACT 2 FROM    DAGENS-DATUM-AAR                              
015110       END-IF                                                             
015120     END-IF                                                               
015130                                                                          
015200     MOVE DAGENS-DATUM TO W-TEST-DATUM                                    
016600     .                                                                    
016700     EJECT                                                                
016800 C-FLYTTA-TILL-UTREG SECTION.                                             
016900     MOVE REGIN-AREA TO REGUT-AREA                                        
017000     .                                                                    
017100     EJECT                                                                
017200 D-FLYTTA-NYA-TILL-UTREG SECTION.                                         
017300     MOVE TRANS-AREA TO REGUT-AREA                                        
017400     .                                                                    
017500     EJECT                                                                
017600 Z-FINIT SECTION.                                                         
017700     CLOSE W37138                                                         
017800           W37148-IN                                                      
017900           W37148-UT                                                      
018000     SKIP2                                                                
018100     MOVE 'S' TO POSTSUM-OPKOD                                            
018200     CALL POSTSUM USING POSTSUM-PARM                                      
018300     .                                                                    
018400     EJECT                                                                
018500 S01-LAES-W37138  SECTION.                                                
018600     READ W37138 INTO TRANS-AREA                                          
018700     AT END                                                               
018800*        MOVE HIGH-VALUE TO TRANS-ID                                      
018900        SET END-OF-W37138 TO TRUE                                         
019000                                                                          
019100     NOT AT END                                                           
019200        MOVE 'W37138'   TO POSTSUM-FDNAMN                                 
019300        MOVE 'W37141D1' TO POSTSUM-DDNAMN2                                
019400        MOVE 'TRANS'    TO POSTSUM-TRANSTYP                               
019500        CALL POSTSUM USING POSTSUM-PARM                                   
019600     END-READ                                                             
019700     .                                                                    
019800     EJECT                                                                
019900 S02-LAES-W37148-IN  SECTION.                                             
020000     READ W37148-IN INTO REGIN-AREA                                       
020100     AT END                                                               
020200*       MOVE HIGH-VALUE TO REGIN-ID                                       
020300        SET END-OF-W37148 TO TRUE                                         
020400                                                                          
020500     NOT AT END                                                           
020600        MOVE 'W37148'   TO POSTSUM-FDNAMN                                 
020700        MOVE 'W37141D2' TO POSTSUM-DDNAMN2                                
020800        MOVE 'REGIN'    TO POSTSUM-TRANSTYP                               
020900        CALL POSTSUM USING POSTSUM-PARM                                   
021000     END-READ                                                             
021100     .                                                                    
021200     EJECT                                                                
021300 S11-SKRIV-W37148-UT SECTION.                                             
021400                                                                          
021500     WRITE REGUT-POST FROM REGUT-AREA                                     
021600                                                                          
021700     MOVE 'REGUT' TO POSTSUM-TRANSTYP                                     
021800     MOVE 'W37148' TO POSTSUM-FDNAMN                                      
021900     MOVE 'W37141D3' TO POSTSUM-DDNAMN2                                   
022000     CALL POSTSUM USING POSTSUM-PARM                                      
022100     .                                                                    
022200     EJECT                                                                
022300 S99-ABEND SECTION.                                                       
022400                                                                          
022500     SKIP2                                                                
022600     MOVE 'S' TO POSTSUM-OPKOD                                            
022700     CALL POSTSUM USING POSTSUM-PARM                                      
022800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
022900     .                                                                    
022910     EJECT                                                                
023000*    -COPY WY2000P1                                                       
