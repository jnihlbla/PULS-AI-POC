000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3715D00.                                                
000300 AUTHOR.         INGVAR SKJELBRED.                                        
000400 DATE-WRITTEN.   98/04/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SKAPAR UPPFÖLJNINGSFIL MOT EXEL                                  
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002301     SKIP2                                                                
002302*          --- UPPFÖLJNINGSREGISTER                                       
002303     SELECT W3715C                     ASSIGN TO W3715DD1.                
002304     SKIP2                                                                
002305*          --- UPPFÖLJNINGSFIL I EXELFORMAT                               
002310     SELECT W3715D                     ASSIGN TO W3715DD2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002901     SKIP3                                                                
002902 FD  W3715C                                                               
002903     RECORDING       F                                                    
002904     BLOCK CONTAINS  0.                                                   
002905                                                                          
002906*01  -COPY W3715C      -L.                                                
002907     SKIP3                                                                
002908 FD  W3715D                                                               
002909     RECORDING       F                                                    
002910     BLOCK CONTAINS  0.                                                   
002911                                                                          
002920*01  POST -COPY W3715D -PRE  UT-  -L.                                     
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W3715D00'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003701                                                                          
003702 77  W3715C-EOF-SW               PIC X       VALUE 'N'.                   
003710     88  END-OF-W3715C                       VALUE 'J'.                   
003800     EJECT                                                                
003900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004000 01  FILLER REDEFINES DAGENS-DATUM.                                       
004100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004400     EJECT                                                                
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600*                                                                         
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004810     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004900     SKIP2                                                                
005000*    --- PARAMETRAR TILL ABEND                                            
005100                                                                          
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005901     EJECT                                                                
005902*    --- PARAMETRAR TILL POSTSUM                                          
005903*                                                                         
005910*01  -COPY W0005   -PRE  POSTSUM-                                         
006101     EJECT                                                                
006102 01  IN-AREA-START               PIC X(24)   VALUE                        
006103                                 'IN-AREA-START  '.                       
006104     SKIP2                                                                
006105                                                                          
006106*01  AREA -COPY W3715C     -PRE IN-                                       
006107     EJECT                                                                
006108 01  UT-AREA-START               PIC X(24)   VALUE                        
006109                                 'UT-AREA-START  '.                       
006110     SKIP2                                                                
006111                                                                          
006120*01  AREA -COPY W3715D     -PRE UT-                                       
006200     EJECT                                                                
006300 PROCEDURE DIVISION.                                                      
006400 MAIN SECTION.                                                            
006600     SKIP2                                                                
006700                                                                          
006800     PERFORM A-INIT                                                       
006910     PERFORM S01-LAES-W3715C                                              
007000     PERFORM UNTIL END-OF-W3715C                                          
007100       PERFORM B-BEARBETA                                                 
007710       PERFORM S01-LAES-W3715C                                            
007800     END-PERFORM                                                          
007900                                                                          
008000                                                                          
008100     PERFORM Z-FINIT                                                      
008200                                                                          
008300     MOVE ZERO TO RETURN-CODE                                             
008400     GOBACK                                                               
008500     .                                                                    
008600     EJECT                                                                
008700 A-INIT SECTION.                                                          
008801                                                                          
008810     OPEN INPUT  W3715C                                                   
008901                                                                          
008910     OPEN OUTPUT W3715D                                                   
009000     SKIP2                                                                
009100     ACCEPT DAGENS-DATUM  FROM DATE                                       
009210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009300     .                                                                    
009400     EJECT                                                                
009410 B-BEARBETA SECTION.                                                      
009411                                                                          
009412     MOVE IN-IDARTNR      TO                                              
009413                              UT-IDARTNR                                  
009414     MOVE IN-IDARTNR-OBJ  TO                                              
009415                              UT-IDARTNR-OBJ                              
009416     MOVE IN-KDPRODSL     TO                                              
009417                              UT-KDPRODSL                                 
009418     MOVE IN-IDFKNGRP     TO                                              
009419                              UT-IDFKNGRP                                 
009420     MOVE IN-BEART        TO                                              
009421                              UT-BEART                                    
009422     MOVE IN-IDDISTR      TO                                              
009423                              UT-IDDISTR                                  
009424     MOVE IN-KDMARK-BUDG  TO                                              
009425                              UT-KDMARK-BUDG                              
009428     MOVE IN-TIFSGVV      TO                                              
009429                              UT-TIFSGVV                                  
009433     MOVE IN-SULEVANT     TO                                              
009434                              UT-SULEVANT                                 
009438     MOVE IN-KVRETUR-GODK TO                                              
009439                              UT-KVRETUR-GODK                             
009440     MOVE IN-IDTABNR      TO                                              
009441                              UT-IDTABNR                                  
009442     MOVE IN-IDBYTRAP     TO                                              
009443                              UT-IDBYTRAP                                 
009444     MOVE IN-IDDC         TO                                              
009445                              UT-IDDC                                     
009446                                                                          
009447     MOVE ';'             TO  UT-SEMI-1                                   
009448     MOVE ';'             TO  UT-SEMI-2                                   
009449     MOVE ';'             TO  UT-SEMI-3                                   
009450     MOVE ';'             TO  UT-SEMI-4                                   
009451     MOVE ';'             TO  UT-SEMI-5                                   
009452     MOVE ';'             TO  UT-SEMI-6                                   
009453     MOVE ';'             TO  UT-SEMI-7                                   
009454     MOVE ';'             TO  UT-SEMI-8                                   
009455     MOVE ';'             TO  UT-SEMI-9                                   
009456     MOVE ';'             TO  UT-SEMI-10                                  
009457     MOVE ';'             TO  UT-SEMI-11                                  
009458     MOVE ';'             TO  UT-SEMI-12                                  
009459     PERFORM S11-SKRIV-W3715D                                             
009460                                                                          
009461     .                                                                    
009470     EJECT                                                                
009500 Z-FINIT SECTION.                                                         
009601     CLOSE W3715C                                                         
009610           W3715D                                                         
009701     SKIP2                                                                
009702     MOVE 'S' TO POSTSUM-OPKOD                                            
009710     CALL POSTSUM USING POSTSUM-PARM                                      
009800     .                                                                    
009901     EJECT                                                                
009902 S01-LAES-W3715C  SECTION.                                                
009903     READ W3715C INTO IN-AREA                                             
009904     AT END                                                               
009905        MOVE HIGH-VALUE TO IN-AREA                                        
009906        SET END-OF-W3715C TO TRUE                                         
009907                                                                          
009908     NOT AT END                                                           
009909        MOVE 'W3715C' TO POSTSUM-FDNAMN                                   
009910        MOVE 'W3715DD1' TO POSTSUM-DDNAMN2                                
009913        MOVE 'IN'      TO POSTSUM-TRANSTYP                                
009914        CALL POSTSUM USING POSTSUM-PARM                                   
009915     END-READ                                                             
009920     .                                                                    
010001     EJECT                                                                
010002 S11-SKRIV-W3715D SECTION.                                                
010003                                                                          
010004     WRITE UT-POST FROM UT-AREA                                           
010005                                                                          
010006     MOVE 'UT'      TO POSTSUM-TRANSTYP                                   
010007     MOVE 'W3715D' TO POSTSUM-FDNAMN                                      
010008     MOVE 'W3715DD2' TO POSTSUM-DDNAMN2                                   
010009     CALL POSTSUM USING POSTSUM-PARM                                      
010010     .                                                                    
010200     EJECT                                                                
010300 S99-ABEND SECTION.                                                       
010400                                                                          
010501     SKIP2                                                                
010502     MOVE 'S' TO POSTSUM-OPKOD                                            
010510     CALL POSTSUM USING POSTSUM-PARM                                      
010600     CALL ABEND USING RKOD-ABEND                                          
010700     .                                                                    
