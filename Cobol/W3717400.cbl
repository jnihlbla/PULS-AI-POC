000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3717400.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   99/12/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        FILERNA W37102, W37106, W37134 OCH W37135 LÄGGS IHOP TILL        
000901*        W37141 SORTERAD PÅ IDDISTR, KDEXCHA OCH IDPTYP                   
000902*        SOM SEDAN TAS IN I PROGRAMMET.                                   
000903*        LÄSER INFIL W37141 SUMMERAR IHOP DEN OCH SKAPAR UTFIL            
000910*                    W37142                                               
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
002302*          --- INFILERNA                                                  
002303     SELECT W37141                     ASSIGN TO W37174D1.                
002304     SKIP2                                                                
002305*          --- UTFIL                                                      
002310     SELECT W37142                     ASSIGN TO W37174D2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002901     SKIP3                                                                
002902 FD  W37141                                                               
002903     RECORDING       F                                                    
002904     BLOCK CONTAINS  0.                                                   
002905                                                                          
002906*01  -COPY W37109      -L.                                                
002907     SKIP3                                                                
002908 FD  W37142                                                               
002909     RECORDING       F                                                    
002910     BLOCK CONTAINS  0.                                                   
002911                                                                          
002920*01  POST -COPY W37109 -PRE  UT-  -L.                                     
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W3717400'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003701 01  WS-IDDISTR                  PIC S9(5) COMP-3 VALUE ZERO.             
003702 01  WS-KDEXCHA                  PIC S9(3) COMP-3 VALUE ZERO.             
003703 01  WS-SUPOINT                  PIC S9(9) COMP-3 VALUE ZERO.             
003704 01  WS-IDPTYP                   PIC X(3)         VALUE SPACE.            
003705 01  WS-FLINKLBS                 PIC X            VALUE SPACE.            
003706                                                                          
003709 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
003710     88  POST-FINNS                          VALUE 'J'.                   
003711 77  POST-SW                     PIC X       VALUE 'N'.                   
003712     88  POST-OK                             VALUE 'J'.                   
003720 77  W37141-EOF-SW               PIC X       VALUE 'N'.                   
003730     88  END-OF-W37141                       VALUE 'J'.                   
003800     EJECT                                                                
003900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004000 01  FILLER REDEFINES DAGENS-DATUM.                                       
004100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004310                                                                          
004311 01  W-DAAAVV                    PIC 9(6)    VALUE ZERO.                  
004312 01  FILLER REDEFINES W-DAAAVV.                                           
004313     03 W-SEKEL                  PIC 9(2).                                
004314     03 W-AAR                    PIC 9(2).                                
004315     03 W-VECKA                  PIC 9(2).                                
004316     EJECT                                                                
004320                                                                          
004400     EJECT                                                                
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600*                                                                         
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004810     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004820     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
004900     SKIP2                                                                
004910*    --- PARAMETRAR TILL DATKORT                                          
004920*                                                                         
004930 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
004940     SKIP2                                                                
004950*01  -COPY WDATKORT                                                       
004960     EJECT                                                                
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
006106*01  AREA -COPY W37109     -PRE IN-                                       
006107     EJECT                                                                
006108 01  UT-AREA-START               PIC X(24)   VALUE                        
006109                                 'UT-AREA-START  '.                       
006110     SKIP2                                                                
006111                                                                          
006120*01  AREA -COPY W37109     -PRE UT-                                       
006200     EJECT                                                                
006300 PROCEDURE DIVISION.                                                      
006400 MAIN SECTION.                                                            
006600     SKIP2                                                                
006700                                                                          
006800     PERFORM A-INIT                                                       
006910     PERFORM S01-LAES-W37141                                              
006911     PERFORM UNTIL END-OF-W37141 OR POST-OK                               
006912       IF IN-FLINKLBS = 'J'                                               
006913***CHANGED                                                                
006914          AND IN-FLBYTKND = 'J'                                           
006915******                                                                    
006920         MOVE IN-IDDISTR           TO WS-IDDISTR                          
006930         MOVE IN-KDEXCHA           TO WS-KDEXCHA                          
006940         MOVE IN-IDPTYP            TO WS-IDPTYP                           
006941*********CHANGED                                                          
006942         COMPUTE WS-SUPOINT = WS-SUPOINT + IN-SUPOINT                     
006943*******                                                                   
006944         MOVE JA                   TO POST-SW                             
006950       END-IF                                                             
006960       PERFORM S01-LAES-W37141                                            
006970     END-PERFORM                                                          
007000     PERFORM UNTIL END-OF-W37141                                          
007010       IF IN-FLINKLBS = 'J'                                               
007020***CHANGED                                                                
007021          AND IN-FLBYTKND = 'J'                                           
007022******                                                                    
007030                                                                          
007100         IF IN-IDDISTR = WS-IDDISTR                                       
007200           IF IN-KDEXCHA = WS-KDEXCHA                                     
007300             IF IN-IDPTYP = WS-IDPTYP                                     
007400               COMPUTE WS-SUPOINT = WS-SUPOINT + IN-SUPOINT               
007500             ELSE                                                         
007501               PERFORM B-FYLL-UT-AREA                                     
007510               PERFORM S11-SKRIV-W37142                                   
007511               MOVE IN-IDPTYP       TO WS-IDPTYP                          
007512               MOVE IN-SUPOINT      TO WS-SUPOINT                         
007513               MOVE IN-FLINKLBS     TO WS-FLINKLBS                        
007520             END-IF                                                       
007530           ELSE                                                           
007531             PERFORM B-FYLL-UT-AREA                                       
007532             PERFORM S11-SKRIV-W37142                                     
007533             MOVE IN-KDEXCHA        TO WS-KDEXCHA                         
007534             MOVE IN-IDPTYP         TO WS-IDPTYP                          
007535             MOVE IN-SUPOINT        TO WS-SUPOINT                         
007536             MOVE IN-FLINKLBS       TO WS-FLINKLBS                        
007537           END-IF                                                         
007540         ELSE                                                             
007550           PERFORM B-FYLL-UT-AREA                                         
007600           PERFORM S11-SKRIV-W37142                                       
007610           MOVE IN-IDDISTR          TO WS-IDDISTR                         
007611           MOVE IN-KDEXCHA          TO WS-KDEXCHA                         
007620           MOVE IN-IDPTYP           TO WS-IDPTYP                          
007630           MOVE IN-SUPOINT          TO WS-SUPOINT                         
007640           MOVE IN-FLINKLBS         TO WS-FLINKLBS                        
007703         END-IF                                                           
007704       END-IF                                                             
007705                                                                          
007710       PERFORM S01-LAES-W37141                                            
007800     END-PERFORM                                                          
007900                                                                          
007910     IF END-OF-W37141                                                     
007920       IF POST-FINNS                                                      
007921         IF WS-IDDISTR = UT-IDDISTR AND                                   
007930            WS-KDEXCHA = UT-KDEXCHA AND                                   
007940            WS-IDPTYP  = UT-IDPTYP  AND                                   
007950            WS-SUPOINT = UT-SUPOINT                                       
007951*           WS-FLINKLBS = 'J'                                             
007960           CONTINUE                                                       
007970         ELSE                                                             
007971           IF WS-FLINKLBS = 'J'                                           
007980             PERFORM B-FYLL-UT-AREA                                       
007990             PERFORM S11-SKRIV-W37142                                     
007991           END-IF                                                         
007992         END-IF                                                           
007993       END-IF                                                             
007994     END-IF                                                               
008000                                                                          
008100     PERFORM Z-FINIT                                                      
008200                                                                          
008300     MOVE ZERO TO RETURN-CODE                                             
008400     GOBACK                                                               
008500     .                                                                    
008600     EJECT                                                                
008700 A-INIT SECTION.                                                          
008801                                                                          
008810     OPEN INPUT  W37141                                                   
008901                                                                          
008910     OPEN OUTPUT W37142                                                   
009000     SKIP2                                                                
009100     ACCEPT DAGENS-DATUM  FROM DATE                                       
009200     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
009201     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
009202                        W-AAR                                             
009203     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
009204     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
009205     MOVE D-VECKA   TO W-VECKA                                            
009206     IF DAGENS-DATUM-AAR > 50                                             
009207       MOVE 19      TO W-SEKEL                                            
009208     ELSE                                                                 
009209       MOVE 20      TO W-SEKEL                                            
009210     END-IF                                                               
009220     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009300     .                                                                    
009400     EJECT                                                                
009410 B-FYLL-UT-AREA  SECTION.                                                 
009411     MOVE W-DAAAVV          TO UT-DAAAVV                                  
009420     MOVE WS-IDDISTR        TO UT-IDDISTR                                 
009421     MOVE WS-KDEXCHA        TO UT-KDEXCHA                                 
009422     MOVE WS-IDPTYP         TO UT-IDPTYP                                  
009423     MOVE WS-SUPOINT        TO UT-SUPOINT                                 
009424     MOVE SPACE             TO UT-KDBYTREF                                
009425                               UT-KDBYTSTA-RAPP                           
009426                               UT-KDBYTSTA-OBJ                            
009427                               UT-FLBYTKND                                
009428                               UT-FLINKLBS                                
009429                               UT-BEART-ENG                               
009430                               UT-TENOTE                                  
009431                                                                          
009432     MOVE ZERO              TO UT-DADATUM                                 
009433                               UT-IDDISTR-BET                             
009434                               UT-IDARTNR                                 
009435                               UT-IDFKNGRP                                
009436                               UT-IDKUNDNR                                
009437                               UT-IDBYTRAD                                
009438                               UT-IDORDER                                 
009439                               UT-IDBYTRAP                                
009440                               UT-KVPOINT                                 
009441                               UT-KVANTAL                                 
009442                               UT-KVRETUR-GODK                            
009443                               UT-KVVECKOR                                
009444     .                                                                    
009450     EJECT                                                                
009500 Z-FINIT SECTION.                                                         
009601     CLOSE W37141                                                         
009610           W37142                                                         
009701     SKIP2                                                                
009702     MOVE 'S' TO POSTSUM-OPKOD                                            
009710     CALL POSTSUM USING POSTSUM-PARM                                      
009800     .                                                                    
009901     EJECT                                                                
009902 S01-LAES-W37141  SECTION.                                                
009903     READ W37141 INTO IN-AREA                                             
009904     AT END                                                               
009905        MOVE HIGH-VALUE TO IN-AREA                                        
009906        SET END-OF-W37141 TO TRUE                                         
009907                                                                          
009908     NOT AT END                                                           
009910        MOVE 'W37141' TO POSTSUM-FDNAMN                                   
009911        MOVE 'W37174D1' TO POSTSUM-DDNAMN2                                
009912*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
009913*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
009914        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
009915        CALL POSTSUM USING POSTSUM-PARM                                   
009916     END-READ                                                             
009920     .                                                                    
010001     EJECT                                                                
010002 S11-SKRIV-W37142 SECTION.                                                
010003                                                                          
010004     WRITE UT-POST FROM UT-AREA                                           
010005                                                                          
010006     MOVE 'J'        TO POST-FINNS-SW                                     
010007     MOVE UT-IDPTYP  TO POSTSUM-TRANSTYP                                  
010008     MOVE 'W37142'   TO POSTSUM-FDNAMN                                    
010009     MOVE 'W37174D2' TO POSTSUM-DDNAMN2                                   
010010     CALL POSTSUM USING POSTSUM-PARM                                      
010020     .                                                                    
010200*    EJECT                                                                
010300*S99-ABEND SECTION.                                                       
010400*                                                                         
010501*    SKIP2                                                                
010502*    MOVE 'S' TO POSTSUM-OPKOD                                            
010510*    CALL POSTSUM USING POSTSUM-PARM                                      
010600*    CALL ABEND USING RKOD-ABEND                                          
010700*    .                                                                    
