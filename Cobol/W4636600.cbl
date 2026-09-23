000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4636600.                                                
000300 AUTHOR.         Sathish Thiruvengadam.                                   
000400 DATE-WRITTEN.   Aug 2024.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*FUNCTION:                                                                
000900*DIRECT BUSINESS.                                                         
001000*TO ADD HEADER RECORDS FOR THE RESPECTIVE MARKETS IN THE MQ FLOW.         
001100*ONE FILE FOR VIPS,ONE FILE FOR PRICE WILL BE CREATED.                    
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INFIL                                                      
002700     SELECT W46367                     ASSIGN TO W46366D1.                
002800     SKIP2                                                                
002900*          --- MQ FILE,VIPS                                               
003000     SELECT W4636MQ                    ASSIGN TO W46366D2.                
003100     SKIP2                                                                
003200*          --- MQ FILE,PRICE                                              
003300     SELECT W463PMQ                    ASSIGN TO W46366D3.                
003400     SKIP2                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W46367                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300 01  IN-POST           PIC X(220).                                        
004400     SKIP3                                                                
004500 FD  W4636MQ                                                              
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900 01  UT-A-POST         PIC X(190).                                        
005000     SKIP3                                                                
005100 FD  W463PMQ                                                              
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500 01  UT-PA-POST        PIC X(190).                                        
005600                                                                          
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200 77  IDPGM                       PIC X(8)    VALUE 'W4636600'.            
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500                                                                          
006600 77  W46367-EOF-SW               PIC X       VALUE 'N'.                   
006700     88  END-OF-W46367                       VALUE 'J'.                   
006800                                                                          
006900     SKIP2                                                                
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100*                                                                         
007200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     SKIP2                                                                
007500*    --- PARAMETRAR TILL ABEND                                            
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     SKIP2                                                                
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900*    --- TABELL FÖR LANDKOD                                               
009000*                                                                         
009100*01  -COPY W463LAND                                                       
009200     EJECT                                                                
009300 01  IN-AREA-START               PIC X(24)   VALUE                        
009400                                 'IN-AREA-START  '.                       
009500     SKIP2                                                                
009600                                                                          
009700 01  IN-AREA.                                                             
009800     03 IN-S-DEL                 PIC X(30).                               
009900     03 IN-URSP-POST             PIC X(160).                              
010000     03 IN-PRODTYP               PIC X(1).                                
010100     03 IN-BONUSBASE             PIC 9(11).                               
010200     03 IN-KDPRODSL              PIC 9(2).                                
010300     03 IN-IDFKNGRP              PIC 9(4).                                
010400     03 IN-IDPARTNR              PIC 9(7).                                
010500     03 IN-BASEDISC              PIC X(5).                                
010600     EJECT                                                                
010700 01  UTOK-AREA-START             PIC X(24)   VALUE                        
010800                                 'UTOK-AREA-START  '.                     
010900     SKIP2                                                                
011000                                                                          
011100 01  UT-AREA.                                                             
011200     03 UT-URSP-POST             PIC X(160).                              
011300*    03 -COPY WINVBBB0       -RED UT-URSP-POST                            
011400     03 UT-PRODTYP               PIC X(1).                                
011500     03 UT-BONUSBASE             PIC 9(11).                               
011600     03 UT-KDPRODSL              PIC 9(2).                                
011700     03 UT-IDFKNGRP              PIC 9(4).                                
011800     03 UT-IDPARTNR              PIC 9(7).                                
011900     03 UT-BASEDISC              PIC X(5).                                
012000     EJECT                                                                
012100                                                                          
012200* --- MQ HEADER WITH THE COUNTRY CODE INFO ---  **                        
012300 01  WS-LAND                     PIC X(3)    VALUE SPACE.                 
012400 01  WS-LAND-P                   PIC X(3)    VALUE SPACE.                 
012500 01  WS-IDLANDX2                 PIC X(2)    VALUE SPACE.                 
012600 01  WS-HEADER                   PIC X(16)   VALUE                        
012700                                 '¤MQMPROP Market='.                      
012800 01  UT-HEADER                   PIC X(18)   VALUE SPACE.                 
012900                                                                          
013000 PROCEDURE DIVISION.                                                      
013100 MAIN SECTION.                                                            
013200                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013500     PERFORM S01-LAES-W46367                                              
013600                                                                          
013700     PERFORM UNTIL END-OF-W46367                                          
013800       MOVE IN-URSP-POST    TO UT-URSP-POST                               
013900       MOVE IN-PRODTYP      TO UT-PRODTYP                                 
014000       MOVE IN-BONUSBASE    TO UT-BONUSBASE                               
014100       MOVE IN-KDPRODSL     TO UT-KDPRODSL                                
014200       MOVE IN-IDFKNGRP     TO UT-IDFKNGRP                                
014300       MOVE IN-IDPARTNR     TO UT-IDPARTNR                                
014400       MOVE IN-BASEDISC     TO UT-BASEDISC                                
014500                                                                          
014600       SEARCH ALL LAND-X2-ING                                             
014700          AT END                                                          
014800             IF B0-RECORDTYPE NOT = 'P'                                   
014900                 DISPLAY 'FIL MED BILLINGTRANSAR'                         
015000             ELSE                                                         
015100                 DISPLAY 'FIL MED PRICETRANSAR'                           
015200             END-IF                                                       
015300             DISPLAY 'FÖR ' B0-BILL-LOC ' KAN EJ SKRIVAS '                
015400             PERFORM S99-ABEND                                            
015500          WHEN LAND-SOK(LAND-IX) = B0-BILL-LOC                            
015600             MOVE LAND-IDLANDX2(LAND-IX) TO WS-IDLANDX2                   
015700                                                                          
015800             PERFORM S02-PREP-HEADER                                      
015900             IF B0-RECORDTYPE NOT = 'P'                                   
016000*              MQ file preparation for VIPS                               
016100                PERFORM S1A-SKRIV-W4636MQ                                 
016200             ELSE                                                         
016300*              MQ file preparation for PRICE                              
016400                PERFORM S1PA-SKRIV-W463PMQ                                
016500             END-IF                                                       
016600       END-SEARCH                                                         
016700                                                                          
016800       PERFORM S01-LAES-W46367                                            
016900     END-PERFORM                                                          
017000                                                                          
017100     PERFORM Z-FINIT                                                      
017200                                                                          
017300     MOVE ZERO TO RETURN-CODE                                             
017400     GOBACK                                                               
017500     .                                                                    
017600     EJECT                                                                
017700 A-INIT SECTION.                                                          
017800                                                                          
017900     OPEN INPUT  W46367                                                   
018000                                                                          
018100     OPEN OUTPUT W4636MQ                                                  
018200                 W463PMQ                                                  
018300                                                                          
018400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018500     .                                                                    
018600     EJECT                                                                
018700 Z-FINIT SECTION.                                                         
018800     CLOSE W46367                                                         
018900           W4636MQ                                                        
019000           W463PMQ                                                        
019100     SKIP2                                                                
019200     MOVE 'S' TO POSTSUM-OPKOD                                            
019300     CALL POSTSUM USING POSTSUM-PARM                                      
019400     .                                                                    
019500     EJECT                                                                
019600 S01-LAES-W46367  SECTION.                                                
019700                                                                          
019800     READ W46367 INTO IN-AREA                                             
019900     AT END                                                               
020000        MOVE HIGH-VALUE TO IN-AREA                                        
020100        SET END-OF-W46367 TO TRUE                                         
020200                                                                          
020300     NOT AT END                                                           
020400        MOVE 'W46367' TO POSTSUM-FDNAMN                                   
020500        MOVE 'W46366D1' TO POSTSUM-DDNAMN2                                
020600        MOVE SPACE TO POSTSUM-TRANSTYP                                    
020700        CALL POSTSUM USING POSTSUM-PARM                                   
020800     END-READ                                                             
020900     .                                                                    
021000     EJECT                                                                
021100 S02-PREP-HEADER SECTION.                                                 
021200     INITIALIZE UT-HEADER                                                 
021300                                                                          
021400     STRING WS-HEADER      DELIMITED BY SIZE                              
021500            WS-IDLANDX2    DELIMITED BY SIZE                              
021600       INTO UT-HEADER                                                     
021700     END-STRING                                                           
021800     .                                                                    
021900     EJECT                                                                
022000                                                                          
022100 S1A-SKRIV-W4636MQ SECTION.                                               
022200                                                                          
022300     IF WS-LAND NOT = B0-BILL-LOC                                         
022400        WRITE UT-A-POST    FROM UT-HEADER                                 
022500        MOVE B0-BILL-LOC   TO WS-LAND                                     
022600     END-IF                                                               
022700                                                                          
022800     WRITE UT-A-POST FROM UT-AREA                                         
022900                                                                          
023000     MOVE SPACE TO POSTSUM-TRANSTYP                                       
023100     MOVE 'W4636MQ' TO POSTSUM-FDNAMN                                     
023200     MOVE 'W46366D2' TO POSTSUM-DDNAMN2                                   
023300     CALL POSTSUM USING POSTSUM-PARM                                      
023400     .                                                                    
023500     EJECT                                                                
023600 S1PA-SKRIV-W463PMQ SECTION.                                              
023700                                                                          
023800     IF WS-LAND-P NOT = B0-BILL-LOC                                       
023900        WRITE UT-PA-POST   FROM UT-HEADER                                 
024000        MOVE B0-BILL-LOC   TO WS-LAND-P                                   
024100     END-IF                                                               
024200                                                                          
024300     WRITE UT-PA-POST FROM UT-AREA                                        
024400                                                                          
024500     MOVE SPACE TO POSTSUM-TRANSTYP                                       
024600     MOVE 'W463PMQ' TO POSTSUM-FDNAMN                                     
024700     MOVE 'W46366D3' TO POSTSUM-DDNAMN2                                   
024800     CALL POSTSUM USING POSTSUM-PARM                                      
024900     .                                                                    
025000     EJECT                                                                
025100 S99-ABEND SECTION.                                                       
025200                                                                          
025300     SKIP2                                                                
025400     MOVE 'S' TO POSTSUM-OPKOD                                            
025500     CALL POSTSUM USING POSTSUM-PARM                                      
025600     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
025700     .                                                                    
