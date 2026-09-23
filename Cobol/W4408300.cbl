000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4408300.                                                
000300 AUTHOR.         LINDA NILSSON.                                           
000400 DATE-WRITTEN.   05/02/24.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAM READS W44061 AND SELECTS BACKORDERS                      
001000*        ON SPECIFIED DISTRICTS TO BE SENT BY EMAIL LATER                 
001100*        THREE FILES CREATED TO BE USED FOR THREE LISTS                   
001200*                                                                         
001300*        W4408A USED FOR W4408400 (EPLUS)                                 
001400*        W4408B USED FOR W4408500 (EPLUS)                                 
001500*        W4408C USED FOR W4408600 (EPLUS)                                 
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U1000 - FEL SVAR FRÅN WDATKONV                                   
001900*                                                                         
002000* ETRACKER: 1545209                                                       
002010* STORY   : 2111923                                                       
002020*           23/8/2021 ADD BO-MAIL 1,2 AND 3 TO IMPORTERS PLUS             
002100*                     AUSTRALIA.                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- RESTORDERFILEN                                             
003000     SELECT W44061                     ASSIGN TO W44083D1.                
003100     SKIP2                                                                
003200*          --- RESTORDERFIL FÖR SENASTE VECKAN FÖR VISSA DISTRIKT         
003300     SELECT W4408A                     ASSIGN TO W44083D2.                
003400     SKIP2                                                                
003500*          --- TOTAL RESTORDERFIL FÖR VISSA DISTRIKT                      
003600     SELECT W4408B                     ASSIGN TO W44083D3.                
003700     SKIP2                                                                
003800*          --- RESTORDERFIL ÄLDRE ÄN TVÅ VECKOR FÖR VISSA DISTRIKT        
003900     SELECT W4408C                     ASSIGN TO W44083D4.                
004000     SKIP2                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W44061                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800*01  -COPY W44060      -L.                                                
004900     SKIP3                                                                
005000 FD  W4408A                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300*01  POST -COPY W440083 -PRE  UT1-  -L.                                   
005400     SKIP3                                                                
005500 FD  W4408B                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800*01  POST -COPY W440083 -PRE  UT2-  -L.                                   
005900     SKIP3                                                                
006000 FD  W4408C                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300*01  POST -COPY W440083 -PRE  UT3-  -L.                                   
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600                                                                          
006700 77  IDPGM                       PIC X(8)    VALUE 'W4408300'.            
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000                                                                          
007100 77  WS-EN-VECKA-SEN             PIC 9(6)    VALUE ZERO.                  
007200 77  WS-TVA-VECKOR-BAK           PIC 9(6)    VALUE ZERO.                  
007300 77  WS-RODAT                    PIC 9(6)    VALUE ZERO.                  
007400                                                                          
007500 77  W44061-EOF-SW               PIC X       VALUE 'N'.                   
007600     88  END-OF-W44061                       VALUE 'J'.                   
007700     EJECT                                                                
007800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007900 01  FILLER REDEFINES DAGENS-DATUM.                                       
008000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008300     EJECT                                                                
008400 01  WS-TOTALSUMMA               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
008500 01  WS-PRARTNTO-LOCPREL         PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008600 01  WS-PRARTNTO-LOC             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008700 01  WS-PRARTNTO                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008710 01  WS-PRAVCOST                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008800 01  TEST-IDDISTR                PIC  9(5)      COMP-3.                   
009000 01  FILLER REDEFINES TEST-IDDISTR.                                       
009100*    03 -COPY WWDIST99.                                                   
009200                                                                          
009300 01  DYNAMISKA-SUBPROGRAM.                                                
009400*                                                                         
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009800     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
009900     SKIP2                                                                
010000*    --- PARAMETRAR TILL ABEND                                            
010100                                                                          
010200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010500     SKIP2                                                                
010600 01  FELTEXT.                                                             
010700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010900*                                                                         
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL DATKORT                                          
011200*                                                                         
011300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W44083'.              
011400     SKIP2                                                                
011500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011600     SKIP2                                                                
011700*01  -COPY WDATKORT                                                       
011800     EJECT                                                                
011900*    --- PARAMETRAR TILL POSTSUM                                          
012000*                                                                         
012100*01  -COPY W0005   -PRE  POSTSUM-                                         
012200     EJECT                                                                
012300*    --- PARAMETRAR TILL WDAGAREA                                         
012400 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
012500*01  -COPY WDAGAREA                                                       
012600                                                                          
012700 01  RAD-AREA-START              PIC X(24)   VALUE                        
012800                                 'RAD-AREA-START  '.                      
012900     SKIP2                                                                
013000*01  AREA -COPY W44060     -PRE IN-                                       
013100     EJECT                                                                
013200 01  UT1-AREA-START              PIC X(24)   VALUE                        
013300                                 'UT1-AREA-START  '.                      
013400     SKIP2                                                                
013500*01  AREA -COPY W440083    -PRE UT1-                                      
013600     EJECT                                                                
013700 01  UT2-AREA-START              PIC X(24)   VALUE                        
013800                                 'UT2-AREA-START  '.                      
013900     SKIP2                                                                
014000*01  AREA -COPY W440083    -PRE UT2-                                      
014100     EJECT                                                                
014200 01  UT3-AREA-START              PIC X(24)   VALUE                        
014300                                 'UT3-AREA-START  '.                      
014400     SKIP2                                                                
014500*01  AREA -COPY W440083    -PRE UT3-                                      
014600     EJECT                                                                
014700 PROCEDURE DIVISION.                                                      
014800 MAIN SECTION.                                                            
014900     SKIP2                                                                
015000                                                                          
015100     PERFORM A-INIT                                                       
015200                                                                          
015300     PERFORM S01-LAES-W44061                                              
015400     PERFORM UNTIL END-OF-W44061                                          
015500       IF IN-RAD-KDSTARAD = '2'                                           
015600         MOVE IN-RAD-IDDISTR TO TEST-IDDISTR                              
015700         IF DIST99-RO-DIST                                                
015800           PERFORM B-BEHANDLA                                             
015900         END-IF                                                           
016000       END-IF                                                             
016100       PERFORM S01-LAES-W44061                                            
016200     END-PERFORM                                                          
016300                                                                          
016400     PERFORM Z-FINIT                                                      
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     OPEN INPUT  W44061                                                   
017300                                                                          
017400     OPEN OUTPUT W4408A                                                   
017500                 W4408B                                                   
017600                 W4408C                                                   
017700     SKIP2                                                                
017800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
017900     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
018000     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
018100     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
018200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018300                                                                          
018400                                                                          
018500*    --- TILL FIL W4408A                                                  
018600     MOVE DAGENS-DATUM    TO DAG-TIAAMMDD-TOM                             
018700     MOVE 7               TO DAG-KVKALDAG                                 
018800*    --- 1 VECKA  BAKÅT - COMPUTE FOM = TOM - DAGAR                       
018900     MOVE 003 TO DAG-KDCALL                                               
019000                                                                          
019100     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
019200                                                                          
019300     IF DAG-KDSVAR = 'F'                                                  
019400       MOVE 'FEL SVAR FRÅN WDAGKONV' TO FELTEXT                           
019500       MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                             
019600       PERFORM S99-ABEND                                                  
019700     ELSE                                                                 
019800       MOVE DAG-TIAAMMDD-FOM TO WS-EN-VECKA-SEN                           
019900     END-IF                                                               
020000*    --- TILL FIL W4408C                                                  
020100     MOVE DAGENS-DATUM    TO DAG-TIAAMMDD-TOM                             
020200     MOVE 14              TO DAG-KVKALDAG                                 
020300*    --- 2 VECKOR BAKÅT - COMPUTE FOM = TOM - DAGAR                       
020400     MOVE 003 TO DAG-KDCALL                                               
020500                                                                          
020600     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
020700     IF DAG-KDSVAR = 'F'                                                  
020800       MOVE 'FEL SVAR FRÅN WDAGKONV' TO FELTEXT                           
020900       MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                             
021000       PERFORM S99-ABEND                                                  
021100     ELSE                                                                 
021200       MOVE DAG-TIAAMMDD-FOM TO WS-TVA-VECKOR-BAK                         
021300     END-IF                                                               
021400                                                                          
021500     INITIALIZE UT1-W440083                                               
021600                UT2-W440083                                               
021700                UT3-W440083                                               
021800     .                                                                    
021900     EJECT                                                                
022000                                                                          
022100 B-BEHANDLA SECTION.                                                      
022200                                                                          
022300     IF DIST99-RO-AT                                                      
022400        MOVE 'AT ' TO UT1-IDLANDX3                                        
022500                      UT2-IDLANDX3                                        
022600                      UT3-IDLANDX3                                        
022700     END-IF                                                               
022800     IF DIST99-RO-BE                                                      
022900        MOVE 'BE ' TO UT1-IDLANDX3                                        
023000                      UT2-IDLANDX3                                        
023100                      UT3-IDLANDX3                                        
023200     END-IF                                                               
023800     IF DIST99-RO-CH                                                      
023900        MOVE 'CH ' TO UT1-IDLANDX3                                        
024000                      UT2-IDLANDX3                                        
024100                      UT3-IDLANDX3                                        
024200     END-IF                                                               
024300     IF DIST99-RO-CN4                                                     
024400        MOVE 'CN4' TO UT1-IDLANDX3                                        
024500                      UT2-IDLANDX3                                        
024600                      UT3-IDLANDX3                                        
024700     END-IF                                                               
024800     IF DIST99-RO-DE                                                      
024900        MOVE 'DE ' TO UT1-IDLANDX3                                        
025000                      UT2-IDLANDX3                                        
025100                      UT3-IDLANDX3                                        
025200     END-IF                                                               
025300     IF DIST99-RO-DK                                                      
025400        MOVE 'DK ' TO UT1-IDLANDX3                                        
025500                      UT2-IDLANDX3                                        
025600                      UT3-IDLANDX3                                        
025700     END-IF                                                               
025800     IF DIST99-RO-ES                                                      
025900        MOVE 'ES ' TO UT1-IDLANDX3                                        
026000                      UT2-IDLANDX3                                        
026100                      UT3-IDLANDX3                                        
026200     END-IF                                                               
026300     IF DIST99-RO-FI                                                      
026400        MOVE 'FI ' TO UT1-IDLANDX3                                        
026500                      UT2-IDLANDX3                                        
026600                      UT3-IDLANDX3                                        
026700     END-IF                                                               
026800     IF DIST99-RO-FR                                                      
026900        MOVE 'FR ' TO UT1-IDLANDX3                                        
027000                      UT2-IDLANDX3                                        
027100                      UT3-IDLANDX3                                        
027200     END-IF                                                               
027300     IF DIST99-RO-GB                                                      
027400        MOVE 'GB ' TO UT1-IDLANDX3                                        
027500                      UT2-IDLANDX3                                        
027600                      UT3-IDLANDX3                                        
027700     END-IF                                                               
027800     IF DIST99-RO-GR                                                      
027900        MOVE 'GR ' TO UT1-IDLANDX3                                        
028000                      UT2-IDLANDX3                                        
028100                      UT3-IDLANDX3                                        
028200     END-IF                                                               
028300     IF DIST99-RO-HK                                                      
028400        MOVE 'HK ' TO UT1-IDLANDX3                                        
028500                      UT2-IDLANDX3                                        
028600                      UT3-IDLANDX3                                        
028700     END-IF                                                               
028800     IF DIST99-RO-IL                                                      
028900        MOVE 'IL ' TO UT1-IDLANDX3                                        
029000                      UT2-IDLANDX3                                        
029100                      UT3-IDLANDX3                                        
029200     END-IF                                                               
029300     IF DIST99-RO-IT                                                      
029400        MOVE 'IT ' TO UT1-IDLANDX3                                        
029500                      UT2-IDLANDX3                                        
029600                      UT3-IDLANDX3                                        
029700     END-IF                                                               
029800     IF DIST99-RO-MY                                                      
029900        MOVE 'MY ' TO UT1-IDLANDX3                                        
030000                      UT2-IDLANDX3                                        
030100                      UT3-IDLANDX3                                        
030200     END-IF                                                               
030300     IF DIST99-RO-NL                                                      
030400        MOVE 'NL ' TO UT1-IDLANDX3                                        
030500                      UT2-IDLANDX3                                        
030600                      UT3-IDLANDX3                                        
030700     END-IF                                                               
030800     IF DIST99-RO-NO                                                      
030900        MOVE 'NO ' TO UT1-IDLANDX3                                        
031000                      UT2-IDLANDX3                                        
031100                      UT3-IDLANDX3                                        
031200     END-IF                                                               
031300     IF DIST99-RO-PT                                                      
031400        MOVE 'PT ' TO UT1-IDLANDX3                                        
031500                      UT2-IDLANDX3                                        
031600                      UT3-IDLANDX3                                        
031700     END-IF                                                               
031800     IF DIST99-RO-SE                                                      
031900        MOVE 'SE ' TO UT1-IDLANDX3                                        
032000                      UT2-IDLANDX3                                        
032100                      UT3-IDLANDX3                                        
032200     END-IF                                                               
032300     IF DIST99-RO-SG                                                      
032400        MOVE 'SG ' TO UT1-IDLANDX3                                        
032500                      UT2-IDLANDX3                                        
032600                      UT3-IDLANDX3                                        
032700     END-IF                                                               
032800     IF DIST99-RO-TH                                                      
032900        MOVE 'TH ' TO UT1-IDLANDX3                                        
033000                      UT2-IDLANDX3                                        
033100                      UT3-IDLANDX3                                        
033200     END-IF                                                               
033300     IF DIST99-RO-TW                                                      
033400        MOVE 'TW ' TO UT1-IDLANDX3                                        
033500                      UT2-IDLANDX3                                        
033600                      UT3-IDLANDX3                                        
033700     END-IF                                                               
033800     IF DIST99-RO-US                                                      
033900        MOVE 'US ' TO UT1-IDLANDX3                                        
034000                      UT2-IDLANDX3                                        
034100                      UT3-IDLANDX3                                        
034200     END-IF                                                               
034300     IF DIST99-RO-ZA                                                      
034400        MOVE 'ZA ' TO UT1-IDLANDX3                                        
034500                      UT2-IDLANDX3                                        
034600                      UT3-IDLANDX3                                        
034700     END-IF                                                               
034710     IF DIST99-RO-IN                                                      
034720        MOVE 'IN ' TO UT1-IDLANDX3                                        
034730                      UT2-IDLANDX3                                        
034740                      UT3-IDLANDX3                                        
034750     END-IF                                                               
034760     IF DIST99-RO-KR                                                      
034770        MOVE 'KR ' TO UT1-IDLANDX3                                        
034780                      UT2-IDLANDX3                                        
034790                      UT3-IDLANDX3                                        
034791     END-IF                                                               
034792     IF DIST99-RO-IS                                                      
034793        MOVE 'IS ' TO UT1-IDLANDX3                                        
034794                      UT2-IDLANDX3                                        
034795                      UT3-IDLANDX3                                        
034796     END-IF                                                               
034797     IF DIST99-RO-IE                                                      
034798        MOVE 'IE ' TO UT1-IDLANDX3                                        
034799                      UT2-IDLANDX3                                        
034800                      UT3-IDLANDX3                                        
034801     END-IF                                                               
034802     IF DIST99-RO-CZ                                                      
034806        MOVE 'CZ ' TO UT1-IDLANDX3                                        
034807                      UT2-IDLANDX3                                        
034808                      UT3-IDLANDX3                                        
034813     END-IF                                                               
034814     IF DIST99-RO-HU                                                      
034815        MOVE 'HU ' TO UT1-IDLANDX3                                        
034816                      UT2-IDLANDX3                                        
034817                      UT3-IDLANDX3                                        
034818     END-IF                                                               
034819     IF DIST99-RO-SI                                                      
034820        MOVE 'SI ' TO UT1-IDLANDX3                                        
034821                      UT2-IDLANDX3                                        
034822                      UT3-IDLANDX3                                        
034823     END-IF                                                               
034824     IF DIST99-RO-BA                                                      
034825        MOVE 'BA ' TO UT1-IDLANDX3                                        
034826                      UT2-IDLANDX3                                        
034827                      UT3-IDLANDX3                                        
034828     END-IF                                                               
034829     IF DIST99-RO-MK                                                      
034830        MOVE 'MK ' TO UT1-IDLANDX3                                        
034831                      UT2-IDLANDX3                                        
034832                      UT3-IDLANDX3                                        
034833     END-IF                                                               
034834     IF DIST99-RO-ME                                                      
034835        MOVE 'ME ' TO UT1-IDLANDX3                                        
034836                      UT2-IDLANDX3                                        
034837                      UT3-IDLANDX3                                        
034838     END-IF                                                               
034839     IF DIST99-RO-SK                                                      
034840        MOVE 'SK ' TO UT1-IDLANDX3                                        
034841                      UT2-IDLANDX3                                        
034842                      UT3-IDLANDX3                                        
034843     END-IF                                                               
034844     IF DIST99-RO-RS                                                      
034845        MOVE 'RS ' TO UT1-IDLANDX3                                        
034846                      UT2-IDLANDX3                                        
034847                      UT3-IDLANDX3                                        
034848     END-IF                                                               
034849     IF DIST99-RO-AL                                                      
034850        MOVE 'AL ' TO UT1-IDLANDX3                                        
034851                      UT2-IDLANDX3                                        
034852                      UT3-IDLANDX3                                        
034853     END-IF                                                               
034854     IF DIST99-RO-BG                                                      
034855        MOVE 'BG ' TO UT1-IDLANDX3                                        
034856                      UT2-IDLANDX3                                        
034857                      UT3-IDLANDX3                                        
034858     END-IF                                                               
034859     IF DIST99-RO-RU                                                      
034860        MOVE 'RU ' TO UT1-IDLANDX3                                        
034861                      UT2-IDLANDX3                                        
034862                      UT3-IDLANDX3                                        
034863     END-IF                                                               
034864     IF DIST99-RO-UA                                                      
034865        MOVE 'UA ' TO UT1-IDLANDX3                                        
034866                      UT2-IDLANDX3                                        
034867                      UT3-IDLANDX3                                        
034868     END-IF                                                               
034869     IF DIST99-RO-UZ                                                      
034870        MOVE 'UZ ' TO UT1-IDLANDX3                                        
034871                      UT2-IDLANDX3                                        
034872                      UT3-IDLANDX3                                        
034873     END-IF                                                               
034874     IF DIST99-RO-AZ                                                      
034875        MOVE 'AZ ' TO UT1-IDLANDX3                                        
034876                      UT2-IDLANDX3                                        
034877                      UT3-IDLANDX3                                        
034878     END-IF                                                               
034879     IF DIST99-RO-RO                                                      
034880        MOVE 'RO ' TO UT1-IDLANDX3                                        
034881                      UT2-IDLANDX3                                        
034882                      UT3-IDLANDX3                                        
034883     END-IF                                                               
034884     IF DIST99-RO-MD                                                      
034885        MOVE 'MD ' TO UT1-IDLANDX3                                        
034886                      UT2-IDLANDX3                                        
034887                      UT3-IDLANDX3                                        
034888     END-IF                                                               
034889     IF DIST99-RO-GE                                                      
034890        MOVE 'GE ' TO UT1-IDLANDX3                                        
034891                      UT2-IDLANDX3                                        
034892                      UT3-IDLANDX3                                        
034893     END-IF                                                               
034894     IF DIST99-RO-AM                                                      
034895        MOVE 'AM ' TO UT1-IDLANDX3                                        
034896                      UT2-IDLANDX3                                        
034897                      UT3-IDLANDX3                                        
034898     END-IF                                                               
034899     IF DIST99-RO-PL                                                      
034900        MOVE 'PL ' TO UT1-IDLANDX3                                        
034901                      UT2-IDLANDX3                                        
034902                      UT3-IDLANDX3                                        
034903     END-IF                                                               
034904     IF DIST99-RO-MT                                                      
034905        MOVE 'MT ' TO UT1-IDLANDX3                                        
034906                      UT2-IDLANDX3                                        
034907                      UT3-IDLANDX3                                        
034908     END-IF                                                               
034909     IF DIST99-RO-AO                                                      
034910        MOVE 'AO ' TO UT1-IDLANDX3                                        
034911                      UT2-IDLANDX3                                        
034912                      UT3-IDLANDX3                                        
034913     END-IF                                                               
034914     IF DIST99-RO-EG                                                      
034915        MOVE 'EG ' TO UT1-IDLANDX3                                        
034916                      UT2-IDLANDX3                                        
034917                      UT3-IDLANDX3                                        
034918     END-IF                                                               
034919     IF DIST99-RO-MA                                                      
034920        MOVE 'MA ' TO UT1-IDLANDX3                                        
034921                      UT2-IDLANDX3                                        
034922                      UT3-IDLANDX3                                        
034923     END-IF                                                               
034924     IF DIST99-RO-TN                                                      
034925        MOVE 'TN ' TO UT1-IDLANDX3                                        
034926                      UT2-IDLANDX3                                        
034927                      UT3-IDLANDX3                                        
034928     END-IF                                                               
034929     IF DIST99-RO-MU                                                      
034930        MOVE 'MU ' TO UT1-IDLANDX3                                        
034931                      UT2-IDLANDX3                                        
034932                      UT3-IDLANDX3                                        
034933     END-IF                                                               
034934     IF DIST99-RO-SA                                                      
034935        MOVE 'SA ' TO UT1-IDLANDX3                                        
034936                      UT2-IDLANDX3                                        
034937                      UT3-IDLANDX3                                        
034938     END-IF                                                               
034939     IF DIST99-RO-JO                                                      
034940        MOVE 'JO ' TO UT1-IDLANDX3                                        
034941                      UT2-IDLANDX3                                        
034942                      UT3-IDLANDX3                                        
034943     END-IF                                                               
034944     IF DIST99-RO-KW                                                      
034945        MOVE 'KW ' TO UT1-IDLANDX3                                        
034946                      UT2-IDLANDX3                                        
034947                      UT3-IDLANDX3                                        
034948     END-IF                                                               
034949     IF DIST99-RO-QA                                                      
034950        MOVE 'QA ' TO UT1-IDLANDX3                                        
034951                      UT2-IDLANDX3                                        
034952                      UT3-IDLANDX3                                        
034953     END-IF                                                               
034954     IF DIST99-RO-LB                                                      
034955        MOVE 'LB ' TO UT1-IDLANDX3                                        
034956                      UT2-IDLANDX3                                        
034957                      UT3-IDLANDX3                                        
034958     END-IF                                                               
034959     IF DIST99-RO-MM                                                      
034960        MOVE 'MM ' TO UT1-IDLANDX3                                        
034961                      UT2-IDLANDX3                                        
034962                      UT3-IDLANDX3                                        
034963     END-IF                                                               
034964     IF DIST99-RO-BN                                                      
034965        MOVE 'BN ' TO UT1-IDLANDX3                                        
034966                      UT2-IDLANDX3                                        
034967                      UT3-IDLANDX3                                        
034968     END-IF                                                               
034969     IF DIST99-RO-TR                                                      
034970        MOVE 'TR ' TO UT1-IDLANDX3                                        
034971                      UT2-IDLANDX3                                        
034972                      UT3-IDLANDX3                                        
034973     END-IF                                                               
034974     IF DIST99-RO-ID                                                      
034975        MOVE 'ID ' TO UT1-IDLANDX3                                        
034976                      UT2-IDLANDX3                                        
034977                      UT3-IDLANDX3                                        
034978     END-IF                                                               
034979     IF DIST99-RO-PH                                                      
034980        MOVE 'PH ' TO UT1-IDLANDX3                                        
034981                      UT2-IDLANDX3                                        
034982                      UT3-IDLANDX3                                        
034983     END-IF                                                               
034984     IF DIST99-RO-BD                                                      
034985        MOVE 'BD ' TO UT1-IDLANDX3                                        
034986                      UT2-IDLANDX3                                        
034987                      UT3-IDLANDX3                                        
034988     END-IF                                                               
034989     IF DIST99-RO-LK                                                      
034990        MOVE 'LK ' TO UT1-IDLANDX3                                        
034991                      UT2-IDLANDX3                                        
034992                      UT3-IDLANDX3                                        
034993     END-IF                                                               
034994     IF DIST99-RO-CY                                                      
034995        MOVE 'CY ' TO UT1-IDLANDX3                                        
034996                      UT2-IDLANDX3                                        
034997                      UT3-IDLANDX3                                        
034998     END-IF                                                               
034999     IF DIST99-RO-YE                                                      
035000        MOVE 'YE ' TO UT1-IDLANDX3                                        
035001                      UT2-IDLANDX3                                        
035002                      UT3-IDLANDX3                                        
035003     END-IF                                                               
035004     IF DIST99-RO-OM                                                      
035005        MOVE 'OM ' TO UT1-IDLANDX3                                        
035006                      UT2-IDLANDX3                                        
035007                      UT3-IDLANDX3                                        
035008     END-IF                                                               
035009     IF DIST99-RO-BH                                                      
035010        MOVE 'BH ' TO UT1-IDLANDX3                                        
035011                      UT2-IDLANDX3                                        
035012                      UT3-IDLANDX3                                        
035013     END-IF                                                               
035014     IF DIST99-RO-AE                                                      
035015        MOVE 'AE ' TO UT1-IDLANDX3                                        
035016                      UT2-IDLANDX3                                        
035017                      UT3-IDLANDX3                                        
035018     END-IF                                                               
035024     IF DIST99-RO-MX                                                      
035025        MOVE 'MX ' TO UT1-IDLANDX3                                        
035026                      UT2-IDLANDX3                                        
035027                      UT3-IDLANDX3                                        
035028     END-IF                                                               
035029     IF DIST99-RO-PY                                                      
035030        MOVE 'PY ' TO UT1-IDLANDX3                                        
035031                      UT2-IDLANDX3                                        
035032                      UT3-IDLANDX3                                        
035033     END-IF                                                               
035034     IF DIST99-RO-PE                                                      
035035        MOVE 'PE ' TO UT1-IDLANDX3                                        
035036                      UT2-IDLANDX3                                        
035037                      UT3-IDLANDX3                                        
035038     END-IF                                                               
035039     IF DIST99-RO-BR                                                      
035040        MOVE 'BR ' TO UT1-IDLANDX3                                        
035041                      UT2-IDLANDX3                                        
035042                      UT3-IDLANDX3                                        
035043     END-IF                                                               
035044     IF DIST99-RO-GY                                                      
035045        MOVE 'GY ' TO UT1-IDLANDX3                                        
035046                      UT2-IDLANDX3                                        
035047                      UT3-IDLANDX3                                        
035048     END-IF                                                               
035049     IF DIST99-RO-SR                                                      
035050        MOVE 'SR ' TO UT1-IDLANDX3                                        
035051                      UT2-IDLANDX3                                        
035052                      UT3-IDLANDX3                                        
035053     END-IF                                                               
035054     IF DIST99-RO-CR                                                      
035055        MOVE 'CR ' TO UT1-IDLANDX3                                        
035056                      UT2-IDLANDX3                                        
035057                      UT3-IDLANDX3                                        
035058     END-IF                                                               
035059     IF DIST99-RO-PA                                                      
035060        MOVE 'PA ' TO UT1-IDLANDX3                                        
035061                      UT2-IDLANDX3                                        
035062                      UT3-IDLANDX3                                        
035063     END-IF                                                               
035064     IF DIST99-RO-DO                                                      
035065        MOVE 'DO ' TO UT1-IDLANDX3                                        
035066                      UT2-IDLANDX3                                        
035067                      UT3-IDLANDX3                                        
035068     END-IF                                                               
035069     IF DIST99-RO-PR                                                      
035070        MOVE 'PR ' TO UT1-IDLANDX3                                        
035071                      UT2-IDLANDX3                                        
035072                      UT3-IDLANDX3                                        
035073     END-IF                                                               
035074     IF DIST99-RO-EC                                                      
035075        MOVE 'EC ' TO UT1-IDLANDX3                                        
035076                      UT2-IDLANDX3                                        
035077                      UT3-IDLANDX3                                        
035078     END-IF                                                               
035079     IF DIST99-RO-CO                                                      
035080        MOVE 'CO ' TO UT1-IDLANDX3                                        
035081                      UT2-IDLANDX3                                        
035082                      UT3-IDLANDX3                                        
035083     END-IF                                                               
035084     IF DIST99-RO-UY                                                      
035085        MOVE 'UY ' TO UT1-IDLANDX3                                        
035086                      UT2-IDLANDX3                                        
035087                      UT3-IDLANDX3                                        
035088     END-IF                                                               
035089     IF DIST99-RO-KH                                                      
035090        MOVE 'KH ' TO UT1-IDLANDX3                                        
035091                      UT2-IDLANDX3                                        
035092                      UT3-IDLANDX3                                        
035093     END-IF                                                               
035094     IF DIST99-RO-VN                                                      
035095        MOVE 'VN ' TO UT1-IDLANDX3                                        
035096                      UT2-IDLANDX3                                        
035097                      UT3-IDLANDX3                                        
035098     END-IF                                                               
035099     IF DIST99-RO-AU                                                      
035100        MOVE 'AU ' TO UT1-IDLANDX3                                        
035101                      UT2-IDLANDX3                                        
035102                      UT3-IDLANDX3                                        
035103     END-IF                                                               
035104     IF DIST99-RO-ZA                                                      
035105        MOVE 'ZA ' TO UT1-IDLANDX3                                        
035106                      UT2-IDLANDX3                                        
035107                      UT3-IDLANDX3                                        
035108     END-IF                                                               
035109                                                                          
035110     PERFORM BD-SAME-DIST-MANY-COUNTRIES                                  
035111     PERFORM BB-FLYTTA-TILL-W4408B                                        
035112     MOVE IN-RAD-DARODAT (3:6) TO WS-RODAT                                
035120     IF WS-RODAT >= WS-EN-VECKA-SEN                                       
035200     AND WS-RODAT <= DAGENS-DATUM                                         
035300       PERFORM BA-FLYTTA-TILL-W4408A                                      
035400     END-IF                                                               
035500     IF WS-TVA-VECKOR-BAK >= WS-RODAT                                     
035600       PERFORM BC-FLYTTA-TILL-W4408C                                      
035700     END-IF                                                               
035800     .                                                                    
035900     EJECT                                                                
036000 BA-FLYTTA-TILL-W4408A SECTION.                                           
036100                                                                          
036200     MOVE ZERO                        TO WS-TOTALSUMMA                    
036300                                         WS-PRARTNTO-LOCPREL              
036400                                         WS-PRARTNTO-LOC                  
036500                                         WS-PRARTNTO                      
036600     MOVE +1                          TO UT1-KVRADER                      
036700     MOVE IN-RAD-IDARTNR              TO UT1-IDARTNR                      
036800     MOVE SPACE                       TO UT1-IDLEVNR                      
036900     MOVE IN-RAD-KDPRODSL             TO UT1-KDPRODSL                     
037000     MOVE IN-RAD-DARODAT              TO UT1-DARODAT                      
037100     MOVE IN-RAD-KVBEART-Q            TO UT1-KVBEART-Q                    
037200     IF IN-RAD-PRARTNTO-LOCPREL > 0                                       
037300       COMPUTE WS-PRARTNTO-LOCPREL = IN-RAD-PRARTNTO-LOCPREL              
037400                                   * IN-RAD-KVBEART-Q                     
037500       MOVE WS-PRARTNTO-LOCPREL       TO UT1-PRARTNTO-LOCPREL             
037600       MOVE ZERO                      TO UT1-PRARTNTO-LOC                 
037700                                         UT1-PRARTNTO                     
037800     ELSE                                                                 
037900       IF IN-RAD-PRARTNTO-LOC > 0                                         
038000         COMPUTE WS-PRARTNTO-LOC = IN-RAD-PRARTNTO-LOC                    
038100                                 * IN-RAD-KVBEART-Q                       
038200         MOVE WS-PRARTNTO-LOC         TO UT1-PRARTNTO-LOC                 
038300         MOVE ZERO                    TO UT1-PRARTNTO-LOCPREL             
038400                                         UT1-PRARTNTO                     
038500       ELSE                                                               
038501         IF IN-RAD-PRAVCOST > 0                                           
038502            COMPUTE WS-PRAVCOST = IN-RAD-PRAVCOST                         
038503                                * IN-RAD-KVBEART-Q                        
038504            MOVE WS-PRAVCOST         TO UT1-PRARTNTO                      
038505            MOVE ZERO                TO UT1-PRARTNTO-LOCPREL              
038506                                        UT1-PRARTNTO-LOC                  
038508         ELSE                                                             
038600            COMPUTE WS-PRARTNTO = IN-RAD-PRARTNTO                         
038700                                * IN-RAD-KVBEART-Q                        
038800            MOVE WS-PRARTNTO         TO UT1-PRARTNTO                      
038900            MOVE ZERO                TO UT1-PRARTNTO-LOCPREL              
039000                                        UT1-PRARTNTO-LOC                  
039010         END-IF                                                           
039100       END-IF                                                             
039200     END-IF                                                               
039210     IF WS-PRAVCOST > 0                                                   
039211        COMPUTE WS-TOTALSUMMA = WS-TOTALSUMMA                             
039214                            + WS-PRAVCOST                                 
039215     ELSE                                                                 
039300        COMPUTE WS-TOTALSUMMA = WS-TOTALSUMMA                             
039400                            + WS-PRARTNTO-LOCPREL                         
039500                            + WS-PRARTNTO-LOC                             
039600                            + WS-PRARTNTO                                 
039610     END-IF                                                               
039700     MOVE WS-TOTALSUMMA               TO UT1-SUORDV                       
039800     MOVE IN-RAD-IDDC-RO              TO UT1-IDDC-RO                      
039900     PERFORM S11-SKRIV-W4408A                                             
040000     .                                                                    
040100     EJECT                                                                
040200 BB-FLYTTA-TILL-W4408B SECTION.                                           
040300                                                                          
040400     MOVE ZERO                        TO WS-TOTALSUMMA                    
040500                                         WS-PRARTNTO-LOCPREL              
040600                                         WS-PRARTNTO-LOC                  
040700                                         WS-PRARTNTO                      
040710                                         WS-PRAVCOST                      
040800     MOVE +1                          TO UT2-KVRADER                      
040900     MOVE IN-RAD-IDARTNR              TO UT2-IDARTNR                      
041000     MOVE SPACE                       TO UT2-IDLEVNR                      
041100     MOVE IN-RAD-KDPRODSL             TO UT2-KDPRODSL                     
041200     MOVE IN-RAD-DARODAT              TO UT2-DARODAT                      
041300     MOVE IN-RAD-KVBEART-Q            TO UT2-KVBEART-Q                    
041400                                                                          
041500     IF IN-RAD-PRARTNTO-LOCPREL > 0                                       
041600       COMPUTE WS-PRARTNTO-LOCPREL = IN-RAD-PRARTNTO-LOCPREL              
041700                                   * IN-RAD-KVBEART-Q                     
041800       MOVE WS-PRARTNTO-LOCPREL       TO UT2-PRARTNTO-LOCPREL             
041900       MOVE ZERO                      TO UT2-PRARTNTO-LOC                 
042000                                         UT2-PRARTNTO                     
042100     ELSE                                                                 
042200       IF IN-RAD-PRARTNTO-LOC > 0                                         
042300         COMPUTE WS-PRARTNTO-LOC = IN-RAD-PRARTNTO-LOC                    
042400                                 * IN-RAD-KVBEART-Q                       
042500         MOVE WS-PRARTNTO-LOC         TO UT2-PRARTNTO-LOC                 
042600         MOVE ZERO                    TO UT2-PRARTNTO-LOCPREL             
042700                                         UT2-PRARTNTO                     
042800       ELSE                                                               
042810         IF IN-RAD-PRAVCOST > 0                                           
042820            COMPUTE WS-PRAVCOST = IN-RAD-PRAVCOST                         
042830                                * IN-RAD-KVBEART-Q                        
042840            MOVE WS-PRAVCOST      TO UT2-PRARTNTO                         
042860            MOVE ZERO             TO UT2-PRARTNTO-LOCPREL                 
042870                                     UT2-PRARTNTO-LOC                     
042880         ELSE                                                             
042900            COMPUTE WS-PRARTNTO = IN-RAD-PRARTNTO                         
043000                                * IN-RAD-KVBEART-Q                        
043100            MOVE WS-PRARTNTO             TO UT2-PRARTNTO                  
043200            MOVE ZERO                    TO UT2-PRARTNTO-LOCPREL          
043300                                            UT2-PRARTNTO-LOC              
043320         END-IF                                                           
043400       END-IF                                                             
043500     END-IF                                                               
043510     IF WS-PRAVCOST > 0                                                   
043520        COMPUTE WS-TOTALSUMMA = WS-TOTALSUMMA                             
043530                              + WS-PRAVCOST                               
043540     ELSE                                                                 
043600        COMPUTE WS-TOTALSUMMA = WS-TOTALSUMMA                             
043700                               + WS-PRARTNTO-LOCPREL                      
043800                               + WS-PRARTNTO-LOC                          
043900                               + WS-PRARTNTO                              
043910     END-IF                                                               
044000     MOVE WS-TOTALSUMMA               TO UT2-SUORDV                       
044010     MOVE IN-RAD-IDDC-RO              TO UT2-IDDC-RO                      
044100     PERFORM S12-SKRIV-W4408B                                             
044200     .                                                                    
044300     EJECT                                                                
044400 BC-FLYTTA-TILL-W4408C SECTION.                                           
044500                                                                          
044600     MOVE ZERO                        TO WS-TOTALSUMMA                    
044700                                         WS-PRARTNTO-LOCPREL              
044800                                         WS-PRARTNTO-LOC                  
044900                                         WS-PRARTNTO                      
044910                                         WS-PRAVCOST                      
045000     MOVE +1                          TO UT3-KVRADER                      
045100     MOVE IN-RAD-IDARTNR              TO UT3-IDARTNR                      
045200     MOVE SPACE                       TO UT3-IDLEVNR                      
045300     MOVE IN-RAD-KDPRODSL             TO UT3-KDPRODSL                     
045400     MOVE IN-RAD-DARODAT              TO UT3-DARODAT                      
045500     MOVE IN-RAD-KVBEART-Q            TO UT3-KVBEART-Q                    
045510     MOVE IN-RAD-IDDISTR              TO UT3-IDDISTR                      
045520     MOVE IN-RAD-IDKUNDNR             TO UT3-IDKUNDNR                     
045530     MOVE IN-RAD-IDORDNR5             TO UT3-IDORDNR5                     
045540     MOVE IN-RAD-KVRO                 TO UT3-KVRO                         
045550     MOVE IN-RAD-TIREGDAT             TO UT3-TIREGDAT                     
045600                                                                          
045700     IF IN-RAD-PRARTNTO-LOCPREL > 0                                       
045800       COMPUTE WS-PRARTNTO-LOCPREL = IN-RAD-PRARTNTO-LOCPREL              
045900                                   * IN-RAD-KVBEART-Q                     
046000       MOVE WS-PRARTNTO-LOCPREL       TO UT3-PRARTNTO-LOCPREL             
046100       MOVE ZERO                      TO UT3-PRARTNTO-LOC                 
046200                                         UT3-PRARTNTO                     
046300     ELSE                                                                 
046400       IF IN-RAD-PRARTNTO-LOC > 0                                         
046500         COMPUTE WS-PRARTNTO-LOC = IN-RAD-PRARTNTO-LOC                    
046600                                 * IN-RAD-KVBEART-Q                       
046700         MOVE WS-PRARTNTO-LOC         TO UT3-PRARTNTO-LOC                 
046800         MOVE ZERO                    TO UT3-PRARTNTO-LOCPREL             
046900                                         UT3-PRARTNTO                     
047000       ELSE                                                               
047010         IF IN-RAD-PRAVCOST > 0                                           
047020           COMPUTE WS-PRAVCOST = IN-RAD-PRAVCOST                          
047030                               * IN-RAD-KVBEART-Q                         
047040           MOVE WS-PRAVCOST             TO UT3-PRARTNTO                   
047050           MOVE ZERO                    TO UT3-PRARTNTO-LOCPREL           
047060                                           UT3-PRARTNTO-LOC               
047070         ELSE                                                             
047100           COMPUTE WS-PRARTNTO = IN-RAD-PRARTNTO                          
047200                               * IN-RAD-KVBEART-Q                         
047300           MOVE WS-PRARTNTO             TO UT3-PRARTNTO                   
047400           MOVE ZERO                    TO UT3-PRARTNTO-LOCPREL           
047500                                           UT3-PRARTNTO-LOC               
047510         END-IF                                                           
047600       END-IF                                                             
047700     END-IF                                                               
047710     IF WS-PRAVCOST > 0                                                   
047720        COMPUTE WS-TOTALSUMMA = WS-TOTALSUMMA                             
047730                               + WS-PRAVCOST                              
047740     ELSE                                                                 
047800        COMPUTE WS-TOTALSUMMA = WS-TOTALSUMMA                             
047900                               + WS-PRARTNTO-LOCPREL                      
048000                               + WS-PRARTNTO-LOC                          
048100                               + WS-PRARTNTO                              
048110     END-IF                                                               
048200     MOVE WS-TOTALSUMMA               TO UT3-SUORDV                       
048210     MOVE IN-RAD-IDDC-RO              TO UT3-IDDC-RO                      
048300     PERFORM S13-SKRIV-W4408C                                             
048400     .                                                                    
048500     EJECT                                                                
048600 BD-SAME-DIST-MANY-COUNTRIES SECTION.                                     
048601*FOR DISTRICT 6480,IDKUNDNR = 0   ->CHILE                                 
048602*                  IDKUNDNR = 100 ->BOLIVIA                               
048603*                  IDKUNDNR = 200 ->ARGENTINA(NOT USED ANYMORE)           
048604     IF DIST99-RO-CL                                                      
048605        IF IN-RAD-IDKUNDNR = 0                                            
048606           MOVE 'CL ' TO UT1-IDLANDX3                                     
048607                         UT2-IDLANDX3                                     
048608                         UT3-IDLANDX3                                     
048609        END-IF                                                            
048610        IF IN-RAD-IDKUNDNR = 100                                          
048611           MOVE 'BL ' TO UT1-IDLANDX3                                     
048612                         UT2-IDLANDX3                                     
048613                         UT3-IDLANDX3                                     
048614        END-IF                                                            
048615        IF IN-RAD-IDKUNDNR = 200                                          
048616           MOVE 'AR ' TO UT1-IDLANDX3                                     
048617                         UT2-IDLANDX3                                     
048618                         UT3-IDLANDX3                                     
048619        END-IF                                                            
048620     END-IF                                                               
048621*FOR DISTRICT 2635,IDKUNDNR = 2630 ->ESTLAND                              
048622*                  IDKUNDNR = 2645 ->LETTLAND                             
048623*                  IDKUNDNR = 2650 ->VILNIUS                              
048624     IF DIST99-RO-LV                                                      
048625        IF IN-RAD-IDKUNDNR = 2630                                         
048626           MOVE 'EE ' TO UT1-IDLANDX3                                     
048627                         UT2-IDLANDX3                                     
048628                         UT3-IDLANDX3                                     
048629        END-IF                                                            
048630        IF IN-RAD-IDKUNDNR = 2645                                         
048631           MOVE 'LV ' TO UT1-IDLANDX3                                     
048632                         UT2-IDLANDX3                                     
048633                         UT3-IDLANDX3                                     
048634        END-IF                                                            
048635        IF IN-RAD-IDKUNDNR = 2650                                         
048636           MOVE 'LT ' TO UT1-IDLANDX3                                     
048637                         UT2-IDLANDX3                                     
048638                         UT3-IDLANDX3                                     
048639        END-IF                                                            
048640     END-IF                                                               
048641*FOR DISTRICT 7490,IDKUNDNR = 200  ->EL SALVADOR                          
048642*                  IDKUNDNR = 0    ->GUATEMALA                            
048643*                  IDKUNDNR = 300  ->HONDURAS                             
048644*                  IDKUNDNR = 500  ->NICARAGUA                            
048645*                  IDKUNDNR = 701  ->TRINIDAD                             
048646     IF DIST99-RO-SV                                                      
048647        IF IN-RAD-IDKUNDNR = 200                                          
048648           MOVE 'SV ' TO UT1-IDLANDX3                                     
048649                         UT2-IDLANDX3                                     
048650                         UT3-IDLANDX3                                     
048651        END-IF                                                            
048652        IF IN-RAD-IDKUNDNR = 0                                            
048653           MOVE 'GT ' TO UT1-IDLANDX3                                     
048654                         UT2-IDLANDX3                                     
048655                         UT3-IDLANDX3                                     
048656        END-IF                                                            
048657        IF IN-RAD-IDKUNDNR = 300                                          
048658           MOVE 'HN ' TO UT1-IDLANDX3                                     
048659                         UT2-IDLANDX3                                     
048660                         UT3-IDLANDX3                                     
048661        END-IF                                                            
048662        IF IN-RAD-IDKUNDNR = 500                                          
048663           MOVE 'NI ' TO UT1-IDLANDX3                                     
048664                         UT2-IDLANDX3                                     
048665                         UT3-IDLANDX3                                     
048666        END-IF                                                            
048667        IF IN-RAD-IDKUNDNR = 701                                          
048668           MOVE 'TT ' TO UT1-IDLANDX3                                     
048669                         UT2-IDLANDX3                                     
048670                         UT3-IDLANDX3                                     
048671        END-IF                                                            
048672     END-IF                                                               
048673     .                                                                    
048674     EJECT                                                                
048680 Z-FINIT SECTION.                                                         
048700     CLOSE W44061                                                         
048800           W4408A                                                         
048900           W4408B                                                         
049000           W4408C                                                         
049100     SKIP2                                                                
049200     MOVE 'S' TO POSTSUM-OPKOD                                            
049300     CALL POSTSUM USING POSTSUM-PARM                                      
049400     .                                                                    
049500     EJECT                                                                
049600 S01-LAES-W44061  SECTION.                                                
049700     READ W44061 INTO IN-AREA                                             
049800     AT END                                                               
049900        MOVE HIGH-VALUE TO IN-AREA                                        
050000        SET END-OF-W44061 TO TRUE                                         
050100                                                                          
050200     NOT AT END                                                           
050300        MOVE 'W44083'   TO POSTSUM-FDNAMN                                 
050400        MOVE 'W44083D1' TO POSTSUM-DDNAMN2                                
050500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
050600        CALL POSTSUM USING POSTSUM-PARM                                   
050700     END-READ                                                             
050800     .                                                                    
050900     EJECT                                                                
051000 S11-SKRIV-W4408A SECTION.                                                
051100                                                                          
051200     WRITE UT1-POST FROM UT1-AREA                                         
051300                                                                          
051400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
051500     MOVE 'W4408A'   TO POSTSUM-FDNAMN                                    
051600     MOVE 'W44083D2' TO POSTSUM-DDNAMN2                                   
051700     CALL POSTSUM USING POSTSUM-PARM                                      
051800     .                                                                    
051900     EJECT                                                                
052000 S12-SKRIV-W4408B SECTION.                                                
052100                                                                          
052200     WRITE UT2-POST FROM UT2-AREA                                         
052300                                                                          
052400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
052500     MOVE 'W4408B'   TO POSTSUM-FDNAMN                                    
052600     MOVE 'W44083D3' TO POSTSUM-DDNAMN2                                   
052700     CALL POSTSUM USING POSTSUM-PARM                                      
052800     .                                                                    
052900     EJECT                                                                
053000 S13-SKRIV-W4408C SECTION.                                                
053100                                                                          
053200     WRITE UT3-POST FROM UT3-AREA                                         
053300                                                                          
053400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
053500     MOVE 'W4408C'   TO POSTSUM-FDNAMN                                    
053600     MOVE 'W44083D4' TO POSTSUM-DDNAMN2                                   
053700     CALL POSTSUM USING POSTSUM-PARM                                      
053800     .                                                                    
053900     EJECT                                                                
054000 S99-ABEND SECTION.                                                       
054100                                                                          
054200     SKIP2                                                                
054300     MOVE 'S' TO POSTSUM-OPKOD                                            
054400     CALL POSTSUM USING POSTSUM-PARM                                      
054500     CALL ABEND USING RKOD-ABEND                                          
054600     .                                                                    
054700     EJECT                                                                
