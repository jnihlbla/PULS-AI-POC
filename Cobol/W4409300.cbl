000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4409300.                                                
000300 AUTHOR.         NILSSON LINDA.                                           
000400 DATE-WRITTEN.   05/06/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        SENDS ONE BACKORDER LIST PER MARKET                              
001000*        TO MAIL RECIPIENTS WITH DISTRIBUTION & PRINT                     
001100*        W44089 - BACKORDER OLDER THAN TWO WEEKS (OLDEST FIRST)           
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U1000 - D&P                                                      
001500*                                                                         
001600* ETRACKER: 1545209                                                       
002000* STORY   : 2111923                                                       
002000*           23/8/2021 ADD BO-MAIL 1,2 AND 3 TO IMPORTERS PLUS             
002100*                     AUSTRALIA.                                          
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INFIL                                                      
002600     SELECT W44089                     ASSIGN TO W44093D1.                
002700     SKIP2                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W44089                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W440087   -L.                                                  
003700     SKIP3                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W4409300'.            
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400                                                                          
004500 77  W44089-EOF-SW               PIC X       VALUE 'N'.                   
004600     88  END-OF-W44089                       VALUE 'Y'.                   
004700                                                                          
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900                                                                          
005000 01  SPAR-IDLANDX3               PIC X(3)    VALUE SPACE.                 
005100 01  DAP-IDLANDX3                PIC X(3)    VALUE SPACE.                 
005200 01  WS-LISTA                    PIC X(8)    VALUE SPACE.                 
005300 01  WS-TOTALSUMMA               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005400*    --- COMMUNICATION AREAS                                              
005500*                                                                         
005600 01  HDR-AREA.                                                            
005700*    03  -COPY WZ01REQU                                                   
005800*    03  -COPY WZ04HDR                                                    
005900     EJECT                                                                
006000 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
006100 01  SEND-AREA.                                                           
006200*    03  -COPY WZ01SEND                                                   
006300     EJECT                                                                
006400 01  SEND-RAD-STYRTECKEN.                                                 
006500     03  STYRTECKEN-RAD          PIC X.                                   
006600     03  SEND-RAD                PIC X(165)  VALUE SPACE.                 
006700*    --- CONTROL CHARACTERS                                               
006800 01  WS-SKIP1                    PIC X       VALUE ' '.                   
006900 01  WS-SKIP2                    PIC X       VALUE '0'.                   
007000 01  WS-SKIP3                    PIC X       VALUE '-'.                   
007100 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
007200     EJECT                                                                
007300                                                                          
007400 01  FILLER                      PIC X(16)   VALUE 'BOLIST'.              
007500                                                                          
007600     EJECT                                                                
007700*    --- LISTLAYOUT                                                       
007800 01  BO-LIST.                                                             
007900     03  BO-RUBRIK1.                                                      
008000         05  BO-RUB1-MARKNAD     PIC X(15)   VALUE SPACE.                 
008100         05  FILLER              PIC X       VALUE X'05'.                 
008200         05  FILLER              PIC X(15)   VALUE SPACE.                 
008300         05  FILLER              PIC X       VALUE X'05'.                 
008400         05  FILLER              PIC X(8)    VALUE SPACE.                 
008500         05  FILLER              PIC X       VALUE X'05'.                 
008600         05  FILLER              PIC X(8)    VALUE SPACE.                 
008700         05  FILLER              PIC X       VALUE X'05'.                 
008800         05  FILLER              PIC X(5)    VALUE SPACE.                 
008900         05  FILLER              PIC X       VALUE X'05'.                 
009000         05  FILLER              PIC X(8)    VALUE SPACE.                 
009100         05  FILLER              PIC X       VALUE X'05'.                 
009200         05  FILLER              PIC X(6)    VALUE SPACE.                 
009300         05  FILLER              PIC X       VALUE X'05'.                 
009400         05  FILLER              PIC X(7)    VALUE SPACE.                 
009500         05  FILLER              PIC X       VALUE X'05'.                 
009600         05  FILLER              PIC X(8)    VALUE SPACE.                 
009700         05  FILLER              PIC X       VALUE X'05'.                 
009800         05  FILLER              PIC X(5)    VALUE SPACE.                 
009900         05  FILLER              PIC X       VALUE X'05'.                 
010000         05  FILLER              PIC X(5)    VALUE SPACE.                 
010100         05  FILLER              PIC X       VALUE X'05'.                 
010200         05  FILLER              PIC X(5)    VALUE SPACE.                 
010300         05  FILLER              PIC X       VALUE X'05'.                 
010400                                                                          
010500     03  BO-RUBRIK2.                                                      
010600         05  BO-RUB2-IDARTNR     PIC X(15)   VALUE                        
010700                                             'Part number    '.           
010800         05  FILLER              PIC X       VALUE X'05'.                 
010900         05  BO-RUB2-BEART       PIC X(15)   VALUE                        
011000                                             'Description    '.           
011100         05  FILLER              PIC X       VALUE X'05'.                 
011200         05  BO-RUB2-KDPRODSL    PIC X(8)    VALUE 'Prod grp'.            
011300         05  FILLER              PIC X       VALUE X'05'.                 
011400         05  BO-RUB2-IDFKNGRP    PIC X(8)    VALUE 'Func grp'.            
011500         05  FILLER              PIC X       VALUE X'05'.                 
011600         05  BO-RUB2-KVRADER     PIC X(5)    VALUE 'Lines'.               
011700         05  FILLER              PIC X       VALUE X'05'.                 
011800         05  BO-RUB2-KVBEART-Q   PIC X(8)    VALUE 'Quantity'.            
011900         05  FILLER              PIC X       VALUE X'05'.                 
012000         05  BO-RUB2-DARODAT     PIC X(6)    VALUE 'BO dat'.              
012100         05  FILLER              PIC X       VALUE X'05'.                 
012200         05  BO-RUB2-FLSPARR     PIC X(7)    VALUE 'Q-block'.             
012300         05  FILLER              PIC X       VALUE X'05'.                 
012400         05  BO-RUB2-FLSALDCH    PIC X(8)    VALUE 'Invest b'.            
012500         05  FILLER              PIC X       VALUE X'05'.                 
012600         05  BO-RUB2-DEL-INFO-1  PIC X(5)    VALUE 'Deliv'.               
012700         05  FILLER              PIC X       VALUE X'05'.                 
012800         05  BO-RUB2-DEL-INFO-2  PIC X(5)    VALUE 'avail'.               
012900         05  FILLER              PIC X       VALUE X'05'.                 
013000         05  BO-RUB2-DEL-INFO-3  PIC X(5)    VALUE 'CDC  '.               
013100         05  FILLER              PIC X       VALUE X'05'.                 
013200         05  BO-RUB2-IDDISTR     PIC X(8)   VALUE 'DISTRICT'.             
013300         05  FILLER              PIC X       VALUE X'05'.                 
013400         05  BO-RUB2-IDKUNDNR    PIC X(8)   VALUE 'CUSTOMER'.             
013500         05  FILLER              PIC X       VALUE X'05'.                 
013600         05  BO-RUB2-IDORDNR5    PIC X(8)   VALUE 'ORDER NO'.             
013700         05  FILLER              PIC X       VALUE X'05'.                 
013800         05  BO-RUB2-KVRO        PIC X(9)   VALUE 'ORDER QTY'.            
013900         05  FILLER              PIC X       VALUE X'05'.                 
014000         05  BO-RUB2-TIREGDAT    PIC X(18)                                
014100                                 VALUE 'ORDER DATE(YYMMDD)'.              
014200         05  FILLER              PIC X       VALUE X'05'.                 
014300     03  BO-RAD.                                                          
014400         05  BO-RAD-IDARTNR      PIC ZZZZZZZZZZZZZZ9.                     
014500         05  FILLER              PIC X       VALUE X'05'.                 
014600         05  BO-RAD-BEART        PIC X(15).                               
014700         05  FILLER              PIC X       VALUE X'05'.                 
014800         05  BO-RAD-KDPRODSL     PIC ZZZZZZZ9.                            
014900         05  FILLER              PIC X       VALUE X'05'.                 
015000         05  BO-RAD-IDFKNGRP     PIC ZZZZZZZ9.                            
015100         05  FILLER              PIC X       VALUE X'05'.                 
015200         05  BO-RAD-KVRADER      PIC ZZZZ9.                               
015300         05  FILLER              PIC X       VALUE X'05'.                 
015400         05  BO-RAD-KVBEART-Q    PIC ZZZZZZZ9.                            
015500         05  FILLER              PIC X       VALUE X'05'.                 
015600         05  BO-RAD-DARODAT      PIC ZZZZZ9.                              
015700         05  FILLER              PIC X       VALUE X'05'.                 
015800         05  BO-RAD-FLSPARR      PIC X(7).                                
015900         05  FILLER              PIC X       VALUE X'05'.                 
016000         05  BO-RAD-FLSALDCH     PIC X(8).                                
016100         05  FILLER              PIC X       VALUE X'05'.                 
016200         05  BO-RAD-TILEVBSK-GRP OCCURS 3.                                
016300             07 BO-RAD-TILEVBSK-INL PIC ZZZZ.                             
016400             07 FILLER            PIC X      VALUE X'05'.                 
016500         05  BO-RAD-IDDISTR      PIC ZZZZZZZ9.                            
016600         05  FILLER              PIC X       VALUE X'05'.                 
016700         05  BO-RAD-IDKUNDNR     PIC ZZZZZZZ9.                            
016800         05  FILLER              PIC X       VALUE X'05'.                 
016900         05  BO-RAD-IDORDNR5     PIC ZZZZZZZ9.                            
017000         05  FILLER              PIC X       VALUE X'05'.                 
017100         05  BO-RAD-KVRO         PIC ZZZZZZZZZ9.                          
017200         05  FILLER              PIC X       VALUE X'05'.                 
017300         05  BO-RAD-TIREGDAT     PIC ZZZZZZZZZZZZZZZZZ9.                  
017400         05  FILLER              PIC X       VALUE X'05'.                 
017500     03  BO-SLUTRAD.                                                      
017600         05  BO-SLUTRAD-RUB       PIC X(15)  VALUE                        
017700                                             'BACKORDER VALUE'.           
017800         05  FILLER               PIC X      VALUE X'05'.                 
017900         05  BO-SLUTRAD-VALUE     PIC ZZZZZZZZZZZ9.99.                    
018000         05  FILLER               PIC X      VALUE X'05'.                 
018100         05  FILLER               PIC X(8)   VALUE SPACE.                 
018200         05  FILLER               PIC X      VALUE X'05'.                 
018300         05  FILLER               PIC X(8)   VALUE SPACE.                 
018400         05  FILLER               PIC X      VALUE X'05'.                 
018500         05  FILLER               PIC X(5)   VALUE SPACE.                 
018600         05  FILLER               PIC X      VALUE X'05'.                 
018700         05  FILLER               PIC X(8)   VALUE SPACE.                 
018800         05  FILLER               PIC X      VALUE X'05'.                 
018900         05  FILLER               PIC X(6)   VALUE SPACE.                 
019000         05  FILLER               PIC X      VALUE X'05'.                 
019100         05  FILLER               PIC X(7)   VALUE SPACE.                 
019200         05  FILLER               PIC X      VALUE X'05'.                 
019300         05  FILLER               PIC X(8)   VALUE SPACE.                 
019400         05  FILLER               PIC X      VALUE X'05'.                 
019500         05  FILLER               PIC X(5)   VALUE SPACE.                 
019600         05  FILLER               PIC X      VALUE X'05'.                 
019700         05  FILLER               PIC X(5)   VALUE SPACE.                 
019800         05  FILLER               PIC X      VALUE X'05'.                 
019900         05  FILLER               PIC X(5)   VALUE SPACE.                 
020000         05  FILLER               PIC X      VALUE X'05'.                 
020100                                                                          
020200                                                                          
020300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
020400 01  FILLER REDEFINES TODAYS-DATE.                                        
020500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
020600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
020700     03  TODAYS-DATE-DAY         PIC 9(2).                                
020800     EJECT                                                                
020900 01  GENERAL-SUBPROGRAMS.                                                 
021000*                                                                         
021100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
021200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
021400     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
021500     SKIP2                                                                
021600*    --- PARAMETERS TO ABEND                                              
021700                                                                          
021800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
022000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
022100     SKIP2                                                                
022200 01  ERRTEXT.                                                             
022300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
022400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
022500     EJECT                                                                
022600*    --- PARAMETRAR TILL POSTSUM                                          
022700*                                                                         
022800*01  -COPY W0005   -PRE  POSTSUM-                                         
022900     EJECT                                                                
023000 01  IN-AREA-START               PIC X(24)   VALUE                        
023100                                 'IN-AREA-START  '.                       
023200     SKIP2                                                                
023300*01  AREA -COPY W440087     -PRE IN-                                      
023400     EJECT                                                                
023500                                                                          
023600 LINKAGE SECTION.                                                         
023700                                                                          
023800*01  -COPY W0009            -PRE MSG-                                     
023900                                                                          
024000*01  -COPY W0009            -PRE DISTRDOC-                                
024100     EJECT                                                                
024200 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
024300 MAIN SECTION.                                                            
024400     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
024500                                                                          
024600     PERFORM A-INIT                                                       
024700                                                                          
024800     PERFORM S01-READ-W44089                                              
024900     PERFORM UNTIL END-OF-W44089                                          
025000       MOVE ZERO TO WS-TOTALSUMMA                                         
025100       MOVE IN-IDLANDX3 TO SPAR-IDLANDX3                                  
025200                           DAP-IDLANDX3                                   
025300       PERFORM S90-SEND-OPEN                                              
025400       PERFORM S90-PUT-DAP-START                                          
025500                                                                          
025600       PERFORM B-INIT-MARKET                                              
025700       PERFORM C-PRINT-HEAD-BO                                            
025800       PERFORM UNTIL END-OF-W44089 OR                                     
025900         (SPAR-IDLANDX3 NOT = IN-IDLANDX3)                                
026000         PERFORM D-RAD-DATA                                               
026100         COMPUTE WS-TOTALSUMMA = WS-TOTALSUMMA + IN-SUORDV                
026200         PERFORM S01-READ-W44089                                          
026300       END-PERFORM                                                        
026400       PERFORM E-TOT-RAD                                                  
026500       PERFORM S90-SEND-CLOSE                                             
026600     END-PERFORM                                                          
026700                                                                          
026800     PERFORM Z-FINIT                                                      
026900                                                                          
027000     MOVE ZERO TO RETURN-CODE                                             
027100     GOBACK                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 A-INIT SECTION.                                                          
027500                                                                          
027600     OPEN INPUT  W44089                                                   
027700     SKIP2                                                                
027800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027900     .                                                                    
028000     EJECT                                                                
028100 B-INIT-MARKET SECTION.                                                   
028200                                                                          
028210     IF IN-IDLANDX3 = 'AT '                                               
028220       MOVE 'Austria        '        TO BO-RUB1-MARKNAD                   
028230     END-IF                                                               
028240     IF IN-IDLANDX3 = 'BE '                                               
028250       MOVE 'Belgium        '        TO BO-RUB1-MARKNAD                   
028260     END-IF                                                               
028270     IF IN-IDLANDX3 = 'CA '                                               
028280       MOVE 'Canada         '        TO BO-RUB1-MARKNAD                   
028290     END-IF                                                               
028291     IF IN-IDLANDX3 = 'CH '                                               
028292       MOVE 'Switzerland    '        TO BO-RUB1-MARKNAD                   
028293     END-IF                                                               
028294     IF IN-IDLANDX3 = 'CN1'                                               
028295       MOVE 'China 6214     '        TO BO-RUB1-MARKNAD                   
028296     END-IF                                                               
028297     IF IN-IDLANDX3 = 'CN2'                                               
028298       MOVE 'China 6232     '        TO BO-RUB1-MARKNAD                   
028299     END-IF                                                               
028300     IF IN-IDLANDX3 = 'CN3'                                               
028301       MOVE 'China 6260     '        TO BO-RUB1-MARKNAD                   
028302     END-IF                                                               
028303     IF IN-IDLANDX3 = 'CN4'                                               
028304       MOVE 'China 6271/6281'        TO BO-RUB1-MARKNAD                   
028305     END-IF                                                               
028306     IF IN-IDLANDX3 = 'DE '                                               
028307       MOVE 'Germany        '        TO BO-RUB1-MARKNAD                   
028308     END-IF                                                               
028309     IF IN-IDLANDX3 = 'DK '                                               
028310       MOVE 'Denmark        '        TO BO-RUB1-MARKNAD                   
028311     END-IF                                                               
028312     IF IN-IDLANDX3 = 'ES '                                               
028313       MOVE 'Spain          '        TO BO-RUB1-MARKNAD                   
028314     END-IF                                                               
028315     IF IN-IDLANDX3 = 'FI '                                               
028316       MOVE 'Finland        '        TO BO-RUB1-MARKNAD                   
028317     END-IF                                                               
028318     IF IN-IDLANDX3 = 'FR '                                               
028319       MOVE 'France         '        TO BO-RUB1-MARKNAD                   
028320     END-IF                                                               
028321     IF IN-IDLANDX3 = 'GB '                                               
028322       MOVE 'Great Britain  '        TO BO-RUB1-MARKNAD                   
028323     END-IF                                                               
028324     IF IN-IDLANDX3 = 'GR '                                               
028325       MOVE 'Greece         '        TO BO-RUB1-MARKNAD                   
028326     END-IF                                                               
028327     IF IN-IDLANDX3 = 'HK '                                               
028328       MOVE 'Hong Kong      '        TO BO-RUB1-MARKNAD                   
028329     END-IF                                                               
028330     IF IN-IDLANDX3 = 'IL '                                               
028331       MOVE 'Israel         '        TO BO-RUB1-MARKNAD                   
028332     END-IF                                                               
028333     IF IN-IDLANDX3 = 'IT '                                               
028334       MOVE 'Italy          '        TO BO-RUB1-MARKNAD                   
028335     END-IF                                                               
028336     IF IN-IDLANDX3 = 'MY '                                               
028337       MOVE 'Malaysia       '        TO BO-RUB1-MARKNAD                   
028338     END-IF                                                               
028339     IF IN-IDLANDX3 = 'NL '                                               
028340       MOVE 'The Netherlands'        TO BO-RUB1-MARKNAD                   
028341     END-IF                                                               
028342     IF IN-IDLANDX3 = 'NO '                                               
028343       MOVE 'Norway         '        TO BO-RUB1-MARKNAD                   
028344     END-IF                                                               
028345     IF IN-IDLANDX3 = 'PT '                                               
028346       MOVE 'Portugal       '        TO BO-RUB1-MARKNAD                   
028347     END-IF                                                               
028348     IF IN-IDLANDX3 = 'SE '                                               
028349       MOVE 'Sweden         '        TO BO-RUB1-MARKNAD                   
028350     END-IF                                                               
028351     IF IN-IDLANDX3 = 'SG '                                               
028352       MOVE 'Singapore      '        TO BO-RUB1-MARKNAD                   
028353     END-IF                                                               
028354     IF IN-IDLANDX3 = 'TH '                                               
028355       MOVE 'Thailand       '        TO BO-RUB1-MARKNAD                   
028356     END-IF                                                               
028357     IF IN-IDLANDX3 = 'TW '                                               
028358       MOVE 'Taiwan         '        TO BO-RUB1-MARKNAD                   
028359     END-IF                                                               
028360     IF IN-IDLANDX3 = 'US '                                               
028361       MOVE 'United States  '        TO BO-RUB1-MARKNAD                   
028362     END-IF                                                               
028363     IF IN-IDLANDX3 = 'ZA '                                               
028364       MOVE 'South Africa   '        TO BO-RUB1-MARKNAD                   
028365     END-IF                                                               
028366     IF IN-IDLANDX3 = 'KR '                                               
028367       MOVE 'Korea          '        TO BO-RUB1-MARKNAD                   
028368     END-IF                                                               
028369     IF IN-IDLANDX3 = 'IS'                                                
028370       MOVE 'Iceland        '        TO BO-RUB1-MARKNAD                   
028371     END-IF                                                               
028372     IF IN-IDLANDX3 = 'IE'                                                
028373       MOVE 'Ireland        '        TO BO-RUB1-MARKNAD                   
028374     END-IF                                                               
028375     IF IN-IDLANDX3 = 'CZ'                                                
028376       MOVE 'Czech          '        TO BO-RUB1-MARKNAD                   
028377     END-IF                                                               
028378     IF IN-IDLANDX3 = 'HU'                                                
028379       MOVE 'Hungary        '        TO BO-RUB1-MARKNAD                   
028380     END-IF                                                               
028381     IF IN-IDLANDX3 = 'SI'                                                
028382       MOVE 'Slov & Croa    '        TO  BO-RUB1-MARKNAD                  
028383     END-IF                                                               
028384     IF IN-IDLANDX3 = 'BA'                                                
028385       MOVE 'Bosnia         '        TO BO-RUB1-MARKNAD                   
028386     END-IF                                                               
028387     IF IN-IDLANDX3 = 'MK'                                                
028388       MOVE 'Macedonia      '        TO BO-RUB1-MARKNAD                   
028389     END-IF                                                               
028390     IF IN-IDLANDX3 = 'ME'                                                
028391       MOVE 'Montenegro     '        TO BO-RUB1-MARKNAD                   
028392     END-IF                                                               
028393     IF IN-IDLANDX3 = 'SK'                                                
028394       MOVE 'Slovakia       '        TO BO-RUB1-MARKNAD                   
028395     END-IF                                                               
028396     IF IN-IDLANDX3 = 'RS'                                                
028397       MOVE 'Serbia         '        TO BO-RUB1-MARKNAD                   
028398     END-IF                                                               
028399     IF IN-IDLANDX3 = 'AL'                                                
028400       MOVE 'Albania        '        TO BO-RUB1-MARKNAD                   
028401     END-IF                                                               
028402     IF IN-IDLANDX3 = 'BG'                                                
028403       MOVE 'Bulgaria       '        TO BO-RUB1-MARKNAD                   
028404     END-IF                                                               
028405     IF IN-IDLANDX3 = 'RU'                                                
028406       MOVE 'Russia         '        TO BO-RUB1-MARKNAD                   
028407     END-IF                                                               
028408     IF IN-IDLANDX3 = 'UA'                                                
028409       MOVE 'Ukraine        '        TO BO-RUB1-MARKNAD                   
028410     END-IF                                                               
028411     IF IN-IDLANDX3 = 'UZ'                                                
028412       MOVE 'Uzbekistan     '        TO BO-RUB1-MARKNAD                   
028413     END-IF                                                               
028414     IF IN-IDLANDX3 = 'AZ'                                                
028415       MOVE 'Azerbaijan     '        TO BO-RUB1-MARKNAD                   
028416     END-IF                                                               
028417     IF IN-IDLANDX3 = 'RO'                                                
028418       MOVE 'Romania        '        TO BO-RUB1-MARKNAD                   
028419     END-IF                                                               
028420     IF IN-IDLANDX3 = 'MD'                                                
028421       MOVE 'Moldova        '        TO BO-RUB1-MARKNAD                   
028422     END-IF                                                               
028423     IF IN-IDLANDX3 = 'GE'                                                
028424       MOVE 'Georgia        '        TO BO-RUB1-MARKNAD                   
028425     END-IF                                                               
028426     IF IN-IDLANDX3 = 'AM'                                                
028427       MOVE 'Armenia        '        TO BO-RUB1-MARKNAD                   
028428     END-IF                                                               
028429     IF IN-IDLANDX3 = 'PL'                                                
028430       MOVE 'Poland         '        TO BO-RUB1-MARKNAD                   
028431     END-IF                                                               
028432     IF IN-IDLANDX3 = 'MT'                                                
028433       MOVE 'Malta          '        TO BO-RUB1-MARKNAD                   
028434     END-IF                                                               
028435     IF IN-IDLANDX3 = 'AO'                                                
028436       MOVE 'Angola         '        TO BO-RUB1-MARKNAD                   
028437     END-IF                                                               
028438     IF IN-IDLANDX3 = 'EG'                                                
028439       MOVE 'Egypt          '        TO BO-RUB1-MARKNAD                   
028440     END-IF                                                               
028441     IF IN-IDLANDX3 = 'MA'                                                
028442       MOVE 'Morroco        '        TO BO-RUB1-MARKNAD                   
028443     END-IF                                                               
028444     IF IN-IDLANDX3 = 'TN'                                                
028445       MOVE 'Tunisia        '        TO BO-RUB1-MARKNAD                   
028446     END-IF                                                               
028447     IF IN-IDLANDX3 = 'MU'                                                
028448       MOVE 'Mauritius      '        TO BO-RUB1-MARKNAD                   
028449     END-IF                                                               
028450     IF IN-IDLANDX3 = 'SA'                                                
028451       MOVE 'Saudi Arabia   '        TO BO-RUB1-MARKNAD                   
028452     END-IF                                                               
028453     IF IN-IDLANDX3 = 'JO'                                                
028454       MOVE 'Jordan         '        TO BO-RUB1-MARKNAD                   
028455     END-IF                                                               
028456     IF IN-IDLANDX3 = 'KW'                                                
028457       MOVE 'Kuwait         '        TO BO-RUB1-MARKNAD                   
028458     END-IF                                                               
028459     IF IN-IDLANDX3 = 'QA'                                                
028460       MOVE 'Qatar          '        TO BO-RUB1-MARKNAD                   
028461     END-IF                                                               
028462     IF IN-IDLANDX3 = 'LB'                                                
028463       MOVE 'Lebanon        '        TO BO-RUB1-MARKNAD                   
028464     END-IF                                                               
028465     IF IN-IDLANDX3 = 'MM'                                                
028466       MOVE 'Myanmar        '        TO BO-RUB1-MARKNAD                   
028467     END-IF                                                               
028468     IF IN-IDLANDX3 = 'BN'                                                
028469       MOVE 'Brunei         '        TO BO-RUB1-MARKNAD                   
028470     END-IF                                                               
028471     IF IN-IDLANDX3 = 'TR'                                                
028472       MOVE 'Turkey         '        TO BO-RUB1-MARKNAD                   
028473     END-IF                                                               
028474     IF IN-IDLANDX3 = 'ID'                                                
028475       MOVE 'Indonesia      '        TO BO-RUB1-MARKNAD                   
028476     END-IF                                                               
028477     IF IN-IDLANDX3 = 'PH'                                                
028478       MOVE 'Philippines    '        TO BO-RUB1-MARKNAD                   
028479     END-IF                                                               
028480     IF IN-IDLANDX3 = 'BD'                                                
028481       MOVE 'Bangladesh     '        TO BO-RUB1-MARKNAD                   
028482     END-IF                                                               
028483     IF IN-IDLANDX3 = 'LK'                                                
028484       MOVE 'Sri Lanka      '        TO BO-RUB1-MARKNAD                   
028485     END-IF                                                               
028486     IF IN-IDLANDX3 = 'CY'                                                
028487       MOVE 'Cyprus         '        TO BO-RUB1-MARKNAD                   
028488     END-IF                                                               
028489     IF IN-IDLANDX3 = 'YE'                                                
028490       MOVE 'Yemen          '        TO BO-RUB1-MARKNAD                   
028491     END-IF                                                               
028492     IF IN-IDLANDX3 = 'OM'                                                
028493       MOVE 'Oman           '        TO BO-RUB1-MARKNAD                   
028494     END-IF                                                               
028495     IF IN-IDLANDX3 = 'BH'                                                
028496       MOVE 'Bahrain        '        TO BO-RUB1-MARKNAD                   
028497     END-IF                                                               
028498     IF IN-IDLANDX3 = 'AE'                                                
028499       MOVE 'Arab Emirates  '        TO BO-RUB1-MARKNAD                   
028500     END-IF                                                               
028501     IF IN-IDLANDX3 = 'CL'                                                
028502       MOVE 'Chile          '        TO BO-RUB1-MARKNAD                   
028503     END-IF                                                               
028504     IF IN-IDLANDX3 = 'MX'                                                
028505       MOVE 'Mexico         '        TO BO-RUB1-MARKNAD                   
028506     END-IF                                                               
028507     IF IN-IDLANDX3 = 'PY'                                                
028508       MOVE 'Paraguay       '        TO BO-RUB1-MARKNAD                   
028509     END-IF                                                               
028510     IF IN-IDLANDX3 = 'PE'                                                
028511       MOVE 'Peru           '        TO BO-RUB1-MARKNAD                   
028512     END-IF                                                               
028513     IF IN-IDLANDX3 = 'BR'                                                
028514       MOVE 'Brazil         '        TO BO-RUB1-MARKNAD                   
028515     END-IF                                                               
028516     IF IN-IDLANDX3 = 'GY'                                                
028517       MOVE 'Guyana         '        TO BO-RUB1-MARKNAD                   
028518     END-IF                                                               
028519     IF IN-IDLANDX3 = 'SR'                                                
028520       MOVE 'Surinam        '        TO BO-RUB1-MARKNAD                   
028521     END-IF                                                               
028522     IF IN-IDLANDX3 = 'CR'                                                
028523       MOVE 'Costa Rica     '        TO BO-RUB1-MARKNAD                   
028524     END-IF                                                               
028525     IF IN-IDLANDX3 = 'PA'                                                
028526       MOVE 'Panama         '        TO BO-RUB1-MARKNAD                   
028527     END-IF                                                               
028528     IF IN-IDLANDX3 = 'DO'                                                
028529       MOVE 'Dom. Rep.      '        TO BO-RUB1-MARKNAD                   
028530     END-IF                                                               
028531     IF IN-IDLANDX3 = 'PR'                                                
028532       MOVE 'Puerto Rico    '        TO BO-RUB1-MARKNAD                   
028533     END-IF                                                               
028534     IF IN-IDLANDX3 = 'EC'                                                
028535       MOVE 'Ecuador        '        TO BO-RUB1-MARKNAD                   
028536     END-IF                                                               
028537     IF IN-IDLANDX3 = 'CO'                                                
028538       MOVE 'Colombia       '        TO BO-RUB1-MARKNAD                   
028539     END-IF                                                               
028540     IF IN-IDLANDX3 = 'UY'                                                
028541       MOVE 'Uruguay        '        TO BO-RUB1-MARKNAD                   
028542     END-IF                                                               
036726     IF IN-IDLANDX3 = 'KH'                                                
036727       MOVE 'Cambodia       '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'VN'                                                
036727       MOVE 'Vietnam        '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'AU'                                                
036727       MOVE 'Australia      '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'BL'                                                
036727       MOVE 'Bolivia        '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'AR'                                                
036727       MOVE 'Argentina      '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'EE'                                                
036727       MOVE 'Estland        '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'LV'                                                
036727       MOVE 'Lettland       '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'LT'                                                
036727       MOVE 'Vilnius        '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'SV'                                                
036727       MOVE 'El Salvador    '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'GT'                                                
036727       MOVE 'Gautemala      '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'HN'                                                
036727       MOVE 'Honduras       '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'NI'                                                
036727       MOVE 'Nicaragua      '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
036726     IF IN-IDLANDX3 = 'TT'                                                
036727       MOVE 'Trinidad       '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
037021     .                                                                    
037022     EJECT                                                                
037023 C-PRINT-HEAD-BO SECTION.                                                 
037030                                                                          
037100     MOVE SPACE                      TO SEND-RAD-STYRTECKEN               
037200     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
037300     MOVE SPACE                      TO SEND-RAD                          
037400     PERFORM S90-PUT-DOC-LINE                                             
037500     MOVE BO-RUBRIK1                 TO SEND-RAD                          
037600     PERFORM S90-PUT-DOC-LINE                                             
037700                                                                          
037800     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
037900     MOVE SPACE                      TO SEND-RAD                          
038000     PERFORM S90-PUT-DOC-LINE                                             
038100     MOVE BO-RUBRIK2                 TO SEND-RAD                          
038200     PERFORM S90-PUT-DOC-LINE                                             
038300                                                                          
038400     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
038500     MOVE SPACE                      TO SEND-RAD                          
038600     PERFORM S90-PUT-DOC-LINE                                             
038700     .                                                                    
038800     EJECT                                                                
038900 D-RAD-DATA SECTION.                                                      
039000                                                                          
039100     MOVE IN-IDARTNR               TO BO-RAD-IDARTNR                      
039200     MOVE IN-BEART                 TO BO-RAD-BEART                        
039300     MOVE IN-KDPRODSL              TO BO-RAD-KDPRODSL                     
039400     MOVE IN-IDFKNGRP              TO BO-RAD-IDFKNGRP                     
039500     MOVE IN-KVRADER               TO BO-RAD-KVRADER                      
039600     MOVE IN-KVBEART-Q             TO BO-RAD-KVBEART-Q                    
039700     MOVE IN-DARODAT               TO BO-RAD-DARODAT                      
039800     MOVE IN-FLSPARR               TO BO-RAD-FLSPARR                      
039900     MOVE IN-FLSALDCH              TO BO-RAD-FLSALDCH                     
040000     MOVE IN-TIAAVVD (1)           TO BO-RAD-TILEVBSK-INL (1)             
040100     MOVE IN-TIAAVVD (2)           TO BO-RAD-TILEVBSK-INL (2)             
040200     MOVE IN-TIAAVVD (3)           TO BO-RAD-TILEVBSK-INL (3)             
040300     MOVE IN-IDDISTR               TO BO-RAD-IDDISTR                      
040400     MOVE IN-IDKUNDNR              TO BO-RAD-IDKUNDNR                     
040500     MOVE IN-IDORDNR5              TO BO-RAD-IDORDNR5                     
040600     MOVE IN-KVRO                  TO BO-RAD-KVRO                         
040700     MOVE IN-TIREGDAT              TO BO-RAD-TIREGDAT                     
040800     MOVE BO-RAD                   TO SEND-RAD                            
040900     PERFORM S90-PUT-DOC-LINE                                             
041000     .                                                                    
041100     EJECT                                                                
041200 E-TOT-RAD SECTION.                                                       
041300                                                                          
041400     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
041500     MOVE SPACE                    TO SEND-RAD                            
041600     PERFORM S90-PUT-DOC-LINE                                             
041700     MOVE WS-TOTALSUMMA            TO BO-SLUTRAD-VALUE                    
041800     MOVE BO-SLUTRAD               TO SEND-RAD                            
041900     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
042000     PERFORM S90-PUT-DOC-LINE                                             
042100     .                                                                    
042200     EJECT                                                                
042300 Z-FINIT SECTION.                                                         
042400     CLOSE W44089                                                         
042500     SKIP2                                                                
042600     MOVE 'S' TO POSTSUM-OPKOD                                            
042700     CALL POSTSUM USING POSTSUM-PARM                                      
042800     .                                                                    
042900     EJECT                                                                
043000 S01-READ-W44089  SECTION.                                                
043100     READ W44089 INTO IN-AREA                                             
043200     AT END                                                               
043300        MOVE HIGH-VALUE TO IN-AREA                                        
043400        SET END-OF-W44089 TO TRUE                                         
043500                                                                          
043600     NOT AT END                                                           
043700        MOVE 'W44089' TO POSTSUM-FDNAMN                                   
043800        MOVE 'W44093D1' TO POSTSUM-DDNAMN2                                
043900        MOVE SPACE TO POSTSUM-TRANSTYP                                    
044000        CALL POSTSUM USING POSTSUM-PARM                                   
044100     END-READ                                                             
044200     .                                                                    
044300     EJECT                                                                
044400*    --- DISPATCHER SECTIONS                                              
044500 S90-SEND-OPEN SECTION.                                                   
044600                                                                          
044700     MOVE 'OPEN'                        TO SEND-KDFUNC                    
044800     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
044900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
045000                         SEND-OPEN-AREA                                   
045100     IF SEND-KDRC > 0                                                     
045200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
045300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
045400       DELIMITED BY SIZE INTO ERRTEXT                                     
045500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900 S90-PUT-DAP-START SECTION.                                               
046000                                                                          
046100     MOVE 1                       TO REQU-IDMSGVER                        
046200     MOVE 'R'                     TO REQU-KDPGMACT                        
046300     MOVE IDPGM                   TO REQU-IDUSER                          
046400     MOVE 'BO-MAIL3'              TO HDR-IDOUTTYPE                        
046500     MOVE SPACE                   TO HDR-IDOUTREC                         
046600                                     HDR-IDLIST                           
046700     MOVE SPAR-IDLANDX3           TO HDR-IDOUTREC (1:3)                   
046800                                     HDR-IDLIST                           
046900     MOVE 'PUT'                   TO SEND-KDFUNC                          
047000     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
047100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
047200                         SEND-KVDLEN                                      
047300                         HDR-AREA                                         
047400     IF SEND-KDRC > ZERO                                                  
047500       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
047600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
047700       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
047800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
047900     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200 S90-PUT-DOC-LINE SECTION.                                                
048300                                                                          
048400     MOVE 'PUT'                           TO SEND-KDFUNC                  
048500*                     -- UTAN STYRTECKEN:                                 
048600     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
048700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
048800                         SEND-KVDLEN                                      
048900*                     -- UTAN STYRTECKEN:                                 
049000                         SEND-RAD                                         
049100     IF SEND-KDRC > ZERO                                                  
049200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
049300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
049400       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
049500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
049600     END-IF                                                               
049700     .                                                                    
049800     EJECT                                                                
049900 S90-SEND-CLOSE SECTION.                                                  
050000                                                                          
050100     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
050200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
050300                                                                          
050400     IF SEND-KDRC > 0                                                     
050500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
050600       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
050700       DELIMITED BY SIZE INTO ERRTEXT                                     
050800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
