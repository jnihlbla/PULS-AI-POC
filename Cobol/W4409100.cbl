000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4409100.                                                
000300 AUTHOR.         NILSSON LINDA.                                           
000400 DATE-WRITTEN.   05/06/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        SENDS ONE BACKORDER LIST PER MARKET                              
001000*        TO MAIL RECIPIENTS WITH DISTRIBUTION & PRINT                     
001100*        FILE W44087 - LATEST WEEKS BACKORDER TOP 30                      
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U1000 - D&P ERROR                                                
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
002600     SELECT W44087                     ASSIGN TO W44091D1.                
002700     SKIP2                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W44087                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W440087   -L.                                                  
003700     SKIP3                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W4409100'.            
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400                                                                          
004500 77  W44087-EOF-SW               PIC X       VALUE 'N'.                   
004600     88  END-OF-W44087                       VALUE 'Y'.                   
004700                                                                          
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900                                                                          
005000 01  SPAR-IDLANDX3               PIC X(3)    VALUE SPACE.                 
005100 01  DAP-IDLANDX3                PIC X(3)    VALUE SPACE.                 
005200 01  WS-LISTA                    PIC X(8)    VALUE SPACE.                 
005300 01  WS-TOTALSUMMA               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005400*    --- COMMUNICATION AREAS                                              
005500 77  INDX                        PIC S9(3)   VALUE +000 COMP SYNC.        
005600*                                                                         
005700 01  HDR-AREA.                                                            
005800*    03  -COPY WZ01REQU                                                   
005900*    03  -COPY WZ04HDR                                                    
006000     EJECT                                                                
006100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
006200 01  SEND-AREA.                                                           
006300*    03  -COPY WZ01SEND                                                   
006400     EJECT                                                                
006500 01  SEND-RAD-STYRTECKEN.                                                 
006600     03  STYRTECKEN-RAD          PIC X.                                   
006700     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
006800*    --- CONTROL CHARACTERS                                               
006900 01  WS-SKIP1                    PIC X       VALUE ' '.                   
007000 01  WS-SKIP2                    PIC X       VALUE '0'.                   
007100 01  WS-SKIP3                    PIC X       VALUE '-'.                   
007200 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
007300     EJECT                                                                
007400                                                                          
007500 01  FILLER                      PIC X(16)   VALUE 'BOLIST'.              
007600                                                                          
007700     EJECT                                                                
007800*    --- LISTLAYOUT                                                       
007900 01  BO-LIST.                                                             
008000     03  BO-RUBRIK1.                                                      
008100         05  BO-RUB1-MARKNAD     PIC X(15)   VALUE SPACE.                 
008200         05  FILLER              PIC X       VALUE X'05'.                 
008300         05  FILLER              PIC X(15)   VALUE SPACE.                 
008400         05  FILLER              PIC X       VALUE X'05'.                 
008500         05  FILLER              PIC X(8)    VALUE SPACE.                 
008600         05  FILLER              PIC X       VALUE X'05'.                 
008700         05  FILLER              PIC X(8)    VALUE SPACE.                 
008800         05  FILLER              PIC X       VALUE X'05'.                 
008900         05  FILLER              PIC X(5)    VALUE SPACE.                 
009000         05  FILLER              PIC X       VALUE X'05'.                 
009100         05  FILLER              PIC X(8)    VALUE SPACE.                 
009200         05  FILLER              PIC X       VALUE X'05'.                 
009300         05  FILLER              PIC X(12)   VALUE SPACE.                 
009400*        05  FILLER              PIC X(10)   VALUE SPACE.                 
009500         05  FILLER              PIC X       VALUE X'05'.                 
009600         05  FILLER              PIC X       VALUE SPACE.                 
009700         05  FILLER              PIC X       VALUE X'05'.                 
009800         05  FILLER              PIC X(7)    VALUE SPACE.                 
009900         05  FILLER              PIC X       VALUE X'05'.                 
010000         05  FILLER              PIC X(8)    VALUE SPACE.                 
010100         05  FILLER              PIC X       VALUE X'05'.                 
010200         05  FILLER              PIC X(5)    VALUE SPACE.                 
010300         05  FILLER              PIC X       VALUE X'05'.                 
010400         05  FILLER              PIC X(5)    VALUE SPACE.                 
010500         05  FILLER              PIC X       VALUE X'05'.                 
010600         05  FILLER              PIC X(5)    VALUE SPACE.                 
010700         05  FILLER              PIC X       VALUE X'05'.                 
010800                                                                          
010900     03  BO-RUBRIK2.                                                      
011000         05  BO-RUB2-IDARTNR     PIC X(15)   VALUE                        
011100                                             'Part number    '.           
011200         05  FILLER              PIC X       VALUE X'05'.                 
011300         05  BO-RUB2-BEART       PIC X(15)   VALUE                        
011400                                             'Description    '.           
011500         05  FILLER              PIC X       VALUE X'05'.                 
011600         05  BO-RUB2-KDPRODSL    PIC X(8)    VALUE 'Prod grp'.            
011700         05  FILLER              PIC X       VALUE X'05'.                 
011800         05  BO-RUB2-IDFKNGRP    PIC X(8)    VALUE 'Func grp'.            
011900         05  FILLER              PIC X       VALUE X'05'.                 
012000         05  BO-RUB2-KVRADER     PIC X(5)    VALUE 'Lines'.               
012100         05  FILLER              PIC X       VALUE X'05'.                 
012200         05  BO-RUB2-KVBEART-Q   PIC X(8)    VALUE 'Quantity'.            
012300         05  FILLER              PIC X       VALUE X'05'.                 
012400         05  BO-RUB2-SUORDV      PIC X(12)   VALUE '    BO value'.        
012500         05  FILLER              PIC X       VALUE X'05'.                 
012600         05  BO-RUB2-TEASTRIX    PIC X       VALUE SPACE.                 
012700         05  FILLER              PIC X       VALUE X'05'.                 
012800         05  BO-RUB2-FLSPARR     PIC X(7)    VALUE 'Q-block'.             
012900         05  FILLER              PIC X       VALUE X'05'.                 
013000         05  BO-RUB2-FLSALDCH    PIC X(8)    VALUE 'Invest b'.            
013100         05  FILLER              PIC X       VALUE X'05'.                 
013200         05  BO-RUB2-DEL-INFO-1  PIC X(5)    VALUE 'Deliv'.               
013300         05  FILLER              PIC X       VALUE X'05'.                 
013400         05  BO-RUB2-DEL-INFO-2  PIC X(5)    VALUE 'avail'.               
013500         05  FILLER              PIC X       VALUE X'05'.                 
013600         05  BO-RUB2-DEL-INFO-3  PIC X(5)    VALUE 'CDC  '.               
013700         05  FILLER              PIC X       VALUE X'05'.                 
013800                                                                          
013900     03  BO-RAD.                                                          
014000         05  BO-RAD-IDARTNR      PIC ZZZZZZZZZZZZZZ9.                     
014100         05  FILLER              PIC X       VALUE X'05'.                 
014200         05  BO-RAD-BEART        PIC X(15).                               
014300         05  FILLER              PIC X       VALUE X'05'.                 
014400         05  BO-RAD-KDPRODSL     PIC ZZZZZZZ9.                            
014500         05  FILLER              PIC X       VALUE X'05'.                 
014600         05  BO-RAD-IDFKNGRP     PIC ZZZZZZZ9.                            
014700         05  FILLER              PIC X       VALUE X'05'.                 
014800         05  BO-RAD-KVRADER      PIC ZZZZ9.                               
014900         05  FILLER              PIC X       VALUE X'05'.                 
015000         05  BO-RAD-KVBEART-Q    PIC ZZZZZZZ9.                            
015100         05  FILLER              PIC X       VALUE X'05'.                 
015200         05  BO-RAD-SUORDV       PIC ZZZZZZZZ9.99.                        
015300         05  FILLER              PIC X       VALUE X'05'.                 
015400         05  BO-RAD-TEASTRIX     PIC X.                                   
015500         05  FILLER              PIC X       VALUE X'05'.                 
015600         05  BO-RAD-FLSPARR      PIC X(7).                                
015700         05  FILLER              PIC X       VALUE X'05'.                 
015800         05  BO-RAD-FLSALDCH     PIC X(8).                                
015900         05  FILLER              PIC X       VALUE X'05'.                 
016000         05  BO-RAD-TILEVBSK-GRP OCCURS 3.                                
016100             07 BO-RAD-TILEVBSK-INL PIC ZZZZ.                             
016200             07 FILLER           PIC X       VALUE X'05'.                 
016300                                                                          
016400     03  BO-SLUTRAD.                                                      
016500         05  BO-SLUTRAD-RUB       PIC X(15)  VALUE                        
016600                                             'BACKORDER VALUE'.           
016700         05  FILLER               PIC X      VALUE X'05'.                 
016800         05  BO-SLUTRAD-VALUE     PIC ZZZZZZZZZZZ9.99.                    
016900         05  FILLER               PIC X      VALUE X'05'.                 
017000         05  FILLER               PIC X(8)   VALUE SPACE.                 
017100         05  FILLER               PIC X      VALUE X'05'.                 
017200         05  FILLER               PIC X(8)   VALUE SPACE.                 
017300         05  FILLER               PIC X      VALUE X'05'.                 
017400         05  FILLER               PIC X(5)   VALUE SPACE.                 
017500         05  FILLER               PIC X      VALUE X'05'.                 
017600         05  FILLER               PIC X(8)   VALUE SPACE.                 
017700         05  FILLER               PIC X      VALUE X'05'.                 
017800         05  FILLER               PIC X(12)  VALUE SPACE.                 
017900*        05  FILLER               PIC X(10)  VALUE SPACE.                 
018000         05  FILLER               PIC X      VALUE X'05'.                 
018100         05  FILLER               PIC X      VALUE SPACE.                 
018200         05  FILLER               PIC X      VALUE X'05'.                 
018300         05  FILLER               PIC X(7)   VALUE SPACE.                 
018400         05  FILLER               PIC X      VALUE X'05'.                 
018500         05  FILLER               PIC X(8)   VALUE SPACE.                 
018600         05  FILLER               PIC X      VALUE X'05'.                 
018700         05  FILLER               PIC X(5)   VALUE SPACE.                 
018800         05  FILLER               PIC X      VALUE X'05'.                 
018900         05  FILLER               PIC X(5)   VALUE SPACE.                 
019000         05  FILLER               PIC X      VALUE X'05'.                 
019100         05  FILLER               PIC X(5)   VALUE SPACE.                 
019200         05  FILLER               PIC X      VALUE X'05'.                 
019300                                                                          
019400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
019500 01  FILLER REDEFINES TODAYS-DATE.                                        
019600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
019700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
019800     03  TODAYS-DATE-DAY         PIC 9(2).                                
019900     EJECT                                                                
020000 01  GENERAL-SUBPROGRAMS.                                                 
020100*                                                                         
020200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
020400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
020500     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
020600     SKIP2                                                                
020700*    --- PARAMETERS TO ABEND                                              
020800                                                                          
020900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
021100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
021200     SKIP2                                                                
021300 01  ERRTEXT.                                                             
021400     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
021500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
021600     EJECT                                                                
021700*    --- PARAMETRAR TILL POSTSUM                                          
021800*                                                                         
021900*01  -COPY W0005   -PRE  POSTSUM-                                         
022000     EJECT                                                                
022100 01  IN-AREA-START               PIC X(24)   VALUE                        
022200                                 'IN-AREA-START  '.                       
022300     SKIP2                                                                
022400*01  AREA -COPY W440087     -PRE IN-                                      
022500     EJECT                                                                
022600                                                                          
022700 LINKAGE SECTION.                                                         
022800                                                                          
022900*01  -COPY W0009            -PRE MSG-                                     
023000                                                                          
023100*01  -COPY W0009            -PRE DISTRDOC-                                
023200     EJECT                                                                
023300 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
023400 MAIN SECTION.                                                            
023500     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
023600                                                                          
023700     PERFORM A-INIT                                                       
023800                                                                          
023900     PERFORM S01-READ-W44087                                              
024000     PERFORM UNTIL END-OF-W44087                                          
024100       MOVE ZERO TO WS-TOTALSUMMA                                         
024200       MOVE IN-IDLANDX3 TO SPAR-IDLANDX3                                  
024300                            DAP-IDLANDX3                                  
                                                                                
024400       PERFORM S90-SEND-OPEN                                              
024500       PERFORM S90-PUT-DAP-START                                          
024600                                                                          
024700       PERFORM B-INIT-MARKET                                              
024800       PERFORM C-PRINT-HEAD-BO                                            
024900       MOVE +1 TO INDX                                                    
025000       PERFORM UNTIL END-OF-W44087 OR                                     
025100         (SPAR-IDLANDX3 NOT = IN-IDLANDX3)                                
025200         IF INDX <= 30                                                    
025300           PERFORM D-RAD-DATA                                             
025400         END-IF                                                           
025500         COMPUTE WS-TOTALSUMMA = WS-TOTALSUMMA + IN-SUORDV                
025600         ADD +1 TO INDX                                                   
025700         PERFORM S01-READ-W44087                                          
025800       END-PERFORM                                                        
025900       PERFORM E-TOT-RAD                                                  
026000       PERFORM S90-SEND-CLOSE                                             
026100     END-PERFORM                                                          
026200                                                                          
026300     PERFORM Z-FINIT                                                      
026400                                                                          
026500     MOVE ZERO TO RETURN-CODE                                             
026600     GOBACK                                                               
026700     .                                                                    
026800     EJECT                                                                
026900 A-INIT SECTION.                                                          
027000                                                                          
027100     OPEN INPUT  W44087                                                   
027200     SKIP2                                                                
027300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027400     .                                                                    
027500     EJECT                                                                
027600 B-INIT-MARKET SECTION.                                                   
027700                                                                          
027800     IF IN-IDLANDX3 = 'AT '                                               
027900       MOVE 'Austria        '        TO BO-RUB1-MARKNAD                   
028000     END-IF                                                               
028100     IF IN-IDLANDX3 = 'BE '                                               
028200       MOVE 'Belgium        '        TO BO-RUB1-MARKNAD                   
028300     END-IF                                                               
028400     IF IN-IDLANDX3 = 'CA '                                               
028500       MOVE 'Canada         '        TO BO-RUB1-MARKNAD                   
028600     END-IF                                                               
028700     IF IN-IDLANDX3 = 'CH '                                               
028800       MOVE 'Switzerland    '        TO BO-RUB1-MARKNAD                   
028900     END-IF                                                               
029000     IF IN-IDLANDX3 = 'CN1'                                               
029100       MOVE 'China 6214     '        TO BO-RUB1-MARKNAD                   
029200     END-IF                                                               
029300     IF IN-IDLANDX3 = 'CN2'                                               
029400       MOVE 'China 6232     '        TO BO-RUB1-MARKNAD                   
029500     END-IF                                                               
029600     IF IN-IDLANDX3 = 'CN3'                                               
029700       MOVE 'China 6260     '        TO BO-RUB1-MARKNAD                   
029800     END-IF                                                               
029900     IF IN-IDLANDX3 = 'CN4'                                               
030000       MOVE 'China 6271/6281'        TO BO-RUB1-MARKNAD                   
030100     END-IF                                                               
030200     IF IN-IDLANDX3 = 'DE '                                               
030300       MOVE 'Germany        '        TO BO-RUB1-MARKNAD                   
030400     END-IF                                                               
030500     IF IN-IDLANDX3 = 'DK '                                               
030600       MOVE 'Denmark        '        TO BO-RUB1-MARKNAD                   
030700     END-IF                                                               
030800     IF IN-IDLANDX3 = 'ES '                                               
030900       MOVE 'Spain          '        TO BO-RUB1-MARKNAD                   
031000     END-IF                                                               
031100     IF IN-IDLANDX3 = 'FI '                                               
031200       MOVE 'Finland        '        TO BO-RUB1-MARKNAD                   
031300     END-IF                                                               
031400     IF IN-IDLANDX3 = 'FR '                                               
031500       MOVE 'France         '        TO BO-RUB1-MARKNAD                   
031600     END-IF                                                               
031700     IF IN-IDLANDX3 = 'GB '                                               
031800       MOVE 'Great Britain  '        TO BO-RUB1-MARKNAD                   
031900     END-IF                                                               
032000     IF IN-IDLANDX3 = 'GR '                                               
032100       MOVE 'Greece         '        TO BO-RUB1-MARKNAD                   
032200     END-IF                                                               
032300     IF IN-IDLANDX3 = 'HK '                                               
032400       MOVE 'Hong Kong      '        TO BO-RUB1-MARKNAD                   
032500     END-IF                                                               
032600     IF IN-IDLANDX3 = 'IL '                                               
032700       MOVE 'Israel         '        TO BO-RUB1-MARKNAD                   
032800     END-IF                                                               
032900     IF IN-IDLANDX3 = 'IT '                                               
033000       MOVE 'Italy          '        TO BO-RUB1-MARKNAD                   
033100     END-IF                                                               
033200     IF IN-IDLANDX3 = 'MY '                                               
033300       MOVE 'Malaysia       '        TO BO-RUB1-MARKNAD                   
033400     END-IF                                                               
033500     IF IN-IDLANDX3 = 'NL '                                               
033600       MOVE 'The Netherlands'        TO BO-RUB1-MARKNAD                   
033700     END-IF                                                               
033800     IF IN-IDLANDX3 = 'NO '                                               
033900       MOVE 'Norway         '        TO BO-RUB1-MARKNAD                   
034000     END-IF                                                               
034100     IF IN-IDLANDX3 = 'PT '                                               
034200       MOVE 'Portugal       '        TO BO-RUB1-MARKNAD                   
034300     END-IF                                                               
034400     IF IN-IDLANDX3 = 'SE '                                               
034500       MOVE 'Sweden         '        TO BO-RUB1-MARKNAD                   
034600     END-IF                                                               
034700     IF IN-IDLANDX3 = 'SG '                                               
034800       MOVE 'Singapore      '        TO BO-RUB1-MARKNAD                   
034900     END-IF                                                               
035000     IF IN-IDLANDX3 = 'TH '                                               
035100       MOVE 'Thailand       '        TO BO-RUB1-MARKNAD                   
035200     END-IF                                                               
035300     IF IN-IDLANDX3 = 'TW '                                               
035400       MOVE 'Taiwan         '        TO BO-RUB1-MARKNAD                   
035500     END-IF                                                               
035600     IF IN-IDLANDX3 = 'US '                                               
035700       MOVE 'United States  '        TO BO-RUB1-MARKNAD                   
035800     END-IF                                                               
035900     IF IN-IDLANDX3 = 'ZA '                                               
036000       MOVE 'South Africa   '        TO BO-RUB1-MARKNAD                   
036100     END-IF                                                               
036200     IF IN-IDLANDX3 = 'KR '                                               
036210       MOVE 'Korea          '        TO BO-RUB1-MARKNAD                   
036220     END-IF                                                               
036230     IF IN-IDLANDX3 = 'IS'                                                
036231       MOVE 'Iceland        '        TO BO-RUB1-MARKNAD                   
036270     END-IF                                                               
036280     IF IN-IDLANDX3 = 'IE'                                                
036290       MOVE 'Ireland        '        TO BO-RUB1-MARKNAD                   
036293     END-IF                                                               
036294     IF IN-IDLANDX3 = 'CZ'                                                
036295       MOVE 'Czech          '        TO BO-RUB1-MARKNAD                   
036297     END-IF                                                               
036298     IF IN-IDLANDX3 = 'HU'                                                
036299       MOVE 'Hungary        '        TO BO-RUB1-MARKNAD                   
036300     END-IF                                                               
036310     IF IN-IDLANDX3 = 'SI'                                                
036400       MOVE 'Slov & Croa    '        TO  BO-RUB1-MARKNAD                  
036500     END-IF                                                               
036510     IF IN-IDLANDX3 = 'BA'                                                
036520       MOVE 'Bosnia         '        TO BO-RUB1-MARKNAD                   
036530     END-IF                                                               
036540     IF IN-IDLANDX3 = 'MK'                                                
036550       MOVE 'Macedonia      '        TO BO-RUB1-MARKNAD                   
036560     END-IF                                                               
036570     IF IN-IDLANDX3 = 'ME'                                                
036571       MOVE 'Montenegro     '        TO BO-RUB1-MARKNAD                   
036572     END-IF                                                               
036573     IF IN-IDLANDX3 = 'SK'                                                
036574       MOVE 'Slovakia       '        TO BO-RUB1-MARKNAD                   
036575     END-IF                                                               
036576     IF IN-IDLANDX3 = 'RS'                                                
036577       MOVE 'Serbia         '        TO BO-RUB1-MARKNAD                   
036578     END-IF                                                               
036579     IF IN-IDLANDX3 = 'AL'                                                
036580       MOVE 'Albania        '        TO BO-RUB1-MARKNAD                   
036581     END-IF                                                               
036582     IF IN-IDLANDX3 = 'BG'                                                
036583       MOVE 'Bulgaria       '        TO BO-RUB1-MARKNAD                   
036584     END-IF                                                               
036585     IF IN-IDLANDX3 = 'RU'                                                
036586       MOVE 'Russia         '        TO BO-RUB1-MARKNAD                   
036587     END-IF                                                               
036588     IF IN-IDLANDX3 = 'UA'                                                
036589       MOVE 'Ukraine        '        TO BO-RUB1-MARKNAD                   
036590     END-IF                                                               
036591     IF IN-IDLANDX3 = 'UZ'                                                
036592       MOVE 'Uzbekistan     '        TO BO-RUB1-MARKNAD                   
036593     END-IF                                                               
036594     IF IN-IDLANDX3 = 'AZ'                                                
036595       MOVE 'Azerbaijan     '        TO BO-RUB1-MARKNAD                   
036596     END-IF                                                               
036597     IF IN-IDLANDX3 = 'RO'                                                
036598       MOVE 'Romania        '        TO BO-RUB1-MARKNAD                   
036599     END-IF                                                               
036600     IF IN-IDLANDX3 = 'MD'                                                
036601       MOVE 'Moldova        '        TO BO-RUB1-MARKNAD                   
036602     END-IF                                                               
036603     IF IN-IDLANDX3 = 'GE'                                                
036604       MOVE 'Georgia        '        TO BO-RUB1-MARKNAD                   
036605     END-IF                                                               
036606     IF IN-IDLANDX3 = 'AM'                                                
036607       MOVE 'Armenia        '        TO BO-RUB1-MARKNAD                   
036608     END-IF                                                               
036609     IF IN-IDLANDX3 = 'PL'                                                
036610       MOVE 'Poland         '        TO BO-RUB1-MARKNAD                   
036611     END-IF                                                               
036612     IF IN-IDLANDX3 = 'MT'                                                
036613       MOVE 'Malta          '        TO BO-RUB1-MARKNAD                   
036614     END-IF                                                               
036615     IF IN-IDLANDX3 = 'AO'                                                
036616       MOVE 'Angola         '        TO BO-RUB1-MARKNAD                   
036617     END-IF                                                               
036618     IF IN-IDLANDX3 = 'EG'                                                
036619       MOVE 'Egypt          '        TO BO-RUB1-MARKNAD                   
036620     END-IF                                                               
036621     IF IN-IDLANDX3 = 'MA'                                                
036622       MOVE 'Morroco        '        TO BO-RUB1-MARKNAD                   
036623     END-IF                                                               
036624     IF IN-IDLANDX3 = 'TN'                                                
036625       MOVE 'Tunisia        '        TO BO-RUB1-MARKNAD                   
036626     END-IF                                                               
036627     IF IN-IDLANDX3 = 'MU'                                                
036628       MOVE 'Mauritius      '        TO BO-RUB1-MARKNAD                   
036629     END-IF                                                               
036630     IF IN-IDLANDX3 = 'SA'                                                
036631       MOVE 'Saudi Arabia   '        TO BO-RUB1-MARKNAD                   
036632     END-IF                                                               
036633     IF IN-IDLANDX3 = 'JO'                                                
036634       MOVE 'Jordan         '        TO BO-RUB1-MARKNAD                   
036635     END-IF                                                               
036636     IF IN-IDLANDX3 = 'KW'                                                
036637       MOVE 'Kuwait         '        TO BO-RUB1-MARKNAD                   
036638     END-IF                                                               
036639     IF IN-IDLANDX3 = 'QA'                                                
036640       MOVE 'Qatar          '        TO BO-RUB1-MARKNAD                   
036641     END-IF                                                               
036642     IF IN-IDLANDX3 = 'LB'                                                
036643       MOVE 'Lebanon        '        TO BO-RUB1-MARKNAD                   
036644     END-IF                                                               
036645     IF IN-IDLANDX3 = 'MM'                                                
036646       MOVE 'Myanmar        '        TO BO-RUB1-MARKNAD                   
036647     END-IF                                                               
036648     IF IN-IDLANDX3 = 'BN'                                                
036649       MOVE 'Brunei         '        TO BO-RUB1-MARKNAD                   
036650     END-IF                                                               
036651     IF IN-IDLANDX3 = 'TR'                                                
036652       MOVE 'Turkey         '        TO BO-RUB1-MARKNAD                   
036653     END-IF                                                               
036654     IF IN-IDLANDX3 = 'ID'                                                
036655       MOVE 'Indonesia      '        TO BO-RUB1-MARKNAD                   
036656     END-IF                                                               
036657     IF IN-IDLANDX3 = 'PH'                                                
036658       MOVE 'Philippines    '        TO BO-RUB1-MARKNAD                   
036659     END-IF                                                               
036660     IF IN-IDLANDX3 = 'BD'                                                
036661       MOVE 'Bangladesh     '        TO BO-RUB1-MARKNAD                   
036662     END-IF                                                               
036663     IF IN-IDLANDX3 = 'LK'                                                
036664       MOVE 'Sri Lanka      '        TO BO-RUB1-MARKNAD                   
036665     END-IF                                                               
036666     IF IN-IDLANDX3 = 'CY'                                                
036667       MOVE 'Cyprus         '        TO BO-RUB1-MARKNAD                   
036668     END-IF                                                               
036669     IF IN-IDLANDX3 = 'YE'                                                
036670       MOVE 'Yemen          '        TO BO-RUB1-MARKNAD                   
036671     END-IF                                                               
036672     IF IN-IDLANDX3 = 'OM'                                                
036673       MOVE 'Oman           '        TO BO-RUB1-MARKNAD                   
036674     END-IF                                                               
036675     IF IN-IDLANDX3 = 'BH'                                                
036676       MOVE 'Bahrain        '        TO BO-RUB1-MARKNAD                   
036677     END-IF                                                               
036678     IF IN-IDLANDX3 = 'AE'                                                
036679       MOVE 'Arab Emirates  '        TO BO-RUB1-MARKNAD                   
036680     END-IF                                                               
036681     IF IN-IDLANDX3 = 'CL'                                                
036682       MOVE 'Chile          '        TO BO-RUB1-MARKNAD                   
036683     END-IF                                                               
036684     IF IN-IDLANDX3 = 'MX'                                                
036685       MOVE 'Mexico         '        TO BO-RUB1-MARKNAD                   
036686     END-IF                                                               
036687     IF IN-IDLANDX3 = 'PY'                                                
036688       MOVE 'Paraguay       '        TO BO-RUB1-MARKNAD                   
036689     END-IF                                                               
036693     IF IN-IDLANDX3 = 'PE'                                                
036694       MOVE 'Peru           '        TO BO-RUB1-MARKNAD                   
036695     END-IF                                                               
036696     IF IN-IDLANDX3 = 'BR'                                                
036697       MOVE 'Brazil         '        TO BO-RUB1-MARKNAD                   
036698     END-IF                                                               
036699     IF IN-IDLANDX3 = 'GY'                                                
036700       MOVE 'Guyana         '        TO BO-RUB1-MARKNAD                   
036701     END-IF                                                               
036702     IF IN-IDLANDX3 = 'SR'                                                
036703       MOVE 'Surinam        '        TO BO-RUB1-MARKNAD                   
036704     END-IF                                                               
036705     IF IN-IDLANDX3 = 'CR'                                                
036706       MOVE 'Costa Rica     '        TO BO-RUB1-MARKNAD                   
036707     END-IF                                                               
036708     IF IN-IDLANDX3 = 'PA'                                                
036709       MOVE 'Panama         '        TO BO-RUB1-MARKNAD                   
036710     END-IF                                                               
036714     IF IN-IDLANDX3 = 'DO'                                                
036715       MOVE 'Dom. Rep.      '        TO BO-RUB1-MARKNAD                   
036716     END-IF                                                               
036717     IF IN-IDLANDX3 = 'PR'                                                
036718       MOVE 'Puerto Rico    '        TO BO-RUB1-MARKNAD                   
036719     END-IF                                                               
036720     IF IN-IDLANDX3 = 'EC'                                                
036721       MOVE 'Ecuador        '        TO BO-RUB1-MARKNAD                   
036722     END-IF                                                               
036723     IF IN-IDLANDX3 = 'CO'                                                
036724       MOVE 'Colombia       '        TO BO-RUB1-MARKNAD                   
036725     END-IF                                                               
036726     IF IN-IDLANDX3 = 'UY'                                                
036727       MOVE 'Uruguay        '        TO BO-RUB1-MARKNAD                   
036728     END-IF                                                               
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
036729     .                                                                    
036730     EJECT                                                                
036731 C-PRINT-HEAD-BO SECTION.                                                 
036732                                                                          
036740     MOVE SPACE                      TO SEND-RAD-STYRTECKEN               
036800     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
036900     MOVE SPACE                      TO SEND-RAD                          
037000     PERFORM S90-PUT-DOC-LINE                                             
037100     MOVE BO-RUBRIK1                 TO SEND-RAD                          
037200     PERFORM S90-PUT-DOC-LINE                                             
037300                                                                          
037400     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
037500     MOVE SPACE                      TO SEND-RAD                          
037600     PERFORM S90-PUT-DOC-LINE                                             
037700     MOVE BO-RUBRIK2                 TO SEND-RAD                          
037800     PERFORM S90-PUT-DOC-LINE                                             
037900                                                                          
038000     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
038100     MOVE SPACE                      TO SEND-RAD                          
038200     PERFORM S90-PUT-DOC-LINE                                             
038300     .                                                                    
038400     EJECT                                                                
038500 D-RAD-DATA SECTION.                                                      
038600                                                                          
038700     MOVE IN-IDARTNR               TO BO-RAD-IDARTNR                      
038800     MOVE IN-BEART                 TO BO-RAD-BEART                        
038900     MOVE IN-KDPRODSL              TO BO-RAD-KDPRODSL                     
039000     MOVE IN-IDFKNGRP              TO BO-RAD-IDFKNGRP                     
039100     MOVE IN-KVRADER               TO BO-RAD-KVRADER                      
039200     MOVE IN-KVBEART-Q             TO BO-RAD-KVBEART-Q                    
039300     MOVE IN-SUORDV                TO BO-RAD-SUORDV                       
039400     MOVE IN-TEASTRIX              TO BO-RAD-TEASTRIX                     
039500     MOVE IN-FLSPARR               TO BO-RAD-FLSPARR                      
039600     MOVE IN-FLSALDCH              TO BO-RAD-FLSALDCH                     
039700     MOVE IN-TIAAVVD (1)           TO BO-RAD-TILEVBSK-INL (1)             
039800     MOVE IN-TIAAVVD (2)           TO BO-RAD-TILEVBSK-INL (2)             
039900     MOVE IN-TIAAVVD (3)           TO BO-RAD-TILEVBSK-INL (3)             
040000     MOVE BO-RAD                   TO SEND-RAD                            
040100     PERFORM S90-PUT-DOC-LINE                                             
040200     .                                                                    
040300     EJECT                                                                
040400 E-TOT-RAD SECTION.                                                       
040500                                                                          
040600     MOVE WS-SKIP2                 TO STYRTECKEN-RAD                      
040700     MOVE SPACE                    TO SEND-RAD                            
040800     PERFORM S90-PUT-DOC-LINE                                             
040900     MOVE WS-TOTALSUMMA            TO BO-SLUTRAD-VALUE                    
041000     MOVE BO-SLUTRAD               TO SEND-RAD                            
041100     MOVE WS-SKIP1                 TO STYRTECKEN-RAD                      
041200     PERFORM S90-PUT-DOC-LINE                                             
041300     .                                                                    
041400     EJECT                                                                
041500 Z-FINIT SECTION.                                                         
041600     CLOSE W44087                                                         
041700     SKIP2                                                                
041800     MOVE 'S' TO POSTSUM-OPKOD                                            
041900     CALL POSTSUM USING POSTSUM-PARM                                      
042000     .                                                                    
042100     EJECT                                                                
042200 S01-READ-W44087  SECTION.                                                
042300     READ W44087 INTO IN-AREA                                             
042400     AT END                                                               
042500        MOVE HIGH-VALUE TO IN-AREA                                        
042600        SET END-OF-W44087 TO TRUE                                         
042700                                                                          
042800     NOT AT END                                                           
042900        MOVE 'W44087' TO POSTSUM-FDNAMN                                   
043000        MOVE 'W44091D1' TO POSTSUM-DDNAMN2                                
043100        MOVE SPACE TO POSTSUM-TRANSTYP                                    
043200        CALL POSTSUM USING POSTSUM-PARM                                   
043300     END-READ                                                             
043400     .                                                                    
043500     EJECT                                                                
043600*    --- DISPATCHER SECTIONS                                              
043700 S90-SEND-OPEN SECTION.                                                   
043800                                                                          
043900     MOVE 'OPEN'                        TO SEND-KDFUNC                    
044000     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
044100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
044200                         SEND-OPEN-AREA                                   
044300     IF SEND-KDRC > 0                                                     
             MOVE SEND-KDRC TO KDRC-DISPLAY                                     
044500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
044600       DELIMITED BY SIZE INTO ERRTEXT                                     
044700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
044800     END-IF                                                               
044900     .                                                                    
045000     EJECT                                                                
045100 S90-PUT-DAP-START SECTION.                                               
045200                                                                          
045300     MOVE 1                       TO REQU-IDMSGVER                        
045400     MOVE 'R'                     TO REQU-KDPGMACT                        
045500     MOVE IDPGM                   TO REQU-IDUSER                          
045600     MOVE 'BO-MAIL1'              TO HDR-IDOUTTYPE                        
045700     MOVE SPACE                   TO HDR-IDOUTREC                         
045800                                     HDR-IDLIST                           
045900     MOVE DAP-IDLANDX3            TO HDR-IDOUTREC (1:3)                   
046000                                     HDR-IDLIST                           
046100     MOVE 'PUT'                   TO SEND-KDFUNC                          
046200     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
046300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
046400                         SEND-KVDLEN                                      
046500                         HDR-AREA                                         
046600     IF SEND-KDRC > ZERO                                                  
046700       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
046800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
046900       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
047000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
047100     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 S90-PUT-DOC-LINE SECTION.                                                
047500                                                                          
047600     MOVE 'PUT'                           TO SEND-KDFUNC                  
047700*                     -- UTAN STYRTECKEN:                                 
047800     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
047900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
048000                         SEND-KVDLEN                                      
048100*                     -- UTAN STYRTECKEN:                                 
048200                         SEND-RAD                                         
048300     IF SEND-KDRC > ZERO                                                  
048400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
048500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
048600       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
048700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 S90-SEND-CLOSE SECTION.                                                  
049200                                                                          
049300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
049400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
049500                                                                          
049600     IF SEND-KDRC > 0                                                     
049700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
049800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
049900       DELIMITED BY SIZE INTO ERRTEXT                                     
050000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050100     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
