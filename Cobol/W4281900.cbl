000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4281900.                                                
000400 AUTHOR.         ÖSTRÖM ELEONOR.                                          
000500 DATE-WRITTEN.   08/09/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PROGRAMMET SUMMERAR LÄGGER TILL LANDSPREFIX TILL                 
001100*        FIL FÖR "DISCREPANCY DEVIATIONS WEEKLY FOLLOW UP" LISTA.         
001200*        PROGRAMMET INGÅR I RUTIN W428V1.                                 
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDB6                                       
001700*                                                                         
001800*        E'TRACKER. 6785206  DATED 2008-06-18                             
001900*                                                                         
001910*  SO    E'TRACKER. 8997128  DATED 2011-05-12 DISCR. FOLLOW-UP LDC        
001911*                            CORRECTION.                                  
001920*                                                                         
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- UPPFÖLJNINGSFIL LEVERANSANMÄRKNINGAR                       
003000     SELECT W42818                     ASSIGN TO W42819D1.                
003100     EJECT                                                                
003200*          --- UPPFÖLJNINGSFIL FAKTURERADE RADER                          
003300     SELECT W42819                     ASSIGN TO W42819D2.                
003400     EJECT                                                                
003410*          --- UTFIL LANDXPREFIX LEVERANSANMÄRKNING                       
003420     SELECT W42823                     ASSIGN TO W42819D3.                
003430     EJECT                                                                
003440*                                                                         
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W42818                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W4281801    -L.                                                
004400     SKIP2                                                                
004500 FD  W42819                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  -COPY W4281901    -L.                                                
005000     EJECT                                                                
005010 FD  W42823                                                               
005020     RECORDING       F                                                    
005030     BLOCK CONTAINS  0.                                                   
005040                                                                          
005050*01  POST -COPY W4282301 -PRE  UT1-    -L.                                
005060     SKIP2                                                                
005093     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'W4281900'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
006000 77  SPAR-DOC-TIAAPP             PIC 9(4)    VALUE ZERO.                  
006100 77  SPAR-IDLANDX2               PIC X(2)    VALUE SPACE.                 
006110 77  SPAR-TIAAVV                 PIC 9(4)    VALUE ZERO.                  
006120 77  SPAR-TIAAPP                 PIC 9(4)    VALUE ZERO.                  
006200 77  WS-SUART-WEEK-00            PIC 9(5)    VALUE ZERO.                  
006300 77  WS-SUART-YEAR-00            PIC 9(5)    VALUE ZERO.                  
006800                                                                          
006900 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
007000 77  KDRC-DISPLAY                PIC Z(5).                                
007001                                                                          
007002 77  FLKOD-SW             PIC X       VALUE 'N'.                          
007003     88  FLKOD-OK                     VALUE 'Y'.                          
007004     88  FLKOD-EJ-OK                  VALUE 'N'.                          
007005                                                                          
007006 77  W-KDANMORS-00               PIC X(2) VALUE SPACE.                    
007007     88  KOD-00                       VALUE '00'.                         
007008                                                                          
007009 77  W-KDANMORS-11               PIC X(2) VALUE SPACE.                    
007010     88  KOD-11                       VALUE '11' '12' '13'.               
007011                                                                          
007012 77  W-KDANMORS-2X               PIC X(2) VALUE SPACE.                    
007013     88  KOD-2X                       VALUE '20' '21' '22' '23'.          
007014                                                                          
007015 77  W-KDANMORS-4X               PIC X(2) VALUE SPACE.                    
007016     88  KOD-4X                       VALUE '42' '43'.                    
007017                                                                          
007018 77  W-KDANMORS-6X               PIC X(2) VALUE SPACE.                    
007019     88  KOD-6X                       VALUE '60' '61' '62'.               
007020                                                                          
007021 01  WW-DCTAB.                                                            
007022   03  WWDC-TAB                  OCCURS 500 INDEXED BY TAB-IX.            
007030     05  WWDC-IDDC               PIC X(2)   VALUE SPACE.                  
007040     05  WWDC-SURADER-WEEK       PIC 9(9)   VALUE ZERO.                   
007050     05  WWDC-SURADER-YEAR       PIC 9(9)   VALUE ZERO.                   
007060     05  WWDC-TIAAVV             PIC 9(4)   VALUE ZERO.                   
007080                                                                          
007100     SKIP2                                                                
007200 01  FELTEXT.                                                             
007300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500                                                                          
007600 77  W42818-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W42818                       VALUE 'J'.                   
007800     EJECT                                                                
007801                                                                          
007810 77  W42819-EOF-SW               PIC X       VALUE 'N'.                   
007820     88  END-OF-W42819                       VALUE 'J'.                   
007830     EJECT                                                                
007900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008000 01  FILLER REDEFINES DAGENS-DATUM.                                       
008100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008400     EJECT                                                                
008500 01  DYNAMISKA-SUBPROGRAM.                                                
008600*                                                                         
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600*    --- PARAMETERS TO ABEND                                              
009700                                                                          
009800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010100                                                                          
010200     EJECT                                                                
010300*                                                                         
010400 01  IN1-AREA-START               PIC X(24)   VALUE                       
010500                                             'IN1-AREA-START'.            
010600     SKIP2                                                                
010700                                                                          
010800*01  AREA -COPY W4281801   -PRE IN1-                                      
010900*                                                                         
011000 01  IN2-AREA-START               PIC X(24)   VALUE                       
011100                                             'IN2-AREA-START'.            
011200     SKIP2                                                                
011300                                                                          
011400*01  AREA -COPY W4281901   -PRE IN2-                                      
011500*                                                                         
011510 01  UT1-AREA-START               PIC X(24)   VALUE                       
011520                                             'UT1-AREA-START'.            
011530     SKIP2                                                                
011540                                                                          
011550*01  AREA -COPY W4282301   -PRE UT1-                                      
011560*                                                                         
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  NYCKLAR-TILL-DLI.                                                    
012100     03  W-IDDC-X.                                                        
012200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012300     SKIP2                                                                
012400*    --- STATUS-KOD FRÅN IMS                                              
012500 01  STATUS-WS                   PIC XX.                                  
012600     88  SEGMENT-FINNS                       VALUE '  '.                  
012700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013000     88  IMS-EJ-OK                           VALUE 'XD'.                  
013100     SKIP2                                                                
013200 01  GODK-STATUSKODER.                                                    
013300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400     SKIP3                                                                
013500 01  SSA1                        PIC X(64).                               
013600 01  SSA2                        PIC X(64).                               
013700     EJECT                                                                
013800*    --- IMS FUNKTIONSKODER                                               
013900*01  -COPY W0003                                                          
014000     EJECT                                                                
014100*    ---  DLI INPUT-OUTPUT AREA                                           
014200                                                                          
014300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014400 01  DLI-IO-WDB601.                                                       
014500*    03  -COPY WDB601                                                     
014600     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800                                                                          
014900*01  -COPY W0009   -PRE MSG-                                              
015000     EJECT                                                                
015100 01  DISTRDOC-PCB                PIC X.                                   
015200     EJECT                                                                
015300                                                                          
015400*01  -COPY W0008  -PRE WDB6-                                              
015500     05  FILLER                  PIC X.                                   
015510     EJECT                                                                
015910 PROCEDURE DIVISION  USING MSG-PCB WDB6-PCB.                              
015920 MAIN SECTION.                                                            
015930     ENTRY 'DLITCBL' USING MSG-PCB WDB6-PCB.                              
016000                                                                          
016100     SKIP2                                                                
016200     PERFORM A-INIT                                                       
016300                                                                          
016400     PERFORM S01-LAES-W42818                                              
016500                                                                          
016600     PERFORM UNTIL END-OF-W42818                                          
016700       MOVE IN1-IDDC      TO SPAR-IDDC                                    
016800                             W-IDDC                                       
016810       MOVE IN1-TIAAVV    TO SPAR-TIAAVV                                  
016811       MOVE IN1-TIAAPP    TO SPAR-TIAAPP                                  
016820                                                                          
016900       PERFORM S21-SOK-DC-TAB                                             
016910       PERFORM IMS-GET-WDB601                                             
016920       IF SEGMENT-FINNS                                                   
016930         MOVE DCS-ADGMT-PADR(11:20)  TO UT1-ADCITY                        
017000         MOVE DCS-IDLANDX2           TO UT1-IDLANDX2                      
017100       ELSE                                                               
017110         MOVE SPACE                  TO UT1-ADCITY                        
017120         MOVE SPACE                  TO UT1-IDLANDX2                      
017130       END-IF                                                             
017200                                                                          
017600       PERFORM UNTIL END-OF-W42818 OR                                     
017700            IN1-IDDC NOT = SPAR-IDDC                                      
017802                                                                          
017810         IF IN1-KDANMORS = '00'                                           
017900           PERFORM B-BEHANDLA-00                                          
017921         ELSE                                                             
017930           IF IN1-KDANMORS = '11' OR IN1-KDANMORS = '12' OR               
017931              IN1-KDANMORS = '13'                                         
017940             PERFORM B-BEHANDLA-11                                        
017950           ELSE                                                           
018010             IF IN1-KDANMORS = '20' OR IN1-KDANMORS = '21' OR             
018011                IN1-KDANMORS = '22' OR IN1-KDANMORS = '23'                
018020               PERFORM B-BEHANDLA-2X                                      
018030             ELSE                                                         
018050               IF IN1-KDANMORS = '42' OR IN1-KDANMORS = '43'              
018061                 PERFORM B-BEHANDLA-4X                                    
018070               ELSE                                                       
018090                 IF IN1-KDANMORS = '60' OR IN1-KDANMORS = '61' OR         
018091                    IN1-KDANMORS = '62'                                   
018092                   PERFORM B-BEHANDLA-6X                                  
018093                 END-IF                                                   
018094               END-IF                                                     
018095             END-IF                                                       
018096           END-IF                                                         
018097         END-IF                                                           
018098                                                                          
018100       END-PERFORM                                                        
018200                                                                          
018400                                                                          
018500     END-PERFORM                                                          
018600                                                                          
018700     PERFORM Z-FINIT                                                      
018800                                                                          
018900     MOVE ZERO TO RETURN-CODE                                             
019000     GOBACK                                                               
019100     .                                                                    
019300 A-INIT SECTION.                                                          
019600     OPEN INPUT  W42818                                                   
019610                 W42819                                                   
019700                                                                          
019800     OPEN OUTPUT W42823                                                   
019900                                                                          
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020010                                                                          
020011     SET TAB-IX TO 1                                                      
020012     PERFORM UNTIL TAB-IX > 500                                           
020013       MOVE SPACE              TO WWDC-IDDC(TAB-IX)                       
020014       MOVE ZERO               TO WWDC-SURADER-WEEK(TAB-IX)               
020015       MOVE ZERO               TO WWDC-SURADER-WEEK(TAB-IX)               
020018       MOVE ZERO               TO WWDC-TIAAVV(TAB-IX)                     
020019       SET TAB-IX              UP BY 1                                    
020020     END-PERFORM                                                          
020021                                                                          
020022     PERFORM S20-LADDA-DC-TAB                                             
020100     .                                                                    
020300 B-BEHANDLA-00 SECTION.                                                   
020500     MOVE ZERO          TO WS-SUART-WEEK-00                               
020600     MOVE ZERO          TO WS-SUART-YEAR-00                               
020610     MOVE IN1-KDANMORS  TO W-KDANMORS-00                                  
020700     MOVE JA  TO FLKOD-SW                                                 
020800                                                                          
021300     PERFORM UNTIL IN1-IDDC NOT = SPAR-IDDC OR FLKOD-EJ-OK                
021400      IF KOD-00                                                           
021700                                                                          
021710       COMPUTE WS-SUART-WEEK-00 = WS-SUART-WEEK-00 +                      
021800                                  IN1-SUART-WEEK                          
021810       END-COMPUTE                                                        
021820       COMPUTE WS-SUART-YEAR-00 = WS-SUART-YEAR-00 +                      
021830                                  IN1-SUART-YEAR                          
021840       END-COMPUTE                                                        
022100                                                                          
023000       PERFORM S01-LAES-W42818                                            
023010       MOVE IN1-KDANMORS TO W-KDANMORS-00                                 
023020      ELSE                                                                
023030       MOVE NEJ  TO FLKOD-SW                                              
023040      END-IF                                                              
023100     END-PERFORM                                                          
023101                                                                          
023109     MOVE SPAR-IDDC         TO   UT1-IDDC                                 
023110     MOVE DCS-IDLANDX2      TO   UT1-IDLANDX2                             
023111*    MOVE 'SE'              TO   UT1-IDLANDX2                             
023112     MOVE '00'              TO   UT1-KDANMORS-001                         
023113     MOVE WS-SUART-WEEK-00  TO   UT1-SUART-WEEK                           
023114     MOVE WS-SUART-YEAR-00  TO   UT1-SUART-YEAR                           
023115     MOVE SPAR-TIAAVV       TO   UT1-TIAAVV                               
023116     MOVE SPAR-TIAAPP       TO   UT1-TIAAPP                               
023117                                                                          
023140     PERFORM S11-SKRIV-W42823                                             
023200     .                                                                    
023300     EJECT                                                                
023400 B-BEHANDLA-11 SECTION.                                                   
023500                                                                          
023600     MOVE ZERO          TO WS-SUART-WEEK-00                               
023700     MOVE ZERO          TO WS-SUART-YEAR-00                               
023710     MOVE JA  TO FLKOD-SW                                                 
023720     MOVE IN1-KDANMORS  TO W-KDANMORS-11                                  
023800                                                                          
023900     PERFORM UNTIL IN1-IDDC NOT = SPAR-IDDC OR FLKOD-EJ-OK                
024000                                                                          
024100      IF KOD-11                                                           
024110       COMPUTE WS-SUART-WEEK-00 = WS-SUART-WEEK-00 +                      
024120                                  IN1-SUART-WEEK                          
024130       END-COMPUTE                                                        
024140       COMPUTE WS-SUART-YEAR-00 = WS-SUART-YEAR-00 +                      
024150                                  IN1-SUART-YEAR                          
024160       END-COMPUTE                                                        
024400                                                                          
024500       PERFORM S01-LAES-W42818                                            
024510       MOVE IN1-KDANMORS TO W-KDANMORS-11                                 
024520      ELSE                                                                
024530        MOVE NEJ TO FLKOD-SW                                              
024540      END-IF                                                              
024600     END-PERFORM                                                          
024700                                                                          
024800     MOVE SPAR-IDDC         TO   UT1-IDDC                                 
024900     MOVE DCS-IDLANDX2      TO   UT1-IDLANDX2                             
024910*    MOVE 'SE'              TO   UT1-IDLANDX2                             
025000     MOVE '11'              TO   UT1-KDANMORS-001                         
025100     MOVE WS-SUART-WEEK-00  TO   UT1-SUART-WEEK                           
025200     MOVE WS-SUART-YEAR-00  TO   UT1-SUART-YEAR                           
025300     MOVE SPAR-TIAAVV       TO   UT1-TIAAVV                               
025301     MOVE SPAR-TIAAPP       TO   UT1-TIAAPP                               
025302                                                                          
026100     PERFORM S11-SKRIV-W42823                                             
026200     .                                                                    
026300     EJECT                                                                
026400 B-BEHANDLA-2X SECTION.                                                   
026600     MOVE ZERO          TO WS-SUART-WEEK-00                               
026700     MOVE ZERO          TO WS-SUART-YEAR-00                               
026710     MOVE JA  TO FLKOD-SW                                                 
026720     MOVE IN1-KDANMORS  TO W-KDANMORS-2X                                  
026800                                                                          
026900     PERFORM UNTIL IN1-IDDC NOT = SPAR-IDDC OR FLKOD-EJ-OK                
027100                                                                          
027101      IF KOD-2X                                                           
027110       COMPUTE WS-SUART-WEEK-00 = WS-SUART-WEEK-00 +                      
027120                                  IN1-SUART-WEEK                          
027130       END-COMPUTE                                                        
027140       COMPUTE WS-SUART-YEAR-00 = WS-SUART-YEAR-00 +                      
027150                                  IN1-SUART-YEAR                          
027160       END-COMPUTE                                                        
027170                                                                          
027230       PERFORM S01-LAES-W42818                                            
027231       MOVE IN1-KDANMORS TO W-KDANMORS-2X                                 
027232      ELSE                                                                
027233       MOVE NEJ TO FLKOD-SW                                               
027234      END-IF                                                              
027240     END-PERFORM                                                          
027250                                                                          
027260     MOVE SPAR-IDDC         TO   UT1-IDDC                                 
027270     MOVE DCS-IDLANDX2      TO   UT1-IDLANDX2                             
027271*    MOVE 'SE'              TO   UT1-IDLANDX2                             
027280     MOVE '2X'              TO   UT1-KDANMORS-001                         
027290     MOVE WS-SUART-WEEK-00  TO   UT1-SUART-WEEK                           
027291     MOVE WS-SUART-YEAR-00  TO   UT1-SUART-YEAR                           
027292     MOVE SPAR-TIAAVV       TO   UT1-TIAAVV                               
027293     MOVE SPAR-TIAAPP       TO   UT1-TIAAPP                               
027294                                                                          
027314     PERFORM S11-SKRIV-W42823                                             
027315     .                                                                    
027316     EJECT                                                                
027317 B-BEHANDLA-4X SECTION.                                                   
027318                                                                          
027319     MOVE ZERO          TO WS-SUART-WEEK-00                               
027320     MOVE ZERO          TO WS-SUART-YEAR-00                               
027321     MOVE JA  TO FLKOD-SW                                                 
027322     MOVE IN1-KDANMORS  TO W-KDANMORS-4X                                  
027323                                                                          
027324     PERFORM UNTIL IN1-IDDC NOT = SPAR-IDDC OR FLKOD-EJ-OK                
027325                                                                          
027326      IF KOD-4X                                                           
027327       COMPUTE WS-SUART-WEEK-00 = WS-SUART-WEEK-00 +                      
027328                                  IN1-SUART-WEEK                          
027329       END-COMPUTE                                                        
027330       COMPUTE WS-SUART-YEAR-00 = WS-SUART-YEAR-00 +                      
027331                                  IN1-SUART-YEAR                          
027332       END-COMPUTE                                                        
027333                                                                          
027334       PERFORM S01-LAES-W42818                                            
027335       MOVE IN1-KDANMORS TO W-KDANMORS-4X                                 
027336      ELSE                                                                
027337       MOVE NEJ TO FLKOD-SW                                               
027338      END-IF                                                              
027339     END-PERFORM                                                          
027340                                                                          
027341     MOVE SPAR-IDDC         TO   UT1-IDDC                                 
027342     MOVE DCS-IDLANDX2      TO   UT1-IDLANDX2                             
027343*    MOVE 'SE'              TO   UT1-IDLANDX2                             
027344     MOVE '4X'              TO   UT1-KDANMORS-001                         
027345     MOVE WS-SUART-WEEK-00  TO   UT1-SUART-WEEK                           
027346     MOVE WS-SUART-YEAR-00  TO   UT1-SUART-YEAR                           
027347     MOVE SPAR-TIAAVV       TO   UT1-TIAAVV                               
027348     MOVE SPAR-TIAAPP       TO   UT1-TIAAPP                               
027349                                                                          
027350     PERFORM S11-SKRIV-W42823                                             
027351     .                                                                    
027352     EJECT                                                                
027353 B-BEHANDLA-6X SECTION.                                                   
027355     MOVE ZERO          TO WS-SUART-WEEK-00                               
027356     MOVE ZERO          TO WS-SUART-YEAR-00                               
027357     MOVE JA  TO FLKOD-SW                                                 
027358     MOVE IN1-KDANMORS  TO W-KDANMORS-6X                                  
027359                                                                          
027360     PERFORM UNTIL IN1-IDDC NOT = SPAR-IDDC OR FLKOD-EJ-OK                
027361                                                                          
027362      IF KOD-6X                                                           
027363       COMPUTE WS-SUART-WEEK-00 = WS-SUART-WEEK-00 +                      
027364                                  IN1-SUART-WEEK                          
027365       END-COMPUTE                                                        
027366       COMPUTE WS-SUART-YEAR-00 = WS-SUART-YEAR-00 +                      
027367                                  IN1-SUART-YEAR                          
027368       END-COMPUTE                                                        
027369                                                                          
027370       PERFORM S01-LAES-W42818                                            
027371       MOVE IN1-KDANMORS TO W-KDANMORS-6X                                 
027372      ELSE                                                                
027373       MOVE NEJ TO FLKOD-SW                                               
027374      END-IF                                                              
027375     END-PERFORM                                                          
027376                                                                          
027377     MOVE SPAR-IDDC         TO   UT1-IDDC                                 
027378     MOVE DCS-IDLANDX2      TO   UT1-IDLANDX2                             
027379*    MOVE 'SE'              TO   UT1-IDLANDX2                             
027380     MOVE '6X'              TO   UT1-KDANMORS-001                         
027381     MOVE WS-SUART-WEEK-00  TO   UT1-SUART-WEEK                           
027382     MOVE WS-SUART-YEAR-00  TO   UT1-SUART-YEAR                           
027383     MOVE SPAR-TIAAVV       TO   UT1-TIAAVV                               
027384     MOVE SPAR-TIAAPP       TO   UT1-TIAAPP                               
027385                                                                          
027386     PERFORM S11-SKRIV-W42823                                             
027387     .                                                                    
027388     EJECT                                                                
027390 Z-FINIT SECTION.                                                         
027700     CLOSE W42818                                                         
027701           W42819                                                         
027710           W42823                                                         
027900     MOVE 'S' TO POSTSUM-OPKOD                                            
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     .                                                                    
028200     EJECT                                                                
028300 S01-LAES-W42818  SECTION.                                                
028500     READ W42818 INTO IN1-AREA                                            
028600     AT END                                                               
028700        MOVE HIGH-VALUE TO IN1-W42818                                     
028800        SET END-OF-W42818 TO TRUE                                         
028900                                                                          
029000     NOT AT END                                                           
029100        MOVE 'W42818'   TO POSTSUM-FDNAMN                                 
029200        MOVE 'W42819D1' TO POSTSUM-DDNAMN2                                
029300        MOVE SPACE      TO POSTSUM-TRANSTYP                               
029400        CALL POSTSUM USING POSTSUM-PARM                                   
029500     END-READ                                                             
029600     .                                                                    
029700     EJECT                                                                
029710 S02-LAES-W42819  SECTION.                                                
029730     READ W42819 INTO IN2-AREA                                            
029740     AT END                                                               
029750        MOVE HIGH-VALUE TO IN2-W42819                                     
029760        SET END-OF-W42819 TO TRUE                                         
029770                                                                          
029780     NOT AT END                                                           
029790        MOVE 'W42819'   TO POSTSUM-FDNAMN                                 
029791        MOVE 'W42819D2' TO POSTSUM-DDNAMN2                                
029792        MOVE SPACE      TO POSTSUM-TRANSTYP                               
029793        CALL POSTSUM USING POSTSUM-PARM                                   
029794     END-READ                                                             
029795     .                                                                    
029800 S11-SKRIV-W42823 SECTION.                                                
030000     WRITE UT1-POST FROM UT1-AREA                                         
030100     MOVE 'W42823'      TO POSTSUM-FDNAMN                                 
030200     MOVE 'W42819D3'    TO POSTSUM-DDNAMN2                                
030300     MOVE 'UTPOST'      TO POSTSUM-TRANSTYP                               
030400     CALL POSTSUM      USING POSTSUM-PARM                                 
030500     .                                                                    
030610 S20-LADDA-DC-TAB SECTION.                                                
030640     PERFORM S02-LAES-W42819                                              
030650                                                                          
030660     SET TAB-IX TO 1                                                      
030670     PERFORM UNTIL END-OF-W42819                                          
030680       MOVE IN2-IDDC           TO WWDC-IDDC(TAB-IX)                       
030690       MOVE IN2-SURADER-WEEK   TO WWDC-SURADER-WEEK(TAB-IX)               
030691       MOVE IN2-SURADER-YEAR   TO WWDC-SURADER-YEAR(TAB-IX)               
030692       MOVE IN2-TIAAVV         TO WWDC-TIAAVV(TAB-IX)                     
030694       SET TAB-IX              UP BY 1                                    
030695                                                                          
030696       PERFORM S02-LAES-W42819                                            
030697     END-PERFORM                                                          
030701                                                                          
030702     PERFORM UNTIL TAB-IX > 500                                           
030703       MOVE SPACE              TO WWDC-IDDC(TAB-IX)                       
030704       MOVE ZERO               TO WWDC-SURADER-WEEK(TAB-IX)               
030705       MOVE ZERO               TO WWDC-SURADER-YEAR(TAB-IX)               
030706       MOVE ZERO               TO WWDC-TIAAVV(TAB-IX)                     
030707       SET TAB-IX              UP BY 1                                    
030708     END-PERFORM                                                          
030710     .                                                                    
030711 S21-SOK-DC-TAB SECTION.                                                  
030713     SET TAB-IX TO 1                                                      
030714     SEARCH WWDC-TAB                                                      
030715       AT END                                                             
030716         CONTINUE                                                         
030720       WHEN WWDC-IDDC(TAB-IX) = SPAR-IDDC                                 
030721         MOVE WWDC-SURADER-WEEK(TAB-IX)    TO UT1-SURADER-WEEK            
030722         MOVE WWDC-SURADER-YEAR(TAB-IX)    TO UT1-SURADER-YEAR            
030725     END-SEARCH                                                           
030726     .                                                                    
030727     EJECT                                                                
030730* --- IMS SEKTIONER ---                                                   
030800                                                                          
030900     EJECT                                                                
031000 IMS-GET-WDB601 SECTION.                                                  
031200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
031300          DELIMITED BY SIZE INTO SSA1                                     
031400     MOVE '  GE' TO GODK-STATUSKODER                                      
031500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
031600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
031700     PERFORM IMS-STATUSKONTROLL                                           
031800     .                                                                    
031900     EJECT                                                                
032000 IMS-STATUSKONTROLL SECTION.                                              
032200     SET STATUS-IX TO 1                                                   
032300     SEARCH GODK-STATUS                                                   
032400       AT END                                                             
032500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032600           DELIMITED BY SIZE INTO FELTEXT                                 
032700         DISPLAY FELTEXT                                                  
032800         CALL FELLOG                                                      
032900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033000         CONTINUE                                                         
033100     END-SEARCH                                                           
033200     .                                                                    
