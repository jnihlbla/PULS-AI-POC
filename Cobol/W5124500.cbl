001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5124500.                                                
001300 AUTHOR.         SARASWATHY S.                                            
001400 DATE-WRITTEN.   21/05/21.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*    REPORT WHICH SHOWS CURRENCY EFFECT FOR LYNK&CO                       
001810*    AND SHOWS THE STOCK BALANCE IN THE START AND END OF THE MONTH        
001820*    AND SHOWS THE SEK COST TO BUY WITH EACH INBOUND DAY RATE             
001900*                                                                         
002010*    THE PROGRAM READS     WDL2                                           
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- CURRENT VALUE AND PROCUREMENT VALUE FOR PARTS              
003303     SELECT W51239                     ASSIGN TO W51245D1.                
003304     SKIP2                                                                
003305*          --- CURRENT VALUE AND PROCUREMENT VALUE FOR PARTS              
003306     SELECT W51239N                    ASSIGN TO W51245D2.                
003307     SKIP2                                                                
003308*          --- COURSES FOR INVENTORY VALUATION                            
003310     SELECT W51237                     ASSIGN TO W51245D3.                
003311     SKIP2                                                                
003320*          --- LYNK & CO - CURRENCY EFFECT REPORT                         
003330     SELECT W51245                     ASSIGN TO W51245D4.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W51239                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003906*01  -COPY W51239      -L.                                                
003907     SKIP3                                                                
003908 FD  W51239N                                                              
003909     RECORDING       F                                                    
003910     BLOCK CONTAINS  0.                                                   
003911                                                                          
003912*01  -COPY W51239      -L.                                                
003913     SKIP3                                                                
003914 FD  W51237                                                               
003915     RECORDING       F                                                    
003916     BLOCK CONTAINS  0.                                                   
003917                                                                          
003920*01  -COPY W51237      -L.                                                
004000     EJECT                                                                
004010 FD  W51245                                                               
004020     RECORDING V                                                          
004030     BLOCK CONTAINS 0.                                                    
004040                                                                          
004050 01  UT-POST                 PIC X(185).                                  
004060     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)      VALUE 'W5124500'.          
004400 77  YES                         PIC X         VALUE 'J'.                 
004500 77  NOO                         PIC X         VALUE 'N'.                 
004600 77  WS-W51245-HEADER            PIC 9(5)      VALUE ZERO.                
004700 77  WS-STOCK-START              PIC S9(9)     COMP-3 VALUE ZERO.         
004701 77  WS-STOCK-FINAL              PIC S9(9)     COMP-3 VALUE ZERO.         
004702 77  WS-STOCK-REM                PIC S9(9)     COMP-3 VALUE ZERO.         
004703 77  WS-START                    PIC X(5)      VALUE 'START'.             
004704 77  WS-INBOUND                  PIC X(7)      VALUE 'INBOUND'.           
004705 77  WS-FINAL                    PIC X(5)      VALUE 'FINAL'.             
004706 77  WS-STOCK-VAL-START          PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
004707 77  WS-STOCK-VAL-FINAL          PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
004708 77  WS-SEK-COST                 PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
004709 77  WS-SEK-VAL                  PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
004711 77  W-DAUPPDAT                  PIC 9(8)      VALUE ZERO.                
004712 77  W-INLEV-DATUM               PIC 9(8)      VALUE ZERO.                
004713 77  W-PRKURS                    PIC 9(6)V9(5) VALUE ZERO.                
004714 77  WS-KVAVIS-TOT               PIC S9(7) COMP-3 VALUE ZERO.             
004715 77  WS-KVAVIS                   PIC S9(7) COMP-3 VALUE ZERO.             
004716 77  WS-DATUM                    PIC X(8)  VALUE SPACE.                   
004717 77  WS-DATUM-N                  PIC 9(8)  VALUE ZERO.                    
004719 77  WS-DIFF                     PIC 9(2)  VALUE ZERO.                    
004720 77  INDX                        PIC S9(9) VALUE ZERO.                    
004721 77  WS-INBOUND-CNT              PIC S9(9) VALUE ZERO.                    
004722                                                                          
004723 77  W51239-EOF-SW               PIC X         VALUE 'N'.                 
004724     88  END-OF-W51239                         VALUE 'J'.                 
004725                                                                          
004726 77  W51239N-EOF-SW              PIC X         VALUE 'N'.                 
004727     88  END-OF-W51239N                         VALUE 'J'.                
004728                                                                          
004729 77  W51237-EOF-SW               PIC X         VALUE 'N'.                 
004730     88  END-OF-W51237                         VALUE 'J'.                 
004800     EJECT                                                                
004801 77  WS-WDL2-SW                  PIC X         VALUE 'N'.                 
004802     88  WS-WDL2-FOUND                         VALUE 'J'.                 
004803     88  WS-WDL2-NOT-FOUND                     VALUE 'N'.                 
004804     EJECT                                                                
004805 01  WS-YYYYMMDD                 PIC 9(8)    VALUE ZERO.                  
004806 01  FILLER REDEFINES WS-YYYYMMDD.                                        
004807     03  WS-CC                   PIC 9(2).                                
004808     03  WS-YYMMDD.                                                       
004809         05  WS-YY               PIC 9(2).                                
004810         05  WS-MM               PIC 9(2).                                
004811         05  WS-DD               PIC 9(2).                                
004812     EJECT                                                                
004813 01  KURSTABELL-EUR.                                                      
004820     03  EUR           OCCURS 12000  INDEXED BY EUR-IX.                   
004830         05 EUR-DATUM-FOM      PIC 9(8).                                  
004840         05 FILLER             PIC X.                                     
004850         05 EUR-DATUM-TOM      PIC 9(8).                                  
004860         05 FILLER             PIC X.                                     
004870         05 EUR-PRKURS         PIC 9(6)V9(5).                             
004880 01  WS-WDL2-TAB.                                                         
004890     03  WS-TAB        OCCURS 100.                                        
004892         05 WS-TAB-IDARTNR     PIC S9(9) COMP-3 VALUE ZERO.               
004893         05 WS-TAB-KVAVIS      PIC S9(7) VALUE ZERO.                      
004894         05 WS-TAB-PRKURS      PIC 9(6)V9(5) VALUE ZERO.                  
004900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES TODAYS-DATE.                                        
005100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005300     03  TODAYS-DATE-DAY         PIC 9(2).                                
005400     EJECT                                                                
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006020     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
006100     SKIP2                                                                
006200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  ERROR-TEXT.                                                          
006900     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102 01  W001-DAP.                                                            
007103     03  FILLER                  PIC X(185)  VALUE SPACE.                 
007104 01  WS-START-FINAL-DATE.                                                 
007107     03  WS-START-DATE.                                                   
007108         05  WS-START-YY         PIC X(2).                                
007109         05  WS-START-MM         PIC X(2).                                
007110         05  WS-START-DD         PIC X(2).                                
007111     03  WS-FINAL-DATE           PIC X(6)    VALUE SPACE.                 
007112     EJECT                                                                
007113*    --- PARAMETRAR TILL POSTSUM                                          
007114*                                                                         
007120*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302*01  -COPY WWPRODSL                                                       
007303     EJECT                                                                
007304*01  -COPY WZ20DAYS                                                       
007305     EJECT                                                                
007306 01  IN1-AREA-START              PIC X(24)   VALUE                        
007307                                 'IN1-AREA-START  '.                      
007308     SKIP2                                                                
007309                                                                          
007310*01  AREA -COPY W51239     -PRE IN1-                                      
007311     EJECT                                                                
007312 01  IN2-AREA-START              PIC X(24)   VALUE                        
007313                                 'IN2-AREA-START  '.                      
007314     SKIP2                                                                
007315                                                                          
007316*01  AREA -COPY W51239     -PRE IN2-                                      
007317     EJECT                                                                
007318 01  IN3-AREA-START              PIC X(24)   VALUE                        
007319                                 'IN2-AREA-START  '.                      
007320     SKIP2                                                                
007321                                                                          
007330*01  AREA -COPY W51237     -PRE IN3-                                      
007400     EJECT                                                                
007410 01  TEXT-AREA.                                                           
007420     03  UT-HEADER.                                                       
007430         05  FILLER           PIC X(11) VALUE 'PART NUMBER'.              
007440         05  FILLER           PIC X(1)  VALUE ';'.                        
007450         05  FILLER           PIC X(5)  VALUE 'STATE'.                    
007460         05  FILLER           PIC X(1)  VALUE ';'.                        
007470         05  FILLER           PIC X(13) VALUE 'STOCK BALANCE'.            
007480         05  FILLER           PIC X(1)  VALUE ';'.                        
007490         05  FILLER           PIC X(16) VALUE 'INFREIGHT FACTOR'.         
007491         05  FILLER           PIC X(1)  VALUE ';'.                        
007492         05  FILLER           PIC X(14) VALUE 'STANDARD PRICE'.           
007493         05  FILLER           PIC X(1)  VALUE ';'.                        
007494         05  FILLER           PIC X(15) VALUE 'PRICE LIST DATE'.          
007495         05  FILLER           PIC X(1)  VALUE ';'.                        
007496         05  FILLER           PIC X(11) VALUE 'INBOUND QTY'.              
007497         05  FILLER           PIC X(1)  VALUE ';'.                        
007498         05  FILLER           PIC X(11) VALUE 'ORDER PRICE'.              
007499         05  FILLER           PIC X(1)  VALUE ';'.                        
007500         05  FILLER           PIC X(12) VALUE 'INBOUND DATE'.             
007501         05  FILLER           PIC X(1)  VALUE ';'.                        
007502         05  FILLER           PIC X(17) VALUE 'DAY CURRENCY RATE'.        
007503         05  FILLER           PIC X(1)  VALUE ';'.                        
007504         05  FILLER           PIC X(29)                                   
007505                            VALUE 'SEK COST TO BUY WITH DAY RATE'.        
007506         05  FILLER           PIC X(1)  VALUE ';'.                        
007507         05  FILLER           PIC X(11) VALUE 'STOCK VALUE'.              
007508         05  FILLER           PIC X(1)  VALUE ';'.                        
007509         05  FILLER           PIC X(8)  VALUE 'CURRENCY'.                 
007510                                                                          
007511     03 UT-AREA.                                                          
007512         05 UT-IDARTNR        PIC Z(8)9.                                  
007513         05 FILLER            PIC X(1)    VALUE ';'.                      
007514         05 UT-STATE.                                                     
007515           07 UT-STATE-DESC   PIC X(7).                                   
007516           07 UT-STATE-DATE   PIC X(6).                                   
007517         05 FILLER            PIC X(1)    VALUE ';'.                      
007518         05 UT-STOCK          PIC -(7)9.                                  
007519         05 FILLER            PIC X(1)    VALUE ';'.                      
007520         05 UT-RETULF         PIC Z(2)9.9(4).                             
007521         05 FILLER            PIC X(1)    VALUE ';'.                      
007522         05 UT-PRINK          PIC Z(9)9.9(2).                             
007523         05 FILLER            PIC X(1)    VALUE ';'.                      
007524         05 UT-TIPRLIST       PIC 9(6).                                   
007525         05 FILLER            PIC X(1)    VALUE ';'.                      
007526         05 UT-INBQTY         PIC -(7)9.                                  
007527         05 FILLER            PIC X(1)    VALUE ';'.                      
007528         05 UT-ORDERPR        PIC Z(9)9.9(2).                             
007529         05 FILLER            PIC X(1)    VALUE ';'.                      
007530         05 UT-INBDATE        PIC 9(6).                                   
007531         05 FILLER            PIC X(1)    VALUE ';'.                      
007532         05 UT-DAYCUR         PIC Z(5)9.9(5).                             
007533         05 FILLER            PIC X(1)    VALUE ';'.                      
007534         05 UT-SEKCOST        PIC Z(9)9.9(2).                             
007535         05 FILLER            PIC X(1)    VALUE ';'.                      
007536         05 UT-STOCKVAL       PIC Z(9)9.9(2).                             
007537         05 FILLER            PIC X(1)    VALUE ';'.                      
007538         05 UT-KDVALISO       PIC X(3).                                   
007539                                                                          
007540*    --- AREAS FOR IMS-SECTIONS                                           
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-FOR-DLI.                                                        
008101     03  W-IDARTNR-X.                                                     
008110         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
008111     03 W-DAINLEV-X.                                                      
008112        05  W-DAINLEV           PIC 9(16).                                
008120     03  W-IDPTYP-X.                                                      
008130       05  W-IDPTYP              PIC X(3)   VALUE 'R32'.                  
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008800     SKIP2                                                                
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNCTION CODES                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010020 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDL201'.          
010030 01  DLI-IO-WDL201.                                                       
010040*        05  -COPY WDL201                                                 
010050     EJECT                                                                
010051 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDL211'.           
010052 01  DLI-IO-WDL211.                                                       
010053*    03  -COPY WDL211                                                     
010054     EJECT                                                                
010060 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDL221'.          
010070 01  DLI-IO-WDL221.                                                       
010080*        05  -COPY WDL221                                                 
010090     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601                                                                          
010602*01  -COPY W0008  -PRE WDL2-                                              
010610     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING WDL2-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDL2-PCB.                                      
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011310     PERFORM AA-READ-W51237-INTO-TABLE                                    
011320                                                                          
011400     PERFORM S02-READ-W51239N                                             
011401     PERFORM S01-READ-W51239                                              
011420     PERFORM UNTIL END-OF-W51239N                                         
011440       PERFORM UNTIL END-OF-W51239 OR                                     
011450                       IN1-IDARTNR > IN2-IDARTNR                          
011460         IF IN2-IDARTNR = IN1-IDARTNR                                     
011470           MOVE IN2-KDPRODSL TO TEST-KDPRODSL                             
011471           IF KDPRODSL-LYNK                                               
011472             MOVE IN2-IDARTNR TO W-IDARTNR                                
011473             PERFORM B-PROCESS                                            
011474           END-IF                                                         
011480         END-IF                                                           
011490         PERFORM S01-READ-W51239                                          
011491       END-PERFORM                                                        
011492       PERFORM S02-READ-W51239N                                           
011493     END-PERFORM                                                          
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013301                                                                          
013302     OPEN INPUT  W51239                                                   
013310     OPEN INPUT  W51239N                                                  
013320     OPEN INPUT  W51237                                                   
013500                                                                          
013510     OPEN OUTPUT W51245                                                   
013520                                                                          
013600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-YYYYMMDD                      
013603     COMPUTE WS-DIFF = WS-MM - 1                                          
013604     MOVE WS-YYMMDD TO WS-FINAL-DATE                                      
013605     COMPUTE WS-MM = WS-MM - 1                                            
013606     IF WS-MM = 0                                                         
013609       MOVE 12 TO WS-MM                                                   
013610       COMPUTE WS-YY = WS-YY - 1                                          
013620     END-IF                                                               
013621     MOVE 01 TO WS-DD                                                     
013630     MOVE WS-YYMMDD TO WS-START-DATE                                      
013720     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000     EJECT                                                                
014001                                                                          
014002 AA-READ-W51237-INTO-TABLE SECTION.                                       
014003     MOVE HIGH-VALUES TO KURSTABELL-EUR                                   
014005     MOVE NOO         TO W51237-EOF-SW                                    
014006     SET EUR-IX TO +1                                                     
014007     INITIALIZE IN3-AREA                                                  
014008     PERFORM S03-READ-W51237                                              
014009     PERFORM UNTIL END-OF-W51237                                          
014012       IF IN3-KDVALISO = 'EUR'                                            
014013         MOVE IN3-AREA(5:29) TO EUR (EUR-IX)                              
014015         SET EUR-IX UP BY +1                                              
014016       END-IF                                                             
014017       PERFORM S03-READ-W51237                                            
014018     END-PERFORM                                                          
014020     .                                                                    
014021     EJECT                                                                
014022                                                                          
014023 B-PROCESS SECTION.                                                       
014024                                                                          
014026     IF WS-W51245-HEADER = 0                                              
014027       PERFORM S04-SKRIV-DAP1                                             
014028       PERFORM S05-SKRIV-DAP2                                             
014030       WRITE UT-POST   FROM UT-HEADER                                     
014031       ADD +1 TO WS-W51245-HEADER                                         
014032     END-IF                                                               
014035     PERFORM BA-START-LINE-DATA                                           
014036     PERFORM BB-INBOUND-LINE-DATA                                         
014040     PERFORM BC-FINAL-LINE-DATA                                           
014050     PERFORM BD-INIT-WSDATA                                               
014060     .                                                                    
014070     EJECT                                                                
014080 BA-START-LINE-DATA SECTION.                                              
014090                                                                          
014100     MOVE IN1-IDARTNR     TO UT-IDARTNR                                   
014101     MOVE WS-START        TO UT-STATE-DESC                                
014102     MOVE WS-START-DATE   TO UT-STATE-DATE                                
014107     COMPUTE WS-STOCK-START = IN1-KVAKS + IN1-KVLS + IN1-KVEFRS           
014108     MOVE WS-STOCK-START  TO UT-STOCK                                     
014117     MOVE ZERO            TO UT-RETULF                                    
014118     MOVE IN1-PRINK       TO UT-PRINK                                     
014119     MOVE ZERO            TO UT-TIPRLIST                                  
014120     MOVE ZERO            TO UT-INBQTY                                    
014121     MOVE ZERO            TO UT-ORDERPR                                   
014122     MOVE ZERO            TO UT-INBDATE                                   
014123     MOVE ZERO            TO UT-DAYCUR                                    
014124     MOVE ZERO            TO UT-SEKCOST                                   
014125     COMPUTE WS-STOCK-VAL-START = WS-STOCK-START * IN1-PRINK              
014126     MOVE WS-STOCK-VAL-START    TO UT-STOCKVAL                            
014127     MOVE 'SEK'           TO UT-KDVALISO                                  
014128     WRITE UT-POST   FROM UT-AREA                                         
014129     .                                                                    
014130     EJECT                                                                
014131 BB-INBOUND-LINE-DATA SECTION.                                            
014140                                                                          
014141     PERFORM IMS-GU-WDL201                                                
014142     IF SEGMENT-FOUND                                                     
014144       PERFORM IMS-GNP-WDL211                                             
014145       IF SEGMENT-FOUND                                                   
014146         PERFORM UNTIL SEGMENT-MISSING                                    
014147           MOVE INL-DAINLEV             TO W-DAINLEV                      
014148           PERFORM IMS-GNP-WDL221                                         
014149           IF SEGMENT-FOUND                                               
014150             PERFORM UNTIL SEGMENT-MISSING                                
014151               IF MOT-IDPTYP = 'R32'                                      
014153                 IF MOT-KVAVIS > 0                                        
014154                   MOVE MOT-KVAVIS      TO WS-KVAVIS                      
014158                   MOVE MOT-TIUPPDAT    TO W-DAUPPDAT(2:7)                
014160                   IF W-DAUPPDAT(3:2) < 50                                
014161                     MOVE 20            TO W-DAUPPDAT(1:2)                
014162                   ELSE                                                   
014163                     MOVE 19            TO W-DAUPPDAT(1:2)                
014164                   END-IF                                                 
014165                   MOVE W-DAUPPDAT      TO W-INLEV-DATUM                  
014166                   IF W-INLEV-DATUM(5:2) = WS-DIFF                        
014168                     PERFORM BBA-MOVE-DATA                                
014169                   END-IF                                                 
014170                   MOVE YES TO WS-WDL2-SW                                 
014171                 ELSE                                                     
014172                   MOVE NOO TO WS-WDL2-SW                                 
014173                 END-IF                                                   
014174               ELSE                                                       
014175                 MOVE NOO TO WS-WDL2-SW                                   
014176               END-IF                                                     
014177               PERFORM IMS-GNP-WDL221                                     
014178             END-PERFORM                                                  
014179           ELSE                                                           
014180             MOVE NOO TO WS-WDL2-SW                                       
014181           END-IF                                                         
014182          PERFORM IMS-GNP-WDL211                                          
014183         END-PERFORM                                                      
014184       ELSE                                                               
014185         MOVE NOO TO WS-WDL2-SW                                           
014186       END-IF                                                             
014187     ELSE                                                                 
014188       MOVE NOO TO WS-WDL2-SW                                             
014189     END-IF                                                               
014190     .                                                                    
014191     EJECT                                                                
014192 BBA-MOVE-DATA SECTION.                                                   
014193                                                                          
014195     ADD +1 TO INDX                                                       
014197     SET EUR-IX TO 1                                                      
014198     SEARCH  EUR                                                          
014199       AT END DISPLAY 'KURS FÖR VALUTA ' IN2-KDVALISO (1)' SAKNAS'        
014200       WHEN (EUR-DATUM-FOM  (EUR-IX)  <= W-INLEV-DATUM) AND               
014201            (EUR-DATUM-TOM  (EUR-IX)  >= W-INLEV-DATUM)                   
014205            COMPUTE W-PRKURS ROUNDED = EUR-PRKURS (EUR-IX)                
014213     END-SEARCH                                                           
014214     MOVE IN2-IDARTNR     TO UT-IDARTNR                                   
014215                             WS-TAB-IDARTNR(INDX)                         
014216     MOVE WS-INBOUND      TO UT-STATE-DESC                                
014217     MOVE SPACE           TO UT-STATE-DATE                                
014218     MOVE ZERO            TO UT-STOCK                                     
014219     MOVE IN2-RETULF      TO UT-RETULF                                    
014220     MOVE ZERO            TO UT-PRINK                                     
014221     MOVE IN1-TIPRLIST(1) TO UT-TIPRLIST                                  
014222     MOVE WS-KVAVIS       TO UT-INBQTY                                    
014223                             WS-TAB-KVAVIS(INDX)                          
014224     MOVE IN2-PRARTBEL-PR(1)  TO UT-ORDERPR                               
014225     MOVE W-INLEV-DATUM   TO UT-INBDATE                                   
014226     MOVE W-PRKURS        TO UT-DAYCUR                                    
014227                             WS-TAB-PRKURS(INDX)                          
014231     COMPUTE WS-SEK-VAL = IN2-PRARTBEL-PR(1)                              
014232                          * WS-KVAVIS * W-PRKURS                          
014235     MOVE WS-SEK-VAL      TO UT-SEKCOST                                   
014236     MOVE ZERO            TO UT-STOCKVAL                                  
014237     MOVE IN2-KDVALISO(1) TO UT-KDVALISO                                  
014238     WRITE UT-POST   FROM UT-AREA                                         
014239     .                                                                    
014240     EJECT                                                                
014241 BC-FINAL-LINE-DATA SECTION.                                              
014242                                                                          
014243     MOVE IN2-IDARTNR     TO UT-IDARTNR                                   
014244     MOVE WS-FINAL        TO UT-STATE-DESC                                
014245     MOVE WS-FINAL-DATE   TO UT-STATE-DATE                                
014246     COMPUTE WS-STOCK-FINAL = IN2-KVAKS + IN2-KVLS + IN2-KVEFRS           
014247     MOVE WS-STOCK-FINAL  TO UT-STOCK                                     
014248     MOVE ZERO            TO UT-RETULF                                    
014249     MOVE IN2-PRINK       TO UT-PRINK                                     
014250     MOVE ZERO            TO UT-TIPRLIST                                  
014251     MOVE ZERO            TO UT-INBQTY                                    
014252     MOVE ZERO            TO UT-ORDERPR                                   
014253     MOVE ZERO            TO UT-INBDATE                                   
014254     MOVE ZERO            TO UT-DAYCUR                                    
014255*****INBOUND SEK COST TO BUY WITH DAY RATE IS CALCULATED WITH THE         
014256*****CURRENCY RATE ON THE INBOUND DATE                                    
014257*****AND SUMMED UP TO GET FINAL SEK COST                                  
014258     IF WS-WDL2-FOUND                                                     
014259      MOVE WS-STOCK-FINAL  TO WS-STOCK-REM                                
014260      PERFORM UNTIL INDX = 0 OR WS-STOCK-REM = 0                          
014266       IF WS-STOCK-REM  >= WS-TAB-KVAVIS(INDX)                            
014267        COMPUTE WS-SEK-COST = WS-SEK-COST +                               
014268          (WS-TAB-KVAVIS(INDX) * WS-TAB-PRKURS(INDX) *                    
014269           IN2-PRARTBEL-PR(1))                                            
014270        COMPUTE WS-STOCK-REM = WS-STOCK-REM - WS-TAB-KVAVIS(INDX)         
014271        COMPUTE INDX = INDX - 1                                           
014272       ELSE                                                               
014273        COMPUTE WS-SEK-COST = WS-SEK-COST +                               
014274          (WS-STOCK-REM * WS-TAB-PRKURS(INDX) *                           
014275           IN2-PRARTBEL-PR(1))                                            
014280        COMPUTE WS-STOCK-REM = WS-STOCK-REM - WS-TAB-KVAVIS(INDX)         
014281        COMPUTE INDX = INDX - 1                                           
014282       END-IF                                                             
014283      END-PERFORM                                                         
014284     ELSE                                                                 
014285      MOVE ZERO              TO WS-SEK-COST                               
014286     END-IF                                                               
014288     MOVE WS-SEK-COST        TO UT-SEKCOST                                
014289     COMPUTE WS-STOCK-VAL-FINAL = WS-STOCK-FINAL * IN1-PRINK              
014290     MOVE WS-STOCK-VAL-FINAL TO UT-STOCKVAL                               
014291     MOVE 'SEK'              TO UT-KDVALISO                               
014292     WRITE UT-POST   FROM UT-AREA                                         
014293     .                                                                    
014294     EJECT                                                                
014295 BD-INIT-WSDATA SECTION.                                                  
014296     MOVE ZERO TO WS-STOCK-START                                          
014297     MOVE ZERO TO WS-STOCK-VAL-START                                      
014298     MOVE ZERO TO WS-SEK-VAL                                              
014299     MOVE ZERO TO WS-SEK-COST                                             
014300     MOVE ZERO TO WS-STOCK-REM                                            
014302     MOVE ZERO TO WS-STOCK-FINAL                                          
014303     MOVE ZERO TO WS-STOCK-VAL-FINAL                                      
014304     .                                                                    
014305     EJECT                                                                
014306 Z-FINIT SECTION.                                                         
014307     CLOSE W51239                                                         
014308           W51239N                                                        
014309           W51237                                                         
014310     SKIP2                                                                
014311     MOVE 'S' TO POSTSUM-OPKOD                                            
014320     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014501     EJECT                                                                
014502 S01-READ-W51239  SECTION.                                                
014503     READ W51239 INTO IN1-AREA                                            
014504     AT END                                                               
014505        MOVE HIGH-VALUE TO IN1-AREA                                       
014506        SET END-OF-W51239 TO TRUE                                         
014507                                                                          
014508     NOT AT END                                                           
014509        MOVE 'W51239' TO POSTSUM-FDNAMN                                   
014510        MOVE 'W51245D1' TO POSTSUM-DDNAMN2                                
014511*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
014512        MOVE SPACE      TO POSTSUM-TRANSTYP                               
014513        CALL POSTSUM USING POSTSUM-PARM                                   
014514     END-READ                                                             
014515     .                                                                    
014516     EJECT                                                                
014517 S02-READ-W51239N SECTION.                                                
014518     READ W51239N INTO IN2-AREA                                           
014519     AT END                                                               
014520        MOVE HIGH-VALUE TO IN2-AREA                                       
014521        SET END-OF-W51239N TO TRUE                                        
014522                                                                          
014523     NOT AT END                                                           
014524        MOVE 'W51239N' TO POSTSUM-FDNAMN                                  
014525        MOVE 'W51245D2' TO POSTSUM-DDNAMN2                                
014526*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
014527        MOVE SPACE      TO POSTSUM-TRANSTYP                               
014528        CALL POSTSUM USING POSTSUM-PARM                                   
014529     END-READ                                                             
014530     .                                                                    
014531     EJECT                                                                
014532 S03-READ-W51237  SECTION.                                                
014533     READ W51237 INTO IN3-AREA                                            
014534     AT END                                                               
014536        SET END-OF-W51237 TO TRUE                                         
014539     NOT AT END                                                           
014540        MOVE 'W51237' TO POSTSUM-FDNAMN                                   
014541        MOVE 'W51245D3' TO POSTSUM-DDNAMN2                                
014542        MOVE SPACE      TO POSTSUM-TRANSTYP                               
014543        CALL POSTSUM USING POSTSUM-PARM                                   
014544     END-READ                                                             
014550     .                                                                    
014800     EJECT                                                                
014810 S04-SKRIV-DAP1 SECTION.                                                  
014820                                                                          
014830     MOVE ' ¤DAPW51245-001' TO W001-DAP                                   
014840     WRITE UT-POST   FROM W001-DAP                                        
014850                                                                          
014860     MOVE SPACE TO W001-DAP                                               
014870     .                                                                    
014880 S05-SKRIV-DAP2 SECTION.                                                  
014890                                                                          
014891     MOVE ' ¤DAPW51245'       TO W001-DAP                                 
014892     WRITE UT-POST   FROM W001-DAP                                        
014893                                                                          
014894     MOVE SPACE TO W001-DAP                                               
014895     .                                                                    
014910 S99-ABEND SECTION.                                                       
015000                                                                          
015101     SKIP2                                                                
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015200     CALL ABEND USING RKOD-ABEND                                          
015300     .                                                                    
015400     EJECT                                                                
015500* --- IMS SECTIONS  ---                                                   
015600                                                                          
015610 IMS-GU-WDL201 SECTION.                                                   
015620                                                                          
015630     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
015640          DELIMITED BY SIZE INTO SSA1                                     
015650     MOVE '  GE' TO GOOD-STATUSCODES                                      
015660     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
015670     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
015680     PERFORM IMS-STATUSCHECK                                              
015690     .                                                                    
015691     EJECT                                                                
015692 IMS-GNP-WDL211 SECTION.                                                  
015693     MOVE 'WDL211'         TO SSA1                                        
015694     MOVE '  GE'           TO GOOD-STATUSCODES                            
015695     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL211 SSA1                   
015696     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
015697     PERFORM IMS-STATUSCHECK                                              
015698     .                                                                    
015699 IMS-GNP-WDL221 SECTION.                                                  
015700                                                                          
015701     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
015702             DELIMITED BY SIZE INTO SSA1                                  
015703     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
015704             DELIMITED BY SIZE INTO SSA2                                  
015707     MOVE '  GE' TO GOOD-STATUSCODES                                      
015708     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
015709     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
015710     PERFORM IMS-STATUSCHECK                                              
015711     .                                                                    
015720     EJECT                                                                
015900 IMS-STATUSCHECK SECTION.                                                 
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GOOD-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO ERROR-TEXT                              
016600         DISPLAY ERROR-TEXT                                               
016700         CALL FELLOG                                                      
016800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
