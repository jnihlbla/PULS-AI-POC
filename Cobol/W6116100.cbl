000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6116100.                                                
000400*AUTHOR.         ROS-MARIE CLASON  -  GUIDE DATAKONSULT AB                
000500*DATE-WRITTEN.   92/08/13.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN BMP, SOM LÄSER EN FIL FRÅN HL-SYSTEMET.         
001100*        FILEN W61160D1 INNEHÅLLER UPPGIFT OM INLAGDA KOLLIN I HL.        
001200*        - OM INLEVERANS-SYSTEMETS UPPGIFTER SKILJER SIG                  
001300*          (MAP ANTAL OCH FLSATS) FRÅN HL-SYSTEMET, SKALL EN              
001400*          FIL W61161D2 SKICKAS TILLBAKA TIL HL-SYSTEMET.                 
001410*        - OM DET VISAR SIG DET ÄR ETT DIV-KOLLI SKICKAS UPPG.            
001500*          TILL MEMO (VCC.DECFEL  ) VIA MEMOAPI.                          
001501*        - OM DET FINNS ICKE PLATSSATTA ARTIKLAR ELLER ARTIKLAR           
001502*          MED KVALITETSFEL SÅ SKRIVS DESSA UT PÅ FIL                     
001510*                                                                         
001520*                                                                         
001600*        PROGRAMMET LÄSER/UPD  WLARTD (WDD8)                              
001700*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- HL FIL                                                     
003200     SELECT W61160                     ASSIGN TO W61161D1.                
003300     EJECT                                                                
003400     SELECT W61161                     ASSIGN TO W61161D2.                
003500     EJECT                                                                
003501     SELECT W61162                     ASSIGN TO W61161D3.                
003502     EJECT                                                                
003510     SELECT MEMOFIL                    ASSIGN TO W61161D4.                
003520     EJECT                                                                
003530     SELECT W61163                     ASSIGN TO W61161D5.                
003540     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W61160                                                               
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS   0.                                                  
004400                                                                          
004500 01  INPOST.                                                              
004600     03  IN-RAD                    PIC X(80).                             
005600     EJECT                                                                
005700                                                                          
005800 FD  W61161                                                               
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
007401 01  UTPOST.                                                              
007402     03  UT-RAD                    PIC X(80).                             
007403     EJECT                                                                
007404                                                                          
007405 FD  W61162                                                               
007406     RECORDING       F                                                    
007407     BLOCK CONTAINS  0.                                                   
007409                                                                          
007410 01  LISTPOST.                                                            
007411     03  LIST-RAD                  PIC X(80).                             
007412     EJECT                                                                
007413                                                                          
007414 FD  MEMOFIL                                                              
007420     RECORDING       F                                                    
007430     BLOCK CONTAINS  0.                                                   
007440                                                                          
007460 01  MEMO-POST                 PIC X(80).                                 
007470     EJECT                                                                
007480                                                                          
007490 FD  W61163                                                               
007491     RECORDING       F                                                    
007492     BLOCK CONTAINS  0.                                                   
007493                                                                          
007494 01  UT3-POST.                                                            
007495     03  UT3-RAD                  PIC X(80).                              
007496     EJECT                                                                
007497                                                                          
007500 WORKING-STORAGE SECTION.                                                 
007600     SKIP2                                                                
007601                                                                          
007610*    -- CHECKED BY WY2000                                                 
007700 77  IDPGM                       PIC X(8)    VALUE 'W6116100'.            
007800 77  JA                          PIC X       VALUE 'J'.                   
007900 77  NEJ                         PIC X       VALUE 'N'.                   
008000 77  W-KVINLART-TOT              PIC S9(8)   VALUE ZERO COMP-3.           
008010 77  W-UTPOST-RAKN               PIC S9(2)   VALUE ZERO COMP-3.           
008011 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008020 77  MEMO-SEND-REQ               PIC X(5)    VALUE ')SEND'.               
008030 77  MEMO-SEND-TITLE             PIC X(6)    VALUE 'TITLE '.              
008040 77  MEMO-SEND-FORCE             PIC X(12)   VALUE 'OPTION FORCE'.        
008050 77  MEMO-SEND-DEST              PIC X(5)    VALUE 'DEST '.               
008060 77  MEMO-SEND-LINESIZE          PIC X(9)    VALUE 'LINESIZE '.           
008070 77  MEMO-SEND-MEMO              PIC X(4)    VALUE 'MEMO'.                
008080 77  MEMO-SEND-REQ-END           PIC X(4)    VALUE ')END'.                
008100     SKIP2                                                                
008200                                                                          
008300*     --- ARBETSFÄLT FÖR SWITCHAR ---                                     
008400                                                                          
008500                                                                          
008600 77  BEARB-SW                    PIC X       VALUE ' '.                   
008700     88  BEARB1                              VALUE '1'.                   
008710     88  BEARB2                              VALUE '2'.                   
008900                                                                          
008910 77  FOERSTA-T91-SW              PIC X       VALUE 'N'.                   
008920     88  FOERSTA-T91-JA                      VALUE 'J'.                   
008930     88  FOERSTA-T91-NEJ                     VALUE 'N'.                   
008940                                                                          
009000 77  T91-SW                      PIC X       VALUE 'N'.                   
009100     88  T91-JA                              VALUE 'J'.                   
009200     88  T91-NEJ                             VALUE 'N'.                   
009210                                                                          
009220 77  LIST-SW                     PIC X       VALUE 'N'.                   
009230     88  SKAPA-LISTA                         VALUE 'J'.                   
009300                                                                          
009800 77  RAPP-OK-SW                  PIC X       VALUE 'N'.                   
009900     88  RAPP-OK-JA                          VALUE 'J'.                   
010000     88  RAPP-OK-NEJ                         VALUE 'N'.                   
010100                                                                          
010110 77  START-OK-SW                 PIC X       VALUE 'N'.                   
010120     88  START-OK                            VALUE 'J'.                   
010140                                                                          
010200 77  W61160-EOF-SW               PIC X       VALUE 'N'.                   
010300     88  END-OF-W61160                       VALUE 'J'.                   
010400     EJECT                                                                
010500                                                                          
010501*  SPAR-AREA FÖR W6D121                                                   
010520 01  FILLER                   PIC X(16)   VALUE 'SPAR-AREA-RAD'.          
010530*01  -COPY W6D121    -PRE SPAR-                                           
010540     EJECT                                                                
010550*  SPAR- OCH ARBETSAREOR                                                  
010600 01  SPAR-AREA.                                                           
010710   03  W-RETURKOD                PIC 9(2)     VALUE ZERO.                 
010720   03  W-FLSATS                  PIC X(1).                                
010800   03  SPAR-ANTAL                PIC S9(4)    COMP-3  VALUE ZERO.         
010801   03  SPAR-IDLOPNRM             PIC S9(9)    COMP-3  VALUE ZERO.         
010810   03  SPAR-PRARTSTD             PIC S9(7)V99 COMP-3  VALUE ZERO.         
010820   03  SPAR-IDARTNR              PIC S9(9)    COMP-3  VALUE ZERO.         
010830   03  SPAR-ADGANG               PIC S9(3)    COMP-3  VALUE ZERO.         
010840   03  SPAR-ADLAGOMR             PIC S9(3)    COMP-3  VALUE ZERO.         
010850   03  SPAR-ADPLATS              PIC S9(5)    COMP-3  VALUE ZERO.         
010860   03  SPAR-FLKVAFEL             PIC X(1).                                
011000                                                                          
011005                                                                          
011010   03  W-W61160.                                                          
011020       05 IN-START-POST.                                                  
011041           07 IN-START               PIC X(09).                           
011042           07 FILLER                 PIC X(71).                           
011050       05 IN-RAD-POST REDEFINES IN-START-POST.                            
011060           07 FILLER                 PIC X(1).                            
011061           07 IN-IDARTNR             PIC 9(9).                            
011062           07 FILLER                 PIC X(1).                            
011070           07 IN-IDLEVNR-KOLLI       PIC X(5).                            
011071           07 FILLER                 PIC X(1).                            
011080           07 IN-IDOKOLLI            PIC 9(9).                            
011081           07 FILLER                 PIC X(1).                            
011090           07 IN-KVINLART-TOT        PIC 9(7).                            
011091           07 FILLER                 PIC X(1).                            
011092           07 IN-FLSATS              PIC X(1).                            
011093           07 FILLER                 PIC X(44).                           
011094       05 IN-SLUT-POST REDEFINES IN-RAD-POST.                             
011095           07 IN-SLUT                PIC X(05).                           
011096           07 IN-ANTAL               PIC 9(03).                           
011097           07 FILLER                 PIC X(72).                           
011098                                                                          
011099   03  W-W61161.                                                          
011100       05 UT-START-POST.                                                  
011101           07 UT-START               PIC X(09).                           
011102           07 UT-BLANK               PIC X(72) VALUE SPACE.               
011103       05 UT-RAD-POST REDEFINES UT-START-POST.                            
011104           07 STAR-1                 PIC X(1).                            
011105           07 UT-IDARTNR             PIC 9(9).                            
011106           07 STAR-2                 PIC X(1).                            
011108           07 UT-IDLEVNR-KOLLI       PIC X(5).                            
011109           07 STAR-3                 PIC X(1).                            
011111           07 UT-IDOKOLLI            PIC 9(9).                            
011112           07 STAR-4                 PIC X(1).                            
011114           07 UT-KVINLART-TOT        PIC 9(7).                            
011115           07 STAR-5                 PIC X(1).                            
011117           07 UT-FLSATS              PIC X(1).                            
011118           07 STAR-6                 PIC X(44).                           
011120       05 UT-SLUT-POST REDEFINES UT-RAD-POST.                             
011121           07 UT-SLUT                PIC X(05).                           
011122           07 UT-ANTAL               PIC 9(03).                           
011123           07 UT-STAR                PIC X(1).                            
011124           07 FILLER                 PIC X(71).                           
011125*                                                                         
011126*  CTEXT TILL FIL W61162                                                  
011128*01  -COPY W6116201  -PRE UT2-                                            
011129     EJECT                                                                
011130*  CTEXT TILL FIL W61163                                                  
011131*01  -COPY W6116301  -PRE UT3-                                            
011132     EJECT                                                                
011133*                                                                         
011134*MEMO*                                                                    
011135 01  MEMO-AREA                   PIC X(80).                               
011136 01  MEMO-NAMN                   PIC X(14) VALUE '*R32*/DIVKOLLI'.        
011139 01  MAIL-SENDER-ID      PIC X(22) VALUE 'SSAMUEL2@VOLVOCARS.COM'.        
011140 01  LINE-LENGTH-X               PIC X(3)  VALUE '080'.                   
011160                                                                          
011400 01  FELTEXT.                                                             
011500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011700                                                                          
013400                                                                          
013500*INDEX                                                                    
013600 01  FILLER                      PIC X(16)   VALUE 'INDEX '.              
013700 01  T91-IX                      PIC S9(4)   VALUE +0 COMP SYNC.          
013800     EJECT                                                                
013810*      --- VALID IDDC CODES                                               
013820*                                                                         
013830*01    -COPY WWDCKONS                                                     
013840       EJECT                                                              
013900                                                                          
014000 01  DYNAMISKA-SUBPROGRAM.                                                
014100*                                                                         
014200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014410     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
014420     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
014700     EJECT                                                                
014710 01  FILLER                      PIC X(16)   VALUE 'W611PMRK'.            
014800     SKIP3                                                                
014810*    -COPY W611PMRK                                                       
014820     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015000 01  NYCKLAR-TILL-DLI.                                                    
015100*INLA                                                                     
015110*C-INDEX NYCKLAR                                                          
015200     03  W-W6D1C1KY-X.                                                    
015300         05  WC-IDLEVNR           PIC  X(5)   VALUE SPACE.                
015400         05  WC-IDOKOLLI          PIC 9(9).                               
015800                                                                          
015801*C-INDEX SÖKNYCKLAR                                                       
015802     03  WCS-IDLEVNR-X.                                                   
015803         05  WCS-IDLEVNR          PIC  X(5)   VALUE SPACE.                
015804                                                                          
015805     03  WCS-IDOKOLLI-X.                                                  
015806         05  WCS-IDOKOLLI         PIC 9(9).                               
015807                                                                          
015808     03  W-IDDC                   PIC X(2)    VALUE '11'.                 
016300     EJECT                                                                
016400                                                                          
016500*    --- STATUS-KOD FRÅN IMS                                              
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-FINNS                       VALUE '  '.                  
016800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017100     88  IMS-EJ-OK                           VALUE 'XD'.                  
017200     SKIP2                                                                
017300                                                                          
017400 01  GODK-STATUSKODER.                                                    
017500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017600     SKIP3                                                                
017700 01  SSA1                        PIC X(128).                              
017800 01  SSA2                        PIC X(64).                               
017900 01  SSA3                        PIC X(64).                               
018000     EJECT                                                                
018100                                                                          
018200*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
018300*    --- COPYTEXTER FÖR  W60191, W60193                                   
018400                                                                          
018410 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
018420     SKIP3                                                                
018430 01  KOM-MSG-IO-AREA.                                                     
018431*03  -COPY WMSGKOM                                                        
018440     EJECT                                                                
018500 01  FILLER                    PIC X(16)   VALUE 'P-TO-P-AREA'.           
018600     SKIP3                                                                
018700*01  -COPY WMSGSNUF            -PRE P-TO-P-                               
018800     EJECT                                                                
018900 01  FILLER                PIC X(16)   VALUE 'T91-MID-W6I19101'.          
019000     SKIP3                                                                
019500     -COPY W6I19101        -PRE T91-                                      
019600     EJECT                                                                
019610 01  FILLER                PIC X(16)   VALUE 'T93-MID-W6I19301'.          
019620     SKIP3                                                                
019630     -COPY W6I19301        -PRE T93-                                      
019640     EJECT                                                                
020800                                                                          
021400*    --- IMS FUNKTIONSKODER                                               
021500*01  -COPY W0003                                                          
021600     EJECT                                                                
021700                                                                          
021800*    ---  DLI INPUT-OUTPUT AREA                                           
021900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
022000     SKIP3                                                                
023100                                                                          
023110 01  DLI-IO-AREA.                                                         
023120     03  IO-AREA               PIC X(150)  VALUE SPACE.                   
023121                                                                          
023130     SKIP3                                                                
023200     03  W6INLA01 REDEFINES IO-AREA.                                      
023300*        05  -COPY W6D101                                                 
023400     EJECT                                                                
023500     03  W6INLA11 REDEFINES IO-AREA.                                      
023600*        05  -COPY W6D111                                                 
023700     EJECT                                                                
023800     03  W6INLA21 REDEFINES IO-AREA.                                      
023900*        05  -COPY W6D121                                                 
024000     EJECT                                                                
024100                                                                          
024200 LINKAGE SECTION.                                                         
024300                                                                          
024400*01  -COPY W0009   -PRE MSG-                                              
024500     EJECT                                                                
024504                                                                          
024510*01  -COPY W0009   -PRE ALT1-                                             
024520     EJECT                                                                
024530                                                                          
024540*01  -COPY W0009   -PRE DISP-                                             
024550     EJECT                                                                
024900*01  -COPY W0008   -PRE INLC-                                             
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025110                                                                          
025111*****************************************************************         
025112*    PCB-ER FÖR SUBPROGRAM                                      *         
025113*****************************************************************         
025114                                                                          
025120 01  PMRK-INLB-PCB               PIC X.                                   
025121     SKIP3                                                                
025122 01  PMRK-INLC-PCB               PIC X.                                   
025123     SKIP3                                                                
025124 01  PMRK-PLAA-PCB               PIC X.                                   
025125     SKIP3                                                                
025126 01  KOM-KOMA-PCB                PIC X.                                   
025300     EJECT                                                                
025400 PROCEDURE DIVISION  USING MSG-PCB                                        
025501                           ALT1-PCB                                       
025510                           DISP-PCB                                       
025511                           INLC-PCB                                       
025512                           PMRK-INLB-PCB                                  
025513                           PMRK-INLC-PCB                                  
025514                           PMRK-PLAA-PCB                                  
025520                           KOM-KOMA-PCB.                                  
025800                                                                          
025900     ENTRY 'DLITCBL' USING MSG-PCB                                        
026000                           ALT1-PCB                                       
026010                           DISP-PCB                                       
026011                           INLC-PCB                                       
026012                           PMRK-INLB-PCB                                  
026013                           PMRK-INLC-PCB                                  
026014                           PMRK-PLAA-PCB                                  
026020                           KOM-KOMA-PCB.                                  
026300                                                                          
026310     MOVE ZERO TO W-RETURKOD                                              
026400     PERFORM A-INIT                                                       
026410     IF START-OK                                                          
026500        PERFORM B-BEARB                                                   
026510     END-IF                                                               
026700     PERFORM Z-FINIT                                                      
026710     MOVE W-RETURKOD TO RETURN-CODE                                       
026800                                                                          
027000     GOBACK                                                               
027100     .                                                                    
027200     EJECT                                                                
027400 A-INIT SECTION.                                                          
027500     SKIP2                                                                
027600                                                                          
027700     OPEN INPUT W61160                                                    
027800     OPEN OUTPUT W61161                                                   
027801                 W61162                                                   
027802                 W61163                                                   
027810                 MEMOFIL                                                  
027820                                                                          
028310     PERFORM S01-LAES-W61160                                              
028400*----KONTROLLERA START-POST                                               
028500                                                                          
028510     IF IN-START = '*R32RAPP*'                                            
028520        MOVE JA             TO START-OK-SW                                
028530        PERFORM S10-INIT-MEMOFIL                                          
028540*-------SKAPA START-POST TILL HL                                          
028550        MOVE '*R11KORR*'         TO UT-START                              
028551        PERFORM S02-WRITE-W61161                                          
028560     END-IF                                                               
028570     MOVE ZERO              TO SPAR-ANTAL                                 
028580     ACCEPT DAGENS-DATUM FROM DATE                                        
028900     .                                                                    
029000     EJECT                                                                
029200 B-BEARB                SECTION.                                          
029300                                                                          
029400     PERFORM S01-LAES-W61160                                              
029500     PERFORM UNTIL END-OF-W61160                                          
029600*-------KONTROLLERA SLUT-POST                                             
029610*                   SAMT DESS ANTAL                                       
029700        IF IN-SLUT = '*032*' OR '*R32*'                                   
029800           CONTINUE                                                       
029900        ELSE                                                              
030000           PERFORM BA-KONTR-RAD                                           
030200        END-IF                                                            
030210        PERFORM S01-LAES-W61160                                           
030300     END-PERFORM                                                          
030400     IF T91-JA                                                            
030500        PERFORM S50-SKICKA-W60191                                         
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
031000 BA-KONTR-RAD               SECTION.                                      
031100                                                                          
031110     MOVE NEJ                     TO LIST-SW                              
031200     MOVE IN-IDLEVNR-KOLLI        TO WC-IDLEVNR                           
031210                                     WCS-IDLEVNR                          
031300     MOVE IN-IDOKOLLI             TO WC-IDOKOLLI                          
031310                                     WCS-IDOKOLLI                         
031400     PERFORM IMS-GU-INLC-D111                                             
031500     IF SEGMENT-FINNS                                                     
031600        MOVE ART-IDLOPNRM         TO SPAR-IDLOPNRM                        
031610        MOVE ART-PRARTSTD         TO SPAR-PRARTSTD                        
031620        IF (ART-ADLAGOMR = ZERO AND ART-ADGANG = ZERO AND                 
031630           ART-ADPLATS = ZERO)         OR                                 
031631           (ART-ADLAGOMR = +59  AND ART-ADGANG = ZERO AND                 
031632           ART-ADPLATS = +9700)        OR                                 
031633           (ART-ADLAGOMR = ZERO AND ART-ADGANG = ZERO AND                 
031634           ART-ADPLATS = +9998)        OR                                 
031635           (ART-ADLAGOMR = ZERO AND ART-ADGANG = ZERO AND                 
031636           ART-ADPLATS = +9999)        OR                                 
031637           (ART-ADLAGOMR = +21  AND ART-ADGANG = ZERO AND                 
031638           ART-ADPLATS = +0001)        OR                                 
031639           (ART-ADLAGOMR = +30  AND ART-ADGANG = ZERO AND                 
031640           ART-ADPLATS = +7852)        OR                                 
031641           ART-FLKVAFEL = JA           OR                                 
031642           ART-FLKVAKAR = JA                                              
031650                                                                          
031660           MOVE JA                     TO LIST-SW                         
031661           MOVE ART-IDARTNR            TO SPAR-IDARTNR                    
031662           MOVE ART-ADLAGOMR           TO SPAR-ADLAGOMR                   
031663           MOVE ART-ADGANG             TO SPAR-ADGANG                     
031664           MOVE ART-ADPLATS            TO SPAR-ADGANG                     
031665           IF ART-FLKVAFEL = JA                                           
031666             MOVE ART-FLKVAFEL         TO SPAR-FLKVAFEL                   
031667           ELSE                                                           
031668             MOVE ART-FLKVAKAR         TO SPAR-FLKVAFEL                   
031669           END-IF                                                         
031670        END-IF                                                            
031700        PERFORM BAA-LAES-INLA-RADER                                       
031710        IF BEARB1                                                         
031800           PERFORM BAB-KONTR-ANTAL-SATS                                   
031900        END-IF                                                            
031901        IF BEARB2                                                         
031902*----------SKRIV FELMEDDELANDE PÅ MEMO                                    
031904           PERFORM BAC-SKRIV-MEMO                                         
031905        END-IF                                                            
031906     ELSE                                                                 
031908         MOVE IN-IDARTNR  TO UT3-IDARTNR                                  
031909         MOVE IN-KVINLART-TOT TO UT3-KVINLART                             
031910         MOVE IN-IDLEVNR-KOLLI  TO UT3-IDLEVNR                            
031911         MOVE IN-IDOKOLLI TO UT3-IDOKOLLI                                 
031912         MOVE NEJ         TO UT3-FLRAPP                                   
031913         PERFORM S05-WRITE-W61163                                         
031920     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032300 BAA-LAES-INLA-RADER    SECTION.                                          
032400                                                                          
032600     MOVE ZERO                    TO W-KVINLART-TOT                       
032800     MOVE SPACE                   TO BEARB-SW                             
032900                                                                          
033000     PERFORM IMS-GHNP-INLC-D121                                           
033200                                                                          
033300     PERFORM UNTIL SEGMENT-SAKNAS                                         
033500        IF RAD-KDINLSTA = 'SAK' OR 'FPK' OR SPACE                         
033501           MOVE IN-IDARTNR  TO UT3-IDARTNR                                
033502           MOVE IN-KVINLART-TOT TO UT3-KVINLART                           
033503           MOVE IN-IDLEVNR-KOLLI  TO UT3-IDLEVNR                          
033504           MOVE IN-IDOKOLLI TO UT3-IDOKOLLI                               
033505           MOVE JA          TO UT3-FLRAPP                                 
033506           PERFORM S05-WRITE-W61163                                       
033510           IF SKAPA-LISTA AND RAD-FLSATS = NEJ                            
033511             PERFORM S04-SKAPA-LISTPOST                                   
033522             MOVE 'HL  '            TO RAD-ADINLOMR                       
033523             MOVE SPACE             TO RAD-ADINLOMR-NXT                   
033524             PERFORM IMS-REPL-INLC                                        
033530           ELSE                                                           
033600             IF RAD-FLDIVKLI = NEJ                                        
033610                IF SKAPA-LISTA                                            
033611                  PERFORM S04-SKAPA-LISTPOST                              
033620                END-IF                                                    
033700                MOVE 1                 TO BEARB-SW                        
034000                MOVE RAD-W6D121        TO SPAR-RAD-W6D121                 
034100                ADD RAD-KVINLART       TO W-KVINLART-TOT                  
034110                IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')              
034120                   PERFORM S07-CALL-W611PMRK                              
034130                   PERFORM S08-LAES-IN-IGEN                               
034140                END-IF                                                    
034150                                                                          
034200                MOVE 'INL'             TO RAD-KDINLSTA                    
034201                MOVE SPACE             TO RAD-ADINLOMR                    
034202                                          RAD-ADINLOMR-NXT                
034210                MOVE ZERO              TO RAD-IDILIST                     
034220                                          RAD-IDILIRAD                    
034221                                          RAD-IDINLVGN                    
034230                MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                    
034300                PERFORM IMS-REPL-INLC                                     
034400                MOVE JA                TO T91-SW                          
034500                IF T91-MID-KVPOST = 24                                    
034600                   PERFORM S50-SKICKA-W60191                              
034700                   MOVE ZERO  TO T91-MID-KVPOST                           
034800                END-IF                                                    
034900                PERFORM S40-TRANS-W60191                                  
035000                PERFORM S51-SKICKA-W60193                                 
035300             ELSE                                                         
035400                IF RAD-FLDIVKLI = JA                                      
035410                   MOVE 2              TO BEARB-SW                        
035800                END-IF                                                    
035900             END-IF                                                       
035910           END-IF                                                         
036000        ELSE                                                              
036120           MOVE IN-IDARTNR  TO UT3-IDARTNR                                
036130           MOVE IN-KVINLART-TOT TO UT3-KVINLART                           
036140           MOVE IN-IDLEVNR-KOLLI TO UT3-IDLEVNR                           
036150           MOVE IN-IDOKOLLI TO UT3-IDOKOLLI                               
036160           MOVE NEJ         TO UT3-FLRAPP                                 
036170           PERFORM S05-WRITE-W61163                                       
036500        END-IF                                                            
036600                                                                          
036700        PERFORM IMS-GHNP-INLC-D121                                        
036800     END-PERFORM                                                          
037500     .                                                                    
037600     EJECT                                                                
037800 BAB-KONTR-ANTAL-SATS   SECTION.                                          
037900                                                                          
037941     IF IN-FLSATS = 'F'                                                   
037942        MOVE NEJ          TO W-FLSATS                                     
037943     ELSE                                                                 
037944        MOVE JA           TO W-FLSATS                                     
037945     END-IF                                                               
037950     MOVE NEJ                  TO RAPP-OK-SW                              
038000     IF W-KVINLART-TOT = IN-KVINLART-TOT AND                              
038500        SPAR-RAD-FLSATS = W-FLSATS                                        
038600*-------OK                                                                
038700        MOVE JA                TO RAPP-OK-SW                              
038800     END-IF                                                               
038900                                                                          
039000     IF RAPP-OK-NEJ                                                       
039100*-------FEL - OLIKA ANTAL ELLER SATS                                      
039200*-------SKAPA POST TILL HL                                                
039300*-------SKICKA INLA'S UPPGIFTER                                           
039400        PERFORM BABA-SKAPA-HL-POST                                        
039500     END-IF                                                               
039601     .                                                                    
039602     EJECT                                                                
039620 BABA-SKAPA-HL-POST     SECTION.                                          
039630                                                                          
039634*----FEL - OLIKA ANTAL ELLER SATS                                         
039635*----SKAPA POST TILL HL                                                   
039637                                                                          
039646     MOVE IN-IDARTNR          TO UT-IDARTNR                               
039647     MOVE IN-IDLEVNR-KOLLI    TO UT-IDLEVNR-KOLLI                         
039648     MOVE IN-IDOKOLLI         TO UT-IDOKOLLI                              
039649     MOVE W-KVINLART-TOT      TO UT-KVINLART-TOT                          
039650     IF SPAR-RAD-FLSATS = NEJ                                             
039652        MOVE 'F'              TO UT-FLSATS                                
039653     ELSE                                                                 
039654        MOVE 'O'              TO UT-FLSATS                                
039656     END-IF                                                               
039657     MOVE '*'                 TO STAR-1                                   
039658                                 STAR-2                                   
039659                                 STAR-3                                   
039660                                 STAR-4                                   
039661                                 STAR-5                                   
039662                                 STAR-6                                   
039664     ADD 1                    TO W-UTPOST-RAKN                            
039665                                                                          
039670     PERFORM S02-WRITE-W61161                                             
039810     .                                                                    
039820     EJECT                                                                
039899 BAC-SKRIV-MEMO        SECTION.                                           
039900                                                                          
040014     MOVE W-W61160        TO MEMO-AREA                                    
040015     WRITE MEMO-POST    FROM MEMO-AREA                                    
040016     MOVE  1   TO W-RETURKOD                                              
040107     .                                                                    
040108     EJECT                                                                
040109*----------------------------------------------------------------*        
040110 Z-FINIT SECTION.                                                         
040200                                                                          
040230*-------SKAPA SLUT-POST TILL HL                                           
040240     MOVE SPACE                        TO UT-SLUT-POST                    
040241     MOVE '*011*'                      TO UT-SLUT                         
040242     MOVE W-UTPOST-RAKN                TO UT-ANTAL                        
040243     MOVE '*'                          TO UT-STAR                         
040250     PERFORM S02-WRITE-W61161                                             
040295                                                                          
040297     MOVE MEMO-SEND-REQ-END TO MEMO-AREA                                  
040298     WRITE MEMO-POST FROM MEMO-AREA                                       
040303                                                                          
040310     CLOSE W61160                                                         
040400           W61161                                                         
040410           W61162                                                         
040420           W61163                                                         
040500           MEMOFIL                                                        
040800     .                                                                    
040900     EJECT                                                                
041000 S01-LAES-W61160     SECTION.                                             
041100                                                                          
041200     READ W61160    INTO W-W61160                                         
041300     AT END                                                               
041400        SET END-OF-W61160      TO TRUE                                    
041500     END-READ                                                             
041600     .                                                                    
041700     SKIP3                                                                
041900 S02-WRITE-W61161      SECTION.                                           
042000                                                                          
042100     WRITE UTPOST    FROM W-W61161                                        
042200                                                                          
042300     .                                                                    
042400     EJECT                                                                
042410 S03-WRITE-W61162      SECTION.                                           
042420                                                                          
042430     WRITE LISTPOST  FROM UT2-W6116201                                    
042440                                                                          
042450     .                                                                    
042460     EJECT                                                                
042470 S04-SKAPA-LISTPOST    SECTION.                                           
042471                                                                          
042472     MOVE RAD-IDOKOLLI         TO UT2-IDOKOLLI                            
042473     MOVE RAD-IDLEVNR-KOLLI    TO UT2-IDLEVNR-KOLLI                       
042474     MOVE SPAR-IDLOPNRM        TO UT2-IDLOPNRM                            
042475     MOVE SPAR-IDARTNR         TO UT2-IDARTNR                             
042476     MOVE RAD-KVINLART         TO UT2-KVINLART                            
042477     MOVE SPAR-ADGANG          TO UT2-ADGANG                              
042478     MOVE SPAR-ADLAGOMR        TO UT2-ADLAGOMR                            
042479     MOVE SPAR-ADLAGOMR        TO UT2-ADLAGOMR                            
042480     MOVE SPAR-FLKVAFEL        TO UT2-FLKVAFEL                            
042481     PERFORM S03-WRITE-W61162                                             
042482     .                                                                    
042490     EJECT                                                                
042491 S05-WRITE-W61163      SECTION.                                           
042492                                                                          
042493     WRITE UT3-POST  FROM UT3-W6116301                                    
042494                                                                          
042495     .                                                                    
042496     EJECT                                                                
042500 S07-CALL-W611PMRK     SECTION.                                           
042510                                                                          
042520     MOVE RAD-IDLEVNR-KOLLI     TO PMRK-IDLEVNR                           
042530     MOVE RAD-IDOKOLLI          TO PMRK-IDOKOLLI                          
042540     MOVE ZERO                  TO PMRK-IDLOPNRM                          
042550                                   PMRK-IDRADNR                           
042560     CALL W611PMRK USING  PMRK-W611PMRK  PMRK-INLB-PCB                    
042570                          PMRK-INLC-PCB  PMRK-PLAA-PCB                    
042580     .                                                                    
042590     SKIP3                                                                
042591 S08-LAES-IN-IGEN      SECTION.                                           
042592                                                                          
042593     PERFORM  IMS-GU-INLC-D111                                            
042594     PERFORM  IMS-GHNP-INLC-D121                                          
042595                                                                          
042596     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
042597          RAD-IDRADNR  = SPAR-RAD-IDRADNR                                 
042598        PERFORM IMS-GHNP-INLC-D121                                        
042599     END-PERFORM                                                          
042600     .                                                                    
042601     EJECT                                                                
042610 S10-INIT-MEMOFIL      SECTION.                                           
042700                                                                          
042800     MOVE MEMO-SEND-REQ   TO MEMO-AREA                                    
042900     WRITE MEMO-POST    FROM MEMO-AREA                                    
043000                                                                          
043100     STRING MEMO-SEND-TITLE MEMO-NAMN                                     
043200            DELIMITED BY SIZE                                             
043300            INTO MEMO-AREA                                                
043400     WRITE MEMO-POST    FROM MEMO-AREA                                    
043500                                                                          
043600     MOVE MEMO-SEND-FORCE TO MEMO-AREA                                    
043700     WRITE MEMO-POST    FROM MEMO-AREA                                    
043800                                                                          
043900     STRING MEMO-SEND-DEST DELIMITED BY SIZE                              
044000            MAIL-SENDER-ID                                                
044100            DELIMITED BY SPACE                                            
044200            INTO MEMO-AREA                                                
044300     WRITE MEMO-POST    FROM MEMO-AREA                                    
044400                                                                          
044500     MOVE SPACE TO MEMO-AREA                                              
044600     STRING MEMO-SEND-LINESIZE LINE-LENGTH-X                              
044700            DELIMITED BY SIZE                                             
044800            INTO MEMO-AREA                                                
044900     WRITE MEMO-POST    FROM MEMO-AREA                                    
045000                                                                          
045100     MOVE MEMO-SEND-MEMO  TO MEMO-AREA                                    
045200     WRITE MEMO-POST    FROM MEMO-AREA                                    
045300                                                                          
045400     MOVE SPACE TO MEMO-AREA                                              
045500     WRITE MEMO-POST    FROM MEMO-AREA                                    
045600                                                                          
045700     MOVE SPACE TO MEMO-AREA                                              
045800     STRING '*R32-RAPP* DIVERSEKOLLIN                 '                   
045900            DELIMITED BY SIZE                                             
046000            INTO MEMO-AREA                                                
046100     WRITE MEMO-POST    FROM MEMO-AREA                                    
046200                                                                          
046300     MOVE SPACE TO MEMO-AREA                                              
046400     WRITE MEMO-POST    FROM MEMO-AREA                                    
046500                                                                          
046600     MOVE SPACE TO MEMO-AREA                                              
046700     STRING ' ARTNR     LEVNR KOLLINR   TOT-ANT F/OF'                     
046800            DELIMITED BY SIZE                                             
046900            INTO MEMO-AREA                                                
047000     WRITE MEMO-POST    FROM MEMO-AREA                                    
047100                                                                          
047200     MOVE SPACE TO MEMO-AREA                                              
047300     STRING '----------------------------------------'                    
047400            DELIMITED BY SIZE                                             
047500            INTO MEMO-AREA                                                
047600     WRITE MEMO-POST    FROM MEMO-AREA                                    
047700                                                                          
047800     .                                                                    
047900     EJECT                                                                
048100 S40-TRANS-W60191      SECTION.                                           
048200                                                                          
048300     IF T91-MID-KVPOST = ZERO                                             
048400        MOVE JA                  TO T91-SW                                
048500        MOVE SPACE               TO T91-MID-W6I19101                      
048510                                                                          
048600        MOVE +1                  TO T91-MID-KVPOST                        
048800        MOVE 'W6116100'          TO T91-MID-IDPGM                         
048810        MOVE WC-CDC-SE           TO T91-MID-IDDC                          
048900                                                                          
049000        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(1)                 
049100        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(1)                 
049200        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(1)                  
049300        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(1)                
049310        MOVE +0                    TO T91-MID-KVKOLLI  (1)                
049320        MOVE 'N'                   TO T91-MID-FLINLI   (1)                
049400        MOVE SPAR-RAD-ADINLOMR     TO T91-MID-ADINLOMR-OLD(1)             
049500        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(1)             
049600        MOVE SPAR-RAD-ADINLOMR-NXT TO T91-MID-ADINLOMR-NXT-OLD(1)         
049700        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-NEW(1)         
049800        MOVE SPAR-RAD-KVINLART     TO T91-MID-KVINLART-OLD(1)             
049900        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(1)             
050000        MOVE SPAR-RAD-KDINLSTA     TO T91-MID-KDINLSTA-OLD(1)             
050100        MOVE RAD-KDINLSTA          TO T91-MID-KDINLSTA-NEW(1)             
050200        MOVE +1                    TO T91-IX                              
050300     ELSE                                                                 
050400        COMPUTE T91-IX = T91-MID-KVPOST + 1                               
050500        ADD 1                         TO T91-MID-KVPOST                   
050510        MOVE 'W6116100'               TO T91-MID-IDPGM                    
050520        MOVE WC-CDC-SE                TO T91-MID-IDDC                     
050600                                                                          
050700        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(T91-IX)            
050800        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(T91-IX)            
050900        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(T91-IX)             
051000        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(T91-IX)           
051010        MOVE +0                    TO T91-MID-KVKOLLI  (T91-IX)           
051020        MOVE 'N'                   TO T91-MID-FLINLI   (T91-IX)           
051100        MOVE SPAR-RAD-ADINLOMR     TO T91-MID-ADINLOMR-OLD(T91-IX)        
051200        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(T91-IX)        
051300        MOVE SPAR-RAD-ADINLOMR-NXT                                        
051400             TO T91-MID-ADINLOMR-NXT-OLD(T91-IX)                          
051500        MOVE RAD-ADINLOMR-NXT                                             
051600             TO T91-MID-ADINLOMR-NXT-NEW(T91-IX)                          
051700        MOVE SPAR-RAD-KVINLART     TO T91-MID-KVINLART-OLD(T91-IX)        
051800        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(T91-IX)        
051900        MOVE SPAR-RAD-KDINLSTA     TO T91-MID-KDINLSTA-OLD(T91-IX)        
052000        MOVE RAD-KDINLSTA          TO T91-MID-KDINLSTA-NEW(T91-IX)        
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052500 S50-SKICKA-W60191 SECTION.                                               
052600                                                                          
052800     COMPUTE P-TO-P-MSG-KVLL = (T91-MID-KVPOST * 64) + 35                 
052900     MOVE 1                  TO P-TO-P-MSG-KDMFSFOR                       
053000                                                                          
053010     MOVE 'W6T191X '         TO P-TO-P-MSG-KDTRANS                        
053020     MOVE '6161'             TO P-TO-P-MSG-IDTRANS                        
053030     MOVE T91-MID-W6I19101   TO P-TO-P-MSG-INDATA                         
053040                                                                          
053100     IF FOERSTA-T91-JA                                                    
053200        PERFORM IMS-ISRT-MSG-ALT1-6191                                    
053300        MOVE NEJ     TO FOERSTA-T91-SW                                    
053400     ELSE                                                                 
053500        PERFORM IMS-PURG-MSG-ALT1-6191                                    
053600     END-IF                                                               
053610     MOVE NEJ                 TO T91-SW                                   
053700     .                                                                    
053800     EJECT                                                                
054000 S51-SKICKA-W60193 SECTION.                                               
054100                                                                          
054110     MOVE SPAR-IDLOPNRM        TO T93-MID-IDLOPNRM                        
054120     MOVE RAD-IDRADNR          TO T93-MID-IDRADNR                         
054200     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
054400     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
054500     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
054600     MOVE SPACE                TO MSG-KOM-KDTRANS                         
054700     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
054800     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
054900     MOVE 'W6116100'           TO MSG-KOM-IDSNDJOB                        
055000     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
055100     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
055200     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
055300                                                                          
055400     MOVE +29                  TO P-TO-P-MSG-KVLL                         
055600     MOVE 'W6T193X '           TO P-TO-P-MSG-KDTRANS                      
055700     MOVE '6161'               TO P-TO-P-MSG-IDTRANS                      
055800     MOVE 1                    TO P-TO-P-MSG-KDMFSFOR                     
055900                                                                          
055920     MOVE T93-MID-W6I19301     TO P-TO-P-MSG-INDATA                       
055930                                                                          
056000     CALL W006KOM USING MSG-PCB                                           
056100                        DISP-PCB                                          
056200                        KOM-KOMA-PCB                                      
056300                        MSG-KOM-WMSGKOM                                   
056310                        P-TO-P-MSG-IO-AREA-SNUF                           
056360     .                                                                    
056370     EJECT                                                                
056400******************************************************************        
056500* --- IMS SEKTIONER ---                                          *        
056700******************************************************************        
056800*    ALT1-PCB  (TRANS W60191)                                    *        
056900******************************************************************        
057000                                                                          
057100 IMS-ISRT-MSG-ALT1-6191 SECTION.                                          
057200                                                                          
057300     MOVE SPACE              TO GODK-STATUSKODER                          
057400     CALL CBLTDLI USING      ISRT ALT1-PCB                                
057500                                  P-TO-P-MSG-IO-AREA-SNUF                 
057600     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
057700     PERFORM IMS-STATUSKONTROLL                                           
057800     .                                                                    
058000     SKIP3                                                                
058600 IMS-PURG-MSG-ALT1-6191 SECTION.                                          
058700                                                                          
058800     MOVE SPACE              TO GODK-STATUSKODER                          
058900     CALL CBLTDLI USING      PURG ALT1-PCB                                
059000                                  P-TO-P-MSG-IO-AREA-SNUF                 
059100     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
059200     PERFORM IMS-STATUSKONTROLL                                           
059300     .                                                                    
059400     EJECT                                                                
059700******************************************************************        
059800*    INLC-PCB                                                    *        
059900******************************************************************        
060000                                                                          
060200 IMS-GU-INLC-D111  SECTION.                                               
060300     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1C1KY-X                            
060310                    '&IDDC     =' W-IDDC  ')'                             
060400             DELIMITED BY SIZE INTO SSA1                                  
060500     MOVE '  GE'                 TO GODK-STATUSKODER                      
060600     CALL CBLTDLI USING GU       INLC-PCB                                 
060700                                 DLI-IO-AREA                              
060800                                 SSA1                                     
060900     MOVE INLC-STATUS-CODE       TO STATUS-WS                             
061000     PERFORM IMS-STATUSKONTROLL                                           
061100     .                                                                    
061300     SKIP3                                                                
062800 IMS-GHNP-INLC-D121    SECTION.                                           
062810     STRING 'W6INLA21(IDLEVNRK =' WCS-IDLEVNR-X                           
062820                    '&IDOKOLLI =' WCS-IDOKOLLI-X ')'                      
062830          DELIMITED BY SIZE INTO SSA1                                     
063300     MOVE '  GE'            TO GODK-STATUSKODER                           
063400     CALL CBLTDLI USING GHNP INLC-PCB                                     
063500                             DLI-IO-AREA                                  
063600                             SSA1                                         
063800     MOVE INLC-STATUS-CODE  TO STATUS-WS                                  
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064300     SKIP3                                                                
064400 IMS-REPL-INLC SECTION.                                                   
064500                                                                          
064600     MOVE '  '               TO GODK-STATUSKODER                          
064700     CALL CBLTDLI USING REPL INLC-PCB                                     
064800                             DLI-IO-AREA                                  
064900     MOVE INLC-STATUS-CODE   TO STATUS-WS                                 
065000     PERFORM IMS-STATUSKONTROLL                                           
065100     .                                                                    
065200     EJECT                                                                
067900 IMS-STATUSKONTROLL SECTION.                                              
068000     SKIP2                                                                
068100     SET STATUS-IX TO 1                                                   
068200     SEARCH GODK-STATUS                                                   
068300       AT END                                                             
068400         MOVE 'FEL VID DL1 ANROP' TO FELTEXT-STR                          
068500         DISPLAY FELTEXT                                                  
068600         CALL FELLOG                                                      
068700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
068800     END-SEARCH                                                           
068900     .                                                                    
068910     SKIP3                                                                
068920 PGM-ABEND  SECTION.                                                      
068930     SKIP2                                                                
068980     DISPLAY FELTEXT                                                      
068990     CALL FELLOG                                                          
068993     .                                                                    
