001600 ID DIVISION.                                                             
001700     SKIP2                                                                
001800 PROGRAM-ID.     W4266300.                                                
001900*AUTHOR.         GERRY CARMICHAEL.                                        
002000*DATE-WRITTEN.   92/08/19.                                                
002100                                                                          
002200*    REMARKS.                                                             
002300*                                                                         
002400*    FUNKTION:                                                            
002500*        LÄSER EN INFIL MED ARTIKELINFO, KOMPLETTERAR DEN OCH             
002600*        SKRIVER EN UTFIL                                                 
002700*                                                                         
002800*        PROGRAMMET LÄSER      W6KVAH (W6D2)                              
002900*        PROGRAMMET LÄSER      W6LEVA (W6F1)                              
002910*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
003000*                                                                         
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP2                                                                
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200     SKIP2                                                                
004300*          --- INFIL                                                      
004400     SELECT W4266A                     ASSIGN TO W42663D1.                
004500     SKIP2                                                                
004600*          --- UTFIL                                                      
004700     SELECT W42663                     ASSIGN TO W42663D2.                
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000     SKIP3                                                                
005100 FILE SECTION.                                                            
005200     SKIP3                                                                
005300 FD  W4266A                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600     SKIP2                                                                
005700*01  -COPY W4266001      -L.                                              
005800     SKIP3                                                                
005900 FD  W42663                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200     SKIP2                                                                
006300*01  POST -COPY W4266301 -PRE  UT-  -L.                                   
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600     SKIP2                                                                
006601                                                                          
006610*    -- CHECKED BY WY2000                                                 
006700 77  IDPGM                       PIC X(8)    VALUE 'W4266300'.            
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000                                                                          
007010 77  W-LEV-FLSKPLOT              PIC X.                                   
007020                                                                          
007100 77  W4266A-EOF-SW               PIC X       VALUE 'N'.                   
007200     88  END-OF-W4266A                       VALUE 'J'.                   
007210                                                                          
007230 77  KVALITET-SW                 PIC X       VALUE 'N'.                   
007240     88  KVAL-SAEKRAD                        VALUE 'J'.                   
007300     EJECT                                                                
007400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007500 01  FILLER REDEFINES DAGENS-DATUM.                                       
007600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007900     EJECT                                                                
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008600     SKIP2                                                                
009200 01  FELTEXT.                                                             
009300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009500     EJECT                                                                
009510*    --- ANVÄNDS VID TEST AV FUNKTIONSGRUPP                               
009520 01  IDFKNGRP-AREA               PIC S9(5)   COMP-3.                      
009530     88  IDFKNGRP-EMB            VALUE +1920 THRU +1999.                  
009560     EJECT                                                                
009600*    --- PARAMETRAR TILL POSTSUM                                          
009700*                                                                         
009800*01  -COPY W0005   -PRE  POSTSUM-                                         
009900     EJECT                                                                
010000 01  TEST-IDARTNR                PIC S9(9)   COMP-3.                      
010100     SKIP3                                                                
010200*01  FILLER   -COPY WWBYT03  -RED TEST-IDARTNR.                           
010500     EJECT                                                                
010600 01  IN-AREA-START               PIC X(24)   VALUE                        
010700                                 'IN-AREA-START  '.                       
010800     SKIP2                                                                
010900                                                                          
011000*01  AREA -COPY W4266001     -PRE IN-                                     
011100     EJECT                                                                
011200 01  UT-AREA-START               PIC X(24)   VALUE                        
011300                                 'UT-AREA-START  '.                       
011400     SKIP2                                                                
011500                                                                          
011600*01  AREA -COPY W4266301     -PRE UT-                                     
011700     EJECT                                                                
011800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011900*                                                                         
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012200     SKIP3                                                                
012300 01  NYCKLAR-TILL-DLI.                                                    
012400     03  W-IDARTNR-X.                                                     
012500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012600     03  W-IDLEVNR-X.                                                     
012700         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
012800     SKIP2                                                                
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                       VALUE '  '.                  
013200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013400     SKIP2                                                                
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000     EJECT                                                                
014100*    --- IMS FUNKTIONSKODER                                               
014200*01  -COPY W0003                                                          
014300     EJECT                                                                
014400*    ---  DLI INPUT-OUTPUT AREA                                           
014500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014600     SKIP3                                                                
014700 01  DLI-IO-AREA.                                                         
014800     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
014900     SKIP3                                                                
015000     03  W6KVAH01 REDEFINES IO-AREA.                                      
015100*        05  -COPY W6D201                                                 
015200     SKIP3                                                                
015300     03  W6KVAH12 REDEFINES IO-AREA.                                      
015400*        05  -COPY W6D212                                                 
015500     SKIP3                                                                
015600     03  W6LEVA01 REDEFINES IO-AREA.                                      
015700*        05  -COPY W6F101   -PRE LEVA-                                    
015701 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
015702     SKIP3                                                                
015710 01  DLI-IO-AREA2.                                                        
015720     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
015730     SKIP3                                                                
015740     03  WLARTC01 REDEFINES IO-AREA2.                                     
015750*        05  -COPY WDK601                                                 
015800     EJECT                                                                
015801     03  WLARTC01 REDEFINES IO-AREA2.                                     
015810*        05  -COPY WDK611                                                 
015820     EJECT                                                                
015900 LINKAGE SECTION.                                                         
016000                                                                          
016100     EJECT                                                                
016200*01  -COPY W0008  -PRE KVAH-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008  -PRE LEVA-                                              
016600     05  FILLER                  PIC X.                                   
016610*01  -COPY W0008  -PRE ARTC-                                              
016620     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800 PROCEDURE DIVISION  USING KVAH-PCB LEVA-PCB ARTC-PCB.                    
016900     ENTRY 'DLITCBL' USING KVAH-PCB LEVA-PCB ARTC-PCB.                    
017000                                                                          
017100     SKIP2                                                                
017200     PERFORM A-INIT                                                       
017300                                                                          
017400     PERFORM S01-LAES-W4266A                                              
017500                                                                          
017600     PERFORM UNTIL END-OF-W4266A                                          
017700                                                                          
017800       PERFORM B-BEHANDLA-INPOSTER                                        
017900                                                                          
018000       PERFORM S01-LAES-W4266A                                            
018100                                                                          
018200     END-PERFORM                                                          
018300                                                                          
018400     PERFORM Z-FINIT                                                      
018500                                                                          
018600     MOVE ZERO TO RETURN-CODE                                             
018700     GOBACK                                                               
018800     .                                                                    
018900     EJECT                                                                
019000 A-INIT SECTION.                                                          
019100                                                                          
019200     OPEN INPUT  W4266A                                                   
019300                                                                          
019400     OPEN OUTPUT W42663                                                   
019500     SKIP2                                                                
019600     ACCEPT DAGENS-DATUM  FROM DATE                                       
019700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019800     .                                                                    
019900     EJECT                                                                
020000 B-BEHANDLA-INPOSTER SECTION.                                             
020100                                                                          
020200     IF IN-IDLEVNR-NEW NOT = '     ' AND '9996 '                          
020300                         AND '9998 ' AND '9999 '                          
020400        IF IN-KDERS < 21                                                  
020420          MOVE IN-IDFKNGRP TO IDFKNGRP-AREA                               
020430          IF NOT IDFKNGRP-EMB                                             
020700             MOVE IN-IDARTNR TO TEST-IDARTNR                              
020800             IF NOT BYT03-OBJEKT                                          
020930                PERFORM BA-KOLLA-SKIPLOT                                  
021001                IF W-LEV-FLSKPLOT = JA                                    
021100                   PERFORM BB-KOLLA-ARTIKELINFO                           
021200                   IF NOT KVAL-SAEKRAD                                    
021300                      PERFORM BC-SKRIV-UTFIL                              
021400                   END-IF                                                 
021500                END-IF                                                    
021600             END-IF                                                       
021700          END-IF                                                          
021900        END-IF                                                            
022000     END-IF                                                               
022100                                                                          
022200     .                                                                    
022300     EJECT                                                                
022400 BA-KOLLA-SKIPLOT SECTION.                                                
022600     MOVE IN-IDLEVNR-NEW TO W-IDLEVNR                                     
022700     PERFORM IMS-GET-LEVA-LEVA01                                          
022800     IF SEGMENT-FINNS                                                     
022900       MOVE LEVA-LEV-FLSKPLOT TO W-LEV-FLSKPLOT                           
023210     ELSE                                                                 
023211       MOVE JA                TO W-LEV-FLSKPLOT                           
023220     END-IF                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 BB-KOLLA-ARTIKELINFO SECTION.                                            
023600     MOVE NEJ TO KVALITET-SW                                              
023610     MOVE SPACE            TO UT-ADKVAULG                                 
023620                              UT-KDKVAKTL                                 
023630     MOVE ZERO             TO UT-TIKVASAK                                 
023700     MOVE IN-IDARTNR TO W-IDARTNR                                         
023710     PERFORM IMS-GET-KVAH-KVAH01                                          
023720     IF SEGMENT-FINNS                                                     
023730       MOVE ART-ADKVAULG   TO UT-ADKVAULG                                 
023740       MOVE ART-KDKVAKTL   TO UT-KDKVAKTL                                 
023750       PERFORM IMS-GET-KVAH-KVAH12                                        
023760       IF SEGMENT-FINNS                                                   
023770         IF LEV-FLKVASAK = NEJ                                            
023771           MOVE LEV-TIKVASAK  TO UT-TIKVASAK                              
023790         ELSE                                                             
023792           MOVE JA TO KVALITET-SW                                         
023793         END-IF                                                           
023796       END-IF                                                             
023797     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 BC-SKRIV-UTFIL SECTION.                                                  
024001     SKIP2                                                                
024002     MOVE IN-IDARTNR TO W-IDARTNR                                         
024005     PERFORM IMS-GU-WLARTC11                                              
024006     IF SEGMENT-FINNS                                                     
024007         MOVE CLAG-IDANSK TO UT-IDANSK                                    
024008     ELSE                                                                 
024009         MOVE ZERO  TO UT-IDANSK                                          
024010     END-IF                                                               
024011                                                                          
024012     MOVE IN-IDARTNR      TO  UT-IDARTNR                                  
024020     MOVE IN-BEART        TO  UT-BEART                                    
024030     MOVE IN-IDFKNGRP     TO  UT-IDFKNGRP                                 
024040     MOVE IN-IDBERED      TO  UT-IDBERED                                  
024050     MOVE IN-IDLEVNR-OLD  TO  UT-IDLEVNR-OLD                              
024060     MOVE IN-IDLEVNR-NEW  TO  UT-IDLEVNR-NEW                              
024070     MOVE IN-KDERS        TO  UT-KDERS                                    
024080     MOVE IN-PRARTSTD     TO  UT-PRARTSTD                                 
024090     MOVE IN-BEFT         TO  UT-BEFT                                     
024091     MOVE IN-KDVVKL       TO  UT-KDVVKL                                   
024092     MOVE IN-TIFINLV      TO  UT-TIFINLV                                  
024093     MOVE IN-KDFARLIG     TO  UT-KDFARLIG                                 
024094     MOVE IN-KDYTBEH      TO  UT-KDYTBEH                                  
024095     MOVE IN-KDPRODSL     TO  UT-KDPRODSL                                 
024096                                                                          
024097     IF IN-IDLEVNR-NEW NOT = IN-IDLEVNR-OLD                               
024099       IF IN-IDBERED > 19 OR IN-IDBERED = ZERO                            
024100         MOVE 'BYTE LEVNR' TO UT-TEXT                                     
024101       ELSE                                                               
024102         MOVE 'NEW SUPPL.' TO UT-TEXT                                     
024104       END-IF                                                             
024105     ELSE                                                                 
024107       IF IN-IDBERED > 19 OR IN-IDBERED = ZERO                            
024108         MOVE 'NYREG ARTNR' TO UT-TEXT                                    
024109       ELSE                                                               
024110         MOVE 'NEW P.NO.'    TO UT-TEXT                                   
024111       END-IF                                                             
024112     END-IF                                                               
024113     PERFORM S11-SKRIV-W42663                                             
024120     .                                                                    
024200     EJECT                                                                
024300 Z-FINIT SECTION.                                                         
024400     CLOSE W4266A                                                         
024500           W42663                                                         
024600     SKIP2                                                                
024700     MOVE 'S' TO POSTSUM-OPKOD                                            
024800     CALL POSTSUM USING POSTSUM-PARM                                      
024900     .                                                                    
025000     EJECT                                                                
025100 S01-LAES-W4266A  SECTION.                                                
025200     SKIP2                                                                
025300     READ W4266A INTO IN-AREA                                             
025400     AT END                                                               
025600        MOVE JA TO W4266A-EOF-SW                                          
025700                                                                          
025800     NOT AT END                                                           
025900        MOVE 'W4266A' TO POSTSUM-FDNAMN                                   
026000        MOVE 'W42663D1' TO POSTSUM-DDNAMN2                                
026200        CALL POSTSUM USING POSTSUM-PARM                                   
026300     END-READ                                                             
026400     .                                                                    
026500     EJECT                                                                
026600 S11-SKRIV-W42663 SECTION.                                                
026700     SKIP2                                                                
026800     WRITE UT-POST FROM UT-AREA                                           
026900                                                                          
027100     MOVE 'W42663' TO POSTSUM-FDNAMN                                      
027200     MOVE 'W42663D2' TO POSTSUM-DDNAMN2                                   
027300     CALL POSTSUM USING POSTSUM-PARM                                      
027400     .                                                                    
027500     EJECT                                                                
028400* --- IMS SEKTIONER ---                                                   
028500     SKIP3                                                                
028600     EJECT                                                                
028700 IMS-GET-KVAH-KVAH01 SECTION.                                             
028800     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
028900          DELIMITED BY SIZE INTO SSA1                                     
029000     MOVE '  GE' TO GODK-STATUSKODER                                      
029100     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA SSA1                      
029200     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
029300     PERFORM IMS-STATUSKONTROLL                                           
029400     .                                                                    
029500     EJECT                                                                
029600 IMS-GET-KVAH-KVAH12 SECTION.                                             
029700     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
029800          DELIMITED BY SIZE INTO SSA1                                     
029900     MOVE '  GE' TO GODK-STATUSKODER                                      
030000     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA SSA1                     
030100     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     .                                                                    
030400     EJECT                                                                
030500 IMS-GET-LEVA-LEVA01 SECTION.                                             
030600     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
030700          DELIMITED BY SIZE INTO SSA1                                     
030800     MOVE '  GE' TO GODK-STATUSKODER                                      
030900     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA SSA1                      
031000     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300     EJECT                                                                
031310 IMS-GU-WLARTC11 SECTION.                                                 
031320     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031330          DELIMITED BY SIZE INTO SSA1                                     
031331     STRING 'WLARTC11 '                                                   
031332          DELIMITED BY SIZE INTO SSA2                                     
031340     MOVE '  GE' TO GODK-STATUSKODER                                      
031350     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1 SSA2                
031360     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031370     PERFORM IMS-STATUSKONTROLL                                           
031380     .                                                                    
031390     EJECT                                                                
031400 IMS-STATUSKONTROLL SECTION.                                              
031500     SKIP2                                                                
031600     SET STATUS-IX TO 1                                                   
031700     SEARCH GODK-STATUS                                                   
031800       AT END                                                             
031900         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
032100         CALL FELLOG                                                      
032200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032300         CONTINUE                                                         
032400     END-SEARCH                                                           
032500     .                                                                    
