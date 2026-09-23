000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W6012111.                                                
000500*AUTHOR.         ROSEMARIE CLAESSON.                                      
000600*DATE-WRITTEN.   92/08/08.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W6012100                       
001200*        SOM SKRIVER UT LISTA VID AVVIKELSER VID LOSSNINGEN               
001300*                                                                         
001400*        PROGRAMMET LÄSER      W6LASA (W6G2)                              
001500*        CPY-TEXT              W6LASA01  (W6GX01)                         
001600*                                    11  (W6GX6108)                       
001700*                                    21  (W6GX6110)                       
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100                                                                          
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400                                                                          
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700                                                                          
003800 WORKING-STORAGE SECTION.                                                 
003900     SKIP2                                                                
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W6012111'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300     SKIP2                                                                
004400                                                                          
004500 77  TRAEFF-SW                   PIC X       VALUE 'J'.                   
004600     88 TRAEFF-JA                            VALUE 'J'.                   
004700     88 TRAEFF-NEJ                           VALUE 'N'.                   
004800                                                                          
004810 77  SAKNAT-SW                   PIC X       VALUE 'N'.                   
004820     88 SAKNAT-PARTI                         VALUE 'J'.                   
004830                                                                          
004900 77  PRINTER-OK                  PIC X       VALUE 'J'.                   
005000 77  UTSKRIFT-OK                 PIC X       VALUE 'N'.                   
005100                                                                          
005200 01  WORK-SPAR-AREA.                                                      
005201*PÅGÅENDE RAD (ATT SKRIVA)                                                
005210     03  W-RAD.                                                           
005220         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
005230         05  W-IDFS              PIC X(8)    VALUE SPACE.                 
005240         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
005250         05  W-TIAVIDAT          PIC S9(7)   COMP-3.                      
005260                                                                          
005270*FÖREGÅENDE RAD (SKRIVEN RAD)                                             
005280     03  WX-RAD.                                                          
005290         05  WX-IDLEVNR           PIC  X(5)   VALUE SPACE.                
005291         05  WX-IDFS              PIC X(8)    VALUE SPACE.                
005293                                                                          
005300*ACKAR                                                                    
005400     03  W-TOT-ANT               PIC S9(7)    COMP-3.                     
005500*RÄKNARE                                                                  
005600     03  W-SIDNR                 PIC S9(3)    COMP-3.                     
005700     03  W-RADNR                 PIC S9(3)    COMP-3.                     
006300                                                                          
006310*DIVERSE                                                                  
006330     03 W-ADINLOMR-LPL           PIC X(4)    VALUE SPACE.                 
006400***********************************************                           
006500*  PRINTRADER                                 *                           
006600***********************************************                           
006700 01  BLANK-RAD.                                                           
006800     03 FILLER               PIC X(121) VALUE SPACE.                      
006900                                                                          
007000 01  RAD4.                                                                
007100     03 FILLER          PIC X(22) VALUE ' VOLVO CAR AFTER SALES'.         
007300     03 FILLER          PIC X(18) VALUE SPACE.                            
007400     03 FILLER          PIC X(10) VALUE 'W60121-001'.                     
007500     03 FILLER          PIC X(5)  VALUE SPACE.                            
007700     03 FILLER          PIC X(39) VALUE                                   
007710                        'FELLISTA VID LOSSNING - SAKNADE PARTIER'.        
007800     03 FILLER          PIC X(6)  VALUE SPACE.                            
008100     03 RAD4-DATUM      PIC X(6).                                         
008200     03 FILLER          PIC X(5)  VALUE ' SID '.                          
008300     03 RAD4-SIDNR      PIC ZZ9.                                          
008500                                                                          
008510 01  RAD4-ENG.                                                            
008520     03 FILLER          PIC X(22) VALUE ' VOLVO CAR AFTER SALES'.         
008530     03 FILLER          PIC X(18) VALUE SPACE.                            
008540     03 FILLER          PIC X(10) VALUE 'W60121-001'.                     
008550     03 FILLER          PIC X(5)  VALUE SPACE.                            
008560     03 FILLER          PIC X(39) VALUE                                   
008570                        'ERROR LIST - MISSING BATCHES           '.        
008580     03 FILLER          PIC X(5)  VALUE SPACE.                            
008590     03 RAD4-DATUM-ENG  PIC X(6).                                         
008591     03 FILLER          PIC X(6)  VALUE ' PAGE '.                         
008592     03 RAD4-SIDNR-ENG  PIC ZZ9.                                          
008593                                                                          
008600 01  RAD7.                                                                
008700     03 FILLER               PIC X(13)  VALUE ' LASTBÄRARE  '.            
008800     03 RAD7-IDLBBET         PIC X(12).                                   
008900     03 FILLER               PIC X(09)  VALUE '    LPL  '.                
009000     03 RAD7-ADINLOMR-LPL    PIC X(4).                                    
009100     03 FILLER               PIC X(83) VALUE SPACE.                       
009200                                                                          
009210 01  RAD7-ENG.                                                            
009220     03 FILLER               PIC X(13)  VALUE ' CARRIER     '.            
009230     03 RAD7-IDLBBET-ENG     PIC X(12).                                   
009240     03 FILLER               PIC X(09)  VALUE '    UNL  '.                
009250     03 RAD7-ADINLOMR-LPL-ENG PIC X(4).                                   
009260     03 FILLER               PIC X(83) VALUE SPACE.                       
009270                                                                          
009300 01  RAD10.                                                               
009400     03 FILLER               PIC X(28)  VALUE SPACE.                      
009500     03 FILLER               PIC X(5)   VALUE 'LEVNR'.                    
009600     03 FILLER               PIC X(15)  VALUE SPACE.                      
009700     03 FILLER               PIC X(4)   VALUE 'FSNR'.                     
009800     03 FILLER               PIC X(19)  VALUE SPACE.                      
009900     03 FILLER               PIC X(5)   VALUE 'ARTNR'.                    
010000     03 FILLER               PIC X(12)  VALUE SPACE.                      
010100     03 FILLER               PIC X(5)   VALUE 'ANTAL'.                    
010200     03 FILLER               PIC X(38)  VALUE SPACE.                      
010300                                                                          
010310 01  RAD10-ENG.                                                           
010320     03 FILLER               PIC X(28)  VALUE SPACE.                      
010330     03 FILLER               PIC X(5)   VALUE 'SUPPL'.                    
010340     03 FILLER               PIC X(15)  VALUE SPACE.                      
010350     03 FILLER               PIC X(4)   VALUE 'ADNO'.                     
010360     03 FILLER               PIC X(18)  VALUE SPACE.                      
010370     03 FILLER               PIC X(6)   VALUE 'PARTNO'.                   
010380     03 FILLER               PIC X(12)  VALUE SPACE.                      
010390     03 FILLER               PIC X(5)   VALUE 'QTY  '.                    
010391     03 FILLER               PIC X(38)  VALUE SPACE.                      
010392                                                                          
010400 01  DETALJ-RAD.                                                          
010500     03 FILLER               PIC X(28)  VALUE SPACE.                      
010600     03 RAD-IDLEVNR          PIC X(5).                                    
010700     03 FILLER               PIC X(15)  VALUE SPACE.                      
010800     03 RAD-IDFS             PIC X(8).                                    
010900     03 FILLER               PIC X(11)  VALUE SPACE.                      
011000     03 RAD-IDARTNR          PIC Z(8)9.                                   
011100     03 FILLER               PIC X(10)  VALUE SPACE.                      
011200     03 RAD-TOT-ANT          PIC Z(6)9.                                   
011300     03 FILLER               PIC X(40)  VALUE SPACE.                      
011400                                                                          
011500 01  NOT-RAD.                                                             
011600     03 FILLER               PIC X(10)  VALUE ' NOTERING '.               
011700     03 RAD-TELOSSN          PIC X(105).                                  
011800     03 FILLER               PIC X(6)   VALUE SPACE.                      
011900                                                                          
011910 01  NOT-RAD-ENG.                                                         
011920     03 FILLER               PIC X(10)  VALUE ' NOTE     '.               
011930     03 RAD-TELOSSN-ENG      PIC X(105).                                  
011940     03 FILLER               PIC X(6)   VALUE SPACE.                      
011950                                                                          
012000 01  USER-RAD.                                                            
012100     03 FILLER               PIC X(10)  VALUE '   USERID '.               
012200     03 RAD-IDUSER           PIC X(08).                                   
012300     03 FILLER               PIC X(103) VALUE SPACE.                      
012400                                                                          
012500 01  FELTEXT.                                                             
012600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012800     EJECT                                                                
012900                                                                          
013000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013100 01  FILLER REDEFINES DAGENS-DATUM.                                       
013200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013500     EJECT                                                                
013510*      --- VALID IDDC CODES                                               
013520*                                                                         
013530*01    -COPY WWDC99                                                       
013540       EJECT                                                              
013600                                                                          
013700 01  DYNAMISKA-SUBPROGRAM.                                                
013800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014100     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
014200     EJECT                                                                
014300                                                                          
014400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014500*01 -COPY WMEDAREA                                                        
014600     SKIP3                                                                
014700*    ---AREA FÖR SUBPGM W006PRR1                                          
014800 01 FILLER                      PIC X(16)   VALUE 'W006PRR1'.             
014900                                                                          
015000*01 -COPY W006PRAR                                                        
015100                                                                          
015200 01 WS-PRINTER-PARM.                                                      
015300     03 WS-IDPRT.                                                         
015310       05 FILLER                 PIC X(8).                                
015400     03 WS-RAD.                                                           
015500       05 WS-FILLER              PIC X(1).                                
015600       05 WS-LISTRAD             PIC X(120).                              
015700     03 WS-DUMMY                 PIC X(1).                                
015710     03 WS-RAPP-PRINTER          PIC X(8).                                
015800                                                                          
015900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016000     SKIP3                                                                
016100 01  NYCKLAR-TILL-DLI.                                                    
016200*----FYSISK NYCKEL TILL LASA                                              
016300     03  W-W6GX01KY-X.                                                    
016400         05  W-IDHTYP            PIC X(4)    VALUE SPACE.                 
016410         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016500         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016600                                                                          
016700     03  W-W6GX11KY-X.                                                    
016800         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
016900         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
017000                                                                          
017100     03  W-W6GX21KY-X.                                                    
017200         05  WL-IDLEVNR           PIC  X(5)   VALUE SPACE.                
017300         05  WL-IDFS              PIC X(8)    VALUE SPACE.                
017400         05  WL-IDARTNR           PIC S9(9)   COMP-3.                     
017500         05  WL-KDSORT1           PIC S9      COMP-3.                     
017600                                                                          
017610*----FYSISK NYCKEL TILL INLA                                              
017620     03  W-W6D101KY-X.                                                    
017621         05  W-IDDC-D1           PIC X(2)    VALUE SPACE.                 
017630         05  W-IDLEVNR-D1        PIC  X(5)   VALUE SPACE.                 
017640         05  W-IDFS-D1           PIC X(8)    VALUE SPACE.                 
017650         05  W-TIAVIDAT-D1       PIC S9(7)   COMP-3.                      
017700                                                                          
017710     03  W-IDARTNR-X.                                                     
017720         05  W-IDARTNR-D1        PIC S9(9)   COMP-3.                      
017730                                                                          
017800     SKIP2                                                                
017900     EJECT                                                                
018000                                                                          
018100     SKIP2                                                                
018200*    --- STATUS-KOD FRÅN IMS                                              
018300 01  STATUS-WS                   PIC XX.                                  
018400     88  SEGMENT-FINNS                       VALUE '  '.                  
018500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018800     88  IMS-EJ-OK                           VALUE 'XD'.                  
018900     EJECT                                                                
019000                                                                          
019100     SKIP2                                                                
019200 01  GODK-STATUSKODER.                                                    
019300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019400     EJECT                                                                
019500                                                                          
019600     SKIP3                                                                
019700 01  SSA1                        PIC X(90).                               
019800 01  SSA2                        PIC X(64).                               
019900 01  SSA3                        PIC X(64).                               
020000     EJECT                                                                
020100                                                                          
020200*    --- IMS FUNKTIONSKODER                                               
020300*01  -COPY W0003                                                          
020400     EJECT                                                                
020500                                                                          
020600*    ---  DLI INPUT-OUTPUT AREA                                           
020700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020800     SKIP3                                                                
020900 01  DLI-IO-AREA-1.                                                       
021000     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
021100                                                                          
021200     03  W6LASA01 REDEFINES IO-AREA-1.                                    
021300*        05  -COPY W6GX01                                                 
021400     EJECT                                                                
021500                                                                          
021600     03  W6LASA11 REDEFINES IO-AREA-1.                                    
021700*        05  -COPY W6GX6108                                               
021800     EJECT                                                                
021900                                                                          
022000     03  W6LASA21 REDEFINES IO-AREA-1.                                    
022100*        05  -COPY W6GX6110                                               
022400     EJECT                                                                
022410 01  DLI-IO-AREA-2.                                                       
022420     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
022430                                                                          
022440     03  W6INLA11 REDEFINES IO-AREA-2.                                    
022450*        05  -COPY W6D111   -PRE INLA-                                    
022460     EJECT                                                                
022470                                                                          
022480     03  W6INLA21 REDEFINES IO-AREA-2.                                    
022490*        05  -COPY W6D121   -PRE INLA-                                    
022497     EJECT                                                                
022500                                                                          
022600                                                                          
022700 LINKAGE SECTION.                                                         
022800                                                                          
022810*01  -COPY W0009  -PRE ALT-                                               
022830     EJECT                                                                
022840                                                                          
022850*01  -COPY W0008  -PRE LISB-                                              
022851     05  FILLER                  PIC X.                                   
022860     EJECT                                                                
022870                                                                          
022900*01  -COPY W6012111                                                       
023100     EJECT                                                                
023200                                                                          
023300*01  -COPY W0008  -PRE LASA-W6012111-                                     
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023510                                                                          
023520*01  -COPY W0008  -PRE INLA-W6012111-                                     
023530     05  FILLER                  PIC X.                                   
023540     EJECT                                                                
023600                                                                          
023700                                                                          
023800                                                                          
023900 PROCEDURE DIVISION  USING ALT-PCB                                        
023910                           LISB-PCB                                       
023920                           W601-W6012111                                  
024000                           LASA-W6012111-PCB                              
024010                           INLA-W6012111-PCB.                             
024100                                                                          
024300     PERFORM A-INIT                                                       
024500     PERFORM B-BEARB                                                      
024600     PERFORM C-AVSLUTA                                                    
024700                                                                          
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200******************************************************************        
025300 A-INIT SECTION.                                                          
025400     SKIP2                                                                
025500                                                                          
025600     ACCEPT DAGENS-DATUM     FROM DATE                                    
025700                                                                          
026100*-----------------------------------                                      
026200*----INITIERA INFÖR LÄSNING AV LASA                                       
026300*-----------------------------------                                      
026400     MOVE '6107'               TO W-IDHTYP                                
026500     MOVE W601-IDDC            TO W-IDDC                                  
026501                                  W-IDDC-D1                               
026502                                  WS-IDDC                                 
026510     MOVE W601-IDLBBET         TO W-IDLBBET                               
026600                                                                          
026700*-----------------------------------                                      
026800*----INIT PRINTERID                                                       
026900*-----------------------------------                                      
026910     MOVE W601-IDPRTLST             TO WS-RAPP-PRINTER                    
027600                                                                          
028000*-----------------------------------                                      
028001*----ÖPPNA PRINTER                                                        
028002*-----------------------------------                                      
028010     PERFORM S01-PRT-OPEN                                                 
028011                                                                          
028012*-----------------------------------                                      
028013*----INITIERA RÄKNARE                                                     
028014*-----------------------------------                                      
028020     MOVE +1                   TO W-SIDNR                                 
028021     MOVE +1                   TO W-RADNR                                 
028030                                                                          
028040     MOVE ZERO                 TO W-TOT-ANT                               
028051                                  W-TIAVIDAT                              
028052                                  W-IDARTNR                               
028060     MOVE SPACE                TO W-IDFS                                  
028070                                  WX-IDFS                                 
028080                                  W-IDLEVNR                               
028090                                  WX-IDLEVNR                              
028200     .                                                                    
028300     EJECT                                                                
029700******************************************************************        
029800*  BEARBETA LÄSA LASA/REDIGERA LISTA                             *        
029900******************************************************************        
030000 B-BEARB SECTION.                                                         
030100                                                                          
030400*----------------------------------------------------------------*        
030500*--- LÄSNING AV W6LASA                                        ---*        
030600*--- MED ENBART LASTBÄRARE SOM NYCKEL                         ---*        
030700*----------------------------------------------------------------*        
030800                                                                          
030900     PERFORM IMS-GU-LASA-G111                                             
030910     MOVE 6108-ADINLOMR-LPL        TO W-ADINLOMR-LPL                      
031000     PERFORM BA-SKRIV-HUVUD                                               
031100                                                                          
031200     PERFORM IMS-GNP-LASA-G121                                            
031210     PERFORM BD-KOLLA-OM-NGT-SAKNAS                                       
031300                                                                          
031400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
031500        IF W-RADNR = 42                                                   
031600           MOVE +1       TO W-RADNR                                       
031700           ADD +1        TO W-SIDNR                                       
031800           PERFORM BA-SKRIV-HUVUD                                         
031900        END-IF                                                            
031910        IF SAKNAT-PARTI                                                   
032000          IF W-IDARTNR > ZERO                                             
032100             IF W-IDARTNR = 6110-IDARTNR AND                              
032101                W-IDFS    = 6110-IDFS AND                                 
032102                W-IDLEVNR = 6110-IDLEVNR AND                              
032103                W-TIAVIDAT = 6110-TIAVIDAT                                
032110*-------------SAMMA ARTIKEL IGEN - ADDERA UPP ANTAL                       
032200                ADD 6110-KVAVIS TO W-TOT-ANT                              
032400             ELSE                                                         
032500*-------------NÄSTKOMMANDE RADER PÅ LISTAN                                
032600                PERFORM BB-SKRIV-RAD                                      
032610*-------------INITIERA INFÖR NÄSTA RAD                                    
032700                ADD 1                TO W-RADNR                           
032710                MOVE W-IDLEVNR       TO WX-IDLEVNR                        
032720                MOVE W-IDFS          TO WX-IDFS                           
032730                                                                          
032800                MOVE ZERO            TO W-TOT-ANT                         
032900                MOVE 6110-IDLEVNR    TO W-IDLEVNR                         
033000                MOVE 6110-IDFS       TO W-IDFS                            
033010                MOVE 6110-TIAVIDAT   TO W-TIAVIDAT                        
033100                MOVE 6110-IDARTNR    TO W-IDARTNR                         
033200                ADD 6110-KVAVIS      TO W-TOT-ANT                         
033300             END-IF                                                       
033400          ELSE                                                            
033410*----------FÖRSTA RADEN PÅ LISTAN                                         
033500             ADD 6110-KVAVIS    TO W-TOT-ANT                              
033600             MOVE 6110-IDLEVNR  TO W-IDLEVNR                              
033700             MOVE 6110-IDFS     TO W-IDFS                                 
033710             MOVE 6110-TIAVIDAT TO W-TIAVIDAT                             
033800             MOVE 6110-IDARTNR  TO W-IDARTNR                              
033900          END-IF                                                          
033910        END-IF                                                            
033911        MOVE NEJ TO SAKNAT-SW                                             
034020        PERFORM IMS-GNP-LASA-G121                                         
034030        PERFORM BD-KOLLA-OM-NGT-SAKNAS                                    
034100     END-PERFORM                                                          
034200                                                                          
034300*----SKRIV SISTA RADEN                                                    
034400     IF W-RADNR = 42                                                      
034500        MOVE +1          TO W-RADNR                                       
034600        ADD +1           TO W-SIDNR                                       
034700        PERFORM BA-SKRIV-HUVUD                                            
034800     END-IF                                                               
034900     PERFORM BB-SKRIV-RAD                                                 
034902                                                                          
034910*----SKRIV NOTERING OCH USER                                              
035000     PERFORM BC-SKRIV-NOT-USER                                            
035300     .                                                                    
035400     EJECT                                                                
035500******************************************************************        
035600*  HUVUD                                                         *        
035610*  REDIGERA RUBRIKER                                             *        
035700******************************************************************        
035800 BA-SKRIV-HUVUD        SECTION.                                           
036010                                                                          
036050     MOVE DAGENS-DATUM     TO RAD4-DATUM                                  
036051                              RAD4-DATUM-ENG                              
036052     MOVE W-SIDNR          TO RAD4-SIDNR                                  
036053                              RAD4-SIDNR-ENG                              
036054     IF CDC-SE                                                            
036060       MOVE RAD4             TO WS-LISTRAD                                
036061     ELSE                                                                 
036062       MOVE RAD4-ENG         TO WS-LISTRAD                                
036063     END-IF                                                               
036070     MOVE PRT-NYSIDA-RAD4  TO PRT-RADSKIP                                 
036080     PERFORM S02-SKRIV-RAD                                                
036090                                                                          
036092     MOVE W601-IDLBBET     TO RAD7-IDLBBET                                
036093                              RAD7-IDLBBET-ENG                            
036094     MOVE W-ADINLOMR-LPL   TO RAD7-ADINLOMR-LPL                           
036095                              RAD7-ADINLOMR-LPL-ENG                       
036096     IF CDC-SE                                                            
036097       MOVE RAD7             TO WS-LISTRAD                                
036098     ELSE                                                                 
036099       MOVE RAD7-ENG         TO WS-LISTRAD                                
036100     END-IF                                                               
036101     MOVE PRT-AFTER-3      TO PRT-RADSKIP                                 
036102     PERFORM S02-SKRIV-RAD                                                
036103                                                                          
036104     IF CDC-SE                                                            
036105       MOVE RAD10            TO WS-LISTRAD                                
036106     ELSE                                                                 
036107       MOVE RAD10            TO WS-LISTRAD                                
036108     END-IF                                                               
036109     MOVE PRT-AFTER-3      TO PRT-RADSKIP                                 
036110     PERFORM S02-SKRIV-RAD                                                
036200     .                                                                    
036300     EJECT                                                                
036400******************************************************************        
036500*  RADER                                                         *        
036600******************************************************************        
036700 BB-SKRIV-RAD          SECTION.                                           
036800                                                                          
036903*----WX-...=  GAMLA                                                       
036906*----W-....=  PÅGÅENDE                                                    
036911                                                                          
036912     MOVE SPACE            TO DETALJ-RAD                                  
036913     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
036914                                                                          
036915     IF W-IDLEVNR = WX-IDLEVNR                                            
036922        MOVE SPACE      TO RAD-IDLEVNR                                    
036923        IF W-IDFS = WX-IDFS                                               
036924           IF W-RADNR = 1                                                 
036925              MOVE W-IDFS  TO RAD-IDFS                                    
036926           ELSE                                                           
036927              MOVE SPACE   TO RAD-IDFS                                    
036928           END-IF                                                         
036929        ELSE                                                              
036930           MOVE W-IDFS     TO RAD-IDFS                                    
036933        END-IF                                                            
036934     ELSE                                                                 
036935        IF W-RADNR > 1                                                    
036936           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
036937        END-IF                                                            
036938        MOVE W-IDLEVNR     TO RAD-IDLEVNR                                 
036939        MOVE W-IDFS        TO RAD-IDFS                                    
036947     END-IF                                                               
036948                                                                          
036949     MOVE W-IDARTNR        TO RAD-IDARTNR                                 
036950     MOVE W-TOT-ANT        TO RAD-TOT-ANT                                 
036951                                                                          
036960     MOVE DETALJ-RAD       TO WS-LISTRAD                                  
036970     PERFORM S02-SKRIV-RAD                                                
036971                                                                          
037100     .                                                                    
037200     EJECT                                                                
037210******************************************************************        
037220*  SISTA RADEN                                                   *        
037230******************************************************************        
037240 BC-SKRIV-NOT-USER     SECTION.                                           
037250                                                                          
037310*---------------------------------------------                            
037400*----SKRIV RAD NOTERING OCH IDUSER                                        
037410*----NOTERING SKA ALLTID LIGGA PÅ RAD 43                                  
037411*----IDUSER SKA ALLTID LIGGA PÅ RAD 45                                    
037500*---------------------------------------------                            
037600     MOVE W601-TELOSSN             TO RAD-TELOSSN                         
037700                                      RAD-TELOSSN-ENG                     
037710     IF CDC-SE                                                            
037711       MOVE NOT-RAD                  TO WS-LISTRAD                        
037720     ELSE                                                                 
037721       MOVE NOT-RAD-ENG              TO WS-LISTRAD                        
037730     END-IF                                                               
037900     MOVE PRT-EQUAL-43             TO PRT-RADSKIP                         
038000     PERFORM S02-SKRIV-RAD                                                
038100                                                                          
038200     MOVE W601-IDUSER              TO RAD-IDUSER                          
038210     MOVE USER-RAD                 TO WS-LISTRAD                          
038220     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
038230     PERFORM S02-SKRIV-RAD                                                
038500     .                                                                    
038600     EJECT                                                                
038610******************************************************************        
038620*                                                                *        
038630******************************************************************        
038700 BD-KOLLA-OM-NGT-SAKNAS SECTION.                                          
038710                                                                          
038720     IF SEGMENT-FINNS                                                     
038730       MOVE 6110-IDLEVNR  TO W-IDLEVNR-D1                                 
038740       MOVE 6110-IDFS     TO W-IDFS-D1                                    
038750       MOVE 6110-TIAVIDAT TO W-TIAVIDAT-D1                                
038760       MOVE 6110-IDARTNR  TO W-IDARTNR-D1                                 
038761       PERFORM IMS-GU-INLA-W6D111                                         
038762       PERFORM IMS-GNP-INLA-W6D121                                        
038763       PERFORM UNTIL SEGMENT-SAKNAS OR SAKNAT-PARTI                       
038765         IF INLA-RAD-KDINLSTA = 'SAK'                                     
038766           MOVE JA TO SAKNAT-SW                                           
038767         END-IF                                                           
038768         PERFORM IMS-GNP-INLA-W6D121                                      
038769       END-PERFORM                                                        
038770       MOVE SPACE TO STATUS-WS                                            
038771     END-IF                                                               
038780                                                                          
038800     .                                                                    
038900     EJECT                                                                
039800******************************************************************        
039900*                                                                *        
040000******************************************************************        
040100 C-AVSLUTA       SECTION.                                                 
040200                                                                          
040314*-----------------------------------                                      
040315*----STÄNG PRINTER                                                        
040316*-----------------------------------                                      
040320     PERFORM S03-PRT-CLOSE                                                
040500     .                                                                    
040600     EJECT                                                                
040700******************************************************************        
040800******************************************************************        
040900*  IMS-SECTIONER                                                 *        
041000******************************************************************        
041100******************************************************************        
041200                                                                          
043219******************************************************************        
043220*    LASA-PCB                                                    *        
043230******************************************************************        
043240     SKIP3                                                                
043250*----------------------------------------------------------------*        
043260 IMS-GU-LASA-G111      SECTION.                                           
043270     STRING 'W6LASA01(W6GXKEY  =' W-W6GX01KY-X ')'                        
043280          DELIMITED BY SIZE INTO SSA1                                     
043290     STRING 'W6LASA11(W6GXKEY  =' W-W6GX11KY-X ')'                        
043291          DELIMITED BY SIZE INTO SSA2                                     
043292     MOVE '  GE'            TO GODK-STATUSKODER                           
043293     CALL CBLTDLI USING GU  LASA-W6012111-PCB                             
043295                            DLI-IO-AREA-1                                 
043296                            SSA1                                          
043297                            SSA2                                          
043298     MOVE LASA-W6012111-STATUS-CODE  TO STATUS-WS                         
043299     PERFORM IMS-STATUSKONTROLL                                           
043300     .                                                                    
043301     EJECT                                                                
043302     SKIP3                                                                
043310*----------------------------------------------------------------*        
043400 IMS-GNP-LASA-G121       SECTION.                                         
043500     STRING 'W6LASA01(W6GXKEY  =' W-W6GX01KY-X ')'                        
043600          DELIMITED BY SIZE INTO SSA1                                     
043700     STRING 'W6LASA11(W6GXKEY  =' W-W6GX11KY-X ')'                        
043800          DELIMITED BY SIZE INTO SSA2                                     
043900     MOVE 'W6LASA21 '            TO SSA3                                  
044000     MOVE '  GE'            TO GODK-STATUSKODER                           
044100     CALL CBLTDLI USING GNP LASA-W6012111-PCB                             
044200                            DLI-IO-AREA-1                                 
044300                            SSA1                                          
044400                            SSA2                                          
044500                            SSA3                                          
044600     MOVE LASA-W6012111-STATUS-CODE  TO STATUS-WS                         
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     EJECT                                                                
045110                                                                          
045120******************************************************************        
045130*    INLA-PCB                                                    *        
045140******************************************************************        
045150     SKIP3                                                                
045160*----------------------------------------------------------------*        
045170 IMS-GU-INLA-W6D111    SECTION.                                           
045171                                                                          
045180     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
045190          DELIMITED BY SIZE INTO SSA1                                     
045191     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
045192          DELIMITED BY SIZE INTO SSA2                                     
045193     MOVE '  '            TO GODK-STATUSKODER                             
045194     CALL CBLTDLI USING GU  INLA-W6012111-PCB                             
045195                            DLI-IO-AREA-2                                 
045196                            SSA1                                          
045197                            SSA2                                          
045198     MOVE INLA-W6012111-STATUS-CODE  TO STATUS-WS                         
045199     PERFORM IMS-STATUSKONTROLL                                           
045200     .                                                                    
045201     EJECT                                                                
045202     SKIP3                                                                
045203*----------------------------------------------------------------*        
045204 IMS-GNP-INLA-W6D121     SECTION.                                         
045205                                                                          
045209     MOVE 'W6INLA21 '            TO SSA1                                  
045210     MOVE '  GE'            TO GODK-STATUSKODER                           
045211     CALL CBLTDLI USING GNP INLA-W6012111-PCB                             
045212                            DLI-IO-AREA-2                                 
045213                            SSA1                                          
045216     MOVE INLA-W6012111-STATUS-CODE  TO STATUS-WS                         
045217     PERFORM IMS-STATUSKONTROLL                                           
045218     .                                                                    
045219     EJECT                                                                
045220     SKIP3                                                                
045221*----------------------------------------------------------------*        
045230     SKIP3                                                                
045300 IMS-STATUSKONTROLL SECTION.                                              
045400     SKIP2                                                                
045500     SET STATUS-IX TO 1                                                   
045600     SEARCH GODK-STATUS                                                   
045700       AT END                                                             
045800         MOVE 'FEL I W6012111-PGM - W6INLA' TO FELTEXT-STR                
045900         DISPLAY FELTEXT                                                  
046000         CALL FELLOG                                                      
046100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046200         CONTINUE                                                         
046300     END-SEARCH                                                           
046400     .                                                                    
046500     EJECT                                                                
051800                                                                          
051810*----------------------------------------------------------------*        
051900 S01-PRT-OPEN SECTION.                                                    
051910                                                                          
052010                                                                          
052100     CALL W006PRR1  USING PRT-SPOOL-OVR                                   
052200                          PRT-OPEN                                        
052300                          WS-RAPP-PRINTER                                 
052400                          ALT-PCB                                         
052410                          LISB-PCB                                        
052420                          WS-IDPRT                                        
052500                          WS-DUMMY                                        
052600                          WS-DUMMY                                        
052700     .                                                                    
052800     EJECT                                                                
052810*----------------------------------------------------------------*        
052900 S02-SKRIV-RAD SECTION.                                                   
052910                                                                          
053100                                                                          
053200     CALL W006PRR1  USING PRT-SPOOL-OVR                                   
053300                          PRT-WRITE                                       
053310                          WS-RAPP-PRINTER                                 
053320                          ALT-PCB                                         
053330                          LISB-PCB                                        
053400                          WS-IDPRT                                        
053600                          PRT-RADSKIP                                     
053700                          WS-RAD                                          
053800     .                                                                    
053900     EJECT                                                                
053910*----------------------------------------------------------------*        
054000 S03-PRT-CLOSE SECTION.                                                   
054010                                                                          
054110                                                                          
054200     CALL W006PRR1  USING PRT-SPOOL-OVR                                   
054300                          PRT-CLOSE                                       
054310                          WS-RAPP-PRINTER                                 
054320                          ALT-PCB                                         
054330                          LISB-PCB                                        
054400                          WS-IDPRT                                        
054600                          WS-DUMMY                                        
054700                          WS-DUMMY                                        
054800     .                                                                    
054900     EJECT                                                                
