000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2169000.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   92/10/05.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER WDK601 WDK611 WDK626 WDK627, SKAPAR EN FIL MED             
001100*        UPPGIFTER OM SKROT OCH EN MED UPPGIFTER OM SÄSONG                
001200*        PROGRAMMET KOLLAR HUR WDK626-TISKROT-BEORD FÖRHÅLLER             
001300*        SIG TILL KONSTANTEN WS-SKROT-START-DAG                           
001400*                                                                         
001500*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- FIL MED UPPGIFT OM SKROT                                   
003000     SELECT W21691                     ASSIGN TO W21690D1.                
003100*          --- FIL MED UPPGIFT OM SÄSONG                                  
003200     SELECT W21692                     ASSIGN TO W21690D2.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W21691                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100     SKIP2                                                                
004110*01  POST -COPY W21691 -PRE  W21691-  -L.                                 
004120     EJECT                                                                
004130 FD  W21692                                                               
004140     RECORDING       F                                                    
004150     BLOCK CONTAINS  0.                                                   
004160     SKIP2                                                                
004170*01  POST -COPY W21692 -PRE  W21692-  -L.                                 
004180     EJECT                                                                
004190 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004201*    -COPY WY2000W1                                                       
004210     SKIP3                                                                
004300 77  IDPGM                       PIC X(8)    VALUE 'W2169000'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  IX                          PIC S9(3)   VALUE ZERO.                  
004710                                                                          
004720 01  ARBETSFAELT.                                                         
004730     03  WS-SKROT-START-DAG      PIC S9(7)   VALUE 0920101 COMP-3.        
004740                                                                          
004750                                                                          
004760     EJECT                                                                
004770                                                                          
004780 01  FLAGGOR.                                                             
004790                                                                          
004800     03  FL-FIRST-ARTIKEL        PIC X          VALUE 'J'.                
004900     03  FL-SEASON-FINNS         PIC X          VALUE 'N'.                
005000                                                                          
005010                                                                          
005020 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005030 01  FILLER REDEFINES DAGENS-DATUM.                                       
005040     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005050     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005060     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005070     EJECT                                                                
005080 01  DYNAMISKA-SUBPROGRAM.                                                
005090*                                                                         
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005500     SKIP2                                                                
005600*    --- PARAMETRAR TILL ABEND                                            
005700                                                                          
005800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006000     SKIP2                                                                
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL POSTSUM                                          
006600*                                                                         
006700*01  -COPY W0005   -PRE  POSTSUM-                                         
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)  VALUE 'SPAR-AREOR'.           
007000                                                                          
007100  01 WDK601-AREA  -COPY WDK601.                                           
007200  EJECT                                                                   
007210  01 WDK611-AREA  -COPY WDK611.                                           
007220  EJECT                                                                   
007412 01 TREND.                                                                
007413     03 TREND-SASONG-JUST-AREA.                                           
007414*        05  WDK626-AREA -COPY WDK626.                                    
007415  SKIP3                                                                   
007416 01 SKROT.                                                                
007417     03 SKROTNINGSAREA.                                                   
007418*        05  WDK627-AREA -COPY WDK627.                                    
007419     SKIP3                                                                
007420 01  MATERIAL-NOLL.                                                       
007421     03        -COPY WDK611   -PRE NOLL- .                                
007422                                                                          
007423 01  TREND-NOLL.                                                          
007424     03        -COPY WDK626 -PRE NOLL- .                                  
007425                                                                          
007426 01  SKROT-NOLL.                                                          
007427     03        -COPY WDK627   -PRE NOLL- .                                
007428                                                                          
007429                                                                          
007430 01  UT-AREA-START               PIC X(24)   VALUE                        
007431                                 'UT-AREA-START  '.                       
007432     SKIP2                                                                
007433                                                                          
007434 01  AREA -COPY W21691     -PRE W21691-                                   
007435     EJECT                                                                
007436 01  AREA -COPY W21692     -PRE W21692-                                   
007437     EJECT                                                                
007438*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007439*                                                                         
007440     EJECT                                                                
007450 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007460     SKIP3                                                                
007470 01  NYCKLAR-TILL-DLI.                                                    
007480     03  W-IDARTNR-X.                                                     
007490         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007500     03  W-KDSEGKEY-X.                                                    
007600         05  W-KDSEGKEY          PIC  X(1)   VALUE '1'.                   
007900     03  W-TIPBJUST-X.                                                    
008000         05  W-TIPBJUST          PIC S9(5)   VALUE ZERO COMP-3.           
008100     03  W-TISKROT-X.                                                     
008200         05  W-TISKROT           PIC S9(7)   VALUE ZERO COMP-3.           
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010000     SKIP3                                                                
010100 01  DLI-IO-AREA.                                                         
010200     03  IO-AREA                 PIC X(700)  VALUE SPACE.                 
010300     SKIP3                                                                
010400     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010600                                                                          
010700     EJECT                                                                
010800*01  -COPY W0008  -PRE WDK6-                                              
010900     05  FILLER                  PIC X.                                   
011000     EJECT                                                                
011100 PROCEDURE DIVISION  USING WDK6-PCB.                                      
011200     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
011300                                                                          
011400                                                                          
011500     PERFORM A-INIT                                                       
011600     PERFORM IMS-GET-WDK6                                                 
011700     PERFORM UNTIL SEGMENT-SLUT                                           
011800        EVALUATE WDK6-SEG-NAME-FB                                         
011900           WHEN 'WDK601  '                                                
011901                 IF FL-FIRST-ARTIKEL = JA                                 
011902                    MOVE NEJ TO FL-FIRST-ARTIKEL                          
011903                 ELSE                                                     
011904                    PERFORM B-BEHANDLA-ARTIKEL                            
011905                 END-IF                                                   
011906                 MOVE DLI-IO-AREA TO WDK601-AREA                          
011907                 PERFORM S01-NOLLSTALL                                    
011908           WHEN 'WDK611'                                                  
011909                 MOVE DLI-IO-AREA TO WDK611-AREA                          
011920           WHEN 'WDK626  '                                                
011921                 MOVE DLI-IO-AREA TO WDK626-AREA                          
011922           WHEN 'WDK627  '                                                
011923                 MOVE DLI-IO-AREA TO WDK627-AREA                          
011924        END-EVALUATE                                                      
011925        PERFORM IMS-GET-WDK6                                              
011926     END-PERFORM                                                          
011927     PERFORM B-BEHANDLA-ARTIKEL                                           
011928     PERFORM Z-AVSLUTNING                                                 
011929     MOVE ZERO TO RETURN-CODE                                             
011930     GOBACK                                                               
011940     .                                                                    
011941     EJECT                                                                
011942                                                                          
011943                                                                          
011944                                                                          
011945 A-INIT SECTION.                                                          
011946                                                                          
011947     OPEN OUTPUT W21691 W21692                                            
011948                                                                          
011949     ACCEPT DAGENS-DATUM  FROM DATE                                       
011950                                                                          
011960     MOVE LOW-VALUE    TO WDK611-AREA                                     
011970                          WDK626-AREA                                     
011980                          WDK627-AREA                                     
011990                                                                          
012011     MOVE +1           TO IX                                              
012012     PERFORM UNTIL IX > 12                                                
012013       MOVE ZERO       TO JUST-RESEASON (IX)                              
012014       ADD +1          TO IX                                              
012015     END-PERFORM                                                          
012016                                                                          
012017     MOVE ZERO         TO SKROT-KVSKROT                                   
012018                          SKROT-DASKROT                                   
012019                          SKROT-TISKROT-BEORD                             
012020                                                                          
012021                                                                          
012022     MOVE WDK611-AREA     TO NOLL-CLAG-WDK611                             
012023     MOVE WDK626-AREA     TO NOLL-JUST-WDK626                             
012024     MOVE WDK627-AREA     TO NOLL-SKROT-WDK627                            
012025                                                                          
012028                                                                          
012029     MOVE JA          TO FL-FIRST-ARTIKEL                                 
012030     MOVE NEJ         TO FL-SEASON-FINNS                                  
012040     .                                                                    
012050     EJECT                                                                
012051                                                                          
012052 B-BEHANDLA-ARTIKEL SECTION.                                              
012053                                                                          
012054     PERFORM BA-BEHANDLA-SKROT                                            
012055     PERFORM BB-BEHANDLA-SEASON                                           
012056     .                                                                    
012057     EJECT                                                                
012058                                                                          
012059 BA-BEHANDLA-SKROT SECTION.                                               
012060                                                                          
012061        MOVE SKROT-TISKROT-BEORD   TO TMP1-YYMMDD                         
012062        MOVE WS-SKROT-START-DAG    TO TMP2-YYMMDD                         
012070        PERFORM WY2000P1                                                  
012074        IF TMP1-YYMMDD >= TMP2-YYMMDD                                     
012075           PERFORM S02-FLYTTA-SKROT                                       
012076           PERFORM S11-SKRIV-W21691                                       
012077        END-IF                                                            
012080     .                                                                    
012090     EJECT                                                                
012100                                                                          
012110 BB-BEHANDLA-SEASON SECTION.                                              
012120                                                                          
012123        MOVE +1 TO IX                                                     
012124        PERFORM UNTIL IX > 12                                             
012125           IF JUST-RESEASON(IX) NOT = ALL ZERO                            
012126              MOVE JA TO FL-SEASON-FINNS                                  
012127           END-IF                                                         
012128           ADD +1 TO IX                                                   
012129        END-PERFORM                                                       
012130                                                                          
012131        IF FL-SEASON-FINNS = JA                                           
012132           PERFORM S03-FLYTTA-SEASON                                      
012133           PERFORM S12-SKRIV-W21692                                       
012134           MOVE NEJ         TO FL-SEASON-FINNS                            
012135        END-IF                                                            
012138     .                                                                    
012139     EJECT                                                                
012140                                                                          
012150                                                                          
012160 Z-AVSLUTNING SECTION.                                                    
012170     CLOSE W21691 W21692                                                  
012180     SKIP2                                                                
012190     MOVE 'S' TO POSTSUM-OPKOD                                            
012200     CALL POSTSUM USING POSTSUM-PARM                                      
012210     .                                                                    
012220     EJECT                                                                
012230                                                                          
012240 S01-NOLLSTALL SECTION.                                                   
012250                                                                          
012260     MOVE NOLL-CLAG-WDK611   TO WDK611-AREA                               
012280     MOVE NOLL-JUST-WDK626   TO WDK626-AREA                               
012291     MOVE NOLL-SKROT-WDK627  TO WDK627-AREA                               
012293                                                                          
012296     MOVE NEJ         TO FL-SEASON-FINNS                                  
012297                                                                          
012298     .                                                                    
012299     EJECT                                                                
012300                                                                          
012310 S02-FLYTTA-SKROT SECTION.                                                
012320                                                                          
012330     MOVE ART-IDARTNR        TO W21691-IDARTNR                            
012331     MOVE ART-IDLEVNR        TO W21691-IDLEVNR                            
012332     MOVE +1                 TO W21691-KDCLAGER                           
012333     MOVE ART-IDFTG          TO W21691-IDFTG                              
012334                                                                          
012335     MOVE CLAG-IDANSK        TO W21691-IDANSK                             
012336                                                                          
012337     MOVE CLAG-FLSKROT-BEORD         TO W21691-FLSKROT-BEORD              
012338     MOVE SKROT-TISKROT-BEORD        TO W21691-TISKROT-BEORD              
012339     MOVE SKROT-DASKROT (3:6)        TO W21691-TISKROT                    
012340     MOVE SKROT-KVSKROT              TO W21691-KVSKROT                    
012341     .                                                                    
012342     EJECT                                                                
012343                                                                          
012344                                                                          
012345 S03-FLYTTA-SEASON SECTION.                                               
012346                                                                          
012347     MOVE ART-IDARTNR       TO W21692-IDARTNR                             
012348     MOVE ART-IDFTG         TO W21692-IDFTG                               
012349     MOVE +1                TO W21692-KDCLAGER                            
012350                                                                          
012351     MOVE +1 TO IX                                                        
012352     PERFORM UNTIL IX > 12                                                
012353        MOVE JUST-RESEASON(IX) TO                                         
012354                    W21692-RESEASON(IX)                                   
012355        ADD +1 TO IX                                                      
012356     END-PERFORM                                                          
012357     .                                                                    
012358     EJECT                                                                
012359                                                                          
012360                                                                          
012361 S11-SKRIV-W21691 SECTION.                                                
012362     SKIP2                                                                
012363                                                                          
012364     WRITE W21691-POST FROM W21691-AREA                                   
012365     MOVE '      ' TO  POSTSUM-TRANSTYP                                   
012366     MOVE 'W21691' TO POSTSUM-FDNAMN                                      
012367     MOVE 'W21690D1' TO POSTSUM-DDNAMN2                                   
012368     CALL POSTSUM USING POSTSUM-PARM                                      
012369     .                                                                    
012370     EJECT                                                                
012371                                                                          
012372                                                                          
012373                                                                          
012374 S12-SKRIV-W21692 SECTION.                                                
012380     SKIP2                                                                
012390                                                                          
012400     WRITE W21692-POST FROM W21692-AREA                                   
012500     MOVE '      ' TO  POSTSUM-TRANSTYP                                   
012600     MOVE 'W21692' TO POSTSUM-FDNAMN                                      
012700     MOVE 'W21690D2' TO POSTSUM-DDNAMN2                                   
012800     CALL POSTSUM USING POSTSUM-PARM                                      
012900     .                                                                    
013000     EJECT                                                                
013100                                                                          
013200                                                                          
013300* --- IMS SEKTIONER ---                                                   
013400     SKIP3                                                                
013600 IMS-GET-WDK6   SECTION.                                                  
013700     SKIP2                                                                
013800     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
013900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
014000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014100     PERFORM IMS-STATUSKONTROLL                                           
014200     .                                                                    
014300     EJECT                                                                
014400 IMS-STATUSKONTROLL SECTION.                                              
014500     SKIP2                                                                
014600     SET STATUS-IX TO 1                                                   
014700     SEARCH GODK-STATUS                                                   
014800       AT END                                                             
014900         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
015000         DISPLAY FELTEXT                                                  
015100         CALL FELLOG                                                      
015200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015300         CONTINUE                                                         
015400     END-SEARCH                                                           
015500     .                                                                    
015510     EJECT                                                                
015600*    -COPY WY2000P1                                                       
