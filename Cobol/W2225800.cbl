000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2225800.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   00/04/26.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000810*                                                                         
000900*    FUNKTION:                                                            
001100*        PROGRAM FÖR ATT TA FRAM MANUELLT SATTA SÄSONGER                  
001200*        DÄR TOM DATUM KOMMER ATT UPPHÖRA INNAN NÄSTA                     
001300*        SÄSONGSBERÄKNING (EN GÅNG VARANNAN MÅNAD)                        
001400*        DETTA PROGRAM KÖRS EN MÅNAD INNAN SÄSONGSBERÄKNINGEN             
001500*        OCH LIGGER TILL GRUND FÖR EN LISTA                               
001600*                                                                         
002900*        PROGRAMMET LÄSER      WDK6                                       
003100*                                                                         
003120*    SUBPROGRAM.                                                          
003130*            W009VADD    ADD AV VECKOR TILL DATUM                         
003140*                                                                         
003200*    ABENDKODER:                                                          
003300*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
003500*        U1000 -  . . . .                                                 
003600*                                                                         
003700                                                                          
003800     SKIP3                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000     SKIP2                                                                
004100 INPUT-OUTPUT SECTION.                                                    
004200                                                                          
004300 FILE-CONTROL.                                                            
004400     SKIP2                                                                
005200                                                                          
005300     SELECT W22258                     ASSIGN TO W22258D1.                
005400*          --- UT-FIL                                                     
005800     EJECT                                                                
005900 DATA DIVISION.                                                           
006000     SKIP3                                                                
006100 FILE SECTION.                                                            
006200     SKIP3                                                                
006300                                                                          
007900                                                                          
008000 FD  W22258                                                               
008100     RECORDING       F                                                    
008200     BLOCK CONTAINS  0.                                                   
008300                                                                          
008400*01  POST -COPY W22258  -PRE UT-    -L.                                   
008410                                                                          
009200     EJECT                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400     SKIP2                                                                
009500*    -COPY WY2000W1                                                       
009600     SKIP2                                                                
009700*    -COPY WY2000W3                                                       
009800     SKIP3                                                                
009900*    -COPY WY2000W2                                                       
010000     SKIP3                                                                
010100 77  IDPGM                       PIC X(8)    VALUE 'W2225800'.            
010200 77  JA                          PIC X       VALUE 'J'.                   
010300 77  NEJ                         PIC X       VALUE 'N'.                   
010400 77  AKTIV                       PIC X       VALUE 'A'.                   
010500                                                                          
010600*    --- INDEX SAMT MAX-INDEX                                             
010700 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
010800 77  INDX                        PIC 9(2)    VALUE ZERO.                  
010810 77  IX                          PIC 9(3)    VALUE ZERO.                  
010900 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
010910 77  PER-INDX                    PIC 9(2)    VALUE ZERO.                  
010920 77  TREND-INDX                  PIC 9(2)    VALUE ZERO.                  
011000 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 11.                    
011100                                                                          
011200 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
011300                                                                          
011400 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
011500 77  MAX-TAB-RADIX               PIC 9(2)    VALUE ZERO.                  
011600                                                                          
011700 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
011800 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
011900                                                                          
012000*    --- SWITCHAR                                                         
012100 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
012200                                                                          
012300 01  DC-POST.                                                             
012400     03  DC-PARAMETER        PIC X(4).                                    
012500         88 SAMTLIGA-DC      VALUE 'ALLA'.                                
012600         88 SAMTLIGA-SDC     VALUE 'EURO'.                                
012700         88 SAMTLIGA-NDC     VALUE 'AMER'.                                
012800         88 ENSTAKA-DC       VALUE 'DC21' 'DC22' 'DC23'                   
012900                                   'DC24' 'DC25' 'DC26'                   
012910                                   'DC3A'                                 
013000                                   'DC41' 'DC42' 'DC43' 'DC51'.           
013100                                                                          
013200     03  FILLER               PIC X(76).                                  
013300                                                                          
013400                                                                          
013500*      --- VALID IDDC CODES                                               
013600*                                                                         
013700*01    -COPY WWDC99                                                       
015700                                                                          
015800 01 DC-PARAMETER-DELAR.                                                   
015900     03 FILLER                PIC X(2)  VALUE SPACE.                      
016000     03 DC-PARAMETER-LAGER    PIC X(2)  VALUE SPACE.                      
016100                                                                          
016200 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
016300     88  INGEN-TREND                         VALUE 'INGEN'.               
016400     88  SVAG-TREND                          VALUE 'SVAG '.               
016500     88  STARK-TREND                         VALUE 'STARK'.               
016600                                                                          
016700 01  INDX-SW                     PIC X       VALUE 'N'.                   
016800     88  INDX-HITTAT                         VALUE 'J'.                   
016900                                                                          
017000 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
017100     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
017200                                                                          
017300 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
017400     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
017500                                                                          
017600*    --- ARBETSFÄLT                                                       
017700 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
017800 01  ARBETSFAELT.                                                         
017900     03  PERIODTABELL            OCCURS 13.                               
018000         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
018100         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
018200         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
018300         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
018400                                                                          
018500     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
018600     03  START-VV                PIC 9(2)    VALUE ZERO.                  
018700                                                                          
018800     03  WS-NOLL                 PIC 9(4)    VALUE ZERO.                  
018810     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
018900     03  FILLER REDEFINES WS-TIAAVV.                                      
019000         05 WS-TIAA              PIC 9(2).                                
019100         05 WS-TIVV              PIC 9(2).                                
019110     03  WS-IDFKNGRP             PIC S9(5)   VALUE ZERO COMP-3.           
019120     03  WS-DAKVARTAL            PIC 9(8)    VALUE ZERO.                  
019130     03  WS-KDERS                PIC S9(3)   VALUE ZERO COMP-3.           
019131     03  WS-IDANSK               PIC S9(3)   VALUE ZERO COMP-3.           
019200                                                                          
019620     03 DAGENS-TISSSSMMDD        PIC 9(8)       VALUE ZERO.               
019630     03 DAGENS-TISSSSMMDD-GRP    REDEFINES DAGENS-TISSSSMMDD.             
019640       05 DAGENS-TISS            PIC 9(2).                                
019650       05 DAGENS-TISSMMDD        PIC 9(6).                                
024800     EJECT                                                                
024900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
025000 01  FILLER REDEFINES DAGENS-DATUM.                                       
025100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
025200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
025300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
025400     EJECT                                                                
025500                                                                          
025600* INFO OM KÖRTYP(DAG) FRPN CONSTANTMEDLEM VALD AV JCL'EN                  
025700* INFON KOMMER SOM FIL D1                                                 
025800                                                                          
025900 01  W271TYP-POST.                                                        
026000     03  TYP-PARAMETER        PIC X(4).                                   
026100         88 DAY-KORNING       VALUE 'DAY '.                               
026200         88 WEEK-KORNING      VALUE 'WEEK'.                               
026300         88 ACC-KORNING       VALUE 'ACC '.                               
026400     03  FILLER               PIC X(76).                                  
026500                                                                          
026600 01  DYNAMISKA-SUBPROGRAM.                                                
026700*                                                                         
026800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
026900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
027100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
027200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
027300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
027400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
027410     03  W222SEAS                PIC X(8)    VALUE 'W222SEAS'.            
027420     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
027500     SKIP2                                                                
027600*    --- PARAMETRAR TILL ABEND                                            
027700                                                                          
027800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
027900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
028000     SKIP2                                                                
028100 01  FELTEXT.                                                             
028200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
028300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
028400     EJECT                                                                
028500*    --- PARAMETRAR TILL DATKORT                                          
028600*                                                                         
028700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22258'.              
028800     SKIP2                                                                
028900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
029000     SKIP2                                                                
029100*01  -COPY WDATKORT                                                       
029200     EJECT                                                                
029300*    --- PARAMETRAR TILL POSTSUM                                          
029400*                                                                         
029500*01  -COPY W0005   -PRE  POSTSUM-                                         
029600     EJECT                                                                
029700*    --- PARAMETRAR TILL WDATKONV                                         
029800*                                                                         
029900*01  -COPY WDATAREA                                                       
030000     EJECT                                                                
030710*    --- PARAMETRAR TILL SUBPROGRAM W222SEAS                              
030720*                                                                         
030730 01  FILLER                      PIC X(16)   VALUE 'W222SEAS'.            
030740     SKIP3                                                                
030750*01 -COPY W222SEAS                                                        
030760     EJECT                                                                
030770*    --- PARAMETRAR TILL W009VADD                                         
030780 01  W009VADDW.                                                           
030790     03  W009VADDW-AAVV      PIC S9(5)               COMP-3.              
030791     03  W009VADDW-ANTAL     PIC S9(3)               COMP-3.              
030800     EJECT                                                                
030900 01  UT-AREA1-START              PIC X(24)   VALUE                        
031000                                 'UT-AREA1-START '.                       
031100     SKIP2                                                                
031200                                                                          
031300*01  AREA -COPY W22258     -PRE UT-                                       
031400     EJECT                                                                
032100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032200                                                                          
032300     SKIP3                                                                
032400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
032500     SKIP3                                                                
032600 01  NYCKLAR-TILL-DLI.                                                    
032700     03  W-IDARTNR-X.                                                     
032800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
032900                                                                          
033300     03  W-KDSEGKEY-X.                                                    
033400         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
033410                                                                          
033420     03  W-IDSKYLT-X.                                                     
033430         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
033500                                                                          
033600*                                                                         
033700     SKIP2                                                                
033800*    --- STATUS-KOD FRÅN IMS                                              
033900 01  STATUS-WS                   PIC XX.                                  
034000     88  SEGMENT-FINNS                       VALUE '  '.                  
034100     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
034200     SKIP2                                                                
034300 01  GODK-STATUSKODER.                                                    
034400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034500     SKIP3                                                                
034600 01  SSA1                        PIC X(64).                               
034700 01  SSA2                        PIC X(64).                               
034710 01  SSA3                        PIC X(64).                               
034800     EJECT                                                                
034900*    --- IMS FUNKTIONSKODER                                               
035000*01  -COPY W0003                                                          
035100     EJECT                                                                
035200*    ---  DLI INPUT-OUTPUT AREA                                           
035210                                                                          
035220 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K6'.        
035230     SKIP3                                                                
035240 01  DLI-IO-AREA-K6.                                                      
035250     03  IO-AREA-K6              PIC X(900)  VALUE SPACE.                 
035260     SKIP3                                                                
035270     03  K601 REDEFINES IO-AREA-K6.                                       
035280*        05  -COPY WDK601                                                 
035290     SKIP3                                                                
035291     03  K611 REDEFINES IO-AREA-K6.                                       
035292*        05  -COPY WDK611                                                 
035293     SKIP3                                                                
035294     03  K626 REDEFINES IO-AREA-K6.                                       
035295*        05  -COPY WDK626                                                 
035296     EJECT                                                                
035297                                                                          
035298 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDD301'.           
035299     SKIP3                                                                
035300 01  DLI-IO-WDD301.                                                       
035400*    03  -COPY WDD301                                                     
035500     EJECT                                                                
035600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDD311'.           
035700     SKIP3                                                                
035800 01  DLI-IO-WDD311.                                                       
035900*    03  -COPY WDD311                                                     
036000     EJECT                                                                
036110                                                                          
038500     EJECT                                                                
038600                                                                          
038700 LINKAGE SECTION.                                                         
038800                                                                          
038900     EJECT                                                                
039000*01  -COPY W0008  -PRE WDK6-                                              
039100     05  WDK6-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
039710     EJECT                                                                
039720*01  -COPY W0008  -PRE WDD3-                                              
039730     05  FILLER                  PIC X.                                   
039800     EJECT                                                                
039900 PROCEDURE DIVISION  USING WDK6-PCB WDD3-PCB.                             
040000     ENTRY 'DLITCBL' USING WDK6-PCB WDD3-PCB.                             
040100                                                                          
040200     PERFORM A-INIT                                                       
040300     PERFORM IMS-GN-WDK6                                                  
040400     PERFORM UNTIL SEGMENT-SAKNAS                                         
040500       EVALUATE WDK6-SEG-NAME-FB                                          
040600         WHEN 'WDK601  '                                                  
040700           MOVE WDK6-KEY-FB-AREA-IDARTNR                                  
040701                             TO W-IDARTNR                                 
040800         WHEN 'WDK611  '                                                  
040900                                                                          
040910           MOVE CLAG-KDERS   TO WS-KDERS                                  
040911           MOVE CLAG-IDANSK  TO WS-IDANSK                                 
040912                                                                          
040920         WHEN 'WDK626  '                                                  
040921                                                                          
040922           IF WS-KDERS < 10                                               
040923           AND JUST-DASPSEA > ZERO                                        
040924           AND JUST-DASPSEA < WS-DAKVARTAL                                
040930              PERFORM C-BEHANDLA-ARTIKEL                                  
040960           END-IF                                                         
043800                                                                          
043900       END-EVALUATE                                                       
044000       PERFORM IMS-GN-WDK6                                                
044100     END-PERFORM                                                          
044200                                                                          
044300     PERFORM Z-FINIT                                                      
044400                                                                          
044500     MOVE ZERO TO RETURN-CODE                                             
044600     GOBACK                                                               
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000                                                                          
049400 A-INIT SECTION.                                                          
049500                                                                          
049800     OPEN OUTPUT W22258                                                   
050000                                                                          
050100     ACCEPT DAGENS-DATUM     FROM DATE                                    
051100                                                                          
051200     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
051300     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
051400                                                                          
051500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
051600                         DAT-O-TIDATUM DAT-KDSVAR                         
051700                                                                          
051800     IF DAT-KDSVAR-OK                                                     
052430*******                                                                   
052440******* TA FRAM DEN FÖRSTA I NÄSTA MÅNAD FÖR SENARE JÄMFÖRELSE            
052450*******                                                                   
052460       MOVE DAT-TIAAVVD (1:4)                                             
052470                             TO W009VADDW-AAVV                            
052480       MOVE 6                TO W009VADDW-ANTAL                           
052490       CALL W009VADD USING W009VADDW-AAVV W009VADDW-ANTAL                 
052491       MOVE W009VADDW-AAVV   TO DAT-I-TIDATUM                             
052492       MOVE 'AAVV'           TO DAT-KDDATFORM                             
052493                                                                          
052494       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
052495                       DAT-O-TIDATUM DAT-KDSVAR                           
052496                                                                          
052497       IF DAT-KDSVAR-OK                                                   
052498*******                                                                   
052499******* TA FRAM DEN FÖRSTA I NÄSTA MÅNAD FÖR SENARE JÄMFÖRELSE            
052500*******                                                                   
052501          MOVE DAT-TISEKEL   TO WS-DAKVARTAL (1:2)                        
052502          MOVE DAT-TIAA      TO WS-DAKVARTAL (3:2)                        
052503          MOVE DAT-TIMM      TO WS-DAKVARTAL (5:2)                        
052504          MOVE 01            TO WS-DAKVARTAL (7:2)                        
052505       ELSE                                                               
052506           STRING ' FEL FRÅN DATUMRUTIN WDATKONV2 '                       
052507           DELIMITED BY SIZE INTO FELTEXT                                 
052508           CALL FELLOG                                                    
052509       END-IF                                                             
052510                                                                          
052520     ELSE                                                                 
052600       MOVE 'SVAR 1 FRÅN WDATKONV I A SECTION EJ OK'                      
052700                         TO FELTEXT-STR                                   
052800       DISPLAY FELTEXT                                                    
052900       PERFORM S99-ABEND                                                  
053000     END-IF                                                               
054500     .                                                                    
054600     EJECT                                                                
074800                                                                          
074900                                                                          
075000 C-BEHANDLA-ARTIKEL SECTION.                                              
075100                                                                          
075110     MOVE W-IDARTNR          TO UT-IDARTNR                                
075111     MOVE JUST-DAMANSEA      TO UT-DAMANSEA                               
075112     MOVE JUST-DASPSEA       TO UT-DASPSEA                                
075113     MOVE WS-IDANSK          TO UT-IDANSK                                 
075114     MOVE SPACE              TO UT-BEART                                  
075115                                                                          
075120     PERFORM IMS-GU-D301                                                  
075130     IF SEGMENT-FINNS                                                     
075140       MOVE 'S  '            TO W-IDSKYLT                                 
075150       PERFORM IMS-GNP-D311                                               
075160       IF SEGMENT-FINNS                                                   
075170         MOVE TEXT-BEART     TO UT-BEART                                  
075400       END-IF                                                             
075500     END-IF                                                               
075510                                                                          
075600     PERFORM S01-SKRIV-W22258                                             
075700     .                                                                    
075800     EJECT                                                                
098646                                                                          
098650 Z-FINIT SECTION.                                                         
098700                                                                          
099000     CLOSE W22258                                                         
099200                                                                          
099300     MOVE 'S' TO POSTSUM-OPKOD                                            
099400     CALL POSTSUM USING POSTSUM-PARM                                      
099500     .                                                                    
099600     EJECT                                                                
130800                                                                          
130900 S01-SKRIV-W22258 SECTION.                                                
131000                                                                          
131100     WRITE UT-POST FROM UT-AREA                                           
131200                                                                          
131300     MOVE 'W22258'   TO POSTSUM-FDNAMN                                    
131400     MOVE 'W22258D1' TO POSTSUM-DDNAMN2                                   
131500     CALL POSTSUM USING POSTSUM-PARM                                      
131600     .                                                                    
133300     EJECT                                                                
133400 S99-ABEND SECTION.                                                       
133500                                                                          
133600     MOVE 'S' TO POSTSUM-OPKOD                                            
133700     CALL POSTSUM USING POSTSUM-PARM                                      
133800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
133900     .                                                                    
134000     EJECT                                                                
134100* --- IMS SEKTIONER ---                                                   
134200     SKIP3                                                                
134300     EJECT                                                                
136400 IMS-GN-WDK6 SECTION.                                                     
136500                                                                          
136600     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA-K6                        
136700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
136800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
136900     PERFORM IMS-STATUSKONTROLL                                           
137000     .                                                                    
138320     EJECT                                                                
138340 IMS-GU-D301 SECTION.                                                     
138350                                                                          
138360     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
138370          DELIMITED BY SIZE INTO SSA1                                     
138380     MOVE '  GE' TO GODK-STATUSKODER                                      
138390     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
138391     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
138392     PERFORM IMS-STATUSKONTROLL                                           
138393     .                                                                    
138394     SKIP3                                                                
138395 IMS-GNP-D311 SECTION.                                                    
138396                                                                          
138397     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
138398          DELIMITED BY SIZE INTO SSA1                                     
138399     MOVE '  GE' TO GODK-STATUSKODER                                      
138400     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
138401     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
138402     PERFORM IMS-STATUSKONTROLL                                           
138403     .                                                                    
138404     EJECT                                                                
138410 IMS-STATUSKONTROLL SECTION.                                              
138500                                                                          
138600     SET STATUS-IX TO 1                                                   
138700     SEARCH GODK-STATUS                                                   
138800       AT END                                                             
138900         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
139000           DELIMITED BY SIZE INTO FELTEXT-STR                             
139100         DISPLAY FELTEXT                                                  
139200         CALL FELLOG                                                      
139300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
139400         CONTINUE                                                         
139500     END-SEARCH                                                           
139600     .                                                                    
139700     EJECT                                                                
139800*    -COPY WY2000P1                                                       
139900     EJECT                                                                
140000*    -COPY WY2000P2                                                       
140100     EJECT                                                                
140200*    -COPY WY2000Q3                                                       
