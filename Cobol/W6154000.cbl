000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W61540                                                   
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   98/02/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKRIVER EN UTFIL MED LAGERPLATSINFO IFRÅN             
000900*        WDJ8 OCH FREKVENSKOD SAMT LAGRINGSKOD                            
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLLOCA (WDJ8)                              
001200*                              WL6313 WL6314 WL6315 WL6316 (WDR2)         
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- UTFIL .........                                            
002800     SELECT W61541                     ASSIGN TO W61540D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W61541                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  POST -COPY W61540 -PRE  UT1-  -L.                                    
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200                                                                          
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W6154000'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004610                                                                          
004620*01  -COPY WWDCKONS                                                       
004630                                                                          
004700     EJECT                                                                
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005300     EJECT                                                                
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006200                                                                          
006300*    --- MINNESTABELL FÖR DATA IFRÅN WDR2                                 
006400 01  FREKVENSTABELL.                                                      
006600     03 FREKV-TAB1 OCCURS 30.                                             
006700        05  TAB1-KDFREQ   PIC X(2)         VALUE SPACE.                   
006800        05  TAB1-KVPB-FOM PIC S9(6)V9(1) COMP-3                           
006900                                           VALUE ZERO.                    
007000        05  TAB1-KVPB-TOM PIC S9(6)V9(1) COMP-3                           
007100                                           VALUE ZERO.                    
007200        05  TAB1-TEFREQ   PIC X(10)        VALUE SPACE.                   
007300                                                                          
007400*    --- MINNESTABELL TVÅ FÖR DATA IFRÅN WDR2                             
007500 01  STORAGETABELL.                                                       
007700     03 STOR-TAB2 OCCURS 150.                                             
007800        05  TAB2-KDSTOR    PIC X(3)       VALUE SPACE.                    
007900        05  TAB2-TESTORAGE PIC X(18)      VALUE SPACE.                    
008000                                                                          
008100*    --- DC/ FREKVENSINDEX                                                
008300 01  FREKV-IX                    PIC S9(9)   VALUE ZERO.                  
008310 01  STOR-IX                     PIC S9(9)   VALUE ZERO.                  
008400                                                                          
008500*    --- PARAMETRAR TILL ABEND                                            
008600                                                                          
008700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009000     SKIP2                                                                
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL POSTSUM                                          
009600*                                                                         
009700*01  -COPY W0005   -PRE  POSTSUM-                                         
009800     EJECT                                                                
009900 01  UT1-AREA-START              PIC X(24)   VALUE                        
010000                                 'UT1-AREA-START  '.                      
010100     SKIP2                                                                
010200                                                                          
010300*01  AREA -COPY W61540     -PRE UT1-                                      
010400     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011200                                                                          
011300 01  NYCKLAR-TILL-DLI.                                                    
011400     03  W-WDGXKEY-6313-X.                                                
011500         05  WS1-IDHTYP            PIC X(4)    VALUE '6313'.              
011600         05  WS1-IDDC              PIC X(2)    VALUE SPACE.               
011700         05  WS1-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.           
011800                                                                          
011900     03  W-WDGXKEY-6314-X.                                                
012000         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
012100                                                                          
012200     03  W-WDGXKEY-6315-X.                                                
012300         05  WS2-IDHTYP            PIC X(4)    VALUE '6315'.              
012400         05  WS2-IDDC              PIC X(2)    VALUE SPACE.               
012500         05  WS2-6315-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.           
012600                                                                          
012700     03  W-WDGXKEY-6316-X.                                                
012800         05  W-KDSTOR            PIC X(2)    VALUE SPACE.                 
012900                                                                          
013000     03  W-WDJ801KY-X.                                                    
013100         05  W-WDJ801KY          PIC X(11)   VALUE SPACE.                 
013200     SKIP2                                                                
013300                                                                          
013400*    --- STATUS-KOD FRÅN IMS                                              
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FINNS                       VALUE '  '.                  
013700     88  SEGMENT-SAKNAS                      VALUE 'GB'                   
013800                                                   'GE'.                  
013900     SKIP2                                                                
014000 01  GODK-STATUSKODER.                                                    
014100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200     SKIP3                                                                
014300 01  SSA1                        PIC X(64).                               
014400 01  SSA2                        PIC X(64).                               
014500     EJECT                                                                
014600*    --- IMS FUNKTIONSKODER                                               
014700*01  -COPY W0003                                                          
014800     EJECT                                                                
014900*    ---  DLI INPUT-OUTPUT AREA                                           
015000                                                                          
015100 01  FILLER         PIC X(30) VALUE 'WL631301-AREA'.                      
015200 01  WL631301-AREA.                                                       
015300*    03  -COPY WDGX6313                                                   
015400 01  FILLER         PIC X(23) VALUE 'WL631311-AREA'.                      
015500 01  WL631311-AREA.                                                       
015600*    03  -COPY WDGX6314                                                   
015700 01  FILLER         PIC X(30) VALUE 'WL631501-AREA'.                      
015800 01  WL631501-AREA.                                                       
015900*    03  -COPY WDGX6315                                                   
016000 01  FILLER         PIC X(23) VALUE 'WL631511-AREA'.                      
016100 01  WL631511-AREA.                                                       
016200*    03  -COPY WDGX6316                                                   
016300 01  FILLER         PIC X(34) VALUE 'DLI-IO-WLLOCA'.                      
016400 01  DLI-IO-WLLOCA.                                                       
016500*    03  -COPY WDJ801                                                     
016600     EJECT                                                                
016700                                                                          
016800 LINKAGE SECTION.                                                         
016900                                                                          
017000*01  -COPY W0008  -PRE LOCA-                                              
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300*01  -COPY W0008  -PRE 6313-                                              
017400     05  FILLER                  PIC X.                                   
017500     EJECT                                                                
017600*01  -COPY W0008  -PRE 6315-                                              
017700     05  FILLER                  PIC X.                                   
017800     EJECT                                                                
017900                                                                          
018000 PROCEDURE DIVISION  USING LOCA-PCB 6313-PCB 6315-PCB.                    
018100 MAIN SECTION.                                                            
018200     ENTRY 'DLITCBL' USING LOCA-PCB 6313-PCB 6315-PCB.                    
018400                                                                          
018500     PERFORM A-INIT                                                       
018600                                                                          
018700     PERFORM B1-LAES-FLYTTA-TILL-FREKV-TAB1                               
018800     PERFORM B2-LAES-FLYTTA-TILL-STOR-TAB2                                
018900     PERFORM IMS-GN-LOCA                                                  
019000     PERFORM UNTIL SEGMENT-SAKNAS                                         
019100       EVALUATE LOCA-SEG-NAME-FB                                          
019200         WHEN 'WDJ801'                                                    
019210           IF LOC-IDDC = WC-CDC-SE                                        
019300              PERFORM C1-HAEMTA-FREKVENSTABELL-INFO                       
019400              PERFORM C2-HAEMTA-STORAGETABELL-INFO                        
019600              PERFORM D-FLYTTA-TILL-UT1-AREA                              
019700              PERFORM S11-SKRIV-W61540                                    
019800           END-IF                                                         
019900       END-EVALUATE                                                       
020000       PERFORM IMS-GN-LOCA                                                
020100     END-PERFORM                                                          
020200     PERFORM Z-FINIT                                                      
020300                                                                          
020400     MOVE ZERO TO RETURN-CODE                                             
020500     GOBACK                                                               
020600     .                                                                    
020700     EJECT                                                                
020800                                                                          
020900 A-INIT SECTION.                                                          
021000                                                                          
021100     OPEN OUTPUT W61541                                                   
021200                                                                          
021300     ACCEPT DAGENS-DATUM  FROM DATE                                       
021400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021500     .                                                                    
021600     EJECT                                                                
021700                                                                          
021800 B1-LAES-FLYTTA-TILL-FREKV-TAB1 SECTION.                                  
021900                                                                          
022100     MOVE +1        TO FREKV-IX                                           
022300     MOVE WC-CDC-SE TO WS1-IDDC                                           
022310                                                                          
022400     PERFORM IMS-GU-WL631301                                              
022500     IF SEGMENT-FINNS                                                     
022600        PERFORM IMS-GNP-WL631311                                          
022700        PERFORM UNTIL SEGMENT-SAKNAS OR FREKV-IX > 30                     
022800           MOVE 6314-KDFREQ   TO TAB1-KDFREQ   (FREKV-IX)                 
022900           MOVE 6314-KVPB-FOM TO TAB1-KVPB-FOM (FREKV-IX)                 
023000           MOVE 6314-KVPB-TOM TO TAB1-KVPB-TOM (FREKV-IX)                 
023100           MOVE 6314-TEFREQ   TO TAB1-TEFREQ   (FREKV-IX)                 
023200           ADD +1 TO FREKV-IX                                             
023300           PERFORM IMS-GNP-WL631311                                       
023400        END-PERFORM                                                       
023600     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100                                                                          
024110 B2-LAES-FLYTTA-TILL-STOR-TAB2 SECTION.                                   
024120                                                                          
024140     MOVE +1        TO STOR-IX                                            
024160     MOVE WC-CDC-SE TO WS2-IDDC                                           
024161                                                                          
024170     PERFORM IMS-GU-WL631501                                              
024180     IF SEGMENT-FINNS                                                     
024190        PERFORM IMS-GNP-WL631511                                          
024193        PERFORM UNTIL SEGMENT-SAKNAS OR STOR-IX > 150                     
024194           MOVE 6316-KDSTOR     TO TAB2-KDSTOR    (STOR-IX)               
024195           MOVE 6316-TESTORAGE  TO TAB2-TESTORAGE (STOR-IX)               
024198           ADD +1 TO STOR-IX                                              
024199           PERFORM IMS-GNP-WL631511                                       
024200        END-PERFORM                                                       
024201        MOVE +1 TO STOR-IX                                                
024202     END-IF                                                               
024205     .                                                                    
024206     EJECT                                                                
024207                                                                          
024210 C1-HAEMTA-FREKVENSTABELL-INFO SECTION.                                   
024300                                                                          
024500     MOVE +1 TO FREKV-IX                                                  
024800     PERFORM UNTIL FREKV-IX > 30                                          
024900        IF TAB1-KDFREQ (FREKV-IX) = LOC-KDFREQ                            
025000           MOVE TAB1-KVPB-FOM (FREKV-IX) TO UT1-KVPB-FOM                  
025100           MOVE TAB1-KVPB-TOM (FREKV-IX) TO UT1-KVPB-TOM                  
025200           MOVE TAB1-TEFREQ   (FREKV-IX) TO UT1-TEFREQ                    
025300           MOVE 31 TO FREKV-IX                                            
025400        ELSE                                                              
025500           ADD +1 TO FREKV-IX                                             
025600        END-IF                                                            
025700     END-PERFORM                                                          
026200     .                                                                    
026300     EJECT                                                                
026310                                                                          
026320 C2-HAEMTA-STORAGETABELL-INFO SECTION.                                    
026330                                                                          
026350     MOVE +1 TO STOR-IX                                                   
026380     PERFORM UNTIL STOR-IX > 150                                          
026390        IF TAB2-KDSTOR (STOR-IX) = LOC-KDSTOR                             
026393           MOVE TAB2-TESTORAGE (STOR-IX) TO UT1-TESTORAGE                 
026394           MOVE 151 TO STOR-IX                                            
026395        ELSE                                                              
026396           ADD +1 TO STOR-IX                                              
026397        END-IF                                                            
026398     END-PERFORM                                                          
026403     .                                                                    
026404     EJECT                                                                
026410                                                                          
026500 D-FLYTTA-TILL-UT1-AREA SECTION.                                          
026600                                                                          
026700     MOVE LOC-IDDC      TO UT1-IDDC                                       
026800     MOVE LOC-ADLAGOMR  TO UT1-ADLAGOMR                                   
026900     MOVE LOC-ADGANG    TO UT1-ADGANG                                     
027000     MOVE LOC-ADPLATS   TO UT1-ADPLATS                                    
027100     MOVE LOC-KDLOC     TO UT1-KDLOC                                      
027110     MOVE LOC-KDFREQ    TO UT1-KDFREQ                                     
027200     MOVE LOC-KDSTOR    TO UT1-KDSTOR                                     
027300     .                                                                    
027400     EJECT                                                                
027500                                                                          
027600 Z-FINIT SECTION.                                                         
027700     CLOSE W61541                                                         
027800     SKIP2                                                                
027900     MOVE 'S' TO POSTSUM-OPKOD                                            
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     .                                                                    
028200     EJECT                                                                
028300                                                                          
028400 S11-SKRIV-W61540 SECTION.                                                
028500                                                                          
028600     WRITE UT1-POST FROM UT1-AREA                                         
028700                                                                          
028800     MOVE LOC-ADLAGOMR TO POSTSUM-TRANSTYP                                
028900     MOVE 'W61541' TO POSTSUM-FDNAMN                                      
029000     MOVE 'W61540D1' TO POSTSUM-DDNAMN2                                   
029100     CALL POSTSUM USING POSTSUM-PARM                                      
029200     .                                                                    
029300     EJECT                                                                
030300* --- IMS SEKTIONER ---                                                   
030400     SKIP3                                                                
030500     EJECT                                                                
030600                                                                          
030700 IMS-GU-WL631301 SECTION.                                                 
030800                                                                          
030900     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
031000          DELIMITED BY SIZE INTO SSA1                                     
031100     MOVE '  GE' TO GODK-STATUSKODER                                      
031200     CALL CBLTDLI USING GU 6313-PCB WL631301-AREA SSA1                    
031300     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
031400     PERFORM IMS-STATUSKONTROLL                                           
031500     .                                                                    
031600     SKIP3                                                                
031700                                                                          
031800 IMS-GNP-WL631311 SECTION.                                                
031900                                                                          
032000     MOVE 'WL631311 ' TO SSA1                                             
032100     MOVE '  GE' TO GODK-STATUSKODER                                      
032200     CALL CBLTDLI USING GNP 6313-PCB WL631311-AREA SSA1                   
032300     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
032400     PERFORM IMS-STATUSKONTROLL                                           
032500     .                                                                    
032600     SKIP3                                                                
032610                                                                          
032620 IMS-GU-WL631501 SECTION.                                                 
032630                                                                          
032640     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
032650          DELIMITED BY SIZE INTO SSA1                                     
032660     MOVE '  GE' TO GODK-STATUSKODER                                      
032670     CALL CBLTDLI USING GU 6315-PCB WL631501-AREA SSA1                    
032680     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
032690     PERFORM IMS-STATUSKONTROLL                                           
032691     .                                                                    
032692     SKIP3                                                                
032693                                                                          
032694 IMS-GNP-WL631511 SECTION.                                                
032695                                                                          
032696     MOVE 'WL631511 ' TO SSA1                                             
032697     MOVE '  GE' TO GODK-STATUSKODER                                      
032698     CALL CBLTDLI USING GNP 6315-PCB WL631511-AREA SSA1                   
032699     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
032700     PERFORM IMS-STATUSKONTROLL                                           
032701     .                                                                    
032702     SKIP3                                                                
032710 IMS-GN-LOCA   SECTION.                                                   
032800                                                                          
032900     CALL CBLTDLI USING GN LOCA-PCB DLI-IO-WLLOCA                         
033000     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
033100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
033200     PERFORM IMS-STATUSKONTROLL                                           
033300     .                                                                    
033400     EJECT                                                                
033500                                                                          
033600 IMS-STATUSKONTROLL SECTION.                                              
033700                                                                          
033800     SET STATUS-IX TO 1                                                   
033900     SEARCH GODK-STATUS                                                   
034000       AT END                                                             
034100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
034200           DELIMITED BY SIZE INTO FELTEXT                                 
034300         DISPLAY FELTEXT                                                  
034400         CALL FELLOG                                                      
034500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034600         CONTINUE                                                         
034700     END-SEARCH                                                           
034800     .                                                                    
