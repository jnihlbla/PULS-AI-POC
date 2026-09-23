000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4767600.                                                
000300 AUTHOR.         S MOGREN                                                 
000400 DATE-WRITTEN.   050209.                                                  
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPDATING OF GOODS RECEIVAL FOLLOW-UP DATABASE FOR                
000900*        THE WAREHOUSES IN THE US AND CANADA                              
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLINLC (WDL6)                              
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700*    KOPIERAD FRÅN W4758100  05-02                                        
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- FAKTURAINFO                                                
002700     SELECT W47676                     ASSIGN TO W47676D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W47676                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700 01  INFIL.                                                               
003800*03  -COPY W4758301     -L.                                               
003900     SKIP2                                                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300*    -- CHECKED BY WY2000                                                 
004400     SKIP3                                                                
004500 77  IDPGM                       PIC X(8)    VALUE 'W4767600'.            
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900                                                                          
005000 01  CHKP-VAR.                                                            
005100 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005200 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005300 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005400 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005500 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005600 03  CHKP-MAX                    PIC S9(3)   VALUE +10.                   
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005810 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005820 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005900                                                                          
006000*01  -COPY WWDCKONS                                                       
006100                                                                          
006200 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
006300 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
006400                                                                          
006500 01  WS-ARBETSAREA.                                                       
006600*    DATE + TIME  FÖR SKAPANDE AV INLEVERANSNUMMER                        
006700     03  WS-TIAAAAMMDDTTMMSSTH   PIC 9(16)  VALUE ZERO.                   
006800     03  FILLER REDEFINES WS-TIAAAAMMDDTTMMSSTH.                          
006900         05  WS-TISEKEL          PIC 9(2).                                
007000         05  WS-TIAAMMDD-DATE    PIC 9(6).                                
007100         05  WS-TTMMSSTH-TIME    PIC 9(8).                                
007200     03  WS-DAINLEV              PIC 9(16) VALUE ZERO.                    
007300                                                                          
007400 77  W47676-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W47676                       VALUE 'J'.                   
007600     EJECT                                                                
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200     SKIP3                                                                
008300 01  WS-AAMMDD                   PIC 9(6).                                
008400 01  FILLER REDEFINES WS-AAMMDD.                                          
008500     03  WS-AA               PIC 9(2).                                    
008600     03  FILLER              PIC 9(4).                                    
008700     EJECT                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900*                                                                         
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009400     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
009410     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
009500     EJECT                                                                
009600*    --- PARAMETRAR TILL POSTSUM                                          
009700*                                                                         
009800*01  -COPY W0005   -PRE  POSTSUM-                                         
009900     EJECT                                                                
009910*    --- PARAMETRAR TILL W510CURR                                         
009920*01  -COPY W510CURR                                                       
010000*    --- PARAMETRAR TILL SUBPROGRAM W218ETA                               
010100*                                                                         
010200 01  FILLER                    PIC X(16) VALUE 'START W218LETA  '.        
010300*01  -COPY W218LETA  -PRE ETA-                                            
010310 01  FILLER                    PIC X(12) VALUE 'DUMMY ARTC'.              
010320 01  ETA-ARTC-PCB              PIC X(1).                                  
010330 01  FILLER                    PIC X(12) VALUE 'DUMMY ARTS'.              
010340 01  ETA-ARTS-PCB              PIC X(1).                                  
010350 01  FILLER                    PIC X(12) VALUE 'DUMMY INLC'.              
010360 01  ETA-INLC-PCB              PIC X(1).                                  
010370 01  FILLER                    PIC X(12) VALUE 'DUMMY LEVA'.              
010380 01  ETA-LEVA-PCB              PIC X(1).                                  
010400     EJECT                                                                
010500 01  IN-AREA-START               PIC X(24)   VALUE                        
010600                                             'IN-AREA-START'.             
010700     SKIP2                                                                
010800 01  IN-AREA -COPY W4758301      -PRE FAKT-.                              
010900     EJECT                                                                
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011200     SKIP3                                                                
011300 01  NYCKLAR-TILL-DLI.                                                    
011400                                                                          
011500     03  W-IDARTNR-X.                                                     
011600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011700                                                                          
011800     03  W-DAINLEV-X.                                                     
011900         05  W-DAINLEV           PIC 9(16)  VALUE ZERO.                   
012000                                                                          
013100     03  W-IDDC-B6-X.                                                     
013200         05 W-IDDC-B6                  PIC X(2).                          
013300                                                                          
013400     03  W-IDDC-B6-REC-X.                                                 
013500         05 W-IDDC-B6-REC              PIC X(2).                          
013600                                                                          
013700     03  W-IDDC-B6-SEND-X.                                                
013800         05 W-IDDC-B6-SEND             PIC X(2).                          
013900     SKIP2                                                                
014000*    --- STATUS-KOD FRÅN IMS                                              
014100 01  STATUS-WS                   PIC XX.                                  
014200     88  SEGMENT-FINNS                       VALUE '  '.                  
014300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014600     88  IMS-EJ-OK                           VALUE 'XD'.                  
014700     SKIP2                                                                
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015000     SKIP3                                                                
015100 01  SSA1                        PIC X(64).                               
015200 01  SSA2                        PIC X(64).                               
015300     EJECT                                                                
015400*    --- IMS FUNKTIONSKODER                                               
015500*01  -COPY W0003                                                          
015600     EJECT                                                                
015700*    ---  DLI INPUT-OUTPUT AREA                                           
015800 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDL601'.               
015900 01  DLI-IO-AREA-WDL601.                                                  
016000*    03  -COPY WDL601  -PRE INLC-                                         
016100     EJECT                                                                
016200 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDL611'.               
016300 01  DLI-IO-AREA-WDL611.                                                  
016400*    03  -COPY WDL611  -PRE INLC-                                         
016500     EJECT                                                                
017000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017100 01   DLI-IO-AREA-B601.                                                   
017200*     03  -COPY WDB601                                                    
017300                                                                          
017400 01  FILLER               PIC X(16)   VALUE 'WDB601 REC '.                
017500 01   DLI-IO-AREA-B601-REC.                                               
017600*     03  -COPY WDB601   -PRE REC-                                        
017700                                                                          
017800 01  FILLER               PIC X(16)   VALUE 'WDB601 SEND'.                
017900 01   DLI-IO-AREA-B601-SEND.                                              
018000*     03  -COPY WDB601   -PRE SEND-                                       
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400*01  -COPY W0009   -PRE MSG-                                              
018500     EJECT                                                                
018600*01  -COPY W0008  -PRE WDL6-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008  -PRE WDG2-                                              
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008  -PRE WDB6-                                              
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500 01  ETA-WDB6-PCB                PIC X.                                   
019510     EJECT                                                                
019600 PROCEDURE DIVISION  USING MSG-PCB WDL6-PCB WDG2-PCB WDB6-PCB             
019610                           ETA-WDB6-PCB.                                  
019700 MAIN SECTION.                                                            
019800     ENTRY 'DLITCBL' USING MSG-PCB WDL6-PCB WDG2-PCB WDB6-PCB             
019810                           ETA-WDB6-PCB.                                  
019900                                                                          
020000     PERFORM A-INIT                                                       
020100     PERFORM S01-LAES-W47676                                              
020200     IF NOT END-OF-W47676                                                 
020300                                                                          
020400       PERFORM UNTIL END-OF-W47676                                        
020500         IF CHKP-ANT > CHKP-MAX                                           
020600           PERFORM X-TAG-CHECKPOINT                                       
020700         END-IF                                                           
020800                                                                          
020900         PERFORM B-BEHANDLA-INFIL                                         
021000                                                                          
021100         PERFORM S01-LAES-W47676                                          
021200       END-PERFORM                                                        
021300                                                                          
021400     END-IF                                                               
021500                                                                          
021600     PERFORM Z-FINIT                                                      
021700                                                                          
021800     MOVE ZERO TO RETURN-CODE                                             
021900     GOBACK                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 A-INIT SECTION.                                                          
022300     SKIP2                                                                
022400                                                                          
022500     PERFORM IMS-RESTART                                                  
022600                                                                          
022700     OPEN INPUT W47676                                                    
022800                                                                          
022900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023000                                                                          
023100     ACCEPT DAGENS-DATUM FROM DATE                                        
023200                                                                          
023310     MOVE DAGENS-DATUM-AAR    TO W-DATE-AAMM(1:2)                         
023320     MOVE DAGENS-DATUM-MAANAD TO W-DATE-AAMM(3:2)                         
023400     .                                                                    
023500     EJECT                                                                
023600 B-BEHANDLA-INFIL SECTION.                                                
023700     SKIP2                                                                
023800     MOVE FAKT-IDARTNR TO INLC-ART-IDARTNR                                
023900                          W-IDARTNR                                       
024000                                                                          
024100     PERFORM IMS-GHU-WDL601                                               
024200                                                                          
024300     IF SEGMENT-SAKNAS                                                    
024400       PERFORM IMS-ISRT-WDL601                                            
024500       ADD +1 TO CHKP-ANT                                                 
024600     END-IF                                                               
024700                                                                          
024800     PERFORM BA-SKAPA-INLEVNR                                             
024900                                                                          
025000     MOVE NEJ               TO INLC-INL-FLMAKUL                           
025100                               INLC-INL-FLSKAKOL                          
025200                               INLC-INL-FLPRIO                            
025210                               INLC-INL-FLTULLST                          
025300                                                                          
025400     MOVE ZERO              TO INLC-INL-ADLAGOMR                          
025500                               INLC-INL-ADGANG                            
025600                               INLC-INL-ADPLATS                           
025700                               INLC-INL-KVANTMOT                          
025800                               INLC-INL-KVART-SKROT                       
025900                               INLC-INL-TIINLMOT                          
026000                               INLC-INL-TIINLINL                          
026100                               INLC-INL-KDRT                              
026200                               INLC-INL-IDLOPNRM                          
026300                               INLC-INL-TIINLMTI                          
026400                               INLC-INL-TIINLITI                          
026610                               INLC-INL-KVTULRET                          
026611                               INLC-INL-KVRETUR                           
026620                               INLC-INL-KDAVVANT                          
026630                               INLC-INL-TIAVIDAT                          
026640                               INLC-INL-IDKONTO                           
026700     MOVE SPACE             TO INLC-INL-IDUSER-003                        
026701                               INLC-INL-ADINLOMR                          
026702                               INLC-INL-IDKST                             
026720                               INLC-INL-IDANALYS                          
026730                               INLC-INL-IDDC-LEV                          
026800                                                                          
026900     MOVE FAKT-IDDC-REC     TO INLC-INL-IDDC                              
027000                                                                          
027100     IF FAKT-IDDC-REC NOT = REC-DCS-IDDC                                  
027200        MOVE FAKT-IDDC-REC  TO W-IDDC-B6-REC                              
027300        PERFORM IMS-GU-WDB601-REC                                         
027400     END-IF                                                               
027500     IF FAKT-IDDC-SEND NOT = SEND-DCS-IDDC                                
027600        MOVE FAKT-IDDC-SEND  TO W-IDDC-B6-SEND                            
027700        PERFORM IMS-GU-WDB601-SEND                                        
027800     END-IF                                                               
027900     MOVE FAKT-IDFAKT       TO INLC-INL-IDFAKT                            
028000     MOVE FAKT-IDDISTR      TO INLC-INL-IDDISTR                           
028100     MOVE FAKT-IDKUNDNR     TO INLC-INL-IDKUNDNR                          
028200     MOVE FAKT-IDKUNDRF     TO INLC-INL-IDKUNDRF                          
028300     MOVE FAKT-IDPTYP       TO INLC-INL-IDPTYP                            
028400     MOVE FAKT-KDFRAKT      TO INLC-INL-KDFRAKT                           
028500     MOVE FAKT-IDKOLLI      TO INLC-INL-IDKOLLI                           
028600     MOVE FAKT-KDKOLLI      TO INLC-INL-KDKOLLI                           
028700     MOVE FAKT-KVAVIS       TO INLC-INL-KVAVIS                            
028800     MOVE FAKT-KDVALISO     TO INLC-INL-KDVALISO                          
028900                                                                          
029000     PERFORM BC-BERAKNA-ETA                                               
029100     IF ETA-SVAR-OK = SPACE OR JA                                         
029200       MOVE ETA-TIAAMMDD-SVAR  TO INLC-INL-TIBERANK                       
029300     ELSE                                                                 
029310       MOVE DAGENS-DATUM       TO INLC-INL-TIBERANK                       
029400*      MOVE ' FEL I SUBPGM W218ETA   ' TO FELTEXT                         
029500*      CALL  ABEND USING RKOD-ABEND-MED-DUMP                              
029600     END-IF                                                               
029700                                                                          
029800     IF SEND-DCS-NDC-NA                                                   
029900       MOVE FAKT-PRAVCOST     TO INLC-INL-PRARTNTO                        
030000       PERFORM BB-BERAKNA-KURS                                            
030100     ELSE                                                                 
030200                                                                          
030300       MOVE FAKT-PRARTNTO     TO INLC-INL-PRARTNTO                        
030400       MOVE FAKT-PRKURS       TO INLC-INL-PRKURS                          
030500     END-IF                                                               
030600                                                                          
030700     IF SEND-DCS-DDC                                                      
030800       MOVE WC-CDC-SE         TO W-IDDC-B6                                
030900     ELSE                                                                 
031000       MOVE FAKT-IDDC-SEND    TO W-IDDC-B6                                
031100     END-IF                                                               
031200                                                                          
031300     IF DCS-IDDC NOT = W-IDDC-B6                                          
031400        PERFORM IMS-GU-WDB601                                             
031500     END-IF                                                               
031600     MOVE DCS-IDLEVNR-DC TO INLC-INL-IDLEVNR                              
031700*    MOVE DCS-FILLER(1:5) TO INLC-INL-IDLEVNR                             
031800                                                                          
031900     PERFORM IMS-ISRT-WDL611                                              
032000                                                                          
032100     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
032200       IF SEGMENT-FINNS-REDAN                                             
032300         SUBTRACT 1 FROM INLC-INL-DAINLEV                                 
032400         SUBTRACT 1 FROM W-DAINLEV                                        
032500         PERFORM IMS-ISRT-WDL611                                          
032600       END-IF                                                             
032700     END-PERFORM                                                          
032800                                                                          
032900     ADD +1 TO CHKP-ANT                                                   
033000     .                                                                    
033100     EJECT                                                                
033200 BA-SKAPA-INLEVNR SECTION.                                                
033300     SKIP2                                                                
033400     ACCEPT WS-TIAAMMDD-DATE FROM DATE                                    
033500     ACCEPT WS-TTMMSSTH-TIME FROM TIME                                    
033600     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
033700                                                                          
033800     COMPUTE WS-DAINLEV = 9999999999999999                                
033900                        - WS-TIAAAAMMDDTTMMSSTH                           
034000                                                                          
034100     MOVE WS-DAINLEV TO INLC-INL-DAINLEV                                  
034200                        W-DAINLEV                                         
034300     .                                                                    
034400     EJECT                                                                
034500 BB-BERAKNA-KURS SECTION.                                                 
034600                                                                          
034700**********  HÄR BESTÄMS VILKEN KURS SOM SKALL GÄLLA FÖR                   
034800**********  TRANSFERS INOM NORDAMERIKA                                    
034900                                                                          
035000     IF SEND-DCS-NDC-NA AND SEND-DCS-USA AND                              
035100        REC-DCS-NDC-NA  AND REC-DCS-USA                                   
035200       MOVE 1.00000                   TO INLC-INL-PRKURS                  
035300                                                                          
035400     ELSE                                                                 
035500       IF SEND-DCS-NDC-NA AND SEND-DCS-USA                                
035600         MOVE 'USD'                   TO CURR-KDVALISO-ROW                
035700       ELSE                                                               
035800         IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                           
035900           MOVE 'CAD'                 TO CURR-KDVALISO-ROW                
036000         END-IF                                                           
036100       END-IF                                                             
036200                                                                          
036300       MOVE W-DATE-AAMM               TO CURR-TIAAMM                      
036400       MOVE WS-KDVALISO-HUV           TO CURR-KDVALISO-HUV                
036500       MOVE 'M'                       TO CURR-KDVALTYP                    
036510                                                                          
036520       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
036530       IF CURR-KDSVAR = ' '                                               
036540         CONTINUE                                                         
036550       ELSE                                                               
036560         MOVE 1                       TO CURR-PRKURS-NEW                  
036570       END-IF                                                             
036700       COMPUTE INLC-INL-PRKURS ROUNDED =                                  
036800            CURR-PRKURS-NEW / FAKT-PRKURS                                 
036900     END-IF                                                               
037000     .                                                                    
037100     EJECT                                                                
037200 BC-BERAKNA-ETA SECTION.                                                  
037300                                                                          
037400**** SKICKA MED FAKT-TIFAKT TILL SUBMODUL W218ETA OCH                     
037500**** FÅ TILLBAKA TIBERANK                                                 
037600                                                                          
037700     IF FAKT-KDORDKL-MAX < 2                                              
037800       MOVE 603              TO ETA-KDCALL                                
037900     ELSE                                                                 
038000       MOVE 604              TO ETA-KDCALL                                
038100     END-IF                                                               
038200     MOVE FAKT-IDDC-SEND     TO ETA-IDDC-SEND                             
038300     MOVE FAKT-IDDC-REC      TO ETA-IDDC-REC                              
038400     MOVE FAKT-IDARTNR       TO ETA-IDARTNR                               
038500     MOVE SPACE              TO ETA-IDLEVNR                               
038600     MOVE 0                  TO ETA-KDFRAKT                               
038700     MOVE FAKT-TIFAKT        TO ETA-TIAAMMDD-ANROP                        
038800                                WS-AAMMDD                                 
038900     IF WS-AA < 50                                                        
039000       MOVE 20               TO ETA-TISEKEL-ANROP                         
039100     ELSE                                                                 
039200       MOVE 19               TO ETA-TISEKEL-ANROP                         
039300     END-IF                                                               
039400                                                                          
039500     CALL W218ETA USING ETA-W218LETA ETA-ARTC-PCB ETA-ARTS-PCB            
039501                                     ETA-INLC-PCB ETA-LEVA-PCB            
039510                                     ETA-WDB6-PCB                         
039600     .                                                                    
039700     EJECT                                                                
039800 Z-FINIT SECTION.                                                         
039900                                                                          
040000                                                                          
040100     CLOSE W47676                                                         
040200     SKIP2                                                                
040300     MOVE 'S' TO POSTSUM-OPKOD                                            
040400     CALL POSTSUM USING POSTSUM-PARM                                      
040500     .                                                                    
040600     EJECT                                                                
040700 S01-LAES-W47676  SECTION.                                                
040800     SKIP2                                                                
040900     READ W47676 INTO FAKT-IN-AREA                                        
041000     AT END                                                               
041100        MOVE JA TO W47676-EOF-SW                                          
041200                                                                          
041300     NOT AT END                                                           
041400        MOVE 'W47676' TO POSTSUM-FDNAMN                                   
041500        MOVE 'W47676D1' TO POSTSUM-DDNAMN2                                
041600        MOVE FAKT-IDPTYP TO POSTSUM-TRANSTYP                              
041700        CALL POSTSUM USING POSTSUM-PARM                                   
041800                                                                          
041900     END-READ                                                             
042000     .                                                                    
042100     EJECT                                                                
042200 X-TAG-CHECKPOINT   SECTION.                                              
042300                                                                          
042400     PERFORM IMS-CHECKPOINT                                               
042500     MOVE ZERO TO CHKP-ANT                                                
042600     .                                                                    
042700     EJECT                                                                
042800* --- IMS SEKTIONER ---                                                   
042900     SKIP3                                                                
043000 IMS-GHU-WDL601 SECTION.                                                  
043100                                                                          
043200     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
043300          DELIMITED BY SIZE INTO SSA1                                     
043400     MOVE '  GE' TO GODK-STATUSKODER                                      
043500     CALL CBLTDLI USING GHU                                               
043600                        WDL6-PCB                                          
043700                        DLI-IO-AREA-WDL601                                
043800                        SSA1                                              
043900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     .                                                                    
044200     SKIP3                                                                
044300 IMS-ISRT-WDL601 SECTION.                                                 
044400                                                                          
044500     MOVE 'WDL601   ' TO SSA1                                             
044600     MOVE '  II' TO GODK-STATUSKODER                                      
044700     CALL CBLTDLI USING ISRT                                              
044800                        WDL6-PCB                                          
044900                        DLI-IO-AREA-WDL601                                
045000                        SSA1                                              
045100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
045200     PERFORM IMS-STATUSKONTROLL                                           
045300     .                                                                    
045400     EJECT                                                                
045500 IMS-ISRT-WDL611 SECTION.                                                 
045600                                                                          
045700     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
045800          DELIMITED BY SIZE INTO SSA1                                     
045900     MOVE 'WDL611   ' TO SSA2                                             
046000     MOVE '  II' TO GODK-STATUSKODER                                      
046100     CALL CBLTDLI USING ISRT                                              
046200                        WDL6-PCB                                          
046300                        DLI-IO-AREA-WDL611                                
046400                        SSA1                                              
046500                        SSA2                                              
046600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
046700     PERFORM IMS-STATUSKONTROLL                                           
046800     .                                                                    
046900     EJECT                                                                
048600 IMS-RESTART SECTION.                                                     
048700     SKIP2                                                                
048800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
048900     MOVE '  ' TO GODK-STATUSKODER                                        
049000     CALL CBLTDLI USING XRST MSG-PCB                                      
049100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
049200                        CHKP-AREA-LENGTH CHKP-AREA                        
049300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049400     PERFORM IMS-STATUSKONTROLL                                           
049500     .                                                                    
049600     EJECT                                                                
049700 IMS-CHECKPOINT SECTION.                                                  
049800     SKIP2                                                                
049900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
050000     MOVE '  XD' TO GODK-STATUSKODER                                      
050100     CALL CBLTDLI USING CHKP MSG-PCB                                      
050200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
050300                        CHKP-AREA-LENGTH CHKP-AREA                        
050400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050500     PERFORM IMS-STATUSKONTROLL                                           
050600                                                                          
050700     IF IMS-EJ-OK                                                         
050800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
050900       DISPLAY FELTEXT                                                    
051000       CALL FELLOG                                                        
051100     END-IF                                                               
051200     .                                                                    
051300 IMS-GU-WDB601    SECTION.                                                
051400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
051500          DELIMITED BY SIZE INTO SSA1                                     
051600     MOVE '  ' TO GODK-STATUSKODER                                        
051700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
051800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
051900     PERFORM IMS-STATUSKONTROLL                                           
052000     IF SEGMENT-SAKNAS                                                    
052100         MOVE SPACE TO DCS-KDDC                                           
052200                       DCS-IDLEVNR-DC                                     
052300     END-IF                                                               
052400     .                                                                    
052500 IMS-GU-WDB601-REC    SECTION.                                            
052600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-REC-X ')'                     
052700          DELIMITED BY SIZE INTO SSA1                                     
052800     MOVE '  GE' TO GODK-STATUSKODER                                      
052900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-REC SSA1             
053000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     IF SEGMENT-SAKNAS                                                    
053300         MOVE SPACE TO REC-DCS-KDDC                                       
053400     END-IF                                                               
053500     .                                                                    
053600 IMS-GU-WDB601-SEND   SECTION.                                            
053700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-SEND-X ')'                    
053800          DELIMITED BY SIZE INTO SSA1                                     
053900     MOVE '  GE' TO GODK-STATUSKODER                                      
054000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
054100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
054200     PERFORM IMS-STATUSKONTROLL                                           
054300     IF SEGMENT-SAKNAS                                                    
054400         MOVE SPACE TO SEND-DCS-KDDC                                      
054500     END-IF                                                               
054600     .                                                                    
054700     EJECT                                                                
054800 IMS-STATUSKONTROLL SECTION.                                              
054900     SKIP2                                                                
055000     SET STATUS-IX TO 1                                                   
055100     SEARCH GODK-STATUS                                                   
055200       AT END                                                             
055300         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
055400         DISPLAY FELTEXT                                                  
055500         CALL FELLOG                                                      
055600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055700         CONTINUE                                                         
055800     END-SEARCH                                                           
055900     .                                                                    
