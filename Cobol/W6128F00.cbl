000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6128F00.                                                
000400 AUTHOR.         KJELL.                                                   
000500 DATE-WRITTEN.   2011-10-13                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*FUNKTION:                                                                
001000*                                                                         
001100*   SKAPAR DATA FÖR REFILL/AK FOLLOW-UP-RAPPORTER                         
001200*   FÖR SENARE UTSKRIFT TILL PAPPER/MAIL/WEB                              
001300*   PROGRAMMET ÄR EN DEL AV W61286 SOM DELATS UPP                         
001400*   I FLERA DELAR. NEDANSTGÅENDE ÄNDRINGAR GÄLLER DET                     
001500*   URSPRUNGLIGA PROGRAMMET.                                              
001600*                                                                         
001700*    ÄNDRING:   JOHAN L 25/6 -98                                          
001800*               NY KOLUMN, "BINNED PRIO LINES"                            
001900*                                                                         
002000*    ETRACKER: 8218038 CORRECTION OF PERIOD                               
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- INFIL SORTERAD PÅ DC                                       
003100     SELECT W61284                     ASSIGN TO W6128FD1.                
003200     SKIP2                                                                
003300*          --- UTFIL IHOPSUMMERAT PER DC                                  
003400     SELECT W6128F                     ASSIGN TO W6128FD2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W61284                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W61265      -L.                                                
004500     SKIP3                                                                
004600 FD  W6128F                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000 01  UT-POST.                                                             
005100*    03 -COPY W6128F      -L.                                             
005200                                                                          
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W6128F00'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  KDRC-DISPLAY                PIC Z(5).                                
006000 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
006100                                                                          
006200 77  WDCS-KDMFUP                 PIC X(2)    VALUE SPACE.                 
006300                                                                          
006400*    -- INDEX IN THE TABLE BELOW.                                         
006500*    -- 1=AIR, 2=BOAT, 3=TRANSFER 4=OTHER 5=TOTAL, SUM OF 1-4             
006600 01  I                           PIC S9  COMP-3.                          
006700 01  MAX-I                       PIC S9  COMP-3    VALUE 5.               
006800*    -- CONSTANTS FOR VALUES OF I                                         
006900 01  IA                          PIC S9  COMP-3    VALUE 1.               
007000 01  IB                          PIC S9  COMP-3    VALUE 2.               
007100 01  IT                          PIC S9  COMP-3    VALUE 3.               
007200 01  IZ                          PIC S9  COMP-3    VALUE 4.               
007300                                                                          
007400*    -- TABLE FOR SAVING ACCUMULATED VALUES PER DC                        
007500 01  W-DC-ACCUM-TAB.                                                      
007600     03 FILLER OCCURS 5.                                                  
007700       05 W-KVANTAL-LINES-AK     PIC S9(7)         COMP-3.                
007800       05 W-KVANTAL-LINES-BINNED PIC S9(7)         COMP-3.                
007900       05 W-KVANTAL-LINES-PRIO   PIC S9(7)         COMP-3.                
008000       05 W-KVDAGDEC-DAYS-BINNED PIC S9(4)V9(1)    COMP-3.                
008100       05 W-KVDAGDEC-DAYS-PRIO   PIC S9(4)V9(1)    COMP-3.                
008200       05 W-SUARTNTO-BINNED      PIC S9(9)V9(2)    COMP-3.                
008300                                                                          
008400 01  SPAR-IDDC                   PIC XX      VALUE SPACE.                 
008500                                                                          
008600 01  TEMP-SUARTNTO-BINNED        PIC S9(9)V9(2)    COMP-3.                
008700                                                                          
008800 01  FL-OK-POST                  PIC X       VALUE 'J'.                   
008900                                                                          
009000 01  W61284-EOF-SW               PIC X       VALUE 'N'.                   
009100     88  END-OF-W61284                       VALUE 'J'.                   
009200                                                                          
009300                                                                          
009400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009500 01  FILLER REDEFINES DAGENS-DATUM.                                       
009600     03  DATUM-AAR               PIC 9(2).                                
009700     03  DATUM-MAANAD            PIC 9(2).                                
009800     03  DATUM-DAG               PIC 9(2).                                
009900                                                                          
010000 01  DAGENS-PERIOD               PIC 9(4)    VALUE ZERO.                  
010100                                                                          
010200     EJECT                                                                
010300 01  WS-YYMMDDHHMM.                                                       
010400     03 WS-YYMMDD                PIC  9(6).                               
010500     03 WS-TIME                  PIC  9(4).                               
010600                                                                          
010700 01  WS-HHMMSSTH.                                                         
010800     03 WS-HHMM                  PIC  9(4).                               
010900     03 WS-SSTH                  PIC  9(4).                               
011000                                                                          
011100     EJECT                                                                
011200 01  DYNAMISKA-SUBPROGRAM.                                                
011300*                                                                         
011400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011800     03  W612TIME                PIC X(8)    VALUE 'W612TIME'.            
011900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012100     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
012200                                                                          
012300*    --- PARAMETRAR TILL ABEND                                            
012400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012900                                                                          
013000 01  FELTEXT.                                                             
013100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013300                                                                          
013400                                                                          
013500*    --- PARAMETRAR TILL DATKORT                                          
013600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W6128F'.              
013700 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
013800                                                                          
013900*01  -COPY WDATKORT                                                       
014000     EJECT                                                                
014100*    --- PARAMETRAR TILL WDATKONV                                         
014200*01  -COPY WDATAREA                                                       
014300     EJECT                                                                
014400*    --- PARAMETRAR TILL POSTSUM                                          
014500*01  -COPY W0005   -PRE  POSTSUM-                                         
014600     EJECT                                                                
014700*    --- PARAMETRAR TILL W612TIME                                         
014800*01  -COPY W612TID     -PRE TIME-                                         
014900     EJECT                                                                
015000*    --- PARAMETRAR TILL WL10WBDC                                         
015100*01  -COPY WL10WBDC                                                       
015200     EJECT                                                                
015300*01  -COPY WWDIST35                                                       
015400                                                                          
015500*01  -COPY WWDC99                                                         
015600                                                                          
015700     EJECT                                                                
015800 01  IN-AREA-START               PIC X(24)   VALUE                        
015900                                 'IN-AREA-START  '.                       
016000                                                                          
016100*01  AREA -COPY W61265     -PRE IN-SHIST-                                 
016200                                                                          
016300     EJECT                                                                
016400 01  UT-AREA-START               PIC X(24)   VALUE                        
016500                                 'UT-AREA-START  '.                       
016600                                                                          
016700*01  AREA -COPY W6128F     -PRE UT-                                       
016800                                                                          
016900     EJECT                                                                
017000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017100*                                                                         
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300                                                                          
017400 01  NYCKLAR-TILL-DLI.                                                    
017500     03  W-IDDC-B6-X.                                                     
017600         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
017700                                                                          
017800*    --- STATUS-KOD FRÅN IMS                                              
017900 01  STATUS-WS                   PIC XX.                                  
018000     88  SEGMENT-FINNS                       VALUE '  '.                  
018100     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
018200     SKIP2                                                                
018300 01  GODK-STATUSKODER.                                                    
018400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018500     SKIP3                                                                
018600 01  SSA1                        PIC X(64).                               
018700 01  SSA2                        PIC X(64).                               
018800     EJECT                                                                
018900*    --- IMS FUNKTIONSKODER                                               
019000*01  -COPY W0003                                                          
019100     EJECT                                                                
019200*    ---  DLI INPUT-OUTPUT AREA                                           
019300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
019400 01   DLI-IO-AREA-B601.                                                   
019500*     03  -COPY WDB601                                                    
019600                                                                          
019700     EJECT                                                                
019800 LINKAGE SECTION.                                                         
019900                                                                          
020000*01  -COPY W0008      -PRE WDB6-                                          
020100     05  FILLER                  PIC X.                                   
020200                                                                          
020300                                                                          
020400     EJECT                                                                
020500 PROCEDURE DIVISION  USING  WDB6-PCB.                                     
020600 MAIN SECTION.                                                            
020700     ENTRY 'DLITCBL' USING  WDB6-PCB.                                     
020800                                                                          
020900     PERFORM A-INIT                                                       
021000     PERFORM S01-LAES-W61284                                              
021100     IF NOT END-OF-W61284                                                 
021200       PERFORM B-NYTT-DC                                                  
021300     END-IF                                                               
021400     PERFORM UNTIL END-OF-W61284                                          
021500       IF IN-SHIST-IDDC NOT = SPAR-IDDC                                   
021600         PERFORM E-SKRIV-DC-DATA                                          
021700         PERFORM B-NYTT-DC                                                
021800       END-IF                                                             
021900                                                                          
022000       PERFORM C-CHECK-INPOST                                             
022100       IF FL-OK-POST = JA                                                 
022200         PERFORM D-ACCUMULERA-DC-DATA                                     
022300       END-IF                                                             
022400                                                                          
022500       PERFORM S01-LAES-W61284                                            
022600     END-PERFORM                                                          
022700     PERFORM E-SKRIV-DC-DATA                                              
022800                                                                          
022900     PERFORM Z-FINIT                                                      
023000                                                                          
023100     MOVE ZERO TO RETURN-CODE                                             
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 A-INIT SECTION.                                                          
023600                                                                          
023700     OPEN INPUT  W61284                                                   
023800     OPEN OUTPUT W6128F                                                   
023900                                                                          
024000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024100                                                                          
024200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
024300     MOVE D-AAR     TO  DATUM-AAR                                         
024400     MOVE D-MAANAD  TO  DATUM-MAANAD                                      
024500     MOVE D-DAG     TO  DATUM-DAG                                         
024600                                                                          
024700     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
024800     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
024900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
025000                         DAT-O-TIDATUM DAT-KDSVAR                         
025100                                                                          
025200     IF DAT-KDSVAR-OK                                                     
025300       MOVE DAT-TIAARP  TO DAGENS-PERIOD                                  
025400                                                                          
025500     ELSE                                                                 
025600       MOVE 'FEL I WDATKONV' TO FELTEXT-STR                               
025700       DISPLAY FELTEXT                                                    
025800       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
025900       PERFORM S99-ABEND                                                  
026000     END-IF                                                               
026100                                                                          
026200     ACCEPT WS-YYMMDD      FROM DATE                                      
026300     ACCEPT WS-HHMMSSTH    FROM TIME                                      
026400     MOVE   WS-HHMM        TO WS-TIME                                     
026500     .                                                                    
026600                                                                          
026700     EJECT                                                                
026800 B-NYTT-DC SECTION.                                                       
026900                                                                          
027000*    -- SET ALL ACCUMULATED VALUES TO ZERO                                
027100     INITIALIZE W-DC-ACCUM-TAB                                            
027200                                                                          
027300*    -- REMEMBER CURRENT DC                                               
027400     MOVE IN-SHIST-IDDC TO SPAR-IDDC                                      
027500                                                                          
027600*    -- FETCH DATA ABOUT CURRENT DC                                       
027700     MOVE SPAR-IDDC TO W-IDDC-B6                                          
027800     PERFORM IMS-GU-WDB601                                                
027900                                                                          
028000     IF SEGMENT-SAKNAS                                                    
028100        MOVE NEJ          TO DCS-FLWEBDC                                  
028200        MOVE SPACE        TO WDCS-KDMFUP                                  
028300        MOVE SPACE        TO DCS-IDLANDX2                                 
028400        MOVE SPACE        TO DCS-ADCITY IN DCS-ADPOST-PNRORT              
028500     ELSE                                                                 
028600*       -- COMPUTE WHICH MANANGEMENT FOLLOW UP GROUP THE DC               
028700*       -- BELONGS TO.                                                    
028800        MOVE SPAR-IDDC    TO WBDC-IDDC                                    
028900        CALL WL10WBDC USING WBDC-AREA                                     
029000        IF WBDC-FLWEBDC = JA                                              
029100          MOVE WBDC-KDMFUP TO WDCS-KDMFUP                                 
029200        ELSE                                                              
029300          MOVE SPACE       TO WDCS-KDMFUP                                 
029400        END-IF                                                            
029500     END-IF                                                               
029600     .                                                                    
029700                                                                          
029800     EJECT                                                                
029900 C-CHECK-INPOST SECTION.                                                  
030000                                                                          
030100                                                                          
030200     MOVE JA TO FL-OK-POST                                                
030300*    -- 310-POSTER HAR NOLL I DATUM & KLOCKSLAG MEN ÄR OK ÄNDÅ            
030400     IF IN-SHIST-IDPTYP = 'R32'                                           
030500       EVALUATE FALSE                                                     
030600       WHEN IN-SHIST-TIINLMOT > 0                                         
030700         MOVE NEJ TO FL-OK-POST                                           
030800       WHEN IN-SHIST-TIINLMTI > 0                                         
030900         MOVE NEJ TO FL-OK-POST                                           
031000       WHEN IN-SHIST-TIINLINL > 0                                         
031100         MOVE NEJ TO FL-OK-POST                                           
031200       WHEN IN-SHIST-TIINLITI > 0                                         
031300         MOVE NEJ TO FL-OK-POST                                           
031400       END-EVALUATE                                                       
031500     END-IF                                                               
031600     .                                                                    
031700                                                                          
031800 D-ACCUMULERA-DC-DATA SECTION.                                            
031900                                                                          
032000     MOVE IN-SHIST-IDDISTR TO DIST35-IDDISTR                              
032100     IF DIST35-NA-TRANSFER                                                
032200     OR DIST35-CN-TRANSFER                                                
032300     OR DIST35-PACIFIC-TRANSFER                                           
032310     OR DIST35-REFILL-INOM-JP                                             
032400*      -- TRANSFER                                                        
032500       MOVE IT TO I                                                       
032600     ELSE                                                                 
032700*      --AIR/FLYG. AUSTRALA/JAPAN USES 19                                 
032800       IF IN-SHIST-KDFRAKT = 17 OR 19                                     
032900         MOVE IA TO I                                                     
033000       ELSE                                                               
033100*        --BOAT                                                           
033200         IF IN-SHIST-KDFRAKT >= 41 AND <= 45                              
033300           MOVE IB TO I                                                   
033400         ELSE                                                             
033500*          -- THEN IT IS SOMETHING ELSE                                   
033600           MOVE IZ TO I                                                   
033700         END-IF                                                           
033800       END-IF                                                             
033900     END-IF                                                               
034000                                                                          
034100     IF IN-SHIST-IDPTYP = 'R32'                                           
034200       PERFORM DA-TIDSBERAK                                               
034300                                                                          
034400*      -- JAPAN / AUSTRALIEN SKALL HA VÄRDEN I SEK                        
034500       IF DCS-NDC-PF OR DCS-NDC-OTHERS                                    
034600         COMPUTE TEMP-SUARTNTO-BINNED ROUNDED =                           
034700                 IN-SHIST-KVANTMOT * IN-SHIST-PRARTNTO                    
034800       ELSE                                                               
034900         IF IN-SHIST-PRKURS > 0                                           
035000           COMPUTE TEMP-SUARTNTO-BINNED ROUNDED =                         
035100                   IN-SHIST-KVANTMOT * IN-SHIST-PRARTNTO                  
035200                   / IN-SHIST-PRKURS                                      
035300         ELSE                                                             
035400           MOVE ZERO TO TEMP-SUARTNTO-BINNED                              
035500         END-IF                                                           
035600       END-IF                                                             
035700       ADD TEMP-SUARTNTO-BINNED TO W-SUARTNTO-BINNED(I)                   
035800       ADD TEMP-SUARTNTO-BINNED TO W-SUARTNTO-BINNED(MAX-I)               
035900                                                                          
036000     ELSE                                                                 
036100*      -- 310 RECORDS - NOT YET BINNED                                    
036200       ADD 1 TO W-KVANTAL-LINES-AK(I)                                     
036300       ADD 1 TO W-KVANTAL-LINES-AK(MAX-I)                                 
036400     END-IF                                                               
036500     .                                                                    
036600                                                                          
036700     EJECT                                                                
036800 DA-TIDSBERAK SECTION.                                                    
036900                                                                          
037000     MOVE IN-SHIST-IDDC      TO  TIME-IDDC                                
037100     MOVE IN-SHIST-TIINLMOT  TO  TIME-TIINLMOT                            
037200     MOVE IN-SHIST-TIINLMTI  TO  TIME-TIINLMTI                            
037300     MOVE IN-SHIST-TIINLINL  TO  TIME-TIINLINL                            
037400     MOVE IN-SHIST-TIINLITI  TO  TIME-TIINLITI                            
037500                                                                          
037600     CALL W612TIME USING TIME-W612TID WDB6-PCB                            
037700                                                                          
037800     IF TIME-KDSVAR = JA                                                  
037900       IF IN-SHIST-FLPRIO = JA                                            
038000         ADD TIME-KVDAGDEC TO W-KVDAGDEC-DAYS-PRIO (I)                    
038100         ADD 1             TO W-KVANTAL-LINES-PRIO (I)                    
038200         ADD TIME-KVDAGDEC TO W-KVDAGDEC-DAYS-PRIO (MAX-I)                
038300         ADD 1             TO W-KVANTAL-LINES-PRIO (MAX-I)                
038400       END-IF                                                             
038500                                                                          
038600       ADD TIME-KVDAGDEC TO W-KVDAGDEC-DAYS-BINNED (I)                    
038700       ADD 1             TO W-KVANTAL-LINES-BINNED (I)                    
038800       ADD TIME-KVDAGDEC TO W-KVDAGDEC-DAYS-BINNED (MAX-I)                
038900       ADD 1             TO W-KVANTAL-LINES-BINNED (MAX-I)                
039000                                                                          
039100     ELSE                                                                 
039200       MOVE 'FELAKTIG RETURKOD FRÅN W612TIME' TO FELTEXT-STR              
039300       DISPLAY FELTEXT                                                    
039400       PERFORM S99-ABEND                                                  
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800 E-SKRIV-DC-DATA SECTION.                                                 
039900                                                                          
040000*      ALL NDC:S PLUS CHINESE LDC:S AND CDC SHOULD HAVE ONE LINE          
040100*      PER TRANSPORT TYPE, PLUS A LINE WITH TOTAL SUMS,                   
040200*      BUT EUROPEAN LDC:S AND SDC:S SHOULD ONLY HAVE ONE                  
040300*      LINE WITH TOTAL SUMS FOR ALL TRANSPORT TYPES                       
040400       MOVE SPAR-IDDC TO WS-IDDC                                          
040500       IF NDC OR LDC-CN OR CDC-SE                                         
040600         MOVE 1 TO I                                                      
040700         PERFORM UNTIL I > MAX-I                                          
040800           PERFORM EA-SKRIV-DC-DATA-INDEX-I                               
040900           ADD 1 TO I                                                     
041000         END-PERFORM                                                      
041100                                                                          
041200       ELSE                                                               
041300*        -- ONLY TOTAL VALUES                                             
041400         MOVE MAX-I TO I                                                  
041500         PERFORM EA-SKRIV-DC-DATA-INDEX-I                                 
041600                                                                          
041700       END-IF                                                             
041800     .                                                                    
041900                                                                          
042000     EJECT                                                                
042100 EA-SKRIV-DC-DATA-INDEX-I SECTION.                                        
042200                                                                          
042300     MOVE SPAR-IDDC        TO UT-IDDC                                     
042400     MOVE DAGENS-PERIOD    TO UT-TIAARP                                   
042500     MOVE DCS-FLWEBDC      TO UT-FLWEBDC                                  
042600     MOVE WDCS-KDMFUP      TO UT-KDMFUP                                   
042700     MOVE DCS-IDLANDX2     TO UT-IDLANDX2                                 
042800     MOVE DCS-ADCITY IN DCS-ADPOST-PNRORT                                 
042900                           TO UT-ADCITY                                   
043000                                                                          
043100     EVALUATE I                                                           
043200      WHEN IA      MOVE 'A'       TO UT-KDREFTYP                          
043300      WHEN IB      MOVE 'B'       TO UT-KDREFTYP                          
043400      WHEN IT      MOVE 'T'       TO UT-KDREFTYP                          
043500      WHEN IZ      MOVE 'Z'       TO UT-KDREFTYP                          
043600      WHEN MAX-I   MOVE '9'       TO UT-KDREFTYP                          
043700     END-EVALUATE                                                         
043800                                                                          
043900*    -- RÄKNAR OM TOTAL TID TILL GENOMSNITTSTID PER RAD                   
044000     IF NOT W-KVANTAL-LINES-BINNED(I) = 0                                 
044100       COMPUTE W-KVDAGDEC-DAYS-BINNED(I) ROUNDED =                        
044200          W-KVDAGDEC-DAYS-BINNED(I) / W-KVANTAL-LINES-BINNED(I)           
044300     END-IF                                                               
044400                                                                          
044500     IF NOT W-KVANTAL-LINES-PRIO(I) = 0                                   
044600       COMPUTE W-KVDAGDEC-DAYS-PRIO(I) ROUNDED =                          
044700          W-KVDAGDEC-DAYS-PRIO(I) / W-KVANTAL-LINES-PRIO(I)               
044800     END-IF                                                               
044900                                                                          
045000     IF DCS-FLPRISSPR = JA                                                
045100        MOVE ZEROES TO W-SUARTNTO-BINNED(I)                               
045200     END-IF                                                               
045300                                                                          
045400     MOVE W-KVANTAL-LINES-AK(I)      TO UT-KVANTAL-LINES-AK               
045500     MOVE W-KVANTAL-LINES-BINNED(I)  TO UT-KVANTAL-LINES-BINNED           
045600     MOVE W-KVANTAL-LINES-PRIO(I)    TO UT-KVANTAL-LINES-PRIO             
045700     MOVE W-KVDAGDEC-DAYS-BINNED(I)  TO UT-KVDAGDEC-DAYS-BINNED           
045800     MOVE W-KVDAGDEC-DAYS-PRIO(I)    TO UT-KVDAGDEC-DAYS-PRIO             
045900     MOVE W-SUARTNTO-BINNED(I)       TO UT-SUARTNTO-BINNED                
046000                                                                          
046100     PERFORM S02-SKRIV-W6128F                                             
046200                                                                          
046300     .                                                                    
046400                                                                          
046500     EJECT                                                                
046600 Z-FINIT SECTION.                                                         
046700                                                                          
046800     CLOSE W61284                                                         
046900     CLOSE W6128F                                                         
047000                                                                          
047100     MOVE 'S' TO POSTSUM-OPKOD                                            
047200     CALL POSTSUM USING POSTSUM-PARM                                      
047300     .                                                                    
047400                                                                          
047500     EJECT                                                                
047600 S01-LAES-W61284  SECTION.                                                
047700                                                                          
047800     READ W61284 INTO IN-SHIST-AREA                                       
047900     AT END                                                               
048000        MOVE HIGH-VALUE TO IN-SHIST-AREA                                  
048100        SET END-OF-W61284 TO TRUE                                         
048200                                                                          
048300     NOT AT END                                                           
048400        MOVE 'W61284'   TO POSTSUM-FDNAMN                                 
048500        MOVE 'W6128FD1' TO POSTSUM-DDNAMN2                                
048600        MOVE IN-SHIST-IDDC TO POSTSUM-TRANSTYP                            
048700        CALL POSTSUM USING POSTSUM-PARM                                   
048800     END-READ                                                             
048900     .                                                                    
049000                                                                          
049100     EJECT                                                                
049200 S02-SKRIV-W6128F  SECTION.                                               
049300                                                                          
049400     WRITE UT-POST FROM UT-AREA                                           
049500                                                                          
049600     MOVE 'W6128F'   TO POSTSUM-FDNAMN                                    
049700     MOVE 'W6128FD2' TO POSTSUM-DDNAMN2                                   
049800     MOVE UT-IDDC    TO POSTSUM-TRANSTYP                                  
049900     CALL POSTSUM USING POSTSUM-PARM                                      
050000     .                                                                    
050100                                                                          
050200     EJECT                                                                
050300 S99-ABEND SECTION.                                                       
050400                                                                          
050500     SKIP2                                                                
050600     MOVE 'S' TO POSTSUM-OPKOD                                            
050700     CALL POSTSUM USING POSTSUM-PARM                                      
050800     CALL ABEND USING RKOD-ABEND                                          
050900     .                                                                    
051000                                                                          
051100     EJECT                                                                
051200* --- IMS SEKTIONER ---                                                   
051300                                                                          
051400                                                                          
051500 IMS-GU-WDB601    SECTION.                                                
051600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
051700          DELIMITED BY SIZE INTO SSA1                                     
051800     MOVE '  GE' TO GODK-STATUSKODER                                      
051900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
052000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
052100     PERFORM IMS-STATUSKONTROLL                                           
052200     .                                                                    
052300                                                                          
052400     EJECT                                                                
052500 IMS-STATUSKONTROLL SECTION.                                              
052600                                                                          
052700     SET STATUS-IX TO 1                                                   
052800     SEARCH GODK-STATUS                                                   
052900       AT END                                                             
053000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
053100           DELIMITED BY SIZE INTO FELTEXT                                 
053200         DISPLAY FELTEXT                                                  
053300         CALL FELLOG                                                      
053400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
053500         CONTINUE                                                         
053600     END-SEARCH                                                           
053700     .                                                                    
