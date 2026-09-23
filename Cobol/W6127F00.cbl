000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6127F00.                                                
000400 AUTHOR.         KJELL.                                                   
000500 DATE-WRITTEN.   2011-10-13                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*FUNKTION:                                                                
001000*                                                                         
001100*   SKAPAR DATA FÖR REFILL/AK FOLLOW-UP-RAPPORTER                         
001200*   FÖR SENARE UTSKRIFT TILL PAPPER/MAIL/WEB                              
001300*   PROGRAMMET ÄR EN DEL AV W61276 SOM DELATS UPP                         
001400*   I FLERA DELAR. NEDANSTGÅENDE ÄNDRINGAR GÄLLER DET                     
001500*   URSPRUNGLIGA PROGRAMMET.                                              
001600*                                                                         
001700*                                                                         
001800*   ÄNDRING:97-11-26 JOHAN LINDKVIST                                      
001900*   ÄVEN TRANSFERS MELLAN NDC MED PÅ LISTA                                
002000*                                                                         
002100*   ÄNDRING:98-02-23 JOHAN LINDKVIST                                      
002200*   ÄVEN BINNED PRIO LINES MED PÅ LISTA                                   
002300*                                                                         
002400*   ÄNDRING:99-03-11 JOHAN LINDKVIST                                      
002500*   JAPAN / AUSTRALIEN SKALL HA VALUES I SEK                              
002600*                                                                         
002700*   ÄNDRING:99-09-29 JOHAN LINDKVIST                                      
002800*   AUSTRALIEN ANVÄNDER INTE LÄNGRE FRAKTKOD 17 UTAN 19                   
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*          --- INFIL SORTERAD PÅ DC                                       
003900     SELECT W61265                     ASSIGN TO W6127FD1.                
004000     SKIP2                                                                
004100*          --- UTFIL IHOPSUMMERAT PER DC                                  
004200     SELECT W6127F                     ASSIGN TO W6127FD2.                
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP3                                                                
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  W61265                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  -COPY W61265      -L.                                                
005300     SKIP3                                                                
005400 FD  W6127F                                                               
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700                                                                          
005800 01  UT-POST.                                                             
005900*    03 -COPY W6127F      -L.                                             
006000                                                                          
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400 77  IDPGM                       PIC X(8)    VALUE 'W6127F00'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  KDRC-DISPLAY                PIC Z(5).                                
006800 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
006900                                                                          
007000*    MGMT REPORT GROUP  MA/CN/PF/NA ...                                   
007100 77  WDCS-KDMFUP                 PIC X(2)    VALUE SPACE.                 
007200                                                                          
007300*    -- INDEX IN THE TABLE BELOW.                                         
007400*    -- 1=AIR, 2=BOAT, 3=TRANSFER 4=OTHER 5=TOTAL, SUM OF 1-4             
007500 01  I                           PIC S9  COMP-3.                          
007600 01  MAX-I                       PIC S9  COMP-3    VALUE 5.               
007700*    -- CONSTANTS FOR VALUES OF I                                         
007800 01  IA                          PIC S9  COMP-3    VALUE 1.               
007900 01  IB                          PIC S9  COMP-3    VALUE 2.               
008000 01  IT                          PIC S9  COMP-3    VALUE 3.               
008100 01  IZ                          PIC S9  COMP-3    VALUE 4.               
008200                                                                          
008300*    -- TABLE FOR SAVING ACCUMULATED VALUES PER DC                        
008400 01  W-DC-ACCUM-TAB.                                                      
008500     03 FILLER OCCURS 5.                                                  
008600       05 W-KVANTAL-LINES-AK     PIC S9(7)         COMP-3.                
008700       05 W-KVANTAL-LINES-BINNED PIC S9(7)         COMP-3.                
008800       05 W-KVANTAL-LINES-PRIO   PIC S9(7)         COMP-3.                
008900       05 W-KVDAGDEC-DAYS-BINNED PIC S9(4)V9(1)    COMP-3.                
009000       05 W-KVDAGDEC-DAYS-PRIO   PIC S9(4)V9(1)    COMP-3.                
009100       05 W-SUARTNTO-BINNED      PIC S9(9)V9(2)    COMP-3.                
009200                                                                          
009300 01  SPAR-IDDC                   PIC XX      VALUE SPACE.                 
009400                                                                          
009500 01  TEMP-SUARTNTO-BINNED        PIC S9(9)V9(2)    COMP-3.                
009600                                                                          
009700 01  FL-OK-POST                  PIC X       VALUE 'J'.                   
009800                                                                          
009900 01  W61265-EOF-SW               PIC X       VALUE 'N'.                   
010000     88  END-OF-W61265                       VALUE 'J'.                   
010100                                                                          
010200                                                                          
010300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010400 01  FILLER REDEFINES DAGENS-DATUM.                                       
010500     03  DATUM-AAR               PIC 9(2).                                
010600     03  DATUM-MAANAD            PIC 9(2).                                
010700     03  DATUM-DAG               PIC 9(2).                                
010800                                                                          
010900 01  DAGENS-VECKA                PIC 9(4)    VALUE ZERO.                  
011000 01  FILLER REDEFINES DAGENS-VECKA.                                       
011100     03  VECKA-AA                PIC 99.                                  
011200     03  VECKA-VV                PIC 99.                                  
011300                                                                          
011400     EJECT                                                                
011500 01  WS-YYMMDDHHMM.                                                       
011600     03 WS-YYMMDD                PIC  9(6).                               
011700     03 WS-TIME                  PIC  9(4).                               
011800                                                                          
011900 01  WS-HHMMSSTH.                                                         
012000     03 WS-HHMM                  PIC  9(4).                               
012100     03 WS-SSTH                  PIC  9(4).                               
012200                                                                          
012300     EJECT                                                                
012400 01  DYNAMISKA-SUBPROGRAM.                                                
012500*                                                                         
012600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
012800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012900     03  W612TIME                PIC X(8)    VALUE 'W612TIME'.            
013000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013200     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
013300                                                                          
013400*    --- PARAMETRAR TILL ABEND                                            
013500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014000                                                                          
014100 01  FELTEXT.                                                             
014200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014400                                                                          
014500                                                                          
014600*    --- PARAMETRAR TILL DATKORT                                          
014700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W6127F'.              
014800 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
014900                                                                          
015000*01  -COPY WDATKORT                                                       
015100     EJECT                                                                
015200*    --- PARAMETRAR TILL POSTSUM                                          
015300*01  -COPY W0005   -PRE  POSTSUM-                                         
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL W612TIME                                         
015600*01  -COPY W612TID     -PRE TIME-                                         
015700     EJECT                                                                
015800*    --- PARAMETRAR TILL WL10WBDC                                         
015900*01  -COPY WL10WBDC                                                       
016000     EJECT                                                                
016100*01  -COPY WWDIST35                                                       
016200                                                                          
016300*01  -COPY WWDC99                                                         
016400                                                                          
016500     EJECT                                                                
016600 01  IN-AREA-START               PIC X(24)   VALUE                        
016700                                 'IN-AREA-START  '.                       
016800                                                                          
016900*01  AREA -COPY W61265     -PRE IN-SHIST-                                 
017000                                                                          
017100     EJECT                                                                
017200 01  UT-AREA-START               PIC X(24)   VALUE                        
017300                                 'UT-AREA-START  '.                       
017400                                                                          
017500*01  AREA -COPY W6127F     -PRE UT-                                       
017600                                                                          
017700     EJECT                                                                
017800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017900*                                                                         
018000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018100                                                                          
018200 01  NYCKLAR-TILL-DLI.                                                    
018300     03  W-IDDC-B6-X.                                                     
018400         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
018500                                                                          
018600*    --- STATUS-KOD FRÅN IMS                                              
018700 01  STATUS-WS                   PIC XX.                                  
018800     88  SEGMENT-FINNS                       VALUE '  '.                  
018900     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
019000     SKIP2                                                                
019100 01  GODK-STATUSKODER.                                                    
019200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019300     SKIP3                                                                
019400 01  SSA1                        PIC X(64).                               
019500 01  SSA2                        PIC X(64).                               
019600     EJECT                                                                
019700*    --- IMS FUNKTIONSKODER                                               
019800*01  -COPY W0003                                                          
019900     EJECT                                                                
020000*    ---  DLI INPUT-OUTPUT AREA                                           
020100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020200 01   DLI-IO-AREA-B601.                                                   
020300*     03  -COPY WDB601                                                    
020400                                                                          
020500     EJECT                                                                
020600 LINKAGE SECTION.                                                         
020700                                                                          
020800*01  -COPY W0008      -PRE WDB6-                                          
020900     05  FILLER                  PIC X.                                   
021000                                                                          
021100                                                                          
021200     EJECT                                                                
021300 PROCEDURE DIVISION  USING  WDB6-PCB.                                     
021400 MAIN SECTION.                                                            
021500     ENTRY 'DLITCBL' USING  WDB6-PCB.                                     
021600                                                                          
021700     PERFORM A-INIT                                                       
021800     PERFORM S01-LAES-W61265                                              
021900     IF NOT END-OF-W61265                                                 
022000       PERFORM B-NYTT-DC                                                  
022100     END-IF                                                               
022200     PERFORM UNTIL END-OF-W61265                                          
022300       IF IN-SHIST-IDDC NOT = SPAR-IDDC                                   
022400         PERFORM E-SKRIV-DC-DATA                                          
022500         PERFORM B-NYTT-DC                                                
022600       END-IF                                                             
022700                                                                          
022800       PERFORM C-CHECK-INPOST                                             
022900       IF FL-OK-POST = JA                                                 
023000         PERFORM D-ACCUMULERA-DC-DATA                                     
023100       END-IF                                                             
023200                                                                          
023300       PERFORM S01-LAES-W61265                                            
023400     END-PERFORM                                                          
023410     IF NOT SPAR-IDDC = SPACE                                             
023500        PERFORM E-SKRIV-DC-DATA                                           
023510     END-IF                                                               
023600                                                                          
023700     PERFORM Z-FINIT                                                      
023800                                                                          
023900     MOVE ZERO TO RETURN-CODE                                             
024000     GOBACK                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 A-INIT SECTION.                                                          
024400                                                                          
024500     OPEN INPUT  W61265                                                   
024600     OPEN OUTPUT W6127F                                                   
024700                                                                          
024800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024900                                                                          
025000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
025100     MOVE D-AAR     TO  DATUM-AAR                                         
025200     MOVE D-MAANAD  TO  DATUM-MAANAD                                      
025300     MOVE D-DAG     TO  DATUM-DAG                                         
025400                                                                          
025500     MOVE D-AAR     TO  VECKA-AA                                          
025600     MOVE D-VECKA   TO  VECKA-VV                                          
025700                                                                          
025800     ACCEPT WS-YYMMDD      FROM DATE                                      
025900     ACCEPT WS-HHMMSSTH    FROM TIME                                      
026000     MOVE   WS-HHMM        TO WS-TIME                                     
026100     .                                                                    
026200                                                                          
026300     EJECT                                                                
026400 B-NYTT-DC SECTION.                                                       
026500                                                                          
026600*    -- SET ALL ACCUMULATED VALUES TO ZERO                                
026700     INITIALIZE W-DC-ACCUM-TAB                                            
026800                                                                          
026900*    -- REMEMBER CURRENT DC                                               
027000     MOVE IN-SHIST-IDDC TO SPAR-IDDC                                      
027100                                                                          
027200*    -- FETCH DATA ABOUT CURRENT DC                                       
027300     MOVE SPAR-IDDC TO W-IDDC-B6                                          
027400     PERFORM IMS-GU-WDB601                                                
027500                                                                          
027600     IF SEGMENT-SAKNAS                                                    
027700        MOVE NEJ          TO DCS-FLWEBDC                                  
027800        MOVE SPACE        TO WDCS-KDMFUP                                  
027900        MOVE SPACE        TO DCS-IDLANDX2                                 
028000        MOVE SPACE        TO DCS-ADCITY IN DCS-ADPOST-PNRORT              
028100     ELSE                                                                 
028200*       -- COMPUTE WHICH MANANGEMENT FOLLOW UP GROUP THE DC               
028300*       -- BELONGS TO.                                                    
028400        MOVE SPAR-IDDC    TO WBDC-IDDC                                    
028500        CALL WL10WBDC USING WBDC-AREA                                     
028600        IF WBDC-FLWEBDC = JA                                              
028700          MOVE WBDC-KDMFUP TO WDCS-KDMFUP                                 
028800        ELSE                                                              
028900          MOVE SPACE       TO WDCS-KDMFUP                                 
029000        END-IF                                                            
029100     END-IF                                                               
029200     .                                                                    
029300                                                                          
029400     EJECT                                                                
029500 C-CHECK-INPOST SECTION.                                                  
029600                                                                          
029700                                                                          
029800     MOVE JA TO FL-OK-POST                                                
029900*    -- 310-POSTER HAR NOLL I DATUM & KLOCKSLAG MEN ÄR OK ÄNDÅ            
030000     IF IN-SHIST-IDPTYP = 'R32'                                           
030100       EVALUATE FALSE                                                     
030200       WHEN IN-SHIST-TIINLMOT > 0                                         
030300         MOVE NEJ TO FL-OK-POST                                           
030400       WHEN IN-SHIST-TIINLMTI > 0                                         
030500         MOVE NEJ TO FL-OK-POST                                           
030600       WHEN IN-SHIST-TIINLINL > 0                                         
030700         MOVE NEJ TO FL-OK-POST                                           
030800       WHEN IN-SHIST-TIINLITI > 0                                         
030900         MOVE NEJ TO FL-OK-POST                                           
031000       END-EVALUATE                                                       
031100     END-IF                                                               
031200     .                                                                    
031300                                                                          
031400 D-ACCUMULERA-DC-DATA SECTION.                                            
031500                                                                          
031600     MOVE IN-SHIST-IDDISTR TO DIST35-IDDISTR                              
031700     IF DIST35-NA-TRANSFER                                                
031800     OR DIST35-CN-TRANSFER                                                
031900     OR DIST35-PACIFIC-TRANSFER                                           
031910     OR DIST35-REFILL-INOM-JP                                             
032000*      -- TRANSFER                                                        
032100       MOVE IT TO I                                                       
032200     ELSE                                                                 
032300*      --AIR/FLYG. AUSTRALA/JAPAN USES 19                                 
032400       IF IN-SHIST-KDFRAKT = 17 OR 19                                     
032500         MOVE IA TO I                                                     
032600       ELSE                                                               
032700*        --BOAT                                                           
032800         IF IN-SHIST-KDFRAKT >= 41 AND <= 45                              
032900           MOVE IB TO I                                                   
033000         ELSE                                                             
033100*          -- THEN IT IS SOMETHING ELSE                                   
033200           MOVE IZ TO I                                                   
033300         END-IF                                                           
033400       END-IF                                                             
033500     END-IF                                                               
033600                                                                          
033700     IF IN-SHIST-IDPTYP = 'R32'                                           
033800       PERFORM DA-TIDSBERAK                                               
033900                                                                          
034000*      -- JAPAN / AUSTRALIEN SKALL HA VÄRDEN I SEK                        
034100       IF DCS-NDC-PF OR DCS-NDC-OTHERS                                    
034200         COMPUTE TEMP-SUARTNTO-BINNED ROUNDED =                           
034300                 IN-SHIST-KVANTMOT * IN-SHIST-PRARTNTO                    
034400       ELSE                                                               
034500         IF IN-SHIST-PRKURS > 0                                           
034600           COMPUTE TEMP-SUARTNTO-BINNED ROUNDED =                         
034700                   IN-SHIST-KVANTMOT * IN-SHIST-PRARTNTO                  
034800                   / IN-SHIST-PRKURS                                      
034900         ELSE                                                             
035000           MOVE ZERO TO TEMP-SUARTNTO-BINNED                              
035100         END-IF                                                           
035200       END-IF                                                             
035300       ADD TEMP-SUARTNTO-BINNED TO W-SUARTNTO-BINNED(I)                   
035400       ADD TEMP-SUARTNTO-BINNED TO W-SUARTNTO-BINNED(MAX-I)               
035500                                                                          
035600     ELSE                                                                 
035700*      -- 310 RECORDS - NOT YET BINNED                                    
035800       ADD 1 TO W-KVANTAL-LINES-AK(I)                                     
035900       ADD 1 TO W-KVANTAL-LINES-AK(MAX-I)                                 
036000     END-IF                                                               
036100     .                                                                    
036200                                                                          
036300     EJECT                                                                
036400 DA-TIDSBERAK SECTION.                                                    
036500                                                                          
036600     MOVE IN-SHIST-IDDC      TO  TIME-IDDC                                
036700     MOVE IN-SHIST-TIINLMOT  TO  TIME-TIINLMOT                            
036800     MOVE IN-SHIST-TIINLMTI  TO  TIME-TIINLMTI                            
036900     MOVE IN-SHIST-TIINLINL  TO  TIME-TIINLINL                            
037000     MOVE IN-SHIST-TIINLITI  TO  TIME-TIINLITI                            
037100                                                                          
037200     CALL W612TIME USING TIME-W612TID WDB6-PCB                            
037300                                                                          
037400     IF TIME-KDSVAR = JA                                                  
037500       IF IN-SHIST-FLPRIO = JA                                            
037600         ADD TIME-KVDAGDEC TO W-KVDAGDEC-DAYS-PRIO (I)                    
037700         ADD 1             TO W-KVANTAL-LINES-PRIO (I)                    
037800         ADD TIME-KVDAGDEC TO W-KVDAGDEC-DAYS-PRIO (MAX-I)                
037900         ADD 1             TO W-KVANTAL-LINES-PRIO (MAX-I)                
038000       END-IF                                                             
038100                                                                          
038200       ADD TIME-KVDAGDEC TO W-KVDAGDEC-DAYS-BINNED (I)                    
038300       ADD 1             TO W-KVANTAL-LINES-BINNED (I)                    
038400       ADD TIME-KVDAGDEC TO W-KVDAGDEC-DAYS-BINNED (MAX-I)                
038500       ADD 1             TO W-KVANTAL-LINES-BINNED (MAX-I)                
038600                                                                          
038700     ELSE                                                                 
038800       MOVE 'FELAKTIG RETURKOD FRÅN W612TIME' TO FELTEXT-STR              
038900       DISPLAY FELTEXT                                                    
039000       PERFORM S99-ABEND                                                  
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 E-SKRIV-DC-DATA SECTION.                                                 
039500                                                                          
039600*      ALL NDC:S PLUS CHINESE LDC:S AND CDC SHOULD HAVE ONE LINE          
039700*      PER TRANSPORT TYPE, PLUS A LINE WITH TOTAL SUMS,                   
039800*      BUT EUROPEAN LDC:S AND SDC:S SHOULD ONLY HAVE ONE                  
039900*      LINE WITH TOTAL SUMS FOR ALL TRANSPORT TYPES                       
040000       MOVE SPAR-IDDC TO WS-IDDC                                          
040100       IF NDC OR LDC-CN OR CDC-SE                                         
040200         MOVE 1 TO I                                                      
040300         PERFORM UNTIL I > MAX-I                                          
040400           PERFORM EA-SKRIV-DC-DATA-INDEX-I                               
040500           ADD 1 TO I                                                     
040600         END-PERFORM                                                      
040700                                                                          
040800       ELSE                                                               
040900*        -- ONLY TOTAL VALUES                                             
041000         MOVE MAX-I TO I                                                  
041100         PERFORM EA-SKRIV-DC-DATA-INDEX-I                                 
041200                                                                          
041300       END-IF                                                             
041400     .                                                                    
041500                                                                          
041600     EJECT                                                                
041700 EA-SKRIV-DC-DATA-INDEX-I SECTION.                                        
041800                                                                          
041900     MOVE SPAR-IDDC        TO UT-IDDC                                     
042000     MOVE DAGENS-VECKA     TO UT-TIAAVV                                   
042100     MOVE DCS-FLWEBDC      TO UT-FLWEBDC                                  
042200     MOVE WDCS-KDMFUP      TO UT-KDMFUP                                   
042300     MOVE DCS-IDLANDX2     TO UT-IDLANDX2                                 
042400     MOVE DCS-ADCITY IN DCS-ADPOST-PNRORT                                 
042500                           TO UT-ADCITY                                   
042600                                                                          
042700     EVALUATE I                                                           
042800      WHEN IA      MOVE 'A'       TO UT-KDREFTYP                          
042900      WHEN IB      MOVE 'B'       TO UT-KDREFTYP                          
043000      WHEN IT      MOVE 'T'       TO UT-KDREFTYP                          
043100      WHEN IZ      MOVE 'Z'       TO UT-KDREFTYP                          
043200      WHEN MAX-I   MOVE '9'       TO UT-KDREFTYP                          
043300     END-EVALUATE                                                         
043400                                                                          
043500*    -- RÄKNAR OM TOTAL TID TILL GENOMSNITTSTID PER RAD                   
043600     IF NOT W-KVANTAL-LINES-BINNED(I) = 0                                 
043700       COMPUTE W-KVDAGDEC-DAYS-BINNED(I) ROUNDED =                        
043800          W-KVDAGDEC-DAYS-BINNED(I) / W-KVANTAL-LINES-BINNED(I)           
043900     END-IF                                                               
044000                                                                          
044100     IF NOT W-KVANTAL-LINES-PRIO(I) = 0                                   
044200       COMPUTE W-KVDAGDEC-DAYS-PRIO(I) ROUNDED =                          
044300          W-KVDAGDEC-DAYS-PRIO(I) / W-KVANTAL-LINES-PRIO(I)               
044400     END-IF                                                               
044500                                                                          
044600     IF DCS-FLPRISSPR = JA                                                
044700        MOVE ZEROES TO W-SUARTNTO-BINNED(I)                               
044800     END-IF                                                               
044900                                                                          
045000     MOVE W-KVANTAL-LINES-AK(I)      TO UT-KVANTAL-LINES-AK               
045100     MOVE W-KVANTAL-LINES-BINNED(I)  TO UT-KVANTAL-LINES-BINNED           
045200     MOVE W-KVANTAL-LINES-PRIO(I)    TO UT-KVANTAL-LINES-PRIO             
045300     MOVE W-KVDAGDEC-DAYS-BINNED(I)  TO UT-KVDAGDEC-DAYS-BINNED           
045400     MOVE W-KVDAGDEC-DAYS-PRIO(I)    TO UT-KVDAGDEC-DAYS-PRIO             
045500     MOVE W-SUARTNTO-BINNED(I)       TO UT-SUARTNTO-BINNED                
045600                                                                          
045700     PERFORM S02-SKRIV-W6127F                                             
045800                                                                          
045900     .                                                                    
046000                                                                          
046100     EJECT                                                                
046200 Z-FINIT SECTION.                                                         
046300                                                                          
046400     CLOSE W61265                                                         
046500     CLOSE W6127F                                                         
046600                                                                          
046700     MOVE 'S' TO POSTSUM-OPKOD                                            
046800     CALL POSTSUM USING POSTSUM-PARM                                      
046900     .                                                                    
047000                                                                          
047100     EJECT                                                                
047200 S01-LAES-W61265  SECTION.                                                
047300                                                                          
047400     READ W61265 INTO IN-SHIST-AREA                                       
047500     AT END                                                               
047600        MOVE HIGH-VALUE TO IN-SHIST-AREA                                  
047700        SET END-OF-W61265 TO TRUE                                         
047800                                                                          
047900     NOT AT END                                                           
048000        MOVE 'W61265'   TO POSTSUM-FDNAMN                                 
048100        MOVE 'W6127FD1' TO POSTSUM-DDNAMN2                                
048200        MOVE IN-SHIST-IDDC TO POSTSUM-TRANSTYP                            
048300        CALL POSTSUM USING POSTSUM-PARM                                   
048400     END-READ                                                             
048500     .                                                                    
048600                                                                          
048700     EJECT                                                                
048800 S02-SKRIV-W6127F  SECTION.                                               
048900                                                                          
049000     WRITE UT-POST FROM UT-AREA                                           
049100                                                                          
049200     MOVE 'W6127F'   TO POSTSUM-FDNAMN                                    
049300     MOVE 'W6127FD2' TO POSTSUM-DDNAMN2                                   
049400     MOVE UT-IDDC    TO POSTSUM-TRANSTYP                                  
049500     CALL POSTSUM USING POSTSUM-PARM                                      
049600     .                                                                    
049700                                                                          
049800     EJECT                                                                
049900 S99-ABEND SECTION.                                                       
050000                                                                          
050100     SKIP2                                                                
050200     MOVE 'S' TO POSTSUM-OPKOD                                            
050300     CALL POSTSUM USING POSTSUM-PARM                                      
050400     CALL ABEND USING RKOD-ABEND                                          
050500     .                                                                    
050600                                                                          
050700     EJECT                                                                
050800* --- IMS SEKTIONER ---                                                   
050900                                                                          
051000                                                                          
051100 IMS-GU-WDB601    SECTION.                                                
051200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
051300          DELIMITED BY SIZE INTO SSA1                                     
051400     MOVE '  GE' TO GODK-STATUSKODER                                      
051500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
051600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
051700     PERFORM IMS-STATUSKONTROLL                                           
051800     .                                                                    
051900                                                                          
052000     EJECT                                                                
052100 IMS-STATUSKONTROLL SECTION.                                              
052200                                                                          
052300     SET STATUS-IX TO 1                                                   
052400     SEARCH GODK-STATUS                                                   
052500       AT END                                                             
052600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
052700           DELIMITED BY SIZE INTO FELTEXT                                 
052800         DISPLAY FELTEXT                                                  
052900         CALL FELLOG                                                      
053000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
053100         CONTINUE                                                         
053200     END-SEARCH                                                           
053300     .                                                                    
