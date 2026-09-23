000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4054000.                                                
000300 AUTHOR.         CHRISTINE LINDQVIST.                                     
000400 DATE-WRITTEN.   00/03/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT BAKGRUNDSMPP                                   
001000*        SKAPAR BLANKETTER VIA SUBPROGRAM  : W475DGR                      
001100*                                            W475IMDG                     
001200*                                                                         
001300*        (ALLA BLANKETTERNA STYRS MOT PRINTER R44052 (NJO00037))          
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T540X                                             
001700*                                                                         
001800                                                                          
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W4054000'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  W-PRINTER-ID                PIC X(3)    VALUE '412'.                 
003500 77  W-BLANKETT-ID               PIC X(7)    VALUE SPACE.                 
003600 77  WS-UPPD                     PIC 9(3)    VALUE ZERO.                  
003700 77  WS-DAGENS-DATUM             PIC 9(8)    VALUE ZERO.                  
003800*      --- VALID IDDC CODES                                               
003900*                                                                         
004000*01    -COPY WWDCKONS                                                     
004100       EJECT                                                              
004200 77  ALLT-SW                     PIC X.                                   
004300     88  ALLT-OK                             VALUE 'J'.                   
004400     88  ALLT-FEL                            VALUE 'N'.                   
004500                                                                          
004600 01  W-IDDCTEXT-MSGI.                                                     
004700     03  FILLER              PIC X(5)   VALUE 'WIDDC'.                    
004800     03  W-IDDC-MSGI         PIC X(2).                                    
004900                                                                          
005000 01  WS-FIXA-DATUM               PIC 9(6).                                
005100 01  WS-CENTURY-DATUM.                                                    
005200     03 WS-CENTURY                 PIC 9(2).                              
005300     03 WS-AAMMDD.                                                        
005400       05 WS-AA                    PIC 9(2).                              
005500       05 WS-MM                    PIC 9(2).                              
005600       05 WS-DD                    PIC 9(2).                              
005700                                                                          
005800 01  FELKODER.                                                            
005900     03  FEL-IDDISTR             PIC X(4)    VALUE SPACE.                 
006000     03  FEL-IDKUNDNR            PIC X(6)    VALUE SPACE.                 
006100     03  FEL-IDSKEPPN            PIC X(7)    VALUE SPACE.                 
006200     03  FEL-IDSYSTEM            PIC X(4)    VALUE SPACE.                 
006300     03  FEL-IDDC                PIC X(2)    VALUE SPACE.                 
006400                                                                          
006500     SKIP3                                                                
006600 01  RETURKODER.                                                          
006700     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
006800                                                                          
006900     EJECT                                                                
007000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007100 01  GENERELLA-SUBPROGRAM.                                                
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
007600     03  W475DGR                 PIC X(8)    VALUE 'W475DGR '.            
007700     03  W475IMDG                PIC X(8)    VALUE 'W475IMDG'.            
007800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*01 -COPY WMSGINIT                                                        
008200     EJECT                                                                
008300*                                                                         
008400     EJECT                                                                
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008700                                                                          
008800*01  MID -COPY W4I54001                                                   
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
009100                                                                          
009200*01  -COPY WMSGAREA                                                       
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
009500*01  -COPY WORKAREA                                                       
009600     EJECT                                                                
009700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000                                                                          
010100 01  NYCKLAR-TILL-DLI.                                                    
010200     03  W-WDGXKEY-X.                                                     
010300         05  W-IDHTYP            PIC X(4)    VALUE '4513'.                
010400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
010500     SKIP2                                                                
010600     03  W-DASKEPPN-MIN-X.                                                
010700         05  W-DASKEPPN-MIN      PIC 9(8)   VALUE ZERO.                   
010800                                                                          
010900     03  W-IDDC-B6-X.                                                     
011000         05 W-IDDC-B6                  PIC X(2).                          
011100                                                                          
011200     EJECT                                                                
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE ALL 'W475BLKT'.        
012500*    --- BLANKETT-ID-TABELL                                               
012600*01  FILLER   -COPY W475BLKT                                              
012700     EJECT                                                                
012800                                                                          
012900*    --- LÄNKCOPYTEXTER                                                   
013000*01  FILLER   -COPY W475DGR     -PRE DGR-                                 
013100     EJECT                                                                
013200*01  FILLER   -COPY W475IMDG    -PRE IMDG-                                
013300     EJECT                                                                
013400*    --- IMS FUNKTIONSKODER                                               
013500*01  -COPY W0003                                                          
013600     EJECT                                                                
013700*    ---  DLI INPUT-OUTPUT AREA                                           
013800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4513'.         
013900     SKIP3                                                                
014000 01  DLI-IO-AREA-4513.                                                    
014100     03  WL451301.                                                        
014200*        05  -COPY WDGX01                                                 
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4514'.         
014500     SKIP3                                                                
014600 01  DLI-IO-AREA-4514.                                                    
014700     03  WL451311.                                                        
014800*        05  -COPY WDGX4514                                               
014900                                                                          
015000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
015100 01   DLI-IO-AREA-B601.                                                   
015200*     03  -COPY WDB601                                                    
015300     EJECT                                                                
015400 LINKAGE SECTION.                                                         
015500*01  -COPY W0009   -PRE MSG-                                              
015600                                                                          
015700*01  -COPY W0009   -PRE ALT-                                              
015800     EJECT                                                                
015900*01  -COPY W0009   -PRE DAP-                                              
015910     EJECT                                                                
016000*01  -COPY W0008   -PRE USEA-                                             
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300*01  -COPY W0008  -PRE LISB-                                              
016400     05  FILLER                  PIC X.                                   
016500     EJECT                                                                
016600*01  -COPY W0008  -PRE 4513-                                              
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016900*01  -COPY W0008  -PRE 1165-                                              
017000     05  FILLER                  PIC X.                                   
017100                                                                          
017200*01  -COPY W0008  -PRE GMTA-                                              
017300     05  FILLER                  PIC X.                                   
017400     EJECT                                                                
017500*01  -COPY W0008  -PRE BETC-                                              
017600     05  FILLER                  PIC X.                                   
017700                                                                          
017800*01  -COPY W0008  -PRE ORDD-                                              
017900     05  FILLER                  PIC X.                                   
018000     EJECT                                                                
018100*01  -COPY W0008  -PRE WDB6-                                              
018200     05  FILLER                  PIC X.                                   
018300     EJECT                                                                
018400 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB DAP-PCB                        
018500                           USEA-PCB LISB-PCB                              
018600                           4513-PCB 1165-PCB GMTA-PCB                     
018700                           BETC-PCB ORDD-PCB WDB6-PCB.                    
018800 MAIN SECTION.                                                            
018900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB DAP-PCB                        
019000                           USEA-PCB LISB-PCB                              
019100                           4513-PCB 1165-PCB GMTA-PCB                     
019200                           BETC-PCB ORDD-PCB WDB6-PCB.                    
019300                                                                          
019400     PERFORM IMS-GET-MSG                                                  
019500     IF SEGMENT-FINNS                                                     
019600       IF MSG-KDTRTYP = 'X'                                               
019700         PERFORM A-INIT                                                   
019800         PERFORM B-RENSA-GAMLA-4513                                       
019900         PERFORM F-LAES-TABELLER                                          
020000                                                                          
020100         IF W-BLANKETT-ID = 'IMDG   ' OR 'IMDG-SF'                        
020200           PERFORM H-ANROPA-W475IMDG                                      
020300         ELSE                                                             
020400           IF W-BLANKETT-ID = 'DGR    '                                   
020500             PERFORM I-ANROPA-W475DGR                                     
020600           END-IF                                                         
020800         END-IF                                                           
020810                                                                          
020900       END-IF                                                             
021000     END-IF                                                               
021100                                                                          
021200     MOVE ZERO TO RETURN-CODE                                             
021300     GOBACK                                                               
021400     .                                                                    
021500     EJECT                                                                
021600 A-INIT SECTION.                                                          
021700                                                                          
021800     MOVE JA  TO ALLT-SW                                                  
021900                                                                          
022000     MOVE MSG-INDATA-MINUS-1-TRANSKOD     TO MID-W4I54001                 
022100                                                                          
022200     IF MID-IDDISTR NUMERIC                                               
022300        CONTINUE                                                          
022400     ELSE                                                                 
022500        MOVE MID-IDDISTR                  TO FEL-IDDISTR                  
022600        MOVE NEJ TO ALLT-SW                                               
022700     END-IF                                                               
022800                                                                          
022900     IF MID-IDKUNDNR NUMERIC                                              
023000        CONTINUE                                                          
023100     ELSE                                                                 
023200        MOVE MID-IDKUNDNR                 TO FEL-IDKUNDNR                 
023300        MOVE NEJ TO ALLT-SW                                               
023400     END-IF                                                               
023500                                                                          
023600     IF MID-IDSKEPPN NUMERIC                                              
023700        CONTINUE                                                          
023800     ELSE                                                                 
023900        MOVE MID-IDSKEPPN                 TO FEL-IDSKEPPN                 
024000        MOVE NEJ TO ALLT-SW                                               
024100     END-IF                                                               
024200                                                                          
024300     MOVE MID-IDDC                        TO W-IDDC-B6                    
024400     PERFORM IMS-GU-WDB601                                                
024500     IF DCS-KDDC = SPACE                                                  
024600        MOVE W-IDDC-B6                    TO FEL-IDDC                     
024700        MOVE NEJ TO ALLT-SW                                               
024800     END-IF                                                               
024900                                                                          
025000     IF MID-IDSYSTEM NOT = '4533' AND '4535' AND '4662' AND '4665'        
025100                           AND '4675' AND '4639'                          
025200        MOVE MID-IDSYSTEM                 TO FEL-IDSYSTEM                 
025300        MOVE NEJ TO ALLT-SW                                               
025400     END-IF                                                               
025500                                                                          
025600     IF ALLT-OK                                                           
025700       MOVE MID-IDDC         TO W-IDDC-MSGI                               
025800       MOVE ALL '+'          TO MSGI-WMSGINIT                             
025900       MOVE '013'            TO MSGI-KDCALL                               
026000       MOVE W-IDDCTEXT-MSGI  TO MSGI-IDUSER                               
026100       MOVE '4540'           TO MSGI-IDTRANS                              
026200       MOVE MSG-LTERM-NAME   TO MSGI-IDLTERM-USER                         
026300       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
026400     ELSE                                                                 
026500        MOVE '*** FEL PÅ INDATAT KOLLA PÅ FEL-  **'                       
026600                               TO FELTEXT                                 
026700        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
026800     END-IF                                                               
026900                                                                          
027000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
027100     .                                                                    
027200     EJECT                                                                
027300 B-RENSA-GAMLA-4513 SECTION.                                              
027400                                                                          
027500** ALLA SEGM. SOM ÄR ÄLDRE ÄN 5 ARB.DAGAR RENSAS.                         
027600                                                                          
027700*FIX FÖR ATT KUNNA SKRIVA UT FARLIGT-GODS DOKUMENT PÅ DIREKT-             
027800*LEVERANSARTIKLAR SOM MOTTAGNINGSRAPPORTERAS PÅ DC11                      
027900     IF DCS-DDC AND DCS-IDLANDX2 = 'SE'                                   
028000       MOVE WC-CDC-SE TO W-IDDC-B6                                        
028100       PERFORM IMS-GU-WDB601                                              
028200     END-IF                                                               
028300*SLUTFIX TL 040927                                                        
028400                                                                          
028500     MOVE DCS-IDDC              TO WORK-IDDC                              
028600     MOVE 3                     TO WORK-KVWORKD                           
028700     MOVE WS-DAGENS-DATUM (3:6) TO WORK-TIAAMMDD-TOM                      
028800     MOVE 003                   TO WORK-KDCALL                            
028900     CALL WORKDAY   USING WORK-KDCALL                                     
029000                          WORK-DATE-AREA                                  
029100                          WORK-KDSVAR                                     
029200     IF WORK-KDSVAR-FEL                                                   
029300        MOVE 'FEL FRÅN WORKDAY  I C-SECTION'                              
029400                          TO FELTEXT                                      
029500        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
029600     END-IF                                                               
029700                                                                          
029800     PERFORM IMS-GU-WL451301                                              
029900                                                                          
030000     MOVE WORK-TIAAMMDD-FOM       TO WS-FIXA-DATUM                        
030100     MOVE WS-FIXA-DATUM           TO WS-AAMMDD                            
030200     IF WS-AA < 50                                                        
030300       MOVE 20                    TO WS-CENTURY                           
030400     ELSE                                                                 
030500       MOVE 19                    TO WS-CENTURY                           
030600     END-IF                                                               
030700     MOVE WS-CENTURY-DATUM        TO W-DASKEPPN-MIN                       
030800                                                                          
030900     PERFORM IMS-GHNP-WL451311                                            
031000     MOVE +1               TO WS-UPPD                                     
031100                                                                          
031200     PERFORM UNTIL SEGMENT-SAKNAS OR WS-UPPD > 113                        
031300                                                                          
031400       PERFORM IMS-DLET-WL451311                                          
031500       ADD +1              TO WS-UPPD                                     
031600       PERFORM IMS-GHNP-WL451311                                          
031700     END-PERFORM                                                          
031800     .                                                                    
031900     EJECT                                                                
032000 F-LAES-TABELLER SECTION.                                                 
032100                                                                          
032300     SET BLK-IX TO 1                                                      
032400*                                                                         
032410* TABELLEN NEDAN GÄLLER ALLA LAGER SOM SKRIVER UT FG.DOK.                 
032420*                                                                         
032500     SEARCH BLANKETT-TABELL-CDC                                           
032600        AT END                                                            
032700             STRING ' HITTAR EJ BLANKETTID :  '                           
032800             DELIMITED BY SIZE INTO FELTEXT                               
032900             MOVE SPACE             TO W-BLANKETT-ID                      
033000                                                                          
033100        WHEN BLK-CDC-KDFRAKT      (BLK-IX)    = MID-KDFRAKT               
033200             AND                                                          
033300               BLK-CDC-IDDISTR-FOM (BLK-IX) NOT > MID-IDDISTR             
033400             AND                                                          
033500               BLK-CDC-IDDISTR-TOM (BLK-IX) NOT < MID-IDDISTR             
033600                                                                          
033700          MOVE BLK-CDC-BLANKETT-ID (BLK-IX) TO W-BLANKETT-ID              
033800     END-SEARCH                                                           
036000     .                                                                    
036100     EJECT                                                                
036200 H-ANROPA-W475IMDG SECTION.                                               
036300                                                                          
036400     IF W-BLANKETT-ID = 'IMDG   '                                         
036500        MOVE '   '                  TO IMDG-IDPTYP                        
036600     ELSE                                                                 
036700        MOVE ' SF'                  TO IMDG-IDPTYP                        
036800     END-IF                                                               
036900     MOVE MID-IDDISTR               TO IMDG-IDDISTR                       
037000     MOVE MID-IDKUNDNR              TO IMDG-IDKUNDNR                      
037100     MOVE MID-KDFRAKT               TO IMDG-KDFRAKT                       
037200     MOVE MID-IDSKEPPN              TO IMDG-IDSKEPPN                      
037300     MOVE MID-IDDC                  TO IMDG-IDDC                          
037400     MOVE W-PRINTER-ID              TO IMDG-IDPRT                         
037500     MOVE MID-IDSYSTEM              TO IMDG-IDSYSTEM                      
037600     MOVE MID-IDTRPTNR              TO IMDG-IDTRPTNR                      
037700     MOVE WS-DAGENS-DATUM           TO IMDG-DASKEPPN                      
037800                                                                          
037900                                                                          
038000     CALL W475IMDG USING IMDG-W475IMDG ALT-PCB                            
038100                                       LISB-PCB 4513-PCB                  
038200                                       1165-PCB GMTA-PCB BETC-PCB         
038300                                       ORDD-PCB WDB6-PCB                  
038400     .                                                                    
038500     EJECT                                                                
038600 I-ANROPA-W475DGR SECTION.                                                
038700                                                                          
038800     MOVE MID-IDDISTR               TO DGR-IDDISTR                        
038900     MOVE MID-IDKUNDNR              TO DGR-IDKUNDNR                       
039000     MOVE MID-KDFRAKT               TO DGR-KDFRAKT                        
039100     MOVE MID-IDSKEPPN              TO DGR-IDSKEPPN                       
039200     MOVE MID-IDDC                  TO DGR-IDDC                           
039300     MOVE W-PRINTER-ID              TO DGR-IDPRT                          
039400     MOVE MID-IDSYSTEM              TO DGR-IDSYSTEM                       
039500     MOVE MID-IDTRPTNR              TO DGR-IDTRPTNR                       
039510*                                                                         
039600*** ANVÄND NEDANSTÅENDE HÅRDKODNING OM MAN VILL SKAPA DGR FÖR             
039700*** ETT GAMMALT DATUM. ASTERISKMÄRK I STÄLLET DE FÖLJANDE                 
039800*** 8 RADERNA. OBS! GÖR DETTA ENBART I TEST OCH UNDER MYCKET              
039900*** ORDNADE FORMER. DET FÅR ALDRIG ALDRIG LIGGA KVAR!!!                   
040000*    MOVE 19970911                  TO DGR-DASKEPPN                       
040010*                                                                         
040100     MOVE MSGI-TILOKDAT             TO WS-FIXA-DATUM                      
040200     MOVE WS-FIXA-DATUM             TO WS-AAMMDD                          
040300     IF WS-AA < 50                                                        
040400       MOVE 20                      TO WS-CENTURY                         
040500     ELSE                                                                 
040600       MOVE 19                      TO WS-CENTURY                         
040700     END-IF                                                               
040800     MOVE WS-CENTURY-DATUM          TO DGR-DASKEPPN                       
040900                                                                          
041000                                                                          
041100                                                                          
041200     CALL W475DGR USING DGR-W475DGR ALT-PCB                               
041300                                    LISB-PCB 4513-PCB                     
041400                                    1165-PCB GMTA-PCB                     
041500                                    ORDD-PCB WDB6-PCB                     
041600     .                                                                    
041700     EJECT                                                                
041800* --- IMS SEKTIONER ---                                                   
041900                                                                          
042000 IMS-GET-MSG SECTION.                                                     
042100                                                                          
042200     MOVE '  QC' TO GODK-STATUSKODER                                      
042300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
042400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042500     PERFORM IMS-STATUSKONTROLL                                           
042600     .                                                                    
042700     SKIP3                                                                
042800 IMS-GU-WL451301 SECTION.                                                 
042900                                                                          
043000     STRING 'WL451301(WDGXKEY  =' W-WDGXKEY-X ')'                         
043100          DELIMITED BY SIZE INTO SSA1                                     
043200     MOVE '  GE' TO GODK-STATUSKODER                                      
043300     CALL CBLTDLI USING GU 4513-PCB DLI-IO-AREA-4513 SSA1                 
043400     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     .                                                                    
043700     SKIP3                                                                
043800 IMS-GHNP-WL451311 SECTION.                                               
043900                                                                          
044000     STRING 'WL451311(DASKEPPN <' W-DASKEPPN-MIN-X ')'                    
044100          DELIMITED BY SIZE INTO SSA1                                     
044200     MOVE '  GE' TO GODK-STATUSKODER                                      
044300     CALL CBLTDLI USING GHNP 4513-PCB DLI-IO-AREA-4514 SSA1               
044400     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
044500     PERFORM IMS-STATUSKONTROLL                                           
044600     .                                                                    
044700     SKIP3                                                                
044800 IMS-DLET-WL451311 SECTION.                                               
044900                                                                          
045000     MOVE '  ' TO GODK-STATUSKODER                                        
045100     CALL CBLTDLI USING DLET 4513-PCB DLI-IO-AREA-4514                    
045200     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
045300     PERFORM IMS-STATUSKONTROLL                                           
045400     .                                                                    
045500     EJECT                                                                
045600 IMS-GU-WDB601    SECTION.                                                
045700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
045800          DELIMITED BY SIZE INTO SSA1                                     
045900     MOVE '  GE' TO GODK-STATUSKODER                                      
046000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
046100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     IF SEGMENT-SAKNAS                                                    
046400         MOVE SPACE TO DCS-KDDC                                           
046500     END-IF                                                               
046600     .                                                                    
046700 IMS-STATUSKONTROLL SECTION.                                              
046800                                                                          
046900     SET STATUS-IX TO 1                                                   
047000     SEARCH GODK-STATUS                                                   
047100       AT END                                                             
047200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047300           DELIMITED BY SIZE INTO FELTEXT                                 
047400         CALL FELLOG                                                      
047500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047600         CONTINUE                                                         
047700     END-SEARCH                                                           
047800     .                                                                    
