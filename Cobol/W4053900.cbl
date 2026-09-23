000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4053900.                                                
000300 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000400 DATE-WRITTEN.   94/02/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT BAKGRUNDSMPP                                   
001000*        SKAPAR BLANKETTER VIA SUBPROGRAM  : W475ADR                      
001100*                                            W475DGR                      
001200*                                                                         
001300*       (ALLA BLANKETTERNA STYRS MOT PRINTER R44052 (NJO00037))           
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T539X                                             
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
002700 77  IDPGM                       PIC X(08)   VALUE 'W4053900'.            
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
003800       EJECT                                                              
003900 77  ALLT-SW                     PIC X.                                   
004000     88  ALLT-OK                             VALUE 'J'.                   
004100     88  ALLT-FEL                            VALUE 'N'.                   
004200                                                                          
004300*01  -COPY WWDCKONS                                                       
004400                                                                          
004500 01  W-IDDCTEXT-MSGI.                                                     
004600     03  FILLER              PIC X(5)   VALUE 'WIDDC'.                    
004700     03  W-IDDC-MSGI         PIC X(2).                                    
004800                                                                          
004900 01  WS-FIXA-DATUM               PIC 9(6).                                
005000 01  WS-CENTURY-DATUM.                                                    
005100     03 WS-CENTURY                 PIC 9(2).                              
005200     03 WS-AAMMDD.                                                        
005300       05 WS-AA                    PIC 9(2).                              
005400       05 WS-MM                    PIC 9(2).                              
005500       05 WS-DD                    PIC 9(2).                              
005600                                                                          
005700 01  FELKODER.                                                            
005800     03  FEL-IDDISTR             PIC X(4)    VALUE SPACE.                 
005900     03  FEL-IDKUNDNR            PIC X(6)    VALUE SPACE.                 
006000     03  FEL-IDSKEPPN            PIC X(7)    VALUE SPACE.                 
006100     03  FEL-IDSYSTEM            PIC X(4)    VALUE SPACE.                 
006200     03  FEL-IDDC                PIC X(2)    VALUE SPACE.                 
006300                                                                          
006400     SKIP3                                                                
006500 01  RETURKODER.                                                          
006600     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
006700                                                                          
006800     EJECT                                                                
006900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007000 01  GENERELLA-SUBPROGRAM.                                                
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
007500     03  W475ADR                 PIC X(8)    VALUE 'W475ADR '.            
007600     03  W475DGR                 PIC X(8)    VALUE 'W475DGR '.            
007700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008000*01 -COPY WMSGINIT                                                        
008100     EJECT                                                                
008200*                                                                         
008300     EJECT                                                                
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008600                                                                          
008700*01  MID -COPY W4I53901                                                   
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
009000                                                                          
009100*01  -COPY WMSGAREA                                                       
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
009400*01  -COPY WORKAREA                                                       
009500     EJECT                                                                
009600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009700*                                                                         
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900                                                                          
010000 01  NYCKLAR-TILL-DLI.                                                    
010100     03  W-WDGXKEY-X.                                                     
010200         05  W-IDHTYP            PIC X(4)    VALUE '4513'.                
010300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
010400     SKIP2                                                                
010500     03  W-DASKEPPN-MIN-X.                                                
010600         05  W-DASKEPPN-MIN      PIC 9(8)   VALUE ZERO.                   
010700                                                                          
010800     03  W-IDDC-B6-X.                                                     
010900         05 W-IDDC-B6                  PIC X(2).                          
011000                                                                          
011100     EJECT                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200     EJECT                                                                
012300 01  FILLER                      PIC X(16)   VALUE ALL 'W475BLKT'.        
012400*    --- BLANKETT-ID-TABELL                                               
012500*01  FILLER   -COPY W475BLKT                                              
012600     EJECT                                                                
012700                                                                          
012800*    --- LÄNKCOPYTEXTER                                                   
012900*01  FILLER   -COPY W475ADR     -PRE ADR-                                 
013000     EJECT                                                                
013100*01  FILLER   -COPY W475DGR     -PRE DGR-                                 
013200     EJECT                                                                
013300 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
013400*01  FILLER   -COPY WWDIST66    -RED TEST-IDDISTR.                        
013500     EJECT                                                                
013501*      --- VALID IDDC CODES                                               
013502*                                                                         
013503*01    -COPY WWDC99                                                       
013504      EJECT                                                               
013600                                                                          
013700*    --- IMS FUNKTIONSKODER                                               
013800*01  -COPY W0003                                                          
013900     EJECT                                                                
014000*    ---  DLI INPUT-OUTPUT AREA                                           
014100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4513'.         
014200     SKIP3                                                                
014300 01  DLI-IO-AREA-4513.                                                    
014400     03  WL451301.                                                        
014500*        05  -COPY WDGX01                                                 
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4514'.         
014800     SKIP3                                                                
014900 01  DLI-IO-AREA-4514.                                                    
015000     03  WL451311.                                                        
015100*        05  -COPY WDGX4514                                               
015200                                                                          
015300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
015400 01   DLI-IO-AREA-B601.                                                   
015500*     03  -COPY WDB601                                                    
015600                                                                          
015700     EJECT                                                                
015800 LINKAGE SECTION.                                                         
015900*01  -COPY W0009   -PRE MSG-                                              
016000                                                                          
016100*01  -COPY W0009   -PRE ALT-                                              
016200     EJECT                                                                
016300*01  -COPY W0009   -PRE DAP-                                              
016310     EJECT                                                                
016400*01  -COPY W0008   -PRE USEA-                                             
016500     05  FILLER                  PIC X.                                   
016600     EJECT                                                                
016700*01  -COPY W0008  -PRE LISB-                                              
016800     05  FILLER                  PIC X.                                   
016900     EJECT                                                                
017000*01  -COPY W0008  -PRE 4513-                                              
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300*01  -COPY W0008  -PRE 1165-                                              
017400     05  FILLER                  PIC X.                                   
017500                                                                          
017600*01  -COPY W0008  -PRE GMTA-                                              
017700     05  FILLER                  PIC X.                                   
017800     EJECT                                                                
018200*01  -COPY W0008  -PRE ORDD-                                              
018300     05  FILLER                  PIC X.                                   
018400     EJECT                                                                
018500*01  -COPY W0008  -PRE WDB6-                                              
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB DAP-PCB                        
018900                           USEA-PCB LISB-PCB                              
019000                           4513-PCB 1165-PCB GMTA-PCB                     
019100                           ORDD-PCB WDB6-PCB.                             
019200 MAIN SECTION.                                                            
019300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB DAP-PCB                        
019400                           USEA-PCB LISB-PCB                              
019500                           4513-PCB 1165-PCB GMTA-PCB                     
019600                           ORDD-PCB WDB6-PCB.                             
019700                                                                          
019800     PERFORM IMS-GET-MSG                                                  
019900     IF SEGMENT-FINNS                                                     
020000       IF MSG-KDTRTYP = 'X'                                               
020100         PERFORM A-INIT                                                   
020200         PERFORM B-RENSA-GAMLA-4513                                       
020210                                                                          
020300         IF (DCS-NDC-NA AND DCS-CANADA) OR                                
020310            (DCS-NDC-CN AND NDC-CN-71)                                    
020400           PERFORM I-ANROPA-W475DGR                                       
020500         ELSE                                                             
020600           PERFORM G-ANROPA-W475ADR                                       
020700         END-IF                                                           
020710                                                                          
020800       END-IF                                                             
020900     END-IF                                                               
021000                                                                          
021100     MOVE ZERO TO RETURN-CODE                                             
021200     GOBACK                                                               
021300     .                                                                    
021400     EJECT                                                                
021500 A-INIT SECTION.                                                          
021600                                                                          
021700     MOVE JA  TO ALLT-SW                                                  
021800                                                                          
021900     MOVE MSG-INDATA-MINUS-1-TRANSKOD     TO MID-W4I53901                 
022000                                                                          
022100     IF MID-IDDISTR NUMERIC                                               
022200        MOVE MID-IDDISTR                  TO TEST-IDDISTR                 
022300     ELSE                                                                 
022400        MOVE MID-IDDISTR                  TO FEL-IDDISTR                  
022500        MOVE NEJ TO ALLT-SW                                               
022600     END-IF                                                               
022700                                                                          
022800     IF MID-IDKUNDNR NUMERIC                                              
022900        CONTINUE                                                          
023000     ELSE                                                                 
023100        MOVE MID-IDKUNDNR                 TO FEL-IDKUNDNR                 
023200        MOVE NEJ TO ALLT-SW                                               
023300     END-IF                                                               
023400                                                                          
023500     IF MID-IDSKEPPN NUMERIC                                              
023600        CONTINUE                                                          
023700     ELSE                                                                 
023800        MOVE MID-IDSKEPPN                 TO FEL-IDSKEPPN                 
023900        MOVE NEJ TO ALLT-SW                                               
024000     END-IF                                                               
024100                                                                          
024200     MOVE MID-IDDC                        TO W-IDDC-B6                    
024300     PERFORM IMS-GU-WDB601                                                
024400                                                                          
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
026100       MOVE '4539'           TO MSGI-IDTRANS                              
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
032000 G-ANROPA-W475ADR SECTION.                                                
032100                                                                          
032200     MOVE MID-IDDISTR               TO ADR-IDDISTR                        
032300     MOVE MID-IDKUNDNR              TO ADR-IDKUNDNR                       
032400     MOVE MID-KDFRAKT               TO ADR-KDFRAKT                        
032500     MOVE MID-IDSKEPPN              TO ADR-IDSKEPPN                       
032600     MOVE MID-IDDC                  TO ADR-IDDC                           
032700     MOVE W-PRINTER-ID              TO ADR-IDPRT                          
032800     MOVE MID-IDSYSTEM              TO ADR-IDSYSTEM                       
032900     MOVE MID-IDTRPTNR              TO ADR-IDTRPTNR                       
033000     MOVE WS-DAGENS-DATUM           TO ADR-DASKEPPN                       
033100     CALL W475ADR USING ADR-W475ADR ALT-PCB                               
033200                                    LISB-PCB 4513-PCB                     
033300                                    1165-PCB GMTA-PCB                     
033400                                    ORDD-PCB WDB6-PCB                     
033500                                                                          
033600     MOVE MID-IDDISTR               TO TEST-IDDISTR                       
033700     IF DIST66-STYRN-REP                                                  
033800        MOVE 'REP'                  TO ADR-IDPRT                          
033900                                                                          
034000        CALL W475ADR USING ADR-W475ADR ALT-PCB                            
034100                                       LISB-PCB 4513-PCB                  
034200                                       1165-PCB GMTA-PCB                  
034300                                       ORDD-PCB WDB6-PCB                  
034400     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 I-ANROPA-W475DGR SECTION.                                                
034800                                                                          
034900     MOVE MID-IDDISTR               TO DGR-IDDISTR                        
035000     MOVE MID-IDKUNDNR              TO DGR-IDKUNDNR                       
035100     MOVE MID-KDFRAKT               TO DGR-KDFRAKT                        
035200     MOVE MID-IDSKEPPN              TO DGR-IDSKEPPN                       
035300     MOVE MID-IDDC                  TO DGR-IDDC                           
035400     MOVE W-PRINTER-ID              TO DGR-IDPRT                          
035500     MOVE MID-IDSYSTEM              TO DGR-IDSYSTEM                       
035600     MOVE MID-IDTRPTNR              TO DGR-IDTRPTNR                       
035700*** ANVÄND NEDANSTÅENDE HÅRDKODNING OM MAN VILL SKAPA DGR FÖR             
035800*** ETT GAMMALT DATUM. ASTERISKMÄRK I STÄLLET DE FÖLJANDE                 
035900*** 8 RADERNA. OBS! GÖR DETTA ENBART I TEST OCH UNDER MYCKET              
036000*** ORDNADE FORMER. DET FÅR ALDRIG ALDRIG LIGGA KVAR!!!                   
036100*    MOVE 19970911                  TO DGR-DASKEPPN                       
036200     MOVE MSGI-TILOKDAT             TO WS-FIXA-DATUM                      
036300     MOVE WS-FIXA-DATUM             TO WS-AAMMDD                          
036400     IF WS-AA < 50                                                        
036500       MOVE 20                      TO WS-CENTURY                         
036600     ELSE                                                                 
036700       MOVE 19                      TO WS-CENTURY                         
036800     END-IF                                                               
036900     MOVE WS-CENTURY-DATUM          TO DGR-DASKEPPN                       
037000                                                                          
037100                                                                          
037200                                                                          
037300     CALL W475DGR USING DGR-W475DGR ALT-PCB                               
037400                                    LISB-PCB 4513-PCB                     
037500                                    1165-PCB GMTA-PCB                     
037600                                    ORDD-PCB WDB6-PCB                     
037700     .                                                                    
037800     EJECT                                                                
037900* --- IMS SEKTIONER ---                                                   
038000                                                                          
038100 IMS-GET-MSG SECTION.                                                     
038200                                                                          
038300     MOVE '  QC' TO GODK-STATUSKODER                                      
038400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
038500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038600     PERFORM IMS-STATUSKONTROLL                                           
038700     .                                                                    
038800     SKIP3                                                                
038900 IMS-GU-WL451301 SECTION.                                                 
039000                                                                          
039100     STRING 'WL451301(WDGXKEY  =' W-WDGXKEY-X ')'                         
039200          DELIMITED BY SIZE INTO SSA1                                     
039300     MOVE '  GE' TO GODK-STATUSKODER                                      
039400     CALL CBLTDLI USING GU 4513-PCB DLI-IO-AREA-4513 SSA1                 
039500     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
039600     PERFORM IMS-STATUSKONTROLL                                           
039700     .                                                                    
039800     SKIP3                                                                
039900 IMS-GHNP-WL451311 SECTION.                                               
040000                                                                          
040100     STRING 'WL451311(DASKEPPN <' W-DASKEPPN-MIN-X ')'                    
040200          DELIMITED BY SIZE INTO SSA1                                     
040300     MOVE '  GE' TO GODK-STATUSKODER                                      
040400     CALL CBLTDLI USING GHNP 4513-PCB DLI-IO-AREA-4514 SSA1               
040500     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     .                                                                    
040800     SKIP3                                                                
040900 IMS-DLET-WL451311 SECTION.                                               
041000                                                                          
041100     MOVE '  ' TO GODK-STATUSKODER                                        
041200     CALL CBLTDLI USING DLET 4513-PCB DLI-IO-AREA-4514                    
041300     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
041400     PERFORM IMS-STATUSKONTROLL                                           
041500     .                                                                    
041600     EJECT                                                                
041700 IMS-GU-WDB601    SECTION.                                                
041800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
041900          DELIMITED BY SIZE INTO SSA1                                     
042000     MOVE '  GE' TO GODK-STATUSKODER                                      
042100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
042200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
042300     PERFORM IMS-STATUSKONTROLL                                           
042400     IF SEGMENT-SAKNAS                                                    
042500         MOVE SPACE TO DCS-KDDC                                           
042600     END-IF                                                               
042700     .                                                                    
042800 IMS-STATUSKONTROLL SECTION.                                              
042900                                                                          
043000     SET STATUS-IX TO 1                                                   
043100     SEARCH GODK-STATUS                                                   
043200       AT END                                                             
043300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
043400           DELIMITED BY SIZE INTO FELTEXT                                 
043500         CALL FELLOG                                                      
043600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
043700         CONTINUE                                                         
043800     END-SEARCH                                                           
043900     .                                                                    
