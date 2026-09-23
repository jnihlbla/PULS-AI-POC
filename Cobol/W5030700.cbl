000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5030700.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   94/12/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        INLEVERANS HISTORIK SDC/LDC (WDL6)                               
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001200*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W5T307                                              
001600*        MID:         W5I30701                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W5O30701                                            
002000*                                                                         
002100*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
002200*                                                                         
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W5030700'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  WS-IDDC                     PIC X(2)   VALUE 'N'.                    
003900 77  WS-NDC-US-RU                PIC X(2)   VALUE '41'.                   
004000 77  WS-NDC-CA                   PIC X(2)   VALUE '51'.                   
004100                                                                          
004200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004500 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004600*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
004700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +998  COMP SYNC.        
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005100 77  WS-TIREGDAT                 PIC 9(6)    VALUE ZERO.                  
005200 77  WS-IDINLEV-REGDAT           PIC 9(6)    VALUE ZERO.                  
005300 77  WS-DAINLEV                  PIC 9(16)   VALUE ZERO.                  
005400                                                                          
005500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005600     88  NYCKLAR-OK                          VALUE 'J'.                   
005700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005800                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  EGEN-MID                            VALUE '5307'.                
006100     88  GODK-MID                            VALUE '5301' '5302'          
006200                                                   '5303' '5304'          
006300                                                   '5305' '5306'          
006400                                                   '5307' '5308'          
006500                                                   '5309'.                
006600     88  HELP-MID                            VALUE '0551'.                
006700     EJECT                                                                
006800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006900 01  GENERELLA-SUBPROGRAM.                                                
007000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007700*01 -COPY WMSGINIT                                                        
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008000*01 -COPY WMEDAREA                                                        
008100     EJECT                                                                
008200 01  MESSAGE-CODES.                                                       
008300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008600     03  PARTNO-MISSING          PIC X(3)    VALUE '017'.                 
008700     EJECT                                                                
008800*01  -COPY WDATAREA                                                       
008900     EJECT                                                                
009000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009300     SKIP3                                                                
009400*01  MID -COPY W5I30701                                                   
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009700     SKIP3                                                                
009800*01  -COPY WMSGAREA                                                       
009900     EJECT                                                                
010000     03  MOD REDEFINES MSG-AREA.                                          
010100*      05  -COPY W5O30701                                                 
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010400     SKIP3                                                                
010500*01  -COPY WMFSAREA                                                       
010600     EJECT                                                                
010700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800*                                                                         
010900     SKIP2                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100     SKIP3                                                                
011200 01  NYCKLAR-TILL-DLI.                                                    
011300     03  W-IDARTNR-X.                                                     
011400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011500                                                                          
011600     03  W-DAINLEV-X.                                                     
011700         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
011800                                                                          
011900     03  W-IDSKYLT-X.                                                     
012000         05  W-IDSKYLT           PIC X(3)     VALUE 'GB '.                
012100                                                                          
012200     03  W-IDDC                  PIC X(2)     VALUE SPACE.                
012300                                                                          
012400     03  W-IDDC-B6-X.                                                     
012500         05 W-IDDC-B6                  PIC X(2).                          
012600                                                                          
012610     03  W-IDDC-MIN-X.                                                    
012620         05 W-IDDC-MIN                 PIC X(2) VALUE LOW-VALUES.         
012621                                                                          
012630     03  W-IDDC-MAX-X.                                                    
012640         05 W-IDDC-MAX                 PIC X(2) VALUE HIGH-VALUES.        
012700     SKIP2                                                                
012800*    --- STATUS-KOD FRÅN IMS                                              
012900 01  STATUS-WS                   PIC XX.                                  
013000     88  SEGMENT-FINNS                       VALUE '  '.                  
013100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013300     SKIP2                                                                
013400 01  GODK-STATUSKODER.                                                    
013500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013600     SKIP3                                                                
013700 01  SSA1                        PIC X(128).                              
013800 01  SSA2                        PIC X(64).                               
013900     EJECT                                                                
014000*    --- IMS FUNKTIONSKODER                                               
014100*01  -COPY W0003                                                          
014200     EJECT                                                                
014300*    ---  DLI INPUT-OUTPUT AREA                                           
014400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014500     SKIP3                                                                
014600 01  DLI-IO-AREA.                                                         
014700     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
014800     SKIP3                                                                
014900     03  WLINLC01 REDEFINES IO-AREA.                                      
015000*        05  -COPY WDL601  -PRE INLC-                                     
015100     EJECT                                                                
015200     03  WLINLC11 REDEFINES IO-AREA.                                      
015300*        05  -COPY WDL611  -PRE INLC-                                     
015400     EJECT                                                                
015500 01  DLI-IO-AREA2.                                                        
015600     03  WLBENA11.                                                        
015700*        05  -COPY WDD311  -PRE BENA-                                     
015800                                                                          
015900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
016000 01   DLI-IO-AREA-B601.                                                   
016100*     03  -COPY WDB601                                                    
016200                                                                          
016300     EJECT                                                                
016400 LINKAGE SECTION.                                                         
016500                                                                          
016600*01  -COPY W0009   -PRE MSG-                                              
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE USEA-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008  -PRE INLC-                                              
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400*01  -COPY W0008  -PRE BENA-                                              
017500     05  FILLER                  PIC X.                                   
017600     EJECT                                                                
017700*01  -COPY W0008  -PRE WDB6-                                              
017800     05  FILLER                  PIC X.                                   
017900     EJECT                                                                
018000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
018100                                   INLC-PCB BENA-PCB WDB6-PCB.            
018200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
018300                                   INLC-PCB BENA-PCB WDB6-PCB.            
018400                                                                          
018500     PERFORM IMS-GET-MSG                                                  
018600     IF SEGMENT-FINNS                                                     
018700       PERFORM A-INIT                                                     
018800       PERFORM B-KOLLA-NYCKLAR                                            
018900       IF NYCKLAR-OK                                                      
019000           IF MFS-FIRST                                                   
019100             PERFORM C-FOERSTA-SIDA                                       
019200           ELSE                                                           
019300             IF MFS-NEXT                                                  
019400               PERFORM D-NAESTA-SIDA                                      
019500             ELSE                                                         
019600               PERFORM E-SAMMA-SIDA                                       
019700             END-IF                                                       
019800           END-IF                                                         
019900         PERFORM F-LAES-VISA-INFO                                         
020000       END-IF                                                             
020100       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
020200       PERFORM IMS-INSERT-MSG                                             
020300     END-IF                                                               
020400                                                                          
020500     MOVE ZERO TO RETURN-CODE                                             
020600     GOBACK                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 A-INIT SECTION.                                                          
021000                                                                          
021100     IF MSG-DUBBLA-TRANSKODER                                             
021200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I30701                 
021300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021500     ELSE                                                                 
021600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I30701                  
021700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
021800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021900     END-IF                                                               
022000                                                                          
022100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022400                                                                          
022500     MOVE LOW-VALUE TO MSG-AREA                                           
022600     MOVE 'W5O30701' TO MFS-IDMOD                                         
022700     MOVE '5307' TO MOD-IDTRANS                                           
022800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
022900                                                                          
023000*    --- OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4               
023100*    --- OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17              
023200     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W5O30701 + 4                  
023300                                                                          
023400     IF EGEN-MID OR HELP-MID                                              
023500       CONTINUE                                                           
023600     ELSE                                                                 
023700       MOVE SPACE TO MFS-KDTRTYP                                          
023800       MOVE '7' TO MFS-IDPFK                                              
023900     END-IF                                                               
024000                                                                          
024100     MOVE +2 TO SPRAK-IX                                                  
024200     MOVE 'GB ' TO MED-IDSKYLT                                            
024300     .                                                                    
024400     EJECT                                                                
024500 B-KOLLA-NYCKLAR SECTION.                                                 
024600                                                                          
024700     MOVE JA TO NYCKLAR-SW                                                
024800                                                                          
024900*    -- KONTROLL AV IDARTNR                                               
025000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
025100                                                                          
025200     MOVE ALL '+' TO MSGI-WMSGINIT                                        
025300     MOVE '001'             TO MSGI-KDCALL                                
025400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
025500     MOVE '5307'            TO MSGI-IDTRANS                               
025600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
025700                                                                          
025800     IF MFS-IDTRANS = '5307'                                              
025900     OR (MID-IDARTNR-IN NUMERIC                                           
026000     AND MID-IDARTNR-IN > ZERO)                                           
026100         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
026200     END-IF                                                               
026300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026400     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
026500     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
026600                                                                          
026700     IF MID-IDARTNR-IN = ALL '+'                                          
026800       CONTINUE                                                           
026900     ELSE                                                                 
027000       MOVE '7'         TO MFS-IDPFK                                      
027100       MOVE SPACE       TO MFS-KDTRTYP                                    
027200     END-IF                                                               
027300                                                                          
027400     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
027500       MOVE WS-IDARTNR TO W-IDARTNR                                       
027600     ELSE                                                                 
027700       MOVE NEJ TO NYCKLAR-SW                                             
027800     END-IF                                                               
027900                                                                          
028000*    -- KONTROLL AV IDDC                                                  
028100     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
028200                                                                          
028300     IF MID-IDDC-IN = ALL '+'                                             
028400       MOVE MID-IDDC-UT TO WS-IDDC                                        
028500       INSPECT WS-IDDC REPLACING LEADING SPACE BY ZERO                    
028600     ELSE                                                                 
028700       MOVE MID-IDDC-IN TO WS-IDDC                                        
028800       MOVE '7'         TO MFS-IDPFK                                      
028900       MOVE SPACE       TO MFS-KDTRTYP                                    
029000     END-IF                                                               
029100                                                                          
029200     MOVE WS-IDDC TO W-IDDC-B6                                            
029300     PERFORM IMS-GU-WDB601                                                
029400                                                                          
029500     IF DCS-KDDC NOT = SPACE AND NOT DCS-DDC                              
029600       MOVE WS-IDDC TO W-IDDC                                             
029700     ELSE                                                                 
029800       IF WS-IDDC = '00' OR                                               
029900          NOT EGEN-MID                                                    
030000         MOVE ZERO TO W-IDDC                                              
030100                      WS-IDDC                                             
030200       ELSE                                                               
030300         MOVE ZERO TO W-IDDC                                              
030400                      WS-IDDC                                             
030500         MOVE NEJ TO NYCKLAR-SW                                           
030600       END-IF                                                             
030700     END-IF                                                               
030800                                                                          
030900     MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                    
031000     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
031100     MOVE WS-IDDC    TO MOD-IDDC-UT                                       
031200                                                                          
031300     IF NYCKLAR-FEL                                                       
031400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
031500       CALL WMEDKONV USING MED-WMEDAREA                                   
031600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
031700*      PERFORM MFS-RENSA-FAELT-IN                                         
031800       PERFORM MFS-RENSA-FAELT-UT                                         
031900     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 C-FOERSTA-SIDA SECTION.                                                  
032300                                                                          
032400     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
032500     CALL WMEDKONV USING MED-WMEDAREA                                     
032600     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
032700                                                                          
032800*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
032900*    MOVE SPACE TO W-IDDC                                                 
033000*    MOVE ZERO  TO W-DAINLEV                                              
033100*    PERFORM MFS-RENSA-FAELT-IN                                           
033200     .                                                                    
033300     EJECT                                                                
033400 D-NAESTA-SIDA SECTION.                                                   
033500                                                                          
033600     MOVE MID-IDDC-NEXT TO W-IDDC                                         
033700     IF MID-IDINLEV-NEXT  NUMERIC                                         
033800       MOVE MID-IDINLEV-NEXT TO W-DAINLEV                                 
033900       IF MID-IDINLEV-NEXT (1:1) = 0                                      
034000         MOVE 8 TO W-DAINLEV (1:1)                                        
034100       ELSE                                                               
034200         MOVE 7 TO W-DAINLEV (1:1)                                        
034300       END-IF                                                             
034400     ELSE                                                                 
034500       MOVE ZERO TO W-DAINLEV                                             
034600     END-IF                                                               
034700*    PERFORM MFS-RENSA-FAELT-IN                                           
034800     .                                                                    
034900     EJECT                                                                
035000 E-SAMMA-SIDA SECTION.                                                    
035100                                                                          
035200     IF EGEN-MID OR HELP-MID                                              
035300       MOVE MID-IDDC-ENTER TO W-IDDC                                      
035400       IF MID-IDINLEV-ENTER NUMERIC                                       
035500         MOVE MID-IDINLEV-ENTER TO W-DAINLEV                              
035600         IF MID-IDINLEV-ENTER (1:1) = 0                                   
035700           MOVE 8 TO W-DAINLEV (1:1)                                      
035800         ELSE                                                             
035900           MOVE 7 TO W-DAINLEV (1:1)                                      
036000         END-IF                                                           
036100       ELSE                                                               
036200         MOVE ZERO TO W-DAINLEV                                           
036300       END-IF                                                             
036400     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 F-LAES-VISA-INFO SECTION.                                                
036800                                                                          
036900     PERFORM IMS-GU-ART-INLC                                              
037000                                                                          
037100     IF SEGMENT-SAKNAS                                                    
037200       MOVE PARTNO-MISSING TO MED-IDMFSFEL                                
037300       CALL WMEDKONV USING MED-WMEDAREA                                   
037400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
037500       PERFORM MFS-RENSA-FAELT-UT                                         
037600     ELSE                                                                 
037700       PERFORM IMS-GU-WLBENA11                                            
037800       IF SEGMENT-FINNS                                                   
037900         MOVE BENA-TEXT-BEART TO MOD-BEART                                
038000       ELSE                                                               
038100         MOVE SPACE           TO MOD-BEART                                
038200       END-IF                                                             
038300                                                                          
038400       EVALUATE TRUE                                                      
038500         WHEN DCS-SDC AND DCS-IDLANDX2 = 'NL'                             
038600           MOVE 'NL'    TO MOD-SDC-LAGER                                  
038700         WHEN DCS-SDC AND DCS-IDLANDX2 = 'GB'                             
038800           MOVE 'UK'    TO MOD-SDC-LAGER                                  
038900         WHEN DCS-SDC AND DCS-IDLANDX2 = 'DE'                             
039000           MOVE 'DE'    TO MOD-SDC-LAGER                                  
039100         WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                             
039200           MOVE 'SPAIN' TO MOD-SDC-LAGER                                  
039300         WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                             
039400           MOVE 'ITALY' TO MOD-SDC-LAGER                                  
039500         WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                             
039600           MOVE 'AUSTR' TO MOD-SDC-LAGER                                  
039700         WHEN DCS-SDC AND DCS-IDLANDX2 = 'SE'                             
039800           MOVE 'SWE  ' TO MOD-SDC-LAGER                                  
039900         WHEN DCS-IDLANDX2 = 'CN'                                         
040000           MOVE 'CHN  ' TO MOD-SDC-LAGER                                  
040100         WHEN DCS-IDLANDX2 = 'US'                                         
040110           MOVE 'US'    TO MOD-SDC-LAGER                                  
040120         WHEN DCS-IDLANDX2 = 'CA'                                         
040130           MOVE 'CA'    TO MOD-SDC-LAGER                                  
040200         WHEN OTHER                                                       
040300           MOVE SPACE   TO MOD-SDC-LAGER                                  
040400       END-EVALUATE                                                       
040500                                                                          
040600       MOVE +1 TO INDX                                                    
040700                                                                          
040800       IF WS-IDDC = ZERO                                                  
040900         PERFORM IMS-GNP-INLEVSDC-ALL                                     
041000       ELSE                                                               
041100         PERFORM IMS-GNP-INLEVSDC                                         
041200       END-IF                                                             
041300                                                                          
041400       IF SEGMENT-FINNS                                                   
041500         MOVE INLC-INL-IDDC    TO MOD-IDDC-ENTER                          
041600         MOVE INLC-INL-DAINLEV (2:15) TO MOD-IDINLEV-ENTER                
041700       ELSE                                                               
041800         MOVE SPACE            TO MOD-IDDC-ENTER                          
041900         MOVE ZERO             TO MOD-IDINLEV-ENTER                       
042000       END-IF                                                             
042100                                                                          
042200       PERFORM UNTIL INDX > MAX-INDX                                      
042300         IF SEGMENT-FINNS                                                 
042400           MOVE INLC-INL-IDPTYP   TO MOD-IDPTYP   (INDX)                  
042500           MOVE INLC-INL-IDDC     TO MOD-IDDC     (INDX)                  
042600                                                                          
042700           MOVE INLC-INL-DAINLEV  TO WS-DAINLEV                           
042800           MOVE WS-DAINLEV (3:6)  TO WS-IDINLEV-REGDAT                    
042900           COMPUTE WS-TIREGDAT = 999999 - WS-IDINLEV-REGDAT               
043000           MOVE WS-TIREGDAT       TO MOD-TIREGDAT (INDX)                  
043100                                                                          
043200           MOVE INLC-INL-TIINLMOT TO MOD-TIINLMOT (INDX)                  
043300           MOVE INLC-INL-TIINLINL TO MOD-TIINLINL (INDX)                  
043400           MOVE INLC-INL-IDKUNDRF TO MOD-IDKUNDRF (INDX)                  
043500           MOVE INLC-INL-IDFAKT   TO MOD-IDFAKT   (INDX)                  
043600           MOVE INLC-INL-KVAVIS   TO MOD-KVAVIS   (INDX)                  
043700           MOVE INLC-INL-KVANTMOT TO MOD-KVANTMOT (INDX)                  
043800           MOVE INLC-INL-ADLAGOMR TO MOD-ADLAGOMR (INDX)                  
043900           MOVE INLC-INL-ADGANG   TO MOD-ADGANG   (INDX)                  
044000           MOVE INLC-INL-ADPLATS  TO MOD-ADPLATS  (INDX)                  
044100                                                                          
044200           IF WS-IDDC = ZERO                                              
044300             PERFORM IMS-GNP-INLEVSDC-ALL                                 
044400           ELSE                                                           
044500             PERFORM IMS-GNP-INLEVSDC                                     
044600           END-IF                                                         
044700         ELSE                                                             
044800           MOVE MFS-RENSA-FAELT   TO MOD-IDPTYP   (INDX)                  
044900                                     MOD-IDDC     (INDX)                  
045000                                     MOD-TIREGDAT (INDX)                  
045100                                     MOD-TIINLMOT (INDX)                  
045200                                     MOD-TIINLINL (INDX)                  
045300                                     MOD-IDKUNDRF (INDX)                  
045400                                     MOD-IDFAKT   (INDX)                  
045500                                     MOD-KVAVIS   (INDX)                  
045600                                     MOD-KVANTMOT (INDX)                  
045700                                     MOD-ADLAGOMR (INDX)                  
045800                                     MOD-ADGANG   (INDX)                  
045900                                     MOD-ADPLATS  (INDX)                  
046000         END-IF                                                           
046100         ADD 1 TO INDX                                                    
046200       END-PERFORM                                                        
046300                                                                          
046400       IF SEGMENT-FINNS                                                   
046500         MOVE INLC-INL-IDDC TO MOD-IDDC-NEXT                              
046600         MOVE INLC-INL-DAINLEV (2:15) TO MOD-IDINLEV-NEXT                 
046700         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
046800         CALL WMEDKONV USING MED-WMEDAREA                                 
046900         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
047000       ELSE                                                               
047100         MOVE SPACE            TO MOD-IDDC-NEXT                           
047200         MOVE ZERO             TO MOD-IDINLEV-NEXT                        
047300       END-IF                                                             
047400                                                                          
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 MFS-RENSA-FAELT-UT SECTION.                                              
047900                                                                          
048000*    --- ALLA UTDATA-FÄLT                                                 
048100*    --- INKL. BLÄDDRINGSNYCKLAR                                          
048200     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
048300                             MOD-IDDC-ENTER                               
048400                             MOD-IDDC-NEXT                                
048500                             MOD-IDINLEV-ENTER                            
048600                             MOD-IDINLEV-NEXT                             
048700     MOVE +1 TO INDX                                                      
048800     PERFORM UNTIL INDX > MAX-INDX                                        
048900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
049000       ADD +1 TO INDX                                                     
049100     END-PERFORM                                                          
049200     .                                                                    
049300     SKIP3                                                                
049400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
049500                                                                          
049600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
049700     MOVE MFS-RENSA-FAELT TO MOD-IDPTYP   (INDX)                          
049800                             MOD-IDDC     (INDX)                          
049900                             MOD-TIREGDAT (INDX)                          
050000                             MOD-TIINLMOT (INDX)                          
050100                             MOD-TIINLINL (INDX)                          
050200                             MOD-IDKUNDRF (INDX)                          
050300                             MOD-IDFAKT   (INDX)                          
050400                             MOD-KVAVIS   (INDX)                          
050500                             MOD-KVANTMOT (INDX)                          
050600                             MOD-ADLAGOMR (INDX)                          
050700                             MOD-ADGANG   (INDX)                          
050800                             MOD-ADPLATS  (INDX)                          
050900     .                                                                    
051000     EJECT                                                                
051100* --- IMS SEKTIONER ---                                                   
051200     SKIP3                                                                
051300 IMS-GET-MSG SECTION.                                                     
051400                                                                          
051500     MOVE '  QC' TO GODK-STATUSKODER                                      
051600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
051700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
051800     PERFORM IMS-STATUSKONTROLL                                           
051900     .                                                                    
052000     SKIP3                                                                
052100 IMS-INSERT-MSG SECTION.                                                  
052200                                                                          
052300     IF ENGLISH-TEXT                                                      
052400       MOVE 'N' TO MFS-KDHUVOMR                                           
052500     END-IF                                                               
052600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
052700     MOVE SPACE TO GODK-STATUSKODER                                       
052800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
052900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053000     PERFORM IMS-STATUSKONTROLL                                           
053100     .                                                                    
053200     EJECT                                                                
053300 IMS-GU-ART-INLC SECTION.                                                 
053400                                                                          
053500     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
053600          DELIMITED BY SIZE INTO SSA1                                     
053700     MOVE '  GE' TO GODK-STATUSKODER                                      
053800     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA SSA1                      
053900     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
054000     PERFORM IMS-STATUSKONTROLL                                           
054100     .                                                                    
054200     EJECT                                                                
054300 IMS-GNP-INLEVSDC SECTION.                                                
054400                                                                          
054500     STRING 'WLINLC11(DAINLEV =>' W-DAINLEV-X                             
054600                    '&IDDC     =' W-IDDC ')'                              
054700          DELIMITED BY SIZE INTO SSA1                                     
054800     MOVE '  GE' TO GODK-STATUSKODER                                      
054900     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-AREA SSA1                     
055000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
055100     PERFORM IMS-STATUSKONTROLL                                           
055200     .                                                                    
055300     SKIP2                                                                
055400 IMS-GNP-INLEVSDC-ALL SECTION.                                            
055500                                                                          
055600     STRING 'WLINLC11(DAINLEV =>' W-DAINLEV-X                             
055700                    '&IDDC     >' W-IDDC-MIN-X                            
055800                    '+DAINLEV =>' W-DAINLEV-X                             
055900                    '&IDDC     <' W-IDDC-MAX-X ')'                        
056000          DELIMITED BY SIZE INTO SSA1                                     
056100     MOVE '  GE' TO GODK-STATUSKODER                                      
056200     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-AREA SSA1                     
056300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600     EJECT                                                                
056700 IMS-GU-WLBENA11 SECTION.                                                 
056800                                                                          
056900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
057000          DELIMITED BY SIZE INTO SSA1                                     
057100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
057200          DELIMITED BY SIZE INTO SSA2                                     
057300     MOVE '  GE' TO GODK-STATUSKODER                                      
057400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1 SSA2                
057500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
057600     PERFORM IMS-STATUSKONTROLL                                           
057700     .                                                                    
057800     EJECT                                                                
057900 IMS-GU-WDB601    SECTION.                                                
058000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
058100          DELIMITED BY SIZE INTO SSA1                                     
058200     MOVE '  GE' TO GODK-STATUSKODER                                      
058300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
058400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
058500     PERFORM IMS-STATUSKONTROLL                                           
058600     IF SEGMENT-SAKNAS                                                    
058700         MOVE SPACE TO DCS-KDDC                                           
058800     END-IF                                                               
058900     .                                                                    
059000 IMS-STATUSKONTROLL SECTION.                                              
059100                                                                          
059200     SET STATUS-IX TO 1                                                   
059300     SEARCH GODK-STATUS                                                   
059400       AT END                                                             
059500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059600         DELIMITED BY SIZE INTO FELTEXT                                   
059700         CALL FELLOG                                                      
059800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
059900         CONTINUE                                                         
060000     END-SEARCH                                                           
060100     .                                                                    
