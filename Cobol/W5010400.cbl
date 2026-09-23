000100 ID DIVISION.                                                             
000200 PROGRAM-ID. W5010400.                                                    
000300 AUTHOR. RALPH ANDERSON.                                                  
000400 DATE-WRITTEN. APRIL -79.                                                 
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*        TP-PROGRAM FÖR EKONOMI (PRIS OCH BESTÄLLNING).                   
000800*        PROGRAMMET LÄSER DATABASEN WDK6.                                 
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W5T104.                                             
001200*        MID: W5I1041.                                                    
001300*    UTDATA.                                                              
001400*        MOD: W5O1041.                                                    
001500*    SUBPROGRAM.                                                          
001600*        FELLOG.                                                          
001700*        WMEDKONV.                                                        
001800*    SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP3                                                                
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  JA                          PIC X        VALUE 'J'.                  
002700 77  WS-KDAVT                    PIC S9(3)    VALUE ZERO COMP-3.          
002800 77  WS-IDLEVNR                  PIC X(5)     VALUE SPACE.                
002900 77  IDARTNR-WS                  PIC X(9).                                
003000 77  WS-KDNOTTYP-BEST            PIC S9       COMP-3.                     
003100*                                                                         
003200 01  DATUM-FAELT.                                                         
003300     03  W-DAPRLIST-MAX          PIC 9(8)     VALUE 99999999.             
003400     03  W-DAPRLIST              PIC 9(8).                                
003500     03  W-DAPRLIST-X  REDEFINES W-DAPRLIST.                              
003600      05 FILLER                  PIC 9(2).                                
003700      05 W-LISTDATUM             PIC 9(6).                                
003800*                                                                         
003900 01  DYNAMISKA-SUBPROGRAM.                                                
004000     03  CBLTDLI                 PIC X(8)     VALUE 'CBLTDLI '.           
004100     03  FELLOG                  PIC X(8)     VALUE 'FELLOG  '.           
004200     03  WMEDKONV                PIC X(8)     VALUE 'WMEDKONV'.           
004300     03  W005INIT                PIC X(8)     VALUE 'W005INIT'.           
004400     EJECT                                                                
004500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
004600*01 -COPY WMSGINIT                                                        
004700     EJECT                                                                
004800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
004900*01  -COPY WMEDAREA                                                       
005000     SKIP2                                                                
005100 01  MESSAGE-CODES.                                                       
005200     03  ERR-PART-MISSING        PIC X(3)     VALUE '017'.                
005300     03  ERR-PART-DELETED        PIC X(3)     VALUE '018'.                
005400     03  ERR-WRONG-INPUT         PIC X(3)     VALUE '020'.                
005500     03  INF-SPEC-COSTS          PIC X(3)     VALUE '217'.                
005600     EJECT                                                                
005700*                                                                         
005800 01  NYCKEL-TILL-DLI.                                                     
005900     03  W-IDARTNR-X.                                                     
006000         05  W-IDARTNR           PIC S9(9)    VALUE +0   COMP-3.          
006100     03  W-IDBEST-X.                                                      
006200         05  W-IDBEST            PIC S9(13)   VALUE +0   COMP-3.          
006300     03  W-KDSEGKEY-X.                                                    
006400         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
006500     EJECT                                                                
006600*                                                                         
006700 01  FILLER             PIC X(16)    VALUE 'MFS-WS         '.             
006800     SKIP2                                                                
006900*01  MID  -COPY W5I10401    -PRE MID-                                     
007000     EJECT                                                                
007100*01  -COPY WMSGAREA                                                       
007200*                                                                         
007300*    03  MOD -COPY W5O10401  -PRE MOD-  -RED MSG-AREA.                    
007400     EJECT                                                                
007500*01  -COPY WMFSAREA                                                       
007600     EJECT                                                                
007700*****  ARBETSAREOR FÖR IMS-SEKTIONERNA.                                   
007800*                                                                         
007900 01  IMS-WS.                                                              
008000   03 FILLER             PIC X(8)    VALUE 'IMS-WS'.                      
008100     SKIP2                                                                
008200*****  STATUSKOD                                                          
008300*                                                                         
008400   03 STATUS-WS          PIC XX.                                          
008500     88 SEGMENT-FINNS                VALUE '  '.                          
008600     88 SEGMENT-SAKNAS               VALUE 'GE'.                          
008700     SKIP2                                                                
008800   03 GODK-STATUSKODER.                                                   
008900     05  GODK-STATUS  OCCURS 5  INDEXED BY STATUS-IX PIC XX.              
009000     EJECT                                                                
009100*****  IMS-FUNKTIONSKODER                                                 
009200*                                                                         
009300*  03 -COPY W0003                                                         
009400     EJECT                                                                
009500*01  WLARTC01 -COPY WDK601                                                
009600     EJECT                                                                
009700*01  WLARTC11 -COPY WDK611                                                
009800     EJECT                                                                
009900*01  WLARTC21 -COPY WDK621                                                
010000     EJECT                                                                
010100*01  WLARTC22 -COPY WDK622                                                
010200     EJECT                                                                
010300*01  WLARTC23 -COPY WDK623                                                
010400     EJECT                                                                
010500*****  SSA                                                                
010600*                                                                         
010700 01  SSA1                PIC X(40)    VALUE SPACE.                        
010800 01  SSA2                PIC X(40)    VALUE SPACE.                        
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100*01  -COPY W0009 -PRE MSG-                                                
011200     EJECT                                                                
011300*01  -COPY W0008     -PRE USEA-.                                          
011400         05  FILLER           PIC X.                                      
011500*01  -COPY W0008 -PRE ARTC-                                               
011600       05 ARTC-KONKAT-KEY PIC X.                                          
011700                                                                          
011800 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
011900                                  ARTC-PCB.                               
012000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
012100                                   ARTC-PCB                               
012200     PERFORM IMS-GET-MSG                                                  
012300     IF SEGMENT-FINNS                                                     
012400         PERFORM AC-INIT-SPARA-INPUT                                      
012500         IF IDARTNR-WS NOT NUMERIC                                        
012600            MOVE ERR-WRONG-INPUT TO MED-IDMFSFEL                          
012700            CALL WMEDKONV USING MED-WMEDAREA                              
012800            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
012900         ELSE                                                             
013000            PERFORM AA-RENS-FAELT                                         
013100            PERFORM AB-LAS                                                
013200         END-IF                                                           
013300         COMPUTE  MSG-KVLL  = LENGTH OF MOD-W5O10401 + 4                  
013400         PERFORM IMS-ISRT-MSG                                             
013500     END-IF                                                               
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 AA-RENS-FAELT SECTION.                                                   
014100     SET MOD-IX-BE TO 1                                                   
014200     SET MOD-IX-PB TO 1                                                   
014300                                                                          
014400     PERFORM UNTIL MOD-IX-PB > 6                                          
014500         MOVE MFS-BLANKA-UT-FAELT TO MOD-PRIS-BEST (MOD-IX-PB)            
014600         IF MOD-IX-BE < 5                                                 
014700             MOVE MFS-BLANKA-UT-FAELT TO                                  
014800                      MOD-BESTALLNING (MOD-IX-BE)                         
014900             SET MOD-IX-BE UP BY 1                                        
015000         END-IF                                                           
015100         SET MOD-IX-PB UP BY 1                                            
015200     END-PERFORM                                                          
015300     .                                                                    
015400     EJECT                                                                
015500 AB-LAS SECTION.                                                          
015600*                                                                         
015700     MOVE IDARTNR-WS TO W-IDARTNR                                         
015800     PERFORM IMS-GET-ARTIKEL                                              
015900     IF SEGMENT-FINNS                                                     
016000       IF ART-KDERS-UTG > 0                                               
016100         MOVE ERR-PART-DELETED TO MED-IDMFSFEL                            
016200         CALL WMEDKONV USING MED-WMEDAREA                                 
016300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
016400       ELSE                                                               
016500         MOVE '-'     TO  MOD-BINDESTRECK                                 
016600         MOVE ART-REKSIFFR      TO  MOD-REKSIFFR                          
016700         MOVE ART-IDLEVNR       TO  WS-IDLEVNR                            
016800                                                                          
016900         PERFORM IMS-GNP-WDK611                                           
017000         IF SEGMENT-FINNS                                                 
017100           MOVE CLAG-IDINK       TO  MOD-IDINK                            
017200           MOVE CLAG-IDANSK      TO  MOD-IDANSK                           
017300           MOVE CLAG-KDAVT       TO  WS-KDAVT                             
017400           PERFORM ABA-BEHANDLA-BEST-PRIS-INFO                            
017500           IF WS-KDAVT = 0 OR 1                                           
017600               PERFORM ABB-BEHANDLA-BEST                                  
017700               PERFORM ABC-BEHANDLA-AVTAL                                 
017800           ELSE                                                           
017900               PERFORM ABB-BEHANDLA-BEST                                  
018000               PERFORM ABD-BEHANDLA-OVR-AVTAL                             
018100           END-IF                                                         
018200           PERFORM ABE-BEHANDLA-EKONOMI-INFO                              
018300         END-IF                                                           
018400       END-IF                                                             
018500     ELSE                                                                 
018600         MOVE ERR-PART-MISSING TO MED-IDMFSFEL                            
018700         CALL WMEDKONV USING MED-WMEDAREA                                 
018800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
018900     END-IF                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 ABA-BEHANDLA-BEST-PRIS-INFO SECTION.                                     
019300*                                                                         
019400     SET MOD-IX-PB TO 1                                                   
019500                                                                          
019600     PERFORM UNTIL MOD-IX-PB > 6                                          
019700       PERFORM IMS-GET-BEST-PRIS-INFO                                     
019800       IF SEGMENT-FINNS                                                   
019900         IF PRL-FLHUVLEV = JA                                             
020000           SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX               
020100           GIVING W-DAPRLIST                                              
020200           MOVE W-LISTDATUM     TO  MOD-TIPRLIST (MOD-IX-PB)              
020300           MOVE PRL-IDLEVNR     TO MOD-IDLEVNR-PR (MOD-IX-PB)             
020400           MOVE PRL-PRARTBES-PR TO MOD-PRARTBES-PR   (MOD-IX-PB)          
020500           MOVE PRL-PRARTBEL-PR TO MOD-PRARTBEL-PR   (MOD-IX-PB)          
020600           IF PRL-SUINLEV-PR > 0    AND PRL-KDSTATUS-PR = 1               
020700             MOVE 'INLEV' TO MOD-KDSTATUS-PR (MOD-IX-PB)                  
020800             IF MOD-PRARTBES (9:2) NUMERIC                                
020810               CONTINUE                                                   
020900             ELSE                                                         
020901               MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                       
020902             END-IF                                                       
020910           ELSE                                                           
021000             IF PRL-KDSTATUS-PR = 1                                       
021100               MOVE 'GODK'  TO  MOD-KDSTATUS-PR (MOD-IX-PB)               
021200             ELSE                                                         
021300               MOVE 'PREL' TO MOD-KDSTATUS-PR (MOD-IX-PB)                 
021400             END-IF                                                       
021500           END-IF                                                         
021600           SET MOD-IX-PB UP BY 1                                          
021700         END-IF                                                           
021800       ELSE                                                               
021900         SET MOD-IX-PB UP BY 6                                            
022000       END-IF                                                             
022100     END-PERFORM                                                          
022200     .                                                                    
022300     EJECT                                                                
022400 ABB-BEHANDLA-BEST SECTION.                                               
022500*                                                                         
022600      SET MOD-IX-BE TO 1                                                  
022700      PERFORM IMS-GET-BESTALLN-FIRST                                      
022800                                                                          
022900      PERFORM UNTIL NOT (MOD-IX-BE < 5 AND SEGMENT-FINNS)                 
023000          IF BEST-KDBEH-BEST = 1 OR 5 OR 6                                
023100            IF BEST-IDBEST > 0                                            
023200              MOVE BEST-IDBEST  TO  MOD-IDBEST (MOD-IX-BE)                
023300                                    W-IDBEST                              
023400                                                                          
023500              MOVE BEST-KVBEST     TO MOD-KVBEST (MOD-IX-BE)              
023600              MOVE BEST-TIBEST     TO MOD-TIBEST (MOD-IX-BE)              
023700              MOVE BEST-IDLEVNR-BEST TO MOD-IDLEVNR-B(MOD-IX-BE)          
023800              MOVE BEST-KVBEST-BEKR TO MOD-KVBEST-BEKR (MOD-IX-BE)        
023900              IF BEST-KDBEH-BEST = +1                                     
024000                   MOVE 'BESTÄLLN '  TO MOD-KDJUST (MOD-IX-BE)            
024100              ELSE                                                        
024200                 IF BEST-KDBEH-BEST = +5                                  
024300                     MOVE 'ANNULL'   TO MOD-KDJUST (MOD-IX-BE)            
024400                 ELSE                                                     
024500                     MOVE 'BEKR.ANN' TO MOD-KDJUST (MOD-IX-BE)            
024600                 END-IF                                                   
024700             END-IF                                                       
024800             SET MOD-IX-BE UP BY 1                                        
024900         END-IF                                                           
025000         END-IF                                                           
025100         PERFORM IMS-GET-BESTALLN                                         
025200     END-PERFORM                                                          
025300     .                                                                    
025400     EJECT                                                                
025500 ABC-BEHANDLA-AVTAL SECTION.                                              
025600                                                                          
025700     SET MOD-IX-BE TO 4                                                   
025800     PERFORM IMS-GET-AVTAL-FIRST                                          
025900     IF  SEGMENT-FINNS                                                    
026000         MOVE AVT-IDAVTAL       TO MOD-IDBEST (MOD-IX-BE)                 
026100         MOVE AVT-IDLEVNR-AVT   TO MOD-IDLEVNR-B (MOD-IX-BE)              
026200         MOVE AVT-KVAVTANT      TO MOD-KVBEST (MOD-IX-BE)                 
026300         MOVE AVT-TIAVTAL       TO MOD-TIBEST (MOD-IX-BE)                 
026400         MOVE 'AVTAL '          TO MOD-KDJUST (MOD-IX-BE)                 
026500     END-IF                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 ABD-BEHANDLA-OVR-AVTAL SECTION.                                          
026900*                                                                         
027000     SET MOD-IX-BE TO 4                                                   
027100     MOVE SPACE       TO  MOD-BESTALLNING (MOD-IX-BE)                     
027200     IF WS-KDAVT = 3                                                      
027300         MOVE 'KONCERN '      TO MOD-KDJUST (MOD-IX-BE)                   
027400     ELSE                                                                 
027500         IF WS-KDAVT = 2                                                  
027600             MOVE 'USA / CAN' TO MOD-KDJUST (MOD-IX-BE)                   
027700         ELSE                                                             
027800             MOVE ' HF '      TO MOD-KDJUST (MOD-IX-BE)                   
027900         END-IF                                                           
028000     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 ABE-BEHANDLA-EKONOMI-INFO SECTION.                                       
028400*                                                                         
028500      MOVE CLAG-PRINK       TO MOD-PRINK                                  
028600      MOVE CLAG-PRARTSTD TO MOD-PRARTSTD                                  
028800      MOVE CLAG-PRARTSJK TO MOD-PRARTSJK                                  
028900      MOVE CLAG-PRDIRLON TO MOD-PRDIRLON                                  
029000      MOVE CLAG-PRDMTRL TO MOD-PRDMTRL                                    
029100      MOVE CLAG-PROVRPAL TO MOD-PROVRPAL                                  
029101      EVALUATE CLAG-KDTIPPR                                               
029102        WHEN 1                                                            
029103          MOVE 'Y'       TO MOD-KDTIPPR                                   
029104        WHEN 0                                                            
029105          MOVE 'N'       TO MOD-KDTIPPR                                   
029106        WHEN 3                                                            
029107          MOVE 'A'       TO MOD-KDTIPPR                                   
029108      END-EVALUATE                                                        
029330                                                                          
029400****      IF EKO-FLSPKOST = +1  ** DETTA FÄLT ANVÄNDS EJ LÄNGRE *         
029500****      BÖR KANSKE RENSAS FRÅN BASEN        ****                        
029600      MOVE MFS-BLANKA-UT-FAELT TO MOD-TEMFSINF                            
029700     .                                                                    
029800     EJECT                                                                
029900 AC-INIT-SPARA-INPUT SECTION.                                             
030000*                                                                         
030100     IF MSG-DUBBLA-TRANSKODER                                             
030200         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I10401               
030300         MOVE MSG-IDTRANS-2   TO  MFS-IDTRANS                             
030400         MOVE MSG-KDMFSFOR-2  TO  MFS-KDMFSFOR                            
030500     ELSE                                                                 
030600         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W5I10401               
030700         MOVE MSG-IDTRANS-1   TO  MFS-IDTRANS                             
030800         MOVE MSG-KDMFSFOR-1  TO  MFS-KDMFSFOR                            
030900     END-IF                                                               
031000*                                                                         
031100                                                                          
031200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
031300     MOVE '001'             TO MSGI-KDCALL                                
031400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
031500     MOVE '5104'               TO MSGI-IDTRANS                            
031600     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
031700     IF MFS-IDTRANS = '5104'                                              
031800     OR (MID-IDARTNR1 NUMERIC                                             
031900     AND MID-IDARTNR1 > ZERO)                                             
032000         MOVE MID-IDARTNR1 TO MSGI-IDARTNR                                
032100     END-IF                                                               
032200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
032300     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
032400     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
032500                                                                          
032600*                                                                         
032700     MOVE SPACE      TO  MSG-AREA                                         
032800     MOVE 'W5O104N1' TO MFS-IDMOD                                         
032900     MOVE '5104'     TO MOD-IDTRANS                                       
033000     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
033100*==>                                                                      
033200     INSPECT MOD-IDARTNR-UT REPLACING  LEADING ZERO BY SPACE              
033300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
033400                             MOD-TEMFSINF                                 
033500     EJECT                                                                
033600*****  IMS SEKTIONER                                                      
033700*                                                                         
033800     .                                                                    
033900 IMS-GET-MSG SECTION.                                                     
034000     MOVE '  QC' TO GODK-STATUSKODER                                      
034100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
034200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034300     PERFORM IMS-STATUSKONTROLL                                           
034400     SKIP3                                                                
034500     .                                                                    
034600 IMS-ISRT-MSG SECTION.                                                    
034700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
034800        MOVE '0' TO MFS-KDHUVOMR                                          
034900     END-IF                                                               
035000                                                                          
035100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
035200     MOVE SPACE TO GODK-STATUSKODER                                       
035300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
035400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035500     PERFORM IMS-STATUSKONTROLL                                           
035600     EJECT                                                                
035700     .                                                                    
035800 IMS-GET-ARTIKEL SECTION.                                                 
035900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
036000     DELIMITED  BY SIZE INTO SSA1                                         
036100     MOVE '  GE' TO GODK-STATUSKODER                                      
036200     CALL CBLTDLI USING GU ARTC-PCB WLARTC01 SSA1                         
036300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
036400     PERFORM IMS-STATUSKONTROLL                                           
036500     SKIP3                                                                
036600     .                                                                    
036700 IMS-GNP-WDK611 SECTION.                                                  
036800*                                                                         
036900     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
037000     DELIMITED  BY SIZE INTO SSA1                                         
037100     MOVE SPACE TO GODK-STATUSKODER                                       
037200     CALL CBLTDLI USING GNP ARTC-PCB WLARTC11 SSA1                        
037300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
037400     PERFORM IMS-STATUSKONTROLL                                           
037500     .                                                                    
037600     EJECT                                                                
037700 IMS-GET-BEST-PRIS-INFO SECTION.                                          
037800*                                                                         
037900     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
038000     MOVE 'WLARTC21 ' TO SSA2                                             
038100*                                                                         
038200     MOVE '  GE' TO GODK-STATUSKODER                                      
038300     CALL CBLTDLI USING GNP ARTC-PCB WLARTC21 SSA1 SSA2                   
038400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
038500     PERFORM IMS-STATUSKONTROLL                                           
038600     SKIP3                                                                
038700     .                                                                    
038800 IMS-GET-BESTALLN SECTION.                                                
038900*                                                                         
039000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
039100     MOVE 'WLARTC22 ' TO SSA2                                             
039200*                                                                         
039300     MOVE '  GE' TO GODK-STATUSKODER                                      
039400     CALL CBLTDLI USING GNP ARTC-PCB WLARTC22 SSA1 SSA2                   
039500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
039600     PERFORM IMS-STATUSKONTROLL                                           
039700     .                                                                    
039800 IMS-GET-BESTALLN-FIRST  SECTION.                                         
039900*                                                                         
040000     MOVE 'WLARTC11*F' TO SSA1                                            
040100     MOVE 'WLARTC22 ' TO SSA2                                             
040200*                                                                         
040300     MOVE '  GE' TO GODK-STATUSKODER                                      
040400     CALL CBLTDLI USING GNP ARTC-PCB WLARTC22 SSA1 SSA2                   
040500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     SKIP3                                                                
040800     .                                                                    
040900 IMS-GET-AVTAL-FIRST  SECTION.                                            
041000*                                                                         
041100     MOVE 'WLARTC11*F' TO SSA1                                            
041200     MOVE 'WLARTC23 ' TO SSA2                                             
041300*                                                                         
041400     MOVE '  GE' TO GODK-STATUSKODER                                      
041500     CALL CBLTDLI USING GNP ARTC-PCB WLARTC23 SSA1 SSA2                   
041600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
041700     PERFORM IMS-STATUSKONTROLL                                           
041800     .                                                                    
041900     EJECT                                                                
042000 IMS-STATUSKONTROLL SECTION.                                              
042100*                                                                         
042200     SET STATUS-IX TO 1                                                   
042300     SEARCH GODK-STATUS                                                   
042400        AT END CALL FELLOG                                                
042500           WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE              
042600     END-SEARCH                                                           
042700     .                                                                    
