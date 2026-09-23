000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9042500.                                                
000300 AUTHOR.         KENT HELLQVIST.                                          
000400 DATE-WRITTEN.   FEBRUARI 1986.                                           
000500     REMARKS.                                                             
000600*        FUNKTION.                                                        
000700*                                                                         
000800*        SKIP2                                                            
000900*        INDATA.                                                          
001000*            TRANSAKTION: W90425T                                         
001100*                         W90425U                                         
001200*            MID:     W90425I1                                            
001300*        UTDATA.                                                          
001400*            MOD:     W90425O1                                            
001500*        DYNAMISKA SUBPROGRAM.                                            
001600*                                                                         
001700*   ÄNDRINGAR:                                                            
001800*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
001900*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002000*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002100*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002200*                                                                         
002300                                                                          
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900*    -COPY WY2000W1                                                       
003000     SKIP3                                                                
003100 77  PROGRAM-NAMN                PIC X(8)   VALUE 'W9042500'.             
003200 77  JA                          PIC X(01)         VALUE 'J'.             
003300 77  NEJ                         PIC X(01)         VALUE 'N'.             
003400 77  INDX                        PIC S9(9)  VALUE +0   COMP SYNC.         
003500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +410 COMP SYNC.         
003600 77  MAX-TILEVDAGAR              PIC 9(5)   VALUE 5.                      
003700 77  WS-IDARTNR                  PIC X(09).                               
003800 77  WS-REDIRLEV-C1              PIC 9V9(02)       VALUE ZERO.            
003900 77  WS-DISP-LAGER               PIC 9(09) COMP-3  VALUE ZERO.            
004000 77  WS-IDLOGLOP                 PIC 9(01) COMP-3  VALUE ZERO.            
004100 77  WS-IDARTNR-8                PIC 9(08).                               
004200                                                                          
004300 77  WS-IDLEVNR-8                PIC X(08) VALUE SPACE.                   
004400                                                                          
005500 77  WS-IDTRANS                  PIC X(04).                               
005600     88  EGEN-BILD                                 VALUE '9425'.          
007300                                                                          
007400                                                                          
007500     EJECT                                                                
007600 01  SWITCHAR.                                                            
007700     05  SW-INPUT-RAETT          PIC X(01)  VALUE 'J'.                    
007800     05  SW-BASEN-RAETT          PIC X(01)  VALUE 'J'.                    
007900     05  SW-GODKAENT-ID          PIC X(01)  VALUE 'N'.                    
008000                                                                          
008100 01  WS-HJALP-AAVV1.                                                      
008200     05  WS-AAVV-NOLL            PIC 9(01)  VALUE ZERO.                   
008300     05  WS-AA                   PIC 9(02).                               
008400     05  WS-VV                   PIC 9(02).                               
008500                                                                          
008600 01  WS-HJALP-AAVV2  REDEFINES WS-HJALP-AAVV1                             
008700                                 PIC S9(05).                              
008800                                                                          
008900 01  WS-IDAVTAL                  PIC 9(13).                               
009000 01  WS-IDAVTAL-RED REDEFINES WS-IDAVTAL.                                 
009100     05  FILLER                  PIC X(01).                               
009200     05  WS-IDAVTAL-PREFIX       PIC X(03).                               
009300     05  WS-IDAVTAL-AVTALNR      PIC X(06).                               
009400     05  WS-IDAVTAL-SUFFIX       PIC X(03).                               
009500                                                                          
009600 01  DAGENS-DATUM                PIC 9(06).                               
009700                                                                          
011300 01  DYNAMISKA-SUBPROGRAM.                                                
011400     05  WDATKONV                PIC X(08)  VALUE 'WDATKONV'.             
011500     05  WDECEDIT                PIC X(08)  VALUE 'WDECEDIT'.             
011600     05  FELLOG                  PIC X(08)  VALUE 'FELLOG  '.             
011700     05  CBLTDLI                 PIC X(08)  VALUE 'CBLTDLI '.             
011800     05  WMEDKONV                PIC X(08)  VALUE 'WMEDKONV'.             
011900     05  W005INIT                PIC X(08)  VALUE 'W005INIT'.             
012000     EJECT                                                                
012100*01  AREA -COPY W092W001        -PRE W092-                                
012200     EJECT                                                                
012300******************************************************************        
012400*    I N K Ö P S - P O S T    P V                                         
012500******************************************************************        
012600*                                                                         
012700*01  -COPY A310TB65             -PRE A310-                                
012800     EJECT                                                                
012900******************************************************************        
013000*    I N K Ö P S - P O S T    L V                                         
013100******************************************************************        
013200*                                                                         
013300*    ANV EJ  T310TTV5                                                     
013400     EJECT                                                                
013500                                                                          
013600******************************************************************        
013700*    N Y C K L A R  T I L L  D L I                                        
013800******************************************************************        
013900*                                                                         
014000 01  NYCKLAR-TILL-DLI.                                                    
014100     03  W-IDARTNR-X.                                                     
014200         05  W-IDARTNR            PIC S9(09) COMP-3 VALUE ZERO.           
014300                                                                          
014400     03  W-IDLEVNR-X.                                                     
014500         05  W-IDLEVNR            PIC X(05)  VALUE SPACE.                 
014600                                                                          
014700     03  W-WDG3KEY-2221-X.                                                
014800         05  FILLER               PIC X(04)  VALUE '2221'.                
014900         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
015000                                                                          
015100     03  W-WDG3KEY-2213-X.                                                
015200         05  FILLER               PIC X(04)  VALUE '2213'.                
015300         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
015400                                                                          
015500     03  W-IDSKYLT-X.                                                     
015600         05  W-IDSKYLT            PIC X(03)  VALUE SPACE.                 
015700                                                                          
015800     03  W-KDNOTTYP-X.                                                    
015900         05  W-KDNOTTYP           PIC S9(01) COMP-3 VALUE ZERO.           
016000                                                                          
016100     03  W-1141KEY-X.                                                     
016200         05  FILLER              PIC X(04)  VALUE '1141'.                 
016300         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
016400     EJECT                                                                
016500******************************************************************        
016600*    F E L M E D D E L A N D E N                                          
016700******************************************************************        
016800*                                                                         
016900*01 -COPY WMEDAREA                                                        
017000     SKIP3                                                                
017100 01  MESSAGE-CODES.                                                       
017200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
017300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
017500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
017600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017700                                                                          
017800 01  MEDDELANDE.                                                          
017900     03  FEL-1                   PIC X(32) VALUE                          
018000            'ARTIKELNUMMER EJ NUMERISKT      '.                           
018100     03  FEL-2                   PIC X(32) VALUE                          
018200            'UPPLYSTA FÄLT FEL               '.                           
018300     03  FEL-3                   PIC X(32) VALUE                          
018400            'ARTIKELNUMMER SAKNAS            '.                           
018500     03  FEL-4                   PIC X(34) VALUE                          
018600            'ARTIKEL UTGÅNGEN, UPPD EJ TILLÅTEN'.                         
018700     03  FEL-5                   PIC X(34) VALUE                          
018800            'STD.PRIS = 0,     UPPD EJ TILLÅTEN'.                         
018900     03  FEL-6                   PIC X(34) VALUE                          
019000            'OBEHÖRIG ANVÄNDARE '.                                        
019100                                                                          
019200     03  MED-1                   PIC X(32) VALUE                          
019300            'MER INFO PÅ NÄSTA SIDA          '.                           
019400     03  MED-2                   PIC X(32) VALUE                          
019500            'TRYCK PF11 FÖR UPPDATERING      '.                           
019600     03  MED-3                   PIC X(45) VALUE                          
019700            'UPPDATERING UTFÖRD, UPPLYSTA FÄLT UPPDATERADE'.              
019800     EJECT                                                                
019900*01  -COPY WDATAREA                                                       
020000     EJECT                                                                
020100*                    **** PARAMETRAR TILL W005INIT                        
020200*01  -COPY WMSGINIT                                                       
020300     EJECT                                                                
020400*01  -COPY WDECAREA                                                       
020500     EJECT                                                                
020600*                        ****    MFS OCH SKÄRMHANTERING                   
020700 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
020800     SKIP2                                                                
020900*01  MID -COPY W90425I1                                                   
021000     EJECT                                                                
021100*01  -COPY WMSGAREA                                                       
021200     EJECT                                                                
021300*    03  MOD -COPY W90425O1  -RED MSG-AREA.                               
021400     EJECT                                                                
021500*01  -COPY WMFSAREA.                                                      
021600     EJECT                                                                
021700******************************************************************        
021800*****                                                                     
021900*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022000*****                                                                     
022100 01  IMS-WS.                                                              
022200     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
022300     SKIP3                                                                
022400*****                    **** STATUS-KOD FRÅN IMS                         
022500     03  STATUS-WS               PIC X(2).                                
022600         88  SEGMENT-FINNS                   VALUE '  '.                  
022700         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
022800     SKIP3                                                                
022900     03  GODK-STATUSKODER.                                                
023000         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
023100     SKIP3                                                                
023200 01  SSA1                        PIC X(64).                               
023300 01  SSA2                        PIC X(64).                               
023400 01  SSA3                        PIC X(64).                               
023500     EJECT                                                                
023600*                            IMS FUNKTIONSKODER                           
023700*01  -COPY W0003                                                          
023800     EJECT                                                                
023900*                            DLI INPUT-OUTPUT AREA                        
024000 01  DLI-IO-AREA-01.                                                      
024300*    03  -COPY WDK601.                                                    
024400     EJECT                                                                
024500 01  DLI-IO-AREA-11.                                                      
024800*    03  -COPY WDK611.                                                    
024900     EJECT                                                                
025000 01  DLI-IO-AREA1.                                                        
025300*    03  -COPY WDK625.                                                    
025400     EJECT                                                                
025500 01  DLI-IO-AREA4.                                                        
025600     03  IO-AREA4                  PIC X(50) VALUE SPACE.                 
025700     SKIP3                                                                
025800*    03  XXBN -COPY WDGX01      -PRE XXBN-        -RED IO-AREA4.          
025900     EJECT                                                                
026000*    03  XXBN -COPY WDGX2222    -PRE XXBN-        -RED IO-AREA4.          
026100     EJECT                                                                
026200 LINKAGE SECTION.                                                         
026300     SKIP2                                                                
026400*01  -COPY W0009     -PRE MSG-                                            
026500     EJECT                                                                
026600*01  -COPY W0008     -PRE USEA-                                           
026700         05  FILLER              PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008     -PRE ARTC-                                           
027000         05  FILLER              PIC X.                                   
027100     EJECT                                                                
027200*01  -COPY W0008     -PRE XXBN-                                           
027300         05  FILLER              PIC X.                                   
027400     EJECT                                                                
027500 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
027600                                  ARTC-PCB                                
027700                                  XXBN-PCB.                               
027800     SKIP1                                                                
027900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
028000                                   ARTC-PCB                               
028100                                   XXBN-PCB.                              
028200     PERFORM IMS-GET-MSG                                                  
028300     IF SEGMENT-FINNS                                                     
028400        PERFORM A-INIT-SPARA-INPUT                                        
028500        IF WS-IDARTNR NUMERIC                                             
028600           MOVE WS-IDARTNR  TO W-IDARTNR                                  
028700           PERFORM IMS-GET-ARTC01                                         
028800           IF SEGMENT-FINNS                                               
028900              MOVE ART-IDLEVNR TO WS-IDLEVNR-8                            
029000*             --- KOLLA BEHÖRIGHET                                        
029100              IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                   
029200              OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE             
029300*                --- BEHÖRIG USER                                         
029400                 IF ART-KDERS-UTG > ZERO                                  
029500                    MOVE FEL-4 TO MOD-TEMFSFEL                            
029600                 ELSE                                                     
030200                    IF (MFS-UPDATE AND EGEN-BILD) OR                      
030300                       (MFS-UPD-V AND EGEN-BILD)                          
030400                       PERFORM B-KOLLA-INPUT                              
030500                       IF SW-INPUT-RAETT = JA                             
030600                          PERFORM C-KOLLA-MOT-BASEN                       
030700                          IF SW-BASEN-RAETT = JA                          
030800                             PERFORM D-UPPDATERA                          
030900                          END-IF                                          
031000                       END-IF                                             
031100                    ELSE                                                  
031200                       PERFORM E-VISA-BILD                                
031300                    END-IF                                                
031400                 END-IF                                                   
031500              ELSE                                                        
031600*                -- EJ BEHÖRIG USER                                       
031700                 MOVE FEL-6 TO MOD-TEMFSFEL                               
031800              END-IF                                                      
031900           ELSE                                                           
032000              MOVE FEL-3                    TO MOD-TEMFSFEL               
032100           END-IF                                                         
032200        ELSE                                                              
032300           MOVE FEL-1 TO MOD-TEMFSFEL                                     
032400        END-IF                                                            
032500                                                                          
032600        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
032700        PERFORM IMS-INSERT-MSG                                            
032800     END-IF                                                               
032900                                                                          
033000     MOVE ZERO TO RETURN-CODE                                             
033100     GOBACK                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 A-INIT-SPARA-INPUT SECTION.                                              
033500     SKIP2                                                                
033600     IF MSG-DUBBLA-TRANSKODER                                             
033700         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90425I1               
033800         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS WS-IDTRANS                     
033900         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
034000         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
034100     ELSE                                                                 
034200         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90425I1                
034300         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS WS-IDTRANS                     
034400         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
034500         MOVE SPACE TO MFS-KDTRTYP                                        
034600     END-IF                                                               
034700                                                                          
034800     MOVE ALL '+' TO MSGI-WMSGINIT                                        
034900     MOVE '001'             TO MSGI-KDCALL                                
035000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
035100                               MSGI-IDLTERM-USER                          
035200     MOVE '9425'            TO MSGI-IDTRANS                               
035300     IF MFS-IDTRANS = '9425'                                              
035400     OR (MID-IDARTNR-IN NUMERIC                                           
035500     AND MID-IDARTNR-IN > ZERO)                                           
035600         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
035700     END-IF                                                               
035800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035900     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
036000     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
036100                                                                          
036200     IF MID-IDARTNR-IN = ALL '+'                                          
036300        CONTINUE                                                          
036400     ELSE                                                                 
036500        MOVE SPACE TO MFS-KDTRTYP                                         
036600     END-IF                                                               
036700                                                                          
036800     MOVE LOW-VALUE TO MOD-W90425O1                                       
036900     MOVE 'W90425O1' TO MFS-IDMOD                                         
037000     MOVE '9425' TO MOD-IDTRANS                                           
037100                                                                          
037200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
037300                             MOD-TEMFSINF                                 
037400                                                                          
037500     ACCEPT DAGENS-DATUM FROM DATE                                        
037600     .                                                                    
037700     EJECT                                                                
037800 B-KOLLA-INPUT SECTION.                                                   
037900     SKIP2                                                                
038000     MOVE JA TO SW-INPUT-RAETT                                            
038100     IF MID-INPUT = ALL '+'                                               
038200        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
038300        CALL WMEDKONV USING MED-WMEDAREA                                  
038400        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
038500        PERFORM S02-ROER-EJ-FAELT                                         
038600        MOVE NEJ TO SW-INPUT-RAETT                                        
038700     ELSE                                                                 
038800        PERFORM BA-KOLLA-MID-INDATA                                       
038900        IF SW-INPUT-RAETT = NEJ                                           
039000          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
039100          CALL WMEDKONV USING MED-WMEDAREA                                
039200          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                               
039300          PERFORM S02-ROER-EJ-FAELT                                       
039400        END-IF                                                            
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800                                                                          
039900                                                                          
040000 BA-KOLLA-MID-INDATA SECTION.                                             
040100                                                                          
040200     IF MID-TEARTNOT1 NOT = ALL '+'                                       
040300        MOVE MFS-ALFA-FAELT-RAETT       TO MOD-TEARTNOT1-IN-ATTR          
040400     ELSE                                                                 
040500        MOVE MFS-RENSA-FAELT            TO MOD-TEARTNOT1-IN               
040600     END-IF                                                               
040700                                                                          
040800     IF MID-TEARTNOT2 NOT = ALL '+'                                       
040900        MOVE MFS-ALFA-FAELT-RAETT       TO MOD-TEARTNOT2-IN-ATTR          
041000     ELSE                                                                 
041100        MOVE MFS-RENSA-FAELT            TO MOD-TEARTNOT2-IN               
041200     END-IF                                                               
041300                                                                          
041400     IF SW-INPUT-RAETT = NEJ                                              
041500        PERFORM S02-ROER-EJ-FAELT                                         
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900                                                                          
042000 C-KOLLA-MOT-BASEN SECTION.                                               
042100     SKIP3                                                                
042200******************************************************************        
042300*  TEST OM C2-SEGMENT FINNS UTFÖRS NÄR             KDGK ÄR IFYLLD*        
042400*  I VISSA LÄGEN SKALL OM INTE C2 FINNS ANROP PÅ SUBMODULEN      *        
042500*  W200C2UP GÖRAS, SOM SKAPAR C2-SEGMENT MED DEFAULT-VÄRDEN.     *        
042600******************************************************************        
042700*                                                                         
042800     MOVE JA                         TO SW-BASEN-RAETT                    
043000                                                                          
043100     PERFORM IMS-GNP-ARTC11                                               
043200                                                                          
043300     IF SW-BASEN-RAETT = NEJ                                              
043400        PERFORM S02-ROER-EJ-FAELT                                         
043500        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
043600        CALL WMEDKONV USING MED-WMEDAREA                                  
043700        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
043800     END-IF                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 D-UPPDATERA SECTION.                                                     
044200     SKIP2                                                                
044300                                                                          
044400     PERFORM S02-ROER-EJ-FAELT                                            
044500                                                                          
044600        PERFORM IMS-GET-ARTC01                                            
044800        PERFORM IMS-GHNP-ARTC11                                           
045100                                                                          
045200     IF MID-TEARTNOT1 = ALL '+'  AND                                      
045300        MID-TEARTNOT2 = ALL '+'                                           
045400           CONTINUE                                                       
045500     ELSE                                                                 
045600        IF MID-TEARTNOT1 NOT = ALL '+'                                    
045700           MOVE 1                      TO W-KDNOTTYP                      
045800           PERFORM IMS-GHNP-ARTC25                                        
045900           IF MID-TEARTNOT1  = SPACE                                      
046000              IF SEGMENT-FINNS                                            
046100                 PERFORM IMS-DELETE                                       
046200                 MOVE SPACE            TO MOD-TEARTNOT1-IN                
046300                 MOVE MFS-ADD-LYS-UPP-FAELT TO                            
046400                                          MOD-TEARTNOT1-IN-ATTR           
046500              ELSE                                                        
046600                 CONTINUE                                                 
046700              END-IF                                                      
046800           ELSE                                                           
046900              MOVE MID-TEARTNOT1       TO NOT-TEARTNOT                    
047000                                          MOD-TEARTNOT1-IN                
047100              MOVE 1                   TO NOT-KDNOTTYP                    
047200              IF SEGMENT-FINNS                                            
047300                 PERFORM IMS-REPLACE-ARTC                                 
047400              ELSE                                                        
047500                 PERFORM IMS-INSERT-ARTC25                                
047600              END-IF                                                      
047700              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEARTNOT1-IN-ATTR         
047800           END-IF                                                         
047900        END-IF                                                            
048000                                                                          
048100        IF MID-TEARTNOT2 NOT = ALL '+'                                    
048200           MOVE 2                      TO W-KDNOTTYP                      
048300           PERFORM IMS-GHNP-ARTC25                                        
048400           IF MID-TEARTNOT2  = SPACE                                      
048500              IF SEGMENT-FINNS                                            
048600                 PERFORM IMS-DELETE                                       
048700                 MOVE SPACE            TO MOD-TEARTNOT2-IN                
048800                 MOVE MFS-ADD-LYS-UPP-FAELT TO                            
048900                                        MOD-TEARTNOT2-IN-ATTR             
049000              ELSE                                                        
049100                 CONTINUE                                                 
049200              END-IF                                                      
049300           ELSE                                                           
049400              MOVE MID-TEARTNOT2       TO NOT-TEARTNOT                    
049500                                          MOD-TEARTNOT2-IN                
049600              MOVE 2                   TO NOT-KDNOTTYP                    
049700              IF SEGMENT-FINNS                                            
049800                 PERFORM IMS-REPLACE-ARTC                                 
049900              ELSE                                                        
050000                 PERFORM IMS-INSERT-ARTC25                                
050100              END-IF                                                      
050200              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEARTNOT2-IN-ATTR         
050300           END-IF                                                         
050400        END-IF                                                            
050500     END-IF                                                               
050600                                                                          
050700     MOVE 'S  '                      TO W-IDSKYLT                         
050800     MOVE MED-3                      TO MOD-TEMFSINF                      
050900     .                                                                    
051000     EJECT                                                                
051100 E-VISA-BILD SECTION.                                                     
051200     SKIP2                                                                
051300     PERFORM IMS-GNP-ARTC11                                               
051400                                                                          
051500     MOVE 1                          TO W-KDNOTTYP                        
051600     PERFORM IMS-GET-ARTC25                                               
051700     IF SEGMENT-FINNS                                                     
051800        MOVE NOT-TEARTNOT            TO MOD-TEARTNOT1-IN                  
051900     ELSE                                                                 
052000        MOVE MFS-RENSA-FAELT         TO MOD-TEARTNOT1-IN                  
052100     END-IF                                                               
052200                                                                          
052300     MOVE 2                          TO W-KDNOTTYP                        
052400     PERFORM IMS-GET-ARTC25                                               
052500     IF SEGMENT-FINNS                                                     
052600        MOVE NOT-TEARTNOT            TO MOD-TEARTNOT2-IN                  
052700     ELSE                                                                 
052800        MOVE MFS-RENSA-FAELT         TO MOD-TEARTNOT2-IN                  
052900     END-IF                                                               
053000                                                                          
053100*    FORTSÄTTNING.                                                        
053200     MOVE 'S  '                      TO W-IDSKYLT                         
053300     .                                                                    
053400     EJECT                                                                
053500 S02-ROER-EJ-FAELT SECTION.                                               
053600     SKIP3                                                                
053700     MOVE MFS-ROER-EJ-FAELT          TO                                   
053800                                        MOD-TEARTNOT1-IN                  
053900                                        MOD-TEARTNOT2-IN                  
054000     .                                                                    
054100     EJECT                                                                
054200* IMS SEKTIONER                                                           
054300     SKIP3                                                                
054400 IMS-GET-MSG SECTION.                                                     
054500     SKIP2                                                                
054600     MOVE '  QC' TO GODK-STATUSKODER                                      
054700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
054800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
054900     PERFORM IMS-STATUS-KONTROLL                                          
055000     .                                                                    
055100     SKIP3                                                                
055200 IMS-INSERT-MSG SECTION.                                                  
055300      SKIP2                                                               
055400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
055500     MOVE SPACE TO GODK-STATUSKODER                                       
055600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
055700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055800     PERFORM IMS-STATUS-KONTROLL                                          
055900     .                                                                    
056000     EJECT                                                                
056100 IMS-GET-ARTC01 SECTION.                                                  
056200     SKIP2                                                                
056300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
056400             DELIMITED BY SIZE INTO SSA1                                  
056500     MOVE '  GE' TO GODK-STATUSKODER                                      
056600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
056700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
056800     PERFORM IMS-STATUS-KONTROLL                                          
056900     .                                                                    
057000     SKIP3                                                                
057100 IMS-GHNP-ARTC11 SECTION.                                                 
057200     SKIP2                                                                
057300     MOVE 'WLARTC11*F'  TO SSA1                                           
057400     MOVE '  GE' TO GODK-STATUSKODER                                      
057500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-11 SSA1                 
057600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
057700     PERFORM IMS-STATUS-KONTROLL                                          
057800     .                                                                    
057900     SKIP3                                                                
058000 IMS-GNP-ARTC11 SECTION.                                                  
058100     SKIP2                                                                
058200     MOVE 'WLARTC11*F '  TO SSA1                                          
058300     MOVE '  GE' TO GODK-STATUSKODER                                      
058400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
058500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
058600     PERFORM IMS-STATUS-KONTROLL                                          
058700     .                                                                    
058800     EJECT                                                                
058900 IMS-INSERT-ARTC25 SECTION.                                               
059000     SKIP2                                                                
059100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
059200             DELIMITED BY SIZE INTO SSA1                                  
059300     MOVE  'WLARTC11(KDSEGKEY =1)'  TO SSA2                               
059400     MOVE  'WLARTC25 ' TO SSA3                                            
059500     MOVE '  ' TO GODK-STATUSKODER                                        
059600     CALL CBLTDLI USING ISRT ARTC-PCB                                     
059700                               DLI-IO-AREA1                               
059800                               SSA1                                       
059900                               SSA2                                       
060000                               SSA3                                       
060100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
060200     PERFORM IMS-STATUS-KONTROLL                                          
060300     .                                                                    
060400     SKIP3                                                                
060500 IMS-GHNP-ARTC25 SECTION.                                                 
060600     SKIP2                                                                
060700     MOVE  'WLARTC11*F(KDSEGKEY =1)'  TO SSA1                             
060800     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
060900            DELIMITED BY SIZE INTO SSA2                                   
061000     MOVE '  GE' TO GODK-STATUSKODER                                      
061100     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2              
061200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
061300     PERFORM IMS-STATUS-KONTROLL                                          
061400     .                                                                    
061500     SKIP3                                                                
061600 IMS-GET-ARTC25 SECTION.                                                  
061700     SKIP2                                                                
061800     MOVE  'WLARTC11*F(KDSEGKEY =1)'  TO SSA1                             
061900     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
062000            DELIMITED BY SIZE INTO SSA2                                   
062100     MOVE '  GE' TO GODK-STATUSKODER                                      
062200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2               
062300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
062400     PERFORM IMS-STATUS-KONTROLL                                          
062500     .                                                                    
062600     EJECT                                                                
062700 IMS-REPLACE-ARTC SECTION.                                                
062800     SKIP2                                                                
062900     MOVE '  '   TO GODK-STATUSKODER                                      
063000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA1                        
063100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
063200     PERFORM IMS-STATUS-KONTROLL                                          
063300     .                                                                    
063400     SKIP3                                                                
063500 IMS-DELETE SECTION.                                                      
063600     SKIP2                                                                
063700     MOVE '  '   TO GODK-STATUSKODER                                      
063800     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA1                        
063900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
064000     PERFORM IMS-STATUS-KONTROLL                                          
064100     .                                                                    
064200     EJECT                                                                
064300 IMS-STATUS-KONTROLL SECTION.                                             
064400     SET STATUS-IX TO 1                                                   
064500     SEARCH GODK-STATUS                                                   
064600       AT END CALL FELLOG                                                 
064700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
064800     END-SEARCH                                                           
064900     .                                                                    
065000     EJECT                                                                
