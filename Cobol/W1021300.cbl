000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1021300.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   APRIL 90.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*      - VISA STRUKTURRADER.                                              
001100*                                                                         
001200*      - VISAR INFORMATION OM STRUKTUREN SAMT ALLA ARTIKELRADER           
001300*        SOM INGÅR I STRUKTUREN OCH INTE ÄR HISTORIKRADER                 
001400*        - ALLTSÅ NU OCH FRAMÅT                                           
001500*                                                                         
001600*      - RAD SOM HAR STRUKTURTYP KAN MARKERAS FÖR ATT VISA RADENS         
001700*        STRUKTUR . RETURFUNKTION MED PF-TANGENT TILL URSPRUNGLIG         
001800*        STRUKTUR.                                                        
001900*                                                                         
002000*      - RAD KAN MARKERAS FÖR ATT VISA RADEN BILD 1212. RETUR MED         
002100*        MED PF-TANGENT .                                                 
002200*                                                                         
002300*      - LÄSER WDD1 BENREG (WDD3) OCH RASA                                
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W1T213                                              
002700*        MID:         W1I21301                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W1O21301                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                   PIC X(8)    VALUE 'W1021300'.                
003800 77  JA                      PIC X       VALUE 'J'.                       
003900 77  NEJ                     PIC X       VALUE 'N'.                       
004000 77  S-IX                    PIC S9(9)   VALUE +1   COMP SYNC.            
004100 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
004200 77  SPAR-INDX               PIC S9(9)   VALUE +0   COMP SYNC.            
004300 77  SPAR-IDRADNR            PIC S9(9)   VALUE ZERO COMP-3.               
004400 77  MAX-INDX                PIC S9(9)   VALUE +12.                       
004500 77  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
004600 77  IDARTNR-WS1             PIC X(9)    VALUE SPACE.                     
004700 77  IDRADNR-WS              PIC X(4)    VALUE SPACE.                     
004800 77  IDSKYLT-WS              PIC X(3)    VALUE SPACE.                     
004900 77  SATS-STR-RA             PIC X(3)    VALUE '222'.                     
005000 77  SATS-STR-RB             PIC X(3)    VALUE '223'.                     
005100 77  SATS-STR-BERPV          PIC X(3)    VALUE '226'.                     
005200 77  SATS-STR-CARP           PIC X(3)    VALUE '221'.                     
005300 77  MAX-KVRADER             PIC S9(4)   COMP VALUE +12.                  
005400 01  ALL-SPACE.                                                           
005500     03  FILLER              PIC X(50)   VALUE SPACE.                     
005600 01  ALL-PLUS.                                                            
005700     03  FILLER              PIC X(50)   VALUE ALL '+'.                   
005800 77  KDPRTVAL-WS             PIC X       VALUE SPACE.                     
005900   88  GODK-PRINTER VALUE 'A' 'B' 'C' 'D'.                                
006000 77  C-PART-LINE             PIC X(1)    VALUE 'P'.                       
006100 77  C-SUPPL-PART-LINE       PIC X(1)    VALUE 'S'.                       
006200 77  C-NOTE-LINE             PIC X(1)    VALUE 'N'.                       
006300                                                                          
006400                                                                          
006500 77  INDATA-SW               PIC X       VALUE 'J'.                       
006600   88  INDATA-OK                         VALUE 'J'.                       
006700   88  INDATA-FEL                        VALUE 'N'.                       
006800                                                                          
006900 77  INPUT-SW                PIC X       VALUE 'J'.                       
007000   88  INPUT-FINNS                       VALUE 'J'.                       
007100   88  INPUT-FINNS-EJ                    VALUE 'N'.                       
007200                                                                          
007300 77  ALLT-SW                 PIC X       VALUE 'J'.                       
007400   88  ALLT-OK                           VALUE 'J'.                       
007500                                                                          
007600 77  PF-TANGENT-SW           PIC X       VALUE 'J'.                       
007700   88  PF-TANGENT-TRYCKT                 VALUE 'J'.                       
007800   88  PF-TANGENT-EJ-TRYCKT              VALUE 'N'.                       
007900                                                                          
008000 77  PRINT-SW                PIC X       VALUE 'J'.                       
008100   88  PRINT-OK                          VALUE 'J'.                       
008200   88  PRINT-EJ-OK                       VALUE 'N'.                       
008300                                                                          
008400 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
008500   88  EGEN-MID                          VALUE '1213'.                    
008600   88  GODK-MID                          VALUE '1211' '1212'              
008700                                               '1213' '1214'              
008800                                               '1215' '1231'.             
008900   88  GODK-MID-MED-IDSKYLT              VALUE '1211' '1212'              
009000                                               '1213' '1214'.             
009100   88  GODK-MID-MED-IDRADNR              VALUE '1212' '1213'.             
009200     EJECT                                                                
009300                                                                          
009400                                                                          
009500 01  MEDDELANDE.                                                          
009600   03  FELMED-4.                                                          
009700       05  FILLER            PIC X(61)   VALUE                            
009800       'FEL I W006PRT  DEFINITION, KONTAKTA SYSTEMAVD'.                   
009900       05  FILLER            PIC X(61)   VALUE                            
010000       'MAJOR ERROR IN W006PRT , CONTACT YOUR SYSTEM SUPPORT'.            
010100   03  FILLER REDEFINES FELMED-4.                                         
010200       05  FELMED4 OCCURS 2  PIC X(61).                                   
010300*                                                                         
010400   03  MED-4-AREA.                                                        
010500       05  FILLER            PIC X(27)   VALUE                            
010600             'LISTA KÖAD FÖR UTSKRIFT PÅ '.                               
010700       05  FILLER            PIC X(27)   VALUE                            
010800             'LIST IS QUEUED TO PRINTER  '.                               
010900   03  FILLER REDEFINES MED-4-AREA.                                       
011000       05  MED4TXT OCCURS 2  PIC X(27).                                   
011100   03  MED-4.                                                             
011200       05  MED4-TXT          PIC X(27).                                   
011300       05  MED4-IDLTERM      PIC X(8)    VALUE  SPACE.                    
011400       05  FILLER            PIC X       VALUE  ','.                      
011500       05  MED4-BEPRT        PIC X(25)   VALUE  SPACE.                    
011600*                                                                         
011700   03  FEL-4.                                                             
011800      05 FILLER              PIC X(26) VALUE                              
011900           'KORRIGERA UPPLYSTA FÄLT '.                                    
012000      05 FILLER              PIC X(26) VALUE                              
012100           'CORRECT HIGHLIGHTED FIELDS'.                                  
012200   03  FILLER REDEFINES FEL-4.                                            
012300      05 FEL4 OCCURS 2       PIC X(26).                                   
012400*                                                                         
012500   03  FEL-5.                                                             
012600      05 FILLER              PIC X(28) VALUE                              
012700           'STRUKTUR SAKNAS             '.                                
012800      05 FILLER              PIC X(28) VALUE                              
012900           'STRUCTURE IS MISSING        '.                                
013000   03  FILLER REDEFINES FEL-5.                                            
013100      05 FEL5 OCCURS 2       PIC X(28).                                   
013200*                                                                         
013300   03  FEL-7.                                                             
013400      05 FILLER              PIC X(26) VALUE                              
013500           'TRYCK PF15 FÖR NEDBRYTNING'.                                  
013600      05 FILLER              PIC X(26) VALUE                              
013700           'PRESS PF15 FOR SPLIT-UP   '.                                  
013800   03  FILLER REDEFINES FEL-7.                                            
013900      05 FEL7 OCCURS 2       PIC X(26).                                   
014000     EJECT                                                                
014100 01  GENERELLA-SUBPROGRAM.                                                
014200   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
014300   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
014400   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
014500   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
014600   03  W1021310              PIC X(8)    VALUE 'W1021310'.                
014700   03  WL01MCNV              PIC X(8)    VALUE 'WL01MCNV'.                
014800     EJECT                                                                
014900*01  -COPY W006PRT                                                        
015000     EJECT                                                                
015100*                    ****  PARAMETRAR TILL W005INIT                       
015200*01  -COPY WMSGINIT                                                       
015300     EJECT                                                                
015400*    ---  COPYTEXT FÖR TRANS TILL WL01MCNV                                
015500*01  -COPY WL01MCNV                                                       
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)  VALUE 'W1021310'.             
015800 01  REQU-AREA.                                                           
015900*    03 -COPY WZ01REQU                                                    
016000*    03 -COPY W10213I1                                                    
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
016300 01  RESP-AREA.                                                           
016400*    03 -COPY WZ01RESP                                                    
016500*    03 -COPY W10213O1                                                    
016600     EJECT                                                                
016700******************************************************************        
016800*                                                                         
016900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
017000*                                                                         
017100 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
017200     SKIP3                                                                
017300*01  MID -COPY W1I21301                                                   
017400     EJECT                                                                
017500*01  -COPY WMSGAREA                                                       
017600     EJECT                                                                
017700*  03  MOD -COPY W1O21301 -RED MSG-AREA.                                  
017800     EJECT                                                                
017900****************************************************************          
018000*    PROG-TO-PROG-SW   AREA                                    *          
018100****************************************************************          
018200 01  FILLER                  PIC X(16) VALUE 'PROG-TO-PROG-SW'.           
018300 01  PROG-TO-PROG-SW.                                                     
018400     03  P-WS-LL     PIC S9(4)  VALUE +127 COMP SYNC.                     
018500     03  P-WS-Z1-Z2  PIC X(2)   VALUE LOW-VALUE.                          
018600     03  P-TRANSKOD  PIC X(8)   VALUE 'W1T293X '.                         
018700     03  P-IDTRANS   PIC X(4)   VALUE '1213'.                             
018800     03  P-MFSFOR    PIC X      VALUE '1'.                                
018900*    03  MID   -COPY W1I21301 -PRE PROGSW-.                               
019000     EJECT                                                                
019100*01  -COPY WMFSAREA                                                       
019200     EJECT                                                                
019300******************************************************************        
019400*                                                                         
019500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019600*                                                                         
019700 01  IMS-WS.                                                              
019800   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
019900     SKIP3                                                                
020000 01  NYCKLAR-TILL-DLI.                                                    
020100   03  W-IDARTNR-X.                                                       
020200     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
020300   03  W-KDSTRRAD-X.                                                      
020400     05  W-KDSTRRAD          PIC X       VALUE SPACE.                     
020500   03  W-IDRADNR-X.                                                       
020600     05  W-IDRADNR           PIC  S9(5)  VALUE ZERO  COMP-3.              
020700     SKIP3                                                                
020800*                        **** STATUS-KOD FRÅN IMS                         
020900   03  STATUS-WS             PIC XX.                                      
021000     88  SEGMENT-FINNS                   VALUE '  '.                      
021100     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
021200     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
021300     SKIP3                                                                
021400   03  GODK-STATUSKODER.                                                  
021500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021600     SKIP3                                                                
021700 01    SSA1                  PIC X(64).                                   
021800 01    SSA2                  PIC X(64).                                   
021900     EJECT                                                                
022000*                            IMS FUNKTIONSKODER                           
022100*01    -COPY W0003                                                        
022200     EJECT                                                                
022300*                            DLI INPUT-OUTPUT AREA                        
022400 01  DLI-IO-AREA.                                                         
022500   03  IO-AREA               PIC X(250)  VALUE SPACE.                     
022600     SKIP3                                                                
022700*  03  WLSATB01  -COPY WDJ101  -PRE SATB-  -RED IO-AREA.                  
022800     EJECT                                                                
022900*  03  WLSATB11  -COPY WDJ111  -PRE SATB-  -RED IO-AREA.                  
023000     EJECT                                                                
023100*  03  WLSATB22  -COPY WDJ122  -PRE SATB-  -RED IO-AREA.                  
023200     EJECT                                                                
023300 01  WDK7-PCB-PLACEHOLDER.                                                
023400     05  FILLER              PIC X.                                       
023500     EJECT                                                                
023600                                                                          
023700 LINKAGE SECTION.                                                         
023800*01  -COPY W0009     -PRE MSG-                                            
023900     EJECT                                                                
024000*01  -COPY W0009     -PRE ALT-                                            
024100     EJECT                                                                
024200*01  -COPY W0008     -PRE USEA-                                           
024300     05  FILLER              PIC X.                                       
024400     EJECT                                                                
024500*01  -COPY W0008     -PRE SATB-                                           
024600     05  FILLER              PIC X.                                       
024700     EJECT                                                                
024800*01  -COPY W0008     -PRE BENA-                                           
024900     05  FILLER              PIC X.                                       
025000     EJECT                                                                
025100*01  -COPY W0008     -PRE BENA-A-                                         
025200     05  FILLER              PIC X.                                       
025300     EJECT                                                                
025400*01  -COPY W0008     -PRE ARTC-                                           
025500     05  FILLER              PIC X.                                       
025600     EJECT                                                                
025700*01  -COPY W0008     -PRE WDK9-                                           
025800     05  FILLER              PIC X.                                       
025900     EJECT                                                                
026000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB SATB-PCB              
026100                           BENA-PCB BENA-A-PCB ARTC-PCB                   
026200                           WDK9-PCB.                                      
026300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB SATB-PCB              
026400                           BENA-PCB BENA-A-PCB ARTC-PCB                   
026500                           WDK9-PCB.                                      
026600     PERFORM IMS-GET-MSG                                                  
026700     IF SEGMENT-FINNS                                                     
026800       MOVE NEJ                  TO PF-TANGENT-SW                         
026900       PERFORM A-INIT                                                     
027000       PERFORM B-KOLLA-INPUT                                              
027100       PERFORM B-KOLLA-NYCKLAR                                            
027200       PERFORM C-INIT-REQU                                                
027300       IF MFS-PRINT                                                       
027400         IF PRINT-OK                                                      
027500           MOVE MID              TO PROGSW-MID                            
027600           MOVE MED-4            TO MOD-TEMFSINF                          
027700           PERFORM MFS-ROER-EJ-FAELT-UT                                   
027800           PERFORM IMS-INSERT-ALT-MSG                                     
027900         ELSE                                                             
028000           IF INDATA-FEL                                                  
028100             PERFORM MFS-ROER-EJ-FAELT-UT                                 
028200           END-IF                                                         
028300         END-IF                                                           
028400       ELSE                                                               
028500         IF MFS-FIRST                                                     
028600           SET REQU-FIRST        TO TRUE                                  
028700           PERFORM MFS-RENSA-FAELT-IN                                     
028800         ELSE                                                             
028900           IF MFS-NEXT                                                    
029000             SET REQU-NEXT       TO TRUE                                  
029100             PERFORM D-NAESTA-SIDA                                        
029200           ELSE                                                           
029300             SET REQU-QUERY      TO TRUE                                  
029400             PERFORM E-SAMMA-SIDA                                         
029500           END-IF                                                         
029600         END-IF                                                           
029700         IF ALLT-OK                                                       
029800           PERFORM F-CALL-BIZ-LOGIC-W1021310                              
029900         END-IF                                                           
030000       END-IF                                                             
030100       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O21301 + 4                      
030200       PERFORM IMS-INSERT-MSG                                             
030300     END-IF                                                               
030400                                                                          
030500     MOVE ZERO                   TO RETURN-CODE                           
030600     GOBACK                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 A-INIT SECTION.                                                          
031000                                                                          
031100     IF MSG-DUBBLA-TRANSKODER                                             
031200       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
031300                                 TO MID-W1I21301                          
031400       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
031500       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
031600       MOVE JA                   TO PF-TANGENT-SW                         
031700     ELSE                                                                 
031800       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
031900                                 TO MID-W1I21301                          
032000       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
032100       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
032200     END-IF                                                               
032300                                                                          
032400     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
032500     MOVE MSG-IDPFK              TO MFS-IDPFK                             
032600     MOVE MFS-IDTRANS            TO W-IDTRANS                             
032700                                                                          
032800     MOVE LOW-VALUE              TO MSG-AREA                              
032900     MOVE 'W1O213N1'             TO MFS-IDMOD                             
033000     MOVE '1213'                 TO MOD-IDTRANS                           
033100     MOVE MFS-RENSA-FAELT        TO MOD-TEMFSFEL MOD-TEMFSINF             
033200                                                                          
033300     IF NOT EGEN-MID                                                      
033400       MOVE SPACE                TO MFS-KDTRTYP                           
033500       MOVE '7'                  TO MFS-IDPFK                             
033600     END-IF                                                               
033700                                                                          
033800     IF ENGLISH-TEXT                                                      
033900        MOVE +2    TO S-IX                                                
034000        MOVE '2'   TO P-MFSFOR                                            
034100     END-IF                                                               
034200                                                                          
034300     PERFORM MFS-FORM-ATTR                                                
034400     MOVE '101'                  TO REQU-IDMSGVER                         
034500     MOVE MSGI-IDUSER            TO REQU-IDUSER                           
034600     .                                                                    
034700     EJECT                                                                
034800 B-KOLLA-INPUT SECTION.                                                   
034900                                                                          
035000     MOVE +1                     TO INDX                                  
035100     MOVE NEJ                    TO INPUT-SW                              
035200     MOVE MID-IDARTNR-UT         TO IDARTNR-WS1                           
035300     INSPECT IDARTNR-WS1 REPLACING ALL SPACE BY ZERO                      
035400                                                                          
035500     PERFORM UNTIL INDX > MAX-INDX                                        
035600       IF (MID-SELECT(INDX) = ALL '+') OR                                 
035700          (MID-SELECT(INDX) = SPACE )                                     
035800         MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                      
035900       ELSE                                                               
036000         IF  MID-SELECT(INDX) = 'S'                                       
036100         AND (EGEN-MID OR GODK-MID)                                       
036200           IF IDARTNR-WS1 NUMERIC                                         
036300              MOVE MFS-ALFA-FAELT-RAETT                                   
036400                                 TO MOD-SELECT-ATTR(INDX)                 
036500              MOVE JA            TO INPUT-SW                              
036600              MOVE IDARTNR-WS1   TO W-IDARTNR                             
036700              MOVE ZERO          TO W-KDSTRRAD                            
036800              MOVE MID-IDRADNR(INDX)                                      
036900                                 TO W-IDRADNR                             
037000              MOVE INDX          TO SPAR-INDX                             
037100              ADD +20            TO INDX                                  
037200           ELSE                                                           
037300             MOVE MFS-ALFA-FAELT-FEL                                      
037400                                 TO MOD-SELECT-ATTR(INDX)                 
037500             MOVE MFS-ROER-EJ-FAELT                                       
037600                                 TO MOD-SELECT(INDX)                      
037700             MOVE FEL4(S-IX)     TO MOD-TEMFSFEL                          
037800           END-IF                                                         
037900         ELSE                                                             
038000           IF MFS-FIRST OR MFS-NEXT                                       
038100             PERFORM MFS-RENSA-FAELT-INMAT                                
038200             MOVE MFS-RENSA-FAELT                                         
038300                                 TO MOD-TEMFSFEL                          
038400           ELSE                                                           
038500             MOVE MFS-ALFA-FAELT-FEL                                      
038600                                 TO MOD-SELECT-ATTR(INDX)                 
038700             MOVE MFS-ROER-EJ-FAELT                                       
038800                                 TO MOD-SELECT(INDX)                      
038900             MOVE FEL4(S-IX)     TO MOD-TEMFSFEL                          
039000           END-IF                                                         
039100         END-IF                                                           
039200       END-IF                                                             
039300       ADD +1                    TO INDX                                  
039400     END-PERFORM                                                          
039500                                                                          
039600     IF INPUT-FINNS                                                       
039700       PERFORM IMS-GET-SATB-RAD-UNIK                                      
039800       IF SEGMENT-FINNS                                                   
039900         PERFORM BA-KOLLA-STRUKTURTYP                                     
040000       ELSE                                                               
040100         MOVE '9'                TO W-KDSTRRAD                            
040200         PERFORM IMS-GET-SATB-RAD-UNIK                                    
040300         IF SEGMENT-FINNS                                                 
040400           PERFORM BA-KOLLA-STRUKTURTYP                                   
040500         END-IF                                                           
040600       END-IF                                                             
040700     ELSE                                                                 
040800       IF PF-TANGENT-TRYCKT                                               
040900         IF MFS-FIRST OR  MFS-NEXT  OR  MFS-PRINT                         
041000           CONTINUE                                                       
041100         ELSE                                                             
041200           IF EGEN-MID                                                    
041300             IF MID-IDSATSNR-DOLT = ZERO                                  
041400               CONTINUE                                                   
041500             ELSE                                                         
041600               MOVE MID-IDSATSNR-DOLT                                     
041700                                 TO MID-IDARTNR-UT                        
041800                                    MOD-IDSATSNR-DOLT                     
041900               MOVE ZERO         TO MID-IDRADNR-UT                        
042000               MOVE '7'          TO MFS-IDPFK                             
042100               MOVE SPACE        TO MFS-KDTRTYP                           
042200             END-IF                                                       
042300           END-IF                                                         
042400         END-IF                                                           
042500       END-IF                                                             
042600     END-IF                                                               
042700     MOVE MID-KDPRTVAL           TO MOD-KDPRTVAL                          
042800     IF MFS-PRINT                                                         
042900       MOVE JA                   TO PRINT-SW                              
043000       MOVE MID-KDPRTVAL         TO KDPRTVAL-WS                           
043100       IF GODK-PRINTER                                                    
043200         MOVE MFS-ALFA-FAELT-RAETT                                        
043300                                 TO MOD-KDPRTVAL-ATTR                     
043400         EVALUATE MID-KDPRTVAL                                            
043500           WHEN 'A'                                                       
043600             MOVE SATS-STR-RA    TO PRT-IDPRTLST                          
043700           WHEN 'B'                                                       
043800             MOVE SATS-STR-RB    TO PRT-IDPRTLST                          
043900           WHEN 'C'                                                       
044000             MOVE SATS-STR-BERPV TO PRT-IDPRTLST                          
044100           WHEN 'D'                                                       
044200             MOVE SATS-STR-CARP  TO PRT-IDPRTLST                          
044300         END-EVALUATE                                                     
044400*        **************************************************               
044500*        * HÄMTAR PRINTERNS LOGISKA NAMN TILL TEMFSINF *                  
044600*        **************************************************               
044700                                                                          
044800         MOVE 1                  TO PRT-KDCALL                            
044900         CALL W006PRT         USING PRT-W006PRT                           
045000         IF PRT-IDLTERM = 'SAKNAS  '                                      
045100           MOVE NEJ              TO INDATA-SW                             
045200           MOVE FELMED4(S-IX)    TO MED-4                                 
045300           MOVE MED-4            TO MOD-TEMFSINF                          
045400         ELSE                                                             
045500           MOVE MED4TXT(S-IX)    TO MED4-TXT                              
045600           MOVE PRT-IDLTERM      TO MED4-IDLTERM                          
045700           IF S-IX = +1                                                   
045800             MOVE PRT-BEPRTLST   TO MED4-BEPRT                            
045900           ELSE                                                           
046000             MOVE SPACE          TO MED4-BEPRT                            
046100           END-IF                                                         
046200         END-IF                                                           
046300       ELSE                                                               
046400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-ATTR                     
046500         MOVE NEJ                TO PRINT-SW                              
046600                                    INDATA-SW                             
046700         MOVE FEL4(S-IX)         TO MOD-TEMFSFEL                          
046800       END-IF                                                             
046900     END-IF                                                               
047000     .                                                                    
047100     EJECT                                                                
047200 BA-KOLLA-STRUKTURTYP SECTION.                                            
047300                                                                          
047400     IF PF-TANGENT-TRYCKT                                                 
047500       IF MFS-FIRST OR MFS-NEXT OR MFS-PRINT                              
047600         CONTINUE                                                         
047700       ELSE                                                               
047800         IF EGEN-MID                                                      
047900           IF SATB-RAD-IDSTRTYP = 'S' OR 'R' OR 'K'                       
048000             MOVE MID-IDARTNR-UT TO MOD-IDSATSNR-DOLT                     
048100             MOVE SATB-RAD-IDARTNR                                        
048200                                 TO MID-IDARTNR-UT                        
048300             MOVE ZERO           TO MID-IDRADNR-UT                        
048400             MOVE '7'            TO MFS-IDPFK                             
048500             MOVE SPACE          TO MFS-KDTRTYP                           
048600           ELSE                                                           
048700             MOVE FEL5(S-IX)     TO MOD-TEMFSFEL                          
048800             MOVE MFS-ALFA-FAELT-FEL                                      
048900                                 TO MOD-SELECT-ATTR(SPAR-INDX)            
049000             MOVE MFS-ROER-EJ-FAELT                                       
049100                                 TO MOD-SELECT(SPAR-INDX)                 
049200           END-IF                                                         
049300         END-IF                                                           
049400       END-IF                                                             
049500     END-IF                                                               
049600                                                                          
049700     .                                                                    
049800     EJECT                                                                
049900                                                                          
050000 B-KOLLA-NYCKLAR SECTION.                                                 
050100                                                                          
050200     MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-IN                        
050300                                    MOD-IDRADNR-IN                        
050400                                    MOD-IDSKYLT-IN                        
050500                                                                          
050600     MOVE ALL '+'                TO MSGI-WMSGINIT                         
050700     MOVE '001'                  TO MSGI-KDCALL                           
050800     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
050900                                    REQU-IDUSER                           
051000     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
051100     MOVE '1213'                 TO MSGI-IDTRANS                          
051200                                                                          
051300     IF MFS-IDTRANS = '1213'                                              
051400     OR (MID-IDARTNR-IN NUMERIC                                           
051500     AND MID-IDARTNR-IN > ZERO)                                           
051600       MOVE MID-IDARTNR-IN       TO MSGI-IDARTNR                          
051700     END-IF                                                               
051800                                                                          
051900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
052000                                                                          
052100     IF MID-IDARTNR-IN = ALL '+'                                          
052200       IF EGEN-MID                                                        
052300         MOVE MID-IDARTNR-UT     TO IDARTNR-WS                            
052400       ELSE                                                               
052500         MOVE MSGI-IDARTNR       TO MOD-IDSATSNR-DOLT                     
052600                                    IDARTNR-WS                            
052700       END-IF                                                             
052800     ELSE                                                                 
052900       MOVE '7'                  TO MFS-IDPFK                             
053000       MOVE SPACE                TO MFS-KDTRTYP                           
053100       MOVE MSGI-IDARTNR         TO MOD-IDSATSNR-DOLT                     
053200                                    IDARTNR-WS                            
053300       MOVE NEJ                  TO PRINT-SW                              
053400     END-IF                                                               
053500                                                                          
053600     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
053700                                                                          
053800     MOVE IDARTNR-WS             TO REQU-IDARTNR-KEY                      
053900                                    MOD-IDARTNR-UT                        
054000                                                                          
054100     IF GODK-MID-MED-IDSKYLT                                              
054200       IF MID-IDSKYLT-IN = ALL '+'                                        
054300         IF MID-IDSKYLT-UT = SPACE                                        
054400           IF ENGLISH-TEXT                                                
054500             MOVE 'GB'           TO IDSKYLT-WS                            
054600           ELSE                                                           
054700             MOVE 'S '           TO IDSKYLT-WS                            
054800           END-IF                                                         
054900         ELSE                                                             
055000           MOVE MID-IDSKYLT-UT   TO IDSKYLT-WS                            
055100         END-IF                                                           
055200       ELSE                                                               
055300         MOVE MID-IDSKYLT-IN     TO IDSKYLT-WS                            
055400         MOVE '7'                TO MFS-IDPFK                             
055500         MOVE SPACE              TO MFS-KDTRTYP                           
055600         MOVE NEJ                TO PRINT-SW                              
055700       END-IF                                                             
055800     ELSE                                                                 
055900       IF ENGLISH-TEXT                                                    
056000         MOVE 'GB'               TO IDSKYLT-WS                            
056100       ELSE                                                               
056200         MOVE 'S '               TO IDSKYLT-WS                            
056300       END-IF                                                             
056400     END-IF                                                               
056500                                                                          
056600     MOVE IDSKYLT-WS             TO REQU-IDSKYLT-KEY                      
056700                                    MOD-IDSKYLT-UT                        
056800                                                                          
056900     IF IDSKYLT-WS = 'S '                                                 
057000       MOVE 'SV'                 TO MCNV-IDSPRAK                          
057100                                    REQU-IDSPRAK                          
057200     ELSE                                                                 
057300       MOVE 'EN'                 TO MCNV-IDSPRAK                          
057400                                    REQU-IDSPRAK                          
057500     END-IF                                                               
057600                                                                          
057700     IF GODK-MID-MED-IDRADNR                                              
057800       IF MID-IDRADNR-IN = ALL '+'                                        
057900         MOVE MID-IDRADNR-UT     TO IDRADNR-WS                            
058000       ELSE                                                               
058100         MOVE MID-IDRADNR-IN     TO IDRADNR-WS                            
058200         MOVE '7'                TO MFS-IDPFK                             
058300         MOVE SPACE              TO MFS-KDTRTYP                           
058400         MOVE NEJ                TO PRINT-SW                              
058500       END-IF                                                             
058600     ELSE                                                                 
058700       MOVE ZERO                 TO IDRADNR-WS                            
058800     END-IF                                                               
058900                                                                          
059000     INSPECT IDRADNR-WS REPLACING LEADING SPACE BY ZERO                   
059100     MOVE IDRADNR-WS             TO REQU-IDRADNR-KEY                      
059200                                    MOD-IDRADNR-UT                        
059300                                                                          
059400     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
059500     INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE               
059600                                                                          
059700     IF GODK-MID                                                          
059800       CONTINUE                                                           
059900     ELSE                                                                 
060000       MOVE NEJ                  TO PRINT-SW                              
060100     END-IF                                                               
060200                                                                          
060300     .                                                                    
060400     EJECT                                                                
060500                                                                          
060600 C-INIT-REQU SECTION.                                                     
060700                                                                          
060800     MOVE MAX-KVRADER            TO REQU-KVRADER                          
060900                                                                          
061000     .                                                                    
061100     EJECT                                                                
061200 D-NAESTA-SIDA SECTION.                                                   
061300                                                                          
061400     PERFORM MFS-ROER-EJ-FAELT-UT                                         
061500     MOVE MID-IDRADNR-DOLT2      TO REQU-IDRADNR-START                    
061600     .                                                                    
061700     EJECT                                                                
061800                                                                          
061900 E-SAMMA-SIDA SECTION.                                                    
062000                                                                          
062100     MOVE MID-IDRADNR-DOLT       TO REQU-IDRADNR-START                    
062200     IF INPUT-FINNS                                                       
062300       IF PF-TANGENT-TRYCKT                                               
062400         IF EGEN-MID                                                      
062500           MOVE NEJ              TO ALLT-SW                               
062600           MOVE FEL5(S-IX)       TO MOD-TEMFSFEL                          
062700           PERFORM MFS-ROER-EJ-FAELT-IN                                   
062800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
062900         END-IF                                                           
063000       ELSE                                                               
063100         IF MFS-ENTER                                                     
063200           MOVE NEJ              TO ALLT-SW                               
063300           MOVE FEL7(S-IX)       TO MOD-TEMFSFEL                          
063400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
063500           PERFORM MFS-ROER-EJ-FAELT-UT                                   
063600         END-IF                                                           
063700       END-IF                                                             
063800     ELSE                                                                 
063900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
064000       MOVE JA                   TO ALLT-SW                               
064100     END-IF                                                               
064200     .                                                                    
064300     EJECT                                                                
064400                                                                          
064500 F-CALL-BIZ-LOGIC-W1021310 SECTION.                                       
064600                                                                          
064700     CALL W1021310 USING REQU-AREA RESP-AREA MAX-KVRADER                  
064800                         SATB-PCB  BENA-PCB  BENA-A-PCB                   
064900                         ARTC-PCB WDK7-PCB-PLACEHOLDER WDK9-PCB           
065000     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
065100        RESP-IDMSG-INFO  NOT = SPACE                                      
065200       PERFORM FA-SET-MSG-AND-HILIGHT                                     
065300     END-IF                                                               
065400     PERFORM FB-MOVE-RESP-TO-MOD                                          
065500     .                                                                    
065600     EJECT                                                                
065700 FA-SET-MSG-AND-HILIGHT SECTION.                                          
065800                                                                          
065900     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
066000     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
066100     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
066200                                                                          
066300     CALL WL01MCNV USING MCNV-AREA                                        
066400     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
066500     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
066600     .                                                                    
066700     EJECT                                                                
066800 FB-MOVE-RESP-TO-MOD SECTION.                                             
066900                                                                          
067000     IF RESP-IDRADNR-START = SPACE                                        
067100       MOVE MFS-RENSA-FAELT      TO MOD-IDRADNR-DOLT                      
067200     ELSE                                                                 
067300       IF RESP-IDRADNR-START = ALL '+'                                    
067400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDRADNR-DOLT                      
067500       ELSE                                                               
067600         MOVE RESP-IDRADNR-START TO MOD-IDRADNR-DOLT                      
067700       END-IF                                                             
067800     END-IF                                                               
067900                                                                          
068000     IF RESP-IDRADNR-NEXT = SPACE                                         
068100       MOVE MFS-RENSA-FAELT      TO MOD-IDRADNR-DOLT2                     
068200     ELSE                                                                 
068300       IF RESP-IDRADNR-NEXT = ALL '+'                                     
068400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDRADNR-DOLT2                     
068500       ELSE                                                               
068600         MOVE RESP-IDRADNR-NEXT  TO MOD-IDRADNR-DOLT2                     
068700       END-IF                                                             
068800     END-IF                                                               
068900                                                                          
069000     IF RESP-BEART = SPACE                                                
069100       MOVE MFS-RENSA-FAELT      TO MOD-BEART                             
069200     ELSE                                                                 
069300       IF RESP-BEART = ALL '+'                                            
069400         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                             
069500       ELSE                                                               
069600         MOVE RESP-BEART         TO MOD-BEART                             
069700       END-IF                                                             
069800     END-IF                                                               
069900                                                                          
070000     IF RESP-KDPRODSL = SPACE                                             
070100       MOVE MFS-RENSA-FAELT      TO MOD-KDPRODSL                          
070200     ELSE                                                                 
070300       IF RESP-KDPRODSL = ALL '+'                                         
070400         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDPRODSL                          
070500       ELSE                                                               
070600         MOVE RESP-KDPRODSL      TO MOD-KDPRODSL                          
070700       END-IF                                                             
070800     END-IF                                                               
070900                                                                          
071000     IF RESP-IDFKNGRP = SPACE                                             
071100       MOVE MFS-RENSA-FAELT      TO MOD-IDFKNGRP                          
071200     ELSE                                                                 
071300       IF RESP-IDFKNGRP = ALL '+'                                         
071400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDFKNGRP                          
071500       ELSE                                                               
071600         MOVE RESP-IDFKNGRP      TO MOD-IDFKNGRP                          
071700       END-IF                                                             
071800     END-IF                                                               
071900                                                                          
072000     IF RESP-IDSTRTYP = SPACE                                             
072100       MOVE MFS-RENSA-FAELT      TO MOD-IDSTRTYP                          
072200     ELSE                                                                 
072300       IF RESP-IDSTRTYP = ALL '+'                                         
072400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDSTRTYP                          
072500       ELSE                                                               
072600         MOVE RESP-IDSTRTYP      TO MOD-IDSTRTYP                          
072700       END-IF                                                             
072800     END-IF                                                               
072900                                                                          
073000     IF RESP-KDPSLLOC = SPACE                                             
073100       MOVE MFS-RENSA-FAELT      TO MOD-KDPSLLOC                          
073200     ELSE                                                                 
073300       IF RESP-KDPSLLOC = ALL '+'                                         
073400         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDPSLLOC                          
073500       ELSE                                                               
073600         MOVE RESP-KDPSLLOC      TO MOD-KDPSLLOC                          
073700       END-IF                                                             
073800     END-IF                                                               
073900                                                                          
074000     PERFORM                                                              
074100     VARYING INDX FROM +1 BY +1                                           
074200       UNTIL INDX > RESP-KVRADER                                          
074300       MOVE RESP-SELECT-LINE-ATTR (INDX)                                  
074400                                 TO MOD-SELECT-ATTR (INDX)                
074500       IF RESP-SELECT-LINE (INDX) = SPACE                                 
074600         MOVE MFS-RENSA-FAELT    TO MOD-SELECT (INDX)                     
074700       ELSE                                                               
074800         IF RESP-SELECT-LINE (INDX) = ALL '+'                             
074900           MOVE MFS-ROER-EJ-FAELT                                         
075000                                 TO MOD-SELECT (INDX)                     
075100         ELSE                                                             
075200           MOVE RESP-SELECT-LINE (INDX)                                   
075300                                 TO MOD-SELECT (INDX)                     
075400         END-IF                                                           
075500       END-IF                                                             
075600                                                                          
075700       IF RESP-IDRADNR-LINE (INDX) = SPACE                                
075800         MOVE MFS-RENSA-FAELT    TO MOD-IDRADNR (INDX)                    
075900       ELSE                                                               
076000         IF RESP-IDRADNR-LINE (INDX) = ALL '+'                            
076100           MOVE MFS-ROER-EJ-FAELT                                         
076200                                 TO MOD-IDRADNR (INDX)                    
076300         ELSE                                                             
076400           MOVE RESP-IDRADNR-LINE (INDX)                                  
076500                                 TO MOD-IDRADNR (INDX)                    
076600         END-IF                                                           
076700       END-IF                                                             
076800                                                                          
076900       IF   RESP-REANTPSA-LINE    (INDX) = SPACE                          
077000        AND RESP-IDARTNR-LINE     (INDX) = SPACE                          
077100        AND RESP-IDANSK-LINE      (INDX) = SPACE                          
077200        AND RESP-PRARTSTD-LINE    (INDX) = SPACE                          
077300        AND RESP-KVVECKOR-LT-LINE (INDX) = SPACE                          
077400        AND RESP-ARB-SALDO-LINE   (INDX) = SPACE                          
077500        AND RESP-BEART-LINE       (INDX) = SPACE                          
077600        AND RESP-IDSTRTYP-LINE    (INDX) = SPACE                          
077700        AND RESP-KDISATS-LINE     (INDX) = SPACE                          
077800        AND RESP-TIAAVV-LINE      (INDX) = SPACE                          
077900        AND RESP-KDFARLIG-LINE    (INDX) = SPACE                          
078000        AND RESP-TESTRNOT-LINE    (INDX) = SPACE                          
078100         MOVE MFS-RENSA-FAELT    TO MOD-UTRAD (INDX)                      
078200       ELSE                                                               
078300         IF   RESP-REANTPSA-LINE    (INDX) = ALL '+'                      
078400          AND RESP-IDARTNR-LINE     (INDX) = ALL '+'                      
078500          AND RESP-IDANSK-LINE      (INDX) = ALL '+'                      
078600          AND RESP-PRARTSTD-LINE    (INDX) = ALL '+'                      
078700          AND RESP-KVVECKOR-LT-LINE (INDX) = ALL '+'                      
078800          AND RESP-ARB-SALDO-LINE   (INDX) = ALL '+'                      
078900          AND RESP-BEART-LINE       (INDX) = ALL '+'                      
079000          AND RESP-IDSTRTYP-LINE    (INDX) = ALL '+'                      
079100          AND RESP-KDISATS-LINE     (INDX) = ALL '+'                      
079200          AND RESP-TIAAVV-LINE      (INDX) = ALL '+'                      
079300          AND RESP-KDFARLIG-LINE    (INDX) = ALL '+'                      
079400          AND RESP-TESTRNOT-LINE    (INDX) = ALL '+'                      
079500           MOVE MFS-ROER-EJ-FAELT                                         
079600                                 TO MOD-UTRAD (INDX)                      
079700         ELSE                                                             
079800           MOVE SPACE            TO MOD-UTRAD (INDX)                      
079900           EVALUATE RESP-LINE-TYPE-LINE (INDX)                            
080000           WHEN C-PART-LINE                                               
080100             MOVE RESP-REANTPSA-LINE    (INDX)                            
080200                                    TO MOD-REANTPSA-LINE    (INDX)        
080300             MOVE RESP-IDARTNR-LINE     (INDX)                            
080400                                    TO MOD-IDARTNR-LINE     (INDX)        
080500             MOVE RESP-IDANSK-LINE      (INDX)                            
080600                                    TO MOD-IDANSK-LINE      (INDX)        
080700             MOVE RESP-PRARTSTD-LINE    (INDX)                            
080800                                    TO MOD-PRARTSTD-LINE    (INDX)        
080900             MOVE RESP-KVVECKOR-LT-LINE (INDX)                            
081000                                    TO MOD-KVVECKOR-LT-LINE (INDX)        
081100             MOVE RESP-ARB-SALDO-LINE   (INDX)                            
081200                                    TO MOD-ARB-SALDO-LINE   (INDX)        
081300             MOVE RESP-BEART-LINE       (INDX)                            
081400                                    TO MOD-BEART-LINE       (INDX)        
081500             MOVE RESP-IDSTRTYP-LINE    (INDX)                            
081600                                    TO MOD-IDSTRTYP-LINE    (INDX)        
081700             MOVE RESP-KDISATS-LINE     (INDX)                            
081800                                    TO MOD-KDISATS-LINE     (INDX)        
081900             IF RESP-TIAAVV-LINE (INDX) = ZERO                            
082000               INSPECT MOD-TIAAVV-LINE    (INDX)                          
082100                 REPLACING CHARACTERS BY SPACE                            
082200             ELSE                                                         
082300               MOVE RESP-TIAAVV-LINE    (INDX)                            
082400                                      TO MOD-TIAAVV-LINE    (INDX)        
082500             END-IF                                                       
082600             MOVE RESP-KDFARLIG-LINE    (INDX)                            
082700                                    TO MOD-KDFARLIG-LINE    (INDX)        
082800           WHEN C-SUPPL-PART-LINE                                         
082900             MOVE RESP-REANTPSA-LINE    (INDX)                            
083000                                    TO MOD-REANTPSA-LINE-S  (INDX)        
083100             MOVE RESP-BELEVART-LINE    (INDX)                            
083200                                    TO MOD-BELEVART-LINE-S  (INDX)        
083300             MOVE 'SUP'                                                   
083400                                    TO MOD-SUPPL-LINE-S     (INDX)        
083500             MOVE RESP-IDLEVNR-LINE     (INDX)                            
083600                                    TO MOD-IDLEVNR-LINE-S   (INDX)        
083700             MOVE RESP-BEART-LINE       (INDX)                            
083800                                    TO MOD-BEART-LINE-S     (INDX)        
083900             MOVE RESP-IDSTRTYP-LINE    (INDX)                            
084000                                    TO MOD-IDSTRTYP-LINE-S  (INDX)        
084100             MOVE RESP-KDISATS-LINE     (INDX)                            
084200                                    TO MOD-KDISATS-LINE-S   (INDX)        
084300             IF RESP-TIAAVV-LINE (INDX) = ZERO                            
084400               INSPECT MOD-TIAAVV-LINE    (INDX)                          
084500                 REPLACING CHARACTERS BY SPACE                            
084600             ELSE                                                         
084700               MOVE RESP-TIAAVV-LINE    (INDX)                            
084800                                      TO MOD-TIAAVV-LINE    (INDX)        
084900             END-IF                                                       
085000           WHEN C-NOTE-LINE                                               
085100             MOVE RESP-TESTRNOT-LINE    (INDX)                            
085200                                    TO MOD-TESTRNOT-LINE    (INDX)        
085300             MOVE RESP-CONTINUE-LINE    (INDX)                            
085400                                    TO MOD-CONTINUE-LINE    (INDX)        
085500           END-EVALUATE                                                   
085600         END-IF                                                           
085700       END-IF                                                             
085800     END-PERFORM                                                          
085900                                                                          
086000     PERFORM                                                              
086100     VARYING INDX FROM INDX BY +1                                         
086200       UNTIL INDX > MAX-INDX                                              
086300       MOVE MFS-RENSA-FAELT      TO MOD-SELECT   (INDX)                   
086400                                    MOD-IDRADNR  (INDX)                   
086500                                    MOD-UTRAD    (INDX)                   
086600     END-PERFORM                                                          
086700                                                                          
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100 MFS-RENSA-FAELT-IN SECTION.                                              
087200                                                                          
087300     MOVE MFS-RENSA-FAELT        TO MOD-KDPRTVAL                          
087400     MOVE +1                     TO INDX                                  
087500     PERFORM UNTIL INDX > MAX-INDX                                        
087600       MOVE MFS-RENSA-FAELT      TO MOD-SELECT(INDX)                      
087700                                    MOD-IDRADNR(INDX)                     
087800       ADD +1                    TO INDX                                  
087900     END-PERFORM                                                          
088000     .                                                                    
088100                                                                          
088200 MFS-RENSA-FAELT-INMAT SECTION.                                           
088300                                                                          
088400     MOVE MFS-RENSA-FAELT        TO MOD-KDPRTVAL                          
088500     MOVE +1                     TO INDX                                  
088600     PERFORM UNTIL INDX > MAX-INDX                                        
088700       MOVE MFS-RENSA-FAELT      TO MOD-SELECT(INDX)                      
088800       ADD +1                    TO INDX                                  
088900     END-PERFORM                                                          
089000     .                                                                    
089100                                                                          
089200 MFS-FORM-ATTR SECTION.                                                   
089300                                                                          
089400     MOVE +1                     TO INDX                                  
089500     PERFORM UNTIL INDX > MAX-INDX                                        
089600       MOVE MFS-FORMATETS-ATTR   TO MOD-SELECT-ATTR(INDX)                 
089700       ADD +1                    TO INDX                                  
089800     END-PERFORM                                                          
089900     .                                                                    
090000                                                                          
090100 MFS-RENSA-FAELT-UT SECTION.                                              
090200                                                                          
090300     MOVE MFS-RENSA-FAELT        TO MOD-BEART                             
090400                                    MOD-KDPRODSL                          
090500                                    MOD-IDFKNGRP                          
090600                                    MOD-IDSTRTYP                          
090700                                    MOD-IDRADNR-DOLT                      
090800                                    MOD-IDRADNR-DOLT2                     
090900                                    MOD-IDSATSNR-DOLT                     
091000                                    MOD-KDPRTVAL                          
091100                                    MOD-KDPSLLOC                          
091200     MOVE +1                     TO INDX                                  
091300     PERFORM UNTIL INDX > MAX-INDX                                        
091400       MOVE MFS-RENSA-FAELT      TO MOD-SELECT(INDX)                      
091500                                    MOD-IDRADNR(INDX)                     
091600                                    MOD-UTRAD(INDX)                       
091700       ADD +1                    TO INDX                                  
091800     END-PERFORM                                                          
091900     .                                                                    
092000     SKIP2                                                                
092100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
092200     MOVE MFS-ROER-EJ-FAELT      TO MOD-IDRADNR-DOLT                      
092300                                    MOD-IDRADNR-DOLT2                     
092400                                    MOD-IDSATSNR-DOLT                     
092500                                    MOD-BEART                             
092600                                    MOD-KDPRODSL                          
092700                                    MOD-IDFKNGRP                          
092800                                    MOD-IDSTRTYP                          
092900                                    MOD-KDPRTVAL                          
093000                                    MOD-KDPSLLOC                          
093100     MOVE +1                     TO INDX                                  
093200     PERFORM UNTIL INDX > MAX-INDX                                        
093300       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDRADNR(INDX)                     
093400                                    MOD-SELECT(INDX)                      
093500                                    MOD-UTRAD(INDX)                       
093600       ADD +1                    TO INDX                                  
093700     END-PERFORM                                                          
093800     .                                                                    
093900     SKIP2                                                                
094000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
094100                                                                          
094200     MOVE MFS-ROER-EJ-FAELT      TO MOD-KDPRTVAL                          
094300     MOVE +1                     TO INDX                                  
094400     PERFORM UNTIL INDX > MAX-INDX                                        
094500       MOVE MFS-ROER-EJ-FAELT    TO MOD-SELECT(INDX)                      
094600                                    MOD-IDRADNR(INDX)                     
094700       ADD +1                    TO INDX                                  
094800     END-PERFORM                                                          
094900     .                                                                    
095000     EJECT                                                                
095100******************************************************                    
095200*                  IMS SEKTIONER                     *                    
095300******************************************************                    
095400 IMS-GET-MSG SECTION.                                                     
095500     MOVE '  QC' TO GODK-STATUSKODER                                      
095600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
095700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095800     PERFORM IMS-STATUSKONTROLL                                           
095900     .                                                                    
096000                                                                          
096100 IMS-INSERT-MSG SECTION.                                                  
096200     SKIP2                                                                
096300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
096400     MOVE SPACE TO GODK-STATUSKODER                                       
096500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
096600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096700     PERFORM IMS-STATUSKONTROLL                                           
096800     .                                                                    
096900                                                                          
097000 IMS-INSERT-ALT-MSG SECTION.                                              
097100     SKIP2                                                                
097200     MOVE SPACE TO GODK-STATUSKODER                                       
097300     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
097400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
097500     PERFORM IMS-STATUSKONTROLL                                           
097600     .                                                                    
097700     EJECT                                                                
097800 IMS-GET-SATB-RAD-UNIK SECTION.                                           
097900     SKIP2                                                                
098000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
098100          DELIMITED BY SIZE INTO SSA1                                     
098200     STRING 'WLSATB11(WDJ111KY =' W-KDSTRRAD-X                            
098300                                  W-IDRADNR-X ')'                         
098400          DELIMITED BY SIZE INTO SSA2                                     
098500     MOVE '  GE' TO GODK-STATUSKODER                                      
098600     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1 SSA2                 
098700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
098800     PERFORM IMS-STATUSKONTROLL                                           
098900     .                                                                    
099000                                                                          
099100 IMS-STATUSKONTROLL SECTION.                                              
099200     SKIP2                                                                
099300     SET STATUS-IX TO 1                                                   
099400     SEARCH GODK-STATUS                                                   
099500       AT END CALL FELLOG                                                 
099600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
099700     END-SEARCH                                                           
099800     .                                                                    
099900     EJECT                                                                
100000     EJECT                                                                
