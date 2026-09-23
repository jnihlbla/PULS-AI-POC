000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9022200.                                                
000400 AUTHOR.         BODIL LINDAHL                                            
000500 DATE-WRITTEN.   MARS 2000.                                               
000600*                                                                         
000700*REMARKS.                                                                 
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET LÄSER I VILKA SATSER EN ARTIKEL INGÅR.                
001100*        VIA VDI-SYSTEMET.                                                
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W90222T                                             
001500*        MID:         W9I22201                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W9O22201                                            
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -COPY WY2000W1                                                       
002700     SKIP3                                                                
002800 77  IDPGM                       PIC X(8)    VALUE 'W9022200'.            
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
003200 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003210 77  WS-AKTUELL-RAD              PIC X       VALUE SPACE.                 
003220 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
003230 77  MAX-RAD                     PIC S9(3)   VALUE +13  COMP-3.           
003240 77  RAD-IX                      PIC S9(3)   VALUE +0   COMP-3.           
003250 77  MAX-MOD-LANGD               PIC S9(3)   VALUE +606 COMP-3.           
003260 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003270     88  NYCKLAR-OK                          VALUE 'J'.                   
003280                                                                          
003290 01  DYNAMISKA-SUBPGM.                                                    
003300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003500     EJECT                                                                
003600 01  NYCKLAR-TILL-DLI.                                                    
003700   03  W-IDARTNR-X.                                                       
003800     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
003900   03  W-IDSKYLT-X.                                                       
004000     05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                     
004250   03  W-WDJ1CSEQ-X.                                                      
004260     05  W-IDLEVNR-S         PIC X(5)    VALUE SPACE.                     
004270     05  W-BELEVART-S        PIC X(30)   VALUE SPACE.                     
004280     05  W-IDARTNR-S         PIC S9(9)   VALUE ZERO   COMP-3.             
004290   03  W-WDJ111KY-X.                                                      
004291     05  W-KDSTRRAD          PIC X       VALUE SPACE.                     
004292     05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.               
004300     EJECT                                                                
004400******************************************************************        
004500*                                                                         
004600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
004700*                                                                         
004800 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
004900     SKIP3                                                                
005000*01    MID -COPY W9I22201.                                                
005100     EJECT                                                                
005200*01    -COPY WMSGAREA                                                     
005300     EJECT                                                                
005400*  03    MOD -COPY W9O22201  -RED MSG-AREA.                               
005500     EJECT                                                                
005600*01    -COPY WMFSAREA                                                     
005700     EJECT                                                                
005800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
005900*                                                                         
006000 01    IMS-WS.                                                            
006100   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
006200     SKIP3                                                                
006300*                        **** STATUS-KOD FRÅN IMS                         
006400   03    STATUS-WS               PIC XX.                                  
006500     88    SEGMENT-FINNS                     VALUE '  '.                  
006600     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
006700     88    SEGMENT-SLUT                      VALUE 'GB'.                  
006800     SKIP3                                                                
006900   03    GODK-STATUSKODER.                                                
007000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
007100     SKIP3                                                                
007200 01    SSA1                      PIC X(96).                               
007300 01    SSA2                      PIC X(64).                               
007400     EJECT                                                                
007500*                            IMS FUNKTIONSKODER                           
007600*01    -COPY W0003                                                        
007700     EJECT                                                                
007800*    ---  DLI INPUT-OUTPUT AREA                                           
007900                                                                          
007901 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDJ1'.                      
007910 01  DLI-IO-WDJ1.                                                         
007950     05 WLSATB11.                                                         
007960*       07 -COPY WDJ111                                                   
007970     05 WLSATB01.                                                         
007980*       07 -COPY WDJ101                                                   
007990     EJECT                                                                
008800 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDD311'.                    
008900 01  DLI-IO-WDD311.                                                       
009000*  03  -COPY WDD311                                                       
009100     EJECT                                                                
009110 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDK601'.                    
009120 01  DLI-IO-WDK601.                                                       
009130*  03  -COPY WDK601                                                       
009140     EJECT                                                                
009200 LINKAGE SECTION.                                                         
009300                                                                          
009400*01    -COPY W0009     -PRE MSG-                                          
009500     EJECT                                                                
009600*01    -COPY W0008     -PRE WDJ1-                                         
009700     05  FILLER                  PIC X.                                   
009800     EJECT                                                                
009900*01    -COPY W0008     -PRE WDD3-                                         
010000     05  FILLER                  PIC X.                                   
010100     EJECT                                                                
010110*01    -COPY W0008     -PRE WDK6-                                         
010120     05  FILLER                  PIC X.                                   
010130     EJECT                                                                
010200 PROCEDURE DIVISION  USING MSG-PCB WDJ1-PCB WDD3-PCB WDK6-PCB.            
010300     ENTRY 'DLITCBL' USING MSG-PCB WDJ1-PCB WDD3-PCB WDK6-PCB.            
010400                                                                          
010500 STYR SECTION.                                                            
010600                                                                          
010700     PERFORM IMS-GET-MSG                                                  
010800     IF SEGMENT-FINNS                                                     
010900        PERFORM A-INIT                                                    
011000        PERFORM B-KONTROLLERA-NYCKLAR                                     
011100        IF NYCKLAR-OK                                                     
011200           PERFORM C-BEHANDLA-SATS                                        
011300        END-IF                                                            
011400     END-IF                                                               
011500     PERFORM E-KONTROLLERA-OM-TOM-SIDA                                    
011600     PERFORM F-BERAKNA-MAX-MOD-LANGD                                      
011700                                                                          
011800     MOVE MAX-MOD-LANGD TO MSG-KVLL                                       
011900     PERFORM IMS-INSERT-MSG                                               
012000     MOVE ZERO TO RETURN-CODE                                             
012100     GOBACK                                                               
012200     .                                                                    
012300     EJECT                                                                
012500 A-INIT SECTION.                                                          
012600                                                                          
012700     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I22201                    
012800     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
012900     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
013000     MOVE ' '                          TO MFS-KDTRTYP                     
013100     MOVE LOW-VALUE                    TO MSG-AREA                        
013200     MOVE 'W9O22201'                   TO MFS-IDMOD                       
013300     MOVE '9222'                       TO MOD-IDTRANS                     
013400     MOVE ZERO                         TO MOD-IDMFSFEL                    
013500                                          MOD-IDRADNR-NEXT                
013510                                          MOD-IDARTNR-NEXT                
013520     MOVE SPACE                        TO MOD-KDSTRRAD-NEXT               
013600                                                                          
013700     MOVE SPACE TO MOD-BEART-ING                                          
013800     MOVE +1 TO  RAD-IX                                                   
013900     PERFORM UNTIL RAD-IX > MAX-RAD                                       
014000        MOVE ZERO  TO MOD-IDARTNR-RAD  (RAD-IX)                           
014100                      MOD-REANTPSA-RAD (RAD-IX)                           
014200        MOVE SPACE TO MOD-BEART-RAD    (RAD-IX)                           
014300        ADD +1 TO RAD-IX                                                  
014400     END-PERFORM                                                          
014500                                                                          
014600     ACCEPT DAGENS-DATUM FROM DATE                                        
014700     .                                                                    
014800     EJECT                                                                
014900 B-KONTROLLERA-NYCKLAR SECTION.                                           
015000                                                                          
015100     IF MID-IDARTNR NUMERIC                                               
015200        MOVE MID-IDARTNR TO WS-IDARTNR                                    
015210                            MOD-IDARTNR                                   
015300     ELSE                                                                 
015400        MOVE NEJ   TO NYCKLAR-SW                                          
015500        MOVE 'B01' TO MOD-IDMFSFEL                                        
015600     END-IF                                                               
015700     .                                                                    
015800     EJECT                                                                
015900 C-BEHANDLA-SATS SECTION.                                                 
016000                                                                          
016100     PERFORM CA-LAS-FORSTA-ARTIKEL                                        
016110     IF SEGMENT-FINNS                                                     
016200        MOVE WS-IDARTNR TO W-IDARTNR                                      
016300        PERFORM IMS-GET-WDD311                                            
016400        IF SEGMENT-FINNS                                                  
016500           MOVE TEXT-BEART TO MOD-BEART-ING                               
016600        END-IF                                                            
016700     END-IF                                                               
020150                                                                          
020160     MOVE +1 TO RAD-IX                                                    
020171     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
020172                   OR RAD-IX > MAX-RAD                                    
020190        IF STR-IDARTNR < 100000000                                        
020191           IF STR-TIBORT = ZERO                                           
020197              PERFORM CB-KOLLA-ARTIKELRAD                                 
020198              IF WS-AKTUELL-RAD = JA                                      
020199                 MOVE STR-IDARTNR TO W-IDARTNR                            
020200                 PERFORM IMS-GET-WDK601                                   
020201                 IF SEGMENT-FINNS                                         
020203                    MOVE ART-KDSORT TO MOD-KDSORT-RAD(RAD-IX)             
020205                    MOVE STR-IDARTNR TO MOD-IDARTNR-RAD(RAD-IX)           
020209                    MOVE RAD-REANTPSA TO MOD-REANTPSA-RAD(RAD-IX)         
020210                    PERFORM IMS-GET-WDD311                                
020211                    IF SEGMENT-FINNS                                      
020212                       MOVE TEXT-BEART TO MOD-BEART-RAD(RAD-IX)           
020213                    END-IF                                                
020218                    ADD +1 TO RAD-IX                                      
020219                 ELSE                                                     
020220                    MOVE NEJ TO WS-AKTUELL-RAD                            
020221                 END-IF                                                   
020222              END-IF                                                      
020223           END-IF                                                         
020230        END-IF                                                            
020231                                                                          
020238        PERFORM IMS-GET-SATB-CSEQ-NEXT                                    
020239        MOVE NEJ TO WS-AKTUELL-RAD                                        
020240                                                                          
020250        IF SEGMENT-FINNS AND RAD-IX > MAX-RAD                             
020280           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
020281                         OR WS-AKTUELL-RAD = JA                           
020282              IF STR-IDARTNR < 100000000                                  
020283                 IF STR-TIBORT = ZERO                                     
020300                    PERFORM CB-KOLLA-ARTIKELRAD                           
020400                    IF WS-AKTUELL-RAD = JA                                
020410                       MOVE STR-IDARTNR  TO W-IDARTNR                     
020420                       PERFORM IMS-GET-WDK601                             
020430                       IF SEGMENT-SAKNAS                                  
020440                          MOVE NEJ TO WS-AKTUELL-RAD                      
020450                       ELSE                                               
020500                          MOVE RAD-IDRADNR  TO MOD-IDRADNR-NEXT           
020501                          MOVE RAD-KDSTRRAD TO MOD-KDSTRRAD-NEXT          
020502                          MOVE STR-IDARTNR  TO MOD-IDARTNR-NEXT           
020503                       END-IF                                             
020530                    END-IF                                                
020540                 END-IF                                                   
020550              END-IF                                                      
020551              IF WS-AKTUELL-RAD = NEJ                                     
020560                 PERFORM IMS-GET-SATB-CSEQ-NEXT                           
020561              END-IF                                                      
020570           END-PERFORM                                                    
020571        END-IF                                                            
020572     END-PERFORM                                                          
020600     .                                                                    
020700     EJECT                                                                
020800 CA-LAS-FORSTA-ARTIKEL SECTION.                                           
020801                                                                          
020802     MOVE WS-IDARTNR TO W-IDARTNR-S                                       
020803     MOVE SPACE      TO W-IDLEVNR-S                                       
020804     MOVE SPACE      TO W-BELEVART-S                                      
020805                                                                          
020806     IF MID-IDRADNR-NEXT NUMERIC                                          
020807     AND MID-IDARTNR-NEXT NUMERIC                                         
020808        IF MID-IDRADNR-NEXT  > ZERO                                       
020809        AND MID-IDARTNR-NEXT > ZERO                                       
020810           MOVE MID-IDARTNR-NEXT  TO W-IDARTNR                            
020811           MOVE MID-IDRADNR-NEXT  TO W-IDRADNR                            
020812           MOVE MID-KDSTRRAD-NEXT TO W-KDSTRRAD                           
020813           PERFORM IMS-GET-SATB-CSEQ-UNIK                                 
020814        ELSE                                                              
020818           PERFORM IMS-GET-SATB-CSEQ-FIRST                                
020819        END-IF                                                            
020820     ELSE                                                                 
020821        PERFORM IMS-GET-SATB-CSEQ-FIRST                                   
020822     END-IF                                                               
020823     .                                                                    
020824     EJECT                                                                
020830 CB-KOLLA-ARTIKELRAD SECTION.                                             
020900                                                                          
021000     MOVE NEJ TO WS-AKTUELL-RAD                                           
021100     IF SEGMENT-FINNS                                                     
021200        MOVE RAD-TISTADAT TO TMP1-YYMMDD                                  
021300        MOVE DAGENS-DATUM TO TMP2-YYMMDD                                  
021500        PERFORM WY2000P1                                                  
021600        IF TMP1-YYMMDD <= TMP2-YYMMDD                                     
021700           MOVE RAD-TISTODAT TO TMP1-YYMMDD                               
021710           MOVE DAGENS-DATUM TO TMP2-YYMMDD                               
021800           PERFORM WY2000P1                                               
021900           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
022000              MOVE JA TO WS-AKTUELL-RAD                                   
022100           END-IF                                                         
022110        END-IF                                                            
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
023400 E-KONTROLLERA-OM-TOM-SIDA SECTION.                                       
023500                                                                          
023600     IF RAD-IX = 1                                                        
023700        MOVE SPACE TO MOD-BEART-ING                                       
023800        MOVE 'B10' TO MOD-IDMFSFEL                                        
023900     END-IF                                                               
024000     .                                                                    
024100     EJECT                                                                
024200 F-BERAKNA-MAX-MOD-LANGD SECTION.                                         
024300                                                                          
024400     MOVE 13 TO RAD-IX                                                    
024500     PERFORM UNTIL RAD-IX = 0                                             
024600        IF MOD-IDARTNR-RAD(RAD-IX) = ZERO                                 
024700           SUBTRACT +42 FROM MAX-MOD-LANGD                                
024800           SUBTRACT +1  FROM RAD-IX                                       
024900        ELSE                                                              
025000           MOVE ZERO TO RAD-IX                                            
025100        END-IF                                                            
025200     END-PERFORM                                                          
025300     .                                                                    
025400     EJECT                                                                
025500* IMS SEKTIONER                                                           
025600     SKIP3                                                                
025700 IMS-GET-MSG SECTION.                                                     
025800     MOVE '  QC' TO GODK-STATUSKODER                                      
025900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
026000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026100     PERFORM IMS-STATUSKONTROLL                                           
026200     .                                                                    
026300     SKIP3                                                                
026400 IMS-INSERT-MSG SECTION.                                                  
026500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
026600     MOVE SPACE TO GODK-STATUSKODER                                       
026700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
026800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100     EJECT                                                                
027110 IMS-GET-SATB-CSEQ-FIRST SECTION.                                         
027120     STRING 'WDJ111  *D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
027130            DELIMITED BY SIZE INTO SSA1                                   
027140     MOVE 'WDJ101  ' TO SSA2                                              
027160     MOVE '  GEGB' TO GODK-STATUSKODER                                    
027170     CALL CBLTDLI USING GU WDJ1-PCB DLI-IO-WDJ1 SSA1 SSA2                 
027180     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
027190     PERFORM IMS-STATUSKONTROLL                                           
027191     .                                                                    
027192     SKIP3                                                                
027193 IMS-GET-SATB-CSEQ-NEXT SECTION.                                          
027194     STRING 'WDJ111  *D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
027195            DELIMITED BY SIZE INTO SSA1                                   
027196     MOVE 'WDJ101  ' TO SSA2                                              
027199     MOVE '  GEGB' TO GODK-STATUSKODER                                    
027200     CALL CBLTDLI USING GN WDJ1-PCB DLI-IO-WDJ1 SSA1 SSA2                 
027201     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
027202     PERFORM IMS-STATUSKONTROLL                                           
027203     .                                                                    
027204     EJECT                                                                
028910 IMS-GET-SATB-CSEQ-UNIK SECTION.                                          
028930     STRING 'WDJ111  *D(WDJ1CSEQ =' W-WDJ1CSEQ-X                          
028940                     '&WDJ111KY =' W-WDJ111KY-X ')'                       
028950          DELIMITED BY SIZE INTO SSA1                                     
028960     STRING 'WDJ101  (IDARTNR  =' W-IDARTNR-X ')'                         
028970          DELIMITED BY SIZE INTO SSA2                                     
028980     MOVE '  GE' TO GODK-STATUSKODER                                      
028990     CALL CBLTDLI USING GU WDJ1-PCB DLI-IO-WDJ1 SSA1 SSA2                 
028991     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
028992     PERFORM IMS-STATUSKONTROLL                                           
028993     .                                                                    
028994     SKIP3                                                                
029000 IMS-GET-WDD311 SECTION.                                                  
029100     STRING 'WDD301  (WDD3BSEQ= ' W-IDARTNR-X ')'                         
029200          DELIMITED BY SIZE INTO SSA1                                     
029300     STRING 'WDD311  (IDSKYLT = ' W-IDSKYLT-X ')'                         
029400          DELIMITED BY SIZE INTO SSA2                                     
029500     MOVE '  GE' TO GODK-STATUSKODER                                      
029600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
029700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
029800     PERFORM IMS-STATUSKONTROLL                                           
029900     .                                                                    
030000     EJECT                                                                
030010 IMS-GET-WDK601 SECTION.                                                  
030020     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
030030          DELIMITED BY SIZE INTO SSA1                                     
030040     MOVE '  GE' TO GODK-STATUSKODER                                      
030050     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
030060     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
030070     PERFORM IMS-STATUSKONTROLL                                           
030080     .                                                                    
030090     SKIP3                                                                
030100 IMS-STATUSKONTROLL SECTION.                                              
030200     SET STATUS-IX TO 1                                                   
030300     SEARCH GODK-STATUS                                                   
030400       AT END                                                             
030500         CALL FELLOG                                                      
030600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030700         CONTINUE                                                         
030800     END-SEARCH                                                           
030900     .                                                                    
031000     EJECT                                                                
031100*    -COPY WY2000P1                                                       
