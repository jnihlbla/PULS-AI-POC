000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9022100.                                                
000400 AUTHOR.         BODIL LINDAHL                                            
000600 DATE-WRITTEN.   MARS 2000.                                               
000700*                                                                         
000800*REMARKS.                                                                 
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET LÄSER INGÅENDE ARTIKLAR I SATS.                       
001200*        VIA VDI-SYSTEMET.                                                
001840*                                                                         
001850*    INDATA.                                                              
001860*        TRANSAKTION: W90221T                                             
001870*        MID:         W9I22101                                            
001880*                                                                         
001890*    UTDATA.                                                              
001900*        MOD:         W9O22101                                            
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP3                                                                
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002610*    -COPY WY2000W1                                                       
002700     SKIP3                                                                
002900 77  IDPGM                       PIC X(8)    VALUE 'W9022100'.            
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003210 77  WS-IDARTNR-SATS             PIC 9(9)    VALUE ZERO.                  
003220 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003230 77  WS-AKTUELL-RAD              PIC X       VALUE SPACE.                 
003300 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
004100 77  MAX-RAD                     PIC S9(3)   VALUE +13  COMP-3.           
004110 77  RAD-IX                      PIC S9(3)   VALUE +0   COMP-3.           
004200 77  MAX-MOD-LANGD               PIC S9(3)   VALUE +572 COMP-3.           
004400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004500     88  NYCKLAR-OK                          VALUE 'J'.                   
005400                                                                          
005500 01  DYNAMISKA-SUBPGM.                                                    
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     EJECT                                                                
007200 01  NYCKLAR-TILL-DLI.                                                    
007300   03  W-IDARTNR-X.                                                       
007400     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
007500   03  W-IDSKYLT-X.                                                       
007600     05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                     
007900   03  W-IDRADNR-X.                                                       
008000     05  W-IDRADNR           PIC S9(5)   VALUE ZERO  COMP-3.              
014200     EJECT                                                                
014300******************************************************************        
014400*                                                                         
014500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014600*                                                                         
014700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
014800     SKIP3                                                                
014900*01    MID -COPY W9I22101.                                                
015000     EJECT                                                                
015100*01    -COPY WMSGAREA                                                     
015200     EJECT                                                                
015300*  03    MOD -COPY W9O22101  -RED MSG-AREA.                               
015400     EJECT                                                                
015500*01    -COPY WMFSAREA                                                     
015600     EJECT                                                                
015900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016000*                                                                         
016100 01    IMS-WS.                                                            
016200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
016300     SKIP3                                                                
016400*                        **** STATUS-KOD FRÅN IMS                         
016500   03    STATUS-WS               PIC XX.                                  
016600     88    SEGMENT-FINNS                     VALUE '  '.                  
016700     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
016800     88    SEGMENT-SLUT                      VALUE 'GB'.                  
016900     SKIP3                                                                
017000   03    GODK-STATUSKODER.                                                
017100     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
017200     SKIP3                                                                
017300 01    SSA1                      PIC X(64).                               
017400 01    SSA2                      PIC X(64).                               
017500     EJECT                                                                
017600*                            IMS FUNKTIONSKODER                           
017700*01    -COPY W0003                                                        
017800     EJECT                                                                
017900*    ---  DLI INPUT-OUTPUT AREA                                           
018000                                                                          
018100 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDJ101'.                    
018200 01  DLI-IO-WDJ101.                                                       
018300*  03  -COPY WDJ101                                                       
018400     EJECT                                                                
018410 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDJ111'.                    
018420 01  DLI-IO-WDJ111.                                                       
018430*  03  -COPY WDJ111                                                       
018440     EJECT                                                                
018500 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDD311'.                    
018600 01  DLI-IO-WDD311.                                                       
018700*  03  -COPY WDD311                                                       
018800     EJECT                                                                
018900 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDK601'.                    
019000 01  DLI-IO-WDK601.                                                       
019100*  03  -COPY WDK601                                                       
019200     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000                                                                          
020100*01    -COPY W0009     -PRE MSG-                                          
020200     EJECT                                                                
020300*01    -COPY W0008     -PRE WDJ1-                                         
020400     05  FILLER                  PIC X.                                   
020500     EJECT                                                                
020510*01    -COPY W0008     -PRE WDD3-                                         
020520     05  FILLER                  PIC X.                                   
020530     EJECT                                                                
020540*01    -COPY W0008     -PRE WDK6-                                         
020550     05  FILLER                  PIC X.                                   
020560     EJECT                                                                
021200 PROCEDURE DIVISION  USING MSG-PCB WDJ1-PCB WDD3-PCB WDK6-PCB.            
021400     ENTRY 'DLITCBL' USING MSG-PCB WDJ1-PCB WDD3-PCB WDK6-PCB.            
021600                                                                          
021700 STYR SECTION.                                                            
021800                                                                          
021900     PERFORM IMS-GET-MSG                                                  
022000     IF SEGMENT-FINNS                                                     
022100        PERFORM A-INIT                                                    
022200        PERFORM B-KONTROLLERA-NYCKLAR                                     
022300        IF NYCKLAR-OK                                                     
022400           PERFORM C-BEHANDLA-SATS                                        
022500        END-IF                                                            
022600     END-IF                                                               
022700     PERFORM E-KONTROLLERA-OM-TOM-SIDA                                    
022800     PERFORM F-BERAKNA-MAX-MOD-LANGD                                      
022810                                                                          
022900     MOVE MAX-MOD-LANGD TO MSG-KVLL                                       
023000     PERFORM IMS-INSERT-MSG                                               
023100     MOVE ZERO TO RETURN-CODE                                             
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500                                                                          
023600 A-INIT SECTION.                                                          
023700                                                                          
023800     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I22101                    
023900     MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                     
024000     MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                    
024100     MOVE ' '                          TO MFS-KDTRTYP                     
024200     MOVE LOW-VALUE                    TO MSG-AREA                        
024300     MOVE 'W9O22101'                   TO MFS-IDMOD                       
024400     MOVE '9221'                       TO MOD-IDTRANS                     
024500     MOVE ZERO                         TO MOD-IDMFSFEL                    
024600                                          MOD-IDRADNR-NEXT                
024900                                                                          
024910     MOVE SPACE TO MOD-BEART-SATS                                         
024920                   MOD-KDSORT-SATS                                        
025000     MOVE +1 TO  RAD-IX                                                   
025100     PERFORM UNTIL RAD-IX > MAX-RAD                                       
025200        MOVE ZERO  TO MOD-IDARTNR-RAD  (RAD-IX)                           
025300                      MOD-REANTPSA-RAD (RAD-IX)                           
025320        MOVE SPACE TO MOD-BEART-RAD    (RAD-IX)                           
026300        ADD +1 TO RAD-IX                                                  
026400     END-PERFORM                                                          
026410                                                                          
026420     ACCEPT DAGENS-DATUM FROM DATE                                        
026500     .                                                                    
026600     EJECT                                                                
026700 B-KONTROLLERA-NYCKLAR SECTION.                                           
026800                                                                          
026900     IF MID-IDARTNR NUMERIC                                               
027100        MOVE MID-IDARTNR TO WS-IDARTNR-SATS                               
027110                            MOD-IDARTNR                                   
027200     ELSE                                                                 
027300        MOVE NEJ   TO NYCKLAR-SW                                          
027400        MOVE 'B01' TO MOD-IDMFSFEL                                        
027500     END-IF                                                               
027600     .                                                                    
027700     EJECT                                                                
027800 C-BEHANDLA-SATS SECTION.                                                 
027900                                                                          
028000     IF MID-IDRADNR-NEXT NUMERIC                                          
028010        IF MID-IDRADNR-NEXT >  ZERO                                       
028100           MOVE MID-IDRADNR-NEXT TO W-IDRADNR                             
028101        ELSE                                                              
028102           MOVE ZERO TO W-IDRADNR                                         
028103        END-IF                                                            
028110     ELSE                                                                 
028111        MOVE ZERO TO W-IDRADNR                                            
028120     END-IF                                                               
028130                                                                          
028200     PERFORM CA-LAS-SATS                                                  
028300     MOVE +1 TO RAD-IX                                                    
028701                                                                          
028703     IF SEGMENT-FINNS                                                     
028704        IF STR-IDLEVNR = '1002'                                           
028705        AND STR-IDARTNR < 100000000                                       
028706        AND STR-TIBORT = ZERO                                             
028707           MOVE WS-IDARTNR-SATS TO W-IDARTNR                              
028708           PERFORM IMS-GET-WDD311                                         
028709           IF SEGMENT-FINNS                                               
028710              MOVE TEXT-BEART TO MOD-BEART-SATS                           
028711           END-IF                                                         
028712           PERFORM IMS-GET-WDK601                                         
028713           IF SEGMENT-FINNS                                               
028714              MOVE ART-KDSORT TO MOD-KDSORT-SATS                          
028715           END-IF                                                         
028720                                                                          
028730           PERFORM IMS-GET-WDJ111                                         
029000           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
029100                         OR RAD-IX > MAX-RAD                              
029300                                                                          
029400              PERFORM CB-KOLLA-INGAENDE-ART                               
029410              IF WS-AKTUELL-RAD = JA                                      
029420                 MOVE RAD-IDARTNR TO MOD-IDARTNR-RAD(RAD-IX)              
029430                                     W-IDARTNR                            
029440                 MOVE RAD-REANTPSA TO MOD-REANTPSA-RAD(RAD-IX)            
029450                 PERFORM IMS-GET-WDD311                                   
029460                 IF SEGMENT-FINNS                                         
029470                    MOVE TEXT-BEART TO MOD-BEART-RAD(RAD-IX)              
029480                 END-IF                                                   
029500                 ADD +1 TO RAD-IX                                         
029600              END-IF                                                      
029800                                                                          
029810              PERFORM IMS-GET-WDJ111                                      
029811              MOVE NEJ TO WS-AKTUELL-RAD                                  
029813                                                                          
029900              IF SEGMENT-FINNS AND RAD-IX > MAX-RAD                       
029904                 PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT             
029905                         OR WS-AKTUELL-RAD = JA                           
029910                    PERFORM CB-KOLLA-INGAENDE-ART                         
029911                    IF WS-AKTUELL-RAD = JA                                
030000                       MOVE RAD-IDRADNR TO  MOD-IDRADNR-NEXT              
030010                    ELSE                                                  
030100                       PERFORM IMS-GET-WDJ111                             
030110                    END-IF                                                
030120                 END-PERFORM                                              
030200              END-IF                                                      
030210                                                                          
030300           END-PERFORM                                                    
030320        END-IF                                                            
030420     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
031700 CA-LAS-SATS SECTION.                                                     
031701                                                                          
031702     MOVE WS-IDARTNR-SATS TO W-IDARTNR                                    
031703     PERFORM IMS-GET-WDJ101                                               
031707     .                                                                    
031708     EJECT                                                                
031710 CB-KOLLA-INGAENDE-ART SECTION.                                           
031800                                                                          
031810     MOVE NEJ TO WS-AKTUELL-RAD                                           
031820     IF SEGMENT-FINNS                                                     
031870        MOVE RAD-TISTODAT TO TMP1-YYMMDD                                  
031880        MOVE DAGENS-DATUM TO TMP2-YYMMDD                                  
031890        PERFORM WY2000P1                                                  
031892        IF TMP1-YYMMDD > TMP2-YYMMDD                                      
031894           MOVE RAD-TISTADAT TO TMP1-YYMMDD                               
031895           MOVE DAGENS-DATUM TO TMP2-YYMMDD                               
031901           PERFORM WY2000P1                                               
031902           IF TMP1-YYMMDD <= TMP2-YYMMDD                                  
031903              MOVE JA TO WS-AKTUELL-RAD                                   
031910           END-IF                                                         
031940        END-IF                                                            
033600     END-IF                                                               
035500     .                                                                    
035600     EJECT                                                                
041200 E-KONTROLLERA-OM-TOM-SIDA SECTION.                                       
041300                                                                          
041400     IF RAD-IX = 1                                                        
041410        MOVE SPACE TO MOD-BEART-SATS                                      
041420                      MOD-KDSORT-SATS                                     
041500        MOVE 'B10' TO MOD-IDMFSFEL                                        
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 F-BERAKNA-MAX-MOD-LANGD SECTION.                                         
042000                                                                          
042100     MOVE 13 TO RAD-IX                                                    
042200     PERFORM UNTIL RAD-IX = 0                                             
042300        IF MOD-IDARTNR-RAD(RAD-IX) = ZERO                                 
042400           SUBTRACT +40 FROM MAX-MOD-LANGD                                
042500           SUBTRACT +1  FROM RAD-IX                                       
042600        ELSE                                                              
042700           MOVE ZERO TO RAD-IX                                            
042800        END-IF                                                            
042900     END-PERFORM                                                          
043000     .                                                                    
043100     EJECT                                                                
047600* IMS SEKTIONER                                                           
047700     SKIP3                                                                
047800 IMS-GET-MSG SECTION.                                                     
048000     MOVE '  QC' TO GODK-STATUSKODER                                      
048100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
048200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048300     PERFORM IMS-STATUSKONTROLL                                           
048400     .                                                                    
048500     SKIP3                                                                
048600 IMS-INSERT-MSG SECTION.                                                  
048800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
048900     MOVE SPACE TO GODK-STATUSKODER                                       
049000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
049100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     .                                                                    
049400     SKIP3                                                                
053693 IMS-GET-WDK601 SECTION.                                                  
053694     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
053695          DELIMITED BY SIZE INTO SSA1                                     
053698     MOVE '  GE' TO GODK-STATUSKODER                                      
053699     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
053700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
053701     PERFORM IMS-STATUSKONTROLL                                           
053702     .                                                                    
053703     EJECT                                                                
053704 IMS-GET-WDJ101 SECTION.                                                  
053705     STRING 'WDJ101  (IDARTNR = ' W-IDARTNR-X ')'                         
053706          DELIMITED BY SIZE INTO SSA1                                     
053707     MOVE '  GE' TO GODK-STATUSKODER                                      
053708     CALL CBLTDLI USING GU WDJ1-PCB DLI-IO-WDJ101 SSA1                    
053709     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
053710     PERFORM IMS-STATUSKONTROLL                                           
053711     .                                                                    
053712     SKIP3                                                                
053713 IMS-GET-WDJ111 SECTION.                                                  
053714     STRING 'WDJ111  (IDRADNR =>' W-IDRADNR-X ')'                         
053715          DELIMITED BY SIZE INTO SSA1                                     
053716     MOVE '  GE' TO GODK-STATUSKODER                                      
053717     CALL CBLTDLI USING GNP WDJ1-PCB DLI-IO-WDJ111 SSA1                   
053718     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
053719     PERFORM IMS-STATUSKONTROLL                                           
053720     .                                                                    
053722     SKIP3                                                                
053723 IMS-GET-WDD311 SECTION.                                                  
053724     STRING 'WDD301  (WDD3BSEQ= ' W-IDARTNR-X ')'                         
053725          DELIMITED BY SIZE INTO SSA1                                     
053726     STRING 'WDD311  (IDSKYLT = ' W-IDSKYLT-X ')'                         
053727          DELIMITED BY SIZE INTO SSA2                                     
053728     MOVE '  GE' TO GODK-STATUSKODER                                      
053729     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
053730     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
053731     PERFORM IMS-STATUSKONTROLL                                           
053732     .                                                                    
053733     EJECT                                                                
053740 IMS-STATUSKONTROLL SECTION.                                              
053900     SET STATUS-IX TO 1                                                   
054000     SEARCH GODK-STATUS                                                   
054100       AT END                                                             
054200         CALL FELLOG                                                      
054300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
054400         CONTINUE                                                         
054500     END-SEARCH                                                           
054600     .                                                                    
054610     EJECT                                                                
054700*    -COPY WY2000P1                                                       
