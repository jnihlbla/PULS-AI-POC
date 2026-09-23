000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1029400.                                                
000400 AUTHOR.         HENRIK ARONSSON.                                         
000500 DATE-WRITTEN.   AUG 1990.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        BAKGRUNDSPROGRAM TILL BILD 1221.                                 
001100*        SKÖTER OM UPPDATERING(BYTE/BORTTAG)/OMNUMRERING AV RADER         
001200*        I VALDA STRUKTURER. PROGRAMMET UPPDATERAR 10 STRUKTURER          
001300*        OCH STARTAR SEDAN OM SIG SJÄLV.                                  
001400*                                                                         
001500*        ÄT SPLIT 930404 BL                                               
001600*          - KONTROLL FÖRPACKNING ÄNDRAT                                  
001700*                                                                         
001800*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001900*        PROGRAMMET UPPDATERAR WLXXAZ (WDR5)                              
002000*        PROGRAMMET UPPDATERAR WLXXBY (WDR5)                              
002100*        PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
002200*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002300*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W1T294U                                             
002700*        MID:         W1I29401                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W1O22201                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900*                                                                         
004000 77  IDPGM                       PIC X(08)   VALUE 'W1029400'.            
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +668  COMP SYNC.        
004700                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005100 77  WS-BELEVART                 PIC X(25)   VALUE SPACE.                 
005200                                                                          
005300*    --- ARBETSFÄLT FÖR ATT KUNNA SPARA NYCKELVÄRDEN                      
005400*    --- NÄR MAN KOMMER FRÅN BILD 1221 TILL 1222                          
005500 77  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
005600 77  WS-1002-SATS                PIC X(1)    VALUE SPACE.                 
005700 77  WS-KDPRODSL                 PIC X(2)    VALUE SPACE.                 
005800                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  EGEN-MID                            VALUE '1294'.                
006100     88  GODK-MID                            VALUE '1222' '1294'.         
006200                                                                          
006300 77  NYCKLAR-SW                  PIC X       VALUE SPACE.                 
006400     88 NYCKLAR-OK                           VALUE 'J'.                   
006500     88 NYCKLAR-FEL                          VALUE 'N'.                   
006600                                                                          
006700 77  KONVERTERADE-STRUKT-KVAR-SW PIC X       VALUE SPACE.                 
006800     88 KONVERTERADE-STRUK-FINNS-KVAR        VALUE 'J'.                   
006900                                                                          
007000 77  SOEKNYCKELTYP-SW            PIC X       VALUE SPACE.                 
007100     88 SOEKNYCKEL-IDARTNR                   VALUE 'A'.                   
007200     88 SOEKNYCKEL-IDALEVNR-O-BELEVART       VALUE 'I'.                   
007300                                                                          
007400 77  RAD-FINNS-I-TABELL-SW       PIC X       VALUE SPACE.                 
007500     88 RAD-FINNS-I-TABELL                   VALUE 'J'.                   
007600                                                                          
007700 77  STRUKTUR-AER-1002-SATS-SW   PIC X       VALUE SPACE.                 
007800     88 STRUKTUR-AER-1002-SATS               VALUE 'J'.                   
007900                                                                          
008000 77  TILLK-ARTIKEL-FORPACKNING-SW PIC X      VALUE SPACE.                 
008100     88 TILLK-ARTIKEL-FORPACKNING            VALUE 'J'.                   
008200                                                                          
008300 77  TISTODAT-PAA-WDR5-IFYLLT-SW  PIC X      VALUE SPACE.                 
008400     88 TISTODAT-PAA-WDR5-IFYLLT             VALUE 'J'.                   
008500     88 TISTODAT-PAA-WDR5-EJ-IFYLLT          VALUE 'N'.                   
008600                                                                          
008700                                                                          
008800 77  WS-BEFT-AKTUELL                         PIC S9(3) COMP-3.            
008900     88  BEFT-AKTUELL                        VALUE 70 71 72               
009000                                                   73 74 75.              
009100     EJECT                                                                
009200*                                                                         
009300*01    -COPY WWPRODSL                                                     
009400*      --- VALID IDDC CODES                                               
009500*                                                                         
009600*01    -COPY WWDCKONS                                                     
009700*01    -COPY WWDC99                                                       
009800       EJECT                                                              
009900*********** SWITCHAR SOM ANVÄNDS VID TESTER AV HUR DEN                    
010000*********** BEHANDLADE (BYTTA/BORTTAGNA) RADENS KDISATS                   
010100*********** SER UT FÖRE OCH EFTER UPPDATERING                             
010200                                                                          
010300 77  STRUKTURRAD-KONV-KDISATS-SW  PIC X      VALUE SPACE.                 
010400     88 STRUKTURRAD-KONV-AER-E-MAERKT        VALUE 'E'.                   
010500     88 STRUKTURRAD-KONV-AER-T-MAERKT        VALUE 'T'.                   
010600                                                                          
010700 77  STRUKTURRAD-OKONV-KDISATS-SW PIC X      VALUE SPACE.                 
010800     88 STRUKTURRAD-OKONV-AER-E-MAERKT       VALUE 'E'.                   
010900     88 STRUKTURRAD-OKONV-AER-T-MAERKT       VALUE 'T'.                   
011000                                                                          
011100     EJECT                                                                
011200                                                                          
011300*    -------------- DIVERSE VARIABLER                                     
011400                                                                          
011500 01  DAGENS-DATUM                PIC 9(6).                                
011600 01  IDARTNR                     PIC X        VALUE 'A'.                  
011700 01  IDLEVNR-O-BELEVART          PIC X        VALUE 'I'.                  
011800 01  WS-IDARTNR-OKONV            PIC 9(9).                                
011900 01  WS-IDARTNR-KONV             PIC 9(9).                                
012000 01  WS-IDARTNR-RAD              PIC 9(9).                                
012100 01  WS-IDRADNR                  PIC S9(5).                               
012200 01  WS-IDRADNR-FORPACKNING      PIC S9(5).                               
012300 01  WS-KDSTRRAD                 PIC X(1).                                
012400 01  WS-IDAO                     PIC X(10).                               
012500 01  WS-TISTODAT                 PIC S9(7).                               
012600 01  WS-TIAAVV                   PIC 9(4).                                
012700                                                                          
012800 01  INDX                        PIC S9(3)    VALUE ZERO.                 
012900 01  STRUKTUR-INDX               PIC S9(3)    VALUE ZERO.                 
013000 01  MAX-INDX                    PIC S9(3)    VALUE +2.                   
013100 01  MAX-TABELL-LAENGD           PIC S9(3)    VALUE +50.                  
013200 01  MAX-ANTAL-STRUKT-I-TAGET    PIC S9(3)    VALUE +10.                  
013300                                                                          
013400     EJECT                                                                
013500                                                                          
013600*    ----------------- TABELLER                                           
013700 01  RADNRTABELL.                                                         
013800     03 IDRADNR    OCCURS 50     PIC S9(5) COMP-3.                        
013900                                                                          
014000     EJECT                                                                
014100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014200 01  GENERELLA-SUBPROGRAM.                                                
014300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
014600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014800     EJECT                                                                
014900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015000*   -COPY WMEDAREA                                                        
015100     SKIP3                                                                
015200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
015300*   -COPY WDATAREA                                                        
015400     SKIP3                                                                
015500*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
015600*   -COPY WORKAREA                                                        
015700     SKIP3                                                                
015800 01  MESSAGE-CODES.                                                       
015900                                                                          
016000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016100     EJECT                                                                
016200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016300*                                                                         
016400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016500     SKIP3                                                                
016600*01  MID -COPY W1I29401                                                   
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016900     SKIP3                                                                
017000*01  -COPY WMSGAREA                                                       
017100     EJECT                                                                
017200     03  MOD REDEFINES MSG-AREA.                                          
017300*      05  -COPY W1O22201                                                 
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017600     SKIP3                                                                
017700*01  -COPY WMFSAREA                                                       
017800     EJECT                                                                
017900*    --------------------- ALT-AREA                                       
018000*                                                                         
018100 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
018200     SKIP3                                                                
018300 01  W-PROG-TO-PROG-SW.                                                   
018400     03  M-SW-LL                 PIC S9(4)   VALUE +571                   
018500                                             COMP SYNC.                   
018600     03  M-SW-Z1-Z2              PIC  X(2)   VALUE LOW-VALUE.             
018700     03  M-SW-KDTRANS            PIC  X(8)   VALUE 'W1T294X '.            
018800     03  M-SW-IDTRANS            PIC  X(4)   VALUE '1294'.                
018900     03  M-SW-KDMFSTYP           PIC  X(1)   VALUE '1'.                   
019000     03  MID-W10294.                                                      
019100*        05 MID1 -COPY W1I29401   -PRE MID-                               
019200     EJECT                                                                
019300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019400*                                                                         
019500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019600     SKIP3                                                                
019700 01  NYCKLAR-TILL-DLI.                                                    
019800                                                                          
019900     03  W-IDARTNR-X.                                                     
020000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020100                                                                          
020200     03  W-TIBEHOV-X.                                                     
020300         05  W-TIBEHOV           PIC S9(5)   VALUE ZERO COMP-3.           
020400                                                                          
020500     03  W-IDHTYP-X.                                                      
020600         05 W-IDHTYP             PIC X(4)    VALUE SPACE.                 
020700                                                                          
020800     03  W-KDCLAGER-X.                                                    
020900         05 W-KDCLAGER           PIC S9      VALUE ZERO COMP-3.           
021000                                                                          
021100     03  W-IDUSER-X.                                                      
021200         05  W-IDUSER            PIC  X(8)   VALUE SPACE.                 
021300                                                                          
021400     03  W-IDLEVNR-X.                                                     
021500         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
021600                                                                          
021700     03  W-IDARTNR-OKONV-X.                                               
021800         05  W-IDARTNR-OKONV     PIC S9(9)   VALUE ZERO COMP-3.           
021900                                                                          
022000     03  W-IDARTNR-KONV-X.                                                
022100         05  W-IDARTNR-KONV      PIC S9(9)   VALUE ZERO COMP-3.           
022200                                                                          
022300     03  W-MIN-IDARTNR-KONV-X.                                            
022400         05  W-MIN-IDARTNR-KONV  PIC S9(9) VALUE                          
022500                                             +100000000 COMP-3.           
022600                                                                          
022700     03  W-MAX-IDARTNR-KONV-X.                                            
022800         05  W-MAX-IDARTNR-KONV  PIC S9(9) VALUE                          
022900                                             +999999999 COMP-3.           
023000                                                                          
023100     03  W-WDJ111KY-OKONV-X.                                              
023200         05  W-KDSTRRAD-OKONV    PIC X(1)    VALUE SPACE.                 
023300         05  W-IDRADNR-OKONV     PIC S9(5)   VALUE ZERO COMP-3.           
023400                                                                          
023500     03  W-WDJ111KY-KONV-X.                                               
023600         05  W-KDSTRRAD-KONV     PIC X(1)    VALUE SPACE.                 
023700         05  W-IDRADNR-KONV      PIC S9(5)   VALUE ZERO COMP-3.           
023800                                                                          
023900     03  W-BELEVART-X.                                                    
024000         05  W-BELEVART          PIC  X(30)  VALUE SPACE.                 
024100                                                                          
024200     03  W-KDSEGKEY-X.                                                    
024300         05  W-KDSEGKEY          PIC  X(1)   VALUE SPACE.                 
024400                                                                          
024500     03  W-LOW-VALUE-X.                                                   
024600         05  W-LOW-VALUE         PIC  X(4)   VALUE SPACE.                 
024700                                                                          
024800     03  W-LOW-VALUE-2-X.                                                 
024900         05  W-LOW-VALUE-2       PIC X(26)   VALUE SPACE.                 
025000                                                                          
025100*    --- STATUS-KOD FRÅN IMS                                              
025200 01  STATUS-WS                   PIC XX.                                  
025300     88  SEGMENT-FINNS                       VALUE '  '.                  
025400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025600     SKIP2                                                                
025700 01  GODK-STATUSKODER.                                                    
025800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025900     SKIP3                                                                
026000 01  SSA1                        PIC X(96).                               
026100 01  SSA2                        PIC X(96).                               
026200 01  SSA3                        PIC X(64).                               
026300     EJECT                                                                
026400*    --- IMS FUNKTIONSKODER                                               
026500*01  -COPY W0003                                                          
026600     EJECT                                                                
026700                                                                          
026800*    ---  DLI INPUT-OUTPUT AREA                                           
026900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027000     SKIP3                                                                
027100 01  DLI-IO-AREA.                                                         
027200     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
027300     SKIP3                                                                
027400     03  WLARTC01 REDEFINES IO-AREA.                                      
027500*        05  -COPY WDK601                                                 
027600     EJECT                                                                
027700     03  WLARTC14 REDEFINES IO-AREA.                                      
027800*        05  -COPY WDK611                                                 
027900     EJECT                                                                
028000     03  WLXXBY11 REDEFINES IO-AREA.                                      
028100*        05  -COPY WDGX2234   -PRE XXBY11-                                
028200     EJECT                                                                
028300*    ---  DLI INPUT-OUTPUT AREA FÖR WLXXAZ (TILLKOMMANDE ART)             
028400 01  FILLER                      PIC X(16)   VALUE                        
028500                                             'DLI-IO-AREA-XXAZ'.          
028600     SKIP3                                                                
028700 01  DLI-IO-AREA-XXAZ.                                                    
028800     03  IO-AREA-XXAZ            PIC X(256)  VALUE SPACE.                 
028900     SKIP3                                                                
029000     03  WLXXAZ11 REDEFINES IO-AREA-XXAZ.                                 
029100*        05  -COPY WDGX1152   -PRE XXAZ11-                                
029200     SKIP3                                                                
029300     03  WLXXAZ21 REDEFINES IO-AREA-XXAZ.                                 
029400*        05  -COPY WDGX1154   -PRE XXAZ21-                                
029500     EJECT                                                                
029600                                                                          
029700*    ---  DLI INPUT-OUTPUT AREA FÖR OKONVERTERAD STRUKTUR                 
029800 01  FILLER                      PIC X(17)   VALUE                        
029900                                             'DLI-IO-AREA-OKONV'.         
030000     SKIP3                                                                
030100 01  DLI-IO-AREA-OKONV.                                                   
030200     03  IO-AREA-OKONV           PIC X(240)  VALUE SPACE.                 
030300     SKIP3                                                                
030400     03  WLSATB01 REDEFINES IO-AREA-OKONV.                                
030500*        05  -COPY WDJ101     -PRE SATB01O-                               
030600     EJECT                                                                
030700     03  WLSATB11 REDEFINES IO-AREA-OKONV.                                
030800*        05  -COPY WDJ111     -PRE SATB11O-                               
030900     SKIP3                                                                
031000*    ---  DLI INPUT-OUTPUT AREA FÖR KONVERTERAD STRUKTUR                  
031100 01  FILLER                      PIC X(16)   VALUE                        
031200                                             'DLI-IO-AREA-KONV'.          
031300     SKIP3                                                                
031400 01  DLI-IO-AREA-KONV.                                                    
031500     03  IO-AREA-KONV            PIC X(240)  VALUE SPACE.                 
031600     SKIP3                                                                
031700     03  WLSATB01 REDEFINES IO-AREA-KONV.                                 
031800*        05  -COPY WDJ101     -PRE SATB01K-                               
031900     EJECT                                                                
032000     03  WLSATB11 REDEFINES IO-AREA-KONV.                                 
032100*        05  -COPY WDJ111     -PRE SATB11K-                               
032200     SKIP3                                                                
032300     03  WLSATB22 REDEFINES IO-AREA-KONV.                                 
032400*        05  -COPY WDJ122     -PRE SATB22K-                               
032500     EJECT                                                                
032600 LINKAGE SECTION.                                                         
032700                                                                          
032800*01  -COPY W0009      -PRE MSG-                                           
032900     EJECT                                                                
033000*01  -COPY W0009      -PRE ALT-                                           
033100     EJECT                                                                
033200*01  -COPY W0008      -PRE SATB-O-                                        
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500*01  -COPY W0008      -PRE SATB-K-                                        
033600     05  FILLER                  PIC X.                                   
033700     EJECT                                                                
033800*01  -COPY W0008      -PRE SATB-D-                                        
033900     05  FILLER                  PIC X.                                   
034000     EJECT                                                                
034100*01  -COPY W0008      -PRE ARTC-                                          
034200     05  FILLER                  PIC X.                                   
034300     EJECT                                                                
034400*01  -COPY W0008      -PRE XXAZ-                                          
034500     05  FILLER                  PIC X.                                   
034600     EJECT                                                                
034700*01  -COPY W0008      -PRE XXBY-                                          
034800     05  FILLER                  PIC X.                                   
034900     EJECT                                                                
035000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB SATB-O-PCB SATB-K-PCB          
035100                           SATB-D-PCB ARTC-PCB                            
035200                           XXAZ-PCB XXBY-PCB.                             
035300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB SATB-O-PCB SATB-K-PCB          
035400                           SATB-D-PCB ARTC-PCB                            
035500                           XXAZ-PCB XXBY-PCB.                             
035600                                                                          
035700     PERFORM IMS-GET-MSG                                                  
035800     IF SEGMENT-FINNS                                                     
035900       PERFORM A-INIT                                                     
036000       IF GODK-MID                                                        
036100         PERFORM B-KOLLA-NYCKLAR                                          
036200         IF NYCKLAR-OK                                                    
036300           PERFORM C-UPPDATERA                                            
036400         END-IF                                                           
036500         IF KONVERTERADE-STRUK-FINNS-KVAR                                 
036600*********  STARTA OM PROGRAMMET IGEN                                      
036700           MOVE MID-W1I29401 TO MID-W10294                                
036800           PERFORM IMS-INSERT-ALT-MSG                                     
036900         ELSE                                                             
037000*********  VISA 1222-BILD                                                 
037100           PERFORM MFS-ROR-EJ-FAELT-IN                                    
037200           PERFORM MFS-ROR-EJ-FAELT-UT                                    
037300           MOVE MFS-RENSA-FAELT TO MOD-BORTTAG                            
037400                                   MOD-AANGRA                             
037500                                   MOD-KLAR                               
037600           MOVE INF-UPDATE-DONE TO MED-IDMFSFEL                           
037700           CALL WMEDKONV USING MED-WMEDAREA                               
037800           MOVE MED-MFSFEL     TO MOD-TEMFSFEL                            
037900           MOVE MFS-ADD-SAETT-CURSOR TO MOD-KLAR-ATTR                     
038000           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
038100           PERFORM IMS-INSERT-MSG                                         
038200         END-IF                                                           
038300       END-IF                                                             
038400     END-IF                                                               
038500                                                                          
038600     MOVE ZERO TO RETURN-CODE                                             
038700     GOBACK                                                               
038800     .                                                                    
038900     EJECT                                                                
039000 A-INIT SECTION.                                                          
039100                                                                          
039200     IF MSG-DUBBLA-TRANSKODER                                             
039300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I29401                 
039400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
039500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
039600     ELSE                                                                 
039700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I29401                  
039800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
039900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
040000     END-IF                                                               
040100                                                                          
040200     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
040300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
040400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
040500                                                                          
040600     MOVE LOW-VALUE  TO MSG-AREA                                          
040700     MOVE 'W1O22201' TO MFS-IDMOD                                         
040800     MOVE '1222'     TO MOD-IDTRANS                                       
040900     MOVE SPACE           TO MOD-TEMFSFEL                                 
041000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
041100                                                                          
041200     IF ENGLISH-TEXT                                                      
041300       MOVE +2    TO SPRAK-IX                                             
041400       MOVE 'GB ' TO MED-IDSKYLT                                          
041500     ELSE                                                                 
041600       MOVE +1    TO SPRAK-IX                                             
041700       MOVE 'S  ' TO MED-IDSKYLT                                          
041800     END-IF                                                               
041900                                                                          
042000     ACCEPT DAGENS-DATUM FROM DATE                                        
042100     .                                                                    
042200     EJECT                                                                
042300 B-KOLLA-NYCKLAR SECTION.                                                 
042400                                                                          
042500     MOVE JA TO NYCKLAR-SW                                                
042600                                                                          
042700     MOVE MID-IDLEVNR-UT      TO WS-IDLEVNR                               
042800     MOVE MID-BELEVART-UT     TO WS-BELEVART                              
042900     MOVE MID-IDARTNR-UT      TO WS-IDARTNR                               
043000     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
043100                                                                          
043200**** DESSA NYCKLAR ANVÄNDS EJ HÄR                                         
043300     MOVE MID-IDSKYLT-UT-DOLT   TO WS-IDSKYLT                             
043400     MOVE MID-1002-SATS-UT-DOLT TO WS-1002-SATS                           
043500     MOVE MID-KDPRODSL-UT-DOLT  TO WS-KDPRODSL                            
043600                                                                          
043700**** IF WS-IDLEVNR(1:1) NOT = ' ' AND '+' AND '0'                         
043800       IF WS-IDLEVNR NOT = SPACE                                          
043900****   OM LEVNR > SPACE, ÄR 'SÖKNYCKEL' IDLEVNR IHOP MED BELEVART         
044000         MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                      
044100         MOVE WS-IDLEVNR         TO W-IDLEVNR                             
044200         MOVE WS-BELEVART        TO W-BELEVART                            
044300         MOVE ZERO               TO W-IDARTNR                             
044400                                                                          
044500         MOVE ZERO               TO WS-IDARTNR                            
044600       ELSE                                                               
044700         IF WS-IDLEVNR = SPACE                                            
044800******   OM IDLEVNR = SPACE,  ÄR SÖKNYCKEL IDARTNR                        
044900           MOVE IDARTNR TO SOEKNYCKELTYP-SW                               
045000           IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                    
045100             MOVE WS-IDARTNR TO W-IDARTNR                                 
045200             MOVE SPACE      TO W-BELEVART                                
045300                                W-IDLEVNR                                 
045400                                WS-IDLEVNR                                
045500                                WS-BELEVART                               
045600           ELSE                                                           
045700             MOVE NEJ TO NYCKLAR-SW                                       
045800           END-IF                                                         
045900         ELSE                                                             
046000           MOVE NEJ TO NYCKLAR-SW                                         
046100         END-IF                                                           
046200       END-IF                                                             
046300***  ELSE                                                                 
046400***    MOVE NEJ                TO NYCKLAR-SW                              
046500***    MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                        
046600***  END-IF                                                               
046700                                                                          
046800     IF GODK-MID OR NYCKLAR-OK                                            
046900       MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                  
047000       IF SOEKNYCKEL-IDARTNR                                              
047100         MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                
047200         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
047300       ELSE                                                               
047400         MOVE WS-BELEVART TO MOD-BELEVART-UT                              
047500       END-IF                                                             
047600                                                                          
047700       MOVE WS-IDSKYLT   TO MOD-IDSKYLT-UT-DOLT                           
047800       MOVE WS-1002-SATS TO MOD-1002-SATS-UT-DOLT                         
047900       MOVE WS-KDPRODSL  TO MOD-KDPRODSL-UT-DOLT                          
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300 C-UPPDATERA SECTION.                                                     
048400                                                                          
048500     MOVE JA TO KONVERTERADE-STRUKT-KVAR-SW                               
048600                                                                          
048700     MOVE '1151'    TO W-IDHTYP                                           
048800     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
048900     PERFORM IMS-GET-XXAZ-XXAZ01                                          
049000                                                                          
049100     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
049200     MOVE WS-BELEVART       TO W-BELEVART                                 
049300     MOVE WS-IDARTNR        TO W-IDARTNR                                  
049400     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
049500     MOVE LOW-VALUE         TO W-LOW-VALUE                                
049600     PERFORM IMS-GET-XXAZ-XXAZ11                                          
049700                                                                          
049800     MOVE XXAZ11-1152-IDAO     TO WS-IDAO                                 
049900     MOVE XXAZ11-1152-TISTODAT TO WS-TISTODAT                             
050000     IF WS-TISTODAT = ZERO                                                
050100       MOVE NEJ TO TISTODAT-PAA-WDR5-IFYLLT-SW                            
050200     ELSE                                                                 
050300       MOVE JA  TO TISTODAT-PAA-WDR5-IFYLLT-SW                            
050400     END-IF                                                               
050500                                                                          
050600     MOVE +1 TO STRUKTUR-INDX                                             
050700     PERFORM IMS-GET-SATB01-DSEQ-UNIK                                     
050800     PERFORM UNTIL STRUKTUR-INDX > MAX-ANTAL-STRUKT-I-TAGET               
050900       IF SEGMENT-FINNS                                                   
051000         MOVE 001                  TO WORK-KDCALL                         
051100         MOVE WC-CDC-SE            TO WORK-IDDC                           
051200         MOVE SATB01K-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                   
051300         MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM                   
051400         CALL WORKDAY USING WORK-KDCALL                                   
051500                            WORK-DATE-AREA                                
051600                            WORK-KDSVAR                                   
051700         IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
051800*********  OM MAN TRÄFFAR PÅ EN KONV. STRUKTUR                            
051900*********  ÄLDRE ÄN 2 DAGAR TAS DEN BORT                                  
052000*********  ARB-KDSVAR-FEL INNEBÄR ATT ANTAL DAGAR < 999.                  
052100           MOVE SATB01K-STR-IDARTNR TO W-IDARTNR                          
052200           PERFORM IMS-GET-SATB-SATB01-KONV                               
052300           PERFORM IMS-DLET-SATB-KONV                                     
052400                                                                          
052500           PERFORM IMS-GET-SATB01-DSEQ-NEXT                               
052600         ELSE                                                             
052700           MOVE SATB01K-STR-IDARTNR TO WS-IDARTNR-KONV                    
052800           COMPUTE WS-IDARTNR-OKONV = 999999999 -                         
052900                                      WS-IDARTNR-KONV                     
053000                                                                          
053100           PERFORM S01-NOLLSTAELL-RADTABELL                               
053200           PERFORM S02-LAEGG-IN-RADER-I-TABELL                            
053300           PERFORM CA-UPPDATERA-KONV-STRUKTUR                             
053400           PERFORM CB-KOPIERA-KONV-STR-TILL-OKONV                         
053500           PERFORM IMS-GET-SATB01-DSEQ-NEXT                               
053600           IF SEGMENT-FINNS                                               
053700             ADD 1 TO STRUKTUR-INDX                                       
053800           END-IF                                                         
053900         END-IF                                                           
054000       ELSE                                                               
054100         MOVE +999 TO STRUKTUR-INDX                                       
054200         MOVE NEJ  TO KONVERTERADE-STRUKT-KVAR-SW                         
054300         PERFORM CC-TABORT-LAASNING-O-TILLK-ART                           
054400       END-IF                                                             
054500     END-PERFORM                                                          
054600                                                                          
054700     .                                                                    
054800     EJECT                                                                
054900 CA-UPPDATERA-KONV-STRUKTUR SECTION.                                      
055000                                                                          
055100     MOVE NEJ TO STRUKTUR-AER-1002-SATS-SW                                
055200                                                                          
055300     MOVE WS-IDARTNR-KONV TO W-IDARTNR-KONV                               
055400     PERFORM IMS-GET-SATB-SATB01-KONV                                     
055500                                                                          
055600     MOVE WS-IDARTNR-OKONV TO W-IDARTNR-OKONV                             
055700     PERFORM IMS-GET-SATB-SATB01-OKONV                                    
055800                                                                          
055900     IF SATB01O-STR-IDLEVNR = '1002 '                                     
056000       MOVE JA TO STRUKTUR-AER-1002-SATS-SW                               
056100     END-IF                                                               
056200                                                                          
056300     MOVE +10    TO WS-IDRADNR                                            
056400     MOVE +99999 TO WS-IDRADNR-FORPACKNING                                
056500     PERFORM IMS-GET-SATB-SATB11-OKONV                                    
056600     PERFORM UNTIL SEGMENT-SAKNAS                                         
056700       IF SEGMENT-FINNS                                                   
056800         MOVE SATB11O-RAD-KDISATS  TO STRUKTURRAD-OKONV-KDISATS-SW        
056900                                                                          
057000         MOVE SATB11O-RAD-KDSTRRAD TO W-KDSTRRAD-OKONV                    
057100                                      W-KDSTRRAD-KONV                     
057200                                      WS-KDSTRRAD                         
057300         MOVE SATB11O-RAD-IDRADNR  TO W-IDRADNR-OKONV                     
057400         MOVE WS-IDRADNR           TO W-IDRADNR-KONV                      
057500         PERFORM S03-KOLLA-OM-RAD-FINNS-TABELL                            
057600         IF RAD-FINNS-I-TABELL                                            
057700*********  RADEN SKALL BEHANDLAS                                          
057800           IF MID-BORTTAG = 'J'                                           
057900             PERFORM S04-TA-BORT-RAD                                      
058000           ELSE                                                           
058100             PERFORM S04-TA-BORT-RAD                                      
058200             PERFORM CAA-LAEGG-UPP-TILLK-ARTIKLAR                         
058300           END-IF                                                         
058400         ELSE                                                             
058500*********  KOPIERA RAD FRÅN 'RIKTIG' STRUKT TILL KONVERTERAD STRUK        
058600           PERFORM CAB-KOPIERA-RAD-O-BEH-O-NOT                            
058700           ADD 10 TO WS-IDRADNR                                           
058800         END-IF                                                           
058900         PERFORM IMS-GET-SATB-SATB11-OKONV                                
059000       END-IF                                                             
059100     END-PERFORM                                                          
059200     .                                                                    
059300     EJECT                                                                
059400 CAA-LAEGG-UPP-TILLK-ARTIKLAR SECTION.                                    
059500*************************************************                         
059600* HÄR LÄGGS TILLKOMMANDE ARTIKLAR FRÅN WDR5 UPP *                         
059700* PÅ DEN KONVERTERADE STRUKTUREN                *                         
059800*************************************************                         
059900                                                                          
060000     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
060100     MOVE WS-BELEVART       TO W-BELEVART                                 
060200     MOVE WS-IDARTNR        TO W-IDARTNR                                  
060300     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
060400     MOVE LOW-VALUE         TO W-LOW-VALUE                                
060500     PERFORM IMS-GET-XXAZ-XXAZ21-FIRST                                    
060600     PERFORM UNTIL SEGMENT-SAKNAS                                         
060700       IF SEGMENT-FINNS                                                   
060800                                                                          
060900         MOVE NEJ TO TILLK-ARTIKEL-FORPACKNING-SW                         
061000         IF XXAZ21-1154-IDARTNR > 0                                       
061100*********  TILLKOMMANDE ARTIKEL ÄR ETT ARTIKELNR                          
061200           PERFORM CAAA-KOLLA-OM-ART-FORPACKNING                          
061300         END-IF                                                           
061400                                                                          
061500         IF TILLK-ARTIKEL-FORPACKNING                                     
061600           MOVE '9'                    TO SATB11K-RAD-KDSTRRAD            
061700                                          W-KDSTRRAD-KONV                 
061800           MOVE WS-IDRADNR-FORPACKNING TO SATB11K-RAD-IDRADNR             
061900                                          W-IDRADNR-KONV                  
062000         ELSE                                                             
062100           MOVE '0'                 TO SATB11K-RAD-KDSTRRAD               
062200                                          W-KDSTRRAD-KONV                 
062300           MOVE WS-IDRADNR          TO SATB11K-RAD-IDRADNR                
062400                                          W-IDRADNR-KONV                  
062500         END-IF                                                           
062600         MOVE XXAZ21-1154-IDLEVNR   TO SATB11K-RAD-IDLEVNR                
062700         MOVE XXAZ21-1154-BELEVART  TO SATB11K-RAD-BELEVART               
062800         MOVE XXAZ21-1154-IDARTNR   TO SATB11K-RAD-IDARTNR                
062900         MOVE XXAZ21-1154-BEART-SVE TO SATB11K-RAD-BEART-SVE              
063000         MOVE WS-IDAO               TO SATB11K-RAD-IDAO-STA               
063100         MOVE SPACE                 TO SATB11K-RAD-IDAO-STO               
063200         MOVE XXAZ21-1154-IDSTRTYP  TO SATB11K-RAD-IDSTRTYP               
063300         MOVE XXAZ21-1154-KDHOM     TO SATB11K-RAD-KDBENHOM               
063400         IF STRUKTURRAD-KONV-AER-E-MAERKT                                 
063500********* OM DEN KONVERTERADE RADEN ÄR E-MÄRKT                            
063600********* TILLKOMMANDE ARTIKLAR FÅR KDISATS = T                           
063700           MOVE 'T'                 TO SATB11K-RAD-KDISATS                
063800         ELSE                                                             
063900********* TILLKOMMANDE ARTIKLAR FÅR KDISATS = N                           
064000           MOVE 'N'                 TO SATB11K-RAD-KDISATS                
064100         END-IF                                                           
064200         MOVE XXAZ21-1154-KDSORT    TO SATB11K-RAD-KDSORT                 
064300         MOVE XXAZ21-1154-REANTPSA  TO SATB11K-RAD-REANTPSA               
064400         MOVE DAGENS-DATUM          TO SATB11K-RAD-TIREGDAT               
064500         MOVE WS-TISTODAT           TO SATB11K-RAD-TISTADAT               
064600         MOVE +999999               TO SATB11K-RAD-TISTODAT               
064700         PERFORM IMS-ISRT-SATB-SATB11-KONV                                
064800                                                                          
064900         IF (XXAZ21-1154-TESTRNOT(1) = SPACE) AND                         
065000            (XXAZ21-1154-TESTRNOT(2) = SPACE)                             
065100           CONTINUE                                                       
065200         ELSE                                                             
065300           MOVE '1'                     TO SATB22K-NOT-IDSTRNOT           
065400           MOVE XXAZ21-1154-TESTRNOT(1) TO                                
065500                                         SATB22K-NOT-TESTRNOT(1)          
065600           MOVE XXAZ21-1154-TESTRNOT(2) TO                                
065700                                         SATB22K-NOT-TESTRNOT(2)          
065800           PERFORM IMS-ISRT-SATB-SATB22-KONV                              
065900         END-IF                                                           
066000                                                                          
066100         IF TILLK-ARTIKEL-FORPACKNING                                     
066200           SUBTRACT 1 FROM WS-IDRADNR-FORPACKNING                         
066300         ELSE                                                             
066400           ADD 10 TO WS-IDRADNR                                           
066500         END-IF                                                           
066600                                                                          
066700         IF STRUKTUR-AER-1002-SATS                                        
066800           PERFORM CAAB-SKICKA-2233-TRANS                                 
066900           PERFORM CAAC-UPPDATERA-FLIART-WDK601                           
067000         END-IF                                                           
067100                                                                          
067200         MOVE WS-IDLEVNR        TO W-IDLEVNR                              
067300         MOVE WS-BELEVART       TO W-BELEVART                             
067400         MOVE WS-IDARTNR        TO W-IDARTNR                              
067500         MOVE MSG-SIGNON-USERID TO W-IDUSER                               
067600         MOVE LOW-VALUE         TO W-LOW-VALUE                            
067700         PERFORM IMS-GET-XXAZ-XXAZ21                                      
067800       END-IF                                                             
067900     END-PERFORM                                                          
068000     .                                                                    
068100     SKIP3                                                                
068200 CAAA-KOLLA-OM-ART-FORPACKNING SECTION.                                   
068300                                                                          
068400     MOVE XXAZ21-1154-IDARTNR TO W-IDARTNR                                
068500     PERFORM IMS-GET-ARTC-ARTC01                                          
068600     IF SEGMENT-FINNS                                                     
068700       MOVE ART-KDPRODSL         TO TEST-KDPRODSL                         
068800       IF KDPRODSL-VOLVO-EMB                                              
068900         MOVE JA TO TILLK-ARTIKEL-FORPACKNING-SW                          
069000       END-IF                                                             
069100     END-IF                                                               
069200     .                                                                    
069300     SKIP3                                                                
069400 CAAB-SKICKA-2233-TRANS SECTION.                                          
069500                                                                          
069600     IF STRUKTURRAD-KONV-AER-E-MAERKT                                     
069700********* TILLKOMMANDE ARTIKLAR HAR FÅTT KDISATS = T                      
069800        MOVE XXAZ21-1154-IDARTNR TO XXBY11-2234-IDARTNR-ING               
069900        MOVE WS-IDARTNR-OKONV    TO XXBY11-2234-IDARTNR-SATS              
070000        MOVE ZERO                TO XXBY11-2234-KVPB-SEP-TOT              
070100                                    XXBY11-2234-REANTPSA-NY               
070200                                    XXBY11-2234-REANTPSA-GAMMAL           
070300                                    XXBY11-2234-TIBEHDAT                  
070400        MOVE SPACE               TO XXBY11-2234-KDISATS                   
070500      ELSE                                                                
070600        MOVE XXAZ21-1154-IDARTNR  TO XXBY11-2234-IDARTNR-ING              
070700        MOVE WS-IDARTNR-OKONV     TO XXBY11-2234-IDARTNR-SATS             
070800        MOVE ZERO                 TO XXBY11-2234-KVPB-SEP-TOT             
070900                                     XXBY11-2234-REANTPSA-GAMMAL          
071000        MOVE XXAZ21-1154-REANTPSA TO XXBY11-2234-REANTPSA-NY              
071100        MOVE 'N'                  TO XXBY11-2234-KDISATS                  
071200                                                                          
071300        MOVE 'AAMMDD'    TO DAT-KDDATFORM                                 
071400        MOVE WS-TISTODAT TO DAT-I-TIDATUM                                 
071500        CALL WDATKONV USING DAT-KDDATFORM                                 
071600                            DAT-I-TIDATUM                                 
071700                            DAT-O-TIDATUM                                 
071800                            DAT-KDSVAR                                    
071900        MOVE DAT-TIAAVV-GRP TO WS-TIAAVV                                  
072000        MOVE WS-TIAAVV      TO XXBY11-2234-TIBEHDAT                       
072100      END-IF                                                              
072200                                                                          
072300      MOVE '2233'    TO W-IDHTYP                                          
072400      MOVE LOW-VALUE TO W-LOW-VALUE-2                                     
072500      PERFORM IMS-ISRT-XXBY-XXBY11                                        
072600     .                                                                    
072700     SKIP2                                                                
072800 CAAC-UPPDATERA-FLIART-WDK601 SECTION.                                    
072900********************************************************                  
073000* HÄR UPPDATERAS FLIART PÅ WDK601, VILKEN TALAR OM ATT *                  
073100* EN ARTIKEL INGÅR I EN 1002-SATS.                     *                  
073200********************************************************                  
073300                                                                          
073400     MOVE XXAZ21-1154-IDARTNR TO W-IDARTNR                                
073500     PERFORM IMS-GET-ARTC-ARTC01                                          
073600     IF SEGMENT-FINNS                                                     
073700       IF ART-FLIART = 'J'                                                
073800         CONTINUE                                                         
073900       ELSE                                                               
074000         MOVE 'J' TO ART-FLIART                                           
074100         PERFORM IMS-REPL-ARTC                                            
074200       END-IF                                                             
074300     END-IF                                                               
074400     .                                                                    
074500     SKIP2                                                                
074600 CAB-KOPIERA-RAD-O-BEH-O-NOT SECTION.                                     
074700********************************************************                  
074800* HÄR KOPIERAS RADEN OCH BEHOVS- OCH NOTERINGSSEGMENT  *                  
074900* TILL DEN KONVERTERADE STRUKTUREN.                    *                  
075000********************************************************                  
075100                                                                          
075200     MOVE IO-AREA-OKONV TO IO-AREA-KONV                                   
075300     MOVE WS-IDRADNR    TO SATB11K-RAD-IDRADNR                            
075400     PERFORM IMS-ISRT-SATB-SATB11-KONV                                    
075500                                                                          
075600     PERFORM S06-KOPIERA-NOTERINGSSEGMENT                                 
075700     .                                                                    
075800     EJECT                                                                
075900 CB-KOPIERA-KONV-STR-TILL-OKONV SECTION.                                  
076000**************************************************************            
076100* HÄR KOPIERAS DEN KONVERTERADE STRUKTURERN TILL DEN RIKTIGA *            
076200* STRUKTUREN (MED OMNUMRERING AV EV. FÖRPACKNINGSRADER )     *            
076300**************************************************************            
076400                                                                          
076500     MOVE ZERO TO WS-IDRADNR                                              
076600                                                                          
076700     PERFORM IMS-GET-SATB-SATB01-OKONV                                    
076800     PERFORM IMS-DLET-SATB-OKONV                                          
076900                                                                          
077000     MOVE DAGENS-DATUM TO SATB01O-STR-TIUPPDAT                            
077100     IF SATB01O-STR-IDSTRTYP = 'S'                                        
077200       MOVE WS-IDARTNR-OKONV TO W-IDARTNR                                 
077300       PERFORM IMS-GET-ARTC-ARTC01                                        
077400       IF SEGMENT-FINNS                                                   
077500         IF SATB01O-STR-IDLEVNR = '1002 '                                 
077600            MOVE 'J' TO SATB01O-STR-FLFORPQ                               
077700         ELSE                                                             
077800            PERFORM IMS-GET-ARTC11                                        
077900            IF SEGMENT-FINNS                                              
078000               MOVE CLAG-BEFT TO WS-BEFT-AKTUELL                          
078100               IF BEFT-AKTUELL                                            
078200                  MOVE 'J' TO SATB01O-STR-FLFORPQ                         
078300               END-IF                                                     
078400            END-IF                                                        
078500          END-IF                                                          
078600       END-IF                                                             
078700     END-IF                                                               
078800                                                                          
078900     PERFORM IMS-ISRT-SATB-SATB01-OKONV                                   
079000                                                                          
079100     PERFORM IMS-GET-SATB-SATB01-KONV                                     
079200     PERFORM IMS-GET-SATB-SATB11-KONV                                     
079300     PERFORM UNTIL SEGMENT-SAKNAS                                         
079400       IF SEGMENT-FINNS                                                   
079500         IF SATB11K-RAD-KDSTRRAD = SPACE                                  
079600*********  INNEBÄR ATT DET ÄR EN RAD SOM VALTS FRÅN                       
079700*********  BILD 1221 OCH BEHANDLATS (BORTTAG/BYTE) I DETTA PGM,           
079800*********  D.V.S DE FINNS REDAN I UPPDATERAD FORM I STRUKTUREN.           
079900*********  SÅDANA RADER LIGGER KVAR I DEN KONV. STRUKTUREN                
080000*********  MEN SKALL EJ KOPIERAS TILL DEN RIKTIGA STRUKTUTREN.            
080100           PERFORM IMS-GET-SATB-SATB11-KONV                               
080200         ELSE                                                             
080300           IF SATB11K-RAD-KDSTRRAD = '9'                                  
080400***********  ÄNDRA RADNUMMER                                              
080500             MOVE SATB11K-RAD-KDSTRRAD TO W-KDSTRRAD-KONV                 
080600                                          W-KDSTRRAD-OKONV                
080700             MOVE SATB11K-RAD-IDRADNR  TO W-IDRADNR-KONV                  
080800                                                                          
080900             MOVE IO-AREA-KONV TO IO-AREA-OKONV                           
081000             MOVE WS-IDRADNR-FORPACKNING TO SATB11O-RAD-IDRADNR           
081100                                            W-IDRADNR-OKONV               
081200             ADD 10 TO WS-IDRADNR-FORPACKNING                             
081300           ELSE                                                           
081400             MOVE SATB11K-RAD-KDSTRRAD TO W-KDSTRRAD-KONV                 
081500                                          W-KDSTRRAD-OKONV                
081600             MOVE SATB11K-RAD-IDRADNR  TO W-IDRADNR-KONV                  
081700                                          W-IDRADNR-OKONV                 
081800             MOVE IO-AREA-KONV TO IO-AREA-OKONV                           
081900************ NÄSTA RADNR SPARAS UNDAN FÖR ATT KUNNA NUMRERA               
082000************ OM EV. FÖRPACKNINGSRADER                                     
082100             MOVE ZERO TO WS-IDRADNR-FORPACKNING                          
082200             COMPUTE WS-IDRADNR-FORPACKNING =                             
082300                                     SATB11K-RAD-IDRADNR + 10             
082400           END-IF                                                         
082500                                                                          
082600           PERFORM IMS-ISRT-SATB-SATB11-OKONV                             
082700                                                                          
082800           PERFORM IMS-GET-SATB-SATB22-KONV                               
082900           PERFORM UNTIL SEGMENT-SAKNAS                                   
083000             IF SEGMENT-FINNS                                             
083100               MOVE IO-AREA-KONV TO IO-AREA-OKONV                         
083200               PERFORM IMS-ISRT-SATB-SATB22-OKONV                         
083300               PERFORM IMS-GET-SATB-SATB22-KONV                           
083400             END-IF                                                       
083500           END-PERFORM                                                    
083600                                                                          
083700           PERFORM IMS-GET-SATB-SATB11-KONV                               
083800         END-IF                                                           
083900       END-IF                                                             
084000     END-PERFORM                                                          
084100                                                                          
084200     PERFORM IMS-GET-SATB-SATB01-KONV                                     
084300     PERFORM IMS-DLET-SATB-KONV                                           
084400     .                                                                    
084500     SKIP3                                                                
084600 CC-TABORT-LAASNING-O-TILLK-ART SECTION.                                  
084700********************************************************                  
084800* HÄR TAS LÅSNINGEN BORT PÅ ARTIKELN OCH DÄRMED OCKSÅ  *                  
084900* DE EV. TILLKOMMANDE ARTIKLARNA.                      *                  
085000********************************************************                  
085100                                                                          
085200     MOVE '1151'    TO W-IDHTYP                                           
085300     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
085400     PERFORM IMS-GET-XXAZ-XXAZ01                                          
085500                                                                          
085600     MOVE WS-IDLEVNR        TO W-IDLEVNR                                  
085700     MOVE WS-BELEVART       TO W-BELEVART                                 
085800     MOVE WS-IDARTNR        TO W-IDARTNR                                  
085900     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
086000     MOVE LOW-VALUE         TO W-LOW-VALUE                                
086100     PERFORM IMS-GET-XXAZ-XXAZ11                                          
086200     PERFORM IMS-DLET-XXAZ                                                
086300     .                                                                    
086400     EJECT                                                                
086500 S01-NOLLSTAELL-RADTABELL SECTION.                                        
086600                                                                          
086700     MOVE +1 TO INDX                                                      
086800     PERFORM UNTIL INDX > MAX-TABELL-LAENGD                               
086900       MOVE ZERO TO IDRADNR(INDX)                                         
087000       ADD 1 TO INDX                                                      
087100     END-PERFORM                                                          
087200     .                                                                    
087300     EJECT                                                                
087400 S02-LAEGG-IN-RADER-I-TABELL SECTION.                                     
087500********************************************************                  
087600* HÄR LÄGGS DE RADER SOM SKALL BEHANDLAS (BYTAS UT/TAS *                  
087700* BORT) I PÅTRÄFFAD KONV. STRUKTUR IN I EN TABELL      *                  
087800********************************************************                  
087900                                                                          
088000     MOVE +1 TO INDX                                                      
088100                                                                          
088200     PERFORM IMS-GET-SATB11-DSEQ                                          
088300     PERFORM UNTIL SEGMENT-SAKNAS                                         
088400       IF SEGMENT-FINNS                                                   
088500         MOVE SATB11K-RAD-IDRADNR TO IDRADNR(INDX)                        
088600         ADD 1 TO INDX                                                    
088700         PERFORM IMS-GET-SATB11-DSEQ                                      
088800       END-IF                                                             
088900     END-PERFORM                                                          
089000     .                                                                    
089100     EJECT                                                                
089200 S03-KOLLA-OM-RAD-FINNS-TABELL SECTION.                                   
089300*****************************************************                     
089400*  KONTROLL OM RAD ÄR EN AV DE SOM SKALL BEHANDLAS  *                     
089500*****************************************************                     
089600                                                                          
089700     MOVE NEJ TO RAD-FINNS-I-TABELL-SW                                    
089800                                                                          
089900     MOVE +1 TO INDX                                                      
090000     PERFORM UNTIL INDX > MAX-TABELL-LAENGD                               
090100       IF SATB11O-RAD-IDRADNR = IDRADNR(INDX)                             
090200         MOVE JA   TO RAD-FINNS-I-TABELL-SW                               
090300         MOVE +999 TO INDX                                                
090400       ELSE                                                               
090500         IF IDRADNR(INDX) = ZERO                                          
090600           MOVE +999 TO INDX                                              
090700         ELSE                                                             
090800           ADD 1 TO INDX                                                  
090900         END-IF                                                           
091000       END-IF                                                             
091100     END-PERFORM                                                          
091200     .                                                                    
091300     EJECT                                                                
091400 S04-TA-BORT-RAD SECTION.                                                 
091500**********************************************************                
091600* BORTTAG AV RAD. RADEN KOPIERAS  TILL DEN KONVERTERADE  *                
091700* STRUKTUREN OCH U- ELLER E-MÄRKS                        *                
091800**********************************************************                
091900                                                                          
092000       IF TISTODAT-PAA-WDR5-EJ-IFYLLT                                     
092100*******  OM MAN EJ SATT TISTODAT PÅ WDR5 (KAN ENDAST INTRÄFFA NÄR         
092200*******  DEN BEHANDLADE ARTIKELN, NYCKELN, ALTERNATIVT ERSATT DVS         
092300*******  RAD E-MÄRKT) SÄTTS TISTODAT TILL KOPIERD RADS TISTODAT           
092400         MOVE SATB11O-RAD-TISTODAT TO WS-TISTODAT                         
092500         MOVE SATB11O-RAD-IDAO-STO TO WS-IDAO                             
092600       ELSE                                                               
092700         MOVE 'U'                 TO STRUKTURRAD-OKONV-KDISATS-SW         
092800*******  SÄTTS FÖR ATT OM KDISATS = 'E' OCH DATUM IFYLLT SKALL            
092900*******  ARTIKELN U-MÄRKAS                                                
093000         IF STRUKTUR-AER-1002-SATS                                        
093100*******    SKICKA 2233-TRANS                                              
093200           MOVE SATB11O-RAD-IDARTNR TO XXBY11-2234-IDARTNR-ING            
093300           MOVE WS-IDARTNR-OKONV  TO XXBY11-2234-IDARTNR-SATS             
093400           MOVE ZERO              TO XXBY11-2234-KVPB-SEP-TOT             
093500           MOVE 'U'               TO XXBY11-2234-KDISATS                  
093600           MOVE ZERO              TO XXBY11-2234-REANTPSA-NY              
093700           IF SATB11O-RAD-KDISATS = 'U'                                   
093800             MOVE ZERO            TO XXBY11-2234-REANTPSA-GAMMAL          
093900           ELSE                                                           
094000             MOVE SATB11O-RAD-REANTPSA TO                                 
094100                                     XXBY11-2234-REANTPSA-GAMMAL          
094200           END-IF                                                         
094300           MOVE 'AAMMDD'    TO DAT-KDDATFORM                              
094400           MOVE WS-TISTODAT TO DAT-I-TIDATUM                              
094500           CALL WDATKONV USING DAT-KDDATFORM                              
094600                               DAT-I-TIDATUM                              
094700                               DAT-O-TIDATUM                              
094800                               DAT-KDSVAR                                 
094900           MOVE DAT-TIAAVV-GRP TO WS-TIAAVV                               
095000           MOVE WS-TIAAVV      TO XXBY11-2234-TIBEHDAT                    
095100           MOVE '2233'         TO W-IDHTYP                                
095200           MOVE LOW-VALUE      TO W-LOW-VALUE-2                           
095300           PERFORM IMS-ISRT-XXBY-XXBY11                                   
095400         END-IF                                                           
095500       END-IF                                                             
095600                                                                          
095700       MOVE IO-AREA-OKONV TO IO-AREA-KONV                                 
095800       IF STRUKTURRAD-OKONV-AER-E-MAERKT                                  
095900*******  RADEN FÖRBLIR E-MÄRKT                                            
096000         MOVE 'E'       TO STRUKTURRAD-KONV-KDISATS-SW                    
096100       ELSE                                                               
096200*******  RADEN U-MÄRKS (UTGÅR)                                            
096300         MOVE 'U'       TO SATB11K-RAD-KDISATS                            
096400                           STRUKTURRAD-KONV-KDISATS-SW                    
096500       END-IF                                                             
096600       MOVE WS-IDAO     TO SATB11K-RAD-IDAO-STO                           
096700       MOVE WS-TISTODAT TO SATB11K-RAD-TISTODAT                           
096800       MOVE WS-IDRADNR  TO SATB11K-RAD-IDRADNR                            
096900       PERFORM IMS-ISRT-SATB-SATB11-KONV                                  
097000                                                                          
097100       PERFORM S06-KOPIERA-NOTERINGSSEGMENT                               
097200       ADD 10 TO WS-IDRADNR                                               
097300     .                                                                    
097400     EJECT                                                                
097500 S06-KOPIERA-NOTERINGSSEGMENT SECTION.                                    
097600*******************************************************                   
097700*  KOPIERING AV NOT-SEGMENT TILL KONVERTERAD STRUKTUR *                   
097800*******************************************************                   
097900                                                                          
098000     PERFORM IMS-GET-SATB-SATB22-OKONV                                    
098100     PERFORM UNTIL SEGMENT-SAKNAS                                         
098200       IF SEGMENT-FINNS                                                   
098300         MOVE IO-AREA-OKONV TO IO-AREA-KONV                               
098400         PERFORM IMS-ISRT-SATB-SATB22-KONV                                
098500         PERFORM IMS-GET-SATB-SATB22-OKONV                                
098600       END-IF                                                             
098700     END-PERFORM                                                          
098800     .                                                                    
098900     EJECT                                                                
099000 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
099100                                                                          
099200*    --- ALLA UTDATA-FÄLT                                                 
099300*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
099400     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-UTG-ART                          
099500                               MOD-ANTAL-SEGMENT-ENTER                    
099600                               MOD-ANTAL-SEGMENT-NEXT                     
099700                               MOD-IDAO                                   
099800                               MOD-TIAAVV                                 
099900                                                                          
100000     .                                                                    
100100     SKIP2                                                                
100200 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
100300                                                                          
100400*    --- ALLA INDATA-FÄLT                                                 
100500     MOVE MFS-ROER-EJ-FAELT TO MOD-AANGRA                                 
100600                               MOD-IDAO                                   
100700                               MOD-BORTTAG                                
100800                               MOD-TIAAVV                                 
100900                               MOD-KLAR                                   
101000     MOVE +1 TO INDX                                                      
101100     PERFORM UNTIL INDX > MAX-INDX                                        
101200       PERFORM MFS-ROR-EJ-RAD-FAELT-IN                                    
101300       ADD 1 TO INDX                                                      
101400     END-PERFORM                                                          
101500     .                                                                    
101600     SKIP3                                                                
101700 MFS-ROR-EJ-RAD-FAELT-IN  SECTION.                                        
101800                                                                          
101900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR (INDX)                       
102000                                 MOD-BELEVART (INDX)                      
102100                                 MOD-REANTPSA (INDX)                      
102200                                 MOD-KDSORT (INDX)                        
102300                                 MOD-BEART (INDX)                         
102400                                 MOD-KDBENHOM (INDX)                      
102500                                 MOD-IDSTRTYP (INDX)                      
102600                                 MOD-TESTRNOT (INDX, 1)                   
102700                                 MOD-TESTRNOT (INDX, 2)                   
102800     .                                                                    
102900     EJECT                                                                
103000* --- IMS SEKTIONER ---                                                   
103100     SKIP3                                                                
103200 IMS-GET-MSG SECTION.                                                     
103300                                                                          
103400     MOVE '  QC' TO GODK-STATUSKODER                                      
103500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
103600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103700     PERFORM IMS-STATUSKONTROLL                                           
103800     .                                                                    
103900     SKIP3                                                                
104000 IMS-INSERT-MSG SECTION.                                                  
104100                                                                          
104200     IF ENGLISH-TEXT                                                      
104300       MOVE 'N' TO MFS-KDHUVOMR                                           
104400     END-IF                                                               
104500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
104600     MOVE SPACE TO GODK-STATUSKODER                                       
104700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
104800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
104900     PERFORM IMS-STATUSKONTROLL                                           
105000     .                                                                    
105100     SKIP3                                                                
105200 IMS-INSERT-ALT-MSG SECTION.                                              
105300                                                                          
105400     MOVE SPACE TO GODK-STATUSKODER                                       
105500     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
105600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
105700     PERFORM IMS-STATUSKONTROLL                                           
105800     .                                                                    
105900     EJECT                                                                
106000**** IMS-ANROP MOT WDJ1 FÖR DEN 'RIKTIGA' STRUKTUREN                      
106100                                                                          
106200 IMS-DLET-SATB-OKONV SECTION.                                             
106300                                                                          
106400     MOVE '    ' TO GODK-STATUSKODER                                      
106500     CALL CBLTDLI USING DLET SATB-O-PCB DLI-IO-AREA-OKONV                 
106600     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
106700     PERFORM IMS-STATUSKONTROLL                                           
106800     .                                                                    
106900     SKIP3                                                                
107000 IMS-GET-SATB-SATB01-OKONV SECTION.                                       
107100                                                                          
107200     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-OKONV-X ')'                   
107300          DELIMITED BY SIZE INTO SSA1                                     
107400     MOVE '  GE' TO GODK-STATUSKODER                                      
107500     CALL CBLTDLI USING GHU SATB-O-PCB DLI-IO-AREA-OKONV SSA1             
107600     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
107700     PERFORM IMS-STATUSKONTROLL                                           
107800     .                                                                    
107900     SKIP3                                                                
108000 IMS-GET-SATB-SATB11-OKONV SECTION.                                       
108100                                                                          
108200     MOVE 'WLSATB11 ' TO SSA1                                             
108300     MOVE '  GE' TO GODK-STATUSKODER                                      
108400     CALL CBLTDLI USING GHNP SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
108500     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
108600     PERFORM IMS-STATUSKONTROLL                                           
108700     .                                                                    
108800     SKIP3                                                                
108900 IMS-GET-SATB-SATB22-OKONV SECTION.                                       
109000                                                                          
109100     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-OKONV-X ')'                  
109200          DELIMITED BY SIZE INTO SSA1                                     
109300     MOVE 'WLSATB22 ' TO SSA2                                             
109400     MOVE '  GE' TO GODK-STATUSKODER                                      
109500     CALL CBLTDLI USING GHNP SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
109600                                                          SSA2            
109700     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
109800     PERFORM IMS-STATUSKONTROLL                                           
109900     .                                                                    
110000     SKIP3                                                                
110100 IMS-ISRT-SATB-SATB01-OKONV SECTION.                                      
110200                                                                          
110300     MOVE 'WLSATB01 ' TO SSA1                                             
110400     MOVE '     '     TO GODK-STATUSKODER                                 
110500     CALL CBLTDLI USING ISRT SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
110600     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
110700     PERFORM IMS-STATUSKONTROLL                                           
110800     .                                                                    
110900     SKIP3                                                                
111000 IMS-ISRT-SATB-SATB11-OKONV SECTION.                                      
111100                                                                          
111200     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-OKONV-X ')'                   
111300          DELIMITED BY SIZE INTO SSA1                                     
111400     MOVE 'WLSATB11 ' TO SSA2                                             
111500     MOVE '    '      TO GODK-STATUSKODER                                 
111600     CALL CBLTDLI USING ISRT SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
111700                                                         SSA2             
111800     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
111900     PERFORM IMS-STATUSKONTROLL                                           
112000     .                                                                    
112100     SKIP3                                                                
112200 IMS-ISRT-SATB-SATB22-OKONV SECTION.                                      
112300                                                                          
112400     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-OKONV-X ')'                   
112500          DELIMITED BY SIZE INTO SSA1                                     
112600     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-OKONV-X ')'                  
112700          DELIMITED BY SIZE INTO SSA2                                     
112800     MOVE 'WLSATB22 ' TO SSA3                                             
112900     MOVE '  GE' TO GODK-STATUSKODER                                      
113000     CALL CBLTDLI USING ISRT SATB-O-PCB DLI-IO-AREA-OKONV SSA1            
113100                                                          SSA2            
113200                                                          SSA3            
113300     MOVE SATB-O-STATUS-CODE TO STATUS-WS                                 
113400     PERFORM IMS-STATUSKONTROLL                                           
113500     .                                                                    
113600     EJECT                                                                
113700                                                                          
113800**** IMS-ANROP MOT WDJ1 FÖR DEN KONVERTERTADE STRUKTUREN                  
113900                                                                          
114000 IMS-GET-SATB01-DSEQ-UNIK SECTION.                                        
114100**** OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                         
114200                                                                          
114300     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
114400                                  W-MIN-IDARTNR-KONV-X                    
114500                    '&WDJ1DSEQ<=' W-IDUSER-X                              
114600                                  W-MAX-IDARTNR-KONV-X ')'                
114700          DELIMITED BY SIZE INTO SSA1                                     
114800     MOVE '  GE' TO GODK-STATUSKODER                                      
114900     CALL CBLTDLI USING GU SATB-D-PCB DLI-IO-AREA-KONV SSA1               
115000     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
115100     PERFORM IMS-STATUSKONTROLL                                           
115200     .                                                                    
115300     SKIP3                                                                
115400 IMS-GET-SATB01-DSEQ-NEXT SECTION.                                        
115500**** OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                         
115600                                                                          
115700     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
115800                                  W-MIN-IDARTNR-KONV-X                    
115900                    '&WDJ1DSEQ<=' W-IDUSER-X                              
116000                                  W-MAX-IDARTNR-KONV-X ')'                
116100          DELIMITED BY SIZE INTO SSA1                                     
116200     MOVE '  GE' TO GODK-STATUSKODER                                      
116300     CALL CBLTDLI USING GN SATB-D-PCB DLI-IO-AREA-KONV SSA1               
116400     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
116500     PERFORM IMS-STATUSKONTROLL                                           
116600     .                                                                    
116700     SKIP3                                                                
116800 IMS-GET-SATB11-DSEQ SECTION.                                             
116900                                                                          
117000     MOVE 'WLSATB11 ' TO SSA1                                             
117100     MOVE '  GE' TO GODK-STATUSKODER                                      
117200     CALL CBLTDLI USING GNP SATB-D-PCB DLI-IO-AREA-KONV SSA1              
117300     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
117400     PERFORM IMS-STATUSKONTROLL                                           
117500     .                                                                    
117600     SKIP3                                                                
117700 IMS-DLET-SATB-KONV SECTION.                                              
117800                                                                          
117900     MOVE '    ' TO GODK-STATUSKODER                                      
118000     CALL CBLTDLI USING DLET SATB-K-PCB DLI-IO-AREA-KONV                  
118100     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
118200     PERFORM IMS-STATUSKONTROLL                                           
118300     .                                                                    
118400     SKIP3                                                                
118500 IMS-GET-SATB-SATB01-KONV SECTION.                                        
118600                                                                          
118700     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-KONV-X ')'                    
118800          DELIMITED BY SIZE INTO SSA1                                     
118900     MOVE '  GE' TO GODK-STATUSKODER                                      
119000     CALL CBLTDLI USING GHU SATB-K-PCB DLI-IO-AREA-KONV SSA1              
119100     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
119200     PERFORM IMS-STATUSKONTROLL                                           
119300     .                                                                    
119400     SKIP3                                                                
119500 IMS-GET-SATB-SATB11-KONV SECTION.                                        
119600                                                                          
119700     MOVE 'WLSATB11 ' TO SSA1                                             
119800     MOVE '  GE' TO GODK-STATUSKODER                                      
119900     CALL CBLTDLI USING GHNP SATB-K-PCB DLI-IO-AREA-KONV SSA1             
120000     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
120100     PERFORM IMS-STATUSKONTROLL                                           
120200     .                                                                    
120300     SKIP3                                                                
120400 IMS-GET-SATB-SATB22-KONV SECTION.                                        
120500                                                                          
120600     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-KONV-X ')'                   
120700          DELIMITED BY SIZE INTO SSA1                                     
120800     MOVE 'WLSATB22 ' TO SSA2                                             
120900     MOVE '  GE' TO GODK-STATUSKODER                                      
121000     CALL CBLTDLI USING GHNP SATB-K-PCB DLI-IO-AREA-KONV SSA1             
121100                                                         SSA2             
121200     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
121300     PERFORM IMS-STATUSKONTROLL                                           
121400     .                                                                    
121500     SKIP3                                                                
121600 IMS-ISRT-SATB-SATB11-KONV SECTION.                                       
121700                                                                          
121800     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-KONV-X ')'                    
121900          DELIMITED BY SIZE INTO SSA1                                     
122000     MOVE 'WLSATB11 ' TO SSA2                                             
122100     MOVE '    '      TO GODK-STATUSKODER                                 
122200     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-KONV SSA1             
122300                                                         SSA2             
122400     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
122500     PERFORM IMS-STATUSKONTROLL                                           
122600     .                                                                    
122700     SKIP3                                                                
122800 IMS-ISRT-SATB-SATB22-KONV SECTION.                                       
122900                                                                          
123000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-KONV-X ')'                    
123100          DELIMITED BY SIZE INTO SSA1                                     
123200     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-KONV-X ')'                   
123300          DELIMITED BY SIZE INTO SSA2                                     
123400     MOVE 'WLSATB22 ' TO SSA3                                             
123500     MOVE '  GE' TO GODK-STATUSKODER                                      
123600     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-KONV SSA1             
123700                                                         SSA2             
123800                                                         SSA3             
123900     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
124000     PERFORM IMS-STATUSKONTROLL                                           
124100     .                                                                    
124200     EJECT                                                                
124300     EJECT                                                                
124400 IMS-GET-ARTC-ARTC01 SECTION.                                             
124500                                                                          
124600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
124700          DELIMITED BY SIZE INTO SSA1                                     
124800     MOVE '  GE' TO GODK-STATUSKODER                                      
124900     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
125000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
125100     PERFORM IMS-STATUSKONTROLL                                           
125200     .                                                                    
125300     SKIP3                                                                
125400 IMS-GET-ARTC11 SECTION.                                                  
125500                                                                          
125600     MOVE 'WLARTC11 ' TO SSA1                                             
125700     MOVE '  GE' TO GODK-STATUSKODER                                      
125800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
125900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
126000     PERFORM IMS-STATUSKONTROLL                                           
126100     .                                                                    
126200     SKIP3                                                                
126300 IMS-REPL-ARTC SECTION.                                                   
126400                                                                          
126500     MOVE '    ' TO GODK-STATUSKODER                                      
126600     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
126700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
126800     PERFORM IMS-STATUSKONTROLL                                           
126900     .                                                                    
127000     EJECT                                                                
127100 IMS-DLET-XXAZ SECTION.                                                   
127200                                                                          
127300     MOVE '    ' TO GODK-STATUSKODER                                      
127400     CALL CBLTDLI USING DLET XXAZ-PCB DLI-IO-AREA-XXAZ                    
127500     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     .                                                                    
127800     SKIP3                                                                
127900 IMS-GET-XXAZ-XXAZ01 SECTION.                                             
128000                                                                          
128100     STRING 'WLXXAZ01(WDGXKEY  =' W-IDHTYP-X                              
128200                                  W-LOW-VALUE-2 ')'                       
128300          DELIMITED BY SIZE INTO SSA1                                     
128400     MOVE '    ' TO GODK-STATUSKODER                                      
128500     CALL CBLTDLI USING GU XXAZ-PCB DLI-IO-AREA-XXAZ SSA1                 
128600     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
128700     PERFORM IMS-STATUSKONTROLL                                           
128800     .                                                                    
128900     SKIP3                                                                
129000 IMS-GET-XXAZ-XXAZ11 SECTION.                                             
129100                                                                          
129200     STRING 'WLXXAZ11(WDGXKEY  =' W-IDLEVNR-X                             
129300                                  W-BELEVART-X                            
129400                                  W-IDARTNR-X                             
129500                                  W-IDUSER-X                              
129600                                  W-LOW-VALUE-X ')'                       
129700          DELIMITED BY SIZE INTO SSA1                                     
129800     MOVE '  GE' TO GODK-STATUSKODER                                      
129900     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA-XXAZ SSA1               
130000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
130100     PERFORM IMS-STATUSKONTROLL                                           
130200     .                                                                    
130300     SKIP3                                                                
130400 IMS-GET-XXAZ-XXAZ21-FIRST SECTION.                                       
130500                                                                          
130600     STRING 'WLXXAZ11(WDGXKEY  =' W-IDLEVNR-X                             
130700                                  W-BELEVART-X                            
130800                                  W-IDARTNR-X                             
130900                                  W-IDUSER-X                              
131000                                  W-LOW-VALUE-X ')'                       
131100          DELIMITED BY SIZE INTO SSA1                                     
131200     MOVE 'WLXXAZ21*F' TO SSA2                                            
131300     MOVE '  GE' TO GODK-STATUSKODER                                      
131400     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA-XXAZ SSA1 SSA2          
131500     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
131600     PERFORM IMS-STATUSKONTROLL                                           
131700     .                                                                    
131800     SKIP3                                                                
131900 IMS-GET-XXAZ-XXAZ21 SECTION.                                             
132000                                                                          
132100     STRING 'WLXXAZ11(WDGXKEY  =' W-IDLEVNR-X                             
132200                                  W-BELEVART-X                            
132300                                  W-IDARTNR-X                             
132400                                  W-IDUSER-X                              
132500                                  W-LOW-VALUE-X ')'                       
132600          DELIMITED BY SIZE INTO SSA1                                     
132700     MOVE 'WLXXAZ21 ' TO SSA2                                             
132800     MOVE '  GE' TO GODK-STATUSKODER                                      
132900     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA-XXAZ SSA1 SSA2          
133000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
133100     PERFORM IMS-STATUSKONTROLL                                           
133200     .                                                                    
133300     EJECT                                                                
133400 IMS-ISRT-XXBY-XXBY11 SECTION.                                            
133500                                                                          
133600     STRING 'WLXXBY01(WDG3KEY  =' W-IDHTYP-X                              
133700                                  W-LOW-VALUE-2 ')'                       
133800          DELIMITED BY SIZE INTO SSA1                                     
133900     MOVE 'WLXXBY11 ' TO SSA2                                             
134000     MOVE '    ' TO GODK-STATUSKODER                                      
134100     CALL CBLTDLI USING ISRT XXBY-PCB DLI-IO-AREA SSA1 SSA2               
134200     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
134300     PERFORM IMS-STATUSKONTROLL                                           
134400     .                                                                    
134500     EJECT                                                                
134600 IMS-STATUSKONTROLL SECTION.                                              
134700                                                                          
134800     SET STATUS-IX TO 1                                                   
134900     SEARCH GODK-STATUS                                                   
135000       AT END CALL FELLOG                                                 
135100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
135200     END-SEARCH                                                           
135300     .                                                                    
