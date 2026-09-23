000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9043300.                                                
000400*AUTHOR.         GERRY CARMICHAEL.                                        
000500*DATE-WRITTEN.   92/07/20.                                                
000600*DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        KVALITETSKONTROLL                                                
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001200*                                                                         
001300*        PROGRAMMET LÄSER      W6KVAG (W6H7B)                             
001400*                              WLBENA (WDD3)                              
001500*                              W6PROA (W6G1)                              
001600*                              WLKATN (WDN6)                              
001700*                                                                         
001800*    INDATA:                                                              
001900*        TRANSAKTION: W90433T                                             
002000*        MID:         W90433I1                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W90433O1                                            
002400*                                                                         
002500*    ÄNDRING:                                                             
002600*        97-02-05 C.E./  SDC/NDC.  KDLEVSP - LEVERANSSPÄRR                
002700*                        OCH TEARTNOT- SPÄRRKODSNOTERING                  
002800*        SKALL NUMERA BARA VISAS. UPPDATERING PÅ 6308 ISTÄLLET.           
002900*                                                                         
003000*    2012-01-09 E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1              
003100*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W9043300'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004900 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
005000                                                                          
005100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005300 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005400 77  WS-IDKVAINF                 PIC X(2)    VALUE SPACE.                 
005500 77  WS-TIREGDAT                 PIC X(6)    VALUE SPACE.                 
005600 77  WS-KDKVAINF                 PIC X(1)    VALUE SPACE.                 
005700 77  WS-KDKVAKTL                 PIC X(4)    VALUE SPACE.                 
005800 77  WS-KDKVATYP                 PIC X(1)    VALUE SPACE.                 
005900 77  SPAR-KDKVAKTL               PIC X(4)    VALUE '0000'.                
006000 77  WS-IDPROVPL-PRI             PIC X(1)    VALUE SPACE.                 
006100 77  WS-IDPROVPL-SEK             PIC X(1)    VALUE SPACE.                 
006200 77  WS-KDKVAULG                 PIC X(1)    VALUE SPACE.                 
006300 77  WS-ADKVAULG                 PIC X(2)    VALUE SPACE.                 
006400 77  MAX-ANT-KAT                 PIC S9(9)   VALUE +11  COMP SYNC.        
006500 77  MAX-ANT-LIK                 PIC S9(9)   VALUE +4   COMP SYNC.        
006600 77  LIK-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006700 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
006800 77  CL-INDX                     PIC S9(9)   VALUE +0   COMP SYNC.        
006900 77  MAX-CL-INDX                 PIC S9(9)   VALUE +2   COMP SYNC.        
007000 77  NUM-ADLAGOMR                PIC 9(3)    VALUE ZERO.                  
007100 77  NUM-ADGANG                  PIC 9(3)    VALUE ZERO.                  
007200 77  NUM-ADPLATS                 PIC 9(5)    VALUE ZERO.                  
007300 77  NUM-KDFARLIG                PIC 9(1)    VALUE ZERO.                  
007400 77  WS-VALUE-40                 PIC S9(3)   VALUE +040.                  
007500 77  WS-6102-KVSKPLOT-PRI        PIC S9(3)   COMP-3 VALUE +0.             
007600 77  WS-6102-KVSKPLOT-SEK        PIC S9(3)   COMP-3 VALUE +0.             
007700 77  DISPONIBELT                 PIC S9(7)   COMP-3 VALUE +0.             
007800 77  W-ART-KVLS                  PIC S9(7)   COMP-3 VALUE +0.             
007900 77  W-ART-KVUTRS                PIC S9(7)   COMP-3 VALUE +0.             
008000 77  W-ART-KVRESS                PIC S9(7)   COMP-3 VALUE +0.             
008100 77  W-ART-KVSPANT               PIC S9(7)   COMP-3 VALUE +0.             
008200 77  W-ART-KDERS                 PIC S9(3)   COMP-3 VALUE +0.             
008300 77  W-ART-KVSLAGER              PIC S9(7)   COMP-3 VALUE +0.             
008400 77  W-ART-KDLTK                 PIC S9      COMP-3 VALUE +0.             
008500 77  WS-CLAG-KDERS               PIC S9(3)   COMP-3 VALUE +0.             
008600 77  W-SDC-KVPB                  PIC S9(6)V9  VALUE ZERO COMP-3.          
008700 77  W-NDC-KVPB                  PIC S9(6)V9  VALUE ZERO COMP-3.          
008800                                                                          
008900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009000     88  INDATA-OK                           VALUE 'J'.                   
009100     88  INDATA-FEL                          VALUE 'N'.                   
009200                                                                          
009300 77  PROVPL-PRI-SW               PIC X       VALUE 'J'.                   
009400     88  PROVPL-PRI-OK                       VALUE 'J'.                   
009500     88  PROVPL-PRI-FEL                      VALUE 'N'.                   
009600                                                                          
009700 77  PROVPL-SEK-SW               PIC X       VALUE 'J'.                   
009800     88  PROVPL-SEK-OK                       VALUE 'J'.                   
009900     88  PROVPL-SEK-FEL                      VALUE 'N'.                   
010000                                                                          
010100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010200     88  NYCKLAR-OK                          VALUE 'J'.                   
010300     88  NYCKLAR-FEL                         VALUE 'N'.                   
010400                                                                          
010500 77  NYUPPLAEGG-SW               PIC X       VALUE 'N'.                   
010600     88  NYUPPLAEGG                          VALUE 'J'.                   
010700                                                                          
010800 77  SKIPLOT-SW                  PIC X       VALUE 'N'.                   
010900     88  SKIPLOT-UPDATE                      VALUE 'J'.                   
011000                                                                          
011100 77  KDKVATYP-SW                 PIC X       VALUE 'N'.                   
011200     88  KDKVATYP-UPDATE                     VALUE 'J'.                   
011300                                                                          
011400 77  ANNULLERAD-SW               PIC X       VALUE 'N'.                   
011500     88  ANNULLERAD-FINNS                    VALUE 'J'.                   
011600                                                                          
011700 77  KR-FINNS-SW                 PIC X       VALUE 'N'.                   
011800     88  KR-FINNS                            VALUE 'J'.                   
011900     88  KR-FINNS-INTE                       VALUE 'N'.                   
012000                                                                          
012100                                                                          
012200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012300     88  EGEN-MID                            VALUE '9433'.                
012400     88  GODK-MID                            VALUE '9433' '6212'          
012500                                                   '6213' '6214'          
012600                                                   '6215' '6216'          
012700                                                   '6217' '6218'          
012800                                                   '6219'.                
012900     88  HELP-MID                            VALUE '0551'.                
013000                                                                          
013100 01    WS-SATS       PIC X.                                               
013200         88  SAKNAS-I-SATS       VALUE 'N'.                               
013300                                                                          
013400 01      FILLER.                                                          
013500   03      WS-KDYTBEH            PIC 9(2)   VALUE ZERO.                   
013600   03      FILLER REDEFINES WS-KDYTBEH.                                   
013700     05      KDYTBEH1            PIC 9.                                   
013800     05      KDYTBEH2            PIC 9.                                   
013900                                                                          
014000 01  WS-TID                      PIC S9(9).                               
014100 01  WS-DAREGDAT                 PIC 9(8).                                
014200     EJECT                                                                
014300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014400 01  GENERELLA-SUBPROGRAM.                                                
014500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
015000                                                                          
015100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015200*01 -COPY WMSGINIT                                                        
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015500*01 -COPY WMEDAREA                                                        
015600     EJECT                                                                
015700*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
015800*01  -COPY WDECAREA                                                       
015900     EJECT                                                                
016000 01  MESSAGE-CODES.                                                       
016100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
016200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
016400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
016500     03  ERR-ART-NO-MORE         PIC X(3)    VALUE '018'.                 
016600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
016800     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
016900     03  ERR-ART-ERSATT          PIC X(3)    VALUE '220'.                 
017000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017100     03  ERR-ART-MISSING         PIC X(3)    VALUE '769'.                 
017200     03  FLUPG-KVASAK-UPDATED    PIC X(3)    VALUE '235'.                 
017300     EJECT                                                                
017400*      --- VALID IDDC CODES                                               
017500*                                                                         
017600*01    -COPY WWDC99                                                       
017700     EJECT                                                                
017800*    --- UNDERLAG FÖR KVALITETSKONTROLL                                   
017900*    -COPY W426KTL                                                        
018000     EJECT                                                                
018100*                                                                         
018200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018300*                                                                         
018400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018500     SKIP3                                                                
018600*01  MID -COPY W90433I1                                                   
018700     EJECT                                                                
018800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018900     SKIP3                                                                
019000*01  -COPY WMSGAREA                                                       
019100     EJECT                                                                
019200     03  MOD REDEFINES MSG-AREA.                                          
019300*      05  -COPY W90433O1                                                 
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019600     SKIP3                                                                
019700*01  -COPY WMFSAREA                                                       
019800     EJECT                                                                
019900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020000*                                                                         
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020300     SKIP3                                                                
020400 01  NYCKLAR-TILL-DLI.                                                    
020500                                                                          
020600     03  W-IDARTNR-X.                                                     
020700         05 W-IDARTNR            PIC S9(9)  VALUE ZERO COMP-3.            
020800     03  W-IDSKYLT-KEY-X.                                                 
020900         05 W-IDSKYLT-KEY        PIC X(3)   VALUE SPACE.                  
021000     03  W-W6H701KY-X.                                                    
021100         05  W-IDKR              PIC 9(5)  VALUE ZERO.                    
021200     03  W-W6H7B1KY-MIN-X.                                                
021300         05  W-SEQB-IDARTNR-MIN  PIC S9(9)  VALUE ZERO COMP-3.            
021400         05  W-SEQB-DAREGDAT-9KOMPL-MIN                                   
021500                                 PIC  9(8)   VALUE ZERO.                  
021600         05  W-SEQB-IDLEVNR-MIN  PIC  X(5)  VALUE SPACE.                  
021700         05  W-SEQB-KVKRKNTR-MIN PIC S9(1)  VALUE ZERO COMP-3.            
021800         05  W-SEQB-IDKR-MIN     PIC  9(5)  VALUE ZERO.                   
021900     03  W-W6H7B1KY-MAX-X.                                                
022000         05  W-SEQB-IDARTNR-MAX  PIC S9(9)  VALUE ZERO COMP-3.            
022100         05  W-SEQB-DAREGDAT-9KOMPL-MAX                                   
022200                                 PIC  9(8)   VALUE ZERO.                  
022300         05  W-SEQB-IDLEVNR-MAX  PIC  X(5)  VALUE SPACE.                  
022400         05  W-SEQB-KVKRKNTR-MAX PIC S9(1)  VALUE ZERO COMP-3.            
022500         05  W-SEQB-IDKR-MAX     PIC  9(5)  VALUE ZERO.                   
022600     03  W-WDN611KY-X.                                                    
022700         05 W-IDFORDON           PIC S9(3)  VALUE ZERO COMP-3.            
022800         05 W-TIOMBRYT-1         PIC S9(7)  VALUE ZERO COMP-3.            
022900     03  W-IDLAND-X.                                                      
023000         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
023100                                                                          
023200     03  W-IDDC-B6-X.                                                     
023300         05 W-IDDC-B6                  PIC X(2).                          
023400     SKIP2                                                                
023500*    --- STATUS-KOD FRÅN IMS                                              
023600 01  STATUS-WS                   PIC XX.                                  
023700     88  SEGMENT-FINNS                       VALUE '  '.                  
023800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024000     SKIP2                                                                
024100 01  GODK-STATUSKODER.                                                    
024200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024300     SKIP3                                                                
024400 01  SSA1                        PIC X(96).                               
024500 01  SSA2                        PIC X(64).                               
024600     EJECT                                                                
024700*    --- IMS FUNKTIONSKODER                                               
024800*01  -COPY W0003                                                          
024900     EJECT                                                                
025000*    ---  DLI INPUT-OUTPUT AREA                                           
025100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
025200     SKIP3                                                                
025300 01  DLI-IO-AREA.                                                         
025400     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
025500     SKIP3                                                                
025600     03  W6KVAG01 REDEFINES IO-AREA.                                      
025700*        05  -COPY W6H7B1                                                 
025800     SKIP3                                                                
025900     03  WDK601 REDEFINES IO-AREA.                                        
026000*        05  -COPY WDK601                                                 
026100     SKIP3                                                                
026200     03  WDK611 REDEFINES IO-AREA.                                        
026300*        05  -COPY WDK611                                                 
026400     SKIP3                                                                
026500     03  WDK625 REDEFINES IO-AREA.                                        
026600*        05  -COPY WDK625                                                 
026700     SKIP3                                                                
026800     03  WLKATN01 REDEFINES IO-AREA.                                      
026900*        05  -COPY WDN601                                                 
027000     SKIP3                                                                
027100     03  WLKATN11 REDEFINES IO-AREA.                                      
027200*        05  -COPY WDN611                                                 
027300     EJECT                                                                
027400*                            DLI INPUT-OUTPUT AREA3                       
027500*                            DLI INPUT-OUTPUT AREA4                       
027600 01  DLI-IO-AREA5.                                                        
027700     03  IO-AREA5                PIC X(600)  VALUE SPACE.                 
027800     03  W6KVAE01 REDEFINES IO-AREA5.                                     
027900*        05  -COPY W6H701                                                 
028000     EJECT                                                                
028100*                            DLI INPUT-OUTPUT WDT3                        
028200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301'.                      
028300 01  DLI-IO-WDT301.                                                       
028400*    03  -COPY WDT301                                                     
028500     EJECT                                                                
028600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311'.                      
028700 01  DLI-IO-WDT311.                                                       
028800*    03  -COPY WDT311                                                     
028900     EJECT                                                                
029000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
029100 01  DLI-IO-WDK701.                                                       
029200*    03  -COPY WDK701                                                     
029300     EJECT                                                                
029400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
029500 01  DLI-IO-WDK711.                                                       
029600*    03  -COPY WDK711                                                     
029700                                                                          
029800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
029900 01   DLI-IO-AREA-B601.                                                   
030000*     03  -COPY WDB601                                                    
030100 LINKAGE SECTION.                                                         
030200                                                                          
030300*01  -COPY W0009  -PRE MSG-                                               
030400     EJECT                                                                
030500*01  -COPY W0008  -PRE USEA-                                              
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800*01  -COPY W0008  -PRE ARTC-                                              
030900     05  FILLER                  PIC X.                                   
031000     EJECT                                                                
031100*01  -COPY W0008  -PRE WDK7-                                              
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400*01  -COPY W0008  -PRE KATN-                                              
031500     05  FILLER                  PIC X.                                   
031600*01  -COPY W0008  -PRE KVAG-                                              
031700     05  FILLER                  PIC X.                                   
031800     EJECT                                                                
031900*01  -COPY W0008  -PRE KVAE-                                              
032000     05  FILLER                  PIC X.                                   
032100     EJECT                                                                
032200*01  -COPY W0008  -PRE WDB6-                                              
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01  -COPY W0008  -PRE WDT3-                                              
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB                               
032900                                   ARTC-PCB WDK7-PCB                      
033000                                   KATN-PCB                               
033100                          KVAG-PCB KVAE-PCB WDB6-PCB WDT3-PCB.            
033200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
033300                                   ARTC-PCB WDK7-PCB                      
033400                                   KATN-PCB                               
033500                          KVAG-PCB KVAE-PCB WDB6-PCB WDT3-PCB.            
033600                                                                          
033700     PERFORM IMS-GET-MSG                                                  
033800     IF SEGMENT-FINNS                                                     
033900       PERFORM A-INIT                                                     
034000       PERFORM B-KOLLA-NYCKLAR                                            
034100       IF NYCKLAR-OK                                                      
034200         IF MFS-UPDATE  OR MFS-UPD-V                                      
034300           PERFORM G-KOLLA-INPUT                                          
034400           IF INDATA-OK                                                   
034500             PERFORM H-UPPDATERA                                          
034600           END-IF                                                         
034700         ELSE                                                             
034800           IF MFS-FIRST                                                   
034900             PERFORM C-FOERSTA-SIDA                                       
035000           ELSE                                                           
035100             IF MFS-NEXT                                                  
035200               PERFORM D-NAESTA-SIDA                                      
035300             ELSE                                                         
035400               PERFORM E-SAMMA-SIDA                                       
035500             END-IF                                                       
035600           END-IF                                                         
035700         END-IF                                                           
035800         IF INDATA-OK                                                     
035900           PERFORM F-LAES-VISA-INFO                                       
036000         END-IF                                                           
036100       END-IF                                                             
036200       MOVE LENGTH OF MOD-W90433O1  TO MSG-KVLL                           
036300       ADD         +4               TO MSG-KVLL                           
036400       PERFORM IMS-INSERT-MSG                                             
036500     END-IF                                                               
036600                                                                          
036700     MOVE ZERO TO RETURN-CODE                                             
036800     GOBACK                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 A-INIT SECTION.                                                          
037200     IF MSG-DUBBLA-TRANSKODER                                             
037300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90433I1                 
037400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
037500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
037600     ELSE                                                                 
037700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90433I1                  
037800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
037900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038000     END-IF                                                               
038100                                                                          
038200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
038300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
038400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
038500                                                                          
038600     MOVE LOW-VALUE   TO MSG-AREA                                         
038700     MOVE  'W90433O1' TO MFS-IDMOD                                        
038800     MOVE '9433'      TO MOD-IDTRANS                                      
038900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
039000                                                                          
039100     IF EGEN-MID OR HELP-MID                                              
039200       CONTINUE                                                           
039300     ELSE                                                                 
039400       MOVE SPACE TO MFS-KDTRTYP                                          
039500       MOVE '7' TO MFS-IDPFK                                              
039600     END-IF                                                               
039700     .                                                                    
039800     EJECT                                                                
039900 B-KOLLA-NYCKLAR SECTION.                                                 
040000                                                                          
040100     MOVE JA  TO NYCKLAR-SW                                               
040200                 INDATA-SW                                                
040300                 PROVPL-PRI-SW                                            
040400                 PROVPL-SEK-SW                                            
040500     MOVE NEJ TO SKIPLOT-SW                                               
040600                 KDKVATYP-SW                                              
040700                 NYUPPLAEGG-SW                                            
040800                                                                          
040900     MOVE LOW-VALUE       TO W-W6H7B1KY-MIN-X                             
041000     MOVE HIGH-VALUE      TO W-W6H7B1KY-MAX-X                             
041100                                                                          
041200     MOVE ALL '+' TO MSGI-WMSGINIT                                        
041300     MOVE '001'             TO MSGI-KDCALL                                
041400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041500                                    MSGI-IDLTERM-USER                     
041600     MOVE '9433'                 TO MSGI-IDTRANS                          
041700                                                                          
041800     IF MFS-IDTRANS = '9433'                                              
041900     OR (MID-IDARTNR-IN NUMERIC                                           
042000     AND MID-IDARTNR-IN > ZERO)                                           
042100         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
042200     END-IF                                                               
042300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042400                                                                          
042500     IF MSGI-IDLAND-SPR = 'GB'                                            
042600       MOVE +2 TO SPRAK-IX                                                
042700       MOVE 'GB ' TO MED-IDSKYLT                                          
042800     ELSE                                                                 
042900       MOVE +1 TO SPRAK-IX                                                
043000       MOVE 'S  ' TO MED-IDSKYLT                                          
043100     END-IF                                                               
043200                                                                          
043300     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
043400     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
043500                                                                          
043600     IF MID-IDARTNR-IN = ALL '+'                                          
043700       CONTINUE                                                           
043800     ELSE                                                                 
043900       MOVE '7'         TO MFS-IDPFK                                      
044000       MOVE SPACE       TO MFS-KDTRTYP                                    
044100     END-IF                                                               
044200                                                                          
044300     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
044400       MOVE WS-IDARTNR TO W-IDARTNR                                       
044500                          W-SEQB-IDARTNR-MIN                              
044600                          W-SEQB-IDARTNR-MAX                              
044700     ELSE                                                                 
044800       MOVE NEJ TO NYCKLAR-SW                                             
044900     END-IF                                                               
045000                                                                          
045100     IF NYCKLAR-FEL                                                       
045200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
045300       CALL WMEDKONV USING MED-WMEDAREA                                   
045400       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
045500       PERFORM MFS-RENSA-FAELT-UT                                         
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900 C-FOERSTA-SIDA SECTION.                                                  
046000                                                                          
046100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
046200     CALL WMEDKONV USING MED-WMEDAREA                                     
046300     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
046400                                                                          
046500     PERFORM MFS-RENSA-FAELT-UT                                           
046600     .                                                                    
046700     EJECT                                                                
046800 D-NAESTA-SIDA SECTION.                                                   
046900                                                                          
047000     PERFORM MFS-RENSA-FAELT-UT                                           
047100     .                                                                    
047200     EJECT                                                                
047300 E-SAMMA-SIDA SECTION.                                                    
047400                                                                          
047500     IF EGEN-MID OR HELP-MID                                              
047600                                                                          
047700       IF MID-INPUT1 = ALL '+' AND MID-INPUT2 = ALL '+' AND               
047800          MID-INPUT3 = ALL '+'                                            
047900         CONTINUE                                                         
048000       ELSE                                                               
048100         IF MID-INPUT2 NOT = ALL '+'                                      
048200           MOVE INF-PRESS-PF23 TO MED-IDMFSINF                            
048300         ELSE                                                             
048400           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
048500         END-IF                                                           
048600         CALL WMEDKONV USING MED-WMEDAREA                                 
048700         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
048800         PERFORM EA-MID-INDATA-TILL-MOD                                   
048900       END-IF                                                             
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 EA-MID-INDATA-TILL-MOD SECTION.                                          
049400                                                                          
049500* * * * * FÖR VARJE MID-FÄLT                                              
049600* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
049700* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
049800* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
049900                                                                          
050000* TEARTNOT2 SKALL NUMERA UPPDATERAS FRÅN BILD 6308                        
050100* KDLEVSP SKALL NUMERA UPPDATERAS FRÅN BILD 6308                          
050200                                                                          
050300     IF MID-KDFARLIG  NOT = ALL '+'                                       
050400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFARLIG-IN-ATTR                 
050500     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800 F-LAES-VISA-INFO SECTION.                                                
050900                                                                          
051000     PERFORM IMS-GU-ARTC-WDK601                                           
051100     IF SEGMENT-FINNS                                                     
051200       IF ART-KDERS-UTG NOT > 0                                           
051300         PERFORM FA-BEHANDLA-ARTC                                         
051400         PERFORM FD-BEHANDLA-WDK7                                         
051500       ELSE                                                               
051600         PERFORM MFS-STAENG-FAELT-IN                                      
051700         MOVE ERR-ART-NO-MORE TO MED-IDMFSFEL                             
051800         CALL WMEDKONV USING MED-WMEDAREA                                 
051900         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
052000       END-IF                                                             
052100                                                                          
052200       PERFORM FB-BEHANDLA-KATN11                                         
052300                                                                          
052400       PERFORM FC-BEHANDLA-KVAG01                                         
052500     ELSE                                                                 
052600       MOVE ERR-ART-MISSING TO MED-IDMFSFEL                               
052700       CALL WMEDKONV USING MED-WMEDAREA                                   
052800       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
052900       PERFORM MFS-RENSA-FAELT-UT                                         
053000     END-IF                                                               
053100     .                                                                    
053200     EJECT                                                                
053300 FA-BEHANDLA-ARTC SECTION.                                                
053400                                                                          
053500     PERFORM IMS-GNP-ARTC-WDK611                                          
053600     IF SEGMENT-FINNS                                                     
053700       IF     CLAG-KDYTBEH  NUMERIC                                       
053800         MOVE CLAG-KDYTBEH    TO MOD-KDYTBEH-UT                           
053900       ELSE                                                               
054000         MOVE ZERO            TO MOD-KDYTBEH-UT                           
054100       END-IF                                                             
054200     END-IF                                                               
054300                                                                          
054400     MOVE CLAG-KDFARLIG TO NUM-KDFARLIG                                   
054500     MOVE NUM-KDFARLIG       TO MOD-KDFARLIG-UT                           
054600                                                                          
054700     IF CLAG-KDERS > 0 AND < 29                                           
054800       MOVE ERR-ART-ERSATT TO MED-IDMFSFEL                                
054900       CALL WMEDKONV USING MED-WMEDAREA                                   
055000       MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                
055100     ELSE                                                                 
055200       IF CLAG-KDERS > 28                                                 
055300         MOVE ERR-ART-NO-MORE TO MED-IDMFSFEL                             
055400         CALL WMEDKONV USING MED-WMEDAREA                                 
055500         MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                             
055600       END-IF                                                             
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000                                                                          
056100 FB-BEHANDLA-KATN11 SECTION.                                              
056200     MOVE +1 TO INDX                                                      
056300     PERFORM IMS-GU-KATN-WLKATN01                                         
056400     IF SEGMENT-FINNS                                                     
056500       PERFORM IMS-GNP-KATN-WLKATN11                                      
056600       IF SEGMENT-FINNS                                                   
056700          IF MFS-UPDATE  OR MFS-UPD-V  OR                                 
056800             MID-INPUT1 NOT = ALL '+'  OR                                 
056900             MID-INPUT2 NOT = ALL '+'                                     
057000            CONTINUE                                                      
057100          ELSE                                                            
057200            MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                     
057300            CALL WMEDKONV USING MED-WMEDAREA                              
057400            MOVE MED-TEMFSINF TO MOD-TEMFSINF                             
057500          END-IF                                                          
057600       END-IF                                                             
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000 FC-BEHANDLA-KVAG01 SECTION.                                              
058100                                                                          
058200     PERFORM IMS-GU-KVAG-W6KVAG01                                         
058300     IF SEGMENT-FINNS                                                     
058400       PERFORM UNTIL SEGMENT-SAKNAS                                       
058500           MOVE SEQB-IDKR TO W-IDKR                                       
058600           PERFORM IMS-GU-KVAE-W6KVAE01                                   
058700           IF KR-FLANNULL = JA                                            
058800               MOVE JA     TO ANNULLERAD-SW                               
058900           ELSE                                                           
059000               MOVE JA     TO KR-FINNS-SW                                 
059100           END-IF                                                         
059200           PERFORM IMS-GN-KVAG-W6KVAG01                                   
059300       END-PERFORM                                                        
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700                                                                          
059800 FD-BEHANDLA-WDK7 SECTION.                                                
059900                                                                          
060000     PERFORM IMS-GET-WDK701                                               
060100     IF SEGMENT-FINNS                                                     
060200        PERFORM IMS-GET-WDK711                                            
060300        PERFORM UNTIL SEGMENT-SAKNAS                                      
060400          MOVE SLAG-IDDC TO WS-IDDC                                       
060500                            W-IDDC-B6                                     
060600          PERFORM IMS-GU-WDB601                                           
060700          IF DCS-SDC                                                      
060800            ADD SLAG-KVPB-REF        TO   W-SDC-KVPB                      
060900          ELSE                                                            
061000            IF NDC                                                        
061100              IF SLAG-IDLEVNR = '1441'                                    
061200                ADD SLAG-KVPB-REF    TO   W-NDC-KVPB                      
061300              END-IF                                                      
061400            END-IF                                                        
061500          END-IF                                                          
061600          PERFORM IMS-GET-WDK711                                          
061700        END-PERFORM                                                       
061800        COMPUTE W-SDC-KVPB ROUNDED =  W-SDC-KVPB                          
061900                                                                          
062000        COMPUTE W-NDC-KVPB ROUNDED =  W-NDC-KVPB                          
062100     END-IF                                                               
062200     .                                                                    
062300     EJECT                                                                
062400                                                                          
062500 G-KOLLA-INPUT SECTION.                                                   
062600                                                                          
062700     MOVE JA TO INDATA-SW                                                 
062800                                                                          
062900     IF MID-INPUT1 = ALL '+' AND MID-INPUT2 = ALL '+' AND                 
063000        MID-INPUT3 = ALL '+'                                              
063100       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
063200       CALL WMEDKONV USING MED-WMEDAREA                                   
063300       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
063400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
063500       MOVE NEJ TO INDATA-SW                                              
063600     ELSE                                                                 
063700                                                                          
063800       IF MID-INPUT2 NOT = ALL '+' AND MFS-UPDATE                         
063900         MOVE INF-PRESS-PF23 TO MED-IDMFSINF                              
064000         CALL WMEDKONV USING MED-WMEDAREA                                 
064100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
064200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
064300         PERFORM GA-LAES-IN-IGEN                                          
064400         MOVE NEJ TO INDATA-SW                                            
064500       ELSE                                                               
064600                                                                          
064700         IF MID-INPUT1 NOT = ALL '+'                                      
064800                                                                          
064900           IF MID-KDFARLIG NOT = ALL '+'                                  
065000**** ENLIGT ROLF ANDERSSON SKALL ENDAST KODERNA 3,4 OCH 6 VARA            
065100*** GODKÄNDA VÄRDEN PÅ KDFARLIG. OCH 0 ENL RA                             
065200** ÄNDRAT 971112 /KENT                                                    
065300*             IF MID-KDFARLIG NUMERIC                                     
065400              IF MID-KDFARLIG = 3 OR 4 OR 6 OR 0 OR 7                     
065500                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFARLIG-IN-ATTR         
065600              ELSE                                                        
065700                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-IN-ATTR           
065800                 MOVE NEJ TO INDATA-SW                                    
065900              END-IF                                                      
066000           END-IF                                                         
066100           IF MID-KDYTBEH NOT = ALL '+'                                   
066200              PERFORM GB-NUMERIC-CHECK                                    
066300              IF INDATA-OK                                                
066400                 IF WS-KDYTBEH NUMERIC AND                                
066500                    WS-KDYTBEH < 10                                       
066600                    CONTINUE                                              
066700                 ELSE                                                     
066800                    MOVE NEJ TO INDATA-SW                                 
066900                 END-IF                                                   
067000              END-IF                                                      
067100           END-IF                                                         
067200         END-IF                                                           
067300                                                                          
067400         IF INDATA-FEL                                                    
067500           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
067600           CALL WMEDKONV USING MED-WMEDAREA                               
067700           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
067800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
067900         END-IF                                                           
068000       END-IF                                                             
068100     END-IF                                                               
068200     .                                                                    
068300     EJECT                                                                
068400 GA-LAES-IN-IGEN SECTION.                                                 
068500                                                                          
068600     IF MID-KDFARLIG NOT = ALL '+'                                        
068700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFARLIG-IN-ATTR                 
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100                                                                          
069200 GB-NUMERIC-CHECK SECTION.                                                
069300                                                                          
069400     MOVE MID-KDYTBEH    TO DEC-IDFRIDATA                                 
069500     MOVE 2              TO DEC-KVHELTAL                                  
069600     MOVE 0              TO DEC-KVDECIMAL                                 
069700     CALL WDECEDIT USING DEC-WDECAREA                                     
069800     IF DEC-KDSVAR-OK                                                     
069900       MOVE DEC-IDEDITDATA                                                
070000                         TO WS-KDYTBEH                                    
070100     ELSE                                                                 
070200       MOVE NEJ TO INDATA-SW                                              
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600                                                                          
070700 H-UPPDATERA SECTION.                                                     
070800                                                                          
070900     IF MID-KDYTBEH   NOT = ALL '+'                                       
071000       PERFORM HA-UPPDATERA-ARTC                                          
071100     END-IF                                                               
071200                                                                          
071300     IF MID-INPUT1 NOT = ALL '+'                                          
071400       PERFORM HB-UPPDATERA-ARTC                                          
071500     END-IF                                                               
071600                                                                          
071700     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
071800     CALL WMEDKONV USING MED-WMEDAREA                                     
071900     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
072000     PERFORM MFS-FORM-ATTR                                                
072100     .                                                                    
072200     EJECT                                                                
072300 HA-UPPDATERA-ARTC SECTION.                                               
072400     PERFORM IMS-GU-ARTC-WDK601                                           
072500     IF SEGMENT-FINNS                                                     
072600        IF ART-FLIART = JA                                                
072700           MOVE JA                   TO WS-SATS                           
072800        ELSE                                                              
072900           MOVE NEJ                  TO WS-SATS                           
073000        END-IF                                                            
073100        IF MID-KDYTBEH NOT = ALL '+'                                      
073200          PERFORM IMS-GHNP-ARTC-WDK611                                    
073300          IF SEGMENT-FINNS                                                
073400            IF MID-KDYTBEH NOT = ALL '+'                                  
073500              MOVE WS-KDYTBEH  TO MOD-KDYTBEH-UT                          
073600                                  CLAG-KDYTBEH                            
073700            END-IF                                                        
073800            PERFORM IMS-REPL-ARTC                                         
073900          END-IF                                                          
074000        END-IF                                                            
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400 HB-UPPDATERA-ARTC SECTION.                                               
074500     SKIP2                                                                
074600     IF MID-KDFARLIG NOT = ALL '+'                                        
074700     OR KDYTBEH2 = 4                                                      
074800       MOVE 'SE'            TO W-IDLAND                                   
074900       PERFORM IMS-GU-WDT311                                              
075000       IF NOT SEGMENT-FINNS                                               
075100          MOVE 'SE'         TO FPCK-IDLANDX2                              
075200          MOVE ZEROES       TO FPCK-KDFORP                                
075300          MOVE SPACE        TO FPCK-TEBEFT(1)                             
075400          MOVE SPACE        TO FPCK-TEBEFT(2)                             
075500          MOVE SPACE        TO FPCK-TEBEFT(3)                             
075600          MOVE SPACE        TO FPCK-TEBEFT(4)                             
075700          MOVE SPACE        TO FPCK-TEBEFT(5)                             
075800       END-IF                                                             
075900       PERFORM IMS-GHU-ARTC-WDK611                                        
076000       IF SEGMENT-FINNS                                                   
076100         IF MID-KDFARLIG NOT = ALL '+'                                    
076200           MOVE MID-KDFARLIG TO MOD-KDFARLIG-UT                           
076300                                CLAG-KDFARLIG                             
076400         END-IF                                                           
076500                                                                          
076600         IF KDYTBEH2 = 4                                                  
076700           MOVE MSG-SIGNON-USERID TO FPCK-IDUSER                          
076800           MOVE WS-VALUE-40       TO CLAG-BEFT                            
076900                                     FPCK-BEFT                            
077000           MOVE 'UPPDATERING AV YTBEHANDLINGSKOD TILL 4'                  
077100                                  TO FPCK-TEBEFT(1)                       
077200           PERFORM HB1-CONVERT-TO-9KOMPL                                  
077300         END-IF                                                           
077400         PERFORM IMS-REPL-ARTC                                            
077500         IF KDYTBEH2 = 4                                                  
077600            PERFORM IMS-GU-WDT301                                         
077700            IF SEGMENT-SAKNAS                                             
077800               MOVE W-IDARTNR     TO FART-IDARTNR                         
077900               PERFORM IMS-ISRT-WDT301                                    
078000            END-IF                                                        
078100            PERFORM IMS-ISRT-WDT311                                       
078200         END-IF                                                           
078300       END-IF                                                             
078400     END-IF                                                               
078500     .                                                                    
078600     EJECT                                                                
078700 HB1-CONVERT-TO-9KOMPL SECTION.                                           
078800     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                       
078900     COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WS-DAREGDAT               
079000     ACCEPT WS-TID FROM TIME                                              
079100     COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WS-TID                     
079200     .                                                                    
079300     EJECT                                                                
079400 MFS-RENSA-FAELT-UT SECTION.                                              
079500                                                                          
079600     MOVE MFS-RENSA-FAELT TO MOD-KDYTBEH-UT                               
079700                             MOD-KDFARLIG-UT                              
079800     .                                                                    
079900     SKIP2                                                                
080000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
080100                                                                          
080200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDYTBEH-UT                             
080300                               MOD-KDFARLIG-UT                            
080400                                                                          
080500     .                                                                    
080600     EJECT                                                                
080700 MFS-FORM-ATTR SECTION.                                                   
080800                                                                          
080900*    --- ALLA INDATA-FÄLT                                                 
081000     MOVE MFS-FORMATETS-ATTR TO MOD-KDFARLIG-IN-ATTR                      
081100     .                                                                    
081200     SKIP2                                                                
081300 MFS-STAENG-FAELT-IN SECTION.                                             
081400     MOVE MFS-STAENG-FAELT TO       MOD-KDFARLIG-IN-ATTR                  
081500     .                                                                    
081600     EJECT                                                                
081700* --- IMS SEKTIONER ---                                                   
081800     SKIP3                                                                
081900 IMS-GET-MSG SECTION.                                                     
082000                                                                          
082100     MOVE '  QC' TO GODK-STATUSKODER                                      
082200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
082300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     .                                                                    
082600     SKIP3                                                                
082700 IMS-INSERT-MSG SECTION.                                                  
082800                                                                          
082900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
083000     MOVE SPACE TO GODK-STATUSKODER                                       
083100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
083200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     .                                                                    
083500     EJECT                                                                
083600                                                                          
083700 IMS-GU-ARTC-WDK601 SECTION.                                              
083800*WDK6                                                                     
083900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
084000            DELIMITED BY SIZE INTO SSA1                                   
084100     MOVE '  GE' TO GODK-STATUSKODER                                      
084200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
084300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
084400     PERFORM IMS-STATUSKONTROLL                                           
084500     .                                                                    
084600     SKIP3                                                                
084700 IMS-GNP-ARTC-WDK611 SECTION.                                             
084800*WDK6                                                                     
084900     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
085000     MOVE '  GE' TO GODK-STATUSKODER                                      
085100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
085200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     .                                                                    
085500     SKIP3                                                                
085600 IMS-GHNP-ARTC-WDK611 SECTION.                                            
085700*WDK6                                                                     
085800     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
085900     MOVE '  GE' TO GODK-STATUSKODER                                      
086000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
086100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
086200     PERFORM IMS-STATUSKONTROLL                                           
086300     SKIP3                                                                
086400     .                                                                    
086500     EJECT                                                                
086600 IMS-REPL-ARTC SECTION.                                                   
086700*WDK6                                                                     
086800     MOVE '  ' TO GODK-STATUSKODER                                        
086900     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
087000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
087100     PERFORM IMS-STATUSKONTROLL                                           
087200     .                                                                    
087300     EJECT                                                                
087400 IMS-GU-WDT301           SECTION.                                         
087500                                                                          
087600     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
087700          DELIMITED BY SIZE INTO SSA1                                     
087800     MOVE '  GE' TO GODK-STATUSKODER                                      
087900     CALL CBLTDLI USING GU  WDT3-PCB DLI-IO-WDT301 SSA1                   
088000     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
088100     PERFORM IMS-STATUSKONTROLL                                           
088200     .                                                                    
088300     SKIP3                                                                
088400 IMS-ISRT-WDT301 SECTION.                                                 
088500                                                                          
088600     MOVE 'WDT301' TO SSA1                                                
088700     MOVE '  '     TO GODK-STATUSKODER                                    
088800     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
088900     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
089000     PERFORM IMS-STATUSKONTROLL                                           
089100     .                                                                    
089200     SKIP3                                                                
089300 IMS-GU-WDT311           SECTION.                                         
089400                                                                          
089500     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
089600          DELIMITED BY SIZE INTO SSA1                                     
089700     STRING 'WDT311  (IDLAND   =' W-IDLAND-X ')'                          
089800          DELIMITED BY SIZE INTO SSA2                                     
089900     MOVE '  GE' TO GODK-STATUSKODER                                      
090000     CALL CBLTDLI USING GU  WDT3-PCB DLI-IO-WDT311 SSA1 SSA2              
090100     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
090200     PERFORM IMS-STATUSKONTROLL                                           
090300     .                                                                    
090400     SKIP3                                                                
090500 IMS-ISRT-WDT311 SECTION.                                                 
090600                                                                          
090700     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
090800          DELIMITED BY SIZE INTO SSA1                                     
090900     MOVE 'WDT311' TO SSA2                                                
091000     MOVE '    ' TO GODK-STATUSKODER                                      
091100     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
091200     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
091300     PERFORM IMS-STATUSKONTROLL                                           
091400     .                                                                    
091500     EJECT                                                                
091600 IMS-GHU-ARTC-WDK611 SECTION.                                             
091700*WDK6                                                                     
091800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
091900            DELIMITED BY SIZE INTO SSA1                                   
092000     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
092100     MOVE '  GE' TO GODK-STATUSKODER                                      
092200     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
092300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
092400     PERFORM IMS-STATUSKONTROLL                                           
092500     .                                                                    
092600     SKIP3                                                                
092700 IMS-GET-WDK701 SECTION.                                                  
092800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
092900            DELIMITED BY SIZE INTO SSA1                                   
093000     MOVE '  GE' TO GODK-STATUSKODER                                      
093100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
093200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
093300     PERFORM IMS-STATUSKONTROLL                                           
093400     .                                                                    
093500     SKIP3                                                                
093600 IMS-GET-WDK711 SECTION.                                                  
093700     MOVE 'WDK711' TO SSA1                                                
093800     MOVE '  GE' TO GODK-STATUSKODER                                      
093900     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
094000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
094100     PERFORM IMS-STATUSKONTROLL                                           
094200     .                                                                    
094300     EJECT                                                                
094400                                                                          
094500 IMS-GU-KVAG-W6KVAG01 SECTION.                                            
094600     STRING 'W6KVAG01*F(W6H7B1KY>=' W-W6H7B1KY-MIN-X                      
094700                    '&W6H7B1KY<=' W-W6H7B1KY-MAX-X ')'                    
094800          DELIMITED BY SIZE INTO SSA1                                     
094900     MOVE '  GE' TO GODK-STATUSKODER                                      
095000     CALL CBLTDLI USING GU KVAG-PCB DLI-IO-AREA SSA1                      
095100     MOVE KVAG-STATUS-CODE TO STATUS-WS                                   
095200     PERFORM IMS-STATUSKONTROLL                                           
095300     .                                                                    
095400     SKIP3                                                                
095500 IMS-GN-KVAG-W6KVAG01 SECTION.                                            
095600     STRING 'W6KVAG01(W6H7B1KY>=' W-W6H7B1KY-MIN-X                        
095700                    '&W6H7B1KY<=' W-W6H7B1KY-MAX-X ')'                    
095800          DELIMITED BY SIZE INTO SSA1                                     
095900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
096000     CALL CBLTDLI USING GN KVAG-PCB DLI-IO-AREA SSA1                      
096100     MOVE KVAG-STATUS-CODE TO STATUS-WS                                   
096200     PERFORM IMS-STATUSKONTROLL                                           
096300     .                                                                    
096400     EJECT                                                                
096500 IMS-GU-KVAE-W6KVAE01 SECTION.                                            
096600     STRING 'W6KVAE01(IDKR     =' W-W6H701KY-X ')'                        
096700          DELIMITED BY SIZE INTO SSA1                                     
096800     MOVE '    ' TO GODK-STATUSKODER                                      
096900     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA5 SSA1                     
097000     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
097100     PERFORM IMS-STATUSKONTROLL                                           
097200     .                                                                    
097300     EJECT                                                                
097400 IMS-GU-KATN-WLKATN01 SECTION.                                            
097500                                                                          
097600     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
097700     DELIMITED BY SIZE INTO SSA1                                          
097800     MOVE '  GE' TO GODK-STATUSKODER                                      
097900     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA SSA1                      
098000     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
098100     PERFORM IMS-STATUSKONTROLL                                           
098200     .                                                                    
098300     EJECT                                                                
098400 IMS-GNP-KATN-WLKATN11 SECTION.                                           
098500                                                                          
098600     STRING 'WLKATN11(WDN611KY>=' W-WDN611KY-X ')'                        
098700     DELIMITED BY SIZE INTO SSA1                                          
098800     MOVE '  GE' TO GODK-STATUSKODER                                      
098900     CALL CBLTDLI USING GNP KATN-PCB DLI-IO-AREA SSA1                     
099000     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
099100     PERFORM IMS-STATUSKONTROLL                                           
099200     .                                                                    
099300     EJECT                                                                
099400 IMS-GU-WDB601    SECTION.                                                
099500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
099600          DELIMITED BY SIZE INTO SSA1                                     
099700     MOVE '  '   TO GODK-STATUSKODER                                      
099800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
099900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
100000     PERFORM IMS-STATUSKONTROLL                                           
100100     .                                                                    
100200     SKIP3                                                                
100300 IMS-STATUSKONTROLL SECTION.                                              
100400                                                                          
100500     SET STATUS-IX TO 1                                                   
100600     SEARCH GODK-STATUS                                                   
100700       AT END                                                             
100800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
100900         DELIMITED BY SIZE INTO FELTEXT                                   
101000         CALL FELLOG                                                      
101100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
101200         CONTINUE                                                         
101300     END-SEARCH                                                           
101400     .                                                                    
