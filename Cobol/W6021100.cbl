000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6021100.                                                
000400*AUTHOR.         GERRY CARMICHAEL.                                        
000500*DATE-WRITTEN.   92/07/20.                                                
000600*DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        KVALITETSKONTROLL                                                
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR W6KVAH (W6D2)                              
001200*                              WLARTC (WDK6)                              
001300*        PROGRAMMET LÄSER      W6KVAG (W6H7B)                             
001400*                              WLBENA (WDD3)                              
001500*                              W6PROA (W6G1)                              
001600*                              WLKATN (WDN6)                              
001700*                              WLARTG (WDD2)                              
001800*                                                                         
001900*    INDATA:                                                              
002000*        TRANSAKTION: W6T211                                              
002100*        MID:         W6I21101                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W6O21101                                            
002500*                                                                         
002600*    ÄNDRING:                                                             
002700*        97-02-05 C.E./  SDC/NDC.  KDLEVSP - LEVERANSSPÄRR                
002800*                        OCH TEARTNOT- SPÄRRKODSNOTERING                  
002900*        SKALL NUMERA BARA VISAS. UPPDATERING PÅ 6308 ISTÄLLET.           
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W6021100'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005100 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005200 77  WS-IDKVAINF                 PIC X(2)    VALUE SPACE.                 
005300 77  WS-TIREGDAT                 PIC X(6)    VALUE SPACE.                 
005400 77  WS-KDKVAINF                 PIC X(1)    VALUE SPACE.                 
005500 77  WS-KDKVAKTL                 PIC X(4)    VALUE SPACE.                 
005600 77  WS-KDKVATYP                 PIC X(1)    VALUE SPACE.                 
005700 77  SPAR-KDKVAKTL               PIC X(4)    VALUE '0000'.                
005800 77  WS-IDPROVPL-PRI             PIC X(1)    VALUE SPACE.                 
005900 77  WS-IDPROVPL-SEK             PIC X(1)    VALUE SPACE.                 
006000 77  WS-KDKVAULG                 PIC X(1)    VALUE SPACE.                 
006100 77  WS-ADKVAULG                 PIC X(2)    VALUE SPACE.                 
006200 77  MAX-ANT-KAT                 PIC S9(9)   VALUE +11  COMP SYNC.        
006300 77  MAX-ANT-LIK                 PIC S9(9)   VALUE +4   COMP SYNC.        
006400 77  LIK-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006500 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
006600 77  CL-INDX                     PIC S9(9)   VALUE +0   COMP SYNC.        
006700 77  MAX-CL-INDX                 PIC S9(9)   VALUE +2   COMP SYNC.        
006800 77  NUM-KDKVAULG                PIC 9(1)    VALUE ZERO.                  
006900 77  NUM-KDKVATYP                PIC 9(1)    VALUE ZERO.                  
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
009300 77  KKOD-SW                     PIC X       VALUE 'J'.                   
009400     88  KKOD-OK                             VALUE 'J'.                   
009500     88  KKOD-FEL                            VALUE 'N'.                   
009600                                                                          
009700 77  PROVPL-PRI-SW               PIC X       VALUE 'J'.                   
009800     88  PROVPL-PRI-OK                       VALUE 'J'.                   
009900     88  PROVPL-PRI-FEL                      VALUE 'N'.                   
010000                                                                          
010100 77  PROVPL-SEK-SW               PIC X       VALUE 'J'.                   
010200     88  PROVPL-SEK-OK                       VALUE 'J'.                   
010300     88  PROVPL-SEK-FEL                      VALUE 'N'.                   
010400                                                                          
010500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010600     88  NYCKLAR-OK                          VALUE 'J'.                   
010700     88  NYCKLAR-FEL                         VALUE 'N'.                   
010800                                                                          
010900 77  NYUPPLAEGG-SW               PIC X       VALUE 'N'.                   
011000     88  NYUPPLAEGG                          VALUE 'J'.                   
011100                                                                          
011200 77  SKIPLOT-SW                  PIC X       VALUE 'N'.                   
011300     88  SKIPLOT-UPDATE                      VALUE 'J'.                   
011400                                                                          
011500 77  KDKVATYP-SW                 PIC X       VALUE 'N'.                   
011600     88  KDKVATYP-UPDATE                     VALUE 'J'.                   
011700                                                                          
011800 77  ANNULLERAD-SW               PIC X       VALUE 'N'.                   
011900     88  ANNULLERAD-FINNS                    VALUE 'J'.                   
012000                                                                          
012100 77  KR-FINNS-SW                 PIC X       VALUE 'N'.                   
012200     88  KR-FINNS                            VALUE 'J'.                   
012300     88  KR-FINNS-INTE                       VALUE 'N'.                   
012400                                                                          
012500                                                                          
012600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012700     88  EGEN-MID                            VALUE '6211'.                
012800     88  GODK-MID                            VALUE '6211' '6212'          
012900                                                   '6213' '6214'          
013000                                                   '6215' '6216'          
013100                                                   '6217' '6218'          
013200                                                   '6219'.                
013300     88  HELP-MID                            VALUE '0551'.                
013400                                                                          
013500 01    WS-SATS       PIC X.                                               
013600         88  SAKNAS-I-SATS       VALUE 'N'.                               
013700                                                                          
013800 01      FILLER.                                                          
013900   03      WS-KDYTBEH            PIC X(2)   VALUE SPACE.                  
014000   03      FILLER REDEFINES WS-KDYTBEH.                                   
014100     05      KDYTBEH1            PIC X.                                   
014200     05      KDYTBEH2            PIC X.                                   
014300                                                                          
014400 01  WS-TID                      PIC S9(9).                               
014500 01  WS-DAREGDAT                 PIC 9(8).                                
014600     EJECT                                                                
014700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014800 01  GENERELLA-SUBPROGRAM.                                                
014900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015300                                                                          
015400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015500*01 -COPY WMSGINIT                                                        
015600     EJECT                                                                
015700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015800*01 -COPY WMEDAREA                                                        
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
017400*    --- UNDERLAG FÖR KVALITETSKONTROLL                                   
017500*    -COPY W426KTL                                                        
017600     EJECT                                                                
017700*                                                                         
017800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017900*                                                                         
018000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018100     SKIP3                                                                
018200*01  MID -COPY W6I21101                                                   
018300     EJECT                                                                
018400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018500     SKIP3                                                                
018600*01  -COPY WMSGAREA                                                       
018700     EJECT                                                                
018800     03  MOD REDEFINES MSG-AREA.                                          
018900*      05  -COPY W6O21101                                                 
019000     EJECT                                                                
019100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019200     SKIP3                                                                
019300*01  -COPY WMFSAREA                                                       
019400     EJECT                                                                
019500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019600*                                                                         
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019900     SKIP3                                                                
020000 01  NYCKLAR-TILL-DLI.                                                    
020100                                                                          
020200     03  W-IDARTNR-X.                                                     
020300         05 W-IDARTNR            PIC S9(9)  VALUE ZERO COMP-3.            
020400     03  W-KDNOTTYP-X.                                                    
020500         05 W-KDNOTTYP           PIC S9(1)  VALUE ZERO  COMP-3.           
020600     03  W-IDSKYLT-KEY-X.                                                 
020700         05 W-IDSKYLT-KEY        PIC X(3)   VALUE SPACE.                  
020800     03  W-W6D211KY-X.                                                    
020900         05  W-DAREGDAT-9KOMPL   PIC 9(8)   VALUE ZERO.                   
021000         05  W-TIKLOCK-9KOMPL    PIC S9(9)  VALUE ZERO COMP-3.            
021100     03  W-IDLEVNR-X.                                                     
021200         05  W-IDLEVNR           PIC  X(5)  VALUE SPACE.                  
021300     03  W-IDLIKARE-X.                                                    
021400         05  W-IDLIKARE          PIC X(9)   VALUE SPACE.                  
021500     03  W-W6H701KY-X.                                                    
021600         05  W-IDKR              PIC 9(5)  VALUE ZERO.                    
021700     03  W-W6H7B1KY-MIN-X.                                                
021800         05  W-SEQB-IDARTNR-MIN  PIC S9(9)  VALUE ZERO COMP-3.            
021900         05  W-SEQB-DAREGDAT-9KOMPL-MIN                                   
022000                                 PIC  9(8)   VALUE ZERO.                  
022100         05  W-SEQB-IDLEVNR-MIN  PIC  X(5)  VALUE SPACE.                  
022200         05  W-SEQB-KVKRKNTR-MIN PIC S9(1)  VALUE ZERO COMP-3.            
022300         05  W-SEQB-IDKR-MIN     PIC  9(5)  VALUE ZERO.                   
022400     03  W-W6H7B1KY-MAX-X.                                                
022500         05  W-SEQB-IDARTNR-MAX  PIC S9(9)  VALUE ZERO COMP-3.            
022600         05  W-SEQB-DAREGDAT-9KOMPL-MAX                                   
022700                                 PIC  9(8)   VALUE ZERO.                  
022800         05  W-SEQB-IDLEVNR-MAX  PIC  X(5)  VALUE SPACE.                  
022900         05  W-SEQB-KVKRKNTR-MAX PIC S9(1)  VALUE ZERO COMP-3.            
023000         05  W-SEQB-IDKR-MAX     PIC  9(5)  VALUE ZERO.                   
023100     03  W-WDGX-4505-KEY-X.                                               
023200         05 W-IDHTYP-4505        PIC X(04)  VALUE '4505'.                 
023300         05 FILLER               PIC X(26)  VALUE LOW-VALUE.              
023400     03  W-W6GX-6101-KEY-X.                                               
023500         05 W-IDHTYP-6101        PIC X(04)  VALUE '6101'.                 
023600         05 FILLER               PIC X(26)  VALUE LOW-VALUE.              
023700     03  W-W6GX-6102-KEY-X.                                               
023800         05 W-IDPROVPL-X.                                                 
023900           07 W-IDPROVPL         PIC 9(1).                                
024000         05 W-KDPROVPL-X.                                                 
024100           07 W-KDPROVPL         PIC X(1)   VALUE SPACE.                  
024200     03  W-WDN611KY-X.                                                    
024300         05 W-IDFORDON           PIC S9(3)  VALUE ZERO COMP-3.            
024400         05 W-TIOMBRYT-1         PIC S9(7)  VALUE ZERO COMP-3.            
024500     03  W-IDDC-B6-X.                                                     
024600         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
024700     SKIP2                                                                
024800*    --- STATUS-KOD FRÅN IMS                                              
024900 01  STATUS-WS                   PIC XX.                                  
025000     88  SEGMENT-FINNS                       VALUE '  '.                  
025100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025300     SKIP2                                                                
025400 01  GODK-STATUSKODER.                                                    
025500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025600     SKIP3                                                                
025700 01  SSA1                        PIC X(96).                               
025800 01  SSA2                        PIC X(64).                               
025900 01  SSA3                        PIC X(64).                               
026000     EJECT                                                                
026100*    --- IMS FUNKTIONSKODER                                               
026200*01  -COPY W0003                                                          
026300     EJECT                                                                
026400*    ---  DLI INPUT-OUTPUT AREA                                           
026500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026600     SKIP3                                                                
026700 01  DLI-IO-AREA.                                                         
026800     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
026900     SKIP3                                                                
027000     03  W6KVAG01 REDEFINES IO-AREA.                                      
027100*        05  -COPY W6H7B1                                                 
027200     SKIP3                                                                
027300     03  WDK601 REDEFINES IO-AREA.                                        
027400*        05  -COPY WDK601                                                 
027500     SKIP3                                                                
027600     03  WDK611 REDEFINES IO-AREA.                                        
027700*        05  -COPY WDK611                                                 
027800     SKIP3                                                                
027900     03  WDK625 REDEFINES IO-AREA.                                        
028000*        05  -COPY WDK625                                                 
028100     SKIP3                                                                
028200     03  WLBENA01 REDEFINES IO-AREA.                                      
028300*        05  -COPY WDD301                                                 
028400     SKIP3                                                                
028500     03  WLBENA11 REDEFINES IO-AREA.                                      
028600*        05  -COPY WDD311                                                 
028700     SKIP3                                                                
028800     03  WLKATN01 REDEFINES IO-AREA.                                      
028900*        05  -COPY WDN601                                                 
029000     SKIP3                                                                
029100     03  WLKATN11 REDEFINES IO-AREA.                                      
029200*        05  -COPY WDN611                                                 
029300     EJECT                                                                
029400*                            DLI INPUT-OUTPUT AREA3                       
029500 01  DLI-IO-AREA3.                                                        
029600     SKIP3                                                                
029700*    03  W6PROA11 -COPY W6GX6102                                          
029800     SKIP3                                                                
029900*                            DLI INPUT-OUTPUT AREA4                       
030000 01  DLI-IO-AREA4.                                                        
030100     03  IO-AREA4                PIC X(1154)  VALUE SPACE.                
030200     03  W6KVAH01 REDEFINES IO-AREA4.                                     
030300*        05  -COPY W6D201   -PRE KVAH-                                    
030400     SKIP3                                                                
030500     03  W6KVAH11 REDEFINES IO-AREA4.                                     
030600*        05  -COPY W6D211                                                 
030700     SKIP3                                                                
030800     03  W6KVAH12 REDEFINES IO-AREA4.                                     
030900*        05  -COPY W6D212                                                 
031000     SKIP3                                                                
031100     03  W6KVAH13 REDEFINES IO-AREA4.                                     
031200*        05  -COPY W6D213                                                 
031300     EJECT                                                                
031400*                            DLI INPUT-OUTPUT AREA5                       
031500 01  DLI-IO-AREA5.                                                        
031600     03  IO-AREA5                PIC X(600)  VALUE SPACE.                 
031700     03  W6KVAE01 REDEFINES IO-AREA5.                                     
031800*        05  -COPY W6H701                                                 
031900     EJECT                                                                
032000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
032100 01  DLI-IO-WDK701.                                                       
032200*    03  -COPY WDK701                                                     
032300     EJECT                                                                
032400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
032500 01  DLI-IO-WDK711.                                                       
032600*    03  -COPY WDK711                                                     
032700     EJECT                                                                
032800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032900 01   DLI-IO-AREA-B601.                                                   
033000*     03  -COPY WDB601                                                    
033100     EJECT                                                                
033200 01  FILLER               PIC X(16)   VALUE 'WDD201 AREA'.                
033300 01   DLI-IO-AREA-WDD2.                                                   
033400*     03  -COPY WDD201    -PRE ARTG-                                      
033500     EJECT                                                                
033600 LINKAGE SECTION.                                                         
033700                                                                          
033800*01  -COPY W0009  -PRE MSG-                                               
033900     EJECT                                                                
034000*01  -COPY W0008  -PRE USEA-                                              
034100     05  FILLER                  PIC X.                                   
034200     EJECT                                                                
034300*01  -COPY W0008  -PRE ARTC-                                              
034400     05  FILLER                  PIC X.                                   
034500     EJECT                                                                
034600*01  -COPY W0008  -PRE WDK7-                                              
034700     05  FILLER                  PIC X.                                   
034800     EJECT                                                                
034900*01  -COPY W0008  -PRE BENA-                                              
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200*01  -COPY W0008  -PRE KATN-                                              
035300     05  FILLER                  PIC X.                                   
035400*01  -COPY W0008  -PRE KVAH-                                              
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008  -PRE KVAG-                                              
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008  -PRE PROA-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE KVAE-                                              
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600*01  -COPY W0008      -PRE WDB6-                                          
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01  -COPY W0008      -PRE WDD2-                                          
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB                               
037300                                   ARTC-PCB WDK7-PCB                      
037400                          BENA-PCB KATN-PCB KVAH-PCB KVAG-PCB             
037500                          PROA-PCB KVAE-PCB WDB6-PCB WDD2-PCB.            
037600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
037700                                   ARTC-PCB WDK7-PCB                      
037800                          BENA-PCB KATN-PCB KVAH-PCB KVAG-PCB             
037900                          PROA-PCB KVAE-PCB WDB6-PCB WDD2-PCB.            
038000                                                                          
038100     PERFORM IMS-GET-MSG                                                  
038200     IF SEGMENT-FINNS                                                     
038300       PERFORM A-INIT                                                     
038400       PERFORM B-KOLLA-NYCKLAR                                            
038500       IF NYCKLAR-OK                                                      
038600         IF MFS-UPDATE  OR MFS-UPD-V                                      
038700           PERFORM G-KOLLA-INPUT                                          
038800           IF INDATA-OK                                                   
038900             PERFORM H-UPPDATERA                                          
039000           END-IF                                                         
039100         ELSE                                                             
039200           IF MFS-FIRST                                                   
039300             PERFORM C-FOERSTA-SIDA                                       
039400           ELSE                                                           
039500             IF MFS-NEXT                                                  
039600               PERFORM D-NAESTA-SIDA                                      
039700             ELSE                                                         
039800               PERFORM E-SAMMA-SIDA                                       
039900             END-IF                                                       
040000           END-IF                                                         
040100         END-IF                                                           
040200         IF INDATA-OK                                                     
040300           PERFORM F-LAES-VISA-INFO                                       
040400         END-IF                                                           
040500       END-IF                                                             
040600       MOVE LENGTH OF MOD-W6O21101  TO MSG-KVLL                           
040700       ADD         +4               TO MSG-KVLL                           
040800       PERFORM IMS-INSERT-MSG                                             
040900     END-IF                                                               
041000                                                                          
041100     MOVE ZERO TO RETURN-CODE                                             
041200     GOBACK                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 A-INIT SECTION.                                                          
041600     IF MSG-DUBBLA-TRANSKODER                                             
041700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I21101                 
041800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
041900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
042000     ELSE                                                                 
042100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I21101                  
042200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
042300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
042400     END-IF                                                               
042500                                                                          
042600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
042700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
042800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
042900                                                                          
043000     MOVE LOW-VALUE   TO MSG-AREA                                         
043100     MOVE  'W6O211N1' TO MFS-IDMOD                                        
043200     MOVE '6211'      TO MOD-IDTRANS                                      
043300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
043400                                                                          
043500     IF EGEN-MID OR HELP-MID                                              
043600       CONTINUE                                                           
043700     ELSE                                                                 
043800       MOVE SPACE TO MFS-KDTRTYP                                          
043900       MOVE '7' TO MFS-IDPFK                                              
044000     END-IF                                                               
044100     .                                                                    
044200     EJECT                                                                
044300 B-KOLLA-NYCKLAR SECTION.                                                 
044400                                                                          
044500     MOVE JA  TO NYCKLAR-SW                                               
044600                 INDATA-SW                                                
044700                 PROVPL-PRI-SW                                            
044800                 PROVPL-SEK-SW                                            
044900     MOVE NEJ TO SKIPLOT-SW                                               
045000                 KDKVATYP-SW                                              
045100                 NYUPPLAEGG-SW                                            
045200                                                                          
045300     MOVE LOW-VALUE       TO W-W6H7B1KY-MIN-X                             
045400     MOVE HIGH-VALUE      TO W-W6H7B1KY-MAX-X                             
045500                                                                          
045600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
045700                             MOD-IDLEVNR-IN                               
045800                             MOD-IDKVAINF-IN                              
045900                             MOD-TIREGDAT-IN                              
046000                             MOD-KDKVAINF-IN                              
046100                                                                          
046200                                                                          
046300     MOVE ALL '+' TO MSGI-WMSGINIT                                        
046400     MOVE '001'             TO MSGI-KDCALL                                
046500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
046600     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
046700     MOVE '6211'                 TO MSGI-IDTRANS                          
046800                                                                          
046900     IF MFS-IDTRANS = '6211'                                              
047000     OR (MID-IDARTNR-IN NUMERIC                                           
047100     AND MID-IDARTNR-IN > ZERO)                                           
047200         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
047300     END-IF                                                               
047400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047500                                                                          
047600     IF MSGI-IDLAND-SPR = 'GB'                                            
047700       MOVE +2 TO SPRAK-IX                                                
047800       MOVE 'GB ' TO MED-IDSKYLT                                          
047900     ELSE                                                                 
048000       MOVE +1 TO SPRAK-IX                                                
048100       MOVE 'S  ' TO MED-IDSKYLT                                          
048200     END-IF                                                               
048300                                                                          
048400     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
048500     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
048600                                                                          
048700     IF MID-IDARTNR-IN = ALL '+'                                          
048800       CONTINUE                                                           
048900     ELSE                                                                 
049000       MOVE '7'         TO MFS-IDPFK                                      
049100       MOVE SPACE       TO MFS-KDTRTYP                                    
049200     END-IF                                                               
049300                                                                          
049400     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
049500       MOVE WS-IDARTNR TO W-IDARTNR                                       
049600                          W-SEQB-IDARTNR-MIN                              
049700                          W-SEQB-IDARTNR-MAX                              
049800     ELSE                                                                 
049900       MOVE NEJ TO NYCKLAR-SW                                             
050000     END-IF                                                               
050100                                                                          
050200     IF MID-IDLEVNR-IN = ALL '+'                                          
050300       MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                                  
050400     ELSE                                                                 
050500       MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                  
050600     END-IF                                                               
050700                                                                          
050800     IF MID-IDKVAINF-IN = ALL '+'                                         
050900       MOVE MID-IDKVAINF-UT TO WS-IDKVAINF                                
051000       INSPECT WS-IDKVAINF REPLACING LEADING SPACE BY ZERO                
051100     ELSE                                                                 
051200       MOVE MID-IDKVAINF-IN TO WS-IDKVAINF                                
051300     END-IF                                                               
051400                                                                          
051500     IF MID-TIREGDAT-IN = ALL '+'                                         
051600       MOVE MID-TIREGDAT-UT TO WS-TIREGDAT                                
051700       INSPECT WS-TIREGDAT REPLACING LEADING SPACE BY ZERO                
051800     ELSE                                                                 
051900       MOVE MID-TIREGDAT-IN TO WS-TIREGDAT                                
052000     END-IF                                                               
052100                                                                          
052200     IF MID-KDKVAINF-IN = ALL '+'                                         
052300       MOVE MID-KDKVAINF-UT TO WS-KDKVAINF                                
052400       INSPECT WS-KDKVAINF REPLACING LEADING SPACE BY ZERO                
052500     ELSE                                                                 
052600       MOVE MID-KDKVAINF-IN TO WS-KDKVAINF                                
052700     END-IF                                                               
052800                                                                          
052900     IF NYCKLAR-OK                                                        
053000       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
053100       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
053200       IF NOT EGEN-MID                                                    
053300         MOVE +1 TO INDX                                                  
053400         PERFORM UNTIL INDX > MAX-ANT-LIK                                 
053500           MOVE '+++++++++' TO MID-IDLIKARE(INDX)                         
053600           ADD +1 TO INDX                                                 
053700         END-PERFORM                                                      
053800       END-IF                                                             
053900       IF MID-IDARTNR-IN NOT = ALL '+'                                    
054000         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                           
054100                                 MOD-IDKVAINF-UT                          
054200                                 MOD-TIREGDAT-UT                          
054300                                 MOD-KDKVAINF-UT                          
054400       ELSE                                                               
054500         MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                
054600         MOVE WS-IDKVAINF TO MOD-IDKVAINF-UT                              
054700         INSPECT MOD-IDKVAINF-UT REPLACING LEADING ZERO BY SPACE          
054800         MOVE WS-TIREGDAT TO MOD-TIREGDAT-UT                              
054900         INSPECT MOD-TIREGDAT-UT REPLACING LEADING ZERO BY SPACE          
055000         MOVE WS-KDKVAINF TO MOD-KDKVAINF-UT                              
055100         INSPECT MOD-KDKVAINF-UT REPLACING LEADING ZERO BY SPACE          
055200       END-IF                                                             
055300     ELSE                                                                 
055400       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
055500                               MOD-IDLEVNR-UT                             
055600                               MOD-IDKVAINF-UT                            
055700                               MOD-TIREGDAT-UT                            
055800                               MOD-KDKVAINF-UT                            
055900     END-IF                                                               
056000                                                                          
056100     IF NYCKLAR-FEL                                                       
056200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
056300       CALL WMEDKONV USING MED-WMEDAREA                                   
056400       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
056500       PERFORM MFS-RENSA-ALLA-FAELT-IN                                    
056600       PERFORM MFS-RENSA-FAELT-UT                                         
056700     END-IF                                                               
056800     .                                                                    
056900     EJECT                                                                
057000 C-FOERSTA-SIDA SECTION.                                                  
057100                                                                          
057200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
057300     CALL WMEDKONV USING MED-WMEDAREA                                     
057400     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
057500                                                                          
057600*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
057700     MOVE ZERO       TO MOD-IDFORDON-ENTER                                
057800                        MOD-IDFORDON-NEXT                                 
057900                        MOD-TIOMBRYT-1-ENTER                              
058000                        MOD-TIOMBRYT-1-NEXT                               
058100     PERFORM MFS-RENSA-ALLA-FAELT-IN                                      
058200     PERFORM MFS-RENSA-FAELT-UT                                           
058300     .                                                                    
058400     EJECT                                                                
058500 D-NAESTA-SIDA SECTION.                                                   
058600                                                                          
058700     MOVE MID-IDFORDON-NEXT   TO W-IDFORDON                               
058800     MOVE MID-TIOMBRYT-1-NEXT TO W-TIOMBRYT-1                             
058900     PERFORM MFS-RENSA-ALLA-FAELT-IN                                      
059000     PERFORM MFS-RENSA-FAELT-UT                                           
059100     .                                                                    
059200     EJECT                                                                
059300 E-SAMMA-SIDA SECTION.                                                    
059400                                                                          
059500     IF EGEN-MID OR HELP-MID                                              
059600       MOVE MID-IDFORDON-ENTER TO W-IDFORDON                              
059700       MOVE MID-TIOMBRYT-1-ENTER TO W-TIOMBRYT-1                          
059800                                                                          
059900       IF MID-INPUT1 = ALL '+' AND MID-INPUT2 = ALL '+' AND               
060000          MID-INPUT3 = ALL '+'                                            
060100         PERFORM MFS-RENSA-ALLA-FAELT-IN                                  
060200       ELSE                                                               
060300         IF MID-INPUT2 NOT = ALL '+'                                      
060400           MOVE INF-PRESS-PF23 TO MED-IDMFSINF                            
060500         ELSE                                                             
060600           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
060700         END-IF                                                           
060800         CALL WMEDKONV USING MED-WMEDAREA                                 
060900         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
061000         PERFORM EA-MID-INDATA-TILL-MOD                                   
061100       END-IF                                                             
061200     ELSE                                                                 
061300       PERFORM MFS-RENSA-ALLA-FAELT-IN                                    
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700 EA-MID-INDATA-TILL-MOD SECTION.                                          
061800                                                                          
061900* * * * * FÖR VARJE MID-FÄLT                                              
062000* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
062100* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
062200* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
062300                                                                          
062400* TEARTNOT2 SKALL NUMERA UPPDATERAS FRÅN BILD 6308                        
062500* KDLEVSP SKALL NUMERA UPPDATERAS FRÅN BILD 6308                          
062600                                                                          
062700     IF MID-KDYTBEH   NOT = ALL '+'                                       
062800       MOVE MID-KDYTBEH   TO MOD-KDYTBEH-IN                               
062900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDYTBEH-IN-ATTR                  
063000     ELSE                                                                 
063100       MOVE MFS-RENSA-FAELT       TO MOD-KDYTBEH-IN                       
063200     END-IF                                                               
063300                                                                          
063400     IF MID-KDFARLIG  NOT = ALL '+'                                       
063500       MOVE MID-KDFARLIG  TO MOD-KDFARLIG-IN                              
063600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFARLIG-IN-ATTR                 
063700     ELSE                                                                 
063800       MOVE MFS-RENSA-FAELT       TO MOD-KDFARLIG-IN                      
063900     END-IF                                                               
064000                                                                          
064100     IF MID-IDLIKARE(1) NOT = ALL '+'                                     
064200       MOVE MID-IDLIKARE(1) TO MOD-IDLIKARE(1)                            
064300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLIKARE-ATTR(1)                 
064400     ELSE                                                                 
064500       MOVE MFS-RENSA-FAELT       TO MOD-IDLIKARE-ATTR(1)                 
064600     END-IF                                                               
064700                                                                          
064800     IF MID-IDLIKARE(2) NOT = ALL '+'                                     
064900       MOVE MID-IDLIKARE(2) TO MOD-IDLIKARE(2)                            
065000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLIKARE-ATTR(2)                 
065100     ELSE                                                                 
065200       MOVE MFS-RENSA-FAELT       TO MOD-IDLIKARE-ATTR(2)                 
065300     END-IF                                                               
065400                                                                          
065500     IF MID-IDLIKARE(3) NOT = ALL '+'                                     
065600       MOVE MID-IDLIKARE(3) TO MOD-IDLIKARE(3)                            
065700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLIKARE-ATTR(3)                 
065800     ELSE                                                                 
065900       MOVE MFS-RENSA-FAELT       TO MOD-IDLIKARE-ATTR(3)                 
066000     END-IF                                                               
066100                                                                          
066200     IF MID-IDLIKARE(4) NOT = ALL '+'                                     
066300       MOVE MID-IDLIKARE(4) TO MOD-IDLIKARE(4)                            
066400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLIKARE-ATTR(4)                 
066500     ELSE                                                                 
066600       MOVE MFS-RENSA-FAELT       TO MOD-IDLIKARE-ATTR(4)                 
066700     END-IF                                                               
066800                                                                          
066900     IF MID-KDKVATYP  NOT = ALL '+'                                       
067000       MOVE MID-KDKVATYP TO MOD-KDKVATYP                                  
067100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKVATYP-ATTR                    
067200     ELSE                                                                 
067300       MOVE MFS-RENSA-FAELT       TO MOD-KDKVATYP                         
067400     END-IF                                                               
067500                                                                          
067600     IF MID-IDPROVPL-PRI NOT = ALL '+'                                    
067700       MOVE MID-IDPROVPL-PRI TO MOD-IDPROVPL-PRI                          
067800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPROVPL-PRI-ATTR                
067900     ELSE                                                                 
068000       MOVE MFS-RENSA-FAELT       TO MOD-IDPROVPL-PRI                     
068100     END-IF                                                               
068200                                                                          
068300     IF MID-IDPROVPL-SEK NOT = ALL '+'                                    
068400       MOVE MID-IDPROVPL-PRI TO MOD-IDPROVPL-SEK                          
068500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPROVPL-SEK-ATTR                
068600     ELSE                                                                 
068700       MOVE MFS-RENSA-FAELT       TO MOD-IDPROVPL-SEK                     
068800     END-IF                                                               
068900                                                                          
069000     IF MID-KDKVAULG     NOT = ALL '+'                                    
069100       MOVE MID-KDKVAULG     TO MOD-KDKVAULG                              
069200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKVAULG-ATTR                    
069300     ELSE                                                                 
069400       MOVE MFS-RENSA-FAELT       TO MOD-KDKVAULG                         
069500     END-IF                                                               
069600                                                                          
069700     IF MID-ADKVAULG     NOT = ALL '+'                                    
069800       MOVE MID-ADKVAULG     TO MOD-ADKVAULG                              
069900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADKVAULG-ATTR                    
070000     ELSE                                                                 
070100       MOVE MFS-RENSA-FAELT       TO MOD-ADKVAULG                         
070200     END-IF                                                               
070300                                                                          
070400     .                                                                    
070500     EJECT                                                                
070600 F-LAES-VISA-INFO SECTION.                                                
070700                                                                          
070800     PERFORM IMS-GU-ARTC-WDK601                                           
070900     IF SEGMENT-FINNS                                                     
071000       MOVE  ART-IDFKNGRP        TO MOD-IDFKNGRP                          
071100       MOVE  ART-KDSORT          TO MOD-KDSORT                            
071200       MOVE  ART-IDLEVNR         TO MOD-IDLEVNR                           
071300       IF ART-KDERS-UTG NOT > 0                                           
071400         PERFORM FA-BEHANDLA-ARTC                                         
071500         PERFORM FD-BEHANDLA-WDK7                                         
071600       ELSE                                                               
071700         MOVE ART-KDERS-UTG      TO MOD-KDERS                             
071800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDERS-ATTR                     
071900         PERFORM MFS-STAENG-FAELT-IN                                      
072000         MOVE ERR-ART-NO-MORE TO MED-IDMFSFEL                             
072100         CALL WMEDKONV USING MED-WMEDAREA                                 
072200         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
072300       END-IF                                                             
072400                                                                          
072500       PERFORM IMS-GU-BENA-WLBENA01                                       
072600       IF SEGMENT-FINNS                                                   
072700         MOVE 'GB ' TO W-IDSKYLT-KEY                                      
072800         PERFORM IMS-GNP-BENA-WLBENA11                                    
072900         IF SEGMENT-FINNS                                                 
073000           MOVE TEXT-BEART TO MOD-BEART-ENG                               
073100         END-IF                                                           
073200         MOVE 'S  ' TO W-IDSKYLT-KEY                                      
073300         PERFORM IMS-GNP-BENA-WLBENA11                                    
073400         IF SEGMENT-FINNS                                                 
073500           MOVE TEXT-BEART TO MOD-BEART-SVE                               
073600         END-IF                                                           
073700       END-IF                                                             
073800                                                                          
073900       PERFORM IMS-GET-WDD201                                             
074000       IF SEGMENT-FINNS                                                   
074100          MOVE ARTG-ART-KDTPD TO MOD-KDTPD                                
074200          IF ARTG-ART-TITPD = ZERO                                        
074300             MOVE MFS-RENSA-FAELT TO MOD-TITPD                            
074400          ELSE                                                            
074500             MOVE ARTG-ART-TITPD TO MOD-TITPD                             
074600          END-IF                                                          
074700          IF ARTG-ART-FLUPG = 'J'                                         
074800             MOVE MFS-RENSA-FAELT TO MOD-FLUPG                            
074900                                     MOD-TIUPG                            
075000          ELSE                                                            
075100             IF ARTG-ART-FLUPG = 'N'                                      
075200                IF ARTG-ART-TIUPG = ZERO                                  
075300                   MOVE ARTG-ART-FLUPG TO MOD-FLUPG                       
075400                   MOVE MFS-RENSA-FAELT TO MOD-TIUPG                      
075500                ELSE                                                      
075600                   MOVE MFS-RENSA-FAELT TO MOD-FLUPG                      
075700                                           MOD-TIUPG                      
075800                END-IF                                                    
075900             ELSE                                                         
076000                MOVE ARTG-ART-FLUPG TO MOD-FLUPG                          
076100                IF ARTG-ART-TIUPG = ZERO                                  
076200                   MOVE MFS-RENSA-FAELT TO MOD-TIUPG                      
076300                ELSE                                                      
076400                   MOVE ARTG-ART-TIUPG TO MOD-TIUPG                       
076500                END-IF                                                    
076600             END-IF                                                       
076700          END-IF                                                          
076800       END-IF                                                             
076900                                                                          
077000       PERFORM FB-BEHANDLA-KATN11                                         
077100                                                                          
077200       PERFORM FC-BEHANDLA-KVAH01                                         
077300     ELSE                                                                 
077400       MOVE ERR-ART-MISSING TO MED-IDMFSFEL                               
077500       CALL WMEDKONV USING MED-WMEDAREA                                   
077600       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
077700       PERFORM MFS-RENSA-FAELT-UT                                         
077800     END-IF                                                               
077900     .                                                                    
078000     EJECT                                                                
078100 FA-BEHANDLA-ARTC SECTION.                                                
078200                                                                          
078300     PERFORM IMS-GNP-ARTC-WDK611                                          
078400     IF SEGMENT-FINNS                                                     
078500       MOVE CLAG-IDANSK       TO MOD-IDANSK                               
078600       MOVE CLAG-IDBERED      TO MOD-IDBERED                              
078700       MOVE CLAG-IDRITN       TO MOD-IDRITN                               
078800       MOVE CLAG-KVPB-SEP     TO MOD-KVPB-SEP (1)                         
078900                                                                          
079000       MOVE CLAG-KVPB-SATS    TO MOD-KVPB-SATS                            
079100       IF     CLAG-KDYTBEH  NUMERIC                                       
079200         MOVE CLAG-KDYTBEH    TO MOD-KDYTBEH-UT                           
079300       ELSE                                                               
079400         MOVE ZERO            TO MOD-KDYTBEH-UT                           
079500       END-IF                                                             
079600       MOVE   CLAG-PRARTSTD   TO MOD-PRARTSTD                             
079700     END-IF                                                               
079800                                                                          
079900     MOVE CLAG-KDLEVSP       TO MOD-KDLEVSP-UT                            
080000     MOVE CLAG-KDFARLIG TO NUM-KDFARLIG                                   
080100     MOVE NUM-KDFARLIG       TO MOD-KDFARLIG-UT                           
080200     MOVE CLAG-BEFT          TO MOD-BEFT                                  
080300                                                                          
080400     IF CLAG-KDERS > 0 AND < 29                                           
080500       MOVE ERR-ART-ERSATT TO MED-IDMFSFEL                                
080600       CALL WMEDKONV USING MED-WMEDAREA                                   
080700       MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                
080800     ELSE                                                                 
080900       IF CLAG-KDERS > 28                                                 
081000         MOVE ERR-ART-NO-MORE TO MED-IDMFSFEL                             
081100         CALL WMEDKONV USING MED-WMEDAREA                                 
081200         MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                             
081300       END-IF                                                             
081400     END-IF                                                               
081500     MOVE CLAG-KDERS          TO MOD-KDERS                                
081600     IF CLAG-KDERS > 0                                                    
081700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDERS-ATTR                       
081800     END-IF                                                               
081900     MOVE CLAG-KVAKS-CDC      TO MOD-KVAKS-CDC                            
082000     MOVE CLAG-KVAKS-PAV      TO MOD-KVAKS-PAV                            
082100     MOVE CLAG-KVAKS-T        TO MOD-KVAKS-T                              
082200     MOVE CLAG-KVLS           TO MOD-KVLS                                 
082300     MOVE CLAG-KVROS          TO MOD-KVROS                                
082400     MOVE CLAG-ADLAGOMR TO NUM-ADLAGOMR                                   
082500     MOVE NUM-ADLAGOMR        TO MOD-ADLAGOMR                             
082600     MOVE CLAG-ADGANG TO NUM-ADGANG                                       
082700     MOVE NUM-ADGANG          TO MOD-ADGANG                               
082800     MOVE CLAG-ADPLATS TO NUM-ADPLATS                                     
082900     MOVE NUM-ADPLATS         TO MOD-ADPLATS                              
083000     MOVE CLAG-FLGEMART       TO MOD-FLGEMART                             
083100                                                                          
083200     MOVE +3 TO W-KDNOTTYP                                                
083300     PERFORM IMS-GNP-ARTC-WDK625                                          
083400     IF SEGMENT-FINNS                                                     
083500       MOVE    NOT-TEARTNOT TO MOD-TEARTNOT1                              
083600     END-IF                                                               
083700                                                                          
083800     MOVE +4 TO W-KDNOTTYP                                                
083900     PERFORM IMS-GNP-ARTC-WDK625                                          
084000     IF SEGMENT-FINNS                                                     
084100       MOVE    NOT-TEARTNOT TO MOD-TEARTNOT2                              
084200     END-IF                                                               
084300                                                                          
084400     MOVE +7 TO W-KDNOTTYP                                                
084500     PERFORM IMS-GNP-ARTC-WDK625                                          
084600     IF SEGMENT-FINNS                                                     
084700       MOVE    NOT-TEARTNOT TO MOD-TEARTNOT7                              
084800     END-IF                                                               
084900     .                                                                    
085000     EJECT                                                                
085100                                                                          
085200 FB-BEHANDLA-KATN11 SECTION.                                              
085300     MOVE +1 TO INDX                                                      
085400     PERFORM IMS-GU-KATN-WLKATN01                                         
085500     IF SEGMENT-FINNS                                                     
085600       PERFORM IMS-GNP-KATN-WLKATN11                                      
085700       IF SEGMENT-FINNS                                                   
085800         MOVE KAT-IDFORDON TO MOD-IDFORDON-ENTER                          
085900         MOVE KAT-IDFORDON TO MOD-IDFORDON-NEXT                           
086000         MOVE KAT-TIOMBRYT-9KOMPL TO MOD-TIOMBRYT-1-ENTER                 
086100                                     MOD-TIOMBRYT-1-NEXT                  
086200         PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-ANT-KAT               
086300             MOVE KAT-BEEMBLEM TO MOD-IDKAT(INDX)                         
086400           ADD +1 TO INDX                                                 
086500           PERFORM IMS-GNP-KATN-WLKATN11                                  
086600         END-PERFORM                                                      
086700         IF SEGMENT-FINNS                                                 
086800           MOVE KAT-IDFORDON TO MOD-IDFORDON-NEXT                         
086900           MOVE KAT-TIOMBRYT-9KOMPL TO MOD-TIOMBRYT-1-NEXT                
087000           IF MFS-UPDATE  OR MFS-UPD-V  OR                                
087100              MID-INPUT1 NOT = ALL '+'  OR                                
087200              MID-INPUT2 NOT = ALL '+'                                    
087300             CONTINUE                                                     
087400           ELSE                                                           
087500             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
087600             CALL WMEDKONV USING MED-WMEDAREA                             
087700             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
087800           END-IF                                                         
087900         END-IF                                                           
088000       END-IF                                                             
088100     END-IF                                                               
088200     .                                                                    
088300     EJECT                                                                
088400 FC-BEHANDLA-KVAH01 SECTION.                                              
088500     PERFORM IMS-GU-KVAH-W6KVAH01                                         
088600     IF SEGMENT-FINNS                                                     
088700                                                                          
088800       MOVE KVAH-ART-ADKVAULG TO MOD-ADKVAULG                             
088900                                                                          
089000       MOVE KVAH-ART-KDKVAULG TO MOD-KDKVAULG                             
089100                                 NUM-KDKVAULG                             
089200       IF KVAH-ART-KDKVAULG > ZERO                                        
089300         MOVE KVA-BEKVAULG (SPRAK-IX NUM-KDKVAULG) TO                     
089400                                 MOD-BEKVAULG                             
089500       ELSE                                                               
089600         IF KVAH-ART-KDKVAULG = ZERO                                      
089700           MOVE KVA-BEKVAULG (SPRAK-IX 10) TO                             
089800                                 MOD-BEKVAULG                             
089900         ELSE                                                             
090000            MOVE MFS-RENSA-FAELT TO MOD-BEKVAULG                          
090100         END-IF                                                           
090200       END-IF                                                             
090300                                                                          
090400       MOVE KVAH-ART-KDKVATYP TO MOD-KDKVATYP                             
090500                                 NUM-KDKVATYP                             
090600       IF KVAH-ART-KDKVATYP NOT = 0                                       
090700         MOVE KVA-KDKVATYP (SPRAK-IX NUM-KDKVATYP) TO                     
090800                                  MOD-KDKVATYP-TEXT                       
090900       ELSE                                                               
091000         MOVE MFS-RENSA-FAELT TO MOD-KDKVATYP-TEXT                        
091100       END-IF                                                             
091200                                                                          
091300       MOVE KVAH-ART-IDPROVPL-PRI TO MOD-IDPROVPL-PRI                     
091400       MOVE KVAH-ART-IDPROVPL-SEK TO MOD-IDPROVPL-SEK                     
091500                                                                          
091600       PERFORM IMS-GNP-KVAH-W6KVAH11                                      
091700       IF SEGMENT-FINNS                                                   
091800         MOVE JA             TO MOD-FLAGGA-KH                             
091900         PERFORM UNTIL SEGMENT-SAKNAS                                     
092000           IF INFO-KDKVAINF = 'R'                                         
092100             MOVE INFO-KDKVAINF TO MOD-KDKVAINF                           
092200           END-IF                                                         
092300           PERFORM IMS-GNP-KVAH-W6KVAH11                                  
092400         END-PERFORM                                                      
092500       END-IF                                                             
092600       PERFORM IMS-GU-KVAH-W6KVAH01                                       
092700       PERFORM IMS-GNP-KVAH-W6KVAH13                                      
092800       MOVE +1 TO LIK-INDX                                                
092900       PERFORM UNTIL SEGMENT-SAKNAS OR LIK-INDX > MAX-ANT-LIK             
093000         IF MID-IDLIKARE(LIK-INDX) = ALL '+'                              
093100           MOVE LIK-IDLIKARE TO MOD-IDLIKARE(LIK-INDX)                    
093200         END-IF                                                           
093300         PERFORM IMS-GNP-KVAH-W6KVAH13                                    
093400         ADD +1 TO LIK-INDX                                               
093500       END-PERFORM                                                        
093600     END-IF                                                               
093700                                                                          
093800     IF NOT MFS-FIRST                                                     
093900       IF MID-KDKVATYP NOT = ALL '+' AND                                  
094000          MID-KDKVATYP NOT = KVAH-ART-KDKVATYP                            
094100         MOVE MID-KDKVATYP TO MOD-KDKVATYP                                
094200       END-IF                                                             
094300                                                                          
094400       IF MID-IDPROVPL-PRI NOT = ALL '+' AND                              
094500          MID-IDPROVPL-PRI NOT = KVAH-ART-IDPROVPL-PRI                    
094600         MOVE MID-IDPROVPL-PRI TO MOD-IDPROVPL-PRI                        
094700       END-IF                                                             
094800                                                                          
094900       IF MID-IDPROVPL-SEK NOT = ALL '+' AND                              
095000          MID-IDPROVPL-SEK NOT = KVAH-ART-IDPROVPL-SEK                    
095100         MOVE MID-IDPROVPL-SEK TO MOD-IDPROVPL-SEK                        
095200       END-IF                                                             
095300                                                                          
095400       IF MID-KDKVAULG NOT = ALL '+' AND                                  
095500          MID-KDKVAULG NOT = KVAH-ART-KDKVAULG                            
095600         MOVE MID-KDKVAULG TO MOD-KDKVAULG                                
095700       END-IF                                                             
095800                                                                          
095900       IF MID-ADKVAULG NOT = ALL '+' AND                                  
096000          MID-ADKVAULG NOT = KVAH-ART-ADKVAULG                            
096100         MOVE MID-ADKVAULG TO MOD-ADKVAULG                                
096200       END-IF                                                             
096300     END-IF                                                               
096400                                                                          
096500     PERFORM IMS-GU-KVAG-W6KVAG01                                         
096600     IF SEGMENT-FINNS                                                     
096700       PERFORM UNTIL SEGMENT-SAKNAS                                       
096800           MOVE SEQB-IDKR TO W-IDKR                                       
096900           PERFORM IMS-GU-KVAE-W6KVAE01                                   
097000           IF KR-FLANNULL = JA                                            
097100               MOVE JA     TO ANNULLERAD-SW                               
097200           ELSE                                                           
097300               MOVE JA     TO KR-FINNS-SW                                 
097400           END-IF                                                         
097500           PERFORM IMS-GN-KVAG-W6KVAG01                                   
097600       END-PERFORM                                                        
097700       IF KR-FINNS                                                        
097800           MOVE JA TO MOD-FLAGGA-KR                                       
097900       ELSE                                                               
098000          IF ANNULLERAD-FINNS                                             
098100          AND KR-FINNS-INTE                                               
098200              MOVE NEJ TO MOD-FLAGGA-KR                                   
098300          END-IF                                                          
098400       END-IF                                                             
098500     ELSE                                                                 
098600       MOVE NEJ        TO MOD-FLAGGA-KR                                   
098700     END-IF                                                               
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100 FD-BEHANDLA-WDK7 SECTION.                                                
099200                                                                          
099300     PERFORM IMS-GET-WDK701                                               
099400     IF SEGMENT-FINNS                                                     
099500        PERFORM IMS-GET-WDK711                                            
099600        PERFORM UNTIL SEGMENT-SAKNAS                                      
099700          MOVE SLAG-IDDC TO W-IDDC-B6                                     
099800          PERFORM IMS-GU-WDB601                                           
099900                                                                          
100000          IF DCS-SDC                                                      
100100            ADD SLAG-KVPB-REF        TO   W-SDC-KVPB                      
100200          ELSE                                                            
100300            IF DCS-NDC                                                    
100400              IF SLAG-IDLEVNR = '1441 '                                   
100500                ADD SLAG-KVPB-REF    TO   W-NDC-KVPB                      
100600              END-IF                                                      
100700            END-IF                                                        
100800          END-IF                                                          
100900          PERFORM IMS-GET-WDK711                                          
101000        END-PERFORM                                                       
101100        COMPUTE W-SDC-KVPB ROUNDED =  W-SDC-KVPB                          
101200                                                                          
101300        COMPUTE W-NDC-KVPB ROUNDED =  W-NDC-KVPB                          
101400     END-IF                                                               
101500     MOVE W-SDC-KVPB                 TO   MOD-KVPB-SEP (2)                
101600     MOVE W-NDC-KVPB                 TO   MOD-KVPB-SEP (3)                
101700     .                                                                    
101800     EJECT                                                                
101900                                                                          
102000 G-KOLLA-INPUT SECTION.                                                   
102100                                                                          
102200     MOVE JA TO INDATA-SW                                                 
102300     IF MID-INPUT1 = ALL '+' AND MID-INPUT2 = ALL '+' AND                 
102400        MID-INPUT3 = ALL '+'                                              
102500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
102600       CALL WMEDKONV USING MED-WMEDAREA                                   
102700       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
102800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
102900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
103000       MOVE NEJ TO INDATA-SW                                              
103100     ELSE                                                                 
103200                                                                          
103300       IF MID-INPUT2 NOT = ALL '+' AND MFS-UPDATE                         
103400         MOVE INF-PRESS-PF23 TO MED-IDMFSINF                              
103500         CALL WMEDKONV USING MED-WMEDAREA                                 
103600         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
103700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
103800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
103900         PERFORM GA-LAES-IN-IGEN                                          
104000         MOVE NEJ TO INDATA-SW                                            
104100       ELSE                                                               
104200                                                                          
104300         IF MID-INPUT1 NOT = ALL '+'                                      
104400                                                                          
104500           IF MID-KDFARLIG NOT = ALL '+'                                  
104600**** ENLIGT ROLF ANDERSSON SKALL ENDAST KODERNA 3,4 OCH 6 VARA            
104700*** GODKÄNDA VÄRDEN PÅ KDFARLIG. OCH 0 ENL RA                             
104800** ÄNDRAT 971112 /KENT                                                    
104900*             IF MID-KDFARLIG NUMERIC                                     
105000              IF MID-KDFARLIG = 3 OR 4 OR 6 OR 0 OR 7                     
105100                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFARLIG-IN-ATTR         
105200              ELSE                                                        
105300                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARLIG-IN-ATTR           
105400                 MOVE NEJ TO INDATA-SW                                    
105500              END-IF                                                      
105600           END-IF                                                         
105700                                                                          
105800           IF MID-KDYTBEH NOT = ALL '+'                                   
105900              IF MID-KDYTBEH NUMERIC AND                                  
106000                 MID-KDYTBEH < 10                                         
106100                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KDYTBEH-IN-ATTR          
106200              ELSE                                                        
106300                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDYTBEH-IN-ATTR            
106400                 MOVE NEJ TO INDATA-SW                                    
106500              END-IF                                                      
106600           END-IF                                                         
106700         ELSE                                                             
106800           IF MID-INPUT3 NOT = ALL '+'                                    
106900             IF MID-IDLIKARE(1) NOT = ALL '+'                             
107000               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLIKARE-ATTR(1)          
107100             END-IF                                                       
107200             IF MID-IDLIKARE(2) NOT = ALL '+'                             
107300               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLIKARE-ATTR(2)          
107400             END-IF                                                       
107500             IF MID-IDLIKARE(3) NOT = ALL '+'                             
107600               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLIKARE-ATTR(3)          
107700             END-IF                                                       
107800             IF MID-IDLIKARE(4) NOT = ALL '+'                             
107900               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLIKARE-ATTR(4)          
108000             END-IF                                                       
108100           END-IF                                                         
108200         END-IF                                                           
108300                                                                          
108400         IF MID-INPUT2 NOT = ALL '+'                                      
108500          PERFORM IMS-GU-KVAH-W6KVAH01                                    
108600          IF MID-KDKVATYP NOT = ALL '+' OR                                
108700             MID-IDPROVPL-PRI NOT = ALL '+' OR                            
108800             MID-IDPROVPL-SEK NOT = ALL '+' OR                            
108900             MID-KDKVAULG NOT = ALL '+'                                   
109000             IF SEGMENT-FINNS                                             
109100               MOVE KVAH-ART-KDKVATYP TO WS-KDKVATYP                      
109200               MOVE KVAH-ART-IDPROVPL-PRI TO WS-IDPROVPL-PRI              
109300               MOVE KVAH-ART-IDPROVPL-SEK TO WS-IDPROVPL-SEK              
109400               MOVE KVAH-ART-KDKVAULG TO WS-KDKVAULG                      
109500             ELSE                                                         
109600               MOVE 0                TO WS-KDKVATYP                       
109700                                        WS-IDPROVPL-PRI                   
109800                                        WS-IDPROVPL-SEK                   
109900                                        WS-KDKVAULG                       
110000             END-IF                                                       
110100          END-IF                                                          
110200                                                                          
110300          IF SEGMENT-FINNS                                                
110400            IF MID-KDKVATYP NOT = ALL '+'                                 
110500              IF MID-KDKVATYP > 0 AND MID-KDKVATYP < 3                    
110600                MOVE MFS-NUM-FAELT-RAETT TO MOD-KDKVATYP-ATTR             
110700                MOVE MID-KDKVATYP TO WS-KDKVATYP                          
110800              ELSE                                                        
110900                IF MID-KDKVATYP = 0                                       
111000* SPÄRRAR NOLLNING AV KDKVATYP PÅ EJ DEFININTIVT ERSATTA ARTIKLAR         
111100                  MOVE ZERO TO WS-CLAG-KDERS                              
111200                  PERFORM IMS-GU-ARTC-WDK601                              
111300                  IF SEGMENT-FINNS                                        
111400                    PERFORM IMS-GNP-ARTC-WDK611                           
111500                    IF SEGMENT-FINNS                                      
111600                      MOVE CLAG-KDERS TO WS-CLAG-KDERS                    
111700                    END-IF                                                
111800                  END-IF                                                  
111900                  IF WS-CLAG-KDERS < 21                                   
112000                    PERFORM IMS-GHNP-KVAH-W6KVAH12                        
112100                    IF SEGMENT-FINNS                                      
112200                      PERFORM UNTIL SEGMENT-SAKNAS                        
112300                        IF LEV-FLUPG = 'N' OR LEV-FLKVASAK = 'N'          
112400                          MOVE MFS-NUM-FAELT-FEL TO                       
112500                                             MOD-KDKVATYP-ATTR            
112600                          MOVE NEJ TO INDATA-SW                           
112700                                      KKOD-SW                             
112800                        ELSE                                              
112900                          MOVE MFS-NUM-FAELT-RAETT TO                     
113000                                          MOD-KDKVATYP-ATTR               
113100                          MOVE MID-KDKVATYP TO WS-KDKVATYP                
113200                        END-IF                                            
113300                        PERFORM IMS-GHNP-KVAH-W6KVAH12                    
113400                      END-PERFORM                                         
113500                    ELSE                                                  
113600                     MOVE MFS-NUM-FAELT-RAETT TO MOD-KDKVATYP-ATTR        
113700                     MOVE MID-KDKVATYP TO WS-KDKVATYP                     
113800                    END-IF                                                
113900                  ELSE                                                    
114000                    MOVE ZERO TO WS-KDKVATYP                              
114100                  END-IF                                                  
114200                ELSE                                                      
114300                  MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVATYP-ATTR             
114400                  MOVE NEJ TO INDATA-SW                                   
114500                END-IF                                                    
114600              END-IF                                                      
114700            END-IF                                                        
114800          ELSE                                                            
114900            IF MID-KDKVATYP NOT = ALL '+'                                 
115000              IF MID-KDKVATYP NUMERIC AND MID-KDKVATYP < 3                
115100                MOVE MFS-NUM-FAELT-RAETT TO MOD-KDKVATYP-ATTR             
115200                MOVE MID-KDKVATYP TO WS-KDKVATYP                          
115300              ELSE                                                        
115400                MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVATYP-ATTR               
115500                MOVE NEJ TO INDATA-SW                                     
115600              END-IF                                                      
115700            END-IF                                                        
115800          END-IF                                                          
115900                                                                          
116000          IF MID-IDPROVPL-PRI NOT = ALL '+'                               
116100            IF MID-IDPROVPL-PRI NUMERIC                                   
116200              MOVE MID-IDPROVPL-PRI TO WS-IDPROVPL-PRI                    
116300              IF WS-IDPROVPL-PRI NOT = 0                                  
116400                                                                          
116500                MOVE WS-IDPROVPL-PRI TO W-IDPROVPL                        
116600                MOVE 'N'             TO W-KDPROVPL                        
116700                PERFORM IMS-GU-PROA-W6PROA11                              
116800                IF SEGMENT-FINNS                                          
116900                  MOVE 6102-KVSKPLOT TO WS-6102-KVSKPLOT-PRI              
117000                  MOVE MFS-NUM-FAELT-RAETT TO                             
117100                                        MOD-IDPROVPL-PRI-ATTR             
117200                ELSE                                                      
117300                  MOVE NEJ TO INDATA-SW                                   
117400                              PROVPL-PRI-SW                               
117500                END-IF                                                    
117600                                                                          
117700                MOVE 'R'                TO W-KDPROVPL                     
117800                PERFORM IMS-GU-PROA-W6PROA11                              
117900                IF SEGMENT-FINNS                                          
118000                  MOVE MFS-NUM-FAELT-RAETT TO                             
118100                                        MOD-IDPROVPL-PRI-ATTR             
118200                ELSE                                                      
118300                  MOVE NEJ TO INDATA-SW                                   
118400                              PROVPL-PRI-SW                               
118500                END-IF                                                    
118600                IF PROVPL-PRI-FEL                                         
118700                  MOVE MFS-NUM-FAELT-FEL TO                               
118800                                        MOD-IDPROVPL-PRI-ATTR             
118900                END-IF                                                    
119000              ELSE                                                        
119100                                                                          
119200                 MOVE MFS-NUM-FAELT-RAETT TO                              
119300                                        MOD-IDPROVPL-PRI-ATTR             
119400              END-IF                                                      
119500            ELSE                                                          
119600              MOVE MFS-NUM-FAELT-FEL TO MOD-IDPROVPL-PRI-ATTR             
119700              MOVE NEJ TO INDATA-SW                                       
119800            END-IF                                                        
119900          END-IF                                                          
120000                                                                          
120100          IF MID-IDPROVPL-SEK NOT = ALL '+'                               
120200            IF MID-IDPROVPL-SEK NUMERIC                                   
120300              MOVE MID-IDPROVPL-SEK TO WS-IDPROVPL-SEK                    
120400              IF WS-IDPROVPL-SEK NOT = 0                                  
120500                                                                          
120600                MOVE WS-IDPROVPL-SEK TO W-IDPROVPL                        
120700                MOVE 'N'                TO W-KDPROVPL                     
120800                PERFORM IMS-GU-PROA-W6PROA11                              
120900                IF SEGMENT-FINNS                                          
121000                  MOVE 6102-KVSKPLOT TO WS-6102-KVSKPLOT-SEK              
121100                  MOVE MFS-NUM-FAELT-RAETT TO                             
121200                                        MOD-IDPROVPL-SEK-ATTR             
121300                ELSE                                                      
121400                  MOVE NEJ TO INDATA-SW                                   
121500                              PROVPL-SEK-SW                               
121600                END-IF                                                    
121700                                                                          
121800                MOVE 'R'                TO W-KDPROVPL                     
121900                PERFORM IMS-GU-PROA-W6PROA11                              
122000                IF SEGMENT-FINNS                                          
122100                  MOVE MFS-NUM-FAELT-RAETT TO                             
122200                                        MOD-IDPROVPL-SEK-ATTR             
122300                ELSE                                                      
122400                  MOVE NEJ TO INDATA-SW                                   
122500                              PROVPL-SEK-SW                               
122600                END-IF                                                    
122700                IF PROVPL-SEK-FEL                                         
122800                  MOVE MFS-NUM-FAELT-FEL TO                               
122900                                        MOD-IDPROVPL-SEK-ATTR             
123000                END-IF                                                    
123100              ELSE                                                        
123200                                                                          
123300                 MOVE MFS-NUM-FAELT-RAETT TO                              
123400                                        MOD-IDPROVPL-SEK-ATTR             
123500              END-IF                                                      
123600            ELSE                                                          
123700              MOVE MFS-NUM-FAELT-FEL TO MOD-IDPROVPL-SEK-ATTR             
123800              MOVE NEJ TO INDATA-SW                                       
123900            END-IF                                                        
124000          END-IF                                                          
124100                                                                          
124200                                                                          
124300          IF WS-KDKVATYP = 0                                              
124400            IF WS-IDPROVPL-PRI NOT = 0                                    
124500            OR WS-IDPROVPL-SEK NOT = 0                                    
124600              IF WS-IDPROVPL-PRI NOT = 0                                  
124700                MOVE MFS-NUM-FAELT-FEL TO                                 
124800                                     MOD-IDPROVPL-PRI-ATTR                
124900                MOVE NEJ TO INDATA-SW                                     
125000              END-IF                                                      
125100              IF WS-IDPROVPL-SEK NOT = 0                                  
125200                MOVE MFS-NUM-FAELT-FEL TO                                 
125300                                     MOD-IDPROVPL-SEK-ATTR                
125400                MOVE NEJ TO INDATA-SW                                     
125500              END-IF                                                      
125600            END-IF                                                        
125700          END-IF                                                          
125800                                                                          
125900          IF MID-KDKVAULG NOT = ALL '+'                                   
126000            IF MID-KDKVAULG NUMERIC                                       
126100              MOVE MID-KDKVAULG TO WS-KDKVAULG                            
126200              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDKVAULG-ATTR               
126300            ELSE                                                          
126400               MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVAULG-ATTR                
126500               MOVE NEJ TO INDATA-SW                                      
126600            END-IF                                                        
126700          END-IF                                                          
126800                                                                          
126900          IF MID-ADKVAULG NOT = ALL '+'                                   
127000              MOVE MFS-NUM-FAELT-RAETT TO MOD-ADKVAULG-ATTR               
127100          END-IF                                                          
127200                                                                          
127300         END-IF                                                           
127400                                                                          
127500         IF INDATA-FEL                                                    
127600           IF KKOD-FEL                                                    
127700             MOVE FLUPG-KVASAK-UPDATED TO MED-IDMFSFEL                    
127800           ELSE                                                           
127900             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
128000           END-IF                                                         
128100           CALL WMEDKONV USING MED-WMEDAREA                               
128200           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
128300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
128400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
128500         END-IF                                                           
128600                                                                          
128700       END-IF                                                             
128800                                                                          
128900     END-IF                                                               
129000     .                                                                    
129100     EJECT                                                                
129200 GA-LAES-IN-IGEN SECTION.                                                 
129300                                                                          
129400     IF MID-KDYTBEH NOT = ALL '+'                                         
129500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDYTBEH-IN-ATTR                  
129600     END-IF                                                               
129700                                                                          
129800     IF MID-KDFARLIG NOT = ALL '+'                                        
129900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFARLIG-IN-ATTR                 
130000     END-IF                                                               
130100                                                                          
130200     IF MID-IDLIKARE(1) NOT = ALL '+'                                     
130300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLIKARE-ATTR(1)                 
130400     END-IF                                                               
130500                                                                          
130600     IF MID-IDLIKARE(2) NOT = ALL '+'                                     
130700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLIKARE-ATTR(2)                 
130800     END-IF                                                               
130900                                                                          
131000     IF MID-IDLIKARE(3) NOT = ALL '+'                                     
131100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLIKARE-ATTR(3)                 
131200     END-IF                                                               
131300                                                                          
131400     IF MID-IDLIKARE(4) NOT = ALL '+'                                     
131500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLIKARE-ATTR(4)                 
131600     END-IF                                                               
131700                                                                          
131800     IF MID-KDKVATYP NOT = ALL '+'                                        
131900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKVATYP-ATTR                    
132000     END-IF                                                               
132100                                                                          
132200     IF MID-IDPROVPL-PRI NOT = ALL '+'                                    
132300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPROVPL-PRI-ATTR                
132400     END-IF                                                               
132500                                                                          
132600     IF MID-IDPROVPL-SEK NOT = ALL '+'                                    
132700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPROVPL-SEK-ATTR                
132800     END-IF                                                               
132900                                                                          
133000     IF MID-KDKVAULG NOT = ALL '+'                                        
133100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKVAULG-ATTR                    
133200     END-IF                                                               
133300                                                                          
133400     IF MID-ADKVAULG NOT = ALL '+'                                        
133500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADKVAULG-ATTR                    
133600     END-IF                                                               
133700                                                                          
133800     .                                                                    
133900     EJECT                                                                
134000                                                                          
134100 H-UPPDATERA SECTION.                                                     
134200                                                                          
134300     IF MID-KDYTBEH   NOT = ALL '+'                                       
134400       PERFORM HA-UPPDATERA-ARTC                                          
134500     END-IF                                                               
134600                                                                          
134700     IF MID-INPUT1 NOT = ALL '+'                                          
134800       PERFORM HB-UPPDATERA-ARTC                                          
134900     END-IF                                                               
135000                                                                          
135100     IF MID-KDKVAULG NOT = ALL '+'                                        
135200     OR MID-ADKVAULG NOT = ALL '+'                                        
135300     OR MID-KDKVATYP NOT = ALL '+'                                        
135400     OR MID-IDPROVPL-PRI NOT = ALL '+'                                    
135500     OR MID-IDPROVPL-SEK NOT = ALL '+'                                    
135600       PERFORM HC-UPPDATERA-KVAH                                          
135700     END-IF                                                               
135800                                                                          
135900     IF MID-INPUT3 NOT = ALL '+'                                          
136000       PERFORM HD-UPPDATERA-KVAH13                                        
136100     END-IF                                                               
136200                                                                          
136300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
136400     CALL WMEDKONV USING MED-WMEDAREA                                     
136500     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
136600     PERFORM MFS-RENSA-ALLA-FAELT-IN                                      
136700     PERFORM MFS-FORM-ATTR                                                
136800     .                                                                    
136900     EJECT                                                                
137000 HA-UPPDATERA-ARTC SECTION.                                               
137100     PERFORM IMS-GU-ARTC-WDK601                                           
137200     IF SEGMENT-FINNS                                                     
137300        IF ART-FLIART = JA                                                
137400           MOVE JA                   TO WS-SATS                           
137500        ELSE                                                              
137600           MOVE NEJ                  TO WS-SATS                           
137700        END-IF                                                            
137800        IF MID-KDYTBEH NOT = ALL '+'                                      
137900          PERFORM IMS-GHNP-ARTC-WDK611                                    
138000          IF SEGMENT-FINNS                                                
138100            IF MID-KDYTBEH NOT = ALL '+'                                  
138200              MOVE MID-KDYTBEH TO WS-KDYTBEH                              
138300                                  MOD-KDYTBEH-UT                          
138400                                  CLAG-KDYTBEH                            
138500*             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDYTBEH-UT-ATTR           
138600              MOVE MFS-RENSA-FAELT TO MOD-KDYTBEH-IN                      
138700            END-IF                                                        
138800            PERFORM IMS-REPL-ARTC                                         
138900          END-IF                                                          
139000        END-IF                                                            
139100     END-IF                                                               
139200     .                                                                    
139300     EJECT                                                                
139400 HB-UPPDATERA-ARTC SECTION.                                               
139500     SKIP2                                                                
139600     IF MID-KDFARLIG NOT = ALL '+'                                        
139700       PERFORM IMS-GHU-ARTC-WDK611                                        
139800       IF SEGMENT-FINNS                                                   
139900         MOVE MID-KDFARLIG TO MOD-KDFARLIG-UT                             
140000                                CLAG-KDFARLIG                             
140100         MOVE MFS-RENSA-FAELT TO MOD-KDFARLIG-IN                          
140200                                                                          
140300         PERFORM IMS-REPL-ARTC                                            
140400       END-IF                                                             
140500     END-IF                                                               
140600     .                                                                    
140700     EJECT                                                                
140800 HC-UPPDATERA-KVAH SECTION.                                               
140900     PERFORM IMS-GHU-KVAH-W6KVAH01                                        
141000     IF SEGMENT-FINNS                                                     
141100       MOVE KVAH-ART-KDKVAKTL TO SPAR-KDKVAKTL                            
141200     ELSE                                                                 
141300       PERFORM HCA-NYUPPLAEGG                                             
141400     END-IF                                                               
141500                                                                          
141600     IF MID-KDKVAULG NOT = ALL '+'                                        
141700        MOVE WS-KDKVAULG TO  KVAH-ART-KDKVAULG                            
141800                             MOD-KDKVAULG                                 
141900*       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDKVAULG-ATTR                   
142000*       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEKVAULG-ATTR                   
142100     END-IF                                                               
142200                                                                          
142300     IF MID-ADKVAULG NOT = ALL '+'                                        
142400        MOVE MID-ADKVAULG TO KVAH-ART-ADKVAULG                            
142500                             MOD-ADKVAULG                                 
142600*       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADKVAULG-ATTR                   
142700     END-IF                                                               
142800                                                                          
142900     IF MID-KDKVATYP NOT = ALL '+'                                        
143000       IF WS-KDKVATYP = 1 AND SPAR-KDKVAKTL (1:1) = 2                     
143100         MOVE JA TO SKIPLOT-SW                                            
143200                    KDKVATYP-SW                                           
143300       END-IF                                                             
143400       MOVE WS-KDKVATYP TO  KVAH-ART-KDKVATYP                             
143500                            MOD-KDKVATYP                                  
143600*      MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDKVATYP-ATTR                    
143700*      MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDKVATYP-TEXT-ATTR               
143800     END-IF                                                               
143900                                                                          
144000     IF MID-IDPROVPL-PRI NOT = ALL '+'                                    
144100       IF WS-IDPROVPL-PRI NOT = SPAR-KDKVAKTL (2:1)                       
144200         MOVE JA TO SKIPLOT-SW                                            
144300       END-IF                                                             
144400       MOVE WS-IDPROVPL-PRI TO  KVAH-ART-IDPROVPL-PRI                     
144500                                MOD-IDPROVPL-PRI                          
144600*      MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPROVPL-PRI-ATTR                
144700     END-IF                                                               
144800                                                                          
144900     IF MID-IDPROVPL-SEK NOT = ALL '+'                                    
145000       IF WS-IDPROVPL-SEK NOT = SPAR-KDKVAKTL (3:1)                       
145100         MOVE JA TO SKIPLOT-SW                                            
145200       END-IF                                                             
145300       MOVE WS-IDPROVPL-SEK TO  KVAH-ART-IDPROVPL-SEK                     
145400                                MOD-IDPROVPL-SEK                          
145500*      MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPROVPL-SEK-ATTR                
145600     END-IF                                                               
145700                                                                          
145800     IF WS-IDPROVPL-PRI = 0 AND WS-IDPROVPL-SEK = 0                       
145900       IF SPAR-KDKVAKTL (2:2) NOT = '00'                                  
146000          MOVE JA TO SKIPLOT-SW                                           
146100       END-IF                                                             
146200     END-IF                                                               
146300                                                                          
146400     PERFORM IMS-REPL-KVAH                                                
146500                                                                          
146600     IF SKIPLOT-UPDATE                                                    
146700       MOVE KVAH-ART-KDKVAKTL TO WS-KDKVAKTL                              
146800       PERFORM HCB-UPPDATERA-KVAH12                                       
146900     END-IF                                                               
147000     .                                                                    
147100     EJECT                                                                
147200 HCA-NYUPPLAEGG SECTION.                                                  
147300     MOVE JA          TO NYUPPLAEGG-SW                                    
147400     MOVE W-IDARTNR   TO KVAH-ART-IDARTNR                                 
147500     MOVE SPACE       TO KVAH-ART-ADKVAULG                                
147600     MOVE '0000'      TO KVAH-ART-KDKVAKTL                                
147700     PERFORM IMS-ISRT-KVAH                                                
147800     PERFORM IMS-GHU-KVAH-W6KVAH01                                        
147900     .                                                                    
148000     EJECT                                                                
148100 HCB-UPPDATERA-KVAH12 SECTION.                                            
148200     PERFORM IMS-GU-KVAH-W6KVAH01                                         
148300     IF SEGMENT-FINNS                                                     
148400                                                                          
148500       IF NYUPPLAEGG OR WS-KDKVAKTL (1:3) = '000'                         
148600                                                                          
148700         PERFORM IMS-GHNP-KVAH-W6KVAH12                                   
148800         IF SEGMENT-FINNS                                                 
148900           PERFORM UNTIL SEGMENT-SAKNAS                                   
149000             MOVE +0 TO LEV-KVSKPLOT-PRI                                  
149100             MOVE +0 TO LEV-KVSKPLOT-SEK                                  
149200             MOVE NEJ TO LEV-FLSKPSAK                                     
149300             PERFORM IMS-REPL-KVAH                                        
149400             PERFORM IMS-GHNP-KVAH-W6KVAH12                               
149500           END-PERFORM                                                    
149600         END-IF                                                           
149700                                                                          
149800       ELSE                                                               
149900                                                                          
150000         PERFORM IMS-GHNP-KVAH-W6KVAH12                                   
150100         IF SEGMENT-FINNS                                                 
150200           PERFORM UNTIL SEGMENT-SAKNAS                                   
150300             IF LEV-FLSKPSAK = JA                                         
150400               IF WS-IDPROVPL-PRI > 0                                     
150500                 MOVE +1 TO LEV-KVSKPLOT-PRI                              
150600               END-IF                                                     
150700               IF WS-IDPROVPL-SEK > 0                                     
150800                 MOVE +1 TO LEV-KVSKPLOT-SEK                              
150900               END-IF                                                     
151000               PERFORM IMS-REPL-KVAH                                      
151100             ELSE                                                         
151200               IF NOT KDKVATYP-UPDATE                                     
151300                 IF MID-IDPROVPL-PRI NOT = ALL '+' OR                     
151400                    MID-IDPROVPL-SEK NOT = ALL '+'                        
151500                    IF MID-IDPROVPL-PRI = ALL '0'                         
151600                      MOVE ZERO TO LEV-KVSKPLOT-PRI                       
151700                    ELSE                                                  
151800                      IF MID-IDPROVPL-SEK = ALL '0'                       
151900                       MOVE ZERO TO LEV-KVSKPLOT-SEK                      
152000                      ELSE                                                
152100                       COMPUTE LEV-KVSKPLOT-PRI =                         
152200                                       WS-6102-KVSKPLOT-PRI + 1           
152300                       COMPUTE LEV-KVSKPLOT-SEK =                         
152400                                       WS-6102-KVSKPLOT-SEK + 1           
152500                      END-IF                                              
152600                    END-IF                                                
152700                 END-IF                                                   
152800                 PERFORM IMS-REPL-KVAH                                    
152900               END-IF                                                     
153000             END-IF                                                       
153100             PERFORM IMS-GHNP-KVAH-W6KVAH12                               
153200           END-PERFORM                                                    
153300         END-IF                                                           
153400       END-IF                                                             
153500     END-IF                                                               
153600     .                                                                    
153700     EJECT                                                                
153800 HD-UPPDATERA-KVAH13 SECTION.                                             
153900                                                                          
154000     PERFORM IMS-GU-KVAH-W6KVAH01                                         
154100     IF SEGMENT-FINNS                                                     
154200       MOVE +1 TO LIK-INDX                                                
154300       PERFORM UNTIL LIK-INDX > MAX-ANT-LIK                               
154400         PERFORM IMS-GHNP-KVAH-W6KVAH13                                   
154500         IF MID-IDLIKARE (LIK-INDX) NOT = ALL '+'                         
154600           IF SEGMENT-FINNS                                               
154700             PERFORM IMS-DLET-KVAH                                        
154800           END-IF                                                         
154900           IF MID-IDLIKARE (LIK-INDX) NOT = ALL SPACE                     
155000             MOVE MID-IDLIKARE (LIK-INDX) TO LIK-IDLIKARE                 
155100             PERFORM IMS-ISRT-KVAH-W6KVAH13                               
155200           END-IF                                                         
155300           MOVE ALL '+' TO MID-IDLIKARE (LIK-INDX)                        
155400         END-IF                                                           
155500         ADD +1 TO LIK-INDX                                               
155600       END-PERFORM                                                        
155700     END-IF                                                               
155800     .                                                                    
155900     EJECT                                                                
156000 MFS-RENSA-FAELT-UT SECTION.                                              
156100                                                                          
156200*    --- ALLA UTDATA-FÄLT                                                 
156300     MOVE MFS-RENSA-FAELT TO MOD-IDFORDON-ENTER                           
156400                             MOD-IDFORDON-NEXT                            
156500                             MOD-TIOMBRYT-1-ENTER                         
156600                             MOD-TIOMBRYT-1-NEXT                          
156700                             MOD-BEART-SVE                                
156800                             MOD-KVLS                                     
156900                             MOD-BEART-ENG                                
157000                             MOD-KVAKS-CDC                                
157100                             MOD-KVAKS-PAV                                
157200                             MOD-KVAKS-T                                  
157300                             MOD-IDLEVNR                                  
157400                             MOD-KVROS                                    
157500                             MOD-IDANSK                                   
157600                             MOD-IDFKNGRP                                 
157700                             MOD-IDBERED                                  
157800                             MOD-KDSORT                                   
157900                             MOD-KVPB-SATS                                
158000                             MOD-PRARTSTD                                 
158100                             MOD-BEFT                                     
158200                             MOD-ADLAGOMR                                 
158300                             MOD-ADGANG                                   
158400                             MOD-ADPLATS                                  
158500                             MOD-FLGEMART                                 
158600                             MOD-IDRITN                                   
158700                             MOD-KDERS                                    
158800                             MOD-FLUPG                                    
158900                             MOD-TIUPG                                    
159000                             MOD-KDTPD                                    
159100                             MOD-TITPD                                    
159200                                                                          
159300     MOVE +1 TO INDX                                                      
159400     PERFORM UNTIL INDX > MAX-ANT-KAT                                     
159500       MOVE MFS-RENSA-FAELT TO MOD-IDKAT (INDX)                           
159600       ADD +1 TO INDX                                                     
159700     END-PERFORM                                                          
159800                                                                          
159900     MOVE +1 TO IX                                                        
160000     PERFORM UNTIL IX > 3                                                 
160100       MOVE MFS-RENSA-FAELT TO MOD-KVPB-SEP (IX)                          
160200       ADD +1 TO IX                                                       
160300     END-PERFORM                                                          
160400                                                                          
160500     MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT1                                
160600                             MOD-KDLEVSP-UT                               
160700                             MOD-KDYTBEH-UT                               
160800                             MOD-KDKVATYP-TEXT                            
160900                             MOD-KDFARLIG-UT                              
161000                             MOD-FLAGGA-KH                                
161100                             MOD-FLAGGA-KR                                
161200                             MOD-BEKVAULG                                 
161300                             MOD-KDKVATYP                                 
161400                             MOD-IDPROVPL-PRI                             
161500                             MOD-IDPROVPL-SEK                             
161600                             MOD-KDKVAULG                                 
161700                             MOD-ADKVAULG                                 
161800                             MOD-TEARTNOT2                                
161900                             MOD-TEARTNOT7                                
162000     MOVE +1 TO INDX                                                      
162100     PERFORM UNTIL INDX > MAX-ANT-LIK                                     
162200       MOVE MFS-RENSA-FAELT TO MOD-IDLIKARE(INDX)                         
162300       ADD +1 TO INDX                                                     
162400     END-PERFORM                                                          
162500     .                                                                    
162600     SKIP2                                                                
162700 MFS-RENSA-FAELT-IN SECTION.                                              
162800                                                                          
162900*    --- ALLA INDATA-FÄLT                                                 
163000     MOVE MFS-RENSA-FAELT TO MOD-KDYTBEH-IN                               
163100                             MOD-KDFARLIG-IN                              
163200     .                                                                    
163300     EJECT                                                                
163400 MFS-RENSA-ALLA-FAELT-IN SECTION.                                         
163500                                                                          
163600*    --- ALLA INDATA-FÄLT                                                 
163700     PERFORM MFS-RENSA-FAELT-IN                                           
163800     MOVE MFS-RENSA-FAELT TO MOD-KDKVATYP                                 
163900                             MOD-IDPROVPL-PRI                             
164000                             MOD-IDPROVPL-SEK                             
164100                             MOD-KDKVAULG                                 
164200                             MOD-ADKVAULG                                 
164300     MOVE +1 TO INDX                                                      
164400     PERFORM UNTIL INDX > MAX-ANT-LIK                                     
164500       MOVE MFS-RENSA-FAELT TO MOD-IDLIKARE(INDX)                         
164600       ADD +1 TO INDX                                                     
164700     END-PERFORM                                                          
164800     .                                                                    
164900     EJECT                                                                
165000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
165100                                                                          
165200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFORDON-ENTER                         
165300                               MOD-IDFORDON-NEXT                          
165400                               MOD-TIOMBRYT-1-ENTER                       
165500                               MOD-TIOMBRYT-1-NEXT                        
165600                               MOD-BEART-SVE                              
165700                               MOD-KVLS                                   
165800                               MOD-BEART-ENG                              
165900                               MOD-KVAKS-CDC                              
166000                               MOD-KVAKS-PAV                              
166100                               MOD-KVAKS-T                                
166200                               MOD-IDLEVNR                                
166300                               MOD-KVROS                                  
166400                               MOD-IDANSK                                 
166500                               MOD-IDFKNGRP                               
166600                               MOD-IDBERED                                
166700                               MOD-KDSORT                                 
166800                               MOD-KVPB-SATS                              
166900                               MOD-PRARTSTD                               
167000                               MOD-BEFT                                   
167100                               MOD-ADLAGOMR                               
167200                               MOD-ADGANG                                 
167300                               MOD-ADPLATS                                
167400                               MOD-FLGEMART                               
167500                               MOD-IDRITN                                 
167600                               MOD-KDERS                                  
167700                               MOD-FLUPG                                  
167800                               MOD-TIUPG                                  
167900                               MOD-KDTPD                                  
168000                               MOD-TITPD                                  
168100                                                                          
168200     MOVE +1 TO INDX                                                      
168300     PERFORM UNTIL INDX > MAX-ANT-KAT                                     
168400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKAT (INDX)                         
168500       ADD +1 TO INDX                                                     
168600     END-PERFORM                                                          
168700                                                                          
168800     MOVE +1 TO IX                                                        
168900     PERFORM UNTIL IX > 3                                                 
169000       MOVE MFS-ROER-EJ-FAELT TO MOD-KVPB-SEP (IX)                        
169100       ADD +1 TO IX                                                       
169200     END-PERFORM                                                          
169300                                                                          
169400     MOVE MFS-ROER-EJ-FAELT TO MOD-TEARTNOT1                              
169500                               MOD-TEARTNOT7                              
169600                               MOD-KDLEVSP-UT                             
169700                               MOD-KDYTBEH-UT                             
169800                               MOD-KDKVATYP-TEXT                          
169900                               MOD-KDFARLIG-UT                            
170000                               MOD-FLAGGA-KH                              
170100                               MOD-FLAGGA-KR                              
170200                               MOD-BEKVAULG                               
170300                                                                          
170400     MOVE +1 TO INDX                                                      
170500     PERFORM UNTIL INDX > MAX-ANT-LIK                                     
170600       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLIKARE(INDX)                       
170700       ADD +1 TO INDX                                                     
170800     END-PERFORM                                                          
170900     .                                                                    
171000     EJECT                                                                
171100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
171200                                                                          
171300*    --- ALLA INDATA-FÄLT                                                 
171400     MOVE MFS-ROER-EJ-FAELT TO MOD-KDYTBEH-IN                             
171500                               MOD-KDFARLIG-IN                            
171600                               MOD-KDKVATYP                               
171700                               MOD-KDKVAINF                               
171800                               MOD-IDPROVPL-PRI                           
171900                               MOD-IDPROVPL-SEK                           
172000                               MOD-KDKVAULG                               
172100                               MOD-ADKVAULG                               
172200     MOVE +1 TO INDX                                                      
172300     PERFORM UNTIL INDX > MAX-ANT-LIK                                     
172400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLIKARE(INDX)                       
172500       ADD +1 TO INDX                                                     
172600     END-PERFORM                                                          
172700     .                                                                    
172800     EJECT                                                                
172900 MFS-FORM-ATTR SECTION.                                                   
173000                                                                          
173100*    --- ALLA INDATA-FÄLT                                                 
173200     MOVE MFS-FORMATETS-ATTR TO     MOD-KDYTBEH-IN-ATTR                   
173300                                    MOD-KDFARLIG-IN-ATTR                  
173400                                    MOD-KDKVATYP-ATTR                     
173500                                    MOD-IDPROVPL-PRI-ATTR                 
173600                                    MOD-IDPROVPL-SEK-ATTR                 
173700                                    MOD-KDKVAULG-ATTR                     
173800                                    MOD-ADKVAULG-ATTR                     
173900     MOVE +1 TO INDX                                                      
174000     PERFORM UNTIL INDX > MAX-ANT-LIK                                     
174100       MOVE MFS-FORMATETS-ATTR TO MOD-IDLIKARE-ATTR(INDX)                 
174200       ADD +1 TO INDX                                                     
174300     END-PERFORM                                                          
174400     .                                                                    
174500     SKIP2                                                                
174600 MFS-STAENG-FAELT-IN SECTION.                                             
174700     MOVE MFS-STAENG-FAELT TO       MOD-KDFARLIG-IN-ATTR                  
174800                                    MOD-KDYTBEH-IN-ATTR                   
174900                                    MOD-KDKVATYP-ATTR                     
175000                                    MOD-IDPROVPL-PRI-ATTR                 
175100                                    MOD-IDPROVPL-SEK-ATTR                 
175200                                    MOD-KDKVAULG-ATTR                     
175300                                    MOD-ADKVAULG-ATTR                     
175400     MOVE +1 TO INDX                                                      
175500     PERFORM UNTIL INDX > MAX-ANT-LIK                                     
175600       MOVE MFS-STAENG-FAELT TO MOD-IDLIKARE-ATTR(INDX)                   
175700       ADD +1 TO INDX                                                     
175800     END-PERFORM                                                          
175900     .                                                                    
176000     EJECT                                                                
176100* --- IMS SEKTIONER ---                                                   
176200     SKIP3                                                                
176300 IMS-GET-MSG SECTION.                                                     
176400                                                                          
176500     MOVE '  QC' TO GODK-STATUSKODER                                      
176600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
176700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
176800     PERFORM IMS-STATUSKONTROLL                                           
176900     .                                                                    
177000     SKIP3                                                                
177100 IMS-INSERT-MSG SECTION.                                                  
177200                                                                          
177300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
177400       MOVE '0' TO MFS-KDHUVOMR                                           
177500     END-IF                                                               
177600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
177700     MOVE SPACE TO GODK-STATUSKODER                                       
177800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
177900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
178000     PERFORM IMS-STATUSKONTROLL                                           
178100     .                                                                    
178200     EJECT                                                                
178300                                                                          
178400 IMS-GU-ARTC-WDK601 SECTION.                                              
178500*WDK6                                                                     
178600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
178700            DELIMITED BY SIZE INTO SSA1                                   
178800     MOVE '  GE' TO GODK-STATUSKODER                                      
178900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
179000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
179100     PERFORM IMS-STATUSKONTROLL                                           
179200     .                                                                    
179300     SKIP3                                                                
179400 IMS-GNP-ARTC-WDK611 SECTION.                                             
179500*WDK6                                                                     
179600     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
179700     MOVE '  GE' TO GODK-STATUSKODER                                      
179800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
179900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
180000     PERFORM IMS-STATUSKONTROLL                                           
180100     .                                                                    
180200     SKIP3                                                                
180300 IMS-GHNP-ARTC-WDK611 SECTION.                                            
180400*WDK6                                                                     
180500     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
180600     MOVE '  GE' TO GODK-STATUSKODER                                      
180700     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
180800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
180900     PERFORM IMS-STATUSKONTROLL                                           
181000     SKIP3                                                                
181100     .                                                                    
181200     EJECT                                                                
181300 IMS-GNP-ARTC-WDK625 SECTION.                                             
181400*WDK6                                                                     
181500     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
181600     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
181700            DELIMITED BY SIZE INTO SSA2                                   
181800     MOVE '  GE' TO GODK-STATUSKODER                                      
181900     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
182000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
182100     PERFORM IMS-STATUSKONTROLL                                           
182200     .                                                                    
182300     EJECT                                                                
182400 IMS-REPL-ARTC SECTION.                                                   
182500*WDK6                                                                     
182600     MOVE '  ' TO GODK-STATUSKODER                                        
182700     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
182800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
182900     PERFORM IMS-STATUSKONTROLL                                           
183000     .                                                                    
183100     EJECT                                                                
183200 IMS-GHU-ARTC-WDK611 SECTION.                                             
183300*WDK6                                                                     
183400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
183500            DELIMITED BY SIZE INTO SSA1                                   
183600     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
183700     MOVE '  GE' TO GODK-STATUSKODER                                      
183800     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
183900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
184000     PERFORM IMS-STATUSKONTROLL                                           
184100     .                                                                    
184200     SKIP3                                                                
184300 IMS-GET-WDK701 SECTION.                                                  
184400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
184500            DELIMITED BY SIZE INTO SSA1                                   
184600     MOVE '  GE' TO GODK-STATUSKODER                                      
184700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
184800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
184900     PERFORM IMS-STATUSKONTROLL                                           
185000     .                                                                    
185100     SKIP3                                                                
185200 IMS-GET-WDK711 SECTION.                                                  
185300     MOVE 'WDK711' TO SSA1                                                
185400     MOVE '  GE' TO GODK-STATUSKODER                                      
185500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
185600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
185700     PERFORM IMS-STATUSKONTROLL                                           
185800     .                                                                    
185900 IMS-GU-BENA-WLBENA01 SECTION.                                            
186000                                                                          
186100     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
186200     DELIMITED BY SIZE INTO SSA1                                          
186300     MOVE '  GE' TO GODK-STATUSKODER                                      
186400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
186500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
186600     PERFORM IMS-STATUSKONTROLL                                           
186700     .                                                                    
186800     EJECT                                                                
186900                                                                          
187000 IMS-GNP-BENA-WLBENA11 SECTION.                                           
187100                                                                          
187200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-KEY-X ')'                     
187300     DELIMITED BY SIZE INTO SSA1                                          
187400     MOVE '  GE' TO GODK-STATUSKODER                                      
187500     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
187600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
187700     PERFORM IMS-STATUSKONTROLL                                           
187800     .                                                                    
187900     EJECT                                                                
188000 IMS-GU-KVAG-W6KVAG01 SECTION.                                            
188100     STRING 'W6KVAG01*F(W6H7B1KY>=' W-W6H7B1KY-MIN-X                      
188200                    '&W6H7B1KY<=' W-W6H7B1KY-MAX-X ')'                    
188300          DELIMITED BY SIZE INTO SSA1                                     
188400     MOVE '  GE' TO GODK-STATUSKODER                                      
188500     CALL CBLTDLI USING GU KVAG-PCB DLI-IO-AREA SSA1                      
188600     MOVE KVAG-STATUS-CODE TO STATUS-WS                                   
188700     PERFORM IMS-STATUSKONTROLL                                           
188800     .                                                                    
188900     SKIP3                                                                
189000 IMS-GN-KVAG-W6KVAG01 SECTION.                                            
189100     STRING 'W6KVAG01(W6H7B1KY>=' W-W6H7B1KY-MIN-X                        
189200                    '&W6H7B1KY<=' W-W6H7B1KY-MAX-X ')'                    
189300          DELIMITED BY SIZE INTO SSA1                                     
189400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
189500     CALL CBLTDLI USING GN KVAG-PCB DLI-IO-AREA SSA1                      
189600     MOVE KVAG-STATUS-CODE TO STATUS-WS                                   
189700     PERFORM IMS-STATUSKONTROLL                                           
189800     .                                                                    
189900     EJECT                                                                
190000 IMS-GU-KVAE-W6KVAE01 SECTION.                                            
190100     STRING 'W6KVAE01(IDKR     =' W-W6H701KY-X ')'                        
190200          DELIMITED BY SIZE INTO SSA1                                     
190300     MOVE '    ' TO GODK-STATUSKODER                                      
190400     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA5 SSA1                     
190500     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
190600     PERFORM IMS-STATUSKONTROLL                                           
190700     .                                                                    
190800     EJECT                                                                
190900 IMS-GHU-KVAH-W6KVAH01 SECTION.                                           
191000     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
191100          DELIMITED BY SIZE INTO SSA1                                     
191200     MOVE '  GE' TO GODK-STATUSKODER                                      
191300     CALL CBLTDLI USING GHU KVAH-PCB DLI-IO-AREA4 SSA1                    
191400     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
191500     PERFORM IMS-STATUSKONTROLL                                           
191600     .                                                                    
191700     SKIP3                                                                
191800 IMS-GU-KVAH-W6KVAH01 SECTION.                                            
191900     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
192000          DELIMITED BY SIZE INTO SSA1                                     
192100     MOVE '  GE' TO GODK-STATUSKODER                                      
192200     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA4 SSA1                     
192300     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
192400     PERFORM IMS-STATUSKONTROLL                                           
192500     .                                                                    
192600     SKIP3                                                                
192700 IMS-GNP-KVAH-W6KVAH11 SECTION.                                           
192800     MOVE 'W6KVAH11 ' TO SSA1                                             
192900     MOVE '  GE' TO GODK-STATUSKODER                                      
193000     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA4 SSA1                    
193100     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
193200     PERFORM IMS-STATUSKONTROLL                                           
193300     .                                                                    
193400     SKIP3                                                                
193500 IMS-GNP-KVAH-W6KVAH13 SECTION.                                           
193600     MOVE 'W6KVAH13 ' TO SSA1                                             
193700     MOVE '  GE' TO GODK-STATUSKODER                                      
193800     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA4 SSA1                    
193900     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
194000     PERFORM IMS-STATUSKONTROLL                                           
194100     .                                                                    
194200     SKIP3                                                                
194300 IMS-GHNP-KVAH-W6KVAH12 SECTION.                                          
194400     MOVE 'W6KVAH12 ' TO SSA1                                             
194500     MOVE '  GE' TO GODK-STATUSKODER                                      
194600     CALL CBLTDLI USING GHNP KVAH-PCB DLI-IO-AREA4 SSA1                   
194700     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
194800     PERFORM IMS-STATUSKONTROLL                                           
194900     .                                                                    
195000     SKIP3                                                                
195100 IMS-GHNP-KVAH-W6KVAH13 SECTION.                                          
195200     MOVE 'W6KVAH13 ' TO SSA1                                             
195300     MOVE '  GE' TO GODK-STATUSKODER                                      
195400     CALL CBLTDLI USING GHNP KVAH-PCB DLI-IO-AREA4 SSA1                   
195500     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
195600     PERFORM IMS-STATUSKONTROLL                                           
195700     .                                                                    
195800     SKIP3                                                                
195900 IMS-ISRT-KVAH SECTION.                                                   
196000                                                                          
196100     MOVE 'W6KVAH01 ' TO SSA1                                             
196200     MOVE '  ' TO GODK-STATUSKODER                                        
196300     CALL CBLTDLI USING ISRT KVAH-PCB DLI-IO-AREA4 SSA1                   
196400     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
196500     PERFORM IMS-STATUSKONTROLL                                           
196600     .                                                                    
196700     SKIP3                                                                
196800 IMS-ISRT-KVAH-W6KVAH13 SECTION.                                          
196900                                                                          
197000     MOVE 'W6KVAH13 ' TO SSA1                                             
197100     MOVE '  II' TO GODK-STATUSKODER                                      
197200     CALL CBLTDLI USING ISRT KVAH-PCB DLI-IO-AREA4 SSA1                   
197300     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
197400     PERFORM IMS-STATUSKONTROLL                                           
197500     .                                                                    
197600     SKIP3                                                                
197700 IMS-REPL-KVAH SECTION.                                                   
197800                                                                          
197900     MOVE '  ' TO GODK-STATUSKODER                                        
198000     CALL CBLTDLI USING REPL KVAH-PCB DLI-IO-AREA4                        
198100     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
198200     PERFORM IMS-STATUSKONTROLL                                           
198300     .                                                                    
198400     EJECT                                                                
198500 IMS-DLET-KVAH SECTION.                                                   
198600                                                                          
198700     MOVE '  ' TO GODK-STATUSKODER                                        
198800     CALL CBLTDLI USING DLET KVAH-PCB DLI-IO-AREA4                        
198900     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
199000     PERFORM IMS-STATUSKONTROLL                                           
199100     .                                                                    
199200     EJECT                                                                
199300 IMS-GU-KATN-WLKATN01 SECTION.                                            
199400                                                                          
199500     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
199600     DELIMITED BY SIZE INTO SSA1                                          
199700     MOVE '  GE' TO GODK-STATUSKODER                                      
199800     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA SSA1                      
199900     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
200000     PERFORM IMS-STATUSKONTROLL                                           
200100     .                                                                    
200200     EJECT                                                                
200300 IMS-GNP-KATN-WLKATN11 SECTION.                                           
200400                                                                          
200500     STRING 'WLKATN11(WDN611KY>=' W-WDN611KY-X ')'                        
200600     DELIMITED BY SIZE INTO SSA1                                          
200700     MOVE '  GE' TO GODK-STATUSKODER                                      
200800     CALL CBLTDLI USING GNP KATN-PCB DLI-IO-AREA SSA1                     
200900     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
201000     PERFORM IMS-STATUSKONTROLL                                           
201100     .                                                                    
201200     EJECT                                                                
201300 IMS-GU-PROA-W6PROA11 SECTION.                                            
201400                                                                          
201500     STRING 'W6PROA01(W6GXKEY  =' W-W6GX-6101-KEY-X ')'                   
201600            DELIMITED BY SIZE INTO SSA1                                   
201700     STRING 'W6PROA11(IDPROVPL =' W-IDPROVPL-X ')'                        
201800            DELIMITED BY SIZE INTO SSA2                                   
201900     MOVE '  GE'                  TO GODK-STATUSKODER                     
202000     CALL CBLTDLI USING GU PROA-PCB DLI-IO-AREA3 SSA1 SSA2                
202100     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
202200     PERFORM IMS-STATUSKONTROLL                                           
202300     .                                                                    
202400     EJECT                                                                
202500                                                                          
202600 IMS-GU-WDB601    SECTION.                                                
202700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
202800          DELIMITED BY SIZE INTO SSA1                                     
202900     MOVE '  ' TO GODK-STATUSKODER                                        
203000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
203100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
203200     PERFORM IMS-STATUSKONTROLL                                           
203300     .                                                                    
203400     EJECT                                                                
203500 IMS-GET-WDD201 SECTION.                                                  
203600     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
203700            DELIMITED BY SIZE INTO SSA1                                   
203800     MOVE '  GE' TO GODK-STATUSKODER                                      
203900     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-AREA-WDD2 SSA1                 
204000     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
204100     PERFORM IMS-STATUSKONTROLL                                           
204200     .                                                                    
204300     EJECT                                                                
204400 IMS-STATUSKONTROLL SECTION.                                              
204500                                                                          
204600     SET STATUS-IX TO 1                                                   
204700     SEARCH GODK-STATUS                                                   
204800       AT END                                                             
204900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
205000         DELIMITED BY SIZE INTO FELTEXT                                   
205100         CALL FELLOG                                                      
205200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
205300         CONTINUE                                                         
205400     END-SEARCH                                                           
205500     .                                                                    
