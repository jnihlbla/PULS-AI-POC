000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4022400.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   90/08/16.                                                
000600                                                                          
000700                                                                          
000800*    FUNKTION.                                                            
000900*        PROGRAMMET HANTERAR VOR-KÖN (WDR4).                              
001000*        VIA CMD PÅ BILDEN ANGER MAN VAD MAN VILL GÖRA:                   
001100*          CMD = S : SKAPA NY ORDER; TILL 4231 & FLVOR = J                
001110*          CMD = N : SKAPA NY ORDER; TILL 4221 & FLVOR = J                
001200*          CMD = F : SKAPA NY ORDER; TILL 4221 & FLVOR = J                
001300*                                              & FLVORBI = J              
001400*          CMD = D : ANNULLERA RADEN FRÅN VOR-KÖN                         
001500*                                                                         
001600*        TEVORMRK ÄR VOR-GRUPPENS VALFRIA MÄRKNING AV                     
001700*          RAD SOM ÄR UNDER UTREDNING.                                    
001800*                                                                         
001900*        KDVORATG ANGER VAD MAN GJORT.                                    
002000*        KDVORATG = 0 : RADEN OBEHANDLAD PÅ VOR-KÖN                       
002100*        KDVORATG = 1 : RADEN TILL NY ORDER                               
002200*                                                                         
002300*                                                                         
002400*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002500*        PROGRAMMET UPPDATERAR WL4541 (WDR4)                              
002510*        PROGRAMMET UPPDATERAR WLFILA (WDR6)                              
002600*                                                                         
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W4T224                                              
003000*        MID:         W4I22401                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         W4O22401                                            
003400                                                                          
003500                                                                          
003600 ENVIRONMENT DIVISION.                                                    
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W4022400'.            
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004210 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004800                                                                          
004900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005100 77  DISTR-INDX                  PIC S9(1)  VALUE +1    COMP-3.           
005200 77  SPAR-KVBEART                PIC S9(7)  VALUE +0    COMP-3.           
005300 77  RAD-INDX                    PIC S9(3)  VALUE +0    COMP-3.           
005500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
005700 77  WS-CMD-SPAR                 PIC  X(1)  VALUE SPACE.                  
005900 77  WS-IDDISTR                  PIC  X(5) VALUE SPACE JUST RIGHT.        
006000 77  WS-IDARTNR                  PIC  X(9) VALUE SPACE JUST RIGHT.        
006110 77  WS-IDANSK                   PIC  X(3)  VALUE SPACE.                  
006120 77  WS-IDDISTR-SPAR             PIC  X(4)  VALUE SPACE.                  
006130 77  WS-IDKUNDNR-SPAR            PIC  X(6)  VALUE SPACE.                  
006140 77  W-IDANSK                    PIC S9(3)  VALUE +0    COMP-3.           
006150 77  WS-611-IDARTNR              PIC S9(9)  VALUE ZERO COMP-3.            
006160 77  WS-711-IDARTNR              PIC S9(9)  VALUE ZERO COMP-3.            
006170 77  WS-711-IDDC                 PIC X(2)   VALUE SPACES.                 
006200 77  DIFF-NUM                    PIC  9(6)  VALUE ZERO.                   
006300 77  BEHORIG                     PIC  X     VALUE SPACE.                  
006400 77  DATUM                       PIC S9(7)  VALUE ZERO COMP-3.            
006500 77  TID                         PIC S9(9)  VALUE ZERO COMP-3.            
006600                                                                          
006610*   --- VALID IDDC CODES                                                  
006620*                                                                         
006630*01  -COPY WWDC99                                                         
006680                                                                          
006700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006800                                                                          
007100                                                                          
007500                                                                          
007600 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
007700     88  FIRST-TIME                          VALUE 'J'.                   
007800                                                                          
007801 77  DISTR-IFYLLD-SW             PIC X       VALUE 'N'.                   
007802     88  DISTR-IFYLLD                        VALUE 'J'.                   
007803     88  DISTR-EJ-IFYLLD                     VALUE 'N'.                   
007804                                                                          
007805 77  ARTIKEL-IFYLLD-SW           PIC X       VALUE 'N'.                   
007806     88  ARTIKEL-IFYLLD                      VALUE 'J'.                   
007807     88  ARTIKEL-EJ-IFYLLD                   VALUE 'N'.                   
007808                                                                          
007810 77  STARTA-4221-SW              PIC X       VALUE 'N'.                   
007820     88  STARTA-4221                         VALUE 'J'.                   
007830                                                                          
007831 77  STARTA-4231-SW              PIC X       VALUE 'N'.                   
007832     88  STARTA-4231                         VALUE 'J'.                   
007833                                                                          
007840 77  VOR-KOE-SW                  PIC X       VALUE 'J'.                   
007850     88  VOR-KOE-FINNS                       VALUE 'J'.                   
007860                                                                          
007900 77  PFK-SW                      PIC X       VALUE '0'.                   
008000     88  PFK-ENTER-TRYCKT                    VALUE ' '.                   
008100     88  PFK-7-TRYCKT                        VALUE '7'.                   
008200     88  PFK-8-TRYCKT                        VALUE '8'.                   
008300                                                                          
008400 77  GODK-CMD-KOD-SW             PIC X      VALUE ' '.                    
008500     88  GODK-CMD-KOD                       VALUE 'D' 'F' 'N' 'S'.        
008600                                                                          
008700                                                                          
008800 77  USER-SW                     PIC X       VALUE 'J'.                   
008900     88  USER-NDC                            VALUE 'J'.                   
009000     88  USER-CDC                            VALUE 'N'.                   
009100                                                                          
009110 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009120     88  INDATA-OK                           VALUE 'J'.                   
009130     88  INDATA-FEL                          VALUE 'N'.                   
009140                                                                          
009200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009300     88  NYCKLAR-OK                          VALUE 'J'.                   
009400     88  NYCKLAR-FEL                         VALUE 'N'.                   
009500                                                                          
009600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009700     88  ALLT-OK                             VALUE 'J'.                   
009800                                                                          
010800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010900     88  EGEN-MID                            VALUE '4224'.                
011000     88  GODK-MID                            VALUE '4221' '4222'          
011100                                                   '4223' '4224'          
011200                                                   '4225' '4226'          
011300                                                   '4227' '4228'          
011400                                                   '4229' '4233'.         
011500 01  DISTR-TABELL.                                                        
011600     03  DISTR-INDX-TABELL OCCURS 5 TIMES.                                
011700       05  DISTRIKT              PIC  9(5).                               
011800                                                                          
011900 01  HELP-TABELL.                                                         
012000     03  HELP-INDX-TABELL OCCURS 5 TIMES.                                 
012100       05  HELP-DISTRIKT         PIC  9(5).                               
012200     EJECT                                                                
012300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012400 01  GENERELLA-SUBPROGRAM.                                                
012500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012710     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013000*   -COPY WMSGINIT                                                        
013010     EJECT                                                                
013020*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013030*   -COPY WMEDAREA                                                        
013100     SKIP3                                                                
013200 01  MESSAGE-CODES.                                                       
013300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013600     03  ERR-EMPTY-ROW-INDICATED PIC X(3)    VALUE '080'.                 
013700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014100     03  INF-AVSL-ORDER          PIC X(3)    VALUE '082'.                 
014200     EJECT                                                                
014300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014400*                                                                         
014500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014600     SKIP3                                                                
014700*01  MID -COPY W4I22401                                                   
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015000     SKIP3                                                                
015100*01  -COPY WMSGAREA                                                       
015200     EJECT                                                                
015300     03  MOD REDEFINES MSG-AREA.                                          
015400*      05  -COPY W4O22401    -PRE MOD-                                    
015500     EJECT                                                                
015900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016000     SKIP3                                                                
016100*01  -COPY WMFSAREA                                                       
016200     EJECT                                                                
016300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016400*                                                                         
016500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016600     SKIP3                                                                
016700 01  NYCKLAR-TILL-DLI.                                                    
016800     03  W-IDHTYP-X.                                                      
016900         05  W-IDHTYP            PIC X(4)     VALUE '4541'.               
017000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
017100                                                                          
017200     03  W-WDGXKEY-GU-X.                                                  
017300         05  W-IDDISTR-GU        PIC S9(5)    VALUE ZERO COMP-3.          
017400         05  W-IDANSK-GU         PIC S9(3)    VALUE ZERO COMP-3.          
017500         05  W-IDARTNR-GU        PIC S9(9)    VALUE ZERO COMP-3.          
017600         05  W-IDLOPNR-GU        PIC S9(3)    VALUE ZERO COMP-3.          
017700         05  W-IDORDER-GU        PIC S9(7)    VALUE ZERO COMP-3.          
017900                                                                          
018000                                                                          
018100     03  W-WDGXKEY-MIN-X.                                                 
018200         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO COMP-3.          
018300         05  W-IDANSK-MIN        PIC S9(3)    VALUE ZERO COMP-3.          
018400         05  W-IDARTNR-MIN       PIC S9(9)    VALUE ZERO COMP-3.          
018500         05  W-IDLOPNR-MIN       PIC S9(3)    VALUE ZERO COMP-3.          
018600         05  W-IDORDER-MIN       PIC S9(7)    VALUE ZERO COMP-3.          
018800                                                                          
018900     03  W-WDGXKEY-MAX-X.                                                 
019000         05  W-IDDISTR-MAX       PIC S9(5)    VALUE ZERO COMP-3.          
019100         05  W-IDANSK-MAX        PIC S9(3)    VALUE ZERO COMP-3.          
019200         05  W-IDARTNR-MAX       PIC S9(9)    VALUE ZERO COMP-3.          
019300         05  W-IDLOPNR-MAX       PIC S9(3)    VALUE ZERO COMP-3.          
019400         05  W-IDORDER-MAX       PIC S9(7)    VALUE ZERO COMP-3.          
019600                                                                          
019700     03  W-KDVORATG-X.                                                    
019800         05  W-KDVORATG          PIC X        VALUE '2'.                  
019900                                                                          
020000     03  W-IDARTNR-X.                                                     
020100         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
020200                                                                          
020210     03  W-IDDC-X.                                                        
020220         05  W-IDDC              PIC X(2)     VALUE SPACE.                
020230                                                                          
020270                                                                          
020300*    --- STATUS-KOD FRÅN IMS                                              
020400 01  STATUS-WS                   PIC XX.                                  
020500     88  SEGMENT-FINNS                       VALUE '  '.                  
020600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020800     88  BASEN-SLUT                          VALUE 'GB'.                  
020900                                                                          
021000     SKIP2                                                                
021100 01  GODK-STATUSKODER.                                                    
021200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021300     SKIP3                                                                
021400 01  SSA1                        PIC X(128).                              
021500 01  SSA2                        PIC X(128).                              
021600     EJECT                                                                
021700*    --- IMS FUNKTIONSKODER                                               
021800*01  -COPY W0003                                                          
021900     EJECT                                                                
022000*    ---  DLI INPUT-OUTPUT AREA                                           
022100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
022200     SKIP3                                                                
022300 01  DLI-IO-AREA.                                                         
022600     03  -COPY WDGX4542                                                   
022800     EJECT                                                                
022900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA2'.         
023000     SKIP3                                                                
023100 01  DLI-IO-AREA2.                                                        
023400     03  -COPY WDK901                                                     
023600     EJECT                                                                
023610 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA3'.         
023620     SKIP3                                                                
023630 01  DLI-IO-AREA3.                                                        
023640     03  -COPY WDR601                                                     
023641       05  -COPY W414205A -RED FIL-WDR601-DATA                            
023650     EJECT                                                                
023660 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
023670 01  DLI-IO-AREA-ARTS.                                                    
023680     03  WLARTS11.                                                        
023690*        05  -COPY WDK711                                                 
023691 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
023692 01  DLI-IO-WDK611.                                                       
023694*    03  -COPY WDK611                                                     
023695     EJECT                                                                
023700*---MSG-AERA FÖR HOPP TILL 4221-ORDERHUVUD FORBI/VOR                      
023800 01  FILLER                PIC X(16)  VALUE '4221-MSG-IO-AREA'.           
023900 01  4221-MSG-IO-AREA.                                                    
024000     03  4221-LL              PIC S9(4)  VALUE +35  COMP SYNC.            
024100     03  4221-Z1              PIC X.                                      
024200     03  4221-Z2              PIC X.                                      
024300     03  4221-TRANSKOD        PIC X(8)   VALUE 'W4T221  '.                
024400     03  4221-IDTRANS         PIC X(4)   VALUE '4224'.                    
024500     03  4221-SPRAK           PIC X.                                      
024600     03  4221-FLVORKO         PIC X(1).                                   
024700     03  4221-FLFORBI         PIC X(1).                                   
024800     03  4221-IDDISTR         PIC X(4).                                   
024900     03  4221-IDKUNDNR        PIC X(6).                                   
025000     03  4221-IDORDNR5        PIC X(5)   VALUE '+++++'.                   
025100     03  4221-KDORDKL         PIC X(1)   VALUE '0'.                       
025200                                                                          
025210*---MSG-AERA FÖR HOPP TILL 4231-ORDERHUVUD FORBI/VOR                      
025220 01  FILLER                PIC X(16)  VALUE '4231-MSG-IO-AREA'.           
025230 01  4231-MSG-IO-AREA.                                                    
025240     03  4231-LL              PIC S9(4)  VALUE +34  COMP SYNC.            
025250     03  4231-Z1              PIC X.                                      
025260     03  4231-Z2              PIC X.                                      
025270     03  4231-TRANSKOD        PIC X(8)   VALUE 'W4T231  '.                
025280     03  4231-IDTRANS         PIC X(4)   VALUE '4224'.                    
025290     03  4231-SPRAK           PIC X.                                      
025291     03  4231-FLVORKO         PIC X(1).                                   
025293     03  4231-IDDISTR         PIC X(4).                                   
025294     03  4231-IDKUNDNR        PIC X(6).                                   
025295     03  4231-IDORDNR5        PIC X(5)   VALUE '+++++'.                   
025296     03  4231-KDORDKL         PIC X(1)   VALUE '0'.                       
025304                                                                          
025310 LINKAGE SECTION.                                                         
025400                                                                          
025500*01  -COPY W0009      -PRE MSG-                                           
025600                                                                          
025700*01  -COPY W0009      -PRE 4221-                                          
025800     EJECT                                                                
025900                                                                          
025910*01  -COPY W0009      -PRE 4231-                                          
025920     EJECT                                                                
025930                                                                          
026000*01  -COPY W0008      -PRE USEA-                                          
026100     05  FILLER                  PIC X.                                   
026110                                                                          
026120*01  -COPY W0008      -PRE 4541-                                          
026130     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0008      -PRE ARTM-                                          
026400     05  FILLER                  PIC X.                                   
026410                                                                          
026420*01  -COPY W0008      -PRE FILA-                                          
026430     05  FILLER                  PIC X.                                   
026440                                                                          
026450*01  -COPY W0008      -PRE WDK6-                                          
026460     05  FILLER                  PIC X.                                   
026470                                                                          
026480*01  -COPY W0008      -PRE ARTS-                                          
026490     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600 PROCEDURE DIVISION  USING MSG-PCB 4221-PCB 4231-PCB USEA-PCB             
026610                                   4541-PCB ARTM-PCB FILA-PCB             
026620                                   WDK6-PCB ARTS-PCB.                     
026700 MAIN SECTION.                                                            
026800     ENTRY 'DLITCBL' USING MSG-PCB 4221-PCB 4231-PCB USEA-PCB             
026801                                   4541-PCB ARTM-PCB FILA-PCB             
026802                                   WDK6-PCB ARTS-PCB.                     
026900                                                                          
027000     PERFORM IMS-GET-MSG                                                  
027100     IF SEGMENT-FINNS                                                     
027200       PERFORM A-INIT                                                     
027300       PERFORM B-KOLLA-NYCKLAR                                            
027400       IF NYCKLAR-OK                                                      
027500         IF MFS-UPDATE                                                    
027800           PERFORM G-KOLLA-INPUT                                          
028300           IF INDATA-OK                                                   
028400             PERFORM H-UPPDATERA                                          
028410             PERFORM F-LAES-VISA-INFO                                     
028500           END-IF                                                         
029100         ELSE                                                             
029300           IF MFS-FIRST                                                   
029400             PERFORM C-FOERSTA-SIDA                                       
029500           ELSE                                                           
029600             IF MFS-NEXT                                                  
029700               PERFORM D-NAESTA-SIDA                                      
029800             ELSE                                                         
029900               PERFORM E-SAMMA-SIDA                                       
030000             END-IF                                                       
030100           END-IF                                                         
030200           IF ALLT-OK                                                     
030300             PERFORM F-LAES-VISA-INFO                                     
030400           END-IF                                                         
030500         END-IF                                                           
030600       END-IF                                                             
030700       IF STARTA-4221 OR STARTA-4231                                      
030710         CONTINUE                                                         
030720       ELSE                                                               
030800         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
030900         PERFORM IMS-INSERT-MSG                                           
031000       END-IF                                                             
031100     END-IF                                                               
031200                                                                          
031300     MOVE ZERO TO RETURN-CODE                                             
031400     GOBACK                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 A-INIT SECTION.                                                          
031800     IF MSG-DUBBLA-TRANSKODER                                             
031900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I22401                 
032000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
032100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032200     ELSE                                                                 
032300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I22401                  
032400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
032500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032600     END-IF                                                               
032700                                                                          
032800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
032900     MOVE MSG-IDPFK            TO MFS-IDPFK                               
033000     MOVE MFS-IDTRANS          TO W-IDTRANS                               
033100                                                                          
033200     MOVE LOW-VALUE            TO MSG-AREA                                
033300     MOVE 'W4O22401'           TO MFS-IDMOD                               
033400     MOVE '4224'               TO MOD-IDTRANS                             
033500     PERFORM MFS-RENSA-FAELT-IN                                           
033510     PERFORM MFS-RENSA-FAELT-UT                                           
033600     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
033800                                                                          
033900     IF NOT EGEN-MID                                                      
033910       MOVE SPACE           TO MID-IDDISTR-1-IN                           
033920                               MID-IDDISTR-2-IN                           
033930                               MID-IDDISTR-3-IN                           
033940                               MID-IDDISTR-4-IN                           
033950                               MID-IDANSK-IN                              
033960                               MID-IDARTNR-IN                             
033970                               MID-IDDISTR-1-UT                           
033980                               MID-IDDISTR-2-UT                           
033990                               MID-IDDISTR-3-UT                           
033991                               MID-IDDISTR-4-UT                           
033992                               MID-IDANSK-UT                              
033993                               MID-IDARTNR-UT                             
034000       MOVE SPACE TO MFS-KDTRTYP                                          
034100       MOVE '7' TO MFS-IDPFK                                              
034200     END-IF                                                               
034300                                                                          
034400     IF ENGLISH-TEXT                                                      
034500       MOVE +2 TO SPRAK-IX                                                
034600       MOVE 'GB ' TO MED-IDSKYLT                                          
034700     ELSE                                                                 
034800       MOVE +1 TO SPRAK-IX                                                
034900       MOVE 'S  ' TO MED-IDSKYLT                                          
035000     END-IF                                                               
035200                                                                          
035210     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O22401 + 4                  
035300     MOVE LOW-VALUE            TO W-WDGXKEY-MIN-X                         
035400     MOVE HIGH-VALUE           TO W-WDGXKEY-MAX-X                         
035500     .                                                                    
035600     EJECT                                                                
037300 B-KOLLA-NYCKLAR SECTION.                                                 
037400                                                                          
037500     MOVE JA TO NYCKLAR-SW                                                
037600     MOVE JA TO ALLT-SW                                                   
037700     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-1-IN                             
037900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-2-IN                             
038100     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-3-IN                             
038300     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-4-IN                             
038500     MOVE MFS-RENSA-FAELT TO MOD-IDANSK-IN                                
038700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
038800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
038810     MOVE '001'             TO MSGI-KDCALL                                
038820     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
038821     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
038822     MOVE '4224'            TO MSGI-IDTRANS                               
038830     IF MFS-IDTRANS = '4224'                                              
038840        MOVE MID-IDDISTR-1-IN  TO MSGI-IDDISTR                            
038850        MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                            
038851        MOVE MID-IDANSK-IN     TO MSGI-IDANSK                             
038860     END-IF                                                               
038870     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038900                                                                          
038910     IF MSGI-IDUSER(1:4) = 'PHUS' OR 'PHCA'                               
038920       MOVE JA            TO USER-SW                                      
038930     ELSE                                                                 
038940       MOVE NEJ           TO USER-SW                                      
038950     END-IF                                                               
038951     IF NOT EGEN-MID                                                      
038952       IF W-IDTRANS = '4223' OR W-IDTRANS = '4233'                        
038953         MOVE MSGI-IDDISTR      TO MID-IDDISTR-1-IN                       
038955         MOVE MSGI-IDANSK       TO MID-IDANSK-IN                          
038956         MOVE MSGI-IDARTNR      TO MID-IDARTNR-IN                         
038958       END-IF                                                             
038959     END-IF                                                               
038960                                                                          
039000     MOVE +1 TO DISTR-INDX                                                
039100     PERFORM 5 TIMES                                                      
039200       MOVE ZERO          TO DISTRIKT(DISTR-INDX)                         
039300       MOVE ZERO          TO HELP-DISTRIKT(DISTR-INDX)                    
039400       ADD +1 TO DISTR-INDX                                               
039500     END-PERFORM                                                          
039600                                                                          
039700     MOVE LOW-VALUE       TO W-WDGXKEY-MIN-X                              
039800     MOVE HIGH-VALUE      TO W-WDGXKEY-MAX-X                              
039900                                                                          
040000     PERFORM BB-KOLLA-DISTR                                               
040100     PERFORM BC-KOLLA-IDANSK                                              
040200     PERFORM BD-KOLLA-ARTNR                                               
040201     IF EGEN-MID                                                          
040210      IF DISTRIKT(1) = ZERO AND                                           
040220        W-IDANSK = ZERO AND                                               
040230        ARTIKEL-EJ-IFYLLD                                                 
040240        MOVE NEJ TO NYCKLAR-SW                                            
040241        MOVE '401' TO MED-IDMFSFEL                                        
040242      END-IF                                                              
040250     END-IF                                                               
040300                                                                          
040410     IF NYCKLAR-FEL                                                       
040500       IF MED-IDMFSFEL = SPACE                                            
040600          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
040700       END-IF                                                             
040800       CALL WMEDKONV USING MED-WMEDAREA                                   
040900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
041000       PERFORM MFS-RENSA-FAELT-UT                                         
041100     END-IF                                                               
041200                                                                          
041300     IF EGEN-MID                                                          
041310     AND MID-INPUT NOT = ALL '+' AND MFS-QUERY                            
041400       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
041500       CALL WMEDKONV USING MED-WMEDAREA                                   
041600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
041700     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700                                                                          
042800 BB-KOLLA-DISTR SECTION.                                                  
042900                                                                          
043000     PERFORM BBA-KOLLA-DISTR-1                                            
043100     PERFORM BBB-KOLLA-DISTR-2                                            
043200     PERFORM BBC-KOLLA-DISTR-3                                            
043300     PERFORM BBD-KOLLA-DISTR-4                                            
043400     PERFORM BBE-STUVA-OM-DISTR                                           
043500     IF DISTRIKT(1) NOT NUMERIC                                           
043700           MOVE NEJ TO NYCKLAR-SW                                         
043800     END-IF                                                               
043900                                                                          
043910     IF DISTRIKT(1) = ZERO                                                
043920        MOVE NEJ TO DISTR-IFYLLD-SW                                       
043921     ELSE                                                                 
043922        MOVE JA  TO DISTR-IFYLLD-SW                                       
043930     END-IF                                                               
043940                                                                          
044000     .                                                                    
044100     EJECT                                                                
044200                                                                          
044300 BBA-KOLLA-DISTR-1 SECTION.                                               
044400                                                                          
044420     MOVE MSGI-IDDISTR   TO WS-IDDISTR                                    
044421                            MOD-IDDISTR-1-UT                              
044430     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
044431     INSPECT MOD-IDDISTR-1-UT REPLACING LEADING ZERO BY SPACE             
044440                                                                          
044500     IF MID-IDDISTR-1-IN = ALL '+'                                        
044600       CONTINUE                                                           
045000     ELSE                                                                 
045500       MOVE '7'         TO MFS-IDPFK                                      
045600       MOVE SPACE       TO MFS-KDTRTYP                                    
045700     END-IF                                                               
045800                                                                          
046100                                                                          
046200     IF WS-IDDISTR NUMERIC                                                
046300       IF WS-IDDISTR = ALL '0'                                            
046400         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-1-UT                         
046600       ELSE                                                               
046700         MOVE WS-IDDISTR TO HELP-DISTRIKT(1)                              
046900       END-IF                                                             
047000     ELSE                                                                 
047100       MOVE NEJ TO NYCKLAR-SW                                             
047200     END-IF                                                               
047300                                                                          
047400     .                                                                    
047500     EJECT                                                                
047600                                                                          
047700 BBB-KOLLA-DISTR-2 SECTION.                                               
047800                                                                          
047900     IF MID-IDDISTR-2-IN = ALL '+'                                        
048000       MOVE MID-IDDISTR-2-UT TO WS-IDDISTR                                
048100                              MOD-IDDISTR-2-UT                            
048300       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
048400     ELSE                                                                 
048500       MOVE MID-IDDISTR-2-IN TO WS-IDDISTR                                
048600                              MOD-IDDISTR-2-UT                            
048800       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
048900       MOVE '7'         TO MFS-IDPFK                                      
049000       MOVE SPACE       TO MFS-KDTRTYP                                    
049100     END-IF                                                               
049200                                                                          
049300     INSPECT MOD-IDDISTR-2-UT REPLACING LEADING ZERO BY SPACE             
049500                                                                          
049600     IF WS-IDDISTR NUMERIC                                                
049700       IF WS-IDDISTR = ALL '0'                                            
049800         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-2-UT                         
050000       ELSE                                                               
050100         MOVE WS-IDDISTR TO HELP-DISTRIKT(2)                              
050300       END-IF                                                             
050400     ELSE                                                                 
050500       MOVE NEJ TO NYCKLAR-SW                                             
050600     END-IF                                                               
050700                                                                          
050800     .                                                                    
050900     EJECT                                                                
051000                                                                          
051100 BBC-KOLLA-DISTR-3 SECTION.                                               
051200                                                                          
051300     IF MID-IDDISTR-3-IN = ALL '+'                                        
051400       MOVE MID-IDDISTR-3-UT TO WS-IDDISTR                                
051500                              MOD-IDDISTR-3-UT                            
051700       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
051800     ELSE                                                                 
051900       MOVE MID-IDDISTR-3-IN TO WS-IDDISTR                                
052000                              MOD-IDDISTR-3-UT                            
052200       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
052300       MOVE '7'         TO MFS-IDPFK                                      
052400       MOVE SPACE       TO MFS-KDTRTYP                                    
052500     END-IF                                                               
052600                                                                          
052700     INSPECT MOD-IDDISTR-3-UT REPLACING LEADING ZERO BY SPACE             
052900                                                                          
053000     IF WS-IDDISTR NUMERIC                                                
053100       IF WS-IDDISTR = ALL '0'                                            
053200         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-3-UT                         
053400       ELSE                                                               
053500         MOVE WS-IDDISTR TO HELP-DISTRIKT(3)                              
053700       END-IF                                                             
053800     ELSE                                                                 
053900       MOVE NEJ TO NYCKLAR-SW                                             
054000     END-IF                                                               
054100                                                                          
054200     .                                                                    
054300     EJECT                                                                
054400                                                                          
054500 BBD-KOLLA-DISTR-4 SECTION.                                               
054600                                                                          
054700     IF MID-IDDISTR-4-IN = ALL '+'                                        
054800       MOVE MID-IDDISTR-4-UT TO WS-IDDISTR                                
054900                              MOD-IDDISTR-4-UT                            
055100       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
055200     ELSE                                                                 
055300       MOVE MID-IDDISTR-4-IN TO WS-IDDISTR                                
055400                              MOD-IDDISTR-4-UT                            
055600       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
055700       MOVE '7'         TO MFS-IDPFK                                      
055800       MOVE SPACE       TO MFS-KDTRTYP                                    
055900     END-IF                                                               
056000                                                                          
056100     INSPECT MOD-IDDISTR-4-UT REPLACING LEADING ZERO BY SPACE             
056300                                                                          
056400     IF WS-IDDISTR NUMERIC                                                
056500       IF WS-IDDISTR = ALL '0'                                            
056600         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-4-UT                         
056800       ELSE                                                               
056900         MOVE WS-IDDISTR TO HELP-DISTRIKT(4)                              
057100       END-IF                                                             
057200     ELSE                                                                 
057300       MOVE NEJ TO NYCKLAR-SW                                             
057400     END-IF                                                               
057500                                                                          
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 BBE-STUVA-OM-DISTR SECTION.                                              
058000                                                                          
058100     MOVE +1 TO DISTR-INDX                                                
058200     MOVE +1 TO INDX                                                      
058300     PERFORM 4 TIMES                                                      
058400       IF HELP-DISTRIKT(DISTR-INDX) > ZERO                                
058500          MOVE HELP-DISTRIKT(DISTR-INDX) TO                               
058600                    DISTRIKT(INDX)                                        
058700          ADD +1 TO INDX                                                  
058800       END-IF                                                             
058900       ADD +1 TO DISTR-INDX                                               
059000     END-PERFORM                                                          
059100     PERFORM BBEA-KOLLA-OM-DISTR-AR-LIKA                                  
059200     .                                                                    
059300     EJECT                                                                
059400                                                                          
059500 BBEA-KOLLA-OM-DISTR-AR-LIKA SECTION.                                     
059600                                                                          
059700     IF DISTRIKT(1) > ZERO                                                
059800        IF DISTRIKT(1) = DISTRIKT(2) OR                                   
059900                         DISTRIKT(3) OR                                   
060000                         DISTRIKT(4)                                      
060100           MOVE NEJ TO NYCKLAR-SW                                         
060200           MOVE '747' TO MED-IDMFSFEL                                     
060300        END-IF                                                            
060400     END-IF                                                               
060500                                                                          
060600     IF DISTRIKT(2) > ZERO                                                
060700        IF DISTRIKT(2) = DISTRIKT(3) OR                                   
060800                         DISTRIKT(4)                                      
060900           MOVE NEJ TO NYCKLAR-SW                                         
061000           MOVE '747' TO MED-IDMFSFEL                                     
061100        END-IF                                                            
061200     END-IF                                                               
061300                                                                          
061400     IF DISTRIKT(3) > ZERO                                                
061500        IF DISTRIKT(3) = DISTRIKT(4)                                      
061600           MOVE NEJ TO NYCKLAR-SW                                         
061700           MOVE '747' TO MED-IDMFSFEL                                     
061800        END-IF                                                            
061900     END-IF                                                               
062000     .                                                                    
062100     EJECT                                                                
062200                                                                          
062300 BC-KOLLA-IDANSK SECTION.                                                 
062400                                                                          
062500     IF MID-IDANSK-IN = ALL '+'                                           
062600       MOVE MID-IDANSK-UT TO WS-IDANSK                                    
062700     ELSE                                                                 
062800       MOVE MID-IDANSK-IN TO WS-IDANSK                                    
062900       MOVE '7'           TO MFS-IDPFK                                    
063000       MOVE SPACE         TO MFS-KDTRTYP                                  
063100     END-IF                                                               
063200                                                                          
063300     INSPECT WS-IDANSK REPLACING LEADING SPACE BY ZERO                    
063400                                                                          
063500     IF WS-IDANSK NUMERIC                                                 
063510        IF WS-IDANSK         >  ZERO                                      
063600           MOVE WS-IDANSK   TO MOD-IDANSK-UT                              
063800                               W-IDANSK-MIN                               
063900                               W-IDANSK-MAX                               
063901                               W-IDANSK                                   
063902        END-IF                                                            
063910     ELSE                                                                 
063920       MOVE NEJ             TO NYCKLAR-SW                                 
063940     END-IF                                                               
064001                                                                          
064010     INSPECT MOD-IDANSK-UT REPLACING LEADING ZERO BY SPACE                
064100                                                                          
064200     .                                                                    
064300     EJECT                                                                
064400                                                                          
064500                                                                          
064510 BD-KOLLA-ARTNR SECTION.                                                  
064520                                                                          
064530     MOVE NEJ              TO ARTIKEL-IFYLLD-SW                           
064540     IF MID-IDARTNR-IN     = ALL '+'                                      
064550       MOVE MID-IDARTNR-UT TO WS-IDARTNR                                  
064560                              MOD-IDARTNR-UT                              
064570       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
064580     ELSE                                                                 
064590       MOVE MID-IDARTNR-IN TO WS-IDARTNR                                  
064591                              MOD-IDARTNR-UT                              
064592       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
064593       MOVE '7'         TO MFS-IDPFK                                      
064594       MOVE SPACE       TO MFS-KDTRTYP                                    
064595     END-IF                                                               
064596                                                                          
064597     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
064598                                                                          
064599     IF WS-IDARTNR NUMERIC                                                
064600       IF WS-IDARTNR      = ALL '0'                                       
064601         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
064602       ELSE                                                               
064603         MOVE JA          TO ARTIKEL-IFYLLD-SW                            
064604         MOVE WS-IDARTNR  TO W-IDARTNR                                    
064605       END-IF                                                             
064606     ELSE                                                                 
064607       MOVE NEJ TO NYCKLAR-SW                                             
064608     END-IF                                                               
064609                                                                          
064610     .                                                                    
064611     EJECT                                                                
064612                                                                          
068100 C-FOERSTA-SIDA SECTION.                                                  
068200                                                                          
068300     IF W-IDTRANS = '4223' OR W-IDTRANS = '4233'                          
068400       MOVE INF-AVSL-ORDER TO MED-IDMFSINF                                
068500     END-IF                                                               
068600     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
068700     CALL WMEDKONV USING MED-WMEDAREA                                     
068800     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
068900     IF W-IDTRANS = '4223' OR W-IDTRANS = '4233'                          
069000       MOVE MED-MFSINF       TO MOD-TEMFSINF                              
069100       MOVE '   NR=> '       TO MOD-TEMFSINF (17:8)                       
069200       MOVE MID-IDORDNR-VOR  TO MOD-TEMFSINF (25:5)                       
069300     END-IF                                                               
069400                                                                          
069500*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
069600     MOVE ZERO  TO MOD-IDDISTR-ENTER                                      
069700     MOVE ZERO  TO MOD-IDANSK-ENTER                                       
069800     MOVE ZERO  TO MOD-IDARTNR-ENTER                                      
069900     MOVE ZERO  TO MOD-IDLOPNR-ENTER                                      
070000     MOVE ZERO  TO MOD-IDORDER-ENTER                                      
070100     MOVE ZERO  TO MOD-IDDISTR-NEXT                                       
070200     MOVE ZERO  TO MOD-IDANSK-NEXT                                        
070300     MOVE ZERO  TO MOD-IDARTNR-NEXT                                       
070400     MOVE ZERO  TO MOD-IDLOPNR-NEXT                                       
070500     MOVE ZERO  TO MOD-IDORDER-NEXT                                       
070600     MOVE JA TO ALLT-SW                                                   
070700     MOVE '7' TO PFK-SW                                                   
070800     .                                                                    
070900     EJECT                                                                
071000 D-NAESTA-SIDA SECTION.                                                   
071100                                                                          
071200     MOVE MID-IDDISTR-NEXT  TO W-IDDISTR-MIN                              
071300                               W-IDDISTR-MAX                              
071400     MOVE MID-IDANSK-NEXT   TO W-IDANSK-MIN                               
071500     MOVE MID-IDARTNR-NEXT  TO W-IDARTNR-MIN                              
071600     MOVE MID-IDLOPNR-NEXT  TO W-IDLOPNR-MIN                              
071700     MOVE MID-IDORDER-NEXT  TO W-IDORDER-MIN                              
071800     MOVE JA TO ALLT-SW                                                   
071900     MOVE '8' TO PFK-SW                                                   
072000     .                                                                    
072100     EJECT                                                                
072200 E-SAMMA-SIDA SECTION.                                                    
072300                                                                          
072400     MOVE MID-IDDISTR-ENTER TO W-IDDISTR-MIN                              
072500                               W-IDDISTR-MAX                              
072600     MOVE MID-IDANSK-ENTER  TO W-IDANSK-MIN                               
072700     MOVE MID-IDARTNR-ENTER TO W-IDARTNR-MIN                              
072800     MOVE MID-IDLOPNR-ENTER TO W-IDLOPNR-MIN                              
072900     MOVE MID-IDORDER-ENTER TO W-IDORDER-MIN                              
073000     MOVE JA TO ALLT-SW                                                   
073100     MOVE ' ' TO PFK-SW                                                   
073200     .                                                                    
073300     EJECT                                                                
073400 F-LAES-VISA-INFO SECTION.                                                
073500                                                                          
073600     MOVE +1                       TO INDX                                
073700     MOVE JA                       TO FIRST-TIME-SW                       
073800     IF PFK-7-TRYCKT                                                      
073900       MOVE +1                     TO DISTR-INDX                          
074000     ELSE                                                                 
074100       IF PFK-8-TRYCKT                                                    
074200         MOVE MID-DISTR-INDX-NEXT  TO DISTR-INDX                          
074300       ELSE                                                               
074400         MOVE MID-DISTR-INDX-ENTER TO DISTR-INDX                          
074500       END-IF                                                             
074600     END-IF                                                               
074700                                                                          
074800     IF DISTR-INDX                 =  ZERO                                
074900        MOVE +1                    TO DISTR-INDX                          
075000     END-IF                                                               
075100                                                                          
075200     PERFORM UNTIL DISTRIKT(DISTR-INDX) = ZERO AND DISTR-IFYLLD OR        
075210                   DISTR-EJ-IFYLLD AND (BASEN-SLUT OR                     
075220                   SEGMENT-SAKNAS) OR                                     
075300               INDX > MAX-INDX                                            
075400                                                                          
075500        PERFORM IMS-GU-4541                                               
075600                                                                          
075610        MOVE DISTRIKT(DISTR-INDX)  TO W-IDDISTR-MIN                       
075620                                      W-IDDISTR-MAX                       
075670                                                                          
075680        IF DISTR-EJ-IFYLLD AND PFK-7-TRYCKT                               
075690           MOVE ZERO               TO W-IDDISTR-MIN                       
075691           MOVE 9999               TO W-IDDISTR-MAX                       
075692        END-IF                                                            
075693                                                                          
075694        IF DISTR-EJ-IFYLLD AND PFK-8-TRYCKT                               
075695           MOVE MID-IDDISTR-NEXT   TO W-IDDISTR-MIN                       
075696           MOVE 9999               TO W-IDDISTR-MAX                       
075697        END-IF                                                            
075698                                                                          
075699        IF DISTR-EJ-IFYLLD AND PFK-ENTER-TRYCKT                           
075700           MOVE MID-IDDISTR-ENTER  TO W-IDDISTR-MIN                       
075701           MOVE 9999               TO W-IDDISTR-MAX                       
075702        END-IF                                                            
075703                                                                          
075710        PERFORM FA-LAES-VISA-DISTR                                        
075800                                                                          
075900        ADD +1                     TO DISTR-INDX                          
076000        IF  WS-IDANSK NUMERIC AND                                         
076100            WS-IDANSK > ZERO                                              
076110             MOVE WS-IDANSK        TO W-IDANSK-MIN                        
076120                                      W-IDANSK-MAX                        
076130        ELSE                                                              
076140             MOVE ZERO             TO W-IDANSK-MIN                        
076150             MOVE 999              TO W-IDANSK-MAX                        
076160        END-IF                                                            
076200     END-PERFORM                                                          
076300                                                                          
076410     IF INDX           = 1                                                
076500       IF MED-IDMFSFEL = '006'                                            
076600          MOVE SPACE   TO MED-MFSFEL                                      
076700       END-IF                                                             
076800       IF W-IDTRANS    =  '4223' OR W-IDTRANS = '4233'                    
076900          CONTINUE                                                        
076910       ELSE                                                               
077000          MOVE '059'   TO MED-IDMFSINF                                    
077100          CALL WMEDKONV USING MED-WMEDAREA                                
077200          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
077300          PERFORM MFS-RENSA-FAELT-UT                                      
077400          MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                            
077600       END-IF                                                             
077610     END-IF                                                               
077700                                                                          
077710     PERFORM UNTIL  INDX > MAX-INDX                                       
077720                                                                          
077730        MOVE MFS-STAENG-FAELT      TO MOD-CMD-ATTR (INDX)                 
077740                                      MOD-TEVORMRK-ATTR (INDX)            
077750                                      MOD-BERADREF-ATTR (INDX)            
077751        PERFORM MFS-RENSA-FAELT-PA-RAD                                    
077752        ADD +1                     TO INDX                                
077760                                                                          
077770     END-PERFORM                                                          
077780                                                                          
078400                                                                          
078500     .                                                                    
078600     EJECT                                                                
078700 FA-LAES-VISA-DISTR SECTION.                                              
078800                                                                          
078900     PERFORM IMS-GNP-DISTR                                                
079000     IF INDX = +1 AND SEGMENT-FINNS                                       
079100       PERFORM FAA-FLYTTA-IN-ENTER-NYCKLAR                                
079200     END-IF                                                               
079300                                                                          
079400     IF SEGMENT-FINNS                                                     
079500       PERFORM UNTIL INDX > MAX-INDX  OR                                  
079600                      SEGMENT-SAKNAS  OR                                  
079700                      BASEN-SLUT                                          
079710         IF ((ARTIKEL-EJ-IFYLLD)  OR                                      
079720            (ARTIKEL-IFYLLD AND 4542-IDARTNR = W-IDARTNR)) AND            
079721            ((W-IDANSK > +0  AND 4542-IDANSK = W-IDANSK) OR               
079722            W-IDANSK = +0)                                                
079730            IF USER-CDC                                                   
079740              MOVE 4542-IDDC       TO WS-IDDC                             
079750              IF NDC                                                      
079800                PERFORM FAB-FLYTTA-MOD-TILL-BILD                          
079801              END-IF                                                      
079802            ELSE                                                          
079803              MOVE 4542-IDDC       TO WS-IDDC                             
079804              IF NDC                                                      
079805                PERFORM FAB-FLYTTA-MOD-TILL-BILD                          
079806              END-IF                                                      
079807            END-IF                                                        
079810         END-IF                                                           
079900         PERFORM IMS-GNP-DISTR                                            
080000       END-PERFORM                                                        
080100                                                                          
080200       IF INDX > MAX-INDX                                                 
080300         PERFORM FAC-FLYTTA-IN-NEXT-NYCKLAR                               
080400       END-IF                                                             
080500     END-IF                                                               
080600                                                                          
080700     .                                                                    
080800     EJECT                                                                
080900                                                                          
081000 FAA-FLYTTA-IN-ENTER-NYCKLAR SECTION.                                     
081100                                                                          
081200     MOVE 4542-IDDISTR            TO MOD-IDDISTR-ENTER                    
081300                                     MOD-IDDISTR-NEXT                     
081400     MOVE 4542-IDANSK             TO MOD-IDANSK-ENTER                     
081500                                     MOD-IDANSK-NEXT                      
081600     MOVE 4542-IDARTNR            TO MOD-IDARTNR-ENTER                    
081700                                     MOD-IDARTNR-NEXT                     
081800     MOVE 4542-IDLOPNR            TO MOD-IDLOPNR-ENTER                    
081900                                     MOD-IDLOPNR-NEXT                     
082000     MOVE 4542-IDORDER            TO MOD-IDORDER-ENTER                    
082100                                     MOD-IDORDER-NEXT                     
082200     MOVE DISTR-INDX              TO MOD-DISTR-INDX-ENTER                 
082300                                     MOD-DISTR-INDX-NEXT                  
082400                                                                          
082500     .                                                                    
082600     EJECT                                                                
082610                                                                          
082700 FAB-FLYTTA-MOD-TILL-BILD SECTION.                                        
082800                                                                          
082900     MOVE 4542-TIREGDAT           TO MOD-TIREGDAT(INDX)                   
083000     IF 4542-TEVORMRK = '**'                                              
083100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVORMRK-ATTR(INDX)             
083200        MOVE 4542-TEVORMRK         TO MOD-TEVORMRK(INDX)                  
083300        IF FIRST-TIME                                                     
083400           MOVE MFS-ADD-SAETT-CURSOR TO MOD-CMD-ATTR(INDX)                
083500           MOVE NEJ                  TO FIRST-TIME-SW                     
083600        END-IF                                                            
083610        IF MFS-UPDATE                                                     
083620           CONTINUE                                                       
083630        ELSE                                                              
083700          IF ENGLISH-TEXT                                                 
083800             MOVE 'ATTEMPT TO ORDER HAS FAILED!! TRY AGAIN.'              
083900                                     TO MOD-TEMFSINF                      
084000             MOVE 'ASTERIX MARKED ORDERLINE IS WRONG.'                    
084100                                     TO MOD-TEMFSFEL                      
084200          ELSE                                                            
084300                                                                          
084400             MOVE                                                         
084500         'FÖRSÖK ATT LÄGGA ORDERN HAR MISSLYCKATS!! FÖRSÖK IGEN.'         
084600                                  TO MOD-TEMFSINF                         
084700             MOVE 'ASTERIX-MÄRKT ORDERRAD FELAKTIG'                       
084800                                  TO MOD-TEMFSFEL                         
084900          END-IF                                                          
084910        END-IF                                                            
085000     END-IF                                                               
085100     MOVE 4542-TEVORMRK           TO MOD-TEVORMRK(INDX)                   
085200     MOVE 4542-IDDISTR            TO MOD-IDDISTR(INDX)                    
085300     MOVE 4542-IDKUNDNR           TO MOD-IDKUNDNR(INDX)                   
085400     MOVE 4542-IDORDNR7           TO MOD-IDORDNR7(INDX)                   
085500     MOVE 4542-IDANSK             TO MOD-IDANSK(INDX)                     
085600     MOVE 4542-IDARTNR            TO MOD-IDARTNR(INDX)                    
085700     MOVE 4542-KVBEART-Q          TO MOD-KVBEART(INDX)                    
085800     COMPUTE DIFF-NUM             = 4542-KVBEART-Q -                      
085900                                    4542-KVPREAVB                         
086000     MOVE DIFF-NUM                TO MOD-DIFF(INDX)                       
086100     INSPECT MOD-DIFF(INDX) REPLACING LEADING ZERO BY SPACE               
086200                                                                          
086300     MOVE 4542-KDORDBEK           TO MOD-KDORDBEK(INDX)                   
086310     MOVE 4542-BERADREF           TO MOD-BERADREF(INDX)                   
086320     MOVE 4542-IDDC               TO MOD-IDDC(INDX)                       
086400     MOVE 4542-IDLOPNR            TO MOD-IDLOPNR(INDX)                    
086500     MOVE 4542-IDORDER            TO MOD-IDORDER(INDX)                    
086510     PERFORM FABA-KOLLA-AK-I-LAGER                                        
086600     ADD +1 TO INDX                                                       
086700     .                                                                    
086800     EJECT                                                                
086810 FABA-KOLLA-AK-I-LAGER SECTION.                                           
086811                                                                          
086812     MOVE 4542-IDARTNR TO W-IDARTNR                                       
086813     MOVE 4542-IDDC    TO W-IDDC  WS-IDDC                                 
086814     IF CDC                                                               
086815       IF W-IDARTNR = WS-611-IDARTNR                                      
086816         MOVE '  ' TO STATUS-WS                                           
086817       ELSE                                                               
086818         PERFORM IMS-GU-ARTC-WDK611                                       
086821       END-IF                                                             
086822       IF SEGMENT-FINNS                                                   
086823         MOVE W-IDARTNR TO WS-611-IDARTNR                                 
086824         IF CLAG-KVAKS-CDC > ZERO                                         
086825           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIREGDAT-ATTR(INDX)          
086826*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-ATTR(INDX)           
086827*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-ATTR(INDX)          
086828*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDORDNR7-ATTR(INDX)          
086829*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANSK-ATTR(INDX)            
086830*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-ATTR(INDX)           
086831         END-IF                                                           
086832       END-IF                                                             
086833     ELSE                                                                 
086834       IF W-IDARTNR = WS-711-IDARTNR AND W-IDDC = WS-711-IDDC             
086835         MOVE '  ' TO STATUS-WS                                           
086836       ELSE                                                               
086837         PERFORM IMS-GU-ARTS-WDK711                                       
086838       END-IF                                                             
086839                                                                          
086840       IF SEGMENT-FINNS                                                   
086841         MOVE W-IDARTNR TO WS-711-IDARTNR                                 
086842         MOVE W-IDDC    TO WS-711-IDDC                                    
086843         IF SLAG-KVAKS-SDC > ZERO                                         
086844           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIREGDAT-ATTR(INDX)          
086845*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-ATTR(INDX)           
086846*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-ATTR(INDX)          
086847*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDORDNR7-ATTR(INDX)          
086848*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANSK-ATTR(INDX)            
086849*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-ATTR(INDX)           
086850         END-IF                                                           
086851       END-IF                                                             
086852     END-IF                                                               
086853     .                                                                    
086860     EJECT                                                                
086900 FAC-FLYTTA-IN-NEXT-NYCKLAR SECTION.                                      
087000                                                                          
087100     MOVE 4542-IDDISTR            TO MOD-IDDISTR-NEXT                     
087200     MOVE 4542-IDANSK             TO MOD-IDANSK-NEXT                      
087300     MOVE 4542-IDARTNR            TO MOD-IDARTNR-NEXT                     
087400     MOVE 4542-IDLOPNR            TO MOD-IDLOPNR-NEXT                     
087500     MOVE 4542-IDORDER            TO MOD-IDORDER-NEXT                     
087600     MOVE DISTR-INDX              TO MOD-DISTR-INDX-NEXT                  
087700                                                                          
087800     IF MOD-TEMFSINF = SPACE OR MFS-RENSA-FAELT                           
087900        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
088000        CALL WMEDKONV USING MED-WMEDAREA                                  
088100        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
088200     END-IF                                                               
088300     .                                                                    
088400     EJECT                                                                
088500                                                                          
088600 G-KOLLA-INPUT SECTION.                                                   
088700                                                                          
088800     MOVE JA                    TO FIRST-TIME-SW                          
088900     MOVE JA                    TO INDATA-SW                              
089000                                                                          
089100     MOVE +1 TO INDX                                                      
089200     IF MID-INPUT                  =  ALL '+'                             
089300        MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                        
089400        CALL WMEDKONV USING MED-WMEDAREA                                  
089500        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
089600        PERFORM MFS-ROR-EJ-FAELT-UT                                       
089700        MOVE NEJ                   TO INDATA-SW                           
089800     ELSE                                                                 
089900        IF MID-INDATA-CMD              = ALL '+' AND                      
090000          (MID-INDATA-BERADREF     NOT = ALL '+'      OR                  
090100           MID-INDATA-MAERKE       NOT = ALL '+')                         
090200            CONTINUE                                                      
090300        ELSE                                                              
090400           IF MID-INDATA-CMD         NOT = ALL '+'                        
090500              IF MID-INDATA-BERADREF NOT = ALL '+'  OR                    
090600                 MID-INDATA-MAERKE   NOT = ALL '+'                        
090700                  MOVE NEJ           TO INDATA-SW                         
090710                  MOVE '007'         TO  MED-IDMFSFEL                     
090720                  CALL WMEDKONV USING MED-WMEDAREA                        
090730                  MOVE MED-MFSFEL    TO MOD-TEMFSFEL                      
090800                  PERFORM GA-MARKERA-INDATA-FAELT                         
090900              ELSE                                                        
091000                  PERFORM GB-KOLLA-CMD                                    
091100              END-IF                                                      
091200           END-IF                                                         
091300        END-IF                                                            
091310        IF INDATA-FEL                                                     
091320           PERFORM MFS-ROR-EJ-FAELT-UT                                    
091321           PERFORM MFS-ROR-EJ-FAELT-IN                                    
091330        END-IF                                                            
091400     END-IF                                                               
091500     .                                                                    
091600     EJECT                                                                
091700 GA-MARKERA-INDATA-FAELT   SECTION.                                       
091800                                                                          
091900     MOVE +1                        TO INDX                               
092000     PERFORM UNTIL INDX             > MAX-INDX                            
092100                                                                          
092110     INSPECT MID-IDDISTR(INDX)  REPLACING LEADING SPACE BY ZERO           
092140     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
092150                                                                          
092200        IF MID-CMD(INDX)            = ALL '+' OR SPACE                    
092300           CONTINUE                                                       
092400        ELSE                                                              
092500           MOVE MFS-ROER-EJ-FAELT     TO MOD-CMD(INDX)                    
092600           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR(INDX)               
092700        END-IF                                                            
092800                                                                          
092900        IF MID-MAERKE(INDX)         = ALL '+' OR SPACE                    
093000           CONTINUE                                                       
093100        ELSE                                                              
093200           MOVE MFS-ROER-EJ-FAELT     TO MOD-TEVORMRK(INDX)               
093300           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEVORMRK-ATTR(INDX)          
093400        END-IF                                                            
093500                                                                          
093600        IF MID-BERADREF(INDX)       = ALL '+' OR SPACE                    
093700           CONTINUE                                                       
093800        ELSE                                                              
093900           MOVE MFS-ROER-EJ-FAELT     TO MOD-BERADREF(INDX)               
094000           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BERADREF-ATTR(INDX)          
094100        END-IF                                                            
094200                                                                          
094300        ADD +1                        TO INDX                             
094400     END-PERFORM                                                          
094500     .                                                                    
094600     EJECT                                                                
094700 GB-KOLLA-CMD    SECTION.                                                 
094800                                                                          
094810     MOVE ZERO                       TO WS-IDDISTR-SPAR                   
094820                                        WS-IDKUNDNR-SPAR                  
094830     MOVE SPACE                      TO WS-CMD-SPAR                       
094900     MOVE +1                         TO INDX                              
095000     PERFORM UNTIL  INDX             > MAX-INDX                           
095001                                                                          
095010        INSPECT MID-IDDISTR(INDX) REPLACING LEADING SPACE BY ZERO         
095020        INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO        
095030                                                                          
095100        IF MID-CMD(INDX)             = ALL '+' OR SPACE                   
095200           CONTINUE                                                       
095300        ELSE                                                              
095400           MOVE MID-CMD(INDX)       TO GODK-CMD-KOD-SW                    
095500           IF GODK-CMD-KOD                                                
095600              IF WS-CMD-SPAR        = SPACE                               
095700                 MOVE MID-CMD(INDX) TO WS-CMD-SPAR                        
095710                 IF MID-IDDISTR(INDX) = ZERO                              
095720                   MOVE ERR-EMPTY-ROW-INDICATED TO MED-IDMFSFEL           
095721                   CALL WMEDKONV USING MED-WMEDAREA                       
095722                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
095730                   MOVE NEJ TO INDATA-SW                                  
095740                   MOVE MFS-ALFA-FAELT-FEL                                
095750                                       TO MOD-CMD-ATTR(INDX)              
095760                 END-IF                                                   
095800              ELSE                                                        
095900                 IF MID-CMD(INDX)   NOT = WS-CMD-SPAR                     
096000                    MOVE '060'      TO  MED-IDMFSFEL                      
096100                    CALL WMEDKONV USING MED-WMEDAREA                      
096200                    MOVE MED-MFSFEL TO MOD-TEMFSFEL                       
096300                    MOVE NEJ        TO INDATA-SW                          
096400                    MOVE MFS-ALFA-FAELT-FEL                               
096500                                    TO MOD-CMD-ATTR(INDX)                 
096600                 END-IF                                                   
096700                 IF (MID-IDDISTR(INDX)   NOT = WS-IDDISTR-SPAR OR         
096800                  MID-IDKUNDNR (INDX) NOT = WS-IDKUNDNR-SPAR) AND         
096810                    WS-IDDISTR-SPAR     > ZERO                            
096900                    MOVE '061'          TO  MED-IDMFSFEL                  
097000                    CALL WMEDKONV USING MED-WMEDAREA                      
097100                    MOVE MED-MFSFEL     TO MOD-TEMFSFEL                   
097200                    MOVE NEJ            TO INDATA-SW                      
097300                    MOVE MFS-ALFA-FAELT-FEL                               
097400                                        TO MOD-CMD-ATTR(INDX)             
097410                 ELSE                                                     
097411                    IF MID-IDDISTR(INDX) > ZERO                           
097420                      MOVE MID-IDDISTR(INDX) TO WS-IDDISTR-SPAR           
097430                      MOVE MID-IDKUNDNR (INDX) TO WS-IDKUNDNR-SPAR        
097440                    ELSE                                                  
097441                      MOVE ERR-EMPTY-ROW-INDICATED TO MED-IDMFSFEL        
097442                      CALL WMEDKONV USING MED-WMEDAREA                    
097443                      MOVE MED-MFSFEL TO MOD-TEMFSFEL                     
097460                      MOVE NEJ         TO INDATA-SW                       
097470                      MOVE MFS-ALFA-FAELT-FEL                             
097480                                        TO MOD-CMD-ATTR(INDX)             
097490                    END-IF                                                
097500                 END-IF                                                   
097600              END-IF                                                      
097601           ELSE                                                           
097610              MOVE '001'                  TO  MED-IDMFSFEL                
097620              CALL WMEDKONV USING MED-WMEDAREA                            
097630              MOVE MED-MFSFEL             TO MOD-TEMFSFEL                 
097640              MOVE NEJ                    TO INDATA-SW                    
097650              MOVE MFS-ALFA-FAELT-FEL                                     
097660                                          TO MOD-CMD-ATTR(INDX)           
097700           END-IF                                                         
097800        END-IF                                                            
097900        ADD +1              TO INDX                                       
098000     END-PERFORM                                                          
098010                                                                          
098030                                                                          
098100     .                                                                    
098200     EJECT                                                                
098300 H-UPPDATERA SECTION.                                                     
098400                                                                          
098410     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSINF                           
098420                                   MOD-TEMFSFEL                           
098500     MOVE JA                    TO FIRST-TIME-SW                          
098600     MOVE +1                    TO INDX                                   
098700     PERFORM UNTIL INDX         > MAX-INDX                                
098800                                                                          
098900       IF MID-MAERKE(INDX)       NOT = ALL '+' OR                         
099000          MID-BERADREF(INDX)     NOT = ALL '+' OR                         
099100          MID-CMD (INDX)         NOT = ALL '+'                            
099110           MOVE JA TO VOR-KOE-SW                                          
099200           PERFORM HA-LAES-VOR-KOE                                        
099300       END-IF                                                             
099400       IF VOR-KOE-FINNS                                                   
099500         IF MID-CMD(INDX)       NOT = ALL '+'                             
099600            IF MID-CMD(INDX)    = 'D'                                     
099700               PERFORM HB-TA-BORT-OCH-RAKNA-NER-OKS                       
099800            ELSE                                                          
099810               IF MID-CMD(INDX) = 'F' OR 'N'                              
099900                  MOVE JA       TO STARTA-4221-SW                         
100000                  PERFORM HC-UPPDATERA-4541                               
100100                  IF FIRST-TIME                                           
100200                     PERFORM HD-SKAPA-4221-MID                            
100300                     MOVE NEJ   TO FIRST-TIME-SW                          
100400                  END-IF                                                  
100401               ELSE                                                       
100402                  IF MID-CMD(INDX) = 'S'                                  
100403                     MOVE JA       TO STARTA-4231-SW                      
100404                     PERFORM HC-UPPDATERA-4541                            
100405                     IF FIRST-TIME                                        
100406                        PERFORM HF-SKAPA-4231-MID                         
100407                        MOVE NEJ   TO FIRST-TIME-SW                       
100408                     END-IF                                               
100409                  END-IF                                                  
100410               END-IF                                                     
100500            END-IF                                                        
100600         ELSE                                                             
100700            IF MID-MAERKE(INDX)     NOT = ALL '+' OR                      
100800               MID-BERADREF(INDX)   NOT = ALL '+'                         
100900                PERFORM HE-UPPDAT-MAERKE-RADREF                           
101000            END-IF                                                        
101100         END-IF                                                           
101200       END-IF                                                             
101300       ADD +1                   TO INDX                                   
101400     END-PERFORM                                                          
101500                                                                          
101600     IF STARTA-4221                                                       
101700       PERFORM IMS-INSERT-4221-MSG                                        
101800     END-IF                                                               
101900                                                                          
101910     IF STARTA-4231                                                       
101920       PERFORM IMS-INSERT-4231-MSG                                        
101930     END-IF                                                               
101940                                                                          
102500     MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                             
102600     CALL WMEDKONV USING MED-WMEDAREA                                     
102700     MOVE MED-MFSINF          TO MOD-TEMFSINF                             
102900     .                                                                    
103000     EJECT                                                                
103100                                                                          
103200 HA-LAES-VOR-KOE  SECTION.                                                
103300                                                                          
103310     INSPECT MID-IDANSK(INDX)  REPLACING LEADING SPACE BY ZERO            
103320     INSPECT MID-IDARTNR(INDX) REPLACING LEADING SPACE BY ZERO            
103330                                                                          
103400     MOVE MID-IDDISTR(INDX)   TO W-IDDISTR-GU                             
103500     MOVE MID-IDANSK(INDX)    TO W-IDANSK-GU                              
103600     MOVE MID-IDARTNR(INDX)   TO W-IDARTNR-GU                             
103700     MOVE MID-IDLOPNR(INDX)   TO W-IDLOPNR-GU                             
103800     MOVE MID-IDORDER(INDX)   TO W-IDORDER-GU                             
103900     PERFORM IMS-GHU-4542                                                 
103901     IF SEGMENT-SAKNAS                                                    
103902       MOVE NEJ TO VOR-KOE-SW                                             
103903     ELSE                                                                 
103910       MOVE MID-IDDISTR-ENTER TO W-IDDISTR-MIN                            
103920                                 W-IDDISTR-MAX                            
103930       MOVE MID-IDANSK-ENTER  TO W-IDANSK-MIN                             
103940       MOVE MID-IDARTNR-ENTER TO W-IDARTNR-MIN                            
103950       MOVE MID-IDLOPNR-ENTER TO W-IDLOPNR-MIN                            
103960       MOVE MID-IDORDER-ENTER TO W-IDORDER-MIN                            
103970     END-IF                                                               
104000     .                                                                    
104100     EJECT                                                                
104200                                                                          
104300 HB-TA-BORT-OCH-RAKNA-NER-OKS SECTION.                                    
104400                                                                          
104500     PERFORM IMS-DLET-4541                                                
104600                                                                          
104700     MOVE MFS-RENSA-FAELT      TO MOD-CMD(INDX)                           
104800     MOVE MFS-FORMATETS-ATTR   TO MOD-CMD-ATTR(INDX)                      
104900                                                                          
105000*--- MINSKAR ORDERKÖSALDOT PÅ WDK901                                      
105100                                                                          
105200     IF 4542-IDLEVNR            = SPACE                                   
105201       MOVE 4542-IDDC           TO WS-IDDC                                
105202       MOVE MID-IDARTNR(INDX)   TO W-IDARTNR                              
105203       COMPUTE SPAR-KVBEART     = 4542-KVBEART-Q - 4542-KVPREAVB          
105204                                                                          
105210       IF CDC                                                             
105220         PERFORM IMS-GHU-ARTM-WDK901                                      
105230         COMPUTE ART-KVOKS-VOR  =  ART-KVOKS-VOR - SPAR-KVBEART           
105240         PERFORM IMS-REPL-ARTM-WDK901                                     
105810       ELSE                                                               
105811         IF 4542-KDORDBEK = 92 OR 93                                      
105812           MOVE 4542-IDDC       TO W-IDDC                                 
105813           PERFORM IMS-GHU-ARTS-WDK711                                    
105814           COMPUTE SLAG-KVOKS-DAG = SLAG-KVOKS-DAG - SPAR-KVBEART         
105815           PERFORM IMS-REPL-ARTS-WDK711                                   
105816         END-IF                                                           
105880       END-IF                                                             
105890       MOVE ZERO                TO SPAR-KVBEART                           
105900     END-IF                                                               
105910                                                                          
105920*--- SKRIVER LOGGPOST PÅ WDR601                                           
105930                                                                          
105940     MOVE IDPGM            TO FIL-IDPGM                                   
105950     ACCEPT FIL-TIREGDAT   FROM DATE                                      
105960     ACCEPT FIL-TIKLOCK    FROM TIME                                      
105970     MOVE ZERO             TO FIL-IDSEKVNR                                
105980     MOVE 'W414'           TO FIL-CT-IDSYSTEM                             
105990     MOVE 'A'              TO FIL-CT-IDVTYP                               
105991     MOVE '205'            TO FIL-CT-IDPTYP                               
105992     ADD +1                TO FIL-IDSEKVNR                                
105993     MOVE 4542-IDARTNR     TO 205-IDARTNR                                 
105994     MOVE 4542-IDDC        TO 205-IDDC                                    
105995     MOVE 4542-IDDISTR     TO 205-IDDISTR                                 
105996     MOVE 4542-IDKUNDNR    TO 205-IDKUNDNR                                
105997     MOVE 4542-IDKUNDRF    TO 205-IDKUNDRF                                
105998     MOVE 4542-KVPREAVB    TO 205-KVAVBART                                
106002     MOVE 4542-KVBEART-Q   TO 205-KVBEART-Q                               
106003     IF 4542-IDLEVNR > SPACE                                              
106004       MOVE JA             TO 205-FLDIRLEV                                
106005     ELSE                                                                 
106007       MOVE NEJ            TO 205-FLDIRLEV                                
106008     END-IF                                                               
106009     MOVE 4542-TIREGDAT    TO 205-TIREGDAT                                
106010     MOVE 4542-TIREGTID    TO 205-TIREGTID                                
106011                                                                          
106012     PERFORM IMS-ISRT-FILA-WDR601                                         
106013     IF SEGMENT-FINNS-REDAN                                               
106014        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
106015           ADD +1 TO FIL-IDSEKVNR                                         
106016           PERFORM IMS-ISRT-FILA-WDR601                                   
106017        END-PERFORM                                                       
106018     END-IF                                                               
106020     .                                                                    
106100     EJECT                                                                
106200 HC-UPPDATERA-4541  SECTION.                                              
106300                                                                          
106400     MOVE '1'                  TO 4542-KDVORATG                           
106500     MOVE '**'                 TO 4542-TEVORMRK                           
106600     MOVE MSG-SIGNON-USERID    TO 4542-IDUSER                             
106900     MOVE MSGI-TILOKDAT        TO 4542-TIUPPDAT                           
107000     MOVE MSGI-TILOKTID        TO 4542-TIUPPTID                           
107100                                                                          
107200     PERFORM IMS-REPL-4541                                                
107300     .                                                                    
107400     EJECT                                                                
107500                                                                          
107600 HD-SKAPA-4221-MID  SECTION.                                              
107700                                                                          
107800     MOVE MID-IDDISTR(INDX)     TO 4221-IDDISTR                           
107900     INSPECT 4221-IDDISTR REPLACING LEADING SPACE BY ZERO                 
107910                                                                          
108000     MOVE MID-IDKUNDNR(INDX)    TO 4221-IDKUNDNR                          
108100     INSPECT 4221-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
108200                                                                          
108300     MOVE MFS-KDMFSFOR          TO 4221-SPRAK                             
108301     MOVE YES                   TO 4221-FLVORKO                           
108310     IF MID-CMD(INDX)           =  'F'                                    
108330         MOVE JA                TO 4221-FLFORBI                           
108340     ELSE                                                                 
108360         MOVE NEJ               TO 4221-FLFORBI                           
108370     END-IF                                                               
108400     .                                                                    
108500     EJECT                                                                
108600                                                                          
108610 HF-SKAPA-4231-MID  SECTION.                                              
108620                                                                          
108630     MOVE MID-IDDISTR(INDX)     TO 4231-IDDISTR                           
108640     INSPECT 4231-IDDISTR REPLACING LEADING SPACE BY ZERO                 
108650                                                                          
108660     MOVE MID-IDKUNDNR(INDX)    TO 4231-IDKUNDNR                          
108670     INSPECT 4231-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
108680                                                                          
108690     MOVE MFS-KDMFSFOR          TO 4231-SPRAK                             
108691     MOVE YES                   TO 4231-FLVORKO                           
108697     .                                                                    
108698     EJECT                                                                
108699                                                                          
108700 HE-UPPDAT-MAERKE-RADREF SECTION.                                         
108800                                                                          
108900     IF MID-MAERKE(INDX)          NOT = ALL '+'                           
109000       MOVE MID-MAERKE(INDX)      TO 4542-TEVORMRK                        
109100       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVORMRK-ATTR(INDX)              
109200     END-IF                                                               
109300                                                                          
109400     IF MID-BERADREF(INDX)        NOT = ALL '+'                           
109500       MOVE MID-BERADREF(INDX)    TO 4542-BERADREF                        
109600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BERADREF-ATTR(INDX)              
109700     END-IF                                                               
109800                                                                          
109900     PERFORM IMS-REPL-4541                                                
110000     .                                                                    
110100     EJECT                                                                
114000                                                                          
114100 MFS-RENSA-FAELT-PA-RAD  SECTION.                                         
114200                                                                          
114300     MOVE MFS-RENSA-FAELT   TO MOD-TIREGDAT(INDX)                         
114400                               MOD-CMD(INDX)                              
114500                               MOD-TEVORMRK(INDX)                         
114600                               MOD-IDDISTR(INDX)                          
114700                               MOD-IDKUNDNR(INDX)                         
114800                               MOD-IDORDNR7(INDX)                         
114900                               MOD-IDANSK(INDX)                           
115000                               MOD-IDARTNR(INDX)                          
115100                               MOD-KVBEART(INDX)                          
115200                               MOD-DIFF(INDX)                             
115300                               MOD-KDORDBEK(INDX)                         
115400                               MOD-BERADREF(INDX)                         
115500                               MOD-IDDC(INDX)                             
115600     .                                                                    
115700                                                                          
115800 MFS-RENSA-FAELT-UT SECTION.                                              
115900                                                                          
116000     MOVE +1 TO INDX                                                      
116100     PERFORM UNTIL INDX > MAX-INDX                                        
116200       MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT(INDX)                         
116300                               MOD-CMD(INDX)                              
116400                               MOD-TEVORMRK(INDX)                         
116500                               MOD-IDDISTR(INDX)                          
116600                               MOD-IDKUNDNR(INDX)                         
116700                               MOD-IDORDNR7(INDX)                         
116800                               MOD-IDANSK(INDX)                           
116900                               MOD-IDARTNR(INDX)                          
117000                               MOD-KVBEART(INDX)                          
117100                               MOD-DIFF(INDX)                             
117200                               MOD-KDORDBEK(INDX)                         
117300                               MOD-BERADREF(INDX)                         
117400                               MOD-IDDC(INDX)                             
117500       ADD +1 TO INDX                                                     
117600     END-PERFORM                                                          
117700     .                                                                    
117800     EJECT                                                                
117900                                                                          
123600 MFS-RENSA-FAELT-IN SECTION.                                              
123700                                                                          
123800     MOVE +1 TO INDX                                                      
123900     PERFORM UNTIL INDX > MAX-INDX                                        
124000       MOVE MFS-RENSA-FAELT TO MOD-CMD(INDX)                              
124100                               MOD-TEVORMRK(INDX)                         
124200                               MOD-BERADREF(INDX)                         
124300       ADD +1 TO INDX                                                     
124400     END-PERFORM                                                          
124500                                                                          
124600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-1-UT                             
124700                             MOD-IDDISTR-2-UT                             
124800                             MOD-IDDISTR-3-UT                             
124900                             MOD-IDDISTR-4-UT                             
125200                             MOD-IDARTNR-UT                               
125300     .                                                                    
125400     EJECT                                                                
125500 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
125600                                                                          
125610     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDISTR-ENTER                         
125620                                MOD-IDANSK-ENTER                          
125630                                MOD-IDARTNR-ENTER                         
125640                                MOD-IDLOPNR-ENTER                         
125650                                MOD-IDORDER-ENTER                         
125660                                MOD-IDDISTR-NEXT                          
125670                                MOD-IDANSK-NEXT                           
125680                                MOD-IDARTNR-NEXT                          
125690                                MOD-IDLOPNR-NEXT                          
125691                                MOD-IDORDER-NEXT                          
125692                                                                          
125700     MOVE +1 TO INDX                                                      
125800     PERFORM UNTIL INDX > MAX-INDX                                        
125900       PERFORM MFS-ROR-EJ-RAD-FAELT-UT                                    
126000       ADD +1 TO INDX                                                     
126100     END-PERFORM                                                          
126200     .                                                                    
126300     SKIP2                                                                
126400 MFS-ROR-EJ-RAD-FAELT-UT  SECTION.                                        
126500                                                                          
126600     MOVE MFS-ROER-EJ-FAELT TO MOD-TIREGDAT(INDX)                         
126700                               MOD-IDDISTR(INDX)                          
126800                               MOD-IDKUNDNR(INDX)                         
126900                               MOD-IDORDNR7(INDX)                         
127000                               MOD-IDANSK(INDX)                           
127100                               MOD-IDARTNR(INDX)                          
127200                               MOD-KVBEART(INDX)                          
127300                               MOD-DIFF(INDX)                             
127400                               MOD-KDORDBEK(INDX)                         
127410                               MOD-IDDC(INDX)                             
127500                               MOD-IDLOPNR(INDX)                          
127510                               MOD-IDORDER(INDX)                          
127600                                                                          
127700     IF MID-CMD(INDX) NOT = ALL '+'                                       
127800       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD(INDX)                            
127900     END-IF                                                               
128000                                                                          
128100     IF MID-MAERKE(INDX) NOT = ALL '+'                                    
128200       MOVE MFS-ROER-EJ-FAELT TO MOD-TEVORMRK(INDX)                       
128300     END-IF                                                               
128400                                                                          
128500     IF MID-BERADREF(INDX) NOT = ALL '+'                                  
128600       MOVE MFS-ROER-EJ-FAELT TO MOD-BERADREF(INDX)                       
128700     END-IF                                                               
128800     .                                                                    
128900     EJECT                                                                
128901                                                                          
128910 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
128920                                                                          
128921     MOVE +1                   TO INDX                                    
128922     PERFORM UNTIL INDX        > MAX-INDX                                 
128930        MOVE MFS-ROER-EJ-FAELT TO MOD-CMD(INDX)                           
128940                                  MOD-TEVORMRK(INDX)                      
128950                                  MOD-BERADREF(INDX)                      
128951        ADD +1                 TO INDX                                    
128960     END-PERFORM                                                          
129007     .                                                                    
129008     EJECT                                                                
129010                                                                          
129100* --- IMS SEKTIONER ---                                                   
129200                                                                          
129300 IMS-GET-MSG SECTION.                                                     
129400                                                                          
129500     MOVE '  QC' TO GODK-STATUSKODER                                      
129600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
129700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
129800     PERFORM IMS-STATUSKONTROLL                                           
129900     .                                                                    
130000     SKIP3                                                                
130100 IMS-INSERT-MSG SECTION.                                                  
130200                                                                          
130300     IF ENGLISH-TEXT                                                      
130400       MOVE 'N' TO MFS-KDHUVOMR                                           
130500     END-IF                                                               
130600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
130700     MOVE SPACE TO GODK-STATUSKODER                                       
130800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
130900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     .                                                                    
131200     SKIP3                                                                
131300 IMS-INSERT-4221-MSG SECTION.                                             
131400                                                                          
131500     IF ENGLISH-TEXT                                                      
131600       MOVE 'N' TO MFS-KDHUVOMR                                           
131700     END-IF                                                               
131800     MOVE LOW-VALUE TO 4221-Z1 4221-Z2                                    
131900     MOVE SPACE TO GODK-STATUSKODER                                       
132000     CALL CBLTDLI USING ISRT 4221-PCB 4221-MSG-IO-AREA                    
132100     MOVE 4221-STATUS-CODE TO STATUS-WS                                   
132200     PERFORM IMS-STATUSKONTROLL                                           
132300     .                                                                    
132400     EJECT                                                                
132410 IMS-INSERT-4231-MSG SECTION.                                             
132420                                                                          
132430     IF ENGLISH-TEXT                                                      
132440       MOVE 'N' TO MFS-KDHUVOMR                                           
132450     END-IF                                                               
132460     MOVE LOW-VALUE TO 4231-Z1 4231-Z2                                    
132470     MOVE SPACE TO GODK-STATUSKODER                                       
132480     CALL CBLTDLI USING ISRT 4231-PCB 4231-MSG-IO-AREA                    
132490     MOVE 4231-STATUS-CODE TO STATUS-WS                                   
132491     PERFORM IMS-STATUSKONTROLL                                           
132492     .                                                                    
132493     EJECT                                                                
132500 IMS-GU-4541 SECTION.                                                     
132600                                                                          
132700     STRING 'WL454101(WDGXKEY  =' W-IDHTYP-X ')'                          
132800          DELIMITED BY SIZE INTO SSA1                                     
132900     MOVE '    ' TO GODK-STATUSKODER                                      
133000     CALL CBLTDLI USING GU  4541-PCB DLI-IO-AREA SSA1                     
133100     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
133200     PERFORM IMS-STATUSKONTROLL                                           
133300     .                                                                    
133400                                                                          
133500                                                                          
133600 IMS-GHU-4542 SECTION.                                                    
133700                                                                          
133800     STRING 'WL454101(WDGXKEY  =' W-IDHTYP-X ')'                          
133900          DELIMITED BY SIZE INTO SSA1                                     
134210     STRING 'WL454111(KY4542   =' W-WDGXKEY-GU-X                          
134220                    '&KDVORATG <' W-KDVORATG-X ')'                        
134230          DELIMITED BY SIZE INTO SSA2                                     
134300     MOVE '  GE' TO GODK-STATUSKODER                                      
134400     CALL CBLTDLI USING GHU 4541-PCB DLI-IO-AREA SSA1 SSA2                
134500     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
134600     PERFORM IMS-STATUSKONTROLL                                           
134700     .                                                                    
134800                                                                          
134900                                                                          
135000 IMS-GNP-DISTR SECTION.                                                   
135100                                                                          
135200     STRING 'WL454111(KY4542  =>' W-WDGXKEY-MIN-X                         
135300                    '&KY4542  =<' W-WDGXKEY-MAX-X                         
135400                    '&KDVORATG <' W-KDVORATG-X ')'                        
135500          DELIMITED BY SIZE INTO SSA1                                     
135600     MOVE '  GE' TO GODK-STATUSKODER                                      
135700     CALL CBLTDLI USING GNP 4541-PCB DLI-IO-AREA SSA1                     
135800     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
135900     PERFORM IMS-STATUSKONTROLL                                           
136000     .                                                                    
136100     EJECT                                                                
137600                                                                          
137700 IMS-REPL-4541 SECTION.                                                   
137800                                                                          
137900                                                                          
138000     MOVE '  ' TO GODK-STATUSKODER                                        
138100     CALL CBLTDLI USING REPL 4541-PCB DLI-IO-AREA                         
138200     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
138300     PERFORM IMS-STATUSKONTROLL                                           
138400     .                                                                    
138500                                                                          
138600                                                                          
138700 IMS-DLET-4541 SECTION.                                                   
138800                                                                          
138900     MOVE '  ' TO GODK-STATUSKODER                                        
139000     CALL CBLTDLI USING DLET 4541-PCB DLI-IO-AREA                         
139100     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400     EJECT                                                                
139500 IMS-GHU-ARTM-WDK901 SECTION.                                             
139600                                                                          
139700     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
139800          DELIMITED BY SIZE INTO SSA1                                     
139900     MOVE '  '   TO GODK-STATUSKODER                                      
140000     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA2 SSA1                    
140100     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
140200     PERFORM IMS-STATUSKONTROLL                                           
140300     .                                                                    
140400     SKIP3                                                                
140600 IMS-REPL-ARTM-WDK901 SECTION.                                            
140700                                                                          
140800     MOVE '  ' TO GODK-STATUSKODER                                        
140900     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA2                        
141000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
141100     PERFORM IMS-STATUSKONTROLL                                           
141200     .                                                                    
141300     EJECT                                                                
141301 IMS-GU-ARTC-WDK611 SECTION.                                              
141302                                                                          
141303     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
141304          DELIMITED BY SIZE  INTO SSA1                                    
141305     MOVE 'WDK611  ' TO SSA2                                              
141307     MOVE '  GE'               TO GODK-STATUSKODER                        
141308     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
141309     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
141310     PERFORM IMS-STATUSKONTROLL                                           
141311     .                                                                    
141312     SKIP3                                                                
141313 IMS-GU-ARTS-WDK711 SECTION.                                              
141314                                                                          
141315     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
141316          DELIMITED BY SIZE  INTO SSA1                                    
141317     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
141318          DELIMITED BY SIZE  INTO SSA2                                    
141319     MOVE '  GE'               TO GODK-STATUSKODER                        
141320     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-ARTS SSA1 SSA2            
141321     MOVE ARTS-STATUS-CODE     TO STATUS-WS                               
141322     PERFORM IMS-STATUSKONTROLL                                           
141323     .                                                                    
141324     SKIP3                                                                
141325 IMS-GHU-ARTS-WDK711 SECTION.                                             
141326                                                                          
141327     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
141328          DELIMITED BY SIZE  INTO SSA1                                    
141329     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
141330          DELIMITED BY SIZE  INTO SSA2                                    
141331     MOVE '    '               TO GODK-STATUSKODER                        
141332     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA-ARTS SSA1 SSA2           
141333     MOVE ARTS-STATUS-CODE     TO STATUS-WS                               
141334     PERFORM IMS-STATUSKONTROLL                                           
141335     .                                                                    
141336     SKIP3                                                                
141337 IMS-REPL-ARTS-WDK711 SECTION.                                            
141338                                                                          
141339     MOVE '    '               TO GODK-STATUSKODER                        
141340     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA-ARTS                    
141341     MOVE ARTS-STATUS-CODE     TO STATUS-WS                               
141342     PERFORM IMS-STATUSKONTROLL                                           
141343     .                                                                    
141344     EJECT                                                                
141345                                                                          
141346 IMS-ISRT-FILA-WDR601 SECTION.                                            
141347                                                                          
141348     MOVE 'WLFILA01' TO SSA1                                              
141349     MOVE '  II' TO GODK-STATUSKODER                                      
141350     CALL CBLTDLI USING ISRT FILA-PCB DLI-IO-AREA3 SSA1                   
141360     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
141370     PERFORM IMS-STATUSKONTROLL                                           
141380     .                                                                    
141390     SKIP3                                                                
141400 IMS-STATUSKONTROLL SECTION.                                              
141500                                                                          
141600     SET STATUS-IX TO 1                                                   
141700     SEARCH GODK-STATUS                                                   
141800       AT END                                                             
141900         CALL FELLOG                                                      
142000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
142100         CONTINUE                                                         
142200     END-SEARCH                                                           
142300     .                                                                    
