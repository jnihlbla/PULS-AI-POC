000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4035600.                                                
000400 AUTHOR.         PER BERGH.                                               
000500 DATE-WRITTEN.   90/09/28.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        UPPDATERING AV SPECIALEMBALLAGEKOD.                              
001100*                                                                         
001200*        PROGRAMMET VISAR (PER C-LAGER)                                   
001300*                         ANTINGEN                                        
001400*        ALLA RADER PÅ EN ORDER MED STATUS R                              
001500*                         ELLER                                           
001600*        RADER MED VISS EMBALLAGEKOD (KDSPEEMB), OM                       
001700*        DETTA ANGES PÅ BILDEN (EKOD).                                    
001800*                                                                         
001900*        NYCKEL ÄR ANTINGEN DISTR/KUND/ORDER ELLER PRODNR.                
002000*        URVAL KAN GÖRAS ÄVEN PÅ ARTNR ELLER LAGEROMRÅDE.                 
002100*                                                                         
002200*        UPPDATERING AV IDSPECEMB SKER PÅ RESP. RAD.                      
002300*        FLERA RADER KAN UPPDATERAS SAMTIDIGT.                            
002400*                                                                         
002500*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002600*                                                                         
002700*        PROGRAMMET LÄSER      WLORQA (WDQ3)                              
002800*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
002900*        PROGRAMMET UPPDATERAR WLORQI (WDQ212;KVSEMBRA)                   
003000*        PROGRAMMET LÄSER      WLORQF (WDQ4)                              
003100*        PROGRAMMET UPPDATERAR WLORQF (WDQ4;KDSPECEMB)                    
003200*        PROGRAMMET LÄSER      WLBENA (WDD3;BEART)                        
003400*                                                                         
003500*                                                                         
003600*    INDATA.                                                              
003700*        TRANSAKTION: W4T356                                              
003800*        MID:         W4I35601                                            
003900*                                                                         
004000*    UTDATA.                                                              
004100*        MOD:         W4O35601                                            
004200     SKIP3                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600 WORKING-STORAGE SECTION.                                                 
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'W4035600'.            
004800                                                                          
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100*                                                                         
005200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005400 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
005500*                                                                         
005600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1454 COMP SYNC.        
005800*                                                                         
005900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006000 SKIP2                                                                    
006100 77  WS-IDDISTR                  PIC X(04)   VALUE SPACE.                 
006200 77  WS-IDKUNDNR                 PIC X(06)   VALUE SPACE.                 
006300*                                                                         
006400 01  WS-IDKUNDRF.                                                         
006500     03  WS-IDORDNR7             PIC X(07)   VALUE SPACE.                 
006600     03  FILLER                  PIC X(03)   VALUE SPACE.                 
006601*                                                                         
006610 01  WS-IDKUNDRF-RED.                                                     
006620     03 WS-IDKUNDRF-1-7          PIC X(7)    VALUE SPACE.                 
006630     03 WS-IDKUNDRF-8-10         PIC X(3)    VALUE SPACE.                 
006700*                                                                         
006800 77  WS-ADLAGOMR                 PIC X(02)   VALUE SPACE.                 
006900 77  WS-IDPRODNR                 PIC X(07)   VALUE SPACE.                 
007000 77  WS-FLEKOD                   PIC X(01)   VALUE SPACE.                 
007100 77  WS-IDARTNR                  PIC X(09)   VALUE SPACE.                 
007101*                                                                         
007102 01  WS1-IDDC                     PIC X(2).                               
007200     EJECT                                                                
007300*    --- SWITCHAR                                                         
007400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007500     88  INDATA-OK                           VALUE 'J'.                   
007600     88  INDATA-FEL                          VALUE 'N'.                   
007700                                                                          
007800 77  INPUT-SW                    PIC X       VALUE 'N'.                   
007900     88  INPUT-SAKNAS                        VALUE 'N'.                   
008000                                                                          
008100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008200     88  NYCKLAR-OK                          VALUE 'J'.                   
008300     88  NYCKLAR-FEL                         VALUE 'N'.                   
008400                                                                          
008500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008600     88  ALLT-OK                             VALUE 'J'.                   
008700                                                                          
008800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008900     88  EGEN-MID                            VALUE '4356'.                
009000     88  GODK-MID                            VALUE '4356'                 
009100                                                   '4357'.                
009200 77  FLER-SIDOR-SW               PIC X       VALUE 'N'.                   
009300     88  FLER-SIDOR                          VALUE 'J'.                   
009400     88  EJ-FLER-SIDOR                       VALUE 'N'.                   
009500                                                                          
009600 77  LAES-SW                     PIC X       VALUE '0'.                   
009700     88  IDGMTREF-LAESNING                   VALUE '1'.                   
009800     88  IDPRODNR-LAESNING                   VALUE '2'.                   
009900     EJECT                                                                
009910*      --- VALID IDDC CODES                                               
009920*                                                                         
009930*01    -COPY WWDC99                                                       
009940       EJECT                                                              
010000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010100 01  GENERELLA-SUBPROGRAM.                                                
010200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010410     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010700*   -COPY WMSGINIT                                                        
010710     EJECT                                                                
010720*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010730*   -COPY WMEDAREA                                                        
010800     SKIP3                                                                
010900 01  MESSAGE-CODES.                                                       
011000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011600     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
011700     03  ERR-LAST-PAGE-SHOWED    PIC X(3)    VALUE '115'.                 
011800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011900     03  ERR-INF-MISS            PIC X(3)    VALUE '413'.                 
012000     03  ERR-NO-CHANGE           PIC X(3)    VALUE '414'.                 
012100     EJECT                                                                
012200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012300*                                                                         
012400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012500     SKIP3                                                                
012600*01  MID -COPY W4I35601                                                   
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012900     SKIP3                                                                
013000*01  -COPY WMSGAREA                                                       
013100     EJECT                                                                
013200     03  MOD REDEFINES MSG-AREA.                                          
013300*      05  -COPY W4O35601                                                 
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013600     SKIP3                                                                
013700*01  -COPY WMFSAREA                                                       
013800     EJECT                                                                
013900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014200     SKIP3                                                                
014300 01  NYCKLAR-TILL-DLI.                                                    
014400*                                                                         
014500*    --- NYCKLAR FÖR SÖKNING  WDQ2CSEQ                                    
014600     03  W-IDGMTREF-X.                                                    
014700         05  W-IDDISTR           PIC S9(05)   VALUE ZERO COMP-3.          
014800         05  W-IDKUNDNR          PIC S9(07)   VALUE ZERO COMP-3.          
014900         05  W-IDKUNDRF.                                                  
015000             07  W-IDORDNR7      PIC  9(07)   VALUE ZERO.                 
015100             07  FILLER          PIC  X(03)   VALUE SPACE.                
015200*                                                                         
015300*    --- DIREKTNYCKEL UPPDATERING KVSEMBRA WDQ212                         
015400     03  W-IDORDER-SOEK-X.                                                
015500        05  W-IDORDER-SOEK       PIC S9(07)   VALUE ZERO COMP-3.          
015600*                                                                         
015700*    --- MIN- OCH MAX-NYCKLAR IDPLKLST WDQ3DSEQ                           
015800     03  W-WDQ3DSEQ-MIN-X.                                                
015900         05  W-IDPRODNR-MIN      PIC S9(07)   VALUE ZERO COMP-3.          
016000         05  W-IDPLKLST-MIN      PIC S9(03)   VALUE ZERO COMP-3.          
016100*                                                                         
016200     03  W-WDQ3DSEQ-MAX-X.                                                
016300         05  W-IDPRODNR-MAX      PIC S9(07)   VALUE ZERO COMP-3.          
016400         05  W-IDPLKLST-MAX      PIC S9(03)   VALUE ZERO COMP-3.          
016500*                                                                         
016600*    --- NYCKLAR FÖR SÖKNING  WDQ4ASEQ                                    
016700     03  W-WDQ4ASEQ-X.                                                    
016800         05  W-4A1-IDORDER       PIC S9(07)   VALUE ZERO COMP-3.          
016900         05  W-4A1-IDARTNR       PIC S9(09)   VALUE ZERO COMP-3.          
017000         05  W-4A1-IDLOPNR       PIC S9(03)   VALUE ZERO COMP-3.          
017100*                                                                         
017200*    --- MIN- OCH MAX-NYCKLAR IDLOPNR  WDQ4ASEQ                           
017300     03  W-WDQ4ASEQ-MIN-X.                                                
017400         05  W-IDORDER-MIN       PIC S9(07)   VALUE ZERO COMP-3.          
017500         05  W-IDARTNR-MIN       PIC S9(09)   VALUE ZERO COMP-3.          
017600         05  W-IDLOPNR-MIN       PIC S9(03)   VALUE ZERO COMP-3.          
017700*                                                                         
017800     03  W-WDQ4ASEQ-MAX-X.                                                
017900         05  W-IDORDER-MAX       PIC S9(07)   VALUE ZERO COMP-3.          
018000         05  W-IDARTNR-MAX       PIC S9(09)   VALUE ZERO COMP-3.          
018100         05  W-IDLOPNR-MAX       PIC S9(03)   VALUE ZERO COMP-3.          
018200*                                                                         
018300*    --- NYCKLAR FÖR UPPDATERING KDSPECEMP  WDQ401                        
018400     03  W-WDQ401KY-X.                                                    
018500         05  W-401-IDORDER       PIC S9(07)   VALUE ZERO COMP-3.          
018600         05  W-401-IDDC          PIC  X(2)    VALUE '00'.                 
018700         05  W-401-ADLAGOMR      PIC S9(03)   VALUE ZERO COMP-3.          
018800         05  W-401-ADGANG        PIC S9(03)   VALUE ZERO COMP-3.          
018900         05  W-401-ADPLATS       PIC S9(05)   VALUE ZERO COMP-3.          
019000         05  W-401-IDARTNR       PIC S9(09)   VALUE ZERO COMP-3.          
019100         05  W-401-IDLOPNR       PIC S9(03)   VALUE ZERO COMP-3.          
019200*                                                                         
019300     03  W-IDORDER-SPAR-X.                                                
019400         05  W-IDORDER-SPAR      PIC S9(07)   VALUE ZERO COMP-3.          
019500*                                                                         
019600*    --- DIREKTNYCKEL IDDC WDQ212 WDQ3DSEQ WDQ4ASEQ                       
019700     03  W-IDDC-SOEK-X.                                                   
019800        05  W-IDDC-SOEK          PIC  X(02)   VALUE SPACE.                
019900*                                                                         
020000*    --- DIREKTNYCKEL ADLAGOMR WDQ4ASEQ                                   
020100     03  W-ADLAGOMR-X.                                                    
020200        05  W-ADLAGOMR           PIC S9(03)   VALUE ZERO COMP-3.          
020300*                                                                         
020400*    --- DIREKTNYCKEL IDARTNR  WDD301 WDD101                              
020500     03  W-IDARTNR-SOEK-X.                                                
020600        05  W-IDARTNR-SOEK       PIC S9(09)   VALUE ZERO COMP-3.          
020700*                                                                         
020800*    --- DIREKTNYCKEL IDSKYLT  WDD311                                     
020900     03  W-IDSKYLT-SOEK-X.                                                
021000        05  W-IDSKYLT-SOEK       PIC  X(03)   VALUE SPACE.                
021100*                                                                         
021200*    --- DIREKTNYCKEL ADGANG                                              
021300     03  W-ADGANG-X.                                                      
021400        05  W-ADGANG             PIC S9(03)   VALUE ZERO COMP-3.          
021500 EJECT                                                                    
021600*    --- STATUS-KOD FRÅN IMS                                              
021700 01  STATUS-WS                   PIC XX.                                  
021800     88  SEGMENT-FINNS                       VALUE '  '.                  
021900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022100     SKIP2                                                                
022200 01  GODK-STATUSKODER.                                                    
022300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022400     SKIP3                                                                
022500 01  SSA1                        PIC X(128).                              
022600 01  SSA2                        PIC X(64).                               
022700 01  SSA3                        PIC X(64).                               
022800     SKIP3                                                                
022900*    --- HJÄLPFÄLT                                                        
023000*                                                                         
023100 01  W-ADPLATS                   PIC S9(05)   VALUE ZERO  COMP-3.         
023200*                                                                         
023300 01  W-ADLAGOMR-HELP             PIC S9(02)   VALUE ZERO.                 
023400*                                                                         
023500 01  W-IDDISTR-NUM               PIC S9(04)   VALUE ZERO.                 
023600 01  W-IDKUNDNR-NUM              PIC S9(06)   VALUE ZERO.                 
023700*01  W-IDSPECEMB-NUM             PIC S9(04)   VALUE ZERO.                 
023800*                                                                         
023801 01  ARB-ADPLATS.                                                         
023802     03 WU-ADPLATS               PIC 9(5)    VALUE ZERO.                  
023803     03 FILLER                   REDEFINES WU-ADPLATS.                    
023804        05 FILLER                PIC X.                                   
023805        05 WU-ADPLATS-2-3        PIC 9(2).                                
023806        05 WU-ADPLATS-4-5        PIC 9(2).                                
023807                                                                          
023808     03 WI-ADPLATS               PIC 9(5)    VALUE ZERO.                  
023809     03 FILLER                   REDEFINES WI-ADPLATS.                    
023810        05 WI-ADPLATS-1-2        PIC 9(2).                                
023811        05 WI-ADPLATS-3          PIC 9(1).                                
023812        05 WI-ADPLATS-4-5        PIC 9(2).                                
023813*                                                                         
023900*    --- ARBETSFÄLT FRÅN SKÄRMEN                                          
024000 01  W-FLEKOD                    PIC S9       VALUE ZERO.                 
024100     EJECT                                                                
024200*    --- IMS FUNKTIONSKODER                                               
024300*01  -COPY W0003                                                          
024400     EJECT                                                                
024500*    ---  DLI INPUT-OUTPUT AREA                                           
024600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA1'.          
024700     SKIP3                                                                
024800 01  DLI-IO-AREA1.                                                        
024900     03  IO-AREA1                PIC X(3000) VALUE SPACE.                 
025000     SKIP3                                                                
025100     03  WLORQA01 REDEFINES IO-AREA1.                                     
025200*        05  -COPY WDQ301     -PRE ORQA-                                  
025300     EJECT                                                                
025400     03  WLORQF01 REDEFINES IO-AREA1.                                     
025500*        05  -COPY WDQ401     -PRE ORQF-                                  
025600     EJECT                                                                
025700     03  WLBENA11 REDEFINES IO-AREA1.                                     
025800*        05  -COPY WDD311     -PRE BENA-                                  
025900     EJECT                                                                
026900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA2'.          
027000     SKIP3                                                                
027100 01  DLI-IO-AREA2.                                                        
027200     03  IO-AREA2                PIC X(4064) VALUE SPACE.                 
027300     SKIP3                                                                
027400     03  WLORQI01 REDEFINES IO-AREA2.                                     
027500*        05  -COPY WDQ201     -PRE ORQI-                                  
027600     EJECT                                                                
027700     03  WLORQI12 REDEFINES IO-AREA2.                                     
027800*        05  -COPY WDQ212     -PRE ORQI-                                  
027900     EJECT                                                                
028000 LINKAGE SECTION.                                                         
028100                                                                          
028200*01  -COPY W0009      -PRE MSG-                                           
028300     EJECT                                                                
028400*01  -COPY W0008      -PRE USEA-                                          
028500     05  FILLER                  PIC X.                                   
028510     EJECT                                                                
028520*01  -COPY W0008      -PRE ORQA-                                          
028530     05  FILLER                  PIC X.                                   
028600     EJECT                                                                
028700*01  -COPY W0008      -PRE ORQI1-                                         
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000*01  -COPY W0008      -PRE ORQI2-                                         
029100     05  FILLER                  PIC X.                                   
029200     EJECT                                                                
029300*01  -COPY W0008      -PRE ORQF1-                                         
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600*01  -COPY W0008      -PRE ORQF2-                                         
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008      -PRE BENA-                                          
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
030510                                   ORQA-PCB ORQI1-PCB ORQI2-PCB           
030600                           ORQF1-PCB ORQF2-PCB BENA-PCB.                  
030700                                                                          
030800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
030810                                   ORQA-PCB ORQI1-PCB ORQI2-PCB           
030900                           ORQF1-PCB ORQF2-PCB BENA-PCB.                  
031100                                                                          
031200     PERFORM IMS-GET-MSG                                                  
031300     IF SEGMENT-FINNS                                                     
031400       PERFORM A-INIT                                                     
031500       PERFORM B-KOLLA-NYCKLAR                                            
031600       IF NYCKLAR-OK                                                      
031700         IF MFS-UPDATE                                                    
031800           PERFORM G-KOLLA-INPUT-UPPDATERA                                
031900         ELSE                                                             
032000           IF MFS-FIRST                                                   
032100             PERFORM C-FOERSTA-SIDA                                       
032200           ELSE                                                           
032300             IF MFS-NEXT                                                  
032400               PERFORM D-NAESTA-SIDA                                      
032500             ELSE                                                         
032600               PERFORM E-SAMMA-SIDA                                       
032700             END-IF                                                       
032800           END-IF                                                         
032900         END-IF                                                           
033000         IF ALLT-OK                                                       
033100           PERFORM F-LAES-VISA-INFO                                       
033200         END-IF                                                           
033300       END-IF                                                             
033400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
033500       PERFORM IMS-INSERT-MSG                                             
033600     END-IF                                                               
033700                                                                          
033800     MOVE ZERO TO RETURN-CODE                                             
033900     GOBACK                                                               
034000     .                                                                    
034100     EJECT                                                                
034200 A-INIT SECTION.                                                          
034300     IF MSG-DUBBLA-TRANSKODER                                             
034400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I35601                 
034500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
034600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
034700     ELSE                                                                 
034800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I35601                  
034900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
035000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
035100     END-IF                                                               
035200                                                                          
035300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
035400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
035500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
035600                                                                          
035700     MOVE LOW-VALUE TO MSG-AREA                                           
035800     MOVE 'W4O356N1' TO MFS-IDMOD                                         
035900     MOVE '4356' TO MOD-IDTRANS                                           
036000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
036100                                                                          
036200     IF NOT EGEN-MID                                                      
036300       MOVE SPACE TO MFS-KDTRTYP                                          
036400       MOVE '7' TO MFS-IDPFK                                              
036410       MOVE MFS-RENSA-FAELT TO MID-IDDISTR-IN                             
036420                               MID-IDKUNDNR-IN                            
036430                               MID-IDORDNR7-IN                            
036431                               MID-ADLAGOMR-IN                            
036432                               MID-IDPRODNR-IN                            
036433                               MID-FLEKOD-IN                              
036440                               MID-IDARTNR-IN                             
036450                               MID-IDDISTR-UT                             
036460                               MID-IDKUNDNR-UT                            
036470                               MID-IDORDNR7-UT                            
036471                               MID-ADLAGOMR-UT                            
036472                               MID-IDPRODNR-UT                            
036473                               MID-FLEKOD-UT                              
036480                               MID-IDARTNR-UT                             
036500     END-IF                                                               
037400     .                                                                    
037500     EJECT                                                                
037600 B-KOLLA-NYCKLAR SECTION.                                                 
037710                                                                          
037720     MOVE ALL '+'           TO MSGI-WMSGINIT                              
037730     MOVE '001'             TO MSGI-KDCALL                                
037740     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037741     MOVE '4356'            TO MSGI-IDTRANS                               
037742     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037750     IF MFS-IDTRANS = '4356'                                              
037760        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
037770        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
037772                                                                          
037773        MOVE MID-IDORDNR7-IN TO WS-IDKUNDRF-1-7                           
037774        IF WS-IDKUNDRF-1-7 = ALL '+'                                      
037775          MOVE '+++'         TO WS-IDKUNDRF-8-10                          
037776        ELSE                                                              
037777          MOVE SPACE         TO WS-IDKUNDRF-8-10                          
037778        END-IF                                                            
037779        MOVE WS-IDKUNDRF-RED TO MSGI-IDKUNDRF                             
037780        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
037781     END-IF                                                               
037790     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037791                                                                          
037792     IF MSGI-IDLAND-SPR = 'GB'                                            
037793       MOVE +2 TO SPRAK-IX                                                
037794       MOVE 'GB ' TO MED-IDSKYLT                                          
037795     ELSE                                                                 
037796       MOVE +1 TO SPRAK-IX                                                
037797       MOVE 'S  ' TO MED-IDSKYLT                                          
037798     END-IF                                                               
037799                                                                          
037800     PERFORM BA-FLYTTA-VAERDEN                                            
037900     PERFORM BB-KOLL-IDDISTR                                              
038000     PERFORM BC-KOLL-IDKUNDNR                                             
038100     PERFORM BD-KOLL-IDKUNDRF                                             
038200     PERFORM BE-KOLL-IDPRODNR                                             
038300     PERFORM BF-KOLL-IDARTNR                                              
038400     PERFORM BG-KOLL-ADLAGOMR                                             
038500     PERFORM BH-KOLL-FLEKOD                                               
038510     PERFORM BI-KOLL-IDDC                                                 
038600                                                                          
038900     MOVE WS-IDDISTR  TO MOD-IDDISTR-UT                                   
039000     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
039100                                                                          
039200     MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                  
039300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
039400                                                                          
039500     MOVE WS-IDORDNR7 TO MOD-IDORDNR7-UT                                  
039600     INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE              
039700                                                                          
039800     IF IDPRODNR-LAESNING                                                 
039900       MOVE WS-IDPRODNR TO MOD-IDPRODNR-UT                                
040000       INSPECT MOD-IDPRODNR-UT                                            
040100               REPLACING LEADING ZERO BY SPACE                            
040200     END-IF                                                               
040300                                                                          
040400     MOVE WS-ADLAGOMR TO MOD-ADLAGOMR-UT                                  
040500     INSPECT MOD-ADLAGOMR-UT REPLACING LEADING ZERO BY SPACE              
040600                                                                          
040700     MOVE WS-FLEKOD   TO MOD-FLEKOD-UT                                    
040800     INSPECT MOD-FLEKOD-UT REPLACING LEADING ZERO BY SPACE                
040900                                                                          
041000     MOVE WS-IDARTNR  TO MOD-IDARTNR-UT                                   
041100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
041110     MOVE WS1-IDDC    TO MOD-IDDC-UT                                      
041200                                                                          
042300     IF NYCKLAR-FEL                                                       
042400        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
042500        CALL WMEDKONV USING MED-WMEDAREA                                  
042600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
042700        PERFORM MFS-RENSA-FAELT-IN                                        
042800        PERFORM MFS-RENSA-FAELT-UT                                        
042900     END-IF                                                               
043000     .                                                                    
043100     EJECT                                                                
043200 BA-FLYTTA-VAERDEN SECTION.                                               
043300                                                                          
043400     MOVE JA               TO NYCKLAR-SW                                  
043500     MOVE '0'              TO LAES-SW                                     
043600     MOVE MED-IDSKYLT      TO W-IDSKYLT-SOEK                              
043700     MOVE LOW-VALUE        TO W-WDQ3DSEQ-MIN-X                            
043800                              W-WDQ4ASEQ-MIN-X                            
043900     MOVE HIGH-VALUE       TO W-WDQ3DSEQ-MAX-X                            
044000                              W-WDQ4ASEQ-MAX-X                            
044200     IF MID-IDORDER-SPAR = ALL '+'                                        
044210       MOVE ZERO             TO W-IDORDER-SPAR                            
044310     ELSE                                                                 
044311       IF MID-IDORDER-SPAR NUMERIC                                        
044312         MOVE MID-IDORDER-SPAR TO W-IDORDER-SPAR                          
044313       ELSE                                                               
044314         MOVE ZERO             TO W-IDORDER-SPAR                          
044320       END-IF                                                             
044321     END-IF                                                               
044330     MOVE MFS-RENSA-FAELT  TO MOD-IDDISTR-IN                              
044400                              MOD-IDKUNDNR-IN                             
044500                              MOD-IDORDNR7-IN                             
044600                              MOD-IDPRODNR-IN                             
044700                              MOD-IDARTNR-IN                              
044800                              MOD-ADLAGOMR-IN                             
044900                              MOD-FLEKOD-IN                               
045000     MOVE 1   TO INDX                                                     
045100                                                                          
045200     PERFORM UNTIL INDX > MAX-INDX                                        
045300       IF MID-IDARTNR-RAD (INDX) (9:1) NUMERIC                            
045400         IF MID-IDSPECEMB-NY (INDX) = ALL '+' OR ALL SPACE                
045500           CONTINUE                                                       
045600         ELSE                                                             
045700           MOVE JA  TO INPUT-SW                                           
045800         END-IF                                                           
045900       END-IF                                                             
046000       ADD 1 TO INDX                                                      
046100     END-PERFORM                                                          
046200     .                                                                    
046300     EJECT                                                                
046400 BB-KOLL-IDDISTR SECTION.                                                 
046510                                                                          
046520     MOVE MSGI-IDDISTR   TO WS-IDDISTR                                    
046530     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
046540                                                                          
046550     IF MID-IDDISTR-IN = ALL '+'                                          
046560       CONTINUE                                                           
046570     ELSE                                                                 
046580       MOVE '7'         TO MFS-IDPFK                                      
046590       MOVE SPACE       TO MFS-KDTRTYP                                    
046591       MOVE SPACE       TO MID-IDKUNDNR-UT                                
046592                           MID-IDORDNR7-UT                                
046593     END-IF                                                               
046594                                                                          
047600     IF MID-IDPRODNR-IN = ALL '+' OR ZERO                                 
047610     OR MFS-IDTRANS NOT = '4356'                                          
047700       IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                        
047800         MOVE WS-IDDISTR TO W-IDDISTR                                     
047900         MOVE '1'        TO LAES-SW                                       
048000       ELSE                                                               
048100         MOVE NEJ TO NYCKLAR-SW                                           
048200       END-IF                                                             
048300     ELSE                                                                 
048400       MOVE SPACE TO WS-IDDISTR                                           
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 BC-KOLL-IDKUNDNR SECTION.                                                
048910                                                                          
048920     MOVE MSGI-IDKUNDNR    TO WS-IDKUNDNR                                 
048930     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
048940                                                                          
048950     IF MID-IDKUNDNR-IN = ALL '+'                                         
048960       CONTINUE                                                           
048970     ELSE                                                                 
048980       MOVE '7'         TO MFS-IDPFK                                      
048990       MOVE SPACE       TO MFS-KDTRTYP                                    
048991     END-IF                                                               
048998                                                                          
049800     IF MID-IDPRODNR-IN = ALL '+' OR ZERO                                 
049810     OR MFS-IDTRANS NOT = '4356'                                          
049900        IF WS-IDKUNDNR NUMERIC                                            
050000          MOVE WS-IDKUNDNR TO W-IDKUNDNR                                  
050100        ELSE                                                              
050200          MOVE NEJ TO NYCKLAR-SW                                          
050300        END-IF                                                            
050400     ELSE                                                                 
050500        MOVE SPACE TO WS-IDKUNDNR                                         
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 BD-KOLL-IDKUNDRF SECTION.                                                
051010                                                                          
051020     MOVE MSGI-IDKUNDRF    TO WS-IDORDNR7                                 
051030     INSPECT WS-IDORDNR7 REPLACING LEADING SPACE BY ZERO                  
051040                                                                          
051100     IF MID-IDORDNR7-IN = ALL '+'                                         
051200       CONTINUE                                                           
051400     ELSE                                                                 
051600       MOVE '7'         TO MFS-IDPFK                                      
051700       MOVE SPACE       TO MFS-KDTRTYP                                    
051800     END-IF                                                               
051900     IF MID-IDPRODNR-IN = ALL '+' OR ZERO                                 
051910     OR MFS-IDTRANS NOT = '4356'                                          
052000        IF WS-IDORDNR7 NUMERIC AND WS-IDORDNR7 > ZERO                     
052100          MOVE WS-IDKUNDRF TO W-IDKUNDRF                                  
052200        ELSE                                                              
052300          MOVE NEJ TO NYCKLAR-SW                                          
052400        END-IF                                                            
052500     ELSE                                                                 
052600        MOVE SPACE TO WS-IDKUNDRF                                         
052700     END-IF                                                               
052800     .                                                                    
052900     EJECT                                                                
053000 BE-KOLL-IDPRODNR SECTION.                                                
053100                                                                          
053200     IF MID-IDPRODNR-IN = ALL '+'                                         
053300       MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                                
053400       INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO                
053500     ELSE                                                                 
053600       MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                                
053700       MOVE '7'         TO MFS-IDPFK                                      
053800       MOVE SPACE       TO MFS-KDTRTYP                                    
053900     END-IF                                                               
054000                                                                          
054100     IF WS-IDPRODNR NUMERIC AND WS-IDPRODNR > ZERO                        
054200       MOVE WS-IDPRODNR TO W-IDPRODNR-MIN                                 
054300                           W-IDPRODNR-MAX                                 
054400       MOVE '2'           TO LAES-SW                                      
054500     END-IF                                                               
054650     .                                                                    
054700     EJECT                                                                
054800 BF-KOLL-IDARTNR SECTION.                                                 
054900                                                                          
054910     IF EGEN-MID                                                          
055000       IF MID-IDARTNR-IN = ALL '+'                                        
055100          IF MFS-UPDATE                                                   
055200            MOVE MID-IDARTNR-UT TO WS-IDARTNR                             
055300          ELSE                                                            
055400            MOVE      ZEROS     TO WS-IDARTNR                             
055500          END-IF                                                          
055600       ELSE                                                               
055700          MOVE MID-IDARTNR-IN TO WS-IDARTNR                               
055800          MOVE '7'         TO MFS-IDPFK                                   
055900          MOVE SPACE       TO MFS-KDTRTYP                                 
056000       END-IF                                                             
056010     ELSE                                                                 
056020       MOVE ZERO           TO WS-IDARTNR                                  
056030     END-IF                                                               
056100     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
056200     IF WS-IDARTNR NUMERIC                                                
056300       IF WS-IDARTNR > ZERO                                               
056400          MOVE WS-IDARTNR TO W-4A1-IDARTNR                                
056500                             W-IDARTNR-MIN                                
056600                             W-IDARTNR-SOEK                               
056700                             MOD-IDARTNR-ENTER                            
056800       END-IF                                                             
056900     ELSE                                                                 
057000        MOVE NEJ TO NYCKLAR-SW                                            
057100     END-IF                                                               
057200     .                                                                    
057300     EJECT                                                                
057400 BG-KOLL-ADLAGOMR SECTION.                                                
057500                                                                          
057510     IF EGEN-MID                                                          
057600       IF MID-ADLAGOMR-IN = ALL '+'                                       
057700          MOVE MID-ADLAGOMR-UT TO WS-ADLAGOMR                             
057800       ELSE                                                               
057900          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
058000          MOVE '7'         TO MFS-IDPFK                                   
058100          MOVE SPACE       TO MFS-KDTRTYP                                 
058200       END-IF                                                             
058210     ELSE                                                                 
058220       MOVE ZERO           TO WS-ADLAGOMR                                 
058230     END-IF                                                               
058300     INSPECT WS-ADLAGOMR REPLACING LEADING SPACE BY ZERO                  
058400     IF WS-ADLAGOMR NUMERIC                                               
058500       IF WS-ADLAGOMR > ZERO                                              
058600          IF WS-IDARTNR NOT > ZERO                                        
058700            MOVE WS-ADLAGOMR     TO W-ADLAGOMR-HELP                       
058800            MOVE W-ADLAGOMR-HELP TO W-ADLAGOMR                            
058900          ELSE                                                            
059000           MOVE NEJ TO NYCKLAR-SW                                         
059100          END-IF                                                          
059200       END-IF                                                             
059300     ELSE                                                                 
059400        MOVE NEJ TO NYCKLAR-SW                                            
059500     END-IF                                                               
059600     .                                                                    
059700     EJECT                                                                
059800 BH-KOLL-FLEKOD SECTION.                                                  
059900                                                                          
059910     IF EGEN-MID                                                          
060000       IF MID-FLEKOD-IN = ALL '+'                                         
060100          MOVE MID-FLEKOD-UT TO WS-FLEKOD                                 
060200       ELSE                                                               
060300          MOVE MID-FLEKOD-IN TO WS-FLEKOD                                 
060400          MOVE '7'         TO MFS-IDPFK                                   
060500          MOVE SPACE       TO MFS-KDTRTYP                                 
060600       END-IF                                                             
060610     ELSE                                                                 
060620       MOVE ZERO           TO WS-FLEKOD                                   
060630     END-IF                                                               
060700     INSPECT WS-FLEKOD REPLACING LEADING SPACE BY ZERO                    
060800     IF WS-FLEKOD NUMERIC                                                 
060900       IF WS-FLEKOD > ZERO                                                
061000         IF WS-IDARTNR NOT > ZERO                                         
061100           MOVE WS-FLEKOD  TO W-FLEKOD                                    
061200         ELSE                                                             
061300           MOVE NEJ TO NYCKLAR-SW                                         
061400         END-IF                                                           
061500       END-IF                                                             
061600     ELSE                                                                 
061700        MOVE NEJ TO NYCKLAR-SW                                            
061800     END-IF                                                               
061900     .                                                                    
062000     EJECT                                                                
062010 BI-KOLL-IDDC     SECTION.                                                
062024                                                                          
062025     IF EGEN-MID                                                          
062026                                                                          
062027       IF MID-IDDC-IN = ALL '+'                                           
062028         MOVE MID-IDDC-UT TO WS1-IDDC                                     
062029       ELSE                                                               
062030         MOVE MID-IDDC-IN TO WS1-IDDC                                     
062031         MOVE '7'         TO MFS-IDPFK                                    
062032         MOVE SPACE       TO MFS-KDTRTYP                                  
062033       END-IF                                                             
062034                                                                          
062035     ELSE                                                                 
062036       MOVE MSGI-IDDC          TO WS1-IDDC                                
062037     END-IF                                                               
062039                                                                          
062040     IF WS1-IDDC > ZERO                                                   
062041       MOVE WS1-IDDC                      TO W-IDDC-SOEK                  
062042     ELSE                                                                 
062043       MOVE NEJ                           TO NYCKLAR-SW                   
062044     END-IF                                                               
062049     .                                                                    
062050     EJECT                                                                
062100 C-FOERSTA-SIDA SECTION.                                                  
062200                                                                          
062300     PERFORM S07-INF-FIRST-PAGE                                           
062400     MOVE ZERO  TO W-4A1-IDARTNR                                          
062500     MOVE ZERO  TO W-4A1-IDLOPNR                                          
062600     .                                                                    
062700     EJECT                                                                
062800 D-NAESTA-SIDA SECTION.                                                   
062900                                                                          
063000     IF MID-IDARTNR-NEXT = 'S L U T .'                                    
063100        PERFORM S04-ERR-LAST-PAGE-SHOWED                                  
063200        MOVE MID-IDARTNR-NEXT TO MOD-IDARTNR-ENTER                        
063300                                 MOD-IDARTNR-NEXT                         
063400        MOVE NEJ TO ALLT-SW                                               
063500     ELSE                                                                 
063600        MOVE MID-IDARTNR-NEXT  TO MOD-IDARTNR-ENTER                       
063700                                  W-4A1-IDARTNR                           
063800                                  W-IDARTNR-MIN                           
063900        MOVE MID-IDLOPNR-NEXT  TO MOD-IDLOPNR-ENTER                       
064000                                  W-4A1-IDLOPNR                           
064100                                  W-IDLOPNR-MIN                           
064200     END-IF                                                               
064300     .                                                                    
064400     EJECT                                                                
064500 E-SAMMA-SIDA SECTION.                                                    
064600                                                                          
064700     IF INPUT-SAKNAS                                                      
064800        IF MID-IDARTNR-ENTER = 'S L U T .'                                
064900           PERFORM S04-ERR-LAST-PAGE-SHOWED                               
065000           MOVE MID-IDARTNR-ENTER TO MOD-IDARTNR-ENTER                    
065100                                     MOD-IDARTNR-NEXT                     
065200           MOVE NEJ TO ALLT-SW                                            
065300        ELSE                                                              
065400          MOVE MID-IDARTNR-ENTER  TO MOD-IDARTNR-ENTER                    
065500                                     W-4A1-IDARTNR                        
065600                                     W-IDARTNR-MIN                        
065700          MOVE MID-IDLOPNR-ENTER  TO MOD-IDLOPNR-ENTER                    
065800                                     W-4A1-IDLOPNR                        
065900                                     W-IDLOPNR-MIN                        
066000        END-IF                                                            
066100     ELSE                                                                 
066200       MOVE W-IDORDER-SPAR TO MOD-IDORDER-SPAR                            
066300       MOVE NEJ TO ALLT-SW                                                
066400       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
066500       CALL WMEDKONV USING MED-WMEDAREA                                   
066600       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
066700       PERFORM MFS-ROR-EJ-FAELT-IN                                        
066800       PERFORM MFS-ROR-EJ-FAELT-UT                                        
066900       PERFORM MFS-LAS-IN-IGEN                                            
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300 F-LAES-VISA-INFO SECTION.                                                
067400                                                                          
067500     MOVE NEJ TO FLER-SIDOR-SW                                            
067600                                                                          
067700     IF IDGMTREF-LAESNING                                                 
067800        PERFORM IMS-GET-ORQI-Q201                                         
067900        IF SEGMENT-FINNS                                                  
068000           MOVE ORQI-OHUV-IDORDER TO W-4A1-IDORDER                        
068100                                     W-IDORDER-MIN                        
068200                                     W-IDORDER-MAX                        
068300                                     W-IDORDER-SOEK                       
068400                                     W-IDORDER-SPAR                       
068500           PERFORM FA-BASDATA-FINNS                                       
068600        ELSE                                                              
068700           PERFORM S03-ERR-INF-MISS                                       
068800        END-IF                                                            
068900     ELSE                                                                 
069000         IF IDPRODNR-LAESNING                                             
069100            PERFORM IMS-GET-ORQA-Q301                                     
069200            IF SEGMENT-FINNS AND                                          
069300               ORQA-ODEL-IDDC = W-IDDC-SOEK                               
069400               MOVE ORQA-ODEL-IDDISTR  TO W-IDDISTR-NUM                   
069500               MOVE W-IDDISTR-NUM      TO MOD-IDDISTR-UT                  
069600               INSPECT MOD-IDDISTR-UT                                     
069700                       REPLACING LEADING ZERO BY SPACE                    
069800               MOVE ORQA-ODEL-IDKUNDNR TO W-IDKUNDNR-NUM                  
069900               MOVE W-IDKUNDNR-NUM     TO MOD-IDKUNDNR-UT                 
070000               INSPECT MOD-IDKUNDNR-UT                                    
070100                       REPLACING LEADING ZERO BY SPACE                    
070200               MOVE ORQA-ODEL-IDORDNR7 TO MOD-IDORDNR7-UT                 
070300               INSPECT MOD-IDORDNR7-UT                                    
070400                       REPLACING LEADING ZERO BY SPACE                    
070500               MOVE ORQA-ODEL-IDORDER  TO W-4A1-IDORDER                   
070600                                          W-IDORDER-MIN                   
070700                                          W-IDORDER-MAX                   
070800                                          W-IDORDER-SOEK                  
070900                                          W-IDORDER-SPAR                  
071000               PERFORM FA-BASDATA-FINNS                                   
071100            ELSE                                                          
071200               PERFORM S03-ERR-INF-MISS                                   
071300            END-IF                                                        
071400         ELSE                                                             
071500            PERFORM S03-ERR-INF-MISS                                      
071600         END-IF                                                           
071700     END-IF                                                               
071800     .                                                                    
071900     EJECT                                                                
072000 FA-BASDATA-FINNS SECTION.                                                
072100     MOVE W-IDORDER-SPAR TO MOD-IDORDER-SPAR                              
072200     MOVE +1 TO INDX                                                      
072300                                                                          
072400     PERFORM IMS-GU-ORQF-Q4A1                                             
072500     PERFORM UNTIL INDX > MAX-INDX                                        
072600       IF SEGMENT-FINNS                                                   
072700          IF W-FLEKOD NOT = ALL ZEROS                                     
072800             IF W-ADLAGOMR NOT = ALL ZEROS                                
072900                IF W-ADLAGOMR = ORQF-ORAD-ADLAGOMR AND                    
073000                   W-FLEKOD   = ORQF-ORAD-KDSPEEMB                        
073100                   PERFORM FAA-SKRIV-RAD                                  
073200                END-IF                                                    
073300             ELSE                                                         
073400                IF W-FLEKOD = ORQF-ORAD-KDSPEEMB                          
073500                   PERFORM FAA-SKRIV-RAD                                  
073600                END-IF                                                    
073700             END-IF                                                       
073800          ELSE                                                            
073900             IF W-ADLAGOMR NOT = ALL ZEROS                                
074000                IF W-ADLAGOMR = ORQF-ORAD-ADLAGOMR                        
074100                   PERFORM FAA-SKRIV-RAD                                  
074200                END-IF                                                    
074300             ELSE                                                         
074400                PERFORM FAA-SKRIV-RAD                                     
074500             END-IF                                                       
074600          END-IF                                                          
074700          PERFORM IMS-GET-ORQF-Q4A1                                       
074800       ELSE                                                               
074900         MOVE MFS-RENSA-FAELT TO                                          
075000                               MOD-IDSPECEMB-RAD (INDX)                   
075100                               MOD-IDARTNR-RAD (INDX)                     
075200                               MOD-IDLOPNR-RAD (INDX)                     
075300                               MOD-KDSPEEMB-RAD (INDX)                    
075400                               MOD-ADLAGOMR-RAD (INDX)                    
075500                               MOD-ADGANG-RAD (INDX)                      
075600                               MOD-ADPLATS-RAD (INDX)                     
075700                               MOD-KDARTURS-RAD (INDX)                    
075800                               MOD-KVBEART-RAD (INDX)                     
075910                               MOD-BEART-RAD (INDX)                       
076000         MOVE MFS-STAENG-FAELT TO                                         
076100                               MOD-IDSPECEMB-NY-ATTR (INDX)               
076200         ADD 1 TO INDX                                                    
076300       END-IF                                                             
076400     END-PERFORM                                                          
076500                                                                          
076600     MOVE NEJ TO FLER-SIDOR-SW                                            
076700     IF SEGMENT-FINNS                                                     
076800        PERFORM FAC-FLER-SIDOR-KOLL                                       
076900     END-IF                                                               
077000     IF EJ-FLER-SIDOR                                                     
077100        MOVE 'S L U T .' TO MOD-IDARTNR-NEXT                              
077200        IF MFS-UPDATE                                                     
077300           CONTINUE                                                       
077400        ELSE                                                              
077500           IF MFS-FIRST                                                   
077600              CONTINUE                                                    
077700           ELSE                                                           
077800              PERFORM S09-INF-LAST-PAGE                                   
077900           END-IF                                                         
078000        END-IF                                                            
078100     END-IF                                                               
078200                                                                          
078300     PERFORM MFS-RENSA-FAELT-IN                                           
078400     .                                                                    
078500     EJECT                                                                
078600 FAA-SKRIV-RAD SECTION.                                                   
078700                                                                          
078800     IF INDX                    =  1                                      
078900         MOVE ORQF-ORAD-IDARTNR TO MOD-IDARTNR-ENTER                      
079000         MOVE ORQF-ORAD-IDLOPNR TO MOD-IDLOPNR-ENTER                      
079100     END-IF                                                               
079200                                                                          
079300     IF ORQF-ORAD-IDSPECEMB = ZERO                                        
079400        MOVE MFS-RENSA-FAELT TO MOD-IDSPECEMB-RAD (INDX)                  
079500     ELSE                                                                 
079600        MOVE ORQF-ORAD-IDSPECEMB TO MOD-IDSPECEMB-RAD (INDX)              
079700     END-IF                                                               
079800     MOVE ORQF-ORAD-IDARTNR   TO MOD-IDARTNR-RAD (INDX)                   
079900                                 W-IDARTNR-SOEK                           
080000     INSPECT MOD-IDARTNR-RAD (INDX)                                       
080100             REPLACING LEADING ZERO BY SPACE                              
080200     MOVE ORQF-ORAD-IDLOPNR   TO MOD-IDLOPNR-RAD (INDX)                   
080300     INSPECT MOD-IDLOPNR-RAD (INDX)                                       
080400             REPLACING LEADING ZERO BY SPACE                              
080500     MOVE ORQF-ORAD-KDSPEEMB  TO MOD-KDSPEEMB-RAD (INDX)                  
080600     INSPECT MOD-KDSPEEMB-RAD (INDX)                                      
080700             REPLACING LEADING ZERO BY SPACE                              
080800     MOVE ORQF-ORAD-ADLAGOMR  TO W-ADLAGOMR-HELP                          
080900     MOVE W-ADLAGOMR-HELP     TO MOD-ADLAGOMR-RAD (INDX)                  
081000     INSPECT MOD-ADLAGOMR-RAD (INDX)                                      
081100             REPLACING LEADING ZERO BY SPACE                              
081200     MOVE ORQF-ORAD-ADGANG    TO MOD-ADGANG-RAD (INDX)                    
081300                                 W-ADGANG                                 
081400     MOVE ORQF-ORAD-ADPLATS   TO MOD-ADPLATS-Q4 (INDX)                    
081500     PERFORM FAAA-KOLL-ADPLATS                                            
081600                                                                          
081700     MOVE W-ADPLATS           TO MOD-ADPLATS-RAD (INDX)                   
081800     INSPECT MOD-ADPLATS-RAD (INDX)                                       
081900             REPLACING LEADING ZERO BY SPACE                              
082000     MOVE ORQF-ORAD-KDARTURS  TO MOD-KDARTURS-RAD (INDX)                  
082100     MOVE ORQF-ORAD-KVBEART-Q TO MOD-KVBEART-RAD (INDX)                   
082200     INSPECT MOD-KVBEART-RAD (INDX)                                       
082300             REPLACING LEADING ZERO BY SPACE                              
082400                                                                          
082500     PERFORM IMS-GU-BENA-D311                                             
082600                                                                          
082700     MOVE BENA-TEXT-BEART     TO MOD-BEART-RAD (INDX)                     
082800                                                                          
082900     ADD 1 TO INDX                                                        
083000     .                                                                    
083100     EJECT                                                                
083200 FAAA-KOLL-ADPLATS SECTION.                                               
083300                                                                          
083310     MOVE ORQF-ORAD-IDDC    TO WS-IDDC                                    
083330     IF CDC                                                               
083340        AND                                                               
083350       (ORQF-ORAD-ADLAGOMR = 21 OR 22)                                    
083380        MOVE ORQF-ORAD-ADPLATS TO WI-ADPLATS                              
083381        IF WI-ADPLATS-3 = 1                                               
083382          MOVE WI-ADPLATS TO WU-ADPLATS                                   
083383        ELSE                                                              
083384          MOVE ZEROS           TO WU-ADPLATS                              
083390          MOVE WI-ADPLATS-1-2  TO WU-ADPLATS-2-3                          
083392          MOVE WI-ADPLATS-4-5  TO WU-ADPLATS-4-5                          
083397        END-IF                                                            
083398        MOVE WU-ADPLATS        TO W-ADPLATS                               
083399     ELSE                                                                 
084000        MOVE ORQF-ORAD-ADPLATS  TO W-ADPLATS                              
084100     END-IF                                                               
084200     .                                                                    
084300     EJECT                                                                
084400 FAC-FLER-SIDOR-KOLL SECTION.                                             
084500                                                                          
084600     PERFORM UNTIL FLER-SIDOR OR SEGMENT-SAKNAS                           
084700       IF W-FLEKOD NOT = ALL ZEROS                                        
084800          IF W-ADLAGOMR NOT = ALL ZEROS                                   
084900             IF W-ADLAGOMR = ORQF-ORAD-ADLAGOMR AND                       
085000                W-FLEKOD   = ORQF-ORAD-KDSPEEMB                           
085100                PERFORM FACA-FLER-SIDOR                                   
085200             END-IF                                                       
085300          ELSE                                                            
085400             IF W-FLEKOD = ORQF-ORAD-KDSPEEMB                             
085500                PERFORM FACA-FLER-SIDOR                                   
085600             END-IF                                                       
085700          END-IF                                                          
085800       ELSE                                                               
085900          IF W-ADLAGOMR NOT = ALL ZEROS                                   
086000             IF W-ADLAGOMR = ORQF-ORAD-ADLAGOMR                           
086100                PERFORM FACA-FLER-SIDOR                                   
086200             END-IF                                                       
086300          ELSE                                                            
086400             PERFORM FACA-FLER-SIDOR                                      
086500          END-IF                                                          
086600       END-IF                                                             
086700       PERFORM IMS-GET-ORQF-Q4A1                                          
086800     END-PERFORM                                                          
086900     .                                                                    
087000     EJECT                                                                
087100 FACA-FLER-SIDOR SECTION.                                                 
087200                                                                          
087300     MOVE ORQF-ORAD-IDARTNR  TO MOD-IDARTNR-NEXT                          
087400                                W-IDARTNR-MIN                             
087500     MOVE ORQF-ORAD-IDLOPNR  TO MOD-IDLOPNR-NEXT                          
087600                                W-IDLOPNR-MIN                             
087700     MOVE ORQF-ORAD-ADLAGOMR TO W-ADLAGOMR-HELP                           
087800     MOVE W-ADLAGOMR-HELP    TO MOD-ADLAGOMR-NEXT                         
087900                                                                          
088000     PERFORM S06-INF-MORE-INFO                                            
088100     MOVE JA TO FLER-SIDOR-SW                                             
088200     .                                                                    
088300     EJECT                                                                
088400 G-KOLLA-INPUT-UPPDATERA SECTION.                                         
088500                                                                          
088600     MOVE 1   TO INDX                                                     
088700     MOVE JA  TO INDATA-SW                                                
088800     MOVE NEJ TO INPUT-SW                                                 
088900                                                                          
089000     PERFORM UNTIL INDX > MAX-INDX                                        
089100       IF MID-IDARTNR-RAD (INDX) (9:1) NUMERIC                            
089200         IF MID-IDSPECEMB-NY (INDX) = ALL '+'                             
089300           CONTINUE                                                       
089400         ELSE                                                             
089500           PERFORM GA-KOLL-INPUT-PAA-RAD                                  
089600           MOVE JA  TO INPUT-SW                                           
089700         END-IF                                                           
089800       END-IF                                                             
089900       ADD 1 TO INDX                                                      
090000     END-PERFORM                                                          
090100     IF INPUT-SAKNAS                                                      
090200       PERFORM S01-ERR-PF11-AND-NO-DATA                                   
090300     ELSE                                                                 
090400       IF ALLT-OK                                                         
090500         MOVE 1  TO INDX                                                  
090600         MOVE W-IDORDER-SPAR TO W-IDORDER-SOEK                            
090700         PERFORM IMS-GHU-ORQI-Q212                                        
090800         PERFORM UNTIL INDX > MAX-INDX                                    
090900           IF MID-IDARTNR-RAD (INDX) (9:1) NUMERIC AND                    
091000              MID-IDSPECEMB-NY (INDX) NOT = ALL '+'                       
091100              PERFORM GB-UPPDATERA                                        
091200           END-IF                                                         
091300           ADD 1 TO INDX                                                  
091400         END-PERFORM                                                      
091500         PERFORM IMS-REPL-ORQI                                            
091600         PERFORM S08-INF-UPDATE-DONE                                      
091700         MOVE W-IDORDER-SPAR          TO W-IDORDER-MIN                    
091800         MOVE MID-IDARTNR-ENTER       TO MOD-IDARTNR-ENTER                
091900                                         W-IDARTNR-MIN                    
092000         MOVE MID-IDLOPNR-ENTER       TO MOD-IDLOPNR-ENTER                
092100                                         W-IDLOPNR-MIN                    
092200         MOVE MID-ADLAGOMR-ENTER      TO MOD-ADLAGOMR-ENTER               
092300       END-IF                                                             
092400     END-IF                                                               
092500     .                                                                    
092600     EJECT                                                                
092700 GA-KOLL-INPUT-PAA-RAD SECTION.                                           
092800                                                                          
092900     IF MID-IDSPECEMB-NY (INDX) NUMERIC                                   
093000        MOVE MFS-NUM-FAELT-RAETT TO                                       
093100             MOD-IDSPECEMB-NY-ATTR (INDX)                                 
093200       IF MID-IDSPECEMB-NY (INDX) NOT = ZERO                              
093300         INSPECT MID-IDSPECEMB-NY (INDX)                                  
093400                       REPLACING LEADING ZEROS BY SPACE                   
093500         IF MID-IDSPECEMB-NY (INDX) =                                     
093600            MID-IDSPECEMB-RAD (INDX)                                      
093700              MOVE MFS-ADD-LYS-UPP-FAELT TO                               
093800              MOD-IDSPECEMB-NY-ATTR (INDX)                                
093900              PERFORM S05-ERR-NO-CHANGE                                   
094000              MOVE NEJ TO ALLT-SW                                         
094100         END-IF                                                           
094200       END-IF                                                             
094300     ELSE                                                                 
094400        MOVE MFS-NUM-FAELT-FEL TO                                         
094500             MOD-IDSPECEMB-NY-ATTR (INDX)                                 
094600        PERFORM S02-ERR-CORR-HILITE-FLDS                                  
094700        MOVE NEJ TO INDATA-SW                                             
094800                    ALLT-SW                                               
094900     END-IF                                                               
095000     .                                                                    
095100     EJECT                                                                
095200 GB-UPPDATERA SECTION.                                                    
095300                                                                          
095400     PERFORM GBA-FLYTTA-VAERDEN                                           
095500     IF ORQI-ARB-KVSEMBRA > ZERO                                          
095600        SUBTRACT 1 FROM ORQI-ARB-KVSEMBRA                                 
095700        END-SUBTRACT                                                      
095800     END-IF                                                               
095900                                                                          
096000     PERFORM IMS-GHU-ORQF-Q401                                            
096100     MOVE W-401-IDARTNR           TO W-IDARTNR-MIN                        
096200     MOVE W-401-IDLOPNR           TO W-IDLOPNR-MIN                        
096300     MOVE MID-IDSPECEMB-NY (INDX) TO ORQF-ORAD-IDSPECEMB                  
096400     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDSPECEMB-RAD-ATTR (INDX)          
096500     PERFORM IMS-REPL-ORQF                                                
096600     .                                                                    
096700     EJECT                                                                
096800 GBA-FLYTTA-VAERDEN SECTION.                                              
096900                                                                          
097000     MOVE W-IDORDER-SPAR          TO W-IDORDER-SOEK                       
097100                                     W-401-IDORDER                        
097200     MOVE W-IDDC-SOEK             TO W-401-IDDC                           
097300     MOVE MID-ADLAGOMR-RAD (INDX) TO W-ADLAGOMR-HELP                      
097400     MOVE W-ADLAGOMR-HELP         TO W-401-ADLAGOMR                       
097500     MOVE MID-ADGANG-RAD (INDX)   TO W-401-ADGANG                         
097600     MOVE MID-ADPLATS-Q4 (INDX)   TO W-401-ADPLATS                        
097700     MOVE MID-IDARTNR-RAD (INDX)  TO W-401-IDARTNR                        
097800     MOVE MID-IDLOPNR-RAD (INDX)  TO W-401-IDLOPNR                        
097900     .                                                                    
098000     EJECT                                                                
098100 S01-ERR-PF11-AND-NO-DATA SECTION.                                        
098200     MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                            
098300     CALL WMEDKONV USING MED-WMEDAREA                                     
098400     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
098500     PERFORM MFS-ROR-EJ-FAELT-IN                                          
098600     PERFORM MFS-ROR-EJ-FAELT-UT                                          
098700     MOVE NEJ TO INDATA-SW                                                
098800     .                                                                    
098900     SKIP2                                                                
099000 S02-ERR-CORR-HILITE-FLDS SECTION.                                        
099100     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
099200     CALL WMEDKONV USING MED-WMEDAREA                                     
099300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
099400     PERFORM MFS-ROR-EJ-FAELT-UT                                          
099500     PERFORM MFS-ROR-EJ-FAELT-IN                                          
099600     .                                                                    
099700     SKIP2                                                                
099800 S03-ERR-INF-MISS SECTION.                                                
099900     MOVE ERR-INF-MISS TO MED-IDMFSFEL                                    
100000     CALL WMEDKONV USING MED-WMEDAREA                                     
100100     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
100200     PERFORM MFS-RENSA-FAELT-UT                                           
100300     .                                                                    
100400     SKIP2                                                                
100500 S04-ERR-LAST-PAGE-SHOWED SECTION.                                        
100600     MOVE ERR-LAST-PAGE-SHOWED TO MED-IDMFSFEL                            
100700     CALL WMEDKONV USING MED-WMEDAREA                                     
100800     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
100900     PERFORM MFS-RENSA-FAELT-UT                                           
101000     .                                                                    
101100     EJECT                                                                
101200 S05-ERR-NO-CHANGE SECTION.                                               
101300     MOVE ERR-NO-CHANGE TO MED-IDMFSFEL                                   
101400     CALL WMEDKONV USING MED-WMEDAREA                                     
101500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
101600     PERFORM MFS-ROR-EJ-FAELT-UT                                          
101700     PERFORM MFS-ROR-EJ-FAELT-IN                                          
101800     .                                                                    
101900     SKIP2                                                                
102000 S06-INF-MORE-INFO SECTION.                                               
102100     MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                            
102200     CALL WMEDKONV USING MED-WMEDAREA                                     
102300     MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                    
102400     .                                                                    
102500     SKIP2                                                                
102600 S07-INF-FIRST-PAGE SECTION.                                              
102700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
102800     CALL WMEDKONV USING MED-WMEDAREA                                     
102900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
103000     .                                                                    
103100     SKIP2                                                                
103200 S08-INF-UPDATE-DONE SECTION.                                             
103300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
103400     CALL WMEDKONV USING MED-WMEDAREA                                     
103500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
103600     PERFORM MFS-FORM-ATTR                                                
103700     PERFORM MFS-RENSA-FAELT-IN                                           
103800     .                                                                    
103900     SKIP2                                                                
104000 S09-INF-LAST-PAGE SECTION.                                               
104100     MOVE INF-LAST-PAGE TO MED-IDMFSINF                                   
104200     CALL WMEDKONV USING MED-WMEDAREA                                     
104300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
104400     .                                                                    
104500     EJECT                                                                
104600 MFS-RENSA-FAELT-UT SECTION.                                              
104700                                                                          
104800*    --- ALLA UTDATA-FÄLT                                                 
104900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
105000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ENTER                            
105100                             MOD-IDARTNR-NEXT                             
105200                             MOD-IDLOPNR-ENTER                            
105300                             MOD-IDLOPNR-NEXT                             
105400                             MOD-ADLAGOMR-ENTER                           
105500                             MOD-ADLAGOMR-NEXT                            
105600                                                                          
105700     MOVE +1 TO INDX                                                      
105800     PERFORM UNTIL INDX > MAX-INDX                                        
105900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
106000       ADD +1 TO INDX                                                     
106100     END-PERFORM                                                          
106200     .                                                                    
106300     SKIP2                                                                
106400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
106500                                                                          
106600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
106700     MOVE MFS-RENSA-FAELT TO MOD-IDSPECEMB-RAD (INDX)                     
106800                             MOD-IDARTNR-RAD (INDX)                       
106900                             MOD-IDLOPNR-RAD (INDX)                       
107000                             MOD-KDSPEEMB-RAD (INDX)                      
107100                             MOD-ADLAGOMR-RAD (INDX)                      
107200                             MOD-ADGANG-RAD (INDX)                        
107300                             MOD-ADPLATS-RAD (INDX)                       
107400                             MOD-KDARTURS-RAD (INDX)                      
107500                             MOD-KVBEART-RAD (INDX)                       
107600                             MOD-BEART-RAD (INDX)                         
107700     .                                                                    
107800     EJECT                                                                
107900 MFS-RENSA-FAELT-IN SECTION.                                              
108000                                                                          
108100*    --- ALLA INDATA-FÄLT                                                 
108200     MOVE +1 TO INDX                                                      
108300     PERFORM UNTIL INDX > MAX-INDX                                        
108400       MOVE MFS-RENSA-FAELT TO MOD-IDSPECEMB-NY (INDX)                    
108500       ADD 1 TO INDX                                                      
108600     END-PERFORM                                                          
108700     .                                                                    
108800     EJECT                                                                
108900 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
109000                                                                          
109100*    --- ALLA UTDATA-FÄLT                                                 
109200*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
109300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-ENTER                          
109400                               MOD-IDARTNR-NEXT                           
109500                               MOD-IDLOPNR-ENTER                          
109600                               MOD-IDLOPNR-NEXT                           
109700                               MOD-ADLAGOMR-ENTER                         
109800                               MOD-ADLAGOMR-NEXT                          
109900                               MOD-IDORDER-SPAR                           
110000                                                                          
110100     MOVE +1 TO INDX                                                      
110200     PERFORM UNTIL INDX > MAX-INDX                                        
110300       PERFORM MFS-ROR-EJ-RAD-FAELT-UT                                    
110400       ADD +1 TO INDX                                                     
110500     END-PERFORM                                                          
110600     .                                                                    
110700     SKIP2                                                                
110800 MFS-ROR-EJ-RAD-FAELT-UT  SECTION.                                        
110900                                                                          
111000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
111100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDSPECEMB-RAD (INDX)                   
111200                               MOD-IDARTNR-RAD (INDX)                     
111300                               MOD-IDLOPNR-RAD (INDX)                     
111400                               MOD-KDSPEEMB-RAD (INDX)                    
111500                               MOD-ADLAGOMR-RAD (INDX)                    
111600                               MOD-ADGANG-RAD (INDX)                      
111700                               MOD-ADPLATS-RAD (INDX)                     
111800                               MOD-KDARTURS-RAD (INDX)                    
111900                               MOD-KVBEART-RAD (INDX)                     
112000                               MOD-BEART-RAD (INDX)                       
112100     .                                                                    
112200     EJECT                                                                
112300 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
112400                                                                          
112500*    --- ALLA INDATA-FÄLT                                                 
112600     MOVE 1 TO INDX                                                       
112700     PERFORM UNTIL INDX > MAX-INDX                                        
112800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDSPECEMB-NY (INDX)                  
112900                                 MOD-ADPLATS-Q4 (INDX)                    
113000       ADD 1 TO INDX                                                      
113100     END-PERFORM                                                          
113200     .                                                                    
113300     EJECT                                                                
113400 MFS-FORM-ATTR SECTION.                                                   
113500                                                                          
113600*    --- ALLA INDATA-FÄLT                                                 
113700     MOVE 1 TO INDX                                                       
113800     PERFORM UNTIL INDX > MAX-INDX                                        
113900       MOVE MFS-FORMATETS-ATTR TO MOD-IDSPECEMB-NY-ATTR (INDX)            
114000       ADD 1 TO INDX                                                      
114100     END-PERFORM                                                          
114200     .                                                                    
114300     SKIP2                                                                
114400 MFS-LAS-IN-IGEN SECTION.                                                 
114500                                                                          
114600*    --- ALLA INDATA-FÄLT                                                 
114700     MOVE 1 TO INDX                                                       
114800     PERFORM UNTIL INDX > MAX-INDX                                        
114900       MOVE MFS-ADD-LAES-IN-FAELT TO                                      
115000                                 MOD-IDSPECEMB-NY-ATTR (INDX)             
115100       ADD 1 TO INDX                                                      
115200     END-PERFORM                                                          
115300     .                                                                    
115400     EJECT                                                                
115500* --- IMS SEKTIONER ---                                                   
115600     SKIP3                                                                
115700 IMS-GET-MSG SECTION.                                                     
115800                                                                          
115900     MOVE '  QC' TO GODK-STATUSKODER                                      
116000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
116100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
116200     PERFORM IMS-STATUSKONTROLL                                           
116300     .                                                                    
116400     SKIP3                                                                
116500 IMS-INSERT-MSG SECTION.                                                  
116600                                                                          
116610     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
116800       MOVE '0' TO MFS-KDHUVOMR                                           
116900     END-IF                                                               
117000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
117100     MOVE SPACE TO GODK-STATUSKODER                                       
117200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
117300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
117400     PERFORM IMS-STATUSKONTROLL                                           
117500     .                                                                    
117600     EJECT                                                                
117700 IMS-GET-ORQA-Q301 SECTION.                                               
117800                                                                          
117900     STRING 'WLORQA01(WDQ3DSEQ>=' W-WDQ3DSEQ-MIN-X                        
118000                    '&WDQ3DSEQ<=' W-WDQ3DSEQ-MAX-X ')'                    
118100          DELIMITED BY SIZE INTO SSA1                                     
118200     MOVE '  GE' TO GODK-STATUSKODER                                      
118300     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA1 SSA1                     
118400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
118500     PERFORM IMS-STATUSKONTROLL                                           
118600     .                                                                    
118700     EJECT                                                                
118800 IMS-GET-ORQI-Q201 SECTION.                                               
118900                                                                          
119000     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
119100          DELIMITED BY SIZE INTO SSA1                                     
119200     MOVE '  GE' TO GODK-STATUSKODER                                      
119300     CALL CBLTDLI USING GU ORQI1-PCB DLI-IO-AREA2 SSA1                    
119400     MOVE ORQI1-STATUS-CODE TO STATUS-WS                                  
119500     PERFORM IMS-STATUSKONTROLL                                           
119600     .                                                                    
119700     SKIP2                                                                
119800 IMS-GHU-ORQI-Q212 SECTION.                                               
119900                                                                          
120000     STRING 'WLORQI01(IDORDER  =' W-IDORDER-SOEK-X ')'                    
120100          DELIMITED BY SIZE INTO SSA1                                     
120200     STRING 'WLORQI12(IDDC     =' W-IDDC-SOEK-X ')'                       
120300          DELIMITED BY SIZE INTO SSA2                                     
120400     MOVE '    ' TO GODK-STATUSKODER                                      
120500     CALL CBLTDLI USING GHU ORQI2-PCB DLI-IO-AREA2 SSA1 SSA2              
120600     MOVE ORQI2-STATUS-CODE TO STATUS-WS                                  
120700     PERFORM IMS-STATUSKONTROLL                                           
120800     .                                                                    
120900     SKIP2                                                                
121000 IMS-REPL-ORQI SECTION.                                                   
121100                                                                          
121200     MOVE '  ' TO GODK-STATUSKODER                                        
121300     CALL CBLTDLI USING REPL ORQI2-PCB DLI-IO-AREA2                       
121400     MOVE ORQI2-STATUS-CODE TO STATUS-WS                                  
121500     PERFORM IMS-STATUSKONTROLL                                           
121600     .                                                                    
121700     EJECT                                                                
121800 IMS-GET-ORQF-Q4A1 SECTION.                                               
121900                                                                          
122000     STRING 'WLORQF01(WDQ4ASEQ>=' W-WDQ4ASEQ-MIN-X                        
122100                    '&WDQ4ASEQ<=' W-WDQ4ASEQ-MAX-X                        
122200                    '&IDDC     =' W-IDDC-SOEK-X ')'                       
122300          DELIMITED BY SIZE INTO SSA1                                     
122400     MOVE '  GE' TO GODK-STATUSKODER                                      
122500     CALL CBLTDLI USING GN ORQF1-PCB DLI-IO-AREA1 SSA1                    
122600     MOVE ORQF1-STATUS-CODE TO STATUS-WS                                  
122700     PERFORM IMS-STATUSKONTROLL                                           
122800     .                                                                    
122900     SKIP2                                                                
123000 IMS-GU-ORQF-Q4A1 SECTION.                                                
123100                                                                          
123200     STRING 'WLORQF01(WDQ4ASEQ>=' W-WDQ4ASEQ-MIN-X                        
123300                    '&WDQ4ASEQ<=' W-WDQ4ASEQ-MAX-X                        
123400                    '&IDDC     =' W-IDDC-SOEK-X ')'                       
123500          DELIMITED BY SIZE INTO SSA1                                     
123600     MOVE '  GE' TO GODK-STATUSKODER                                      
123700     CALL CBLTDLI USING GU ORQF1-PCB DLI-IO-AREA1 SSA1                    
123800     MOVE ORQF1-STATUS-CODE TO STATUS-WS                                  
123900     PERFORM IMS-STATUSKONTROLL                                           
124000     .                                                                    
124100     EJECT                                                                
124200 IMS-GHU-ORQF-Q401 SECTION.                                               
124300                                                                          
124400     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-X ')'                        
124500          DELIMITED BY SIZE INTO SSA1                                     
124600     MOVE '  GE' TO GODK-STATUSKODER                                      
124700     CALL CBLTDLI USING GHU ORQF2-PCB DLI-IO-AREA1 SSA1                   
124800     MOVE ORQF2-STATUS-CODE TO STATUS-WS                                  
124900     PERFORM IMS-STATUSKONTROLL                                           
125000     .                                                                    
125100     SKIP2                                                                
125200 IMS-REPL-ORQF SECTION.                                                   
125300                                                                          
125400     MOVE '  ' TO GODK-STATUSKODER                                        
125500     CALL CBLTDLI USING REPL ORQF2-PCB DLI-IO-AREA1                       
125600     MOVE ORQF2-STATUS-CODE TO STATUS-WS                                  
125700     PERFORM IMS-STATUSKONTROLL                                           
125800     .                                                                    
125900     EJECT                                                                
126000 IMS-GU-BENA-D311 SECTION.                                                
126100                                                                          
126200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-SOEK-X ')'                    
126300          DELIMITED BY SIZE INTO SSA1                                     
126400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-SOEK-X ')'                    
126500          DELIMITED BY SIZE INTO SSA2                                     
126600     MOVE '  GE' TO GODK-STATUSKODER                                      
126700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA1 SSA1 SSA2                
126800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
126900     PERFORM IMS-STATUSKONTROLL                                           
127000     .                                                                    
127100     EJECT                                                                
128600 IMS-STATUSKONTROLL SECTION.                                              
128700                                                                          
128800     SET STATUS-IX TO 1                                                   
128900     SEARCH GODK-STATUS                                                   
129000       AT END CALL FELLOG                                                 
129100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
129200     END-SEARCH                                                           
129300     .                                                                    
