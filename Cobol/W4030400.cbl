000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4030400.                                                
000400 AUTHOR.         KERSTIN MATTIASSON.                                      
000500 DATE-WRITTEN.   91/01/24.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        KOMPLETTERAT AV GUNILLA JOHANSSON, 1991.                         
001100*        DETTA PROGRAM TAR HAND OM AVVIKELSERAPPORTERINGEN I              
001200*        SATSORDER. I BILDEN RAPPORTERAR MAN IN ANTAL FUNNA               
001300*        ARTIKLAR AV DE INGÅENDE ARTIKLAR SOM EJ KUNNAT PLOCKAS           
001400*        I RÄTT ANTAL.                                                    
001500*        FALL1: MAN HITTAR MINDRE ÄN 90%. DÅ BYGGER MAN DET SOM           
001600*               GÅR ATT BYGGA. DÄREFTER DELAR MAN ORDERN I 2 DELAW        
001700*               OCH GÖR DEN ENA "BYGGBAR" OCH DEN ANDRA "EJ BYGG-         
001800*               BAR". DEN "EJ BYGGBARA" LÄGGS TILLBAKS I SATS-            
001900*               ORDERKÖN.                                                 
002000*        FALL2: MAN HITTAR MER ÄN 90% ELLER ORDERN ÄR REDAN               
002100*               DELAD 9 GÅNGER. DÅ BYGGER MAN DET SOM GÅR                 
002200*               ATT BYGGA OCH ANNULLERAR RESTEN AV ORDERN.                
002300*        FALL3: EN INGÅENDE ARTIKEL SAKNAS HELT. DÅ GÖR MAN HELA          
002400*               ORDERN "EJ BYGGBAR" OCH LÄGGER TILLBAKS DEN I             
002500*               SATSORDERKÖN IGEN.                                        
002600*        TRANSAR TILL LAGERVÄRDE, AVBOKAT ANTAL OCH PLOCKFREKVENS         
002700*                                                                         
002800*        PROGRAMMET STARTAR UTSKRIFTSPROGRAMMEN W40372 OCH W40374.        
002900*        W40374 STARTAR I SIN TUR W40379, SOM UPPDATERAR WDE4 OCH         
003000*        WDE6.                                                            
003100*                                                                         
003200*        PROGRAMMET LÄSER      WDE4                                       
003300*        PROGRAMMET LÄSER      WDE6                                       
003400*        PROGRAMMET LÄSER      WLARTA (WDD1)                              
003500*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
003600*        PROGRAMMET UPPDATERAR WLSATG (WDJ2)                              
003700*        PROGRAMMET UPPDATERAR WLINLB (WDD9)                              
003800*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
003900*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
004000*                                                                         
004100*    INDATA.                                                              
004200*        TRANSAKTION: W4T304                                              
004300*        MID:         W4I30401                                            
004400*                                                                         
004500*    UTDATA.                                                              
004600*        MOD:         W4O30401                                            
004700                                                                          
004800     SKIP3                                                                
004900 ENVIRONMENT DIVISION.                                                    
005000     SKIP2                                                                
005100 CONFIGURATION SECTION.                                                   
005200*                                                                         
005300     EJECT                                                                
005400 DATA DIVISION.                                                           
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(08)   VALUE 'W4030400'.            
005900                                                                          
006000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006200                                                                          
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500                                                                          
006600*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
006700 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
006800 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
006900                                                                          
007000*    --- ARBETSFÄLT FÖR BERÄKNING AV SALDOFÖRÄNDRADE ART.                 
007100 77  WS-KVEFRS-DIFF              PIC S9(7)  COMP-3.                       
007200 77  WS-KVLS-DIFF                PIC S9(7)  COMP-3.                       
007300                                                                          
007400*    --- SPARFÄLT FÖR SALDOJÄMFÖRELSER.                                   
007500 77  SPAR-CLAG-KVEFRS            PIC S9(7)  COMP-3.                       
007600 77  SPAR-CLAG-KVLS              PIC S9(7)  COMP-3.                       
007700                                                                          
007800*    --- INDEX                                                            
007900 77  RAD-IDX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008000 77  RAD-IDX-MAX                 PIC S9(4)  VALUE +11   COMP SYNC.        
008100 77  TAB-IDX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008200 77  TAB-IDX-MAX                 PIC S9(4)  VALUE +33   COMP SYNC.        
008300 77  KOMBKOD-IDX                 PIC S9(4)  VALUE +0    COMP SYNC.        
008400 77  KOMBKOD-IDX-MAX             PIC S9(4)  VALUE +33   COMP SYNC.        
008500 77  KONTR-IDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
008600 77  KONTR-IDX-MAX               PIC S9(4)  VALUE +26   COMP SYNC.        
008700 77  BOKST-IDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
008800 77  BOKST-IDX-MAX               PIC S9(4)  VALUE +26   COMP SYNC.        
008900 77  AND-IDX                     PIC S9(4)  VALUE +0    COMP SYNC.        
009000 77  AND-IDX-MAX                 PIC S9(4)  VALUE +16   COMP SYNC.        
009100 77  JUST-IDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
009200 77  JUST-IDX-MAX                PIC S9(4)  VALUE +16   COMP SYNC.        
009300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +659  COMP SYNC.        
009400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
009500 77  WDK9-IDX                    PIC S9(9)  VALUE +0    COMP SYNC.        
009600                                                                          
009700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009800 77  FILLER                      PIC X(11)   VALUE 'WS-IDPRODNR'.         
009900 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
010000                                                                          
010100 77  WS-MID-REBEART-ALFA         PIC X(7)    VALUE SPACE.                 
010200                                                                          
010300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010400     88  INDATA-OK                           VALUE 'J'.                   
010500     88  INDATA-FEL                          VALUE 'N'.                   
010600                                                                          
010700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010800     88  NYCKLAR-OK                          VALUE 'J'.                   
010900     88  NYCKLAR-FEL                         VALUE 'N'.                   
011000                                                                          
011100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011200     88  ALLT-OK                             VALUE 'J'.                   
011300                                                                          
011400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011500     88  EGEN-MID                            VALUE '4304'.                
011600     88  GODK-MID                            VALUE '4304'.                
011700*                                                                         
011800 01  SWITCHAR.                                                            
011900     03  TAB-TRAFF-SW            PIC X       VALUE 'N'.                   
012000     03  AND-TRAFF-SW            PIC X       VALUE 'N'.                   
012100     03  TRAFF-EJ-BYGGB-SW       PIC X       VALUE 'N'.                   
012200     03  SKAPA-KONTR-TAB-SW      PIC X       VALUE 'N'.                   
012300     03  KDSATKMB-FUNNEN-SW      PIC X       VALUE 'N'.                   
012400*                                                                         
012500 77  MAX-ANTAL-DELNINGAR-SW      PIC X(1)    VALUE 'N'.                   
012600     88  MAX-ANTAL-DELNINGAR                 VALUE 'J'.                   
012700*                                                                         
012800*      --- VALID IDDD CODES                                               
012900*                                                                         
013000*01    -COPY WWDCKONS                                                     
013100 01  FILLER                      PIC X(10)   VALUE 'WS-AWBAREA'.          
013200 01  WS-AWBAREA.                                                          
013300     03  WS-DATUM                PIC 9(8).                                
013400     03  WS-TIME                 PIC X(8).                                
013500     03  WS-PROCSATS             PIC S9(3)V99 VALUE ZERO  COMP-3.         
013600     03  WS-KVBEART-BYGGB        PIC S9(7)   VALUE ZERO  COMP-3.          
013700     03  WS-KVBEART-BYGGB-KOMB   PIC S9(7)   VALUE ZERO  COMP-3.          
013800     03  WS-KVBEART-BYGGB-KOMB-TOT PIC S9(7) VALUE ZERO  COMP-3.          
013900     03  WS-MID-REBEART-NUM      PIC S9(7)   VALUE ZERO  COMP-3.          
014000     03  WS-DISPLS               PIC S9(7)   VALUE ZERO  COMP-3.          
014100     03  WS-AVRUND               PIC S9(7)   VALUE ZERO  COMP-3.          
014200     03  WS-KVANNANT             PIC S9(7)   VALUE ZERO  COMP-3.          
014300     03  WS-KVAVART              PIC S9(7)   VALUE ZERO  COMP-3.          
014400     03  WS-TIUTSKR              PIC S9(7)   VALUE ZERO  COMP-3.          
014500     03  W-ORIG-REBEART          PIC S9(7)   VALUE ZERO  COMP-3.          
014600     03  W-ORIG-KVSATRES         PIC S9(7)   VALUE ZERO  COMP-3.          
014700     03  W-ORIG-KVSATROS         PIC S9(7)   VALUE ZERO  COMP-3.          
014800     03  WS-JAMFOR-REBEART       PIC S9(7)   VALUE ZERO  COMP-3.          
014900     03  WS-SHUV-KDSATKMB        PIC X       VALUE SPACE.                 
015000     03  WS-KDSATKMB             PIC X       VALUE SPACE.                 
015100     03  WS-KDSATKMB-NY          PIC X       VALUE SPACE.                 
015200     03  WS-RO-ANTAL             PIC S9(9)   VALUE ZERO  COMP-3.          
015300     03  W1-KVRADER              PIC S9(3)   VALUE ZERO  COMP-3.          
015400     03  W1-VLORDNTO             PIC S9(8)V9(1) VALUE ZERO COMP-3.        
015500     03  W1-VKORDNTO             PIC S9(4)V9(3) VALUE ZERO COMP-3.        
015600     03  W-KVBYGGB               PIC S9(7)      VALUE ZERO COMP-3.        
015700     03  W-KVANNANT              PIC S9(7)      VALUE ZERO COMP-3.        
015800     03  W-MAX-KVBYGGB           PIC S9(7)      VALUE ZERO COMP-3.        
015900     03  W-MAX-KVBYGGB-REST      PIC S9(7)      VALUE ZERO COMP-3.        
016000     03  W-HELP-REBEART          PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016100     03  W-SPAR-IDORDNSS         PIC S9(1)      VALUE ZERO COMP-3.        
016200     03  WS-KDCLAGER             PIC S9(1)      VALUE ZERO COMP-3.        
016300     03  WS-IDDISTR-NUM4         PIC S9(4).                               
016400     03  WS-IDKUNDNR-NUM6        PIC S9(6).                               
016500     03  WS-IDANSK               PIC S9(3)  COMP-3.                       
016600     03  WS-SATG-SHUV-BEFT       PIC S9(3)      VALUE ZERO.               
016700*                                                                         
016800     03 WS-IDORDNST.                                                      
016900       05 WS-IDORDNSB            PIC  9(4)   VALUE ZERO.                  
017000       05 WS-IDORDNSS            PIC  9(1)   VALUE ZERO.                  
017100     03 WS-IDORDNST-NUM REDEFINES WS-IDORDNST  PIC 9(5).                  
017200*                                                                         
017300 01  FILLER                      PIC X(10)   VALUE 'WS-TOTALER'.          
017400 01  WS-TOTALER.                                                          
017500     03  WS-TAB-REBEART          PIC S9(7)   VALUE ZERO  COMP-3.          
017600     03  WS-TAB-MID-REBEART      PIC 9(7)    VALUE ZERO.                  
017700     03  WS-TAB-REANTPSA         PIC S99V999 VALUE ZERO  COMP-3.          
017800*                                                                         
017900 01  FILLER                      PIC X(11)   VALUE 'BILD-TABELL'.         
018000 01  BILD-TABELL.                                                         
018100     03  BILD-TAB           OCCURS 33.                                    
018200         05  TAB-ANV             PIC X.                                   
018300         05  TAB-MID-IDPURAD     PIC 9(4).                                
018400         05  TAB-MID-REBEART     PIC 9(7).                                
018500         05  TAB-IDARTNR         PIC S9(9)   COMP-3.                      
018600         05  TAB-KDSATKMB        PIC X.                                   
018700         05  TAB-KDSATAND        PIC X.                                   
018800         05  TAB-KVSATRES        PIC S9(7)   COMP-3.                      
018900         05  TAB-REBEART         PIC S9(7)   COMP-3.                      
019000         05  TAB-REANTPSA        PIC S99V999 COMP-3.                      
019100         05  TAB-W2-REBEART      PIC S9(7)   COMP-3.                      
019200         05  TAB-NY-REBEART      PIC S9(7)   COMP-3.                      
019300         05  TAB-INL-ANTAL       PIC S9(7)   COMP-3.                      
019400*                                                                         
019500 01  FILLER                      PIC X(10)   VALUE 'AND-TABELL'.          
019600 01  AND-TABELL.                                                          
019700     03  AND-TAB           OCCURS 16.                                     
019800         05  AND-JUST            PIC X.                                   
019900         05  AND-BRIST           PIC X.                                   
020000         05  AND-MID-IDPURAD     PIC 9(4).                                
020100         05  AND-MID-REBEART     PIC 9(7).                                
020200         05  AND-IDARTNR         PIC S9(9)   COMP-3.                      
020300         05  AND-KDSATKMB        PIC X.                                   
020400         05  AND-KDSATAND        PIC X.                                   
020500         05  AND-KVSATRES        PIC S9(7)   COMP-3.                      
020600         05  AND-REBEART         PIC S9(7)   COMP-3.                      
020700         05  AND-REANTPSA        PIC S99V999 COMP-3.                      
020800         05  AND-KVBYGGB         PIC S9(7)V99 COMP-3.                     
020900         05  AND-NY-REBEART      PIC S9(7)   COMP-3.                      
021000         05  AND-INL-ANTAL       PIC S9(7)   COMP-3.                      
021100         05  AND-W2-REBEART      PIC S9(7)   COMP-3.                      
021200*                                                                         
021300 01  FILLER                      PIC X(12)   VALUE 'BOKSTAVS-TAB'.        
021400 01  WS-KDSATKMB-ALLA.                                                    
021500     03  FILLER                  PIC X(12)   VALUE 'ABCDEFGHIJKL'.        
021600     03  FILLER                  PIC X(12)   VALUE 'MNOPQRSTUVWX'.        
021700     03  FILLER                  PIC X(2)    VALUE 'YZ'.                  
021800 01  FILLER REDEFINES WS-KDSATKMB-ALLA.                                   
021900     03  BOKST-BOKSTAVSRAD       OCCURS 26.                               
022000         05  BOKST-KDSATKMB      PIC X.                                   
022100*                                                                         
022200 01  FILLER                      PIC X(12)   VALUE 'KONTROLL-TAB'.        
022300 01  KONTROLL-TAB.                                                        
022400     03  KONTR-TAB-RAD      OCCURS 26.                                    
022500         05  KONTR-KDSATKMB      PIC X.                                   
022600         05  KONTR-ANV           PIC X.                                   
022700*                                                                         
022800 01  P-TO-P-SW.                                                           
022900     03  PTOP-LL                 PIC S9(4)   VALUE 23 COMP SYNC.          
023000     03  PTOP-Z1                 PIC X       VALUE LOW-VALUE.             
023100     03  PTOP-Z2                 PIC X       VALUE LOW-VALUE.             
023200     03  PTOP-TRANSKOD           PIC X(7)    VALUE SPACE.                 
023300     03  FILLER                  PIC X       VALUE SPACE.                 
023400     03  PTOP-BILDNR             PIC X(4)    VALUE '4304'.                
023500     03  PTOP-KDMFSFOR           PIC X.                                   
023600     03  PTOP-IDORDNSB           PIC X(5).                                
023700     03  PTOP-IDORDNSS           PIC X.                                   
023800*                                                                         
023900 77  FILLER                      PIC X(13)  VALUE 'W-BYGGB-IDORD'.        
024000 01  W-BYGGB-IDORDNST.                                                    
024100   05  W-BYGGB-IDORDNSB          PIC S9(4)   VALUE ZERO COMP-3.           
024200   05  W-BYGGB-IDORDNSS          PIC S9(1)   VALUE ZERO COMP-3.           
024300                                                                          
024400 01  W-EJ-BYGGB-IDORDNST.                                                 
024500   05  W-EJ-BYGGB-IDORDNSB       PIC S9(4)   VALUE ZERO COMP-3.           
024600   05  W-EJ-BYGGB-IDORDNSS       PIC S9(1)   VALUE ZERO COMP-3.           
024700                                                                          
024800 77  FILLER                      PIC X(13)  VALUE 'W-RED-IDORDNS'.        
024900 01  W-RED-IDORDNST.                                                      
025000   05  W-RED-IDORDNSB            PIC S9(4)   VALUE ZERO.                  
025100   05  W-RED-IDORDNSS            PIC S9(1)   VALUE ZERO.                  
025200                                                                          
025300*                                                                         
025400*   ARBETSAREOR FÖR SKAPANDE AV NYA ORDERHUVUD OCH ORDERRRADER            
025500*   W1 ANVÄNDS FÖR BYGGBARA ORDER OCH W2 FÖR EJ BYGGBARA                  
025600*01  -COPY WDJ201       -PRE W1-                                          
025700*                                                                         
025800     EJECT                                                                
025900*01  -COPY WDJ211       -PRE W1-                                          
026000*                                                                         
026100     EJECT                                                                
026200*01  -COPY WDJ201       -PRE W2-                                          
026300*                                                                         
026400     EJECT                                                                
026500*01  -COPY WDJ211       -PRE W2-                                          
026600*                                                                         
026700     EJECT                                                                
026800*01  -COPY WDJ201       -PRE SATG1-                                       
026900*                                                                         
027000     EJECT                                                                
027100*01  -COPY WDJ211       -PRE SATG1-                                       
027200*                                                                         
027300     EJECT                                                                
027400*01  -COPY WDGZRYJ      -PRE W-                                           
027500*                                                                         
027600     EJECT                                                                
027700*                                                                         
027800*    ---  COPY-AREOR TILL SUBPGM                                          
027900 01  DATUMKORT-ID                PIC X(6)   VALUE 'WDATUM'.               
028000*01  -COPY WDATKORT                                                       
028100*                                                                         
028200*                                                                         
028300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
028400 01  GENERELLA-SUBPROGRAM.                                                
028500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
028600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
028900     03  W416PTID                PIC X(8)    VALUE 'W416PTID'.            
029000     03  W215LEVP                PIC X(8)    VALUE 'W215LEVP'.            
029100*            RÄKNA OM VALUTA DDI                                          
029200     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
029300                                                                          
029400     EJECT                                                                
029500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
029600*   -COPY WMEDAREA                                                        
029700*                                                                         
029800     SKIP3                                                                
029900 01  MESSAGE-CODES.                                                       
030000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
030100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
030200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
030300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
030400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
030500                                                                          
030600 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
030700     EJECT                                                                
030800*01  -COPY W416PTID                                                       
030900     EJECT                                                                
031000*01  -COPY W215LEVP                                                       
031100     EJECT                                                                
031200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
031300*                                                                         
031400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
031500     SKIP3                                                                
031600*01  MID -COPY W4I30401                                                   
031700     EJECT                                                                
031800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
031900     SKIP3                                                                
032000*01  -COPY WMSGAREA                                                       
032100     EJECT                                                                
032200     03  MOD REDEFINES MSG-AREA.                                          
032300*      05  -COPY W4O30401                                                 
032400     EJECT                                                                
032500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
032600     SKIP3                                                                
032700*01  -COPY WMFSAREA                                                       
032800     EJECT                                                                
032900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
033000*                                                                         
033100 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DL1'.        
033200 01  NYCKLAR-TILL-DLI.                                                    
033300                                                                          
033400     03  W-WDE4BSEQ-X.                                                    
033500         05  W-WDE4-IDPRODNR     PIC S9(7)   VALUE ZERO COMP-3.           
033600         05  W-WDE4-IDPURAD      PIC S9(5)   VALUE ZERO COMP-3.           
033700                                                                          
033800     03  W-IDPRODNR-X.                                                    
033900         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
034000                                                                          
034100     03  W-IDARTNR-X.                                                     
034200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
034300                                                                          
034400     03  W-IDSKYLT-X.                                                     
034500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
034600                                                                          
034700     03  W-IDORDNST-X.                                                    
034800         05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
034900         05  W-IDORDNSS          PIC S9(1)   VALUE ZERO COMP-3.           
035000                                                                          
035100     03  W-IDORDNST-MIN-X.                                                
035200         05  W-IDORDNSB-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
035300         05  W-IDORDNSS-MIN      PIC S9(1)   VALUE ZERO COMP-3.           
035400                                                                          
035500     03  W-IDLEVNR-X             PIC  X(5)   VALUE SPACE.                 
035600                                                                          
036000     03  W-IDLOPNRM-X.                                                    
036100         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
036200                                                                          
036300     03  W-KDSEGKEY-X.                                                    
036400         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
036500                                                                          
036600     03  W-KDSATKMB-X.                                                    
036700         05  W-KDSATKMB          PIC X       VALUE SPACE.                 
036800*                                                                         
036900*----> PRIORITETSSTYRNING FÖR RESTORDER                                   
037000*                                                                         
037100     03  W-4511-IDHTYP-X.                                                 
037200         05  W-4511-IDHTYP       PIC  X(04) VALUE '4511'.                 
037300         05  W-LOW-VALUE         PIC  X(26) VALUE LOW-VALUE.              
037400                                                                          
037500     03  W-4512-KDTPOTYP-X.                                               
037600         05  W-4512-KDTPOTYP     PIC  S9(1) COMP-3.                       
037700                                                                          
037800     03  W-4512-KDORDKL-X.                                                
037900         05  W-4512-KDORDKL      PIC  S9(1) COMP-3.                       
038000                                                                          
038100     03  W-4512-IDDISTR-FOM-X.                                            
038200         05  W-4512-IDDISTR-FOM  PIC  S9(5) COMP-3.                       
038300                                                                          
038400     03  W-4512-IDDISTR-TOM-X.                                            
038500         05  W-4512-IDDISTR-TOM  PIC  S9(5) COMP-3.                       
038600*                                                                         
038700 01  FILLER                    PIC X(16) VALUE 'ALT2191-IO-AREA'.         
038800 01  ALT2191-IO-AREA.                                                     
038900  03     ALT2191-LL               PIC S9(4) COMP SYNC.                    
039000  03     ALT2191-Z1               PIC X(1)  VALUE LOW-VALUE.              
039100  03     ALT2191-Z2               PIC X(1)  VALUE LOW-VALUE.              
039200  03     ALT2191-TRANSKOD         PIC X(8)  VALUE 'W2T191X '.             
039300  03     ALT2191-IDTRANS          PIC X(4)  VALUE '4354'.                 
039400  03     ALT2191-SPRAK            PIC X(1).                               
039500* 03     MID -COPY W2I19101   -PRE ALT2191-                               
039600     EJECT                                                                
039700*    --- STATUS-KOD FRÅN IMS                                              
039800 01  FILLER                      PIC X(16) VALUE 'STATUS-WS'.             
039900 01  STATUS-WS                   PIC XX.                                  
040000     88  SEGMENT-FINNS                       VALUE '  '.                  
040100     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
040200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
040300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
040400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
040500     SKIP2                                                                
040600 01  GODK-STATUSKODER.                                                    
040700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040800     SKIP3                                                                
040900 01  SSA1                        PIC X(96).                               
041000 01  SSA2                        PIC X(64).                               
041100 01  SSA3                        PIC X(96).                               
041200     EJECT                                                                
041300*    --- IMS FUNKTIONSKODER                                               
041400*01  -COPY W0003                                                          
041500*                                                                         
041600     EJECT                                                                
041700*    ---  DLI INPUT-OUTPUT AREA                                           
041800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
041900     SKIP3                                                                
042000 01  DLI-IO-AREA.                                                         
042100     03  WDE411.                                                          
042200*        05  -COPY WDE411     -PRE WDE4-                                  
042300*                                                                         
042400     03  WDE401.                                                          
042500*        05  -COPY WDE401     -PRE WDE4-                                  
042600     EJECT                                                                
042700 01  FILLER                      PIC X(17)  VALUE 'DLI-IO-AREA1'.         
042800 01  DLI-IO-AREA1.                                                        
042900     03  IO-AREA1              PIC X(135).                                
043000     03  WLSATG01 REDEFINES IO-AREA1.                                     
043100*        05  -COPY WDJ201     -PRE SATG-                                  
043200     EJECT                                                                
043300*                                                                         
043400 01  FILLER                      PIC X(17)  VALUE 'DLI-IO-AREA2'.         
043500 01  DLI-IO-AREA2.                                                        
043600     03  WLSATG11.                                                        
043700*        05  -COPY WDJ211     -PRE SATG-                                  
043800*                                                                         
043900     EJECT                                                                
044000 01  FILLER                      PIC X(17)  VALUE 'DLI-IO-AREA3'.         
044100 01  DLI-IO-AREA3.                                                        
044200     03  WLSATG12.                                                        
044300*        05  -COPY WDJ212     -PRE SATG-                                  
044400*                                                                         
044500     EJECT                                                                
044600 01  FILLER                      PIC X(17)  VALUE 'DLI-IO-AREA4'.         
044700 01  DLI-IO-AREA4.                                                        
044800     03  IO-AREA4              PIC X(900).                                
044900     SKIP3                                                                
045000     03  WLARTC01 REDEFINES IO-AREA4.                                     
045100*        05  -COPY WDK601                                                 
045200     SKIP3                                                                
045300     03  WLARTC11 REDEFINES IO-AREA4.                                     
045400*        05  -COPY WDK611                                                 
045500     SKIP3                                                                
045600     03  WLORDP01 REDEFINES IO-AREA4.                                     
045700*        05  -COPY WDA501     -PRE ORDP-                                  
045800 01  FILLER                      PIC X(17)  VALUE 'DLI-IO-AREA7'.         
045900 01  DLI-IO-AREA7.                                                        
046000     03  IO-AREA7              PIC X(120).                                
046100     SKIP3                                                                
046200     03  WLBENA11 REDEFINES IO-AREA7.                                     
046300*        05  -COPY WDD311     -PRE BENA-                                  
046400     SKIP3                                                                
046500     EJECT                                                                
046600 01  FILLER                      PIC X(17)  VALUE 'DLI-IO-AREA5'.         
046700 01  DLI-IO-AREA5.                                                        
046800     03  IO-AREA5              PIC X(288).                                
046900     03  WDE601 REDEFINES IO-AREA5.                                       
047000*        05  -COPY WDE601     -PRE WDE6-                                  
047100     EJECT                                                                
047200 01  FILLER                      PIC X(17)  VALUE 'DLI-IO-AREA6'.         
047300 01  DLI-IO-AREA6.                                                        
047400     03  IO-AREA6              PIC X(192).                                
047500     03  WLARTM01 REDEFINES IO-AREA6.                                     
047600*        05  -COPY WDK901     -PRE ARTM-                                  
047700     EJECT                                                                
047800 01  FILLER                      PIC X(10)  VALUE 'IO-AREA-TR'.           
047900 01  DLI-IO-AREA-TR.                                                      
048000     03  IO-AREA-TR            PIC X(156).                                
048100     03  WLZZAC01 REDEFINES IO-AREA-TR.                                   
048200*        05  -COPY WDGZ01     -PRE ZZAC-                                  
048300     EJECT                                                                
048400 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLLOGA01'.               
048500 01  DLI-IO-WLLOGA01.                                                     
048600*    03  WLLOGA01  -COPY WDL901                                           
048700     EJECT                                                                
048800     SKIP3                                                                
048900 01  FILLER                      PIC X(11)  VALUE 'IO-AREA-SUB'.          
049000 01  DLI-IO-AREA-SUB.                                                     
049100     03  IO-AREA-SUB           PIC X(64).                                 
049200     03  WLXXJN11 REDEFINES IO-AREA-SUB.                                  
049300*        05  -COPY WDGX4512                                               
049400     EJECT                                                                
049500 LINKAGE SECTION.                                                         
049600                                                                          
049700*01  -COPY W0009      -PRE MSG-                                           
049800     EJECT                                                                
049900*01  -COPY W0009      -PRE ALT1-                                          
050000     EJECT                                                                
050100*01  -COPY W0009      -PRE ALT2-                                          
050200     EJECT                                                                
050300*01  -COPY W0009      -PRE ALT2191-                                       
050400     EJECT                                                                
050500*01  -COPY W0008      -PRE WDE4-                                          
050600     05  FILLER                  PIC X.                                   
050700     EJECT                                                                
050800*01  -COPY W0008      -PRE WDE6-                                          
050900     05  FILLER                  PIC X.                                   
051000     EJECT                                                                
051100*01  -COPY W0008      -PRE SATG-                                          
051200     05  FILLER                  PIC X.                                   
051300     EJECT                                                                
051400*01  -COPY W0008      -PRE SATG1-                                         
051500     05  FILLER                  PIC X.                                   
051600     EJECT                                                                
051700*01  -COPY W0008      -PRE ARTC-                                          
051800     05  FILLER                  PIC X.                                   
051900     EJECT                                                                
052000*01  -COPY W0008      -PRE BENA-                                          
052100     05  FILLER                  PIC X.                                   
052200     EJECT                                                                
052300*01  -COPY W0008      -PRE ORDP-                                          
052400     05  FILLER                  PIC X.                                   
052500     EJECT                                                                
052600*01  -COPY W0008      -PRE ARTM-                                          
052700     05  FILLER                  PIC X.                                   
052800     EJECT                                                                
052900*01  -COPY W0008      -PRE XXJN-                                          
053000     05  FILLER                  PIC X.                                   
053100     EJECT                                                                
053200*01  -COPY W0008      -PRE ZZAC-                                          
053300     05  FILLER                  PIC X.                                   
053400     EJECT                                                                
053500*01  -COPY W0008      -PRE WLLOGA-                                        
053600     05  FILLER                  PIC X.                                   
053700     EJECT                                                                
053800***  PCB FÖR SUB PGM W416PTID                                             
053900                                                                          
054000 01  XXKH-PCB                    PIC X.                                   
054100                                                                          
054200 01  XXKI-PCB                    PIC X.                                   
054300                                                                          
054400 01  SATB-PCB                    PIC X.                                   
054500     EJECT                                                                
054600***  PCB FÖR SUB PGM W215LEVP                                             
054700                                                                          
054800 01  ARTC2-PCB                   PIC X.                                   
054900                                                                          
055000 01  INLB1-PCB                   PIC X.                                   
055100                                                                          
055200 01  INLB2-PCB                   PIC X.                                   
055300                                                                          
055400 01  XXBM-PCB                    PIC X.                                   
055500                                                                          
055600 01  XXBW-PCB                    PIC X.                                   
055700     EJECT                                                                
055800 PROCEDURE DIVISION  USING MSG-PCB                                        
055900                           ALT1-PCB                                       
056000                           ALT2-PCB                                       
056100                           ALT2191-PCB                                    
056200                           WDE4-PCB                                       
056300                           WDE6-PCB                                       
056400                           SATG-PCB                                       
056500                           SATG1-PCB                                      
056600                           ARTC-PCB                                       
056700                           BENA-PCB                                       
056800                           ORDP-PCB                                       
056900                           ARTM-PCB                                       
057000                           XXJN-PCB                                       
057100                           ZZAC-PCB                                       
057200                           WLLOGA-PCB                                     
057300                           XXKH-PCB                                       
057400                           XXKI-PCB                                       
057500                           SATB-PCB                                       
057600                           ARTC2-PCB INLB1-PCB INLB2-PCB                  
057700                           XXBM-PCB XXBW-PCB.                             
057800     ENTRY 'DLITCBL' USING MSG-PCB                                        
057900                           ALT1-PCB                                       
058000                           ALT2-PCB                                       
058100                           ALT2191-PCB                                    
058200                           WDE4-PCB                                       
058300                           WDE6-PCB                                       
058400                           SATG-PCB                                       
058500                           SATG1-PCB                                      
058600                           ARTC-PCB                                       
058700                           BENA-PCB                                       
058800                           ORDP-PCB                                       
058900                           ARTM-PCB                                       
059000                           XXJN-PCB                                       
059100                           ZZAC-PCB                                       
059200                           WLLOGA-PCB                                     
059300                           XXKH-PCB                                       
059400                           XXKI-PCB                                       
059500                           SATB-PCB                                       
059600                           ARTC2-PCB INLB1-PCB INLB2-PCB                  
059700                           XXBM-PCB XXBW-PCB.                             
059800                                                                          
059900*------------------------                                                 
060000     PERFORM IMS-GET-MSG                                                  
060100     IF SEGMENT-FINNS                                                     
060200       PERFORM A-INIT                                                     
060300       PERFORM B-KOLLA-NYCKLAR                                            
060400       IF NYCKLAR-OK                                                      
060500         IF MFS-UPDATE                                                    
060600           PERFORM G-KOLLA-INPUT                                          
060700           IF INDATA-OK                                                   
060800             PERFORM H-UPPDATERA                                          
060900           END-IF                                                         
061000         END-IF                                                           
061100         IF ALLT-OK                                                       
061200            PERFORM F-LAES-VISA-INFO                                      
061300         END-IF                                                           
061400       END-IF                                                             
061500       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
061600       PERFORM IMS-INSERT-MSG                                             
061700     END-IF                                                               
061800                                                                          
061900     MOVE ZERO TO RETURN-CODE                                             
062000     GOBACK                                                               
062100     .                                                                    
062200     EJECT                                                                
062300****************************************************************          
062400*  A-INIT                                                      *          
062500*  INITIERINGSRUTIN                                            *          
062600****************************************************************          
062700 A-INIT SECTION.                                                          
062800                                                                          
062900     IF MSG-DUBBLA-TRANSKODER                                             
063000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I30401                 
063100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
063200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
063300                              WS-KDCLAGER                                 
063400     ELSE                                                                 
063500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I30401                  
063600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
063700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
063800     END-IF                                                               
063900                                                                          
064000     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
064100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
064200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
064300                                                                          
064400     MOVE LOW-VALUE TO MSG-AREA                                           
064500     MOVE 'W4O304N1' TO MFS-IDMOD                                         
064600     MOVE '4304' TO MOD-IDTRANS                                           
064700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
064800                                                                          
064900     IF NOT EGEN-MID                                                      
065000       MOVE SPACE TO MFS-KDTRTYP                                          
065100       MOVE '7' TO MFS-IDPFK                                              
065200     END-IF                                                               
065300                                                                          
065400     IF ENGLISH-TEXT                                                      
065500       MOVE +2 TO SPRAK-IX                                                
065600       MOVE 'GB ' TO MED-IDSKYLT                                          
065700     ELSE                                                                 
065800       MOVE +1 TO SPRAK-IX                                                
065900       MOVE 'S  ' TO MED-IDSKYLT                                          
066000     END-IF                                                               
066100                                                                          
066200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
066300     ACCEPT WS-TIME  FROM TIME                                            
066400     .                                                                    
066500     EJECT                                                                
066600****************************************************************          
066700*  B-KOLLA-NYCKLAR                                             *          
066800*  KONTROLLERA OM NYCKLAR IFYLLDA, SAMT ATT DOM ÄR OK          *          
066900****************************************************************          
067000 B-KOLLA-NYCKLAR SECTION.                                                 
067100                                                                          
067200     MOVE JA TO NYCKLAR-SW                                                
067300                                                                          
067400     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-IN                              
067500                                                                          
067600     IF MID-IDPRODNR-IN = ALL '+'                                         
067700         MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                              
067800         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
067900     ELSE                                                                 
068000         MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                              
068100         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
068300         MOVE '7'         TO MFS-IDPFK                                    
068400         MOVE SPACE       TO MFS-KDTRTYP                                  
068500     END-IF                                                               
068600                                                                          
068700     IF WS-IDPRODNR NUMERIC                                               
068800       IF WS-IDPRODNR > ZERO                                              
068900         MOVE WS-IDPRODNR TO W-IDPRODNR                                   
069000       ELSE                                                               
069100         MOVE NEJ TO NYCKLAR-SW                                           
069200       END-IF                                                             
069300     ELSE                                                                 
069400       MOVE NEJ TO NYCKLAR-SW                                             
069500       MOVE ZERO          TO WS-IDPRODNR                                  
069600     END-IF                                                               
069700                                                                          
069800     IF GODK-MID OR NYCKLAR-OK                                            
069900       MOVE WS-IDPRODNR TO MOD-IDPRODNR-UT                                
070000       INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE            
070100     ELSE                                                                 
070200       MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                            
070300     END-IF                                                               
070400                                                                          
070500     IF NYCKLAR-FEL                                                       
070600        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
070700        CALL WMEDKONV USING MED-WMEDAREA                                  
070800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
070900        PERFORM MFS-RENSA-FAELT-IN                                        
071000        PERFORM MFS-RENSA-FAELT-UT                                        
071100     END-IF                                                               
071200     .                                                                    
071300     EJECT                                                                
071400****************************************************************          
071500*  F-LAES-VISA-INFO                                            *          
071600*  LÄGGER UT INFORMATION I HUVUDET PÅ BILDEN                   *          
071700****************************************************************          
071800 F-LAES-VISA-INFO SECTION.                                                
071900                                                                          
072000     PERFORM FA-LAES-GRUNDDATA                                            
072100                                                                          
072200     PERFORM MFS-RENSA-FAELT-IN                                           
072300     IF MED-IDMFSINF = '101' OR                                           
072400        MED-IDMFSINF = '005' OR                                           
072500        MED-IDMFSINF = '718'                                              
072600        PERFORM MFS-ROER-EJ-FAELT-IN                                      
072700        PERFORM MFS-ROER-EJ-FAELT-UT                                      
072800     ELSE                                                                 
072900        MOVE MFS-ADD-SAETT-CURSOR                                         
073000                                 TO MOD-ATTR-IDPURAD1(1)                  
073100     END-IF                                                               
073200     .                                                                    
073300     EJECT                                                                
073400****************************************************************          
073500*  FA-LAES-GRUNDDATA                                           *          
073600*  KONTROLLERAR WDE6 FÖR ATT FÖRHINDRA AVVIKELSERAPPORTERING   *          
073700*  OM PACKRAPPORTERING SKETT.                                  *          
073800*  LÄSER DATABASER FÖR ATT FÅ UT INFORMATION I HUVUDET         *          
073900*  PÅ BILDEN                                                   *          
074000****************************************************************          
074100 FA-LAES-GRUNDDATA SECTION.                                               
074200                                                                          
074300     MOVE W-IDPRODNR             TO W-WDE4-IDPRODNR                       
074400     MOVE 1                      TO W-WDE4-IDPURAD                        
074500     PERFORM IMS-GU-WDE4-PATH                                             
074600                                                                          
074700     IF SEGMENT-FINNS                                                     
074800        MOVE WDE4-KORD-IDORDNR5  TO W-RED-IDORDNST                        
074900        MOVE W-RED-IDORDNSB      TO W-IDORDNSB                            
075000        MOVE W-RED-IDORDNSS      TO W-IDORDNSS                            
075100                                                                          
075200        PERFORM IMS-GU-WDE6                                               
075300        IF SEGMENT-FINNS                                                  
075400           IF WDE6-VORD-KDORDSTA > 1                                      
075500              MOVE '718'         TO MED-IDMFSINF                          
075600              CALL WMEDKONV USING MED-WMEDAREA                            
075700              MOVE MED-MFSINF    TO MOD-TEMFSINF                          
075800              PERFORM MFS-STAENG-FAELT-NOMOD-IN                           
075900           ELSE                                                           
076000              PERFORM IMS-GU-WDJ201-SATG                                  
076100              IF SEGMENT-FINNS                                            
076200                 MOVE SATG-SHUV-IDARTNR TO MOD-IDARTNR                    
076300                 MOVE SATG-SHUV-KVBEART TO MOD-KVBEART                    
076400                                                                          
076500                 MOVE SATG-SHUV-IDARTNR TO W-IDARTNR                      
076600                 MOVE MED-IDSKYLT TO W-IDSKYLT                            
076700                 PERFORM IMS-GU-BENA11                                    
076800                 IF SEGMENT-FINNS                                         
076900                    MOVE BENA-TEXT-BEART TO MOD-BEART                     
077000                 ELSE                                                     
077100                    MOVE SPACE   TO MOD-BEART                             
077200                 END-IF                                                   
077300              ELSE                                                        
077400                 MOVE '005'      TO MED-IDMFSINF                          
077500                 CALL WMEDKONV USING MED-WMEDAREA                         
077600                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
077700                 PERFORM MFS-STAENG-FAELT-NOMOD-IN                        
077800              END-IF                                                      
077900           END-IF                                                         
078000        ELSE                                                              
078100           MOVE '005'            TO MED-IDMFSINF                          
078200           CALL WMEDKONV USING MED-WMEDAREA                               
078300           MOVE MED-MFSINF       TO MOD-TEMFSINF                          
078400           PERFORM MFS-STAENG-FAELT-NOMOD-IN                              
078500        END-IF                                                            
078600     ELSE                                                                 
078700        MOVE '005'               TO MED-IDMFSINF                          
078800        CALL WMEDKONV USING MED-WMEDAREA                                  
078900        MOVE MED-MFSINF          TO MOD-TEMFSINF                          
079000        PERFORM MFS-STAENG-FAELT-NOMOD-IN                                 
079100     END-IF                                                               
079200     .                                                                    
079300     EJECT                                                                
079400****************************************************************          
079500*  G-KOLLA-INPUT                                               *          
079600*  KONTROLLERAR INMATADE OCCURS-RADER, SÅ ATT DOM ÄR OK        *          
079700*  IFYLLDA                                                     *          
079800****************************************************************          
079900 G-KOLLA-INPUT SECTION.                                                   
080000                                                                          
080100     MOVE JA  TO INDATA-SW                                                
080200     IF MID-INPUT = ALL '+'                                               
080300*  PF11 OCH TOM INDATARAD                                                 
080400        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
080500        CALL WMEDKONV USING MED-WMEDAREA                                  
080600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
080700        PERFORM MFS-ROER-EJ-FAELT-IN                                      
080800        PERFORM MFS-ROER-EJ-FAELT-UT                                      
080900        MOVE NEJ TO INDATA-SW                                             
081000     ELSE                                                                 
081100                                                                          
081200*LÄS IN DATABASSEGMENT FÖR ATT KOLLA INMATNINGSFÄLT                       
081300        PERFORM GA-KOLLA-OCCURS-RADER                                     
081400        IF INDATA-FEL                                                     
081500           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
081600           CALL WMEDKONV USING MED-WMEDAREA                               
081700           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
081800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
081900           PERFORM MFS-ROER-EJ-FAELT-IN                                   
082000        END-IF                                                            
082100     END-IF                                                               
082200                                                                          
082300     .                                                                    
082400     EJECT                                                                
082500****************************************************************          
082600*  GA-KOLLA-OCCURS-RADER                                       *          
082700*  KONTROLLERAR INMATADE OCCURS-RADER, SÅ ATT DOM ÄR OK        *          
082800*  IFYLLDA                                                     *          
082900****************************************************************          
083000 GA-KOLLA-OCCURS-RADER SECTION.                                           
083100                                                                          
083200     MOVE W-IDPRODNR             TO W-WDE4-IDPRODNR                       
083300     MOVE +1                     TO RAD-IDX                               
083400                                                                          
083500     PERFORM UNTIL RAD-IDX > 11                                           
083600                                                                          
083700       IF MID-IDPURAD1(RAD-IDX) NOT = ALL '+'                             
083800          MOVE MID-IDPURAD1(RAD-IDX) TO W-WDE4-IDPURAD                    
083900          PERFORM  IMS-GU-WDE4-PATH                                       
084000          IF SEGMENT-FINNS                                                
084100             MOVE MFS-NUM-FAELT-RAETT                                     
084200                                 TO MOD-ATTR-IDPURAD1(RAD-IDX)            
084300          ELSE                                                            
084400             MOVE MFS-NUM-FAELT-FEL                                       
084500                                 TO MOD-ATTR-IDPURAD1(RAD-IDX)            
084600             MOVE NEJ            TO INDATA-SW                             
084700          END-IF                                                          
084800          IF SEGMENT-FINNS                                                
084900          IF MID-REBEART1(RAD-IDX) NOT = ALL '+'                          
085000             IF MID-REBEART1(RAD-IDX) NUMERIC                             
085100                MOVE MID-REBEART1(RAD-IDX) TO WS-MID-REBEART-NUM          
085200                IF WS-MID-REBEART-NUM <= WDE4-ORAD-KVBEART                
085300                   MOVE MFS-NUM-FAELT-RAETT                               
085400                                 TO MOD-ATTR-REBEART1(RAD-IDX)            
085500                ELSE                                                      
085600                   MOVE MFS-NUM-FAELT-FEL                                 
085700                                 TO MOD-ATTR-REBEART1(RAD-IDX)            
085800                   MOVE NEJ TO INDATA-SW                                  
085900                END-IF                                                    
086000             ELSE                                                         
086100                MOVE MFS-NUM-FAELT-FEL                                    
086200                                 TO MOD-ATTR-REBEART1(RAD-IDX)            
086300                MOVE NEJ TO INDATA-SW                                     
086400             END-IF                                                       
086500          ELSE                                                            
086600             MOVE MFS-NUM-FAELT-FEL                                       
086700                                 TO MOD-ATTR-REBEART1(RAD-IDX)            
086800             MOVE NEJ TO INDATA-SW                                        
086900          END-IF                                                          
087000          END-IF                                                          
087100       END-IF                                                             
087200                                                                          
087300       IF MID-IDPURAD2(RAD-IDX) NOT = ALL '+'                             
087400          MOVE MID-IDPURAD2(RAD-IDX) TO W-WDE4-IDPURAD                    
087500          PERFORM  IMS-GU-WDE4-PATH                                       
087600          IF SEGMENT-FINNS                                                
087700             MOVE MFS-NUM-FAELT-RAETT                                     
087800                                 TO MOD-ATTR-IDPURAD2(RAD-IDX)            
087900          ELSE                                                            
088000             MOVE MFS-NUM-FAELT-FEL                                       
088100                                 TO MOD-ATTR-IDPURAD2(RAD-IDX)            
088200             MOVE NEJ            TO INDATA-SW                             
088300          END-IF                                                          
088400          IF SEGMENT-FINNS                                                
088500          IF MID-REBEART2(RAD-IDX) NOT = ALL '+'                          
088600             IF MID-REBEART2(RAD-IDX) NUMERIC                             
088700                MOVE MID-REBEART2(RAD-IDX) TO WS-MID-REBEART-NUM          
088800                IF WS-MID-REBEART-NUM <= WDE4-ORAD-KVBEART                
088900                   MOVE MFS-NUM-FAELT-RAETT                               
089000                                 TO MOD-ATTR-REBEART2(RAD-IDX)            
089100                ELSE                                                      
089200                   MOVE MFS-NUM-FAELT-FEL                                 
089300                                 TO MOD-ATTR-REBEART2(RAD-IDX)            
089400                   MOVE NEJ TO INDATA-SW                                  
089500                END-IF                                                    
089600             ELSE                                                         
089700                MOVE MFS-NUM-FAELT-FEL                                    
089800                                 TO MOD-ATTR-REBEART2(RAD-IDX)            
089900                MOVE NEJ TO INDATA-SW                                     
090000             END-IF                                                       
090100          ELSE                                                            
090200             MOVE MFS-NUM-FAELT-FEL                                       
090300                                 TO MOD-ATTR-REBEART2(RAD-IDX)            
090400             MOVE NEJ TO INDATA-SW                                        
090500          END-IF                                                          
090600          END-IF                                                          
090700       END-IF                                                             
090800                                                                          
090900       IF MID-IDPURAD3(RAD-IDX) NOT = ALL '+'                             
091000          MOVE MID-IDPURAD3(RAD-IDX) TO W-WDE4-IDPURAD                    
091100          PERFORM  IMS-GU-WDE4-PATH                                       
091200          IF SEGMENT-FINNS                                                
091300             MOVE MFS-NUM-FAELT-RAETT                                     
091400                                 TO MOD-ATTR-IDPURAD3(RAD-IDX)            
091500          ELSE                                                            
091600             MOVE MFS-NUM-FAELT-FEL                                       
091700                                 TO MOD-ATTR-IDPURAD3(RAD-IDX)            
091800             MOVE NEJ            TO INDATA-SW                             
091900          END-IF                                                          
092000          IF SEGMENT-FINNS                                                
092100          IF MID-REBEART3(RAD-IDX) NOT = ALL '+'                          
092200             IF MID-REBEART3(RAD-IDX) NUMERIC                             
092300                MOVE MID-REBEART3(RAD-IDX) TO WS-MID-REBEART-NUM          
092400                IF WS-MID-REBEART-NUM <= WDE4-ORAD-KVBEART                
092500                   MOVE MFS-NUM-FAELT-RAETT                               
092600                                 TO MOD-ATTR-REBEART3(RAD-IDX)            
092700                ELSE                                                      
092800                   MOVE MFS-NUM-FAELT-FEL                                 
092900                                 TO MOD-ATTR-REBEART3(RAD-IDX)            
093000                   MOVE NEJ TO INDATA-SW                                  
093100                END-IF                                                    
093200             ELSE                                                         
093300                MOVE MFS-NUM-FAELT-FEL                                    
093400                                 TO MOD-ATTR-REBEART3(RAD-IDX)            
093500                MOVE NEJ TO INDATA-SW                                     
093600             END-IF                                                       
093700          ELSE                                                            
093800             MOVE MFS-NUM-FAELT-FEL                                       
093900                                 TO MOD-ATTR-REBEART3(RAD-IDX)            
094000             MOVE NEJ TO INDATA-SW                                        
094100          END-IF                                                          
094200          END-IF                                                          
094300       END-IF                                                             
094400                                                                          
094500       ADD +1                      TO RAD-IDX                             
094600                                                                          
094700     END-PERFORM                                                          
094800     .                                                                    
094900     EJECT                                                                
095000****************************************************************          
095100*  H-UPPDATERA                                                 *          
095200*  REDIGERA ORDER BEROENDE PÅ HUR MÅNGA PROCENT AV DE INGÅENDE *          
095300*  ARTIKLARNA SOM SAKNAS.                                      *          
095400*  FALL1: MAN HITTAR MINDRE ÄN 90%. DÅ BYGGER MAN DET SOM GÅR  *          
095500*         ATT BYGGA. DÄREFTER DELAR MAN ORDERN I 2 DELAR OCH   *          
095600*         GÖR DEN ENA "BYGGBAR" OCH DEN ANDRA "EJ BYGGBAR".    *          
095700*         DEN "EJ BYGGBARA" LÄGGS TILLBAKS I SATSORDERKÖN.     *          
095800*  FALL2: MAN HITTAR MER ÄN 90%. DÅ BYGGER MAN DET SOM GÅR     *          
095900*         ATT BYGGA SAMT ATT MAN ANNULLERAR RESTEN AV ORDERN,  *          
096000*         ELLER ORDERN ÄR REDAN DELAN 9 GÅNGER,                *          
096100*  FALL3: EN INGÅENDE ARTIKEL SAKNAS HELT. DÅ GÖR MAN HELA     *          
096200*         ORDERN "EJ BYGGBAR" OCH LÄGGER TILLBAKS DEN I        *          
096300*         SATSORDERKÖN IGEN.                                   *          
096400****************************************************************          
096500 H-UPPDATERA SECTION.                                                     
096600                                                                          
096700     PERFORM HA-BILD-TILL-TABELL                                          
096800     PERFORM HB-RAKNA-BYGGBAR                                             
096900     PERFORM HC-RAKNA-OVRIGT                                              
097000                                                                          
097100     PERFORM HD-KOLLA-ANTAL-DELNINGAR                                     
097200                                                                          
097300     PERFORM IMS-GU-WDJ201-SATG                                           
097400     MOVE SATG-SHUV-WDJ201       TO W1-SHUV-WDJ201                        
097500     MOVE SATG-SHUV-BEFT         TO WS-SATG-SHUV-BEFT                     
097600*                                                                         
097700*  RÄKNA UT PROC.SATS                                                     
097800     COMPUTE WS-PROCSATS      = (W-MAX-KVBYGGB  /                         
097900                                       W1-SHUV-KVBEART) * 100             
098000     EVALUATE TRUE                                                        
098100                                                                          
098200       WHEN (WS-PROCSATS > 0 AND < 90) AND NOT                            
098300                                  MAX-ANTAL-DELNINGAR                     
098400         PERFORM HE-DELA-ORDER                                            
098500         PERFORM HH-STARTA-PLOCKLISTA                                     
098600                                                                          
098700       WHEN WS-PROCSATS >= 90  OR MAX-ANTAL-DELNINGAR                     
098800         PERFORM HF-DELA-ORDER-ANNULLERA                                  
098900         PERFORM HH-STARTA-PLOCKLISTA                                     
099000                                                                          
099100       WHEN WS-PROCSATS = 0 AND NOT MAX-ANTAL-DELNINGAR                   
099200         PERFORM HG-LAGG-TILLBAKA-ORDER                                   
099300         PERFORM HI-STARTA-INLLISTA                                       
099400                                                                          
099500     END-EVALUATE                                                         
099600                                                                          
099700     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
099800     CALL WMEDKONV USING MED-WMEDAREA                                     
099900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
100000     PERFORM MFS-FORM-ATTR                                                
100100     PERFORM MFS-RENSA-FAELT-IN                                           
100200     .                                                                    
100300     EJECT                                                                
100400****************************************************************          
100500*  HA-BILD-TILL-TABELL                                         *          
100600*  LAGRA UNDAN BILDEN I EN INTERNTABELL.                       *          
100700****************************************************************          
100800 HA-BILD-TILL-TABELL SECTION.                                             
100900                                                                          
101000     PERFORM HAA-NOLLSTALL-TABELL                                         
101100     MOVE +1                     TO RAD-IDX                               
101200     MOVE +1                     TO TAB-IDX                               
101300                                                                          
101400     PERFORM UNTIL RAD-IDX > 11                                           
101500                                                                          
101600       IF MID-IDPURAD1(RAD-IDX) NOT = ALL '+'                             
101700          MOVE MID-IDPURAD1(RAD-IDX) TO W-WDE4-IDPURAD                    
101800                                        TAB-MID-IDPURAD(TAB-IDX)          
101900          MOVE MID-REBEART1(RAD-IDX) TO TAB-MID-REBEART(TAB-IDX)          
102000          PERFORM  IMS-GU-WDE4-PATH                                       
102100          MOVE WDE4-KORD-IDORDNR5    TO W-RED-IDORDNST                    
102200          MOVE W-RED-IDORDNSB        TO W-IDORDNSB                        
102300          MOVE W-RED-IDORDNSS        TO W-IDORDNSS                        
102400          MOVE WDE4-ORAD-IDARTNR     TO W-IDARTNR                         
102500          MOVE WDE4-ORAD-TIUTSKR     TO WS-TIUTSKR                        
102600          PERFORM IMS-GU-WDJ211-SATG                                      
102700          MOVE SATG-SRAD-IDARTNR     TO TAB-IDARTNR(TAB-IDX)              
102800          MOVE SATG-SRAD-KDSATKMB    TO TAB-KDSATKMB(TAB-IDX)             
102900          MOVE SATG-SRAD-KDSATAND    TO TAB-KDSATAND(TAB-IDX)             
103000          MOVE SATG-SRAD-KVSATRES    TO TAB-KVSATRES(TAB-IDX)             
103100          MOVE SATG-SRAD-REBEART     TO TAB-REBEART(TAB-IDX)              
103200          MOVE SATG-SRAD-REANTPSA    TO TAB-REANTPSA(TAB-IDX)             
103300          ADD +1                     TO TAB-IDX                           
103400       END-IF                                                             
103500                                                                          
103600       IF MID-IDPURAD2(RAD-IDX) NOT = ALL '+'                             
103700          MOVE MID-IDPURAD2(RAD-IDX) TO W-WDE4-IDPURAD                    
103800                                        TAB-MID-IDPURAD(TAB-IDX)          
103900          MOVE MID-REBEART2(RAD-IDX) TO TAB-MID-REBEART(TAB-IDX)          
104000          PERFORM  IMS-GU-WDE4-PATH                                       
104100          MOVE WDE4-KORD-IDORDNR5    TO W-RED-IDORDNST                    
104200          MOVE W-RED-IDORDNSB        TO W-IDORDNSB                        
104300          MOVE W-RED-IDORDNSS        TO W-IDORDNSS                        
104400          MOVE WDE4-ORAD-IDARTNR     TO W-IDARTNR                         
104500          MOVE WDE4-ORAD-TIUTSKR     TO WS-TIUTSKR                        
104600          PERFORM IMS-GU-WDJ211-SATG                                      
104700          MOVE SATG-SRAD-IDARTNR     TO TAB-IDARTNR(TAB-IDX)              
104800          MOVE SATG-SRAD-KDSATKMB    TO TAB-KDSATKMB(TAB-IDX)             
104900          MOVE SATG-SRAD-KDSATAND    TO TAB-KDSATAND(TAB-IDX)             
105000          MOVE SATG-SRAD-KVSATRES    TO TAB-KVSATRES(TAB-IDX)             
105100          MOVE SATG-SRAD-REBEART     TO TAB-REBEART(TAB-IDX)              
105200          MOVE SATG-SRAD-REANTPSA    TO TAB-REANTPSA(TAB-IDX)             
105300          ADD +1                     TO TAB-IDX                           
105400       END-IF                                                             
105500                                                                          
105600       IF MID-IDPURAD3(RAD-IDX) NOT = ALL '+'                             
105700          MOVE MID-IDPURAD3(RAD-IDX) TO W-WDE4-IDPURAD                    
105800                                        TAB-MID-IDPURAD(TAB-IDX)          
105900          MOVE MID-REBEART3(RAD-IDX) TO TAB-MID-REBEART(TAB-IDX)          
106000          PERFORM  IMS-GU-WDE4-PATH                                       
106100          MOVE WDE4-KORD-IDORDNR5    TO W-RED-IDORDNST                    
106200          MOVE W-RED-IDORDNSB        TO W-IDORDNSB                        
106300          MOVE W-RED-IDORDNSS        TO W-IDORDNSS                        
106400          MOVE WDE4-ORAD-IDARTNR     TO W-IDARTNR                         
106500          MOVE WDE4-ORAD-TIUTSKR     TO WS-TIUTSKR                        
106600          PERFORM IMS-GU-WDJ211-SATG                                      
106700          MOVE SATG-SRAD-IDARTNR     TO TAB-IDARTNR(TAB-IDX)              
106800          MOVE SATG-SRAD-KDSATKMB    TO TAB-KDSATKMB(TAB-IDX)             
106900          MOVE SATG-SRAD-KDSATAND    TO TAB-KDSATAND(TAB-IDX)             
107000          MOVE SATG-SRAD-KVSATRES    TO TAB-KVSATRES(TAB-IDX)             
107100          MOVE SATG-SRAD-REBEART     TO TAB-REBEART(TAB-IDX)              
107200          MOVE SATG-SRAD-REANTPSA    TO TAB-REANTPSA(TAB-IDX)             
107300          ADD +1                     TO TAB-IDX                           
107400       END-IF                                                             
107500                                                                          
107600       ADD +1                        TO RAD-IDX                           
107700                                                                          
107800     END-PERFORM                                                          
107900     .                                                                    
108000     EJECT                                                                
108100****************************************************************          
108200*  HAA-NOLLSTALL-TABELL                                        *          
108300****************************************************************          
108400 HAA-NOLLSTALL-TABELL SECTION.                                            
108500                                                                          
108600     MOVE +1                     TO TAB-IDX                               
108700                                                                          
108800     PERFORM UNTIL TAB-IDX > 33                                           
108900                                                                          
109000        MOVE NEJ                 TO TAB-ANV(TAB-IDX)                      
109100        MOVE ZERO                TO TAB-MID-IDPURAD(TAB-IDX)              
109200        MOVE ZERO                TO TAB-MID-REBEART(TAB-IDX)              
109300        MOVE ZERO                TO TAB-IDARTNR(TAB-IDX)                  
109400        MOVE SPACE               TO TAB-KDSATKMB(TAB-IDX)                 
109500        MOVE SPACE               TO TAB-KDSATAND(TAB-IDX)                 
109600        MOVE ZERO                TO TAB-KVSATRES(TAB-IDX)                 
109700        MOVE ZERO                TO TAB-REBEART(TAB-IDX)                  
109800        MOVE ZERO                TO TAB-REANTPSA(TAB-IDX)                 
109900        MOVE ZERO                TO TAB-W2-REBEART(TAB-IDX)               
110000        MOVE ZERO                TO TAB-NY-REBEART(TAB-IDX)               
110100        MOVE ZERO                TO TAB-INL-ANTAL(TAB-IDX)                
110200                                                                          
110300        ADD +1                   TO TAB-IDX                               
110400                                                                          
110500     END-PERFORM                                                          
110600     .                                                                    
110700     EJECT                                                                
110800****************************************************************          
110900*  HB-RAKNA-BYGBBAR                                            *          
111000*  RÄKNA FRAM HUR MÅNGA SOM GÅR ATT BYGGA.                     *          
111100****************************************************************          
111200 HB-RAKNA-BYGGBAR SECTION.                                                
111300                                                                          
111400     MOVE 9999999                TO W-MAX-KVBYGGB                         
111500     MOVE +1                     TO TAB-IDX                               
111600                                                                          
111700     PERFORM IMS-GU-WDJ201-SATG                                           
111800     MOVE SATG-SHUV-KDSATKMB TO WS-SHUV-KDSATKMB                          
111900                                                                          
112000     IF WS-SHUV-KDSATKMB NOT = 'A'                                        
112100        PERFORM HBA-NOLLSTALL-TABELLER                                    
112200        PERFORM HBD-SKAPA-KONTROLLTABELL                                  
112300        MOVE +1 TO AND-IDX                                                
112400     END-IF                                                               
112500                                                                          
112600     PERFORM UNTIL TAB-IDX > TAB-IDX-MAX OR                               
112700                   TAB-IDARTNR(TAB-IDX) = ZERO                            
112800                                                                          
112900        IF TAB-ANV(TAB-IDX) = NEJ                                         
113000*  INGÅENDE ARTIKEL EJ BEARBETAD                                          
113100           IF TAB-KDSATKMB(TAB-IDX) NOT = SPACE                           
113200              PERFORM HBB-HITTA-KDSATKMB                                  
113300           ELSE                                                           
113400              MOVE TAB-MID-REBEART(TAB-IDX)                               
113500                                     TO WS-TAB-MID-REBEART                
113600              MOVE TAB-REANTPSA(TAB-IDX) TO WS-TAB-REANTPSA               
113700              MOVE JA                TO TAB-ANV(TAB-IDX)                  
113800                                                                          
113900              COMPUTE WS-KVBEART-BYGGB = (WS-TAB-MID-REBEART /            
114000                                             WS-TAB-REANTPSA)             
114100              IF WS-KVBEART-BYGGB     <  W-MAX-KVBYGGB                    
114200                 MOVE WS-KVBEART-BYGGB TO W-MAX-KVBYGGB                   
114300              END-IF                                                      
114400           END-IF                                                         
114500        END-IF                                                            
114600        ADD +1                   TO TAB-IDX                               
114700     END-PERFORM                                                          
114800                                                                          
114900     IF WS-SHUV-KDSATKMB NOT = 'A'                                        
115000        PERFORM HBC-OVRIGA-KDSATKMB                                       
115100     END-IF                                                               
115200     .                                                                    
115300     EJECT                                                                
115400****************************************************************          
115500*  HBA-NOLLSTALL-TABELLER                                      *          
115600*  ÄNDRINGSTABELL OCH KONTROLLTABELL INITIERAS.                *          
115700****************************************************************          
115800 HBA-NOLLSTALL-TABELLER SECTION.                                          
115900                                                                          
116000     MOVE +1                     TO AND-IDX                               
116100                                                                          
116200     PERFORM UNTIL AND-IDX > 16                                           
116300                                                                          
116400        MOVE NEJ                 TO AND-JUST(AND-IDX)                     
116500        MOVE NEJ                 TO AND-BRIST(AND-IDX)                    
116600        MOVE ZERO                TO AND-MID-IDPURAD(AND-IDX)              
116700        MOVE ZERO                TO AND-MID-REBEART(AND-IDX)              
116800        MOVE ZERO                TO AND-IDARTNR(AND-IDX)                  
116900        MOVE SPACE               TO AND-KDSATKMB(AND-IDX)                 
117000        MOVE SPACE               TO AND-KDSATAND(AND-IDX)                 
117100        MOVE ZERO                TO AND-KVSATRES(AND-IDX)                 
117200        MOVE ZERO                TO AND-REBEART(AND-IDX)                  
117300        MOVE ZERO                TO AND-REANTPSA(AND-IDX)                 
117400        MOVE ZERO                TO AND-KVBYGGB(AND-IDX)                  
117500        MOVE ZERO                TO AND-NY-REBEART(AND-IDX)               
117600        MOVE ZERO                TO AND-INL-ANTAL(AND-IDX)                
117700        MOVE ZERO                TO AND-W2-REBEART(AND-IDX)               
117800                                                                          
117900        ADD +1                   TO AND-IDX                               
118000                                                                          
118100     END-PERFORM                                                          
118200                                                                          
118300     MOVE +1                     TO KONTR-IDX                             
118400                                                                          
118500     PERFORM UNTIL KONTR-IDX > KONTR-IDX-MAX                              
118600        MOVE SPACE               TO KONTR-ANV(KONTR-IDX)                  
118700        MOVE SPACE               TO KONTR-KDSATKMB(KONTR-IDX)             
118800        ADD +1                   TO KONTR-IDX                             
118900     END-PERFORM                                                          
119000                                                                          
119100     .                                                                    
119200     EJECT                                                                
119300****************************************************************          
119400*  HBB-HITTA-KDSATKMB                                          *          
119500*  HITTA ALLA RADFÖREKOMSTER MED VISS KOMBINATIONSKOD          *          
119600****************************************************************          
119700 HBB-HITTA-KDSATKMB SECTION.                                              
119800                                                                          
119900     MOVE ZERO                   TO WS-KVBEART-BYGGB-KOMB-TOT             
120000     MOVE WDE4-KORD-IDORDNR5     TO W-RED-IDORDNST                        
120100     MOVE W-RED-IDORDNSB         TO W-IDORDNSB                            
120200     MOVE W-RED-IDORDNSS         TO W-IDORDNSS                            
120300     MOVE +0                     TO W-IDARTNR                             
120400     MOVE TAB-KDSATKMB(TAB-IDX)  TO W-KDSATKMB                            
120500     MOVE NEJ                    TO KDSATKMB-FUNNEN-SW                    
120600                                                                          
120700*    HITTA FÖRSTA RADEN MED KOMBINATIONSKODEN                             
120800     PERFORM IMS-GU-WDJ211-KDSATKMB                                       
120900                                                                          
121000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
121100                   SEGMENT-SLUT                                           
121200                                                                          
121300        IF KDSATKMB-FUNNEN-SW = NEJ                                       
121400           PERFORM HBBA-MARKERA-KONTROLLTAB                               
121500        END-IF                                                            
121600                                                                          
121700        IF KDSATKMB-FUNNEN-SW = JA                                        
121800                                                                          
121900           PERFORM HBBB-SKAPA-ANDRINGSTABELL                              
122000                                                                          
122100           PERFORM HBBC-KOLLA-KDSATKMB-BILD                               
122200                                                                          
122300           MOVE ZERO                 TO WS-KVBEART-BYGGB-KOMB             
122400                                                                          
122500           COMPUTE WS-KVBEART-BYGGB-KOMB =                                
122600               (AND-MID-REBEART(AND-IDX) / AND-REANTPSA(AND-IDX))         
122700                                                                          
122800           MOVE    WS-KVBEART-BYGGB-KOMB TO AND-KVBYGGB(AND-IDX)          
122900           ADD WS-KVBEART-BYGGB-KOMB TO WS-KVBEART-BYGGB-KOMB-TOT         
123000        END-IF                                                            
123100                                                                          
123200        PERFORM IMS-GN-WDJ211-KDSATKMB                                    
123300     END-PERFORM                                                          
123400                                                                          
123500     IF WS-KVBEART-BYGGB-KOMB-TOT < W-MAX-KVBYGGB                         
123600        MOVE WS-KVBEART-BYGGB-KOMB-TOT TO W-MAX-KVBYGGB                   
123700     END-IF                                                               
123800                                                                          
123900     IF KDSATKMB-FUNNEN-SW = NEJ                                          
124000        MOVE 'FEL I HBB- VID KONTROLL KOMBINATIONSKODER '                 
124100        TO FELTEXT                                                        
124200        CALL ABEND USING RKOD-ABEND                                       
124300     END-IF                                                               
124400     .                                                                    
124500     EJECT                                                                
124600****************************************************************          
124700*  HBBA-MARKERA-KONTROLLTAB                                    *          
124800*  MARKERA I KOMBINATIONSTABELLEN ATT ARTIKELRAD MED EN VISS   *          
124900*  KOMBINATIONSKOD HAR BEARBETATS                              *          
125000****************************************************************          
125100 HBBA-MARKERA-KONTROLLTAB SECTION.                                        
125200                                                                          
125300     MOVE +1  TO KONTR-IDX                                                
125400     MOVE NEJ TO KDSATKMB-FUNNEN-SW                                       
125500                                                                          
125600     PERFORM UNTIL KONTR-IDX    >  KONTR-IDX-MAX OR                       
125700             KDSATKMB-FUNNEN-SW =  JA                                     
125800        IF SATG-SRAD-KDSATKMB   =  KONTR-KDSATKMB(KONTR-IDX)              
125900           MOVE JA              TO KONTR-ANV    (KONTR-IDX)               
126000           MOVE JA              TO KDSATKMB-FUNNEN-SW                     
126100        ELSE                                                              
126200           CONTINUE                                                       
126300        END-IF                                                            
126400                                                                          
126500        ADD +1                  TO KONTR-IDX                              
126600                                                                          
126700     END-PERFORM                                                          
126800     .                                                                    
126900     EJECT                                                                
127000                                                                          
127100****************************************************************          
127200*  HBBB-SKAPA-ANDRINGSTABELL                                   *          
127300*  EN ÄNDRINGSRAD FÖR VARJE SATSRAD DÄR KOMBINATIONSKOD FÖRE-  *          
127400*  KOMMER.                                                     *          
127500****************************************************************          
127600 HBBB-SKAPA-ANDRINGSTABELL SECTION.                                       
127700                                                                          
127800                                                                          
127900     MOVE +1                     TO AND-IDX                               
128000                                                                          
128100     PERFORM UNTIL AND-IDX       >  AND-IDX-MAX  OR                       
128200        AND-IDARTNR(AND-IDX)     =  ZERO                                  
128300        ADD +1                   TO AND-IDX                               
128400     END-PERFORM                                                          
128500                                                                          
128600     MOVE SATG-SRAD-IDARTNR      TO AND-IDARTNR(AND-IDX)                  
128700     MOVE SATG-SRAD-KDSATKMB     TO AND-KDSATKMB(AND-IDX)                 
128800     MOVE SATG-SRAD-KDSATAND     TO AND-KDSATAND(AND-IDX)                 
128900     MOVE SATG-SRAD-KVSATRES     TO AND-KVSATRES(AND-IDX)                 
129000     MOVE SATG-SRAD-REBEART      TO AND-REBEART(AND-IDX)                  
129100                                    AND-MID-REBEART(AND-IDX)              
129200     MOVE SATG-SRAD-REANTPSA     TO AND-REANTPSA(AND-IDX)                 
129300                                                                          
129400     .                                                                    
129500     EJECT                                                                
129600****************************************************************          
129700*  HBBC-KOLLA-KDSATKMB-BILD                                    *          
129800*  SE EFTER IFALL DEN INGÅENDE ARIKEL JAG FÅTT TRÄFF PÅ MED    *          
129900*  EN SPECIELL KOMBINATIONKOD OCKSÅ FINNS ANGIVEN I AVVIKELSE- *          
130000*  BILDEN.                                                     *          
130100****************************************************************          
130200 HBBC-KOLLA-KDSATKMB-BILD SECTION.                                        
130300                                                                          
130400     MOVE NEJ                    TO TAB-TRAFF-SW                          
130500     MOVE +1                     TO KOMBKOD-IDX                           
130600                                                                          
130700     PERFORM UNTIL KOMBKOD-IDX > KOMBKOD-IDX-MAX OR                       
130800                   TAB-IDARTNR(KOMBKOD-IDX) = ZERO OR                     
130900                   TAB-TRAFF-SW = JA                                      
131000                                                                          
131100        IF TAB-ANV(KOMBKOD-IDX) = NEJ                                     
131200*  INGÅENDE ARTIKEL EJ BEARBETAD                                          
131300           IF AND-IDARTNR(AND-IDX) = TAB-IDARTNR(KOMBKOD-IDX)             
131400*  HAR FÅTT TRÄFF PÅ ETT ARTIKELNR SOM FINNS MED I BILDEN                 
131500              MOVE TAB-MID-REBEART(KOMBKOD-IDX)                           
131600                                 TO AND-MID-REBEART(AND-IDX)              
131700              MOVE TAB-MID-IDPURAD(KOMBKOD-IDX)                           
131800                                 TO AND-MID-IDPURAD(AND-IDX)              
131900              MOVE JA            TO TAB-ANV(KOMBKOD-IDX)                  
132000                                    AND-BRIST(AND-IDX)                    
132100                                    TAB-TRAFF-SW                          
132200           ELSE                                                           
132300*  ARTIKELN FINNS EJ PÅ BILDEN                                            
132400              CONTINUE                                                    
132500           END-IF                                                         
132600        END-IF                                                            
132700                                                                          
132800        ADD +1                   TO KOMBKOD-IDX                           
132900                                                                          
133000     END-PERFORM                                                          
133100                                                                          
133200     .                                                                    
133300     EJECT                                                                
133400****************************************************************          
133500*  HBC-OVRIGA-KDSATKMB                                         *          
133600*  EN KOMBINATIONSKOD BEHÖVER INTE VARA BRISTRAPPORTERAD.      *          
133700*  DOCK SKALL ALLA KOMBINATIONSKODER SPECIALHANTERAS.          *          
133800****************************************************************          
133900 HBC-OVRIGA-KDSATKMB      SECTION.                                        
134000                                                                          
134100     MOVE +1                     TO KONTR-IDX                             
134200     MOVE JA                     TO KDSATKMB-FUNNEN-SW                    
134300                                                                          
134400     PERFORM UNTIL KONTR-IDX > KONTR-IDX-MAX OR                           
134500                   KONTR-KDSATKMB(KONTR-IDX) = SPACE                      
134600                                                                          
134700        IF KONTR-ANV(KONTR-IDX) = NEJ                                     
134800           MOVE +0                        TO W-IDARTNR                    
134900           MOVE KONTR-KDSATKMB(KONTR-IDX) TO W-KDSATKMB                   
135000           MOVE NEJ                       TO KDSATKMB-FUNNEN-SW           
135100           PERFORM IMS-GU-WDJ211-KDSATKMB                                 
135200                                                                          
135300           PERFORM UNTIL SEGMENT-SAKNAS  OR                               
135400                         SEGMENT-SLUT                                     
135500                                                                          
135600              IF KDSATKMB-FUNNEN-SW = NEJ                                 
135700                 MOVE JA  TO KONTR-ANV(KONTR-IDX)                         
135800                 MOVE JA  TO KDSATKMB-FUNNEN-SW                           
135900                 PERFORM HBCA-HITTA-ANDRINGSRAD                           
136000              END-IF                                                      
136100                                                                          
136200              IF KDSATKMB-FUNNEN-SW = JA                                  
136300                 PERFORM HBCB-SKAPA-ANDRINGSTABELL                        
136400              END-IF                                                      
136500                                                                          
136600              PERFORM IMS-GN-WDJ211-KDSATKMB                              
136700           END-PERFORM                                                    
136800        END-IF                                                            
136900                                                                          
137000        ADD +1                   TO KONTR-IDX                             
137100                                                                          
137200     END-PERFORM                                                          
137300                                                                          
137400     IF KDSATKMB-FUNNEN-SW = NEJ                                          
137500        MOVE 'FEL I HBC- VID KONTROLL KOMBINATIONSKODER '                 
137600        TO FELTEXT                                                        
137700        CALL ABEND USING RKOD-ABEND                                       
137800     END-IF                                                               
137900     .                                                                    
138000     EJECT                                                                
138100 HBCA-HITTA-ANDRINGSRAD    SECTION.                                       
138200                                                                          
138300     MOVE +1                     TO AND-IDX                               
138400                                                                          
138500     PERFORM UNTIL AND-IDX       >  AND-IDX-MAX  OR                       
138600        AND-IDARTNR(AND-IDX)     =  ZERO                                  
138700        ADD +1                   TO AND-IDX                               
138800     END-PERFORM                                                          
138900     .                                                                    
139000     SKIP3                                                                
139100****************************************************************          
139200*  HBCB-SKAPA-ANDRINGSTABELL                                   *          
139300*  EN ÄNDRINGSRAD FÖR VARJE SATSRAD DÄR KOMBINATIONSKOD FÖRE-  *          
139400*  KOMMER. KOMBKODEN FÖREKOMMER EJ I AVVIKELSERAPPORTERINGEN.  *          
139500****************************************************************          
139600 HBCB-SKAPA-ANDRINGSTABELL SECTION.                                       
139700                                                                          
139800     MOVE SATG-SRAD-IDARTNR      TO AND-IDARTNR(AND-IDX)                  
139900     MOVE SATG-SRAD-KDSATKMB     TO AND-KDSATKMB(AND-IDX)                 
140000     MOVE SATG-SRAD-KDSATAND     TO AND-KDSATAND(AND-IDX)                 
140100     MOVE SATG-SRAD-KVSATRES     TO AND-KVSATRES(AND-IDX)                 
140200     MOVE SATG-SRAD-REBEART      TO AND-REBEART(AND-IDX)                  
140300                                    AND-MID-REBEART(AND-IDX)              
140400     MOVE SATG-SRAD-REANTPSA     TO AND-REANTPSA(AND-IDX)                 
140500                                                                          
140600     COMPUTE WS-KVBEART-BYGGB-KOMB =                                      
140700         (AND-MID-REBEART(AND-IDX) / AND-REANTPSA(AND-IDX))               
140800                                                                          
140900     MOVE  WS-KVBEART-BYGGB-KOMB TO AND-KVBYGGB(AND-IDX)                  
141000                                                                          
141100     ADD +1                      TO AND-IDX                               
141200     .                                                                    
141300     EJECT                                                                
141400****************************************************************          
141500*  HBD-SKAPA-KONTROLLTABELL                                    *          
141600*  KONTROLLTABELL FÖR ANVÄNDA KOMBINATIONSKODER SKAPAS.        *          
141700*  BOKSTÄVER "LÄGRE" ÄN KDSATKMB I ORDERHUVUDET LÄGGS UPP.     *          
141800****************************************************************          
141900 HBD-SKAPA-KONTROLLTABELL SECTION.                                        
142000                                                                          
142100     MOVE NEJ TO SKAPA-KONTR-TAB-SW                                       
142200     MOVE +1 TO BOKST-IDX                                                 
142300     MOVE +1 TO KONTR-IDX                                                 
142400     PERFORM UNTIL BOKST-IDX > 26 OR                                      
142500                   KONTR-IDX > 26 OR                                      
142600          SKAPA-KONTR-TAB-SW = JA                                         
142700        IF BOKST-KDSATKMB(BOKST-IDX) = WS-SHUV-KDSATKMB                   
142800           MOVE JA TO SKAPA-KONTR-TAB-SW                                  
142900        ELSE                                                              
143000           MOVE BOKST-KDSATKMB(BOKST-IDX) TO                              
143100                         KONTR-KDSATKMB(KONTR-IDX)                        
143200           MOVE NEJ  TO  KONTR-ANV(KONTR-IDX)                             
143300        END-IF                                                            
143400        ADD +1 TO BOKST-IDX                                               
143500        ADD +1 TO KONTR-IDX                                               
143600     END-PERFORM                                                          
143700     .                                                                    
143800     EJECT                                                                
143900****************************************************************          
144000*  HC-RAKNA-OVRIGT                                             *          
144100*  FÖR AVVIKELSERAPPORTERADE UTAN KOMBINATIONSKOD BERÄKNAS:    *          
144200*  ANTAL BESTÄLLDA PER INGÅENDE ARTIKEL                        *          
144300*  ANTAL SAKNADE INGÅENDE ARTIKLAR                             *          
144400*  FÖR ARTIKELRADER MED KOMBINATIONSKOD BERÄKNAS:              *          
144500*  ANTAL BESTÄLLDA PER INGÅENDE ARTIKEL                        *          
144600*  ANTAL SAKNADE INGÅENDE ARTIKLAR                             *          
144700****************************************************************          
144800 HC-RAKNA-OVRIGT  SECTION.                                                
144900                                                                          
145000     IF WS-SHUV-KDSATKMB NOT = 'A'                                        
145100        PERFORM HCA-JUSTERA-ANDRINGSTABELL                                
145200     END-IF                                                               
145300                                                                          
145400     MOVE +1                   TO TAB-IDX                                 
145500                                                                          
145600     PERFORM UNTIL TAB-IDX > 33 OR                                        
145700                   TAB-IDARTNR(TAB-IDX) = ZERO                            
145800                                                                          
145900        IF TAB-KDSATKMB(TAB-IDX) = SPACE                                  
146000           COMPUTE TAB-NY-REBEART(TAB-IDX) =                              
146100                   W-MAX-KVBYGGB * TAB-REANTPSA(TAB-IDX)                  
146200                                                                          
146300           COMPUTE TAB-W2-REBEART(TAB-IDX) =                              
146400                   TAB-REBEART(TAB-IDX) - TAB-NY-REBEART(TAB-IDX)         
146500                                                                          
146600        ELSE                                                              
146700           CONTINUE                                                       
146800        END-IF                                                            
146900                                                                          
147000        ADD +1                 TO TAB-IDX                                 
147100                                                                          
147200     END-PERFORM                                                          
147300     .                                                                    
147400     EJECT                                                                
147500****************************************************************          
147600*  HCA-JUSTERA-ANDRINGSTABELL                                  *          
147700*  I ÄNDRINGSTABELLEN ÄR SATSRADER MED KOMBINATIONSKODER GRUP- *          
147800*  PERADE I FÖLJD. OM EN ARTIKEL ÄR ERSÄTTNINGSMÄRKT SKALL     *          
147900*  DENNA I FÖRSTA HAND PLOCKAS TILL DEN BYGGBARA ORDERN.       *          
148000*  OM INGEN ARTIKEL INOM KOMBINATIONSKODEN ÄR ERSÄTTNINGSMÄRKT *          
148100*  PLOCKAS DEN FÖRST PÅTRÄFFADE TILL DEN BYGGBARA ORDERN.      *          
148200*  ANTAL SAKNADE INGÅENDE ARTIKLAR OCH ANTAL ÅTERLÄGGNINGSBARA *          
148300*  ARTIKLAR BERÄKNAS.                                          *          
148400****************************************************************          
148500 HCA-JUSTERA-ANDRINGSTABELL SECTION.                                      
148600                                                                          
148700     MOVE NEJ                    TO TAB-TRAFF-SW                          
148800     MOVE +1                     TO JUST-IDX                              
148900     MOVE +1                     TO AND-IDX                               
149000     MOVE AND-KDSATKMB(AND-IDX)  TO WS-KDSATKMB-NY                        
149100                                                                          
149200     PERFORM UNTIL JUST-IDX > JUST-IDX-MAX OR                             
149300                   AND-IDX  > AND-IDX-MAX  OR                             
149400                   AND-IDARTNR(AND-IDX) = ZERO                            
149500                                                                          
149600        MOVE WS-KDSATKMB-NY      TO WS-KDSATKMB                           
149700        MOVE W-MAX-KVBYGGB       TO W-MAX-KVBYGGB-REST                    
149800                                                                          
149900***     ÄR ÄNDRINGSKODEN E ANVÄND I DEN HÄR KOMBINATIONSKODEN?            
150000***     ISÅFALL SKALL DEN PLOCKAS FÖRST                                   
150100        PERFORM UNTIL JUST-IDX > JUST-IDX-MAX OR                          
150200                      AND-KDSATKMB(JUST-IDX) NOT = WS-KDSATKMB            
150300                                                                          
150400           IF AND-JUST(JUST-IDX) = NEJ                                    
150500             IF AND-KDSATAND(JUST-IDX) = 'E'                              
150600               IF AND-KVBYGGB(JUST-IDX) > W-MAX-KVBYGGB-REST              
150700                 COMPUTE AND-NY-REBEART(JUST-IDX) =                       
150800                     W-MAX-KVBYGGB-REST * AND-REANTPSA(JUST-IDX)          
150900                 MOVE ZERO TO W-MAX-KVBYGGB-REST                          
151000               ELSE                                                       
151100                 COMPUTE AND-NY-REBEART(JUST-IDX) ROUNDED =               
151200                     AND-KVBYGGB(JUST-IDX) *                              
151300                     AND-REANTPSA(JUST-IDX)                               
151400                 COMPUTE AND-W2-REBEART(JUST-IDX) =                       
151500                     AND-REBEART(JUST-IDX) -                              
151600                     AND-NY-REBEART(JUST-IDX)                             
151700                 COMPUTE W-MAX-KVBYGGB-REST =                             
151800                     W-MAX-KVBYGGB-REST - AND-KVBYGGB(JUST-IDX)           
151900               END-IF                                                     
152000               MOVE JA TO AND-JUST(JUST-IDX)                              
152100             ELSE                                                         
152200               CONTINUE                                                   
152300             END-IF                                                       
152400           ELSE                                                           
152500             CONTINUE                                                     
152600           END-IF                                                         
152700                                                                          
152800           ADD +1 TO JUST-IDX                                             
152900                                                                          
153000           IF AND-KDSATKMB(JUST-IDX) NOT = WS-KDSATKMB AND                
153100              AND-KDSATKMB(JUST-IDX) NOT = SPACE                          
153200              MOVE AND-KDSATKMB(JUST-IDX) TO WS-KDSATKMB-NY               
153300           END-IF                                                         
153400                                                                          
153500        END-PERFORM                                                       
153600                                                                          
153700***     ÖVRIGA ÄNDRINGSKODER INOM KOMBINATIONSKODEN BEHANDLAS             
153800                                                                          
153900        PERFORM UNTIL AND-IDX > AND-IDX-MAX OR                            
154000                      AND-KDSATKMB(AND-IDX) NOT = WS-KDSATKMB             
154100                                                                          
154200           IF AND-JUST(AND-IDX) = NEJ                                     
154300             IF AND-KVBYGGB(AND-IDX) > W-MAX-KVBYGGB-REST                 
154400               COMPUTE AND-NY-REBEART(AND-IDX) =                          
154500                   W-MAX-KVBYGGB-REST * AND-REANTPSA(AND-IDX)             
154600               MOVE ZERO TO W-MAX-KVBYGGB-REST                            
154700             ELSE                                                         
154800               COMPUTE AND-NY-REBEART(AND-IDX) ROUNDED =                  
154900                   AND-KVBYGGB(AND-IDX) *                                 
155000                   AND-REANTPSA(AND-IDX)                                  
155100               COMPUTE W-MAX-KVBYGGB-REST = W-MAX-KVBYGGB-REST -          
155200                   AND-KVBYGGB(AND-IDX)                                   
155300             END-IF                                                       
155400             COMPUTE AND-W2-REBEART(AND-IDX) =                            
155500                AND-REBEART(AND-IDX) -                                    
155600                AND-NY-REBEART(AND-IDX)                                   
155700             MOVE JA TO AND-JUST(AND-IDX)                                 
155800           ELSE                                                           
155900             CONTINUE                                                     
156000           END-IF                                                         
156100                                                                          
156200           ADD +1 TO AND-IDX                                              
156300                                                                          
156400        END-PERFORM                                                       
156500                                                                          
156600     END-PERFORM                                                          
156700     .                                                                    
156800     EJECT                                                                
156900 HD-KOLLA-ANTAL-DELNINGAR SECTION.                                        
157000****************************************************************          
157100*  KOLLA OM ORDERN REDAN ÄR DELAD 9 GÅNGER, I SÅDANA           *          
157200*  FALL ANNULLERAR MAN RESTEN                                  *          
157300****************************************************************          
157400                                                                          
157500     MOVE W-IDORDNSS           TO W-SPAR-IDORDNSS                         
157600     MOVE +9                   TO W-IDORDNSS                              
157700     MOVE NEJ                  TO MAX-ANTAL-DELNINGAR-SW                  
157800     PERFORM IMS-GU-WDJ201-SATG                                           
157900     IF SEGMENT-FINNS                                                     
158000         MOVE JA               TO MAX-ANTAL-DELNINGAR-SW                  
158100     END-IF                                                               
158200     MOVE W-SPAR-IDORDNSS      TO W-IDORDNSS                              
158300     .                                                                    
158400     EJECT                                                                
158500 HE-DELA-ORDER SECTION.                                                   
158600****************************************************************          
158700*  HE-DELA-ORDER                                               *          
158800*  FALL1: MAN HITTAR MINDRE ÄN 90%. DÅ BYGGER MAN DET SOM GÅR  *          
158900*         ATT BYGGA. DÄREFTER DELAR MAN ORDERN I 2 DELAR OCH   *          
159000*         GÖR DEN ENA "BYGGBAR" OCH DEN ANDRA "EJ BYGGBAR".    *          
159100*         DEN "EJ BYGGBARA" LÄGGS TILLBAKS I SATSORDERKÖN.     *          
159200*           - I W1-AREAN SKAPAS BYGGBARA RADER.                *          
159300*           - I W2-AREAN SKAPAS EJ BYGGBARA RADER.             *          
159400*           - I SATG1-AREAN SPARAS EJ-BYGGBAR-DUBLETT.         *          
159500****************************************************************          
159600                                                                          
159700     MOVE W-IDORDNSB           TO W-BYGGB-IDORDNSB                        
159800     MOVE W-IDORDNSS           TO W-BYGGB-IDORDNSS                        
159900                                                                          
160000     MOVE ZERO                 TO W1-VLORDNTO                             
160100     MOVE ZERO                 TO W1-VKORDNTO                             
160200                                                                          
160300     PERFORM HEA-BEHANDLA-EJ-BYGGB-SATG01                                 
160400     PERFORM HEB-BEHANDLA-ING-ART                                         
160500     PERFORM HEC-UPPD-BYGGBAR-SATG01                                      
160600     PERFORM HED-UPPD-EJ-BYGGBAR-SATG01                                   
160700                                                                          
160800     .                                                                    
160900     EJECT                                                                
161000****************************************************************          
161100*  HEA-BEHANDLA-EJ-BYGGB-SATG01                                *          
161200*  SKAPA WDJ201 FÖR DEN "EJ BYGGBARA" DELEN AV SATSORDERN.     *          
161300****************************************************************          
161400 HEA-BEHANDLA-EJ-BYGGB-SATG01   SECTION.                                  
161500                                                                          
161600     MOVE W1-SHUV-IDORDNSB      TO W-EJ-BYGGB-IDORDNSB                    
161700     COMPUTE W-EJ-BYGGB-IDORDNSS = W-BYGGB-IDORDNSS + 1                   
161800                                                                          
161900     MOVE W1-SHUV-WDJ201       TO  W2-SHUV-WDJ201                         
162000     MOVE W-EJ-BYGGB-IDORDNSS  TO  W2-SHUV-IDORDNSS                       
162100                                                                          
162200     MOVE NEJ                  TO W2-SHUV-FLBYGGB                         
162300     MOVE 'R'                  TO W2-SHUV-KDSATSTA                        
162400     MOVE ZERO                 TO W2-SHUV-IDPRODNR                        
162500                                                                          
162600     COMPUTE W2-SHUV-KVBEART   =  SATG-SHUV-KVBEART -                     
162700                                  W-MAX-KVBYGGB                           
162800                                                                          
162900     MOVE W2-SHUV-WDJ201       TO  SATG-SHUV-WDJ201                       
163000     PERFORM IMS-ISRT-SATG1-SATG01                                        
163100     PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                                 
163200         ADD +1                   TO W-EJ-BYGGB-IDORDNSS                  
163300         MOVE W-EJ-BYGGB-IDORDNSS TO W2-SHUV-IDORDNSS                     
163400         MOVE W2-SHUV-WDJ201      TO  SATG-SHUV-WDJ201                    
163500         PERFORM IMS-ISRT-SATG1-SATG01                                    
163600     END-PERFORM                                                          
163700     .                                                                    
163800     EJECT                                                                
163900 HEB-BEHANDLA-ING-ART           SECTION.                                  
164000                                                                          
164100     MOVE NEJ                  TO TAB-TRAFF-SW                            
164200     MOVE ZERO                 TO W1-KVRADER                              
164300     PERFORM IMS-GHNP-WDJ211-SATG                                         
164400                                                                          
164500     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
164600                   SEGMENT-SLUT                                           
164700                                                                          
164800        IF ((SATG-SRAD-KDSATAND = 'U' AND SATG-SRAD-REBEART = 0)          
164900            OR                                                            
165000            (SATG-SRAD-KDSATAND = 'E' AND SATG-SRAD-REBEART = 0)          
165100            OR                                                            
165200            (SATG-SRAD-KDSATAND = 'B'))                                   
165300            PERFORM IMS-GHU-ARTC11                                        
165400            MOVE CLAG-IDANSK        TO WS-IDANSK                          
165500        ELSE                                                              
165600           MOVE SATG-SRAD-REBEART  TO W-ORIG-REBEART                      
165700           MOVE SATG-SRAD-KVSATRES TO W-ORIG-KVSATRES                     
165800           IF SATG-SRAD-KDSATKMB = SPACE                                  
165900              PERFORM HEBA-KOLLA-TRAFF-I-TAB                              
166000           ELSE                                                           
166100              PERFORM HEBB-KOLLA-TRAFF-I-ANDRINGSTAB                      
166200           END-IF                                                         
166300           MOVE SATG-SRAD-WDJ211 TO W1-SRAD-WDJ211                        
166400           MOVE W1-SRAD-IDARTNR  TO W-IDARTNR                             
166500                                 IN W-IDARTNR-X                           
166600           PERFORM HEBC-UPPDATERA-BYGGB-SATG11                            
166700           PERFORM HEBD-UPPD-ARTC11                                       
166800           IF ((W1-SRAD-KDSATAND = SPACE) OR                              
166900               (W1-SRAD-KDSATAND NOT = SPACE AND                          
167000                W1-SRAD-REBEART > 0))                                     
167100              PERFORM HEBE-SKAPA-PLOCKL-WDJ212                            
167200           END-IF                                                         
167300           PERFORM HEBF-SKAPA-INLL-WDJ212                                 
167400        END-IF                                                            
167500                                                                          
167600                                                                          
167700        PERFORM HEBG-SKAPA-EJ-BYGGB-SATG11                                
167800        IF W2-SRAD-KVSATROS > 0                                           
167900           PERFORM HEBH-SKAPA-ORDP01                                      
168000        END-IF                                                            
168100                                                                          
168200        PERFORM IMS-GHNP-WDJ211-SATG                                      
168300        MOVE NEJ              TO TAB-TRAFF-SW                             
168400        MOVE NEJ              TO AND-TRAFF-SW                             
168500     END-PERFORM                                                          
168600     .                                                                    
168700     EJECT                                                                
168800****************************************************************          
168900*  HEBA-KOLLA-TRAFF-I-TAB                                      *          
169000*  GÅ IGENOM INTERNTABELLEN, FÖR ATT SE OM DEN ING.ARTIKEL     *          
169100*  SOM JAG LÄST I WDJ2 ÄVEN FINNS SPARAD I TABELLEN.           *          
169200****************************************************************          
169300 HEBA-KOLLA-TRAFF-I-TAB SECTION.                                          
169400                                                                          
169500     MOVE NEJ                  TO TAB-TRAFF-SW                            
169600     MOVE +1                   TO TAB-IDX                                 
169700                                                                          
169800     PERFORM UNTIL TAB-IDX > 33 OR                                        
169900                   TAB-IDARTNR(TAB-IDX) = ZERO OR                         
170000                   TAB-TRAFF-SW = JA                                      
170100                                                                          
170200        IF SATG-SRAD-IDARTNR = TAB-IDARTNR(TAB-IDX)                       
170300           MOVE JA             TO TAB-TRAFF-SW                            
170400           COMPUTE TAB-INL-ANTAL(TAB-IDX) =                               
170500                                (TAB-MID-REBEART(TAB-IDX) -               
170600                                 TAB-NY-REBEART(TAB-IDX))                 
170700        ELSE                                                              
170800           ADD +1              TO TAB-IDX                                 
170900        END-IF                                                            
171000                                                                          
171100     END-PERFORM                                                          
171200     .                                                                    
171300     EJECT                                                                
171400****************************************************************          
171500*  HEBB-KOLLA-TRAFF-I-ANDRINGSTAB                              *          
171600*  GÅ IGENOM ÄNDRINGSTABELLEN, FÖR ATT HUR DEN ING.ARTIKELN    *          
171700*  MED KOMBINATIONSKOD SKALL HANTERAS.                         *          
171800****************************************************************          
171900 HEBB-KOLLA-TRAFF-I-ANDRINGSTAB SECTION.                                  
172000                                                                          
172100     MOVE NEJ                  TO AND-TRAFF-SW                            
172200     MOVE +1                   TO AND-IDX                                 
172300                                                                          
172400     PERFORM UNTIL AND-IDX > 16 OR                                        
172500                   AND-IDARTNR(AND-IDX) = ZERO OR                         
172600                   AND-TRAFF-SW = JA                                      
172700                                                                          
172800        IF SATG-SRAD-IDARTNR = AND-IDARTNR(AND-IDX)                       
172900           MOVE JA             TO AND-TRAFF-SW                            
173000           COMPUTE AND-INL-ANTAL(AND-IDX) =                               
173100                                (AND-MID-REBEART(AND-IDX) -               
173200                                 AND-NY-REBEART(AND-IDX))                 
173300           COMPUTE AND-W2-REBEART(AND-IDX) =                              
173400                                (AND-REBEART(AND-IDX) -                   
173500                                 AND-NY-REBEART(AND-IDX))                 
173600        ELSE                                                              
173700           ADD +1              TO AND-IDX                                 
173800        END-IF                                                            
173900                                                                          
174000     END-PERFORM                                                          
174100     .                                                                    
174200     EJECT                                                                
174300 HEBC-UPPDATERA-BYGGB-SATG11    SECTION.                                  
174400                                                                          
174500     MOVE W-BYGGB-IDORDNSB     TO W-IDORDNSB                              
174600     MOVE W-BYGGB-IDORDNSS     TO W-IDORDNSS                              
174700                                                                          
174800**  OBS KVSATRES OCH REBEART AVRUNDAS ALLTID UPP TILL                     
174900**      NÄRMASTE HELTAL                                                   
175000                                                                          
175100     MOVE W1-SRAD-KVSATROS     TO W-ORIG-KVSATROS                         
175200                                                                          
175300     IF TAB-TRAFF-SW = JA                                                 
175400        MOVE TAB-NY-REBEART(TAB-IDX)                                      
175500                               TO W1-SRAD-REBEART                         
175600        MOVE TAB-NY-REBEART(TAB-IDX)                                      
175700                               TO W1-SRAD-KVSATRES                        
175800     ELSE                                                                 
175900        IF AND-TRAFF-SW = JA                                              
176000           MOVE AND-NY-REBEART(AND-IDX)                                   
176100                                  TO W1-SRAD-REBEART                      
176200           MOVE AND-NY-REBEART(AND-IDX)                                   
176300                                  TO W1-SRAD-KVSATRES                     
176400        ELSE                                                              
176500           COMPUTE W-HELP-REBEART = (W-MAX-KVBYGGB *                      
176600                                     SATG-SRAD-REANTPSA) + 0.9            
176700           MOVE W-HELP-REBEART TO W1-SRAD-REBEART                         
176800           MOVE W1-SRAD-REBEART TO W1-SRAD-KVSATRES                       
176900        END-IF                                                            
177000     END-IF                                                               
177100     MOVE ZERO                 TO W1-SRAD-KVSATROS                        
177200                                                                          
177300     ADD +1                    TO W1-KVRADER                              
177400     COMPUTE W1-VLORDNTO       = W1-VLORDNTO + (W1-SRAD-VLARTNTO          
177500                                             * W1-SRAD-REBEART)           
177600     COMPUTE W1-VKORDNTO       = W1-VKORDNTO + (W1-SRAD-VKARTNTO          
177700                                             * W1-SRAD-REBEART)           
177800                                                                          
177900     MOVE W1-SRAD-WDJ211       TO SATG-SRAD-WDJ211                        
178000     PERFORM IMS-REPL-SATG-SATG11                                         
178100     .                                                                    
178200     EJECT                                                                
178300****************************************************************          
178400*  HEBD-UPPD-ARTC11                                            *          
178500*  UPPDATERA ARTIKELREGISTRET MED DE NYA VÄRDENA (BYGGBAR).    *          
178600****************************************************************          
178700 HEBD-UPPD-ARTC11               SECTION.                                  
178800                                                                          
178900     PERFORM IMS-GHU-ARTC11                                               
179000     MOVE CLAG-KVEFRS        TO SPAR-CLAG-KVEFRS                          
179100     MOVE CLAG-KVLS          TO SPAR-CLAG-KVLS                            
179200     MOVE CLAG-IDANSK        TO WS-IDANSK                                 
179300                                                                          
179400     COMPUTE CLAG-KVEFRS    = CLAG-KVEFRS - W-ORIG-KVSATRES               
179500                                          + W1-SRAD-KVSATRES              
179600                                                                          
179700***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
179800                                                                          
179900     COMPUTE WS-KVEFRS-DIFF = SPAR-CLAG-KVEFRS - CLAG-KVEFRS              
180000     MOVE '-'                    TO LOGG-IDTECKEN-KVEFRS                  
180100     MOVE ' '                    TO LOGG-IDTECKEN-KVLS                    
180200     MOVE WS-KVEFRS-DIFF         TO LOGG-KVART-SALDO                      
180300     IF LOGG-KVART-SALDO > ZERO                                           
180400       PERFORM S09-SKAPA-SALDOLOGG                                        
180500     END-IF                                                               
180600                                                                          
180700     COMPUTE CLAG-KVLS     = CLAG-KVLS  + W-ORIG-REBEART                  
180800                                          - W1-SRAD-REBEART               
180900                                                                          
181000     COMPUTE WS-KVLS-DIFF = CLAG-KVLS - SPAR-CLAG-KVLS                    
181100     MOVE ' '                    TO LOGG-IDTECKEN-KVEFRS                  
181200     MOVE '+'                    TO LOGG-IDTECKEN-KVLS                    
181300     MOVE WS-KVLS-DIFF           TO LOGG-KVART-SALDO                      
181400     IF LOGG-KVART-SALDO > ZERO                                           
181500       PERFORM S09-SKAPA-SALDOLOGG                                        
181600     END-IF                                                               
181700                                                                          
181800     IF TAB-TRAFF-SW = JA                                                 
181900        COMPUTE CLAG-KVRESS =                                             
182000                CLAG-KVRESS + TAB-INL-ANTAL(TAB-IDX)                      
182100        COMPUTE WS-KVAVART  =  W-ORIG-REBEART                             
182200                            -  TAB-MID-REBEART(TAB-IDX)                   
182300        PERFORM S06-SKAPA-RYJ-TRANS-BRIST                                 
182400     ELSE                                                                 
182500                                                                          
182600        IF AND-TRAFF-SW = JA                                              
182700           IF AND-BRIST(AND-IDX)  = JA                                    
182800              COMPUTE CLAG-KVRESS = CLAG-KVRESS +                         
182900                                    AND-INL-ANTAL(AND-IDX)                
183000              COMPUTE WS-KVAVART  = W-ORIG-REBEART -                      
183100                                    AND-MID-REBEART(AND-IDX)              
183200              PERFORM S06-SKAPA-RYJ-TRANS-BRIST                           
183300           ELSE                                                           
183400              COMPUTE CLAG-KVRESS = CLAG-KVRESS +                         
183500                                    AND-INL-ANTAL(AND-IDX)                
183600           END-IF                                                         
183700        ELSE                                                              
183800           COMPUTE CLAG-KVRESS = CLAG-KVRESS + W-ORIG-KVSATRES            
183900                                             - W1-SRAD-KVSATRES           
184000        END-IF                                                            
184100     END-IF                                                               
184200                                                                          
184300     PERFORM IMS-REPL-ARTC11                                              
184400     .                                                                    
184500     EJECT                                                                
184600****************************************************************          
184700*  HEBE-SKAPA-PLOCKL-WDJ212                                    *          
184800*  NY PLOCKLISTA SKRIVS UT FÖR DEN "BYGGBARA" DELEN AV         *          
184900*  SATSORDER.                                                  *          
185000****************************************************************          
185100 HEBE-SKAPA-PLOCKL-WDJ212 SECTION.                                        
185200                                                                          
185300     MOVE '1'                  TO SATG-URAD-KDSATLI                       
185400                                                                          
185500     PERFORM IMS-GU-ARTC11                                                
185600*SVS                                                                      
185700     IF WS-SATG-SHUV-BEFT = +015                                          
185800     OR WS-SATG-SHUV-BEFT = +096                                          
185900       MOVE CLAG-ADLAGOMR      TO SATG-URAD-ADLAGOMR                      
186000       MOVE CLAG-ADGANG        TO SATG-URAD-ADGANG                        
186100       MOVE CLAG-ADPLATS       TO SATG-URAD-ADPLATS                       
186200     ELSE                                                                 
186300       IF CLAG-KVLS-SVS > 0                                               
186400         MOVE CLAG-ADLAGOMR-SVS TO SATG-URAD-ADLAGOMR                     
186500         MOVE CLAG-ADGANG-SVS   TO SATG-URAD-ADGANG                       
186600         MOVE CLAG-ADPLATS-SVS  TO SATG-URAD-ADPLATS                      
186700       ELSE                                                               
186800         MOVE CLAG-ADLAGOMR    TO SATG-URAD-ADLAGOMR                      
186900         MOVE CLAG-ADGANG      TO SATG-URAD-ADGANG                        
187000         MOVE CLAG-ADPLATS     TO SATG-URAD-ADPLATS                       
187100       END-IF                                                             
187200     END-IF                                                               
187300                                                                          
187400     MOVE W1-SRAD-IDARTNR      TO SATG-URAD-IDARTNR                       
187500                                                                          
187600     MOVE W1-SRAD-IDARTNR      TO W-IDARTNR                               
187700     MOVE MED-IDSKYLT          TO W-IDSKYLT                               
187800     PERFORM IMS-GU-BENA11                                                
187900     MOVE BENA-TEXT-BEART      TO SATG-URAD-BEART                         
188000                                                                          
188100     MOVE SPACE                TO SATG-URAD-FLSATBRI                      
188200     MOVE W1-SRAD-IDKONTO      TO SATG-URAD-IDKONTO                       
188300     MOVE W1-SRAD-IDANALYS     TO SATG-URAD-IDANALYS                      
188400     MOVE W1-SRAD-IDKST        TO SATG-URAD-IDKST                         
188500     MOVE W1-SRAD-KDPRODSL     TO SATG-URAD-KDPRODSL                      
188600     MOVE W1-SRAD-KDSATAND     TO SATG-URAD-KDSATAND                      
188700     MOVE W1-SRAD-KDSATKMB     TO SATG-URAD-KDSATKMB                      
188800     MOVE W1-SRAD-KDSORT       TO SATG-URAD-KDSORT                        
188900     MOVE W1-SRAD-PRARTSTD     TO SATG-URAD-PRARTSTD                      
189000     MOVE W1-SRAD-REANTPSA     TO SATG-URAD-REANTPSA                      
189100     MOVE W1-SRAD-REBEART      TO SATG-URAD-REBEART                       
189200     MOVE W1-SRAD-REKSIFFR     TO SATG-URAD-REKSIFFR                      
189300     MOVE W1-SRAD-VKARTNTO     TO SATG-URAD-VKARTNTO                      
189400     MOVE W1-SRAD-VLARTNTO     TO SATG-URAD-VLARTNTO                      
189500                                                                          
189600     PERFORM IMS-ISRT-WDJ212-SATG1                                        
189700     .                                                                    
189800     EJECT                                                                
189900****************************************************************          
190000*  HEBF-SKAPA-INLL-WDJ212                                      *          
190100*  INLÄGGNINGSLISTA SKAPAS FÖR DE INGÅENDE ARTIKLAR SOM SKALL  *          
190200*  LÄGGAS TILLBAKA UT I HYLLAN IGEN.                           *          
190300****************************************************************          
190400 HEBF-SKAPA-INLL-WDJ212 SECTION.                                          
190500                                                                          
190600     COMPUTE WS-JAMFOR-REBEART = W-ORIG-REBEART -                         
190700                                 W1-SRAD-REBEART                          
190800                                                                          
190900     IF (TAB-TRAFF-SW = NEJ AND AND-TRAFF-SW = NEJ AND                    
191000        WS-JAMFOR-REBEART > 0) OR                                         
191100        (TAB-TRAFF-SW = JA AND TAB-INL-ANTAL(TAB-IDX) > 0) OR             
191200        (AND-TRAFF-SW = JA AND AND-INL-ANTAL(AND-IDX) > 0)                
191300                                                                          
191400        MOVE W-BYGGB-IDORDNSB  TO W-IDORDNSB                              
191500        MOVE W-BYGGB-IDORDNSS  TO W-IDORDNSS                              
191600                                                                          
191700        MOVE '2'               TO SATG-URAD-KDSATLI                       
191800                                                                          
191900*SVS                                                                      
192000        IF WS-SATG-SHUV-BEFT = +015                                       
192100        OR WS-SATG-SHUV-BEFT = +096                                       
192200          MOVE CLAG-ADLAGOMR   TO SATG-URAD-ADLAGOMR                      
192300          MOVE CLAG-ADGANG     TO SATG-URAD-ADGANG                        
192400          MOVE CLAG-ADPLATS    TO SATG-URAD-ADPLATS                       
192500        ELSE                                                              
192600          IF CLAG-KVLS-SVS > 0                                            
192700            MOVE CLAG-ADLAGOMR-SVS TO SATG-URAD-ADLAGOMR                  
192800            MOVE CLAG-ADGANG-SVS   TO SATG-URAD-ADGANG                    
192900            MOVE CLAG-ADPLATS-SVS  TO SATG-URAD-ADPLATS                   
193000          ELSE                                                            
193100            MOVE CLAG-ADLAGOMR TO SATG-URAD-ADLAGOMR                      
193200            MOVE CLAG-ADGANG   TO SATG-URAD-ADGANG                        
193300            MOVE CLAG-ADPLATS  TO SATG-URAD-ADPLATS                       
193400          END-IF                                                          
193500        END-IF                                                            
193600                                                                          
193700        MOVE W1-SRAD-IDARTNR   TO SATG-URAD-IDARTNR                       
193800                                                                          
193900        MOVE BENA-TEXT-BEART   TO SATG-URAD-BEART                         
194000                                                                          
194100        MOVE SPACE             TO SATG-URAD-FLSATBRI                      
194200        MOVE W1-SRAD-IDKONTO   TO SATG-URAD-IDKONTO                       
194300        MOVE W1-SRAD-IDANALYS  TO SATG-URAD-IDANALYS                      
194400        MOVE W1-SRAD-IDKST     TO SATG-URAD-IDKST                         
194500        MOVE W1-SRAD-KDPRODSL  TO SATG-URAD-KDPRODSL                      
194600        MOVE W1-SRAD-KDSATAND  TO SATG-URAD-KDSATAND                      
194700        MOVE W1-SRAD-KDSATKMB  TO SATG-URAD-KDSATKMB                      
194800        MOVE W1-SRAD-KDSORT    TO SATG-URAD-KDSORT                        
194900        MOVE W1-SRAD-PRARTSTD  TO SATG-URAD-PRARTSTD                      
195000        MOVE W1-SRAD-REANTPSA  TO SATG-URAD-REANTPSA                      
195100        IF TAB-TRAFF-SW = JA                                              
195200           MOVE TAB-INL-ANTAL(TAB-IDX)                                    
195300                               TO SATG-URAD-REBEART                       
195400        ELSE                                                              
195500           IF AND-TRAFF-SW = JA                                           
195600              MOVE AND-INL-ANTAL(AND-IDX)                                 
195700                                  TO SATG-URAD-REBEART                    
195800           ELSE                                                           
195900              MOVE WS-JAMFOR-REBEART                                      
196000                                  TO SATG-URAD-REBEART                    
196100           END-IF                                                         
196200        END-IF                                                            
196300        MOVE W1-SRAD-REKSIFFR  TO SATG-URAD-REKSIFFR                      
196400        MOVE W1-SRAD-VKARTNTO  TO SATG-URAD-VKARTNTO                      
196500        MOVE W1-SRAD-VLARTNTO  TO SATG-URAD-VLARTNTO                      
196600        PERFORM IMS-ISRT-WDJ212-SATG1                                     
196700     END-IF                                                               
196800     .                                                                    
196900     EJECT                                                                
197000 HEBG-SKAPA-EJ-BYGGB-SATG11     SECTION.                                  
197100                                                                          
197200**   REBEART AVRUNDAS ALLTID UPPÅT TILL NÄRMASTE HELTAL                   
197300                                                                          
197400     MOVE W-EJ-BYGGB-IDORDNSB  TO W-IDORDNSB                              
197500     MOVE W-EJ-BYGGB-IDORDNSS  TO W-IDORDNSS                              
197600                                                                          
197700     MOVE SATG-SRAD-WDJ211     TO W2-SRAD-WDJ211                          
197800     MOVE NEJ                  TO W2-SRAD-FLSATUTS                        
197900                                                                          
198000     IF ((SATG-SRAD-KDSATAND = 'U' AND SATG-SRAD-REBEART = 0)             
198100         OR                                                               
198200         (SATG-SRAD-KDSATAND = 'E' AND SATG-SRAD-REBEART = 0)             
198300         OR                                                               
198400         (SATG-SRAD-KDSATAND = 'B'))                                      
198500         CONTINUE                                                         
198600     ELSE                                                                 
198700        IF TAB-TRAFF-SW = JA                                              
198800           COMPUTE W-HELP-REBEART = (W2-SHUV-KVBEART                      
198900                                  * W2-SRAD-REANTPSA) + 0.9               
199000           MOVE W-HELP-REBEART TO  W2-SRAD-REBEART                        
199100                                                                          
199200           MOVE TAB-INL-ANTAL(TAB-IDX) TO W2-SRAD-KVSATRES                
199300                                                                          
199400           COMPUTE W2-SRAD-KVSATROS = (TAB-W2-REBEART(TAB-IDX)            
199500                                    - TAB-INL-ANTAL(TAB-IDX))             
199600        ELSE                                                              
199700           IF AND-TRAFF-SW = JA                                           
199800              MOVE AND-INL-ANTAL(AND-IDX) TO W2-SRAD-KVSATRES             
199900              MOVE AND-W2-REBEART(AND-IDX) TO W2-SRAD-REBEART             
200000                                                                          
200100              COMPUTE W2-SRAD-KVSATROS = (AND-W2-REBEART(AND-IDX)         
200200                                       - AND-INL-ANTAL(AND-IDX))          
200300           ELSE                                                           
200400              COMPUTE W-HELP-REBEART = (W2-SHUV-KVBEART                   
200500                                     * W2-SRAD-REANTPSA) + 0.9            
200600              MOVE W-HELP-REBEART TO W2-SRAD-REBEART                      
200700                                                                          
200800              COMPUTE W2-SRAD-KVSATRES = (W-ORIG-KVSATRES -               
200900                                          W1-SRAD-KVSATRES)               
201000              MOVE ZERO             TO W2-SRAD-KVSATROS                   
201100           END-IF                                                         
201200        END-IF                                                            
201300                                                                          
201400**      BEROENDE PÅ AVRUNDNINGAR, KAN YTTERLIGARE RESERVATIONER/          
201500**      RESTNOTERINGAR BEHÖVA SKE.                                        
201600                                                                          
201700        COMPUTE WS-AVRUND   = (W2-SRAD-REBEART - W2-SRAD-KVSATRES         
201800                               - W2-SRAD-KVSATROS)                        
201900        IF WS-AVRUND        >  +0                                         
202000           PERFORM HEBGA-RESERVERA-ARTC11                                 
202100        END-IF                                                            
202200                                                                          
202300        IF W2-SRAD-KVSATROS >  +0                                         
202400           PERFORM HEBGB-RESTNOTERA-ARTC11                                
202500        END-IF                                                            
202600     END-IF                                                               
202700                                                                          
202800     MOVE W2-SRAD-WDJ211       TO SATG-SRAD-WDJ211                        
202900     PERFORM IMS-ISRT-SATG1-SATG11                                        
203000     .                                                                    
203100     EJECT                                                                
203200****************************************************************          
203300*  HEBGA-RESERVERA-ARTC11                                      *          
203400*  ÖVERSTIGER BESTÄLLT ANTAL SUMMAN AV RESERVERAT OCH REST-    *          
203500*  NOTERAT FÖRSÖKER VI UPPDATERA RESERVERAT, ANNARS UPPDATE-   *          
203600*  RAS RESTNOTERAT (SEKTION HEBGB OCH HEBH).                   *          
203700****************************************************************          
203800 HEBGA-RESERVERA-ARTC11         SECTION.                                  
203900                                                                          
204000     MOVE ZERO                 TO WS-DISPLS                               
204100     PERFORM IMS-GHU-ARTC11                                               
204200     PERFORM IMS-GU-ARTM01                                                
204300                                                                          
204400     IF SEGMENT-FINNS                                                     
204500        COMPUTE WS-DISPLS = CLAG-KVLS - CLAG-KVRESS -                     
204600                            CLAG-KVSPANT - CLAG-KVUTRS -                  
204700                            ARTM-ART-KVPREAVB-DAG -                       
204800                            ARTM-ART-KVPREAVB-VOR                         
204900     ELSE                                                                 
205000        COMPUTE WS-DISPLS = CLAG-KVLS - CLAG-KVRESS -                     
205100                            CLAG-KVSPANT - CLAG-KVUTRS                    
205200     END-IF                                                               
205300                                                                          
205400     IF WS-DISPLS > 0                                                     
205500        COMPUTE CLAG-KVRESS = CLAG-KVRESS + WS-AVRUND                     
205600        ADD WS-AVRUND      TO W2-SRAD-KVSATRES                            
205700        PERFORM IMS-REPL-ARTC11                                           
205800     ELSE                                                                 
205900        ADD WS-AVRUND      TO W2-SRAD-KVSATROS                            
206000     END-IF                                                               
206100     .                                                                    
206200     EJECT                                                                
206300 HEBGB-RESTNOTERA-ARTC11        SECTION.                                  
206400                                                                          
206500     MOVE W2-SRAD-IDARTNR      TO W-IDARTNR                               
206600     PERFORM IMS-GHU-ARTC11                                               
206700                                                                          
206800     PERFORM S08-EV-LARM-2191-ANSKAFFN                                    
206900     COMPUTE CLAG-KVROS   = CLAG-KVROS + W2-SRAD-KVSATROS                 
207000                                                                          
207100     PERFORM IMS-REPL-ARTC11                                              
207200     .                                                                    
207300     EJECT                                                                
207400****************************************************************          
207500*  HEBH-SKAPA-ORDP01                                           *          
207600*  SKAPA RESTORDER FÖR DEN EJ BYGGBARA SATSORDERRADEN (W2).    *          
207700****************************************************************          
207800 HEBH-SKAPA-ORDP01          SECTION.                                      
207900                                                                          
208000     MOVE W2-SRAD-IDARTNR        TO ORDP-RAD-IDARTNR                      
208100     MOVE '2'                    TO ORDP-RAD-KDSTARAD                     
208200     MOVE '4'                    TO ORDP-RAD-KDROO                        
208300     MOVE W2-SRAD-KVSATROS       TO WS-RO-ANTAL                           
208400                                                                          
208500     PERFORM S01-SKAPA-RESTORDER                                          
208600     .                                                                    
208700     EJECT                                                                
208800 HEC-UPPD-BYGGBAR-SATG01        SECTION.                                  
208900                                                                          
209000     MOVE W1-SHUV-IDORDNSB     TO  W-IDORDNSB                             
209100     MOVE W1-SHUV-IDORDNSS     TO  W-IDORDNSS                             
209200     PERFORM IMS-GHU-WDJ201-SATG                                          
209300                                                                          
209400     MOVE JA                   TO  W1-SHUV-FLBYGGB                        
209500     MOVE W-MAX-KVBYGGB        TO  W1-SHUV-KVBEART                        
209600                                   W1-SHUV-KVBYGGB                        
209700                                                                          
209800     COMPUTE  W1-SHUV-VLORDNTO ROUNDED = W1-VLORDNTO / 1000000            
209900     COMPUTE  W1-SHUV-VKORDNTO ROUNDED = W1-VKORDNTO                      
210000                                                                          
210100                                                                          
210200     PERFORM S05-BERAKNA-PTID                                             
210300     MOVE '5'                  TO W1-SHUV-KDSATPLK                        
210400                                                                          
210500     MOVE W1-SHUV-WDJ201       TO  SATG-SHUV-WDJ201                       
210600     PERFORM IMS-REPL-WDJ201-SATG                                         
210700     .                                                                    
210800     EJECT                                                                
210900 HED-UPPD-EJ-BYGGBAR-SATG01     SECTION.                                  
211000                                                                          
211100     MOVE W-EJ-BYGGB-IDORDNSB  TO  W-IDORDNSB                             
211200     MOVE W-EJ-BYGGB-IDORDNSS  TO  W-IDORDNSS                             
211300                                                                          
211400     PERFORM IMS-GU-WDJ201-SATG1                                          
211500                                                                          
211600     PERFORM HEDA-UPPD-NY-EJ-BYGGB                                        
211700                                                                          
211800     .                                                                    
211900     EJECT                                                                
212000 HEDA-UPPD-NY-EJ-BYGGB          SECTION.                                  
212100                                                                          
212200     PERFORM IMS-GHU-WDJ201-SATG1                                         
212300     MOVE SATG-SHUV-WDJ201     TO W2-SHUV-WDJ201                          
212400     MOVE ZERO                 TO W2-SHUV-KVBYGGB                         
212500                                  W2-SHUV-IDPRODNR                        
212600     MOVE '5'                  TO W2-SHUV-KDSATPLK                        
212700                                                                          
212800     MOVE W2-SHUV-WDJ201       TO SATG-SHUV-WDJ201                        
212900     PERFORM IMS-REPL-SATG1-SATG01                                        
213000                                                                          
213100     .                                                                    
213200     EJECT                                                                
213300****************************************************************          
213400*  HF-DELA-ORDER-ANNULERA                                      *          
213500*   FALL2: MAN HITTAR MER ÄN 90%. DÅ BYGGER MAN DET SOM GÅR    *          
213600*          ATT BYGGA SAMT ATT MAN ANNULLERAR RESTEN AV ORDERN, *          
213700*          ELLER OM ORDERN REDAN ÄR DELAD 9 GÅNGER REDAN.      *          
213800****************************************************************          
213900 HF-DELA-ORDER-ANNULLERA SECTION.                                         
214000                                                                          
214100     MOVE ZERO                 TO  W1-VLORDNTO                            
214200                                   W1-VKORDNTO                            
214300                                                                          
214400     PERFORM HFA-UPPD-ING-ART                                             
214500     PERFORM HFB-UPPD-SATS-ART                                            
214600     PERFORM HFC-UPPDATERA-LEVPLAN                                        
214700     IF LEVP-KDSVAR            = '1'                                      
214800         MOVE 'FEL VID UPD AV LEVPLAN VID DELNING '                       
214900         TO FELTEXT                                                       
215000         CALL ABEND USING RKOD-ABEND                                      
215100     END-IF                                                               
215200     .                                                                    
215300     EJECT                                                                
215400 HFA-UPPD-ING-ART SECTION.                                                
215500                                                                          
215600     MOVE NEJ                  TO TAB-TRAFF-SW                            
215700     MOVE NEJ                  TO AND-TRAFF-SW                            
215800     PERFORM IMS-GHNP-WDJ211-SATG                                         
215900                                                                          
216000     MOVE ZERO                 TO W1-KVRADER                              
216100     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
216200                   SEGMENT-SLUT                                           
216300                                                                          
216400        IF ((SATG-SRAD-KDSATAND = 'U' AND SATG-SRAD-REBEART = 0)          
216500            OR                                                            
216600            (SATG-SRAD-KDSATAND = 'E' AND SATG-SRAD-REBEART = 0)          
216700            OR                                                            
216800            (SATG-SRAD-KDSATAND = 'B'))                                   
216900            CONTINUE                                                      
217000        ELSE                                                              
217100           MOVE SATG-SRAD-WDJ211 TO W1-SRAD-WDJ211                        
217200           MOVE W1-SRAD-IDARTNR   TO W-IDARTNR                            
217300                                 IN W-IDARTNR-X                           
217400           MOVE SATG-SRAD-REBEART   TO W-ORIG-REBEART                     
217500           MOVE SATG-SRAD-KVSATRES TO W-ORIG-KVSATRES                     
217600           IF SATG-SRAD-KDSATKMB = SPACE                                  
217700              PERFORM HFAA-KOLLA-TRAFF-I-TAB                              
217800           ELSE                                                           
217900              PERFORM HFAB-KOLLA-TRAFF-I-ANDRINGSTAB                      
218000           END-IF                                                         
218100           PERFORM HFAC-UPPD-SATG11                                       
218200           PERFORM HFAD-UPPD-ARTC11                                       
218300           IF ((W1-SRAD-KDSATAND = SPACE) OR                              
218400               (W1-SRAD-KDSATAND NOT = SPACE AND                          
218500                W1-SRAD-REBEART > 0))                                     
218600              PERFORM HFAE-SKAPA-PLOCKL-WDJ212                            
218700           END-IF                                                         
218800           PERFORM HFAF-SKAPA-INLL-WDJ212                                 
218900        END-IF                                                            
219000                                                                          
219100        PERFORM IMS-GHNP-WDJ211-SATG                                      
219200        MOVE NEJ               TO TAB-TRAFF-SW                            
219300        MOVE NEJ               TO AND-TRAFF-SW                            
219400     END-PERFORM                                                          
219500     .                                                                    
219600     EJECT                                                                
219700****************************************************************          
219800*  HFAA-KOLLA-TRAFF-I-TAB                                      *          
219900*  GÅ IGENOM INTERNTABELLEN, FÖR ATT SE OM DEN ING.ARTIKEL     *          
220000*  SOM JAG LÄST I WDJ2 ÄVEN FINNS SPARAD I TABELLEN.           *          
220100****************************************************************          
220200 HFAA-KOLLA-TRAFF-I-TAB SECTION.                                          
220300*                                                                         
220400     MOVE NEJ                  TO TAB-TRAFF-SW                            
220500     MOVE +1                   TO TAB-IDX                                 
220600                                                                          
220700     PERFORM UNTIL TAB-IDX > 33 OR                                        
220800                   TAB-IDARTNR(TAB-IDX) = ZERO OR                         
220900                   TAB-TRAFF-SW = JA                                      
221000                                                                          
221100        IF SATG-SRAD-IDARTNR = TAB-IDARTNR(TAB-IDX)                       
221200           MOVE JA             TO TAB-TRAFF-SW                            
221300           COMPUTE TAB-INL-ANTAL(TAB-IDX) =                               
221400                                (TAB-MID-REBEART(TAB-IDX) -               
221500                                 TAB-NY-REBEART(TAB-IDX))                 
221600        ELSE                                                              
221700           ADD +1              TO TAB-IDX                                 
221800        END-IF                                                            
221900                                                                          
222000     END-PERFORM                                                          
222100     .                                                                    
222200     EJECT                                                                
222300****************************************************************          
222400*  HFAB-KOLLA-TRAFF-I-ANDRINGSTAB                              *          
222500*  GÅ IGENOM ÄNDRINGSTABELLEN, FÖR ATT HUR DEN ING.ARTIKELN    *          
222600*  MED KOMBINATIONSKOD SKALL HANTERAS.                         *          
222700****************************************************************          
222800 HFAB-KOLLA-TRAFF-I-ANDRINGSTAB SECTION.                                  
222900                                                                          
223000     MOVE NEJ                  TO AND-TRAFF-SW                            
223100     MOVE +1                   TO AND-IDX                                 
223200                                                                          
223300     PERFORM UNTIL AND-IDX > 16 OR                                        
223400                   AND-IDARTNR(AND-IDX) = ZERO OR                         
223500                   AND-TRAFF-SW = JA                                      
223600                                                                          
223700        IF SATG-SRAD-IDARTNR = AND-IDARTNR(AND-IDX)                       
223800           MOVE JA             TO AND-TRAFF-SW                            
223900           COMPUTE AND-INL-ANTAL(AND-IDX) =                               
224000                                (AND-MID-REBEART(AND-IDX) -               
224100                                 AND-NY-REBEART(AND-IDX))                 
224200        ELSE                                                              
224300           ADD +1              TO AND-IDX                                 
224400        END-IF                                                            
224500                                                                          
224600     END-PERFORM                                                          
224700     .                                                                    
224800     EJECT                                                                
224900 HFAC-UPPD-SATG11 SECTION.                                                
225000                                                                          
225100     IF TAB-TRAFF-SW = JA                                                 
225200        MOVE TAB-NY-REBEART(TAB-IDX)                                      
225300                               TO W1-SRAD-REBEART                         
225400        MOVE TAB-NY-REBEART(TAB-IDX)                                      
225500                               TO W1-SRAD-KVSATRES                        
225600     ELSE                                                                 
225700        IF AND-TRAFF-SW = JA                                              
225800           MOVE AND-NY-REBEART(AND-IDX)                                   
225900                                  TO W1-SRAD-REBEART                      
226000           MOVE AND-NY-REBEART(AND-IDX)                                   
226100                                  TO W1-SRAD-KVSATRES                     
226200        ELSE                                                              
226300           COMPUTE W-HELP-REBEART = (W-MAX-KVBYGGB *                      
226400                                     SATG-SRAD-REANTPSA) + 0.9            
226500           MOVE W-HELP-REBEART TO W1-SRAD-REBEART                         
226600           MOVE W1-SRAD-REBEART TO W1-SRAD-KVSATRES                       
226700        END-IF                                                            
226800     END-IF                                                               
226900     MOVE ZERO                 TO W1-SRAD-KVSATROS                        
227000                                                                          
227100     ADD +1                    TO W1-KVRADER                              
227200     COMPUTE W1-VLORDNTO       = W1-VLORDNTO + (W1-SRAD-VLARTNTO          
227300                                             * W1-SRAD-REBEART)           
227400     COMPUTE W1-VKORDNTO       = W1-VKORDNTO + (W1-SRAD-VKARTNTO          
227500                                             * W1-SRAD-REBEART)           
227600                                                                          
227700     MOVE W1-SRAD-WDJ211       TO SATG-SRAD-WDJ211                        
227800     PERFORM IMS-REPL-SATG-SATG11                                         
227900     .                                                                    
228000     EJECT                                                                
228100 HFAD-UPPD-ARTC11               SECTION.                                  
228200                                                                          
228300     PERFORM IMS-GHU-ARTC11                                               
228400                                                                          
228500     MOVE CLAG-KVEFRS        TO SPAR-CLAG-KVEFRS                          
228600     MOVE CLAG-KVLS          TO SPAR-CLAG-KVLS                            
228700                                                                          
228800     COMPUTE CLAG-KVEFRS     =  CLAG-KVEFRS - W-ORIG-KVSATRES             
228900                                            + W1-SRAD-KVSATRES            
229000                                                                          
229100***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
229200                                                                          
229300     COMPUTE WS-KVEFRS-DIFF = SPAR-CLAG-KVEFRS - CLAG-KVEFRS              
229400     MOVE '-'                    TO LOGG-IDTECKEN-KVEFRS                  
229500     MOVE ' '                    TO LOGG-IDTECKEN-KVLS                    
229600     MOVE WS-KVEFRS-DIFF         TO LOGG-KVART-SALDO                      
229700     IF LOGG-KVART-SALDO > ZERO                                           
229800       PERFORM S09-SKAPA-SALDOLOGG                                        
229900     END-IF                                                               
230000                                                                          
230100     COMPUTE CLAG-KVLS       =  CLAG-KVLS   + W-ORIG-REBEART              
230200                                            - W1-SRAD-REBEART             
230300                                                                          
230400     COMPUTE WS-KVLS-DIFF = CLAG-KVLS - SPAR-CLAG-KVLS                    
230500     MOVE ' '                    TO LOGG-IDTECKEN-KVEFRS                  
230600     MOVE '+'                    TO LOGG-IDTECKEN-KVLS                    
230700     MOVE WS-KVLS-DIFF           TO LOGG-KVART-SALDO                      
230800     IF LOGG-KVART-SALDO > ZERO                                           
230900       PERFORM S09-SKAPA-SALDOLOGG                                        
231000     END-IF                                                               
231100                                                                          
231200                                                                          
231300     IF TAB-TRAFF-SW = JA                                                 
231400        COMPUTE WS-KVAVART  =  W-ORIG-REBEART                             
231500                            -  TAB-MID-REBEART(TAB-IDX)                   
231600        PERFORM S06-SKAPA-RYJ-TRANS-BRIST                                 
231700        IF TAB-INL-ANTAL(TAB-IDX) > 0                                     
231800           MOVE TAB-INL-ANTAL(TAB-IDX) TO WS-KVANNANT                     
231900           PERFORM S07-SKAPA-RYJ-TRANS-ANNULL                             
232000        END-IF                                                            
232100     ELSE                                                                 
232200                                                                          
232300        IF AND-TRAFF-SW = JA                                              
232400           IF AND-BRIST(AND-IDX)  = JA                                    
232500              COMPUTE WS-KVAVART  = W-ORIG-REBEART -                      
232600                                    AND-MID-REBEART(AND-IDX)              
232700              PERFORM S06-SKAPA-RYJ-TRANS-BRIST                           
232800           END-IF                                                         
232900           IF AND-INL-ANTAL(AND-IDX) > 0                                  
233000              MOVE AND-INL-ANTAL(AND-IDX) TO WS-KVANNANT                  
233100              PERFORM S07-SKAPA-RYJ-TRANS-ANNULL                          
233200           END-IF                                                         
233300        ELSE                                                              
233400           COMPUTE WS-KVANNANT = W-ORIG-REBEART                           
233500                               - W1-SRAD-REBEART                          
233600           PERFORM S07-SKAPA-RYJ-TRANS-ANNULL                             
233700        END-IF                                                            
233800     END-IF                                                               
233900                                                                          
234000     PERFORM IMS-REPL-ARTC11                                              
234100     .                                                                    
234200     EJECT                                                                
234300****************************************************************          
234400*  HFAE-SKAPA-PLOCKL-WDJ212                                               
234500*  NY PLOCKLISTA SKRIVS UT FÖR DEN "BYGGBARA" DELEN AV         *          
234600*  SATSORDER.                                                  *          
234700****************************************************************          
234800 HFAE-SKAPA-PLOCKL-WDJ212 SECTION.                                        
234900                                                                          
235000     MOVE '1'                  TO SATG-URAD-KDSATLI                       
235100                                                                          
235200     PERFORM IMS-GU-ARTC11                                                
235300*SVS                                                                      
235400     IF WS-SATG-SHUV-BEFT = +015                                          
235500     OR WS-SATG-SHUV-BEFT = +096                                          
235600       MOVE CLAG-ADLAGOMR      TO SATG-URAD-ADLAGOMR                      
235700       MOVE CLAG-ADGANG        TO SATG-URAD-ADGANG                        
235800       MOVE CLAG-ADPLATS       TO SATG-URAD-ADPLATS                       
235900     ELSE                                                                 
236000       IF CLAG-KVLS-SVS > 0                                               
236100         MOVE CLAG-ADLAGOMR-SVS TO SATG-URAD-ADLAGOMR                     
236200         MOVE CLAG-ADGANG-SVS   TO SATG-URAD-ADGANG                       
236300         MOVE CLAG-ADPLATS-SVS  TO SATG-URAD-ADPLATS                      
236400       ELSE                                                               
236500         MOVE CLAG-ADLAGOMR    TO SATG-URAD-ADLAGOMR                      
236600         MOVE CLAG-ADGANG      TO SATG-URAD-ADGANG                        
236700         MOVE CLAG-ADPLATS     TO SATG-URAD-ADPLATS                       
236800       END-IF                                                             
236900     END-IF                                                               
237000                                                                          
237100     MOVE W1-SRAD-IDARTNR      TO SATG-URAD-IDARTNR                       
237200                                                                          
237300     MOVE W1-SRAD-IDARTNR      TO W-IDARTNR                               
237400     MOVE MED-IDSKYLT          TO W-IDSKYLT                               
237500     PERFORM IMS-GU-BENA11                                                
237600     MOVE BENA-TEXT-BEART      TO SATG-URAD-BEART                         
237700                                                                          
237800     MOVE SPACE                TO SATG-URAD-FLSATBRI                      
237900     MOVE W1-SRAD-IDKONTO      TO SATG-URAD-IDKONTO                       
238000     MOVE W1-SRAD-IDANALYS     TO SATG-URAD-IDANALYS                      
238100     MOVE W1-SRAD-IDKST        TO SATG-URAD-IDKST                         
238200     MOVE W1-SRAD-KDPRODSL     TO SATG-URAD-KDPRODSL                      
238300     MOVE W1-SRAD-KDSATAND     TO SATG-URAD-KDSATAND                      
238400     MOVE W1-SRAD-KDSATKMB     TO SATG-URAD-KDSATKMB                      
238500     MOVE W1-SRAD-KDSORT       TO SATG-URAD-KDSORT                        
238600     MOVE W1-SRAD-PRARTSTD     TO SATG-URAD-PRARTSTD                      
238700     MOVE W1-SRAD-REANTPSA     TO SATG-URAD-REANTPSA                      
238800     MOVE W1-SRAD-REBEART      TO SATG-URAD-REBEART                       
238900     MOVE W1-SRAD-REKSIFFR     TO SATG-URAD-REKSIFFR                      
239000     MOVE W1-SRAD-VKARTNTO     TO SATG-URAD-VKARTNTO                      
239100     MOVE W1-SRAD-VLARTNTO     TO SATG-URAD-VLARTNTO                      
239200     PERFORM IMS-ISRT-WDJ212-SATG1                                        
239300     .                                                                    
239400     EJECT                                                                
239500****************************************************************          
239600*  HFAF-SKAPA-INLL-WDJ212                                      *          
239700*  INLÄGGNINGSLISTA SKAPAS FÖR DE INGÅENDE ARTIKLAR SOM SKALL  *          
239800*  LÄGGAS TILLBAKA UT I HYLLAN IGEN.                           *          
239900****************************************************************          
240000 HFAF-SKAPA-INLL-WDJ212 SECTION.                                          
240100                                                                          
240200     COMPUTE WS-JAMFOR-REBEART = W-ORIG-REBEART -                         
240300                                 W1-SRAD-REBEART                          
240400                                                                          
240500     IF (TAB-TRAFF-SW = NEJ AND AND-TRAFF-SW = NEJ AND                    
240600        WS-JAMFOR-REBEART > 0) OR                                         
240700        (TAB-TRAFF-SW = JA AND TAB-INL-ANTAL(TAB-IDX) > 0) OR             
240800        (AND-TRAFF-SW = JA AND AND-INL-ANTAL(AND-IDX) > 0)                
240900                                                                          
241000        MOVE '2'               TO SATG-URAD-KDSATLI                       
241100                                                                          
241200*SVS                                                                      
241300        IF WS-SATG-SHUV-BEFT = +015                                       
241400        OR WS-SATG-SHUV-BEFT = +096                                       
241500          MOVE CLAG-ADLAGOMR   TO SATG-URAD-ADLAGOMR                      
241600          MOVE CLAG-ADGANG     TO SATG-URAD-ADGANG                        
241700          MOVE CLAG-ADPLATS    TO SATG-URAD-ADPLATS                       
241800        ELSE                                                              
241900          IF CLAG-KVLS-SVS > 0                                            
242000            MOVE CLAG-ADLAGOMR-SVS TO SATG-URAD-ADLAGOMR                  
242100            MOVE CLAG-ADGANG-SVS   TO SATG-URAD-ADGANG                    
242200            MOVE CLAG-ADPLATS-SVS  TO SATG-URAD-ADPLATS                   
242300          ELSE                                                            
242400            MOVE CLAG-ADLAGOMR TO SATG-URAD-ADLAGOMR                      
242500            MOVE CLAG-ADGANG   TO SATG-URAD-ADGANG                        
242600            MOVE CLAG-ADPLATS  TO SATG-URAD-ADPLATS                       
242700          END-IF                                                          
242800        END-IF                                                            
242900                                                                          
243000        MOVE W1-SRAD-IDARTNR   TO SATG-URAD-IDARTNR                       
243100                                                                          
243200        MOVE BENA-TEXT-BEART   TO SATG-URAD-BEART                         
243300                                                                          
243400        MOVE SPACE             TO SATG-URAD-FLSATBRI                      
243500        MOVE W1-SRAD-IDKONTO   TO SATG-URAD-IDKONTO                       
243600        MOVE W1-SRAD-IDANALYS  TO SATG-URAD-IDANALYS                      
243700        MOVE W1-SRAD-IDKST     TO SATG-URAD-IDKST                         
243800        MOVE W1-SRAD-KDPRODSL  TO SATG-URAD-KDPRODSL                      
243900        MOVE W1-SRAD-KDSATAND  TO SATG-URAD-KDSATAND                      
244000        MOVE W1-SRAD-KDSATKMB  TO SATG-URAD-KDSATKMB                      
244100        MOVE W1-SRAD-KDSORT    TO SATG-URAD-KDSORT                        
244200        MOVE W1-SRAD-PRARTSTD  TO SATG-URAD-PRARTSTD                      
244300        MOVE W1-SRAD-REANTPSA  TO SATG-URAD-REANTPSA                      
244400        IF TAB-TRAFF-SW = JA                                              
244500           MOVE TAB-INL-ANTAL(TAB-IDX)                                    
244600                               TO SATG-URAD-REBEART                       
244700        ELSE                                                              
244800           IF AND-TRAFF-SW = JA                                           
244900              MOVE AND-INL-ANTAL(AND-IDX)                                 
245000                               TO SATG-URAD-REBEART                       
245100           ELSE                                                           
245200              MOVE WS-JAMFOR-REBEART                                      
245300                               TO SATG-URAD-REBEART                       
245400           END-IF                                                         
245500        END-IF                                                            
245600        MOVE W1-SRAD-REKSIFFR  TO SATG-URAD-REKSIFFR                      
245700        MOVE W1-SRAD-VKARTNTO  TO SATG-URAD-VKARTNTO                      
245800        MOVE W1-SRAD-VLARTNTO  TO SATG-URAD-VLARTNTO                      
245900        PERFORM IMS-ISRT-WDJ212-SATG1                                     
246000     END-IF                                                               
246100     .                                                                    
246200     EJECT                                                                
246300 HFB-UPPD-SATS-ART SECTION.                                               
246400                                                                          
246500     PERFORM IMS-GHU-WDJ201-SATG                                          
246600     MOVE SATG-SHUV-WDJ201     TO  W1-SHUV-WDJ201                         
246700     COMPUTE W-KVANNANT        =   W1-SHUV-KVBEART -                      
246800                                   W-MAX-KVBYGGB                          
246900                                                                          
247000     MOVE JA                   TO  W1-SHUV-FLBYGGB                        
247100     MOVE W-MAX-KVBYGGB        TO  W1-SHUV-KVBEART                        
247200                                   W1-SHUV-KVBYGGB                        
247300                                                                          
247400     COMPUTE  W1-SHUV-VLORDNTO ROUNDED = W1-VLORDNTO / 1000000            
247500     COMPUTE  W1-SHUV-VKORDNTO ROUNDED = W1-VKORDNTO                      
247600                                                                          
247700     PERFORM S05-BERAKNA-PTID                                             
247800     MOVE '5'                  TO  W1-SHUV-KDSATPLK                       
247900                                                                          
248000     MOVE W1-SHUV-WDJ201       TO  SATG-SHUV-WDJ201                       
248100     PERFORM IMS-REPL-WDJ201-SATG                                         
248200     .                                                                    
248300     EJECT                                                                
248400 HFC-UPPDATERA-LEVPLAN          SECTION.                                  
248500                                                                          
248600     MOVE 'SATS'               TO  LEVP-IDSYSTEM                          
248700     MOVE SATG-SHUV-IDARTNR    TO  LEVP-IDARTNR                           
248800     MOVE SATG-SHUV-IDLEVNR    TO  LEVP-IDLEVNR                           
248900     MOVE SATG-SHUV-IDORDNSB   TO  LEVP-IDORDNSB                          
249000     MOVE +1                   TO  LEVP-KDCLAGER                          
249100     MOVE SATG-SHUV-KVBEART    TO  LEVP-KVBEART                           
249200     MOVE W-KVANNANT           TO  LEVP-KVANNANT                          
249300     MOVE SATG-SHUV-TIBEGPAC   TO  LEVP-TIBEGPAC                          
249400     MOVE SATG-SHUV-DAREGDAT (3:6) TO  LEVP-TIREGDAT                      
249500     MOVE SPACE                TO  LEVP-KDSVAR                            
249600                                                                          
249700     CALL W215LEVP USING LEVP-W215LEVP ARTC2-PCB INLB1-PCB                
249800                                       INLB2-PCB XXBM-PCB                 
249900                                       XXBW-PCB                           
250000                                                                          
250100     .                                                                    
250200     EJECT                                                                
250300****************************************************************          
250400*  HG-LAGG-TILLBAKA-ORDER                                      *          
250500*  FALL3: EN INGÅENDE ARTIKEL SAKNAS HELT. HELA ORDERN GÖRS    *          
250600*         "EJ BYGGBAR" OCH LÄGGS TILLBAKA TILL SATSORDERKÖN    *          
250700*         IGEN.                                                *          
250800****************************************************************          
250900 HG-LAGG-TILLBAKA-ORDER SECTION.                                          
251000                                                                          
251100     PERFORM HGA-UPPD-ING-ART                                             
251200     PERFORM HGB-UPPD-SATS-ART                                            
251300     .                                                                    
251400     EJECT                                                                
251500 HGA-UPPD-ING-ART SECTION.                                                
251600                                                                          
251700     MOVE NEJ                  TO TAB-TRAFF-SW                            
251800     MOVE +1                   TO TAB-IDX                                 
251900     MOVE +1                   TO AND-IDX                                 
252000                                                                          
252100     PERFORM IMS-GHNP-WDJ211-SATG                                         
252200                                                                          
252300     PERFORM UNTIL SEGMENT-SLUT OR                                        
252400                   SEGMENT-SAKNAS                                         
252500                                                                          
252600         IF ((SATG-SRAD-KDSATAND = 'U' AND SATG-SRAD-REBEART = 0)         
252700             OR                                                           
252800             (SATG-SRAD-KDSATAND = 'E' AND SATG-SRAD-REBEART = 0)         
252900             OR                                                           
253000             (SATG-SRAD-KDSATAND = 'B'))                                  
253100            CONTINUE                                                      
253200         ELSE                                                             
253300            MOVE SATG-SRAD-WDJ211 TO W2-SRAD-WDJ211                       
253400            MOVE W2-SRAD-IDARTNR  TO W-IDARTNR                            
253500                                  IN W-IDARTNR-X                          
253600            MOVE SATG-SRAD-REBEART  TO W-ORIG-REBEART                     
253700            MOVE SATG-SRAD-KVSATRES TO W-ORIG-KVSATRES                    
253800            IF SATG-SRAD-KDSATKMB = SPACE                                 
253900               PERFORM HGAA-KOLLA-TRAFF-I-TAB                             
254000            ELSE                                                          
254100               PERFORM HGAB-KOLLA-TRAFF-I-ANDRINGSTAB                     
254200            END-IF                                                        
254300                                                                          
254400            PERFORM HGAC-UPPD-SATG11                                      
254500            PERFORM HGAD-UPPD-ARTC11                                      
254600            PERFORM HGAF-SKAPA-INLL-WDJ212                                
254700                                                                          
254800            IF W2-SRAD-KVSATROS > 0                                       
254900               PERFORM HGAH-SKAPA-ORDP01                                  
255000            END-IF                                                        
255100         END-IF                                                           
255200                                                                          
255300         PERFORM IMS-GHNP-WDJ211-SATG                                     
255400         MOVE NEJ              TO TAB-TRAFF-SW                            
255500         MOVE NEJ              TO AND-TRAFF-SW                            
255600     END-PERFORM                                                          
255700     .                                                                    
255800     EJECT                                                                
255900****************************************************************          
256000*  HGAA-KOLLA-TRAFF-I-TAB                                      *          
256100*  GÅ IGENOM INTERNTABELLEN, FÖR ATT SE OM DEN ING.ARTIKEL     *          
256200*  SOM JAG LÄST I WDJ2 ÄVEN FINNS SPARAD I TABELLEN.           *          
256300*  BYGBART ANTAL ÄR HÄR = 0.                                   *          
256400****************************************************************          
256500 HGAA-KOLLA-TRAFF-I-TAB SECTION.                                          
256600                                                                          
256700     MOVE NEJ                  TO TAB-TRAFF-SW                            
256800     MOVE +1                   TO TAB-IDX                                 
256900                                                                          
257000     PERFORM UNTIL TAB-IDX > 33 OR                                        
257100                   TAB-IDARTNR(TAB-IDX) = ZERO OR                         
257200                   TAB-TRAFF-SW = JA                                      
257300                                                                          
257400        IF SATG-SRAD-IDARTNR = TAB-IDARTNR(TAB-IDX)                       
257500           MOVE JA             TO TAB-TRAFF-SW                            
257600           COMPUTE TAB-INL-ANTAL(TAB-IDX) =                               
257700                                (TAB-MID-REBEART(TAB-IDX) -               
257800                                 TAB-NY-REBEART(TAB-IDX))                 
257900        ELSE                                                              
258000           ADD +1              TO TAB-IDX                                 
258100        END-IF                                                            
258200                                                                          
258300     END-PERFORM                                                          
258400     .                                                                    
258500     EJECT                                                                
258600****************************************************************          
258700*  HGAB-KOLLA-TRAFF-I-ANDRINGSTAB                              *          
258800*  GÅ IGENOM ÄNDRINGSTABELLEN, FÖR ATT HUR DEN ING.ARTIKELN    *          
258900*  MED KOMBINATIONSKOD SKALL HANTERAS.                         *          
259000****************************************************************          
259100 HGAB-KOLLA-TRAFF-I-ANDRINGSTAB SECTION.                                  
259200                                                                          
259300     MOVE NEJ                  TO AND-TRAFF-SW                            
259400     MOVE +1                   TO AND-IDX                                 
259500                                                                          
259600     PERFORM UNTIL AND-IDX > 16 OR                                        
259700                   AND-IDARTNR(AND-IDX) = ZERO OR                         
259800                   AND-TRAFF-SW = JA                                      
259900                                                                          
260000        IF SATG-SRAD-IDARTNR = AND-IDARTNR(AND-IDX)                       
260100           MOVE JA             TO AND-TRAFF-SW                            
260200           COMPUTE AND-INL-ANTAL(AND-IDX) =                               
260300                                (AND-MID-REBEART(AND-IDX) -               
260400                                 AND-NY-REBEART(AND-IDX))                 
260500           COMPUTE AND-INL-ANTAL(AND-IDX) =                               
260600                                (AND-MID-REBEART(AND-IDX) -               
260700                                 AND-NY-REBEART(AND-IDX))                 
260800        ELSE                                                              
260900           ADD +1              TO AND-IDX                                 
261000        END-IF                                                            
261100                                                                          
261200     END-PERFORM                                                          
261300     .                                                                    
261400     EJECT                                                                
261500 HGAC-UPPD-SATG11 SECTION.                                                
261600                                                                          
261700     IF TAB-TRAFF-SW = JA                                                 
261800        MOVE TAB-INL-ANTAL(TAB-IDX)  TO W2-SRAD-KVSATRES                  
261900        COMPUTE W2-SRAD-KVSATROS = (TAB-W2-REBEART(TAB-IDX)               
262000                                 -  TAB-INL-ANTAL(TAB-IDX))               
262100     ELSE                                                                 
262200        IF AND-TRAFF-SW = JA                                              
262300           IF AND-BRIST(AND-IDX) = JA                                     
262400              MOVE AND-INL-ANTAL(AND-IDX) TO W2-SRAD-KVSATRES             
262500              COMPUTE W2-SRAD-KVSATROS = (AND-W2-REBEART(AND-IDX)         
262600                                       -  AND-INL-ANTAL(AND-IDX))         
262700           END-IF                                                         
262800        END-IF                                                            
262900     END-IF                                                               
263000                                                                          
263100     MOVE 'N'                  TO W2-SRAD-FLSATUTS                        
263200                                                                          
263300     MOVE W2-SRAD-WDJ211       TO SATG-SRAD-WDJ211                        
263400     PERFORM IMS-REPL-SATG-SATG11                                         
263500     .                                                                    
263600     EJECT                                                                
263700 HGAD-UPPD-ARTC11               SECTION.                                  
263800                                                                          
263900     PERFORM IMS-GHU-ARTC11                                               
264000     MOVE CLAG-IDANSK       TO WS-IDANSK                                  
264100                                                                          
264200     COMPUTE CLAG-KVEFRS    =  CLAG-KVEFRS - W-ORIG-KVSATRES              
264300                                                                          
264400***** SKALL LOGGA DATABAS WDL9 MED ANTAL SALDOFÖRÄNDRADE ART. ***         
264500                                                                          
264600     MOVE '-'                    TO LOGG-IDTECKEN-KVEFRS                  
264700     MOVE ' '                    TO LOGG-IDTECKEN-KVLS                    
264800     MOVE W-ORIG-KVSATRES        TO LOGG-KVART-SALDO                      
264900     IF LOGG-KVART-SALDO > ZERO                                           
265000       PERFORM S09-SKAPA-SALDOLOGG                                        
265100     END-IF                                                               
265200                                                                          
265300     COMPUTE CLAG-KVLS      =  CLAG-KVLS   + W-ORIG-REBEART               
265400                                                                          
265500     MOVE ' '                    TO LOGG-IDTECKEN-KVEFRS                  
265600     MOVE '+'                    TO LOGG-IDTECKEN-KVLS                    
265700     MOVE W-ORIG-REBEART         TO LOGG-KVART-SALDO                      
265800     IF LOGG-KVART-SALDO > ZERO                                           
265900       PERFORM S09-SKAPA-SALDOLOGG                                        
266000     END-IF                                                               
266100                                                                          
266200     IF (TAB-TRAFF-SW = JA)  OR                                           
266300        (AND-TRAFF-SW = JA AND AND-BRIST(AND-IDX) = JA)                   
266400                                                                          
266500        PERFORM S08-EV-LARM-2191-ANSKAFFN                                 
266600                                                                          
266700        COMPUTE CLAG-KVRESS = CLAG-KVRESS + W2-SRAD-KVSATRES              
266800        COMPUTE CLAG-KVROS =   CLAG-KVROS + W2-SRAD-KVSATROS              
266900        MOVE W2-SRAD-KVSATROS  TO WS-KVAVART                              
267000                                                                          
267100        PERFORM S06-SKAPA-RYJ-TRANS-BRIST                                 
267200     ELSE                                                                 
267300        COMPUTE CLAG-KVRESS =   CLAG-KVRESS + W-ORIG-KVSATRES             
267400     END-IF                                                               
267500                                                                          
267600     PERFORM IMS-REPL-ARTC11                                              
267700     .                                                                    
267800     EJECT                                                                
267900****************************************************************          
268000*  HGAF-SKAPA-INLL-WDJ212                                      *          
268100*  INLÄGGNINGSLISTA SKAPAS FÖR DE INGÅENDE ARTIKLAR SOM SKALL  *          
268200*  LÄGGAS TILLBAKA UT I HYLLAN IGEN.                           *          
268300****************************************************************          
268400 HGAF-SKAPA-INLL-WDJ212 SECTION.                                          
268500                                                                          
268600     IF W2-SRAD-KVSATRES > 0                                              
268700                                                                          
268800        MOVE '2'               TO SATG-URAD-KDSATLI                       
268900                                                                          
269000        PERFORM IMS-GU-ARTC11                                             
269100*SVS                                                                      
269200        IF WS-SATG-SHUV-BEFT = +015                                       
269300        OR WS-SATG-SHUV-BEFT = +096                                       
269400          MOVE CLAG-ADLAGOMR   TO SATG-URAD-ADLAGOMR                      
269500          MOVE CLAG-ADGANG     TO SATG-URAD-ADGANG                        
269600          MOVE CLAG-ADPLATS    TO SATG-URAD-ADPLATS                       
269700        ELSE                                                              
269800          IF CLAG-KVLS-SVS > 0                                            
269900            MOVE CLAG-ADLAGOMR-SVS TO SATG-URAD-ADLAGOMR                  
270000            MOVE CLAG-ADGANG-SVS   TO SATG-URAD-ADGANG                    
270100            MOVE CLAG-ADPLATS-SVS  TO SATG-URAD-ADPLATS                   
270200          ELSE                                                            
270300            MOVE CLAG-ADLAGOMR TO SATG-URAD-ADLAGOMR                      
270400            MOVE CLAG-ADGANG   TO SATG-URAD-ADGANG                        
270500            MOVE CLAG-ADPLATS  TO SATG-URAD-ADPLATS                       
270600          END-IF                                                          
270700        END-IF                                                            
270800                                                                          
270900        MOVE W2-SRAD-IDARTNR   TO SATG-URAD-IDARTNR                       
271000                                                                          
271100        MOVE W2-SRAD-IDARTNR   TO W-IDARTNR                               
271200        MOVE MED-IDSKYLT       TO W-IDSKYLT                               
271300        PERFORM IMS-GU-BENA11                                             
271400        MOVE BENA-TEXT-BEART   TO SATG-URAD-BEART                         
271500                                                                          
271600        MOVE SPACE             TO SATG-URAD-FLSATBRI                      
271700        MOVE W2-SRAD-IDKONTO   TO SATG-URAD-IDKONTO                       
271800        MOVE W2-SRAD-IDANALYS  TO SATG-URAD-IDANALYS                      
271900        MOVE W2-SRAD-IDKST     TO SATG-URAD-IDKST                         
272000        MOVE W2-SRAD-KDPRODSL  TO SATG-URAD-KDPRODSL                      
272100        MOVE W2-SRAD-KDSATAND  TO SATG-URAD-KDSATAND                      
272200        MOVE W2-SRAD-KDSATKMB  TO SATG-URAD-KDSATKMB                      
272300        MOVE W2-SRAD-KDSORT    TO SATG-URAD-KDSORT                        
272400        MOVE W2-SRAD-PRARTSTD  TO SATG-URAD-PRARTSTD                      
272500        MOVE W2-SRAD-REANTPSA  TO SATG-URAD-REANTPSA                      
272600        MOVE W2-SRAD-KVSATRES  TO SATG-URAD-REBEART                       
272700        MOVE W2-SRAD-REKSIFFR  TO SATG-URAD-REKSIFFR                      
272800        MOVE W2-SRAD-VKARTNTO  TO SATG-URAD-VKARTNTO                      
272900        MOVE W2-SRAD-VLARTNTO  TO SATG-URAD-VLARTNTO                      
273000        PERFORM IMS-ISRT-WDJ212-SATG1                                     
273100     END-IF                                                               
273200     .                                                                    
273300****************************************************************          
273400*  HGAH-SKAPA-ORDP01                                           *          
273500*  SKAPA RESTORDER FÖR DEN EJ BYGGBARA SATSORDERRADEN (W2).    *          
273600****************************************************************          
273700 HGAH-SKAPA-ORDP01          SECTION.                                      
273800                                                                          
273900     MOVE W2-SRAD-IDARTNR        TO ORDP-RAD-IDARTNR                      
274000     MOVE '2'                    TO ORDP-RAD-KDSTARAD                     
274100     MOVE '4'                    TO ORDP-RAD-KDROO                        
274200     MOVE W2-SRAD-KVSATROS       TO WS-RO-ANTAL                           
274300                                                                          
274400     PERFORM S01-SKAPA-RESTORDER                                          
274500     .                                                                    
274600     EJECT                                                                
274700 HGB-UPPD-SATS-ART SECTION.                                               
274800                                                                          
274900     PERFORM IMS-GHU-WDJ201-SATG                                          
275000     MOVE SATG-SHUV-WDJ201     TO  W2-SHUV-WDJ201                         
275100                                                                          
275200     MOVE NEJ                  TO  W2-SHUV-FLBYGGB                        
275300     MOVE ZERO                 TO  W2-SHUV-KVBYGGB                        
275400     MOVE JA                   TO  W2-SHUV-FLSATNOL                       
275500     MOVE 'R'                  TO  W2-SHUV-KDSATSTA                       
275600                                                                          
275700     PERFORM S05-BERAKNA-PTID                                             
275800     MOVE '5'                  TO  W2-SHUV-KDSATPLK                       
275900                                                                          
276000     MOVE W2-SHUV-WDJ201       TO  SATG-SHUV-WDJ201                       
276100     PERFORM IMS-REPL-WDJ201-SATG                                         
276200     .                                                                    
276300     EJECT                                                                
276400****************************************************************          
276500*  HH-STARTA-PLOCKLISTA                                        *          
276600*   STARTA PROGRAM W4037200, FÖR ATT SKRIVA UT EN NY           *          
276700*   PLOCKLISTA.                                                *          
276800****************************************************************          
276900 HH-STARTA-PLOCKLISTA SECTION.                                            
277000                                                                          
277100     MOVE 'W4T372X'            TO  PTOP-TRANSKOD                          
277200     MOVE W1-SHUV-IDORDNSB     TO  PTOP-IDORDNSB                          
277300     MOVE W1-SHUV-IDORDNSS     TO  PTOP-IDORDNSS                          
277400     MOVE MFS-KDMFSFOR         TO  PTOP-KDMFSFOR                          
277500                                                                          
277600     PERFORM IMS-INSERT-MSG-ALT1                                          
277700     .                                                                    
277800     EJECT                                                                
277900****************************************************************          
278000*  HI-STARTA-INLLISTA                                          *          
278100*   STARTA PROGRAM W4037400, FÖR ATT SKRIVA UT EN INLÄGGNINGS- *          
278200*   LISTA, FÖR DE ING ARTIKLAR SOM SKALL LÄGGAS TILLBAKA I     *          
278300*   HYLLAN IGEN.                                               *          
278400****************************************************************          
278500 HI-STARTA-INLLISTA SECTION.                                              
278600                                                                          
278700     MOVE 'W4T374X'            TO  PTOP-TRANSKOD                          
278800     MOVE W1-SHUV-IDORDNSB     TO  PTOP-IDORDNSB                          
278900     MOVE W1-SHUV-IDORDNSS     TO  PTOP-IDORDNSS                          
279000     MOVE MFS-KDMFSFOR         TO  PTOP-KDMFSFOR                          
279100                                                                          
279200     PERFORM IMS-INSERT-MSG-ALT2                                          
279300     .                                                                    
279400     EJECT                                                                
279500****************************************************************          
279600* S01-SKAPA-RESTORDER                                          *          
279700****************************************************************          
279800 S01-SKAPA-RESTORDER SECTION.                                             
279900                                                                          
280000     MOVE SATG-SHUV-IDDISTR      TO ORDP-RAD-IDDISTR                      
280100     MOVE SATG-SHUV-IDKUNDNR     TO ORDP-RAD-IDKUNDNR                     
280200     MOVE SATG-SHUV-IDORDNSB     TO WS-IDORDNSB                           
280300     MOVE SATG-SHUV-IDORDNSS     TO WS-IDORDNSS                           
280400     MOVE SPACE                  TO ORDP-RAD-IDKUNDRF                     
280500     MOVE WC-CDC-SE              TO ORDP-RAD-IDDC                         
280600                                    ORDP-RAD-IDDC-RO                      
280700     MOVE 'KI'                   TO ORDP-RAD-KDOI                         
280800     MOVE SPACE                  TO ORDP-RAD-CLEARGROUP                   
280900     MOVE WS-IDORDNST-NUM        TO ORDP-RAD-IDORDNR5                     
281000     MOVE 1                      TO ORDP-RAD-IDLOPNR                      
281100     MOVE SPACE                  TO ORDP-RAD-BEKUNDRF                     
281200     MOVE SPACE                  TO ORDP-RAD-BERADREF                     
281300     MOVE NEJ                    TO ORDP-RAD-FLERS                        
281400                                                                          
281500     MOVE SATG-SRAD-IDKONTO      TO ORDP-RAD-IDKONTO                      
281600     MOVE SATG-SRAD-IDKST        TO ORDP-RAD-IDKST                        
281700     MOVE SATG-SRAD-IDANALYS     TO ORDP-RAD-IDANALYS                     
281800     MOVE SPACE                  TO ORDP-RAD-IDKUNDRF-LEV                 
281900     MOVE ZERO                   TO ORDP-RAD-KDDSP                        
282000     MOVE SATG-SHUV-KDFAKTYP     TO ORDP-RAD-KDFAKTYP                     
282100     MOVE ZERO                   TO ORDP-RAD-KDFRAKT                      
282200     MOVE ZERO                   TO ORDP-RAD-KDKVBRYT                     
282300     MOVE 3                      TO ORDP-RAD-KDORDING                     
282400     MOVE SATG-SHUV-KDORDKL      TO ORDP-RAD-KDORDKL                      
282500     MOVE SATG-SRAD-KDPRODSL     TO ORDP-RAD-KDPRODSL                     
282600     MOVE WS-RO-ANTAL            TO ORDP-RAD-KVRO                         
282700     INITIALIZE                     ORDP-RAD-DEAL-PR-LINE                 
282800     MOVE '2'                    TO ORDP-RAD-KDSTARAD                     
282900     MOVE ZERO                   TO ORDP-RAD-KDTPOTYP                     
283000     MOVE ZERO                   TO ORDP-RAD-KDVRINFO                     
283100     MOVE WS-RO-ANTAL            TO ORDP-RAD-KVART                        
283200     MOVE ZERO                   TO ORDP-RAD-PRARTNTO                     
283300     MOVE SATG-SRAD-REKSIFFR     TO ORDP-RAD-REKSIFFR                     
283400     MOVE ZERO                   TO ORDP-RAD-TIAVBOKN                     
283500     MOVE SATG-SHUV-DAREGDAT (3:6) TO ORDP-RAD-TIREGDAT                   
283600     MOVE ZERO                   TO ORDP-RAD-TIRES                        
283700     MOVE WS-DATUM               TO ORDP-RAD-DARODAT                      
283800     MOVE ZERO                   TO ORDP-RAD-TITPO                        
283900     MOVE SPACE                  TO ORDP-RAD-KDPRTYP                      
284000     MOVE SPACE                  TO ORDP-RAD-BEVOLREF                     
284100     MOVE NEJ                    TO ORDP-RAD-FLINVEST                     
284200     MOVE NEJ                    TO ORDP-RAD-FLPRTILL                     
284300     MOVE JA                     TO ORDP-RAD-FLTPOBEK                     
284400     MOVE ZERO                   TO ORDP-RAD-IDKAMPRF                     
284500     MOVE SATG-SHUV-IDLEVNR      TO ORDP-RAD-IDLEVNR                      
284600     MOVE 'SATS'                 TO ORDP-RAD-IDSYSTEM                     
284700     MOVE SATG-SRAD-REBEART      TO ORDP-RAD-KVBEART-Q                    
284800     MOVE WS-TIME (1:6)          TO ORDP-RAD-TIREGTID                     
284900     MOVE 999                    TO ORDP-RAD-DASENDAT                     
285000                                    ORDP-RAD-TISENBEK-KL                  
285100     MOVE WS-IDANSK              TO ORDP-RAD-IDANSK                       
285200                                                                          
285300     MOVE SPACE                  TO ORDP-RAD-KDORDTYP-LDC                 
285400     MOVE ZERO                   TO ORDP-RAD-TIREPDAT                     
285500     MOVE SPACE                  TO ORDP-RAD-IDKUNDRF-WIP                 
285610     MOVE +0                     TO ORDP-RAD-PRAVCOST                     
285620     MOVE SPACE                  TO ORDP-RAD-KDROPACK                     
285630     MOVE SPACE                  TO ORDP-RAD-IDARBREF                     
285700                                                                          
285800     PERFORM S02-HAMTA-PRIORITETSKOD                                      
285900                                                                          
286000     PERFORM IMS-ISRT-WDA5-ORDP01                                         
286100                                                                          
286200     PERFORM UNTIL SEGMENT-FINNS                                          
286300        ADD 1 TO ORDP-RAD-IDLOPNR                                         
286400        PERFORM IMS-ISRT-WDA5-ORDP01                                      
286500     END-PERFORM                                                          
286600     .                                                                    
286700     EJECT                                                                
286800*                                                                         
286900 S02-HAMTA-PRIORITETSKOD SECTION.                                         
287000                                                                          
287100     MOVE ORDP-RAD-KDTPOTYP      TO W-4512-KDTPOTYP                       
287200     MOVE SATG-SHUV-KDORDKL      TO W-4512-KDORDKL                        
287300     MOVE SATG-SHUV-IDDISTR      TO W-4512-IDDISTR-FOM                    
287400                                    W-4512-IDDISTR-TOM                    
287500                                                                          
287600     PERFORM IMS-GU-XXJN-WLXXJN11                                         
287700     MOVE 4512-KDRAPRIO          TO ORDP-RAD-KDRAPRIO                     
287800     .                                                                    
287900     SKIP2                                                                
288000*                                                                         
288100*                                                                         
288200 S05-BERAKNA-PTID               SECTION.                                  
288300                                                                          
288400     MOVE 'SATS'               TO  PTID-IDSYSTEM                          
288500     MOVE W1-SHUV-IDORDNSB     TO  PTID-IDORDNSB                          
288600     MOVE W1-SHUV-IDORDNSS     TO  PTID-IDORDNSS                          
288700     MOVE W1-SHUV-IDARTNR      TO  PTID-IDARTNR                           
288800     MOVE W1-SHUV-IDPRC        TO  PTID-IDPRC                             
288900     MOVE +1                   TO  PTID-KDCLAGER                          
289000     MOVE W1-SHUV-KVBYGGB      TO  PTID-KVBYGGB                           
289100     MOVE W1-KVRADER           TO  PTID-KVRADER                           
289200                                   PTID-KVANTART                          
289300     MOVE ZERO                 TO  PTID-SUSATPTI                          
289400     MOVE W1-SHUV-VKORDNTO     TO  PTID-VKORDNTO                          
289500     MOVE W1-SHUV-VLORDNTO     TO  PTID-VLORDNTO                          
289600     MOVE SPACE                TO  PTID-KDSVAR                            
289700                                                                          
289800     CALL W416PTID USING PTID-W416PTID XXKH-PCB XXKI-PCB                  
289900                                       SATB-PCB                           
290000                                                                          
290100     IF PTID-KDSVAR            =   ZERO                                   
290200         MOVE PTID-SUSATPTI    TO  W1-SHUV-SUSATPTI                       
290300      ELSE                                                                
290400         MOVE ZERO             TO  W1-SHUV-SUSATPTI                       
290500     END-IF                                                               
290600     .                                                                    
290700     EJECT                                                                
290800****************************************************************          
290900*  S06-SKAPA-RYJ-TRANS-BRIST                                   *          
291000*  TRANSAKTIONER VID KONSTATERAD BRIST                         *          
291100****************************************************************          
291200 S06-SKAPA-RYJ-TRANS-BRIST  SECTION.                                      
291300                                                                          
291400     MOVE +1                     TO ZZAC-IDLOGLOP                         
291500     MOVE 'RYJ'                  TO ZZAC-IDPTYP                           
291600                                    W-RYJ-IDPTYP                          
291700                                                                          
291800     ACCEPT ZZAC-TIAAMMDD        FROM DATE                                
291900     ACCEPT ZZAC-TIKLOCK         FROM TIME                                
292000                                                                          
292100     MOVE WC-CDC-SE              TO W-RYJ-IDDC                            
292200     MOVE SATG-SRAD-IDARTNR      TO W-RYJ-IDARTNR                         
292300     MOVE SATG-SHUV-IDDISTR      TO W-RYJ-IDDISTR                         
292400     MOVE SATG-SHUV-KDORDKL      TO W-RYJ-KDORDKL                         
292500     MOVE WS-KVAVART             TO W-RYJ-KVAVART                         
292600     MOVE WS-KVAVART             TO W-RYJ-KVANNANT                        
292700     MOVE '2'                    TO W-RYJ-KDUPPD                          
292800     MOVE +8                     TO W-RYJ-KDRORELS                        
292900     MOVE +1                     TO W-RYJ-KDAVVORS                        
293000     MOVE WS-TIUTSKR             TO W-RYJ-TIUTSKR                         
293100     MOVE W-RYJ-WDGZRYJ          TO ZZAC-LOGGPOST                         
293200     PERFORM IMS-ISRT-WDG6                                                
293300     IF SEGMENT-FINNS-REDAN                                               
293400        PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                              
293500           ADD +1 TO ZZAC-IDLOGLOP                                        
293600           PERFORM IMS-ISRT-WDG6                                          
293700        END-PERFORM                                                       
293800     END-IF                                                               
293900     .                                                                    
294000     EJECT                                                                
294100****************************************************************          
294200*  S07-SKAPA-RYJ-TRANS-ANNULL                                  *          
294300*  TRANSAKTIONER VID ANNULLERING AV DEL AV ORDERRAD            *          
294400****************************************************************          
294500 S07-SKAPA-RYJ-TRANS-ANNULL SECTION.                                      
294600                                                                          
294700     MOVE +1                     TO ZZAC-IDLOGLOP                         
294800     MOVE 'RYJ'                  TO ZZAC-IDPTYP                           
294900                                    W-RYJ-IDPTYP                          
295000                                                                          
295100     ACCEPT ZZAC-TIAAMMDD        FROM DATE                                
295200     ACCEPT ZZAC-TIKLOCK         FROM TIME                                
295300                                                                          
295400     MOVE WC-CDC-SE              TO W-RYJ-IDDC                            
295500     MOVE SATG-SRAD-IDARTNR      TO W-RYJ-IDARTNR                         
295600     MOVE SATG-SHUV-IDDISTR      TO W-RYJ-IDDISTR                         
295700     MOVE SATG-SHUV-KDORDKL      TO W-RYJ-KDORDKL                         
295800     MOVE WS-KVANNANT            TO W-RYJ-KVANNANT                        
295900     MOVE WS-KVANNANT            TO W-RYJ-KVAVART                         
296000     MOVE '2'                    TO W-RYJ-KDUPPD                          
296100     MOVE +8                     TO W-RYJ-KDRORELS                        
296200     MOVE +2                     TO W-RYJ-KDAVVORS                        
296300     MOVE WS-TIUTSKR             TO W-RYJ-TIUTSKR                         
296400     MOVE W-RYJ-WDGZRYJ          TO ZZAC-LOGGPOST                         
296500     PERFORM IMS-ISRT-WDG6                                                
296600     IF SEGMENT-FINNS-REDAN                                               
296700        PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                              
296800           ADD +1 TO ZZAC-IDLOGLOP                                        
296900           PERFORM IMS-ISRT-WDG6                                          
297000        END-PERFORM                                                       
297100     END-IF                                                               
297200     .                                                                    
297300     EJECT                                                                
297400 S08-EV-LARM-2191-ANSKAFFN SECTION.                                       
297500                                                                          
297600** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS.             
297700     IF CLAG-KVROS = 0                                                    
297800        IF CLAG-KVAKS-CDC + CLAG-KVAKS-PAV   = 0                          
297900                                                                          
297910         COMPUTE ALT2191-LL = LENGTH OF ALT2191-MID-W2I19101 + 17         
298000           MOVE '1'                 TO ALT2191-MID-KDCLAGER               
298100           MOVE W-IDARTNR           TO ALT2191-MID-IDARTNR                
298200           MOVE ZERO                TO ALT2191-MID-TISENBEK-DAG           
298300                                       ALT2191-MID-TISENBEK-KL            
298400           MOVE SPACE               TO ALT2191-MID-IDKR                   
298500           MOVE WS-IDANSK           TO ALT2191-MID-IDANSK                 
298600           MOVE 210                 TO ALT2191-MID-KDLARM                 
298700           MOVE SATG-SHUV-IDDISTR   TO WS-IDDISTR-NUM4                    
298800           MOVE WS-IDDISTR-NUM4     TO ALT2191-MID-IDDISTR                
298900           MOVE SATG-SHUV-IDKUNDNR  TO WS-IDKUNDNR-NUM6                   
299000           MOVE WS-IDKUNDNR-NUM6    TO ALT2191-MID-IDKUNDNR               
299100           MOVE SATG-SHUV-IDORDNSB  TO ALT2191-MID-IDORDNR5(1:4)          
299200           MOVE SATG-SHUV-IDORDNSS  TO ALT2191-MID-IDKUNDRF(5:1)          
299300           MOVE 'J'                 TO ALT2191-MID-FLNYLARM               
299310           MOVE WC-CDC-SE           TO ALT2191-MID-IDDC                   
299320           MOVE SPACE               TO ALT2191-MID-IDLEVNR                
299400                                                                          
299500           PERFORM IMS-PURG-ALT2191-MSG                                   
299600        END-IF                                                            
299700     END-IF                                                               
299800     .                                                                    
299900     EJECT                                                                
300000***************************************************************           
300100* FÖLJANDE SECTION SKALL UPPDATERA  DATABAS WDL9/WLLOGA       *           
300200***************************************************************           
300300 S09-SKAPA-SALDOLOGG SECTION.                                             
300400                                                                          
300500     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
300600                                                                          
300700     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
300800     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
300900                                   - WS-AAAAMMDD                          
301000     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
301100     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
301200                                   - WS-TTMMSSTH                          
301300     MOVE 9                        TO LOGG-IDSEKVNR                       
301400     MOVE WC-CDC-SE                TO LOGG-IDDC                           
301500     MOVE 'OUTB'                   TO LOGG-IDHUVTYP                       
301600     MOVE 'KIT'                    TO LOGG-IDSUBTYP                       
301700     MOVE 'W4030400'               TO LOGG-IDPGM                          
301800     MOVE '4304'                   TO LOGG-IDTRANS                        
301900     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
302000     MOVE SPACE                    TO LOGG-REF                            
302100     MOVE SATG-SHUV-IDDISTR        TO LOGG-IDDISTR                        
302200     MOVE SATG-SHUV-IDKUNDNR       TO LOGG-IDKUNDNR                       
302300*    MOVE SATG-SHUV-IDORDNSB       TO LOGG-IDORDNR5                       
302400     MOVE WDE4-KORD-IDORDNR5       TO LOGG-IDORDNR5                       
302500     MOVE W-IDPRODNR               TO LOGG-IDPRODNR                       
302600     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS                 
302700     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
302800                                                                          
302900                                                                          
303000     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC                                  
303100                        + CLAG-KVAKS-T                                    
303200                                                                          
303300     MOVE CLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
303400     MOVE CLAG-KVEFRS              TO LOGG-KVEFRS                         
303500     MOVE CLAG-KVLS                TO LOGG-KVLS                           
303600     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
303700                                                                          
303800     PERFORM IMS-ISRT-WDL901                                              
303900     IF SEGMENT-FINNS-REDAN                                               
304000       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
304100         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
304200         PERFORM IMS-ISRT-WDL901                                          
304300       END-PERFORM                                                        
304400     END-IF                                                               
304500     .                                                                    
304600     EJECT                                                                
304700****************************************************************          
304800*  MFS-SEKTIONER                                               *          
304900****************************************************************          
305000 MFS-RENSA-FAELT-UT SECTION.                                              
305100                                                                          
305200*    --- ALLA UTDATA-FÄLT                                                 
305300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
305400     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR                               
305500                                MOD-TEDDI                                 
305600                                MOD-BEART                                 
305700                                MOD-KVBEART                               
305800                                                                          
305900     MOVE +1                 TO RAD-IDX                                   
306000     PERFORM UNTIL RAD-IDX > RAD-IDX-MAX                                  
306100        MOVE MFS-RENSA-FAELT TO MOD-IDPURAD1(RAD-IDX)                     
306200                                MOD-REBEART1(RAD-IDX)                     
306300                                MOD-IDPURAD2(RAD-IDX)                     
306400                                MOD-REBEART2(RAD-IDX)                     
306500                                MOD-IDPURAD3(RAD-IDX)                     
306600                                MOD-REBEART3(RAD-IDX)                     
306700        ADD +1               TO RAD-IDX                                   
306800     END-PERFORM                                                          
306900     .                                                                    
307000     SKIP3                                                                
307100 MFS-RENSA-FAELT-IN SECTION.                                              
307200                                                                          
307300*    --- ALLA INDATA-FÄLT                                                 
307400                                                                          
307500     MOVE +1                 TO RAD-IDX                                   
307600     PERFORM UNTIL RAD-IDX > RAD-IDX-MAX                                  
307700        MOVE MFS-RENSA-FAELT TO MOD-IDPURAD1(RAD-IDX)                     
307800                                MOD-REBEART1(RAD-IDX)                     
307900                                MOD-IDPURAD2(RAD-IDX)                     
308000                                MOD-REBEART2(RAD-IDX)                     
308100                                MOD-IDPURAD3(RAD-IDX)                     
308200                                MOD-REBEART3(RAD-IDX)                     
308300        ADD +1               TO RAD-IDX                                   
308400     END-PERFORM                                                          
308500     .                                                                    
308600     EJECT                                                                
308700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
308800                                                                          
308900*    --- ALLA UTDATA-FÄLT                                                 
309000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
309100     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR                               
309200                                MOD-TEDDI                                 
309300                                MOD-BEART                                 
309400                                MOD-KVBEART                               
309500                                                                          
309600     MOVE +1                 TO RAD-IDX                                   
309700     PERFORM UNTIL RAD-IDX > RAD-IDX-MAX                                  
309800        MOVE MFS-ROER-EJ-FAELT                                            
309900                             TO MOD-IDPURAD1(RAD-IDX)                     
310000                                MOD-REBEART1(RAD-IDX)                     
310100                                MOD-IDPURAD2(RAD-IDX)                     
310200                                MOD-REBEART2(RAD-IDX)                     
310300                                MOD-IDPURAD3(RAD-IDX)                     
310400                                MOD-REBEART3(RAD-IDX)                     
310500        ADD +1               TO RAD-IDX                                   
310600     END-PERFORM                                                          
310700     .                                                                    
310800     SKIP3                                                                
310900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
311000                                                                          
311100*    --- ALLA INDATA-FÄLT                                                 
311200     MOVE +1                 TO RAD-IDX                                   
311300     PERFORM UNTIL RAD-IDX > RAD-IDX-MAX                                  
311400        MOVE MFS-ROER-EJ-FAELT                                            
311500                             TO MOD-IDPURAD1(RAD-IDX)                     
311600                                MOD-REBEART1(RAD-IDX)                     
311700                                MOD-IDPURAD2(RAD-IDX)                     
311800                                MOD-REBEART2(RAD-IDX)                     
311900                                MOD-IDPURAD3(RAD-IDX)                     
312000                                MOD-REBEART3(RAD-IDX)                     
312100        ADD +1               TO RAD-IDX                                   
312200     END-PERFORM                                                          
312300     .                                                                    
312400     EJECT                                                                
312500 MFS-FORM-ATTR SECTION.                                                   
312600                                                                          
312700*    --- ALLA INDATA-FÄLT                                                 
312800     MOVE +1                 TO RAD-IDX                                   
312900     PERFORM UNTIL RAD-IDX > RAD-IDX-MAX                                  
313000        MOVE MFS-FORMATETS-ATTR                                           
313100                             TO MOD-ATTR-IDPURAD1(RAD-IDX)                
313200                                MOD-ATTR-REBEART1(RAD-IDX)                
313300                                MOD-ATTR-IDPURAD2(RAD-IDX)                
313400                                MOD-ATTR-REBEART2(RAD-IDX)                
313500                                MOD-ATTR-IDPURAD3(RAD-IDX)                
313600                                MOD-ATTR-REBEART3(RAD-IDX)                
313700        ADD +1               TO RAD-IDX                                   
313800     END-PERFORM                                                          
313900     .                                                                    
314000     EJECT                                                                
314100 MFS-STAENG-FAELT-NOMOD-IN    SECTION.                                    
314200                                                                          
314300*    --- ALLA INDATA-FÄLT                                                 
314400     MOVE +1                 TO RAD-IDX                                   
314500     PERFORM UNTIL RAD-IDX > RAD-IDX-MAX                                  
314600        MOVE MFS-STAENG-FAELT-NOMOD                                       
314700                             TO MOD-ATTR-IDPURAD1(RAD-IDX)                
314800                                MOD-ATTR-REBEART1(RAD-IDX)                
314900                                MOD-ATTR-IDPURAD2(RAD-IDX)                
315000                                MOD-ATTR-REBEART2(RAD-IDX)                
315100                                MOD-ATTR-IDPURAD3(RAD-IDX)                
315200                                MOD-ATTR-REBEART3(RAD-IDX)                
315300        ADD +1               TO RAD-IDX                                   
315400     END-PERFORM                                                          
315500     .                                                                    
315600     EJECT                                                                
315700*    IMS-SEKTIONER                                                        
315800*                                                                         
315900*                                                                         
316000 IMS-GET-MSG SECTION.                                                     
316100                                                                          
316200     MOVE '  QC' TO GODK-STATUSKODER                                      
316300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
316400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
316500     PERFORM IMS-STATUSKONTROLL                                           
316600     .                                                                    
316700     SKIP3                                                                
316800 IMS-INSERT-MSG SECTION.                                                  
316900                                                                          
317000     IF NOT ENGLISH-TEXT                                                  
317100       MOVE '0' TO MFS-KDHUVOMR                                           
317200     END-IF                                                               
317300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
317400     MOVE SPACE TO GODK-STATUSKODER                                       
317500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
317600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
317700     PERFORM IMS-STATUSKONTROLL                                           
317800     .                                                                    
317900     EJECT                                                                
318000*                                                                         
318100 IMS-INSERT-MSG-ALT1 SECTION.                                             
318200                                                                          
318300     MOVE SPACE TO GODK-STATUSKODER                                       
318400     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW                           
318500     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
318600     PERFORM IMS-STATUSKONTROLL                                           
318700     .                                                                    
318800     SKIP3                                                                
318900 IMS-INSERT-MSG-ALT2 SECTION.                                             
319000                                                                          
319100     MOVE SPACE TO GODK-STATUSKODER                                       
319200     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW                           
319300     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
319400     PERFORM IMS-STATUSKONTROLL                                           
319500     .                                                                    
319600     EJECT                                                                
319700 IMS-PURG-ALT2191-MSG SECTION.                                            
319800     MOVE LOW-VALUE TO ALT2191-Z1 ALT2191-Z2                              
319900     MOVE '  '  TO GODK-STATUSKODER                                       
320000     CALL CBLTDLI USING PURG ALT2191-PCB ALT2191-IO-AREA                  
320100     MOVE ALT2191-STATUS-CODE TO STATUS-WS                                
320200     PERFORM IMS-STATUSKONTROLL                                           
320300     .                                                                    
320400     SKIP2                                                                
320500*                                                                         
320600*    LÄS WDE401 OCH WDE411  (GU), VIA SEK.INDEX WDE4BSEQ                  
320700*                                                                         
320800 IMS-GU-WDE4-PATH SECTION.                                                
320900     STRING 'WDE411  *D(WDE4BSEQ =' W-WDE4BSEQ-X ')'                      
321000            DELIMITED BY SIZE INTO SSA1                                   
321100     MOVE 'WDE401  ' TO SSA2                                              
321200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
321300     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1 SSA2                 
321400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
321500     PERFORM IMS-STATUSKONTROLL                                           
321600     .                                                                    
321700*                                                                         
321800*    LÄS WDE601  (GU), KOLLIREGISTER                                      
321900*                                                                         
322000 IMS-GU-WDE6      SECTION.                                                
322100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
322200          DELIMITED BY SIZE INTO SSA1                                     
322300     MOVE '  GE' TO GODK-STATUSKODER                                      
322400     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA5 SSA1                     
322500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
322600     PERFORM IMS-STATUSKONTROLL                                           
322700     .                                                                    
322800     EJECT                                                                
322900*                                                                         
323000*    LÄS WDJ201  (GU), SATSORDERHUVUD                                     
323100*                                                                         
323200 IMS-GU-WDJ201-SATG SECTION.                                              
323300     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
323400          DELIMITED BY SIZE INTO SSA1                                     
323500     MOVE '  GE' TO GODK-STATUSKODER                                      
323600     CALL CBLTDLI USING GU SATG-PCB DLI-IO-AREA1 SSA1                     
323700     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
323800     PERFORM IMS-STATUSKONTROLL                                           
323900     .                                                                    
324000     SKIP3                                                                
324100*                                                                         
324200*    LÄS WDJ201  (GU), SATSORDERHUVUD                                     
324300*                                                                         
324400 IMS-GU-WDJ201-SATG1 SECTION.                                             
324500     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
324600          DELIMITED BY SIZE INTO SSA1                                     
324700     MOVE '  ' TO GODK-STATUSKODER                                        
324800     CALL CBLTDLI USING GU SATG1-PCB DLI-IO-AREA1 SSA1                    
324900     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
325000     PERFORM IMS-STATUSKONTROLL                                           
325100     .                                                                    
325200     SKIP3                                                                
325300*                                                                         
325400*    LÄS WDJ201  (GHU), SATSORDERHUVUD                                    
325500*                                                                         
325600 IMS-GHU-WDJ201-SATG SECTION.                                             
325700     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
325800          DELIMITED BY SIZE INTO SSA1                                     
325900     MOVE '  ' TO GODK-STATUSKODER                                        
326000     CALL CBLTDLI USING GHU SATG-PCB DLI-IO-AREA1 SSA1                    
326100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
326200     PERFORM IMS-STATUSKONTROLL                                           
326300     .                                                                    
326400     SKIP3                                                                
326500*                                                                         
326600*    LÄS WDJ201  (GHU), SATSORDERHUVUD                                    
326700*                                                                         
326800 IMS-GHU-WDJ201-SATG1 SECTION.                                            
326900     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
327000          DELIMITED BY SIZE INTO SSA1                                     
327100     MOVE '  ' TO GODK-STATUSKODER                                        
327200     CALL CBLTDLI USING GHU SATG1-PCB DLI-IO-AREA1 SSA1                   
327300     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
327400     PERFORM IMS-STATUSKONTROLL                                           
327500     .                                                                    
327600     SKIP3                                                                
327700*                                                                         
327800*    LÄS WDJ211  (GU), SATSORDERRAD                                       
327900*                                                                         
328000 IMS-GU-WDJ211-SATG SECTION.                                              
328100     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
328200          DELIMITED BY SIZE INTO SSA1                                     
328300     STRING 'WLSATG11(IDARTNR  =' W-IDARTNR-X ')'                         
328400          DELIMITED BY SIZE INTO SSA2                                     
328500     MOVE '  ' TO GODK-STATUSKODER                                        
328600     CALL CBLTDLI USING GU SATG-PCB DLI-IO-AREA2 SSA1 SSA2                
328700     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
328800     PERFORM IMS-STATUSKONTROLL                                           
328900     .                                                                    
329000     SKIP3                                                                
329100*                                                                         
329200*    LÄS WDJ211  (GHNP), SATSORDERRAD                                     
329300*                                                                         
329400 IMS-GHNP-WDJ211-SATG SECTION.                                            
329500     STRING 'WLSATG11 '                                                   
329600          DELIMITED BY SIZE INTO SSA1                                     
329700     MOVE '  GE' TO GODK-STATUSKODER                                      
329800     CALL CBLTDLI USING GHNP SATG-PCB DLI-IO-AREA2 SSA1                   
329900     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
330000     PERFORM IMS-STATUSKONTROLL                                           
330100     .                                                                    
330200     SKIP3                                                                
330300*                                                                         
330400*    LÄS WDJ211, VIA WDJ201  (GN)                                         
330500*    HITTA ING.ARTIKLAR MED SAMMA KDSATKMB                                
330600*                                                                         
330700 IMS-GU-WDJ211-KDSATKMB SECTION.                                          
330800     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
330900          DELIMITED BY SIZE INTO SSA1                                     
331000     STRING 'WLSATG11*F(IDARTNR  >' W-IDARTNR-X                           
331100              '&KDSATKMB =' W-KDSATKMB-X ')'                              
331200          DELIMITED BY SIZE INTO SSA2                                     
331300     MOVE '  GE' TO GODK-STATUSKODER                                      
331400     CALL CBLTDLI USING GU SATG1-PCB DLI-IO-AREA2 SSA1 SSA2               
331500     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
331600     PERFORM IMS-STATUSKONTROLL                                           
331700     .                                                                    
331800     SKIP3                                                                
331900*                                                                         
332000 IMS-GN-WDJ211-KDSATKMB SECTION.                                          
332100     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
332200          DELIMITED BY SIZE INTO SSA1                                     
332300     STRING 'WLSATG11(IDARTNR  >' W-IDARTNR-X                             
332400              '&KDSATKMB =' W-KDSATKMB-X ')'                              
332500          DELIMITED BY SIZE INTO SSA2                                     
332600     MOVE '  GE' TO GODK-STATUSKODER                                      
332700     CALL CBLTDLI USING GN SATG1-PCB DLI-IO-AREA2 SSA1 SSA2               
332800     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
332900     PERFORM IMS-STATUSKONTROLL                                           
333000     .                                                                    
333100     EJECT                                                                
333200*                                                                         
333300*   INSERT PÅ UTSKRIFTSSEGMENT WDJ212                                     
333400*                                                                         
333500 IMS-ISRT-WDJ212-SATG1 SECTION.                                           
333600                                                                          
333700     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
333800          DELIMITED BY SIZE INTO SSA1                                     
333900     MOVE 'WLSATG12 ' TO SSA2                                             
334000*    MOVE '  II' TO GODK-STATUSKODER                                      
334100     MOVE '  ' TO GODK-STATUSKODER                                        
334200     CALL CBLTDLI USING ISRT SATG1-PCB DLI-IO-AREA3 SSA1 SSA2             
334300     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
334400     PERFORM IMS-STATUSKONTROLL                                           
334500     .                                                                    
334600     EJECT                                                                
334700*                                                                         
334800*    INSERT AV WDJ201 (ISRT)                                              
334900*                                                                         
335000 IMS-ISRT-SATG1-SATG01 SECTION.                                           
335100                                                                          
335200     MOVE 'WLSATG01 ' TO SSA1                                             
335300     MOVE '  II' TO GODK-STATUSKODER                                      
335400     CALL CBLTDLI USING ISRT SATG1-PCB DLI-IO-AREA1 SSA1                  
335500     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
335600     PERFORM IMS-STATUSKONTROLL                                           
335700     .                                                                    
335800*                                                                         
335900*    REPLACE AV WDJ201 (REPL)                                             
336000*                                                                         
336100 IMS-REPL-WDJ201-SATG SECTION.                                            
336200                                                                          
336300     MOVE 'WLSATG01 ' TO SSA1                                             
336400     MOVE '  ' TO GODK-STATUSKODER                                        
336500     CALL CBLTDLI USING REPL SATG-PCB DLI-IO-AREA1 SSA1                   
336600     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
336700     PERFORM IMS-STATUSKONTROLL                                           
336800     .                                                                    
336900     EJECT                                                                
337000*                                                                         
337100*    INSERT AV WDJ211 (ISRT)                                              
337200*                                                                         
337300 IMS-ISRT-SATG1-SATG11 SECTION.                                           
337400                                                                          
337500     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
337600          DELIMITED BY SIZE INTO SSA1                                     
337700     MOVE 'WLSATG11 ' TO SSA2                                             
337800     MOVE '  II' TO GODK-STATUSKODER                                      
337900     CALL CBLTDLI USING ISRT SATG1-PCB DLI-IO-AREA2 SSA1 SSA2             
338000     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
338100     PERFORM IMS-STATUSKONTROLL                                           
338200     .                                                                    
338300*                                                                         
338400*    INSERT AV WDJ211 (ISRT)                                              
338500*                                                                         
338600 IMS-REPL-SATG-SATG11 SECTION.                                            
338700                                                                          
338800     MOVE '  ' TO GODK-STATUSKODER                                        
338900     CALL CBLTDLI USING REPL SATG-PCB DLI-IO-AREA2                        
339000     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
339100     PERFORM IMS-STATUSKONTROLL                                           
339200     .                                                                    
339300     SKIP3                                                                
339400*                                                                         
339500 IMS-REPL-SATG1-SATG01 SECTION.                                           
339600                                                                          
339700     MOVE '  ' TO GODK-STATUSKODER                                        
339800     CALL CBLTDLI USING REPL SATG1-PCB DLI-IO-AREA1                       
339900     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
340000     PERFORM IMS-STATUSKONTROLL                                           
340100     .                                                                    
340200     EJECT                                                                
340300*                                                                         
340400*    LÄS WDD3??  (GU), HÄMTA SATSARTIKEL-BENÄMNING                        
340500*                                                                         
340600 IMS-GU-BENA11 SECTION.                                                   
340700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
340800          DELIMITED BY SIZE INTO SSA1                                     
340900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
341000          DELIMITED BY SIZE INTO SSA2                                     
341100     MOVE '  GE' TO GODK-STATUSKODER                                      
341200     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA7 SSA1 SSA2               
341300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
341400     PERFORM IMS-STATUSKONTROLL                                           
341500     .                                                                    
341600     SKIP3                                                                
341700*                                                                         
341800*    SKAPA WDA5, RESTORDER                                                
341900*                                                                         
342000 IMS-ISRT-WDA5-ORDP01 SECTION.                                            
342100                                                                          
342200     MOVE 'WLORDP01 ' TO SSA1                                             
342300     MOVE '  II' TO GODK-STATUSKODER                                      
342400     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA4 SSA1                   
342500     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
342600     PERFORM IMS-STATUSKONTROLL                                           
342700     .                                                                    
342800     EJECT                                                                
342900*                                                                         
343000*    LÄS WDK901  (GU), ORDERKÖ                                            
343100*                                                                         
343200 IMS-GU-ARTM01      SECTION.                                              
343300     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
343400          DELIMITED BY SIZE INTO SSA1                                     
343500     MOVE '  GE' TO GODK-STATUSKODER                                      
343600     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA6 SSA1                     
343700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
343800     PERFORM IMS-STATUSKONTROLL                                           
343900     .                                                                    
344000     SKIP3                                                                
344100*                                                                         
344200*    LÄSA HÄNDELSEBAS, TYP=4512                                           
344300*                                                                         
344400 IMS-GU-XXJN-WLXXJN11 SECTION.                                            
344500                                                                          
344600     STRING 'WLXXJN01(WDGXKEY  =' W-4511-IDHTYP-X  ')'                    
344700          DELIMITED BY SIZE INTO SSA1                                     
344800     STRING 'WLXXJN11(KDTPOTYP =' W-4512-KDTPOTYP-X                       
344900                    '&KDORDKL  =' W-4512-KDORDKL-X                        
345000                    '&IDDISTRF<=' W-4512-IDDISTR-FOM-X                    
345100                    '&IDDISTRT>=' W-4512-IDDISTR-TOM-X ')'                
345200          DELIMITED BY SIZE INTO SSA2                                     
345300     MOVE '    ' TO GODK-STATUSKODER                                      
345400     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA-SUB SSA1 SSA2             
345500     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
345600     PERFORM IMS-STATUSKONTROLL                                           
345700     .                                                                    
345800     EJECT                                                                
345900*                                                                         
346000 IMS-GU-ARTC11 SECTION.                                                   
346100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
346200          DELIMITED BY SIZE INTO SSA1                                     
346300     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
346400          DELIMITED BY SIZE INTO SSA2                                     
346500     MOVE '    ' TO GODK-STATUSKODER                                      
346600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                
346700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
346800     PERFORM IMS-STATUSKONTROLL                                           
346900     .                                                                    
347000     SKIP3                                                                
347100 IMS-GHU-ARTC11 SECTION.                                                  
347200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
347300          DELIMITED BY SIZE INTO SSA1                                     
347400     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
347500          DELIMITED BY SIZE INTO SSA2                                     
347600     MOVE '    ' TO GODK-STATUSKODER                                      
347700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2               
347800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
347900     PERFORM IMS-STATUSKONTROLL                                           
348000     .                                                                    
348100     SKIP3                                                                
348200 IMS-REPL-ARTC11 SECTION.                                                 
348300                                                                          
348400     MOVE '  ' TO GODK-STATUSKODER                                        
348500     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA4                        
348600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
348700     PERFORM IMS-STATUSKONTROLL                                           
348800     .                                                                    
348900     EJECT                                                                
349000 IMS-ISRT-WDG6 SECTION.                                                   
349100     MOVE 'WLZZAC01' TO SSA1                                              
349200     MOVE '  II' TO GODK-STATUSKODER                                      
349300     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA-TR SSA1                 
349400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
349500     PERFORM IMS-STATUSKONTROLL                                           
349600     .                                                                    
349700     SKIP3                                                                
349800 IMS-ISRT-WDL901 SECTION.                                                 
349900                                                                          
350000     MOVE 'WLLOGA01 ' TO SSA1                                             
350100     MOVE '  II' TO GODK-STATUSKODER                                      
350200     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
350300     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
350400     PERFORM IMS-STATUSKONTROLL                                           
350500     .                                                                    
350600     EJECT                                                                
350700 IMS-STATUSKONTROLL SECTION.                                              
350800                                                                          
350900     SET STATUS-IX TO 1                                                   
351000     SEARCH GODK-STATUS                                                   
351100       AT END                                                             
351200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
351300         DELIMITED BY SIZE INTO FELTEXT                                   
351400         CALL FELLOG                                                      
351500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
351600     END-SEARCH                                                           
351700     .                                                                    
