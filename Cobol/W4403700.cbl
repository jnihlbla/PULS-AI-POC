000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4403700                                         
000400 AUTHOR.                 ANNELIE ENGLUND                                  
000500 DATE-WRITTEN.           SEPT 1990                                        
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*                                                                         
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        PROGRAMMET ÄR EN BMP SOM SKALL KÖRAS VARJE TIMMA UNDER           
001300*        DAGTID.                                                          
001400*        DET SKALL AKTIVERA OBEKRÄFTADE TPO-RADER AV TYP 2 OCH 6          
001500*        SOM FALLIT FÖR BEDÖMNING OCH AV NÅGON ANLEDNING EJ BLIVIT        
001600*        BEDÖMDA INOM DEN TIDSRAM SOM FANNS.                              
001700*                                                                         
001800*        PROGRAMMET PRISTILLÄMPAR MHA                                     
001900*                               W335PRIS                                  
002000*                                                                         
002100*        PROGRAMMET LÄSER OCH                                             
002200*                   UPPDATERAR  WLORDP  (WDA501) RESTORDER/TPO-REG        
002300*                               WLXXBU  (WDR501)                          
002400*                               WLARTM  (WDK901) ARTIKEL-REG              
002500*                                       (WDK911)                          
002600*        PROGRAMMET UPPDATERAR  WLZZAC  (WDG601) TRANSAKTIONSBAS          
002700*                               WLORQM  (WDQ101) ORDERBEKR-REG            
002800*                                                                         
002900*        PROGRAMMET LÄSER       WLGMTA  (WDB201) GODSMOTTAGAR-REG         
003000*                               WLARTC  (WDK601) ARTIKEL-REG              
003100*                                       (WDK611)                          
003200*                               WLXXBX  (WDR201) ÖVERSÄTTN ANSK-LA        
003300*                                       (WDR210)                          
003400*                               WLORQI  (WDQ201) ORDERHUVUD-REG           
003500*                                                                         
003600*    040830  SM   INLÄGGNING AV PRISFRÅGOR FÖR DNI                        
003700*    070822  E'TRACKER: 5444132                                           
003800*    081101  E'TRACKER: 7450328  VOHF                                     
003900*    081101  E'TRACKER:10254592  DECOMISSION VOHF                         
004000*                                                                         
004100     EJECT                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP3                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004800*    -COPY WY2000W1                                                       
004900     SKIP3                                                                
005000*    ---- ARBETSVARIABLER                                                 
005100*                                                                         
005200 77  IDPGM                   PIC X(08)  VALUE 'W4403700'.                 
005300 77  FELTEXT                 PIC X(80)  VALUE SPACE.                      
005400                                                                          
005500 77  MSG-IO-AREA-LENGTH      PIC S9(9) VALUE +32 COMP SYNC.               
005600 77  MSG-IO-AREA             PIC X(32) VALUE SPACE.                       
005700 77  CHKP-AREA-1-LENGTH      PIC S9(9) VALUE +32 COMP SYNC.               
005800 77  CHKP-AREA-1             PIC X(32) VALUE SPACE.                       
005900                                                                          
006000 77  RAKNARE                 PIC 9(3)   VALUE ZERO.                       
006100 77  W-KVOKS-VOR             PIC 9(7)   VALUE ZERO.                       
006200 77  W-KVOKS-DAG             PIC 9(7)   VALUE ZERO.                       
006300 77  W-KVOKS-BULK            PIC 9(7)   VALUE ZERO.                       
006400 77  W-SUTPO-TOT             PIC 9(7)   VALUE ZERO.                       
006500 77  W-SUTPO-PB              PIC 9(7)   VALUE ZERO.                       
006600 77  2109-IX                 PIC S9(9)  VALUE ZERO COMP SYNC.             
006700 77  2109-IX-MAX             PIC S9(9)  VALUE +18  COMP SYNC.             
006800 77  W-DAT-TIAAP             PIC 9(3).                                    
006900 77  DATUM-MED-ARHUNDR       PIC 9(8)   VALUE ZERO.                       
007000                                                                          
007100 77  W-TIAAMMDD              PIC 9(6).                                    
007200 01  W-TIAAAAMMDD-Y2K        PIC 9(8).                                    
007300 01  W-TIKLOCK               PIC 9(8).                                    
007400 01  W-TIHHMMSSHT REDEFINES W-TIKLOCK.                                    
007500     03  W-TIHHMMSS          PIC 9(6).                                    
007600     03  W-TIHT              PIC 9(2).                                    
007700 01  W-TPO-TIAAAAVV.                                                      
007800     03 W-TPO-SEKEL          PIC 9(2).                                    
007900     03 W-TPO-TIAAVV         PIC 9(4).                                    
008000                                                                          
008100 77  WS-IDPRQUES             PIC S9(7)   VALUE +0.                        
008200                                                                          
008300 77  KDRC-DISPLAY            PIC Z(5)    VALUE ZERO.                      
008400                                                                          
008500*                                                                         
008600*    ---- KONSTANTER                                                      
008700 77  JA                      PIC X       VALUE 'J'.                       
008800 77  NEJ                     PIC X       VALUE 'N'.                       
008900     EJECT                                                                
009000*    --- ARBETSAREA FÖR BESTÄMNING AV LAGER                               
009100 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
009200*01  FILLER  -COPY WWDIST79   -RED TEST-IDDISTR.                          
009300                                                                          
009310*01  FILLER  -COPY WWBYT03                                                
009320     EJECT                                                                
009400*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
009500     SKIP3                                                                
009600 01  FILLER                  PIC X(16)   VALUE 'PRISTILLAREA '.           
009700                                                                          
009800*01  -COPY W335PRIS                                                       
009900     EJECT                                                                
010000 01  FILLER                  PIC X(16)   VALUE 'W335PRNO     '.           
010100                                                                          
010200*01  -COPY W335PRNO                                                       
010300     EJECT                                                                
010400 01  FILLER                  PIC X(16)   VALUE 'W335PRQU     '.           
010500                                                                          
010600*01  -COPY W335PRQU                                                       
010700     EJECT                                                                
010800 01  FILLER                  PIC X(16)   VALUE 'RY9-TRANSAREA'.           
010900                                                                          
011000*01  -COPY WDGZRY9                                                        
011100     EJECT                                                                
011200*01    -COPY WDGZRY9S                                                     
011300     EJECT                                                                
011400 01  FILLER                  PIC X(16)   VALUE 'RY4-TRANSAREA'.           
011500     SKIP2                                                                
011600*01  -COPY W440300           -PRE RY4-                                    
011700     EJECT                                                                
011800 01  FILLER                  PIC X(16)   VALUE 'RZA-TRANSAREA'.           
011900     SKIP2                                                                
012000*01  -COPY WDGZRZA                                                        
012100     EJECT                                                                
012200*01  -COPY W092P001   -PRE SORT-                                          
012300     EJECT                                                                
012400 01  DYNAMISKA-SUBPROGRAM.                                                
012500   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
012600   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
012700   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
012800   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
012900   03  W335PRIS              PIC X(8)    VALUE 'W335PRIS'.                
013000   03  W335PRNO              PIC X(8)    VALUE 'W335PRNO'.                
013100   03  W335PRQU              PIC X(8)    VALUE 'W335PRQU'.                
013200   03  W411ARTM              PIC X(8)    VALUE 'W411ARTM'.                
013300   03  WZ01SEND              PIC X(8)    VALUE 'WZ01SEND'.                
013400     EJECT                                                                
013500*    ---- PARAMETRAR TILL WDATKONV                                        
013600                                                                          
013700 01  RKOD-ABEND              PIC S9(4)   VALUE +33 COMP SYNC.             
013800 01  RKOD-ABEND-MED-DUMP     PIC S9(4)   VALUE +33 COMP SYNC.             
013900                                                                          
014000*    ---- PARAMETRAR TILL WDATKONV                                        
014100                                                                          
014200*    -COPY WDATAREA                                                       
014300     EJECT                                                                
014400*    ---- PARAMETRAR TILL W411ARTM                                        
014500                                                                          
014600*    -COPY W411ARTM                                                       
014700     EJECT                                                                
014800*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
014900                                                                          
015000 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
015100     SKIP3                                                                
015200*    ---- STATUSKOD FRÅN IMS                                              
015300                                                                          
015400 01  STATUS-WS               PIC XX.                                      
015500     88  SEGMENT-SLUT                     VALUE 'GB'.                     
015600     88  SEGMENT-FINNS                    VALUE '  '.                     
015700     88  SEGMENT-HAR-LAGTS-TILL           VALUE '  '.                     
015800     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
015900     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
016000     88  IMS-EJ-OK                        VALUE 'XD'.                     
016100     SKIP3                                                                
016200 01  GODK-STATUSKODER.                                                    
016300   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
016400     SKIP3                                                                
016500 01  SSA1                    PIC X(224).                                  
016600 01  SSA2                    PIC X(224).                                  
016700 01  SSA3                    PIC X(224).                                  
016800     EJECT                                                                
016900*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
017000 01  FILLER                  PIC X(16)   VALUE 'NYCKLAR-TILL-DLI'.        
017100     SKIP2                                                                
017200 01  NYCKLAR-TILL-DLI.                                                    
017300                                                                          
017400   03  W-WDGX2223-X.                                                      
017500     05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.                   
017600     05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.              
017700     05  W-VALFRI-2223       PIC X(24)    VALUE LOW-VALUE.                
017800                                                                          
017900   03  W-WDGX2224-X.                                                      
018000     05  W-TISENBEK-DAG      PIC S9(7)    VALUE ZERO COMP-3.              
018100     05  W-TISENBEK-KL       PIC S9(7)    VALUE ZERO COMP-3.              
018200     05  W-KDLARM            PIC S9(3)    VALUE ZERO COMP-3.              
018300                                                                          
018400   03  W-WDGX2231-X.                                                      
018500       05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.                 
018600       05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.              
018700                                                                          
018800   03  W-WDGX2232-X.                                                      
018900       05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.            
019000       05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.              
019100                                                                          
019200   03  W-IDARTNR-X.                                                       
019300     05  W-IDARTNR           PIC S9(9)  COMP-3.                           
019400                                                                          
019500   03  W-IDGMT-X.                                                         
019600     05  W-IDDISTR           PIC S9(5)    COMP-3.                         
019700     05  W-IDKUNDNR          PIC S9(7)    COMP-3.                         
019800                                                                          
019900   03  W-DABEHOV-X.                                                       
020000     05  W-DABEHOV          PIC  9(6).                                    
020100                                                                          
020200   03  W-KDSEGKEY-X.                                                      
020300     05  W-KDSEGKEY          PIC X      VALUE '1'.                        
020400                                                                          
020500   03  W1-DASENBEK-X.                                                     
020600     05  W1-DASENBEK-DAG    PIC  9(8).                                    
020700     05  W1-TISENBEK-KL     PIC  9(6).                                    
020800                                                                          
020900   03  W2-DASENBEK-X.                                                     
021000     05  W2-DASENBEK-DAG    PIC  9(8).                                    
021100     05  W2-TISENBEK-KL     PIC  9(6).                                    
021200                                                                          
021300   03  W-WDA501KY-X.                                                      
021400     05  W1-IDDISTR         PIC S9(5)   COMP-3.                           
021500     05  W1-IDKUNDNR        PIC S9(7)   COMP-3.                           
021600     05  W1-IDKUNDRF        PIC X(10)   VALUE SPACE.                      
021700     05  W1-IDARTNR         PIC S9(9)   COMP-3.                           
021800     05  W1-IDLOPNR         PIC S9(3)   COMP-3.                           
021900   03  W-W1-IDDC-X.                                                       
022000     05  W1-IDDC            PIC  X(2).                                    
022100                                                                          
022200   03  W-WDQ2CSEQ-X.                                                      
022300     05  W2-IDDISTR         PIC S9(5)   COMP-3.                           
022400     05  W2-IDKUNDNR        PIC S9(7)   COMP-3.                           
022500     05  W2-IDKUNDRF        PIC X(10)   VALUE SPACE.                      
022600                                                                          
022700   03  W-IDDC-B6-X.                                                       
022800       05 W-IDDC-B6                  PIC X(2).                            
022900     EJECT                                                                
023000                                                                          
023100     SKIP2                                                                
023200*    -COPY W0003                                                          
023300     EJECT                                                                
023400* ---         DLI INOUT OUTPUT AREA                                       
023500* ---         DLI-IO-AREA                                                 
023600                                                                          
023700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-XXBU11'.           
023800 01  DLI-IO-XXBU11.                                                       
023900*  03  WLXXBU11  -COPY WDGX2224  -PRE XXBU11-                             
024000     EJECT                                                                
024100                                                                          
024200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK901'.           
024300 01  DLI-IO-WDK901.                                                       
024400*  03  WDK901  -COPY WDK901                                               
024500     EJECT                                                                
024600                                                                          
024700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK911'.           
024800 01  DLI-IO-WDK911.                                                       
024900*  03  WDK901  -COPY WDK911                                               
025000     EJECT                                                                
025100                                                                          
025200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ZZAC01'.           
025300 01  DLI-IO-ZZAC01.                                                       
025400*  03  WLZZAC01  -COPY WDGZ01  -PRE ZZAC-                                 
025500     EJECT                                                                
025600                                                                          
025700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDA501'.           
025800 01  DLI-IO-WDA501.                                                       
025900*  03  WDA501  -COPY WDA501  -PRE ORDP-                                   
026000     EJECT                                                                
026100                                                                          
026200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-GMTA01'.           
026300 01  DLI-IO-GMTA01.                                                       
026400*  03  WLGMTA01  -COPY WDB201  -PRE GMTA-                                 
026500     EJECT                                                                
026600                                                                          
026700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK601'.           
026800 01  DLI-IO-WDK601.                                                       
026900*  03  WDK601  -COPY WDK601                                               
027000     EJECT                                                                
027100                                                                          
027200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK611'.           
027300 01  DLI-IO-WDK611.                                                       
027400*  03  WDK611  -COPY WDK611                                               
027500     EJECT                                                                
027600                                                                          
027700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-XXBX11'.           
027800 01  DLI-IO-XXBX11.                                                       
027900*  03  WLXXBX11  -COPY WDGX2232     -PRE XXBX-                            
028000     EJECT                                                                
028100                                                                          
028200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDQ201'.           
028300 01  DLI-IO-WDQ201.                                                       
028400*  03  WLWDQ201  -COPY WDQ201       -PRE ORQI-                            
028500     EJECT                                                                
028600                                                                          
028700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ORQM01'.           
028800 01  DLI-IO-ORQM01.                                                       
028900*  03  WLORQM01  -COPY WDQ101       -PRE ORQM-                            
029000     EJECT                                                                
029100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
029200 01   DLI-IO-AREA-B601.                                                   
029300*     03  -COPY WDB601                                                    
029400*    MSG-AREA FÖR HOPP TILL W20109                                        
029500 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
029600 01  W-PROG-TO-PROG-SW-1.                                                 
029700     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
029800     03  2109-Z1                   PIC X.                                 
029900     03  2109-Z2                   PIC X.                                 
030000     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
030100     03  2109-IDTRANS              PIC X(4)  VALUE '440 '.                
030200     03  2109-KDMFSFOR             PIC X     VALUE '1'.                   
030300*    03  -COPY W2I10902    -PRE 2109-                                     
030400     EJECT                                                                
030500 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
030600     SKIP3                                                                
030700 01  -COPY WZ01SEND                                                       
030800     EJECT                                                                
030900 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
031000     SKIP3                                                                
031100 01  SEND-AREA.                                                           
031200*    03  -COPY WZ01REQU  -PRE 3039-                                       
031300*    03  -COPY W30391I1  -PRE 3039-                                       
031400     EJECT                                                                
031500 LINKAGE SECTION.                                                         
031600     SKIP2                                                                
031700*01  -COPY W0009      -PRE  MSG-                                          
031800     EJECT                                                                
031900*01  -COPY W0009      -PRE  2109-                                         
032000     EJECT                                                                
032100*01  -COPY W0009      -PRE PRQRY-                                         
032200     SKIP2                                                                
032300*01  -COPY W0008      -PRE  XXBU-                                         
032400       05  FILLER                PIC X.                                   
032500     EJECT                                                                
032600*01  -COPY W0008      -PRE  WDK9-                                         
032700       05  FILLER                PIC X.                                   
032800     EJECT                                                                
032900*01  -COPY W0008      -PRE  ZZAC-                                         
033000       05  FILLER                PIC X.                                   
033100     EJECT                                                                
033200*01  -COPY W0008      -PRE  WDA51-                                        
033300       05  FILLER                PIC X.                                   
033400     EJECT                                                                
033500*01  -COPY W0008      -PRE  WDA52-                                        
033600       05  FILLER                PIC X.                                   
033700     EJECT                                                                
033800*01  -COPY W0008      -PRE  GMTA-                                         
033900       05  FILLER                PIC X.                                   
034000     EJECT                                                                
034100*01  -COPY W0008      -PRE  WDK6-                                         
034200       05  FILLER                PIC X.                                   
034300     EJECT                                                                
034400*01  -COPY W0008      -PRE  XXBX-                                         
034500       05  FILLER                PIC X.                                   
034600     EJECT                                                                
034700*01  -COPY W0008      -PRE  WDQ2-                                         
034800       05  FILLER                PIC X.                                   
034900     EJECT                                                                
035000*01  -COPY W0008      -PRE  ORQM-                                         
035100       05  FILLER                PIC X.                                   
035200     EJECT                                                                
035300*01  -COPY W0008      -PRE  WDB6-                                         
035400       05  FILLER                PIC X.                                   
035500     EJECT                                                                
035600 01  PRIS-ARTC-PCB               PIC X.                                   
035700 01  PRIS-WDK7-PCB               PIC X.                                   
035710 01  PRIS-GMTA-PCB               PIC X.                                   
035800 01  PRIS-BETA-PCB               PIC X.                                   
035900 01  PRIS-GPRIA-PCB              PIC X.                                   
036000 01  PRIS-GPRIB-PCB              PIC X.                                   
036100 01  PRIS-COST-WDK6-PCB          PIC X.                                   
036200 01  PRIS-COST-WDK7-PCB          PIC X.                                   
036300 01  PRIS-COST-WDF1-PCB          PIC X.                                   
036400 01  PRIS-COST-9305-PCB          PIC X.                                   
036500 01  PRIS-COST-WDK72-PCB         PIC X.                                   
036600 01  PRIS-COST-WDB6-PCB          PIC X.                                   
036800     EJECT                                                                
036900 01  ARTM-ARTM-PCB               PIC X.                                   
037000     EJECT                                                                
037100 01  PRNO-3107-PCB               PIC X.                                   
037200 01  PRQU-WDG2-PCB               PIC X.                                   
037300 01  PRQU-WDC7-PCB               PIC X.                                   
037400 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
037500     EJECT                                                                
037600 PROCEDURE DIVISION  USING MSG-PCB  2109-PCB     PRQRY-PCB                
037700                     XXBU-PCB       WDK9-PCB      ZZAC-PCB                
037800                     WDA51-PCB      WDA52-PCB                             
037900                     GMTA-PCB                     WDK6-PCB                
038000                     XXBX-PCB       WDQ2-PCB      ORQM-PCB                
038100                     WDB6-PCB                                             
038200                     PRIS-ARTC-PCB                                        
038210                     PRIS-WDK7-PCB                                        
038300                     PRIS-GMTA-PCB                                        
038400                     PRIS-BETA-PCB                                        
038500                     PRIS-GPRIA-PCB PRIS-GPRIB-PCB                        
038600                     PRIS-COST-WDK6-PCB                                   
038700                     PRIS-COST-WDK7-PCB                                   
038800                     PRIS-COST-WDF1-PCB                                   
038900                     PRIS-COST-9305-PCB                                   
039000                     PRIS-COST-WDK72-PCB                                  
039100                     PRIS-COST-WDB6-PCB                                   
039300                     ARTM-ARTM-PCB                                        
039400                     PRNO-3107-PCB                                        
039500                     PRQU-WDG2-PCB                                        
039600                     PRQU-WDC7-PCB                                        
039700                     PRQU-SJKO-WDK6-PCB.                                  
039800                                                                          
039900     ENTRY 'DLITCBL' USING MSG-PCB  2109-PCB     PRQRY-PCB                
040000                     XXBU-PCB       WDK9-PCB      ZZAC-PCB                
040100                     WDA51-PCB      WDA52-PCB                             
040200                     GMTA-PCB                     WDK6-PCB                
040300                     XXBX-PCB       WDQ2-PCB   ORQM-PCB                   
040400                     WDB6-PCB                                             
040500                     PRIS-ARTC-PCB                                        
040510                     PRIS-WDK7-PCB                                        
040600                     PRIS-GMTA-PCB                                        
040700                     PRIS-BETA-PCB                                        
040800                     PRIS-GPRIA-PCB PRIS-GPRIB-PCB                        
040900                     PRIS-COST-WDK6-PCB                                   
041000                     PRIS-COST-WDK7-PCB                                   
041100                     PRIS-COST-WDF1-PCB                                   
041200                     PRIS-COST-9305-PCB                                   
041300                     PRIS-COST-WDK72-PCB                                  
041400                     PRIS-COST-WDB6-PCB                                   
041600                     ARTM-ARTM-PCB                                        
041700                     PRNO-3107-PCB                                        
041800                     PRQU-WDG2-PCB                                        
041900                     PRQU-WDC7-PCB                                        
042000                     PRQU-SJKO-WDK6-PCB.                                  
042100                                                                          
042200                                                                          
042300     PERFORM A-INIT                                                       
042400                                                                          
042500     MOVE    LOW-VALUE  TO W1-DASENBEK-X                                  
042600     MOVE    W-TIAAAAMMDD-Y2K TO W2-DASENBEK-DAG                          
042700     MOVE    W-TIHHMMSS TO W2-TISENBEK-KL                                 
042800     PERFORM IMS-GU-ORDP1-WDA5                                            
042900                                                                          
043000     PERFORM UNTIL SEGMENT-SAKNAS                                         
043100       IF ORDP-RAD-IDDC NOT = DCS-IDDC                                    
043200          MOVE ORDP-RAD-IDDC  TO W-IDDC-B6                                
043300          PERFORM IMS-GU-WDB601                                           
043400       END-IF                                                             
043500       IF DCS-CDC                                                         
043600          MOVE ORDP-RAD-IDDISTR  TO W-IDDISTR                             
043700                                    W1-IDDISTR                            
043800                                    W2-IDDISTR                            
043900          MOVE ORDP-RAD-IDARTNR  TO W-IDARTNR                             
044000          MOVE ORDP-RAD-IDKUNDNR TO W-IDKUNDNR                            
044100                                    W1-IDKUNDNR                           
044200                                    W2-IDKUNDNR                           
044300          MOVE ORDP-RAD-IDKUNDRF TO W1-IDKUNDRF                           
044400          MOVE '00'              TO W2-IDKUNDRF(1:2)                      
044500          MOVE ORDP-RAD-IDORDNR5 TO W2-IDKUNDRF(3:5)                      
044600          MOVE ORDP-RAD-IDARTNR  TO W1-IDARTNR                            
044700          MOVE ORDP-RAD-IDLOPNR  TO W1-IDLOPNR                            
044800          MOVE ORDP-RAD-IDDC     TO W1-IDDC                               
044900          PERFORM IMS-GHU-ORDP2-WDA5                                      
045000          IF ORDP-RAD-TITPO = ZERO                                        
045100            PERFORM B-BEHANDLA-TPO2-TPO6-RAD                              
045200          ELSE                                                            
045300            PERFORM C-BEHANDLA-TPO2-MED-DATUM-FRAM                        
045400          END-IF                                                          
045500          PERFORM D-TA-BORT-FRAN-LARMKO                                   
045600          IF RAKNARE > 10                                                 
045700            PERFORM IMS-CHECKPOINT                                        
045800            MOVE ZERO TO RAKNARE                                          
045900          END-IF                                                          
046000          ADD +1 TO RAKNARE                                               
046100       END-IF                                                             
046200       PERFORM IMS-GN-ORDP1-WDA5                                          
046300     END-PERFORM                                                          
046400     IF 2109-MID2-KVANTART > ZERO                                         
046500       PERFORM S06A-STARTA-2109                                           
046600     END-IF                                                               
046700                                                                          
046800     GOBACK                                                               
046900     .                                                                    
047000     EJECT                                                                
047100 A-INIT SECTION.                                                          
047200                                                                          
047300     PERFORM IMS-RESTART                                                  
047400     MOVE ZERO TO RAKNARE                                                 
047500                                                                          
047600     ACCEPT W-TIAAMMDD FROM DATE                                          
047700     ACCEPT W-TIKLOCK  FROM TIME                                          
047800     MOVE FUNCTION CURRENT-DATE (1:8) TO W-TIAAAAMMDD-Y2K                 
047900                                                                          
048000     MOVE SPACE                 TO 2109-MID2-W2I10902                     
048100     MOVE +1                    TO 2109-IX                                
048200     .                                                                    
048300     EJECT                                                                
048400 B-BEHANDLA-TPO2-TPO6-RAD SECTION.                                        
048500                                                                          
048600     MOVE ORDP-RAD-IDDISTR      TO TEST-IDDISTR                           
048700     IF DIST79-DEALER-PRICE                                               
048800       PERFORM S20-KOMPLETTERA-PRIS                                       
048900     ELSE                                                                 
049100       IF ORDP-RAD-PRARTNTO = ZERO                                        
049200        PERFORM S02-PRISTILLEMPA                                          
049300       END-IF                                                             
049400     END-IF                                                               
049500     EVALUATE ORDP-RAD-KDORDKL                                            
049600       WHEN 0                                                             
049700         MOVE ORDP-RAD-KVART TO W-KVOKS-VOR                               
049800       WHEN 1                                                             
049900         MOVE ORDP-RAD-KVART TO W-KVOKS-DAG                               
050000       WHEN 2                                                             
050100         MOVE ORDP-RAD-KVART TO W-KVOKS-BULK                              
050200       WHEN 3                                                             
050300         MOVE ORDP-RAD-KVART TO W-KVOKS-BULK                              
050400       WHEN 4                                                             
050500         MOVE ORDP-RAD-KVART TO W-KVOKS-BULK                              
050600     END-EVALUATE                                                         
050700     MOVE    JA         TO ORDP-RAD-FLTPOBEK                              
050800     MOVE    3          TO ORDP-RAD-KDSTARAD                              
050900     MOVE    W-TIAAMMDD TO ORDP-RAD-TITPO                                 
051000                           ORDP-RAD-TIRES                                 
051100     PERFORM IMS-REPL-ORDP2-WDA5                                          
051200                                                                          
051300     MOVE ORDP-RAD-TITPO   TO DAT-I-TIDATUM                               
051400     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
051500     CALL WDATKONV USING DAT-KDDATFORM                                    
051600                         DAT-I-TIDATUM                                    
051700                         DAT-O-TIDATUM                                    
051800                         DAT-KDSVAR                                       
051900     IF DAT-KDSVAR-OK                                                     
052000       MOVE DAT-TIAAVV-GRP TO W-TPO-TIAAVV                                
052100       MOVE DAT-TISEKEL    TO W-TPO-SEKEL                                 
052200     ELSE                                                                 
052300       MOVE 'FEL FRÅN PROGRAM W44037 I SECTION B' TO FELTEXT              
052400       CALL ABEND USING RKOD-ABEND                                        
052500     END-IF                                                               
052600                                                                          
052700     PERFORM S01-UPPD-SALDO-WDK9                                          
052800     PERFORM S03-SKAPA-RZA-TRANS                                          
052900     PERFORM S05-SKAPA-RY9-TRANS                                          
053000     PERFORM S04-SKAPA-RY4-TRANS                                          
053100     PERFORM BB-SKAPA-ORDERBEK                                            
053200     IF ORDP-RAD-KDTPOTYP NOT = +6                                        
053300       PERFORM S06-SKAPA-W2I10902                                         
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700 BB-SKAPA-ORDERBEK SECTION.                                               
053800                                                                          
053900     MOVE '0000000   '              TO ORQM-OBKR-IDKUNDRF                 
054000     MOVE ORDP-RAD-IDKUNDRF         TO ORQM-OBKR-IDKUNDRF(3:5)            
054100     MOVE ORQI-OHUV-IDORDER         TO ORQM-OBKR-IDORDER                  
054200     MOVE ORDP-RAD-IDARTNR          TO ORQM-OBKR-IDARTNR                  
054300     MOVE +1                        TO ORQM-OBKR-IDLOPNR                  
054400                                       ORQM-OBKR-IDSEKVNR                 
054500     MOVE ORDP-RAD-IDDC             TO ORQM-OBKR-IDDC                     
054600     MOVE ORDP-RAD-IDDC-RO          TO ORQM-OBKR-IDDC-RO                  
054700     MOVE 71                        TO ORQM-OBKR-KDORDBEK                 
054800     MOVE IDPGM                     TO ORQM-OBKR-IDPGM                    
054900     MOVE SPACE                     TO ORQM-OBKR-BEERS                    
055000     MOVE SPACE                     TO ORQM-OBKR-IDBIL                    
055100     MOVE ORDP-RAD-BEKUNDRF         TO ORQM-OBKR-BEKUNDRF                 
055200     MOVE ORDP-RAD-BERADREF         TO ORQM-OBKR-BERADREF                 
055300     MOVE ORDP-RAD-BEVOLREF         TO ORQM-OBKR-BEVOLREF                 
055400     MOVE ORDP-RAD-IDKAMPRF         TO ORQM-OBKR-IDKAMPRF                 
055500     MOVE ZERO                      TO ORQM-OBKR-DIERS-KVOT               
055600     MOVE NEJ                       TO ORQM-OBKR-FLAKPLOC                 
055700     MOVE ORDP-RAD-FLINVEST         TO ORQM-OBKR-FLINVEST                 
055800     MOVE JA                        TO ORQM-OBKR-FLOBOK                   
055900     MOVE NEJ                       TO ORQM-OBKR-FLOBTRAN                 
056000                                       ORQM-OBKR-FLOBPRT                  
056100     MOVE ORDP-RAD-FLPRTILL         TO ORQM-OBKR-FLPRTILL                 
056200     MOVE JA                        TO ORQM-OBKR-FLRESTN                  
056300     MOVE NEJ                       TO ORQM-OBKR-FLSLATT                  
056400     MOVE ORDP-RAD-FLERS            TO ORQM-OBKR-FLTILLK                  
056500     MOVE ZERO                      TO ORQM-OBKR-IDARTNR-TILLK            
056600     MOVE ORDP-RAD-IDDISTR          TO ORQM-OBKR-IDDISTR                  
056700     MOVE ORDP-RAD-IDKUNDNR         TO ORQM-OBKR-IDKUNDNR                 
056800     MOVE '0000000   '              TO ORQM-OBKR-IDKUNDRF-RO              
056900     MOVE ORDP-RAD-IDLEVNR          TO ORQM-OBKR-IDLEVNR                  
057000     MOVE ORDP-RAD-IDLOPNR          TO ORQM-OBKR-IDLOPNR-RO               
057100     MOVE ORDP-RAD-IDSYSTEM         TO ORQM-OBKR-IDSYSTEM                 
057200     MOVE ORDP-RAD-KDDSP            TO ORQM-OBKR-KDDSP                    
057300     MOVE ZERO                      TO ORQM-OBKR-KDERS                    
057400     MOVE ORDP-RAD-KDKVBRYT         TO ORQM-OBKR-KDKVBRYT                 
057500     MOVE ORDP-RAD-KDOI             TO ORQM-OBKR-KDOI                     
057600     MOVE ORDP-RAD-CLEARGROUP       TO ORQM-OBKR-CLEARGROUP               
057700     MOVE ORDP-RAD-KDPRTYP          TO ORQM-OBKR-KDPRTYP                  
057800     MOVE ORDP-RAD-KDTPOTYP         TO ORQM-OBKR-KDTPOTYP                 
057900     MOVE ORDP-RAD-KDVRINFO         TO ORQM-OBKR-KDVRINFO                 
058000     MOVE ZERO                      TO ORQM-OBKR-KVANNANT                 
058100                                       ORQM-OBKR-KVAVBART                 
058200     MOVE ORDP-RAD-KVART            TO ORQM-OBKR-KVBEART                  
058300                                       ORQM-OBKR-KVBEART-Q                
058400     MOVE ZERO                      TO ORQM-OBKR-KVBEART-TILLK            
058500                                       ORQM-OBKR-KVPREAVB                 
058600                                       ORQM-OBKR-KVPRERO                  
058700                                       ORQM-OBKR-KVQPACK                  
058800                                       ORQM-OBKR-KVRO                     
058900                                       ORQM-OBKR-KVSLATT                  
059000     MOVE ORDP-RAD-PRARTNTO         TO ORQM-OBKR-PRARTNTO                 
059100     MOVE ZERO                      TO ORQM-OBKR-PRBPRIS                  
059200     MOVE ORDP-RAD-REKSIFFR         TO ORQM-OBKR-REKSIFFR                 
059300     MOVE ZERO                      TO ORQM-OBKR-REKSIFFR-TILLK           
059400                                       ORQM-OBKR-RERF-RAD                 
059500     MOVE ORDP-RAD-TIREGDAT         TO ORQM-OBKR-TIORDREG                 
059600     MOVE ZERO                      TO ORQM-OBKR-TIPRIS                   
059700                                       ORQM-OBKR-TIRODAT                  
059800     MOVE W-TIAAMMDD                TO ORQM-OBKR-TIREGDAT                 
059900     MOVE W-TIKLOCK                 TO ORQM-OBKR-TIREGTID                 
060000                                                                          
060100     IF  ORQM-OBKR-TIREGDAT > 500000                                      
060200       ADD +19000000 TO ORQM-OBKR-TIREGDAT GIVING                         
060300                                     DATUM-MED-ARHUNDR                    
060400     ELSE                                                                 
060500       ADD +20000000 TO ORQM-OBKR-TIREGDAT GIVING                         
060600                                     DATUM-MED-ARHUNDR                    
060700     END-IF                                                               
060800                                                                          
060900     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
061000                                     ORQM-OBKR-TITIREGD-9KOMPL            
061100     MOVE ORDP-RAD-TITPO            TO ORQM-OBKR-TITPO                    
061200                                                                          
061300     IF  ORQM-OBKR-TIORDREG > 500000                                      
061400       ADD +19000000 TO ORQM-OBKR-TIORDREG GIVING                         
061500                                     DATUM-MED-ARHUNDR                    
061600     ELSE                                                                 
061700       ADD +20000000 TO ORQM-OBKR-TIORDREG GIVING                         
061800                                     DATUM-MED-ARHUNDR                    
061900     END-IF                                                               
062000                                                                          
062100     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
062200                                     ORQM-OBKR-TITIORDD-9KOMPL            
062300     MOVE ORDP-RAD-KDFRAKT          TO ORQM-OBKR-KDFRAKT                  
062400     MOVE ORDP-RAD-KDORDKL          TO ORQM-OBKR-KDORDKL                  
062500     MOVE ORDP-RAD-DEAL-PR-LINE     TO ORQM-OBKR-DEAL-PR-LINE             
062600                                                                          
062700     MOVE ORDP-RAD-KDORDTYP-LDC     TO ORQM-OBKR-KDORDTYP-LDC             
062800     MOVE ORDP-RAD-TIREPDAT         TO ORQM-OBKR-TIREPDAT                 
062900     MOVE ORDP-RAD-IDKUNDRF-WIP     TO ORQM-OBKR-IDKUNDRF-WIP             
063010     MOVE ZERO                      TO ORQM-OBKR-TIDLEVDAT                
063020     MOVE ORDP-RAD-PRAVCOST         TO ORQM-OBKR-PRAVCOST                 
063030     MOVE ORDP-RAD-KDVALISO         TO ORQM-OBKR-KDVALISO                 
063100                                                                          
063200     PERFORM IMS-ISRT-ORQM-WDQ1                                           
063300     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
063400       ADD +1 TO ORQM-OBKR-IDLOPNR                                        
063500       PERFORM IMS-ISRT-ORQM-WDQ1                                         
063600     END-PERFORM                                                          
063700     .                                                                    
063800     EJECT                                                                
063900 C-BEHANDLA-TPO2-MED-DATUM-FRAM SECTION.                                  
064000                                                                          
064100******************************************************************        
064200                                                                          
064300     MOVE ORDP-RAD-KVART    TO W-SUTPO-TOT                                
064400                               W-SUTPO-PB                                 
064500                                                                          
064600     MOVE ORDP-RAD-TITPO   TO DAT-I-TIDATUM                               
064700     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
064800     CALL WDATKONV USING DAT-KDDATFORM                                    
064900                         DAT-I-TIDATUM                                    
065000                         DAT-O-TIDATUM                                    
065100                         DAT-KDSVAR                                       
065200     IF DAT-KDSVAR-OK                                                     
065300       MOVE DAT-TIAAVV-GRP TO W-TPO-TIAAVV                                
065400       MOVE DAT-TISEKEL    TO W-TPO-SEKEL                                 
065500     ELSE                                                                 
065600       MOVE 'FEL FRÅN PROGRAM W44037 I SECTION C' TO FELTEXT              
065700       CALL ABEND USING RKOD-ABEND                                        
065800     END-IF                                                               
065900                                                                          
066000     MOVE JA                    TO ORDP-RAD-FLTPOBEK                      
066100     MOVE 1                     TO ORDP-RAD-KDSTARAD                      
066200                                                                          
066300     PERFORM IMS-REPL-ORDP2-WDA5                                          
066400                                                                          
066500     PERFORM S01-UPPD-SALDO-WDK9                                          
066600     .                                                                    
066700     EJECT                                                                
066800 D-TA-BORT-FRAN-LARMKO SECTION.                                           
066900                                                                          
067000     MOVE ORDP-RAD-IDANSK TO W-IDANSK-2232                                
067100     PERFORM IMS-GU-XXBX-WDR220                                           
067200     IF SEGMENT-FINNS                                                     
067300       MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK-2223                        
067400     ELSE                                                                 
067500       MOVE ZERO TO W-IDANSK-2223                                         
067600     END-IF                                                               
067700     MOVE ORDP-RAD-DASENDAT (3:6) TO W-TISENBEK-DAG                       
067800     MOVE ORDP-RAD-TISENBEK-KL  TO W-TISENBEK-KL                          
067900     IF ORDP-RAD-KDTPOTYP = 2                                             
068000       MOVE 110 TO W-KDLARM                                               
068100     ELSE                                                                 
068200       MOVE 100 TO W-KDLARM                                               
068300     END-IF                                                               
068400     PERFORM IMS-GHU-XXBU-WDR550                                          
068500     IF SEGMENT-SAKNAS                                                    
068600*       IF W-IDANSK-2223 NOT = 680                                        
068700        MOVE ZERO TO W-IDANSK-2223                                        
068800        PERFORM IMS-GHU-XXBU-WDR550                                       
068900*       END-IF                                                            
069000     END-IF                                                               
069100     IF SEGMENT-FINNS                                                     
069200        PERFORM IMS-DLET-XXBU-WDR550                                      
069300     END-IF                                                               
069400                                                                          
069500     .                                                                    
069600     EJECT                                                                
069700 S01-UPPD-SALDO-WDK9 SECTION.                                             
069800                                                                          
069900     IF W-SUTPO-TOT > ZERO                                                
070000       PERFORM IMS-GHU-ARTM-WDK901                                        
070100       IF SEGMENT-SAKNAS                                                  
070200         MOVE ORDP-RAD-IDARTNR TO ARTM-IDARTNR-IN                         
070300         CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                  
070400         PERFORM IMS-GHU-ARTM-WDK901                                      
070500       END-IF                                                             
070600       ADD W-SUTPO-TOT TO ART-SUTPO-TOT                                   
070700       PERFORM IMS-REPL-ARTM-WDK901                                       
070800       MOVE W-TPO-TIAAAAVV TO W-DABEHOV                                   
070900       PERFORM IMS-GHNP-ARTM-WDK911                                       
071000       IF SEGMENT-FINNS                                                   
071100         ADD W-SUTPO-PB TO ANT-SUTPO-PB                                   
071200         PERFORM IMS-REPL-ARTM-WDK911                                     
071300       ELSE                                                               
071400         MOVE W-TPO-TIAAAAVV TO ANT-DABEHOV                               
071500         MOVE W-SUTPO-PB   TO ANT-SUTPO-PB                                
071600         MOVE ZERO         TO ANT-SUTPO-EJPB                              
071700         PERFORM IMS-ISRT-ARTM-WDK911                                     
071800       END-IF                                                             
071900     END-IF                                                               
072000                                                                          
072100     IF W-KVOKS-BULK > ZERO OR                                            
072200        W-KVOKS-DAG  > ZERO OR                                            
072300        W-KVOKS-VOR  > ZERO                                               
072400                                                                          
072500       PERFORM IMS-GHU-ARTM-WDK901                                        
072600       IF SEGMENT-SAKNAS                                                  
072700         MOVE ORDP-RAD-IDARTNR TO ARTM-IDARTNR-IN                         
072800         CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                  
072900         PERFORM IMS-GHU-ARTM-WDK901                                      
073000       END-IF                                                             
073100       ADD W-KVOKS-BULK TO ART-KVOKS-BULK                                 
073200       ADD W-KVOKS-DAG  TO ART-KVOKS-DAG                                  
073300       ADD W-KVOKS-VOR  TO ART-KVOKS-VOR                                  
073400       PERFORM IMS-REPL-ARTM-WDK901                                       
073500     END-IF                                                               
073600                                                                          
073700     MOVE ZERO TO W-KVOKS-BULK W-KVOKS-DAG W-KVOKS-VOR                    
073800                  W-SUTPO-TOT W-SUTPO-PB                                  
073900     .                                                                    
074000     EJECT                                                                
074100 S02-PRISTILLEMPA  SECTION.                                               
074200                                                                          
074300     MOVE 1                    TO PRIS-KDCALL                             
074310     MOVE IDPGM                TO PRIS-IDPGM                              
074400     MOVE ORDP-RAD-IDARTNR     TO PRIS-IDARTNR                            
074500     MOVE ORDP-RAD-IDDISTR     TO PRIS-IDDISTR                            
074600                                  TEST-IDDISTR                            
074700     MOVE ORDP-RAD-IDKUNDNR    TO PRIS-IDKUNDNR                           
074800     MOVE ORDP-RAD-IDDC        TO PRIS-IDDC                               
074900     MOVE ORDP-RAD-KDORDKL     TO PRIS-KDORDKL                            
075000     MOVE ORDP-RAD-KVART       TO PRIS-KVBEART                            
075100     MOVE ORDP-RAD-FLINVEST    TO PRIS-FLINVEST                           
075200                                                                          
075300     CALL W335PRIS USING PRIS-W335PRIS                                    
075400                         PRIS-ARTC-PCB                                    
075410                         PRIS-WDK7-PCB                                    
075500                         PRIS-GMTA-PCB                                    
075600                         PRIS-BETA-PCB                                    
075700                         PRIS-GPRIA-PCB PRIS-GPRIB-PCB                    
075800                         PRIS-COST-WDK6-PCB                               
075900                         PRIS-COST-WDK7-PCB                               
076000                         PRIS-COST-WDF1-PCB                               
076100                         PRIS-COST-9305-PCB                               
076200                         PRIS-COST-WDK72-PCB                              
076300                         PRIS-COST-WDB6-PCB                               
076500                                                                          
076600     IF PRIS-KDSVAR = ' '                                                 
076710       MOVE PRIS-PRARTNTO TO ORDP-RAD-PRARTNTO                            
076800       MOVE PRIS-FLPRTILL TO ORDP-RAD-FLPRTILL                            
076900       MOVE PRIS-KDPRTYP  TO ORDP-RAD-KDPRTYP                             
077500       MOVE PRIS-KDVALISO TO ORDP-RAD-KDVALISO                            
077700     ELSE                                                                 
077800       MOVE 'FEL FRÅN PROGRAM W44037 I SECTION S02' TO FELTEXT            
077900       CALL ABEND USING RKOD-ABEND                                        
078000     END-IF                                                               
078100                                                                          
078200     .                                                                    
078300     EJECT                                                                
078400 S03-SKAPA-RZA-TRANS SECTION.                                             
078500                                                                          
078600     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
078700     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
078800     MOVE +1    TO ZZAC-IDLOGLOP                                          
078900     PERFORM IMS-GU-ARTC-WDK601                                           
079000     PERFORM IMS-GNP-ARTC-WDK611                                          
079100     IF SEGMENT-FINNS                                                     
079200       MOVE CLAG-TIDISPIN     TO ORQM-OBKR-TIDISPIN                       
079300       MOVE CLAG-PRARTBTO-EXP TO RZA-PRARTBTO-EXP                         
079400     ELSE                                                                 
079500       MOVE +0                TO ORQM-OBKR-TIDISPIN                       
079600                                 RZA-PRARTBTO-EXP                         
079700     END-IF                                                               
079800     PERFORM IMS-GU-ORQI-WDQ2                                             
079900                                                                          
080000     IF ORDP-RAD-KDTPOTYP = +2                                            
080100       MOVE 'RZA'               TO RZA-IDPTYP                             
080200       MOVE ORDP-RAD-BERADREF   TO RZA-BERADREF                           
080300       MOVE ORQI-OHUV-BEVARREF  TO RZA-BEVARREF                           
080400       MOVE ORDP-RAD-BEVOLREF   TO RZA-BEVOLREF                           
080500       MOVE ORDP-RAD-IDARTNR    TO RZA-IDARTNR                            
080600       MOVE ORDP-RAD-IDKUNDRF   TO RZA-IDKUNDRF                           
080700       MOVE ORDP-RAD-IDSYSTEM   TO RZA-IDSYSTEM                           
080800       MOVE NEJ                 TO RZA-FLABON                             
080900       MOVE ORDP-RAD-FLINVEST   TO RZA-FLINVEST                           
081000       MOVE ORDP-RAD-IDDC       TO RZA-IDDC                               
081100       MOVE ORDP-RAD-KDDSP      TO RZA-KDDSP                              
081200       MOVE ORDP-RAD-KDFAKTYP   TO RZA-KDFAKTYP                           
081300       IF ORDP-RAD-KDPRTYP = 'P'                                          
081400           MOVE 'M'             TO RZA-KDMANPR                            
081500       ELSE                                                               
081600         MOVE ORDP-RAD-FLPRTILL TO RZA-KDMANPR                            
081700       END-IF                                                             
081800       MOVE ORDP-RAD-KDORDKL    TO RZA-KDORDKL                            
081900       MOVE ORDP-RAD-KDTPOTYP   TO RZA-KDTPOTYP                           
082000       MOVE ORDP-RAD-KDVRINFO   TO RZA-KDVRINFO                           
082100       MOVE ORDP-RAD-KVART      TO RZA-KVBEART                            
082200       MOVE ORDP-RAD-PRARTNTO   TO RZA-PRARTNTO                           
082310       IF DIST79-DEALER-PRICE                                             
082400         MOVE ORDP-RAD-PRARTNTO-LOC TO RZA-PRARTNTO                       
082500       END-IF                                                             
082600       MOVE ORDP-RAD-REKSIFFR   TO RZA-REKSIFFR                           
082700       MOVE W-TIAAMMDD          TO RZA-TIORDREG                           
082800       MOVE ART-KDPRODSL        TO RZA-KDPRODSL                           
082900       MOVE ART-IDFKNGRP        TO RZA-IDFKNGRP                           
083000       IF ORDP-RAD-KDTPOTYP = 1                                           
083100         IF ORDP-RAD-IDSYSTEM = 'VR'                                      
083200           MOVE 1 TO RZA-KDVRTPO                                          
083300         ELSE                                                             
083400           MOVE 2 TO RZA-KDVRTPO                                          
083500         END-IF                                                           
083600       ELSE                                                               
083700         MOVE 0 TO RZA-KDVRTPO                                            
083800       END-IF                                                             
083900                                                                          
084000                                                                          
084100       MOVE RZA-WDGZRZA        TO ZZAC-LOGGPOST                           
084200       MOVE SPACE              TO SORT-W092P001-CTX                       
084300       MOVE ORDP-RAD-IDDISTR   TO SORT-IDDISTR-S                          
084400       MOVE ORDP-RAD-IDKUNDNR  TO SORT-IDKUNDNR-S                         
084500       MOVE SORT-W092P001-CTX  TO ZZAC-SORTPOST                           
084600       PERFORM IMS-ISRT-ZZAC-WDG6                                         
084700       IF SEGMENT-FINNS-REDAN                                             
084800         PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                             
084900           ADD +1 TO ZZAC-IDLOGLOP                                        
085000           PERFORM IMS-ISRT-ZZAC-WDG6                                     
085100         END-PERFORM                                                      
085200       END-IF                                                             
085300     END-IF                                                               
085400                                                                          
085500     .                                                                    
085600     EJECT                                                                
085700 S04-SKAPA-RY4-TRANS SECTION.                                             
085800                                                                          
085900     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
086000     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
086100     ADD +100             TO ZZAC-TIKLOCK                                 
086200     MOVE +1    TO ZZAC-IDLOGLOP                                          
086300                                                                          
086400     MOVE 'RY4'                   TO RY4-IDPTYP                           
086500     MOVE ORDP-RAD-IDDC           TO RY4-IDDC                             
086600     MOVE ORDP-RAD-IDDISTR        TO RY4-IDDISTR                          
086700     MOVE ORDP-RAD-IDKUNDNR       TO RY4-IDKUNDNR                         
086800     MOVE ORDP-RAD-IDKUNDRF (1:5) TO RY4-IDRONR                           
086900     MOVE ORDP-RAD-IDARTNR        TO RY4-IDARTNR                          
087000     MOVE ORDP-RAD-KVART          TO RY4-KVRO                             
087100     MOVE ORDP-RAD-KDORDKL        TO RY4-KDORDKL                          
087200     MOVE ORDP-RAD-KDFAKTYP       TO RY4-KDFAKTYP                         
087300     MOVE ORDP-RAD-KDVRINFO       TO RY4-KDVRINFO                         
087400                                                                          
087500     MOVE RY4-W440300   TO ZZAC-LOGGPOST                                  
087600     MOVE SPACE         TO ZZAC-SORTPOST                                  
087700     PERFORM IMS-ISRT-ZZAC-WDG6                                           
087800     IF SEGMENT-FINNS-REDAN                                               
087900       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
088000         ADD +1 TO ZZAC-IDLOGLOP                                          
088100         PERFORM IMS-ISRT-ZZAC-WDG6                                       
088200       END-PERFORM                                                        
088300     END-IF                                                               
088400                                                                          
088500     .                                                                    
088600     EJECT                                                                
088700 S05-SKAPA-RY9-TRANS SECTION.                                             
088800                                                                          
088900     PERFORM IMS-GU-GMTA-WDB201                                           
089000                                                                          
089100     MOVE +1    TO ZZAC-IDLOGLOP                                          
089200     MOVE 'RY9' TO RY9-IDPTYP                                             
089300                   ZZAC-IDPTYP                                            
089400     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
089500     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
089600     MOVE 'RY9'               TO RY9-IDPTYP                               
089700     MOVE ORDP-RAD-BERADREF   TO RY9-BERADREF                             
089800     MOVE ORDP-RAD-BEVOLREF   TO RY9-BEVOLREF                             
089900     MOVE ORDP-RAD-FLERS      TO RY9-FLERS                                
090000     MOVE GMTA-GMT-FLNC       TO RY9-FLNC                                 
090100     MOVE ORDP-RAD-IDARTNR    TO RY9-IDARTNR                              
090200     MOVE ZERO                TO RY9-IDDIVORD                             
090300     MOVE ORDP-RAD-IDKUNDRF   TO RY9-IDKUNDRF                             
090400     MOVE ORDP-RAD-IDLOPNR    TO RY9-IDLOPNR                              
090500     MOVE SPACE               TO RY9-IDUSER                               
090600     MOVE ORDP-RAD-KDFAKTYP   TO RY9-KDFAKTYP                             
090700     MOVE ORDP-RAD-KDKVBRYT   TO RY9-KDKVBRYT                             
090800     MOVE ORDP-RAD-KDORDKL    TO RY9-KDORDKL                              
090900     MOVE ORDP-RAD-KVART      TO RY9-KVART                                
091000     MOVE ORDP-RAD-KDDSP      TO RY9-KDDSP                                
091100     MOVE ORDP-RAD-KDRAPRIO   TO RY9-KDRAPRIO                             
091200     MOVE ORDP-RAD-KDSTARAD   TO RY9-KDSTARAD                             
091300     MOVE ORDP-RAD-KDTPOTYP   TO RY9-KDTPOTYP                             
091400     MOVE SPACE               TO RY9-KDUART                               
091500     MOVE ORDP-RAD-KDVRINFO   TO RY9-KDVRINFO                             
091600     MOVE ZERO                TO RY9-KDVRTPO                              
091700     MOVE ORDP-RAD-PRARTNTO   TO RY9-PRARTNTO                             
091810     IF DIST79-DEALER-PRICE                                               
091900       MOVE ORDP-RAD-PRARTNTO-LOC TO RY9-PRARTNTO                         
092000     END-IF                                                               
092100     MOVE ORDP-RAD-TIREGDAT   TO RY9-TIREGDAT                             
092200     MOVE ORDP-RAD-TIRES      TO RY9-TIRES                                
092300     MOVE ORDP-RAD-TITPO      TO RY9-TITPO                                
092400     MOVE ZERO                TO RY9-TIRODAT                              
092500     MOVE GMTA-GMT-FLVR       TO RY9-FLVR                                 
092600                                                                          
092700     MOVE RY9-WDGZRY9   TO ZZAC-LOGGPOST                                  
092800                                                                          
092900     MOVE SPACE                 TO RY9S-WDGZRY9S                          
093000     MOVE ORDP-RAD-IDDISTR      TO RY9S-IDDISTR                           
093100     MOVE ORDP-RAD-IDKUNDNR     TO RY9S-IDKUNDNR                          
093200     MOVE ORQI-OHUV-IDORDER     TO RY9S-IDORDER                           
093300     MOVE ORDP-RAD-IDDC         TO RY9S-IDDC                              
093400     MOVE ORDP-RAD-KDFRAKT      TO RY9S-KDFRAKT                           
093500     MOVE ORDP-RAD-KDORDKL      TO RY9S-KDORDKL                           
093600     MOVE 71                    TO RY9S-KDORDBEK                          
093700                                                                          
093800     MOVE RY9S-WDGZRY9S         TO ZZAC-SORTPOST                          
093900     PERFORM IMS-ISRT-ZZAC-WDG6                                           
094000     IF SEGMENT-FINNS-REDAN                                               
094100       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
094200         ADD +1 TO ZZAC-IDLOGLOP                                          
094300         PERFORM IMS-ISRT-ZZAC-WDG6                                       
094400       END-PERFORM                                                        
094500     END-IF                                                               
094600                                                                          
094700     .                                                                    
094800     EJECT                                                                
094900 S06-SKAPA-W2I10902 SECTION.                                              
095000                                                                          
095010*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
095020     MOVE ORDP-RAD-IDARTNR   TO BYT03-IDARTNR                             
095030     IF NOT BYT03-OBJEKT                                                  
095040                                                                          
095100        IF ORDP-RAD-KDOI NOT = SPACE                                      
095200          MOVE 2109-IX          TO 2109-MID2-KVANTART                     
095300          MOVE ORDP-RAD-IDARTNR TO 2109-MID2-IDARTNR (2109-IX)            
095400          MOVE ORDP-RAD-IDDC    TO 2109-MID2-IDDC (2109-IX)               
095500          MOVE '+'              TO 2109-MID2-KDTECKEN (2109-IX)           
095600          MOVE 'DT'             TO 2109-MID2-KDOI (2109-IX)               
095700          MOVE SPACE            TO 2109-MID2-CLEARGROUP(2109-IX)          
095800          MOVE ORDP-RAD-KVART   TO 2109-MID2-KVOI (2109-IX)               
095900          MOVE W-TIAAMMDD       TO 2109-MID2-TIUPPDAT (2109-IX)           
096000                                                                          
096100          ADD +1                TO 2109-IX                                
096200          IF 2109-IX > 2109-IX-MAX                                        
096300            PERFORM S06A-STARTA-2109                                      
096400          END-IF                                                          
096500        END-IF                                                            
096510     END-IF                                                               
096600     .                                                                    
096700     EJECT                                                                
096800 S06A-STARTA-2109 SECTION.                                                
096900                                                                          
097000     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
097100                                                                          
097200     PERFORM IMS-PURG-ALT-MSG-2109                                        
097300                                                                          
097400     MOVE SPACE              TO 2109-MID2-W2I10902                        
097500     MOVE +1                 TO 2109-IX                                   
097600     .                                                                    
097700     EJECT                                                                
097800 S20-KOMPLETTERA-PRIS  SECTION.                                           
097900                                                                          
098000     IF DIST79-DEALER-PRICE                                               
098100      IF ORDP-RAD-PRARTNTO-LOC = ZERO AND                                 
098200         ORDP-RAD-PRARTNTO-LOCPREL = ZERO                                 
098300       IF ORDP-RAD-IDPRQUES > ZERO                                        
098400*         TA BORT GAMLA PRISFRÅGAN                                        
098500         INITIALIZE PRQU-W335PRQU                                         
098600         MOVE ORDP-RAD-IDDISTR       TO PRQU-IDDISTR                      
098700         MOVE ORDP-RAD-IDKUNDNR      TO PRQU-IDKUNDNR                     
098800         MOVE ORDP-RAD-IDKUNDRF(1:5) TO PRQU-IDKUNDRF(3:5)                
098900         MOVE '00'                   TO PRQU-IDKUNDRF(1:2)                
099000         MOVE ORDP-RAD-IDPRQUES      TO PRQU-IDPRQUES                     
099100         MOVE 4                      TO PRQU-KDCALL                       
099200         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
099300                                            PRQU-WDC7-PCB                 
099400                                            PRQU-SJKO-WDK6-PCB            
099500                                                                          
099600       END-IF                                                             
099700       MOVE ZERO                     TO ORDP-RAD-IDPRQUES                 
099800                                        WS-IDPRQUES                       
099900       IF WS-IDPRQUES                = +0                                 
100000          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
100100          MOVE +1                    TO PRNO-KDCALL                       
100200                                                                          
100300          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
100400                                                                          
100500          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
100600                                        WS-IDPRQUES                       
100700          MOVE +1                    TO PRQU-KDCALL                       
100800*      ELSE                                                               
100900*         MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
101000*         MOVE +2                    TO PRNO-KDCALL                       
101100*                                                                         
101200*         CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
101300*                                                                         
101400*         MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
101500*                                     WS-IDPRQUES                         
101600*         MOVE +2                    TO PRQU-KDCALL                       
101700       END-IF                                                             
101800*         LÄGG IN NY PRISFRÅGA                                            
101900       MOVE ORDP-RAD-IDDISTR         TO PRQU-IDDISTR                      
102000                                        W-IDDISTR                         
102100       MOVE ORDP-RAD-IDKUNDNR        TO PRQU-IDKUNDNR                     
102200                                        W-IDKUNDNR                        
102300       MOVE ORDP-RAD-IDKUNDRF(1:5)   TO PRQU-IDKUNDRF(3:5)                
102400       MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                
102500       MOVE ZERO                     TO PRQU-IDORDER                      
102600       MOVE ORDP-RAD-KDORDKL         TO PRQU-KDORDKL                      
102700       IF PRQU-IDDISTR = 0778 AND PRQU-KDORDKL < 3                        
102800         AND ORDP-RAD-DARODAT > 0                                         
102900         MOVE 4                      TO PRQU-KDORDKL                      
103000       END-IF                                                             
103100       MOVE 'N'                      TO PRQU-KDPRSTA                      
103200       MOVE ORDP-RAD-IDARTNR         TO PRQU-IDARTNR                      
103300       MOVE ORDP-RAD-KVBEART-Q       TO PRQU-KVBEART-Q                    
103400       MOVE ORDP-RAD-KDVALISO        TO PRQU-KDVALISO                     
103500       MOVE ORDP-RAD-PRARTNTO-LOC    TO PRQU-PRARTNTO-LOC                 
103600       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
103700       MOVE ORDP-RAD-IDSYSTEM        TO PRQU-IDSYSTEM                     
103800       PERFORM IMS-GU-GMTA-WDB201                                         
103900                                                                          
104000       CALL  W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                  
104100                                           PRQU-WDC7-PCB                  
104200                                           PRQU-SJKO-WDK6-PCB             
104300                                                                          
104400       MOVE PRQU-IDPRQUES            TO  ORDP-RAD-IDPRQUES                
104500                                         WS-IDPRQUES                      
104600       MOVE PRQU-FLPRTILL            TO  ORDP-RAD-FLPRTILL                
104700       IF PRQU-KDORDKL = 4 AND ORDP-RAD-KDORDKL NOT = 4                   
104800         MOVE 'N'                    TO ORDP-RAD-FLPRTILL                 
104900       END-IF                                                             
105000                                                                          
105100       IF ORDP-RAD-PRARTNTO-LOC = +0                                      
105200          MOVE PRQU-PRARTNTO-LOCPREL TO                                   
105300                     ORDP-RAD-PRARTNTO-LOCPREL                            
105400       END-IF                                                             
105500                                                                          
105600       IF ORDP-RAD-PRARTNTO-LOC NOT = +0                                  
105700         IF ORDP-RAD-KDPRTYP = SPACE                                      
105800           MOVE 'P'                TO ORDP-RAD-KDPRTYP                    
105900*          MOVE ORDP-RAD-TIREGDAT  TO ORDP-RAD-TIPRIS                     
106000         END-IF                                                           
106100       END-IF                                                             
106200       PERFORM S21-SKICKA-PRISFRAGA                                       
106300       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
106400       MOVE +3                      TO PRNO-KDCALL                        
106500                                                                          
106600       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
106700      END-IF                                                              
106800     END-IF                                                               
106900     .                                                                    
107000     EJECT                                                                
107100 S21-SKICKA-PRISFRAGA  SECTION.                                           
107200                                                                          
107300     MOVE 1                          TO 3039-REQU-IDMSGVER                
107400     MOVE SPACE                      TO 3039-REQU-KDPGMACT                
107500     MOVE 'W4403700'                 TO 3039-REQU-IDUSER                  
107600                                                                          
107700     MOVE SPACE                      TO 3039-MID-IDBUNDLE                 
107800     MOVE ORDP-RAD-IDDISTR           TO 3039-MID-IDDISTR                  
107900     MOVE ORDP-RAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                 
108000     MOVE ORDP-RAD-IDORDNR7          TO 3039-MID-IDBUNDLE                 
108100     MOVE WS-IDPRQUES                TO 3039-MID-IDPRQUES                 
108200                                                                          
108300     PERFORM S24-SKICKA-OPEN                                              
108400     PERFORM S24-SKICKA-MEDDELANDE                                        
108500     PERFORM S24-SKICKA-CLOSE                                             
108600                                                                          
108700     .                                                                    
108800     EJECT                                                                
108900 S24-SKICKA-OPEN SECTION.                                                 
109000                                                                          
109100     MOVE 'OPEN'                     TO SEND-KDFUNC                       
109200     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
109300     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
109400                                                                          
109500     IF SEND-KDRC > 0                                                     
109600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
109700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
109800       DELIMITED BY SIZE INTO FELTEXT                                     
109900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
110000     END-IF                                                               
110100     .                                                                    
110200     SKIP3                                                                
110300 S24-SKICKA-MEDDELANDE  SECTION.                                          
110400                                                                          
110500     MOVE 'PUT'                      TO SEND-KDFUNC                       
110600     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
110700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
110800                                                                          
110900     IF SEND-KDRC > 0                                                     
111000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
111100       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
111200       DELIMITED BY SIZE INTO FELTEXT                                     
111300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
111400     END-IF                                                               
111500     .                                                                    
111600     SKIP3                                                                
111700 S24-SKICKA-CLOSE SECTION.                                                
111800                                                                          
111900     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
112000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
112100                                                                          
112200     IF SEND-KDRC > 0                                                     
112300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
112400       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
112500       DELIMITED BY SIZE INTO FELTEXT                                     
112600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
112700     END-IF                                                               
112800     .                                                                    
112900     EJECT                                                                
113000                                                                          
113100*    ---- IMS SEKTIONER ----                                              
113200     SKIP2                                                                
113300 IMS-PURG-ALT-MSG-2109 SECTION.                                           
113400     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
113500     MOVE SPACE TO GODK-STATUSKODER                                       
113600     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
113700     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
113800     PERFORM IMS-STATUSKONTROLL                                           
113900     .                                                                    
114000     SKIP2                                                                
114100 IMS-GU-GMTA-WDB201 SECTION.                                              
114200                                                                          
114300     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
114400          DELIMITED BY SIZE INTO SSA1                                     
114500     MOVE '  GE' TO GODK-STATUSKODER                                      
114600     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-GMTA01 SSA1                    
114700     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
114800     PERFORM IMS-STATUSKONTROLL                                           
114900     .                                                                    
115000     EJECT                                                                
115100 IMS-GU-ARTC-WDK601 SECTION.                                              
115200                                                                          
115300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
115400          DELIMITED BY SIZE INTO SSA1                                     
115500     MOVE '  ' TO GODK-STATUSKODER                                        
115600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
115700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
115800     PERFORM IMS-STATUSKONTROLL                                           
115900     .                                                                    
116000     SKIP3                                                                
116100 IMS-GNP-ARTC-WDK611 SECTION.                                             
116200                                                                          
116300     MOVE 'WDK611   ' TO SSA1                                             
116400     MOVE '  GE' TO GODK-STATUSKODER                                      
116500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
116600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
116700     PERFORM IMS-STATUSKONTROLL                                           
116800     .                                                                    
116900     EJECT                                                                
117000 IMS-GU-XXBX-WDR220 SECTION.                                              
117100                                                                          
117200     STRING 'WLXXBX01(WDGXKEY  =' W-WDGX2231-X ')'                        
117300          DELIMITED BY SIZE INTO SSA1                                     
117400     STRING 'WLXXBX11(WDGXKEY  =' W-WDGX2232-X ')'                        
117500          DELIMITED BY SIZE INTO SSA2                                     
117600     MOVE '  GE' TO GODK-STATUSKODER                                      
117700     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-XXBX11 SSA1 SSA2               
117800     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
117900     PERFORM IMS-STATUSKONTROLL                                           
118000     .                                                                    
118100     EJECT                                                                
118200 IMS-GHU-XXBU-WDR550 SECTION.                                             
118300                                                                          
118400     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
118500            DELIMITED BY SIZE INTO SSA1                                   
118600     STRING 'WLXXBU11(WDGXKEY  =' W-WDGX2224-X ')'                        
118700            DELIMITED BY SIZE INTO SSA2                                   
118800     MOVE '  GE'               TO GODK-STATUSKODER                        
118900     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-XXBU11 SSA1 SSA2              
119000     MOVE XXBU-STATUS-CODE     TO STATUS-WS                               
119100     PERFORM IMS-STATUSKONTROLL                                           
119200     .                                                                    
119300     SKIP2                                                                
119400 IMS-DLET-XXBU-WDR550 SECTION.                                            
119500                                                                          
119600     MOVE '  ' TO GODK-STATUSKODER                                        
119700     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-XXBU11                       
119800     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
119900     PERFORM IMS-STATUSKONTROLL                                           
120000     .                                                                    
120100     EJECT                                                                
120200 IMS-GHU-ARTM-WDK901 SECTION.                                             
120300                                                                          
120400     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
120500          DELIMITED BY SIZE INTO SSA1                                     
120600     MOVE '  GE' TO GODK-STATUSKODER                                      
120700     CALL CBLTDLI USING GHU WDK9-PCB DLI-IO-WDK901 SSA1                   
120800     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
120900     PERFORM IMS-STATUSKONTROLL                                           
121000     .                                                                    
121100     SKIP3                                                                
121200 IMS-GHNP-ARTM-WDK911 SECTION.                                            
121300                                                                          
121400     STRING 'WDK911  (DABEHOV  =' W-DABEHOV-X ')'                         
121500          DELIMITED BY SIZE INTO SSA1                                     
121600     MOVE '  GE' TO GODK-STATUSKODER                                      
121700     CALL CBLTDLI USING GHNP WDK9-PCB DLI-IO-WDK911 SSA1                  
121800     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
121900     PERFORM IMS-STATUSKONTROLL                                           
122000     .                                                                    
122100     EJECT                                                                
122200 IMS-GHU-ORDP2-WDA5 SECTION.                                              
122300                                                                          
122400     STRING 'WDA501  (WDA501KY =' W-WDA501KY-X                            
122500                    '&IDDC     =' W-W1-IDDC-X ')'                         
122600            DELIMITED BY SIZE INTO SSA1                                   
122700     MOVE '  GE'               TO GODK-STATUSKODER                        
122800     CALL CBLTDLI USING GHU WDA52-PCB DLI-IO-WDA501 SSA1                  
122900     MOVE WDA52-STATUS-CODE    TO STATUS-WS                               
123000     PERFORM IMS-STATUSKONTROLL                                           
123100     .                                                                    
123200     EJECT                                                                
123300 IMS-GU-ORDP1-WDA5 SECTION.                                               
123400                                                                          
123500     STRING 'WDA501  (WDA5ESEQ=>' W1-DASENBEK-X                           
123600                    '&WDA5ESEQ=<' W2-DASENBEK-X ')'                       
123700            DELIMITED BY SIZE INTO SSA1                                   
123800     MOVE '  GE'                TO GODK-STATUSKODER                       
123900     CALL CBLTDLI USING GU WDA51-PCB DLI-IO-WDA501 SSA1                   
124000     MOVE WDA51-STATUS-CODE     TO STATUS-WS                              
124100     PERFORM IMS-STATUSKONTROLL                                           
124200     .                                                                    
124300     SKIP3                                                                
124400 IMS-GN-ORDP1-WDA5 SECTION.                                               
124500                                                                          
124600     STRING 'WDA501  (WDA5ESEQ=>' W1-DASENBEK-X                           
124700                    '&WDA5ESEQ=<' W2-DASENBEK-X ')'                       
124800            DELIMITED BY SIZE INTO SSA1                                   
124900     MOVE '  GE'                TO GODK-STATUSKODER                       
125000     CALL CBLTDLI USING GN WDA51-PCB DLI-IO-WDA501 SSA1                   
125100     MOVE WDA51-STATUS-CODE     TO STATUS-WS                              
125200     PERFORM IMS-STATUSKONTROLL                                           
125300     .                                                                    
125400     SKIP3                                                                
125500 IMS-REPL-ORDP2-WDA5 SECTION.                                             
125600                                                                          
125700     MOVE '  '               TO GODK-STATUSKODER                          
125800     CALL CBLTDLI USING REPL  WDA52-PCB DLI-IO-WDA501                     
125900     MOVE WDA52-STATUS-CODE TO STATUS-WS                                  
126000     PERFORM IMS-STATUSKONTROLL                                           
126100     .                                                                    
126200     EJECT                                                                
126300 IMS-REPL-ARTM-WDK901 SECTION.                                            
126400                                                                          
126500     MOVE '  ' TO GODK-STATUSKODER                                        
126600     CALL CBLTDLI USING REPL WDK9-PCB DLI-IO-WDK901                       
126700     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
126800     PERFORM IMS-STATUSKONTROLL                                           
126900     .                                                                    
127000     EJECT                                                                
127100 IMS-REPL-ARTM-WDK911 SECTION.                                            
127200                                                                          
127300     MOVE '  ' TO GODK-STATUSKODER                                        
127400     CALL CBLTDLI USING REPL WDK9-PCB DLI-IO-WDK911                       
127500     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     .                                                                    
127800     EJECT                                                                
127900 IMS-ISRT-ARTM-WDK911 SECTION.                                            
128000                                                                          
128100     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
128200          DELIMITED BY SIZE INTO SSA1                                     
128300     MOVE 'WDK911   ' TO SSA2                                             
128400     MOVE '  ' TO GODK-STATUSKODER                                        
128500     CALL CBLTDLI USING ISRT WDK9-PCB DLI-IO-WDK911 SSA1 SSA2             
128600     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
128700     PERFORM IMS-STATUSKONTROLL                                           
128800     .                                                                    
128900     EJECT                                                                
129000 IMS-ISRT-ZZAC-WDG6 SECTION.                                              
129100                                                                          
129200     STRING 'WLZZAC01 '                                                   
129300          DELIMITED BY SIZE INTO SSA1                                     
129400     MOVE '  II' TO GODK-STATUSKODER                                      
129500     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-ZZAC01 SSA1                  
129600     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
129700     PERFORM IMS-STATUSKONTROLL                                           
129800     .                                                                    
129900     EJECT                                                                
130000 IMS-GU-ORQI-WDQ2 SECTION.                                                
130100                                                                          
130200     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
130300             DELIMITED BY SIZE INTO SSA1                                  
130400     MOVE    '  '              TO GODK-STATUSKODER                        
130500     CALL    CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                 
130600     MOVE    WDQ2-STATUS-CODE TO STATUS-WS                                
130700     PERFORM IMS-STATUSKONTROLL                                           
130800     .                                                                    
130900     EJECT                                                                
131000 IMS-ISRT-ORQM-WDQ1   SECTION.                                            
131100                                                                          
131200     MOVE 'WLORQM01 ' TO SSA1                                             
131300     MOVE '  II' TO GODK-STATUSKODER                                      
131400     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-ORQM01 SSA1                  
131500     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
131600     PERFORM IMS-STATUSKONTROLL                                           
131700     .                                                                    
131800     EJECT                                                                
131900*    ---- IMS CHECKPOINTHANTERING ----                                    
132000                                                                          
132100 IMS-RESTART  SECTION.                                                    
132200                                                                          
132300     MOVE SPACE TO MSG-IO-AREA                                            
132400     MOVE '  '  TO GODK-STATUSKODER                                       
132500     CALL CBLTDLI USING XRST MSG-PCB                                      
132600                          MSG-IO-AREA-LENGTH MSG-IO-AREA                  
132700                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
132800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
132900     PERFORM IMS-STATUSKONTROLL                                           
133000                                                                          
133100     IF IMS-EJ-OK                                                         
133200       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
133300       CALL FELLOG                                                        
133400     END-IF                                                               
133500     .                                                                    
133600     SKIP2                                                                
133700 IMS-CHECKPOINT SECTION.                                                  
133800                                                                          
133900     MOVE IDPGM    TO MSG-IO-AREA                                         
134000     MOVE '  XD'       TO GODK-STATUSKODER                                
134100     CALL CBLTDLI USING CHKP MSG-PCB                                      
134200                          MSG-IO-AREA-LENGTH MSG-IO-AREA                  
134300                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
134400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
134500     PERFORM IMS-STATUSKONTROLL                                           
134600     IF IMS-EJ-OK                                                         
134700       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
134800       CALL FELLOG                                                        
134900     END-IF                                                               
135000     .                                                                    
135100     SKIP2                                                                
135200 IMS-GU-WDB601    SECTION.                                                
135300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
135400          DELIMITED BY SIZE INTO SSA1                                     
135500     MOVE '  GE' TO GODK-STATUSKODER                                      
135600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
135700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     IF SEGMENT-SAKNAS                                                    
136000         MOVE SPACE TO DCS-KDDC                                           
136100     END-IF                                                               
136200     .                                                                    
136300 IMS-STATUSKONTROLL SECTION.                                              
136400                                                                          
136500     SET STATUS-IX TO 1                                                   
136600     SEARCH GODK-STATUS                                                   
136700       AT END                                                             
136800       STRING 'STATUSKOD FRÅM IMS ' STATUS-WS                             
136900       DELIMITED BY SIZE INTO FELTEXT                                     
137000       CALL FELLOG                                                        
137100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
137200         CONTINUE                                                         
137300     END-SEARCH                                                           
137400     .                                                                    
