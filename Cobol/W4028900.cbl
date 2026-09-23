000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4028900.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   99/12/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        THE PROGRAM UPDATES NEWLOCATION FOR AN ORDERLINE.                
001100*                                                                         
001200*        UPPDATERAR ORDERKÖ (WDQ401) MED NY LAGERPLATS VID                
001300*        PLATTSÄTTNING = FÖRÄNDRING AV EN ARTIKELS LAGERPLATS             
001400*        UPPDATERAR ALLA FÖREKOMSTER AV EN ARTIKEL PÅ WDQ401 FRÅN         
001500*        BILD 6163 ELLER 6304.                                            
001600*                                                                         
001700*        PROGRAMMET LÄSER      WDK601-11                                  
001800*        PROGRAMMET LÄSER      WDK701-11                                  
001900*        PROGRAMMET LÄSER      WLORQI                                     
002000*        PROGRAMMET LÄSER      WDQ4B1                                     
002100*        PROGRAMMET UPPDATERAR WDQ401                                     
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T289                                              
002500*        MID        : W4I28901                                            
002600*                                                                         
002700*    ABENDKODER:                                                          
002800*        U0016 -  . . . .                                                 
002900*        U1000 -  . . . .                                                 
003000*                                                                         
003100* CHANGE LOG:                                                             
003200*                                                                         
003300*HÖSTEN 2004 GÖRAN KJELLSON                                               
003400*ETRACKER 887753                                                          
003500*                                                                         
003600*  SEPT 2005 LINDA NILSSON                                                
003700*  ETRACKER 1334295                                                       
003800*                                                                         
003900*   HÖSTEN -08   E-TR: 7450328   VOHF                                     
004000*   HÖSTEN -15   E-TR: 10254592  DECOMISSION VOHF                         
004100*                                                                         
004200                                                                          
004300     SKIP3                                                                
004400 ENVIRONMENT DIVISION.                                                    
004500     SKIP2                                                                
004600 INPUT-OUTPUT SECTION.                                                    
004700                                                                          
004800 FILE-CONTROL.                                                            
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100     SKIP3                                                                
005200 FILE SECTION.                                                            
005300     SKIP2                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500*    -- CHECKED BY WY2000                                                 
005600 77  IDPGM                       PIC X(8)    VALUE 'W4028900'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  WS-UPD-WEIVOL               PIC X       VALUE 'N'.                   
006000 77  WS-UPD-LOCATION             PIC X       VALUE 'N'.                   
006100 77  FILLER                      PIC  X(8)   VALUE 'AAAAAAAA'.            
006200 77  WS-MID-IDDC-IN              PIC  X(2).                               
006300 77  WS-MID-ADLAGOMR             PIC  9(3).                               
006400 77  WS-MID-ADGANG               PIC  9(3).                               
006500 77  WS-MID-ADPLATS              PIC  9(5).                               
006600 77  WS-MID-VKART                PIC  9(7).                               
006700 77  WS-MID-VLARTNTO             PIC  9(8)V9(1).                          
006800 77  FILLER                      PIC  X(8)   VALUE 'BBBBBBBB'.            
006900 77  AVSR-INDX                   PIC S9(4)  VALUE +0    COMP-3.           
007000 77  AVSR-INDX-MAX               PIC S9(9)  VALUE +100  COMP-3.           
007100 77  ANTAL-UPPDAT-INDX           PIC S9(9)  VALUE +0    COMP-3.           
007200 77  MAX-ANTAL-UPPDAT-RADER      PIC S9(9)  VALUE +25   COMP-3.           
007300 77  WS-IDPLKLST                 PIC S9(3)  VALUE +0    COMP-3.           
007400 77  WS-IDPRODNR                 PIC S9(7)  VALUE +0    COMP-3.           
007500 77  WS-DUMMY                    PIC X(2)   VALUE SPACE.                  
007600 01  WS-IDPRC.                                                            
007700     03 WS-IDPRCBAS                PIC  X(3).                             
007800     03 WS-IDPRCVAR                PIC  X(1).                             
007900     SKIP2                                                                
008000 77  FILLER                      PIC  X(8)   VALUE 'CCCCCCCC'.            
008100 01  DAGENS-TID                  PIC S9(9)   VALUE ZERO.                  
008200     SKIP2                                                                
008300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008400 01  FILLER REDEFINES DAGENS-DATUM.                                       
008500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008800     SKIP2                                                                
008900 01  FELTEXT.                                                             
009000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009100     03  FELTEXT-STR             PIC X(48)   VALUE SPACE.                 
009200     03  FELTEXT-STR2            PIC X(24)   VALUE SPACE.                 
009300     SKIP2                                                                
009400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009500                                                                          
009600 77  UTSKRIVEN-ORDERDEL-FINNS-SW PIC X       VALUE 'J'.                   
009700     88  UTSKRIVEN-ORDERDEL-FINNS            VALUE 'J'.                   
009800     88  INGEN-ORDERDEL-UTSKRIVEN            VALUE 'N'.                   
009900                                                                          
010000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010100     88  INDATA-OK                           VALUE 'J'.                   
010200     88  INDATA-FEL                          VALUE 'N'.                   
010300                                                                          
010400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010500     88  EGEN-MID                            VALUE '4289'.                
010600     88  GODK-MID                            VALUE '4289' '6163'          
                                                         '6169'                 
                                                         '2133' '2393'          
010700                                                   '6304' 'L104'.         
010800                                                                          
010900 01  FILLER                        PIC X(08) VALUE 'SAVEAREA'.            
011000 01  SAVE-AREA.                                                           
011100     03  SAVE-IDTRANS            PIC  X(4).                               
011200     03  SAVE-ADLAGOMR-NEXT      PIC  9(3).                               
011300     03  SAVE-ADGANG-NEXT        PIC  9(3).                               
011400     03  SAVE-ADPLATS-NEXT       PIC  9(5).                               
011410     03  SAVE-LOCATION-UPD       PIC  X(1).                               
011420     03  SAVE-WEIVOL-UPD         PIC  X(1).                               
011500                                                                          
011600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011700 01  GENERELLA-SUBPROGRAM.                                                
011800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012300     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
012400     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
012500     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
012600     03  W413AVSO                PIC X(8)    VALUE 'W413AVSO'.            
012700     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
012800     SKIP2                                                                
012900*    --- PARAMETRAR TILL ABEND                                            
013000                                                                          
013100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013400     EJECT                                                                
013500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013600*                                                                         
013700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013800     SKIP3                                                                
013900*01  -COPY WMSGAREA                                                       
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014200     SKIP3                                                                
014300*01  -COPY WMFSAREA                                                       
014400     EJECT                                                                
014500*    --- SUB-PROGRAM                                                      
014600 01  FILLER                      PIC X(08)   VALUE 'W411LAST'.            
014700*01  -COPY W411LAST                                                       
014800     EJECT                                                                
014900 01  FILLER                      PIC X(08)   VALUE 'W413ADRS'.            
015000*01  -COPY W413ADRS                                                       
015100     EJECT                                                                
015200 01  FILLER                      PIC X(08)   VALUE 'W413AVSR'.            
015300*01  -COPY W413AVSR                                                       
015400     EJECT                                                                
015500 01  FILLER                      PIC X(08)   VALUE 'W413AVSO'.            
015600*01  -COPY W413AVSO                                                       
015700     EJECT                                                                
015800*    --- AREA FÖR SUBPROGRAM W006PRS1                                     
015900*                                                                         
016000 01  FILLER                      PIC X(16)  VALUE 'W006PRS1'.             
016100                                                                          
016200*01  -COPY W006PRAR                                                       
016300     SKIP3                                                                
016400*                                                                         
016500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
016600*                                                                         
016700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016800     SKIP3                                                                
016900*01 -COPY WMSGINIT                                                        
017000     EJECT                                                                
017100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017200*                                                                         
017300     SKIP3                                                                
017400 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
017500 01  P-TO-P-SW1.                                                          
017600     03  PTOP1-KVLL              PIC S9(4)   VALUE 90 COMP SYNC.          
017700     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
017800     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
017900     03  PTOP1-KDTRANSKOD        PIC  X(7)   VALUE 'W4T289X'.             
018000     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
018100     03  PTOP1-IDTRANS           PIC  X(4)   VALUE '4289'.                
018200     03  PTOP1-KDMFSFOR          PIC  X(1).                               
018300*    03  -COPY W4I28901                                                   
018400     SKIP3                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018600     SKIP3                                                                
018700 01  NYCKLAR-TILL-DLI.                                                    
018800*                                                                         
018900     03  W-IDARTNR-X.                                                     
019000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019100*                                                                         
019200     03  W-IDORDER-X.                                                     
019300         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
019400*                                                                         
019500     03  W-KDSEGKEY-X.                                                    
019600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
019700*                                                                         
019800     03  W-IDDC-X.                                                        
019900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
020000*                                                                         
020100     03  W-WDQ401KY-X.                                                    
020200       05  W-Q401KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
020300       05  W-Q401KY-IDDC         PIC  X(2)   VALUE SPACE.                 
020400       05  W-Q401KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
020500       05  W-Q401KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
020600       05  W-Q401KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
020700       05  W-Q401KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
020800       05  W-Q401KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
020900*                                                                         
021000     03  W-WDQ4B1KY-X.                                                    
021100       05  W-Q4B1KY-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
021200       05  W-Q4B1KY-IDLOPNR      PIC S9(3)   VALUE ZERO  COMP-3.          
021300       05  W-Q4B1KY-IDGMTREF.                                             
021400         07 W-Q4B1KY-IDDISTR     PIC S9(5)   VALUE ZERO  COMP-3.          
021500         07 W-Q4B1KY-IDKUNDNR    PIC S9(7)   VALUE ZERO  COMP-3.          
021600         07 W-Q4B1KY-IDKUNDRF.                                            
021700          09 W-Q4B1KY-IDORDNR7   PIC 9(07)   VALUE ZERO.                  
021800          09 FILLER-1            PIC X(03)   VALUE SPACE.                 
021900       05  W-Q4B1KY-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
022000       05  W-Q4B1KY-IDDC         PIC  X(2)   VALUE SPACE.                 
022100       05  W-Q4B1KY-ADLAGOMR     PIC S9(3)   VALUE ZERO  COMP-3.          
022200       05  W-Q4B1KY-ADGANG       PIC S9(3)   VALUE ZERO  COMP-3.          
022300       05  W-Q4B1KY-ADPLATS      PIC S9(5)   VALUE ZERO  COMP-3.          
022400*                                                                         
022500     03  W-WDQ4B1KY-MIN-X.                                                
022600         05  W-Q4B1-IDARTNR-MIN  PIC S9(9)    COMP-3.                     
022700         05  LOW-FILLER          PIC  X(32)   VALUE LOW-VALUE.            
022800*                                                                         
022900     03  W-WDQ4B1KY-MAX-X.                                                
023000         05  W-Q4B1-IDARTNR-MAX  PIC S9(9)    COMP-3.                     
023100         05  HIGH-FILLER         PIC  X(32)   VALUE HIGH-VALUE.           
023200*                                                                         
023300   03  W-WDGXKEY-4475-X.                                                  
023400     05  FILLER              PIC X(4)    VALUE '4475'.                    
023500     05  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
023600*                                                                         
023700     03  W-WDQ301KY-MIN-X.                                                
023800         05  W-Q301KY-IDORDER        PIC S9(7)  COMP-3.                   
023900         05  W-Q301KY-IDDC           PIC X(2).                            
024000         05  FILLER                  PIC  X(6)  VALUE LOW-VALUE.          
024100*                                                                         
024200     03  W-WDQ301KY-MAX-X.                                                
024300         05  W-Q301KY-IDORDER-MAX    PIC S9(7)  COMP-3.                   
024400         05  W-Q301KY-IDDC-MAX       PIC X(2).                            
024500         05  FILLER                  PIC  X(6)  VALUE HIGH-VALUE.         
024600*                                                                         
024700     03  W-IDDC-B6-X.                                                     
024800         05 W-IDDC-B6                  PIC X(2).                          
024900                                                                          
025000     SKIP2                                                                
025100*    --- STATUS-KOD FRÅN IMS                                              
025200 01  STATUS-WS                   PIC XX.                                  
025300     88  SEGMENT-FINNS                       VALUE '  '.                  
025400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025600     88  END-OF-DATABASE                     VALUE 'GB'.                  
025700     88  IMS-NOT-OK                          VALUE 'XD'.                  
025800     SKIP2                                                                
025900 01  GODK-STATUSKODER.                                                    
026000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026100     SKIP3                                                                
026200 01  SSA1                        PIC X(128).                              
026300 01  SSA2                        PIC X(128).                              
026400 01  SSA-WDQ401                  PIC X(128).                              
026500     EJECT                                                                
026600*    --- IMS FUNKTIONSKODER                                               
026700*01  -COPY W0003                                                          
026800     EJECT                                                                
026900*    ---  DLI INPUT-OUTPUT AREA                                           
027000                                                                          
027100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
027200 01  DLI-IO-WDGX01.                                                       
027300     03  WLXXKY01 -COPY WDGX01                                            
027400*                                                                         
027500 01  DLI-IO-AREA-KY              PIC X(16) VALUE 'IO-AREA-KY'.            
027600 01  IO-AREA-KY.                                                          
027700     03  WLXXKY11 -COPY WDGX4476                                          
027800     EJECT                                                                
027900 01  FILLER                      PIC X(16) VALUE 'IO-AREA-K601'.          
028000 01  IO-AREA-K601.                                                        
028100     03  WDK601   -COPY WDK601                                            
028200     EJECT                                                                
028300 01  FILLER                      PIC X(16) VALUE 'IO-AREA-K611'.          
028400 01  IO-AREA-K611.                                                        
028500     03  WDK611   -COPY WDK611                                            
028600     EJECT                                                                
028700 01  FILLER                      PIC X(16) VALUE 'IO-AREA-K701'.          
028800 01  IO-AREA-K701.                                                        
028900     03  WDK701   -COPY WDK701                                            
029000     EJECT                                                                
029100 01  FILLER                      PIC X(16) VALUE 'IO-AREA-K711'.          
029200 01  IO-AREA-K711.                                                        
029300     03  WDK711   -COPY WDK711                                            
029400     EJECT                                                                
029500 01  FILLER                      PIC X(16) VALUE 'IO-AREA-Q201'.          
029600 01  IO-AREA-Q201.                                                        
029700     03  WLORQI01 -COPY WDQ201                                            
029800     EJECT                                                                
029900 01  FILLER                      PIC X(16) VALUE 'IO-AREA-Q211'.          
030000 01  IO-AREA-Q211.                                                        
030100     03  WLORQI11 -COPY WDQ211                                            
030200     SKIP3                                                                
030300 01  FILLER                      PIC X(16) VALUE 'IO-AREA-Q212'.          
030400 01  IO-AREA-Q212.                                                        
030500     03  WLORQI12 -COPY WDQ212                                            
030600     SKIP3                                                                
030700 01  FILLER                      PIC X(16) VALUE 'IO-AREA-Q401'.          
030800 01  IO-AREA-Q401.                                                        
030900     03  WDQ401   -COPY WDQ401                                            
031000     SKIP3                                                                
031100 01  FILLER                      PIC X(16) VALUE 'IO-AREA-Q4B1'.          
031200 01  IO-AREA-Q4B1.                                                        
031300     03  WDQ4B1   -COPY WDQ4B1                                            
031400     SKIP3                                                                
031500 01  FILLER                      PIC X(16) VALUE 'IO-AREA-Q301'.          
031600 01  IO-AREA-Q301.                                                        
031700     03  WDQ301   -COPY WDQ301                                            
031800     EJECT                                                                
031900                                                                          
032000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032100 01   DLI-IO-AREA-B601.                                                   
032200     03  -COPY WDB601                                                     
032300 01  FILLER                      PIC X(16) VALUE 'LINKAGE-SEC '.          
032400                                                                          
032500 LINKAGE SECTION.                                                         
032600*                                                                         
032700*01  -COPY W0009  -PRE MSG-                                               
032800                                                                          
032900*01  -COPY W0009  -PRE AVSR-ALT-                                          
033000                                                                          
033100*01  -COPY W0009  -PRE ALT-                                               
033200                                                                          
033300*01  -COPY W0009  -PRE ALT2-                                              
033400                                                                          
033500*01  -COPY W0008  -PRE WDP7-                                              
033600     05  FILLER                  PIC X.                                   
033700                                                                          
033800*01  -COPY W0008  -PRE K601-                                              
033900     05  FILLER                  PIC X.                                   
034000                                                                          
034100*01  -COPY W0008  -PRE K701-                                              
034200     05  FILLER                  PIC X.                                   
034300                                                                          
034400*01  -COPY W0008  -PRE Q401-                                              
034500     05  FILLER                  PIC X.                                   
034600                                                                          
034700*01  -COPY W0008  -PRE Q301-                                              
034800     05  FILLER                  PIC X.                                   
034900                                                                          
035000*01  -COPY W0008  -PRE Q201-                                              
035100     05  FILLER                  PIC X.                                   
035200                                                                          
035300*01  -COPY W0008  -PRE Q401B-                                             
035400     05  FILLER                  PIC X.                                   
035500                                                                          
035600*01  -COPY W0008  -PRE WDB6-                                              
035700     05  FILLER                  PIC X.                                   
035800                                                                          
035900*01  -COPY W0008  -PRE Q4B1-                                              
036000     05  FILLER                  PIC X.                                   
036100                                                                          
036200*----> SUBPROGRAM W413AVSR.                                               
036300 01  AVSR-ORQI-PCB               PIC X.                                   
036400 01  AVSR-GMTB-PCB               PIC X.                                   
036500 01  AVSR-GMTC-PCB               PIC X.                                   
036600 01  AVSR-WDB2-PCB               PIC X.                                   
036700 01  AVSR-WDB6-PCB               PIC X.                                   
036800     EJECT                                                                
036900*----> SUBPROGRAM W413AVSO.                                               
037000 01  AVSO-ORDD-PCB               PIC X.                                   
037100 01  AVSO-ORQA-PCB               PIC X.                                   
037200 01  AVSO-ORQI-PCB               PIC X.                                   
037300 01  AVSO-GMTB-PCB               PIC X.                                   
037400 01  AVSO-XXKA-PCB               PIC X.                                   
037500 01  AVSO-4437-PCB               PIC X.                                   
037600 01  AVSO-XXKE-PCB               PIC X.                                   
037700 01  AVSO-XXKF-PCB               PIC X.                                   
037800 01  AVSO-XXKG-PCB               PIC X.                                   
037900 01  AVSO-XXKH-PCB               PIC X.                                   
038000 01  AVSO-XXKI-PCB               PIC X.                                   
038100 01  AVSO-XXKP-PCB               PIC X.                                   
038200 01  AVSO-WDB2-PCB               PIC X.                                   
038300 01  AVSO-WDB6-PCB               PIC X.                                   
038400 01  AVSO-WDP7-PCB               PIC X.                                   
038500 01  TRAN-XXKB-PCB               PIC X.                                   
038600 01  ORDN-ORQL-PCB               PIC X.                                   
038700 01  ORDN-PROC-PCB               PIC X.                                   
038800 01  ORDN-ORQI-PCB               PIC X.                                   
038900 01  ORDN-WDQ3-PCB               PIC X.                                   
039000     EJECT                                                                
039100 PROCEDURE DIVISION  USING MSG-PCB  AVSR-ALT-PCB ALT-PCB ALT2-PCB         
039200                           WDP7-PCB K601-PCB K701-PCB Q401-PCB            
039300                           Q301-PCB Q201-PCB Q401B-PCB                    
039400                           WDB6-PCB Q4B1-PCB                              
039500                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
039600                           AVSR-GMTC-PCB                                  
039700                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
039800                           AVSO-ORDD-PCB AVSO-ORQA-PCB                    
039900                           AVSO-ORQI-PCB                                  
040000                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
040100                           AVSO-4437-PCB AVSO-XXKE-PCB                    
040200                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
040300                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
040400                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
040500                           AVSO-WDB6-PCB AVSO-WDP7-PCB                    
040600                           TRAN-XXKB-PCB                                  
040700                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
040800                           ORDN-ORQI-PCB ORDN-WDQ3-PCB.                   
040900 MAIN SECTION.                                                            
041000     ENTRY 'DLITCBL' USING MSG-PCB  AVSR-ALT-PCB ALT-PCB ALT2-PCB         
041100                           WDP7-PCB K601-PCB K701-PCB Q401-PCB            
041200                           Q301-PCB Q201-PCB Q401B-PCB                    
041300                           WDB6-PCB Q4B1-PCB                              
041400                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
041500                           AVSR-GMTC-PCB                                  
041600                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
041700                           AVSO-ORDD-PCB AVSO-ORQA-PCB                    
041800                           AVSO-ORQA-PCB                                  
041900                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
042000                           AVSO-4437-PCB AVSO-XXKE-PCB                    
042100                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
042200                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
042300                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
042400                           AVSO-WDB6-PCB AVSO-WDP7-PCB                    
042500                           TRAN-XXKB-PCB                                  
042600                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
042700                           ORDN-ORQI-PCB ORDN-WDQ3-PCB.                   
042800                                                                          
042900     PERFORM IMS-GET-MSG                                                  
043000     IF SEGMENT-FINNS                                                     
043100       PERFORM A-INIT                                                     
043200       PERFORM B-KONTROL-INDATA                                           
043300                                                                          
043400       IF INDATA-OK                                                       
043500         PERFORM D-UPPDATERA                                              
043600                                                                          
043700       ELSE                                                               
043800         IF  MID-ADLAGOMR-IN = 000                                        
043900         AND MID-ADPLATS-IN  = 00000                                      
044000           CONTINUE                                                       
044100         ELSE                                                             
044200           IF FELTEXT-STR = SPACE                                         
044300             MOVE 'EJ GODKÄND INDATA, FEL I MID' TO FELTEXT-STR           
044400           END-IF                                                         
044500           CALL FELLOG                                                    
044600         END-IF                                                           
044700       END-IF                                                             
044800     END-IF                                                               
044900                                                                          
045000     MOVE ZERO TO RETURN-CODE                                             
045100     GOBACK                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 A-INIT          SECTION.                                                 
045500                                                                          
045600     ACCEPT DAGENS-DATUM  FROM DATE                                       
045700     ACCEPT DAGENS-TID    FROM TIME                                       
045800     MOVE ALL '+'           TO MID-W4I28901                               
045900                                                                          
046000     IF MSG-DUBBLA-TRANSKODER                                             
046100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I28901                 
046200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
046300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
046400     ELSE                                                                 
046500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I28901                  
046600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
046700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
046800     END-IF                                                               
046900                                                                          
047000     MOVE MSG-KDTRTYP       TO MFS-KDTRTYP                                
047100     MOVE MSG-IDPFK         TO MFS-IDPFK                                  
047200     MOVE MFS-IDTRANS       TO W-IDTRANS                                  
047300                                                                          
047400     MOVE LOW-VALUE TO MSG-AREA                                           
047500     MOVE JA                TO INDATA-SW                                  
047600                                                                          
047700     MOVE HIGH-VALUE        TO FILLER-1                                   
047800*                              W-Q4B1KY-MAX-HIGH-VAL                      
047900                                                                          
048000     MOVE +1                TO ANTAL-UPPDAT-INDX                          
048100     IF GODK-MID                                                          
048200       MOVE 'EJ GODKÄND MID!'      TO FELTEXT-STR2                        
048300     ELSE                                                                 
048400       MOVE NEJ             TO INDATA-SW                                  
048500     END-IF                                                               
048600                                                                          
048700     PERFORM AA-NOLLA-WOPS-TABELL                                         
048800     .                                                                    
048900     EJECT                                                                
049000 AA-NOLLA-WOPS-TABELL SECTION.                                            
049100                                                                          
049200     MOVE +1                   TO AVSR-INDX                               
049300     PERFORM UNTIL AVSR-INDX > AVSR-INDX-MAX                              
049400        MOVE +0                TO AVSR-ADLAGOMR(AVSR-INDX)                
049500        MOVE SPACE             TO AVSR-IDLEVNR(AVSR-INDX)                 
049600        MOVE SPACE             TO AVSR-IDDC(AVSR-INDX)                    
049700        MOVE ZERO              TO AVSR-KDSPEEMB(AVSR-INDX)                
049800        MOVE +0                TO AVSR-KVANNANT(AVSR-INDX)                
049900        MOVE +0                TO AVSR-KVBEART-Q(AVSR-INDX)               
050000        MOVE +0                TO AVSR-PRARTNTO(AVSR-INDX)                
050100        MOVE +0                TO AVSR-PRAVCOST(AVSR-INDX)                
050200        MOVE +0                TO AVSR-VKART(AVSR-INDX)                   
050300        MOVE +0                TO AVSR-VLARTNTO(AVSR-INDX)                
050400        MOVE +0                TO AVSR-KDVSOP(AVSR-INDX)                  
050500                                  AVSR-KDFARLIG(AVSR-INDX)                
050600        MOVE SPACE             TO AVSR-KDORDSTA(AVSR-INDX)                
050700        MOVE +0                TO AVSR-KDVIA   (AVSR-INDX)                
050800        MOVE +0                TO AVSR-KVDAGAR-DIFF(AVSR-INDX)            
050900        MOVE +0                TO AVSR-TISKEPPN-DDC(AVSR-INDX)            
051000        MOVE +0                TO AVSR-IDPRQUES(AVSR-INDX)                
051100        MOVE +0                TO AVSR-PRARTNTO-LOC(AVSR-INDX)            
051200        MOVE +0                TO AVSR-PRARTNTO-LOCPREL(AVSR-INDX)        
051300        MOVE +0                TO AVSR-PRARTBTO-LOC(AVSR-INDX)            
051400        MOVE SPACE             TO AVSR-KDVALISO(AVSR-INDX)                
051500        MOVE SPACE             TO AVSR-KDVAT(AVSR-INDX)                   
051600        MOVE +0                TO AVSR-RERAB(AVSR-INDX)                   
051700        MOVE SPACE             TO AVSR-KDRAB(AVSR-INDX)                   
051800        MOVE SPACE             TO AVSR-BEART-VIPS(AVSR-INDX)              
051900        ADD +1                 TO AVSR-INDX                               
052000     END-PERFORM                                                          
052100                                                                          
052200     MOVE +1                   TO AVSR-INDX                               
052300     .                                                                    
052400     EJECT                                                                
052500 B-KONTROL-INDATA       SECTION.                                          
052600                                                                          
052700     MOVE NEJ TO WS-UPD-WEIVOL                                            
052800                                                                          
052900     IF (MID-VLARTNTO-IN > ZERO OR MID-VKART-IN > ZERO)                   
053000      MOVE JA TO WS-UPD-WEIVOL                                            
053100     END-IF                                                               
053200                                                                          
053300     IF MID-ADLAGOMR-IN > ZERO AND NOT EGEN-MID                           
053400      MOVE JA TO WS-UPD-LOCATION                                          
053500     END-IF                                                               
053600                                                                          
053700     IF MID-IDARTNR-IN = 000000000                                        
053800        MOVE NEJ                    TO INDATA-SW                          
053900        MOVE 'IDARTNR ÄR NOLL'      TO FELTEXT-STR                        
054000     ELSE                                                                 
054100       IF MID-IDARTNR-IN NUMERIC                                          
054200         MOVE MID-IDARTNR-IN       TO W-Q4B1KY-IDARTNR                    
054300                                       W-IDARTNR                          
054400       ELSE                                                               
054500         MOVE NEJ                   TO INDATA-SW                          
054600         MOVE 'IDARTNR EJ NUMERISK' TO FELTEXT-STR                        
054700       END-IF                                                             
054800     END-IF                                                               
054900                                                                          
055300     IF MID-IDDC-IN = ZERO                                                
055400     OR MID-IDDC-IN = SPACE                                               
            IF WS-UPD-WEIVOL = JA                                               
              CONTINUE                                                          
            ELSE                                                                
055500        MOVE NEJ                    TO INDATA-SW                          
055600        MOVE 'IDDC-IN ÄR NOLL ELLER SPACE' TO FELTEXT-STR                 
            END-IF                                                              
055700     ELSE                                                                 
055800       MOVE MID-IDDC-IN             TO W-IDDC-B6                          
055900       PERFORM IMS-GU-WDB601                                              
056000       IF DCS-KDDC NOT = SPACE AND NOT DCS-DDC                            
056100         MOVE MID-IDDC-IN           TO W-Q4B1KY-IDDC                      
056200*                                      W-Q4B1KY-MIN-IDDC                  
056300                                       W-IDDC                             
056400       ELSE                                                               
056500         MOVE NEJ                    TO INDATA-SW                         
056600         MOVE 'IDDC-IN EJ NUMMERISK' TO FELTEXT-STR                       
056700       END-IF                                                             
056800     END-IF                                                               
057000                                                                          
057100     IF MID-ADLAGOMR-IN NUMERIC                                           
057200       IF MID-ADLAGOMR-IN = 000                                           
057300         MOVE +001                  TO W-Q4B1KY-ADLAGOMR                  
057400         MOVE 001                   TO WS-MID-ADLAGOMR                    
057500*OM MID-ADLAGOMR = 0. LÄGGS 1 PGA ATT WDQ212 ARB-TAB HAR                  
057600*LAGEROMRÅDEN 1 TILL 99. OM LAGOMR = 0 ABENDAR W413AVSR.                  
057700*                                                                         
057800*    LAGEROMRÅDE 0 ÄNDRAS ALLTID TILL 1. DETTA PGA AV ATT                 
057900*    LAGEROMRÅDESTABELLEN I WDQ212 ÄR 1 TILL 99. DET FINNS INTE           
058000*    NÅGOT LAGEROMRÅDE NOLL... MEN EFTERSOM MAN MÅSTE LAGRA DEN           
058100*    DATA SOM HÖR TILL DE ARTIKLAR SOM HAR LAGEROMRÅDE NOLL LÄGGS         
058200*    DETTA I LAGEROMRÅDE 1.                                               
058300*                                                                         
058400       ELSE                                                               
058500         MOVE MID-ADLAGOMR-IN       TO W-Q4B1KY-ADLAGOMR                  
058600                                       WS-MID-ADLAGOMR                    
058700       END-IF                                                             
058800     ELSE                                                                 
058900       MOVE NEJ                     TO INDATA-SW                          
059000       MOVE 'ADLAGOMR-IN EJ NUMERISK' TO FELTEXT-STR                      
059100     END-IF                                                               
059200                                                                          
059300     IF MID-ADGANG-IN NUMERIC                                             
059400       MOVE MID-ADGANG-IN           TO W-Q4B1KY-ADGANG                    
059500                                     WS-MID-ADGANG                        
059600     ELSE                                                                 
059700       MOVE NEJ                     TO INDATA-SW                          
059800       MOVE 'ADGANG-IN EJ NUMERISK' TO FELTEXT-STR                        
059900     END-IF                                                               
060000                                                                          
060100     IF MID-ADPLATS-IN NUMERIC                                            
060200       MOVE MID-ADPLATS-IN          TO W-Q4B1KY-ADPLATS                   
060300                                     WS-MID-ADPLATS                       
060400     ELSE                                                                 
060500       MOVE NEJ                     TO INDATA-SW                          
060600       MOVE 'ADPLATS-IN EJ NUMERISK' TO FELTEXT-STR                       
060700     END-IF                                                               
060800                                                                          
060900     IF MID-IDLOPNR-IN = 000                                              
061000     OR MID-IDLOPNR-IN = SPACE                                            
061100        CONTINUE                                                          
061200     ELSE                                                                 
061300       IF MID-IDLOPNR-IN > 000                                            
061400         IF MID-IDLOPNR-IN NUMERIC                                        
061500           CONTINUE                                                       
061600         ELSE                                                             
061700           MOVE NEJ                 TO INDATA-SW                          
061800           MOVE 'IDLOPNR-IN EJ NUMERISK' TO FELTEXT-STR                   
061900         END-IF                                                           
062000       END-IF                                                             
062100     END-IF                                                               
062200                                                                          
062300     IF MID-IDORDER-IN = 000                                              
062400     OR MID-IDORDER-IN = SPACE                                            
062500        CONTINUE                                                          
062600     ELSE                                                                 
062700       IF MID-IDORDER-IN > 000                                            
062800         IF MID-IDORDER-IN NUMERIC                                        
062900           CONTINUE                                                       
063000         ELSE                                                             
063100           MOVE NEJ                 TO INDATA-SW                          
063200           MOVE 'IDORDER-IN EJ NUMERISK' TO FELTEXT-STR                   
063300         END-IF                                                           
063400       END-IF                                                             
063500     END-IF                                                               
063600                                                                          
063700     IF MID-IDDISTR-IN = 00000                                            
063800     OR MID-IDDISTR-IN = SPACE                                            
063900        CONTINUE                                                          
064000     ELSE                                                                 
064100       IF MID-IDDISTR-IN > 000                                            
064200         IF MID-IDDISTR-IN NUMERIC                                        
064300           CONTINUE                                                       
064400         ELSE                                                             
064500           MOVE NEJ                 TO INDATA-SW                          
064600           MOVE 'IDDISTR-IN EJ NUMERISK' TO FELTEXT-STR                   
064700         END-IF                                                           
064800       END-IF                                                             
064900     END-IF                                                               
065000                                                                          
065100     IF MID-IDKUNDNR-IN = 0000000                                         
065200     OR MID-IDKUNDNR-IN = SPACE                                           
065300        CONTINUE                                                          
065400     ELSE                                                                 
065500       IF MID-IDKUNDNR-IN > 000                                           
065600         IF MID-IDKUNDNR-IN NUMERIC                                       
065700           CONTINUE                                                       
065800         ELSE                                                             
065900           MOVE NEJ                 TO INDATA-SW                          
066000           MOVE 'IDKUNDNR EJ NUMERISK' TO FELTEXT-STR                     
066100         END-IF                                                           
066200       END-IF                                                             
066300     END-IF                                                               
066400                                                                          
066500     IF MID-IDORDNR5-IN = 00000                                           
066600     OR MID-IDORDNR5-IN = SPACE                                           
066700        CONTINUE                                                          
066800     ELSE                                                                 
066900       IF MID-IDORDNR5-IN > 000                                           
067000         IF MID-IDORDNR5-IN NUMERIC                                       
067100           CONTINUE                                                       
067200         ELSE                                                             
067300           MOVE NEJ                 TO INDATA-SW                          
067400           MOVE 'IDORDNR5 EJ NUMERISK' TO FELTEXT-STR                     
067500         END-IF                                                           
067600       END-IF                                                             
067700     END-IF                                                               
067800                                                                          
067900     .                                                                    
068000     EJECT                                                                
068100 D-UPPDATERA       SECTION.                                               
068200     MOVE MID-IDARTNR-IN            TO W-IDARTNR                          
068300     PERFORM IMS-GU-WDK611                                                
068500     IF EGEN-MID                                                          
068600* THIS IS TRUE WHEN WE ARE RESTARTING THE PGM                             
068700       PERFORM DF-FLYTTA-MID-NYCKLAR                                      
068800       PERFORM IMS-GU-WDQ4B1-KVAL                                         
068900     ELSE                                                                 
069000       MOVE MID-IDARTNR-IN   TO W-Q4B1-IDARTNR-MIN                        
069100       MOVE MID-IDARTNR-IN   TO W-Q4B1-IDARTNR-MAX                        
069200       MOVE MID-IDDC-IN      TO W-IDDC                                    
069300       MOVE LOW-VALUE        TO LOW-FILLER                                
069400       MOVE HIGH-VALUE       TO HIGH-FILLER                               
069500                                                                          
069600       PERFORM IMS-GU-WDQ4B1-OKVAL                                        
069700     END-IF                                                               
069800**                                                                        
068400                                                                          
069810     IF WS-UPD-LOCATION = JA                                              
069900     PERFORM UNTIL ANTAL-UPPDAT-INDX > MAX-ANTAL-UPPDAT-RADER             
070000                OR SEGMENT-SAKNAS                                         
070100                OR END-OF-DATABASE                                        
070800       IF  (SEQB-IDDC    = MID-IDDC-IN)                                   
071000       IF  SEQB-ADLAGOMR = WS-MID-ADLAGOMR                                
071100       AND SEQB-ADGANG   = WS-MID-ADGANG                                  
071200       AND SEQB-ADPLATS  = WS-MID-ADPLATS                                 
071400         CONTINUE                                                         
071500*-----INGEN FÖRÄNDRING AV LAGEROMRÅDE PÅ ORDERRADEN.                      
071600       ELSE                                                               
071700         PERFORM DA-UPDATE-LOC-WEIGHT-VOLUME                              
081400       END-IF                                                             
081500       ELSE                                                               
081600         CONTINUE                                                         
081700       END-IF                                                             
081800                                                                          
081900       PERFORM IMS-GN-WDQ4B1-OKVAL                                        
082000     END-PERFORM                                                          
082200* IF THERE IS MORE DATA FOR RESTART SAVE THEM FROM Q4B1                   
082300      IF SEGMENT-FINNS                                                    
082400       IF ANTAL-UPPDAT-INDX > MAX-ANTAL-UPPDAT-RADER                      
082500         PERFORM E-STARTA-OM-4289                                         
082600       END-IF                                                             
082700      END-IF                                                              
            IF (SEGMENT-SAKNAS OR END-OF-DATABASE)                              
            AND WS-UPD-WEIVOL = JA                                              
             PERFORM IMS-GU-WDQ4B1-OKVAL                                        
            END-IF                                                              
082100     END-IF                                                               
082110* IF WEIGHT AND VOLUME NEEDS TO BE UPDATED                                
082120     IF WS-UPD-WEIVOL = JA                                                
082121     MOVE NEJ TO WS-UPD-LOCATION                                          
082122     PERFORM UNTIL ANTAL-UPPDAT-INDX > MAX-ANTAL-UPPDAT-RADER             
082123                OR SEGMENT-SAKNAS                                         
082124                OR END-OF-DATABASE                                        
082125       MOVE SEQB-ADLAGOMR TO WS-MID-ADLAGOMR                              
082126       MOVE SEQB-ADGANG   TO WS-MID-ADGANG                                
082127       MOVE SEQB-ADPLATS  TO WS-MID-ADPLATS                               
082129                                                                          
082139       PERFORM DA-UPDATE-LOC-WEIGHT-VOLUME                                
082144                                                                          
082145       PERFORM IMS-GN-WDQ4B1-OKVAL                                        
082146     END-PERFORM                                                          
082150     END-IF                                                               
082200* IF THERE IS MORE DATA FOR RESTART SAVE THEM FROM Q4B1                   
082300     IF SEGMENT-FINNS                                                     
082400       IF ANTAL-UPPDAT-INDX > MAX-ANTAL-UPPDAT-RADER                      
082500         PERFORM E-STARTA-OM-4289                                         
082600       END-IF                                                             
082700     END-IF                                                               
082800     .                                                                    
082900     EJECT                                                                
083000 DA-UPDATE-LOC-WEIGHT-VOLUME SECTION.                                     
083100     PERFORM DI-FLYTTA-NYCKLAR-Q401                                       
083200     PERFORM IMS-GU-WDQ201                                                
083300      IF SEGMENT-FINNS                                                    
083400       IF  OHUV-FLKLAR = "J"                                              
083500                                                                          
083600         IF  OHUV-TIREGDAT > 170101                                       
083700         AND OHUV-TIREGDAT < 500000                                       
083800*FÖR ATT LÖSA PROBLEM MED FÖR GAMLA DATUM                                 
083900                                                                          
084000           PERFORM IMS-GNP-WDQ212                                         
084100           PERFORM IMS-GHU-WDQ401                                         
084200                                                                          
084300           PERFORM DG-ANROP-W411LAST                                      
084400                                                                          
084500           IF LAST-ADLAGOMR-UT = +000                                     
084600           AND LAST-KVANTAL-UT = +0000000                                 
084700           AND LAST-KVBEART-UT = +0000000                                 
084800                                                                          
084900             PERFORM DH-ANROP-W413ADRS                                    
085000                                                                          
085100             IF (ADRS-ADLAGOMR-UT = +000                                  
085200             AND ADRS-ADPLATS-UT  = +00000)                               
085300             OR (ADRS-ADLAGOMR-UT = ORAD-ADLAGOMR                         
085400             AND ADRS-ADPLATS-UT  = ORAD-ADPLATS)                         
085500                                                                          
085600               MOVE OHUV-IDORDER  TO W-Q301KY-IDORDER                     
085700                                     W-Q301KY-IDORDER-MAX                 
085800               IF WS-UPD-WEIVOL = JA AND WS-UPD-LOCATION NOT = JA         
085900                MOVE SEQB-IDDC TO W-Q301KY-IDDC                           
086000                                  W-Q301KY-IDDC-MAX                       
086100               ELSE                                                       
086200                MOVE MID-IDDC-IN   TO W-Q301KY-IDDC                       
086300                                      W-Q301KY-IDDC-MAX                   
086400               END-IF                                                     
086500               MOVE NEJ  TO UTSKRIVEN-ORDERDEL-FINNS-SW                   
086600                                                                          
086700               PERFORM IMS-GU-WDQ301                                      
086800               IF SEGMENT-FINNS                                           
086900                                                                          
087000               PERFORM UNTIL SEGMENT-SAKNAS                               
087100                 IF  ODEL-KDODELSTA = 'U ' OR 'P '                        
087200                 AND ODEL-IDLEVNR = SPACE                                 
087300                   MOVE JA TO UTSKRIVEN-ORDERDEL-FINNS-SW                 
087400                 END-IF                                                   
087500                 PERFORM IMS-GN-WDQ301                                    
087600               END-PERFORM                                                
087700                                                                          
087800               IF UTSKRIVEN-ORDERDEL-FINNS-SW = NEJ                       
087900                                                                          
088000                 PERFORM DB-AVSR-ANROP-TAG-BORT-RAD                       
088100                 PERFORM IMS-DLET-WDQ401                                  
088200                                                                          
088300                 PERFORM S01-STARTA-W413AVSO                              
088400                                                                          
088500                 PERFORM DE-AVSR-ANROP-UPPLAGG-RAD                        
088600                                                                          
088700                 PERFORM DC-MOVE-TO-IO-AREA-Q401                          
088800                                                                          
088900                 PERFORM IMS-ISRT-Q401-B-PCB                              
089000*OBS. INSERT MED ANNAT PCB ÄN I LÄSN. FÖR ATT BEHÅLLA PEKARE              
089100                 PERFORM UNTIL SEGMENT-FINNS                              
089200                    ADD +1           TO ORAD-IDLOPNR                      
089300                    PERFORM IMS-ISRT-Q401-B-PCB                           
089400                 END-PERFORM                                              
089600                                                                          
089700                 PERFORM S01-STARTA-W413AVSO                              
089800                                                                          
089900                 ADD  +1             TO ANTAL-UPPDAT-INDX                 
090000*OBS. ANTAL UPPDATERINGAR PER RAD ÄR 28. DVS PÅ Q2 OCH Q4 INKL            
090100*SEKUNDÄR INDEX. UPPDATERING AV 25 (=MAX-INDX) WDQ401 RADER BLIR          
090200*ALLTSÅ 700 UPPDATERINGAR I IMS.                                          
090300               ELSE                                                       
090400                 CONTINUE                                                 
090500*MINST EN ORDER-DEL UTSKRIVEN INGEN FLYTT AV ORAD KAN SKE.                
090600               END-IF                                                     
090700               ELSE                                                       
090800                 CONTINUE                                                 
090900*ORDER-DEL SAKNAS.                                                        
091000               END-IF                                                     
091100             ELSE                                                         
091200               CONTINUE                                                   
091300*LAGOMR JUSTERAD AV W413ADRS. ÄNDRA INTE PÅ ORDERRADEN.                   
091400             END-IF                                                       
091500           ELSE                                                           
091600             CONTINUE                                                     
091700*LAGOMR JUSTERAD AV W411LAST. ÄNDRA INTE PÅ ORDERRADEN.                   
091800           END-IF                                                         
091900         ELSE                                                             
092000           CONTINUE                                                       
092100*ORDER ANVÄNDS FN. AV UTSKRIFT EL. NÅGOT ANNAT PROGRAM.                   
092200         END-IF                                                           
092300         ELSE                                                             
092400           CONTINUE                                                       
092500*ORDER FÖR GAMMAL FINNS INGA ARBTIDER.                                    
092600      END-IF                                                              
092700     END-IF                                                               
092800     .                                                                    
092900     EJECT                                                                
093000 DB-AVSR-ANROP-TAG-BORT-RAD            SECTION.                           
093100                                                                          
093200     MOVE +1                 TO   AVSR-INDX                               
093300     MOVE +2                 TO   AVSR-KDCALL                             
093400                                                                          
093500     MOVE OHUV-IDORDER       TO   AVSR-IDORDER                            
093600     MOVE OHUV-KDORDKL       TO   AVSR-KDORDKL                            
093700     MOVE +0                 TO   AVSR-KDFRAKT                            
093800     MOVE ZERO               TO   AVSR-KDROPACK                           
093900     MOVE ORAD-ADLAGOMR      TO   AVSR-ADLAGOMR(AVSR-INDX)                
094000     MOVE ORAD-IDLEVNR       TO   AVSR-IDLEVNR(AVSR-INDX)                 
094100     MOVE ORAD-IDDC          TO   AVSR-IDDC(AVSR-INDX)                    
094200     MOVE ORAD-KDSPEEMB      TO   AVSR-KDSPEEMB(AVSR-INDX)                
094300     MOVE ORAD-KVBEART-Q     TO   AVSR-KVANNANT(AVSR-INDX)                
094400     MOVE ORAD-KVBEART-Q     TO   AVSR-KVBEART-Q(AVSR-INDX)               
094500     MOVE ORAD-PRARTNTO      TO   AVSR-PRARTNTO(AVSR-INDX)                
094600     MOVE ORAD-PRAVCOST      TO   AVSR-PRAVCOST(AVSR-INDX)                
094700     MOVE ORAD-VKART         TO   AVSR-VKART(AVSR-INDX)                   
094800     MOVE ORAD-VLARTNTO      TO   AVSR-VLARTNTO(AVSR-INDX)                
094900     MOVE SPACE              TO   AVSR-KDORDSTA(AVSR-INDX)                
095000     MOVE +0                 TO   AVSR-KDVIA   (AVSR-INDX)                
095100     MOVE +0                 TO   AVSR-KVDAGAR-DIFF(AVSR-INDX)            
095200     MOVE +0                 TO   AVSR-TISKEPPN-DDC(AVSR-INDX)            
095300     MOVE ORAD-DEAL-PR-LINE  TO   AVSR-DEAL-PR-LINE(AVSR-INDX)            
095400     CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB                       
095500                         AVSR-ORQI-PCB AVSR-GMTB-PCB                      
095600                         AVSR-GMTC-PCB AVSR-WDB2-PCB                      
095700                         AVSR-WDB6-PCB TRAN-XXKB-PCB                      
095800     .                                                                    
095900     EJECT                                                                
096000 DE-AVSR-ANROP-UPPLAGG-RAD      SECTION.                                  
096100                                                                          
096200     MOVE +1                   TO AVSR-INDX                               
096300     MOVE +1                   TO AVSR-KDCALL                             
096400                                                                          
096500     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
096600     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
096700     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
096800     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
096900     MOVE ORAD-TIREGDAT        TO AVSR-TIREGDAT                           
097000     MOVE ORAD-TIREGTID        TO AVSR-TIHHMM                             
097100                                                                          
097200     MOVE WS-MID-ADLAGOMR      TO AVSR-ADLAGOMR(AVSR-INDX)                
097300     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(AVSR-INDX)                 
097400     MOVE ORAD-IDDC            TO AVSR-IDDC(AVSR-INDX)                    
097500     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(AVSR-INDX)                
097600     MOVE +0                   TO AVSR-KVANNANT(AVSR-INDX)                
097700     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(AVSR-INDX)               
097800     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(AVSR-INDX)                
097900     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(AVSR-INDX)                
098000     MOVE ORAD-VKART           TO AVSR-VKART(AVSR-INDX)                   
098100     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(AVSR-INDX)                
098200     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(AVSR-INDX)            
098300                                                                          
098400     CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB                       
098500          AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                       
098600          AVSR-WDB2-PCB AVSR-WDB6-PCB                                     
098700          TRAN-XXKB-PCB                                                   
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100 DC-MOVE-TO-IO-AREA-Q401              SECTION.                            
099200*INITIATE NEW ADDRESS IN SAME I-O AREA                                    
099300     MOVE WS-MID-ADLAGOMR            TO ORAD-ADLAGOMR                     
099400     MOVE WS-MID-ADPLATS             TO ORAD-ADPLATS                      
099500     MOVE WS-MID-ADGANG              TO ORAD-ADGANG                       
099600     IF MID-VKART-IN NUMERIC AND MID-VKART-IN > ZERO                      
099700       MOVE MID-VKART-IN           TO WS-MID-VKART                        
099800                                      ORAD-VKART                          
099900     ELSE                                                                 
100000       CONTINUE                                                           
100100     END-IF                                                               
100200                                                                          
100300     IF MID-VLARTNTO-IN NUMERIC AND MID-VLARTNTO-IN > ZERO                
100400       MOVE MID-VLARTNTO-IN        TO WS-MID-VLARTNTO                     
100500                                      ORAD-VLARTNTO                       
100600     ELSE                                                                 
100700       CONTINUE                                                           
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100 DG-ANROP-W411LAST   SECTION.                                             
101200                                                                          
101300     MOVE ORAD-ADLAGOMR              TO LAST-ADLAGOMR                     
101400     MOVE OHUV-FLFORBI               TO LAST-FLFORBI                      
101500     MOVE OHUV-FLOVRLEV              TO LAST-FLOVRLEV                     
101600     MOVE OHUV-FLORDSPE              TO LAST-FLORDSPE                     
101700     MOVE ORAD-IDLEVNR               TO LAST-IDLEVNR                      
101800     MOVE ORAD-IDDC                  TO LAST-IDDC                         
101900     MOVE ARB-KDFDKRAV               TO LAST-KDFDKRAV                     
102000     MOVE ORAD-KVPREAVB              TO LAST-KVPREAVB                     
102100     MOVE CLAG-KVQPACK-3             TO LAST-KVQPACK-3                    
102200     MOVE CLAG-KVQPACK-4             TO LAST-KVQPACK-4                    
102300                                                                          
102400     CALL W411LAST USING LAST-W411LAST                                    
102500     .                                                                    
102600     EJECT                                                                
102700 DH-ANROP-W413ADRS   SECTION.                                             
102800                                                                          
102900     MOVE ORAD-ADLAGOMR              TO ADRS-ADLAGOMR-IN                  
103000     MOVE ORAD-ADPLATS               TO ADRS-ADPLATS-IN                   
103100     MOVE OHUV-BEVARREF              TO ADRS-BEVARREF-IN                  
103200     MOVE OHUV-FLFORBI               TO ADRS-FLFORBI-IN                   
103300     MOVE OHUV-IDDISTR               TO ADRS-IDDISTR-IN                   
103400     MOVE 1                          TO ADRS-KDCALL-IN                    
103500     MOVE ORAD-IDDC                  TO ADRS-IDDC-IN                      
103600     MOVE OHUV-KDORDKL               TO ADRS-KDORDKL-IN                   
103700     MOVE ORAD-KVBEART-Q             TO ADRS-KVBEART-Q-IN                 
103800     MOVE ORAD-VLARTNTO              TO ADRS-VLARTNTO-IN                  
103900                                                                          
104000     CALL W413ADRS USING ADRS-W413ADRS                                    
104100     .                                                                    
104200     EJECT                                                                
104300 DI-FLYTTA-NYCKLAR-Q401   SECTION.                                        
104400                                                                          
104500     MOVE SEQB-IDORDER            TO  W-Q401KY-IDORDER                    
104600                                      W-IDORDER                           
104700     MOVE SEQB-IDDC               TO  W-Q401KY-IDDC                       
104800                                      W-IDDC                              
104900     MOVE SEQB-ADLAGOMR           TO  W-Q401KY-ADLAGOMR                   
105000     MOVE SEQB-ADGANG             TO  W-Q401KY-ADGANG                     
105100     MOVE SEQB-ADPLATS            TO  W-Q401KY-ADPLATS                    
105200     MOVE SEQB-IDARTNR            TO  W-Q401KY-IDARTNR                    
105300     MOVE SEQB-IDLOPNR            TO  W-Q401KY-IDLOPNR                    
105400     .                                                                    
105500     EJECT                                                                
105600 DF-FLYTTA-MID-NYCKLAR   SECTION.                                         
105700                                                                          
105800     MOVE MID-IDARTNR-IN          TO W-Q4B1KY-IDARTNR                     
105900                                     W-Q4B1-IDARTNR-MIN                   
106000                                     W-Q4B1-IDARTNR-MAX                   
106100     MOVE MID-IDLOPNR-IN          TO W-Q4B1KY-IDLOPNR                     
106200     MOVE MID-IDDISTR-IN          TO W-Q4B1KY-IDDISTR                     
106300     MOVE MID-IDKUNDNR-IN         TO W-Q4B1KY-IDKUNDNR                    
106400     INITIALIZE W-Q4B1KY-IDKUNDRF                                         
106500     MOVE MID-IDORDNR5-IN         TO W-Q4B1KY-IDORDNR7(3:5)               
106600     MOVE MID-IDORDER-IN          TO W-Q4B1KY-IDORDER                     
106700     MOVE MID-IDDC-IN             TO W-Q4B1KY-IDDC                        
106800*                                                                         
106900     MOVE ALL '+'                 TO MSGI-WMSGINIT                        
107000     MOVE '001'                   TO MSGI-KDCALL                          
107100*    MOVE MSG-LTERM-NAME          TO MSGI-IDLTERM-USER                    
107100     MOVE SPACES                  TO MSGI-IDLTERM-USER                    
107200     MOVE MSG-SIGNON-USERID       TO MSGI-IDUSER                          
107300     MOVE '4289'                  TO MSGI-IDTRANS                         
107400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
107500     MOVE MSGI-SPAR-AREA          TO SAVE-AREA                            
107600     MOVE SAVE-ADLAGOMR-NEXT      TO W-Q4B1KY-ADLAGOMR                    
107700     MOVE SAVE-ADGANG-NEXT        TO W-Q4B1KY-ADGANG                      
107800     MOVE SAVE-ADPLATS-NEXT       TO W-Q4B1KY-ADPLATS                     
107810     MOVE SAVE-LOCATION-UPD       TO WS-UPD-LOCATION                      
107820     MOVE SAVE-WEIVOL-UPD         TO WS-UPD-WEIVOL                        
107900     .                                                                    
108000     SKIP2                                                                
108100 E-STARTA-OM-4289     SECTION.                                            
108200                                                                          
108300*FLYTT FÖR ATT ANVÄNDA VID EN EV. OMSTART AV 4289.                        
108400     MOVE SEQB-IDDISTR            TO MID-IDDISTR-IN                       
108500     MOVE SEQB-IDKUNDNR           TO MID-IDKUNDNR-IN                      
108600     MOVE SEQB-IDORDNR7(3:5)      TO MID-IDORDNR5-IN                      
108700     MOVE SEQB-IDORDER            TO MID-IDORDER-IN                       
108800     MOVE SEQB-IDLOPNR            TO MID-IDLOPNR-IN                       
108900     MOVE SEQB-IDDC               TO MID-IDDC-IN                          
109000     MOVE SEQB-IDARTNR            TO MID-IDARTNR-IN                       
109100     MOVE WS-MID-VKART            TO MID-VKART-IN                         
109200     MOVE WS-MID-VLARTNTO         TO MID-VLARTNTO-IN                      
109300*                                                                         
109400     MOVE '002'                   TO MSGI-KDCALL                          
109500     MOVE '4289'                  TO SAVE-IDTRANS                         
                                           MSGI-IDTRANS                         
109600     MOVE MSG-SIGNON-USERID       TO MSGI-IDUSER                          
109700     MOVE SEQB-ADLAGOMR           TO SAVE-ADLAGOMR-NEXT                   
109800     MOVE SEQB-ADGANG             TO SAVE-ADGANG-NEXT                     
109900     MOVE SEQB-ADPLATS            TO SAVE-ADPLATS-NEXT                    
           MOVE WS-UPD-LOCATION         TO SAVE-LOCATION-UPD                    
           MOVE WS-UPD-WEIVOL           TO SAVE-WEIVOL-UPD                      
110000     MOVE SAVE-AREA               TO MSGI-SPAR-AREA                       
110100     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
110200*                                                                         
110300     COMPUTE PTOP1-KVLL   =  LENGTH OF MID-W4I28901 + 19                  
110400     END-COMPUTE                                                          
110500     MOVE MSG-KDMFSFOR-1          TO PTOP1-KDMFSFOR                       
110600     PERFORM IMS-PURG-4289-ALT2                                           
110700     .                                                                    
110800     EJECT                                                                
110900 S01-STARTA-W413AVSO     SECTION.                                         
111000     MOVE OHUV-IDDISTR       TO   AVSO-IDDISTR                            
111100     MOVE OHUV-IDKUNDNR      TO   AVSO-IDKUNDNR                           
111200     MOVE OHUV-IDKUNDRF      TO   AVSO-IDKUNDRF                           
111300     MOVE OHUV-IDORDER       TO   AVSO-IDORDER                            
111400     MOVE SPACE              TO   AVSO-IDDC                               
111500     MOVE ZERO               TO   AVSO-TIRFS                              
111600     MOVE ZERO               TO   AVSO-TIAAMMDD                           
111700     MOVE ZERO               TO   AVSO-TIHHMM                             
111800     MOVE W-IDTRANS          TO   AVSO-IDTRANS                            
111900                                                                          
112000     CALL W413AVSO USING AVSO-W413AVSO                                    
112100     AVSO-ORDD-PCB AVSO-ORQA-PCB                                          
112200     AVSO-ORQI-PCB AVSO-GMTB-PCB                                          
112300     AVSO-XXKA-PCB AVSO-4437-PCB AVSO-XXKE-PCB                            
112400     AVSO-XXKF-PCB AVSO-XXKG-PCB AVSO-XXKH-PCB                            
112500     AVSO-XXKI-PCB AVSO-XXKP-PCB AVSO-WDB2-PCB                            
112600     AVSO-WDB6-PCB AVSO-WDP7-PCB                                          
112700     TRAN-XXKB-PCB                                                        
112800     ORDN-ORQL-PCB ORDN-PROC-PCB                                          
112900     ORDN-ORQI-PCB ORDN-WDQ3-PCB                                          
113000     .                                                                    
113100     SKIP2                                                                
113200                                                                          
113300* --- IMS SEKTIONER ---                                                   
113400                                                                          
113500     SKIP3                                                                
113600 IMS-GET-MSG SECTION.                                                     
113700     MOVE '  QC' TO GODK-STATUSKODER                                      
113800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
113900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
114000     PERFORM IMS-STATUSKONTROLL                                           
114100     .                                                                    
114200                                                                          
114300     SKIP3                                                                
114400 IMS-PURG-4289-ALT2 SECTION.                                              
114500     MOVE SPACE TO GODK-STATUSKODER                                       
114600     CALL CBLTDLI USING PURG ALT2-PCB P-TO-P-SW1                          
114700     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
114800     PERFORM IMS-STATUSKONTROLL                                           
114900     .                                                                    
115000     SKIP3                                                                
115100 IMS-GU-WDQ201 SECTION.                                                   
115200     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
115300          DELIMITED BY SIZE INTO SSA1                                     
115400     MOVE '  GE'             TO GODK-STATUSKODER                          
115500     CALL CBLTDLI USING GU Q201-PCB IO-AREA-Q201 SSA1                     
115600     MOVE Q201-STATUS-CODE     TO STATUS-WS                               
115700     PERFORM IMS-STATUSKONTROLL                                           
115800     .                                                                    
115900     SKIP2                                                                
116000 IMS-GNP-WDQ212 SECTION.                                                  
116100     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
116200          DELIMITED BY SIZE INTO SSA1                                     
116300     MOVE '  '               TO GODK-STATUSKODER                          
116400     CALL CBLTDLI USING GNP  Q201-PCB IO-AREA-Q212 SSA1                   
116500     MOVE Q201-STATUS-CODE     TO STATUS-WS                               
116600     PERFORM IMS-STATUSKONTROLL                                           
116700     .                                                                    
116800     SKIP2                                                                
116900 IMS-GU-WDQ301     SECTION.                                               
117000     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
117100                     '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                   
117200          DELIMITED BY SIZE INTO SSA1                                     
117300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
117400     CALL CBLTDLI USING GU Q301-PCB IO-AREA-Q301 SSA1                     
117500     MOVE Q301-STATUS-CODE TO STATUS-WS                                   
117600     PERFORM IMS-STATUSKONTROLL                                           
117700     .                                                                    
117800     SKIP2                                                                
117900 IMS-GN-WDQ301     SECTION.                                               
118000     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
118100                     '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                   
118200          DELIMITED BY SIZE INTO SSA1                                     
118300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
118400     CALL CBLTDLI USING GN Q301-PCB IO-AREA-Q301 SSA1                     
118500     MOVE Q301-STATUS-CODE TO STATUS-WS                                   
118600     PERFORM IMS-STATUSKONTROLL                                           
118700     .                                                                    
118800     SKIP2                                                                
118900 IMS-GN-WDQ4B1-OKVAL SECTION.                                             
119000     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
119100                     '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X ')'                   
119200          DELIMITED BY SIZE INTO SSA1                                     
119300     MOVE '  GE' TO GODK-STATUSKODER                                      
119400     CALL CBLTDLI USING GN Q4B1-PCB IO-AREA-Q4B1 SSA1                     
119500     MOVE Q4B1-STATUS-CODE TO STATUS-WS                                   
119600     PERFORM IMS-STATUSKONTROLL                                           
119700     .                                                                    
119800     EJECT                                                                
119900 IMS-GU-WDQ4B1-OKVAL SECTION.                                             
120000     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
120100                     '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X ')'                   
120200          DELIMITED BY SIZE INTO SSA1                                     
120300     MOVE '  GE' TO GODK-STATUSKODER                                      
120400     CALL CBLTDLI USING GU Q4B1-PCB IO-AREA-Q4B1 SSA1                     
120500     MOVE Q4B1-STATUS-CODE TO STATUS-WS                                   
120600     PERFORM IMS-STATUSKONTROLL                                           
120700     .                                                                    
120800     EJECT                                                                
120900 IMS-GU-WDQ4B1-KVAL    SECTION.                                           
121000     STRING 'WDQ4B1  (WDQ4B1KY =' W-WDQ4B1KY-X ')'                        
121100          DELIMITED BY SIZE INTO SSA1                                     
121200     MOVE '  GE' TO GODK-STATUSKODER                                      
121300     CALL CBLTDLI USING GU Q4B1-PCB IO-AREA-Q4B1 SSA1                     
121400     MOVE Q4B1-STATUS-CODE TO STATUS-WS                                   
121500     PERFORM IMS-STATUSKONTROLL                                           
121600     .                                                                    
121700     SKIP3                                                                
121800 IMS-GU-WDK611    SECTION.                                                
121900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
122000          DELIMITED BY SIZE INTO SSA1                                     
122100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
122200          DELIMITED BY SIZE INTO SSA2                                     
122300     MOVE '  GE' TO GODK-STATUSKODER                                      
122400     CALL CBLTDLI USING GU K601-PCB IO-AREA-K611 SSA1 SSA2                
122500     MOVE K601-STATUS-CODE TO STATUS-WS                                   
122600     PERFORM IMS-STATUSKONTROLL                                           
122700     .                                                                    
122800     EJECT                                                                
122900 IMS-GHU-WDQ401    SECTION.                                               
123000     STRING 'WDQ401  (WDQ401KY =' W-WDQ401KY-X ')'                        
123100          DELIMITED BY SIZE INTO SSA1                                     
123200     MOVE '  GE' TO GODK-STATUSKODER                                      
123300     CALL CBLTDLI USING GHU Q401-PCB IO-AREA-Q401 SSA1                    
123400     MOVE Q401-STATUS-CODE TO STATUS-WS                                   
123500     PERFORM IMS-STATUSKONTROLL                                           
123600     .                                                                    
123700     SKIP3                                                                
123800 IMS-DLET-WDQ401  SECTION.                                                
123900     MOVE '  ' TO GODK-STATUSKODER                                        
124000     CALL CBLTDLI USING DLET Q401-PCB IO-AREA-Q401                        
124100     MOVE Q401-STATUS-CODE TO STATUS-WS                                   
124200     PERFORM IMS-STATUSKONTROLL                                           
124300     .                                                                    
124400     EJECT                                                                
124500 IMS-ISRT-Q401-B-PCB SECTION.                                             
124600     MOVE 'WDQ401   '      TO SSA1                                        
124700     MOVE '  II'           TO GODK-STATUSKODER                            
124800     CALL CBLTDLI USING ISRT Q401B-PCB IO-AREA-Q401 SSA1                  
124900     MOVE Q401B-STATUS-CODE TO STATUS-WS                                  
125000     PERFORM IMS-STATUSKONTROLL                                           
125100     .                                                                    
125200     EJECT                                                                
125300 IMS-GU-WDB601    SECTION.                                                
125400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
125500          DELIMITED BY SIZE INTO SSA1                                     
125600     MOVE '  GE' TO GODK-STATUSKODER                                      
125700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
125800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
125900     PERFORM IMS-STATUSKONTROLL                                           
126000     IF SEGMENT-SAKNAS                                                    
126100         MOVE SPACE TO DCS-KDDC                                           
126200     END-IF                                                               
126300     .                                                                    
126400 IMS-STATUSKONTROLL SECTION.                                              
126500     SET STATUS-IX TO 1                                                   
126600     SEARCH GODK-STATUS                                                   
126700       AT END                                                             
126800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
126900         DELIMITED BY SIZE INTO FELTEXT                                   
127000         CALL FELLOG                                                      
127100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
127200         CONTINUE                                                         
127300     END-SEARCH                                                           
127400     .                                                                    
