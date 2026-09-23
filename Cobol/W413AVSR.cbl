000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W413AVSR.                                                
000400 AUTHOR.         LASSE CALAIS.                                            
000500 DATE-WRITTEN.   MAJ -90.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    DETTA ÄR EN SUBMODUL SOM ANROPAS VID RADBEHANDLING.                  
001000*    ARBETSTABELLEN UPPDATERAS MED RADERNAS TOTALER SAMT EV               
001100*    DIREKTLEVERANSSEGMENT LÄGGS UPP/TAS BORT FÖR DIR.LEV-RADER.          
001200*    KDCALL=3 ANVÄNDS FÖR CLEARING LDC FRÅN W40255.                       
001300*                                                                         
001400*    REGISTER :  WLORQI (WDQ2) ORDERHUVUD                                 
001500*                WLGMTB (WDB3) KUNDREGISTER DC-INFO                       
001600*                WLGMTC (WDB5) KUNDREGISTER FRAKT-INFO                    
001700*                       (WDB2) KUNDREGISTER                               
001800*                                                                         
001900*    LÄNKAREA :  W413AVSR                                                 
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600*    -- CHECKED BY WY2000                                                 
002700     SKIP3                                                                
002800 01  IDPGM                       PIC X(08)   VALUE 'W413AVSR'.            
002900 01  FILLER                      PIC X(08)   VALUE 'FELTEXT:'.            
003000 01  FELTEXT                     PIC X(64)   VALUE SPACE.                 
003100 01  JA                          PIC X       VALUE 'J'.                   
003200 01  YES                         PIC X(3)    VALUE 'YES'.                 
003300 01  NEJ                         PIC X       VALUE 'N'.                   
003400 01  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
003500 01  WS-VKORDNTO                 PIC S9(6)V9(5) VALUE +0 COMP-3.          
003600 01  WS-VLORDNTO                 PIC S9(4)V9(5) VALUE +0 COMP-3.          
003700 01  WS-KDROPACK                 PIC X(1)    VALUE SPACE.                 
003800 01  FILLER                      PIC X(8)    VALUE 'INDEX'.               
003900 01  WS-INDEX-100-MAX            PIC S9(9)   COMP-3  VALUE +100.          
004000 77  RFS-IX                      PIC S9(4)   VALUE +0  COMP SYNC.         
004100 77  MAX-RFS-IX                  PIC S9(4)   VALUE +4  COMP SYNC.         
004200 01  RAD                         PIC S9(9)   VALUE +0.                    
004300 01  W-TIRFS                     PIC S9(11)  VALUE ZERO COMP-3.           
004400       EJECT                                                              
004500*                                                                         
004600 01  WS-TIHHMM                   PIC 9(4)    VALUE ZERO.                  
004700 01  FILLER REDEFINES WS-TIHHMM.                                          
004800     03 WS-TIHH                  PIC 9(2).                                
004900     03 WS-TIMM                  PIC 9(2).                                
005000*                                                                         
005100                                                                          
005200 01  WS-TIRFS                    PIC 9(10).                               
005300 01  FILLER REDEFINES WS-TIRFS.                                           
005400     03  WS-TIRFS-DAT            PIC 9(6).                                
005500     03  WS-TIRFS-TID            PIC 9(4).                                
005600     EJECT                                                                
005700*                                                                         
005800                                                                          
005900 01  GENERELLA-SUBPROGRAM.                                                
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006300     03  W411TRAN                PIC X(8)    VALUE 'W411TRAN'.            
006400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
006500                                                                          
006600*    PARAMETRAR TILL SUBPROGRAM ABEND                                     
006700 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
006800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
006900                                                                          
007000     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
007200*    -COPY WORKAREA                                                       
007300     EJECT                                                                
007400                                                                          
007500 01  FILLER                      PIC X(16)   VALUE 'WWDCKONS'.            
007600*01  -COPY WWDCKONS                                                       
007700                                                                          
007800 01  FILLER                      PIC X(16)   VALUE 'WWFRAKT1'.            
007900*01  -COPY WWFRAKT1                                                       
008000                                                                          
008100 01  FILLER                      PIC X(16)   VALUE 'WWDIST17'.            
008200*01  -COPY WWDIST17                                                       
008300                                                                          
008400 01  FILLER                      PIC X(16)   VALUE 'WWDIST52'.            
008500*01  -COPY WWDIST52                                                       
008600                                                                          
008700 01  TEST-IDDISTR              PIC S9(5)    COMP-3.                       
008800 01  FILLER REDEFINES TEST-IDDISTR.                                       
008900*    03     -COPY WWDIST15.                                               
009000                                                                          
009100*    PARAMETRAR TILL SUBPROGRAM W411TRAN                                  
009200 01  FILLER PIC X(16)   VALUE 'W411TRAN'.                                 
009300*    -COPY W411TRAN                                                       
009400     EJECT                                                                
009500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP2                                                                
009900*    --- STATUS-KOD FRÅN IMS                                              
010000 01  STATUS-WS                   PIC XX.                                  
010100     88  SEGMENT-FINNS                       VALUE '  '.                  
010200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010400     SKIP2                                                                
010500 01  GODK-STATUSKODER.                                                    
010600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010700     SKIP2                                                                
010800 01  SSA1                        PIC X(64).                               
010900 01  SSA2                        PIC X(64).                               
011000 01  SSA3                        PIC X(64).                               
011100     EJECT                                                                
011200                                                                          
011300*    --- IMS FUNKTIONSKODER                                               
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600*                                                                         
011700 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
011800     SKIP2                                                                
011900 01  NYCKLAR-TILL-DLI.                                                    
012000     03  W-WDQ211KY-X.                                                    
012100         05  W-IDDC-Q211         PIC  X(2).                               
012200         05  W-IDLEVNR-Q211      PIC  X(5).                               
012300                                                                          
012400     03  W-IDORDER-X.                                                     
012500         05  W-IDORDER           PIC  S9(7)  COMP-3.                      
012600                                                                          
012700     03  W-IDDC-X.                                                        
012800         05  W-IDDC              PIC  X(2).                               
012900                                                                          
013000     03  W-ADLAGOMR-X.                                                    
013100         05  W-ADLAGOMR          PIC  S9(3)  COMP-3.                      
013200                                                                          
013300     03  W-WDB301KY-X.                                                    
013400         05  W-IDDC-WDB3         PIC X(2).                                
013500         05  W-IDDISTR-WDB3      PIC S9(5)   COMP-3.                      
013600         05  W-IDKUNDNR-WDB3     PIC S9(7)   COMP-3.                      
013700                                                                          
013800     03  W-WDB301KY-DEF-X.                                                
013900         05  W-IDDC-WDB3-DEF     PIC X(2).                                
014000         05  W-IDDISTR-WDB3-DEF  PIC S9(5)   COMP-3.                      
014100         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
014200                                                                          
014300     03  W-WDB501KY-X.                                                    
014400         05  W-IDDC-WDB5         PIC X(2).                                
014500         05  W-KDFRAKT-WDB5      PIC S9(3)   COMP-3.                      
014600         05  W-IDDISTR-WDB5      PIC S9(5)   COMP-3.                      
014700         05  W-IDKUNDNR-WDB5     PIC S9(7)   COMP-3.                      
014800                                                                          
014900     03  W-WDB501KY-DEF-X.                                                
015000         05  W-IDDC-WDB5-DEF     PIC X(2).                                
015100         05  W-KDFRAKT-WDB5-DEF  PIC S9(3)   COMP-3.                      
015200         05  W-IDDISTR-WDB5-DEF  PIC S9(5)   COMP-3.                      
015300         05  W-IDKUNDNR-WDB5-DEF PIC S9(7) VALUE +9999999  COMP-3.        
015400                                                                          
015500     03  W-IDGMT-X.                                                       
015600         05  W-IDDISTR     PIC S9(5)   VALUE ZERO COMP-3.                 
015700         05  W-IDKUNDNR    PIC S9(7)   VALUE ZERO COMP-3.                 
015800                                                                          
015900     03  W-IDDC-B6-X.                                                     
016000         05 W-IDDC-B6                  PIC X(2).                          
016100                                                                          
016200     EJECT                                                                
016300*                                                                         
016400 01  FILLER                      PIC X(16) VALUE 'WDQ201-AREA'.           
016500 01  DLI-IO-AREA-WDQ201.                                                  
016600     03  WL0RQI01.                                                        
016700         05  -COPY WDQ201                                                 
016800     EJECT                                                                
016900                                                                          
017000 01  FILLER                      PIC X(16) VALUE 'WDQ211-AREA'.           
017100 01  DLI-IO-AREA-WDQ211.                                                  
017200     03  WL0RQI11.                                                        
017300         05  -COPY WDQ211                                                 
017400     EJECT                                                                
017500                                                                          
017600 01  FILLER                      PIC X(16) VALUE 'WDQ212-AREA'.           
017700 01  DLI-IO-AREA-WDQ212.                                                  
017800     03  WL0RQI12.                                                        
017900         05  -COPY WDQ212                                                 
018000                                                                          
018100 01  FILLER                      PIC X(16) VALUE 'WDQ221-AREA'.           
018200 01  DLI-IO-AREA-WDQ221.                                                  
018300     03  WL0RQI21.                                                        
018400         05  -COPY WDQ221                                                 
018500     EJECT                                                                
018600                                                                          
018700 01  FILLER                      PIC X(16) VALUE 'WDB301-AREA'.           
018800 01  DLI-IO-AREA-WDB301.                                                  
018900     03  WLGMTB01.                                                        
019000         05  -COPY WDB301                                                 
019100     EJECT                                                                
019200                                                                          
019300 01  FILLER                      PIC X(16) VALUE 'WDB501-AREA'.           
019400 01  DLI-IO-AREA-WDB501.                                                  
019500     03  WLGMTC01.                                                        
019600         05  -COPY WDB501                                                 
019700     EJECT                                                                
019800                                                                          
019900 01  FILLER                      PIC X(16)   VALUE                        
020000                                                 'WDB201-AREA'.           
020100 01  DLI-IO-AREA-WDB2.                                                    
020200     03  DLI-IO-WDB201.                                                   
020300*        05  -COPY WDB201                                                 
020400                                                                          
020500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020600 01   DLI-IO-AREA-B601.                                                   
020700*     03  -COPY WDB601                                                    
020800                                                                          
020900 01  FILLER               PIC X(16)   VALUE 'WDB601 HOME'.                
021000 01   DLI-IO-AREA-B601-HOME.                                              
021100*     03  -COPY WDB601 -PRE HOME-                                         
021200                                                                          
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)   VALUE 'LINKAGE-SEC'.         
021500 LINKAGE SECTION.                                                         
021600*                                                                         
021700*   -COPY W413AVSR                                                        
021800     EJECT                                                                
021900*01  -COPY W0009      -PRE ALT-                                           
022000     EJECT                                                                
022100*01  -COPY W0008      -PRE ORQI-                                          
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400*01  -COPY W0008      -PRE GMTB-                                          
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700*01  -COPY W0008      -PRE GMTC-                                          
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000*01  -COPY W0008      -PRE WDB2-                                          
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008      -PRE WDB6-                                          
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600 01  TRAN-XXKB-PCB               PIC X.                                   
023700     EJECT                                                                
023800 PROCEDURE DIVISION  USING AVSR-W413AVSR ALT-PCB                          
023900                           ORQI-PCB GMTB-PCB GMTC-PCB                     
024000                           WDB2-PCB WDB6-PCB                              
024100                           TRAN-XXKB-PCB.                                 
024300     MOVE AVSR-IDORDER             TO W-IDORDER                           
024400     PERFORM IMS-GU-WDQ201                                                
024500     PERFORM IMS-GNP-WDQ212                                               
024600     MOVE ARB-KDROPACK        TO WS-KDROPACK                              
024800     MOVE OHUV-IDDISTR        TO W-IDDISTR                                
024900                                 DIST17-IDDISTR                           
025000                                 DIST52-IDDISTR                           
025100     MOVE OHUV-IDKUNDNR       TO W-IDKUNDNR                               
025200     PERFORM IMS-GU-WDB201                                                
025300     IF GMT-FLLDCKND = JA AND (AVSR-KDCALL NOT = +8)                      
025400        MOVE ARB-TIRFS        TO W-TIRFS                                  
025500     END-IF                                                               
025600                                                                          
025700     MOVE +1                       TO RAD                                 
025800     IF AVSR-KDCALL                 = +1 OR +3 OR +9 OR +8                
025900       PERFORM UNTIL RAD            > WS-INDEX-100-MAX                    
026000         IF AVSR-KVBEART-Q (RAD)    > ZERO                                
026100           PERFORM B-TILLAEGG-AV-RAD                                      
026200         END-IF                                                           
026300         ADD +1                    TO RAD                                 
026400       END-PERFORM                                                        
026500     ELSE                                                                 
026600        IF AVSR-KDCALL              = +2                                  
026700          PERFORM UNTIL RAD         > WS-INDEX-100-MAX                    
026800            IF AVSR-KVBEART-Q (RAD) > ZERO                                
026900              PERFORM C-MINSKNING-PAA-RAD                                 
027000            END-IF                                                        
027100            ADD +1                 TO RAD                                 
027200          END-PERFORM                                                     
027300        END-IF                                                            
027400     END-IF                                                               
027500                                                                          
027600     GOBACK                                                               
027700     .                                                                    
027800     EJECT                                                                
027900                                                                          
028000 B-TILLAEGG-AV-RAD             SECTION.                                   
028100     MOVE 'STA  B-TILLAEGG-AV-RAD   ' TO FELTEXT                          
028200                                                                          
028300     IF AVSR-IDDC(RAD) NOT = DCS-IDDC                                     
028400       MOVE AVSR-IDDC(RAD) TO W-IDDC-B6                                   
028500       PERFORM IMS-GU-WDB601                                              
028600     END-IF                                                               
028700                                                                          
028710     IF DCS-DDC                                                           
028720       MOVE WS-CDC-11          TO W-IDDC-WDB3                             
028730                                  W-IDDC-WDB3-DEF                         
028730                                  W-IDDC-WDB5                             
028730                                  W-IDDC-WDB5-DEF                         
028740     ELSE                                                                 
028750       MOVE AVSR-IDDC(RAD)     TO W-IDDC-WDB3                             
028760                                  W-IDDC-WDB3-DEF                         
028760                                  W-IDDC-WDB5                             
028760                                  W-IDDC-WDB5-DEF                         
028770     END-IF                                                               
028780     MOVE OHUV-IDDISTR         TO W-IDDISTR-WDB3                          
028790                                  W-IDDISTR-WDB3-DEF                      
028790                                  W-IDDISTR-WDB5                          
028790                                  W-IDDISTR-WDB5-DEF                      
028790                                  TEST-IDDISTR                            
028791     MOVE OHUV-IDKUNDNR        TO W-IDKUNDNR-WDB3                         
028791                                  W-IDKUNDNR-WDB5                         
028792     PERFORM IMS-GU-WDB301                                                
028793                                                                          
028800     IF AVSR-IDLEVNR (RAD)             = SPACE                            
028997                                                                          
028998       MOVE AVSR-IDDC     (RAD)  TO W-IDDC                                
029000       PERFORM IMS-GHNP-WDQ212                                            
029100       IF SEGMENT-SAKNAS                                                  
029200         PERFORM BF-PREPARE-Q212                                          
029300         PERFORM BG-NEW-Q212                                              
029400         PERFORM BA-NEW-Q221                                              
029500       ELSE                                                               
029600         IF AVSR-KDSPEEMB (RAD) > 0                                       
029700           ADD +1                TO ARB-KVSEMBRA                          
029800         END-IF                                                           
029900         IF DCS-CDC AND OHUV-FLVORFK = NEJ AND OHUV-KDORDKL = 0           
030000* WHEN FLVORFK=YES FROM SCREEN 4241, DON'T CHANGE FREIGHT CODE            
030100           PERFORM S01-CHECK-AIR-FC                                       
030200         END-IF                                                           
030300         PERFORM IMS-REPL-WDQ212                                          
030400                                                                          
030500         MOVE AVSR-ADLAGOMR (RAD)     TO W-ADLAGOMR                       
030600         PERFORM IMS-GHNP-WDQ221-GE                                       
030700         IF SEGMENT-SAKNAS                                                
030800           PERFORM BA-NEW-Q221                                            
030900         ELSE                                                             
031000           PERFORM BB-UPD-Q221                                            
031100         END-IF                                                           
031200       END-IF                                                             
031300     ELSE                                                                 
031400       MOVE AVSR-IDDC    (RAD)        TO W-IDDC-Q211                      
031500       MOVE AVSR-IDLEVNR (RAD)        TO W-IDLEVNR-Q211                   
031600       PERFORM IMS-GHNP-WDQ211                                            
031700       IF SEGMENT-FINNS                                                   
031800         PERFORM BC-REPL-DIRLEV                                           
031900       ELSE                                                               
032000         PERFORM BD-ISRT-DIRLEV                                           
032100       END-IF                                                             
032200                                                                          
032300       IF DCS-CDC OR DCS-DDC                                              
032400         IF DCS-CDC                                                       
032500           MOVE AVSR-IDDC (RAD)       TO W-IDDC                           
032600         ELSE                                                             
032700           MOVE WS-CDC-11             TO W-IDDC                           
032800         END-IF                                                           
032900         PERFORM IMS-GHNP-WDQ212                                          
033000         IF SEGMENT-SAKNAS                                                
033100           PERFORM BF-PREPARE-Q212                                        
033200           PERFORM BG-NEW-Q212                                            
033300         END-IF                                                           
033400       END-IF                                                             
033500     END-IF                                                               
033600     MOVE 'END  B-TILLAEGG-AV-RAD   ' TO FELTEXT                          
033700     .                                                                    
033800     EJECT                                                                
033900                                                                          
034000 BA-NEW-Q221      SECTION.                                                
034200     MOVE AVSR-ADLAGOMR (RAD)    TO LOR-ADLAGOMR                          
034300     MOVE SPACE                  TO LOR-IDPRC                             
034400     MOVE AVSR-KVBEART-Q (RAD)   TO LOR-KVANTART                          
034500     MOVE +1                     TO LOR-KVRADER                           
034600* SUORDV RULES                                                            
034700     IF AVSR-PRAVCOST(RAD) > 0                                            
034800        IF AVSR-IDDC (RAD) = WC-CDC-SE                                    
034900*          *BOUNCE VOR                                                    
035000           COMPUTE LOR-SUORDV   = (AVSR-KVBEART-Q (RAD) *                 
035100                                   AVSR-PRARTNTO (RAD))                   
035200        ELSE                                                              
035300*          *BOUNCE GLOBAL EXPORT OR NORMAL AVERAGE COST                   
035400           COMPUTE LOR-SUORDV   = (AVSR-KVBEART-Q (RAD) *                 
035500                                   AVSR-PRAVCOST (RAD))                   
035600        END-IF                                                            
035700     ELSE                                                                 
035800         COMPUTE LOR-SUORDV     = (AVSR-KVBEART-Q (RAD) *                 
035900                                   AVSR-PRARTNTO (RAD))                   
036000     END-IF                                                               
036010* WEIGHT                                                                  
036020     COMPUTE WS-VKORDNTO        = (AVSR-KVBEART-Q (RAD) *                 
036200                                  (AVSR-VKART     (RAD) / 1000))          
036210     MOVE    WS-VKORDNTO        TO LOR-VKORDNTO                           
036220* VOLUME                                                                  
036300     COMPUTE WS-VLORDNTO        = (AVSR-KVBEART-Q (RAD) *                 
036400                                 (AVSR-VLARTNTO  (RAD) / 1000000))        
036410     MOVE    WS-VLORDNTO        TO LOR-VLORDNTO                           
036420*                                                                         
036500     COMPUTE LOR-SUORDV-LOC     = (AVSR-KVBEART-Q (RAD) *                 
036600                                   AVSR-PRARTNTO-LOC (RAD))               
036700     COMPUTE LOR-SUORDV-LOCPREL = (AVSR-KVBEART-Q (RAD) *                 
036800                                   AVSR-PRARTNTO-LOCPREL (RAD))           
036900     MOVE AVSR-KDVALISO (RAD)   TO  LOR-KDVALISO                          
037000     PERFORM IMS-ISRT-WDQ221                                              
037100     .                                                                    
037200     EJECT                                                                
037300                                                                          
037400 BB-UPD-Q221   SECTION.                                                   
037600     ADD AVSR-KVBEART-Q (RAD)     TO LOR-KVANTART                         
037700     ADD +1                       TO LOR-KVRADER                          
037800                                                                          
037900     IF AVSR-PRAVCOST(RAD) > 0                                            
038000        IF AVSR-IDDC (RAD) = WC-CDC-SE                                    
038100*          *BOUNCE VOR                                                    
038200           COMPUTE LOR-SUORDV = LOR-SUORDV +                              
038300                   (AVSR-KVBEART-Q (RAD) * AVSR-PRARTNTO (RAD))           
038400        ELSE                                                              
038500*          *BOUNCE GLOBAL EXPORT OR NORMAL AVERAGE COST                   
038600           COMPUTE LOR-SUORDV = LOR-SUORDV +                              
038700                  (AVSR-KVBEART-Q (RAD) * AVSR-PRAVCOST (RAD))            
038800        END-IF                                                            
038900     ELSE                                                                 
039000       COMPUTE LOR-SUORDV = LOR-SUORDV +                                  
039100              (AVSR-KVBEART-Q (RAD) * AVSR-PRARTNTO (RAD) )               
039200     END-IF                                                               
039300* WEIGHT                                                                  
039400     COMPUTE  WS-VKORDNTO =                                               
039500             (AVSR-KVBEART-Q (RAD) * (AVSR-VKART (RAD) / 1000))           
039510     ADD      WS-VKORDNTO TO LOR-VKORDNTO                                 
039600                                                                          
039610* VOLUME                                                                  
039700     COMPUTE  WS-VLORDNTO =                                               
039800             (AVSR-KVBEART-Q (RAD) *                                      
039900             (AVSR-VLARTNTO  (RAD) / 1000000))                            
039910     ADD      WS-VLORDNTO TO LOR-VLORDNTO                                 
040100     COMPUTE LOR-SUORDV-LOC = LOR-SUORDV-LOC +                            
040200            (AVSR-KVBEART-Q (RAD) * AVSR-PRARTNTO-LOC (RAD))              
040300                                                                          
040400     COMPUTE LOR-SUORDV-LOCPREL = LOR-SUORDV-LOCPREL +                    
040500            (AVSR-KVBEART-Q (RAD) * AVSR-PRARTNTO-LOCPREL (RAD))          
040600                                                                          
040700     MOVE AVSR-KDVALISO (RAD) TO LOR-KDVALISO                             
040800     PERFORM IMS-REPL-WDQ221                                              
040900     .                                                                    
041000     EJECT                                                                
041100                                                                          
041200 BC-REPL-DIRLEV                SECTION.                                   
041300     MOVE 'STA  BC-REPL-DIRLEV      ' TO FELTEXT                          
041400                                                                          
041500     ADD +1                       TO DIRL-KVRADER                         
041600                                                                          
041700     COMPUTE DIRL-SUORDV-LOC      =                                       
041800             DIRL-SUORDV-LOC      +                                       
041900             (AVSR-KVBEART-Q (RAD) *                                      
042000             AVSR-PRARTNTO-LOC (RAD))                                     
042100                                                                          
042200     COMPUTE DIRL-SUORDV-LOCPREL  =                                       
042300              DIRL-SUORDV-LOCPREL  +                                      
042400              (AVSR-KVBEART-Q (RAD) *                                     
042500              AVSR-PRARTNTO-LOCPREL (RAD))                                
042600                                                                          
042700     MOVE AVSR-KDVALISO (RAD)  TO DIRL-KDVALISO                           
042800                                                                          
042900     COMPUTE DIRL-SUORDV         =                                        
043000              DIRL-SUORDV         +                                       
043100             (AVSR-KVBEART-Q (RAD) *                                      
043200              AVSR-PRARTNTO (RAD))                                        
043300                                                                          
043400     COMPUTE WS-VKORDNTO           =                                      
043500            (AVSR-KVBEART-Q (RAD)  *                                      
043600            (AVSR-VKART     (RAD)  / 1000))                               
043700         ADD WS-VKORDNTO          TO DIRL-VKORDNTO                        
043800                                                                          
043900     COMPUTE WS-VLORDNTO           =                                      
044000            (AVSR-KVBEART-Q (RAD)  *                                      
044100            (AVSR-VLARTNTO  (RAD)  / 1000000))                            
044200         ADD WS-VLORDNTO          TO DIRL-VLORDNTO                        
044300     PERFORM IMS-REPL-WDQ211                                              
044400     MOVE 'END  BC-REPL-DIRLEV      ' TO FELTEXT                          
044500     .                                                                    
044600     EJECT                                                                
044700                                                                          
044800 BD-ISRT-DIRLEV                SECTION.                                   
044900     MOVE 'STA  BD-ISRT-DIRLEV      ' TO FELTEXT                          
045000                                                                          
045100     MOVE AVSR-KDORDSTA     (RAD) TO DIRL-KDORDSTA                        
045200     MOVE AVSR-KDVIA        (RAD) TO DIRL-KDVIA                           
045300     MOVE AVSR-KVDAGAR-DIFF (RAD) TO DIRL-KVDAGAR-DIFF                    
045400     MOVE AVSR-TISKEPPN-DDC (RAD) TO DIRL-TISKEPPN-DDC                    
045500                                                                          
045600     MOVE AVSR-IDDC         (RAD) TO DIRL-IDDC                            
045700     MOVE AVSR-IDLEVNR      (RAD) TO DIRL-IDLEVNR                         
045800     MOVE +1                      TO DIRL-KVRADER                         
045900                                                                          
046000     COMPUTE DIRL-SUORDV-LOC      =                                       
046100             (AVSR-KVBEART-Q (RAD) *                                      
046200              AVSR-PRARTNTO-LOC (RAD))                                    
046300                                                                          
046400     COMPUTE DIRL-SUORDV-LOCPREL  =                                       
046500              (AVSR-KVBEART-Q (RAD) *                                     
046600               AVSR-PRARTNTO-LOCPREL (RAD))                               
046700                                                                          
046800     MOVE AVSR-KDVALISO (RAD)  TO DIRL-KDVALISO                           
046900                                                                          
047000     COMPUTE DIRL-SUORDV          =                                       
047100             (AVSR-KVBEART-Q (RAD) *                                      
047200              AVSR-PRARTNTO (RAD))                                        
047300                                                                          
047400     COMPUTE WS-VKORDNTO           =                                      
047500            (AVSR-KVBEART-Q (RAD)  *                                      
047600            (AVSR-VKART     (RAD)  / 1000))                               
047700        MOVE WS-VKORDNTO          TO DIRL-VKORDNTO                        
047800                                                                          
047900     COMPUTE WS-VLORDNTO           =                                      
048000            (AVSR-KVBEART-Q (RAD)  *                                      
048100            (AVSR-VLARTNTO  (RAD)  / 1000000))                            
048200        MOVE WS-VLORDNTO          TO DIRL-VLORDNTO                        
048300     PERFORM IMS-ISRT-WDQ211                                              
048400     MOVE 'END  BD-ISRT-DIRLEV      ' TO FELTEXT                          
048500     .                                                                    
048600     EJECT                                                                
048700                                                                          
048800 BF-PREPARE-Q212 SECTION.                                                 
048900     MOVE 'STA BF-PREPARE-Q212      ' TO FELTEXT                          
049000                                                                          
051000     MOVE OHUV-IDDC-PRIM       TO W-IDDC-B6                               
051100     PERFORM IMS-GU-WDB601-HOME                                           
051300     IF DCS-IDLANDX2 = 'CN' AND DIST15-KINA                               
051400       MOVE   AVSR-KDFRAKT     TO  W-KDFRAKT-WDB5                         
051500                                   W-KDFRAKT-WDB5-DEF                     
051600       IF OHUV-KDORDKL  >  1                                              
051700          MOVE DC-KDROPACK-BULK TO  WS-KDROPACK                           
051800       ELSE                                                               
051900          MOVE DC-KDROPACK-DAG  TO  WS-KDROPACK                           
052000       END-IF                                                             
052100     ELSE                                                                 
052200       IF (DIST15-NA      AND (AVSR-KDFRAKT > +11 AND < +20)) OR          
052300          (DIST15-ENGLAND AND OHUV-KDORDKL = +1                           
052400                          AND AVSR-KDFRAKT = +16) OR                      
052500          (DIST15-JAPAN   AND OHUV-KDORDKL = +1                           
052600                          AND (AVSR-KDFRAKT = +15 OR +25)) OR             
052700          (DIST15-AUSTRALIEN AND OHUV-KDORDKL = +1                        
052800                          AND AVSR-KDFRAKT = +5)  OR                      
052900          (DIST15-KINA    AND OHUV-KDORDKL = +1                           
053000                          AND AVSR-KDFRAKT = +17)                         
053100         MOVE AVSR-KDFRAKT     TO W-KDFRAKT-WDB5                          
053200                                  W-KDFRAKT-WDB5-DEF                      
053300         MOVE '0'              TO WS-KDROPACK                             
053400                                  AVSR-KDROPACK                           
053500       ELSE                                                               
053600         IF OHUV-KDORDKL = 0                                              
053700            MOVE DC-KDGENFRA-VOR  TO  W-KDFRAKT-WDB5                      
053800                                      W-KDFRAKT-WDB5-DEF                  
053900            IF OHUV-FLVORFK = JA                                          
054000              MOVE DC-KDGENFRA-DO TO  W-KDFRAKT-WDB5                      
054100                                      W-KDFRAKT-WDB5-DEF                  
054200            ELSE                                                          
054300              IF DCS-CDC                                                  
054400                PERFORM S01-CHECK-AIR-FC                                  
054500              END-IF                                                      
054600            END-IF                                                        
054700            IF WS-KDROPACK NOT = '3'                                      
054800                MOVE DC-KDROPACK-DAG  TO  WS-KDROPACK                     
054900            END-IF                                                        
055000         END-IF                                                           
055100         IF OHUV-KDORDKL = 1                                              
055200            MOVE DC-KDGENFRA-DO   TO  W-KDFRAKT-WDB5                      
055300                                      W-KDFRAKT-WDB5-DEF                  
055400            IF WS-KDROPACK NOT = '3'                                      
055500              MOVE DC-KDROPACK-DAG TO WS-KDROPACK                         
055600            END-IF                                                        
055700         END-IF                                                           
055800         IF OHUV-KDORDKL > 1                                              
055900            MOVE DC-KDGENFRA-MO   TO  W-KDFRAKT-WDB5                      
056000                                      W-KDFRAKT-WDB5-DEF                  
056100            IF WS-KDROPACK NOT = '3'                                      
056200              MOVE DC-KDROPACK-BULK TO WS-KDROPACK                        
056300            END-IF                                                        
056400         END-IF                                                           
056500       END-IF                                                             
056600     END-IF                                                               
056700                                                                          
056800     PERFORM IMS-GU-WDB501                                                
056900     IF OHUV-KDORDKL = 0                                                  
057000       MOVE FK-IDTRP-0         TO ARB-IDTRP                               
057100     END-IF                                                               
057200     IF OHUV-KDORDKL = 1                                                  
057300       MOVE FK-IDTRP-1         TO ARB-IDTRP                               
057400     END-IF                                                               
057500     IF OHUV-KDORDKL = 2                                                  
057600       MOVE FK-IDTRP-2         TO ARB-IDTRP                               
057700     END-IF                                                               
057800     IF OHUV-KDORDKL = 3                                                  
057900       MOVE FK-IDTRP-3         TO ARB-IDTRP                               
058000     END-IF                                                               
058100     IF OHUV-KDORDKL = 4                                                  
058200       MOVE FK-IDTRP-4         TO ARB-IDTRP                               
058300     END-IF                                                               
058400                                                                          
058500     PERFORM BFA-HAMTA-AVGTID                                             
058600                                                                          
058700     MOVE 'END BF-PREPARE-Q212      ' TO FELTEXT                          
058800     .                                                                    
058900     EJECT                                                                
059000 BFA-HAMTA-AVGTID SECTION.                                                
059100     MOVE 'STA BFA-HAMTA-AVGTID     ' TO FELTEXT                          
059200                                                                          
059300     IF AVSR-KDCALL = +3                                                  
059400       MOVE 'LDC '             TO TRAN-IDSYSTEM                           
059500     ELSE                                                                 
059600       IF AVSR-KDCALL = +9                                                
059700         MOVE 'LDCR'           TO TRAN-IDSYSTEM                           
059800       ELSE                                                               
059900         MOVE 'IMS '           TO TRAN-IDSYSTEM                           
060000       END-IF                                                             
060100     END-IF                                                               
060200     MOVE ARB-IDTRP            TO TRAN-IDTRP                              
060300     IF DCS-DDC                                                           
060400       MOVE WS-CDC-11          TO TRAN-IDDC                               
060500     ELSE                                                                 
060600       MOVE AVSR-IDDC(RAD)     TO TRAN-IDDC                               
060700     END-IF                                                               
060800     MOVE OHUV-KDORDKL         TO TRAN-KDORDKL                            
060900     MOVE FK-KDTRPKAT          TO TRAN-KDTRPKAT                           
061000     MOVE DC-KVLEDTIM-0        TO TRAN-KVLEDTIM-0                         
061100     MOVE DC-KVLEDTIM-1        TO TRAN-KVLEDTIM-1                         
061200     MOVE DC-KVLEDTIM-2        TO TRAN-KVLEDTIM-2                         
061300     MOVE DC-KVLEDTIM-3        TO TRAN-KVLEDTIM-3                         
061400     MOVE DC-KVLEDTIM-4        TO TRAN-KVLEDTIM-4                         
061500     MOVE NEJ                  TO TRAN-FLORDSPE                           
061600     MOVE NEJ                  TO TRAN-FLOVRLEV                           
061700     MOVE AVSR-TIREGDAT        TO TRAN-TIREGDAT                           
061800     PERFORM BFAA-EV-AENDRA-TID                                           
061900     MOVE WS-TIHHMM            TO TRAN-TIHHMM-REG                         
062000     IF (OHUV-IDSYSTEM = 'LDC ' OR 'LYNK' OR 'ECOM' OR 'VOUI' OR          
062100                         'TAD ' OR 'ACC ' OR 'APA ' OR 'APB ' OR          
062110                         'APC ' OR 'APD ' OR 'APE ' OR 'APF ' OR          
062120                         'APG ' OR 'APH ' OR 'API ' OR 'APJ ' )           
062130                         AND                                              
062200        OHUV-TIREPDAT > ZERO                                              
062300       PERFORM BFAB-CALC-RFS                                              
062400     ELSE                                                                 
062500       IF OHUV-IDSYSTEM = ('LDC ' OR 'TACD') AND                          
062600          AVSR-TISKEPPN-DDC(RAD) > +0                                     
062700         MOVE AVSR-TISKEPPN-DDC(RAD) TO WS-TIRFS-DAT                      
062800         MOVE ZERO             TO WS-TIRFS-TID                            
062900         MOVE WS-TIRFS         TO TRAN-TIRFS                              
063000       ELSE                                                               
063100         IF GMT-FLLDCKND = JA                                             
063200          IF AVSR-KDCALL NOT = +8                                         
063300           MOVE W-TIRFS        TO TRAN-TIRFS                              
063400          ELSE                                                            
063500           MOVE ZERO           TO TRAN-TIRFS                              
063600          END-IF                                                          
063700         ELSE                                                             
063800           MOVE +0               TO TRAN-TIRFS                            
063900         END-IF                                                           
064000       END-IF                                                             
064100     END-IF                                                               
064200     MOVE +0                   TO TRAN-KDTPOTYP                           
064300                                                                          
064400     CALL W411TRAN USING TRAN-W411TRAN TRAN-XXKB-PCB                      
064500     IF TRAN-KDSVAR = '1'                                                 
064600        MOVE 'FEL FRÅN SUBPROGRAM W411TRAN KDSVAR 1 ' TO FELTEXT          
064700        CALL ABEND USING RKOD-ABEND                                       
064800     ELSE                                                                 
064900       IF TRAN-KDSVAR = '2' OR '3' OR '4'                                 
065000         MOVE 'FEL FRÅN SUBPROGRAM W411TRAN KDSVAR > 1' TO FELTEXT        
065100         CALL ABEND USING RKOD-ABEND                                      
065200       END-IF                                                             
065300     END-IF                                                               
065400     MOVE 'STA BFA-HAMTA-AVGTID     ' TO FELTEXT                          
065500     .                                                                    
065600     EJECT                                                                
065700                                                                          
065800 BFAA-EV-AENDRA-TID SECTION.                                              
065900     MOVE 'STA BFAA-EV-AENDRA-TID   ' TO FELTEXT                          
066000                                                                          
066100******* VID CLEARING JUSTERAR MAN LOKAL TID    **************             
066200******* OM DET ÄR OLIKA TIDZONER               **************             
066300                                                                          
066400     MOVE AVSR-TIHHMM          TO WS-TIHHMM                               
066500     IF (DCS-CDC AND (HOME-DCS-SDC AND HOME-DCS-IDLANDX2 = 'GB'))         
066600     OR (DCS-NDC-NA AND HOME-DCS-NDC-NA)                                  
066700                                                                          
066800        COMPUTE WS-TIHH = WS-TIHH                                         
066900        + (HOME-DCS-IDTIDZON - DCS-IDTIDZON)                              
067000                                                                          
067100     END-IF                                                               
067200     MOVE 'END BFAA-EV-AENDRA-TID   ' TO FELTEXT                          
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067600 BFAB-CALC-RFS SECTION.                                                   
067700     MOVE 'STA BFAB-CALC-RFS   ' TO FELTEXT                               
067800                                                                          
067900     MOVE W-IDDC                 TO WORK-IDDC                             
068000     MOVE +002                   TO WORK-KDCALL                           
068100     MOVE +001                   TO WORK-KVWORKD                          
068200     MOVE OHUV-TIREPDAT          TO WORK-TIAAMMDD-FOM                     
068300                                                                          
068400     CALL WORKDAY             USING WORK-KDCALL                           
068500                                    WORK-DATE-AREA                        
068600                                    WORK-KDSVAR                           
068700                                                                          
068800     IF WORK-KDSVAR-FEL                                                   
068900       MOVE 'BFAB-CALC-RFS 1, DATUM SAKNAS I WORKDAY'                     
069000                                 TO FELTEXT                               
069100       CALL ABEND             USING RKOD-ABEND-UTAN-DUMP                  
069200     ELSE                                                                 
069300       MOVE +003                 TO WORK-KDCALL                           
069400       MOVE GMT-KVDAGAR-RFS-DEF  TO WORK-KVWORKD                          
069500       PERFORM                                                            
069600       VARYING RFS-IX FROM 1 BY 1                                         
069700         UNTIL RFS-IX > MAX-RFS-IX                                        
069800         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
069900           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
070000                                 TO WORK-KVWORKD                          
070100         END-IF                                                           
070200       END-PERFORM                                                        
070300       ADD +1                    TO WORK-KVWORKD                          
070400*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
070500*      ANTAL DAGAR FÖRE RFS.                                              
070600*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
070700*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
070800*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
070900*                                                                         
071000       CALL WORKDAY           USING WORK-KDCALL                           
071100                                    WORK-DATE-AREA                        
071200                                    WORK-KDSVAR                           
071300       IF WORK-KDSVAR-FEL                                                 
071400         MOVE 'BFAB-CALC-RFS 2, DATUM SAKNAS I WORKDAY'                   
071500                                 TO FELTEXT                               
071600         CALL ABEND           USING RKOD-ABEND-UTAN-DUMP                  
071700       ELSE                                                               
071800         IF WORK-TIAAMMDD-FOM < AVSR-TIREGDAT                             
071900           MOVE WC-CDC-SE        TO WORK-IDDC                             
072000           MOVE +002             TO WORK-KDCALL                           
072100           MOVE +001             TO WORK-KVWORKD                          
072200           MOVE AVSR-TIREGDAT    TO WORK-TIAAMMDD-FOM                     
072300           CALL WORKDAY       USING WORK-KDCALL                           
072400                                    WORK-DATE-AREA                        
072500                                    WORK-KDSVAR                           
072600           IF WORK-KDSVAR-FEL                                             
072700             MOVE 'BFAB-CAL-RFS 3, DATUM SAKNAS I WORKDAY'                
072800                                 TO FELTEXT                               
072900             CALL ABEND       USING RKOD-ABEND-UTAN-DUMP                  
073000           ELSE                                                           
073100             MOVE WORK-TIAAMMDD-TOM                                       
073200                                 TO WS-TIRFS-DAT                          
073300           END-IF                                                         
073400         ELSE                                                             
073500           MOVE WORK-TIAAMMDD-FOM                                         
073600                                 TO WS-TIRFS-DAT                          
073700         END-IF                                                           
073800       END-IF                                                             
073900     END-IF                                                               
074000                                                                          
074100     MOVE ZERO                   TO WS-TIRFS-TID                          
074200     MOVE WS-TIRFS               TO TRAN-TIRFS                            
074300     .                                                                    
074400     EJECT                                                                
074500                                                                          
074600 BG-NEW-Q212       SECTION.                                               
074700     MOVE 'STA BG-NEW-Q212      ' TO FELTEXT                              
074800                                                                          
074900     MOVE DC-IDDC              TO ARB-IDDC                                
075000     MOVE FK-BEGMRK            TO ARB-BEGMRK                              
075100     MOVE NEJ                  TO ARB-FLODELUT                            
075200     MOVE +0                   TO ARB-IDRADNR-SISTA                       
075300     MOVE +0                   TO ARB-IDPLKLST-SISTA                      
075400     MOVE FK-KDFRAKT           TO ARB-KDFRAKT                             
075500     MOVE FK-KDFDKRAV          TO ARB-KDFDKRAV                            
075600**IF REPDATE ORDER AND COMES FROM UI THEN KDROPACK SHOULD BE 0            
075700     IF OHUV-FLRESTN  =  NEJ      OR                                      
075800        OHUV-IDSYSTEM = ('LDC ' ) OR ('LYNK'  OR                          
075900              'ECOM'   OR  'VOUI' OR 'TAD ' OR 'ACC ' OR                  
075910                           'APA ' OR 'APB ' OR 'APC'  OR                  
075920                           'APD ' OR 'APE ' OR 'APF ' OR                  
075930                           'APG ' OR 'APH ' OR 'API ' OR 'APJ ')          
075940                          AND                                             
076000                        (OHUV-TIREPDAT > ZERO )                           
076100       MOVE '0'                TO ARB-KDROPACK                            
076200     ELSE                                                                 
076300       MOVE WS-KDROPACK        TO ARB-KDROPACK                            
076400     END-IF                                                               
076500     MOVE FK-KDTRPKAT          TO ARB-KDTRPKAT                            
076600     MOVE +0                   TO ARB-KVSEMBRA                            
076700     MOVE TRAN-TIRFS           TO ARB-TIRFS                               
076800     MOVE TRAN-TIAAMMDD        TO ARB-DATRPAVD                            
076900     IF TRAN-TIAAMMDD NOT = ZERO                                          
077000       IF TRAN-TIAAMMDD < 500000                                          
077100         MOVE 20               TO ARB-DATRPAVD (1:2)                      
077200       ELSE                                                               
077300         IF TRAN-TIAAMMDD < 999999                                        
077400           MOVE 19             TO ARB-DATRPAVD (1:2)                      
077500         ELSE                                                             
077600           MOVE 99999999       TO ARB-DATRPAVD                            
077700         END-IF                                                           
077800       END-IF                                                             
077900     END-IF                                                               
078000     MOVE TRAN-TIHHMM          TO ARB-TIHHMM                              
078100                                                                          
078200********* HÄMTA ALTERNATIV TRANSPORT *************                        
078300                                                                          
078400     IF DCS-NDC-NA AND AVSR-KDFRAKT = +11                                 
078500       ADD +1                  TO W-KDFRAKT-WDB5                          
078600       ADD +1                  TO W-KDFRAKT-WDB5-DEF                      
078700       PERFORM IMS-GU-WDB501                                              
078800       IF SEGMENT-FINNS                                                   
078900         IF OHUV-KDORDKL = 0                                              
079000           MOVE FK-IDTRP-0     TO ARB-IDTRP-ALT                           
079100         END-IF                                                           
079200         IF OHUV-KDORDKL = 1                                              
079300           MOVE FK-IDTRP-1     TO ARB-IDTRP-ALT                           
079400         END-IF                                                           
079500         IF OHUV-KDORDKL = 2                                              
079600           MOVE FK-IDTRP-2     TO ARB-IDTRP-ALT                           
079700         END-IF                                                           
079800         IF OHUV-KDORDKL = 3                                              
079900           MOVE FK-IDTRP-3     TO ARB-IDTRP-ALT                           
080000         END-IF                                                           
080100         IF OHUV-KDORDKL = 4                                              
080200           MOVE FK-IDTRP-4     TO ARB-IDTRP-ALT                           
080300         END-IF                                                           
080400       ELSE                                                               
080500         MOVE SPACE            TO ARB-IDTRP-ALT                           
080300       END-IF                                                             
080700     ELSE                                                                 
080800       MOVE SPACE              TO ARB-IDTRP-ALT                           
080900     END-IF                                                               
081000     MOVE 'E'                  TO ARB-KDORDSTA                            
081100     MOVE SPACE                TO ARB-KDORDSTA-O                          
081200                                                                          
081300     PERFORM IMS-ISRT-WDQ212                                              
081400     .                                                                    
081500     EJECT                                                                
081600                                                                          
081700 C-MINSKNING-PAA-RAD           SECTION.                                   
081800                                                                          
081900     IF AVSR-IDLEVNR (RAD)         = SPACE                                
082000       PERFORM CA-MINSKA-Q212-21                                          
082100       IF LOR-KVRADER           = 0                                       
082200         PERFORM IMS-DLET-WDQ221                                          
082300       ELSE                                                               
082400         PERFORM IMS-REPL-WDQ221                                          
082500       END-IF                                                             
082600     ELSE                                                                 
082700       MOVE AVSR-IDDC    (RAD)     TO W-IDDC-Q211                         
082800       MOVE AVSR-IDLEVNR (RAD)     TO W-IDLEVNR-Q211                      
082900       PERFORM IMS-GHNP-WDQ211                                            
083000       IF SEGMENT-FINNS                                                   
083100         PERFORM CB-MINSKA-DIRLEV                                         
083200         IF DIRL-KVRADER           = 0                                    
083300           PERFORM IMS-DLET-WDQ211                                        
083400         ELSE                                                             
083500           PERFORM IMS-REPL-WDQ211                                        
083600         END-IF                                                           
083700       END-IF                                                             
083800     END-IF                                                               
083900     .                                                                    
084000     EJECT                                                                
084100                                                                          
084200 CA-MINSKA-Q212-21            SECTION.                                    
084300                                                                          
084400     MOVE AVSR-IDDC     (RAD)    TO W-IDDC                                
084500     PERFORM IMS-GHNP-WDQ212                                              
084600                                                                          
084700     IF AVSR-KVBEART-Q (RAD)          = AVSR-KVANNANT (RAD)               
084800*****> ALL LINE QTY DELETED                                               
084900       IF AVSR-KDSPEEMB (RAD)         > 0                                 
085000         COMPUTE ARB-KVSEMBRA         =                                   
085100                 ARB-KVSEMBRA         - 1                                 
085200         PERFORM IMS-REPL-WDQ212                                          
085300       END-IF                                                             
085400     END-IF                                                               
085500                                                                          
085600     MOVE AVSR-ADLAGOMR (RAD)    TO W-ADLAGOMR                            
085700     PERFORM IMS-GHNP-WDQ221                                              
085800                                                                          
085900     IF AVSR-KVBEART-Q (RAD)          = AVSR-KVANNANT (RAD)               
086000*****> ALL LINE QTY DELETED                                               
086100       SUBTRACT 1                  FROM LOR-KVRADER                       
086200     END-IF                                                               
086300                                                                          
086400     SUBTRACT AVSR-KVANNANT (RAD) FROM LOR-KVANTART                       
086500                                                                          
086510     COMPUTE LOR-SUORDV-LOC = LOR-SUORDV-LOC -                            
086600             (AVSR-KVANNANT (RAD) * AVSR-PRARTNTO-LOC (RAD))              
086800                                                                          
086810     COMPUTE LOR-SUORDV-LOCPREL = LOR-SUORDV-LOCPREL -                    
086900             (AVSR-KVANNANT (RAD) * AVSR-PRARTNTO-LOCPREL (RAD))          
087100                                                                          
087200     MOVE AVSR-KDVALISO(RAD)         TO LOR-KDVALISO                      
087300                                                                          
087400     IF AVSR-PRAVCOST(RAD) > 0                                            
087500        IF AVSR-IDDC (RAD) = WC-CDC-SE                                    
087600*        *BOUNCE VOR                                                      
087610          COMPUTE LOR-SUORDV = LOR-SUORDV -                               
087700                  (AVSR-KVANNANT (RAD) * AVSR-PRARTNTO (RAD))             
087900        ELSE                                                              
088000*          *BOUNCE GLOBAL EXPORT OR NORMAL AVERAGE COST                   
088010          COMPUTE LOR-SUORDV = LOR-SUORDV -                               
088100                  (AVSR-KVANNANT (RAD) * AVSR-PRAVCOST (RAD))             
088300        END-IF                                                            
088400     ELSE                                                                 
088410        COMPUTE LOR-SUORDV = LOR-SUORDV -                                 
088500                (AVSR-KVANNANT (RAD) * AVSR-PRARTNTO (RAD))               
088700     END-IF                                                               
088800                                                                          
088900     COMPUTE WS-VKORDNTO =                                                
089000             (AVSR-KVANNANT  (RAD) *                                      
089100             (AVSR-VKART     (RAD) / 1000))                               
089200     SUBTRACT WS-VKORDNTO          FROM LOR-VKORDNTO                      
089300                                                                          
089400     COMPUTE  WS-VLORDNTO =                                               
089500             (AVSR-KVANNANT  (RAD) *                                      
089600             (AVSR-VLARTNTO  (RAD) / 1000000))                            
089700     SUBTRACT WS-VLORDNTO          FROM LOR-VLORDNTO                      
089800     .                                                                    
089900     EJECT                                                                
090000 CB-MINSKA-DIRLEV              SECTION.                                   
090100     MOVE 'STA CB-MINSKA-DIRLEV     ' TO FELTEXT                          
090200                                                                          
090300     IF AVSR-KVBEART-Q (RAD)       = AVSR-KVANNANT (RAD)                  
090400       COMPUTE DIRL-KVRADER        =                                      
090500               DIRL-KVRADER        - 1                                    
090600     END-IF                                                               
090700                                                                          
090800     COMPUTE DIRL-SUORDV-LOC       =                                      
090900                 DIRL-SUORDV-LOC       -                                  
091000                (AVSR-KVANNANT  (RAD)  *                                  
091100                 AVSR-PRARTNTO-LOC (RAD))                                 
091200                                                                          
091300     COMPUTE DIRL-SUORDV-LOCPREL =                                        
091400                 DIRL-SUORDV-LOCPREL -                                    
091500                (AVSR-KVANNANT (RAD) *                                    
091600                 AVSR-PRARTNTO-LOCPREL (RAD))                             
091700                                                                          
091800     MOVE AVSR-KDVALISO(RAD)   TO DIRL-KDVALISO                           
091900                                                                          
092000     COMPUTE DIRL-SUORDV             =                                    
092100               DIRL-SUORDV             -                                  
092200              (AVSR-KVANNANT (RAD)     *                                  
092300               AVSR-PRARTNTO (RAD))                                       
092400                                                                          
092500     COMPUTE DIRL-VKORDNTO         =                                      
092600             DIRL-VKORDNTO         -                                      
092700            (AVSR-KVANNANT  (RAD)  *                                      
092800            (AVSR-VKART     (RAD)  / 1000))                               
092900                                                                          
093000     COMPUTE DIRL-VLORDNTO         =                                      
093100             DIRL-VLORDNTO         -                                      
093200            (AVSR-KVANNANT  (RAD)  *                                      
093300            (AVSR-VLARTNTO  (RAD)  / 1000000))                            
093400     MOVE 'END CB-MINSKA-DIRLEV     ' TO FELTEXT                          
093500     .                                                                    
093600     EJECT                                                                
093700                                                                          
093800 S01-CHECK-AIR-FC SECTION.                                                
093900                                                                          
094000     IF DIST17-CDC-VOR-FC                                                 
094100       IF AVSR-VKART     (RAD) > 20000 OR                                 
094200          AVSR-VLARTNTO  (RAD) > 100000 OR                                
094300          AVSR-KDVSOP    (RAD) > 220 OR                                   
094400          AVSR-KDFARLIG  (RAD) = 4 OR 6                                   
094500         MOVE DC-KDGENFRA-VOR         TO ARB-KDFRAKT                      
094600       ELSE                                                               
094700         IF DIST52-ENGLAND                                                
094800           SET FRAK01-KDFRAKT51       TO TRUE                             
094900           MOVE FRAK01-KDFRAKT        TO ARB-KDFRAKT                      
095000         ELSE                                                             
095100           IF DIST52-IRLAND                                               
095200             SET FRAK01-KDFRAKT50     TO TRUE                             
095300             MOVE FRAK01-KDFRAKT      TO ARB-KDFRAKT                      
095400           ELSE                                                           
095500             SET FRAK01-FLYG          TO TRUE                             
095600             MOVE FRAK01-KDFRAKT      TO ARB-KDFRAKT                      
095700           END-IF                                                         
095800         END-IF                                                           
096500       END-IF                                                             
095900       MOVE ARB-KDFRAKT               TO W-KDFRAKT-WDB5                   
096000                                         W-KDFRAKT-WDB5-DEF               
096100       PERFORM IMS-GU-WDB501                                              
096200       IF SEGMENT-FINNS                                                   
096300          MOVE FK-IDTRP-0             TO ARB-IDTRP                        
096400       END-IF                                                             
096600     END-IF                                                               
096700     .                                                                    
096800     EJECT                                                                
096900*                                                                         
097000 IMS-GU-WDQ201                 SECTION.                                   
097100                                                                          
097200     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
097300            DELIMITED BY SIZE INTO SSA1                                   
097400     MOVE '    '              TO GODK-STATUSKODER                         
097500     CALL CBLTDLI USING GU  ORQI-PCB DLI-IO-AREA-WDQ201 SSA1              
097600     MOVE ORQI-STATUS-CODE      TO STATUS-WS                              
097700     PERFORM IMS-STATUSKONTROLL                                           
097800     .                                                                    
097900     EJECT                                                                
098000 IMS-GHNP-WDQ211               SECTION.                                   
098100                                                                          
098200     STRING 'WLORQI11*F(WDQ211KY =' W-WDQ211KY-X ')'                      
098300            DELIMITED BY SIZE INTO SSA1                                   
098400     MOVE '  GE'               TO GODK-STATUSKODER                        
098500     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-WDQ211 SSA1             
098600     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
098700     PERFORM IMS-STATUSKONTROLL                                           
098800     .                                                                    
098900     SKIP2                                                                
099000 IMS-ISRT-WDQ211               SECTION.                                   
099100                                                                          
099200     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
099300          DELIMITED BY SIZE INTO SSA1                                     
099400     MOVE   'WLORQI11 '       TO SSA2                                     
099500     MOVE   '    '            TO GODK-STATUSKODER                         
099600     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-WDQ211 SSA1 SSA2        
099700     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
099800     PERFORM IMS-STATUSKONTROLL                                           
099900     .                                                                    
100000     EJECT                                                                
100100 IMS-ISRT-WDQ212               SECTION.                                   
100200                                                                          
100300     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
100400          DELIMITED BY SIZE INTO SSA1                                     
100500     MOVE   'WLORQI12 '       TO SSA2                                     
100600     MOVE   '    '            TO GODK-STATUSKODER                         
100700     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-WDQ212 SSA1 SSA2        
100800     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
100900     PERFORM IMS-STATUSKONTROLL                                           
101000     .                                                                    
101100     EJECT                                                                
101200 IMS-ISRT-WDQ221               SECTION.                                   
101300                                                                          
101400     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
101500          DELIMITED BY SIZE INTO SSA1                                     
101600     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
101700          DELIMITED BY SIZE INTO SSA2                                     
101800     MOVE   'WLORQI21 '       TO SSA3                                     
101900     MOVE   '    '            TO GODK-STATUSKODER                         
102000     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-WDQ221 SSA1 SSA2        
102100                                                         SSA3             
102200     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
102300     PERFORM IMS-STATUSKONTROLL                                           
102400     .                                                                    
102500     EJECT                                                                
102600 IMS-GNP-WDQ212               SECTION.                                    
102700                                                                          
102800     MOVE  'WLORQI12'          TO SSA1                                    
102900     MOVE '  GE'               TO GODK-STATUSKODER                        
103000     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-WDQ212 SSA1              
103100     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
103200     PERFORM IMS-STATUSKONTROLL                                           
103300     .                                                                    
103400 IMS-GHNP-WDQ212               SECTION.                                   
103500                                                                          
103600     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
103700            DELIMITED BY SIZE INTO SSA1                                   
103800     MOVE '  GE'               TO GODK-STATUSKODER                        
103900     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-WDQ212 SSA1             
104000     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
104100     PERFORM IMS-STATUSKONTROLL                                           
104200     .                                                                    
104300 IMS-GHNP-WDQ221-GE            SECTION.                                   
104400                                                                          
104410     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
104420            DELIMITED BY SIZE INTO SSA1                                   
104500     STRING 'WLORQI21(ADLAGOMR =' W-ADLAGOMR-X ')'                        
104600            DELIMITED BY SIZE INTO SSA2                                   
104700     MOVE '  GE'               TO GODK-STATUSKODER                        
104800     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-WDQ221 SSA1 SSA2        
104900     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
105000     PERFORM IMS-STATUSKONTROLL                                           
105100     .                                                                    
105200     EJECT                                                                
105210 IMS-GHNP-WDQ221               SECTION.                                   
105220                                                                          
105221     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
105222            DELIMITED BY SIZE INTO SSA1                                   
105230     STRING 'WLORQI21(ADLAGOMR =' W-ADLAGOMR-X ')'                        
105240            DELIMITED BY SIZE INTO SSA2                                   
105250     MOVE '  '                 TO GODK-STATUSKODER                        
105260     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-WDQ221 SSA1 SSA2        
105270     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
105280     PERFORM IMS-STATUSKONTROLL                                           
105290     .                                                                    
105291     EJECT                                                                
105300 IMS-REPL-WDQ211              SECTION.                                    
105400                                                                          
105500     MOVE   '    '            TO GODK-STATUSKODER                         
105600     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-WDQ211                  
105700     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
105800     PERFORM IMS-STATUSKONTROLL                                           
105900     .                                                                    
106000     SKIP2                                                                
106100 IMS-REPL-WDQ212              SECTION.                                    
106200                                                                          
106300     MOVE   '    '            TO GODK-STATUSKODER                         
106400     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-WDQ212                  
106500     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
106600     PERFORM IMS-STATUSKONTROLL                                           
106700     .                                                                    
106800     SKIP2                                                                
106900 IMS-REPL-WDQ221              SECTION.                                    
107000                                                                          
107100     MOVE   '    '            TO GODK-STATUSKODER                         
107200     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-WDQ221                  
107300     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
107400     PERFORM IMS-STATUSKONTROLL                                           
107500     .                                                                    
107600     SKIP2                                                                
107700 IMS-DLET-WDQ221               SECTION.                                   
107800                                                                          
107900     MOVE   '    '            TO GODK-STATUSKODER                         
108000     CALL CBLTDLI USING DLET ORQI-PCB DLI-IO-AREA-WDQ221                  
108100     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
108200     PERFORM IMS-STATUSKONTROLL                                           
108300     .                                                                    
108400     EJECT                                                                
108500 IMS-DLET-WDQ211               SECTION.                                   
108600                                                                          
108700     MOVE   '    '            TO GODK-STATUSKODER                         
108800     CALL CBLTDLI USING DLET ORQI-PCB DLI-IO-AREA-WDQ211                  
108900     MOVE ORQI-STATUS-CODE    TO STATUS-WS                                
109000     PERFORM IMS-STATUSKONTROLL                                           
109100     .                                                                    
109200     EJECT                                                                
109300                                                                          
109400 IMS-GU-WDB301                 SECTION.                                   
109500                                                                          
109600     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
109700                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
109800            DELIMITED BY SIZE INTO SSA1                                   
109900     MOVE '  GE'              TO GODK-STATUSKODER                         
110000     CALL CBLTDLI USING GU  GMTB-PCB DLI-IO-AREA-WDB301 SSA1              
110100     MOVE GMTB-STATUS-CODE      TO STATUS-WS                              
110200     PERFORM IMS-STATUSKONTROLL                                           
110300     .                                                                    
110400     SKIP2                                                                
110500 IMS-GU-WDB501                 SECTION.                                   
110600                                                                          
110700     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X                            
110800                    '+WDB501KY =' W-WDB501KY-DEF-X ')'                    
110900            DELIMITED BY SIZE INTO SSA1                                   
111000     MOVE '  GE'              TO GODK-STATUSKODER                         
111100     CALL CBLTDLI USING GU  GMTC-PCB DLI-IO-AREA-WDB501 SSA1              
111200     MOVE GMTC-STATUS-CODE      TO STATUS-WS                              
111300     PERFORM IMS-STATUSKONTROLL                                           
111400     .                                                                    
111500     EJECT                                                                
111600 IMS-GU-WDB201 SECTION.                                                   
111700                                                                          
111800     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
111900          DELIMITED BY SIZE INTO SSA1                                     
112000     MOVE '  ' TO GODK-STATUSKODER                                        
112100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
112200     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
112300     PERFORM IMS-STATUSKONTROLL                                           
112400     .                                                                    
112500     EJECT                                                                
112600                                                                          
112700 IMS-GU-WDB601    SECTION.                                                
112800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
112900          DELIMITED BY SIZE INTO SSA1                                     
113000     MOVE '  GE' TO GODK-STATUSKODER                                      
113100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
113200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
113300     PERFORM IMS-STATUSKONTROLL                                           
113400     IF SEGMENT-SAKNAS                                                    
113500         MOVE SPACE TO DCS-KDDC                                           
113600     END-IF                                                               
113700     .                                                                    
113800                                                                          
113900 IMS-GU-WDB601-HOME SECTION.                                              
114000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
114100          DELIMITED BY SIZE INTO SSA1                                     
114200     MOVE '  GE' TO GODK-STATUSKODER                                      
114300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-HOME SSA1            
114400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
114500     PERFORM IMS-STATUSKONTROLL                                           
114600     IF SEGMENT-SAKNAS                                                    
114700         MOVE SPACE TO HOME-DCS-KDDC                                      
114800     END-IF                                                               
114900     .                                                                    
115000                                                                          
115100 IMS-STATUSKONTROLL            SECTION.                                   
115200                                                                          
115300     SET STATUS-IX             TO 1                                       
115400     SEARCH GODK-STATUS                                                   
115500       AT END CALL FELLOG                                                 
115600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
115700     END-SEARCH                                                           
115800     .                                                                    
