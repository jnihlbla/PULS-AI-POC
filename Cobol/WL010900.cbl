000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WL010900.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   03/11/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAMN: CARPARTS.LDC.LOSTCASEFOUND.WL0109                              
000900*    WEB-LDC: WL010900 PROGRAM IS A REPLICA OF W6030900 PROGRAM           
001000*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001100*                                                                         
001200*    FUNKTION:                                                            
001300*        LDC INLÄGGNING ÅTERFUNNA LOST KOLLI                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: WL0109U                                             
001700*        REQUEST:     WZ01REQU                                            
001800*                     WL0109I1                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        RESPONSE:    WZ01RESP                                            
002200*                     WL0109O1                                            
002300*                                                                         
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'WL010900'.            
003800 77  WS-ADRESS                   PIC X(50)   VALUE                        
003900        'CARPARTS.LDC.LOSTCASEFOUND'.                                     
       77  WS-ADDRESS-WHSTOCKA          PIC X(50)                               
               VALUE 'CARPARTS.PULS.WHSTOCKADJ'.                                
       77  WS-ADDRESS-MQASYNC           PIC X(50)                               
               VALUE 'CARPARTS.PULS.MQASYNC'.                                   
       77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004000 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
004100 77  CURRENT-IMS-SECTION         PIC X(32)   VALUE SPACE.                 
004200                                                                          
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004500 77  KDRC-DISPLAY                PIC Z(5).                                
004600 77  INDX-DISPLAY                PIC 999     VALUE ZERO.                  
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  REC-WS-IDDC                 PIC X(2)    VALUE SPACE.                 
005100 77  SEND-WS-IDDC                PIC X(2)    VALUE SPACE.                 
005200*01  -COPY WWDCKONS                                                       
005300                                                                          
005400*01  -COPY WWDC99                                                         
005500                                                                          
005600 77  WS-A03-SKAPAD               PIC X       VALUE 'N'.                   
005700 77  WS-UPPD-LOCB                PIC X       VALUE 'N'.                   
005800 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
005900 77  IX1                         PIC S9(3)   VALUE ZERO COMP-3.           
006000 77  IX2                         PIC S9(3)   VALUE ZERO COMP-3.           
006100 77  INDX-1                      PIC S9(5)   VALUE ZERO COMP-3.           
006200 77  INDX-2                      PIC S9(5)   VALUE ZERO COMP-3.           
006300 77  RAD-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
006400 77  INDX-LINE                   PIC S9(3)   VALUE ZERO COMP-3.           
006500 77  MAX-RAD                     PIC S9(5)   VALUE +12  COMP-3.           
006600 77  TAB-IX                      PIC S9(5)   VALUE +0   COMP-3.           
006700 77  TAB-IX-MAX                  PIC S9(5)   VALUE +12  COMP-3.           
006800 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
006900 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
       77  WS-IDARTNR-Z                PIC Z(9).                                
       77  W-IDORDER                   PIC 9(7)  VALUE ZERO.                    
       77  W-IDKOLLI                   PIC 9(5)  VALUE ZERO.                    
007000 77  WS-KDVALISO                 PIC X(3)    VALUE SPACE.                 
007100 77  WS-TID                      PIC 9(8)    VALUE ZERO.                  
007200 77  WS-OLD-KVLS                 PIC S9(7)   VALUE ZERO COMP-3.           
007300 77  WS-OLD-KVEFRS               PIC S9(7)   VALUE ZERO COMP-3.           
007400 77  WS-KVANTMOT                 PIC 9(7)    VALUE ZERO.                  
007500 77  WS-KVSKROT                  PIC 9(7)    VALUE ZERO.                  
007600 77  WS-RO-KVANT                 PIC S9(7)   VALUE ZERO COMP-3.           
007700 77  WS-RAD-IFYLLD               PIC X       VALUE SPACE.                 
007800 77  W-KVAVIS                    PIC S9(7)   VALUE ZERO COMP-3.           
007900 77  WS-SUMMA-KVANT              PIC 9(7)    VALUE ZERO.                  
008000 77  W-PRARTNTO                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008100 77  W-KDFRAKT                   PIC S9(3)      VALUE ZERO COMP-3.        
008200 77  WS-FIXAD-PRARTNTO           PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008300 77  W-KDPRODSL                  PIC 9(2)    VALUE ZERO.                  
008400 77  W-KDPRODSL-LOC              PIC 9(2)    VALUE ZERO.                  
008500 77  W-IDFKNGRP                  PIC 9(5)    VALUE ZERO.                  
008600 77  W-IDLEVNR-R34               PIC  X(5)   VALUE SPACE.                 
008700 77  WS-BAATORDER                PIC X       VALUE SPACE.                 
008800 77  WS-FLYGORDER                PIC X       VALUE SPACE.                 
008900 77  WS-FIKT-FAKT-ANV            PIC X       VALUE SPACE.                 
009000 77  W-DAGENS-DATUM              PIC 9(6)    VALUE ZERO.                  
009100 77  WS-NY-ART                   PIC X       VALUE SPACE.                 
009200 77  WS-TIFAKT                   PIC 9(6)    VALUE ZERO.                  
009300 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
009400 77  WS-6302-IDDC-LEV            PIC X(2)    VALUE SPACE.                 
009500 77  WS-6302-IDDISTR             PIC S9(5)   VALUE ZERO   COMP-3.         
009600 77  WS-6302-IDKUNDNR            PIC S9(7)   VALUE ZERO   COMP-3.         
009700 77  WS-6308-TF-PRARTNTO         PIC S9(7)V9(2)    VALUE ZERO.            
009800 77  WS-6308-TF-KDVALISO         PIC X(3)          VALUE SPACE.           
009900                                                                          
010000 77  W-TIKLOCK                   PIC S9(9)   VALUE ZERO    COMP-3.        
010100 77  W-TID                       PIC 9(8)    VALUE ZERO.                  
010200 77  DAGENS-DATUM                PIC 9(9)    VALUE ZERO.                  
010300 77  W-IDSEKVNR-SAP              PIC S9(3)   VALUE 0   COMP-3.            
010400 77  W-IDSEKVNR-A03              PIC S9(3)   VALUE 0   COMP-3.            
010500 77  W-IDSEKVNR                  PIC S9(3)   VALUE 0   COMP-3.            
010600 77  WS-LOGG-DATUM               PIC S9(8)   VALUE ZERO.                  
010700 77  WS-LOGG-TID                 PIC S9(7)   VALUE ZERO.                  
010800 77  WS-PRIME-LOCATION           PIC X       VALUE 'P'.                   
010900 77  WS-SAP-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
011000 77  WS-SAP-TTMMSSTH             PIC 9(8)    VALUE ZERO.                  
011100 77  WS-SAP-PRARTSTD             PIC S9(7)V9(2)  VALUE 0 COMP-3.          
011200 77  WS-SAP-IDKUNDNR             PIC X(6).                                
011300 77  WS-SAP-IDFAKT               PIC X(7).                                
011400 77  NOLL-RAKNARE                PIC S9(5)   COMP-3 VALUE ZERO.           
011500 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
011600 77  WS-IDUSER-003               PIC X(5)    VALUE SPACE.                 
011700 77  INDATA-FINNS                PIC X       VALUE 'N'.                   
011800 77  SPAR-FLINLREP               PIC X       VALUE SPACE.                 
011900 77  WS-FAKTURA-DATUM            PIC X(16)   VALUE SPACE.                 
012000 77  WS-FAKTURA-DATUM2           PIC S9(16)  COMP-3 VALUE ZERO.           
012100 77  WS-TRCK-KVANTMOT            PIC S9(7)  COMP-3.                       
012200 77  WS-TRCK-KVAVIS              PIC S9(7)  COMP-3.                       
012300 77  WS-TRCK-KVTRACK-KVAR        PIC S9(7)  COMP-3.                       
012400 77  WS-WDL3-TRCK-KVTRACK-KVAR   PIC S9(7)  COMP-3.                       
012500                                                                          
012600 01  WS-SOEK-IDDC-SEND-REC.                                               
012700     03 WS-SOEK-IDDC-SEND        PIC X(2).                                
012800     03 WS-SOEK-IDDC-REC         PIC X(2).                                
012900                                                                          
013000 01  WS-IDFAKT                          PIC X(7).                         
013100 01  IDFAKT-WS  REDEFINES WS-IDFAKT     PIC 9(7).                         
013200 01  WS-IDORDNR                         PIC X(5).                         
013300 01  IDORDNR-WS REDEFINES WS-IDORDNR    PIC 9(5).                         
013400 01  WS-IDKUNDNR                        PIC X(6).                         
013500 01  IDKUNDNR-WS REDEFINES WS-IDKUNDNR  PIC 9(6).                         
013600 01  WS-IDKOLLI                         PIC X(5).                         
013700 01  IDKOLLI-WS REDEFINES WS-IDKOLLI    PIC 9(5).                         
013800                                                                          
013900 01  AKTUELL-TID.                                                         
014000     03  AKTUELL-TTMM            PIC 9(4).                                
014100     03  FILLER                  PIC 9(4).                                
014200                                                                          
014300 01  WS-SEKEL-KOLL               PIC 9(6).                                
014400 01  FILLER REDEFINES WS-SEKEL-KOLL.                                      
014500     03  WS-SEKEL                PIC 9(1).                                
014600     03  FILLER                  PIC 9(5).                                
014700                                                                          
014800 01  WS-SEKEL-EKOA03.                                                     
014900     03  WS-EKOA03-SS            PIC 9(2).                                
015000     03  WS-EKOA03-AAMMDD        PIC 9(6).                                
015100 01  WS-AAAAMMDD REDEFINES WS-SEKEL-EKOA03 PIC 9(8).                      
015200                                                                          
015300 01  WS-SEKEL-TEST               PIC 9(6).                                
015400 01  FILLER REDEFINES WS-SEKEL-TEST.                                      
015500     03  WS-SEK                  PIC 9(1).                                
015600     03  FILLER                  PIC 9(5).                                
015700                                                                          
015800 01  WS-SEKEL-DIFF.                                                       
015900     03  WS-DIFF-SS              PIC 9(2).                                
016000     03  WS-DIFF-AAMMDD          PIC 9(6).                                
016100 01  WS-DIFF-AAAAMMDD REDEFINES WS-SEKEL-DIFF PIC 9(8).                   
016200                                                                          
016300 01  WS-IDDC-KOLL.                                                        
016400     03  FILLER                  PIC X(5) VALUE 'WIDDC'.                  
016500     03  WS-IDDC-TID             PIC X(2) VALUE SPACE.                    
016600     03  FILLER                  PIC X    VALUE SPACE.                    
016700                                                                          
016800 01  W-TIME-X.                                                            
016900     03  W-TIME-TT               PIC 9(2).                                
017000     03  FILLER                  PIC 9(6).                                
017100 01  W-TIME-N REDEFINES W-TIME-X PIC 9(8).                                
017200                                                                          
017300 01  W-TIAAAAMMDDTTMMSSTH        PIC 9(16)   VALUE ZERO.                  
017400 01  FILLER REDEFINES W-TIAAAAMMDDTTMMSSTH.                               
017500     03  W-TISEKEL               PIC 9(2).                                
017600     03  W-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                                
017700     03  W-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                                
017800                                                                          
017900 01  FILLER REDEFINES W-TIAAAAMMDDTTMMSSTH.                               
018000     03  W-TIAAAAMMDDTTMMSSTH-DATE                                        
018100                                 PIC 9(8).                                
018200     03  W-TIAAAAMMDDTTMMSSTH-TIME                                        
018300                                 PIC 9(8).                                
018400 01  W-IDLOPNRM              PIC 9(9)    VALUE ZERO.                      
018500 01  W-0VVDLLLLK             REDEFINES W-IDLOPNRM.                        
018600     03 FILLER               PIC 9(1).                                    
018700     03 W-VVD                PIC 9(3).                                    
018800     03 W-LLLL               PIC 9(4).                                    
018900     03 W-K                  PIC 9(1).                                    
019000                                                                          
019100 77  RKOD-ABEND              PIC S9(4)   COMP VALUE +0.                   
019200 77  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.                  
019300 77  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.                
019400 77  RKOD-ABEND-DB2          PIC S9(4)   COMP VALUE +998.                 
019500                                                                          
019600 77  INDATA-SW               PIC X       VALUE 'J'.                       
019700     88  INDATA-OK                       VALUE 'J'.                       
019800     88  INDATA-FEL                      VALUE 'N'.                       
019900                                                                          
020000 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
020100     88  NYCKLAR-OK                      VALUE 'J'.                       
020200     88  NYCKLAR-FEL                     VALUE 'N'.                       
020300                                                                          
020400 77  PRINT-SW                    PIC X       VALUE 'N'.                   
020500     88  PRINT                               VALUE 'J'.                   
020600     88  EJ-PRINT                            VALUE 'N'.                   
020700                                                                          
020800 77  PRINT-SW-KOREA              PIC X       VALUE 'N'.                   
020900     88  PRINT-KOREA                         VALUE 'J'.                   
021000     88  EJ-PRINT-KOREA                      VALUE 'N'.                   
021100 77  WS-WRITE-IDTRACK            PIC X       VALUE 'N'.                   
021200     88  WRITE-IDTRACK                       VALUE 'J'.                   
021300     88  NOT-WRITE-IDTRACK                   VALUE 'N'.                   
                                                                                
       77  FIRST-REC-TRANS-SW          PIC X       VALUE 'N'.                   
           88  NOT-FIRST-REC-TRANS                 VALUE 'N'.                   
           88  FIRST-REC-TRANS                     VALUE 'J'.                   
                                                                                
021400 01  PLATS-TABELL.                                                        
021500     03 TABELLRAD  OCCURS 12.                                             
021600        05 WS-ADLAGOMR       PIC 9(3).                                    
021700        05 WS-ADGANG         PIC 9(3).                                    
021800        05 WS-ADPLATS        PIC 9(5).                                    
021900                                                                          
022000                                                                          
022100 01  W-LAGERPLATS-LOCB.                                                   
022200        03  W-ADLAGOMR-LOCB  PIC S9(3) COMP-3.                            
022300        03  W-ADGANG-LOCB    PIC S9(3) COMP-3.                            
022400        03  W-ADPLATS-LOCB   PIC S9(5) COMP-3.                            
022500                                                                          
022600     EJECT                                                                
022700 01  GENERELLA-SUBPROGRAM.                                                
022800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
022900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
023100     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
023200     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
023300     03  W612KLBL                PIC X(8)    VALUE 'W612KLBL'.            
023400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
023500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
023600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
023700     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
023800     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
023900     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
024000     EJECT                                                                
024100*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
024200*01 -COPY W005WDK7                                                        
024300     EJECT                                                                
024400*    --- PARAMETRAR TILL SUBPROGRAM W005WDL7                              
024500*01 -COPY W005WDL7                                                        
024600     EJECT                                                                
024700*    --- PARAMETRAR TILL SUBPROGRAM W612KLBL                              
024800*01    -COPY W612KLBL PRE KLBL-                                           
024900       EJECT                                                              
025000                                                                          
025100 01  MESSAGE-CODES.                                                       
025200     03  UPDATE-DONE             PIC X(3)   VALUE '001'.                  
025300     03  PRINTING-REQUESTED      PIC X(3)   VALUE '015'.                  
025400     03  NO-DATA-ENTERED         PIC X(3)   VALUE '014'.                  
025500     03  INVALID-KEY-FIELDS      PIC X(3)   VALUE '022'.                  
025600     03  IS-INVALID              PIC X(3)   VALUE '023'.                  
025700     03  SYSTEM-ERROR            PIC X(3)   VALUE '099'.                  
025800     03  KEYS-ARE-MISSING        PIC X(3)   VALUE '041'.                  
025900     03  CONFIRM-NOT-LAST-LINE-IN-CASE PIC X(3) VALUE '106'.              
026000     03  MUST-ENTER-EMP-ID       PIC X(3)   VALUE '026'.                  
026100     03  TRACKING-ID-MISSING     PIC X(3)   VALUE '422'.                  
026200     EJECT                                                                
026300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
026400*01  -COPY WZ01SUB                                                        
026500     EJECT                                                                
026600 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
026700*01  -COPY WZ01SEND                                                       
026800     EJECT                                                                
026900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
027000 01  REQU-AREA.                                                           
027100*    03  -COPY WZ01REQU                                                   
027200*    03  -COPY WL0109I1                                                   
027300     EJECT                                                                
027400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
027500 01  RESP-AREA.                                                           
027600*    03  -COPY WZ01RESP                                                   
027700*    03  -COPY WL0109O1                                                   
027800     EJECT                                                                
027900*01  -COPY WL01TIDZ                                                       
028000     EJECT                                                                
029000*01 -COPY WWDIST35                                                        
030000     EJECT                                                                
031000*01 -COPY WWDIST79                                                        
031100     EJECT                                                                
031200*01 -COPY WWDC03                                                          
031300     EJECT                                                                
031400*01 -COPY WDATAREA                                                        
031500     EJECT                                                                
031600*01 -COPY W510AVG                                                         
031700     EJECT                                                                
031800*01 -COPY W510A03 -PRE EKOTRA03-                                          
031900     EJECT                                                                
032000*01 -COPY W335PRIS                                                        
032100     EJECT                                                                
032200*01 -COPY W61236  -PRE FILC-                                              
032300     EJECT                                                                
032400*01 -COPY W61244  -PRE FILC2-                                             
032500     EJECT                                                                
032600*01 -COPY W61247  -PRE FILC3-                                             
032700     EJECT                                                                
      *    NOTAFISCAL                                                           
       01  NOTF-AREA.                                                           
      *    03  -COPY W611NOTF                                                   
       01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
      *01  -COPY WZ04PROP                                                       
                                                                                
032800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
033000 01  NYCKLAR-TILL-DLI.                                                    
033100     03  W-IDARTNR-X.                                                     
033200         05  W-IDARTNR           PIC S9(9)              COMP-3.           
033300     03  W-IDKUNDNR-X.                                                    
033400         05  W-IDKUNDNR          PIC S9(7)              COMP-3.           
033500     03  W-DAINLEV-X.                                                     
033600         05  W-DAINLEV           PIC 9(16).                               
033700     03  W-IDDC-X.                                                        
033800         05  W-IDDC71            PIC X(2)    VALUE SPACE.                 
033900     03  W-IDDC                  PIC X(2).                                
034000     03  W-IDDC-B6-X.                                                     
034100         05  W-IDDC-B6           PIC X(2)      VALUE SPACE.               
034200                                                                          
034300     03  W-IDDC-B616-X.                                                   
034400         05  W-IDDC-B616         PIC X(2)      VALUE SPACE.               
034500                                                                          
034600     03  W-IDTRACK-X.                                                     
034700         05  W-IDTRACK           PIC X(25).                               
034800                                                                          
034900     03  W-IDPTYP                PIC X(3).                                
035000     03  W-IDSKYLT               PIC X(3).                                
035100     03  W-IDKUNDRF              PIC X(10).                               
035200     03  W-WDL6A1KY-MIN.                                                  
035300         05  W-SEQA-IDFAKT-MIN    PIC S9(7)             COMP-3.           
035400         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
035500         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)             COMP-3.           
035600         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)             COMP-3.           
035700         05  W-SEQA-IDARTNR-MIN   PIC S9(9)             COMP-3.           
035800         05  W-SEQA-DAINLEV-MIN   PIC 9(16).                              
035900     03  W-WDL6A1KY-MAX.                                                  
036000         05  W-SEQA-IDFAKT-MAX    PIC S9(7)             COMP-3.           
036100         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
036200         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)             COMP-3.           
036300         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)             COMP-3.           
036400         05  W-SEQA-IDARTNR-MAX   PIC S9(9)             COMP-3.           
036500         05  W-SEQA-DAINLEV-MAX   PIC 9(16).                              
036600     03  W-WDL6A1KY-MIN2.                                                 
036700         05  W-IDFAKT-MIN         PIC S9(7) COMP-3.                       
036800         05  FILLER               PIC X(38).                              
036900     03  W-WDL6A1KY-MAX2.                                                 
037000         05  W-IDFAKT-MAX         PIC S9(7) COMP-3.                       
038000         05  FILLER               PIC X(38).                              
038100     03  W-WDL6A1KY-MIN3.                                                 
038200         05  W-IDFAKT-MIN3        PIC S9(7) COMP-3.                       
038300         05  W-IDKUNDRF-MIN       PIC X(10).                              
038400         05  W-IDKUNDNR-MIN       PIC S9(7) COMP-3.                       
038500         05  W-IDKOLLI-MIN        PIC S9(5) COMP-3.                       
038600         05  FILLER               PIC X(21).                              
038700                                                                          
038800     03  W-WDL6A1KY-MAX3.                                                 
038900         05  W-IDFAKT-MAX3        PIC S9(7) COMP-3.                       
039000         05  W-IDKUNDRF-MAX       PIC  X(10).                             
039100         05  W-IDKUNDNR-MAX       PIC S9(7) COMP-3.                       
039200         05  W-IDKOLLI-MAX        PIC S9(5) COMP-3.                       
039300         05  FILLER               PIC X(21).                              
039400                                                                          
039500     03  W-WDL6A1KY-MIN4.                                                 
039600         05  W-SEQA-IDFAKT-MIN4   PIC S9(7)             COMP-3.           
039700         05  W-SEQA-IDKUNDRF-MIN4 PIC X(10).                              
039800         05  W-SEQA-IDKUNDNR-MIN4 PIC S9(7)             COMP-3.           
039900         05  W-SEQA-IDKOLLI-MIN4  PIC S9(5)             COMP-3.           
040000         05  W-SEQA-IDARTNR-MIN4  PIC S9(9)             COMP-3.           
041000         05  W-SEQA-DAINLEV-MIN4  PIC 9(16).                              
042000     03  W-WDL6A1KY-MAX4.                                                 
043000         05  W-SEQA-IDFAKT-MAX4   PIC S9(7)             COMP-3.           
044000         05  W-SEQA-IDKUNDRF-MAX4 PIC X(10).                              
045000         05  W-SEQA-IDKUNDNR-MAX4 PIC S9(7)             COMP-3.           
046000         05  W-SEQA-IDKOLLI-MAX4  PIC S9(5)             COMP-3.           
047000         05  W-SEQA-IDARTNR-MAX4  PIC S9(9)             COMP-3.           
048000         05  W-SEQA-DAINLEV-MAX4  PIC 9(16).                              
048100                                                                          
048200     03  W-WDL6C1KY-MIN-X.                                                
048300         05  W-IDFAKT-SEQC-MIN    PIC S9(7)             COMP-3.           
048400         05  W-IDDC-SEQC-MIN      PIC X(2).                               
048500         05  FILLER               PIC X(21)    VALUE LOW-VALUE.           
048600     03  W-WDL6C1KY-MAX-X.                                                
048700         05  W-IDFAKT-SEQC-MAX    PIC S9(7)             COMP-3.           
048800         05  W-IDDC-SEQC-MAX      PIC X(2).                               
048900         05  FILLER               PIC X(21)    VALUE HIGH-VALUE.          
049000                                                                          
049100     03  W-IDFAKT-X.                                                      
049200         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
049300     03  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
049400     03  W-6017KEY-X.                                                     
049500         05  W-6017-IDHTYP      PIC X(4)     VALUE '6017'.                
049600         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
049700     03  W-6301KEY-X.                                                     
049800         05  W-6301-IDHTYP      PIC X(4)     VALUE '6301'.                
049900         05  W-6301-IDDC        PIC X(2).                                 
050000         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
050100                                                                          
050200     03  W-6305KEY-X.                                                     
050300         05  W-6305-IDHTYP      PIC X(4)     VALUE '6305'.                
050400         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
050500                                                                          
050600     03  W-4505-KEY-X.                                                    
050700         05  FILLER              PIC X(4)    VALUE '4505'.                
050800         05  4505-IDDC           PIC X(2)    VALUE SPACE.                 
050900         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
051000                                                                          
051100     03  W-WDJ911KY-X.                                                    
051200         05  W-IDDC-WDJ9         PIC 9(2)    VALUE ZERO.                  
051300         05  W-DASTADAT          PIC S9(9)   VALUE ZERO.                  
051400         05  W-TISTATID          PIC S9(7)   VALUE ZERO.                  
051500         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO.                  
051600         05  W-ADGANG            PIC 9(2)    VALUE ZERO.                  
051700         05  W-ADPLATS           PIC 9(5)    VALUE ZERO.                  
051800     EJECT                                                                
051900*    --- STATUS-KOD FRÅN IMS                                              
052000 01  STATUS-WS                   PIC XX.                                  
052100     88  SEGMENT-FINNS                       VALUE '  '.                  
052200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
052300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
052400     88  END-OF-DATABASE                     VALUE 'GB'.                  
052500     SKIP2                                                                
052600 01  GODK-STATUSKODER.                                                    
052700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
052800     SKIP3                                                                
052900 01  SSA1                        PIC X(160).                              
053000 01  SSA2                        PIC X(128).                              
053100 01  SSA3                        PIC X(128).                              
053200     EJECT                                                                
053300*    --- IMS FUNKTIONSKODER                                               
053400*01  -COPY W0003                                                          
053500     EJECT                                                                
053600                                                                          
053700*    ----DB2                                                              
053800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
053900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
054000                                                                          
054100 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
054200 01  DB2-WS.                                                              
054300     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
054400         88  CURSOR-OK                       VALUE 000.                   
054500         88  LINES-FOUND                     VALUE 000.                   
054600         88  LINES-MISSING                   VALUE 100.                   
054700         88  TABELL-TOM                      VALUE 305.                   
054800         88  RESOURCE-WRONG                  VALUE 904.                   
054900     03  GOOD-SQLCODECODES.                                               
055000         05  GOOD-SQLCODE OCCURS 5                                        
055100             INDEXED BY SQLCODE-IX PIC 9(3).                              
055200                                                                          
055300     EJECT                                                                
055400 01  FILLER                      PIC X(16)   VALUE 'TP6FAKT-AREA'.        
055500*01  -COPY TP6FAKT -PRE RAD-                                              
055600                                                                          
055700     EXEC SQL INCLUDE TP6FAKT END-EXEC.                                   
055800                                                                          
055900*    ---  DLI INPUT-OUTPUT AREA                                           
056000*01  WLLOGA01  -COPY WDL901                                               
056100     EJECT                                                                
056200 01  DLI-IO-AREA-WDL6.                                                    
056300     03  IO-AREA-WDL6            PIC X(300)  VALUE SPACE.                 
056400     03  WLINLC01 REDEFINES IO-AREA-WDL6.                                 
056500*        05  -COPY WDL601                                                 
056600     EJECT                                                                
056700     03  WLINLC11 REDEFINES IO-AREA-WDL6.                                 
056800*        05  -COPY WDL611                                                 
056900     EJECT                                                                
057000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL623'.                      
057100 01  DLI-IO-WDL623.                                                       
057200*    03  -COPY WDL623                                                     
057300     EJECT                                                                
057400 01  DLI-IO-AREA-WDK7.                                                    
057500*    03  -COPY WDK711                                                     
057600     EJECT                                                                
057700 01  DLI-IO-WDK728.                                                       
057800*    03  -COPY WDK728                                                     
057900     EJECT                                                                
058000 01  DLI-IO-AREA-WDK6.                                                    
058100     03  IO-AREA-WDK6            PIC X(900)  VALUE SPACE.                 
058200     03  WLARTC01 REDEFINES IO-AREA-WDK6.                                 
058300*        05  -COPY WDK601        -PRE K6-                                 
058400     EJECT                                                                
058500     03  WLARTC11 REDEFINES IO-AREA-WDK6.                                 
058600*        05  -COPY WDK611                                                 
058700     EJECT                                                                
058800 01  DLI-IO-AREA-WDGX.                                                    
058900     03  IO-AREA-WDGX            PIC X(300)  VALUE SPACE.                 
059000     03  WL630101 REDEFINES IO-AREA-WDGX.                                 
059100*        05  -COPY WDGX6301                                               
059200     EJECT                                                                
059300     03  WL630111 REDEFINES IO-AREA-WDGX.                                 
059400*        05  -COPY WDGX6302                                               
059500     EJECT                                                                
059600 01  DLI-IO-AREA-WDGX2.                                                   
059700     03  IO-AREA-WDGX2           PIC X(300)  VALUE SPACE.                 
059800     SKIP3                                                                
059900     03  WL630511 REDEFINES IO-AREA-WDGX2.                                
060000*        05  -COPY WDGX6306                                               
060100     03  WL630521 REDEFINES IO-AREA-WDGX2.                                
060200*        05  -COPY WDGX6308                                               
060300     EJECT                                                                
060400     EJECT                                                                
060500 01  DLI-IO-AREA-WDD3.                                                    
060600     03  IO-AREA-WDD3            PIC X(300)  VALUE SPACE.                 
060700     03  WLBENA11 REDEFINES IO-AREA-WDD3.                                 
060800*        05  -COPY WDD311                                                 
060900     EJECT                                                                
061000 01  DLI-IO-AREA-W6GX.                                                    
061100     03  IO-AREA-W6GX            PIC X(300)  VALUE SPACE.                 
061200     03  W6LOPA11 REDEFINES IO-AREA-W6GX.                                 
061300*        05  -COPY W6GX6018                                               
061400     EJECT                                                                
061500 01  DLI-IO-AREA-4505.                                                    
061600     03  IO-AREA-4505            PIC X(300)  VALUE SPACE.                 
061700     SKIP3                                                                
061800     03  WL450611 REDEFINES IO-AREA-4505.                                 
061900*        05  -COPY WDGX4506                                               
062000     EJECT                                                                
062100 01  DLI-IO-AREA-FILC.                                                    
062200     03  IO-AREA-FILC            PIC X(300)  VALUE SPACE.                 
062300     03  WLFILC01 REDEFINES IO-AREA-FILC.                                 
062400*        05  -COPY WDR301 -PRE FILC-.                                     
062500     EJECT                                                                
062600 01  DLI-IO-AREA-FILC2.                                                   
062700     03  IO-AREA-FILC2           PIC X(300)  VALUE SPACE.                 
062800                                                                          
062900     03  WLFILC01 REDEFINES IO-AREA-FILC2.                                
063000*        05  -COPY WDR301 -PRE FILC2-.                                    
063100     EJECT                                                                
063200 01  DLI-IO-AREA-FILC3.                                                   
063300     03  IO-AREA-FILC3           PIC X(300)  VALUE SPACE.                 
063400     03  WLFILC01 REDEFINES IO-AREA-FILC3.                                
063500*        05  -COPY WDR301 -PRE FILC3-.                                    
063600     EJECT                                                                
063700 01  DLI-IO-AREA-LOCB.                                                    
063800     03  IO-AREA-LOCB            PIC X(300)  VALUE SPACE.                 
063900     03  WLLOCB01 REDEFINES IO-AREA-LOCB.                                 
064000*        05  -COPY WDJ901       -PRE LOCB-                                
064100     EJECT                                                                
064200     03  WLLOCB11 REDEFINES IO-AREA-LOCB.                                 
064300*        05  -COPY WDJ911       -PRE LOCB-                                
064400     EJECT                                                                
064500 01  FILLER               PIC X(16)   VALUE 'WDB601 REC '.                
064600 01  DLI-IO-AREA-B601-REC.                                                
064700*    03  -COPY WDB601 -PRE REC-                                           
064800     EJECT                                                                
064900 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDB616'.              
065000 01  DLI-IO-WDB616.                                                       
065100*    03  -COPY WDB616                                                     
065200     EJECT                                                                
065300 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDL301'.              
065400 01  DLI-IO-WDL301.                                                       
065500*    03  -COPY WDL301                                                     
065600     EJECT                                                                
065700 01  DLI-IO-WLSAPA01.                                                     
065800*    03  WLSAPA01  -COPY WDR901                                           
065900*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
066000     EJECT                                                                
066100 01  DLI-IO-WLFILB01.                                                     
066200*    03  WLFILB01  -COPY WDR801                                           
066300*    07  -COPY W510EKHA  -PRE R8- -RED FIL-WDR801-DATA                    
066400     EJECT                                                                
066500 01  FILLER               PIC X(16)   VALUE 'WDB601 SEND'.                
066600 01  DLI-IO-AREA-B601-SEND.                                               
066700*    03  -COPY WDB601  -PRE SEND-                                         
066800     EJECT                                                                
066900 01  DLI-IO-L6A1.                                                         
067000*    03  -COPY WDL6A1   -PRE WDL6A1-                                      
067100 01  DLI-IO-L6C1.                                                         
067200*    03  -COPY WDL6C1                                                     
067300                                                                          
067400 LINKAGE SECTION.                                                         
067500*01  -COPY W0009  -PRE MSG-                                               
067600 01  DISTRDOC-PCB                PIC X.                                   
067700     EJECT                                                                
      *01  -COPY W0009  -PRE WHS-                                               
       01  MQASYNC-PCB                 PIC X.                                   
           EJECT                                                                
067800*01  -COPY W0008  -PRE WLLOGA-                                            
067900     05  FILLER                  PIC X.                                   
068000     EJECT                                                                
068100*01  -COPY W0008  -PRE GX63-                                              
068200     05  FILLER                  PIC X.                                   
068300     EJECT                                                                
068400*01  -COPY W0008  -PRE GX65-                                              
068500     05  FILLER                  PIC X.                                   
068600     EJECT                                                                
068700*01  -COPY W0008  -PRE 4505-                                              
068800     05  FILLER                  PIC X.                                   
068900     EJECT                                                                
069000*01  -COPY W0008  -PRE 9305-                                              
069100     05  FILLER                  PIC X.                                   
069200     EJECT                                                                
069300*01  -COPY W0008  -PRE AVG-WDB6-                                          
069400     05  FILLER                  PIC X.                                   
069500     EJECT                                                                
069600*01  -COPY W0008  -PRE INLC-                                              
069700     05  FILLER                  PIC X.                                   
069800     EJECT                                                                
069900*01  -COPY W0008  -PRE ARTC-                                              
070000     05  FILLER                  PIC X.                                   
070100     EJECT                                                                
070200*01  -COPY W0008  -PRE WDK7-                                              
070300     05  FILLER                  PIC X.                                   
070400     EJECT                                                                
070500*01  -COPY W0008  -PRE PRIS-WDK7-                                         
070600     05  FILLER                  PIC X.                                   
070700     EJECT                                                                
070800*01  -COPY W0008  -PRE FILC-                                              
070900     05  FILLER                  PIC X.                                   
071000     EJECT                                                                
071100*01  -COPY W0008  -PRE LOCB-                                              
071200     05  FILLER                  PIC X.                                   
071300     EJECT                                                                
071400*01  -COPY W0008  -PRE SAPA-                                              
071500     05  FILLER                  PIC X.                                   
071600     EJECT                                                                
071700*01  -COPY W0008  -PRE WDB6-                                              
071800     05  FILLER                  PIC X.                                   
071900     EJECT                                                                
072000*01  -COPY W0008  -PRE WDL3-                                              
072100     05  FILLER                  PIC X.                                   
072200     EJECT                                                                
072300*01  -COPY W0008  -PRE GMTA-                                              
072400     05  FILLER                  PIC X.                                   
072500     EJECT                                                                
072600*01  -COPY W0008  -PRE BETA-                                              
072700     05  FILLER                  PIC X.                                   
072800     EJECT                                                                
072900*01  -COPY W0008  -PRE GPRIA-                                             
073000     05  FILLER                  PIC X.                                   
073100     EJECT                                                                
073200*01  -COPY W0008  -PRE GPRIB-                                             
073300     05  FILLER                  PIC X.                                   
073400     EJECT                                                                
073500 01  PRIS-COST-WDK6-PCB          PIC X.                                   
073600 01  PRIS-COST-WDK7-PCB          PIC X.                                   
073700 01  PRIS-COST-WDF1-PCB          PIC X.                                   
073800 01  PRIS-COST-9305-PCB          PIC X.                                   
073900 01  PRIS-COST-WDK72-PCB         PIC X.                                   
074000 01  PRIS-COST-WDB6-PCB          PIC X.                                   
074100     EJECT                                                                
074200*01  -COPY W0008  -PRE FILB-                                              
074300     05  FILLER                  PIC X.                                   
074400*01  -COPY W0008  -PRE  OIGA-                                             
074500     05  FILLER                  PIC X.                                   
074600*01  -COPY W0008  -PRE WDL6A-                                             
074700     05  FILLER                  PIC X.                                   
074800     EJECT                                                                
074900*01  -COPY W0008  -PRE WDL6C-                                             
075000     05  FILLER                  PIC X.                                   
076000     EJECT                                                                
077000*01  -COPY W0008  -PRE WDD3-                                              
078000     05  FILLER                  PIC X.                                   
079000     EJECT                                                                
080000*01  -COPY W0008  -PRE WDT4-                                              
081000     05  FILLER                  PIC X.                                   
082000     EJECT                                                                
083000*01  -COPY W0008  -PRE WDL6-                                              
084000     05  FILLER                  PIC X.                                   
085000     EJECT                                                                
085100 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB MQASYNC-PCB               
085200                           GX63-PCB  GX65-PCB 4505-PCB                    
085300                           9305-PCB AVG-WDB6-PCB                          
085400                           INLC-PCB ARTC-PCB WDK7-PCB                     
085500                           PRIS-WDK7-PCB                                  
085600                           FILC-PCB WLLOGA-PCB LOCB-PCB                   
085700                           SAPA-PCB WDB6-PCB WDL3-PCB                     
085800                           GMTA-PCB BETA-PCB GPRIA-PCB GPRIB-PCB          
085900                           PRIS-COST-WDK6-PCB                             
086000                           PRIS-COST-WDK7-PCB                             
086100                           PRIS-COST-WDF1-PCB                             
086200                           PRIS-COST-9305-PCB                             
086300                           PRIS-COST-WDK72-PCB                            
086400                           PRIS-COST-WDB6-PCB                             
086500                           FILB-PCB OIGA-PCB WDL6A-PCB WDL6C-PCB          
086600                           WDD3-PCB WDT4-PCB WDL6-PCB.                    
086700 MAIN SECTION.                                                            
086800     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB MQASYNC-PCB               
086900                           GX63-PCB  GX65-PCB 4505-PCB                    
087000                           9305-PCB AVG-WDB6-PCB                          
087100                           INLC-PCB ARTC-PCB WDK7-PCB                     
087200                           PRIS-WDK7-PCB                                  
087300                           FILC-PCB WLLOGA-PCB LOCB-PCB                   
087400                           SAPA-PCB WDB6-PCB WDL3-PCB                     
087500                           GMTA-PCB BETA-PCB GPRIA-PCB GPRIB-PCB          
087600                           PRIS-COST-WDK6-PCB                             
087700                           PRIS-COST-WDK7-PCB                             
087800                           PRIS-COST-WDF1-PCB                             
087900                           PRIS-COST-9305-PCB                             
088000                           PRIS-COST-WDK72-PCB                            
088100                           PRIS-COST-WDB6-PCB                             
088200                           FILB-PCB OIGA-PCB WDL6A-PCB WDL6C-PCB          
088300                           WDD3-PCB WDT4-PCB WDL6-PCB.                    
088400     PERFORM S01-HAEMTA-ANROPSDATA                                        
088500     IF SUB-KDRC = 0                                                      
088600        PERFORM A-INIT                                                    
088700        PERFORM B-KOLLA-NYCKLAR                                           
088800        IF NYCKLAR-OK                                                     
088900           IF REQU-KDPGMACT = 'E'                                         
089000              PERFORM G-KOLLA-INPUT                                       
089100              IF INDATA-OK                                                
089200                 PERFORM H-UPPDATERA                                      
089300              END-IF                                                      
089400           END-IF                                                         
089500           IF PRINT-KOREA                                                 
089600*             PERFORM S95-SEND-CLOSE                                      
089700              MOVE PRINTING-REQUESTED TO RESP-IDMSG-INFO                  
089800           ELSE                                                           
089900              PERFORM S02-RETURNERA-SVAR                                  
090000           END-IF                                                         
090100        ELSE                                                              
090200           PERFORM S02-RETURNERA-SVAR                                     
090300        END-IF                                                            
090400     END-IF                                                               
090500                                                                          
090600     MOVE ZERO TO RETURN-CODE                                             
090700     GOBACK                                                               
090800     .                                                                    
090900     EJECT                                                                
091000 A-INIT SECTION.                                                          
091100                                                                          
091200***  MOVE ALL '+' TO RESP-AREA                                            
091300     MOVE SPACE   TO RESP-AREA                                            
091400     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
091500                     RESP-IDMSG-INFO                                      
091600                     RESP-IDELMT-ERROR                                    
091700     MOVE '001'   TO RESP-IDMSGVER                                        
091800                                                                          
091900     MOVE 'IDAG'          TO DAT-KDDATFORM                                
092000     CALL WDATKONV USING     DAT-KDDATFORM                                
092100                             DAT-I-TIDATUM                                
092200                             DAT-O-TIDATUM                                
092300                             DAT-KDSVAR                                   
092400                                                                          
092500     MOVE DAT-TIAAMMDD    TO W-DAGENS-DATUM                               
092600     ACCEPT W-TIME-X      FROM TIME                                       
092700     ACCEPT DAGENS-DATUM  FROM DATE                                       
092800     MOVE W-TIME-N        TO W-TIKLOCK                                    
092900                                                                          
093000     MOVE 'WL010900'       TO FILC-FIL-IDPGM                              
093100                              FILC2-FIL-IDPGM                             
093200                              FILC3-FIL-IDPGM                             
093300     MOVE W-DAGENS-DATUM   TO FILC-FIL-TIREGDAT                           
093400                              FILC2-FIL-TIREGDAT                          
093500                              FILC3-FIL-TIREGDAT                          
093600     MOVE 'W61236  '       TO FILC-FIL-IDCPYTXT                           
093700     MOVE 'W61244  '       TO FILC2-FIL-IDCPYTXT                          
093800     MOVE 'W61247  '       TO FILC3-FIL-IDCPYTXT                          
093900     MOVE ZERO             TO FILC-FIL-TIKLOCK                            
094000                              FILC2-FIL-TIKLOCK                           
094100                              FILC3-FIL-TIKLOCK                           
094200     MOVE NEJ              TO WS-FIKT-FAKT-ANV                            
094300     INITIALIZE KLBL-W612KLBL                                             
094400     MOVE +0 TO INDX-LINE                                                 
094500     .                                                                    
094600     EJECT                                                                
094700 B-KOLLA-NYCKLAR SECTION.                                                 
094800     MOVE JA TO NYCKLAR-SW                                                
094900                                                                          
095000***  KONTROLL AV REQU-KDPGMACT                                            
095100     IF REQU-KDPGMACT = 'S' OR 'E'                                        
095200        CONTINUE                                                          
095300     ELSE                                                                 
095400        MOVE 'KDPGMACT' TO RESP-IDELMT-ERROR                              
095500        MOVE NEJ TO NYCKLAR-SW                                            
095600     END-IF                                                               
095700                                                                          
095800***  KONTROLL AV REQU-IDFAKT                                              
095900     IF REQU-IDFAKT-KEY NOT = ALL '+'                                     
096000        INSPECT REQU-IDFAKT-KEY REPLACING LEADING SPACE BY ZERO           
096100        MOVE REQU-IDFAKT-KEY TO WS-IDFAKT                                 
096200     ELSE                                                                 
096300        MOVE ZERO TO WS-IDFAKT                                            
096400     END-IF                                                               
096500*** UPDATE OF FOUND CASE WHEN IDFAKT = 0                                  
096600     IF WS-IDFAKT = ZERO                                                  
096700       PERFORM BA-FIXA-FAKTURANUMMER                                      
096800       MOVE JA        TO WS-FIKT-FAKT-ANV                                 
096900     END-IF                                                               
097000                                                                          
097100     IF WS-IDFAKT NUMERIC AND WS-IDFAKT > ZERO                            
097200        CONTINUE                                                          
097300     ELSE                                                                 
097400        MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                                
097500        MOVE NEJ TO NYCKLAR-SW                                            
097600     END-IF                                                               
097700                                                                          
097800***  KONTROLL AV REQU-IDORDNR                                             
097900     IF REQU-IDORDNR-KEY NOT = ALL '+'                                    
098000        INSPECT REQU-IDORDNR-KEY REPLACING LEADING SPACE BY ZERO          
098100        MOVE REQU-IDORDNR-KEY TO WS-IDORDNR                               
098200     ELSE                                                                 
098300        MOVE ZERO TO WS-IDORDNR                                           
098400     END-IF                                                               
098500     IF WS-IDORDNR NUMERIC                                                
098600        IF WS-IDORDNR = ZERO                                              
098700           MOVE '00001' TO WS-IDORDNR                                     
098800        END-IF                                                            
098900     ELSE                                                                 
099000        MOVE 'IDORDNR' TO RESP-IDELMT-ERROR                               
099100        MOVE NEJ TO NYCKLAR-SW                                            
099200     END-IF                                                               
099300                                                                          
099400***  KONTROLL AV REQU-IDKUNDNR                                            
099500     IF REQU-IDKUNDNR-KEY NOT = ALL '+'                                   
099600        INSPECT REQU-IDKUNDNR-KEY REPLACING LEADING SPACE BY ZERO         
099700        MOVE REQU-IDKUNDNR-KEY TO WS-IDKUNDNR                             
099800     ELSE                                                                 
099900        MOVE ZERO TO WS-IDKUNDNR                                          
100000     END-IF                                                               
100100     IF WS-IDKUNDNR NUMERIC                                               
100200         IF WS-IDKUNDNR = ZERO                                            
100300            MOVE '000001' TO WS-IDKUNDNR                                  
100400         END-IF                                                           
100500     ELSE                                                                 
100600        MOVE 'IDKUNDNR' TO RESP-IDELMT-ERROR                              
100700        MOVE NEJ TO NYCKLAR-SW                                            
100800     END-IF                                                               
100900                                                                          
101000***  KONTROLL AV REQU-IDKOLLI                                             
101100     IF REQU-IDKOLLI-KEY NOT = ALL '+'                                    
101200        INSPECT REQU-IDKOLLI-KEY REPLACING LEADING SPACE BY ZERO          
101300        MOVE REQU-IDKOLLI-KEY TO WS-IDKOLLI                               
101400     ELSE                                                                 
101500        MOVE ZERO TO WS-IDKOLLI                                           
101600     END-IF                                                               
101700     IF WS-IDKOLLI NUMERIC                                                
101800        IF WS-IDKOLLI = ZERO                                              
101900           MOVE '00001' TO WS-IDKOLLI                                     
102000        END-IF                                                            
102100     ELSE                                                                 
102200        MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                               
102300        MOVE NEJ TO NYCKLAR-SW                                            
102400     END-IF                                                               
102500                                                                          
102600***  KONTROLL AV REQU-IDDC-SEND                                           
102700                                                                          
102800     MOVE REQU-IDDC-KEY      TO REC-WS-IDDC                               
102900                                WS-IDDC                                   
103000                                W-IDDC-X                                  
103100     MOVE REQU-IDDC-SEND-KEY TO SEND-WS-IDDC                              
103200                                W-IDDC-B6                                 
103300                                                                          
103400     PERFORM IMS-GU-WDB601-SEND                                           
103500     IF SEND-DCS-CDC    OR SEND-DCS-DDC                                   
103600     OR SEND-DCS-SDC    OR SEND-DCS-AUSTRALIA                             
103700     OR SEND-DCS-NDC-NA OR SEND-DCS-JAPAN                                 
103800     OR SEND-DCS-LAND-NON-VCC-OWNED                                       
103900        CONTINUE                                                          
104000     ELSE                                                                 
104100        MOVE 'IDDC' TO RESP-IDELMT-ERROR                                  
104200        MOVE NEJ TO NYCKLAR-SW                                            
104300     END-IF                                                               
104400                                                                          
104500     IF SEND-WS-IDDC = REC-WS-IDDC                                        
104600        MOVE 'IDDC' TO RESP-IDELMT-ERROR                                  
104700        MOVE NEJ TO NYCKLAR-SW                                            
104800     END-IF                                                               
104900                                                                          
105000     MOVE WS-IDFAKT    TO RESP-IDFAKT-KEY                                 
105100     MOVE WS-IDORDNR   TO RESP-IDORDNR-KEY                                
105200     MOVE WS-IDKUNDNR  TO RESP-IDKUNDNR-KEY                               
105300     MOVE WS-IDKOLLI   TO RESP-IDKOLLI-KEY                                
105400     MOVE SEND-WS-IDDC TO RESP-IDDC-SEND-KEY                              
105500     MOVE REC-WS-IDDC  TO RESP-IDDC-KEY                                   
105600     INSPECT RESP-IDFAKT-KEY REPLACING LEADING ZERO BY SPACE              
105700     INSPECT RESP-IDORDNR-KEY REPLACING LEADING ZERO BY SPACE             
105800     INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE            
105900     INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE             
106000     INSPECT RESP-IDDC-SEND-KEY REPLACING LEADING ZERO BY SPACE           
106100                                                                          
106200     IF NYCKLAR-OK                                                        
106300        MOVE IDFAKT-WS TO W-IDFAKT                                        
106400        PERFORM IMS-GHU-WL630511                                          
106500        IF SEGMENT-FINNS                                                  
106600           IF 6306-IDDC-REC = REC-WS-IDDC                                 
106700           AND 6306-IDDC-SEND = SEND-WS-IDDC AND 6306-FLKLAR = 'N'        
106800              CONTINUE                                                    
106900           ELSE                                                           
107000              MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                          
107100              MOVE NEJ TO NYCKLAR-SW                                      
107200           END-IF                                                         
107300        END-IF                                                            
107400     END-IF                                                               
107500                                                                          
107600     IF NYCKLAR-FEL                                                       
107700        IF RESP-IDELMT-ERROR = 'KDPGMACT'                                 
107800           MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
107900        ELSE                                                              
108000           MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                    
108100        END-IF                                                            
108200     END-IF                                                               
108300                                                                          
108400     IF NYCKLAR-OK                                                        
108500        MOVE REC-WS-IDDC TO W-IDDC-B6                                     
108600        PERFORM IMS-GU-WDB601-REC                                         
108700***  VALIDATE IDTRACK IF EXITS OR NOT                                     
108800        IF REC-DCS-FLTRACK = 'J'                                          
108900           MOVE 'J'  TO RESP-FLTRACK                                      
109000           PERFORM BB-KOLLA-IDTRACK                                       
109100        ELSE                                                              
109200           MOVE 'N'  TO RESP-FLTRACK                                      
109300        END-IF                                                            
109400     END-IF                                                               
109500                                                                          
109600                                                                          
109700     .                                                                    
109800     EJECT                                                                
109900                                                                          
110000 BA-FIXA-FAKTURANUMMER SECTION.                                           
110100     PERFORM DB2-SELECT-TP6FAKT                                           
110200     IF LINES-MISSING  OR TABELL-TOM                                      
110300       MOVE 1 TO RAD-IDFAKT                                               
110400       PERFORM DB2-INSERT-TP6FAKT                                         
110500     ELSE                                                                 
110600       IF RAD-IDFAKT > 98                                                 
110700         MOVE 1 TO RAD-IDFAKT                                             
110800       ELSE                                                               
110900         COMPUTE RAD-IDFAKT = RAD-IDFAKT + 1                              
111000       END-IF                                                             
112000       PERFORM DB2-UPDATE-TP6FAKT                                         
113000     END-IF                                                               
114000     MOVE RAD-IDFAKT TO WS-IDFAKT                                         
115000     .                                                                    
116000     EJECT                                                                
117000                                                                          
118000 BB-KOLLA-IDTRACK SECTION.                                                
119000                                                                          
120000                                                                          
130000     MOVE IDFAKT-WS          TO W-SEQA-IDFAKT-MIN4                        
131000                                W-SEQA-IDFAKT-MAX4                        
131100     MOVE SPACE              TO W-SEQA-IDKUNDRF-MIN4                      
131200     MOVE ZERO               TO W-SEQA-IDKUNDNR-MIN4                      
131300                                W-SEQA-IDKOLLI-MIN4                       
131400                                W-SEQA-IDARTNR-MIN4                       
131500                                W-SEQA-DAINLEV-MIN4                       
131600     MOVE '9999999999'       TO W-SEQA-IDKUNDRF-MAX4                      
131700     MOVE 9999999            TO W-SEQA-IDKUNDNR-MAX4                      
131800     MOVE 99999              TO W-SEQA-IDKOLLI-MAX4                       
131900     MOVE 999999999          TO W-SEQA-IDARTNR-MAX4                       
132000     MOVE 9999999999999999   TO W-SEQA-DAINLEV-MAX4                       
132100                                                                          
132200     PERFORM IMS-GU-WDL6A1-04                                             
132300                                                                          
132400     IF SEGMENT-FINNS                                                     
132500       MOVE WDL6A1-SEQA-IDARTNR  TO W-IDARTNR                             
132600       MOVE WDL6A1-SEQA-IDDC     TO W-IDDC                                
132700       MOVE WDL6A1-SEQA-DAINLEV  TO W-DAINLEV                             
132800       PERFORM IMS-GU-WDL623                                              
132900       IF SEGMENT-SAKNAS                                                  
133000          MOVE 'J' TO RESP-FLOLD-IDTRACK                                  
134000       ELSE                                                               
135000          MOVE TINL-IDTRACK TO REQU-IDTRACK                               
136000                               RESP-IDTRACK                               
137000          MOVE 'N' TO RESP-FLOLD-IDTRACK                                  
138000       END-IF                                                             
139000     ELSE                                                                 
140000       MOVE IDFAKT-WS          TO W-IDFAKT-SEQC-MIN                       
150000                                  W-IDFAKT-SEQC-MAX                       
160000       MOVE REQU-IDDC-KEY      TO W-IDDC-SEQC-MIN                         
170000                                  W-IDDC-SEQC-MAX                         
180000       PERFORM IMS-GU-WDL6C1                                              
190000       IF SEGMENT-FINNS                                                   
200000         MOVE SEQC-IDARTNR  TO W-IDARTNR                                  
210000         MOVE SEQC-IDDC     TO W-IDDC                                     
220000         MOVE SEQC-DAINLEV  TO W-DAINLEV                                  
230000         PERFORM IMS-GU-WDL623                                            
240000         IF SEGMENT-SAKNAS                                                
250000            MOVE 'J' TO RESP-FLOLD-IDTRACK                                
251000         ELSE                                                             
252000            MOVE TINL-IDTRACK TO REQU-IDTRACK                             
253000                                 RESP-IDTRACK                             
254000            MOVE 'N' TO RESP-FLOLD-IDTRACK                                
255000         END-IF                                                           
256000       ELSE                                                               
256100          MOVE 'J' TO RESP-FLOLD-IDTRACK                                  
256200       END-IF                                                             
256300     END-IF                                                               
256400     .                                                                    
256500     EJECT                                                                
256600 G-KOLLA-INPUT SECTION.                                                   
256700     MOVE 'G-KOLLA-INPUT '  TO CURRENT-SECTION                            
256800                                                                          
256900     MOVE +1 TO TAB-IX                                                    
257000     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
258000        MOVE ZERO TO WS-ADLAGOMR(TAB-IX)                                  
259000                     WS-ADGANG(TAB-IX)                                    
260000                     WS-ADPLATS(TAB-IX)                                   
261000        ADD +1 TO TAB-IX                                                  
262000     END-PERFORM                                                          
263000                                                                          
264000     MOVE JA TO INDATA-SW                                                 
264100     MOVE NEJ TO INDATA-FINNS                                             
264200                                                                          
264300     MOVE REC-WS-IDDC TO W-IDDC-B6                                        
264400     PERFORM IMS-GU-WDB601-REC                                            
264500     MOVE +1 TO TAB-IX                                                    
264600     PERFORM UNTIL TAB-IX > TAB-IX-MAX OR (INDATA-FINNS = JA)             
264700        IF REQU-TABELL(TAB-IX) = ALL '+'                                  
264800           CONTINUE                                                       
264900        ELSE                                                              
265000           MOVE JA TO INDATA-FINNS                                        
265100        END-IF                                                            
265200        ADD +1 TO TAB-IX                                                  
265300     END-PERFORM                                                          
265400                                                                          
265500     IF INDATA-FINNS = NEJ                                                
265600     AND REQU-KOLLI-KLAR = ALL '+'                                        
265700     AND REQU-IDUSER-003 = ALL '+'                                        
265800        MOVE NO-DATA-ENTERED TO RESP-IDMSG-ERROR                          
265900        MOVE NEJ TO INDATA-SW                                             
266000     ELSE                                                                 
267000        MOVE IDFAKT-WS TO W-IDFAKT                                        
268000        MOVE REC-WS-IDDC TO W-6301-IDDC                                   
269000        PERFORM IMS-GHU-WL630111                                          
270000        IF SEGMENT-FINNS                                                  
280000           MOVE 6302-TIFAKT    TO WS-TIFAKT                               
290000           MOVE 6302-IDLEVNR   TO WS-IDLEVNR                              
300000           MOVE 6302-IDDISTR   TO WS-6302-IDDISTR                         
310000                                  WS-IDDISTR                              
320000           MOVE 6302-IDKUNDNR  TO WS-6302-IDKUNDNR                        
330000           MOVE 6302-IDDC-LEV  TO WS-6302-IDDC-LEV                        
340000           IF 6302-IDDC-SEND = SEND-WS-IDDC                               
350000              CONTINUE                                                    
360000           ELSE                                                           
361000              MOVE NEJ TO INDATA-SW                                       
362000              MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                          
363000              MOVE KEYS-ARE-MISSING TO RESP-IDMSG-ERROR                   
364000           END-IF                                                         
365000        ELSE                                                              
366000          IF WS-IDFAKT < 100                                              
367000            MOVE ZERO  TO WS-TIFAKT                                       
368000            MOVE SPACE TO WS-IDLEVNR                                      
369000            MOVE ZERO  TO WS-6302-IDDISTR                                 
370000                          WS-IDDISTR                                      
370100            MOVE ZERO  TO WS-6302-IDKUNDNR                                
370200            MOVE SPACE TO WS-6302-IDDC-LEV                                
370300                                                                          
370400*** ETT NYTT KOLLI PÅ EJ AVSLUTAD FIKTIV FAKTURA.                         
370500*** SPARAS FÖR ATT SÄTTA 6306-FLKLAR = JA                                 
370600            MOVE JA           TO WS-FIKT-FAKT-ANV                         
370700          ELSE                                                            
370800            MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                            
370900            MOVE KEYS-ARE-MISSING TO RESP-IDMSG-ERROR                     
371000            MOVE NEJ TO INDATA-SW                                         
371100          END-IF                                                          
371200        END-IF                                                            
371300                                                                          
371400        IF INDATA-OK                                                      
371500          IF REQU-KOLLI-KLAR = ALL '+' OR SPACE                           
371600             MOVE 'KOLLI-KLAR' TO RESP-IDELMT-ERROR                       
371700             MOVE NEJ TO INDATA-SW                                        
371800             MOVE IS-INVALID TO RESP-IDMSG-ERROR                          
371900          ELSE                                                            
372000            MOVE REQU-KOLLI-KLAR TO RESP-KOLLI-KLAR                       
372100            IF REQU-KOLLI-KLAR = 'X' OR 'Y' OR 'J' OR 'N'                 
372200               IF REQU-KOLLI-KLAR = 'X' OR 'Y' OR 'J'                     
372300                  CONTINUE                                                
372400               ELSE                                                       
372500                 IF REQU-KOLLI-KLAR = 'N'                                 
372600                   IF REQU-IDMSG-INFO =                                   
372700                      CONFIRM-NOT-LAST-LINE-IN-CASE                       
372800                     CONTINUE                                             
372900                   ELSE                                                   
373000                     MOVE NEJ TO INDATA-SW                                
373100                     MOVE CONFIRM-NOT-LAST-LINE-IN-CASE                   
373200                          TO RESP-IDMSG-INFO                              
373300                   END-IF                                                 
373400                 END-IF                                                   
373500               END-IF                                                     
373600            ELSE                                                          
373700               MOVE 'KOLLI-KLAR' TO RESP-IDELMT-ERROR                     
373800               MOVE NEJ TO INDATA-SW                                      
373900            END-IF                                                        
374000          END-IF                                                          
374100                                                                          
374200          IF REC-DCS-FLTRACK = 'J' OR 'Y'                                 
374300            IF REQU-KOLLI-KLAR = 'Y' OR 'N'                               
374400               IF REQU-IDTRACK = ALL '+' OR SPACE                         
374500                  PERFORM GA-KOLLA-IDTRACK                                
374600               ELSE                                                       
374700                  MOVE REQU-IDTRACK TO RESP-IDTRACK                       
374800               END-IF                                                     
374900            END-IF                                                        
375000          END-IF                                                          
375100                                                                          
375200          MOVE NEJ TO WS-RAD-IFYLLD                                       
375300          MOVE +1 TO RAD-IX                                               
375400          PERFORM UNTIL RAD-IX > MAX-RAD                                  
375500                                                                          
375600          IF REQU-IDARTNR(RAD-IX) NOT = ALL '+'                           
375700          AND REQU-IDARTNR(RAD-IX) > 0                                    
375800             MOVE REQU-IDARTNR(RAD-IX) TO RESP-IDARTNR(RAD-IX)            
375900             INSPECT REQU-IDARTNR(RAD-IX) REPLACING LEADING SPACE         
376000                                 BY ZERO                                  
376100             IF REQU-IDARTNR(RAD-IX) NOT NUMERIC                          
376200                MOVE RAD-IX TO INDX-DISPLAY                               
376300                MOVE SPACE TO RESP-IDELMT-ERROR                           
376400                STRING 'IDARTNR*' INDX-DISPLAY DELIMITED BY SIZE          
376500                     INTO RESP-IDELMT-ERROR                               
376600                MOVE 'ART' TO RESP-IDMSG-ERROR-LINE(RAD-IX)               
376700                MOVE IS-INVALID TO  RESP-IDMSG-ERROR                      
376800                MOVE NEJ TO INDATA-SW                                     
376900             ELSE                                                         
377000                MOVE JA TO WS-RAD-IFYLLD                                  
377100                MOVE REQU-IDARTNR(RAD-IX) TO WS-IDARTNR                   
377200                IF WS-IDARTNR = ZERO                                      
377300                  MOVE RAD-IX TO INDX-DISPLAY                             
377400                  MOVE SPACE TO RESP-IDELMT-ERROR                         
377500                  STRING 'IDARTNR*' INDX-DISPLAY DELIMITED BY SIZE        
377600                     INTO RESP-IDELMT-ERROR                               
377700                  MOVE 'ART' TO RESP-IDMSG-ERROR-LINE(RAD-IX)             
377800                  MOVE IS-INVALID TO  RESP-IDMSG-ERROR                    
377900                  MOVE NEJ TO INDATA-SW                                   
378000                END-IF                                                    
378100             END-IF                                                       
378200          END-IF                                                          
378300                                                                          
378400          IF REQU-KVANTMOT(RAD-IX) NOT = ALL '+'                          
378500          AND REQU-KVANTMOT(RAD-IX) > 0                                   
378600             MOVE REQU-KVANTMOT(RAD-IX) TO RESP-KVANTMOT(RAD-IX)          
378700             INSPECT REQU-KVANTMOT(RAD-IX) REPLACING LEADING SPACE        
378800                                        BY ZERO                           
378900             IF REQU-KVANTMOT(RAD-IX) NOT NUMERIC                         
379000                MOVE RAD-IX TO INDX-DISPLAY                               
379100                MOVE SPACE TO RESP-IDELMT-ERROR                           
379200                STRING 'KVANTMOT*' INDX-DISPLAY DELIMITED BY SIZE         
379300                    INTO RESP-IDELMT-ERROR                                
379400                MOVE IS-INVALID TO  RESP-IDMSG-ERROR                      
379500                MOVE 'QV'       TO RESP-IDMSG-ERROR-LINE(RAD-IX)          
379600                MOVE NEJ TO INDATA-SW                                     
379700             ELSE                                                         
379800                IF REQU-KVANTMOT(RAD-IX) = ZERO                           
379900                  MOVE RAD-IX TO INDX-DISPLAY                             
380000                  MOVE SPACE TO RESP-IDELMT-ERROR                         
380100                 STRING 'KVANTMOT*' INDX-DISPLAY DELIMITED BY SIZE        
380200                    INTO RESP-IDELMT-ERROR                                
380300                  MOVE 'QV'       TO RESP-IDMSG-ERROR-LINE(RAD-IX)        
380400                  MOVE IS-INVALID TO RESP-IDMSG-ERROR                     
380500                  MOVE NEJ TO INDATA-SW                                   
380600                END-IF                                                    
380700             END-IF                                                       
380800          END-IF                                                          
380900                                                                          
381000          IF REQU-KVSKROT(RAD-IX) NOT = ALL '+'                           
381100          AND REQU-KVSKROT(RAD-IX) > 0                                    
381200             MOVE REQU-KVSKROT(RAD-IX) TO RESP-KVSKROT(RAD-IX)            
381300             INSPECT REQU-KVSKROT(RAD-IX) REPLACING LEADING SPACE         
381400                                         BY ZERO                          
381500             IF REQU-KVSKROT(RAD-IX) NOT NUMERIC                          
381600                MOVE RAD-IX TO INDX-DISPLAY                               
381700                MOVE SPACE TO RESP-IDELMT-ERROR                           
381800                STRING 'KVSKROT*' INDX-DISPLAY DELIMITED BY SIZE          
381900                    INTO RESP-IDELMT-ERROR                                
382000                MOVE IS-INVALID TO RESP-IDMSG-ERROR                       
382100                MOVE 'QV'       TO RESP-IDMSG-ERROR-LINE(RAD-IX)          
382200                MOVE NEJ TO INDATA-SW                                     
382300             ELSE                                                         
382400                IF REQU-KVSKROT(RAD-IX) = ZERO                            
382500                  MOVE RAD-IX TO INDX-DISPLAY                             
382600                  MOVE SPACE TO RESP-IDELMT-ERROR                         
382700                  STRING 'KVSKROT*' INDX-DISPLAY DELIMITED BY SIZE        
382800                    INTO RESP-IDELMT-ERROR                                
382900                  MOVE 'QV'       TO RESP-IDMSG-ERROR-LINE(RAD-IX)        
383000                  MOVE IS-INVALID TO RESP-IDMSG-ERROR                     
383100                  MOVE NEJ TO INDATA-SW                                   
383200                END-IF                                                    
383300             END-IF                                                       
383400          END-IF                                                          
383500                                                                          
383600          IF  REQU-KVANTMOT(RAD-IX) = ALL '+'                             
383700          AND REQU-KVSKROT(RAD-IX)= ALL '+'                               
383800            IF REQU-IDARTNR(RAD-IX) = ALL '+'                             
383900               CONTINUE                                                   
384000            ELSE                                                          
384100               MOVE RAD-IX TO INDX-DISPLAY                                
384200               MOVE SPACE TO RESP-IDELMT-ERROR                            
384300               STRING 'IDARTNR*' INDX-DISPLAY DELIMITED BY SIZE           
384400                    INTO RESP-IDELMT-ERROR                                
384500               MOVE IS-INVALID TO RESP-IDMSG-ERROR                        
384600               MOVE NEJ TO INDATA-SW                                      
384700               MOVE 'ART' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                
384800               MOVE IS-INVALID TO  RESP-IDMSG-ERROR                       
384900            END-IF                                                        
385000          END-IF                                                          
385100                                                                          
385200          IF REQU-ADLAGOMR(RAD-IX) NOT = ALL '+'                          
385300             MOVE REQU-ADLAGOMR(RAD-IX) TO RESP-ADLAGOMR(RAD-IX)          
385400             INSPECT REQU-ADLAGOMR(RAD-IX) REPLACING                      
385500                     LEADING SPACE BY ZERO                                
385600             IF REQU-ADLAGOMR(RAD-IX) NOT NUMERIC                         
385700                MOVE RAD-IX TO INDX-DISPLAY                               
385800                MOVE SPACE TO RESP-IDELMT-ERROR                           
385900                STRING 'ADLAGOMR*' INDX-DISPLAY DELIMITED BY SIZE         
386000                     INTO RESP-IDELMT-ERROR                               
386100                MOVE IS-INVALID TO RESP-IDMSG-ERROR                       
386200                MOVE 'OMR' TO RESP-IDMSG-ERROR-LINE(RAD-IX)               
386300                MOVE NEJ TO INDATA-SW                                     
386400             ELSE                                                         
386500                MOVE REQU-ADLAGOMR(RAD-IX) TO WS-ADLAGOMR(RAD-IX)         
386600             END-IF                                                       
386700          END-IF                                                          
386800                                                                          
386900          IF REQU-ADGANG(RAD-IX) NOT = ALL '+'                            
387000***       AND REQU-ADGANG(RAD-IX) > 0                                     
387100             MOVE REQU-ADGANG(RAD-IX) TO RESP-ADGANG(RAD-IX)              
387200             INSPECT REQU-ADGANG(RAD-IX) REPLACING                        
387300                    LEADING SPACE BY ZERO                                 
387400             IF REQU-ADGANG(RAD-IX) NOT NUMERIC                           
387500                MOVE RAD-IX TO INDX-DISPLAY                               
387600                MOVE SPACE TO RESP-IDELMT-ERROR                           
387700                STRING 'ADGANG*' INDX-DISPLAY DELIMITED BY SIZE           
387800                     INTO RESP-IDELMT-ERROR                               
387900                MOVE IS-INVALID TO RESP-IDMSG-ERROR                       
388000                MOVE 'PL' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                
388100                MOVE NEJ TO INDATA-SW                                     
388200             ELSE                                                         
388300                MOVE REQU-ADGANG(RAD-IX) TO WS-ADGANG(RAD-IX)             
388400             END-IF                                                       
388500          END-IF                                                          
388600                                                                          
388700          IF REQU-ADPLATS(RAD-IX) NOT = ALL '+'                           
388800***       AND REQU-ADPLATS(RAD-IX) > 0                                    
388900             MOVE REQU-ADPLATS(RAD-IX) TO RESP-ADPLATS(RAD-IX)            
389000             INSPECT REQU-ADPLATS(RAD-IX) REPLACING                       
389100                     LEADING SPACE BY ZERO                                
389200             IF REQU-ADPLATS(RAD-IX) NOT NUMERIC                          
389300                MOVE RAD-IX TO INDX-DISPLAY                               
389400                MOVE SPACE TO RESP-IDELMT-ERROR                           
389500                STRING 'ADPLATS*' INDX-DISPLAY DELIMITED BY SIZE          
389600                     INTO RESP-IDELMT-ERROR                               
389700                MOVE IS-INVALID TO RESP-IDMSG-ERROR                       
389800                MOVE 'PL' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                
389900                MOVE NEJ TO INDATA-SW                                     
390000             ELSE                                                         
390100                MOVE REQU-ADPLATS(RAD-IX) TO WS-ADPLATS(RAD-IX)           
390200             END-IF                                                       
390300          END-IF                                                          
390400                                                                          
390500*-- TESTA OM DET ÄR EN STUDSFAKTURA CN-TO-US DISTR 914X                   
390600*-- OCH US-TO-CN DISTR 927X, FAKT 1 BLIR WEB-RAPPORT W41841-001           
390700*-- MAN TAR EMOT PÅ FAKTURA 2 I USA/KINA MED IDDC-SEND = 11               
390800*-- FÖR FIKTIV FAKTURA FÅR MAN HÄMTA IDDC-LEV, FYSISKT LEV.DC             
390900          IF INDATA-OK AND (RAD-IX = +1)                                  
391000          AND REQU-IDARTNR(RAD-IX) NOT = ALL '+'                          
391100            IF REC-DCS-NDC-NA AND REC-DCS-USA                             
391200              IF SEND-DCS-CDC                                             
391300                MOVE WS-IDARTNR  TO W-IDARTNR                             
391400                MOVE REC-WS-IDDC TO W-IDDC                                
391500                PERFORM IMS-GET-WDK711                                    
391600                IF SEGMENT-FINNS                                          
391700                  MOVE SLAG-IDDC-REF  TO WS-IDDC                          
391800                  IF NDC-CN                                               
391900                    IF WS-FIKT-FAKT-ANV = JA                              
392000                      IF WS-6302-IDDC-LEV = SPACE                         
392100                        MOVE SLAG-IDDC-REF TO WS-6302-IDDC-LEV            
392200                      END-IF                                              
392300                    ELSE                                                  
392400                      IF SLAG-IDDC-REF = WS-6302-IDDC-LEV                 
392500                        CONTINUE                                          
392600                      ELSE                                                
392700                        MOVE RAD-IX TO INDX-DISPLAY                       
392800                        MOVE SPACE TO RESP-IDELMT-ERROR                   
392900                        STRING 'IDARTNR*' INDX-DISPLAY                    
393000                        DELIMITED BY SIZE INTO RESP-IDELMT-ERROR          
393100                        MOVE 'INV' TO                                     
393200                              RESP-IDMSG-ERROR-LINE(RAD-IX)               
393300                        MOVE IS-INVALID TO  RESP-IDMSG-ERROR              
393400                        MOVE NEJ TO INDATA-SW                             
393500                      END-IF                                              
393600                    END-IF                                                
393700                  ELSE                                                    
393800                    IF WS-6302-IDDC-LEV = SPACE                           
393900                      CONTINUE                                            
394000                    ELSE                                                  
394100                      MOVE RAD-IX TO INDX-DISPLAY                         
394200                      MOVE SPACE TO RESP-IDELMT-ERROR                     
394300                      STRING 'IDARTNR*' INDX-DISPLAY                      
394400                      DELIMITED BY SIZE INTO RESP-IDELMT-ERROR            
394500                      MOVE 'INV' TO                                       
394600                            RESP-IDMSG-ERROR-LINE(RAD-IX)                 
394700                      MOVE IS-INVALID TO  RESP-IDMSG-ERROR                
394800                      MOVE NEJ TO INDATA-SW                               
394900                    END-IF                                                
395000                  END-IF                                                  
395100                END-IF                                                    
395200              END-IF                                                      
395300            END-IF                                                        
395400            IF REC-DCS-NDC-CN                                             
395500              IF SEND-DCS-CDC                                             
395600                MOVE WS-IDARTNR  TO W-IDARTNR                             
395700                MOVE REC-WS-IDDC TO W-IDDC                                
395800                PERFORM IMS-GET-WDK711                                    
395900                IF SEGMENT-FINNS                                          
396000                  MOVE SLAG-IDDC-REF  TO WS-IDDC                          
396100                  IF NDC-US                                               
396200                    IF WS-FIKT-FAKT-ANV = JA                              
396300                      IF WS-6302-IDDC-LEV = SPACE                         
396400                        MOVE SLAG-IDDC-REF  TO WS-6302-IDDC-LEV           
396500                      END-IF                                              
396600                    ELSE                                                  
396700                      IF SLAG-IDDC-REF = WS-6302-IDDC-LEV                 
396800                        CONTINUE                                          
396900                      ELSE                                                
397000                        MOVE RAD-IX TO INDX-DISPLAY                       
397100                        MOVE SPACE TO RESP-IDELMT-ERROR                   
397200                        STRING 'IDARTNR*' INDX-DISPLAY                    
397300                        DELIMITED BY SIZE INTO RESP-IDELMT-ERROR          
397400                        MOVE 'INV' TO                                     
397500                              RESP-IDMSG-ERROR-LINE(RAD-IX)               
397600                        MOVE IS-INVALID TO  RESP-IDMSG-ERROR              
397700                        MOVE NEJ TO INDATA-SW                             
397800                      END-IF                                              
397900                    END-IF                                                
398000                  ELSE                                                    
398100                    IF WS-6302-IDDC-LEV = SPACE                           
398200                      CONTINUE                                            
398300                    ELSE                                                  
398400                      MOVE RAD-IX TO INDX-DISPLAY                         
398500                      MOVE SPACE TO RESP-IDELMT-ERROR                     
398600                      STRING 'IDARTNR*' INDX-DISPLAY                      
398700                      DELIMITED BY SIZE INTO RESP-IDELMT-ERROR            
398800                      MOVE 'INV' TO                                       
398900                            RESP-IDMSG-ERROR-LINE(RAD-IX)                 
399000                      MOVE IS-INVALID TO  RESP-IDMSG-ERROR                
399100                      MOVE NEJ TO INDATA-SW                               
399200                    END-IF                                                
399300                  END-IF                                                  
399400                END-IF                                                    
399500              END-IF                                                      
399600            END-IF                                                        
399700            IF REC-DCS-CDC                                                
399800              MOVE WS-IDARTNR  TO W-IDARTNR                               
399900              PERFORM IMS-GHU-WLARTC11                                    
400000              IF SEGMENT-FINNS                                            
400100                IF CLAG-IDDC-REF = SPACE                                  
400200                  IF SEND-DCS-NDC-CN OR SEND-DCS-USA                      
400401                    MOVE RAD-IX TO INDX-DISPLAY                           
400501                    MOVE SPACE TO RESP-IDELMT-ERROR                       
400601                    STRING 'IDARTNR*' INDX-DISPLAY                        
400701                       DELIMITED BY SIZE INTO RESP-IDELMT-ERROR           
400801                    MOVE 'INV' TO                                         
400901                      RESP-IDMSG-ERROR-LINE(RAD-IX)                       
401001                    MOVE IS-INVALID TO  RESP-IDMSG-ERROR                  
401101                    MOVE NEJ TO INDATA-SW                                 
401900                  END-IF                                                  
402000                ELSE                                                      
402100                  MOVE CLAG-IDDC-REF  TO WS-IDDC                          
402200                  IF NDC-CN                                               
402300                    IF SEND-DCS-NDC-CN                                    
402400                      CONTINUE                                            
402500                    ELSE                                                  
402701                      MOVE RAD-IX TO INDX-DISPLAY                         
402801                      MOVE SPACE TO RESP-IDELMT-ERROR                     
402901                      STRING 'IDARTNR*' INDX-DISPLAY                      
403001                         DELIMITED BY SIZE INTO RESP-IDELMT-ERROR         
403101                      MOVE 'INV' TO                                       
403201                         RESP-IDMSG-ERROR-LINE(RAD-IX)                    
403301                      MOVE IS-INVALID TO  RESP-IDMSG-ERROR                
403401                      MOVE NEJ TO INDATA-SW                               
404200                    END-IF                                                
404300                  END-IF                                                  
404400                  IF NDC-US                                               
404500                    IF SEND-DCS-USA                                       
404600                      CONTINUE                                            
404700                    ELSE                                                  
404901                      MOVE RAD-IX TO INDX-DISPLAY                         
405001                      MOVE SPACE TO RESP-IDELMT-ERROR                     
405101                      STRING 'IDARTNR*' INDX-DISPLAY                      
405201                         DELIMITED BY SIZE INTO RESP-IDELMT-ERROR         
405301                      MOVE 'INV' TO                                       
405401                         RESP-IDMSG-ERROR-LINE(RAD-IX)                    
405501                      MOVE IS-INVALID TO  RESP-IDMSG-ERROR                
405601                      MOVE NEJ TO INDATA-SW                               
406400                    END-IF                                                
406500                  END-IF                                                  
406600                END-IF                                                    
406700              END-IF                                                      
406800            END-IF                                                        
406900          END-IF                                                          
407000                                                                          
407100          IF INDATA-OK                                                    
407200          AND REQU-IDARTNR(RAD-IX) NOT = ALL '+'                          
407300             IF  (REQU-ADLAGOMR(RAD-IX) = ALL '+' OR SPACE)               
407400             AND (REQU-ADGANG(RAD-IX)  = ALL '+' OR SPACE)                
407500             AND (REQU-ADPLATS(RAD-IX) = ALL '+' OR SPACE)                
407600                                                                          
407700                MOVE WS-IDARTNR  TO W-IDARTNR                             
407800                MOVE REC-WS-IDDC TO W-IDDC                                
407901                IF REC-DCS-CDC                                            
408001                   PERFORM IMS-GHU-WLARTC11                               
408101                   IF SEGMENT-FINNS                                       
408201                      IF CLAG-ADLAGOMR = ZERO                             
408301                      AND CLAG-ADGANG = ZERO                              
408401                      AND CLAG-ADPLATS = ZERO                             
408601                       MOVE RAD-IX TO INDX-DISPLAY                        
408701                       MOVE SPACE TO RESP-IDELMT-ERROR                    
408801                       STRING 'ADLAGOMR*' INDX-DISPLAY                    
408901                         DELIMITED BY SIZE                                
409001                         INTO RESP-IDELMT-ERROR                           
409101                       MOVE IS-INVALID TO RESP-IDMSG-ERROR                
409201                       MOVE 'OMR' TO RESP-IDMSG-ERROR-LINE(RAD-IX)        
409301                       MOVE NEJ TO INDATA-SW                              
410001                      END-IF                                              
410701                   END-IF                                                 
410801                ELSE                                                      
410901                   PERFORM IMS-GET-WDK711                                 
411001                   IF SEGMENT-FINNS                                       
411101                      IF  SLAG-ADLAGOMR = ZERO                            
411201                       AND SLAG-ADGANG   = ZERO                           
411301                       AND SLAG-ADPLATS  = ZERO                           
411401                       MOVE RAD-IX TO INDX-DISPLAY                        
411501                       MOVE SPACE TO RESP-IDELMT-ERROR                    
411601                       STRING 'ADLAGOMR*' INDX-DISPLAY                    
411701                         DELIMITED BY SIZE                                
411801                         INTO RESP-IDELMT-ERROR                           
411901                       MOVE IS-INVALID TO RESP-IDMSG-ERROR                
412001                       MOVE 'OMR' TO RESP-IDMSG-ERROR-LINE(RAD-IX)        
412101                       MOVE NEJ TO INDATA-SW                              
412201                      END-IF                                              
412301                   ELSE                                                   
412401                      MOVE RAD-IX TO INDX-DISPLAY                         
412501                      MOVE SPACE TO RESP-IDELMT-ERROR                     
412601                      STRING 'ADLAGOMR*' INDX-DISPLAY                     
412701                       DELIMITED BY SIZE                                  
412801                       INTO RESP-IDELMT-ERROR                             
412901                      MOVE 'OMR' TO RESP-IDMSG-ERROR-LINE(RAD-IX)         
413001                      MOVE IS-INVALID TO RESP-IDMSG-ERROR                 
413101                      MOVE NEJ TO INDATA-SW                               
413201                   END-IF                                                 
413301                END-IF                                                    
413401             ELSE                                                         
413501                MOVE WS-IDARTNR  TO W-IDARTNR                             
413601                MOVE REC-WS-IDDC TO W-IDDC                                
413701                IF REC-DCS-CDC                                            
413801                  PERFORM IMS-GHU-WLARTC11                                
413901                  IF SEGMENT-FINNS                                        
414001                     IF CLAG-ADLAGOMR = ZERO                              
414101                     AND CLAG-ADGANG   = ZERO                             
414201                     AND CLAG-ADPLATS  = ZERO                             
414301                       CONTINUE                                           
414401                     ELSE                                                 
414501                       MOVE RAD-IX TO INDX-DISPLAY                        
414601                       MOVE SPACE TO RESP-IDELMT-ERROR                    
414701                       STRING 'ADLAGOMR*' INDX-DISPLAY                    
414801                         DELIMITED BY SIZE                                
414901                         INTO RESP-IDELMT-ERROR                           
415001                       MOVE 'OMR' TO RESP-IDMSG-ERROR-LINE(RAD-IX)        
415101                       MOVE NEJ TO INDATA-SW                              
415201                       MOVE IS-INVALID TO RESP-IDMSG-ERROR                
415301                     END-IF                                               
415401                  END-IF                                                  
415501                ELSE                                                      
415601                  PERFORM IMS-GET-WDK711                                  
415701                  IF SEGMENT-FINNS                                        
415801                     IF SLAG-ADLAGOMR = ZERO                              
415901                     AND SLAG-ADGANG   = ZERO                             
416001                     AND SLAG-ADPLATS  = ZERO                             
416101                       CONTINUE                                           
416201                     ELSE                                                 
416301                       MOVE RAD-IX TO INDX-DISPLAY                        
416401                       MOVE SPACE TO RESP-IDELMT-ERROR                    
416501                       STRING 'ADLAGOMR*' INDX-DISPLAY                    
416601                         DELIMITED BY SIZE                                
416701                         INTO RESP-IDELMT-ERROR                           
416801                       MOVE 'OMR' TO RESP-IDMSG-ERROR-LINE(RAD-IX)        
416901                       MOVE NEJ TO INDATA-SW                              
417001                       MOVE IS-INVALID TO RESP-IDMSG-ERROR                
417101                     END-IF                                               
417201                  END-IF                                                  
417301                END-IF                                                    
417401             END-IF                                                       
417501          END-IF                                                          
417601                                                                          
417701         IF INDATA-OK                                                     
417801            MOVE WS-IDARTNR TO W-IDARTNR                                  
417901            PERFORM IMS-GU-WLARTC01                                       
418001            IF SEGMENT-FINNS                                              
418101               IF K6-ART-KDERS-UTG = ZERO                                 
418201                  CONTINUE                                                
418301               ELSE                                                       
418401                  MOVE RAD-IX TO INDX-DISPLAY                             
418501                  MOVE SPACE TO RESP-IDELMT-ERROR                         
418601                  STRING 'IDARTNR*' INDX-DISPLAY                          
418701                     DELIMITED BY SIZE                                    
418801                     INTO RESP-IDELMT-ERROR                               
418901                  MOVE 'ART' TO RESP-IDMSG-ERROR-LINE(RAD-IX)             
419001                  MOVE IS-INVALID TO RESP-IDMSG-ERROR                     
419101                  MOVE NEJ TO INDATA-SW                                   
419201               END-IF                                                     
419301            ELSE                                                          
419401               MOVE RAD-IX TO INDX-DISPLAY                                
419501               MOVE SPACE TO RESP-IDELMT-ERROR                            
419601               STRING 'IDARTNR*' INDX-DISPLAY                             
419701                  DELIMITED BY SIZE                                       
419801                  INTO RESP-IDELMT-ERROR                                  
419901               MOVE 'ART' TO RESP-IDMSG-ERROR-LINE(RAD-IX)                
420001               MOVE IS-INVALID TO  RESP-IDMSG-ERROR                       
420101               MOVE NEJ TO INDATA-SW                                      
420201            END-IF                                                        
420301         END-IF                                                           
420401                                                                          
420501         ADD +1 TO RAD-IX                                                 
420601       END-PERFORM                                                        
420701                                                                          
420801      END-IF                                                              
420901                                                                          
421001                                                                          
421101     IF REQU-KOLLI-KLAR = 'X' OR 'Y' OR 'J' OR 'N'                        
421201        IF REQU-IDUSER-003 = ALL '+' OR SPACE                             
421301           IF REC-DCS-FLBINNUT = JA                                       
421401              MOVE NEJ          TO INDATA-SW                              
421501              MOVE 'IDANSTNR'   TO RESP-IDELMT-ERROR                      
421601              MOVE MUST-ENTER-EMP-ID                                      
421701                                TO RESP-IDMSG-ERROR                       
421801           ELSE                                                           
421901              MOVE SPACE        TO WS-IDUSER-003                          
422001           END-IF                                                         
422101        ELSE                                                              
422201           MOVE REQU-IDUSER-003 TO WS-IDUSER-003                          
422301                                   RESP-IDUSER-003                        
422401        END-IF                                                            
422501     ELSE                                                                 
422601        IF REQU-IDUSER-003 = ALL '+' OR SPACE                             
422701           MOVE SPACE TO WS-IDUSER-003                                    
422801        ELSE                                                              
422901           MOVE REQU-IDUSER-003 TO RESP-IDUSER-003                        
423001           MOVE 'KOLLI-KLAR' TO RESP-IDELMT-ERROR                         
423101           MOVE IS-INVALID TO RESP-IDMSG-ERROR                            
423201           MOVE NEJ TO INDATA-SW                                          
423301        END-IF                                                            
423401     END-IF                                                               
423501                                                                          
423601                                                                          
423701***  IF INDATA-FEL                                                        
423801***     IF RESP-IDMSG-INFO = (NO-DATA-ENTERED OR KEYS-ARE-MISSING         
423901***        OR CONFIRM-NOT-LAST-LINE-IN-CASE)                              
424001***        CONTINUE                                                       
424101***     ELSE                                                              
424201***        MOVE IS-INVALID TO RESP-IDMSG-ERROR                            
424301***     END-IF                                                            
424401***  END-IF                                                               
424501     END-IF                                                               
424601     .                                                                    
424701     EJECT                                                                
424801 GA-KOLLA-IDTRACK SECTION.                                                
424901                                                                          
425001     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
425101     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
425201     MOVE WS-IDFAKT       TO W-SEQA-IDFAKT-MIN                            
425301                             W-SEQA-IDFAKT-MAX                            
425401     MOVE WS-IDORDNR      TO W-SEQA-IDKUNDRF-MIN                          
425501                             W-SEQA-IDKUNDRF-MAX                          
425601     MOVE WS-IDKUNDNR     TO W-SEQA-IDKUNDNR-MIN                          
425701                             W-SEQA-IDKUNDNR-MAX                          
425801     MOVE WS-IDKOLLI      TO W-SEQA-IDKOLLI-MIN                           
425901                             W-SEQA-IDKOLLI-MAX                           
426001     MOVE '310'           TO W-IDPTYP                                     
426101     PERFORM IMS-GU-WDL6A1-FIRST-310                                      
426201                                                                          
426301     IF SEGMENT-FINNS                                                     
426401       MOVE WDL6A1-SEQA-IDARTNR  TO W-IDARTNR                             
426501       MOVE WDL6A1-SEQA-IDDC     TO W-IDDC                                
426601       MOVE WDL6A1-SEQA-DAINLEV  TO W-DAINLEV                             
426701       PERFORM IMS-GU-WDL623                                              
426801       IF SEGMENT-SAKNAS                                                  
426901          MOVE 'IDTRACK*' TO RESP-IDELMT-ERROR                            
427001          MOVE NEJ TO INDATA-SW                                           
427101          MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR                    
427201       ELSE                                                               
427301          MOVE TINL-IDTRACK TO REQU-IDTRACK                               
427401       END-IF                                                             
427501     ELSE                                                                 
427601       MOVE 'IDTRACK*' TO RESP-IDELMT-ERROR                               
427701       MOVE NEJ TO INDATA-SW                                              
427801       MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR                       
427901     END-IF                                                               
428001     .                                                                    
428101     EJECT                                                                
428201 H-UPPDATERA SECTION.                                                     
428301     MOVE REC-WS-IDDC TO W-IDDC                                           
428401                         WS-IDDC                                          
428501     MOVE NEJ         TO WS-A03-SKAPAD                                    
428601                                                                          
428701     MOVE +1 TO RAD-IX                                                    
428801     PERFORM UNTIL RAD-IX > MAX-RAD                                       
428901                                                                          
429001        MOVE ZERO TO WS-KVANTMOT                                          
429101                     WS-KVSKROT                                           
429201                                                                          
429301        IF REQU-IDARTNR(RAD-IX) NOT = ALL '+'                             
429401        AND REQU-IDARTNR(RAD-IX) NOT = SPACE                              
429501          INSPECT REQU-IDARTNR(RAD-IX) REPLACING                          
429601                    LEADING SPACE BY ZERO                                 
429701          MOVE REQU-IDARTNR(RAD-IX) TO WS-IDARTNR                         
429801          MOVE WS-IDARTNR           TO W-IDARTNR                          
429901          IF REQU-KVANTMOT(RAD-IX) NOT = ALL '+'                          
430001            INSPECT REQU-KVANTMOT(RAD-IX) REPLACING                       
430101                    LEADING SPACE BY ZERO                                 
430201            MOVE REQU-KVANTMOT(RAD-IX) TO WS-KVANTMOT                     
430301          END-IF                                                          
430401          IF REQU-KVSKROT(RAD-IX) NOT = ALL '+'                           
430501            INSPECT REQU-KVSKROT(RAD-IX) REPLACING                        
430601                    LEADING SPACE BY ZERO                                 
430701            MOVE REQU-KVSKROT(RAD-IX) TO WS-KVSKROT                       
430801          END-IF                                                          
430901                                                                          
431001          COMPUTE WS-SUMMA-KVANT = WS-KVSKROT + WS-KVANTMOT               
431101                                                                          
431201          IF WS-IDDISTR = ZERO                                            
431301            IF WS-6302-IDDC-LEV = SPACE                                   
431401              PERFORM HCX-FIXA-DISTRIKT                                   
431501            ELSE                                                          
431601              PERFORM HCY-STUDS-REFILL-DISTRIKT                           
431701            END-IF                                                        
431801          END-IF                                                          
431901          MOVE WS-IDDISTR TO DIST35-IDDISTR                               
432002                                                                          
432102          IF REC-DCS-CDC                                                  
432202             PERFORM IMS-GHU-WLARTC11                                     
432302             IF SEGMENT-FINNS                                             
432402                PERFORM HF-UPPDATERA-BEF-ART-CDC                          
432502                MOVE NEJ TO WS-NY-ART                                     
432602             END-IF                                                       
432702          ELSE                                                            
432902             PERFORM IMS-GET-WDK711                                       
433002             IF SEGMENT-FINNS                                             
433101* NOT VCCS                                                                
433202               IF XDC-NON-VCC-OWNED                                       
433301**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
433401**** REST OF FLOWS TO US IS IN LAB.                                       
433501**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
433601**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
433702                 OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                
433802                 PERFORM HA-UPPDATERA-BEF-ART                             
433902               ELSE                                                       
434001* VCCS                                                                    
434102                 PERFORM HA-UPPDATERA-BEF-ART-PV                          
434202               END-IF                                                     
434302               MOVE NEJ TO WS-NY-ART                                      
434402             ELSE                                                         
434502               IF XDC-NON-VCC-OWNED                                       
434601**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
434701**** REST OF FLOWS TO US IS IN LAB.                                       
434801**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
434901**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
435002                 OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                
435101* NOT VCCS                                                                
435202                 PERFORM HB-UPPDATERA-NY-ART                              
435302               ELSE                                                       
435401* VCCS                                                                    
435502                 PERFORM HB-UPPDATERA-NY-ART-PV                           
435602               END-IF                                                     
435702               MOVE JA TO WS-NY-ART                                       
435802             END-IF                                                       
435902          END-IF                                                          
436002* NOT VCCS                                                                
436102          IF XDC-NON-VCC-OWNED                                            
436202**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
436302**** REST OF FLOWS TO US IS IN LAB.                                       
436402**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
436502**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
436602          OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                       
436702            PERFORM HC-UPPDATERA-INLAGGNING                               
436802          ELSE                                                            
436902* VCCS                                                                    
437002            PERFORM HC-UPPDATERA-INLAGGNING-PV                            
437102            IF NDC-AU OR NDC-JP OR NDC-NA                                 
437202               IF WS-NY-ART = JA                                          
437302                  PERFORM HD-SKAPA-REFILLTRANS                            
437402               END-IF                                                     
437502            END-IF                                                        
437602          END-IF                                                          
437702        END-IF                                                            
437802                                                                          
437902        ADD +1 TO RAD-IX                                                  
438002     END-PERFORM                                                          
438102                                                                          
438202     IF WS-A03-SKAPAD = JA                                                
438302        IF SEND-DCS-CDC OR SEND-DCS-AUSTRALIA OR SEND-DCS-JAPAN           
438402        OR SEND-DCS-DDC OR SEND-DCS-SDC                                   
438502           CONTINUE                                                       
438602        ELSE                                                              
438702           IF WS-FIKT-FAKT-ANV = JA                                       
438802              IF REQU-KOLLI-KLAR = 'X' OR 'Y' OR 'J'                      
438902                 MOVE 'Y' TO EKOTRA03-FLSLUT                              
439002              END-IF                                                      
439102           END-IF                                                         
439202        END-IF                                                            
439302        PERFORM S041-SKRIV-A03                                            
439402     END-IF                                                               
439502                                                                          
439602     IF WS-FIKT-FAKT-ANV = JA                                             
439702        IF REQU-KOLLI-KLAR = 'X' OR 'Y' OR 'J'                            
439802           MOVE IDFAKT-WS TO W-IDFAKT                                     
439902           PERFORM IMS-GHU-WL630511                                       
440002           IF SEGMENT-FINNS                                               
440102              MOVE 'J' TO 6306-FLKLAR                                     
440202              PERFORM IMS-REPL-WL630511                                   
440302           END-IF                                                         
440402        END-IF                                                            
440502     END-IF                                                               
440602                                                                          
440702     MOVE SPACE              TO RESP-KOLLI-KLAR                           
440802                                RESP-IDUSER-003                           
440902                                RESP-IDTRACK                              
441002     MOVE +1 TO RAD-IX                                                    
441102     PERFORM UNTIL RAD-IX > MAX-RAD                                       
441202        MOVE SPACE           TO RESP-TAB(RAD-IX)                          
441302        ADD +1 TO RAD-IX                                                  
441402     END-PERFORM                                                          
441502                                                                          
441602     MOVE UPDATE-DONE TO RESP-IDMSG-INFO                                  
441702                                                                          
441802     IF PRINT                                                             
441902        IF PRINT-KOREA                                                    
442002        MOVE REQU-IDDC-KEY TO KLBL-IDDC                                   
442102        CALL W612KLBL   USING KLBL-W612KLBL                               
442202                              REQU-WZ01REQU                               
442302                              DISTRDOC-PCB                                
442402                              WDD3-PCB                                    
442502                              WDT4-PCB                                    
442602        END-IF                                                            
442702     END-IF                                                               
442802                                                                          
           IF FIRST-REC-TRANS          AND                                      
              REC-DCS-KDTRADP = 'BR12'                                          
              PERFORM S23-SEND-CLOSE                                            
                                                                                
           END-IF                                                               
442902     .                                                                    
443002     EJECT                                                                
443102                                                                          
443202 HA-UPPDATERA-BEF-ART-PV SECTION.                                         
443302     MOVE NEJ TO WS-UPPD-LOCB                                             
443402                                                                          
443502     IF REQU-ADLAGOMR(RAD-IX) = ALL '+' OR SPACE                          
443602        MOVE SLAG-ADLAGOMR       TO WS-ADLAGOMR(RAD-IX)                   
443702     ELSE                                                                 
443802        MOVE WS-ADLAGOMR(RAD-IX) TO SLAG-ADLAGOMR                         
443902                                    W-ADLAGOMR-LOCB                       
444002        MOVE JA TO WS-UPPD-LOCB                                           
444102     END-IF                                                               
444202     IF REQU-ADGANG(RAD-IX) = ALL '+' OR SPACE                            
444302        MOVE SLAG-ADGANG         TO WS-ADGANG(RAD-IX)                     
444402     ELSE                                                                 
444502        MOVE WS-ADGANG(RAD-IX)   TO SLAG-ADGANG                           
444602                                    W-ADGANG-LOCB                         
444702        MOVE JA TO WS-UPPD-LOCB                                           
444802     END-IF                                                               
444902     IF REQU-ADPLATS(RAD-IX) = ALL '+' OR SPACE                           
445002        MOVE SLAG-ADPLATS        TO WS-ADPLATS(RAD-IX)                    
445102     ELSE                                                                 
445202        MOVE WS-ADPLATS(RAD-IX)  TO SLAG-ADPLATS                          
445302                                    W-ADPLATS-LOCB                        
445402        MOVE JA TO WS-UPPD-LOCB                                           
445502     END-IF                                                               
445602                                                                          
445702     MOVE SLAG-KVLS   TO WS-OLD-KVLS                                      
445802     MOVE SLAG-KVEFRS TO WS-OLD-KVEFRS                                    
445902     IF WS-KVANTMOT > ZERO                                                
446002        ADD WS-KVANTMOT  TO SLAG-KVLS                                     
446102        MOVE WS-KVANTMOT TO WS-RO-KVANT                                   
446202        MOVE ZERO        TO W-KVAVIS                                      
446302        IF WS-UPPD-LOCB = JA                                              
446402           PERFORM S13-UPPDATERA-WDJ9                                     
446502        END-IF                                                            
446602        PERFORM IMS-REPL-WDK711                                           
446702        PERFORM HAA-FLYTTA-SALDOLOGG-DATA                                 
446802        IF NDC-AU OR NDC-JP OR NDC-NA                                     
446902           PERFORM S01-EV-RO-TACKNING                                     
447002        END-IF                                                            
447102     END-IF                                                               
447202     .                                                                    
447302     EJECT                                                                
447402                                                                          
447502 HA-UPPDATERA-BEF-ART SECTION.                                            
447602     MOVE NEJ TO WS-UPPD-LOCB                                             
447702                                                                          
447802     IF REQU-ADLAGOMR(RAD-IX) = ALL '+' OR SPACE                          
447902        MOVE SLAG-ADLAGOMR       TO WS-ADLAGOMR(RAD-IX)                   
448002     ELSE                                                                 
448102        MOVE WS-ADLAGOMR(RAD-IX) TO SLAG-ADLAGOMR                         
448202                                    W-ADLAGOMR-LOCB                       
448302        MOVE JA TO WS-UPPD-LOCB                                           
448402     END-IF                                                               
448502     IF REQU-ADGANG(RAD-IX) = ALL '+' OR SPACE                            
448602        MOVE SLAG-ADGANG         TO WS-ADGANG(RAD-IX)                     
448702     ELSE                                                                 
448802        MOVE WS-ADGANG(RAD-IX)   TO SLAG-ADGANG                           
448902                                    W-ADGANG-LOCB                         
449002        MOVE JA TO WS-UPPD-LOCB                                           
449102     END-IF                                                               
449202     IF REQU-ADPLATS(RAD-IX) = ALL '+' OR SPACE                           
449302        MOVE SLAG-ADPLATS        TO WS-ADPLATS(RAD-IX)                    
449402     ELSE                                                                 
449502        MOVE WS-ADPLATS(RAD-IX)  TO SLAG-ADPLATS                          
449602                                    W-ADPLATS-LOCB                        
449702        MOVE JA TO WS-UPPD-LOCB                                           
449802     END-IF                                                               
449902                                                                          
450002     MOVE SLAG-KVLS          TO WS-OLD-KVLS                               
450102     MOVE SLAG-KVEFRS        TO WS-OLD-KVEFRS                             
450202     IF WS-KVANTMOT > ZERO                                                
450302        ADD WS-KVANTMOT      TO SLAG-KVLS                                 
450402        MOVE WS-KVANTMOT     TO WS-RO-KVANT                               
450502        MOVE ZERO            TO W-KVAVIS                                  
450602                                                                          
450702        IF WS-UPPD-LOCB = JA                                              
450802           PERFORM S13-UPPDATERA-WDJ9                                     
450902        END-IF                                                            
451002        PERFORM IMS-REPL-WDK711                                           
451102        PERFORM HAA-FLYTTA-SALDOLOGG-DATA                                 
451202        PERFORM S01-EV-RO-TACKNING                                        
451302     END-IF                                                               
451402     .                                                                    
451502     EJECT                                                                
451602 HAA-FLYTTA-SALDOLOGG-DATA SECTION.                                       
451702                                                                          
451802     PERFORM S11-FLYTTA-SALDOLOGG-DATA                                    
451902     MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                             
452002     MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                                
452102     PERFORM S12-ISRT-SALDOLOGG                                           
452202     .                                                                    
452302     EJECT                                                                
452402                                                                          
452502 HB-UPPDATERA-NY-ART-PV SECTION.                                          
452602     MOVE ZERO TO W-KVAVIS                                                
452702                  WS-OLD-KVLS                                             
452802                  WS-OLD-KVEFRS                                           
452902                                                                          
453002     MOVE ALL '+'       TO WDK7-W005WDK7                                  
453102     MOVE 'WDK711'      TO WDK7-IDSEGM                                    
453202     MOVE W-IDARTNR     TO WDK7-IDARTNR-KFB                               
453302     MOVE REC-WS-IDDC   TO WDK7-IDDC-KFB                                  
453402                           WDK7-IDDC                                      
453502     MOVE WS-ADLAGOMR(RAD-IX)    TO WDK7-ADLAGOMR                         
453602     MOVE WS-ADGANG(RAD-IX)      TO WDK7-ADGANG                           
453702     MOVE WS-ADPLATS(RAD-IX)     TO WDK7-ADPLATS                          
453802     MOVE WS-KVANTMOT            TO WDK7-KVLS                             
453902     IF NDC-AU OR NDC-JP OR NDC-NA                                        
454002        MOVE JA                  TO WDK7-FLORDSP                          
454102                                    WDK7-FLSPBULK                         
454202     END-IF                                                               
454302     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
454402                                       ARTC-PCB WDK7-PCB                  
454502     PERFORM HBA-AKTIVERA-ARTIKEL-DC                                      
454602     .                                                                    
454702     EJECT                                                                
454802                                                                          
454902 HB-UPPDATERA-NY-ART SECTION.                                             
455002     MOVE ZERO TO W-KVAVIS                                                
455102                  WS-OLD-KVLS                                             
455202                  WS-OLD-KVEFRS                                           
455302     MOVE ALL '+'       TO WDK7-W005WDK7                                  
455402     MOVE 'WDK711'      TO WDK7-IDSEGM                                    
455502     MOVE W-IDARTNR     TO WDK7-IDARTNR-KFB                               
455602     MOVE REC-WS-IDDC   TO WDK7-IDDC-KFB                                  
455702                           WDK7-IDDC                                      
455802     MOVE WS-ADLAGOMR(RAD-IX)    TO WDK7-ADLAGOMR                         
455902     MOVE WS-ADGANG(RAD-IX)      TO WDK7-ADGANG                           
456002     MOVE WS-ADPLATS(RAD-IX)     TO WDK7-ADPLATS                          
456102     MOVE WS-KVANTMOT            TO WDK7-KVLS                             
456202     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
456302                                       ARTC-PCB WDK7-PCB                  
456402     PERFORM HBA-AKTIVERA-ARTIKEL-DC                                      
456502     .                                                                    
456602     EJECT                                                                
456702                                                                          
456802 HBA-AKTIVERA-ARTIKEL-DC SECTION.                                         
456902* MOVE FÖR ATT INITIERA SLAG-AREA EFTER W005WDK7-ANROPET                  
457002     MOVE WDK7-WDK711            TO SLAG-WDK711                           
457102     MOVE WS-ADLAGOMR(RAD-IX)    TO W-ADLAGOMR-LOCB                       
457202     MOVE WS-ADGANG(RAD-IX)      TO W-ADGANG-LOCB                         
457302     MOVE WS-ADPLATS(RAD-IX)     TO W-ADPLATS-LOCB                        
457402                                                                          
457502     PERFORM S13-UPPDATERA-WDJ9                                           
457602     PERFORM HBB-FLYTTA-SALDOLOGG-DATA                                    
457702     .                                                                    
457802     EJECT                                                                
457902                                                                          
458002 HBB-FLYTTA-SALDOLOGG-DATA SECTION.                                       
458102     PERFORM S11-FLYTTA-SALDOLOGG-DATA                                    
458202     MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                             
458302     MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                                
458402     PERFORM S12-ISRT-SALDOLOGG                                           
458502     .                                                                    
458602     EJECT                                                                
458702                                                                          
458802 HC-UPPDATERA-INLAGGNING-PV SECTION.                                      
458902     MOVE 'HC-UPPDATERA-INLAGGNING-PV ' TO CURRENT-SECTION                
459002                                                                          
459102     PERFORM IMS-GU-WLARTC01                                              
459202     MOVE K6-ART-KDPRODSL   TO W-KDPRODSL                                 
459302     MOVE K6-ART-IDFKNGRP   TO W-IDFKNGRP                                 
459402     MOVE K6-ART-KDSORT     TO WS-KDSORT                                  
459502     PERFORM IMS-GHNP-WLARTC11                                            
459602     MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                            
459702     MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                           
459802                                                                          
459902     IF SEND-DCS-CDC OR SEND-DCS-AUSTRALIA OR SEND-DCS-JAPAN              
460002     OR SEND-DCS-DDC OR SEND-DCS-SDC                                      
460102     OR SEND-DCS-LAND-NON-VCC-OWNED                                       
460204     OR (SEND-DCS-USA AND REC-DCS-CDC)                                    
460304     OR (SEND-DCS-NDC-CN AND REC-DCS-CDC)                                 
460404        IF NOT NDC-NA                                                     
460504           MOVE ZERO                TO WS-FIXAD-PRARTNTO                  
460604           MOVE SPACE               TO WS-KDVALISO                        
460704           IF SEND-DCS-DDC                                                
460804              MOVE '1441 '             TO INL-IDLEVNR                     
460904           ELSE                                                           
461004              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
461104           END-IF                                                         
461204           PERFORM S08-SKAPA-SDC-NDC-HIST-MOT-PV                          
461304           MOVE ZERO TO W-KVAVIS                                          
461404           IF WS-KVANTMOT > ZERO                                          
461504             PERFORM S14-SKAPA-SAP-TRANS-PV                               
461604           END-IF                                                         
461704           IF SEND-DCS-DDC                                                
461804              PERFORM HCB-SKAPA-DIFF-TRANS                                
461904           END-IF                                                         
462004           PERFORM HCC-SKAPA-LDC-TRANS                                    
462104           IF DIST35-NONVCC-VCC-REFILL OR                                 
462204              DIST35-NONVCC-CDC-REFILL OR                                 
462205              DIST35-NONVCC-VCC-TRANSFER                                  
462304           PERFORM S03-SKAPA-LEVANM-TRANS                                 
462404           END-IF                                                         
462504        END-IF                                                            
462604                                                                          
462704        IF NDC-NA                                                         
462804           PERFORM S02-PRIS-TILLAMPNING                                   
462904           MOVE PRIS-PRARTNTO       TO WS-FIXAD-PRARTNTO                  
463004           MOVE 'SEK'               TO WS-KDVALISO                        
463104           IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                   
463204             MOVE PRIS-KDVALISO     TO WS-KDVALISO                        
463304           END-IF                                                         
463404           IF SEND-DCS-DDC                                                
463504              MOVE '1441 '          TO INL-IDLEVNR                        
463604           ELSE                                                           
463704              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
463804           END-IF                                                         
463904                                                                          
464004           PERFORM S08-SKAPA-SDC-NDC-HIST-MOT-PV                          
464104           PERFORM S04-SKAPA-EKOTRANS-A03                                 
464204           MOVE SPAR-FLINLREP TO FILC3-FLINLREP                           
464304           PERFORM HCC-SKAPA-LDC-TRANS                                    
464404                                                                          
464504           PERFORM IMS-GET-WDK711                                         
464604           MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD              
464704           MOVE WS-KVANTMOT         TO AVG-KVANTMOT                       
464804                                       EKOTRA03-KVANTMOT                  
464904           MOVE WS-KVSKROT          TO EKOTRA03-KVSKROT                   
465004           ADD WS-KVSKROT           TO AVG-KVANTMOT                       
465104           MOVE '61'                TO EKOTRA03-KDANMORS                  
465204                                                                          
465304           PERFORM S03-SKAPA-LEVANM-TRANS                                 
465404                                                                          
465504           PERFORM S06-OMRAKN-MEDELPRIS-LAB                               
465604           MOVE W-TIME-N         TO AKTUELL-TID                           
465704           PERFORM S09-FIXA-LOKALTID                                      
465804           MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                         
465904           MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST                     
466004                                    SLAG-PRAVCOST                         
466104           MOVE AVG-REMARKUP     TO EKOTRA03-REMARKUP                     
466204           MOVE AVG-PRKURS       TO EKOTRA03-PRKURS                       
466304                                                                          
466404           IF WS-KVANTMOT > ZERO                                          
466504              PERFORM IMS-REPL-WDK711                                     
466604           ELSE                                                           
466704              MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST             
466804           END-IF                                                         
466904        END-IF                                                            
467004     ELSE                                                                 
467104                                                                          
467204        IF  (SEND-DCS-NDC-NA AND SEND-DCS-USA)                            
467304        AND (NDC-NA          AND NDC-CA)                                  
467404        OR  (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                         
467504        AND (NDC-NA          AND NDC-US)                                  
467604                                                                          
467704            MOVE SEND-WS-IDDC  TO W-IDDC                                  
467804            PERFORM IMS-GET-WDK711                                        
467904            MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO                       
468004            IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                        
468104               MOVE 'CAD'      TO WS-KDVALISO                             
468204            ELSE                                                          
468304               MOVE 'USD'      TO WS-KDVALISO                             
468404            END-IF                                                        
468504                                                                          
468604            MOVE REC-WS-IDDC TO W-IDDC                                    
468704            PERFORM IMS-GET-WDK711                                        
468804            MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                       
468904            PERFORM S08-SKAPA-SDC-NDC-HIST-MOT-PV                         
469004                                                                          
469104            PERFORM S04-SKAPA-EKOTRANS-A03                                
469204            MOVE SPAR-FLINLREP TO FILC3-FLINLREP                          
469304            PERFORM HCC-SKAPA-LDC-TRANS                                   
469404                                                                          
469504            MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD             
469604            MOVE WS-KVANTMOT         TO AVG-KVANTMOT                      
469704                                        EKOTRA03-KVANTMOT                 
469804            ADD WS-KVSKROT           TO AVG-KVANTMOT                      
469904            MOVE WS-KVSKROT          TO EKOTRA03-KVSKROT                  
470004            MOVE '61'                TO EKOTRA03-KDANMORS                 
470104                                                                          
470204            PERFORM S03-SKAPA-LEVANM-TRANS                                
470304                                                                          
470404            PERFORM S06-OMRAKN-MEDELPRIS-LAB                              
470504            MOVE W-TIME-N         TO AKTUELL-TID                          
470604            PERFORM S09-FIXA-LOKALTID                                     
470704            MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                        
470804            MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST                    
470904                                     SLAG-PRAVCOST                        
471004            MOVE AVG-REMARKUP     TO EKOTRA03-REMARKUP                    
471104            MOVE AVG-PRKURS       TO EKOTRA03-PRKURS                      
471204                                                                          
471304            IF WS-KVANTMOT > ZERO                                         
471404               PERFORM IMS-REPL-WDK711                                    
471504            ELSE                                                          
471604               MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST            
471704            END-IF                                                        
471804                                                                          
471904        ELSE                                                              
472004                                                                          
472104            IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                         
472204            AND (NDC-NA         AND NDC-US)                               
472304                                                                          
472404                MOVE SEND-WS-IDDC TO W-IDDC                               
472504                PERFORM IMS-GET-WDK711                                    
472604                MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO                   
472704                MOVE 'USD'         TO WS-KDVALISO                         
472804                                                                          
472904                MOVE REC-WS-IDDC TO W-IDDC                                
473004                PERFORM IMS-GET-WDK711                                    
473104                MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                   
473204                PERFORM S08-SKAPA-SDC-NDC-HIST-MOT-PV                     
473304                                                                          
473404                MOVE SPAR-FLINLREP TO FILC3-FLINLREP                      
473504                PERFORM HCC-SKAPA-LDC-TRANS                               
473604                                                                          
473704                PERFORM S04-SKAPA-EKOTRANS-A03                            
473804                MOVE SLAG-PRAVCOST                                        
473904                               TO EKOTRA03-PRAVCOST-OLD                   
474004                MOVE WS-KVANTMOT         TO EKOTRA03-KVANTMOT             
474104                                            AVG-KVANTMOT                  
474204                ADD WS-KVSKROT           TO AVG-KVANTMOT                  
474304                MOVE WS-KVSKROT          TO EKOTRA03-KVSKROT              
474404                MOVE '61'                TO EKOTRA03-KDANMORS             
474504                                                                          
474604                PERFORM S06-OMRAKN-MEDELPRIS-LAB                          
474704                MOVE W-TIME-N         TO AKTUELL-TID                      
474804                PERFORM S09-FIXA-LOKALTID                                 
474904                MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                    
475004                MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST                
475104                                          SLAG-PRAVCOST                   
475204                MOVE AVG-REMARKUP     TO EKOTRA03-REMARKUP                
475304                MOVE AVG-PRKURS       TO EKOTRA03-PRKURS                  
475404                MOVE 1.0              TO EKOTRA03-PRKURS                  
475504                                                                          
475604                IF WS-KVANTMOT > ZERO                                     
475704                   PERFORM IMS-REPL-WDK711                                
475804                ELSE                                                      
475904                   MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST        
476004                END-IF                                                    
476104                                                                          
476204                MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                   
476304                PERFORM S07-SKAPA-NDC-HIST-SANDANDE                       
476404                                                                          
476504                MOVE ZERO              TO 6306-TIFAKT                     
476604                MOVE IDFAKT-WS         TO 6306-IDFAKT                     
476704                                          W-IDFAKT                        
476804                MOVE REC-WS-IDDC       TO 6306-IDDC-REC                   
476904                MOVE SEND-WS-IDDC      TO 6306-IDDC-SEND                  
477004                MOVE WS-6302-IDDC-LEV  TO 6306-IDDC-LEV                   
477104                PERFORM IMS-GHU-WL630511                                  
477204                IF SEGMENT-FINNS                                          
477304                   CONTINUE                                               
477404                ELSE                                                      
477504                   MOVE 'N'            TO 6306-FLKLAR                     
477604                                          6306-FLDIRLEV                   
477704                   PERFORM IMS-ISRT-WL630511                              
477804                END-IF                                                    
477904                                                                          
478004           END-IF                                                         
478104        END-IF                                                            
478204     END-IF                                                               
478304     .                                                                    
478404     EJECT                                                                
478504                                                                          
478604 HC-UPPDATERA-INLAGGNING SECTION.                                         
478704     MOVE 'HC-UPPDATERA-INLAGGNING ' TO CURRENT-SECTION                   
478804                                                                          
478904     PERFORM IMS-GU-WLARTC01                                              
479004     MOVE K6-ART-KDPRODSL   TO W-KDPRODSL                                 
479104     MOVE K6-ART-IDFKNGRP   TO W-IDFKNGRP                                 
479204     MOVE K6-ART-KDSORT     TO WS-KDSORT                                  
479304     PERFORM IMS-GHNP-WLARTC11                                            
479404     MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                            
479504     MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                           
479604                                                                          
479704     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
479804     OR DIST35-VCC-NONVCC-REFILL                                          
479805     OR DIST35-VCC-NONVCC-TRANSFER                                        
479904        PERFORM S02-PRIS-TILLAMPNING                                      
480004        MOVE PRIS-PRARTNTO       TO WS-FIXAD-PRARTNTO                     
480104        MOVE SEND-DCS-KDVALISO   TO WS-KDVALISO                           
480204        IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                      
480304          MOVE PRIS-KDVALISO     TO WS-KDVALISO                           
480404        END-IF                                                            
480504        MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                           
480604                                                                          
480704        PERFORM S08-SKAPA-SDC-NDC-HIST-MOT                                
480804        MOVE SPAR-FLINLREP TO FILC3-FLINLREP                              
480904        PERFORM HCC-SKAPA-LDC-TRANS                                       
481004                                                                          
481104        PERFORM IMS-GET-WDK711                                            
481204        MOVE WS-KVANTMOT         TO AVG-KVANTMOT                          
481304        ADD WS-KVSKROT           TO AVG-KVANTMOT                          
481404                                                                          
481504        PERFORM S03-SKAPA-LEVANM-TRANS                                    
481604                                                                          
481704        PERFORM S06-OMRAKN-MEDELPRIS                                      
481804        MOVE W-TIME-N         TO AKTUELL-TID                              
481904        PERFORM S09-FIXA-LOKALTID                                         
482004        MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                            
482104        MOVE AVG-PRAVCOST-NEW TO SLAG-PRAVCOST                            
482204                                                                          
482304        PERFORM S14-SKAPA-SAP-TRANS                                       
482404                                                                          
482504        IF WS-KVANTMOT > ZERO                                             
482604           PERFORM IMS-REPL-WDK711                                        
482704        END-IF                                                            
482804                                                                          
482904     ELSE                                                                 
483004        IF SEND-DCS-LAND-NON-VCC-OWNED                                    
483104           MOVE SEND-WS-IDDC TO W-IDDC                                    
483204           PERFORM IMS-GET-WDK711                                         
483304           PERFORM S02-PRIS-TILLAMPNING                                   
483404           MOVE WS-SAP-PRARTSTD TO PRIS-PRARTNTO                          
483504           MOVE PRIS-PRARTNTO TO WS-FIXAD-PRARTNTO                        
483604           MOVE SEND-DCS-KDVALISO  TO WS-KDVALISO                         
483704                                                                          
483804           MOVE SEND-WS-IDDC TO W-IDDC                                    
483904           PERFORM IMS-GET-WDK711                                         
484004           MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                        
484104           PERFORM S08-SKAPA-SDC-NDC-HIST-MOT                             
484204                                                                          
484304           MOVE SPAR-FLINLREP TO FILC3-FLINLREP                           
484404           PERFORM HCC-SKAPA-LDC-TRANS                                    
484504                                                                          
484604           MOVE WS-KVANTMOT         TO AVG-KVANTMOT                       
484704           ADD WS-KVSKROT           TO AVG-KVANTMOT                       
484804                                                                          
484904           PERFORM S06-OMRAKN-MEDELPRIS                                   
485004           MOVE W-TIME-N         TO AKTUELL-TID                           
485104           PERFORM S09-FIXA-LOKALTID                                      
485204           MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                         
485304           MOVE AVG-PRAVCOST-NEW TO SLAG-PRAVCOST                         
485404                                                                          
485504           PERFORM S14-SKAPA-SAP-TRANS                                    
485604                                                                          
485704           IF WS-KVANTMOT > ZERO                                          
485804              PERFORM IMS-REPL-WDK711                                     
485904           END-IF                                                         
486004                                                                          
486104           MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                        
486204           PERFORM S07-SKAPA-NDC-HIST-SANDANDE                            
486304                                                                          
486404           MOVE ZERO              TO 6306-TIFAKT                          
486504           MOVE IDFAKT-WS         TO 6306-IDFAKT                          
486604                                         W-IDFAKT                         
486704           MOVE REC-WS-IDDC       TO 6306-IDDC-REC                        
486804           MOVE SEND-WS-IDDC      TO 6306-IDDC-SEND                       
486904           MOVE WS-6302-IDDC-LEV  TO 6306-IDDC-LEV                        
487004           PERFORM IMS-GHU-WL630511                                       
487104           IF SEGMENT-FINNS                                               
487204              CONTINUE                                                    
487304           ELSE                                                           
487404              MOVE 'N'            TO 6306-FLKLAR                          
487504                                     6306-FLDIRLEV                        
487604              PERFORM IMS-ISRT-WL630511                                   
487704           END-IF                                                         
487804        END-IF                                                            
487904     END-IF                                                               
488004     .                                                                    
488104     EJECT                                                                
488204 HCX-FIXA-DISTRIKT SECTION.                                               
488304                                                                          
488404     MOVE ZERO TO WS-IDDISTR                                              
488504                                                                          
488604     MOVE SEND-WS-IDDC TO WS-SOEK-IDDC-SEND                               
488704     MOVE REC-WS-IDDC  TO WS-SOEK-IDDC-REC                                
488804                                                                          
488904     IF SEND-DCS-DDC                                                      
489004        MOVE WC-CDC-SE TO WS-SOEK-IDDC-SEND                               
489104     END-IF                                                               
489204                                                                          
489304     SEARCH ALL WWDC03-IDDISTR                                            
489404       AT END                                                             
489504         MOVE ZERO     TO WS-IDDISTR                                      
489604*        MOVE 'EJ TRÄFF I TABELL TEXTXX' TO FELTEXT                       
489704*        DISPLAY FELTEXT                                                  
489804*        CALL FELLOG                                                      
489904       WHEN WWDC03-IDDC-SEND-REC(WWDC03-IX)                               
490004         = WS-SOEK-IDDC-SEND-REC                                          
490104           MOVE WWDC03-SOK-IDDISTR(WWDC03-IX) TO WS-IDDISTR               
490204     END-SEARCH                                                           
490304     .                                                                    
490404     EJECT                                                                
490504 HCY-STUDS-REFILL-DISTRIKT  SECTION.                                      
490604     MOVE 'HCY-STUDS-REFILL-DISTRIKT ' TO CURRENT-SECTION                 
490704                                                                          
490804     MOVE REC-WS-IDDC      TO W-IDDC-B6                                   
490904     MOVE WS-6302-IDDC-LEV TO W-IDDC-B616                                 
491004                                                                          
491104     PERFORM IMS-GU-WDB616                                                
491204     MOVE REF-IDDISTR-REFILL  TO WS-IDDISTR                               
491304                                                                          
491404     .                                                                    
491504     EJECT                                                                
491604 HCB-SKAPA-DIFF-TRANS SECTION.                                            
491704                                                                          
491804     MOVE WS-TIFAKT       TO WS-SEKEL-TEST                                
491904                             WS-DIFF-AAMMDD                               
492004     IF WS-SEK = 9                                                        
492104        MOVE 19           TO WS-DIFF-SS                                   
492204     ELSE                                                                 
492304        MOVE 20           TO WS-DIFF-SS                                   
492404     END-IF                                                               
492504     MOVE WS-DIFF-AAAAMMDD  TO FILC2-DAFAKT                               
492604     MOVE W-IDARTNR         TO FILC2-IDARTNR                              
492704     MOVE SEND-WS-IDDC      TO FILC2-IDDC-SEND                            
492804     MOVE REC-WS-IDDC       TO FILC2-IDDC-REC                             
492904     MOVE W-IDFAKT          TO FILC2-IDFAKT                               
493004     MOVE WS-IDORDNR        TO FILC2-IDKUNDRF                             
493104     MOVE ZERO              TO FILC2-PRARTBES-PR                          
493204     MOVE WS-IDLEVNR        TO FILC2-IDLEVNR                              
493304     IF WS-KVANTMOT > ZERO                                                
493404        MOVE WS-KVANTMOT       TO FILC2-KVANTAL                           
493504        MOVE 'FOUND'           TO FILC2-AVVIKELSETYP                      
493604        MOVE FILC2-W61244      TO FILC2-FIL-WDR301-DATA                   
493704        ACCEPT W-TID FROM TIME                                            
493804        IF W-TID = FILC2-FIL-TIKLOCK                                      
493904           ADD +1              TO FILC2-FIL-IDSEKVNR                      
494004        ELSE                                                              
494104           MOVE W-TID          TO FILC2-FIL-TIKLOCK                       
494204           MOVE +1             TO FILC2-FIL-IDSEKVNR                      
494304        END-IF                                                            
494404        PERFORM IMS-ISRT-WLFILC2                                          
494504     END-IF                                                               
494604                                                                          
494704     IF WS-KVSKROT > ZERO                                                 
494804        MOVE WS-KVSKROT        TO FILC2-KVANTAL                           
494904        MOVE 'FOUND/DAM'       TO FILC2-AVVIKELSETYP                      
495004        MOVE FILC2-W61244      TO FILC2-FIL-WDR301-DATA                   
495104        ACCEPT W-TID FROM TIME                                            
495204        IF W-TID = FILC2-FIL-TIKLOCK                                      
495304           ADD +1              TO FILC2-FIL-IDSEKVNR                      
495404        ELSE                                                              
495504           MOVE W-TID          TO FILC2-FIL-TIKLOCK                       
495604           MOVE +1             TO FILC2-FIL-IDSEKVNR                      
495704        END-IF                                                            
495804        PERFORM IMS-ISRT-WLFILC2                                          
495904     END-IF                                                               
496004     .                                                                    
496104     EJECT                                                                
496204 HCC-SKAPA-LDC-TRANS SECTION.                                             
496304                                                                          
496404     MOVE 'J'             TO FILC3-FLINLREP                               
496504     MOVE WS-TIFAKT       TO WS-SEKEL-TEST                                
496604                             WS-DIFF-AAMMDD                               
496704     IF WS-SEK = 9                                                        
496804        MOVE 19           TO WS-DIFF-SS                                   
496904     ELSE                                                                 
497004        MOVE 20           TO WS-DIFF-SS                                   
497104     END-IF                                                               
497204     MOVE WS-DIFF-AAAAMMDD  TO FILC3-DAFAKT                               
497304     MOVE W-IDARTNR         TO FILC3-IDARTNR                              
497404     IF WS-6302-IDDC-LEV NOT = SPACE                                      
497504       MOVE WS-6302-IDDC-LEV TO FILC3-IDDC-SEND                           
497604     ELSE                                                                 
497704       MOVE SEND-WS-IDDC    TO FILC3-IDDC-SEND                            
497804     END-IF                                                               
497904     MOVE REC-WS-IDDC       TO FILC3-IDDC-REC                             
498004     MOVE W-IDFAKT          TO FILC3-IDFAKT                               
498104     IF W-IDFAKT = ZERO                                                   
498204        MOVE WS-IDFAKT      TO FILC3-IDFAKT                               
498304     END-IF                                                               
498404     MOVE WS-IDORDNR        TO FILC3-IDKUNDRF                             
498504     MOVE WS-IDKUNDNR       TO FILC3-IDKUNDNR                             
498604     MOVE WS-IDKOLLI        TO FILC3-IDKOLLI                              
498704     MOVE WS-IDORDNR        TO FILC3-IDKUNDRF                             
498804     MOVE ZERO              TO FILC3-PRARTSTD                             
498904                                                                          
499004     MOVE WS-SUMMA-KVANT    TO FILC3-KVANTAL                              
499104     MOVE ZERO              TO FILC3-KVAVIS                               
499204     MOVE 'FOUND'           TO FILC3-AVVIKELSETYP                         
499304     MOVE 2                 TO FILC3-KDSORT1                              
499404     MOVE DAGENS-DATUM      TO FILC3-DAREGDAT                             
499504     IF REQU-IDUSER-003 = ALL '+'                                         
499604        MOVE SPACE          TO FILC3-IDUSER                               
499704     ELSE                                                                 
499804        MOVE REQU-IDUSER-003 TO FILC3-IDUSER                              
499904     END-IF                                                               
500004     MOVE FILC3-W61247      TO FILC3-FIL-WDR301-DATA                      
500104     ACCEPT W-TID FROM TIME                                               
500204     IF W-TID = FILC3-FIL-TIKLOCK                                         
500304        ADD +1              TO FILC3-FIL-IDSEKVNR                         
500404     ELSE                                                                 
500504        MOVE W-TID          TO FILC3-FIL-TIKLOCK                          
500604        MOVE +1             TO FILC3-FIL-IDSEKVNR                         
500704     END-IF                                                               
500804     PERFORM IMS-ISRT-WLFILC3                                             
500904                                                                          
501004     IF WS-KVSKROT > ZERO                                                 
501104        MOVE WS-KVSKROT        TO FILC3-KVANTAL                           
501204        MOVE 'DAM'             TO FILC3-AVVIKELSETYP                      
501304        MOVE 5                 TO FILC3-KDSORT1                           
501404        MOVE FILC3-W61247      TO FILC3-FIL-WDR301-DATA                   
501504        ACCEPT W-TID FROM TIME                                            
501604        IF W-TID = FILC3-FIL-TIKLOCK                                      
501704           ADD +1              TO FILC3-FIL-IDSEKVNR                      
501804        ELSE                                                              
501904           MOVE W-TID          TO FILC3-FIL-TIKLOCK                       
502004           MOVE +1             TO FILC3-FIL-IDSEKVNR                      
502104        END-IF                                                            
502204        PERFORM IMS-ISRT-WLFILC3                                          
502304     END-IF                                                               
502404     .                                                                    
502504     EJECT                                                                
502604                                                                          
502704 HD-SKAPA-REFILLTRANS SECTION.                                            
502804                                                                          
502904     MOVE W-IDARTNR         TO FILC-IDARTNR                               
503004     MOVE WS-KVANTMOT       TO FILC-KVLEVANM                              
503104     MOVE WS-KVSKROT        TO FILC-KVSKROT                               
503204     MOVE SEND-WS-IDDC      TO FILC-IDDC-SEND                             
503304     MOVE REC-WS-IDDC       TO FILC-IDDC-REC                              
503404     MOVE W-DAGENS-DATUM    TO FILC-TILEVANM                              
503504     MOVE FILC-W61236       TO FILC-FIL-WDR301-DATA                       
503604     ACCEPT W-TID FROM TIME                                               
503704     IF W-TID = FILC-FIL-TIKLOCK                                          
503804        ADD +1              TO FILC-FIL-IDSEKVNR                          
503904     ELSE                                                                 
504004        MOVE W-TID          TO FILC-FIL-TIKLOCK                           
504104        MOVE +1             TO FILC-FIL-IDSEKVNR                          
504204     END-IF                                                               
504304     PERFORM IMS-ISRT-WLFILC                                              
504404     .                                                                    
504504     EJECT                                                                
504604                                                                          
504704 HE-CREATE-KOREAN-LABELS SECTION.                                         
504804     PERFORM S15-SKAPA-WDL6A1KY                                           
504904     PERFORM IMS-GU-WDL6A1-01                                             
505004**   IF SEGMENT-FINNS                                                     
505104     ADD  1 TO INDX-LINE                                                  
505204     PERFORM UNTIL SEGMENT-SAKNAS                                         
505304       MOVE WDL6A1-SEQA-IDARTNR   TO W-IDARTNR                            
505404                              KLBL-IDARTNR (INDX-LINE)                    
505504       MOVE WDL6A1-SEQA-IDDC      TO W-IDDC                               
505604       MOVE WDL6A1-SEQA-DAINLEV   TO W-DAINLEV                            
505704       MOVE JA TO PRINT-SW-KOREA                                          
505804       PERFORM IMS-09-GHU-INLC-WLINLC11                                   
505904       IF SEGMENT-FINNS                                                   
506004*         MOVE 1             TO KLBL-KVANTAL(INDX-LINE)                   
506104          MOVE INL-KVAVIS    TO KLBL-KVAVIS(INDX-LINE)                    
506204          MOVE ZERO          TO KLBL-IDORDNR7(INDX-LINE)                  
506304          MOVE INL-IDORDNR5  TO KLBL-IDORDNR7(INDX-LINE)                  
506404          MOVE INL-ADLAGOMR  TO KLBL-ADLAGOMR(INDX-LINE)                  
506504          MOVE INL-ADGANG    TO KLBL-ADGANG(INDX-LINE)                    
506604          MOVE INL-ADPLATS   TO KLBL-ADPLATS(INDX-LINE)                   
506704          MOVE INL-IDKOLLI   TO KLBL-IDKOLLI(INDX-LINE)                   
506804          PERFORM IMS-GU-WLARTC01                                         
506904*         MOVE K6-ART-KDSORT    TO KLBL-KDSORT(INDX-LINE)                 
507004        IF NDC-KR                                                         
507104          IF K6-ART-IDFKNGRP =  5222 OR 8841 OR 8842 OR 3521 OR           
507204                                3531 OR 3532 OR 8417 OR 1912 OR           
507304                                8441 OR 8431 OR 8121 OR 8433 OR           
507404                                8445 OR 8443 OR 5115 OR 5125 OR           
507504                                7713 OR 7703 OR 8369 OR 3551 OR           
                                      3567 OR 3514 OR 8361                      
507604             MOVE '***'      TO KLBL-KRKC(INDX-LINE)                      
507704          ELSE                                                            
507804             MOVE '   '      TO KLBL-KRKC(INDX-LINE)                      
507904          END-IF                                                          
508004        ELSE                                                              
508104          MOVE '   '      TO KLBL-KRKC(INDX-LINE)                         
508204        END-IF                                                            
508304       END-IF                                                             
508404       PERFORM IMS-GN-WDL6A1-01                                           
508504       ADD 1 TO INDX-LINE                                                 
508604     END-PERFORM                                                          
508704**   END-IF                                                               
508804     .                                                                    
508904                                                                          
509004     EJECT                                                                
509104  HF-UPPDATERA-BEF-ART-CDC SECTION.                                       
509204                                                                          
509304     MOVE NEJ TO WS-UPPD-LOCB                                             
509404                                                                          
509504     IF REQU-ADLAGOMR(RAD-IX) = ALL '+' OR SPACE                          
509604        MOVE CLAG-ADLAGOMR       TO WS-ADLAGOMR(RAD-IX)                   
509704     ELSE                                                                 
509804        MOVE WS-ADLAGOMR(RAD-IX) TO CLAG-ADLAGOMR                         
509904                                    W-ADLAGOMR-LOCB                       
510004        MOVE JA TO WS-UPPD-LOCB                                           
510104     END-IF                                                               
510204     IF REQU-ADGANG(RAD-IX) = ALL '+' OR SPACE                            
510304        MOVE CLAG-ADGANG         TO WS-ADGANG(RAD-IX)                     
510404     ELSE                                                                 
510504        MOVE WS-ADGANG(RAD-IX)   TO CLAG-ADGANG                           
510604                                    W-ADGANG-LOCB                         
510704        MOVE JA TO WS-UPPD-LOCB                                           
510804     END-IF                                                               
510904     IF REQU-ADPLATS(RAD-IX) = ALL '+' OR SPACE                           
511004        MOVE CLAG-ADPLATS        TO WS-ADPLATS(RAD-IX)                    
511104     ELSE                                                                 
511204        MOVE WS-ADPLATS(RAD-IX)  TO CLAG-ADPLATS                          
511304                                    W-ADPLATS-LOCB                        
511404        MOVE JA TO WS-UPPD-LOCB                                           
511504     END-IF                                                               
511604     MOVE CLAG-KVLS          TO WS-OLD-KVLS                               
511704     IF WS-KVANTMOT > ZERO                                                
511804        ADD WS-KVANTMOT      TO CLAG-KVLS                                 
511904        MOVE WS-KVANTMOT     TO WS-RO-KVANT                               
512004        MOVE ZERO            TO W-KVAVIS                                  
512104                                                                          
512204        IF WS-UPPD-LOCB = JA                                              
512304           PERFORM S13-UPPDATERA-WDJ9                                     
512404        END-IF                                                            
512504        PERFORM IMS-REPL-WLARTC11                                         
512604        PERFORM HFA-FLYTTA-SALDOLOGG-DATA                                 
512704        PERFORM S01-EV-RO-TACKNING-CDC                                    
512804     END-IF                                                               
512904     .                                                                    
513004     EJECT                                                                
513104                                                                          
513204 HFA-FLYTTA-SALDOLOGG-DATA SECTION.                                       
513304     PERFORM S11-FLYTTA-SALDOLOGG-DATA                                    
513404     MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                             
513504     MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                                
513604     PERFORM S12-ISRT-SALDOLOGG                                           
513704     .                                                                    
513804                                                                          
513904 S01-EV-RO-TACKNING SECTION.                                              
514004                                                                          
514104     IF SLAG-KVROS-BULK > ZERO                                            
514204     OR SLAG-KVROS-DAG > ZERO                                             
514304        IF SLAG-KDLEVSP = ZERO                                            
514404           MOVE W-IDARTNR      TO 4506-IDARTNR                            
514504           MOVE +1             TO 4506-KDTAKORS                           
514604           MOVE WS-RO-KVANT    TO 4506-KVANTMOT                           
514704           MOVE REC-WS-IDDC    TO 4505-IDDC                               
514804           PERFORM IMS-ISRT-4506                                          
514904           MOVE SPACE          TO 4506-WDGX4506                           
515004        END-IF                                                            
515104     END-IF                                                               
515204     .                                                                    
515304     EJECT                                                                
515404                                                                          
515504 S01-EV-RO-TACKNING-CDC SECTION.                                          
515604                                                                          
515704     IF CLAG-KVROS > ZERO                                                 
515804        IF CLAG-KDLEVSP = ZERO                                            
515904           MOVE W-IDARTNR      TO 4506-IDARTNR                            
516004           MOVE +1             TO 4506-KDTAKORS                           
516104           MOVE WS-RO-KVANT    TO 4506-KVANTMOT                           
516204           MOVE REC-WS-IDDC    TO 4505-IDDC                               
516304           PERFORM IMS-ISRT-4506                                          
516404           MOVE SPACE          TO 4506-WDGX4506                           
516504        END-IF                                                            
516604     END-IF                                                               
516704     .                                                                    
516804     EJECT                                                                
516904                                                                          
517004 S02-PRIS-TILLAMPNING SECTION.                                            
517104     MOVE 'S02-PRIS-TILLAMPNING '  TO CURRENT-SECTION                     
517204                                                                          
517304*--- KOD 61 FÖR EXPORT,DISTR 927X US-TO-CN, 914X CN-TO-US                 
517404     MOVE ZERO   TO WS-6308-TF-PRARTNTO                                   
517504     MOVE SPACE  TO WS-6308-TF-KDVALISO                                   
517604                                                                          
517704                                                                          
517804     MOVE 1                   TO PRIS-KDCALL                              
517904     MOVE 'W6030900'          TO PRIS-IDPGM                               
518004     MOVE W-IDARTNR           TO PRIS-IDARTNR                             
518104     IF WS-6302-IDDISTR = ZERO                                            
518204       MOVE WS-IDDISTR        TO PRIS-IDDISTR                             
518304     ELSE                                                                 
518404       MOVE WS-6302-IDDISTR   TO PRIS-IDDISTR                             
518504     END-IF                                                               
518604     MOVE ZERO                TO PRIS-IDKUNDNR                            
518704     MOVE +1                  TO PRIS-KDORDKL                             
518804                                 PRIS-KVBEART                             
518904     MOVE SEND-WS-IDDC        TO PRIS-IDDC                                
519004     MOVE NEJ                 TO PRIS-FLINVEST                            
519104                                                                          
519204     CALL W335PRIS USING PRIS-W335PRIS ARTC-PCB                           
519304                                   PRIS-WDK7-PCB GMTA-PCB                 
519404                                   BETA-PCB GPRIA-PCB GPRIB-PCB           
519504                                   PRIS-COST-WDK6-PCB                     
519604                                   PRIS-COST-WDK7-PCB                     
519704                                   PRIS-COST-WDF1-PCB                     
519804                                   PRIS-COST-9305-PCB                     
519904                                   PRIS-COST-WDK72-PCB                    
520004                                   PRIS-COST-WDB6-PCB                     
520104                                                                          
520204     IF PRIS-KDSVAR = SPACE                                               
520304        MOVE PRIS-PRARTNTO    TO WS-FIXAD-PRARTNTO                        
520404                                                                          
520504        IF PRIS-PRAVCOST > ZERO                                           
520604          MOVE PRIS-PRAVCOST  TO WS-6308-TF-PRARTNTO                      
520704        ELSE                                                              
520804          MOVE PRIS-PRARTNTO  TO WS-6308-TF-PRARTNTO                      
520904        END-IF                                                            
521004        MOVE PRIS-KDVALISO    TO WS-6308-TF-KDVALISO                      
521104                                                                          
521204     ELSE                                                                 
521304        MOVE 'FEL RETURKOD FRÅN W335PRIS' TO FELTEXT                      
521404        DISPLAY FELTEXT                                                   
521504        CALL FELLOG                                                       
521604     END-IF                                                               
521704     .                                                                    
521804     EJECT                                                                
521904                                                                          
522004 S03-SKAPA-LEVANM-TRANS SECTION.                                          
522104     MOVE 'S03-SKAPA-LEVANM-TRANS '  TO CURRENT-SECTION                   
522204                                                                          
522304     MOVE WS-TIFAKT         TO 6306-TIFAKT                                
522404     MOVE IDFAKT-WS         TO 6306-IDFAKT                                
522504                               W-IDFAKT                                   
522604     MOVE REC-WS-IDDC       TO 6306-IDDC-REC                              
522704     MOVE SEND-WS-IDDC      TO 6306-IDDC-SEND                             
522804     MOVE WS-6302-IDDC-LEV  TO 6306-IDDC-LEV                              
522904                                                                          
523004     PERFORM IMS-GHU-WL630511                                             
523104     IF SEGMENT-FINNS                                                     
523204        IF WS-6302-IDDISTR = ZERO                                         
523304          MOVE WS-IDDISTR       TO 6308-IDDISTR                           
523404                                   DIST79-IDDISTR                         
523504                                   DIST35-IDDISTR                         
523604        ELSE                                                              
523704          MOVE WS-6302-IDDISTR  TO 6308-IDDISTR                           
523804                                   DIST79-IDDISTR                         
523904                                   DIST35-IDDISTR                         
524004        END-IF                                                            
524104        IF WS-6302-IDKUNDNR = ZERO                                        
524204          MOVE WS-IDKUNDNR      TO 6308-IDKUNDNR                          
524304        ELSE                                                              
524404          MOVE WS-6302-IDKUNDNR TO 6308-IDKUNDNR                          
524504        END-IF                                                            
524604                                                                          
524704        MOVE IDFAKT-WS      TO 6308-IDRAPPNR                              
524804        MOVE INL-KDFRAKT    TO 6308-KDFRAKT                               
524904        MOVE INL-IDKUNDRF   TO 6308-IDKUNDRF                              
525004        MOVE INL-IDKOLLI    TO 6308-IDKOLLI                               
525104        MOVE WS-KVANTMOT    TO 6308-KVLEVANM                              
525204        ADD  WS-KVSKROT     TO 6308-KVLEVANM                              
525304                                                                          
525404        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
525504        AND (NDC-NA         AND NDC-CA)                                   
525604        OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                          
525704        AND (NDC-NA         AND NDC-US)                                   
525804            MOVE '11'       TO 6308-KDANMORS                              
525904        ELSE                                                              
526004            MOVE '61'       TO 6308-KDANMORS                              
526104        END-IF                                                            
526204                                                                          
526304        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
526404        AND (NDC-NA         AND NDC-CA)                                   
526504        OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                          
526604        AND (NDC-NA         AND NDC-US)                                   
526704            MOVE INL-PRARTNTO    TO 6308-PRARTBTO                         
526804            MOVE ZERO            TO 6308-PRARTBTO-LOC                     
526904            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
527004        ELSE                                                              
527104          IF DIST79-DEALER-PRICE                                          
527204            MOVE INL-PRARTNTO    TO 6308-PRARTBTO-LOC                     
527304            MOVE ZERO            TO 6308-PRARTBTO                         
527404            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
527504          ELSE                                                            
527604            IF DIST35-NONVCC-NONVCC-REFILL                                
527605            OR DIST35-NONVCC-NONVCC-TRANSFER                              
527704            OR DIST35-NONVCC-VCC-REFILL                                   
527805            OR DIST35-NONVCC-CDC-REFILL                                   
527806            OR DIST35-NONVCC-VCC-TRANSFER                                 
527905**** HERE WE MOVE AVERAGE COST                                            
528005              MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                  
528105              MOVE ZERO                 TO 6308-PRARTBTO-LOC              
528205              MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                  
528305            ELSE                                                          
528405              MOVE INL-PRARTNTO    TO 6308-PRARTBTO                       
528505              MOVE ZERO            TO 6308-PRARTBTO-LOC                   
528605              MOVE 'SEK'           TO 6308-KDVALISO                       
528705            END-IF                                                        
528805          END-IF                                                          
528905        END-IF                                                            
529005                                                                          
529105        MOVE W-IDARTNR      TO 6308-IDARTNR                               
529205        MOVE ZERO           TO 6308-IDRADNR                               
529305                               6308-KDEMBLEV                              
529405        MOVE W-DAGENS-DATUM TO 6308-TILEVANM                              
529505                                                                          
529605        PERFORM IMS-ISRT-WL630521                                         
529705        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
529805           ADD +1 TO 6308-IDRADNR                                         
529905           PERFORM IMS-ISRT-WL630521                                      
530005        END-PERFORM                                                       
530105     ELSE                                                                 
530205        MOVE 'N'            TO 6306-FLKLAR                                
530305                               6306-FLDIRLEV                              
530405        PERFORM IMS-ISRT-WL630511                                         
530505                                                                          
530605        IF WS-6302-IDDISTR = ZERO                                         
530705          MOVE WS-IDDISTR       TO 6308-IDDISTR                           
530805                                   DIST79-IDDISTR                         
530905                                   DIST35-IDDISTR                         
531005        ELSE                                                              
531105          MOVE WS-6302-IDDISTR  TO 6308-IDDISTR                           
531205                                   DIST79-IDDISTR                         
531305                                   DIST35-IDDISTR                         
531405        END-IF                                                            
531505        IF WS-6302-IDKUNDNR = ZERO                                        
531605          MOVE WS-IDKUNDNR      TO 6308-IDKUNDNR                          
531705        ELSE                                                              
531805          MOVE WS-6302-IDKUNDNR TO 6308-IDKUNDNR                          
531905        END-IF                                                            
532005        MOVE IDFAKT-WS      TO 6308-IDRAPPNR                              
532105        MOVE INL-KDFRAKT    TO 6308-KDFRAKT                               
532205        MOVE INL-IDKUNDRF   TO 6308-IDKUNDRF                              
532305        MOVE INL-IDKOLLI    TO 6308-IDKOLLI                               
532405        MOVE WS-KVANTMOT    TO 6308-KVLEVANM                              
532505        ADD  WS-KVSKROT     TO 6308-KVLEVANM                              
532605        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
532705        AND (NDC-NA         AND NDC-CA)                                   
532805        OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                          
532905        AND (NDC-NA         AND NDC-US)                                   
533005            MOVE '11'       TO 6308-KDANMORS                              
533105        ELSE                                                              
533205            MOVE '61'       TO 6308-KDANMORS                              
533305        END-IF                                                            
533405                                                                          
533505        IF DIST79-DEALER-PRICE                                            
533605          MOVE INL-PRARTNTO    TO 6308-PRARTBTO-LOC                       
533705          MOVE ZERO            TO 6308-PRARTBTO                           
533805          MOVE INL-KDVALISO    TO 6308-KDVALISO                           
533905        ELSE                                                              
534005          IF DIST35-NONVCC-NONVCC-REFILL                                  
534006          OR DIST35-NONVCC-NONVCC-TRANSFER                                
534105          OR DIST35-NONVCC-VCC-REFILL                                     
534205          OR DIST35-NONVCC-CDC-REFILL                                     
534206          OR DIST35-NONVCC-VCC-TRANSFER                                   
534305**** HERE WE MOVE AVERAGE COST                                            
534405            MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                    
534505            MOVE ZERO                 TO 6308-PRARTBTO-LOC                
534605            MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                    
534705          ELSE                                                            
534805            MOVE INL-PRARTNTO    TO 6308-PRARTBTO                         
534905            MOVE ZERO            TO 6308-PRARTBTO-LOC                     
535005            MOVE 'SEK'           TO 6308-KDVALISO                         
535105          END-IF                                                          
535205        END-IF                                                            
535305                                                                          
535405        MOVE W-IDARTNR      TO 6308-IDARTNR                               
535505        MOVE ZERO           TO 6308-IDRADNR                               
535605                               6308-KDEMBLEV                              
535705        MOVE W-DAGENS-DATUM TO 6308-TILEVANM                              
535805                                                                          
535905        PERFORM IMS-ISRT-WL630521                                         
536005        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
536105           ADD +1 TO 6308-IDRADNR                                         
536205           PERFORM IMS-ISRT-WL630521                                      
536305        END-PERFORM                                                       
536405     END-IF                                                               
536505     .                                                                    
536605     EJECT                                                                
536705                                                                          
536805 S04-SKAPA-EKOTRANS-A03 SECTION.                                          
536905                                                                          
537005     IF WS-A03-SKAPAD = JA                                                
537105        PERFORM S041-SKRIV-A03                                            
537205     END-IF                                                               
537305                                                                          
537405     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
537505        MOVE INL-IDDISTR  TO DIST35-IDDISTR                               
537605                             DIST79-IDDISTR                               
537705        IF DIST35-REFILL-NA                                               
537805           MOVE 'T10'           TO EKOTRA03-KDEKOHT                       
537905           IF DIST35-CDC-NDC51-REFILL                                     
538005              MOVE 54           TO EKOTRA03-IDFTG                         
538105           ELSE                                                           
538205              MOVE 53           TO EKOTRA03-IDFTG                         
538305           END-IF                                                         
538405        ELSE                                                              
538505           IF DIST35-REFILL-NA-JAP                                        
538605              MOVE 'I20'        TO EKOTRA03-KDEKOHT                       
538705              IF DIST35-JAP-NDC41-REFILL                                  
538805              OR DIST35-JAP-NDC43-REFILL                                  
538905              OR DIST35-JAP-NDC44-REFILL                                  
539005                 MOVE 53         TO EKOTRA03-IDFTG                        
539105              ELSE                                                        
539205                 MOVE 54         TO EKOTRA03-IDFTG                        
539305              END-IF                                                      
539405           END-IF                                                         
539505        END-IF                                                            
539605        IF DIST79-DEALER-PRICE                                            
539705          MOVE INL-KDVALISO      TO EKOTRA03-KDVALISO                     
539805        ELSE                                                              
539905          MOVE 'SEK'             TO EKOTRA03-KDVALISO                     
540005        END-IF                                                            
540105     ELSE                                                                 
540205        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
540305           AND (NDC-NA      AND NDC-CA)                                   
540405             MOVE 'T50'      TO EKOTRA03-KDEKOHT                          
540505             MOVE 54         TO EKOTRA03-IDFTG                            
540605             MOVE 'USD'      TO EKOTRA03-KDVALISO                         
540705        ELSE                                                              
540805           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
540905           AND (NDC-NA         AND NDC-US)                                
541005             MOVE 'T40'      TO EKOTRA03-KDEKOHT                          
541105             MOVE 53         TO EKOTRA03-IDFTG                            
541205             MOVE 'CAD'      TO EKOTRA03-KDVALISO                         
541305           ELSE                                                           
541405              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
541505              AND (NDC-NA         AND NDC-US)                             
541605                  MOVE 'T30' TO EKOTRA03-KDEKOHT                          
541705                  MOVE 53    TO EKOTRA03-IDFTG                            
541805                  MOVE 'USD' TO EKOTRA03-KDVALISO                         
541905              END-IF                                                      
542005           END-IF                                                         
542105        END-IF                                                            
542205     END-IF                                                               
542305                                                                          
542405     MOVE 'A03'           TO EKOTRA03-IDPTYP                              
542505******************** ANDRAT 990430                                        
542605     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
542705        MOVE WC-CDC-SE    TO EKOTRA03-IDDC-SEND                           
542805     ELSE                                                                 
542905        MOVE SEND-WS-IDDC TO EKOTRA03-IDDC-SEND                           
543005     END-IF                                                               
543105******************** ANDRAT 990430                                        
543205     MOVE INL-IDDISTR     TO EKOTRA03-IDDISTR                             
543305     MOVE INL-IDKUNDNR    TO EKOTRA03-IDKUNDNR                            
543405     MOVE ZERO            TO EKOTRA03-IDORDNR7                            
543505     MOVE INL-IDORDNR5    TO EKOTRA03-IDORDNR7                            
543605     MOVE INL-PRARTNTO    TO EKOTRA03-PRARTNTO                            
543705                                                                          
543805     MOVE INL-TIINLINL    TO WS-SEKEL-KOLL                                
543905                             WS-EKOA03-AAMMDD                             
544005     IF WS-SEKEL = 9                                                      
544105        MOVE 19           TO WS-EKOA03-SS                                 
544205     ELSE                                                                 
544305        MOVE 20           TO WS-EKOA03-SS                                 
544405     END-IF                                                               
544505     MOVE WS-AAAAMMDD     TO EKOTRA03-DAINLINL                            
544605                             EKOTRA03-DAFAKT                              
544705     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
544805        MOVE WS-TIFAKT       TO WS-SEKEL-KOLL                             
544905                                WS-EKOA03-AAMMDD                          
545005        IF WS-SEKEL = 9                                                   
545105           MOVE 19           TO WS-EKOA03-SS                              
545205        ELSE                                                              
545305           MOVE 20           TO WS-EKOA03-SS                              
545405        END-IF                                                            
545505        MOVE WS-AAAAMMDD     TO EKOTRA03-DAFAKT                           
545605     END-IF                                                               
545705                                                                          
545805     MOVE INL-KVAVIS      TO EKOTRA03-KVLEVART                            
545905     MOVE INL-PRKURS      TO EKOTRA03-PRKURS                              
546005     MOVE INL-IDKOLLI     TO EKOTRA03-IDKOLLI                             
546105     MOVE WS-OLD-KVLS     TO EKOTRA03-KVLS-OLD                            
546205     MOVE REC-WS-IDDC     TO EKOTRA03-IDDC-REC                            
546305     MOVE IDFAKT-WS       TO EKOTRA03-IDFAKT                              
546405     MOVE W-IDARTNR       TO EKOTRA03-IDARTNR                             
546505     MOVE W-KDPRODSL      TO EKOTRA03-KDPRODSL                            
546605     MOVE W-KDPRODSL-LOC  TO EKOTRA03-KDPSLLOC                            
546705     MOVE SPACE           TO EKOTRA03-FLSLUT                              
546805                                                                          
546905     MOVE JA              TO WS-A03-SKAPAD                                
547005     .                                                                    
547105     EJECT                                                                
547205                                                                          
547305 S041-SKRIV-A03 SECTION.                                                  
547405                                                                          
547505     MOVE 'W6030900'        TO FIL-IDPGM IN FIL-WDR801                    
547605     MOVE W-DAGENS-DATUM    TO FIL-TIREGDAT                               
547705     ADD +1                 TO W-TIKLOCK                                  
547805     MOVE W-TIKLOCK         TO FIL-TIKLOCK IN FIL-WDR801                  
547905     ADD +1                 TO W-IDSEKVNR-A03                             
548005     MOVE W-IDSEKVNR-A03    TO FIL-IDSEKVNR IN FIL-WDR801                 
548105     MOVE 'W510'            TO FIL-CT-IDSYSTEM IN FIL-WDR801              
548205     MOVE 'A03'             TO FIL-CT-IDPTYP IN FIL-WDR801                
548305     MOVE ' '               TO FIL-CT-IDVTYP IN FIL-WDR801                
548405     MOVE EKOTRA03-W510A03  TO FIL-WDR801-DATA IN FIL-WDR801              
548505                                                                          
548605     PERFORM IMS-ISRT-WLFILB01                                            
548705                                                                          
548805     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
548905        ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                              
549005        PERFORM IMS-ISRT-WLFILB01                                         
549105     END-PERFORM                                                          
549205                                                                          
549305     MOVE NEJ TO WS-A03-SKAPAD                                            
549405     .                                                                    
549505     EJECT                                                                
549605                                                                          
549705 S01-HAEMTA-ANROPSDATA SECTION.                                           
549805                                                                          
549905     MOVE 'GETARG'               TO SUB-KDFUNC                            
550005     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
550105     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
550205                                                                          
550305     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
550405                                                                          
550505     IF SUB-KDRC > 0                                                      
550605       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
550705       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
550805       DELIMITED BY SIZE INTO FELTEXT                                     
550905       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
551005     END-IF                                                               
551105     .                                                                    
551205     SKIP3                                                                
551305 S02-RETURNERA-SVAR SECTION.                                              
551405                                                                          
551505     MOVE 'RETURN'                   TO SUB-KDFUNC                        
551605     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
551705                                                                          
551805     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
551905                                                                          
552005     IF SUB-KDRC > 0                                                      
552105       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
552205       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
552305       DELIMITED BY SIZE INTO FELTEXT                                     
552405       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
552505     END-IF                                                               
552605     .                                                                    
552705     EJECT                                                                
552805                                                                          
552905 S06-OMRAKN-MEDELPRIS-LAB SECTION.                                        
553005                                                                          
553105***EXISTING SECTION RENAMED TO MEDELPRIS AND                              
553205***THIS SECTION COPIED FROM W6030900                                      
553305     MOVE REC-WS-IDDC    TO AVG-IDDC                                      
553405     MOVE SLAG-PRAVCOST  TO AVG-PRAVCOST-OLD                              
553505     COMPUTE AVG-KVLS-OLD = WS-OLD-KVLS + WS-OLD-KVEFRS                   
553605     MOVE INL-PRARTNTO   TO AVG-PRARTNTO                                  
553705     MOVE W-KDPRODSL-LOC TO AVG-KDPSLLOC                                  
553805     MOVE W-KDPRODSL     TO AVG-KDPRODSL                                  
553905     MOVE W-IDFKNGRP     TO AVG-IDFKNGRP                                  
554005     MOVE WS-KDVALISO    TO AVG-KDVALISO                                  
554105     MOVE INL-IDDISTR    TO DIST79-IDDISTR                                
554205                                                                          
554305     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
554405        IF DIST79-DEALER-PRICE                                            
554505          MOVE '010'      TO AVG-KDCALL                                   
554605          MOVE 1.0        TO AVG-PRKURS                                   
554705          MOVE 'USD'      TO AVG-KDVALISO                                 
554805        ELSE                                                              
554905          MOVE '010'      TO AVG-KDCALL                                   
555005          MOVE INL-PRKURS TO AVG-PRKURS                                   
555105        END-IF                                                            
555205     ELSE                                                                 
555305        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
555405        AND (NDC-NA         AND NDC-CA)                                   
555505          IF DIST79-DEALER-PRICE                                          
555605            MOVE '030'      TO AVG-KDCALL                                 
555705            MOVE 1.0        TO AVG-PRKURS                                 
555805            MOVE 'USD'      TO AVG-KDVALISO                               
555905          ELSE                                                            
556005            MOVE '030'      TO AVG-KDCALL                                 
556105            MOVE INL-PRKURS TO AVG-PRKURS                                 
556205          END-IF                                                          
556305        ELSE                                                              
556405           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
556505           AND (NDC-NA         AND NDC-US)                                
556605             IF DIST79-DEALER-PRICE                                       
556705               MOVE '030'      TO AVG-KDCALL                              
556805               MOVE 1.0        TO AVG-PRKURS                              
556905               MOVE 'USD'      TO AVG-KDVALISO                            
557005             ELSE                                                         
557105               MOVE '030'        TO AVG-KDCALL                            
557205               MOVE INL-PRKURS   TO AVG-PRKURS                            
557305             END-IF                                                       
557405           ELSE                                                           
557505              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
557605              AND (NDC-NA         AND NDC-US)                             
557705                IF DIST79-DEALER-PRICE                                    
557805                  MOVE '020'      TO AVG-KDCALL                           
557905                  MOVE 1.0        TO AVG-PRKURS                           
558005                  MOVE 'USD'      TO AVG-KDVALISO                         
558105                ELSE                                                      
558205                  MOVE '020'      TO AVG-KDCALL                           
558305                  MOVE ZERO       TO AVG-PRKURS                           
558405                END-IF                                                    
558505              END-IF                                                      
558605           END-IF                                                         
558705        END-IF                                                            
558805     END-IF                                                               
558905                                                                          
559005     MOVE W-DAGENS-DATUM(1:2)     TO AVG-TIAA                             
559105     MOVE W-DAGENS-DATUM(3:2)     TO AVG-TIMM                             
559205     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
559305                        AVG-WDB6-PCB                                      
559405     IF AVG-KDSVAR = SPACE                                                
559505        CONTINUE                                                          
559605     ELSE                                                                 
559705        MOVE 'FEL RETURKOD FRÅN W510AVG' TO FELTEXT                       
559805        DISPLAY FELTEXT                                                   
559905        CALL FELLOG                                                       
560005     END-IF                                                               
560105     .                                                                    
560205     EJECT                                                                
560305                                                                          
560405 S06-OMRAKN-MEDELPRIS    SECTION.                                         
560505     MOVE REC-WS-IDDC    TO AVG-IDDC                                      
560605     MOVE SLAG-PRAVCOST  TO AVG-PRAVCOST-OLD                              
560705     COMPUTE AVG-KVLS-OLD = WS-OLD-KVLS + WS-OLD-KVEFRS                   
560805     MOVE INL-PRARTNTO   TO AVG-PRARTNTO                                  
560905     MOVE W-KDPRODSL-LOC TO AVG-KDPSLLOC                                  
561005     MOVE W-KDPRODSL     TO AVG-KDPRODSL                                  
561105     MOVE W-IDFKNGRP     TO AVG-IDFKNGRP                                  
561205     MOVE WS-KDVALISO    TO AVG-KDVALISO                                  
561305     MOVE INL-IDDISTR    TO DIST79-IDDISTR                                
561405                                                                          
561505     MOVE W-DAGENS-DATUM(1:2)     TO AVG-TIAA                             
561605     MOVE W-DAGENS-DATUM(3:2)     TO AVG-TIMM                             
561705                                                                          
561805     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
561905**** HÄMTA RÄTT FAKTURAMÅNAD FÖR ATT RÄKNA UT RÄTT AVERAGECOST            
562005        COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                      
562105                                  - INL-DAINLEV                           
562205        MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                    
562305        MOVE WS-FAKTURA-DATUM(3:2) TO AVG-TIAA                            
562405        MOVE WS-FAKTURA-DATUM(5:2) TO AVG-TIMM                            
562505****                                                                      
562605        IF DIST79-DEALER-PRICE                                            
562705          MOVE 1.0      TO AVG-PRKURS                                     
562805          MOVE SEND-DCS-KDVALISO  TO AVG-KDVALISO                         
562905          IF XDC-NON-VCC-OWNED                                            
563005            IF NDC-IN                                                     
563105              MOVE '012'    TO AVG-KDCALL                                 
563205            ELSE                                                          
563305              MOVE '011'    TO AVG-KDCALL                                 
563405            END-IF                                                        
563505          END-IF                                                          
563605          IF NDC-US                                                       
563705            MOVE '010'      TO AVG-KDCALL                                 
563805          END-IF                                                          
563905        ELSE                                                              
564005          IF XDC-NON-VCC-OWNED                                            
564105            IF NDC-IN                                                     
564205              MOVE '012'    TO AVG-KDCALL                                 
564305              MOVE ZERO     TO AVG-PRKURS                                 
564405            ELSE                                                          
564505              MOVE '011'    TO AVG-KDCALL                                 
564605              MOVE ZERO     TO AVG-PRKURS                                 
564705            END-IF                                                        
564805          END-IF                                                          
564905          IF NDC-US                                                       
565005            MOVE '010'      TO AVG-KDCALL                                 
565105            MOVE INL-PRKURS TO AVG-PRKURS                                 
565205          END-IF                                                          
565305        END-IF                                                            
565405     ELSE                                                                 
565505        IF ((SEND-DCS-NDC-CN)                                             
565605        AND (NDC-CN))                                                     
565705            IF DIST79-DEALER-PRICE                                        
565805               MOVE 1.0                TO AVG-PRKURS                      
565905               MOVE SEND-DCS-KDVALISO  TO AVG-KDVALISO                    
566005               MOVE '021'              TO AVG-KDCALL                      
566105            ELSE                                                          
566205               MOVE '021'    TO AVG-KDCALL                                
566305               MOVE ZERO     TO AVG-PRKURS                                
566405            END-IF                                                        
566505        ELSE                                                              
566605          IF DIST35-VCC-NONVCC-REFILL                                     
566606          OR DIST35-NONVCC-NONVCC-REFILL                                  
566607          OR DIST35-VCC-NONVCC-TRANSFER                                   
566608          OR DIST35-NONVCC-NONVCC-TRANSFER                                
566705            IF XDC-NON-VCC-OWNED                                          
566805              IF NDC-IN                                                   
566905                MOVE '012'    TO AVG-KDCALL                               
567005                MOVE ZERO     TO AVG-PRKURS                               
567105              ELSE                                                        
567205                MOVE '011'    TO AVG-KDCALL                               
567305                MOVE ZERO     TO AVG-PRKURS                               
567405              END-IF                                                      
567505            END-IF                                                        
567605          END-IF                                                          
567705        END-IF                                                            
567805     END-IF                                                               
567905                                                                          
568005     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
568105                        AVG-WDB6-PCB                                      
568205     IF AVG-KDSVAR = SPACE                                                
568305        CONTINUE                                                          
568405     ELSE                                                                 
568505        MOVE 'FEL RETURKOD FRÅN W510AVG' TO FELTEXT                       
568605        DISPLAY FELTEXT                                                   
568705        CALL FELLOG                                                       
568805     END-IF                                                               
568905     .                                                                    
569005     EJECT                                                                
569105                                                                          
569205 S07-SKAPA-NDC-HIST-SANDANDE SECTION.                                     
569305                                                                          
569405**** NYTT IDLEVNR 970918                                                  
569505     MOVE REC-DCS-IDLEVNR-DC TO INL-IDLEVNR                               
569605****                                                                      
569705     MOVE W-IDARTNR         TO ART-IDARTNR                                
569805     PERFORM IMS-ISRT-WLINLC01                                            
569905     ACCEPT W-TIAAMMDDTTMMSSTH-DATE FROM DATE                             
570005     ACCEPT W-TIAAMMDDTTMMSSTH-TIME FROM TIME                             
570105     MOVE FUNCTION CURRENT-DATE (1:2) TO W-TISEKEL                        
570205                                                                          
570305     COMPUTE W-DAINLEV = 9999999999999999                                 
570405                       - W-TIAAAAMMDDTTMMSSTH                             
570505     END-COMPUTE                                                          
570605                                                                          
570705     MOVE W-DAINLEV         TO INL-DAINLEV                                
570805     MOVE ZERO              TO INL-ADLAGOMR                               
570905                               INL-ADGANG                                 
571005                               INL-ADPLATS                                
571105     MOVE SPACE             TO INL-FLMAKUL                                
571205                               INL-FLSKAKOL                               
571305                               INL-ADINLOMR                               
571405                               INL-IDANALYS                               
571505     MOVE 'N'               TO INL-FLPRIO                                 
571605                               INL-FLTULLST                               
571705     MOVE SEND-WS-IDDC      TO INL-IDDC                                   
571805     MOVE SPACE             TO INL-IDDC-LEV                               
571905     MOVE ZERO              TO INL-IDLOPNRM                               
572005     MOVE IDFAKT-WS         TO INL-IDFAKT                                 
572105     MOVE WS-IDDISTR        TO INL-IDDISTR                                
572205     MOVE WS-IDKUNDNR       TO INL-IDKUNDNR                               
572305     MOVE SPACE             TO INL-IDKUNDRF                               
572405     MOVE WS-IDORDNR        TO INL-IDORDNR5                               
572505     MOVE WS-IDKOLLI        TO INL-IDKOLLI                                
572605     MOVE 'R34'             TO INL-IDPTYP                                 
572705     MOVE ZERO              TO INL-KDFRAKT                                
572805     MOVE SPACE             TO INL-KDKOLLI                                
572905                               INL-KDVALISO                               
573005                               INL-IDKST                                  
573105     MOVE WS-IDUSER-003     TO INL-IDUSER-003                             
573205     MOVE ZERO              TO INL-KVANTMOT                               
573305                               INL-KVART-SKROT                            
573405                               INL-KDRT                                   
573505                               INL-IDKONTO                                
573605     SUBTRACT WS-SUMMA-KVANT FROM W-KVAVIS                                
573705                      GIVING   INL-KVAVIS                                 
573805     MOVE ZERO              TO INL-PRARTNTO                               
573905                               INL-PRKURS                                 
574005                               INL-TIBERANK                               
574105                               INL-TIINLINL                               
574205                               INL-TIINLMOT                               
574305                               INL-TIINLMTI                               
574405                               INL-TIINLITI                               
574505                               INL-KVTULRET                               
574605                               INL-KVRETUR                                
574705                               INL-KDAVVANT                               
574805                               INL-TIAVIDAT                               
574905                                                                          
575005     PERFORM IMS-ISRT-WLINLC11                                            
575105     PERFORM UNTIL SEGMENT-FINNS                                          
575205       SUBTRACT 1 FROM W-DAINLEV                                          
575305       MOVE W-DAINLEV TO INL-DAINLEV                                      
575405       PERFORM IMS-ISRT-WLINLC11                                          
575505     END-PERFORM                                                          
575605     .                                                                    
575705     EJECT                                                                
575805                                                                          
575905 S08-SKAPA-SDC-NDC-HIST-MOT-PV SECTION.                                   
576005     MOVE WS-IDARTNR TO ART-IDARTNR                                       
576105                                                                          
576205     PERFORM IMS-ISRT-WLINLC01                                            
576305                                                                          
576405     ACCEPT W-TIAAMMDDTTMMSSTH-DATE FROM DATE                             
576505     ACCEPT W-TIAAMMDDTTMMSSTH-TIME FROM TIME                             
576605     MOVE FUNCTION CURRENT-DATE (1:2) TO W-TISEKEL                        
576705                                                                          
576805     COMPUTE W-DAINLEV = 9999999999999999                                 
576905                       - W-TIAAAAMMDDTTMMSSTH                             
577005     END-COMPUTE                                                          
577105                                                                          
577205     MOVE W-DAINLEV           TO INL-DAINLEV                              
577305     MOVE WS-ADLAGOMR(RAD-IX) TO INL-ADLAGOMR                             
577405     MOVE WS-ADGANG(RAD-IX)   TO INL-ADGANG                               
577505     MOVE WS-ADPLATS(RAD-IX)  TO INL-ADPLATS                              
577605     MOVE REC-WS-IDDC         TO INL-IDDC                                 
577705     MOVE SPACE               TO INL-IDDC-LEV                             
577805     MOVE IDFAKT-WS           TO INL-IDFAKT                               
577905                                                                          
578005     MOVE WS-IDDISTR          TO INL-IDDISTR                              
578105     MOVE WS-IDKUNDNR         TO INL-IDKUNDNR                             
578205     MOVE SPACE               TO INL-IDKUNDRF                             
578305     MOVE WS-IDORDNR          TO INL-IDORDNR5                             
578405     MOVE WS-IDKOLLI          TO INL-IDKOLLI                              
578505     MOVE 'R32'               TO INL-IDPTYP                               
578605     MOVE WS-IDUSER-003       TO INL-IDUSER-003                           
578705     MOVE WS-KVANTMOT         TO INL-KVANTMOT                             
578805     MOVE WS-KVSKROT          TO INL-KVART-SKROT                          
578905     MOVE ZERO                TO INL-KVAVIS                               
579005     MOVE WS-FIXAD-PRARTNTO   TO INL-PRARTNTO                             
579105     MOVE ZERO                TO INL-IDLOPNRM                             
579205                                 INL-KDFRAKT                              
579305                                 INL-PRKURS                               
579405                                 INL-TIBERANK                             
579505                                 INL-KDRT                                 
579605                                 INL-IDKONTO                              
579705                                 INL-KVTULRET                             
579805                                 INL-KVRETUR                              
579905                                 INL-KDAVVANT                             
580005                                 INL-TIAVIDAT                             
580105     MOVE SPACE               TO INL-KDVALISO                             
580205                                 INL-KDKOLLI                              
580305                                 INL-IDKST                                
580405                                 INL-FLMAKUL                              
580505                                 INL-FLSKAKOL                             
580605                                 INL-ADINLOMR                             
580705                                 INL-IDANALYS                             
580805     MOVE 'N'                 TO INL-FLPRIO                               
580905                                 INL-FLTULLST                             
581005     MOVE W-TIME-N            TO AKTUELL-TID                              
581105     PERFORM S09-FIXA-LOKALTID                                            
581205     MOVE MSGI-TILOKDAT       TO INL-TIINLINL                             
581305                                 INL-TIINLMOT                             
581405     MOVE MSGI-TILOKTID       TO INL-TIINLMTI                             
581505                                 INL-TIINLITI                             
581605     MOVE WS-KDVALISO         TO INL-KDVALISO                             
581705                                                                          
581805     PERFORM IMS-ISRT-WLINLC11                                            
581905     PERFORM UNTIL SEGMENT-FINNS                                          
582005       SUBTRACT 1 FROM W-DAINLEV                                          
582105       MOVE W-DAINLEV TO INL-DAINLEV                                      
582205       PERFORM IMS-ISRT-WLINLC11                                          
582305     END-PERFORM                                                          
582405                                                                          
582505** WRITE WDL623 IDTRACK & WDK728 WHEN FLTRACK='J' FOR NEW RECORD          
582605     IF REQU-KVANTMOT(RAD-IX) NOT = ALL '+' OR SPACES                     
582705        IF REC-DCS-FLTRACK = 'J'                                          
582805           MOVE REQU-IDTRACK TO TINL-IDTRACK                              
582905           PERFORM IMS-ISRT-WDL623                                        
583005           MOVE WS-KVANTMOT  TO WS-TRCK-KVANTMOT                          
583105           MOVE ZERO         TO WS-TRCK-KVAVIS                            
583205           PERFORM S16-UPDATE-WDK728                                      
583305        END-IF                                                            
583405     END-IF                                                               
583505*** START DELETE START !!                                                 
583506     IF REC-DCS-CDC                                                       
583507       CONTINUE                                                           
583508     ELSE                                                                 
583605       IF WS-IDDC(1:1) NOT = '7'                                          
583705            ADD  1       TO INDX-LINE                                     
583805*           MOVE 1             TO KLBL-KVANTAL(INDX-LINE)                 
583905            MOVE WS-KVANTMOT   TO KLBL-KVAVIS(INDX-LINE)                  
584005            MOVE WS-IDARTNR    TO KLBL-IDARTNR(INDX-LINE)                 
584105            MOVE ZERO          TO KLBL-IDORDNR7(INDX-LINE)                
584205            MOVE INL-IDORDNR5  TO KLBL-IDORDNR7(INDX-LINE)                
584305            MOVE INL-IDKOLLI   TO KLBL-IDKOLLI(INDX-LINE)                 
584405            MOVE '   '      TO KLBL-KRKC(INDX-LINE)                       
584505            IF WS-IDDC = 65                                               
584605              PERFORM IMS-GU-WLARTC01                                     
584705*             MOVE K6-ART-KDSORT    TO KLBL-KDSORT(INDX-LINE)             
584805              IF K6-ART-IDFKNGRP = 5222 OR 8841 OR 8842 OR 3521 OR        
584905                                   3531 OR 3532 OR 8417 OR 1912 OR        
585005                                   8441 OR 8431 OR 8121 OR 8433 OR        
585105                                   8445 OR 8443 OR 5115 OR 5125 OR        
585205                                   7713 OR 7703 OR 8369 OR 3551 OR        
                                         3567 OR 3514 OR 8361                   
585305                 MOVE '***'      TO KLBL-KRKC(INDX-LINE)                  
585405              END-IF                                                      
585505            END-IF                                                        
585605                                                                          
585705            MOVE WS-IDARTNR  TO W-IDARTNR                                 
585805            MOVE WS-IDDC     TO W-IDDC                                    
585905            PERFORM IMS-GET-WDK711                                        
586005            MOVE SLAG-ADLAGOMR  TO KLBL-ADLAGOMR(INDX-LINE)               
586105            MOVE SLAG-ADGANG    TO KLBL-ADGANG(INDX-LINE)                 
586205            MOVE SLAG-ADPLATS   TO KLBL-ADPLATS(INDX-LINE)                
586305            COMPUTE KLBL-KVOKS(INDX-LINE) = SLAG-KVROS-BULK               
586405                                          + SLAG-KVROS-DAG                
586505*** END   DELETE END   !!                                                 
586605            MOVE JA TO PRINT-SW  PRINT-SW-KOREA                           
586705       END-IF                                                             
586706     END-IF                                                               
586805     .                                                                    
586905     EJECT                                                                
587005                                                                          
587105 S08-SKAPA-SDC-NDC-HIST-MOT SECTION.                                      
587205     MOVE WS-IDARTNR TO ART-IDARTNR                                       
587305     PERFORM IMS-ISRT-WLINLC01                                            
587405                                                                          
587505     MOVE LOW-VALUE           TO W-WDL6A1KY-MIN2                          
587605     MOVE HIGH-VALUE          TO W-WDL6A1KY-MAX2                          
587705     MOVE IDFAKT-WS           TO W-IDFAKT-MIN                             
587805                                 W-IDFAKT-MAX                             
587905     PERFORM IMS-GU-WDL6A1                                                
588005     IF SEGMENT-SAKNAS                                                    
588105       ACCEPT W-TIAAMMDDTTMMSSTH-DATE FROM DATE                           
588205       ACCEPT W-TIAAMMDDTTMMSSTH-TIME FROM TIME                           
588305       MOVE FUNCTION CURRENT-DATE (1:2) TO W-TISEKEL                      
588405                                                                          
588505       COMPUTE W-DAINLEV = 9999999999999999                               
588605                         - W-TIAAAAMMDDTTMMSSTH                           
588705       END-COMPUTE                                                        
588805     ELSE                                                                 
588905**** NYA ARTIKLAR SKALL HA SAMMA DATUM SOM URSPRUNGSFAKTURAN              
589005       MOVE WDL6A1-SEQA-DAINLEV TO W-DAINLEV                              
589105     END-IF                                                               
589205                                                                          
589305     MOVE W-DAINLEV           TO INL-DAINLEV                              
589405     MOVE WS-ADLAGOMR(RAD-IX) TO INL-ADLAGOMR                             
589505     MOVE WS-ADGANG(RAD-IX)   TO INL-ADGANG                               
589605     MOVE WS-ADPLATS(RAD-IX)  TO INL-ADPLATS                              
589705     MOVE REC-WS-IDDC         TO INL-IDDC                                 
589805     IF WS-6302-IDDC-LEV NOT = SPACE                                      
589905       MOVE WS-6302-IDDC-LEV  TO INL-IDDC-LEV                             
590005     ELSE                                                                 
590105       MOVE SPACE             TO INL-IDDC-LEV                             
590205     END-IF                                                               
590305     MOVE IDFAKT-WS           TO INL-IDFAKT                               
590405                                                                          
590505     MOVE WS-IDDISTR          TO INL-IDDISTR                              
590605     MOVE WS-IDKUNDNR         TO INL-IDKUNDNR                             
590705     MOVE SPACE               TO INL-IDKUNDRF                             
590805     MOVE WS-IDORDNR          TO INL-IDORDNR5                             
                                       W-IDORDER                                
                                                                                
590905     MOVE WS-IDKOLLI          TO INL-IDKOLLI                              
                                       W-IDKOLLI                                
                                                                                
591005     MOVE 'R32'               TO INL-IDPTYP                               
591105     MOVE WS-IDUSER-003       TO INL-IDUSER-003                           
591205     MOVE WS-KVANTMOT         TO INL-KVANTMOT                             
591305     MOVE WS-KVSKROT          TO INL-KVART-SKROT                          
591405     MOVE ZERO                TO INL-KVAVIS                               
591505     MOVE WS-FIXAD-PRARTNTO   TO INL-PRARTNTO                             
591605     MOVE ZERO                TO INL-IDLOPNRM                             
591705                                 INL-KDFRAKT                              
591805                                 INL-PRKURS                               
591905                                 INL-TIBERANK                             
592005                                 INL-KDRT                                 
592105                                 INL-IDKONTO                              
592205                                 INL-KVTULRET                             
592305                                 INL-KVRETUR                              
592405                                 INL-KDAVVANT                             
592505                                 INL-TIAVIDAT                             
592605     MOVE SPACE               TO INL-KDVALISO                             
592705                                 INL-KDKOLLI                              
592805                                 INL-FLMAKUL                              
592905                                 INL-FLSKAKOL                             
593005                                 INL-IDKST                                
593105                                 INL-ADINLOMR                             
593205                                 INL-IDANALYS                             
593305     MOVE 'N'                 TO INL-FLPRIO                               
593405                                 INL-FLTULLST                             
593505     MOVE W-TIME-N            TO AKTUELL-TID                              
593605     PERFORM S09-FIXA-LOKALTID                                            
593705     MOVE MSGI-TILOKDAT    TO INL-TIINLINL                                
593805                              INL-TIINLMOT                                
593905     MOVE MSGI-TILOKTID    TO INL-TIINLMTI                                
594005                              INL-TIINLITI                                
594105     MOVE WS-KDVALISO         TO INL-KDVALISO                             
594205     PERFORM IMS-ISRT-WLINLC11                                            
594305     PERFORM UNTIL SEGMENT-FINNS                                          
594405       SUBTRACT 1 FROM W-DAINLEV                                          
594505       MOVE W-DAINLEV TO INL-DAINLEV                                      
594605       PERFORM IMS-ISRT-WLINLC11                                          
594705     END-PERFORM                                                          
594805** WRITE WDL623 IDTRACK & WDK728 WHEN FLTRACK='J' FOR NEW RECORD          
594905     IF REQU-KVANTMOT(RAD-IX) NOT = ALL '+' OR SPACES                     
595005        IF REC-DCS-FLTRACK = 'J'                                          
595105           MOVE REQU-IDTRACK TO TINL-IDTRACK                              
595205           PERFORM IMS-ISRT-WDL623                                        
595305           MOVE WS-KVANTMOT  TO WS-TRCK-KVANTMOT                          
595405           MOVE ZERO         TO WS-TRCK-KVAVIS                            
595505           PERFORM S16-UPDATE-WDK728                                      
595605        END-IF                                                            
595705     END-IF                                                               
595805*** START DELETE START !!                                                 
595905     IF WS-IDDC(1:1) NOT = '7'                                            
596005          ADD  1       TO INDX-LINE                                       
596105*         MOVE 1             TO KLBL-KVANTAL(INDX-LINE)                   
596205          MOVE WS-KVANTMOT   TO KLBL-KVAVIS(INDX-LINE)                    
596305          MOVE WS-IDARTNR    TO KLBL-IDARTNR(INDX-LINE)                   
596405          MOVE ZERO          TO KLBL-IDORDNR7(INDX-LINE)                  
596505          MOVE INL-IDORDNR5  TO KLBL-IDORDNR7(INDX-LINE)                  
596605          MOVE INL-IDKOLLI   TO KLBL-IDKOLLI(INDX-LINE)                   
596705          MOVE '   '      TO KLBL-KRKC(INDX-LINE)                         
596805          IF WS-IDDC = 65                                                 
596905            PERFORM IMS-GU-WLARTC01                                       
597005*           MOVE K6-ART-KDSORT    TO KLBL-KDSORT(INDX-LINE)               
597105            IF K6-ART-IDFKNGRP =  5222 OR 8841 OR 8842 OR 3521 OR         
597205                                  3531 OR 3532 OR 8417 OR 1912 OR         
597305                                  8441 OR 8431 OR 8121 OR 8433 OR         
597405                                  8445 OR 8443 OR 5115 OR 5125 OR         
597505                                  7713 OR 7703 OR 8369 OR 3551 OR         
                                        3567 OR 3514 OR 8361                    
597605               MOVE '***'      TO KLBL-KRKC(INDX-LINE)                    
597705            END-IF                                                        
597805          END-IF                                                          
597905                                                                          
598005          MOVE WS-IDARTNR  TO W-IDARTNR                                   
598105          MOVE WS-IDDC     TO W-IDDC                                      
598205          PERFORM IMS-GET-WDK711                                          
598305          MOVE SLAG-ADLAGOMR  TO KLBL-ADLAGOMR(INDX-LINE)                 
598405          MOVE SLAG-ADGANG    TO KLBL-ADGANG(INDX-LINE)                   
598505          MOVE SLAG-ADPLATS   TO KLBL-ADPLATS(INDX-LINE)                  
598605          COMPUTE KLBL-KVOKS(INDX-LINE) = SLAG-KVROS-BULK                 
598705                                        + SLAG-KVROS-DAG                  
598805*** END   DELETE END   !!                                                 
598905     MOVE JA TO PRINT-SW  PRINT-SW-KOREA                                  
599005      END-IF                                                              
599105     .                                                                    
599205     EJECT                                                                
599305 S09-FIXA-LOKALTID SECTION.                                               
599405                                                                          
599505     PERFORM IMS-GU-WDB601-REC                                            
599605     MOVE '011'            TO MSGI-KDCALL                                 
599705     MOVE REC-DCS-IDTIDZON     TO MSGI-IDTIDZON                           
           MOVE REC-DCS-IDDC         TO MSGI-IDDC                               
599805     MOVE W-DAGENS-DATUM   TO MSGI-TILOKDAT                               
599905     MOVE AKTUELL-TTMM     TO MSGI-TILOKTID                               
600005                                                                          
600105     CALL WL01TIDZ   USING      MSGI-WL01TIDZ                             
600205     IF MSGI-KDSVAR = 'F'                                                 
600305        MOVE 'FEL RETURKOD FRÅN WL01TIDZ' TO FELTEXT                      
600405        DISPLAY FELTEXT                                                   
600505        CALL FELLOG                                                       
600605     END-IF                                                               
           MOVE MSGI-TILOKTID     TO AKTUELL-TTMM                               
600705     .                                                                    
600805     EJECT                                                                
600905                                                                          
601005 S11-FLYTTA-SALDOLOGG-DATA SECTION.                                       
601105     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
601205     ACCEPT W-TIKLOCK                FROM TIME                            
601305     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
601405     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - W-TIKLOCK                 
601505     MOVE 9                   TO LOGG-IDSEKVNR                            
601605     MOVE 'INBO'              TO LOGG-IDHUVTYP                            
601705     MOVE 'R32'               TO LOGG-IDSUBTYP                            
601805     MOVE 'WL010900'          TO LOGG-IDPGM                               
601905     MOVE 'L109'              TO LOGG-IDTRANS                             
602005     MOVE MSG-SIGNON-USERID   TO LOGG-IDUSER                              
602105     MOVE SPACE               TO LOGG-REF                                 
602205     MOVE WS-IDKUNDNR         TO LOGG-IDKUNDNR                            
602305     MOVE WS-IDORDNR          TO LOGG-IDORDNR5                            
602405     MOVE WS-IDARTNR          TO LOGG-IDARTNR                             
602505     MOVE REC-WS-IDDC         TO LOGG-IDDC                                
602605     MOVE WS-IDFAKT           TO LOGG-IDFAKT                              
602705*   ---SALDOFÖRÄNDRINGAR PÅ WDK711 ELLER WDK611                           
602805*   ---LOGGAS PÅ WDL9                                                     
602905     IF REC-DCS-CDC                                                       
603005       MOVE CLAG-KVLS         TO LOGG-KVLS                                
603105       COMPUTE LOGG-KVAKS     = CLAG-KVAKS-CDC                            
603205                              + CLAG-KVAKS-T                              
603305     ELSE                                                                 
603405       MOVE SLAG-KVLS         TO LOGG-KVLS                                
603505       MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                             
603605     END-IF                                                               
603705     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
603805     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
603905     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
604005     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
604105     MOVE WS-KVANTMOT         TO LOGG-KVART-SALDO                         
604205     MOVE '00000000'          TO LOGG-DAREGDAT-LADD                       
604305     .                                                                    
604405     EJECT                                                                
604505 S12-ISRT-SALDOLOGG SECTION.                                              
604605     PERFORM IMS-ISRT-WDL901                                              
604705     IF SEGMENT-FINNS-REDAN                                               
604805       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
604905         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
605005         PERFORM IMS-ISRT-WDL901                                          
605105       END-PERFORM                                                        
605205     END-IF                                                               
605305     .                                                                    
605405     EJECT                                                                
605505 S13-UPPDATERA-WDJ9 SECTION.                                              
605605                                                                          
605705     PERFORM IMS-GU-LOCB01                                                
605805     IF SEGMENT-SAKNAS                                                    
605905       MOVE W-IDARTNR TO LOCB-ART-IDARTNR                                 
606005       PERFORM IMS-ISRT-LOCB01                                            
606105       PERFORM IMS-GU-LOCB01                                              
606205     END-IF                                                               
606305     IF SEGMENT-FINNS                                                     
606405       PERFORM UNTIL SEGMENT-SAKNAS OR LOCB-HIST-KDLOC = 'P'              
606505         PERFORM IMS-GHNP-LOCB11                                          
606605         IF SEGMENT-FINNS AND LOCB-HIST-KDLOC = 'P'                       
606705           MOVE FUNCTION CURRENT-DATE(1:8) TO LOCB-HIST-DASTODAT          
606805           MOVE REQU-IDUSER TO LOCB-HIST-IDUSER-STO                       
606905           PERFORM IMS-REPL-LOCB11                                        
607005         END-IF                                                           
607105       END-PERFORM                                                        
607205       MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-LOGG-DATUM                  
607305       MOVE FUNCTION CURRENT-DATE(9:6)  TO WS-LOGG-TID                    
607405       COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                     
607505                                                 WS-LOGG-DATUM            
607605       COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - WS-LOGG-TID           
607705       MOVE REQU-IDDC-KEY               TO LOCB-HIST-IDDC                 
607805       MOVE WS-PRIME-LOCATION           TO LOCB-HIST-KDLOC                
607905       MOVE W-ADLAGOMR-LOCB             TO LOCB-HIST-ADLAGOMR             
608005       MOVE W-ADGANG-LOCB               TO LOCB-HIST-ADGANG               
608105       MOVE W-ADPLATS-LOCB              TO LOCB-HIST-ADPLATS              
608205       MOVE REQU-IDUSER                 TO LOCB-HIST-IDUSER               
608305       MOVE SPACE                       TO LOCB-HIST-IDUSER-STO           
608405       MOVE ZERO                        TO LOCB-HIST-DASTODAT             
608505                                                                          
608605       PERFORM IMS-ISRT-LOCB11                                            
608705     END-IF                                                               
608805     .                                                                    
608905     EJECT                                                                
609005 S14-SKAPA-SAP-TRANS-PV SECTION.                                          
609105******************************************************************        
609205* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
609305* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
609405*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
609505*         LIGGER KVAR TILLS VIDARE (WDR8).                                
609605******************************************************************        
609705                                                                          
609805     MOVE 'WL010900'                  TO FIL-IDPGM IN FIL-WDR901          
609905     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
610005     MOVE WS-SAP-AAAAMMDD             TO FIL-DAREGDAT                     
610105                                         EKH-DAVERDAT                     
610205     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
610305     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR901        
610405     ADD +1                           TO W-IDSEKVNR-SAP                   
610505     MOVE W-IDSEKVNR-SAP            TO FIL-IDSEKVNR IN FIL-WDR901         
610605     IF DIST35-NONVCC-VCC-REFILL OR                                       
610705        DIST35-NONVCC-CDC-REFILL OR                                       
610706        DIST35-NONVCC-VCC-TRANSFER                                        
610805       MOVE '102'                     TO EKH-KDEKHHT                      
610905       MOVE '122'                     TO EKH-KDEKSHT                      
611005     ELSE                                                                 
611105       MOVE '503'                     TO EKH-KDEKHHT                      
611205       MOVE '504'                     TO EKH-KDEKSHT                      
611305     END-IF                                                               
611405     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
611505     MOVE SEND-WS-IDDC                TO EKH-IDDC-SEND                    
611605     MOVE REC-WS-IDDC                 TO EKH-IDDC-REC                     
611705     MOVE WS-IDDISTR                  TO EKH-IDDISTR                      
611805     MOVE WS-IDKUNDNR                 TO EKH-IDKUNDNR                     
611905                                                                          
612005     MOVE ZERO TO NOLL-RAKNARE                                            
612105     MOVE WS-IDFAKT                   TO WS-SAP-IDFAKT                    
612205     INSPECT WS-SAP-IDFAKT TALLYING NOLL-RAKNARE                          
612305          FOR LEADING ZERO                                                
612405     ADD +1 TO NOLL-RAKNARE                                               
612505     UNSTRING WS-SAP-IDFAKT         INTO EKH-IDVERGL                      
612605          WITH POINTER NOLL-RAKNARE                                       
612705                                                                          
612805     MOVE WS-SAP-PRARTSTD             TO EKH-PRARTSTD                     
612905     MOVE W-KDPRODSL                  TO EKH-KDPRODSL                     
613005     MOVE ZERO                        TO EKH-KDPSLLOC                     
613105                                         EKH-PRARTNTO                     
613205                                         EKH-PRHEMTAG                     
613305                                         EKH-PRARTSJK                     
613405                                         EKH-PRINK                        
613505                                         EKH-PRDIRLON                     
613605                                         EKH-PRDMTRL                      
613705                                         EKH-PROVRPAL                     
613805                                         EKH-SUBEL                        
613905     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
614005     MOVE SPACE                       TO EKH-FLLSBOK                      
614105     MOVE 'SEK'                       TO EKH-KDVALISO                     
614205********* EV ÄNDRING PRKURS                                               
614305     MOVE 1.00                        TO EKH-PRKURS                       
614405*********                                                                 
614505     MOVE WS-KVANTMOT                 TO EKH-KVANTAL                      
614605     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT IN                  
614705                                                      FIL-WDR901          
614805     MOVE 'L109'                      TO EKH-IDTRANS                      
614905     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER IN FIL-WDR901         
615005     MOVE ZERO                        TO EKH-BEVAT                        
615105                                         EKH-IDANALYS                     
615205                                         EKH-IDKONTO                      
615305                                         EKH-KDANMORS                     
615405                                         EKH-SUVAT                        
615505                                         EKH-KDFRAKT                      
615605                                         EKH-PRLANDCO                     
615705                                         EKH-DAAVIDAT                     
615805                                         EKH-IDAVINR                      
615905                                         EKH-KDAVVTYP                     
616005                                         EKH-KDRT                         
616105                                         EKH-KVANTMOT                     
616205                                         EKH-KVAVIS                       
616305     MOVE WS-KDSORT                   TO EKH-KDSORT                       
616405     MOVE 'SEPV'                      TO EKH-KDTRADP                      
616505     MOVE SPACE                       TO EKH-IDLEVNR                      
616605                                         EKH-IDKST                        
616705     MOVE SPACE                       TO EKH-FLDCET                       
616805     MOVE SPACE                       TO EKH-IDKUNDRF                     
616905     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
617005                                                                          
617105     PERFORM IMS-ISRT-WLSAPA01                                            
617205                                                                          
617305     PERFORM UNTIL SEGMENT-FINNS                                          
617405         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                             
617505         PERFORM IMS-ISRT-WLSAPA01                                        
617605     END-PERFORM                                                          
617705     .                                                                    
617805     EJECT                                                                
617905 S14-SKAPA-SAP-TRANS SECTION.                                             
618005******************************************************************        
618105* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
618205* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
618305*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
618405*         LIGGER KVAR TILLS VIDARE (WDR8).                                
618505******************************************************************        
618605                                                                          
618705     MOVE 'WL010900'                  TO FIL-IDPGM IN FIL-WDR801          
618805     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
618905     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
619005     MOVE WS-SAP-AAAAMMDD             TO R8-EKH-DAVERDAT                  
619105     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
619205     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR801        
619305     ADD +1                           TO W-IDSEKVNR-SAP                   
619405     MOVE W-IDSEKVNR-SAP            TO FIL-IDSEKVNR  IN FIL-WDR801        
619505     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
619605       MOVE '102'                     TO R8-EKH-KDEKHHT                   
619705**** FOR BOUNCE FLOW                                                      
619805       IF DIST35-NONVCC-NONVCC-REFILL OR                                  
619806          DIST35-NONVCC-NONVCC-TRANSFER                                   
619905         MOVE '132'                   TO R8-EKH-KDEKSHT                   
620005       ELSE                                                               
620105         MOVE '122'                   TO R8-EKH-KDEKSHT                   
620205       END-IF                                                             
620305     ELSE                                                                 
620405       IF DIST35-VCC-NONVCC-REFILL OR                                     
620406          DIST35-VCC-NONVCC-TRANSFER                                      
620505         MOVE '102'                   TO R8-EKH-KDEKHHT                   
620605         MOVE '122'                   TO R8-EKH-KDEKSHT                   
620705       ELSE                                                               
620805**** WITHIN THE COUNTRY                                                   
620905         MOVE '503'                   TO R8-EKH-KDEKHHT                   
621005         MOVE '504'                   TO R8-EKH-KDEKSHT                   
621105       END-IF                                                             
621205     END-IF                                                               
621305     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
621405     MOVE SEND-WS-IDDC                TO R8-EKH-IDDC-SEND                 
621505     MOVE REC-WS-IDDC                 TO R8-EKH-IDDC-REC                  
621605     MOVE WS-IDDISTR                  TO R8-EKH-IDDISTR                   
621705     MOVE WS-IDKUNDNR                 TO R8-EKH-IDKUNDNR                  
621805                                                                          
621905     MOVE ZERO TO NOLL-RAKNARE                                            
622005     MOVE WS-IDFAKT                   TO WS-SAP-IDFAKT                    
622105     INSPECT WS-SAP-IDFAKT TALLYING NOLL-RAKNARE                          
622205          FOR LEADING ZERO                                                
622305     ADD +1 TO NOLL-RAKNARE                                               
622405     UNSTRING WS-SAP-IDFAKT         INTO R8-EKH-IDVERGL                   
622505          WITH POINTER NOLL-RAKNARE                                       
622605                                                                          
622705     MOVE SLAG-PRAVCOST               TO R8-EKH-PRARTSTD                  
622805     MOVE W-KDPRODSL                  TO R8-EKH-KDPRODSL                  
622905     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
623005     MOVE INL-PRARTNTO                TO R8-EKH-PRARTNTO                  
623105     MOVE ZERO                        TO R8-EKH-PRHEMTAG                  
623205                                         R8-EKH-PRARTSJK                  
623305                                         R8-EKH-PRINK                     
623405                                         R8-EKH-PRDIRLON                  
623505                                         R8-EKH-PRDMTRL                   
623605                                         R8-EKH-PROVRPAL                  
623705                                         R8-EKH-SUBEL                     
623805     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
623905     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
624005********* EV ÄNDRING PRKURS                                               
624105     MOVE 1.00                        TO R8-EKH-PRKURS                    
624205*********                                                                 
624305     MOVE WS-KVANTMOT                 TO R8-EKH-KVANTAL                   
624405     MOVE 'L109'                      TO R8-EKH-IDTRANS                   
624505     MOVE ZERO                        TO R8-EKH-BEVAT                     
624605                                         R8-EKH-IDANALYS                  
624705                                         R8-EKH-IDKONTO                   
624805                                         R8-EKH-KDANMORS                  
624905                                         R8-EKH-SUVAT                     
625005                                         R8-EKH-KDFRAKT                   
625105                                         R8-EKH-PRLANDCO                  
625205                                         R8-EKH-DAAVIDAT                  
625305                                         R8-EKH-IDAVINR                   
625405                                         R8-EKH-KDAVVTYP                  
625505                                         R8-EKH-KDRT                      
625605                                         R8-EKH-KVANTMOT                  
625705                                         R8-EKH-KVAVIS                    
625805     MOVE WS-KDSORT                   TO R8-EKH-KDSORT                    
625905     MOVE SPACE                       TO R8-EKH-IDLEVNR                   
626005                                         R8-EKH-IDKST                     
626105     MOVE SPACE                       TO R8-EKH-FLDCET                    
626205     MOVE SPACE                       TO R8-EKH-IDKUNDRF                  
626305     MOVE SPACE                       TO R8-EKH-IDFAKT-EXP                
626405     MOVE REC-DCS-KDVALISO            TO R8-EKH-KDVALISO                  
626505     MOVE REC-DCS-KDTRADP             TO R8-EKH-KDTRADP                   
626605     IF NDC-CN                                                            
626705       MOVE 'W570'                    TO FIL-IDCPYTXT                     
626805                                      IN FIL-WDR801(1:4)                  
626905     ELSE                                                                 
627005       IF NDC-IN                                                          
627105         MOVE 'W515'                  TO FIL-IDCPYTXT                     
627205                                      IN FIL-WDR801(1:4)                  
627305       ELSE                                                               
627405         MOVE REC-DCS-KDTRADP         TO FIL-IDCPYTXT                     
627505                                      IN FIL-WDR801(1:4)                  
627605       END-IF                                                             
627705     END-IF                                                               
627805     MOVE 'EKHA'                      TO FIL-IDCPYTXT                     
627905                                      IN FIL-WDR801(5:4)                  
628005                                                                          
628105     PERFORM IMS-ISRT-WLFILB01                                            
628205                                                                          
628305     PERFORM UNTIL SEGMENT-FINNS                                          
628405         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
628505         PERFORM IMS-ISRT-WLFILB01                                        
628605     END-PERFORM                                                          
                                                                                
           IF REC-DCS-KDTRADP = 'BR12'                                          
              IF NOT-FIRST-REC-TRANS                                            
                                                                                
                 MOVE ZEROES               TO NOTF-IDSEKVNR                     
                                                                                
                 PERFORM S20-SEND-OPEN                                          
                 MOVE SEND-IDCOM           TO WZ04-SEND-IDCOM                   
                 PERFORM S21-SEND-PUT-PROP                                      
                 MOVE JA                   TO FIRST-REC-TRANS-SW                
              END-IF                                                            
              MOVE R8-EKH-KDEKHHT          TO NOTF-KDEKHHT                      
              MOVE R8-EKH-KDEKSHT          TO NOTF-KDEKSHT                      
              MOVE R8-EKH-DAVERDAT         TO NOTF-DAVERDAT                     
              MOVE AKTUELL-TID(1:6)        TO NOTF-TIREGTID                     
              MOVE R8-EKH-IDVERGL          TO NOTF-IDVERGL                      
              MOVE R8-EKH-IDDC-SEND        TO NOTF-IDDC                         
              MOVE WS-IDFAKT               TO NOTF-IDFAKT                       
              MOVE R8-EKH-IDKUNDNR         TO NOTF-IDKUNDNR                     
              MOVE W-IDORDER               TO NOTF-IDORDER                      
              MOVE W-IDKOLLI               TO NOTF-IDKOLLI                      
              MOVE R8-EKH-IDARTNR          TO WS-IDARTNR-Z                      
              MOVE FUNCTION TRIM (WS-IDARTNR-Z LEADING)                         
                                           TO NOTF-IDARTNR20                    
              MOVE R8-EKH-KVANTAL          TO NOTF-KVANTAL                      
              ADD  +1                      TO NOTF-IDSEKVNR                     
                                                                                
              PERFORM S22-SEND-PUT                                              
                                                                                
           END-IF                                                               
                                                                                
628705     .                                                                    
628805     EJECT                                                                
628905 S15-SKAPA-WDL6A1KY SECTION.                                              
629005                                                                          
629105     MOVE LOW-VALUE                  TO W-WDL6A1KY-MIN3                   
629205     MOVE HIGH-VALUE                 TO W-WDL6A1KY-MAX3                   
629305     MOVE IDFAKT-WS                  TO W-IDFAKT-MIN3                     
629405                                        W-IDFAKT-MAX3                     
629505     INSPECT WS-IDORDNR          REPLACING LEADING SPACE BY ZERO          
629605     MOVE   WS-IDORDNR               TO W-IDKUNDRF-MIN                    
629705                                        W-IDKUNDRF-MAX                    
629805     INSPECT WS-IDKUNDNR         REPLACING LEADING SPACE BY ZERO          
629905     MOVE   WS-IDKUNDNR               TO W-IDKUNDNR-MIN                   
630005                                         W-IDKUNDNR-MAX                   
630105     INSPECT WS-IDKOLLI         REPLACING LEADING SPACE BY ZERO           
630205     MOVE   WS-IDKOLLI               TO W-IDKOLLI-MIN                     
630305                                        W-IDKOLLI-MAX                     
630405     .                                                                    
630505     EJECT                                                                
630605 S16-UPDATE-WDK728 SECTION.                                               
630705     MOVE REQU-IDARTNR(RAD-IX) TO W-IDARTNR                               
630805     MOVE REQU-IDDC-KEY        TO W-IDDC                                  
630905     PERFORM IMS-GET-WDK711                                               
631005     MOVE TINL-IDTRACK TO W-IDTRACK                                       
631105     PERFORM IMS-GHNP-WDK728                                              
631205     IF SEGMENT-SAKNAS                                                    
631305        MOVE ALL '+'           TO WDK7-W005WDK7                           
631405        MOVE 'WDK728'          TO WDK7-IDSEGM                             
631505        MOVE W-IDARTNR         TO WDK7-IDARTNR-KFB                        
631605        MOVE REQU-IDDC-KEY     TO WDK7-IDDC-KFB                           
631705        MOVE WS-TRCK-KVANTMOT  TO WDK7-KVANTMOT                           
631805        MOVE WS-TRCK-KVAVIS    TO WDK7-KVAVIS                             
631905        MOVE WS-TRCK-KVANTMOT  TO WDK7-KVTRACK-KVAR                       
632005                                  WS-WDL3-TRCK-KVTRACK-KVAR               
632105        MOVE W-IDTRACK         TO WDK7-IDTRACK                            
632205                                                                          
632305        CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                        
632405                                          ARTC-PCB WDK7-PCB               
632505     ELSE                                                                 
632605        ADD WS-TRCK-KVANTMOT  TO TRCK-KVANTMOT                            
632705        ADD WS-TRCK-KVAVIS    TO TRCK-KVAVIS                              
632805        ADD WS-TRCK-KVANTMOT  TO TRCK-KVTRACK-KVAR                        
632905        PERFORM IMS-REPL-WDK728                                           
633005        MOVE TRCK-KVTRACK-KVAR TO WS-WDL3-TRCK-KVTRACK-KVAR               
633105     END-IF                                                               
633205**** UPDATE WDL3 LOGG DATABASE ****                                       
633305     PERFORM S17-TRACKLOG-DATA                                            
633405     PERFORM S18-ISRT-TRACKLOG                                            
633505     .                                                                    
633605     EJECT                                                                
633705                                                                          
633805 S17-TRACKLOG-DATA SECTION.                                               
633905     INITIALIZE LOGT-WDL301                                               
634005     ACCEPT WS-TID                   FROM TIME                            
634105     COMPUTE LOGT-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
634205     COMPUTE LOGT-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
634305     MOVE W-IDARTNR          TO LOGT-IDARTNR                              
634405     MOVE 9                  TO LOGT-IDSEKVNR                             
634505     MOVE 'INBO'             TO LOGT-IDHUVTYP                             
634605     MOVE 'R32'              TO LOGT-IDSUBTYP                             
634705     MOVE 'WL010900'         TO LOGT-IDPGM                                
634805     MOVE 'L109'             TO LOGT-IDTRANS                              
634905     MOVE MSG-SIGNON-USERID  TO LOGT-IDUSER                               
635005**   MOVE SPACE              TO LOGT-REF                                  
635105     MOVE WS-IDFAKT          TO LOGT-IDFAKT                               
635205     MOVE WS-IDORDNR         TO LOGT-IDKUNDRF                             
635305     MOVE WS-IDKUNDNR        TO LOGT-IDKUNDNR                             
635405     MOVE '00000000'         TO LOGT-DAREGDAT-LADD                        
635505     MOVE REQU-IDDC-KEY      TO LOGT-IDDC                                 
635605     MOVE WS-TRCK-KVANTMOT   TO LOGT-KVART-SALDO                          
635705     MOVE SLAG-KVLS          TO LOGT-KVLS                                 
635805     MOVE '+'                TO LOGT-IDTECKEN-KVLS                        
635905     MOVE '+'                TO LOGT-IDTECKEN-KVTRACK-KVAR                
636005     MOVE WS-WDL3-TRCK-KVTRACK-KVAR                                       
636105                             TO LOGT-KVTRACK-KVAR                         
637005     MOVE W-IDTRACK          TO LOGT-IDTRACK                              
640000     .                                                                    
650000     EJECT                                                                
660000                                                                          
670000 S18-ISRT-TRACKLOG SECTION.                                               
680000     PERFORM IMS-ISRT-WDL301                                              
690000     IF SEGMENT-FINNS-REDAN                                               
700000       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
710000         SUBTRACT 1 FROM LOGT-IDSEKVNR                                    
720000         PERFORM IMS-ISRT-WDL301                                          
730000       END-PERFORM                                                        
740000     END-IF                                                               
750000     .                                                                    
760000     EJECT                                                                
770000                                                                          
       S20-SEND-OPEN SECTION.                                                   
           MOVE 'OPEN'                        TO SEND-KDFUNC                    
           MOVE WS-ADDRESS-MQASYNC            TO SEND-ADDISPABS                 
           CALL WZ01SEND USING SEND-CONTROL-AREA                                
                               SEND-OPEN-AREA                                   
           IF SEND-KDRC > 0                                                     
             MOVE SEND-KDRC TO KDRC-DISPLAY                                     
             STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
             DELIMITED BY SIZE INTO FELTEXT                                     
             CALL ABEND USING RKOD-ABEND-MED-DUMP                               
           END-IF                                                               
           .                                                                    
           EJECT                                                                
                                                                                
       S21-SEND-PUT-PROP SECTION.                                               
                                                                                
           SET PROP-IX                 TO +1                                    
      *    MANDATORY PROPERTY THAT SPECIFIES THE ACTUAL DESTINATION             
           MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
           MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
           MOVE WS-ADDRESS-WHSTOCKA    TO PROP-BEPROPVALUE (PROP-IX)            
                                                                                
           SET PROP-IX              UP BY +1                                    
      *    OPTIONAL MQ MESSAGE PROPERTIES. CAN BE CASE-SENSITIVE                
           MOVE 'MQMPROP'              TO PROP-IDPROPTYPE  (PROP-IX)            
           MOVE 'CountryCode'          TO PROP-IDPROPNAME  (PROP-IX)            
           MOVE 'BR'                   TO PROP-BEPROPVALUE (PROP-IX)            
                                                                                
      *    SET THE NUMBER OF PROPERTIES (KVANTAL) SO CORRECT LENGTH             
      *    IS CALCULATED.                                                       
           SET PROP-KVANTAL            TO PROP-IX                               
                                                                                
           MOVE 'PUT'                            TO SEND-KDFUNC                 
           MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
           MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
           CALL WZ01SEND USING SEND-CONTROL-AREA                                
                               SEND-KVDLEN                                      
                               PROP-WZ04PROP                                    
           IF SEND-KDRC > 1                                                     
             MOVE SEND-KDRC TO KDRC-DISPLAY                                     
             STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
             DELIMITED BY SIZE INTO FELTEXT                                     
             CALL ABEND USING RKOD-ABEND-MED-DUMP                               
           END-IF                                                               
           .                                                                    
           EJECT                                                                
       S22-SEND-PUT SECTION.                                                    
                                                                                
           MOVE 'PUT'                            TO SEND-KDFUNC                 
           MOVE LENGTH OF NOTF-AREA              TO SEND-KVDLEN                 
           MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
           CALL WZ01SEND USING SEND-CONTROL-AREA                                
                               SEND-KVDLEN                                      
                               NOTF-AREA                                        
           IF SEND-KDRC > 1                                                     
             MOVE SEND-KDRC TO KDRC-DISPLAY                                     
             STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
             DELIMITED BY SIZE INTO FELTEXT                                     
             CALL ABEND USING RKOD-ABEND-MED-DUMP                               
           END-IF                                                               
           .                                                                    
           EJECT                                                                
                                                                                
       S23-SEND-CLOSE SECTION.                                                  
           MOVE 'CLOSE'                    TO SEND-KDFUNC                       
           MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
           CALL WZ01SEND USING SEND-CONTROL-AREA                                
                                                                                
           IF SEND-KDRC > 0                                                     
             MOVE SEND-KDRC TO KDRC-DISPLAY                                     
             STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
             DELIMITED BY SIZE INTO FELTEXT                                     
             CALL ABEND USING RKOD-ABEND-MED-DUMP                               
           END-IF                                                               
           .                                                                    
           EJECT                                                                
                                                                                
780000 S95-SEND-CLOSE SECTION.                                                  
790000                                                                          
800000     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
810000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
820000     .                                                                    
830000     EJECT                                                                
840000* --- IMS SEKTIONER ---                                                   
841000 IMS-GHU-WL630511 SECTION.                                                
842000     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X ')'                         
843000          DELIMITED BY SIZE INTO SSA1                                     
844000     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X  ')'                         
845000          DELIMITED BY SIZE INTO SSA2                                     
846000     MOVE 'GE  ' TO GODK-STATUSKODER                                      
847000     CALL CBLTDLI USING GHU GX65-PCB DLI-IO-AREA-WDGX2 SSA1 SSA2          
848000     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
848100     PERFORM IMS-STATUSKONTROLL                                           
848200     .                                                                    
848300     SKIP2                                                                
848400                                                                          
848500 IMS-ISRT-WL630521 SECTION.                                               
848600     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X    ')'                      
848700          DELIMITED BY SIZE INTO SSA1                                     
848800     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X      ')'                     
848900          DELIMITED BY SIZE INTO SSA2                                     
849000     MOVE 'WL630521 ' TO SSA3                                             
849100     MOVE 'II  ' TO GODK-STATUSKODER                                      
849200     CALL CBLTDLI USING ISRT GX65-PCB DLI-IO-AREA-WDGX2                   
849300                                      SSA1 SSA2 SSA3                      
849400     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
849500     PERFORM IMS-STATUSKONTROLL                                           
849600     .                                                                    
849700     SKIP3                                                                
849800                                                                          
849900 IMS-ISRT-WL630511 SECTION.                                               
850000     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X  ')'                        
850100          DELIMITED BY SIZE INTO SSA1                                     
850200     MOVE 'WL630511 ' TO SSA2                                             
850300     MOVE '  ' TO GODK-STATUSKODER                                        
850400     CALL CBLTDLI USING ISRT GX65-PCB DLI-IO-AREA-WDGX2 SSA1 SSA2         
850500     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
850600     PERFORM IMS-STATUSKONTROLL                                           
850700     .                                                                    
850800     EJECT                                                                
850900                                                                          
851000 IMS-REPL-WL630511 SECTION.                                               
851100     MOVE 'IMS-REPL-WL630511 '  TO CURRENT-IMS-SECTION                    
851200                                                                          
851300     MOVE SPACE TO GODK-STATUSKODER                                       
851400     CALL CBLTDLI USING REPL GX65-PCB DLI-IO-AREA-WDGX2                   
851500     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
851600     PERFORM IMS-STATUSKONTROLL                                           
851700     .                                                                    
851800     EJECT                                                                
851900                                                                          
852000 IMS-ISRT-4506 SECTION.                                                   
852100     STRING 'WL450501(WDGXKEY  =' W-4505-KEY-X ')'                        
852200          DELIMITED BY SIZE INTO SSA1                                     
852300     MOVE 'WL450511 ' TO SSA2                                             
852400     MOVE '  ' TO GODK-STATUSKODER                                        
852500     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA-4505 SSA1 SSA2          
852600     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
852700     PERFORM IMS-STATUSKONTROLL                                           
852800     .                                                                    
852900     SKIP3                                                                
853000                                                                          
853100 IMS-GHU-WL630111 SECTION.                                                
853200     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
853300          DELIMITED BY SIZE INTO SSA1                                     
853400     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
853500          DELIMITED BY SIZE INTO SSA2                                     
853600     MOVE '  GE' TO GODK-STATUSKODER                                      
853700     CALL CBLTDLI USING GHU GX63-PCB DLI-IO-AREA-WDGX SSA1 SSA2           
853800                                                                          
853900     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
854000     PERFORM IMS-STATUSKONTROLL                                           
854100     .                                                                    
854200     SKIP3                                                                
854300 IMS-GET-WDK711 SECTION.                                                  
854400     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
854500          DELIMITED BY SIZE INTO SSA1                                     
854600     STRING 'WDK711  (IDDC    = ' W-IDDC    ')'                           
854700          DELIMITED BY SIZE INTO SSA2                                     
854800     MOVE '  GE' TO GODK-STATUSKODER                                      
854900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
855000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
855100     PERFORM IMS-STATUSKONTROLL                                           
855200     .                                                                    
855300     EJECT                                                                
855400 IMS-REPL-WDK711 SECTION.                                                 
855500     MOVE '  ' TO GODK-STATUSKODER                                        
855600     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
855700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
855800     PERFORM IMS-STATUSKONTROLL                                           
855900     .                                                                    
856000     SKIP3                                                                
856100 IMS-GHNP-WDK728 SECTION.                                                 
856200                                                                          
856300     STRING 'WDK728  (IDTRACK  =' W-IDTRACK ')'                           
856400          DELIMITED BY SIZE INTO SSA1                                     
856500     MOVE '  GE' TO GODK-STATUSKODER                                      
856600     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK728                       
856700                            SSA1                                          
856800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
856900     PERFORM IMS-STATUSKONTROLL                                           
857000     .                                                                    
857100     EJECT                                                                
857200 IMS-REPL-WDK728 SECTION.                                                 
857300                                                                          
857400     MOVE '  ' TO GODK-STATUSKODER                                        
857500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK728                       
857600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
857700     PERFORM IMS-STATUSKONTROLL                                           
857800     .                                                                    
857900     EJECT                                                                
858000 IMS-GU-WLARTC01   SECTION.                                               
859000     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
860000          DELIMITED BY SIZE INTO SSA1                                     
860100     MOVE '  GE' TO GODK-STATUSKODER                                      
860200     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-WDK6 SSA1                
860300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
860400     PERFORM IMS-STATUSKONTROLL                                           
860500     .                                                                    
860600     SKIP3                                                                
860700 IMS-GHNP-WLARTC11   SECTION.                                             
860800     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
860900          DELIMITED BY SIZE INTO SSA1                                     
861000     MOVE SPACE  TO GODK-STATUSKODER                                      
861100     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-WDK6 SSA1               
861200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
861300     PERFORM IMS-STATUSKONTROLL                                           
861400     .                                                                    
861500     SKIP3                                                                
861600 IMS-GHU-WLARTC11   SECTION.                                              
861700                                                                          
861800     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
861900          DELIMITED BY SIZE INTO SSA1                                     
862000     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
862100          DELIMITED BY SIZE INTO SSA2                                     
862200     MOVE SPACE  TO GODK-STATUSKODER                                      
862300     CALL CBLTDLI USING GHU  ARTC-PCB DLI-IO-AREA-WDK6 SSA1 SSA2          
862400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
862500     PERFORM IMS-STATUSKONTROLL                                           
862600     .                                                                    
862601     SKIP3                                                                
862610 IMS-REPL-WLARTC11 SECTION.                                               
862620                                                                          
862630     MOVE '  ' TO GODK-STATUSKODER                                        
862640     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-WDK6                    
862650     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
862660     PERFORM IMS-STATUSKONTROLL                                           
862670     .                                                                    
862700     EJECT                                                                
862800 IMS-ISRT-WLINLC01   SECTION.                                             
862900     MOVE 'WLINLC01 '      TO SSA1                                        
863000     MOVE '  II'           TO GODK-STATUSKODER                            
863100     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-WDL6 SSA1               
863200     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
863300     PERFORM IMS-STATUSKONTROLL                                           
863400     .                                                                    
863500     SKIP3                                                                
863600 IMS-ISRT-WLINLC11   SECTION.                                             
863700     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
863800          DELIMITED BY SIZE INTO SSA1                                     
863900     MOVE 'WLINLC11 '         TO SSA2                                     
864000     MOVE '  II'              TO GODK-STATUSKODER                         
864100     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2          
864200     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
864300     PERFORM IMS-STATUSKONTROLL                                           
864400     .                                                                    
864500     SKIP3                                                                
864600 IMS-GU-WDL623    SECTION.                                                
864700     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
864800          DELIMITED BY SIZE INTO SSA1                                     
864900     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
865000          DELIMITED BY SIZE INTO SSA2                                     
865100     MOVE 'WDL623 ' TO SSA3                                               
865200     MOVE '  GE' TO GODK-STATUSKODER                                      
865300     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL623                         
865400                             SSA1 SSA2 SSA3                               
865500     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
865600     PERFORM IMS-STATUSKONTROLL                                           
865700     .                                                                    
865800     EJECT                                                                
865900 IMS-ISRT-WDL623    SECTION.                                              
866000     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
866100          DELIMITED BY SIZE INTO SSA1                                     
866200     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
866300          DELIMITED BY SIZE INTO SSA2                                     
866400     MOVE 'WDL623 ' TO SSA3                                               
866500     MOVE '  II' TO GODK-STATUSKODER                                      
866600     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL623                       
866700                             SSA1 SSA2 SSA3                               
866800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
866900     PERFORM IMS-STATUSKONTROLL                                           
867000     .                                                                    
868000     EJECT                                                                
869000 IMS-09-GHU-INLC-WLINLC11 SECTION.                                        
870000     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
880000          DELIMITED BY SIZE INTO SSA1                                     
890000     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
900000          DELIMITED BY SIZE INTO SSA2                                     
910000     MOVE '  ' TO GODK-STATUSKODER                                        
920000     CALL CBLTDLI USING GHU INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2           
930000                                                                          
930100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
930200     PERFORM IMS-STATUSKONTROLL                                           
930300     .                                                                    
930400     SKIP3                                                                
930500 IMS-ISRT-WLFILC SECTION.                                                 
930600     STRING 'WLFILC01    '                                                
930700          DELIMITED BY SIZE INTO SSA1                                     
930800     MOVE '   ' TO GODK-STATUSKODER                                       
930900     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
931000     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
931100     PERFORM IMS-STATUSKONTROLL                                           
931200     .                                                                    
931300     EJECT                                                                
931400 IMS-ISRT-WLFILC2 SECTION.                                                
931500     STRING 'WLFILC01    '                                                
931600          DELIMITED BY SIZE INTO SSA1                                     
931700     MOVE '   ' TO GODK-STATUSKODER                                       
931800     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC2 SSA1              
931900     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
932000     PERFORM IMS-STATUSKONTROLL                                           
932100     .                                                                    
932200     SKIP3                                                                
932300 IMS-ISRT-WLFILC3 SECTION.                                                
932400     STRING 'WLFILC01    '                                                
932500          DELIMITED BY SIZE INTO SSA1                                     
932600     MOVE '   ' TO GODK-STATUSKODER                                       
932700     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC3 SSA1              
932800     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
932900     PERFORM IMS-STATUSKONTROLL                                           
933000     .                                                                    
933100     EJECT                                                                
933200 IMS-ISRT-WDL901 SECTION.                                                 
933300     MOVE 'WLLOGA01 ' TO SSA1                                             
933400     MOVE '  II' TO GODK-STATUSKODER                                      
933500     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
933600     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
933700     PERFORM IMS-STATUSKONTROLL                                           
933800     .                                                                    
933900     SKIP3                                                                
934000 IMS-ISRT-WLSAPA01 SECTION.                                               
934100     MOVE 'WLSAPA01 ' TO SSA1                                             
934200     MOVE '  II' TO GODK-STATUSKODER                                      
934300     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
934400     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
934500     PERFORM IMS-STATUSKONTROLL                                           
934600     .                                                                    
934700     SKIP3                                                                
934800 IMS-ISRT-WLFILB01 SECTION.                                               
934900     MOVE 'WLFILB01 ' TO SSA1                                             
935000     MOVE '  II' TO GODK-STATUSKODER                                      
935100     CALL CBLTDLI USING ISRT FILB-PCB WLFILB01 SSA1                       
935200     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
935300     PERFORM IMS-STATUSKONTROLL                                           
935400     .                                                                    
935500     SKIP3                                                                
935600 IMS-GU-LOCB01 SECTION.                                                   
935700     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
935800          DELIMITED BY SIZE INTO SSA1                                     
935900     MOVE '  GE' TO GODK-STATUSKODER                                      
936000     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-AREA-LOCB SSA1                 
936100     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
936200     PERFORM IMS-STATUSKONTROLL                                           
936300     .                                                                    
936400     EJECT                                                                
936500 IMS-ISRT-LOCB01 SECTION.                                                 
936600     MOVE 'WLLOCB01 ' TO SSA1                                             
936700     MOVE '  ' TO GODK-STATUSKODER                                        
936800     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA-LOCB SSA1               
936900     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
937000     PERFORM IMS-STATUSKONTROLL                                           
937100     .                                                                    
937200     SKIP3                                                                
937300 IMS-GHNP-LOCB11 SECTION.                                                 
937400     STRING 'WLLOCB11(IDDC     =' W-IDDC   ')'                            
937500             DELIMITED BY SIZE INTO SSA1                                  
937600     MOVE '  GE' TO GODK-STATUSKODER                                      
937700     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-AREA-LOCB SSA1               
937800     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
937900     PERFORM IMS-STATUSKONTROLL                                           
938000     .                                                                    
938100     SKIP3                                                                
938200 IMS-REPL-LOCB11 SECTION.                                                 
938300     MOVE '  ' TO GODK-STATUSKODER                                        
938400     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-AREA-LOCB                    
938500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
938600     PERFORM IMS-STATUSKONTROLL                                           
938700     .                                                                    
938800     EJECT                                                                
938900 IMS-ISRT-LOCB11 SECTION.                                                 
939000     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
939100          DELIMITED BY SIZE INTO SSA1                                     
939200     MOVE 'WLLOCB11 ' TO SSA2                                             
939300     MOVE '  II' TO GODK-STATUSKODER                                      
939400     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA-LOCB SSA1 SSA2          
939500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
939600     PERFORM IMS-STATUSKONTROLL                                           
939700     .                                                                    
939800     SKIP3                                                                
939900                                                                          
940000 IMS-GU-WDB601-REC  SECTION.                                              
940100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
940200          DELIMITED BY SIZE INTO SSA1                                     
940300     MOVE '  ' TO GODK-STATUSKODER                                        
940400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-REC SSA1             
940500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
940600     PERFORM IMS-STATUSKONTROLL                                           
940700     IF SEGMENT-SAKNAS                                                    
940800        MOVE SPACE TO REC-DCS-KDDC                                        
940900     END-IF                                                               
941000     .                                                                    
941100     EJECT                                                                
941200                                                                          
941300 IMS-GU-WDB601-SEND SECTION.                                              
941400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
941500          DELIMITED BY SIZE INTO SSA1                                     
941600     MOVE '  GE' TO GODK-STATUSKODER                                      
941700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
941800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
941900     PERFORM IMS-STATUSKONTROLL                                           
942000     IF SEGMENT-SAKNAS                                                    
942100        MOVE SPACE TO SEND-DCS-KDDC                                       
942200     END-IF                                                               
942300     .                                                                    
942400     EJECT                                                                
942500 IMS-GU-WDB616 SECTION.                                                   
942600     MOVE 'IMS-GU-WDB616 '  TO CURRENT-IMS-SECTION                        
942700                                                                          
942800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
942900          DELIMITED BY SIZE INTO SSA1                                     
943000     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
943100          DELIMITED BY SIZE INTO SSA2                                     
943200     MOVE '    ' TO GODK-STATUSKODER                                      
943300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
943400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
943500     PERFORM IMS-STATUSKONTROLL                                           
943600     .                                                                    
943700     EJECT                                                                
943800 IMS-GU-WDL6A1   SECTION.                                                 
943900     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN2                         
944000                    '&WDL6A1KY=<' W-WDL6A1KY-MAX2 ')'                     
944100          DELIMITED BY SIZE INTO SSA1                                     
944200     MOVE '  GE' TO GODK-STATUSKODER                                      
944300     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-L6A1 SSA1                     
944400     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
944500     PERFORM IMS-STATUSKONTROLL                                           
944600     .                                                                    
944700     SKIP3                                                                
944800                                                                          
944900 IMS-GU-WDL6A1-01 SECTION.                                                
945000     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN3                         
946000                    '&WDL6A1KY=<' W-WDL6A1KY-MAX3 ')'                     
947000          DELIMITED BY SIZE INTO SSA1                                     
948000     MOVE '  GE' TO GODK-STATUSKODER                                      
949000     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-L6A1 SSA1                     
950000     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
950100     PERFORM IMS-STATUSKONTROLL                                           
950200     .                                                                    
950300     SKIP3                                                                
950400                                                                          
950500 IMS-GN-WDL6A1-01 SECTION.                                                
950600                                                                          
950700     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN3                         
950800                    '&WDL6A1KY=<' W-WDL6A1KY-MAX3 ')'                     
950900          DELIMITED BY SIZE INTO SSA1                                     
951000     MOVE '  GE' TO GODK-STATUSKODER                                      
951100     CALL CBLTDLI USING GN WDL6A-PCB DLI-IO-L6A1 SSA1                     
951200     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
951300     PERFORM IMS-STATUSKONTROLL                                           
951400     .                                                                    
951500     SKIP3                                                                
951600 IMS-GU-WDL6A1-FIRST-310 SECTION.                                         
951700     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN                          
951800                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
951900                    '&IDPTYP  = ' W-IDPTYP ')'                            
952000          DELIMITED BY SIZE INTO SSA1                                     
952100     MOVE '  GE' TO GODK-STATUSKODER                                      
952200     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-L6A1 SSA1                     
952300     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
952400     PERFORM IMS-STATUSKONTROLL                                           
952500     .                                                                    
952600     EJECT                                                                
952700                                                                          
952800 IMS-GU-WDL6A1-04 SECTION.                                                
952900     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN4                         
953000                    '&WDL6A1KY=<' W-WDL6A1KY-MAX4                         
953100                    '&IDDC    = ' W-IDDC-X ')'                            
953200          DELIMITED BY SIZE INTO SSA1                                     
953300     MOVE '  GE' TO GODK-STATUSKODER                                      
953400     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-L6A1 SSA1                     
953500     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
953600     PERFORM IMS-STATUSKONTROLL                                           
953700     .                                                                    
953800     SKIP3                                                                
953900                                                                          
954000 IMS-GU-WDL6C1   SECTION.                                                 
954100     STRING 'WDL6C1  (WDL6C1KY=>' W-WDL6C1KY-MIN-X                        
954200                    '&WDL6C1KY=<' W-WDL6C1KY-MAX-X ')'                    
954300          DELIMITED BY SIZE INTO SSA1                                     
954400     MOVE '  GE' TO GODK-STATUSKODER                                      
954500     CALL CBLTDLI USING GU WDL6C-PCB DLI-IO-L6C1 SSA1                     
954600     MOVE WDL6C-STATUS-CODE TO STATUS-WS                                  
954700     PERFORM IMS-STATUSKONTROLL                                           
954800     .                                                                    
954900     SKIP3                                                                
955000                                                                          
955100 IMS-ISRT-WDL301 SECTION.                                                 
955200     MOVE 'WDL301 ' TO SSA1                                               
955300     MOVE '  II' TO GODK-STATUSKODER                                      
955400     CALL CBLTDLI USING ISRT WDL3-PCB DLI-IO-WDL301 SSA1                  
955500     MOVE WDL3-STATUS-CODE TO STATUS-WS                                   
955600     PERFORM IMS-STATUSKONTROLL                                           
955700     .                                                                    
955800     EJECT                                                                
955900 IMS-STATUSKONTROLL SECTION.                                              
956000     SET STATUS-IX TO 1                                                   
957000     SEARCH GODK-STATUS                                                   
957100       AT END                                                             
957200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
957300         DELIMITED BY SIZE INTO FELTEXT                                   
957400         CALL FELLOG                                                      
957500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
957600         CONTINUE                                                         
957700     END-SEARCH                                                           
957800     .                                                                    
957900                                                                          
958000 DB2-SELECT-TP6FAKT SECTION.                                              
958100     MOVE 000100305 TO GOOD-SQLCODECODES                                  
958200     EXEC SQL                                                             
958300         SELECT  IDFAKT                                                   
958400                                                                          
958500         INTO :RAD-IDFAKT                                                 
958600                                                                          
958700         FROM    TP6FAKT                                                  
958800                                                                          
958900     END-EXEC                                                             
959000     MOVE SQLCODE TO SQLCODE-WS                                           
960000     PERFORM DB2-STATUS-CHECK                                             
961000     .                                                                    
962000     EJECT                                                                
963000                                                                          
964000 DB2-INSERT-TP6FAKT SECTION.                                              
965000     MOVE 000       TO GOOD-SQLCODECODES                                  
965100     EXEC SQL                                                             
965200         INSERT                                                           
965300         INTO TP6FAKT                                                     
965400               (IDFAKT)                                                   
965500         VALUES                                                           
965600              (:RAD-IDFAKT)                                               
965700                                                                          
965800     END-EXEC                                                             
965900     MOVE SQLCODE TO SQLCODE-WS                                           
966000     PERFORM DB2-STATUS-CHECK                                             
966100     .                                                                    
966200     EJECT                                                                
966300                                                                          
966400 DB2-UPDATE-TP6FAKT SECTION.                                              
966500     MOVE 000     TO GOOD-SQLCODECODES                                    
966600     EXEC SQL                                                             
966700         UPDATE TP6FAKT                                                   
966800         SET IDFAKT   = :RAD-IDFAKT                                       
966900     END-EXEC                                                             
967000                                                                          
967100     MOVE SQLCODE TO SQLCODE-WS                                           
967200     PERFORM DB2-STATUS-CHECK                                             
967300     .                                                                    
967400     EJECT                                                                
967500                                                                          
967600 DB2-STATUS-CHECK     SECTION.                                            
967700     SET SQLCODE-IX TO 1                                                  
967800     SEARCH GOOD-SQLCODE                                                  
967900       AT END                                                             
968000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
968100          DELIMITED BY SIZE INTO ERROR-TEXT                               
968200          CALL ABEND USING RKOD-ABEND-DB2                                 
968300       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
968400     END-SEARCH                                                           
968500     .                                                                    
