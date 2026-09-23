000101*                                                                         
000201******************************************************************        
000301*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL010300    *        
000401******************************************************************        
000501*                                                                         
000601 ID DIVISION.                                                             
000701 PROGRAM-ID.     W6030300.                                                
000801 AUTHOR.         BO LINDAHL, FRONTEC/ TOMMIE JIVARP                       
000901 DATE-WRITTEN.   94/12/23.         /  TILLÄGG 98/03/19.                   
001001 DATE-COMPILED.                                                           
001101                                                                          
001201*    FUNKTION:                                                            
001301*        NDC/SDC INLÄGGNING                                               
001401*                                                                         
001501*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001601*                              WLINLD (WDL6)                              
001701*                              WLARTC (WDK6)                              
001801*                              WDK7                                       
001901*                              WLBENA (WDD3)                              
002001*                              WLGMTB (WDB3)                              
002101*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002201*                              WDK7                                       
002301*                              WLINLC (WDL6)                              
002401*                              WLINLE (WDL2)                              
002501*                              WLFILB (WDR8)                              
002601*                              WLFILC (WDR3)                              
002701*                              WLKOMA (WDP8)                              
002801*                              W6LOPA (W6G1)                              
002901*                              WLLOGA (WDL9)                              
003001*                              WLLOCB (WDJ9)                              
003101*                                                                         
003201*    INDATA.                                                              
003301*        TRANSAKTION: W6T303                                              
003401*        MID:         W6I30301                                            
003501*                                                                         
003601*    UTDATA.                                                              
003701*        MOD:         W6O30301                                            
003801                                                                          
003901     SKIP3                                                                
004001 ENVIRONMENT DIVISION.                                                    
004101                                                                          
004201 DATA DIVISION.                                                           
004301     EJECT                                                                
004401 WORKING-STORAGE SECTION.                                                 
004501                                                                          
004601*    -- CHECKED BY WY2000                                                 
004701 77  IDPGM                       PIC X(08)   VALUE 'W6030300'.            
004801                                                                          
004901*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005001 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005101                                                                          
005201 77  JA                          PIC X       VALUE 'J'.                   
005301 77  NEJ                         PIC X       VALUE 'N'.                   
005401                                                                          
005501 77  SW-WS-IDDC                  PIC X(2)    VALUE SPACE.                 
005601 77  SEND-WS-IDDC                PIC X(2)    VALUE SPACE.                 
005701                                                                          
005801*01  -COPY WWDCKONS                                                       
005901*01  -COPY WWDC99                                                         
006001                                                                          
006101 77  W-PLUS                      PIC X       VALUE '+'.                   
006201*  INNEHÅLLER X'3F'                                                       
006301 77  W-X3F                       PIC X       VALUE ''.                   
006401                                                                          
006501*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006601 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006701 77  INDX-2                      PIC S9(4)  VALUE +0    COMP SYNC.        
006801 77  INDX-3                      PIC S9(4)  VALUE +0    COMP SYNC.        
006901 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
007001 77  ORAD-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
007101 77  ORAD-IX-MAX                 PIC S9(9)  VALUE +5    COMP SYNC.        
007201 77  TAB-IX                      PIC S9(3)  VALUE +0    COMP-3.           
007301 77  TAB-IX-MAX                  PIC S9(3)  VALUE +12   COMP-3.           
007401 77  IX                          PIC S9(3)  VALUE +0    COMP-3.           
007501 77  IX1                         PIC 9(2)    VALUE ZERO.                  
007601 77  IX2                         PIC 9(2)    VALUE ZERO.                  
007701 77  INDX1                       PIC 9(3)    VALUE ZERO.                  
007801 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
007901                                                                          
008001*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008101 77  WS-KVANTMOT                 PIC X(6)    VALUE SPACE.                 
008201 77  WS-KVSKROT                  PIC X(7)    VALUE SPACE.                 
008301 77  WS-ADLAGOMR                 PIC X(2)    VALUE SPACE.                 
008401 77  WS-ADGANG                   PIC X(2)    VALUE SPACE.                 
008501 77  WS-ADPLATS                  PIC X(5)    VALUE SPACE.                 
008601 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
008701 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
008801 77  WS-DIFF-KVANT               PIC 9(7)    VALUE ZERO.                  
008901 77  WS-FAKTURA-DATUM            PIC X(16)   VALUE SPACE.                 
009001 77  WS-FAKTURA-DATUM2           PIC S9(16) COMP-3 VALUE ZERO.            
009101 77  WS-OLD-KVEFRS               PIC S9(7)         VALUE ZERO.            
009201                                                                          
009301 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009401     88  INDATA-OK                           VALUE 'J'.                   
009501     88  INDATA-FEL                          VALUE 'N'.                   
009601                                                                          
009701 77  INDATA-FINNS-SW             PIC X       VALUE 'N'.                   
009801     88  INDATA-FINNS                        VALUE 'J'.                   
009901     88  INDATA-SAKNAS                       VALUE 'N'.                   
010001                                                                          
010101 77  RAETT-FAKTURA-SW            PIC X       VALUE 'N'.                   
010201     88  RAETT-FAKTURA                       VALUE 'J'.                   
010301                                                                          
010401 77  FAKTURA-FINNS-SW            PIC X       VALUE 'N'.                   
010501     88  FAKTURA-FINNS                       VALUE 'J'.                   
010601     88  FAKTURA-SAKNAS                      VALUE 'N'.                   
010701                                                                          
010801 77  NYUPPLAEGG-SW               PIC X       VALUE 'N'.                   
010901     88  NYUPPLAEGG                          VALUE 'J'.                   
011001                                                                          
011101 77  NY-BEFINTLIG-ART-SW         PIC X       VALUE 'N'.                   
011201     88  NY-BEFINTLIG-ART                    VALUE 'J'.                   
011301                                                                          
011401 77  NY-NYUPPLAEGG-ART-SW        PIC X       VALUE 'N'.                   
011501     88  NY-NYUPPLAEGG-ART                   VALUE 'J'.                   
011601                                                                          
011701 77  NY-SKROTNING-SW             PIC X       VALUE 'N'.                   
011801     88  NY-SKROTNING                        VALUE 'J'.                   
011901                                                                          
012001 77  NY-SKROT-SW                 PIC X       VALUE 'N'.                   
012101     88  NY-SKROT                            VALUE 'J'.                   
012201                                                                          
012301 77  NY-KVANTMOT-SW              PIC X       VALUE 'N'.                   
012401     88  NY-KVANTMOT                         VALUE 'J'.                   
012501                                                                          
012601 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012701     88  NYCKLAR-OK                          VALUE 'J'.                   
012801     88  NYCKLAR-FEL                         VALUE 'N'.                   
012901                                                                          
013001 77  TRANS-OHUVUD-DAM-SKAPAD-SW  PIC X       VALUE 'N'.                   
013101     88  TRANS-OHUVUD-DAM-SKAPAD             VALUE 'J'.                   
013201                                                                          
013301                                                                          
013401 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013501     88  EGEN-MID                            VALUE '6303'.                
013601     88  GODK-MID                            VALUE '6303'.                
013701     88  HOPP-MID                            VALUE '6302'.                
013801     88  HELP-MID                            VALUE '0551'.                
013901                                                                          
014001 01  STEXT     PIC X(3)  VALUE SPACE.                                     
014101                                                                          
014201 01  TEST-IDDISTR                PIC S9(5) COMP-3.                        
014301*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
014401     EJECT                                                                
014501                                                                          
014601*    --- GENERELLA ARBETSAREAOR.                                          
014701 01  W.                                                                   
014801     05  W-KDPRODSL              PIC  9(2).                               
014901     05  W-KDPRODSL-LOC          PIC  9(2).                               
014902     05  W-IDFKNGRP              PIC  9(5)         VALUE ZERO.            
015001     05  W-TEMP-KVANT            PIC S9(7)         VALUE ZERO.            
015101     05  W-DIFF-KVANT            PIC S9(7)         VALUE ZERO.            
015201     05  W-DAGENS-DATUM          PIC 9(6).                                
015301     05  W-KDFRAKT               PIC S9(3) COMP-3  VALUE ZERO.            
015401     05  W-FLSKAKOL              PIC X             VALUE SPACE.           
015501     05  WS-KVTILLGANG           PIC S9(7)V9(1)    VALUE ZERO.            
015601     05  WS-OLD-KVLS             PIC S9(7)         VALUE ZERO.            
015701     05  WS-KVBEHOV              PIC S9(7)V9(1)    VALUE ZERO.            
015801     05  WS-DIFF                 PIC S9(7)V9(1)    VALUE ZERO.            
015901     05  WS-FIXAD-PRARTNTO       PIC S9(7)V9(2)    VALUE ZERO.            
016001     05  WS-6308-TF-PRARTNTO     PIC S9(7)V9(2)    VALUE ZERO.            
016101     05  WS-6308-TF-KDVALISO     PIC X(3)          VALUE SPACE.           
016201     05  WS-KVAR-AVISERAT        PIC S9(7)  COMP-3 VALUE ZERO.            
016301     05  WS-BAATORDER            PIC X             VALUE SPACE.           
016401     05  WS-FLYGORDER            PIC X             VALUE SPACE.           
016501     05  WS-FAKTURA-KLAR         PIC X             VALUE SPACE.           
016601     05  WS-A03-SKAPAD           PIC X             VALUE SPACE.           
016701     05  WS-KDVALISO             PIC X(3)          VALUE SPACE.           
016801     05  W-KDVALISO              PIC X(3)          VALUE SPACE.           
016901     05  W-PRARTNTO              PIC S9(7)V9(2)    VALUE ZERO.            
017001     05  WS-VAERDE-DIFF          PIC S9(11)V9(2)   VALUE ZERO.            
017101     05  WS-KVDISP               PIC S9(7)  COMP-3 VALUE ZERO.            
017201     05  W-TID                   PIC 9(8)          VALUE ZERO.            
017301     05  DAGENS-DATUM            PIC 9(8)          VALUE ZERO.            
017401     05  WS-TID                  PIC 9(9)          VALUE ZERO.            
017501     05  WS-KVLS                 PIC S9(7)         VALUE ZERO.            
017601     05  WS-LOGG-DATUM           PIC S9(8)         VALUE ZERO.            
017701     05  WS-LOGG-TID             PIC S9(7)         VALUE ZERO.            
017801     05  WS-PRIME-LOCATION       PIC X             VALUE 'P'.             
017901     05  WS-SAP-IDDISTR          PIC 9(5)          VALUE ZERO.            
018001     05  WS-SAP-IDKUNDNR         PIC 9(7)          VALUE ZERO.            
018101     05  WS-SAP-IDFAKT           PIC 9(7)          VALUE ZERO.            
018201     05  WS-SAP-X-IDFAKT         PIC X(7)   VALUE ZERO.                   
018301     05  WS-SAP-PRARTSTD         PIC S9(7)V9(2)    VALUE 0 COMP-3.        
018401     05  NOLL-RAKNARE            PIC S9(5)  VALUE 0 COMP-3.               
018501     05  WS-IDUSER-003           PIC X(5)   VALUE SPACE.                  
018601                                                                          
018701     EJECT                                                                
018801     05  W-SPAR-FAELT.                                                    
018901         10  W-SPAR-IDKUNDRF     PIC X(10).                               
019001         10  W-SPAR-IDKUNDNR     PIC 9(7).                                
019101         10  W-SPAR-IDKOLLI      PIC 9(5).                                
019201         10  W-SPAR-IDDC         PIC X(2).                                
019301         10  W-SPAR-IDFAKT       PIC 9(7).                                
019401         10  W-SPAR-IDSPRAAK     PIC X(3).                                
019501                                                                          
019601     05  W-NEXT-FAELT.                                                    
019701         10  W-NEXT-IDARTNR      PIC 9(9).                                
019801         10  W-NEXT-DAINLEV      PIC 9(16).                               
019901                                                                          
020001     05  W-IDORDNR-X.                                                     
020101         10  FILLER          PIC 9(2)  VALUE ZERO.                        
020201         10  W-IDORDNR-VV    PIC 9(2).                                    
020301         10  W-IDORDNR-D     PIC 9(1).                                    
020401         10  W-IDORDNR-TT    PIC 9(2).                                    
020501                                                                          
020601     05  W-IDKUNDNR-RETUR        PIC 9(6).                                
020701     05  W-IDDISTR-RETUR         PIC 9(4).                                
020801     05  W-IDKUNDNR-REFILL       PIC 9(6).                                
020901     05  W-IDDISTR-REFILL        PIC 9(4).                                
021001     05  W-IDSEKVNR              PIC S9(3)  VALUE 0   COMP-3.             
021101     05  W-IDSEKVNR-A03          PIC S9(3)  VALUE 0   COMP-3.             
021201     05  W-IDSEKVNR-SAP          PIC S9(3)  VALUE 0   COMP-3.             
021301     05  WS-SAP-AAAAMMDD         PIC 9(8)   VALUE ZERO.                   
021401     05  WS-SAP-TTMMSSTH         PIC 9(8)   VALUE ZERO.                   
021501     05  W-TIKLOCK               PIC S9(9)  VALUE 0   COMP-3.             
021601     05  W-KVAVIS                PIC S9(7)            COMP-3.             
021701     05  W-KVKOLLI-MOT           PIC S9(5)            COMP-3.             
021801     05  W-KVRADER               PIC S9(7)            COMP-3.             
021901     05  W-KVSKROT-6-X.                                                   
022001         10  W-KVSKROT-6         PIC 9(6).                                
022101                                                                          
022201     05  W-CMD                   PIC X(3).                                
022301     05  W-TEMFSINF              PIC X(40)  VALUE SPACE.                  
022401     05  W-IDARTNR-INM           PIC S9(9)            COMP-3.             
022501     05  W-KVANTMOT-INM          PIC S9(7)            COMP-3.             
022601     05  W-KVANTMOT              OCCURS 12                                
022701                                 PIC S9(7)            COMP-3.             
022801     05  W-KVSKROT               OCCURS 12                                
022901                                 PIC S9(7)            COMP-3.             
023001     05  WS-KVSKROT-INM          PIC X(7)   VALUE SPACE.                  
023101     05  WS-KVANTMOT-INM         PIC X(6)   VALUE SPACE.                  
023201     05  WS-SUMMA-KVANT          PIC S9(7)  VALUE ZERO COMP-3.            
023301     05  WS-KVSKROT-INM-NUM      PIC 9(7)   VALUE ZERO.                   
023401     05  WS-KVANTMOT-INM-NUM     PIC 9(6)   VALUE ZERO.                   
023501     05  WS-RO-KVANTMOT          PIC 9(7)   VALUE ZERO.                   
023601                                                                          
023701     05  W-LAGERPLATS-SPAR.                                               
023801         10  W-ADLAGOMR-SPAR     PIC S9(3)            COMP-3.             
023901         10  W-ADGANG-SPAR       PIC S9(3)            COMP-3.             
024001         10  W-ADPLATS-SPAR      PIC S9(5)            COMP-3.             
024101*                                                                         
024201     05  W-LAGERPLATS            OCCURS 12.                               
024301         10  W-ADLAGOMR          PIC S9(3)            COMP-3.             
024401         10  W-ADGANG            PIC S9(3)            COMP-3.             
024501         10  W-ADPLATS           PIC S9(5)            COMP-3.             
024601                                                                          
024701     05  W-LAGERPLATS-INM.                                                
024801         10  W-ADLAGOMR-INM      PIC S9(3)            COMP-3.             
024901         10  W-ADGANG-INM        PIC S9(3)            COMP-3.             
025001         10  W-ADPLATS-INM       PIC S9(5)            COMP-3.             
025101                                                                          
025201     05  W-LAGERPLATS-LOCB.                                               
025301         10  W-ADLAGOMR-LOCB     PIC S9(3)            COMP-3.             
025401         10  W-ADGANG-LOCB       PIC S9(3)            COMP-3.             
025501         10  W-ADPLATS-LOCB      PIC S9(5)            COMP-3.             
025601                                                                          
025701     03  AKTUELL-TID.                                                     
025801         05  AKTUELL-TTMM    PIC 9(4).                                    
025901         05  FILLER          PIC 9(4).                                    
026001                                                                          
026101     03  WS-BILLIT-KOLL              PIC 9(6).                            
026201     03  FILLER REDEFINES WS-BILLIT-KOLL.                                 
026301         05  WS-BILLIT-SEKEL         PIC 9(1).                            
026401         05  FILLER                  PIC 9(5).                            
026501                                                                          
026601     03  WS-SEKEL-BILLIT.                                                 
026701         05  WS-BILLIT-SS            PIC 9(2).                            
026801         05  WS-BILLIT-AAMMDD        PIC 9(6).                            
026901     03  WS-BILLIT-AAAAMMDD REDEFINES WS-SEKEL-BILLIT PIC 9(8).           
027001                                                                          
027101     03  WS-SEKEL-KOLL               PIC 9(6).                            
027201     03  FILLER REDEFINES WS-SEKEL-KOLL.                                  
027301         05  WS-SEKEL                PIC 9(1).                            
027401         05  FILLER                  PIC 9(5).                            
027501                                                                          
027601     03  WS-SEKEL-EKOA03.                                                 
027701         05  WS-EKOA03-SS            PIC 9(2).                            
027801         05  WS-EKOA03-AAMMDD        PIC 9(6).                            
027901     03  WS-AAAAMMDD REDEFINES WS-SEKEL-EKOA03 PIC 9(8).                  
028001                                                                          
028101     03  WS-SEKEL-TEST               PIC 9(6).                            
028201     03  FILLER REDEFINES WS-SEKEL-TEST.                                  
028301         05  WS-SEK                  PIC 9(1).                            
028401         05  FILLER                  PIC 9(5).                            
028501                                                                          
028601     03  WS-SEKEL-DIFF.                                                   
028701         05  WS-DIFF-SS              PIC 9(2).                            
028801         05  WS-DIFF-AAMMDD          PIC 9(6).                            
028901     03  WS-DIFF-AAAAMMDD REDEFINES WS-SEKEL-DIFF PIC 9(8).               
029001                                                                          
029101*                                                                         
029201     03  WS-IDDC-KOLL.                                                    
029301         05  FILLER                  PIC X(5) VALUE 'WIDDC'.              
029401         05  WS-IDDC-TID             PIC X(2) VALUE SPACE.                
029501         05  FILLER                  PIC X    VALUE SPACE.                
029601                                                                          
029701     05  W-TIME-X.                                                        
029801         10  W-TIME-TT       PIC 9(2).                                    
029901         10  FILLER          PIC 9(6).                                    
030001     05  W-TIME-N            REDEFINES W-TIME-X                           
030101                             PIC 9(8).                                    
030201                                                                          
030301     05  W-TIAAAAMMDDTTMMSSTH    PIC 9(16)   VALUE ZERO.                  
030401     05  FILLER                  REDEFINES W-TIAAAAMMDDTTMMSSTH.          
030501         10  W-TIAAAAMMDDTTMMSSTH-DATE                                    
030601                                 PIC 9(8).                                
030701         10  W-TIAAAAMMDDTTMMSSTH-TIME                                    
030801                                 PIC 9(8).                                
030901*                                                                         
031001     05  W-IDLOPNRM              PIC 9(9)    VALUE ZERO.                  
031101     05  W-0VVDLLLLK             REDEFINES W-IDLOPNRM.                    
031201         10 FILLER               PIC 9(1).                                
031301         10 W-VVD                PIC 9(3).                                
031401         10 W-LLLL               PIC 9(4).                                
031501         10 W-K                  PIC 9(1).                                
031601*                                                                         
031701     05  W-IDKONTO.                                                       
031801         10 FILLER               PIC X(4)   VALUE '5022'.                 
031901         10 W-IDKONTO-IDDC       PIC X(2).                                
032001         10 W-IDKONTO-KDPRODSL   PIC X(2).                                
032101         10 FILLER               PIC X(2)   VALUE '32'.                   
032201                                                                          
032301*                                                                         
032401 01  KONTROLL-SIFFRA.                                                     
032501     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
032601     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
032701     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
032801                                                                          
032901 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
033001     03 FLT-LGD                  PIC S9(1) COMP SYNC VALUE +7.            
033101     03 VAEGNINGSTAL             PIC 9(7) VALUE 2121212.                  
033201     03 VAEGNTAL-LGD             PIC S9 COMP SYNC VALUE +7.               
033301     03 MODUL-10-11              PIC 9(2) VALUE 10.                       
033401     03 ALT-A-B                  PIC X(1) VALUE 'B'.                      
033501*                                                                         
033601 01  W-IDARTNR-NYCKEL-SPAR       PIC S9(9)  VALUE ZERO COMP-3.            
033701 01  W-IDDC-NYCKEL-SPAR          PIC X(2)   VALUE SPACE.                  
033801 01  W-DAINLEV-NYCKEL-SPAR       PIC 9(16)  VALUE ZERO.                   
033901 01  WS-SPARAT-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
034001 01  WS-SPARAT-IDDC              PIC X(2)   VALUE SPACE.                  
034101*                                                                         
034201 01  BIN-TABELL.                                                          
034301     03 BIN-TAB-RAD OCCURS 12.                                            
034401        05  BIN-TAB-IDFAKT       PIC 9(7).                                
034501        05  BIN-TAB-IDKUNDNR     PIC 9(7).                                
034601        05  BIN-TAB-IDKUNDRF     PIC X(10).                               
034701        05  BIN-TAB-IDKOLLI      PIC 9(5).                                
034801*                                                                         
034901     EJECT                                                                
035001                                                                          
035101*01 -COPY WWPRODSL                                                        
035201     EJECT                                                                
035301*    --- DISTRIKT                                                         
035401*01 -COPY WWDIST35                                                        
035501     EJECT                                                                
035601*    --- EKONOMITRANS                                                     
035701*01 -COPY W510A03               -PRE EKOTRA03-                            
035801     EJECT                                                                
035901*01 -COPY W335PRIS                                                        
036001     EJECT                                                                
036101*01 -COPY W510AVG                                                         
036201     EJECT                                                                
036301*01 -COPY W61236                -PRE FILC-                                
036401     EJECT                                                                
036501*01 -COPY W61244                -PRE FILC2-                               
036601     EJECT                                                                
036701*01 -COPY W61247                -PRE FILC3-                               
036801     EJECT                                                                
036901*01 -COPY W61227                -PRE FILC4-                               
037001     EJECT                                                                
037101*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
037201 01  GENERELLA-SUBPROGRAM.                                                
037301     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
037401     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
037501     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
037601     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
037701     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
037801     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
037901     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
038001     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
038101     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
038201     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
038301     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
038401     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
038501     EJECT                                                                
038601*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
038701*01 -COPY W005WDK7                                                        
038801     EJECT                                                                
038901*    --- PARAMETRAR TILL SUBPROGRAM W005WDL7                              
039001*01 -COPY W005WDL7                                                        
039101     EJECT                                                                
039201*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
039301*01 -COPY WMSGINIT                                                        
039401     EJECT                                                                
039501*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
039601*01 -COPY WMEDAREA                                                        
039701     SKIP3                                                                
039801 01  MESSAGE-CODES.                                                       
039901     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
040001     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
040101     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
040201     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
040301     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
040401     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
040501     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
040601     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
040701     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
040801     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
040901     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
041001     SKIP3                                                                
041101 01  MESSAGE-TEXTS.                                                       
041201     03  INF-KOLLI-EJ-RAPPORTERAT-TEXT                                    
041301                                 PIC X(25)   VALUE                        
041401         'CASE NOT YET UNLOADED'.                                         
041501     03  INF-VALUE-TOO-HIGH-TEXT                                          
041601                                 PIC X(32)   VALUE                        
041701         'THE VALUE OF THE QTY IS TOO HIGH'.                              
041801     03  UPDATING-NOT-ALLOWED-WRONG-DC                                    
041901                                 PIC X(31)   VALUE                        
042001         'UPDATING NOT ALLOWED - WRONG DC'.                               
042101     03  MARKUP-IS-MISSING                                                
042201                                 PIC X(18)   VALUE                        
042301         'MARKUP IS MISSING'.                                             
042401                                                                          
042501     EJECT                                                                
042601 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
042701     SKIP3                                                                
042801 01  DAT-IO-AREA.                                                         
042901*    03  -COPY WDATAREA                                                   
043001     EJECT                                                                
043101*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
043201*                                                                         
043301 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
043401     SKIP3                                                                
043501*01  MID -COPY W6I30301                                                   
043601     EJECT                                                                
043701 01  FILLER                      PIC X(16)   VALUE                        
043801                                 '6302-MID-AREA'.                         
043901     SKIP3                                                                
044001*01  MID -COPY W6I30201          -PRE 6302-                               
044101     EJECT                                                                
044201 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
044301     SKIP3                                                                
044401*01  -COPY WMSGAREA                                                       
044501     EJECT                                                                
044601     03  MOD REDEFINES MSG-AREA.                                          
044701*      05  -COPY W6O30301                                                 
044801     EJECT                                                                
044901     03  MOD-MOD REDEFINES MSG-AREA.                                      
045001*      05  -COPY W6O30201   -PRE MOD-.                                    
045101     EJECT                                                                
045201 01  IN-AREA.                                                             
045301*    03  MID-MID-W6I30201   -COPY W6I30201 -PRE MID-.                     
045401*    03  MID-MOD-W6O30201   -COPY W6O30201 -PRE MID-.                     
045501     EJECT                                                                
045601 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
045701 01  P-TO-P-AREA.                                                         
045801     03  P-TO-P-LL               PIC S9(4)            COMP SYNC.          
045901     03  P-TO-P-Z1               PIC  X(1)   VALUE LOW-VALUE.             
046001     03  P-TO-P-Z2               PIC  X(1)   VALUE LOW-VALUE.             
046101     03  P-TO-P-TRANSKOD         PIC  X(7).                               
046201     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
046301     03  P-TO-P-FROM-MID         PIC  X(4).                               
046401     03  P-TO-P-KDMFSFOR         PIC  X(1).                               
046501     03  P-TO-P-DATA             PIC  X(1000).                            
046601                                                                          
046701     EJECT                                                                
046801 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA2'.        
046901 01  P-TO-P-AREA-2.                                                       
047001     03  P-TO-P-LL-2             PIC S9(4)            COMP SYNC.          
047101     03  P-TO-P-Z1-2             PIC  X(1)   VALUE LOW-VALUE.             
047201     03  P-TO-P-Z2-2             PIC  X(1)   VALUE LOW-VALUE.             
047301     03  P-TO-P-TRANSKOD-2       PIC  X(7).                               
047401     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
047501     03  P-TO-P-FROM-MID-2       PIC  X(4).                               
047601     03  P-TO-P-KDMFSFOR-2       PIC  X(1).                               
047701     03  P-TO-P-DATA-2           PIC  X(1000).                            
047801     EJECT                                                                
047901 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
048001     SKIP3                                                                
048101*01  -COPY WMFSAREA                                                       
048201*                                                                         
048301*    --- AREOR FÖR W006KOM SUBMODUL                                       
048401*                                                                         
048501 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
048601*01  -COPY WMSGKOM                                                        
048701     EJECT                                                                
048801*                                                                         
048901 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
049001 01  KOM-IO-AREA.                                                         
049101   03  KOM-AREA                     PIC X(2457) VALUE SPACE.              
049201   03  OHUV     REDEFINES KOM-AREA.                                       
049301*    05      -COPY W4I25101   -PRE OHUV-                                  
049401     EJECT                                                                
049501   03  ORAD     REDEFINES KOM-AREA.                                       
049601*    05      -COPY W4I25201   -PRE ORAD-                                  
049701     EJECT                                                                
049801*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
049901*                                                                         
050001 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
050101     SKIP3                                                                
050201 01  NYCKLAR-TILL-DLI.                                                    
050301     03  W-IDARTNR-X.                                                     
050401         05  W-IDARTNR           PIC S9(9)              COMP-3.           
050501                                                                          
050601     03  W-IDKUNDNR-X.                                                    
050701         05  W-IDKUNDNR          PIC S9(7)              COMP-3.           
050801                                                                          
050901     03  W-DAINLEV-X.                                                     
051001         05  W-DAINLEV           PIC 9(16).                               
051101                                                                          
051201     03  W-IDDC                  PIC X(2).                                
051301     03  W-IDDC-B6-X.                                                     
051401         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
051501     03  W-IDPTYP                PIC X(3).                                
051601     03  W-IDSKYLT               PIC X(3).                                
051701     03  W-IDKUNDRF              PIC X(10).                               
051801                                                                          
051901     03  W-WDL6A1KY-MIN.                                                  
052001         05  W-SEQA-IDFAKT-MIN    PIC S9(7)             COMP-3.           
052101         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
052201         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)             COMP-3.           
052301         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)             COMP-3.           
052401         05  W-SEQA-IDARTNR-MIN   PIC S9(9)             COMP-3.           
052501         05  W-SEQA-DAINLEV-MIN   PIC 9(16).                              
052601                                                                          
052701     03  W-WDL6A1KY-MAX.                                                  
052801         05  W-SEQA-IDFAKT-MAX    PIC S9(7)             COMP-3.           
052901         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
053001         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)             COMP-3.           
053101         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)             COMP-3.           
053201         05  W-SEQA-IDARTNR-MAX   PIC S9(9)             COMP-3.           
053301         05  W-SEQA-DAINLEV-MAX   PIC 9(16).                              
053401                                                                          
053501     03  W-IDFAKT-X.                                                      
053601         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
053701                                                                          
053801     03  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
053901                                                                          
054001     03  W-6017KEY-X.                                                     
054101         05  W-6017-IDHTYP      PIC X(4)     VALUE '6017'.                
054201         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
054301                                                                          
054401     03  W-6301KEY-X.                                                     
054501         05  W-6301-IDHTYP      PIC X(4)     VALUE '6301'.                
054601         05  W-6301-IDDC        PIC X(2).                                 
054701         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
054801                                                                          
054901     03  W-6305KEY-X.                                                     
055001         05  W-6305-IDHTYP      PIC X(4)     VALUE '6305'.                
055101         05  W-6305-IDDC        PIC X(2)     VALUE LOW-VALUE.             
055201         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
055301                                                                          
055401     03  W-WDB301KY-X.                                                    
055501         05  W-IDDC-WDB3         PIC X(2)    VALUE SPACE.                 
055601         05  W-IDDISTR-WDB3      PIC S9(5)   VALUE ZERO COMP-3.           
055701         05  W-IDKUNDNR-WDB3     PIC S9(7)   VALUE ZERO COMP-3.           
055801                                                                          
055901     03  W-WDB301KY-DEF-X.                                                
056001         05  W-IDDC-WDB3-DEF     PIC X(2)    VALUE SPACE.                 
056101         05  W-IDDISTR-WDB3-DEF  PIC S9(5)   VALUE ZERO COMP-3.           
056201         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
056301                                                                          
056401     03  W-4505-KEY-X.                                                    
056501         05  FILLER              PIC X(4)    VALUE '4505'.                
056601         05  4505-IDDC           PIC X(2)    VALUE SPACE.                 
056701         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
056801                                                                          
056901     03  W-WDJ911KY-X.                                                    
057001         05  W-IDDC-WDJ9         PIC 9(2)    VALUE ZERO.                  
057101         05  W-DASTADAT          PIC S9(9)   VALUE ZERO.                  
057201         05  W-TISTATID          PIC S9(7)   VALUE ZERO.                  
057301         05  W-ADLAGOMR-WDJ9     PIC 9(2)    VALUE ZERO.                  
057401         05  W-ADGANG-WDJ9       PIC 9(2)    VALUE ZERO.                  
057501         05  W-ADPLATS-WDJ9      PIC 9(5)    VALUE ZERO.                  
057601                                                                          
057701     03 W-WDQ2C1KY-X.                                                     
057801        05  W-SEQC-IDDISTR      PIC S9(5)   VALUE +0 COMP-3.              
057901        05  W-SEQC-IDKUNDNR     PIC S9(7)   VALUE +0 COMP-3.              
058001        05  W-SEQC-IDKUNDRF.                                              
058101          07  W-SEQC-IDORDNR7   PIC 9(7)    VALUE ZERO.                   
058201          07  FILLER            PIC X(3)    VALUE SPACE.                  
058301                                                                          
058401     EJECT                                                                
058501*    --- STATUS-KOD FRÅN IMS                                              
058601 01  STATUS-WS                   PIC XX.                                  
058701     88  SEGMENT-FINNS                       VALUE '  '.                  
058801     88  INSERT-OK                           VALUE '  '.                  
058901     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
059001     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
059101     SKIP2                                                                
059201 01  GODK-STATUSKODER.                                                    
059301     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
059401     SKIP3                                                                
059501 01  SSA1                        PIC X(160).                              
059601 01  SSA2                        PIC X(128).                              
059701 01  SSA3                        PIC X(128).                              
059801     EJECT                                                                
059901*    --- IMS FUNKTIONSKODER                                               
060001*01  -COPY W0003                                                          
060101     EJECT                                                                
060201*    ---  DLI INPUT-OUTPUT AREOR                                          
060301 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
060401*01  WLLOGA01  -COPY WDL901                                               
060501     SKIP3                                                                
060601 01  FILLER                      PIC X(16)   VALUE                        
060701     'DLI-IO-AREA-WDL6'.                                                  
060801     SKIP3                                                                
060901 01  DLI-IO-AREA-WDL6.                                                    
061001     03  IO-AREA-WDL6            PIC X(300)  VALUE SPACE.                 
061101     SKIP3                                                                
061201     03  WLINLC01 REDEFINES IO-AREA-WDL6.                                 
061301*        05  -COPY WDL601                                                 
061401     EJECT                                                                
061501     03  WLINLC11 REDEFINES IO-AREA-WDL6.                                 
061601*        05  -COPY WDL611                                                 
061701     EJECT                                                                
061801     03  WLINLD01 REDEFINES IO-AREA-WDL6.                                 
061901*        05  -COPY WDL6A1                                                 
062001     EJECT                                                                
062101 01  FILLER                      PIC X(16)   VALUE                        
062201     'DLI-IO-AREA-WDK7'.                                                  
062301     SKIP3                                                                
062401 01  DLI-IO-AREA-WDK7.                                                    
062501*    03  -COPY WDK711                                                     
062601     EJECT                                                                
062701 01  FILLER                      PIC X(16)   VALUE                        
062801     'DLI-IO-AREA-WDK6'.                                                  
062901     SKIP3                                                                
063001 01  DLI-IO-AREA-WDK6.                                                    
063101     03  IO-AREA-WDK6            PIC X(900)  VALUE SPACE.                 
063201     SKIP3                                                                
063301     03  WLARTC01 REDEFINES IO-AREA-WDK6.                                 
063401*        05  -COPY WDK601   -PRE K6-                                      
063501     EJECT                                                                
063601     03  WLARTC11 REDEFINES IO-AREA-WDK6.                                 
063701*        05  -COPY WDK611                                                 
063801     EJECT                                                                
063901 01  FILLER                      PIC X(16)   VALUE                        
064001     'DLI-IO-AREA-WDGX'.                                                  
064101     SKIP3                                                                
064201 01  DLI-IO-AREA-WDGX.                                                    
064301     03  IO-AREA-WDGX            PIC X(300)  VALUE SPACE.                 
064401     SKIP3                                                                
064501     03  WL630101 REDEFINES IO-AREA-WDGX.                                 
064601*        05  -COPY WDGX6301                                               
064701     EJECT                                                                
064801     03  WL630111 REDEFINES IO-AREA-WDGX.                                 
064901*        05  -COPY WDGX6302                                               
065001     EJECT                                                                
065101 01  DLI-IO-AREA-WDGX2.                                                   
065201     03  IO-AREA-WDGX2           PIC X(300)  VALUE SPACE.                 
065301     SKIP3                                                                
065401     03  WL630511 REDEFINES IO-AREA-WDGX2.                                
065501*        05  -COPY WDGX6306                                               
065601     EJECT                                                                
065701     03  WL630521 REDEFINES IO-AREA-WDGX2.                                
065801*        05  -COPY WDGX6308                                               
065901     EJECT                                                                
066001 01  FILLER                      PIC X(16)   VALUE                        
066101     'DLI-IO-AREA-WDR8'.                                                  
066201     SKIP3                                                                
066301 01  DLI-IO-AREA-WDR8.                                                    
066401     03  IO-AREA-WDR8            PIC X(300)  VALUE SPACE.                 
066501                                                                          
066601     03  WLFILB01 REDEFINES IO-AREA-WDR8.                                 
066701*        05  -COPY WDR801                                                 
066801*        07  -COPY W510EKHA  -PRE R8- -RED FIL-WDR801-DATA                
066901     EJECT                                                                
067001 01  FILLER                      PIC X(16)   VALUE                        
067101     'DLI-IO-AREA-WDL2'.                                                  
067201     SKIP3                                                                
067301 01  DLI-IO-AREA-WDL2.                                                    
067401     03  IO-AREA-WDL2            PIC X(300)  VALUE SPACE.                 
067501     SKIP3                                                                
067601     03  WLINLE01 REDEFINES IO-AREA-WDL2.                                 
067701*        05  -COPY WDL201        -PRE INLE-                               
067801     EJECT                                                                
067901     03  WLINLE11 REDEFINES IO-AREA-WDL2.                                 
068001*        05  -COPY WDL211        -PRE INLE-                               
068101     EJECT                                                                
068201     03  WLINLE22 REDEFINES IO-AREA-WDL2.                                 
068301*        05  -COPY WDL222        -PRE INLE-                               
068401     EJECT                                                                
068501 01  DLI-IO-AREA-WDD3.                                                    
068601     03  IO-AREA-WDD3            PIC X(300)  VALUE SPACE.                 
068701     SKIP3                                                                
068801     03  WLBENA11 REDEFINES IO-AREA-WDD3.                                 
068901*        05  -COPY WDD311                                                 
069001     EJECT                                                                
069101 01  FILLER                      PIC X(16)   VALUE                        
069201     'DLI-IO-AREA-W6GX'.                                                  
069301     SKIP3                                                                
069401 01  DLI-IO-AREA-W6GX.                                                    
069501     03  IO-AREA-W6GX            PIC X(300)  VALUE SPACE.                 
069601     SKIP3                                                                
069701     03  W6LOPA11 REDEFINES IO-AREA-W6GX.                                 
069801*        05  -COPY W6GX6018                                               
069901     EJECT                                                                
070001 01  FILLER                      PIC X(16)   VALUE                        
070101     'DLI-IO-AREA-WDB3'.                                                  
070201     SKIP3                                                                
070301 01  DLI-IO-AREA-WDB3.                                                    
070401     03  IO-AREA-WDB3            PIC X(300)  VALUE SPACE.                 
070501     SKIP3                                                                
070601     03  WLGMTB01 REDEFINES IO-AREA-WDB3.                                 
070701*        05  -COPY WDB301                                                 
070801     EJECT                                                                
070901 01  DLI-IO-AREA-4505.                                                    
071001     03  IO-AREA-4505            PIC X(300)  VALUE SPACE.                 
071101     SKIP3                                                                
071201     03  WL450611 REDEFINES IO-AREA-4505.                                 
071301*        05  -COPY WDGX4506                                               
071401     EJECT                                                                
071501 01  DLI-IO-AREA-FILC.                                                    
071601     03  IO-AREA-FILC            PIC X(300)  VALUE SPACE.                 
071701     SKIP3                                                                
071801     03  WLFILC01 REDEFINES IO-AREA-FILC.                                 
071901*        05  -COPY WDR301       -PRE FILC-                                
072001     EJECT                                                                
072101 01  DLI-IO-AREA-FILC2.                                                   
072201     03  IO-AREA-FILC2           PIC X(300)  VALUE SPACE.                 
072301     SKIP3                                                                
072401     03  WLFILC01 REDEFINES IO-AREA-FILC2.                                
072501*        05  -COPY WDR301       -PRE FILC2-                               
072601     EJECT                                                                
072701 01  DLI-IO-AREA-FILC3.                                                   
072801     03  IO-AREA-FILC3           PIC X(300)  VALUE SPACE.                 
072901     SKIP3                                                                
073001     03  WLFILC01 REDEFINES IO-AREA-FILC3.                                
073101*        05  -COPY WDR301       -PRE FILC3-                               
073201     EJECT                                                                
073301 01  DLI-IO-AREA-FILC4.                                                   
073401     03  IO-AREA-FILC4           PIC X(300)  VALUE SPACE.                 
073501     SKIP3                                                                
073601     03  WLFILC01 REDEFINES IO-AREA-FILC4.                                
073701*        05  -COPY WDR301       -PRE FILC4-                               
073801     EJECT                                                                
073901 01  DLI-IO-AREA-LOCB.                                                    
074001     03  IO-AREA-LOCB            PIC X(300)  VALUE SPACE.                 
074101     SKIP3                                                                
074201     03  WLLOCB01 REDEFINES IO-AREA-LOCB.                                 
074301*        05  -COPY WDJ901       -PRE LOCB-                                
074401     EJECT                                                                
074501     03  WLLOCB11 REDEFINES IO-AREA-LOCB.                                 
074601*        05  -COPY WDJ911       -PRE LOCB-                                
074701     EJECT                                                                
074801 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
074901 01  DLI-IO-WLSAPA01.                                                     
075001*    03  WLSAPA01  -COPY WDR901                                           
075101*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
075201     EJECT                                                                
075301 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDQ2C1'.                
075401 01  DLI-IO-WDQ2C1.                                                       
075501*    03  WDQ2C1 -COPY WDQ2C1                                              
075601     EJECT                                                                
075701 01  FILLER            PIC X(16)   VALUE 'WDB601 AREA'.                   
075801 01  DLI-IO-AREA-B601.                                                    
075901*    03  -COPY WDB601                                                     
076001     EJECT                                                                
076101 01  FILLER            PIC X(16)   VALUE 'WDB601 SEND'.                   
076201 01  DLI-IO-AREA-B601-SEND.                                               
076301*    03  -COPY WDB601 -PRE SEND-                                          
076401     EJECT                                                                
076501 01  FILLER            PIC X(16)   VALUE 'WDB601 SW  '.                   
076601 01  DLI-IO-AREA-B601-SW.                                                 
076701*    03  -COPY WDB601 -PRE SW-                                            
076801     EJECT                                                                
076901 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA11'.                      
077001 01  DLI-IO-OIGA11.                                                       
077101*    03  -COPY WDL711                                                     
077201     EJECT                                                                
077301                                                                          
077401 LINKAGE SECTION.                                                         
077501                                                                          
077601*01  -COPY W0009  -PRE MSG-                                               
077701     EJECT                                                                
077801*01  -COPY W0009  -PRE ALT-                                               
077901     EJECT                                                                
078001*01  -COPY W0009  -PRE ALT2-                                              
078101     EJECT                                                                
078201*01  -COPY W0008  -PRE USEA-                                              
078301     05  FILLER                  PIC X.                                   
078401     EJECT                                                                
078501*01  -COPY W0008  -PRE 6301-                                              
078601     05  FILLER                  PIC X.                                   
078701     EJECT                                                                
078801*01  -COPY W0008  -PRE INLC-                                              
078901     05  FILLER                  PIC X.                                   
079001     EJECT                                                                
079101*01  -COPY W0008  -PRE INLD-                                              
079201     05  FILLER                  PIC X.                                   
079301     EJECT                                                                
079401*01  -COPY W0008  -PRE INLE-                                              
079501     05  FILLER                  PIC X.                                   
079601     EJECT                                                                
079701*01  -COPY W0008  -PRE ARTC-                                              
079801     05  FILLER                  PIC X.                                   
079901     EJECT                                                                
080001*01  -COPY W0008  -PRE WDK7-                                              
080101     05  FILLER                  PIC X.                                   
080201     EJECT                                                                
080301*01  -COPY W0008  -PRE PRIS-WDK7-                                         
080401     05  FILLER                  PIC X.                                   
080501     EJECT                                                                
080601*01  -COPY W0008  -PRE FILB-                                              
080701     05  FILLER                  PIC X.                                   
080801     EJECT                                                                
080901*01  -COPY W0008  -PRE BENA-                                              
081001     05  FILLER                  PIC X.                                   
081101     EJECT                                                                
081201*01  -COPY W0008  -PRE LOPA-                                              
081301     05  FILLER                  PIC X.                                   
081401     EJECT                                                                
081501*01  -COPY W0008  -PRE KOMA-                                              
081601     05  FILLER                  PIC X.                                   
081701     EJECT                                                                
081801*01  -COPY W0008  -PRE GMTA-                                              
081901     05  FILLER                  PIC X.                                   
082001     EJECT                                                                
082101*01  -COPY W0008  -PRE BETA-                                              
082201     05  FILLER                  PIC X.                                   
082301     EJECT                                                                
082401*01  -COPY W0008  -PRE GPRIA-                                             
082501     05  FILLER                  PIC X.                                   
082601     EJECT                                                                
082701*01  -COPY W0008  -PRE GPRIB-                                             
082801     05  FILLER                  PIC X.                                   
082901     EJECT                                                                
083001*01  -COPY W0008  -PRE 6305-                                              
083101     05  FILLER                  PIC X.                                   
083201     EJECT                                                                
083301*01  -COPY W0008  -PRE 9305-                                              
083401     05  FILLER                  PIC X.                                   
083501     EJECT                                                                
083601*01  -COPY W0008  -PRE AVG-WDB6-                                          
083701     05  FILLER                  PIC X.                                   
083801     EJECT                                                                
083901*01  -COPY W0008  -PRE KNDB-                                              
084001     05  FILLER                  PIC X.                                   
084101     EJECT                                                                
084201*01  -COPY W0008  -PRE 4505-                                              
084301     05  FILLER                  PIC X.                                   
084401     EJECT                                                                
084501*01  -COPY W0008  -PRE FILC-                                              
084601     05  FILLER                  PIC X.                                   
084701     EJECT                                                                
084801*01  -COPY W0008  -PRE WLLOGA-                                            
084901     05  FILLER                  PIC X.                                   
085001     EJECT                                                                
085101*01  -COPY W0008  -PRE LOCB-                                              
085201     05  FILLER                  PIC X.                                   
085301     EJECT                                                                
085401*01  -COPY W0008  -PRE SAPA-                                              
085501     05  FILLER                  PIC X.                                   
085601     EJECT                                                                
085701*01  -COPY W0008  -PRE XXKP-                                              
085801     05  FILLER                  PIC X.                                   
085901     EJECT                                                                
086001*01  -COPY W0008  -PRE WDQ2C-                                             
086101     05  FILLER                  PIC X.                                   
086201     EJECT                                                                
086301*01  -COPY W0008  -PRE PROC-                                              
086401     05  FILLER                  PIC X.                                   
086501     EJECT                                                                
086601*01  -COPY W0008  -PRE ORQI-                                              
086701     05  FILLER                  PIC X.                                   
086801     EJECT                                                                
086901 01  PRIS-COST-WDK6-PCB          PIC X.                                   
087001 01  PRIS-COST-WDK7-PCB          PIC X.                                   
087101 01  PRIS-COST-WDF1-PCB          PIC X.                                   
087201 01  PRIS-COST-9305-PCB          PIC X.                                   
087301 01  PRIS-COST-WDK72-PCB         PIC X.                                   
087401 01  PRIS-COST-WDB6-PCB          PIC X.                                   
087601*01  -COPY W0008      -PRE WDB6-                                          
087701     05  FILLER                  PIC X.                                   
087801*01  -COPY W0008      -PRE OIGA-                                          
087901     05  FILLER                  PIC X.                                   
088001     EJECT                                                                
088101 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ALT2-PCB                       
088201                           USEA-PCB 6301-PCB                              
088301                           INLC-PCB INLD-PCB INLE-PCB                     
088401                           ARTC-PCB WDK7-PCB PRIS-WDK7-PCB                
088501                           FILB-PCB BENA-PCB LOPA-PCB KOMA-PCB            
088601                           6305-PCB 9305-PCB                              
088701                           AVG-WDB6-PCB                                   
088801                           GMTA-PCB BETA-PCB GPRIA-PCB GPRIB-PCB          
088901                           KNDB-PCB                                       
089001                           4505-PCB FILC-PCB WLLOGA-PCB LOCB-PCB          
089101                           SAPA-PCB XXKP-PCB WDQ2C-PCB PROC-PCB           
089201                           ORQI-PCB                                       
089301                           PRIS-COST-WDK6-PCB                             
089401                           PRIS-COST-WDK7-PCB                             
089501                           PRIS-COST-WDF1-PCB                             
089601                           PRIS-COST-9305-PCB                             
089701                           PRIS-COST-WDK72-PCB                            
089801                           PRIS-COST-WDB6-PCB                             
090001                           WDB6-PCB OIGA-PCB.                             
090101 MAIN SECTION.                                                            
090201     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT2-PCB                       
090301                           USEA-PCB 6301-PCB                              
090401                           INLC-PCB INLD-PCB INLE-PCB                     
090501                           ARTC-PCB WDK7-PCB PRIS-WDK7-PCB                
090601                           FILB-PCB BENA-PCB LOPA-PCB KOMA-PCB            
090701                           6305-PCB 9305-PCB                              
090801                           AVG-WDB6-PCB                                   
090901                           GMTA-PCB BETA-PCB GPRIA-PCB GPRIB-PCB          
091001                           KNDB-PCB                                       
091101                           4505-PCB FILC-PCB WLLOGA-PCB LOCB-PCB          
091201                           SAPA-PCB XXKP-PCB WDQ2C-PCB PROC-PCB           
091301                           ORQI-PCB                                       
091401                           PRIS-COST-WDK6-PCB                             
091501                           PRIS-COST-WDK7-PCB                             
091601                           PRIS-COST-WDF1-PCB                             
091701                           PRIS-COST-9305-PCB                             
091801                           PRIS-COST-WDK72-PCB                            
091901                           PRIS-COST-WDB6-PCB                             
092101                           WDB6-PCB OIGA-PCB.                             
092201                                                                          
092301     PERFORM IMS-GET-MSG                                                  
092401                                                                          
092501     IF SEGMENT-FINNS                                                     
092601        PERFORM A-INIT                                                    
092701        IF MFS-UPD-X                                                      
092801           CONTINUE                                                       
092901        ELSE                                                              
093001           PERFORM B-KOLLA-NYCKLAR                                        
093101        END-IF                                                            
093201                                                                          
093301        IF NYCKLAR-OK                                                     
093401           IF MFS-UPD-X                                                   
093501              CONTINUE                                                    
093601           ELSE                                                           
093701              MOVE MID-MODFAELT-IN TO MOD-INPUT                           
093801              INSPECT MOD-INPUT REPLACING ALL W-PLUS BY W-X3F             
093901           END-IF                                                         
094001                                                                          
094101           IF (EGEN-MID AND NOT MFS-FIRST) OR HELP-MID                    
094201           OR MFS-UPD-X                                                   
094301              PERFORM I-KOLLA-INDATA-FINNS                                
094401              PERFORM G-KOLLA-INPUT                                       
094501           END-IF                                                         
094601                                                                          
094701           IF MFS-UPDATE OR MFS-UPD-V OR MFS-UPD-X                        
094801              IF INDATA-OK                                                
094901                 PERFORM H-UPPDATERA                                      
095001              END-IF                                                      
095101           ELSE                                                           
095201              IF MFS-FIRST                                                
095301                PERFORM C-FOERSTA-SIDA                                    
095401                                                                          
095501              ELSE                                                        
095601                 IF MFS-NEXT                                              
095701                    PERFORM D-NAESTA-SIDA                                 
095801                                                                          
095901                 ELSE                                                     
096001                    PERFORM I-KOLLA-INDATA-FINNS                          
096101                                                                          
096201                    PERFORM E-SAMMA-SIDA                                  
096301                                                                          
096401                 END-IF                                                   
096501              END-IF                                                      
096601           END-IF                                                         
096701                                                                          
096801           IF MFS-UPD-X                                                   
096901              CONTINUE                                                    
097001           ELSE                                                           
097101              IF INDATA-OK OR                                             
097201                 MED-IDMFSFEL = ERR-PF11-AND-NO-DATA                      
097301                 PERFORM F-LAES-VISA-INFO                                 
097401              END-IF                                                      
097501           END-IF                                                         
097601        END-IF                                                            
097701                                                                          
097801        IF MFS-UPD-X                                                      
097901           PERFORM L-STARTA-6302-TRANS                                    
098001           MOVE 6302-MID-W6I30201 TO P-TO-P-DATA-2                        
098101           PERFORM IMS-ISRT-ALT2-PCB-6302                                 
098201        ELSE                                                              
098301           COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30301 + 4                  
098401           PERFORM IMS-INSERT-MSG                                         
098501        END-IF                                                            
098601     END-IF                                                               
098701                                                                          
098801     MOVE ZERO TO RETURN-CODE                                             
098901     GOBACK                                                               
099001     .                                                                    
099101     EJECT                                                                
099201 A-INIT SECTION.                                                          
099301                                                                          
099401     IF  MSG-DUBBLA-TRANSKODER                                            
099501         IF MSG-KDTRANS-2 = 'W6T303X'                                     
099601            MOVE MSG-INDATA-MINUS-2-TRANSKODER TO IN-AREA                 
099701         ELSE                                                             
099801            MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I30301            
099901         END-IF                                                           
100001         MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                               
100101         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
100201     ELSE                                                                 
100301         IF MSG-KDTRANS-1 = 'W6T303X'                                     
100401            MOVE MSG-INDATA-MINUS-1-TRANSKOD TO IN-AREA                   
100501         ELSE                                                             
100601            MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W6I30301              
100701         END-IF                                                           
100801         MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                               
100901         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
101001     END-IF                                                               
101101                                                                          
101201     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
101301     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
101401     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
101501                                                                          
101601     MOVE LOW-VALUE       TO MSG-AREA                                     
101701     MOVE 'W6O303N1'      TO MFS-IDMOD                                    
101801     MOVE '6303'          TO MOD-IDTRANS                                  
101901     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
102001                                                                          
102101     IF  EGEN-MID OR MFS-UPD-X                                            
102201         CONTINUE                                                         
102301     ELSE                                                                 
102401         MOVE SPACE       TO MFS-KDTRTYP                                  
102501         MOVE '7'         TO MFS-IDPFK                                    
102601     END-IF                                                               
102701                                                                          
102801     MOVE SPACE           TO 4506-WDGX4506                                
102901     MOVE ZERO            TO W-IDLOPNRM                                   
103001     MOVE NEJ             TO TRANS-OHUVUD-DAM-SKAPAD-SW                   
103101     MOVE JA              TO INDATA-SW                                    
103201     MOVE NEJ             TO INDATA-FINNS-SW                              
103301                                                                          
103401     MOVE 'IDAG'          TO DAT-KDDATFORM                                
103501     CALL WDATKONV USING     DAT-KDDATFORM                                
103601                             DAT-I-TIDATUM                                
103701                             DAT-O-TIDATUM                                
103801                             DAT-KDSVAR                                   
103901                                                                          
104001     MOVE DAT-TIAAMMDD    TO W-DAGENS-DATUM                               
104101     ACCEPT W-TIME-X      FROM TIME                                       
104201     MOVE W-TIME-N        TO W-TIKLOCK                                    
104301                                                                          
104401     MOVE 'W6030300'       TO FILC-FIL-IDPGM                              
104501                              FILC2-FIL-IDPGM                             
104601                              FILC3-FIL-IDPGM                             
104701                              FILC4-FIL-IDPGM                             
104801     MOVE W-DAGENS-DATUM   TO FILC-FIL-TIREGDAT                           
104901                              FILC2-FIL-TIREGDAT                          
105001                              FILC3-FIL-TIREGDAT                          
105101                              FILC4-FIL-TIREGDAT                          
105201     MOVE 'W61236  '       TO FILC-FIL-IDCPYTXT                           
105301     MOVE 'W61244  '       TO FILC2-FIL-IDCPYTXT                          
105401     MOVE 'W61247  '       TO FILC3-FIL-IDCPYTXT                          
105501     MOVE 'W61227  '       TO FILC4-FIL-IDCPYTXT                          
105601     MOVE ZERO             TO FILC-FIL-TIKLOCK                            
105701                              FILC2-FIL-TIKLOCK                           
105801                              FILC3-FIL-TIKLOCK                           
105901                              FILC4-FIL-TIKLOCK                           
106001                                                                          
106101     MOVE +1 TO TAB-IX                                                    
106201     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
106301        MOVE ZERO TO  BIN-TAB-IDFAKT(TAB-IX)                              
106401                      BIN-TAB-IDKUNDNR(TAB-IX)                            
106501                      BIN-TAB-IDKOLLI(TAB-IX)                             
106601        MOVE SPACE TO BIN-TAB-IDKUNDRF(TAB-IX)                            
106701        ADD +1 TO TAB-IX                                                  
106801     END-PERFORM                                                          
106901                                                                          
107001     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
107101     .                                                                    
107201     EJECT                                                                
107301 B-KOLLA-NYCKLAR SECTION.                                                 
107401                                                                          
107501     MOVE ALL '+' TO MSGI-WMSGINIT                                        
107601     MOVE '001' TO MSGI-KDCALL                                            
107701     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
107801     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
107901     MOVE '6303'             TO MSGI-IDTRANS                              
108001     IF EGEN-MID OR HELP-MID OR HOPP-MID                                  
108101       IF MID-IDFAKT-IN    NUMERIC                                        
108201          MOVE MID-IDFAKT-IN   TO MSGI-IDFAKT                             
108301       END-IF                                                             
108401       MOVE MID-IDKUNDRF-IN TO MSGI-IDKUNDRF                              
108501       IF MID-IDKUNDNR-IN    NUMERIC                                      
108601          MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                           
108701       END-IF                                                             
108801       IF MID-IDKOLLI-IN    NUMERIC                                       
108901          MOVE MID-IDKOLLI-IN  TO MSGI-IDKOLLI                            
109001       END-IF                                                             
109101     END-IF                                                               
109201     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
109301     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
109401                                                                          
109501     MOVE JA                 TO NYCKLAR-SW                                
109601                                                                          
109701*    -- KONTROLL AV IDFAKT                                                
109801     MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-IN                                
109901                                                                          
110001     IF MID-IDFAKT-IN    NOT = ALL '+'                                    
110101        MOVE '7'         TO MFS-IDPFK                                     
110201        MOVE SPACE       TO MFS-KDTRTYP                                   
110301     END-IF                                                               
110401                                                                          
110501     IF  MSGI-IDFAKT        NUMERIC AND                                   
110601         MSGI-IDFAKT        > ZERO                                        
110701         MOVE MSGI-IDFAKT   TO W-SPAR-IDFAKT                              
110801     ELSE                                                                 
110901         MOVE NEJ           TO NYCKLAR-SW                                 
111001     END-IF                                                               
111101                                                                          
111201*    -- KONTROLL AV IDKUNDRF                                              
111301     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDRF-IN                              
111401                                                                          
111501     IF MID-IDKUNDRF-IN  NOT = ALL '+'                                    
111601        MOVE '7'         TO MFS-IDPFK                                     
111701        MOVE SPACE       TO MFS-KDTRTYP                                   
111801     END-IF                                                               
111901                                                                          
112001     IF  MSGI-IDKUNDRF      > SPACE                                       
112101         MOVE MSGI-IDKUNDRF TO W-SPAR-IDKUNDRF                            
112201     ELSE                                                                 
112301         MOVE NEJ           TO NYCKLAR-SW                                 
112401     END-IF                                                               
112501                                                                          
112601*    -- KONTROLL AV IDKUNDNR                                              
112701     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
112801                                                                          
112901     IF MID-IDKUNDNR-IN  NOT = ALL '+'                                    
113001        MOVE '7'         TO MFS-IDPFK                                     
113101        MOVE SPACE       TO MFS-KDTRTYP                                   
113201     END-IF                                                               
113301                                                                          
113401     IF  MSGI-IDKUNDNR      NUMERIC                                       
113501         MOVE MSGI-IDKUNDNR TO W-SPAR-IDKUNDNR                            
113601     ELSE                                                                 
113701         MOVE NEJ        TO NYCKLAR-SW                                    
113801     END-IF                                                               
113901                                                                          
114001     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-IN                               
114101                                                                          
114201     IF MID-IDKOLLI-IN   NOT = ALL '+'                                    
114301        MOVE '7'         TO MFS-IDPFK                                     
114401        MOVE SPACE       TO MFS-KDTRTYP                                   
114501     END-IF                                                               
114601                                                                          
114701     IF  MSGI-IDKOLLI     NUMERIC                                         
114801     AND MSGI-IDKOLLI     > ZERO                                          
114901         MOVE MSGI-IDKOLLI TO W-SPAR-IDKOLLI                              
115001     ELSE                                                                 
115101         MOVE NEJ        TO NYCKLAR-SW                                    
115201     END-IF                                                               
115301                                                                          
115401*    -- KONTROLL AV IDSPRAAK                                              
115501     MOVE MFS-RENSA-FAELT    TO MOD-IDSPRAK-IN                            
115601                                                                          
115701     IF MID-IDSPRAK-IN       = ALL '+' OR SPACE                           
115801        MOVE 'GB '           TO W-SPAR-IDSPRAAK                           
115901     ELSE                                                                 
116001        MOVE MID-IDSPRAK-IN  TO W-SPAR-IDSPRAAK                           
116101        MOVE '7'             TO MFS-IDPFK                                 
116201        MOVE SPACE           TO MFS-KDTRTYP                               
116301     END-IF                                                               
116401                                                                          
116501*    -- KONTROLL AV IDDC                                                  
116601     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
116701                                                                          
116801     IF MID-IDDC-IN = ALL '+'                                             
116901        MOVE MID-IDDC-UT TO W-SPAR-IDDC                                   
117001     ELSE                                                                 
117101        MOVE MID-IDDC-IN TO W-SPAR-IDDC                                   
117201        MOVE '7'         TO MFS-IDPFK                                     
117301        MOVE SPACE       TO MFS-KDTRTYP                                   
117401     END-IF                                                               
117501                                                                          
117601     IF W-SPAR-IDDC = SPACE                                               
117701        MOVE MSGI-IDDC   TO W-SPAR-IDDC                                   
117801     END-IF                                                               
117901                                                                          
118001     MOVE W-SPAR-IDDC    TO W-IDDC-B6                                     
118101                            WS-IDDC                                       
118201     PERFORM IMS-GU-WDB601                                                
118301     IF DCS-KDDC = SPACE OR DCS-DDC                                       
118401        MOVE NEJ TO NYCKLAR-SW                                            
118501     END-IF                                                               
118601                                                                          
118701     IF NYCKLAR-OK                                                        
118801        MOVE MSGI-IDFAKT         TO MOD-IDFAKT-UT                         
118901        MOVE MSGI-IDKUNDRF       TO MOD-IDKUNDRF-UT                       
119001        MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                       
119101        MOVE MSGI-IDKOLLI        TO MOD-IDKOLLI-UT                        
119201        MOVE W-SPAR-IDSPRAAK     TO MOD-IDSPRAK-UT                        
119301        MOVE W-SPAR-IDDC         TO MOD-IDDC-UT                           
119401        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
119501        INSPECT MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE           
119601     ELSE                                                                 
119701        MOVE MFS-RENSA-FAELT TO MOD-IDKUNDRF-UT                           
119801                                MOD-IDKUNDNR-UT                           
119901                                MOD-IDKOLLI-UT                            
120001                                MOD-IDSPRAK-UT                            
120101                                MOD-IDDC-UT                               
120201     END-IF                                                               
120301                                                                          
120401     IF NYCKLAR-FEL                                                       
120501        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
120601        CALL WMEDKONV USING MED-WMEDAREA                                  
120701        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
120801        PERFORM MFS-RENSA-FAELT-IN                                        
120901        PERFORM MFS-RENSA-NYCKLAR-UT                                      
121001        PERFORM MFS-RENSA-FAELT-UT                                        
121101     END-IF                                                               
121201     .                                                                    
121301     EJECT                                                                
121401 C-FOERSTA-SIDA SECTION.                                                  
121501                                                                          
121601     MOVE INF-FIRST-PAGE  TO MED-IDMFSINF                                 
121701     CALL WMEDKONV USING MED-WMEDAREA                                     
121801     MOVE MED-MFSINF      TO MOD-TEMFSFEL                                 
121901                                                                          
122001*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
122101     MOVE ZERO            TO W-NEXT-IDARTNR                               
122201                             W-NEXT-DAINLEV                               
122301     PERFORM MFS-RENSA-FAELT-IN                                           
122401     PERFORM MFS-RENSA-FAELT-UT                                           
122501     .                                                                    
122601     EJECT                                                                
122701 D-NAESTA-SIDA SECTION.                                                   
122801                                                                          
122901     IF MID-IDARTNR-NEXT  NUMERIC                                         
123001        MOVE MID-IDARTNR-NEXT TO W-NEXT-IDARTNR                           
123101        IF MID-DAINLEV-NEXT NUMERIC                                       
123201           MOVE MID-DAINLEV-NEXT TO W-NEXT-DAINLEV                        
123301        ELSE                                                              
123401           MOVE ZERO             TO W-NEXT-DAINLEV                        
123501        END-IF                                                            
123601     ELSE                                                                 
123701        MOVE ZERO             TO W-NEXT-IDARTNR                           
123801                                 W-NEXT-DAINLEV                           
123901     END-IF                                                               
124001                                                                          
124101     PERFORM MFS-RENSA-FAELT-IN                                           
124201                                                                          
124301     IF  W-NEXT-IDARTNR   = ZERO                                          
124401         MOVE INF-SISTA-SIDAN TO MED-IDMFSINF                             
124501         CALL WMEDKONV USING MED-WMEDAREA                                 
124601         MOVE MED-MFSINF      TO MOD-TEMFSFEL                             
124701                                                                          
124801         IF  MID-IDARTNR-ENTER NUMERIC                                    
124901             MOVE MID-IDARTNR-NEXT TO W-NEXT-IDARTNR                      
125001             IF MID-DAINLEV-NEXT NUMERIC                                  
125101                MOVE MID-DAINLEV-NEXT TO W-NEXT-DAINLEV                   
125201             ELSE                                                         
125301                MOVE ZERO TO W-NEXT-DAINLEV                               
125401             END-IF                                                       
125501         ELSE                                                             
125601             MOVE ZERO             TO W-NEXT-IDARTNR                      
125701                                      W-NEXT-DAINLEV                      
125801         END-IF                                                           
125901     END-IF                                                               
126001     .                                                                    
126101     EJECT                                                                
126201 E-SAMMA-SIDA SECTION.                                                    
126301                                                                          
126401     IF EGEN-MID                                                          
126501     OR HELP-MID                                                          
126601        IF MID-IDARTNR-ENTER NUMERIC                                      
126701           MOVE MID-IDARTNR-ENTER TO W-NEXT-IDARTNR                       
126801           IF MID-DAINLEV-ENTER NUMERIC                                   
126901              MOVE MID-DAINLEV-ENTER TO W-NEXT-DAINLEV                    
127001           ELSE                                                           
127101              MOVE ZERO              TO W-NEXT-DAINLEV                    
127201           END-IF                                                         
127301        ELSE                                                              
127401           MOVE ZERO              TO W-NEXT-IDARTNR                       
127501                                     W-NEXT-DAINLEV                       
127601        END-IF                                                            
127701                                                                          
127801        IF  INDATA-FINNS                                                  
127901            MOVE INF-PRESS-PF11 TO MED-IDMFSINF                           
128001            CALL WMEDKONV USING MED-WMEDAREA                              
128101            MOVE MED-MFSINF TO MOD-TEMFSFEL                               
128201        END-IF                                                            
128301     ELSE                                                                 
128401        PERFORM MFS-RENSA-FAELT-IN                                        
128501     END-IF                                                               
128601     .                                                                    
128701     EJECT                                                                
128801 F-LAES-VISA-INFO SECTION.                                                
128901                                                                          
129001     PERFORM FA-LAES-GRUNDDATA                                            
129101                                                                          
129201     IF SEGMENT-SAKNAS                                                    
129301        IF  W-TEMFSINF > SPACE                                            
129401            MOVE W-TEMFSINF  TO MOD-TEMFSINF                              
129501        ELSE                                                              
129601            MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                         
129701            CALL WMEDKONV USING MED-WMEDAREA                              
129801            MOVE MED-MFSFEL       TO MOD-TEMFSFEL                         
129901        END-IF                                                            
130001        MOVE ZERO             TO MOD-IDARTNR-NEXT                         
130101                                 MOD-IDARTNR-ENTER                        
130201                                 MOD-DAINLEV-NEXT                         
130301                                 MOD-DAINLEV-ENTER                        
130401        PERFORM MFS-STAENG-INDATA-FAELT                                   
130501        PERFORM MFS-RENSA-FAELT-UT                                        
130601     ELSE                                                                 
130701        IF SEQA-IDDC = W-SPAR-IDDC                                        
130801         IF  SEQA-IDPTYP  = 'R30'                                         
130901             MOVE INF-KOLLI-EJ-RAPPORTERAT-TEXT                           
131001                                  TO MOD-TEMFSFEL                         
131101             MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-INM                      
131201                                     MOD-KVANTMOT-INM                     
131301                                     MOD-ADLAGOMR-INM                     
131401                                     MOD-ADGANG-INM                       
131501                                     MOD-ADPLATS-INM                      
131601                                     MOD-KVSKROT-INM                      
131701                                     MOD-CMD-INM                          
131801                                                                          
131901             MOVE +1 TO INDX                                              
132001             PERFORM UNTIL INDX > MAX-INDX                                
132101                 MOVE MFS-RENSA-FAELT TO MOD-CMD-IN(INDX)                 
132201                 ADD +1 TO INDX                                           
132301             END-PERFORM                                                  
132401             PERFORM MFS-STAENG-INDATA-FAELT                              
132501         END-IF                                                           
132601                                                                          
132701         PERFORM FB-LAES-RADDATA                                          
132801         ELSE                                                             
132901           MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                          
133001           CALL WMEDKONV USING MED-WMEDAREA                               
133101           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
133201           MOVE ZERO             TO MOD-IDARTNR-NEXT                      
133301                                    MOD-IDARTNR-ENTER                     
133401                                    MOD-DAINLEV-NEXT                      
133501                                    MOD-DAINLEV-ENTER                     
133601           PERFORM MFS-STAENG-INDATA-FAELT                                
133701           PERFORM MFS-RENSA-FAELT-UT                                     
133801         END-IF                                                           
133901     END-IF                                                               
134001     .                                                                    
134101     EJECT                                                                
134201 FA-LAES-GRUNDDATA SECTION.                                               
134301                                                                          
134401     MOVE JA                  TO FAKTURA-FINNS-SW                         
134501     MOVE LOW-VALUE           TO W-WDL6A1KY-MIN                           
134601     MOVE HIGH-VALUE          TO W-WDL6A1KY-MAX                           
134701     MOVE W-SPAR-IDFAKT       TO W-SEQA-IDFAKT-MIN                        
134801                                 W-SEQA-IDFAKT-MAX                        
134901     MOVE W-SPAR-IDKUNDRF     TO W-SEQA-IDKUNDRF-MIN                      
135001                                 W-SEQA-IDKUNDRF-MAX                      
135101     MOVE W-SPAR-IDKUNDNR     TO W-SEQA-IDKUNDNR-MIN                      
135201                                 W-SEQA-IDKUNDNR-MAX                      
135301     MOVE W-SPAR-IDKOLLI      TO W-SEQA-IDKOLLI-MIN                       
135401                                 W-SEQA-IDKOLLI-MAX                       
135501     PERFORM IMS-GU-WLINLD01-FIRST                                        
135601     .                                                                    
135701     EJECT                                                                
135801 FB-LAES-RADDATA SECTION.                                                 
135901                                                                          
136001                                                                          
136101     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
136201     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
136301     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
136401                             W-SEQA-IDFAKT-MAX                            
136501     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
136601                             W-SEQA-IDKUNDRF-MAX                          
136701     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
136801                             W-SEQA-IDKUNDNR-MAX                          
136901     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
137001                             W-SEQA-IDKOLLI-MAX                           
137101                                                                          
137201     IF  W-NEXT-IDARTNR   > ZERO                                          
137301         MOVE W-NEXT-IDARTNR                                              
137401                          TO W-SEQA-IDARTNR-MIN                           
137501         IF W-NEXT-DAINLEV > ZERO                                         
137601            MOVE W-NEXT-DAINLEV TO W-SEQA-DAINLEV-MIN                     
137701         END-IF                                                           
137801     END-IF                                                               
137901                                                                          
138001     MOVE SEQA-IDPTYP TO W-IDPTYP                                         
138101     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
138201                                                                          
138301     IF  SEGMENT-FINNS                                                    
138401         MOVE SEQA-IDARTNR TO MOD-IDARTNR-ENTER                           
138501         MOVE SEQA-DAINLEV TO MOD-DAINLEV-ENTER                           
138601     END-IF                                                               
138701                                                                          
138801     MOVE +1 TO INDX                                                      
138901                                                                          
139001     PERFORM UNTIL INDX  > MAX-INDX                                       
139101                                                                          
139201         IF  SEGMENT-FINNS                                                
139301             MOVE SEQA-IDARTNR  TO W-IDARTNR                              
139401                                   MOD-IDARTNR  (INDX)                    
139501             MOVE SEQA-DAINLEV  TO W-DAINLEV                              
139601             MOVE SEQA-IDDC     TO W-IDDC                                 
139701             PERFORM IMS-GU-WLINLC11                                      
139801                                                                          
139901             MOVE INL-KVAVIS    TO MOD-KVAVIS   (INDX)                    
140001                                                                          
140101             IF CDC-SE                                                    
140201               PERFORM IMS-GET-WLARTC11                                   
140301               IF  CLAG-ADLAGOMR  = ZERO                                  
140401                 IF  (INDATA-OK                                           
140501                 AND  INDATA-FINNS                                        
140601                 AND  MFS-UPDATE)                                         
140701                 OR  INDATA-SAKNAS                                        
140801                 OR (INDATA-OK                                            
140901                 AND INDATA-FINNS                                         
141001                 AND MFS-UPD-V)                                           
141101                     MOVE MFS-OEPPNA-NUM-FAELT                            
141201                          TO MOD-ADLAGOMR-ATTR (INDX)                     
141301                             MOD-ADGANG-ATTR   (INDX)                     
141401                             MOD-ADPLATS-ATTR  (INDX)                     
141501                                                                          
141601                     MOVE MFS-RENSA-FAELT                                 
141701                          TO MOD-ADLAGOMR      (INDX)                     
141801                             MOD-ADGANG        (INDX)                     
141901                             MOD-ADPLATS       (INDX)                     
142001                 ELSE                                                     
142101                     MOVE MFS-ROER-EJ-FAELT                               
142201                          TO MOD-ADLAGOMR     (INDX)                      
142301                             MOD-ADGANG       (INDX)                      
142401                             MOD-ADPLATS      (INDX)                      
142501                                                                          
142601                     IF  MOD-ADLAGOMR-ATTR (INDX)                         
142701                              NOT = MFS-NUM-FAELT-FEL                     
142801                         MOVE MFS-ADD-LAES-IN-FAELT                       
142901                              TO MOD-ADLAGOMR-ATTR (INDX)                 
143001                     END-IF                                               
143101                                                                          
143201                     IF  MOD-ADGANG-ATTR (INDX)                           
143301                              NOT = MFS-NUM-FAELT-FEL                     
143401                         MOVE MFS-ADD-LAES-IN-FAELT                       
143501                              TO MOD-ADGANG-ATTR   (INDX)                 
143601                     END-IF                                               
143701                                                                          
143801                     IF  MOD-ADPLATS-ATTR (INDX)                          
143901                              NOT = MFS-NUM-FAELT-FEL                     
144001                         MOVE MFS-ADD-LAES-IN-FAELT                       
144101                              TO MOD-ADPLATS-ATTR  (INDX)                 
144201                     END-IF                                               
144301                 END-IF                                                   
144401               ELSE                                                       
144501                 MOVE MFS-STAENG-FAELT-NOMOD                              
144601                          TO MOD-ADLAGOMR-ATTR (INDX)                     
144701                             MOD-ADGANG-ATTR   (INDX)                     
144801                             MOD-ADPLATS-ATTR  (INDX)                     
144901                 MOVE CLAG-ADLAGOMR TO MOD-ADLAGOMR  (INDX)               
145001                 MOVE CLAG-ADGANG   TO MOD-ADGANG    (INDX)               
145101                 MOVE CLAG-ADPLATS  TO MOD-ADPLATS   (INDX)               
145201               END-IF                                                     
145301             ELSE                                                         
145401               PERFORM IMS-GET-WDK711                                     
145501                                                                          
145601               IF  SLAG-ADLAGOMR  = ZERO                                  
145701                 IF  (INDATA-OK                                           
145801                 AND  INDATA-FINNS                                        
145901                 AND  MFS-UPDATE)                                         
146001                 OR  INDATA-SAKNAS                                        
146101                 OR (INDATA-OK                                            
146201                 AND INDATA-FINNS                                         
146301                 AND MFS-UPD-V)                                           
146401                     MOVE MFS-OEPPNA-NUM-FAELT                            
146501                          TO MOD-ADLAGOMR-ATTR (INDX)                     
146601                             MOD-ADGANG-ATTR   (INDX)                     
146701                             MOD-ADPLATS-ATTR  (INDX)                     
146801                                                                          
146901                     MOVE MFS-RENSA-FAELT                                 
147001                          TO MOD-ADLAGOMR      (INDX)                     
147101                             MOD-ADGANG        (INDX)                     
147201                             MOD-ADPLATS       (INDX)                     
147301                 ELSE                                                     
147401                     MOVE MFS-ROER-EJ-FAELT                               
147501                          TO MOD-ADLAGOMR     (INDX)                      
147601                             MOD-ADGANG       (INDX)                      
147701                             MOD-ADPLATS      (INDX)                      
147801                                                                          
147901                     IF  MOD-ADLAGOMR-ATTR (INDX)                         
148001                              NOT = MFS-NUM-FAELT-FEL                     
148101                         MOVE MFS-ADD-LAES-IN-FAELT                       
148201                              TO MOD-ADLAGOMR-ATTR (INDX)                 
148301                     END-IF                                               
148401                                                                          
148501                     IF  MOD-ADGANG-ATTR (INDX)                           
148601                              NOT = MFS-NUM-FAELT-FEL                     
148701                         MOVE MFS-ADD-LAES-IN-FAELT                       
148801                              TO MOD-ADGANG-ATTR   (INDX)                 
148901                     END-IF                                               
149001                                                                          
149101                     IF  MOD-ADPLATS-ATTR (INDX)                          
149201                              NOT = MFS-NUM-FAELT-FEL                     
149301                         MOVE MFS-ADD-LAES-IN-FAELT                       
149401                              TO MOD-ADPLATS-ATTR  (INDX)                 
149501                     END-IF                                               
149601                 END-IF                                                   
149701                                                                          
149801               ELSE                                                       
149901                 MOVE MFS-STAENG-FAELT-NOMOD                              
150001                          TO MOD-ADLAGOMR-ATTR (INDX)                     
150101                             MOD-ADGANG-ATTR   (INDX)                     
150201                             MOD-ADPLATS-ATTR  (INDX)                     
150301                 MOVE SLAG-ADLAGOMR TO MOD-ADLAGOMR  (INDX)               
150401                 MOVE SLAG-ADGANG   TO MOD-ADGANG    (INDX)               
150501                 MOVE SLAG-ADPLATS  TO MOD-ADPLATS   (INDX)               
150601               END-IF                                                     
150701             END-IF                                                       
150801                                                                          
150901             IF INL-FLPRIO = 'Y' OR 'J'                                   
151001                MOVE 'Y'           TO MOD-KDPRIO (INDX)                   
151101             ELSE                                                         
151201                MOVE SPACE         TO MOD-KDPRIO (INDX)                   
151301             END-IF                                                       
151401                                                                          
151501             MOVE W-SPAR-IDSPRAAK   TO W-IDSKYLT                          
151601             PERFORM IMS-GU-WLBENA11                                      
151701                                                                          
151801             IF  SEGMENT-FINNS                                            
151901                 MOVE TEXT-BEART    TO MOD-BEART  (INDX)                  
152001             ELSE                                                         
152101                 MOVE SPACE         TO MOD-BEART  (INDX)                  
152201             END-IF                                                       
152301                                                                          
152401             PERFORM IMS-GN-WLINLD01                                      
152501                                                                          
152601         ELSE                                                             
152701             PERFORM MFS-RENSA-RAD-FAELT-UT                               
152801             PERFORM MFS-STAENG-RAD-FAELT-UT                              
152901         END-IF                                                           
153001                                                                          
153101         ADD 1 TO INDX                                                    
153201     END-PERFORM                                                          
153301                                                                          
153401     IF  SEGMENT-FINNS                                                    
153501          MOVE SEQA-IDARTNR TO MOD-IDARTNR-NEXT                           
153601          MOVE SEQA-DAINLEV TO MOD-DAINLEV-NEXT                           
153701          MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                       
153801          CALL WMEDKONV USING MED-WMEDAREA                                
153901                                                                          
154001          IF  W-TEMFSINF > SPACE                                          
154101              MOVE W-TEMFSINF  TO MOD-TEMFSFEL                            
154201          END-IF                                                          
154301          MOVE MED-TEMFSINF    TO MOD-TEMFSINF                            
154401     ELSE                                                                 
154501          IF  W-TEMFSINF > SPACE                                          
154601              MOVE W-TEMFSINF  TO MOD-TEMFSINF                            
154701          END-IF                                                          
154801          MOVE ZERO            TO MOD-IDARTNR-NEXT                        
154901                                  MOD-DAINLEV-NEXT                        
155001     END-IF                                                               
155101                                                                          
155201     IF MFS-UPDATE OR MFS-UPD-V OR MFS-FIRST OR MFS-NEXT                  
155301        PERFORM MFS-RENSA-NY-ARTIKEL-FAELT                                
155401     ELSE                                                                 
155501        IF MID-KVANTMOT-INM = ALL '+'                                     
155601           MOVE MFS-RENSA-FAELT TO MOD-KVANTMOT-INM                       
155701        ELSE                                                              
155801           MOVE MFS-ROER-EJ-FAELT TO MOD-KVANTMOT-INM                     
155901        END-IF                                                            
156001        IF MID-IDARTNR-INM = ALL '+'                                      
156101           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-INM                        
156201        ELSE                                                              
156301           MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-INM                      
156401        END-IF                                                            
156501        IF MID-KVSKROT-INM = ALL '+'                                      
156601           MOVE MFS-RENSA-FAELT TO MOD-KVSKROT-INM                        
156701        ELSE                                                              
156801           MOVE MFS-ROER-EJ-FAELT TO MOD-KVSKROT-INM                      
156901        END-IF                                                            
157001        IF MID-ADLAGOMR-INM = ALL '+'                                     
157101           MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-INM                       
157201        ELSE                                                              
157301           MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR-INM                     
157401        END-IF                                                            
157501        IF MID-ADGANG-INM = ALL '+'                                       
157601           MOVE MFS-RENSA-FAELT TO MOD-ADGANG-INM                         
157701        ELSE                                                              
157801           MOVE MFS-ROER-EJ-FAELT TO MOD-ADGANG-INM                       
157901        END-IF                                                            
158001        IF MID-ADPLATS-INM = ALL '+'                                      
158101           MOVE MFS-RENSA-FAELT TO MOD-ADPLATS-INM                        
158201        ELSE                                                              
158301           MOVE MFS-ROER-EJ-FAELT TO MOD-ADPLATS-INM                      
158401        END-IF                                                            
158501        IF MID-CMD-INM = ALL '+'                                          
158601           MOVE MFS-RENSA-FAELT TO MOD-CMD-INM                            
158701        ELSE                                                              
158801           MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-INM                          
158901        END-IF                                                            
159001     END-IF                                                               
159101     .                                                                    
159201     EJECT                                                                
159301 G-KOLLA-INPUT SECTION.                                                   
159401                                                                          
159501     IF MFS-UPD-X                                                         
159601        MOVE MID-MID-IDDC-SPAR TO W-SPAR-IDDC                             
159701     END-IF                                                               
159801                                                                          
159901     MOVE W-SPAR-IDDC TO W-IDDC-B6                                        
160001     PERFORM IMS-GU-WDB601                                                
160101     MOVE JA          TO INDATA-SW                                        
160201     MOVE NEJ         TO NY-NYUPPLAEGG-ART-SW                             
160301                         NY-BEFINTLIG-ART-SW                              
160401                         NY-KVANTMOT-SW                                   
160501                                                                          
160601     IF (INDATA-SAKNAS AND MFS-UPDATE)                                    
160701     OR (INDATA-SAKNAS AND MFS-UPD-V)                                     
160801        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
160901        MOVE NEJ TO INDATA-SW                                             
161001                                                                          
161101        IF MID-IDUSER-003 = ALL '+' OR SPACE                              
161201           CONTINUE                                                       
161301        ELSE                                                              
161401           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-003-ATTR                 
161501           MOVE NEJ TO INDATA-SW                                          
161601        END-IF                                                            
161701                                                                          
161801     ELSE                                                                 
161901                                                                          
162001       IF MFS-UPD-X                                                       
162101         PERFORM GI-KOLLA-BIN                                             
162201       ELSE                                                               
162301                                                                          
162401         IF W-SPAR-IDDC = MSGI-IDDC                                       
162501                                                                          
162601           IF MID-IDUSER-003 = ALL '+' OR SPACE                           
162701              CONTINUE                                                    
162801           ELSE                                                           
162901              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDUSER-003-ATTR            
163001           END-IF                                                         
163101                                                                          
163201           PERFORM GA-KOLLA-KOLLI-KLART                                   
163301                                                                          
163401           MOVE +1 TO INDX                                                
163501           PERFORM UNTIL INDX > MAX-INDX                                  
163601              PERFORM GB-KOLLA-CMD                                        
163701                                                                          
163801              PERFORM GC-KOLLA-KVANTMOT                                   
163901                                                                          
164001              PERFORM GD-KOLLA-LAGERPLATS                                 
164101                                                                          
164201              PERFORM GE-KOLLA-KVSKROT                                    
164301                                                                          
164401              IF INDATA-OK                                                
164501                IF W-KVANTMOT(INDX) = ZERO                                
164601                AND W-KVSKROT(INDX) = ZERO                                
164701                   CONTINUE                                               
164801                ELSE                                                      
164901                   PERFORM GH-KOLLA-VAERDEGRANS-IDDC                      
165001                END-IF                                                    
165101              END-IF                                                      
165201                                                                          
165301              ADD 1 TO INDX                                               
165401                                                                          
165501           END-PERFORM                                                    
165601                                                                          
165701           PERFORM GF-KOLLA-NY-ARTIKEL                                    
165801                                                                          
165901           IF INDATA-FEL                                                  
166001              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
166101           END-IF                                                         
166201                                                                          
166301         ELSE                                                             
166401           IF MFS-UPDATE OR MFS-UPD-V                                     
166501              MOVE NEJ TO INDATA-SW                                       
166601           END-IF                                                         
166701         END-IF                                                           
166801         MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                        
166901         MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                        
167001         MOVE W-SPAR-IDFAKT      TO W-SEQA-IDFAKT-MIN                     
167101                                    W-SEQA-IDFAKT-MAX                     
167201         MOVE W-SPAR-IDKUNDRF    TO W-SEQA-IDKUNDRF-MIN                   
167301                                    W-SEQA-IDKUNDRF-MAX                   
167401         MOVE W-SPAR-IDKUNDNR    TO W-SEQA-IDKUNDNR-MIN                   
167501                                    W-SEQA-IDKUNDNR-MAX                   
167601         MOVE W-SPAR-IDKOLLI     TO W-SEQA-IDKOLLI-MIN                    
167701                                    W-SEQA-IDKOLLI-MAX                    
167801         PERFORM IMS-GU-WLINLD01-FIRST                                    
167901         IF SEGMENT-SAKNAS                                                
168001           MOVE UPDATING-NOT-ALLOWED                                      
168101                                 TO MED-IDMFSFEL                          
168201           MOVE NEJ              TO INDATA-SW                             
168301         END-IF                                                           
168401       END-IF                                                             
168501     END-IF                                                               
168601                                                                          
168701     IF  INDATA-FEL                                                       
168801        IF W-SPAR-IDDC = MSGI-IDDC                                        
168901           CALL WMEDKONV USING MED-WMEDAREA                               
169001           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
169101           PERFORM MFS-ROER-EJ-FAELT-UT                                   
169201           PERFORM MFS-ROER-EJ-FAELT-IN                                   
169301        ELSE                                                              
169401           IF MFS-UPDATE OR MFS-UPD-V                                     
169501              MOVE UPDATING-NOT-ALLOWED-WRONG-DC                          
169601                                     TO MOD-TEMFSFEL                      
169701              PERFORM MFS-ROER-EJ-FAELT-UT                                
169801              PERFORM MFS-ROER-EJ-FAELT-IN                                
169901           END-IF                                                         
170001        END-IF                                                            
170101     END-IF                                                               
170201     .                                                                    
170301     EJECT                                                                
170401 GA-KOLLA-KOLLI-KLART SECTION.                                            
170501                                                                          
170601     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KOLLI-KLART-ATTR                    
170701                                                                          
170801     IF  MID-KOLLI-KLART   NOT = 'X'                                      
170901     AND MID-KOLLI-KLART   NOT = 'Y'                                      
171001         IF  MID-KOLLI-KLART   NOT = ALL '+'                              
171101         AND MID-KOLLI-KLART   NOT = SPACE                                
171201             MOVE NEJ      TO INDATA-SW                                   
171301             MOVE MFS-ALFA-FAELT-FEL                                      
171401                           TO MOD-KOLLI-KLART-ATTR                        
171501         END-IF                                                           
171601     ELSE                                                                 
171701         MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                           
171801         MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                           
171901         MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                        
172001                                 W-SEQA-IDFAKT-MAX                        
172101         MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                      
172201                                 W-SEQA-IDKUNDRF-MAX                      
172301         MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                      
172401                                 W-SEQA-IDKUNDNR-MAX                      
172501         MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                       
172601                                 W-SEQA-IDKOLLI-MAX                       
172701         MOVE '310'           TO W-IDPTYP                                 
172801         PERFORM IMS-GU-WLINLD01-FIRST-310                                
172901                                                                          
173001         PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                       
173101                                                                          
173201             MOVE SEQA-IDARTNR  TO W-IDARTNR                              
173301             MOVE SEQA-IDDC     TO W-IDDC                                 
173401             IF DCS-CDC                                                   
173501               PERFORM IMS-GET-WLARTC11                                   
173601               IF CLAG-ADLAGOMR = ZERO                                    
173701                 MOVE NEJ       TO INDATA-SW                              
173801                 MOVE MFS-ALFA-FAELT-FEL                                  
173901                                TO MOD-KOLLI-KLART-ATTR                   
174001               END-IF                                                     
174101             ELSE                                                         
174201               PERFORM IMS-GET-WDK711                                     
174301                                                                          
174401               IF SLAG-ADLAGOMR  = ZERO                                   
174501                 MOVE NEJ       TO INDATA-SW                              
174601                 MOVE MFS-ALFA-FAELT-FEL                                  
174701                                TO MOD-KOLLI-KLART-ATTR                   
174801               END-IF                                                     
174901             END-IF                                                       
175001                                                                          
175101             PERFORM IMS-GN-WLINLD01                                      
175201         END-PERFORM                                                      
175301         PERFORM S35-KOLLA-IDUSER                                         
175401     END-IF                                                               
175501     .                                                                    
175601     EJECT                                                                
175701 GB-KOLLA-CMD SECTION.                                                    
175801                                                                          
175901     MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR (INDX)                     
176001                                                                          
176101     IF  MID-KOLLI-KLART   = 'X'                                          
176201     OR  MID-KOLLI-KLART   = 'Y'                                          
176301         IF  MID-CMD-IN (INDX) NOT = ALL '+'                              
176401         AND MID-CMD-IN (INDX) NOT = SPACE                                
176501             MOVE NEJ TO INDATA-SW                                        
176601             MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)               
176701         END-IF                                                           
176801     ELSE                                                                 
176901         IF  MID-CMD-IN (INDX) NOT = ALL '+'                              
177001         AND MID-CMD-IN (INDX) NOT = 'DAM'                                
177101         AND MID-CMD-IN (INDX) NOT = 'PB '                                
177201         AND MID-CMD-IN (INDX) NOT = SPACE                                
177301             MOVE NEJ TO INDATA-SW                                        
177401             MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)               
177501         END-IF                                                           
177601                                                                          
177701         IF  MID-CMD-IN (INDX) = 'DAM' OR 'PB '                           
177801             IF  MID-IDARTNR (INDX) = ZERO                                
177901                 MOVE NEJ TO INDATA-SW                                    
178001                 MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)           
178101             END-IF                                                       
178201         PERFORM S35-KOLLA-IDUSER                                         
178301         END-IF                                                           
178401     END-IF                                                               
178501     .                                                                    
178601     EJECT                                                                
178701 GC-KOLLA-KVANTMOT SECTION.                                               
178801                                                                          
178901     MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTMOT-ATTR (INDX)                 
179001     MOVE ZERO                TO W-KVANTMOT (INDX)                        
179101                                                                          
179201     IF  MID-KOLLI-KLART   = 'X'                                          
179301     OR  MID-KOLLI-KLART   = 'Y'                                          
179401         IF  MID-KVANTMOT-IN (INDX) NOT = ALL '+'                         
179501         AND MID-KVANTMOT-IN (INDX) NOT = SPACE                           
179601             MOVE NEJ TO INDATA-SW                                        
179701             MOVE MFS-NUM-FAELT-FEL                                       
179801                         TO MOD-KVANTMOT-ATTR (INDX)                      
179901         END-IF                                                           
180001                                                                          
180101     ELSE                                                                 
180201         IF  MID-KVANTMOT-IN (INDX) NOT = ALL '+'                         
180301         AND MID-KVANTMOT-IN (INDX) NOT = SPACE                           
180401             MOVE MID-KVANTMOT-IN (INDX) TO WS-KVANTMOT                   
180501             INSPECT WS-KVANTMOT REPLACING LEADING SPACE BY ZERO          
180601             IF  WS-KVANTMOT NOT NUMERIC                                  
180701                 MOVE NEJ    TO INDATA-SW                                 
180801                 MOVE MFS-NUM-FAELT-FEL                                   
180901                         TO MOD-KVANTMOT-ATTR (INDX)                      
181001             ELSE                                                         
181101                 MOVE WS-KVANTMOT TO W-KVANTMOT (INDX)                    
181201             END-IF                                                       
181301             PERFORM S35-KOLLA-IDUSER                                     
181401         END-IF                                                           
181501     END-IF                                                               
181601                                                                          
181701     IF MID-CMD-IN (INDX) = 'DAM'                                         
181801         IF  MID-KVANTMOT-IN (INDX) = ALL '+'                             
181901         OR  MID-KVANTMOT-IN (INDX) = SPACE                               
182001             MOVE NEJ TO INDATA-SW                                        
182101             MOVE MFS-NUM-FAELT-FEL                                       
182201                         TO MOD-KVANTMOT-ATTR (INDX)                      
182301         END-IF                                                           
182401     END-IF                                                               
182501     .                                                                    
182601     EJECT                                                                
182701 GD-KOLLA-LAGERPLATS SECTION.                                             
182801                                                                          
182901     MOVE ZERO                TO W-ADLAGOMR (INDX)                        
183001                                 W-ADGANG   (INDX)                        
183101                                 W-ADPLATS  (INDX)                        
183201                                                                          
183301     IF MID-ADLAGOMR(INDX) NOT = ALL '+'                                  
183401        IF MID-IDARTNR(INDX) = ZERO                                       
183501             MOVE NEJ TO INDATA-SW                                        
183601             MOVE MFS-NUM-FAELT-FEL                                       
183701                         TO MOD-ADLAGOMR-ATTR (INDX)                      
183801        END-IF                                                            
183901     END-IF                                                               
184001     IF MID-ADGANG(INDX) NOT = ALL '+'                                    
184101        IF MID-IDARTNR(INDX) = ZERO                                       
184201             MOVE NEJ TO INDATA-SW                                        
184301             MOVE MFS-NUM-FAELT-FEL                                       
184401                         TO MOD-ADGANG-ATTR (INDX)                        
184501        END-IF                                                            
184601     END-IF                                                               
184701     IF MID-ADPLATS(INDX) NOT = ALL '+'                                   
184801        IF MID-IDARTNR(INDX) = ZERO                                       
184901             MOVE NEJ TO INDATA-SW                                        
185001             MOVE MFS-NUM-FAELT-FEL                                       
185101                         TO MOD-ADPLATS-ATTR (INDX)                       
185201        END-IF                                                            
185301     END-IF                                                               
185401                                                                          
185501     IF  MID-KOLLI-KLART   = 'X'                                          
185601     OR  MID-KOLLI-KLART   = 'Y'                                          
185701         IF  MID-ADLAGOMR  (INDX) NOT = ALL '+'                           
185801             MOVE NEJ TO INDATA-SW                                        
185901             MOVE MFS-NUM-FAELT-FEL                                       
186001                         TO MOD-ADLAGOMR-ATTR (INDX)                      
186101         END-IF                                                           
186201                                                                          
186301     ELSE                                                                 
186401         IF  MID-ADLAGOMR (INDX) NOT = ALL '+'                            
186501             MOVE MID-ADLAGOMR (INDX) TO WS-ADLAGOMR                      
186601             INSPECT WS-ADLAGOMR REPLACING LEADING SPACE BY ZERO          
186701                                                                          
186801             IF  WS-ADLAGOMR NOT NUMERIC                                  
186901             OR  WS-ADLAGOMR NOT > ZERO                                   
187001                 MOVE NEJ    TO INDATA-SW                                 
187101                 MOVE MFS-NUM-FAELT-FEL                                   
187201                         TO MOD-ADLAGOMR-ATTR (INDX)                      
187301             ELSE                                                         
187401                 MOVE WS-ADLAGOMR TO W-ADLAGOMR (INDX)                    
187501                 MOVE MFS-NUM-FAELT-RAETT                                 
187601                                  TO MOD-ADLAGOMR-ATTR (INDX)             
187701             END-IF                                                       
187801         END-IF                                                           
187901     END-IF                                                               
188001                                                                          
188101     IF  MID-KOLLI-KLART   = 'X'                                          
188201     OR  MID-KOLLI-KLART   = 'Y'                                          
188301         IF  MID-ADGANG (INDX) NOT = ALL '+'                              
188401             MOVE NEJ TO INDATA-SW                                        
188501             MOVE MFS-NUM-FAELT-FEL                                       
188601                         TO MOD-ADGANG-ATTR (INDX)                        
188701         END-IF                                                           
188801     ELSE                                                                 
188901         IF  MID-ADGANG (INDX) NOT = ALL '+'                              
189001             MOVE MID-ADGANG (INDX) TO WS-ADGANG                          
189101             INSPECT WS-ADGANG REPLACING LEADING SPACE BY ZERO            
189201             IF  WS-ADGANG NOT NUMERIC                                    
189301                 MOVE NEJ TO INDATA-SW                                    
189401                 MOVE MFS-NUM-FAELT-FEL                                   
189501                             TO MOD-ADGANG-ATTR (INDX)                    
189601             ELSE                                                         
189701                 MOVE WS-ADGANG TO W-ADGANG (INDX)                        
189801                 MOVE MFS-NUM-FAELT-RAETT                                 
189901                             TO MOD-ADGANG-ATTR (INDX)                    
190001             END-IF                                                       
190101         END-IF                                                           
190201     END-IF                                                               
190301                                                                          
190401     IF  MID-KOLLI-KLART   = 'X'                                          
190501     OR  MID-KOLLI-KLART   = 'Y'                                          
190601         IF  MID-ADPLATS (INDX) NOT = ALL '+'                             
190701             MOVE NEJ TO INDATA-SW                                        
190801             MOVE MFS-NUM-FAELT-FEL                                       
190901                         TO MOD-ADPLATS-ATTR (INDX)                       
191001         END-IF                                                           
191101     ELSE                                                                 
191201         IF  MID-ADPLATS (INDX) NOT = ALL '+'                             
191301             MOVE MID-ADPLATS (INDX) TO WS-ADPLATS                        
191401             INSPECT WS-ADPLATS REPLACING LEADING SPACE BY ZERO           
191501             IF  WS-ADPLATS NOT NUMERIC                                   
191601                 MOVE NEJ TO INDATA-SW                                    
191701                 MOVE MFS-NUM-FAELT-FEL                                   
191801                         TO MOD-ADPLATS-ATTR (INDX)                       
191901             ELSE                                                         
192001                 MOVE WS-ADPLATS TO W-ADPLATS (INDX)                      
192101                 MOVE MFS-NUM-FAELT-RAETT                                 
192201                                 TO MOD-ADPLATS-ATTR (INDX)               
192301             END-IF                                                       
192401         END-IF                                                           
192501     END-IF                                                               
192601                                                                          
192701     IF  MID-KOLLI-KLART   = 'X'                                          
192801     OR  MID-KOLLI-KLART   = 'Y'                                          
192901        CONTINUE                                                          
193001     ELSE                                                                 
193101        IF MID-KVANTMOT-IN(INDX) NOT  = ALL '+'                           
193201        AND MID-KVANTMOT-IN (INDX) NOT = SPACE                            
193301           IF INDATA-OK                                                   
193401           AND W-KVANTMOT(INDX) = ZERO                                    
193501              CONTINUE                                                    
193601           ELSE                                                           
193701              IF MID-IDARTNR(INDX) NOT = ZERO                             
193801                MOVE MID-IDARTNR (INDX) TO W-IDARTNR                      
193901                MOVE W-SPAR-IDDC        TO W-IDDC                         
194001                                                                          
194101                IF DCS-CDC                                                
194201                  PERFORM IMS-GET-WLARTC11                                
194301                  IF CLAG-ADLAGOMR = ZERO                                 
194401                    IF MID-ADLAGOMR(INDX) = ALL '+' OR SPACE              
194501                      MOVE NEJ   TO INDATA-SW                             
194601                      MOVE MFS-NUM-FAELT-FEL                              
194701                                 TO MOD-ADLAGOMR-ATTR(INDX)               
194801                    END-IF                                                
194901                  END-IF                                                  
195001                ELSE                                                      
195101                  PERFORM IMS-GET-WDK711-GE                               
195201                  IF SEGMENT-FINNS                                        
195301                    IF SLAG-ADLAGOMR = ZERO                               
195401                      IF MID-ADLAGOMR(INDX) = ALL '+' OR SPACE            
195501                        MOVE NEJ TO INDATA-SW                             
195601                        MOVE MFS-NUM-FAELT-FEL                            
195701                                 TO MOD-ADLAGOMR-ATTR(INDX)               
195801                      END-IF                                              
195901                    END-IF                                                
196001                  END-IF                                                  
196101                END-IF                                                    
196201              END-IF                                                      
196301           END-IF                                                         
196401        END-IF                                                            
196501     END-IF                                                               
196601     .                                                                    
196701     EJECT                                                                
196801 GE-KOLLA-KVSKROT SECTION.                                                
196901                                                                          
197001     MOVE MFS-NUM-FAELT-RAETT TO MOD-KVSKROT-ATTR(INDX)                   
197101     MOVE ZERO                TO W-KVSKROT (INDX)                         
197201                                                                          
197301     IF  MID-KOLLI-KLART   = 'X'                                          
197401     OR  MID-KOLLI-KLART   = 'Y'                                          
197501         IF  MID-KVSKROT-IN (INDX) NOT = ALL '+'                          
197601         AND MID-KVSKROT-IN (INDX) NOT = SPACE                            
197701             MOVE NEJ  TO INDATA-SW                                       
197801             MOVE MFS-NUM-FAELT-FEL                                       
197901                         TO MOD-KVSKROT-ATTR (INDX)                       
198001         END-IF                                                           
198101     ELSE                                                                 
198201         IF  MID-KVSKROT-IN (INDX) NOT = ALL '+'                          
198301         AND MID-KVSKROT-IN (INDX) NOT = SPACE                            
198401             IF  MID-CMD-IN (INDX) NOT = 'DAM'                            
198501                 MOVE NEJ TO INDATA-SW                                    
198601                 MOVE MFS-NUM-FAELT-FEL                                   
198701                         TO MOD-KVSKROT-ATTR (INDX)                       
198801             ELSE                                                         
198901                 PERFORM S35-KOLLA-IDUSER                                 
199001                 MOVE MID-KVSKROT-IN (INDX) TO WS-KVSKROT                 
199101                 INSPECT WS-KVSKROT                                       
199201                          REPLACING LEADING SPACE BY ZERO                 
199301                 IF  WS-KVSKROT NOT NUMERIC                               
199401                     MOVE NEJ    TO INDATA-SW                             
199501                     MOVE MFS-NUM-FAELT-FEL                               
199601                         TO MOD-KVSKROT-ATTR (INDX)                       
199701                 ELSE                                                     
199801                     MOVE WS-KVSKROT TO W-KVSKROT (INDX)                  
199901                     IF  W-KVSKROT (INDX) = ZERO                          
200001                         MOVE NEJ    TO INDATA-SW                         
200101                         MOVE MFS-NUM-FAELT-FEL                           
200201                             TO MOD-KVSKROT-ATTR (INDX)                   
200301                     END-IF                                               
200401                 END-IF                                                   
200501             END-IF                                                       
200601         ELSE                                                             
200701             IF  MID-CMD-IN (INDX) = 'DAM'                                
200801                 MOVE NEJ TO INDATA-SW                                    
200901                 MOVE MFS-NUM-FAELT-FEL                                   
201001                         TO MOD-KVSKROT-ATTR (INDX)                       
201101             END-IF                                                       
201201         END-IF                                                           
201301     END-IF                                                               
201401                                                                          
201501     IF MID-CMD-IN (INDX) = 'DAM'                                         
201601         IF  MID-KVSKROT-IN (INDX) = ALL '+'                              
201701         OR  MID-KVSKROT-IN (INDX) = SPACE                                
201801             MOVE NEJ TO INDATA-SW                                        
201901             MOVE MFS-NUM-FAELT-FEL                                       
202001                         TO MOD-KVSKROT-ATTR (INDX)                       
202101         END-IF                                                           
202201     END-IF                                                               
202301     .                                                                    
202401     EJECT                                                                
202501 GF-KOLLA-NY-ARTIKEL    SECTION.                                          
202601                                                                          
202701     MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVANTMOT-INM-ATTR                   
202801                                  MOD-IDARTNR-INM-ATTR                    
202901                                  MOD-ADLAGOMR-INM-ATTR                   
203001                                  MOD-ADGANG-INM-ATTR                     
203101                                  MOD-ADPLATS-INM-ATTR                    
203201                                  MOD-KVSKROT-INM-ATTR                    
203301     MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-INM-ATTR                        
203401     MOVE ZERO                 TO W-KVANTMOT-INM                          
203501                                  W-IDARTNR-INM                           
203601     MOVE W-SPAR-IDDC   TO W-6301-IDDC                                    
203701     MOVE W-SPAR-IDFAKT TO W-IDFAKT                                       
203801     PERFORM IMS-GHU-WL630111-GE                                          
203901     IF SEGMENT-FINNS                                                     
204001        MOVE 6302-IDDC-SEND TO SEND-WS-IDDC                               
204101        MOVE 6302-IDDISTR   TO DIST35-IDDISTR                             
204201     ELSE                                                                 
204301        MOVE SPACE          TO SEND-WS-IDDC                               
204401     END-IF                                                               
204501     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
204601        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
204701        PERFORM IMS-GU-WDB601-SEND                                        
204801     END-IF                                                               
204901                                                                          
205001     IF  MID-KOLLI-KLART = 'X'                                            
205101     OR  MID-KOLLI-KLART = 'Y'                                            
205201         IF  MID-IDARTNR-INM NOT = ALL '+'                                
205301         AND MID-IDARTNR-INM NOT = SPACE                                  
205401             MOVE NEJ TO INDATA-SW                                        
205501             MOVE MFS-NUM-FAELT-FEL                                       
205601                         TO MOD-IDARTNR-INM-ATTR                          
205701         END-IF                                                           
205801     ELSE                                                                 
205901         IF  MID-IDARTNR-INM NOT = ALL '+'                                
206001         AND MID-IDARTNR-INM NOT = SPACE                                  
206101             MOVE MID-IDARTNR-INM TO WS-IDARTNR                           
206201             INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO           
206301             IF WS-IDARTNR NOT NUMERIC                                    
206401                MOVE NEJ TO INDATA-SW                                     
206501                MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-INM-ATTR            
206601             ELSE                                                         
206701                MOVE WS-IDARTNR TO W-IDARTNR-INM                          
206801                                   W-IDARTNR                              
206901                IF W-IDARTNR-INM = 100                                    
207001                  MOVE NEJ TO INDATA-SW                                   
207101                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-INM-ATTR          
207201                ELSE                                                      
207301                  MOVE W-SPAR-IDDC TO W-IDDC                              
207401                  IF DCS-CDC                                              
207501                    PERFORM IMS-GET-WLARTC01                              
207601                    IF SEGMENT-SAKNAS                                     
207701                      MOVE NEJ TO INDATA-SW                               
207801                      MOVE MFS-NUM-FAELT-FEL                              
207901                                           TO MOD-IDARTNR-INM-ATTR        
208001                    ELSE                                                  
208101                      MOVE JA TO NY-BEFINTLIG-ART-SW                      
208201                      PERFORM GFC-KOLLA-BEFINTLIG-ART-CDC                 
208301                      PERFORM S35-KOLLA-IDUSER                            
208401                    END-IF                                                
208501                  ELSE                                                    
208601                    PERFORM IMS-GET-WDK711-GE                             
208701                    IF SEGMENT-SAKNAS                                     
208801                      MOVE JA TO NY-NYUPPLAEGG-ART-SW                     
208901                      PERFORM GFA-KOLLA-NYUPPLAGG                         
209001                      PERFORM S35-KOLLA-IDUSER                            
209101                    ELSE                                                  
209201                      MOVE JA TO NY-BEFINTLIG-ART-SW                      
209301                      PERFORM GFB-KOLLA-BEFINTLIG-ARTIKEL                 
209401                      PERFORM S35-KOLLA-IDUSER                            
209501                    END-IF                                                
209601                  END-IF                                                  
209701                END-IF                                                    
209801             END-IF                                                       
209901                                                                          
210001             IF INDATA-OK                                                 
210101               IF MID-ADLAGOMR-INM NOT = ALL '+'                          
210201                  MOVE MID-ADLAGOMR-INM TO WS-ADLAGOMR                    
210301                  INSPECT WS-ADLAGOMR REPLACING LEADING                   
210401                        SPACE BY ZERO                                     
210501                  IF WS-ADLAGOMR NOT NUMERIC                              
210601                  OR WS-ADLAGOMR NOT > ZERO                               
210701                     MOVE NEJ TO INDATA-SW                                
210801                     MOVE MFS-NUM-FAELT-FEL                               
210901                              TO MOD-ADLAGOMR-INM-ATTR                    
211001                  ELSE                                                    
211101                      MOVE WS-ADLAGOMR TO W-ADLAGOMR-INM                  
211201                      MOVE MFS-NUM-FAELT-RAETT                            
211301                              TO MOD-ADLAGOMR-INM-ATTR                    
211401                  END-IF                                                  
211501               END-IF                                                     
211601               IF MID-ADGANG-INM NOT = ALL '+'                            
211701                  MOVE MID-ADGANG-INM TO WS-ADGANG                        
211801                  INSPECT WS-ADGANG REPLACING LEADING                     
211901                          SPACE BY ZERO                                   
212001                  IF WS-ADGANG NOT NUMERIC                                
212101                     MOVE NEJ TO INDATA-SW                                
212201                     MOVE MFS-NUM-FAELT-FEL TO MOD-ADGANG-INM-ATTR        
212301                  ELSE                                                    
212401                     MOVE WS-ADGANG TO W-ADGANG-INM                       
212501                     MOVE MFS-NUM-FAELT-RAETT                             
212601                               TO MOD-ADGANG-INM-ATTR                     
212701                  END-IF                                                  
212801               END-IF                                                     
212901               IF MID-ADPLATS-INM NOT = ALL '+'                           
213001                  MOVE MID-ADPLATS-INM TO WS-ADPLATS                      
213101                  INSPECT WS-ADPLATS REPLACING LEADING                    
213201                               SPACE BY ZERO                              
213301                  IF WS-ADPLATS NOT NUMERIC                               
213401                     MOVE NEJ TO INDATA-SW                                
213501                     MOVE MFS-NUM-FAELT-FEL                               
213601                               TO MOD-ADPLATS-INM-ATTR                    
213701                  ELSE                                                    
213801                     MOVE WS-ADPLATS TO W-ADPLATS-INM                     
213901                     MOVE MFS-NUM-FAELT-RAETT                             
214001                               TO MOD-ADPLATS-INM-ATTR                    
214101                 END-IF                                                   
214201               END-IF                                                     
214301             END-IF                                                       
214401                                                                          
214501             IF INDATA-OK AND (DCS-NDC-NA OR DCS-NDC-PF)                  
214601                MOVE W-SPAR-IDDC   TO W-6301-IDDC                         
214701                MOVE W-SPAR-IDFAKT TO W-IDFAKT                            
214801                PERFORM IMS-GHU-WL630111                                  
214901                MOVE 6302-IDDC-SEND TO SEND-WS-IDDC                       
215001                                                                          
215101                IF  SEND-DCS-CDC                                          
215201                OR  (SEND-DCS-NDC-PF                                      
215301                AND SEND-DCS-AUSTRALIA)                                   
215401                OR  SEND-DCS-DDC                                          
215501                   MOVE 'SEK'          TO W-KDVALISO                      
215601                   IF SEND-DCS-NDC-PF AND SEND-DCS-JAPAN                  
215701                      PERFORM IMS-GHU-WLARTC11-GE                         
215801                      IF SEGMENT-FINNS                                    
215901                         MOVE CLAG-PRARTSTD TO W-PRARTNTO                 
216001                      ELSE                                                
216101                        MOVE UPDATING-NOT-ALLOWED TO MED-IDMFSINF         
216201                        CALL WMEDKONV USING MED-WMEDAREA                  
216301                        MOVE MED-MFSINF TO MOD-TEMFSINF                   
216401                        MOVE NEJ TO INDATA-SW                             
216501                      END-IF                                              
216601                   ELSE                                                   
216701                      PERFORM S16-PRIS-TILLAMPNING                        
216801                      MOVE PRIS-PRARTNTO  TO W-PRARTNTO                   
216901                      IF PRIS-KDVALISO NOT = 'SEK' AND                    
217001                                       NOT = SPACE                        
217101                        MOVE PRIS-KDVALISO   TO W-KDVALISO                
217201                      END-IF                                              
217301                   END-IF                                                 
217401                ELSE                                                      
217501                   IF SEND-DCS-NDC-NA                                     
217601                      IF SEND-DCS-CANADA                                  
217701                         MOVE 'CAD'    TO W-KDVALISO                      
217801                      ELSE                                                
217901                         MOVE 'USD'    TO W-KDVALISO                      
218001                      END-IF                                              
218101                   END-IF                                                 
218201                   MOVE SEND-WS-IDDC TO W-IDDC                            
218301                   PERFORM IMS-GET-WDK711-GE                              
218401                   IF SEGMENT-FINNS                                       
218501                      MOVE SLAG-PRAVCOST  TO W-PRARTNTO                   
218601                   ELSE                                                   
218701                      MOVE UPDATING-NOT-ALLOWED TO MED-IDMFSINF           
218801                      CALL WMEDKONV USING MED-WMEDAREA                    
218901                      MOVE MED-MFSINF TO MOD-TEMFSINF                     
219001                      MOVE NEJ TO INDATA-SW                               
219101                   END-IF                                                 
219201                END-IF                                                    
219301                                                                          
219401                MOVE ZERO TO W-KVAVIS                                     
219501                MOVE WS-SUMMA-KVANT TO W-DIFF-KVANT                       
219601                                                                          
219701                IF MFS-UPDATE AND INDATA-OK                               
219801                   COMPUTE WS-VAERDE-DIFF                                 
219901                          = W-DIFF-KVANT * W-PRARTNTO                     
220001                   END-COMPUTE                                            
220101                   IF WS-VAERDE-DIFF < ZERO                               
220201                      COMPUTE WS-VAERDE-DIFF = WS-VAERDE-DIFF * -1        
220301                      END-COMPUTE                                         
220401                   END-IF                                                 
220501                   IF W-KDVALISO = 'SEK'                                  
220601                      IF DCS-NDC-PF AND DCS-JAPAN                         
220701                         IF WS-VAERDE-DIFF > 3000.00                      
220801                            MOVE NEJ TO INDATA-SW                         
220901                         END-IF                                           
221001                      ELSE                                                
221101                        IF DCS-NDC-NA                                     
221201                           IF WS-VAERDE-DIFF > 7000.00                    
221301                               MOVE NEJ TO INDATA-SW                      
221401                           END-IF                                         
221501                        END-IF                                            
221601                      END-IF                                              
221701                   ELSE                                                   
221801                      IF (W-KDVALISO = 'USD'                              
221901                         AND WS-VAERDE-DIFF > 1000.00)                    
222001                      OR (W-KDVALISO = 'CAD'                              
222101                         AND WS-VAERDE-DIFF >  500.00)                    
222201                         MOVE NEJ TO INDATA-SW                            
222301                      END-IF                                              
222401                   END-IF                                                 
222501                   IF INDATA-FEL                                          
222601                      MOVE INF-VALUE-TOO-HIGH-TEXT                        
222701                                     TO MOD-TEMFSINF                      
222801                      IF MID-CMD-INM = 'DAM'                              
222901                       MOVE MFS-NUM-FAELT-FEL                             
223001                                        TO MOD-KVSKROT-INM-ATTR           
223101                       MOVE MFS-NUM-FAELT-FEL                             
223201                                        TO MOD-KVANTMOT-INM-ATTR          
223301                      ELSE                                                
223401                       MOVE MFS-NUM-FAELT-FEL                             
223501                                        TO MOD-KVANTMOT-INM-ATTR          
223601                      END-IF                                              
223701                   END-IF                                                 
223801                END-IF                                                    
223901             END-IF                                                       
224001****** GÖR EN NY SEKTION NÄR MOTTAGANDE ÄR CDC OCH SÄNDANDE KINA          
224101****** OR US. NÄR DET ÄR EN NY ARTIKEL HÄMTA AVERAGE COST                 
224201             IF INDATA-OK AND DCS-CDC                                     
224301               MOVE W-SPAR-IDDC   TO W-6301-IDDC                          
224401               MOVE W-SPAR-IDFAKT TO W-IDFAKT                             
224501               PERFORM IMS-GHU-WL630111                                   
224601               MOVE 6302-IDDC-SEND TO SEND-WS-IDDC                        
224701               IF SEND-DCS-NDC-CN OR SEND-DCS-USA                         
224801                 MOVE 'CNY'          TO W-KDVALISO                        
224901                 IF SEND-DCS-USA                                          
225001                   MOVE 'USD'        TO W-KDVALISO                        
225101                 END-IF                                                   
225201                 MOVE SEND-WS-IDDC   TO W-IDDC                            
225301                 PERFORM IMS-GET-WDK711-GE                                
225401                 IF SEGMENT-FINNS                                         
225501                    MOVE SLAG-PRAVCOST  TO W-PRARTNTO                     
225601                    IF W-PRARTNTO = 0                                     
225701                      MOVE UPDATING-NOT-ALLOWED TO MED-IDMFSINF           
225801                      CALL WMEDKONV USING MED-WMEDAREA                    
225901                      MOVE MED-MFSINF TO MOD-TEMFSINF                     
226001                      MOVE NEJ TO INDATA-SW                               
226101                    END-IF                                                
226201                 ELSE                                                     
226301                   MOVE UPDATING-NOT-ALLOWED TO MED-IDMFSINF              
226401                   CALL WMEDKONV USING MED-WMEDAREA                       
226501                   MOVE MED-MFSINF TO MOD-TEMFSINF                        
226601                   MOVE NEJ TO INDATA-SW                                  
226701                 END-IF                                                   
226801               END-IF                                                     
226901             END-IF                                                       
227001         END-IF                                                           
227101     END-IF                                                               
227201                                                                          
227301     .                                                                    
227401     EJECT                                                                
227501 GFA-KOLLA-NYUPPLAGG SECTION.                                             
227601                                                                          
227701     MOVE ZERO TO WS-KVANTMOT-INM-NUM                                     
227801                  WS-KVSKROT-INM-NUM                                      
227901                  WS-KVANTMOT-INM                                         
228001                  WS-KVSKROT-INM                                          
228101                                                                          
228201     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
228301        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
228401        PERFORM IMS-GU-WDB601-SEND                                        
228501     END-IF                                                               
228601     PERFORM IMS-GET-WLARTC01                                             
228701     IF SEGMENT-FINNS                                                     
228801       MOVE K6-ART-KDSORT TO WS-KDSORT                                    
228901       MOVE K6-ART-KDPRODSL      TO TEST-KDPRODSL                         
229001       IF KDPRODSL-VOLVO-BIMA                                             
229101          CONTINUE                                                        
229201       ELSE                                                               
229301         IF DCS-NDC-NA AND DCS-USA                                        
229401            IF SEND-DCS-NDC-NA AND SEND-DCS-USA                           
229501               CONTINUE                                                   
229601            ELSE                                                          
229701              MOVE NEJ TO INDATA-SW                                       
229801              MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-INM-ATTR              
229901            END-IF                                                        
230001         ELSE                                                             
230101            MOVE NEJ TO INDATA-SW                                         
230201            MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-INM-ATTR                
230301         END-IF                                                           
230401       END-IF                                                             
230501        IF K6-ART-KDERS-UTG = +0                                          
230601           IF (MID-ADLAGOMR-INM = ALL '+')                                
230701           OR  (MID-ADGANG-INM = ALL '+')                                 
230801           OR  (MID-ADPLATS-INM = ALL '+')                                
230901               MOVE NEJ TO INDATA-SW                                      
231001               MOVE MFS-NUM-FAELT-FEL TO MOD-ADLAGOMR-INM-ATTR            
231101                                         MOD-ADGANG-INM-ATTR              
231201                                         MOD-ADPLATS-INM-ATTR             
231301           END-IF                                                         
231401                                                                          
231501           IF  ((MID-KVANTMOT-INM = ALL '+') OR                           
231601               (MID-KVANTMOT-INM = SPACE))                                
231701           AND ((MID-KVSKROT-INM = ALL '+') OR                            
231801               (MID-KVSKROT-INM = SPACE))                                 
231901                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTMOT-INM-ATTR          
232001                                           MOD-KVSKROT-INM-ATTR           
232101                 MOVE NEJ TO INDATA-SW                                    
232201             ELSE                                                         
232301                IF MID-KVANTMOT-INM NOT = ALL '+'                         
232401                   MOVE MID-KVANTMOT-INM TO WS-KVANTMOT-INM               
232501                   INSPECT MID-KVANTMOT-INM REPLACING ALL                 
232601                           SPACE BY ZERO                                  
232701                   IF WS-KVANTMOT-INM NOT NUMERIC                         
232801                      MOVE NEJ TO INDATA-SW                               
232901                      MOVE MFS-NUM-FAELT-FEL TO                           
233001                           MOD-KVANTMOT-INM-ATTR                          
233101                   ELSE                                                   
233201                      IF WS-KVANTMOT-INM > +0                             
233301                         MOVE JA TO NY-KVANTMOT-SW                        
233401                         MOVE WS-KVANTMOT-INM TO                          
233501                             WS-KVANTMOT-INM-NUM                          
233601                      END-IF                                              
233701                   END-IF                                                 
233801                 END-IF                                                   
233901                                                                          
234001                 IF MID-KVSKROT-INM NOT = ALL '+'                         
234101                    MOVE MID-KVSKROT-INM TO WS-KVSKROT-INM                
234201                    INSPECT WS-KVSKROT-INM REPLACING ALL                  
234301                              SPACE BY ZERO                               
234401                    IF WS-KVSKROT-INM NOT NUMERIC                         
234501                       MOVE MFS-NUM-FAELT-FEL TO                          
234601                              MOD-KVSKROT-INM-ATTR                        
234701                       MOVE NEJ TO INDATA-SW                              
234801                    ELSE                                                  
234901                       IF WS-KVSKROT-INM > +0                             
235001                          IF MID-CMD-INM NOT = 'DAM'                      
235101                             MOVE NEJ TO INDATA-SW                        
235201                             MOVE MFS-ALFA-FAELT-FEL TO                   
235301                                   MOD-CMD-INM-ATTR                       
235401                          ELSE                                            
235501                             MOVE JA TO NY-SKROT-SW                       
235601                             MOVE WS-KVSKROT-INM TO                       
235701                                   WS-KVSKROT-INM-NUM                     
235801                          END-IF                                          
235901                       ELSE                                               
236001                           MOVE NEJ TO INDATA-SW                          
236101                           MOVE MFS-NUM-FAELT-FEL TO                      
236201                                    MOD-KVSKROT-INM-ATTR                  
236301                           MOVE MFS-ALFA-FAELT-FEL TO                     
236401                                    MOD-CMD-INM-ATTR                      
236501                       END-IF                                             
236601                    END-IF                                                
236701                 END-IF                                                   
236801                                                                          
236901                 IF MID-CMD-INM = ALL '+' OR SPACE                        
237001                    CONTINUE                                              
237101                 ELSE                                                     
237201                    IF MID-CMD-INM NOT = 'DAM'                            
237301                       MOVE NEJ TO INDATA-SW                              
237401                       MOVE MFS-ALFA-FAELT-FEL TO                         
237501                                       MOD-CMD-INM-ATTR                   
237601                    END-IF                                                
237701                 END-IF                                                   
237801                                                                          
237901                 COMPUTE WS-SUMMA-KVANT = WS-KVANTMOT-INM-NUM             
238001                    + WS-KVSKROT-INM-NUM                                  
238101                 END-COMPUTE                                              
238201                 IF WS-SUMMA-KVANT = +0                                   
238301                    MOVE NEJ TO INDATA-SW                                 
238401                    MOVE MFS-NUM-FAELT-FEL TO                             
238501                          MOD-KVANTMOT-INM-ATTR                           
238601                          MOD-KVSKROT-INM-ATTR                            
238701                 END-IF                                                   
238801             END-IF                                                       
238901        ELSE                                                              
239001           MOVE NEJ TO INDATA-SW                                          
239101           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-INM-ATTR                 
239201        END-IF                                                            
239301     ELSE                                                                 
239401        MOVE NEJ TO INDATA-SW                                             
239501        MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-INM-ATTR                    
239601     END-IF                                                               
239701     .                                                                    
239801     EJECT                                                                
239901 GFB-KOLLA-BEFINTLIG-ARTIKEL SECTION.                                     
240001                                                                          
240101     MOVE ZERO TO WS-KVANTMOT-INM-NUM                                     
240201                  WS-KVSKROT-INM-NUM                                      
240301                  WS-KVANTMOT-INM                                         
240401                  WS-KVSKROT-INM                                          
240501     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
240601        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
240701        PERFORM IMS-GU-WDB601-SEND                                        
240801     END-IF                                                               
240901                                                                          
241001     PERFORM IMS-GET-WLARTC01                                             
241101     MOVE K6-ART-KDSORT TO WS-KDSORT                                      
241201     IF SEGMENT-FINNS                                                     
241301       MOVE K6-ART-KDPRODSL      TO TEST-KDPRODSL                         
241401       IF KDPRODSL-LOCAL                                                  
241501          IF DCS-NDC-NA AND DCS-USA                                       
241601             IF SEND-DCS-NDC-NA AND SEND-DCS-USA                          
241701                CONTINUE                                                  
241801             ELSE                                                         
241901                MOVE NEJ TO INDATA-SW                                     
242001                MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-INM-ATTR            
242101             END-IF                                                       
242201          ELSE                                                            
242301             MOVE NEJ TO INDATA-SW                                        
242401             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-INM-ATTR               
242501          END-IF                                                          
242601       END-IF                                                             
242701     END-IF                                                               
242801                                                                          
242901     IF MID-KVANTMOT-INM NOT = ALL '+'                                    
243001     AND MID-KVANTMOT-INM NOT = SPACE                                     
243101        MOVE MID-KVANTMOT-INM TO WS-KVANTMOT-INM                          
243201        INSPECT WS-KVANTMOT-INM REPLACING LEADING SPACE BY ZERO           
243301        IF WS-KVANTMOT-INM NOT NUMERIC                                    
243401           MOVE NEJ TO INDATA-SW                                          
243501           MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTMOT-INM-ATTR                
243601        ELSE                                                              
243701           MOVE WS-KVANTMOT-INM TO WS-KVANTMOT-INM-NUM                    
243801        END-IF                                                            
243901     END-IF                                                               
244001                                                                          
244101     IF (MID-ADLAGOMR-INM NOT = ALL '+')                                  
244201     OR (MID-ADGANG-INM NOT = ALL '+')                                    
244301     OR (MID-ADPLATS-INM NOT = ALL '+')                                   
244401        IF SLAG-ADLAGOMR NOT = ZERO                                       
244501           MOVE NEJ TO INDATA-SW                                          
244601           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDARTNR-INM-ATTR                
244701           MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR               
244801                                      MOD-ADGANG-INM-ATTR                 
244901                                      MOD-ADPLATS-INM-ATTR                
245001        END-IF                                                            
245101     ELSE                                                                 
245201        IF SLAG-ADLAGOMR = ZERO                                           
245301           MOVE NEJ TO INDATA-SW                                          
245401           MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR               
245501                                      MOD-ADGANG-INM-ATTR                 
245601                                      MOD-ADPLATS-INM-ATTR                
245701        END-IF                                                            
245801     END-IF                                                               
245901     IF (MID-ADLAGOMR-INM  = ALL '+' OR SPACE)                            
246001     AND (MID-ADGANG-INM   = ALL '+' OR SPACE)                            
246101     AND (MID-ADPLATS-INM  = ALL '+' OR SPACE)                            
246201         IF SLAG-ADLAGOMR  = ZERO                                         
246301         AND SLAG-ADGANG   = ZERO                                         
246401         AND SLAG-ADPLATS  = ZERO                                         
246501           MOVE NEJ TO INDATA-SW                                          
246601           MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR               
246701                                      MOD-ADGANG-INM-ATTR                 
246801                                      MOD-ADPLATS-INM-ATTR                
246901        END-IF                                                            
247001     ELSE                                                                 
247101        IF SLAG-ADLAGOMR  = ZERO                                          
247201        AND SLAG-ADGANG   = ZERO                                          
247301        AND SLAG-ADPLATS  = ZERO                                          
247401            IF MID-ADLAGOMR-INM = ALL '+' OR SPACE                        
247501               MOVE NEJ TO INDATA-SW                                      
247601               MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR           
247701            END-IF                                                        
247801            IF MID-ADGANG-INM = ALL '+' OR SPACE                          
247901               MOVE NEJ TO INDATA-SW                                      
248001               MOVE MFS-NUM-FAELT-FEL  TO MOD-ADGANG-INM-ATTR             
248101            END-IF                                                        
248201            IF MID-ADPLATS-INM = ALL '+' OR SPACE                         
248301               MOVE NEJ TO INDATA-SW                                      
248401               MOVE MFS-NUM-FAELT-FEL  TO MOD-ADPLATS-INM-ATTR            
248501            END-IF                                                        
248601        ELSE                                                              
248701           MOVE NEJ TO INDATA-SW                                          
248801           MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR               
248901                                      MOD-ADGANG-INM-ATTR                 
249001                                      MOD-ADPLATS-INM-ATTR                
249101        END-IF                                                            
249201     END-IF                                                               
249301                                                                          
249401     IF MID-KVSKROT-INM NOT = ALL '+'                                     
249501     AND MID-KVSKROT-INM NOT = ALL SPACE                                  
249601        MOVE MID-KVSKROT-INM TO WS-KVSKROT-INM                            
249701        INSPECT WS-KVSKROT-INM REPLACING LEADING SPACE BY ZERO            
249801        IF WS-KVSKROT-INM NOT NUMERIC                                     
249901           MOVE NEJ TO INDATA-SW                                          
250001           MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKROT-INM-ATTR                 
250101        ELSE                                                              
250201           IF MID-CMD-INM NOT = 'DAM'                                     
250301              MOVE NEJ TO INDATA-SW                                       
250401              MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-INM-ATTR                 
250501           ELSE                                                           
250601              MOVE WS-KVSKROT-INM TO WS-KVSKROT-INM-NUM                   
250701              MOVE JA TO NY-SKROT-SW                                      
250801              MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-INM-ATTR               
250901              MOVE MFS-NUM-FAELT-RAETT TO MOD-KVSKROT-INM-ATTR            
251001           END-IF                                                         
251101        END-IF                                                            
251201     END-IF                                                               
251301                                                                          
251401                 IF MID-CMD-INM = ALL '+' OR SPACE                        
251501                    CONTINUE                                              
251601                 ELSE                                                     
251701                    IF MID-CMD-INM NOT = 'DAM'                            
251801                       MOVE NEJ TO INDATA-SW                              
251901                       MOVE MFS-ALFA-FAELT-FEL TO                         
252001                                       MOD-CMD-INM-ATTR                   
252101                    END-IF                                                
252201                 END-IF                                                   
252301                                                                          
252401     COMPUTE WS-SUMMA-KVANT = (WS-KVSKROT-INM-NUM +                       
252501         WS-KVANTMOT-INM-NUM)                                             
252601     END-COMPUTE                                                          
252701     IF WS-SUMMA-KVANT = ZERO                                             
252801        MOVE NEJ TO INDATA-SW                                             
252901        MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTMOT-INM-ATTR                   
253001                                  MOD-KVSKROT-INM-ATTR                    
253101     END-IF                                                               
253201                                                                          
253301     IF INDATA-OK                                                         
253401         IF WS-KVSKROT-INM-NUM > +0                                       
253501            MOVE JA TO NY-SKROT-SW                                        
253601         END-IF                                                           
253701         IF WS-KVANTMOT-INM-NUM > +0                                      
253801            MOVE JA TO NY-KVANTMOT-SW                                     
253901         END-IF                                                           
254001     END-IF                                                               
254101     .                                                                    
254201     EJECT                                                                
254301 GFC-KOLLA-BEFINTLIG-ART-CDC SECTION.                                     
254401                                                                          
254501     MOVE ZERO TO WS-KVANTMOT-INM-NUM                                     
254601                  WS-KVSKROT-INM-NUM                                      
254701                  WS-KVANTMOT-INM                                         
254801                  WS-KVSKROT-INM                                          
254901     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
255001        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
255101        PERFORM IMS-GU-WDB601-SEND                                        
255201     END-IF                                                               
255301                                                                          
255401     MOVE K6-ART-KDSORT TO WS-KDSORT                                      
255501     MOVE K6-ART-KDPRODSL        TO TEST-KDPRODSL                         
255601     IF KDPRODSL-LOCAL                                                    
255701        MOVE NEJ TO INDATA-SW                                             
255801        MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-INM-ATTR                    
255901     END-IF                                                               
256001                                                                          
256101     PERFORM IMS-GHNP-WLARTC11                                            
256201                                                                          
256301     IF MID-KVANTMOT-INM NOT = ALL '+'                                    
256401     AND MID-KVANTMOT-INM NOT = SPACE                                     
256501        MOVE MID-KVANTMOT-INM TO WS-KVANTMOT-INM                          
256601        INSPECT WS-KVANTMOT-INM REPLACING LEADING SPACE BY ZERO           
256701        IF WS-KVANTMOT-INM NOT NUMERIC                                    
256801           MOVE NEJ TO INDATA-SW                                          
256901           MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTMOT-INM-ATTR                
257001        ELSE                                                              
257101           MOVE WS-KVANTMOT-INM TO WS-KVANTMOT-INM-NUM                    
257201        END-IF                                                            
257301     END-IF                                                               
257401                                                                          
257501     IF (MID-ADLAGOMR-INM NOT = ALL '+')                                  
257601     OR (MID-ADGANG-INM NOT = ALL '+')                                    
257701     OR (MID-ADPLATS-INM NOT = ALL '+')                                   
257801        IF CLAG-ADLAGOMR NOT = ZERO                                       
257901           MOVE NEJ TO INDATA-SW                                          
258001           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDARTNR-INM-ATTR                
258101           MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR               
258201                                      MOD-ADGANG-INM-ATTR                 
258301                                      MOD-ADPLATS-INM-ATTR                
258401        END-IF                                                            
258501     ELSE                                                                 
258601        IF CLAG-ADLAGOMR = ZERO                                           
258701           MOVE NEJ TO INDATA-SW                                          
258801           MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR               
258901                                      MOD-ADGANG-INM-ATTR                 
259001                                      MOD-ADPLATS-INM-ATTR                
259101        END-IF                                                            
259201     END-IF                                                               
259301                                                                          
259401     IF (MID-ADLAGOMR-INM  = ALL '+' OR SPACE)                            
259501     AND (MID-ADGANG-INM   = ALL '+' OR SPACE)                            
259601     AND (MID-ADPLATS-INM  = ALL '+' OR SPACE)                            
259701         IF CLAG-ADLAGOMR  = ZERO                                         
259801         AND CLAG-ADGANG   = ZERO                                         
259901         AND CLAG-ADPLATS  = ZERO                                         
260001           MOVE NEJ TO INDATA-SW                                          
260101           MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR               
260201                                      MOD-ADGANG-INM-ATTR                 
260301                                      MOD-ADPLATS-INM-ATTR                
260401        END-IF                                                            
260501     ELSE                                                                 
260601        IF CLAG-ADLAGOMR  = ZERO                                          
260701        AND CLAG-ADGANG   = ZERO                                          
260801        AND CLAG-ADPLATS  = ZERO                                          
260901            IF MID-ADLAGOMR-INM = ALL '+' OR SPACE                        
261001               MOVE NEJ TO INDATA-SW                                      
261101               MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR           
261201            END-IF                                                        
261301            IF MID-ADGANG-INM = ALL '+' OR SPACE                          
261401               MOVE NEJ TO INDATA-SW                                      
261501               MOVE MFS-NUM-FAELT-FEL  TO MOD-ADGANG-INM-ATTR             
261601            END-IF                                                        
261701            IF MID-ADPLATS-INM = ALL '+' OR SPACE                         
261801               MOVE NEJ TO INDATA-SW                                      
261901               MOVE MFS-NUM-FAELT-FEL  TO MOD-ADPLATS-INM-ATTR            
262001            END-IF                                                        
262101        ELSE                                                              
262201           MOVE NEJ TO INDATA-SW                                          
262301           MOVE MFS-NUM-FAELT-FEL  TO MOD-ADLAGOMR-INM-ATTR               
262401                                      MOD-ADGANG-INM-ATTR                 
262501                                      MOD-ADPLATS-INM-ATTR                
262601        END-IF                                                            
262701     END-IF                                                               
262801                                                                          
262901     IF MID-KVSKROT-INM NOT = ALL '+'                                     
263001     AND MID-KVSKROT-INM NOT = ALL SPACE                                  
263101        MOVE MID-KVSKROT-INM TO WS-KVSKROT-INM                            
263201        INSPECT WS-KVSKROT-INM REPLACING LEADING SPACE BY ZERO            
263301        IF WS-KVSKROT-INM NOT NUMERIC                                     
263401           MOVE NEJ TO INDATA-SW                                          
263501           MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKROT-INM-ATTR                 
263601        ELSE                                                              
263701           IF MID-CMD-INM NOT = 'DAM'                                     
263801              MOVE NEJ TO INDATA-SW                                       
263901              MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-INM-ATTR                 
264001           ELSE                                                           
264101              MOVE WS-KVSKROT-INM TO WS-KVSKROT-INM-NUM                   
264201              MOVE JA TO NY-SKROT-SW                                      
264301              MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-INM-ATTR               
264401              MOVE MFS-NUM-FAELT-RAETT TO MOD-KVSKROT-INM-ATTR            
264501           END-IF                                                         
264601        END-IF                                                            
264701     END-IF                                                               
264801                                                                          
264901     IF MID-CMD-INM = ALL '+' OR SPACE                                    
265001        CONTINUE                                                          
265101     ELSE                                                                 
265201        IF MID-CMD-INM NOT = 'DAM'                                        
265301           MOVE NEJ TO INDATA-SW                                          
265401           MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-INM-ATTR                    
265501        END-IF                                                            
265601     END-IF                                                               
265701                                                                          
265801     COMPUTE WS-SUMMA-KVANT = (WS-KVSKROT-INM-NUM +                       
265901         WS-KVANTMOT-INM-NUM)                                             
266001     END-COMPUTE                                                          
266101                                                                          
266201     IF WS-SUMMA-KVANT = ZERO                                             
266301        MOVE NEJ TO INDATA-SW                                             
266401        MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTMOT-INM-ATTR                   
266501                                  MOD-KVSKROT-INM-ATTR                    
266601     END-IF                                                               
266701                                                                          
266801     IF INDATA-OK                                                         
266901        IF WS-KVSKROT-INM-NUM > +0                                        
267001           MOVE JA TO NY-SKROT-SW                                         
267101        END-IF                                                            
267201        IF WS-KVANTMOT-INM-NUM > +0                                       
267301           MOVE JA TO NY-KVANTMOT-SW                                      
267401        END-IF                                                            
267501     END-IF                                                               
267601     .                                                                    
267701     EJECT                                                                
267801 GH-KOLLA-VAERDEGRANS-IDDC SECTION.                                       
267901                                                                          
268001     MOVE LOW-VALUE         TO W-WDL6A1KY-MIN                             
268101     MOVE HIGH-VALUE        TO W-WDL6A1KY-MAX                             
268201     MOVE W-SPAR-IDFAKT     TO W-SEQA-IDFAKT-MIN                          
268301                               W-SEQA-IDFAKT-MAX                          
268401     MOVE W-SPAR-IDKUNDRF   TO W-SEQA-IDKUNDRF-MIN                        
268501                               W-SEQA-IDKUNDRF-MAX                        
268601     MOVE W-SPAR-IDKUNDNR   TO W-SEQA-IDKUNDNR-MIN                        
268701                               W-SEQA-IDKUNDNR-MAX                        
268801     MOVE W-SPAR-IDKOLLI    TO W-SEQA-IDKOLLI-MIN                         
268901                               W-SEQA-IDKOLLI-MAX                         
269001     MOVE MID-IDARTNR(INDX) TO W-SEQA-IDARTNR-MIN                         
269101                               W-SEQA-IDARTNR-MAX                         
269201     MOVE '310'             TO W-IDPTYP                                   
269301                                                                          
269401     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
269501                                                                          
269601     IF SEGMENT-FINNS                                                     
269701      IF SEQA-IDDC = W-SPAR-IDDC                                          
269801        MOVE SEQA-IDARTNR  TO W-IDARTNR                                   
269901        MOVE SEQA-IDDC     TO W-IDDC                                      
270001        MOVE SEQA-DAINLEV  TO W-DAINLEV                                   
270101        PERFORM IMS-GHU-WLINLC11                                          
270201                                                                          
270301        MOVE INL-KVAVIS    TO W-KVAVIS                                    
270401        MOVE INL-PRARTNTO  TO W-PRARTNTO                                  
270501        MOVE ZERO          TO W-DIFF-KVANT                                
270601                                                                          
270701        MOVE W-SPAR-IDDC    TO W-6301-IDDC                                
270801        MOVE W-SPAR-IDFAKT  TO W-IDFAKT                                   
270901        PERFORM IMS-GHU-WL630111                                          
271001        MOVE 6302-IDDC-SEND TO SW-WS-IDDC                                 
271101        IF SW-WS-IDDC NOT = SW-DCS-IDDC                                   
271201           MOVE SW-WS-IDDC TO W-IDDC-B6                                   
271301           PERFORM IMS-GU-WDB601-SW                                       
271401        END-IF                                                            
271501        IF SW-DCS-CDC OR                                                  
271601          (SW-DCS-NDC-PF AND SW-DCS-AUSTRALIA) OR                         
271701           SW-DCS-DDC                                                     
271801          MOVE INL-IDDISTR TO TEST-IDDISTR                                
271901          IF DIST79-DEALER-PRICE                                          
272001            MOVE INL-KDVALISO   TO W-KDVALISO                             
272101          ELSE                                                            
272201            MOVE 'SEK'          TO W-KDVALISO                             
272301          END-IF                                                          
272401        ELSE                                                              
272501           IF SW-DCS-NDC-NA AND SW-DCS-USA                                
272601              MOVE 'USD'    TO W-KDVALISO                                 
272701           ELSE                                                           
272801              IF SW-DCS-NDC-NA AND SW-DCS-CANADA                          
272901                 MOVE 'CAD' TO W-KDVALISO                                 
273001              END-IF                                                      
273101           END-IF                                                         
273201        END-IF                                                            
273301                                                                          
273401        IF MID-CMD-IN(INDX) = 'DAM'                                       
273501           MOVE W-KVSKROT (INDX) TO W-DIFF-KVANT                          
273601        ELSE                                                              
273701           IF W-KVANTMOT (INDX) NOT = W-KVAVIS                            
273801              MOVE W-KVANTMOT (INDX) TO W-TEMP-KVANT                      
273901              COMPUTE W-DIFF-KVANT = W-KVAVIS                             
274001                                     - W-TEMP-KVANT                       
274101              END-COMPUTE                                                 
274201           END-IF                                                         
274301        END-IF                                                            
274401                                                                          
274501        IF MFS-UPDATE AND (DCS-NDC-NA OR DCS-NDC-PF)                      
274601           COMPUTE WS-VAERDE-DIFF = W-DIFF-KVANT * W-PRARTNTO             
274701           END-COMPUTE                                                    
274801           IF WS-VAERDE-DIFF < ZERO                                       
274901              COMPUTE WS-VAERDE-DIFF = WS-VAERDE-DIFF * -1                
275001              END-COMPUTE                                                 
275101           END-IF                                                         
275201           IF W-KDVALISO = 'SEK'                                          
275301              IF (DCS-NDC-PF AND DCS-JAPAN AND                            
275401                  WS-VAERDE-DIFF > 3000.00)                               
275501                 MOVE NEJ TO INDATA-SW                                    
275601              ELSE                                                        
275701                 IF DCS-NDC-NA                                            
275801                    IF WS-VAERDE-DIFF > 7000.00                           
275901                       MOVE NEJ TO INDATA-SW                              
276001                    END-IF                                                
276101                 END-IF                                                   
276201              END-IF                                                      
276301           ELSE                                                           
276401              IF (W-KDVALISO = 'USD' AND WS-VAERDE-DIFF > 1000.00)        
276501              OR (W-KDVALISO = 'CAD' AND WS-VAERDE-DIFF >  500.00)        
276601                 MOVE NEJ TO INDATA-SW                                    
276701              END-IF                                                      
276801           END-IF                                                         
276901           IF INDATA-FEL                                                  
277001              MOVE INF-VALUE-TOO-HIGH-TEXT TO MOD-TEMFSINF                
277101              IF MID-CMD-IN(INDX) = 'DAM'                                 
277201                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKROT-ATTR(INDX)         
277301              ELSE                                                        
277401                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTMOT-ATTR(INDX)        
277501              END-IF                                                      
277601           END-IF                                                         
277701        END-IF                                                            
277801       ELSE                                                               
277901          MOVE NEJ TO INDATA-SW                                           
278001          IF MID-CMD-IN(INDX) = 'DAM'                                     
278101             MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKROT-ATTR(INDX)             
278201          ELSE                                                            
278301             MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTMOT-ATTR(INDX)            
278401          END-IF                                                          
278501       END-IF                                                             
278601     ELSE                                                                 
278701        MOVE NEJ TO INDATA-SW                                             
278801        IF MID-CMD-IN(INDX) = 'DAM'                                       
278901           MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKROT-ATTR(INDX)               
279001        ELSE                                                              
279101           MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTMOT-ATTR(INDX)              
279201        END-IF                                                            
279301     END-IF                                                               
279401     .                                                                    
279501     EJECT                                                                
279601 GI-KOLLA-BIN SECTION.                                                    
279701                                                                          
279801     MOVE MID-MID-IDUSER-003 TO WS-IDUSER-003                             
279901                                                                          
280001     MOVE +1 TO INDX                                                      
280101                TAB-IX                                                    
280201                                                                          
280301     PERFORM UNTIL INDX > MAX-INDX                                        
280401        IF  MID-MID-IDKUNDRF(INDX) = ALL '+'                              
280501        AND MID-MID-IDKUNDNR(INDX) = ALL '+'                              
280601        AND MID-MID-IDKOLLI (INDX) = ALL '+'                              
280701            CONTINUE                                                      
280801        ELSE                                                              
280901           INSPECT MID-MID-IDFAKT-IN REPLACING                            
281001                   LEADING SPACE BY ZERO                                  
281101           MOVE MID-MID-IDFAKT-IN TO BIN-TAB-IDFAKT(TAB-IX)               
281201           MOVE BIN-TAB-IDFAKT(1) TO W-SPAR-IDFAKT                        
281301                                                                          
281401           MOVE MID-MID-IDKUNDRF(INDX) TO BIN-TAB-IDKUNDRF(TAB-IX)        
281501                                                                          
281601           INSPECT MID-MID-IDKUNDNR(INDX) REPLACING                       
281701                   LEADING SPACE BY ZERO                                  
281801           MOVE MID-MID-IDKUNDNR(INDX) TO BIN-TAB-IDKUNDNR(TAB-IX)        
281901                                                                          
282001           INSPECT MID-MID-IDKOLLI(INDX) REPLACING                        
282101                   LEADING SPACE BY ZERO                                  
282201           MOVE MID-MID-IDKOLLI(INDX)  TO BIN-TAB-IDKOLLI(TAB-IX)         
282301                                                                          
282401           IF  BIN-TAB-IDFAKT(INDX) NUMERIC                               
282501           AND BIN-TAB-IDKUNDNR(INDX) NUMERIC                             
282601           AND BIN-TAB-IDKOLLI(INDX) NUMERIC                              
282701              PERFORM GIA-KOLLA-VIDARE-BIN                                
282801              ADD +1 TO TAB-IX                                            
282901           ELSE                                                           
283001              MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                     
283101              DISPLAY FELTEXT                                             
283201              CALL FELLOG                                                 
283301           END-IF                                                         
283401        END-IF                                                            
283501                                                                          
283601        ADD +1 TO INDX                                                    
283701     END-PERFORM                                                          
283801     .                                                                    
283901     EJECT                                                                
284001 GIA-KOLLA-VIDARE-BIN SECTION.                                            
284101                                                                          
284201     MOVE LOW-VALUE                TO W-WDL6A1KY-MIN                      
284301     MOVE HIGH-VALUE               TO W-WDL6A1KY-MAX                      
284401     MOVE BIN-TAB-IDFAKT(TAB-IX)   TO W-SEQA-IDFAKT-MIN                   
284501                                      W-SEQA-IDFAKT-MAX                   
284601     MOVE BIN-TAB-IDKUNDRF(TAB-IX) TO W-SEQA-IDKUNDRF-MIN                 
284701                                      W-SEQA-IDKUNDRF-MAX                 
284801     MOVE BIN-TAB-IDKUNDNR(TAB-IX) TO W-SEQA-IDKUNDNR-MIN                 
284901                                      W-SEQA-IDKUNDNR-MAX                 
285001     MOVE BIN-TAB-IDKOLLI(TAB-IX)  TO W-SEQA-IDKOLLI-MIN                  
285101                                      W-SEQA-IDKOLLI-MAX                  
285201     MOVE '310'                    TO W-IDPTYP                            
285301                                                                          
285401     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
285501     IF SEGMENT-SAKNAS                                                    
285601        MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                           
285701        DISPLAY FELTEXT                                                   
285801        CALL FELLOG                                                       
285901     ELSE                                                                 
286001        PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                        
286101           MOVE SEQA-IDARTNR TO W-IDARTNR                                 
286201           MOVE SEQA-IDDC    TO W-IDDC                                    
286301                                                                          
286401           IF SEQA-IDDC NOT =  W-SPAR-IDDC                                
286501              MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                     
286601              DISPLAY FELTEXT                                             
286701              CALL FELLOG                                                 
286801           END-IF                                                         
286901                                                                          
287001           IF DCS-CDC                                                     
287101             PERFORM IMS-GET-WLARTC11                                     
287201             IF CLAG-ADLAGOMR  = ZERO                                     
287301               MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                    
287401               DISPLAY FELTEXT                                            
287501               CALL FELLOG                                                
287601             END-IF                                                       
287701           ELSE                                                           
287801             PERFORM IMS-GET-WDK711                                       
287901                                                                          
288001             IF SLAG-ADLAGOMR  = ZERO                                     
288101               MOVE 'FEL VID W6T303X-TRANS' TO FELTEXT                    
288201               DISPLAY FELTEXT                                            
288301               CALL FELLOG                                                
288401             END-IF                                                       
288501           END-IF                                                         
288601                                                                          
288701           PERFORM IMS-GN-WLINLD01                                        
288801        END-PERFORM                                                       
288901     END-IF                                                               
289001     .                                                                    
289101     EJECT                                                                
289201 H-UPPDATERA SECTION.                                                     
289301                                                                          
289401     MOVE W-SPAR-IDDC   TO W-6301-IDDC                                    
289501                           WS-IDDC                                        
289601     MOVE W-SPAR-IDFAKT TO W-IDFAKT                                       
289701     PERFORM IMS-GHU-WL630111                                             
289801     MOVE 6302-IDDISTR   TO DIST35-IDDISTR                                
289901     MOVE 6302-IDDC-SEND TO SEND-WS-IDDC                                  
290001     MOVE NEJ            TO NYUPPLAEGG-SW                                 
290101                            WS-A03-SKAPAD                                 
290201                                                                          
290301     IF MFS-UPD-X                                                         
290401        MOVE NEJ TO WS-A03-SKAPAD                                         
290501        PERFORM K-UPPDATERA-X-TRANS                                       
290601     ELSE                                                                 
290701        IF  MID-KOLLI-KLART   = 'X'                                       
290801        OR  MID-KOLLI-KLART   = 'Y'                                       
290901          IF (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                       
291001            PERFORM HA-UPPDATERA-KOLLI-KLART                              
291101          ELSE                                                            
291201            MOVE NEJ TO WS-A03-SKAPAD                                     
291301            PERFORM HA-UPPDATERA-KOLLI-KLART-PV                           
291401          END-IF                                                          
291501                                                                          
291601        ELSE                                                              
291701            MOVE +1              TO INDX                                  
291801            MOVE NEJ             TO WS-A03-SKAPAD                         
291901            PERFORM UNTIL INDX > MAX-INDX                                 
292001                IF  MID-CMD-IN (INDX) = 'DAM'                             
292101                  IF (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)               
292201                    PERFORM HB-KOLLI-DAMAGED                              
292301                  ELSE                                                    
292401                    PERFORM HB-KOLLI-DAMAGED-PV                           
292501                  END-IF                                                  
292601                ELSE                                                      
292701                    IF MID-CMD-IN (INDX) = 'PB '                          
292801                       CONTINUE                                           
292901                    ELSE                                                  
293001                        IF  MID-KVANTMOT-IN (INDX) NOT = ALL '+'          
293101                        AND MID-KVANTMOT-IN (INDX) NOT = SPACE            
293201                          IF (NDC-US                                      
293301                          AND DIST35-NDCCN-NDCUS-REFILL)                  
293401                            PERFORM HC-OOVER-UNDER-LEVERANS               
293501                          ELSE                                            
293601                            PERFORM HC-OOVER-UNDER-LEVERANS-PV            
293701                          END-IF                                          
293801                                                                          
293901                        ELSE                                              
294001                            IF  MID-ADLAGOMR (INDX) NOT = ALL '+'         
294101                                PERFORM HF-ENDAST-LAGERPLATS              
294201                            END-IF                                        
294301                        END-IF                                            
294401                    END-IF                                                
294501                END-IF                                                    
294601                                                                          
294701                ADD 1        TO INDX                                      
294801            END-PERFORM                                                   
294901                                                                          
295001            IF NY-BEFINTLIG-ART                                           
295101              IF NDC-US AND DIST35-NDCCN-NDCUS-REFILL                     
295201                PERFORM HD-NYUPPLAEGG                                     
295301              ELSE                                                        
295401                PERFORM HD-NYUPPLAEGG-PV                                  
295501              END-IF                                                      
295601            ELSE                                                          
295701               IF NY-NYUPPLAEGG-ART                                       
295801                 IF NDC-US AND DIST35-NDCCN-NDCUS-REFILL                  
295901                   PERFORM HG-NY-NYUPPLAEGG-ART                           
296001                 ELSE                                                     
296101                   PERFORM HG-NY-NYUPPLAEGG-ART-PV                        
296201                 END-IF                                                   
296301               END-IF                                                     
296401            END-IF                                                        
296501                                                                          
296601            IF  W-IDLOPNRM NOT = ZERO                                     
296701              MOVE W-IDLOPNRM TO 6018-IDLOPNRM                            
296801              PERFORM IMS-REPL-W6LOPA                                     
296901            END-IF                                                        
297001                                                                          
297101            IF  TRANS-OHUVUD-DAM-SKAPAD AND ORAD-IX > +1                  
297201                MOVE 'J' TO ORAD-MID-FLSLUT                               
297301                PERFORM S03-CALL-W006KOM                                  
297401            END-IF                                                        
297501        END-IF                                                            
297601     END-IF                                                               
297701                                                                          
297801     PERFORM HE-UPPDATERA-WL630111                                        
297901     IF WS-A03-SKAPAD = JA                                                
298001        IF WS-FAKTURA-KLAR = JA                                           
298101           MOVE 'Y' TO EKOTRA03-FLSLUT                                    
298201        END-IF                                                            
298301        PERFORM S091-SKRIV-EKOTRANS-A03                                   
298401     END-IF                                                               
298501                                                                          
298601                                                                          
298701     IF MFS-UPD-X                                                         
298801        MOVE 'GB' TO MED-IDSKYLT                                          
298901        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
299001        CALL WMEDKONV USING MED-WMEDAREA                                  
299101        MOVE MED-MFSINF TO MID-MOD-TEMFSINF                               
299201     ELSE                                                                 
299301        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
299401        CALL WMEDKONV USING MED-WMEDAREA                                  
299501        MOVE MED-MFSINF TO W-TEMFSINF                                     
299601        PERFORM MFS-RENSA-FAELT-IN                                        
299701        PERFORM MFS-RENSA-NY-ARTIKEL-FAELT                                
299801                                                                          
299901        MOVE +1 TO INDX                                                   
300001        PERFORM UNTIL INDX > MAX-INDX                                     
300101           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
300201           ADD +1 TO INDX                                                 
300301        END-PERFORM                                                       
300401     END-IF                                                               
300501     .                                                                    
300601     EJECT                                                                
300701 HA-UPPDATERA-KOLLI-KLART-PV SECTION.                                     
300801******************************************************************        
300901* DENNA SEKTION ANVÄNDS OCKSÅ FRÅN K-UPPDATERA-X-TRANS                    
301001******************************************************************        
301101     MOVE ZERO            TO W-KVRADER                                    
301201                                                                          
301301     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
301401     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
301501     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
301601                             W-SEQA-IDFAKT-MAX                            
301701     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
301801                             W-SEQA-IDKUNDRF-MAX                          
301901     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
302001                             W-SEQA-IDKUNDNR-MAX                          
302101     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
302201                             W-SEQA-IDKOLLI-MAX                           
302301     MOVE '310'           TO W-IDPTYP                                     
302401     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
302501                                                                          
302601     PERFORM UNTIL SEGMENT-SAKNAS                                         
302701        IF WS-A03-SKAPAD = JA                                             
302801           PERFORM S091-SKRIV-EKOTRANS-A03                                
302901        END-IF                                                            
303001        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
303101        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
303201        MOVE SEQA-IDDC    TO W-IDDC                                       
303301        PERFORM IMS-GHU-WLINLC11                                          
303401        IF DCS-CDC                                                        
303501          PERFORM IMS-GET-WLARTC11                                        
303601          MOVE CLAG-ADLAGOMR   TO INL-ADLAGOMR                            
303701          MOVE CLAG-ADGANG     TO INL-ADGANG                              
303801          MOVE CLAG-ADPLATS    TO INL-ADPLATS                             
303901        ELSE                                                              
304001          PERFORM IMS-GET-WDK711                                          
304101          MOVE SLAG-ADLAGOMR   TO INL-ADLAGOMR                            
304201          MOVE SLAG-ADGANG     TO INL-ADGANG                              
304301          MOVE SLAG-ADPLATS    TO INL-ADPLATS                             
304401        END-IF                                                            
304501                                                                          
304601        MOVE INL-KVAVIS     TO W-KVAVIS                                   
304701        MOVE 'R32'          TO INL-IDPTYP                                 
304801        IF MFS-UPD-X                                                      
304901           IF WS-IDUSER-003 NOT = SPACE                                   
305001              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
305101           END-IF                                                         
305201        ELSE                                                              
305301           IF MID-IDUSER-003 = ALL '+' OR SPACE                           
305401              CONTINUE                                                    
305501           ELSE                                                           
305601              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
305701           END-IF                                                         
305801        END-IF                                                            
305901        MOVE W-TIME-N       TO AKTUELL-TID                                
306001        IF DCS-NDC-NA OR DCS-NDC-PF                                       
306101           PERFORM S18-FIXA-LOKALTID                                      
306201           MOVE MSGI-TILOKDAT  TO INL-TIINLINL                            
306301           MOVE MSGI-TILOKTID  TO INL-TIINLITI                            
306401        ELSE                                                              
306501           MOVE W-DAGENS-DATUM TO INL-TIINLINL                            
306601           MOVE AKTUELL-TTMM   TO INL-TIINLITI                            
306701        END-IF                                                            
306801        MOVE INL-KVAVIS     TO INL-KVANTMOT                               
306901        ADD +1              TO W-KVRADER                                  
307001                                                                          
307101        PERFORM IMS-REPL-WLINLC11                                         
307201                                                                          
307301        PERFORM IMS-GU-WLARTC01                                           
307401        MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                            
307402        MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                            
307501        PERFORM IMS-GHNP-WLARTC11                                         
307601        MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                        
307701                                                                          
307801        IF DCS-CDC                                                        
307901          ADD W-KVAVIS             TO CLAG-KVLS                           
308001          SUBTRACT W-KVAVIS      FROM CLAG-KVAKS-CDC                      
308101        ELSE                                                              
308201          MOVE SLAG-KVLS           TO WS-OLD-KVLS                         
308301          ADD W-KVAVIS             TO SLAG-KVLS                           
308401          SUBTRACT W-KVAVIS      FROM SLAG-KVAKS-SDC                      
308501        END-IF                                                            
308601        MOVE W-KVAVIS              TO WS-RO-KVANTMOT                      
308701                                                                          
308801        IF DCS-NDC-NA OR DCS-NDC-PF                                       
308901           PERFORM S19-EV-RO-TACKNING                                     
309001        ELSE                                                              
309101           IF DCS-CDC                                                     
309201              PERFORM S19-EV-RO-TACKNING-CDC                              
309301           END-IF                                                         
309401        END-IF                                                            
309501                                                                          
309601        IF DCS-NDC-NA                                                     
309701           MOVE INL-KVANTMOT       TO EKOTRA03-KVANTMOT                   
309801                                      AVG-KVANTMOT                        
309901           MOVE SPACE              TO EKOTRA03-KDANMORS                   
310001           MOVE ZERO               TO EKOTRA03-KVSKROT                    
310101           MOVE SLAG-PRAVCOST      TO EKOTRA03-PRAVCOST-OLD               
310201           MOVE 'ETT'  TO STEXT                                           
310301           PERFORM S10-OMRAKN-MEDELPRIS-NA                                
310401           MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                       
310501                                      EKOTRA03-PRAVCOST                   
310601           MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                       
310701           MOVE AVG-REMARKUP       TO EKOTRA03-REMARKUP                   
310801           PERFORM S09-SKAPA-EKOTRANS-A03                                 
310901        END-IF                                                            
311001        IF DCS-CDC                                                        
311101          PERFORM IMS-REPL-WLARTC11                                       
311201        ELSE                                                              
311301          PERFORM IMS-REPL-WDK711                                         
311401        END-IF                                                            
311501        PERFORM S21-SALDOLOGG-DATA                                        
311601        MOVE W-SPAR-IDDC          TO LOGG-IDDC                            
311701        MOVE W-KVAVIS             TO LOGG-KVART-SALDO                     
311801        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
311901        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
312001        IF DCS-CDC                                                        
312101          MOVE CLAG-KVLS          TO LOGG-KVLS                            
312201          COMPUTE LOGG-KVAKS       = CLAG-KVAKS-CDC                       
312301                                   + CLAG-KVAKS-T                         
312401          MOVE CLAG-KVEFRS        TO LOGG-KVEFRS                          
312501          MOVE CLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                       
312601        ELSE                                                              
312701          MOVE SLAG-KVLS          TO LOGG-KVLS                            
312801          MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                           
312901          MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                          
313001          MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                       
313101        END-IF                                                            
313201        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
313301        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
313401        PERFORM S22-ISRT-SALDOLOGG                                        
313501                                                                          
313601        PERFORM IMS-GN-WLINLD01                                           
313701     END-PERFORM                                                          
313801                                                                          
313901     PERFORM S08-UPPDATERA-6301                                           
314001     .                                                                    
314101     EJECT                                                                
314201 HA-UPPDATERA-KOLLI-KLART SECTION.                                        
314301******************************************************************        
314401* DENNA SEKTION ANVÄNDS OCKSÅ FRÅN K-UPPDATERA-X-TRANS                    
314501******************************************************************        
314601     MOVE ZERO            TO W-KVRADER                                    
314701                                                                          
314801     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
314901     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
315001     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
315101                             W-SEQA-IDFAKT-MAX                            
315201     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
315301                             W-SEQA-IDKUNDRF-MAX                          
315401     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
315501                             W-SEQA-IDKUNDNR-MAX                          
315601     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
315701                             W-SEQA-IDKOLLI-MAX                           
315801     MOVE '310'           TO W-IDPTYP                                     
315901     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
316001                                                                          
316101     PERFORM UNTIL SEGMENT-SAKNAS                                         
316201        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
316301        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
316401        MOVE SEQA-IDDC    TO W-IDDC                                       
316501        PERFORM IMS-GHU-WLINLC11                                          
316601        PERFORM IMS-GET-WDK711                                            
316701                                                                          
316801        MOVE INL-KVAVIS     TO W-KVAVIS                                   
316901        MOVE 'R32'          TO INL-IDPTYP                                 
317001        IF MFS-UPD-X                                                      
317101           IF WS-IDUSER-003 NOT = SPACE                                   
317201              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
317301           END-IF                                                         
317401        ELSE                                                              
317501           IF MID-IDUSER-003 = ALL '+' OR SPACE                           
317601              CONTINUE                                                    
317701           ELSE                                                           
317801              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
317901           END-IF                                                         
318001        END-IF                                                            
318101        MOVE W-TIME-N       TO AKTUELL-TID                                
318201                                                                          
318301        PERFORM S18-FIXA-LOKALTID                                         
318401        MOVE MSGI-TILOKDAT  TO INL-TIINLINL                               
318501        MOVE MSGI-TILOKTID  TO INL-TIINLITI                               
318601                                                                          
318701        MOVE SLAG-ADLAGOMR  TO INL-ADLAGOMR                               
318801        MOVE SLAG-ADGANG    TO INL-ADGANG                                 
318901        MOVE SLAG-ADPLATS   TO INL-ADPLATS                                
319001        MOVE INL-KVAVIS     TO INL-KVANTMOT                               
319101        ADD +1              TO W-KVRADER                                  
319201                                                                          
319301        PERFORM IMS-REPL-WLINLC11                                         
319401                                                                          
319501        MOVE SLAG-KVLS    TO WS-OLD-KVLS                                  
319601        MOVE SLAG-KVEFRS  TO WS-OLD-KVEFRS                                
319701        ADD W-KVAVIS      TO SLAG-KVLS                                    
319801        SUBTRACT W-KVAVIS FROM SLAG-KVAKS-SDC                             
319901        MOVE W-KVAVIS     TO WS-RO-KVANTMOT                               
320001                                                                          
320101        PERFORM IMS-GU-WLARTC01                                           
320201        MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                            
320202        MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                            
320301        PERFORM IMS-GHNP-WLARTC11                                         
320401        MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                        
320501                                                                          
320601        PERFORM S19-EV-RO-TACKNING                                        
320701                                                                          
320801* SENDING CDC                                                             
320901* BERÄKNING AVGCOST                                                       
321001        MOVE INL-KVANTMOT       TO AVG-KVANTMOT                           
321101        PERFORM S10-OMRAKN-MEDELPRIS                                      
321201        MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                          
321301        MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                          
321401        PERFORM IMS-REPL-WDK711                                           
321501        PERFORM S21-SALDOLOGG-DATA                                        
321601        MOVE W-SPAR-IDDC          TO LOGG-IDDC                            
321701        MOVE SLAG-KVLS            TO LOGG-KVLS                            
321801        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
321901        MOVE W-KVAVIS             TO LOGG-KVART-SALDO                     
322001        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
322101        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
322201        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
322301        MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                        
322401        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
322501        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
322601        PERFORM S22-ISRT-SALDOLOGG                                        
322701                                                                          
322801        PERFORM IMS-GN-WLINLD01                                           
322901     END-PERFORM                                                          
323001                                                                          
323101     PERFORM S08-UPPDATERA-6301                                           
323201     .                                                                    
323301     EJECT                                                                
323401                                                                          
323501 HB-KOLLI-DAMAGED-PV      SECTION.                                        
323601                                                                          
323701     MOVE ZERO               TO W-KVRADER                                 
323801                                                                          
323901     MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                            
324001     MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                            
324101     MOVE W-SPAR-IDFAKT      TO W-SEQA-IDFAKT-MIN                         
324201                                W-SEQA-IDFAKT-MAX                         
324301     MOVE W-SPAR-IDKUNDRF    TO W-SEQA-IDKUNDRF-MIN                       
324401                                W-SEQA-IDKUNDRF-MAX                       
324501     MOVE W-SPAR-IDKUNDNR    TO W-SEQA-IDKUNDNR-MIN                       
324601                                W-SEQA-IDKUNDNR-MAX                       
324701     MOVE W-SPAR-IDKOLLI     TO W-SEQA-IDKOLLI-MIN                        
324801                                W-SEQA-IDKOLLI-MAX                        
324901     MOVE '310'              TO W-IDPTYP                                  
325001     MOVE MID-IDARTNR (INDX) TO W-SEQA-IDARTNR-MIN                        
325101                                W-SEQA-IDARTNR-MAX                        
325201     PERFORM IMS-GU-WLINLD01                                              
325301                                                                          
325401     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
325501                          W-IDARTNR-NYCKEL-SPAR                           
325601     MOVE SEQA-DAINLEV TO W-DAINLEV                                       
325701                          W-DAINLEV-NYCKEL-SPAR                           
325801     MOVE SEQA-IDDC    TO W-IDDC                                          
325901                          W-IDDC-NYCKEL-SPAR                              
326001     PERFORM IMS-GHU-WLINLC11                                             
326101     IF DCS-CDC                                                           
326201       CONTINUE                                                           
326301     ELSE                                                                 
326401       PERFORM IMS-GET-WDK711                                             
326501     END-IF                                                               
326601                                                                          
326701     PERFORM IMS-GU-WLARTC01                                              
326801     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
326802     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
326901     PERFORM IMS-GHNP-WLARTC11                                            
327001     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
327101     MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                          
327201                                                                          
327301                                                                          
327401     MOVE INL-KVAVIS   TO W-KVAVIS                                        
327501     MOVE INL-KDFRAKT  TO W-KDFRAKT                                       
327601     MOVE INL-FLSKAKOL TO W-FLSKAKOL                                      
327701     MOVE INL-IDDISTR  TO WS-SAP-IDDISTR                                  
327801     MOVE INL-IDKUNDNR TO WS-SAP-IDKUNDNR                                 
327901                                                                          
328001     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
328101        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
328201        PERFORM IMS-GU-WDB601-SEND                                        
328301     END-IF                                                               
328401                                                                          
328501     IF INL-KVANTMOT > ZERO                                               
328601        IF DCS-CDC                                                        
328701           ADD W-KVANTMOT(INDX)    TO CLAG-KVLS                           
328801           SUBTRACT W-KVANTMOT(INDX)                                      
328901                                 FROM CLAG-KVAKS-CDC                      
329001        ELSE                                                              
329101           ADD W-KVANTMOT(INDX)    TO SLAG-KVLS                           
329201           SUBTRACT W-KVANTMOT(INDX)                                      
329301                                 FROM SLAG-KVAKS-SDC                      
329401           IF SEND-DCS-CDC AND DIST35-NDCCN-NDCUS-REFILL                  
329501              ADD W-KVSKROT (INDX)    TO SLAG-KVLS                        
329601           END-IF                                                         
329701        END-IF                                                            
329801*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
329901*   ---FÖR ATT UPPDATERA I WDL9                                           
330001        IF W-KVANTMOT(INDX) NOT = 0                                       
330101           PERFORM S21-SALDOLOGG-DATA                                     
330201           MOVE W-SPAR-IDDC        TO LOGG-IDDC                           
330301           IF DCS-CDC                                                     
330401              MOVE CLAG-KVLS       TO LOGG-KVLS                           
330501              COMPUTE LOGG-KVAKS    = CLAG-KVAKS-CDC                      
330601                                    + CLAG-KVAKS-T                        
330701              MOVE CLAG-KVEFRS     TO LOGG-KVEFRS                         
330801              MOVE CLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                      
330901           ELSE                                                           
331001              MOVE SLAG-KVLS       TO LOGG-KVLS                           
331101              MOVE SLAG-KVAKS-SDC  TO LOGG-KVAKS                          
331201              MOVE SLAG-KVEFRS     TO LOGG-KVEFRS                         
331301              MOVE SLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                      
331401           END-IF                                                         
331501           MOVE W-KVANTMOT(INDX)   TO LOGG-KVART-SALDO                    
331601           MOVE '-'                TO LOGG-IDTECKEN-KVAKS                 
331701           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
331801           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
331901           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
332001           PERFORM S22-ISRT-SALDOLOGG                                     
332101        END-IF                                                            
332201        IF DCS-CDC                                                        
332301           SUBTRACT W-KVSKROT (INDX)                                      
332401                                 FROM CLAG-KVAKS-CDC                      
332501        ELSE                                                              
332601           SUBTRACT W-KVSKROT (INDX)                                      
332701                                 FROM SLAG-KVAKS-SDC                      
332801        END-IF                                                            
332901        IF DCS-SDC                                                        
333001        OR DCS-NDC-PF                                                     
333101           ADD W-KVSKROT (INDX)    TO SLAG-KVLS                           
333201        END-IF                                                            
333301        IF DCS-CDC                                                        
333401           ADD W-KVSKROT (INDX)    TO CLAG-KVLS                           
333501        END-IF                                                            
333601*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
333701*   ---FÖR ATT UPPDATERA I WDL9                                           
333801        IF W-KVSKROT(INDX) NOT = 0                                        
333901           PERFORM S21-SALDOLOGG-DATA                                     
334001           MOVE W-SPAR-IDDC        TO LOGG-IDDC                           
334101           IF DCS-CDC                                                     
334201              MOVE CLAG-KVEFRS     TO LOGG-KVEFRS                         
334301              MOVE CLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                      
334401              COMPUTE LOGG-KVAKS    = CLAG-KVAKS-CDC                      
334501                                    + CLAG-KVAKS-T                        
334601              MOVE CLAG-KVLS       TO LOGG-KVLS                           
334701           ELSE                                                           
334801              MOVE SLAG-KVEFRS     TO LOGG-KVEFRS                         
334901              MOVE SLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                      
335001              MOVE SLAG-KVAKS-SDC  TO LOGG-KVAKS                          
335101              MOVE SLAG-KVLS       TO LOGG-KVLS                           
335201           END-IF                                                         
335301           MOVE W-KVSKROT(INDX)    TO LOGG-KVART-SALDO                    
335401           MOVE '-'                TO LOGG-IDTECKEN-KVAKS                 
335501           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
335601           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
335701           IF DCS-SDC                                                     
335801           OR DCS-NDC-PF                                                  
335901              MOVE '+'             TO LOGG-IDTECKEN-KVLS                  
336001           ELSE                                                           
336101              MOVE SPACE           TO LOGG-IDTECKEN-KVLS                  
336201           END-IF                                                         
336301           PERFORM S22-ISRT-SALDOLOGG                                     
336401           IF DCS-SDC                                                     
336501           OR DCS-NDC-PF                                                  
336601              IF SEND-DCS-DDC                                             
336701                 MOVE W-KVSKROT(INDX) TO FILC2-KVANTAL                    
336801                 MOVE 'DAM'           TO FILC2-AVVIKELSETYP               
336901                 PERFORM S32-SKAPA-DIFF-TRANS                             
337001              END-IF                                                      
337101***           IF  SEND-DCS-CDC                                            
337201***           AND DCS-FLINLREP = JA                                       
337301                 MOVE W-KVAVIS        TO FILC3-KVAVIS                     
337401                 MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                    
337501                 MOVE 'DAM'           TO FILC3-AVVIKELSETYP               
337601                 MOVE 5               TO FILC3-KDSORT1                    
337701                 MOVE DCS-FLINLREP    TO FILC3-FLINLREP                   
337801                 PERFORM S33-SKAPA-LDC-TRANS                              
337901***           END-IF                                                      
338001           END-IF                                                         
338101        END-IF                                                            
338201        ADD W-KVANTMOT(INDX)       TO INL-KVANTMOT                        
338301        MOVE INL-KVANTMOT          TO W-KVANTMOT(INDX)                    
338401        ADD W-KVSKROT(INDX)        TO INL-KVART-SKROT                     
338501        IF DCS-CDC                                                        
338601           CONTINUE                                                       
338701        ELSE                                                              
338801           COMPUTE WS-OLD-KVLS = SLAG-KVLS - INL-KVANTMOT                 
338901           END-COMPUTE                                                    
339001        END-IF                                                            
339101     ELSE                                                                 
339201        IF DCS-CDC                                                        
339301           ADD W-KVANTMOT (INDX)   TO CLAG-KVLS                           
339401        ELSE                                                              
339501           MOVE SLAG-KVLS          TO WS-OLD-KVLS                         
339601           ADD W-KVANTMOT(INDX)    TO SLAG-KVLS                           
339701        END-IF                                                            
339801*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
339901*   ---FÖR ATT UPPDATERA I WDL9                                           
340001        IF W-KVANTMOT(INDX) NOT = 0                                       
340101           PERFORM S21-SALDOLOGG-DATA                                     
340201           MOVE W-SPAR-IDDC        TO LOGG-IDDC                           
340301           IF DCS-CDC                                                     
340401              MOVE CLAG-KVEFRS     TO LOGG-KVEFRS                         
340501              MOVE CLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                      
340601              MOVE CLAG-KVLS       TO LOGG-KVLS                           
340701              COMPUTE LOGG-KVAKS    = CLAG-KVAKS-CDC                      
340801                                    + CLAG-KVAKS-T                        
340901           ELSE                                                           
341001              MOVE SLAG-KVEFRS     TO LOGG-KVEFRS                         
341101              MOVE SLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                      
341201              MOVE SLAG-KVLS       TO LOGG-KVLS                           
341301              MOVE SLAG-KVAKS-SDC  TO LOGG-KVAKS                          
341401           END-IF                                                         
341501           MOVE W-KVANTMOT(INDX)   TO LOGG-KVART-SALDO                    
341601           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                 
341701           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
341801           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
341901           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
342001           PERFORM S22-ISRT-SALDOLOGG                                     
342101        END-IF                                                            
342201*   ---SALDFÖRÄNDRINGAR SLUT                                              
342301        IF DCS-SDC                                                        
342401        OR DCS-NDC-PF                                                     
342501        OR (DCS-NDC-NA AND DIST35-NDCCN-NDCUS-REFILL)                     
342601           ADD W-KVSKROT (INDX)    TO SLAG-KVLS                           
342701        END-IF                                                            
342801        IF DCS-CDC                                                        
342901           ADD W-KVSKROT (INDX)    TO CLAG-KVLS                           
343001        END-IF                                                            
343101*  ---KOD FÖR SALDOUPPDATERING I WDL9                                     
343201        IF (W-KVSKROT(INDX) NOT = 0 AND DCS-SDC)                          
343301        OR (W-KVSKROT(INDX) NOT = 0 AND DCS-NDC-PF)                       
343401        OR (W-KVSKROT(INDX) NOT = 0 AND DIST35-RETUR)                     
343501           PERFORM S21-SALDOLOGG-DATA                                     
343601           MOVE W-SPAR-IDDC        TO LOGG-IDDC                           
343701           IF DIST35-RETUR                                                
343801             MOVE CLAG-KVEFRS      TO LOGG-KVEFRS                         
343901             MOVE CLAG-KVAKS-PAV   TO LOGG-KVAKS-PAV                      
344001             MOVE CLAG-KVLS        TO LOGG-KVLS                           
344101             COMPUTE LOGG-KVAKS    = CLAG-KVAKS-CDC                       
344201                                   + CLAG-KVAKS-T                         
344301           ELSE                                                           
344401             MOVE SLAG-KVEFRS      TO LOGG-KVEFRS                         
344500             MOVE SLAG-KVAKS-PAV   TO LOGG-KVAKS-PAV                      
344600             MOVE SLAG-KVLS        TO LOGG-KVLS                           
344700             MOVE SLAG-KVAKS-SDC   TO LOGG-KVAKS                          
344801           END-IF                                                         
344900           MOVE W-KVSKROT(INDX)    TO LOGG-KVART-SALDO                    
345000           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                 
345100           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
345200           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
345300           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
345400           PERFORM S22-ISRT-SALDOLOGG                                     
345500           IF SEND-DCS-DDC                                                
345600              MOVE W-KVSKROT(INDX) TO FILC2-KVANTAL                       
345700              MOVE 'DAM'           TO FILC2-AVVIKELSETYP                  
345800              PERFORM S32-SKAPA-DIFF-TRANS                                
345900           END-IF                                                         
346000***        IF SEND-DCS-CDC                                                
346100***        AND DCS-FLINLREP = JA                                          
346200              MOVE W-KVAVIS        TO FILC3-KVAVIS                        
346300              MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                       
346400              MOVE 'DAM'           TO FILC3-AVVIKELSETYP                  
346500              MOVE 5               TO FILC3-KDSORT1                       
346600              MOVE DCS-FLINLREP    TO FILC3-FLINLREP                      
346700              PERFORM S33-SKAPA-LDC-TRANS                                 
346800***        END-IF                                                         
346900        END-IF                                                            
347000*  ---SALDOFÖRÄNDRING SLUT                                                
347100                                                                          
347200        IF DCS-CDC                                                        
347300           SUBTRACT W-KVAVIS  FROM CLAG-KVAKS-CDC                         
347400        ELSE                                                              
347500           SUBTRACT W-KVAVIS  FROM SLAG-KVAKS-SDC                         
347600        END-IF                                                            
347700        MOVE W-KVANTMOT(INDX) TO INL-KVANTMOT                             
347800        MOVE W-KVSKROT (INDX) TO INL-KVART-SKROT                          
347900     END-IF                                                               
348000                                                                          
348100*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
348200*   ---FÖR ATT UPPDATERA I WDL9                                           
348300     IF W-KVAVIS        NOT = 0                                           
348400        PERFORM S21-SALDOLOGG-DATA                                        
348500        MOVE W-SPAR-IDDC        TO LOGG-IDDC                              
348600        IF DCS-CDC                                                        
348700           MOVE CLAG-KVEFRS     TO LOGG-KVEFRS                            
348800           MOVE CLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                         
348900           MOVE CLAG-KVLS       TO LOGG-KVLS                              
349000           COMPUTE LOGG-KVAKS    = CLAG-KVAKS-CDC                         
349100                                 + CLAG-KVAKS-T                           
349200        ELSE                                                              
349300           MOVE SLAG-KVEFRS     TO LOGG-KVEFRS                            
349400           MOVE SLAG-KVAKS-PAV  TO LOGG-KVAKS-PAV                         
349500           MOVE SLAG-KVLS       TO LOGG-KVLS                              
349600           MOVE SLAG-KVAKS-SDC  TO LOGG-KVAKS                             
349700        END-IF                                                            
349800        MOVE W-KVAVIS           TO LOGG-KVART-SALDO                       
349900        MOVE '-'                TO LOGG-IDTECKEN-KVAKS                    
350000        MOVE SPACE              TO LOGG-IDTECKEN-KVLS                     
350100        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV                
350200        MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                   
350300        PERFORM S22-ISRT-SALDOLOGG                                        
350400     END-IF                                                               
350500* ---SALDOFÖRÄNDRINGAR SLUT                                               
350600                                                                          
350700     IF W-ADLAGOMR (INDX) > ZERO                                          
350800        MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                         
350900        MOVE W-ADGANG (INDX)   TO W-ADGANG-LOCB                           
351000        MOVE W-ADPLATS (INDX)  TO W-ADPLATS-LOCB                          
351100        IF DCS-CDC                                                        
351200           MOVE W-ADLAGOMR (INDX) TO CLAG-ADLAGOMR                        
351300           MOVE W-ADGANG (INDX)   TO CLAG-ADGANG                          
351400           MOVE W-ADPLATS (INDX)  TO CLAG-ADPLATS                         
351500        ELSE                                                              
351600           MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                        
351700           MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                          
351800           MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                         
351900        END-IF                                                            
352000     END-IF                                                               
352100                                                                          
352200     ADD +1                TO W-KVRADER                                   
352300     MOVE 'R32'            TO INL-IDPTYP                                  
352400     IF MID-IDUSER-003 = ALL '+' OR SPACE                                 
352500        CONTINUE                                                          
352600     ELSE                                                                 
352700        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
352800     END-IF                                                               
352900     MOVE W-TIME-N         TO AKTUELL-TID                                 
353000     IF DCS-NDC-NA OR DCS-NDC-PF                                          
353100        PERFORM S18-FIXA-LOKALTID                                         
353200        MOVE MSGI-TILOKDAT  TO INL-TIINLINL                               
353300        MOVE MSGI-TILOKTID  TO INL-TIINLITI                               
353400     ELSE                                                                 
353500        MOVE W-DAGENS-DATUM TO INL-TIINLINL                               
353600        MOVE AKTUELL-TTMM   TO INL-TIINLITI                               
353700     END-IF                                                               
353800     IF DCS-CDC                                                           
353900        MOVE CLAG-ADLAGOMR TO INL-ADLAGOMR                                
354000        MOVE CLAG-ADGANG   TO INL-ADGANG                                  
354100        MOVE CLAG-ADPLATS  TO INL-ADPLATS                                 
354200        PERFORM IMS-REPL-WLARTC11                                         
354300     ELSE                                                                 
354400        MOVE SLAG-ADLAGOMR TO INL-ADLAGOMR                                
354500        MOVE SLAG-ADGANG   TO INL-ADGANG                                  
354600        MOVE SLAG-ADPLATS  TO INL-ADPLATS                                 
354700        PERFORM IMS-REPL-WDK711                                           
354800     END-IF                                                               
354900                                                                          
355000     PERFORM IMS-REPL-WLINLC11                                            
355100                                                                          
355200     IF DCS-NDC-PF                                                        
355300        IF W-KVANTMOT(INDX) > 0                                           
355400           MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                      
355500           PERFORM S19-EV-RO-TACKNING                                     
355600        END-IF                                                            
355700     END-IF                                                               
355800                                                                          
355900     IF DCS-CDC                                                           
356000        IF W-KVANTMOT(INDX) > 0                                           
356100           MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                      
356200           PERFORM S19-EV-RO-TACKNING-CDC                                 
356300        END-IF                                                            
356400     END-IF                                                               
356500                                                                          
356600     IF DCS-NDC-NA                                                        
356700        PERFORM S09-SKAPA-EKOTRANS-A03                                    
356800        MOVE SPACE            TO EKOTRA03-KDANMORS                        
356900        MOVE SLAG-PRAVCOST    TO EKOTRA03-PRAVCOST-OLD                    
357000        MOVE W-KVANTMOT(INDX) TO AVG-KVANTMOT                             
357100                                 EKOTRA03-KVANTMOT                        
357200        MOVE W-KVSKROT(INDX)  TO EKOTRA03-KVSKROT                         
357300                                                                          
357400        PERFORM IMS-GET-WDK711                                            
357500        MOVE 'TVA'  TO STEXT                                              
357600        PERFORM S10-OMRAKN-MEDELPRIS-NA                                   
357700        MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                          
357800                                   EKOTRA03-PRAVCOST                      
357900        MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                          
358000        MOVE AVG-REMARKUP       TO EKOTRA03-REMARKUP                      
358100                                                                          
358200        IF W-KVANTMOT(INDX) > 0                                           
358300           MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                      
358400           PERFORM S19-EV-RO-TACKNING                                     
358500           PERFORM IMS-REPL-WDK711                                        
358600        ELSE                                                              
358700           MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST                
358800        END-IF                                                            
358900                                                                          
359000                                                                          
359100        PERFORM HBC-NDC-KOLLI-DAMAGED                                     
359200                                                                          
359300        MOVE W-KVAVIS        TO FILC3-KVAVIS                              
359400        MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                             
359500        MOVE 'DAM'           TO FILC3-AVVIKELSETYP                        
359600        MOVE 5               TO FILC3-KDSORT1                             
359700        MOVE DCS-FLINLREP    TO FILC3-FLINLREP                            
359800        PERFORM S33-SKAPA-LDC-TRANS                                       
359900                                                                          
360000     END-IF                                                               
360100                                                                          
360201     IF DCS-CDC                                                           
360301       IF DIST35-RETUR                                                    
360401         CONTINUE                                                         
360501       ELSE                                                               
360601         PERFORM HBC-NDC-KOLLI-DAMAGED                                    
360701       END-IF                                                             
360801     END-IF                                                               
360901                                                                          
361001     IF (W-KVANTMOT (INDX) + W-KVSKROT (INDX))                            
361101                           NOT = W-KVAVIS                                 
361201        ADD W-KVANTMOT (INDX) W-KVSKROT (INDX)                            
361301                           GIVING W-TEMP-KVANT                            
361401        MOVE ZERO TO W-DIFF-KVANT                                         
361501        COMPUTE W-DIFF-KVANT = W-KVAVIS                                   
361601                               - W-TEMP-KVANT                             
361701        END-COMPUTE                                                       
361801                                                                          
361901        IF DCS-SDC                                                        
362001        OR DCS-NDC-PF                                                     
362101        OR DIST35-RETUR                                                   
362201           IF SEND-DCS-CDC OR SEND-DCS-DDC                                
362301              IF SEND-DCS-CDC                                             
362401                PERFORM IMS-GHU-WLARTC11                                  
362501                MOVE CLAG-KVLS    TO WS-KVLS                              
362601                COMPUTE CLAG-KVLS = CLAG-KVLS                             
362701                                  + (W-KVAVIS                             
362801                                  -  W-KVANTMOT (INDX)                    
362901                                  -  W-KVSKROT  (INDX))                   
363001                END-COMPUTE                                               
363101                PERFORM IMS-REPL-WLARTC11                                 
363201                PERFORM HBA-SALDOLOGG-DATA                                
363301                PERFORM S07-SKAPA-HISTORIK                                
363401                MOVE WS-SAP-IDDISTR TO FILC4-IDDISTR                      
363501                PERFORM S12-SKAPA-BILLIT-TRANS                            
363601              ELSE                                                        
363701                IF SEND-DCS-DDC                                           
363801                   MOVE W-DIFF-KVANT       TO WS-DIFF-KVANT               
363901                   MOVE WS-DIFF-KVANT      TO FILC2-KVANTAL               
364001                   IF W-KVAVIS < W-TEMP-KVANT                             
364101                      MOVE 'ÖVERLEV.'      TO FILC2-AVVIKELSETYP          
364201                   ELSE                                                   
364301                      MOVE 'UNDERLEV.'     TO FILC2-AVVIKELSETYP          
364401                   END-IF                                                 
364501                   PERFORM S32-SKAPA-DIFF-TRANS                           
364601                END-IF                                                    
364701              END-IF                                                      
364801              PERFORM S31-SKAPA-SAP-TRANS-VCCS                            
364901           ELSE                                                           
365001              IF SEND-DCS-NDC-PF OR SEND-DCS-SDC                          
365101                 MOVE W-IDDC TO WS-SPARAT-IDDC                            
365201                 MOVE SEND-WS-IDDC TO W-IDDC                              
365301                 PERFORM IMS-GET-WDK711                                   
365401                 MOVE SLAG-KVLS    TO WS-KVLS                             
365501                 COMPUTE SLAG-KVLS = SLAG-KVLS                            
365601                                   + (W-KVAVIS                            
365701                                   -  W-KVANTMOT (INDX)                   
365801                                   -  W-KVSKROT  (INDX))                  
365901                 END-COMPUTE                                              
366001                 PERFORM IMS-REPL-WDK711                                  
366101                 MOVE WS-SPARAT-IDDC TO W-IDDC                            
366201                 PERFORM HBB-SALDOLOGG-DATA                               
366301                 PERFORM S37-SKAPA-NDC-HIST-SANDANDE                      
366401                 PERFORM S31-SKAPA-SAP-TRANS-VCCS                         
366501             END-IF                                                       
366601          END-IF                                                          
366701        ELSE                                                              
366801           IF DCS-NDC-NA                                                  
366901             PERFORM S17-NDC-OOVER-UNDER                                  
367001             MOVE W-IDARTNR-NYCKEL-SPAR TO W-IDARTNR                      
367101             MOVE W-DAINLEV-NYCKEL-SPAR TO W-DAINLEV                      
367201             MOVE W-IDDC-NYCKEL-SPAR    TO W-IDDC                         
367301             PERFORM IMS-GHU-WLINLC11                                     
367401**** BOUNCE INVOICE HAS TO HAVE A DIFFERENT BOOKING FOR THIS              
367501             IF DIST35-NDCCN-NDCUS-REFILL                                 
367601               IF SEND-DCS-CDC                                            
367701                 MOVE INL-PRARTNTO        TO R8-EKH-PRARTNTO              
367801                 PERFORM S31-SKAPA-SAP-TRANS                              
367901               END-IF                                                     
368001             ELSE                                                         
368101**** THE LAB BOOKING DOESNT NEED A EXTRA BOOKING                          
368201               CONTINUE                                                   
368301             END-IF                                                       
368401           ELSE                                                           
368501             IF DCS-CDC                                                   
368601               IF SEND-DCS-NDC-CN OR SEND-DCS-USA                         
368701*-REFILL FRÅN EXPORT TILL CDC.UT PÅ RAPPORT W41841-001.                   
368801                                                                          
368901                 IF W-KVAVIS < W-TEMP-KVANT                               
369001                   MOVE '11'         TO 6308-KDANMORS                     
369101                 ELSE                                                     
369201                   MOVE '00'         TO 6308-KDANMORS                     
369301                 END-IF                                                   
369401                 MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                     
369501                 MOVE WS-DIFF-KVANT  TO 6308-KVLEVANM                     
369601                 PERFORM S11-SKAPA-LEVANM-TRANS                           
369701                                                                          
369801                 PERFORM S31-SKAPA-SAP-TRANS-VCCS                         
369901               END-IF                                                     
370001             END-IF                                                       
370101           END-IF                                                         
370201        END-IF                                                            
370301                                                                          
370401        MOVE W-KVAVIS       TO FILC3-KVAVIS                               
370501        MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                              
370601        MOVE WS-DIFF-KVANT  TO FILC3-KVANTAL                              
370701        IF W-KVAVIS < W-TEMP-KVANT                                        
370801           MOVE 'ÖVERLEV.'  TO FILC3-AVVIKELSETYP                         
370901           MOVE 3           TO FILC3-KDSORT1                              
371001        ELSE                                                              
371101           MOVE 'UNDERLEV.' TO FILC3-AVVIKELSETYP                         
371201           MOVE 4           TO FILC3-KDSORT1                              
371301        END-IF                                                            
371401        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
371501        PERFORM S33-SKAPA-LDC-TRANS                                       
371601                                                                          
371701     END-IF                                                               
371801                                                                          
371901     IF DCS-SDC                                                           
372001     OR DCS-NDC-PF                                                        
372101     OR DCS-CDC                                                           
372201        MOVE NEJ TO NY-SKROT-SW                                           
372301        PERFORM S99-SKAPA-SKROT-ORDER                                     
372401     END-IF                                                               
372501                                                                          
372601     PERFORM S08-UPPDATERA-6301                                           
372701     .                                                                    
372801     EJECT                                                                
372901 HB-KOLLI-DAMAGED         SECTION.                                        
373001     MOVE ZERO               TO W-KVRADER                                 
373101                                                                          
373201     MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                            
373301     MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                            
373401     MOVE W-SPAR-IDFAKT      TO W-SEQA-IDFAKT-MIN                         
373501                                W-SEQA-IDFAKT-MAX                         
373601     MOVE W-SPAR-IDKUNDRF    TO W-SEQA-IDKUNDRF-MIN                       
373701                                W-SEQA-IDKUNDRF-MAX                       
373801     MOVE W-SPAR-IDKUNDNR    TO W-SEQA-IDKUNDNR-MIN                       
373901                                W-SEQA-IDKUNDNR-MAX                       
374001     MOVE W-SPAR-IDKOLLI     TO W-SEQA-IDKOLLI-MIN                        
374101                                W-SEQA-IDKOLLI-MAX                        
374201     MOVE '310'              TO W-IDPTYP                                  
374301     MOVE MID-IDARTNR (INDX) TO W-SEQA-IDARTNR-MIN                        
374401                                 W-SEQA-IDARTNR-MAX                       
374501     PERFORM IMS-GU-WLINLD01                                              
374601                                                                          
374701     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
374801                          W-IDARTNR-NYCKEL-SPAR                           
374901     MOVE SEQA-DAINLEV TO W-DAINLEV                                       
375001                          W-DAINLEV-NYCKEL-SPAR                           
375101     MOVE SEQA-IDDC    TO W-IDDC                                          
375201                          W-IDDC-NYCKEL-SPAR                              
375301     PERFORM IMS-GHU-WLINLC11                                             
375401     PERFORM IMS-GET-WDK711                                               
375501                                                                          
375601     MOVE INL-KVAVIS   TO W-KVAVIS                                        
375701     MOVE INL-KDFRAKT  TO W-KDFRAKT                                       
375801     MOVE INL-FLSKAKOL TO W-FLSKAKOL                                      
375901     MOVE INL-IDDISTR  TO WS-SAP-IDDISTR                                  
376001     MOVE INL-IDKUNDNR TO WS-SAP-IDKUNDNR                                 
376101                                                                          
376201     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
376301        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
376401        PERFORM IMS-GU-WDB601-SEND                                        
376501     END-IF                                                               
376601                                                                          
376701     IF INL-KVANTMOT > ZERO                                               
376801        ADD W-KVANTMOT(INDX)       TO SLAG-KVLS                           
376901        SUBTRACT W-KVANTMOT(INDX)  FROM SLAG-KVAKS-SDC                    
377001*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
377101*   ---FÖR ATT UPPDATERA I WDL9                                           
377201        IF W-KVANTMOT(INDX) NOT = 0                                       
377301           PERFORM S21-SALDOLOGG-DATA                                     
377401           MOVE W-SPAR-IDDC          TO LOGG-IDDC                         
377501           MOVE SLAG-KVLS          TO LOGG-KVLS                           
377601           MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                          
377701           MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                         
377801           MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
377901           MOVE W-KVANTMOT(INDX)   TO LOGG-KVART-SALDO                    
378001           MOVE '-'                TO LOGG-IDTECKEN-KVAKS                 
378101           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
378201           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
378301           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
378401           PERFORM S22-ISRT-SALDOLOGG                                     
378501        END-IF                                                            
378601        SUBTRACT W-KVSKROT (INDX)  FROM SLAG-KVAKS-SDC                    
378701*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
378801*   ---FÖR ATT UPPDATERA I WDL9                                           
378901        IF W-KVSKROT(INDX) NOT = 0                                        
379001           PERFORM S21-SALDOLOGG-DATA                                     
379101           MOVE W-SPAR-IDDC          TO LOGG-IDDC                         
379201           MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                         
379301           MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
379401           MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                          
379501           MOVE SLAG-KVLS          TO LOGG-KVLS                           
379601           MOVE W-KVSKROT(INDX)    TO LOGG-KVART-SALDO                    
379701           MOVE '-'                TO LOGG-IDTECKEN-KVAKS                 
379801           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
379901           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
380001           MOVE SPACE              TO LOGG-IDTECKEN-KVLS                  
380101           PERFORM S22-ISRT-SALDOLOGG                                     
380201           IF SEND-DCS-CDC                                                
380301              MOVE W-KVAVIS        TO FILC3-KVAVIS                        
380401              MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                       
380501              MOVE 'DAM'           TO FILC3-AVVIKELSETYP                  
380601              MOVE 5               TO FILC3-KDSORT1                       
380701              MOVE DCS-FLINLREP    TO FILC3-FLINLREP                      
380801              PERFORM S33-SKAPA-LDC-TRANS                                 
380901           END-IF                                                         
381001        END-IF                                                            
381101        ADD W-KVANTMOT(INDX)       TO INL-KVANTMOT                        
381201        MOVE INL-KVANTMOT          TO W-KVANTMOT(INDX)                    
381301        ADD W-KVSKROT(INDX)        TO INL-KVART-SKROT                     
381401        COMPUTE WS-OLD-KVLS = SLAG-KVLS - INL-KVANTMOT                    
381501        END-COMPUTE                                                       
381601        MOVE SLAG-KVEFRS      TO WS-OLD-KVEFRS                            
381701     ELSE                                                                 
381801        MOVE SLAG-KVLS        TO WS-OLD-KVLS                              
381901        MOVE SLAG-KVEFRS      TO WS-OLD-KVEFRS                            
382001        ADD W-KVANTMOT(INDX)  TO SLAG-KVLS                                
382101*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
382201*   ---FÖR ATT UPPDATERA I WDL9                                           
382301        IF W-KVANTMOT(INDX) NOT = 0                                       
382401           PERFORM S21-SALDOLOGG-DATA                                     
382501           MOVE W-SPAR-IDDC          TO LOGG-IDDC                         
382601           MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                         
382701           MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
382801           MOVE SLAG-KVLS          TO LOGG-KVLS                           
382901           MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                          
383001           MOVE W-KVANTMOT(INDX)   TO LOGG-KVART-SALDO                    
383101           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                 
383201           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
383301           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
383401           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
383501           PERFORM S22-ISRT-SALDOLOGG                                     
383601        END-IF                                                            
383701*   ---SALDFÖRÄNDRINGAR SLUT                                              
383801*  ---KOD FÖR SALDOUPPDATERING I WDL9                                     
383901        IF (W-KVSKROT(INDX) NOT = 0) AND SEND-DCS-CDC                     
384001           MOVE W-KVAVIS        TO FILC3-KVAVIS                           
385001           MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                          
386001           MOVE 'DAM'           TO FILC3-AVVIKELSETYP                     
386101           MOVE 5               TO FILC3-KDSORT1                          
386201           MOVE DCS-FLINLREP    TO FILC3-FLINLREP                         
386301           PERFORM S33-SKAPA-LDC-TRANS                                    
386401        END-IF                                                            
386501*  ---SALDOFÖRÄNDRING SLUT                                                
386601                                                                          
386701        SUBTRACT W-KVAVIS     FROM SLAG-KVAKS-SDC                         
386801        MOVE W-KVANTMOT(INDX) TO INL-KVANTMOT                             
386901        MOVE W-KVSKROT (INDX) TO INL-KVART-SKROT                          
387001     END-IF                                                               
387101                                                                          
387201*   ---NEDANSTÅENDE IF-SATS ANVÄNDS FÖR                                   
387301*   ---FÖR ATT UPPDATERA I WDL9                                           
387401     IF W-KVAVIS        NOT = 0                                           
387501        PERFORM S21-SALDOLOGG-DATA                                        
387601        MOVE W-SPAR-IDDC          TO LOGG-IDDC                            
387701        MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                            
387801        MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                         
387901        MOVE SLAG-KVLS          TO LOGG-KVLS                              
388001        MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                             
388101        MOVE W-KVAVIS           TO LOGG-KVART-SALDO                       
388201        MOVE '-'                TO LOGG-IDTECKEN-KVAKS                    
388301        MOVE SPACE              TO LOGG-IDTECKEN-KVLS                     
388401        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV                
388501        MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                   
388601        PERFORM S22-ISRT-SALDOLOGG                                        
388701     END-IF                                                               
388801* ---SALDOFÖRÄNDRINGAR SLUT                                               
388901                                                                          
389001     IF W-ADLAGOMR (INDX) > ZERO                                          
389101        MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                           
389201        MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                         
389301        MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                             
389401        MOVE W-ADGANG (INDX)   TO W-ADGANG-LOCB                           
389501        MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                            
389601        MOVE W-ADPLATS (INDX)  TO W-ADPLATS-LOCB                          
389701     END-IF                                                               
389801                                                                          
389901     ADD +1                TO W-KVRADER                                   
390001     MOVE 'R32'            TO INL-IDPTYP                                  
390101     IF MID-IDUSER-003 = ALL '+' OR SPACE                                 
390201        CONTINUE                                                          
390301     ELSE                                                                 
390401        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
390501     END-IF                                                               
390601     MOVE W-TIME-N         TO AKTUELL-TID                                 
390701     PERFORM S18-FIXA-LOKALTID                                            
390801     MOVE MSGI-TILOKDAT  TO INL-TIINLINL                                  
390901     MOVE MSGI-TILOKTID  TO INL-TIINLITI                                  
391001                                                                          
391101     MOVE SLAG-ADLAGOMR    TO INL-ADLAGOMR                                
391201     MOVE SLAG-ADGANG      TO INL-ADGANG                                  
391301     MOVE SLAG-ADPLATS     TO INL-ADPLATS                                 
391401                                                                          
391501     PERFORM IMS-GU-WLARTC01                                              
391601     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
391602     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
391701     PERFORM IMS-GHNP-WLARTC11                                            
391801     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
391901     MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                          
392001                                                                          
392101*  BERÄKNING AVGCOST                                                      
392201     MOVE W-KVANTMOT(INDX)   TO AVG-KVANTMOT                              
392301     PERFORM S10-OMRAKN-MEDELPRIS                                         
392401     MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                             
392501     MOVE MSGI-TILOKDAT      TO SLAG-TIAVCOST                             
392601                                                                          
392701     PERFORM IMS-REPL-WLINLC11                                            
392801     PERFORM IMS-REPL-WDK711                                              
392901                                                                          
393001     IF SEND-DCS-CDC                                                      
393101        IF (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                         
393201           IF W-KVANTMOT(INDX) > 0                                        
393301              MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                   
393401              PERFORM S19-EV-RO-TACKNING                                  
393501              PERFORM IMS-REPL-WDK711                                     
393601           END-IF                                                         
393701        END-IF                                                            
393801                                                                          
393901        IF W-KVSKROT(INDX) > 0                                            
394001          MOVE INL-PRARTNTO    TO R8-EKH-PRARTNTO                         
394101          PERFORM S31-SKAPA-SAP-TRANS-DAM                                 
394201        END-IF                                                            
394301                                                                          
394401        PERFORM HBC-NDC-KOLLI-DAMAGED                                     
394501                                                                          
394601        MOVE W-KVAVIS        TO FILC3-KVAVIS                              
394701        MOVE W-KVSKROT(INDX) TO FILC3-KVANTAL                             
394801        MOVE 'DAM'           TO FILC3-AVVIKELSETYP                        
394901        MOVE 5               TO FILC3-KDSORT1                             
395001        MOVE DCS-FLINLREP    TO FILC3-FLINLREP                            
395101        PERFORM S33-SKAPA-LDC-TRANS                                       
395201     END-IF                                                               
395301                                                                          
395401     IF (W-KVANTMOT (INDX) + W-KVSKROT (INDX))                            
395501                           NOT = W-KVAVIS                                 
395601        ADD W-KVANTMOT (INDX) W-KVSKROT (INDX)                            
395701                           GIVING W-TEMP-KVANT                            
395801        MOVE ZERO TO W-DIFF-KVANT                                         
395901        COMPUTE W-DIFF-KVANT = W-KVAVIS                                   
396001                               - W-TEMP-KVANT                             
396101        END-COMPUTE                                                       
396201                                                                          
396301        PERFORM S17-NDC-OOVER-UNDER                                       
396401        MOVE W-IDARTNR-NYCKEL-SPAR TO W-IDARTNR                           
396501        MOVE W-DAINLEV-NYCKEL-SPAR TO W-DAINLEV                           
396601        MOVE W-IDDC-NYCKEL-SPAR    TO W-IDDC                              
396701        PERFORM IMS-GHU-WLINLC11                                          
396801        MOVE INL-PRARTNTO          TO R8-EKH-PRARTNTO                     
396901        PERFORM S31-SKAPA-SAP-TRANS                                       
397001                                                                          
397101        MOVE W-KVAVIS       TO FILC3-KVAVIS                               
397201        MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                              
397301        MOVE WS-DIFF-KVANT  TO FILC3-KVANTAL                              
397401        IF W-KVAVIS < W-TEMP-KVANT                                        
397501           MOVE 'ÖVERLEV.'  TO FILC3-AVVIKELSETYP                         
397601           MOVE 3           TO FILC3-KDSORT1                              
397701        ELSE                                                              
397801           MOVE 'UNDERLEV.' TO FILC3-AVVIKELSETYP                         
397901           MOVE 4           TO FILC3-KDSORT1                              
398001        END-IF                                                            
398101        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
398201        PERFORM S33-SKAPA-LDC-TRANS                                       
398301                                                                          
398401     END-IF                                                               
398501                                                                          
398601     PERFORM S08-UPPDATERA-6301                                           
398701     .                                                                    
398801     EJECT                                                                
398901                                                                          
399001 HBA-SALDOLOGG-DATA SECTION.                                              
399101     PERFORM S21-SALDOLOGG-DATA                                           
399201     MOVE WC-CDC-SE        TO LOGG-IDDC                                   
399301     MOVE 'MISC'              TO LOGG-IDHUVTYP                            
399401     MOVE 'R34'               TO LOGG-IDSUBTYP                            
399501     MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                              
399601     MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
399701     MOVE CLAG-KVLS           TO LOGG-KVLS                                
399801     COMPUTE LOGG-KVAKS       =  CLAG-KVAKS-CDC                           
399901                              +  CLAG-KVAKS-T                             
400001     COMPUTE LOGG-KVART-SALDO =  W-KVAVIS                                 
400101                              -  W-KVANTMOT(INDX)                         
400201                              -  W-KVSKROT(INDX)                          
400301     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
400401     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
400501     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
400601     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
400701                                                                          
400801     PERFORM S22-ISRT-SALDOLOGG                                           
400901     .                                                                    
401001     EJECT                                                                
401101 HBB-SALDOLOGG-DATA SECTION.                                              
401201     PERFORM S21-SALDOLOGG-DATA                                           
401301     MOVE SEND-WS-IDDC        TO LOGG-IDDC                                
401401     MOVE 'MISC'              TO LOGG-IDHUVTYP                            
401501     MOVE 'R34'               TO LOGG-IDSUBTYP                            
401601     MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                              
401701     MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
401801     MOVE SLAG-KVLS           TO LOGG-KVLS                                
401901     MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                               
402001     COMPUTE LOGG-KVART-SALDO =  W-KVAVIS                                 
402101                              -  W-KVANTMOT(INDX)                         
402201                              -  W-KVSKROT(INDX)                          
402301     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
402401     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
402501     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
402601     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
402701                                                                          
402801     PERFORM S22-ISRT-SALDOLOGG                                           
402901     .                                                                    
403001     EJECT                                                                
403101 HBC-NDC-KOLLI-DAMAGED SECTION.                                           
403201                                                                          
403301     MOVE NEJ TO WS-FLYGORDER                                             
403401                 WS-BAATORDER                                             
403501                                                                          
403601     MOVE 6302-IDDC-SEND TO W-IDDC-WDB3                                   
403701                            W-IDDC-WDB3-DEF                               
403801     MOVE INL-IDDISTR    TO W-IDDISTR-WDB3                                
403901                            W-IDDISTR-WDB3-DEF                            
404001     MOVE 6302-IDKUNDNR  TO W-IDKUNDNR-WDB3                               
404101                                                                          
404201     PERFORM IMS-GU-WDB301                                                
404301     IF SEGMENT-FINNS                                                     
404401        IF W-KDFRAKT = DC-KDGENFRA-VOR                                    
404501           MOVE JA TO WS-FLYGORDER                                        
404601        ELSE                                                              
404701           IF W-KDFRAKT = DC-KDGENFRA-MO                                  
404801              MOVE JA TO WS-BAATORDER                                     
404901           END-IF                                                         
405001        END-IF                                                            
405101     END-IF                                                               
405201                                                                          
405301     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
405401        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
405501        PERFORM IMS-GU-WDB601-SEND                                        
405601     END-IF                                                               
405701                                                                          
405801     IF SEND-DCS-CDC OR SEND-DCS-DDC OR                                   
405901        DIST35-NONVCC-CDC-REFILL  OR DIST35-NDCCN-NDCUS-REFILL            
406001                                                                          
406101        IF DIST35-NONVCC-CDC-REFILL OR DIST35-NDCCN-NDCUS-REFILL          
406201*---- DISTR 9111 OCH 9211 SKALL ALLTID HA KOD 43/63.                      
406301          IF W-FLSKAKOL = 'J'                                             
406401             MOVE '63' TO 6308-KDANMORS                                   
406501          ELSE                                                            
406601             MOVE '43' TO 6308-KDANMORS                                   
406701          END-IF                                                          
406801          MOVE W-KVSKROT(INDX) TO 6308-KVLEVANM                           
406901          PERFORM S11-SKAPA-LEVANM-TRANS                                  
407001        ELSE                                                              
407101          IF WS-FLYGORDER = NEJ                                           
407201             IF W-FLSKAKOL = 'J'                                          
407301                MOVE '63' TO 6308-KDANMORS                                
407401             ELSE                                                         
407501                MOVE '43' TO 6308-KDANMORS                                
407601             END-IF                                                       
407701             MOVE W-KVSKROT(INDX) TO 6308-KVLEVANM                        
407801             PERFORM S11-SKAPA-LEVANM-TRANS                               
407901          ELSE                                                            
408001             IF W-FLSKAKOL = 'N'                                          
408101                MOVE '43'            TO 6308-KDANMORS                     
408201                MOVE W-KVSKROT(INDX) TO 6308-KVLEVANM                     
408301                PERFORM S11-SKAPA-LEVANM-TRANS                            
408401             END-IF                                                       
408501          END-IF                                                          
408601        END-IF                                                            
408701        IF W-FLSKAKOL = 'J'                                               
408801           MOVE '63' TO EKOTRA03-KDANMORS                                 
408901        ELSE                                                              
409001           MOVE '43' TO EKOTRA03-KDANMORS                                 
409101        END-IF                                                            
409201     ELSE                                                                 
409301                                                                          
409401        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
409501           AND (DCS-NDC-NA AND DCS-CANADA)                                
409601        OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                          
409701           AND (DCS-NDC-NA AND DCS-USA)                                   
409801           IF WS-FLYGORDER = NEJ                                          
409901              MOVE '42'            TO 6308-KDANMORS                       
410001              MOVE W-KVSKROT(INDX) TO 6308-KVLEVANM                       
410101              PERFORM S11-SKAPA-LEVANM-TRANS                              
410201           END-IF                                                         
410301           MOVE '42'               TO EKOTRA03-KDANMORS                   
410401        ELSE                                                              
410501           IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                          
410601           AND (DCS-NDC-NA AND DCS-USA)                                   
410701              MOVE '42'            TO EKOTRA03-KDANMORS                   
410801           END-IF                                                         
410901        END-IF                                                            
411001     END-IF                                                               
411101     .                                                                    
411201     EJECT                                                                
411301 HC-OOVER-UNDER-LEVERANS-PV  SECTION.                                     
411401                                                                          
411501     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
411601        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
411701        PERFORM IMS-GU-WDB601-SEND                                        
411801     END-IF                                                               
411901                                                                          
412001     MOVE ZERO               TO W-KVRADER                                 
412101                                                                          
412201     MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                            
412301     MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                            
412401     MOVE W-SPAR-IDFAKT      TO W-SEQA-IDFAKT-MIN                         
412501                                W-SEQA-IDFAKT-MAX                         
412601     MOVE W-SPAR-IDKUNDRF    TO W-SEQA-IDKUNDRF-MIN                       
412701                                W-SEQA-IDKUNDRF-MAX                       
412801     MOVE W-SPAR-IDKUNDNR    TO W-SEQA-IDKUNDNR-MIN                       
412901                                W-SEQA-IDKUNDNR-MAX                       
413001     MOVE W-SPAR-IDKOLLI     TO W-SEQA-IDKOLLI-MIN                        
413101                                W-SEQA-IDKOLLI-MAX                        
413201     MOVE '310'              TO W-IDPTYP                                  
413301     MOVE MID-IDARTNR (INDX) TO W-SEQA-IDARTNR-MIN                        
413401                                W-SEQA-IDARTNR-MAX                        
413501     PERFORM IMS-GU-WLINLD01                                              
413601                                                                          
413701     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
413801                          W-IDARTNR-NYCKEL-SPAR                           
413901     MOVE SEQA-DAINLEV TO W-DAINLEV                                       
414001                          W-DAINLEV-NYCKEL-SPAR                           
414101     MOVE SEQA-IDDC    TO W-IDDC                                          
414201                          W-IDDC-NYCKEL-SPAR                              
414301     PERFORM IMS-GHU-WLINLC11                                             
414401     IF DCS-CDC                                                           
414501        CONTINUE                                                          
414601     ELSE                                                                 
414701        PERFORM IMS-GET-WDK711                                            
414801     END-IF                                                               
414901                                                                          
415001     PERFORM IMS-GU-WLARTC01                                              
415101     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
415102     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
415201     PERFORM IMS-GHNP-WLARTC11                                            
415301     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
415401     MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                          
415501                                                                          
415601     MOVE INL-KVAVIS   TO W-KVAVIS                                        
415701     MOVE INL-KDFRAKT  TO W-KDFRAKT                                       
415801     MOVE INL-IDDISTR  TO WS-SAP-IDDISTR                                  
415901     MOVE INL-IDKUNDNR TO WS-SAP-IDKUNDNR                                 
416001                                                                          
416101     IF W-ADLAGOMR (INDX) > ZERO                                          
416201        IF DCS-CDC                                                        
416301           MOVE W-ADLAGOMR (INDX) TO CLAG-ADLAGOMR                        
416401           MOVE W-ADGANG (INDX)   TO CLAG-ADGANG                          
416501           MOVE W-ADPLATS (INDX)  TO CLAG-ADPLATS                         
416601        ELSE                                                              
416701           MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                        
416801           MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                          
416901           MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                         
417001        END-IF                                                            
417101        MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                         
417201        MOVE W-ADGANG (INDX)   TO W-ADGANG-LOCB                           
417301        MOVE W-ADPLATS (INDX)  TO W-ADPLATS-LOCB                          
417401        PERFORM S30-UPPDATERA-WDJ9                                        
417501     END-IF                                                               
417601                                                                          
417701                                                                          
417801     IF INL-KVANTMOT > ZERO                                               
417901        IF DCS-CDC                                                        
418001           SUBTRACT W-KVANTMOT(INDX)                                      
418101                                FROM CLAG-KVAKS-CDC                       
418201           ADD W-KVANTMOT(INDX)   TO CLAG-KVLS                            
418301        ELSE                                                              
418401           SUBTRACT W-KVANTMOT(INDX)                                      
418501                                FROM SLAG-KVAKS-SDC                       
418601           ADD W-KVANTMOT(INDX)   TO SLAG-KVLS                            
418701           COMPUTE WS-OLD-KVLS     = SLAG-KVLS - INL-KVANTMOT             
418801           END-COMPUTE                                                    
418901        END-IF                                                            
419001        MOVE W-KVANTMOT(INDX)     TO LOGG-KVART-SALDO                     
419101        ADD W-KVANTMOT(INDX)      TO INL-KVANTMOT                         
419201        MOVE INL-KVANTMOT         TO W-KVANTMOT(INDX)                     
419301*   ---KOD SOM LOGGAR SALDOFÖRÄNDRINGAR PÅ WDL9                           
419401        PERFORM S21-SALDOLOGG-DATA                                        
419501        MOVE W-SPAR-IDDC          TO LOGG-IDDC                            
419601        IF DCS-CDC                                                        
419701           MOVE CLAG-KVLS         TO LOGG-KVLS                            
419801           COMPUTE LOGG-KVAKS      = CLAG-KVAKS-CDC                       
419901                                   + CLAG-KVAKS-T                         
420001           MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                          
420101           MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
420201        ELSE                                                              
420301           MOVE SLAG-KVLS         TO LOGG-KVLS                            
420401           MOVE SLAG-KVAKS-SDC    TO LOGG-KVAKS                           
420501           MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                          
420601           MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
420701        END-IF                                                            
420801        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
420901        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
421001        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
421101        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
421201        PERFORM S22-ISRT-SALDOLOGG                                        
421301*   ---SALDOFÖRÄNDRINGAR SLUT                                             
421401     ELSE                                                                 
421501        MOVE W-KVANTMOT(INDX)      TO INL-KVANTMOT                        
421601        IF DCS-CDC                                                        
421701           ADD W-KVANTMOT(INDX)    TO CLAG-KVLS                           
421801        ELSE                                                              
421901           MOVE SLAG-KVLS          TO WS-OLD-KVLS                         
422001           ADD W-KVANTMOT(INDX)    TO SLAG-KVLS                           
422101        END-IF                                                            
422201*   ---KOD SOM LOGGAR SALDOFÖRÄNDRINGAR PÅ WDL9                           
422301        PERFORM S21-SALDOLOGG-DATA                                        
422401        MOVE W-SPAR-IDDC          TO LOGG-IDDC                            
422501        MOVE W-KVANTMOT(INDX)     TO LOGG-KVART-SALDO                     
422601        IF DCS-CDC                                                        
422701           MOVE CLAG-KVLS         TO LOGG-KVLS                            
422801           COMPUTE LOGG-KVAKS      = CLAG-KVAKS-CDC                       
422901                                   + CLAG-KVAKS-T                         
423001           MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                          
423101           MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
423201        ELSE                                                              
423301           MOVE SLAG-KVLS         TO LOGG-KVLS                            
423401           MOVE SLAG-KVAKS-SDC    TO LOGG-KVAKS                           
423501           MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                          
423601           MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
423701        END-IF                                                            
423801        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
423901        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
424001        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
424101        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                  
424201        PERFORM S22-ISRT-SALDOLOGG                                        
424301*   ---SALDOFÖRÄNDRINGAR SLUT                                             
424401                                                                          
424501        IF DCS-CDC                                                        
424601           SUBTRACT W-KVAVIS       FROM CLAG-KVAKS-CDC                    
424701        ELSE                                                              
424801           SUBTRACT W-KVAVIS       FROM SLAG-KVAKS-SDC                    
424901        END-IF                                                            
425001                                                                          
425101*   ---KOD SOM LOGGAR SALDOFÖRÄNDRINGAR PÅ WDL9                           
425201        PERFORM S21-SALDOLOGG-DATA                                        
425301        MOVE W-SPAR-IDDC          TO LOGG-IDDC                            
425401        MOVE W-KVAVIS             TO LOGG-KVART-SALDO                     
425501        IF DCS-CDC                                                        
425601           MOVE CLAG-KVLS         TO LOGG-KVLS                            
425701           COMPUTE LOGG-KVAKS      = CLAG-KVAKS-CDC                       
425801                                   + CLAG-KVAKS-T                         
425901           MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                          
426001           MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
426101        ELSE                                                              
426201           MOVE SLAG-KVLS         TO LOGG-KVLS                            
426301           MOVE SLAG-KVAKS-SDC    TO LOGG-KVAKS                           
426401           MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                          
426501           MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                       
426601        END-IF                                                            
426701        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
426801        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
426901        MOVE SPACE                TO LOGG-IDTECKEN-KVLS                   
427001        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
427101        PERFORM S22-ISRT-SALDOLOGG                                        
427201*   ---SALDOFÖRÄNDRINGAR SLUT                                             
427301     END-IF                                                               
427401                                                                          
427501     ADD +1                TO W-KVRADER                                   
427601     MOVE 'R32'            TO INL-IDPTYP                                  
427701     IF MID-IDUSER-003 = ALL '+' OR SPACE                                 
427801        CONTINUE                                                          
427901     ELSE                                                                 
428001        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
428101     END-IF                                                               
428201     MOVE W-TIME-N         TO AKTUELL-TID                                 
428301     IF DCS-NDC-NA OR DCS-NDC-PF                                          
428401        PERFORM S18-FIXA-LOKALTID                                         
428501        MOVE MSGI-TILOKDAT  TO INL-TIINLINL                               
428601        MOVE MSGI-TILOKTID  TO INL-TIINLITI                               
428701     ELSE                                                                 
428801        MOVE W-DAGENS-DATUM TO INL-TIINLINL                               
428901        MOVE AKTUELL-TTMM   TO INL-TIINLITI                               
429001     END-IF                                                               
429101     IF DCS-CDC                                                           
429201        MOVE CLAG-ADLAGOMR TO INL-ADLAGOMR                                
429301        MOVE CLAG-ADGANG   TO INL-ADGANG                                  
429401        MOVE CLAG-ADPLATS  TO INL-ADPLATS                                 
429501     ELSE                                                                 
429601        MOVE SLAG-ADLAGOMR TO INL-ADLAGOMR                                
429701        MOVE SLAG-ADGANG   TO INL-ADGANG                                  
429801        MOVE SLAG-ADPLATS  TO INL-ADPLATS                                 
429901     END-IF                                                               
430001                                                                          
430101     PERFORM IMS-REPL-WLINLC11                                            
430201                                                                          
430301     IF DCS-NDC-PF                                                        
430401        MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                         
430501        PERFORM S19-EV-RO-TACKNING                                        
430601     END-IF                                                               
430701     IF DCS-CDC                                                           
430801        MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                         
430901        PERFORM S19-EV-RO-TACKNING-CDC                                    
431001     END-IF                                                               
431101     IF DCS-NDC-NA                                                        
431201        PERFORM S09-SKAPA-EKOTRANS-A03                                    
431301        MOVE SPACE              TO EKOTRA03-KDANMORS                      
431401        MOVE SLAG-PRAVCOST      TO EKOTRA03-PRAVCOST-OLD                  
431501        MOVE W-KVANTMOT(INDX)   TO AVG-KVANTMOT                           
431601                                   EKOTRA03-KVANTMOT                      
431701        MOVE 'TRE'  TO STEXT                                              
431801        PERFORM S10-OMRAKN-MEDELPRIS-NA                                   
431901        IF W-KVANTMOT(INDX) > 0                                           
432001           MOVE AVG-PRAVCOST-NEW      TO SLAG-PRAVCOST                    
432101                                         EKOTRA03-PRAVCOST                
432201           MOVE MSGI-TILOKDAT         TO SLAG-TIAVCOST                    
432301        ELSE                                                              
432401           MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST                
432501        END-IF                                                            
432601        MOVE AVG-REMARKUP       TO EKOTRA03-REMARKUP                      
432701        MOVE ZERO               TO EKOTRA03-KVSKROT                       
432801                                                                          
432901        MOVE W-KVANTMOT(INDX)   TO WS-RO-KVANTMOT                         
433001        PERFORM S19-EV-RO-TACKNING                                        
433101     END-IF                                                               
433201     IF DCS-CDC                                                           
433301        PERFORM IMS-REPL-WLARTC11                                         
433401     ELSE                                                                 
433501        PERFORM IMS-REPL-WDK711                                           
433601     END-IF                                                               
433701                                                                          
433801     IF W-KVANTMOT (INDX) NOT = W-KVAVIS                                  
433901        MOVE W-KVANTMOT (INDX) TO W-TEMP-KVANT                            
434001        MOVE ZERO TO W-DIFF-KVANT                                         
434101        COMPUTE W-DIFF-KVANT = W-KVAVIS                                   
434201                               - W-TEMP-KVANT                             
434301        END-COMPUTE                                                       
434401                                                                          
434501*-REFILL FRÅN EXPORT-DC TILL CDC.UT PÅ RAPPORT W41841-001.                
434601        IF DCS-CDC                                                        
434701          IF DIST35-RETUR                                                 
434801            CONTINUE                                                      
434901          ELSE                                                            
435001            IF W-KVAVIS < W-TEMP-KVANT                                    
435101              MOVE '11' TO 6308-KDANMORS                                  
435201            ELSE                                                          
435301              MOVE '00' TO 6308-KDANMORS                                  
435401            END-IF                                                        
435501          END-IF                                                          
435601          MOVE W-DIFF-KVANT TO 6308-KVLEVANM                              
435701          PERFORM S11-SKAPA-LEVANM-TRANS                                  
435801        END-IF                                                            
435901                                                                          
436001        IF DCS-SDC                                                        
436101        OR DCS-NDC-PF                                                     
436201        OR DIST35-RETUR                                                   
436301           IF SEND-DCS-CDC OR SEND-DCS-DDC                                
436401              IF SEND-DCS-CDC                                             
436501                 PERFORM IMS-GHU-WLARTC11                                 
436601                 MOVE CLAG-KVLS    TO WS-KVLS                             
436701                 COMPUTE CLAG-KVLS = CLAG-KVLS                            
436801                                   + (W-KVAVIS                            
436901                                   -  W-KVANTMOT (INDX))                  
437001                 END-COMPUTE                                              
437101                 PERFORM IMS-REPL-WLARTC11                                
437201                 PERFORM HCA-SALDOLOGG-DATA                               
437301                 PERFORM S07-SKAPA-HISTORIK                               
437401                 MOVE WS-SAP-IDDISTR TO FILC4-IDDISTR                     
437501                 PERFORM S12-SKAPA-BILLIT-TRANS                           
437601              ELSE                                                        
437701                IF SEND-DCS-DDC                                           
437801                   MOVE W-DIFF-KVANT       TO WS-DIFF-KVANT               
437901                   MOVE WS-DIFF-KVANT      TO FILC2-KVANTAL               
438001                   IF W-KVAVIS < W-TEMP-KVANT                             
438101                      MOVE 'ÖVERLEV.'      TO FILC2-AVVIKELSETYP          
438201                   ELSE                                                   
438301                      MOVE 'UNDERLEV.'     TO FILC2-AVVIKELSETYP          
438401                   END-IF                                                 
438501                   PERFORM S32-SKAPA-DIFF-TRANS                           
438601                 END-IF                                                   
438701              END-IF                                                      
438801              PERFORM S31-SKAPA-SAP-TRANS-VCCS                            
438901           ELSE                                                           
439001              IF SEND-DCS-NDC-PF OR SEND-DCS-SDC                          
439101                 MOVE W-IDDC TO WS-SPARAT-IDDC                            
439201                 MOVE SEND-WS-IDDC TO W-IDDC                              
439301                 PERFORM IMS-GET-WDK711                                   
439401                 MOVE SLAG-KVLS    TO WS-KVLS                             
439501                 COMPUTE SLAG-KVLS = SLAG-KVLS                            
439601                                   + (W-KVAVIS                            
439701                                   -  W-KVANTMOT (INDX))                  
439801                 END-COMPUTE                                              
439901                 PERFORM IMS-REPL-WDK711                                  
440001                 MOVE WS-SPARAT-IDDC TO W-IDDC                            
440101                 PERFORM HCB-SALDOLOGG-DATA                               
440201                 PERFORM S37-SKAPA-NDC-HIST-SANDANDE                      
440301                 PERFORM S31-SKAPA-SAP-TRANS-VCCS                         
440401              END-IF                                                      
440501          END-IF                                                          
440601        ELSE                                                              
440701           IF DCS-NDC-NA                                                  
440801              MOVE NEJ TO WS-A03-SKAPAD                                   
440901              PERFORM S17-NDC-OOVER-UNDER                                 
441001              MOVE W-IDARTNR-NYCKEL-SPAR TO W-IDARTNR                     
441101              MOVE W-DAINLEV-NYCKEL-SPAR TO W-DAINLEV                     
441201              MOVE W-IDDC-NYCKEL-SPAR    TO W-IDDC                        
441301              PERFORM IMS-GHU-WLINLC11                                    
441401              PERFORM S09-SKAPA-EKOTRANS-A03                              
441501           ELSE                                                           
441601             IF DCS-CDC                                                   
441701               IF SEND-DCS-NDC-CN OR SEND-DCS-USA                         
441801                 PERFORM S31-SKAPA-SAP-TRANS-VCCS                         
441901               END-IF                                                     
442001             END-IF                                                       
442101           END-IF                                                         
442201        END-IF                                                            
442301                                                                          
442401        MOVE W-KVAVIS       TO FILC3-KVAVIS                               
442501        MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                              
442601        MOVE WS-DIFF-KVANT  TO FILC3-KVANTAL                              
442701        IF W-KVAVIS < W-TEMP-KVANT                                        
442801           MOVE 'ÖVERLEV.'  TO FILC3-AVVIKELSETYP                         
442901           MOVE 3           TO FILC3-KDSORT1                              
443001        ELSE                                                              
443101           MOVE 'UNDERLEV.' TO FILC3-AVVIKELSETYP                         
443201           MOVE 4           TO FILC3-KDSORT1                              
443301        END-IF                                                            
443401        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
443501        PERFORM S33-SKAPA-LDC-TRANS                                       
443601     END-IF                                                               
443701                                                                          
443801     PERFORM S08-UPPDATERA-6301                                           
443901     .                                                                    
444001     EJECT                                                                
444101 HC-OOVER-UNDER-LEVERANS SECTION.                                         
444201     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
444301        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
444401        PERFORM IMS-GU-WDB601-SEND                                        
444501     END-IF                                                               
444601                                                                          
444701     MOVE ZERO               TO W-KVRADER                                 
444801                                                                          
444901     MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                            
445001     MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                            
445101     MOVE W-SPAR-IDFAKT      TO W-SEQA-IDFAKT-MIN                         
445201                                W-SEQA-IDFAKT-MAX                         
445301     MOVE W-SPAR-IDKUNDRF    TO W-SEQA-IDKUNDRF-MIN                       
445401                                W-SEQA-IDKUNDRF-MAX                       
445501     MOVE W-SPAR-IDKUNDNR    TO W-SEQA-IDKUNDNR-MIN                       
445601                                W-SEQA-IDKUNDNR-MAX                       
445701     MOVE W-SPAR-IDKOLLI     TO W-SEQA-IDKOLLI-MIN                        
445801                                W-SEQA-IDKOLLI-MAX                        
445901     MOVE '310'              TO W-IDPTYP                                  
446001     MOVE MID-IDARTNR (INDX) TO W-SEQA-IDARTNR-MIN                        
446101                                 W-SEQA-IDARTNR-MAX                       
446201     PERFORM IMS-GU-WLINLD01                                              
446301                                                                          
446401     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
446501                          W-IDARTNR-NYCKEL-SPAR                           
446601     MOVE SEQA-DAINLEV TO W-DAINLEV                                       
446701                          W-DAINLEV-NYCKEL-SPAR                           
446801     MOVE SEQA-IDDC    TO W-IDDC                                          
446901                          W-IDDC-NYCKEL-SPAR                              
447001     PERFORM IMS-GHU-WLINLC11                                             
447101     PERFORM IMS-GET-WDK711                                               
447201                                                                          
447301     MOVE INL-KVAVIS   TO W-KVAVIS                                        
447401     MOVE INL-KDFRAKT  TO W-KDFRAKT                                       
447501     MOVE INL-IDDISTR  TO WS-SAP-IDDISTR                                  
447601     MOVE INL-IDKUNDNR TO WS-SAP-IDKUNDNR                                 
447701                                                                          
447801     IF W-ADLAGOMR (INDX) > ZERO                                          
447901        MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                           
448001        MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                         
448101        MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                             
448201        MOVE W-ADGANG (INDX)   TO W-ADGANG-LOCB                           
448301        MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                            
448401        MOVE W-ADPLATS (INDX)  TO W-ADPLATS-LOCB                          
448501        PERFORM S30-UPPDATERA-WDJ9                                        
448601     END-IF                                                               
448701                                                                          
448801     IF INL-KVANTMOT > ZERO                                               
448901        SUBTRACT W-KVANTMOT(INDX)  FROM SLAG-KVAKS-SDC                    
449001        ADD W-KVANTMOT(INDX)      TO SLAG-KVLS                            
449101        MOVE W-KVANTMOT(INDX)     TO LOGG-KVART-SALDO                     
449201        ADD W-KVANTMOT(INDX)      TO INL-KVANTMOT                         
449301        MOVE INL-KVANTMOT         TO W-KVANTMOT(INDX)                     
449401        COMPUTE WS-OLD-KVLS =  SLAG-KVLS - INL-KVANTMOT                   
449501        END-COMPUTE                                                       
449601        MOVE SLAG-KVEFRS           TO WS-OLD-KVEFRS                       
449701*   ---KOD SOM LOGGAR SALDOFÖRÄNDRINGAR PÅ WDL9                           
449801        PERFORM S21-SALDOLOGG-DATA                                        
449901        MOVE W-SPAR-IDDC          TO LOGG-IDDC                            
450001        MOVE SLAG-KVLS            TO LOGG-KVLS                            
450101        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
450201        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
450301        MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                       
450401        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
450501        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
450601        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
450701        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
450801        PERFORM S22-ISRT-SALDOLOGG                                        
450901*   ---SALDOFÖRÄNDRINGAR SLUT                                             
451001     ELSE                                                                 
451101        MOVE SLAG-KVLS             TO WS-OLD-KVLS                         
451201        MOVE SLAG-KVEFRS           TO WS-OLD-KVEFRS                       
451301        MOVE W-KVANTMOT(INDX)      TO INL-KVANTMOT                        
451401        ADD W-KVANTMOT(INDX)       TO SLAG-KVLS                           
451501*   ---KOD SOM LOGGAR SALDOFÖRÄNDRINGAR PÅ WDL9                           
451601        PERFORM S21-SALDOLOGG-DATA                                        
451701        MOVE W-SPAR-IDDC          TO LOGG-IDDC                            
451801        MOVE W-KVANTMOT(INDX)     TO LOGG-KVART-SALDO                     
451901        MOVE SLAG-KVLS            TO LOGG-KVLS                            
452001        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
452101        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
452201        MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                       
452301        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
452401        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
452501        MOVE '+'                  TO LOGG-IDTECKEN-KVLS                   
452601        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                  
452701        PERFORM S22-ISRT-SALDOLOGG                                        
452801*   ---SALDOFÖRÄNDRINGAR SLUT                                             
452901                                                                          
453001        SUBTRACT W-KVAVIS          FROM SLAG-KVAKS-SDC                    
453101                                                                          
453201*   ---KOD SOM LOGGAR SALDOFÖRÄNDRINGAR PÅ WDL9                           
453301        PERFORM S21-SALDOLOGG-DATA                                        
453401        MOVE W-SPAR-IDDC          TO LOGG-IDDC                            
453501        MOVE W-KVAVIS             TO LOGG-KVART-SALDO                     
453601        MOVE SLAG-KVLS            TO LOGG-KVLS                            
453701        MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                           
453801        MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                          
453901        MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                       
454001        MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV              
454101        MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                 
454201        MOVE SPACE                TO LOGG-IDTECKEN-KVLS                   
454301        MOVE '-'                  TO LOGG-IDTECKEN-KVAKS                  
454401        PERFORM S22-ISRT-SALDOLOGG                                        
454501*   ---SALDOFÖRÄNDRINGAR SLUT                                             
454601     END-IF                                                               
454701                                                                          
454801     ADD +1                TO W-KVRADER                                   
454901     MOVE 'R32'            TO INL-IDPTYP                                  
455001     IF MID-IDUSER-003 = ALL '+' OR SPACE                                 
455101        CONTINUE                                                          
455201     ELSE                                                                 
455301        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
455401     END-IF                                                               
455501     MOVE W-TIME-N         TO AKTUELL-TID                                 
455601     PERFORM S18-FIXA-LOKALTID                                            
455701     MOVE MSGI-TILOKDAT  TO INL-TIINLINL                                  
455801     MOVE MSGI-TILOKTID  TO INL-TIINLITI                                  
455901                                                                          
456001     MOVE SLAG-ADLAGOMR    TO INL-ADLAGOMR                                
456101     MOVE SLAG-ADGANG      TO INL-ADGANG                                  
456201     MOVE SLAG-ADPLATS     TO INL-ADPLATS                                 
456301                                                                          
456401     PERFORM IMS-REPL-WLINLC11                                            
456501                                                                          
456601     PERFORM IMS-GU-WLARTC01                                              
456701     MOVE K6-ART-KDPRODSL     TO W-KDPRODSL                               
456702     MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                               
456801     PERFORM IMS-GHNP-WLARTC11                                            
456901     MOVE CLAG-KDPSLLOC       TO W-KDPRODSL-LOC                           
457001     MOVE CLAG-PRARTSTD       TO WS-SAP-PRARTSTD                          
457101                                                                          
457201     IF SEND-DCS-CDC                                                      
457301     AND (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                           
457401                                                                          
457501        MOVE W-KVANTMOT(INDX)      TO WS-RO-KVANTMOT                      
457601        PERFORM S19-EV-RO-TACKNING                                        
457701     END-IF                                                               
457801                                                                          
457901* BERÄKNING AVGCOST                                                       
458001     MOVE W-KVANTMOT(INDX)   TO AVG-KVANTMOT                              
458101     PERFORM S10-OMRAKN-MEDELPRIS                                         
458201     IF W-KVANTMOT(INDX) > 0                                              
458301       MOVE AVG-PRAVCOST-NEW      TO SLAG-PRAVCOST                        
458401       MOVE MSGI-TILOKDAT         TO SLAG-TIAVCOST                        
458501     END-IF                                                               
458601                                                                          
458701     PERFORM IMS-REPL-WDK711                                              
458801                                                                          
458901     IF W-KVANTMOT (INDX) NOT = W-KVAVIS                                  
459001        MOVE W-KVANTMOT (INDX) TO W-TEMP-KVANT                            
459101        MOVE ZERO TO W-DIFF-KVANT                                         
459201        COMPUTE W-DIFF-KVANT = W-KVAVIS                                   
459301                               - W-TEMP-KVANT                             
459401        END-COMPUTE                                                       
459501                                                                          
459601        PERFORM S17-NDC-OOVER-UNDER                                       
459701        MOVE W-IDARTNR-NYCKEL-SPAR TO W-IDARTNR                           
459801        MOVE W-DAINLEV-NYCKEL-SPAR TO W-DAINLEV                           
459901        MOVE W-IDDC-NYCKEL-SPAR    TO W-IDDC                              
460001        PERFORM IMS-GHU-WLINLC11                                          
460101        MOVE INL-PRARTNTO          TO R8-EKH-PRARTNTO                     
460201        PERFORM S31-SKAPA-SAP-TRANS                                       
460301                                                                          
460401        MOVE W-KVAVIS       TO FILC3-KVAVIS                               
460501        MOVE W-DIFF-KVANT   TO WS-DIFF-KVANT                              
460601        MOVE WS-DIFF-KVANT  TO FILC3-KVANTAL                              
460701        IF W-KVAVIS < W-TEMP-KVANT                                        
460801           MOVE 'ÖVERLEV.'  TO FILC3-AVVIKELSETYP                         
460901           MOVE 3           TO FILC3-KDSORT1                              
461001        ELSE                                                              
461101           MOVE 'UNDERLEV.' TO FILC3-AVVIKELSETYP                         
461201           MOVE 4           TO FILC3-KDSORT1                              
461301        END-IF                                                            
461401        MOVE DCS-FLINLREP   TO FILC3-FLINLREP                             
461501        PERFORM S33-SKAPA-LDC-TRANS                                       
461601     END-IF                                                               
461701                                                                          
461801     PERFORM S08-UPPDATERA-6301                                           
461901     .                                                                    
462001     EJECT                                                                
462101                                                                          
462201 HCA-SALDOLOGG-DATA SECTION.                                              
462301     PERFORM S21-SALDOLOGG-DATA                                           
462401     MOVE WC-CDC-SE        TO LOGG-IDDC                                   
462501     MOVE 'MISC'              TO LOGG-IDHUVTYP                            
462601     MOVE 'R34'               TO LOGG-IDSUBTYP                            
462701     MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                              
462801     MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
462901     MOVE CLAG-KVLS           TO LOGG-KVLS                                
463001     COMPUTE LOGG-KVAKS       =  CLAG-KVAKS-CDC                           
463101                              +  CLAG-KVAKS-T                             
463201     COMPUTE LOGG-KVART-SALDO =  W-KVAVIS                                 
463301                              -  W-KVANTMOT(INDX)                         
463401     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
463501     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
463601     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
463701     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
463801                                                                          
463901     PERFORM S22-ISRT-SALDOLOGG                                           
464001     .                                                                    
464101     EJECT                                                                
464201 HCB-SALDOLOGG-DATA SECTION.                                              
464301     PERFORM S21-SALDOLOGG-DATA                                           
464401     MOVE SEND-WS-IDDC        TO LOGG-IDDC                                
464501     MOVE 'MISC'              TO LOGG-IDHUVTYP                            
464601     MOVE 'R34'               TO LOGG-IDSUBTYP                            
464701     MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                              
464801     MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
464901     MOVE SLAG-KVLS           TO LOGG-KVLS                                
465001     MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                               
465101     COMPUTE LOGG-KVART-SALDO =  W-KVAVIS                                 
465201                              -  W-KVANTMOT(INDX)                         
465301     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
465401     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
465501     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
465601     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
465701                                                                          
465801     PERFORM S22-ISRT-SALDOLOGG                                           
465901     .                                                                    
466001     EJECT                                                                
466101 HD-NYUPPLAEGG-PV SECTION.                                                
466201                                                                          
466301     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
466401        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
466501        PERFORM IMS-GU-WDB601-SEND                                        
466601     END-IF                                                               
466701                                                                          
466801     MOVE ZERO            TO W-KVRADER                                    
466901     ADD +1               TO W-KVRADER                                    
467001     MOVE JA              TO NYUPPLAEGG-SW                                
467101     MOVE W-IDARTNR-INM   TO W-IDARTNR                                    
467201     MOVE W-SPAR-IDDC     TO W-IDDC                                       
467301     IF DCS-CDC                                                           
467401        PERFORM IMS-GHU-WLARTC11                                          
467501        MOVE CLAG-ADLAGOMR   TO W-ADLAGOMR-SPAR                           
467601        MOVE CLAG-ADGANG     TO W-ADGANG-SPAR                             
467701        MOVE CLAG-ADPLATS    TO W-ADPLATS-SPAR                            
467801     ELSE                                                                 
467901        PERFORM IMS-GET-WDK711                                            
468001        MOVE SLAG-ADLAGOMR   TO W-ADLAGOMR-SPAR                           
468101        MOVE SLAG-ADGANG     TO W-ADGANG-SPAR                             
468201        MOVE SLAG-ADPLATS    TO W-ADPLATS-SPAR                            
468301     END-IF                                                               
468401                                                                          
468501     IF MID-ADLAGOMR-INM = ALL '+' OR SPACE                               
468601        CONTINUE                                                          
468701     ELSE                                                                 
468801        MOVE W-ADLAGOMR-INM  TO W-ADLAGOMR-SPAR                           
468901                                W-ADLAGOMR-LOCB                           
469001     END-IF                                                               
469101     IF MID-ADGANG-INM = ALL '+' OR SPACE                                 
469201        CONTINUE                                                          
469301     ELSE                                                                 
469401        MOVE W-ADGANG-INM    TO W-ADGANG-SPAR                             
469501                                W-ADGANG-LOCB                             
469601     END-IF                                                               
469701     IF MID-ADPLATS-INM = ALL '+' OR SPACE                                
469801        CONTINUE                                                          
469901     ELSE                                                                 
470001        MOVE W-ADPLATS-INM   TO W-ADPLATS-SPAR                            
470101                                W-ADPLATS-LOCB                            
470201     END-IF                                                               
470301                                                                          
470401     IF DCS-CDC                                                           
470501        MOVE W-ADLAGOMR-SPAR TO CLAG-ADLAGOMR                             
470601        MOVE W-ADGANG-SPAR   TO CLAG-ADGANG                               
470701        MOVE W-ADPLATS-SPAR  TO CLAG-ADPLATS                              
470801     ELSE                                                                 
470901        MOVE W-ADLAGOMR-SPAR TO SLAG-ADLAGOMR                             
471001        MOVE W-ADGANG-SPAR   TO SLAG-ADGANG                               
471101        MOVE W-ADPLATS-SPAR  TO SLAG-ADPLATS                              
471201     END-IF                                                               
471301                                                                          
471401     IF W-ADLAGOMR-LOCB NUMERIC                                           
471501     AND W-ADGANG-LOCB NUMERIC                                            
471601     AND W-ADPLATS-LOCB NUMERIC                                           
471701        IF W-ADLAGOMR-LOCB > ZERO                                         
471801        OR W-ADGANG-LOCB > ZERO                                           
471901        OR W-ADPLATS-LOCB > ZERO                                          
472001           PERFORM S30-UPPDATERA-WDJ9                                     
472101        END-IF                                                            
472201     END-IF                                                               
472301                                                                          
472401     MOVE WS-KVANTMOT-INM    TO WS-KVANTMOT-INM-NUM                       
472501     MOVE WS-KVSKROT-INM     TO WS-KVSKROT-INM-NUM                        
472601     COMPUTE WS-SUMMA-KVANT = (WS-KVANTMOT-INM-NUM                        
472701             + WS-KVSKROT-INM-NUM)                                        
472801     END-COMPUTE                                                          
472901                                                                          
473001     IF DCS-CDC                                                           
473101        ADD WS-SUMMA-KVANT   TO CLAG-KVLS                                 
473201     ELSE                                                                 
473301        MOVE SLAG-KVLS       TO WS-OLD-KVLS                               
473401        ADD WS-SUMMA-KVANT   TO SLAG-KVLS                                 
473501     END-IF                                                               
473601* ---NEDANSTÅENDE KOD LOGGAR                                              
473701* ---SALDOFÖRÄNDRINGAR PÅ WDL9                                            
473801     PERFORM S21-SALDOLOGG-DATA                                           
473901     MOVE W-SPAR-IDDC          TO LOGG-IDDC                               
474001     MOVE WS-SUMMA-KVANT       TO LOGG-KVART-SALDO                        
474101     IF DCS-CDC                                                           
474201        MOVE CLAG-KVLS         TO LOGG-KVLS                               
474301        COMPUTE LOGG-KVAKS      = CLAG-KVAKS-CDC                          
474401                                + CLAG-KVAKS-T                            
474501        MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                             
474601        MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                          
474701     ELSE                                                                 
474801        MOVE SLAG-KVLS         TO LOGG-KVLS                               
474901        MOVE SLAG-KVAKS-SDC    TO LOGG-KVAKS                              
475001        MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                             
475101        MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                          
475201     END-IF                                                               
475301     MOVE '+'                  TO LOGG-IDTECKEN-KVLS                      
475401     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                     
475501     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV                 
475601     MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                    
475701     PERFORM S22-ISRT-SALDOLOGG                                           
475801* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
475901                                                                          
476001     IF DCS-NDC-NA                                                        
476101        SUBTRACT WS-KVSKROT-INM-NUM FROM SLAG-KVLS                        
476201        MOVE WS-KVANTMOT-INM-NUM TO WS-RO-KVANTMOT                        
476301        PERFORM S19-EV-RO-TACKNING                                        
476401* ---NEDANSTÅENDE KOD LOGGAR                                              
476501* ---SALDOFÖRÄNDRINGAR PÅ WDL9                                            
476601        PERFORM S21-SALDOLOGG-DATA                                        
476701        MOVE W-SPAR-IDDC        TO LOGG-IDDC                              
476801        MOVE WS-KVSKROT-INM-NUM TO LOGG-KVART-SALDO                       
476901        MOVE SLAG-KVLS          TO LOGG-KVLS                              
477001        MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                             
477101        MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                            
477201        MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                         
477301        MOVE '-'                TO LOGG-IDTECKEN-KVLS                     
477401        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                    
477501        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV                
477601        MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                   
477701        PERFORM S22-ISRT-SALDOLOGG                                        
477801* ---SLUT SALDOFÖRÄNDRINGAR                                               
477901     END-IF                                                               
478001                                                                          
478101     IF DCS-CDC                                                           
478201        PERFORM IMS-REPL-WLARTC11                                         
478301     ELSE                                                                 
478401        PERFORM IMS-REPL-WDK711                                           
478501     END-IF                                                               
478601                                                                          
478701     MOVE WS-SUMMA-KVANT     TO W-TEMP-KVANT                              
478801     MOVE ZERO               TO W-KVAVIS                                  
478901                                                                          
479001     IF DCS-SDC                                                           
479101     OR DCS-NDC-PF                                                        
479201     OR DIST35-RETUR                                                      
479301        IF SEND-DCS-CDC                                                   
479401           PERFORM IMS-GU-WLARTC01                                        
479501           MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                          
479502           MOVE K6-ART-IDFKNGRP     TO W-IDFKNGRP                         
479601           PERFORM IMS-GHNP-WLARTC11                                      
479701           SUBTRACT WS-SUMMA-KVANT FROM CLAG-KVLS                         
479801           MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                      
479901           MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                     
480001           PERFORM IMS-REPL-WLARTC11                                      
480101**** ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                   
480201           PERFORM S21-SALDOLOGG-DATA                                     
480301           MOVE WC-CDC-SE          TO LOGG-IDDC                           
480401           MOVE 'MISC'             TO LOGG-IDHUVTYP                       
480501           MOVE 'R34'              TO LOGG-IDSUBTYP                       
480601           MOVE WS-SUMMA-KVANT     TO LOGG-KVART-SALDO                    
480701           MOVE CLAG-KVLS          TO LOGG-KVLS                           
480801           MOVE CLAG-KVAKS-CDC     TO LOGG-KVAKS                          
480901           MOVE CLAG-KVEFRS        TO LOGG-KVEFRS                         
481001           MOVE CLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                      
481101           MOVE '-'                TO LOGG-IDTECKEN-KVLS                  
481201           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                 
481301           MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV             
481401           MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                
481501           PERFORM S22-ISRT-SALDOLOGG                                     
481601           PERFORM S07-SKAPA-HISTORIK                                     
481701           MOVE 6302-IDDISTR TO FILC4-IDDISTR                             
481801           PERFORM S12-SKAPA-BILLIT-TRANS                                 
481901**** ---SLUT SALDOFÖRÄNDRINGAR                                            
482001        ELSE                                                              
482101           PERFORM IMS-GU-WLARTC01                                        
482201           MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                          
482202           MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                          
482301           PERFORM IMS-GHNP-WLARTC11                                      
482401           MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                      
482501           MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                     
482601           IF SEND-DCS-NDC-PF OR SEND-DCS-SDC                             
482701              MOVE W-IDDC TO WS-SPARAT-IDDC                               
482801              MOVE SEND-WS-IDDC TO W-IDDC                                 
482901              PERFORM IMS-GET-WDK711                                      
483001              SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                      
483101              PERFORM IMS-REPL-WDK711                                     
483201              MOVE WS-SPARAT-IDDC     TO W-IDDC                           
483301******* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                
483401              PERFORM S21-SALDOLOGG-DATA                                  
483501              MOVE SEND-WS-IDDC       TO LOGG-IDDC                        
483601              MOVE 'MISC'             TO LOGG-IDHUVTYP                    
483701              MOVE 'R34'              TO LOGG-IDSUBTYP                    
483801              MOVE WS-SUMMA-KVANT     TO LOGG-KVART-SALDO                 
483901              MOVE SLAG-KVLS          TO LOGG-KVLS                        
484001              MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                       
484101              MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                      
484201              MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                   
484301              MOVE '-'                TO LOGG-IDTECKEN-KVLS               
484401              MOVE SPACE              TO LOGG-IDTECKEN-KVAKS              
484501              MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV          
484601              MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS             
484701              PERFORM S22-ISRT-SALDOLOGG                                  
484801              PERFORM S37-SKAPA-NDC-HIST-SANDANDE                         
484901           END-IF                                                         
485001        END-IF                                                            
485101        IF SEND-DCS-DDC                                                   
485201           MOVE '1441 '    TO INL-IDLEVNR                                 
485301        ELSE                                                              
485401           MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                        
485501        END-IF                                                            
485601        MOVE ZERO          TO WS-FIXAD-PRARTNTO                           
485701        MOVE SPACE         TO WS-KDVALISO                                 
485801        PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                                
485901        MOVE 6302-IDDISTR     TO WS-SAP-IDDISTR                           
486001        MOVE W-SPAR-IDKUNDNR  TO WS-SAP-IDKUNDNR                          
486101        PERFORM S31-SKAPA-SAP-TRANS-VCCS                                  
486201                                                                          
486301        IF SEND-DCS-DDC                                                   
486401           MOVE WS-SUMMA-KVANT        TO FILC2-KVANTAL                    
486501           MOVE 'ÖVERLEV.'            TO FILC2-AVVIKELSETYP               
486601           PERFORM S32-SKAPA-DIFF-TRANS                                   
486701           IF WS-KVSKROT-INM-NUM > ZERO                                   
486801              MOVE WS-KVSKROT-INM-NUM TO FILC2-KVANTAL                    
486901              MOVE 'DAM'              TO FILC2-AVVIKELSETYP               
487001              PERFORM S32-SKAPA-DIFF-TRANS                                
487101           END-IF                                                         
487201        END-IF                                                            
487301                                                                          
487401***     IF  SEND-DCS-CDC                                                  
487501***     AND DCS-FLINLREP = JA                                             
487601           MOVE DCS-FLINLREP   TO FILC3-FLINLREP                          
487701           MOVE ZERO           TO FILC3-KVAVIS                            
487801           MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                           
487901           MOVE 'NY'           TO FILC3-AVVIKELSETYP                      
488001           MOVE 6              TO FILC3-KDSORT1                           
488101           PERFORM S33-SKAPA-LDC-TRANS                                    
488201           IF WS-KVSKROT-INM-NUM > ZERO                                   
488301              MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                    
488401              MOVE 'DAM'              TO FILC3-AVVIKELSETYP               
488501              MOVE 5                  TO FILC3-KDSORT1                    
488601              PERFORM S33-SKAPA-LDC-TRANS                                 
488701           END-IF                                                         
488801***     END-IF                                                            
488901                                                                          
489001        IF MID-CMD-INM = 'DAM'                                            
489101           MOVE JA TO NY-SKROT-SW                                         
489201           PERFORM S99-SKAPA-SKROT-ORDER                                  
489301        END-IF                                                            
489401                                                                          
489501     ELSE                                                                 
489601                                                                          
489701        IF DCS-NDC-NA                                                     
489801                                                                          
489901           MOVE DCS-FLINLREP   TO FILC3-FLINLREP                          
490001           MOVE ZERO           TO FILC3-KVAVIS                            
490101           MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                           
490201           MOVE 'NY'           TO FILC3-AVVIKELSETYP                      
490301           MOVE 6              TO FILC3-KDSORT1                           
490401           PERFORM S33-SKAPA-LDC-TRANS                                    
490501           IF WS-KVSKROT-INM-NUM > ZERO                                   
490601              MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                    
490701              MOVE 'DAM'              TO FILC3-AVVIKELSETYP               
490801              MOVE 5                  TO FILC3-KDSORT1                    
490901              PERFORM S33-SKAPA-LDC-TRANS                                 
491001           END-IF                                                         
491101                                                                          
491201           PERFORM IMS-GU-WLARTC01                                        
491301           MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                          
491302           MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                          
491401           PERFORM IMS-GHNP-WLARTC11                                      
491501           MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                      
491601           MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                     
491701                                                                          
491801           IF SEND-DCS-CDC OR SEND-DCS-DDC                                
491901              PERFORM S16-PRIS-TILLAMPNING                                
492001              MOVE PRIS-PRARTNTO       TO WS-FIXAD-PRARTNTO               
492101              IF DIST35-NDCCN-NDCUS-REFILL                                
492201                MOVE PRIS-PRARTNTO     TO R8-EKH-PRARTNTO                 
492301                PERFORM S31-SKAPA-SAP-TRANS                               
492401              END-IF                                                      
492501              MOVE 'SEK'               TO WS-KDVALISO                     
492601              IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                
492701                MOVE PRIS-KDVALISO     TO WS-KDVALISO                     
492801              END-IF                                                      
492901              IF SEND-DCS-DDC                                             
493001                 MOVE '1441 '    TO INL-IDLEVNR                           
493101              ELSE                                                        
493201                 MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                  
493301              END-IF                                                      
493401                                                                          
493501              PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                          
493601              PERFORM S09-SKAPA-EKOTRANS-A03                              
493701                                                                          
493801              PERFORM IMS-GET-WDK711                                      
493901              MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD           
494001              MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                    
494101                                          EKOTRA03-KVANTMOT               
494201              MOVE ZERO                TO EKOTRA03-KVSKROT                
494301              MOVE '21'                TO 6308-KDANMORS                   
494401                                          EKOTRA03-KDANMORS               
494501              MOVE WS-SUMMA-KVANT      TO 6308-KVLEVANM                   
494601                                                                          
494701              PERFORM S11-SKAPA-LEVANM-TRANS                              
494801                                                                          
494901              MOVE 'FYR'  TO STEXT                                        
495001              PERFORM S10-OMRAKN-MEDELPRIS-NA                             
495101              MOVE W-TIME-N         TO AKTUELL-TID                        
495201              PERFORM S18-FIXA-LOKALTID                                   
495301              MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                      
495401              MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST                  
495501                                       SLAG-PRAVCOST                      
495601              MOVE AVG-REMARKUP     TO EKOTRA03-REMARKUP                  
495701              MOVE AVG-PRKURS       TO EKOTRA03-PRKURS                    
495801                                                                          
495901              MOVE 6302-IDDISTR     TO WS-SAP-IDDISTR                     
496001              MOVE W-SPAR-IDKUNDNR  TO WS-SAP-IDKUNDNR                    
496101                                                                          
496201              IF WS-KVANTMOT-INM  > 0                                     
496301                 PERFORM IMS-REPL-WDK711                                  
496401              ELSE                                                        
496501                 MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST          
496601              END-IF                                                      
496701                                                                          
496801              IF MID-CMD-INM = 'DAM'                                      
496901                 MOVE '43'                TO 6308-KDANMORS                
497001                 MOVE WS-KVSKROT-INM-NUM  TO 6308-KVLEVANM                
497101                                             EKOTRA03-KVSKROT             
497201                 PERFORM S11-SKAPA-LEVANM-TRANS                           
497301              END-IF                                                      
497401           ELSE                                                           
497501                                                                          
497601             IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                        
497701                AND (DCS-NDC-NA AND DCS-CANADA)                           
497801             OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                     
497901                AND (DCS-NDC-NA AND DCS-USA)                              
498001                                                                          
498101                 MOVE SEND-WS-IDDC  TO W-IDDC                             
498201                 PERFORM IMS-GET-WDK711                                   
498301                 MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO                  
498401                 IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                   
498501                    MOVE 'CAD'      TO WS-KDVALISO                        
498601                 ELSE                                                     
498701                    MOVE 'USD'      TO WS-KDVALISO                        
498801                 END-IF                                                   
498901                                                                          
499001                 MOVE W-SPAR-IDDC TO W-IDDC                               
499101                 PERFORM IMS-GET-WDK711                                   
499201                 MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                  
499301                 PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                       
499401                                                                          
499501                 PERFORM S09-SKAPA-EKOTRANS-A03                           
499601                                                                          
499701                 MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD        
499801                 MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                 
499901                                             EKOTRA03-KVANTMOT            
500001                 MOVE ZERO                TO EKOTRA03-KVSKROT             
500101                 MOVE '21'                TO 6308-KDANMORS                
500201                                             EKOTRA03-KDANMORS            
500301                 MOVE WS-SUMMA-KVANT      TO 6308-KVLEVANM                
500401                 PERFORM S11-SKAPA-LEVANM-TRANS                           
500501                                                                          
500601                 MOVE 'FEM'  TO STEXT                                     
500701                 PERFORM S10-OMRAKN-MEDELPRIS-NA                          
500801                 MOVE W-TIME-N         TO AKTUELL-TID                     
500901                 PERFORM S18-FIXA-LOKALTID                                
501001                 MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                   
501101                 MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST               
501201                                          SLAG-PRAVCOST                   
501301                 MOVE AVG-REMARKUP     TO EKOTRA03-REMARKUP               
501401                 MOVE AVG-PRKURS       TO EKOTRA03-PRKURS                 
501501                                                                          
501601                 IF WS-KVANTMOT-INM  > 0                                  
501701                    PERFORM IMS-REPL-WDK711                               
501801                 ELSE                                                     
501901                    MOVE EKOTRA03-PRAVCOST-OLD                            
502001                                       TO EKOTRA03-PRAVCOST               
502101                 END-IF                                                   
502201                                                                          
502301                 IF MID-CMD-INM = 'DAM'                                   
502401                    MOVE '42'        TO 6308-KDANMORS                     
502501                    MOVE WS-KVSKROT-INM-NUM TO 6308-KVLEVANM              
502601                                               EKOTRA03-KVSKROT           
502701                    PERFORM S11-SKAPA-LEVANM-TRANS                        
502801                 END-IF                                                   
502901              ELSE                                                        
503001                                                                          
503101                 IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                    
503201                 AND (DCS-NDC-NA AND DCS-USA)                             
503301                                                                          
503401                    MOVE SEND-WS-IDDC TO W-IDDC                           
503501                    PERFORM IMS-GET-WDK711                                
503601                    SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                
503701                    MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO               
503801                    MOVE 'USD'         TO WS-KDVALISO                     
503901                    PERFORM IMS-REPL-WDK711                               
504001* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
504101                    PERFORM S21-SALDOLOGG-DATA                            
504201                    MOVE SEND-WS-IDDC   TO LOGG-IDDC                      
504301                    MOVE 'MISC'         TO LOGG-IDHUVTYP                  
504401                    MOVE 'R34'          TO LOGG-IDSUBTYP                  
504501                    MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO               
504601                    MOVE SLAG-KVLS      TO LOGG-KVLS                      
504701                    MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                     
504801                    MOVE SLAG-KVEFRS    TO LOGG-KVEFRS                    
504901                    MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                 
505001                    MOVE '-'            TO LOGG-IDTECKEN-KVLS             
505101                    MOVE SPACE          TO LOGG-IDTECKEN-KVAKS            
505201                    MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV        
505301                    MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS           
505401                    PERFORM S22-ISRT-SALDOLOGG                            
505501* ---SLUT SALDOFÖRÄNDRINGAR                                               
505601                                                                          
505701                    MOVE W-SPAR-IDDC TO W-IDDC                            
505801                    PERFORM IMS-GET-WDK711                                
505901                    MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR               
506001                    PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                    
506101                                                                          
506201                    PERFORM S09-SKAPA-EKOTRANS-A03                        
506301                    MOVE SLAG-PRAVCOST                                    
506401                                  TO EKOTRA03-PRAVCOST-OLD                
506501                    MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT              
506601                                                EKOTRA03-KVANTMOT         
506701                    MOVE ZERO                TO EKOTRA03-KVSKROT          
506801                    MOVE '21'                TO EKOTRA03-KDANMORS         
506901                                                                          
507001                    MOVE 'SEX'  TO STEXT                                  
507101                    PERFORM S10-OMRAKN-MEDELPRIS-NA                       
507201                                                                          
507301                    MOVE W-TIME-N         TO AKTUELL-TID                  
507401                    PERFORM S18-FIXA-LOKALTID                             
507501                    MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                
507601                    MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST            
507701                                             SLAG-PRAVCOST                
507801                    MOVE AVG-REMARKUP     TO EKOTRA03-REMARKUP            
507901                    MOVE AVG-PRKURS       TO EKOTRA03-PRKURS              
508001                    MOVE 1.0              TO EKOTRA03-PRKURS              
508101                                                                          
508201                    IF WS-KVANTMOT-INM  > 0                               
508301                       PERFORM IMS-REPL-WDK711                            
508401                    ELSE                                                  
508501                       MOVE EKOTRA03-PRAVCOST-OLD                         
508601                                          TO EKOTRA03-PRAVCOST            
508701                    END-IF                                                
508801                                                                          
508901                    MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR               
509001                    PERFORM S13-SKAPA-NDC-HIST-SANDANDE                   
509101                                                                          
509201                    IF MID-CMD-INM = 'DAM'                                
509301                      MOVE WS-KVSKROT-INM-NUM TO EKOTRA03-KVSKROT         
509401                    END-IF                                                
509501                 END-IF                                                   
509601              END-IF                                                      
509701           END-IF                                                         
509801        END-IF                                                            
509901     END-IF                                                               
510001                                                                          
510101*- REFILL FRÅN EXPORT TILL CDC (DISTR 9111,9211...).                      
510201     IF DCS-CDC                                                           
510301                                                                          
510401       IF SEND-DCS-NDC-CN OR SEND-DCS-USA                                 
510501         PERFORM IMS-GU-WLARTC01                                          
510601         MOVE K6-ART-KDPRODSL      TO W-KDPRODSL                          
510602         MOVE K6-ART-IDFKNGRP      TO W-IDFKNGRP                          
510701         PERFORM IMS-GHNP-WLARTC11                                        
510801         MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                        
510901         MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                       
511001         MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                          
511101         MOVE ZERO          TO WS-FIXAD-PRARTNTO                          
511201         MOVE SPACE         TO WS-KDVALISO                                
511301         PERFORM S16-PRIS-TILLAMPNING                                     
511401         PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                               
511501         MOVE 6302-IDDISTR     TO WS-SAP-IDDISTR                          
511601         MOVE W-SPAR-IDKUNDNR  TO WS-SAP-IDKUNDNR                         
511701         PERFORM S31-SKAPA-SAP-TRANS-VCCS                                 
511801                                                                          
511901         MOVE '21'             TO 6308-KDANMORS                           
512001         MOVE WS-SUMMA-KVANT   TO 6308-KVLEVANM                           
512101                                                                          
512201         PERFORM S11-SKAPA-LEVANM-TRANS                                   
512301                                                                          
512401         IF MID-CMD-INM = 'DAM'                                           
512501            MOVE JA TO NY-SKROT-SW                                        
512601            PERFORM S99-SKAPA-SKROT-ORDER                                 
512701         END-IF                                                           
512801       END-IF                                                             
512901     END-IF                                                               
513001                                                                          
513101     PERFORM S08-UPPDATERA-6301                                           
513201     .                                                                    
513301     EJECT                                                                
513401 HD-NYUPPLAEGG      SECTION.                                              
513501     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
513601        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
513701        PERFORM IMS-GU-WDB601-SEND                                        
513801     END-IF                                                               
513901                                                                          
514001     MOVE ZERO            TO W-KVRADER                                    
514101     ADD +1               TO W-KVRADER                                    
514201     MOVE JA              TO NYUPPLAEGG-SW                                
514301     MOVE W-IDARTNR-INM   TO W-IDARTNR                                    
514401     MOVE W-SPAR-IDDC     TO W-IDDC                                       
514501     PERFORM IMS-GET-WDK711                                               
514601                                                                          
514701     IF MID-ADLAGOMR-INM = ALL '+' OR SPACE                               
514801        MOVE SLAG-ADLAGOMR   TO W-ADLAGOMR-SPAR                           
514901     ELSE                                                                 
515001        MOVE W-ADLAGOMR-INM  TO W-ADLAGOMR-SPAR                           
515101                                SLAG-ADLAGOMR                             
515201                                W-ADLAGOMR-LOCB                           
515301     END-IF                                                               
515401     IF MID-ADGANG-INM = ALL '+' OR SPACE                                 
515501        MOVE SLAG-ADGANG     TO W-ADGANG-SPAR                             
515601     ELSE                                                                 
515701        MOVE W-ADGANG-INM    TO W-ADGANG-SPAR                             
515801                                SLAG-ADGANG                               
515901                                W-ADGANG-LOCB                             
516001     END-IF                                                               
516101     IF MID-ADPLATS-INM = ALL '+' OR SPACE                                
516201        MOVE SLAG-ADPLATS    TO W-ADPLATS-SPAR                            
516301     ELSE                                                                 
516401        MOVE W-ADPLATS-INM   TO W-ADPLATS-SPAR                            
516501                                SLAG-ADPLATS                              
516601                                W-ADPLATS-LOCB                            
516701     END-IF                                                               
516801                                                                          
516901     IF W-ADLAGOMR-LOCB NUMERIC                                           
517001     AND W-ADGANG-LOCB NUMERIC                                            
517101     AND W-ADPLATS-LOCB NUMERIC                                           
517201        IF W-ADLAGOMR-LOCB > ZERO                                         
517301        OR W-ADGANG-LOCB > ZERO                                           
517401        OR W-ADPLATS-LOCB > ZERO                                          
517501           PERFORM S30-UPPDATERA-WDJ9                                     
517601        END-IF                                                            
517701     END-IF                                                               
517801                                                                          
517901     MOVE SLAG-KVLS          TO WS-OLD-KVLS                               
518001     MOVE SLAG-KVEFRS        TO WS-OLD-KVEFRS                             
518101                                                                          
518201     MOVE WS-KVANTMOT-INM    TO WS-KVANTMOT-INM-NUM                       
518301     MOVE WS-KVSKROT-INM     TO WS-KVSKROT-INM-NUM                        
518401     COMPUTE WS-SUMMA-KVANT = (WS-KVANTMOT-INM-NUM                        
518501             + WS-KVSKROT-INM-NUM)                                        
518601     END-COMPUTE                                                          
518701     ADD WS-SUMMA-KVANT      TO SLAG-KVLS                                 
518801* ---NEDANSTÅENDE KOD LOGGAR                                              
518901* ---SALDOFÖRÄNDRINGAR PÅ WDL9                                            
519001     PERFORM S21-SALDOLOGG-DATA                                           
519101     MOVE W-SPAR-IDDC          TO LOGG-IDDC                               
519201     MOVE WS-SUMMA-KVANT       TO LOGG-KVART-SALDO                        
519301     MOVE SLAG-KVLS            TO LOGG-KVLS                               
519401     MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                              
519501     MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                             
519601     MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                          
519701     MOVE '+'                  TO LOGG-IDTECKEN-KVLS                      
519801     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS                     
519901     MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV                 
520001     MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS                    
520101     PERFORM S22-ISRT-SALDOLOGG                                           
520201* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
520301                                                                          
520401     IF SEND-DCS-CDC                                                      
520501        SUBTRACT WS-KVSKROT-INM-NUM FROM SLAG-KVLS                        
520601        IF (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                         
520701           MOVE WS-KVANTMOT-INM-NUM TO WS-RO-KVANTMOT                     
520801           PERFORM S19-EV-RO-TACKNING                                     
520901        END-IF                                                            
521001* ---NEDANSTÅENDE KOD LOGGAR                                              
521101* ---SALDOFÖRÄNDRINGAR PÅ WDL9                                            
521201        PERFORM S21-SALDOLOGG-DATA                                        
521301        MOVE W-SPAR-IDDC        TO LOGG-IDDC                              
521401        MOVE WS-KVSKROT-INM-NUM TO LOGG-KVART-SALDO                       
521501        MOVE SLAG-KVLS          TO LOGG-KVLS                              
521601        MOVE SLAG-KVAKS-SDC     TO LOGG-KVAKS                             
521701        MOVE SLAG-KVEFRS        TO LOGG-KVEFRS                            
521801        MOVE SLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                         
521901        MOVE '-'                TO LOGG-IDTECKEN-KVLS                     
522001        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS                    
522101        MOVE SPACE              TO LOGG-IDTECKEN-KVAKS-PAV                
522201        MOVE SPACE              TO LOGG-IDTECKEN-KVEFRS                   
522301        PERFORM S22-ISRT-SALDOLOGG                                        
522401* ---SLUT SALDOFÖRÄNDRINGAR                                               
522501     END-IF                                                               
522601                                                                          
522701     PERFORM IMS-REPL-WDK711                                              
522801                                                                          
522901     MOVE WS-SUMMA-KVANT     TO W-TEMP-KVANT                              
523001     MOVE ZERO               TO W-KVAVIS                                  
523101                                                                          
523201     MOVE DCS-FLINLREP   TO FILC3-FLINLREP                                
523301     MOVE ZERO           TO FILC3-KVAVIS                                  
523401     MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                                 
523501     MOVE 'NY'           TO FILC3-AVVIKELSETYP                            
523601     MOVE 6              TO FILC3-KDSORT1                                 
523701     PERFORM S33-SKAPA-LDC-TRANS                                          
523801     IF WS-KVSKROT-INM-NUM > ZERO                                         
523901        MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                          
524001        MOVE 'DAM'              TO FILC3-AVVIKELSETYP                     
524101        MOVE 5                  TO FILC3-KDSORT1                          
524201        PERFORM S33-SKAPA-LDC-TRANS                                       
524301     END-IF                                                               
524401                                                                          
524501     PERFORM IMS-GU-WLARTC01                                              
524601     MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                                
524602     MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                                
524701     PERFORM IMS-GHNP-WLARTC11                                            
524801     MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                            
524901     MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                           
525001                                                                          
525101     PERFORM S16-PRIS-TILLAMPNING                                         
525201     MOVE PRIS-PRARTNTO       TO WS-FIXAD-PRARTNTO                        
525301     MOVE 'USD'               TO WS-KDVALISO                              
525401     IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                         
525501       MOVE PRIS-KDVALISO     TO WS-KDVALISO                              
525601     END-IF                                                               
525701     MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                              
525801                                                                          
525901     PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                                   
526001                                                                          
526101     PERFORM IMS-GET-WDK711                                               
526201     MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                             
526301     MOVE '21'                TO 6308-KDANMORS                            
526401     MOVE WS-SUMMA-KVANT      TO 6308-KVLEVANM                            
526501                                                                          
526601     PERFORM S11-SKAPA-LEVANM-TRANS                                       
526701                                                                          
526801     PERFORM S16-PRIS-TILLAMPNING                                         
526901     MOVE PRIS-PRARTNTO    TO R8-EKH-PRARTNTO                             
527001     PERFORM S31-SKAPA-SAP-TRANS                                          
527101                                                                          
527201     MOVE 6302-IDDISTR     TO WS-SAP-IDDISTR                              
527301     MOVE W-SPAR-IDKUNDNR  TO WS-SAP-IDKUNDNR                             
527401                                                                          
527501     IF WS-KVANTMOT-INM  > 0                                              
527601        PERFORM IMS-REPL-WDK711                                           
527701     END-IF                                                               
527801                                                                          
527901     IF MID-CMD-INM = 'DAM'                                               
528001        MOVE '43'                TO 6308-KDANMORS                         
528101        MOVE WS-KVSKROT-INM-NUM  TO 6308-KVLEVANM                         
528201        PERFORM S11-SKAPA-LEVANM-TRANS                                    
528301     END-IF                                                               
528401                                                                          
528501* BERÄKNING AVGCOST                                                       
528601     MOVE W-SPAR-IDDC         TO W-IDDC                                   
528701     PERFORM IMS-GET-WDK711                                               
528801     MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                             
528901     PERFORM S10-OMRAKN-MEDELPRIS                                         
529001     MOVE W-TIME-N         TO AKTUELL-TID                                 
529101     MOVE W-DAGENS-DATUM   TO SLAG-TIAVCOST                               
529201     MOVE AVG-PRAVCOST-NEW TO SLAG-PRAVCOST                               
529301     PERFORM IMS-REPL-WDK711                                              
529401                                                                          
529501     PERFORM S08-UPPDATERA-6301                                           
529601     .                                                                    
529701     EJECT                                                                
529801                                                                          
529901 HE-UPPDATERA-WL630111    SECTION.                                        
530001                                                                          
530101     MOVE NEJ             TO WS-FAKTURA-KLAR                              
530201                                                                          
530301     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
530401     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
530501     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
530601                             W-SEQA-IDFAKT-MAX                            
530701     MOVE '310'           TO W-IDPTYP                                     
530801     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
530901                                                                          
531001     IF SEGMENT-SAKNAS                                                    
531101        MOVE W-SPAR-IDFAKT TO W-IDFAKT                                    
531201        MOVE W-SPAR-IDDC   TO W-6301-IDDC                                 
531301        PERFORM IMS-GHU-WL630111                                          
531401                                                                          
531501        MOVE LOW-VALUE     TO W-WDL6A1KY-MIN                              
531601        MOVE HIGH-VALUE    TO W-WDL6A1KY-MAX                              
531701        MOVE W-SPAR-IDFAKT TO W-SEQA-IDFAKT-MIN                           
531801                              W-SEQA-IDFAKT-MAX                           
531901        MOVE 'R30'         TO W-IDPTYP                                    
532001        PERFORM IMS-GU-WLINLD01-FIRST-310                                 
532101                                                                          
532201        IF SEGMENT-SAKNAS                                                 
532301           MOVE JA TO WS-FAKTURA-KLAR                                     
532401           PERFORM IMS-DLET-WL630111                                      
532501           PERFORM IMS-GHU-WL630511                                       
532601           IF SEGMENT-FINNS                                               
532701              MOVE JA TO 6306-FLKLAR                                      
532801              PERFORM IMS-REPL-WL630511                                   
532901           END-IF                                                         
533001        END-IF                                                            
533101     END-IF                                                               
533201     .                                                                    
533301     EJECT                                                                
533401                                                                          
533501 HF-ENDAST-LAGERPLATS     SECTION.                                        
533601                                                                          
533701         MOVE MID-IDARTNR (INDX) TO W-IDARTNR                             
533801         MOVE W-SPAR-IDDC    TO W-IDDC                                    
533901         IF DCS-CDC                                                       
534001            PERFORM IMS-GHU-WLARTC11                                      
534101         ELSE                                                             
534201            PERFORM IMS-GET-WDK711                                        
534301         END-IF                                                           
534401                                                                          
534501         IF W-ADLAGOMR (INDX) > ZERO                                      
534601            IF DCS-CDC                                                    
534701               MOVE W-ADLAGOMR (INDX) TO CLAG-ADLAGOMR                    
534801               MOVE W-ADGANG (INDX)   TO CLAG-ADGANG                      
534901               MOVE W-ADPLATS (INDX)  TO CLAG-ADPLATS                     
535001            ELSE                                                          
535101               MOVE W-ADLAGOMR (INDX) TO SLAG-ADLAGOMR                    
535201               MOVE W-ADGANG (INDX)   TO SLAG-ADGANG                      
535301               MOVE W-ADPLATS (INDX)  TO SLAG-ADPLATS                     
535401            END-IF                                                        
535501            MOVE W-ADLAGOMR (INDX) TO W-ADLAGOMR-LOCB                     
535601            MOVE W-ADGANG (INDX) TO W-ADGANG-LOCB                         
535701            MOVE W-ADPLATS (INDX) TO W-ADPLATS-LOCB                       
535801            PERFORM S30-UPPDATERA-WDJ9                                    
535901            IF DCS-CDC                                                    
536001               PERFORM IMS-REPL-WLARTC11                                  
536101            ELSE                                                          
536201               PERFORM IMS-REPL-WDK711                                    
536301            END-IF                                                        
536401         END-IF                                                           
536501     .                                                                    
536601     EJECT                                                                
536701                                                                          
536801 HG-NY-NYUPPLAEGG-ART-PV SECTION.                                         
536901                                                                          
537001     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
537101        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
537201        PERFORM IMS-GU-WDB601-SEND                                        
537301     END-IF                                                               
537401                                                                          
537501     MOVE ZERO            TO W-KVRADER                                    
537601     ADD +1               TO W-KVRADER                                    
537701     MOVE JA              TO NYUPPLAEGG-SW                                
537801     MOVE W-SPAR-IDDC     TO W-IDDC                                       
537901     MOVE ZERO            TO WS-OLD-KVLS                                  
538001                                                                          
538101     MOVE WS-KVANTMOT-INM TO WS-KVANTMOT-INM-NUM                          
538201     MOVE WS-KVSKROT-INM  TO WS-KVSKROT-INM-NUM                           
538301     COMPUTE WS-SUMMA-KVANT = (WS-KVANTMOT-INM-NUM +                      
538401           WS-KVSKROT-INM-NUM)                                            
538501     END-COMPUTE                                                          
538601     MOVE WS-SUMMA-KVANT  TO W-TEMP-KVANT                                 
538701     MOVE ZERO            TO W-KVAVIS                                     
538801                                                                          
538901     MOVE ALL '+'        TO WDK7-W005WDK7                                 
539001     MOVE 'WDK711'       TO WDK7-IDSEGM                                   
539101     MOVE W-IDARTNR-INM  TO WDK7-IDARTNR-KFB                              
539201     MOVE W-SPAR-IDDC    TO WDK7-IDDC-KFB                                 
539301                            WDK7-IDDC                                     
539401     MOVE W-ADLAGOMR-INM TO WDK7-ADLAGOMR                                 
539501     MOVE W-ADGANG-INM   TO WDK7-ADGANG                                   
539601     MOVE W-ADPLATS-INM  TO WDK7-ADPLATS                                  
539701     IF DCS-NDC-NA OR DCS-NDC-PF                                          
539801        MOVE JA          TO WDK7-FLORDSP                                  
539901                            WDK7-FLSPBULK                                 
540001     END-IF                                                               
540101     IF DCS-SDC OR DCS-NDC-PF                                             
540201        MOVE WS-SUMMA-KVANT      TO WDK7-KVLS                             
540301     ELSE                                                                 
540401        MOVE WS-KVANTMOT-INM-NUM TO WDK7-KVLS                             
540501     END-IF                                                               
540601                                                                          
540701     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB WDK7-PCB         
540801     MOVE WDK7-WDK711    TO SLAG-WDK711                                   
540901                                                                          
541001     MOVE W-ADLAGOMR-INM         TO W-ADLAGOMR-SPAR                       
541101                                    W-ADLAGOMR-LOCB                       
541201     MOVE W-ADGANG-INM           TO W-ADGANG-SPAR                         
541301                                    W-ADGANG-LOCB                         
541401     MOVE W-ADPLATS-INM          TO W-ADPLATS-SPAR                        
541501                                    W-ADPLATS-LOCB                        
541601                                                                          
541701     PERFORM S30-UPPDATERA-WDJ9                                           
541801                                                                          
541901* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
542001     PERFORM S21-SALDOLOGG-DATA                                           
542101     MOVE W-SPAR-IDDC    TO LOGG-IDDC                                     
542201     MOVE SLAG-KVLS      TO LOGG-KVLS                                     
542301     MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                                    
542401     MOVE SLAG-KVEFRS    TO LOGG-KVEFRS                                   
542501     MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                                
542601     MOVE '+'            TO LOGG-IDTECKEN-KVLS                            
542701     MOVE SPACE          TO LOGG-IDTECKEN-KVAKS                           
542801     MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV                       
542901     MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS                          
543001     IF DCS-SDC                                                           
543101     OR DCS-NDC-PF                                                        
543201        MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                           
543301     ELSE                                                                 
543401        MOVE WS-KVANTMOT-INM-NUM TO LOGG-KVART-SALDO                      
543501     END-IF                                                               
543601     PERFORM S22-ISRT-SALDOLOGG                                           
543701* ---SLUT SALDOFÖRÄNDRINGAR                                               
543801                                                                          
543901     IF DCS-SDC                                                           
544001     OR DCS-NDC-PF                                                        
544101        IF SEND-DCS-CDC                                                   
544201           PERFORM IMS-GU-WLARTC01                                        
544301           MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                          
544302           MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                          
544401           PERFORM IMS-GHNP-WLARTC11                                      
544501           MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                      
544601           MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                     
544701                                                                          
544801           SUBTRACT WS-SUMMA-KVANT FROM CLAG-KVLS                         
544901           PERFORM IMS-REPL-WLARTC11                                      
545001**** ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                   
545101           PERFORM S21-SALDOLOGG-DATA                                     
545201           MOVE WC-CDC-SE      TO LOGG-IDDC                               
545301           MOVE 'MISC'         TO LOGG-IDHUVTYP                           
545401           MOVE 'R34'          TO LOGG-IDSUBTYP                           
545501           MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                        
545601           MOVE CLAG-KVLS      TO LOGG-KVLS                               
545701           MOVE CLAG-KVAKS-CDC TO LOGG-KVAKS                              
545801           MOVE CLAG-KVEFRS    TO LOGG-KVEFRS                             
545901           MOVE CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                          
546001           MOVE '-'            TO LOGG-IDTECKEN-KVLS                      
546101           MOVE SPACE          TO LOGG-IDTECKEN-KVAKS                     
546201           MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV                 
546301           MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS                    
546401           PERFORM S22-ISRT-SALDOLOGG                                     
546501           PERFORM S07-SKAPA-HISTORIK                                     
546601           MOVE 6302-IDDISTR TO FILC4-IDDISTR                             
546701           PERFORM S12-SKAPA-BILLIT-TRANS                                 
546801           IF DCS-NDC-PF                                                  
546901              PERFORM S20-SKAPA-REFILLTRANS                               
547001           END-IF                                                         
547101**** ---SLUT SALDOFÖRÄNDRINGAR                                            
547201        ELSE                                                              
547301           PERFORM IMS-GU-WLARTC01                                        
547401           MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                          
547402           MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                          
547501           PERFORM IMS-GHNP-WLARTC11                                      
547601           MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                      
547701           MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                     
547801           IF SEND-DCS-NDC-PF OR SEND-DCS-SDC                             
547901              MOVE W-IDDC TO WS-SPARAT-IDDC                               
548001              MOVE SEND-WS-IDDC TO W-IDDC                                 
548101              PERFORM IMS-GET-WDK711                                      
548201              SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                      
548301              PERFORM IMS-REPL-WDK711                                     
548401              MOVE WS-SPARAT-IDDC     TO W-IDDC                           
548501******* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                
548601              PERFORM S21-SALDOLOGG-DATA                                  
548701              MOVE SEND-WS-IDDC   TO LOGG-IDDC                            
548801              MOVE 'MISC'         TO LOGG-IDHUVTYP                        
548901              MOVE 'R34'          TO LOGG-IDSUBTYP                        
549001              MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                     
549101              MOVE SLAG-KVLS      TO LOGG-KVLS                            
549201              MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                           
549301              MOVE SLAG-KVEFRS    TO LOGG-KVEFRS                          
549401              MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                       
549501              MOVE '-'            TO LOGG-IDTECKEN-KVLS                   
549601              MOVE SPACE          TO LOGG-IDTECKEN-KVAKS                  
549701              MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV              
549801              MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS                 
549901              PERFORM S22-ISRT-SALDOLOGG                                  
550001              PERFORM S37-SKAPA-NDC-HIST-SANDANDE                         
550101              IF DCS-NDC-PF                                               
550201                 PERFORM S20-SKAPA-REFILLTRANS                            
550301              END-IF                                                      
550401******* ---SLUT SALDOFÖRÄNDRINGAR                                         
550501           END-IF                                                         
550601        END-IF                                                            
550701        MOVE ZERO  TO WS-FIXAD-PRARTNTO                                   
550801        MOVE SPACE TO WS-KDVALISO                                         
550901        IF SEND-DCS-DDC                                                   
551001           MOVE '1441 '    TO INL-IDLEVNR                                 
551101        ELSE                                                              
551201           MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                        
551301        END-IF                                                            
551401        PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                                
551501        MOVE 6302-IDDISTR    TO WS-SAP-IDDISTR                            
551601        MOVE W-SPAR-IDKUNDNR TO WS-SAP-IDKUNDNR                           
551701        PERFORM S31-SKAPA-SAP-TRANS-VCCS                                  
551801        IF SEND-DCS-DDC                                                   
551901           MOVE WS-SUMMA-KVANT        TO FILC2-KVANTAL                    
552001           MOVE 'ÖVERLEV.'            TO FILC2-AVVIKELSETYP               
552101           PERFORM S32-SKAPA-DIFF-TRANS                                   
552201           IF WS-KVSKROT-INM-NUM > ZERO                                   
552301              MOVE WS-KVSKROT-INM-NUM TO FILC2-KVANTAL                    
552401              MOVE 'DAM'              TO FILC2-AVVIKELSETYP               
552501              PERFORM S32-SKAPA-DIFF-TRANS                                
552601           END-IF                                                         
552701        END-IF                                                            
552801                                                                          
552901***     IF  SEND-DCS-CDC                                                  
553001***     AND DCS-FLINLREP = JA                                             
553101           MOVE DCS-FLINLREP   TO FILC3-FLINLREP                          
553201           MOVE ZERO           TO FILC3-KVAVIS                            
553301           MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                           
553401           MOVE 'NY'           TO FILC3-AVVIKELSETYP                      
553501           MOVE 6              TO FILC3-KDSORT1                           
553601           PERFORM S33-SKAPA-LDC-TRANS                                    
553701           IF WS-KVSKROT-INM-NUM > ZERO                                   
553801              MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                    
553901              MOVE 'DAM'              TO FILC3-AVVIKELSETYP               
554001              MOVE 5                  TO FILC3-KDSORT1                    
554101              PERFORM S33-SKAPA-LDC-TRANS                                 
554201           END-IF                                                         
554301***     END-IF                                                            
554401                                                                          
554501        IF MID-CMD-INM = 'DAM'                                            
554601           MOVE JA TO NY-SKROT-SW                                         
554701           PERFORM S99-SKAPA-SKROT-ORDER                                  
554801        END-IF                                                            
554901     ELSE                                                                 
555001                                                                          
555101        IF DCS-NDC-NA                                                     
555201                                                                          
555301           MOVE DCS-FLINLREP   TO FILC3-FLINLREP                          
555401           MOVE ZERO           TO FILC3-KVAVIS                            
555501           MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                           
555601           MOVE 'NY'           TO FILC3-AVVIKELSETYP                      
555701           MOVE 6              TO FILC3-KDSORT1                           
555801           PERFORM S33-SKAPA-LDC-TRANS                                    
555901           IF WS-KVSKROT-INM-NUM > ZERO                                   
556001              MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                    
556101              MOVE 'DAM'              TO FILC3-AVVIKELSETYP               
556201              MOVE 5                  TO FILC3-KDSORT1                    
556301              PERFORM S33-SKAPA-LDC-TRANS                                 
556401           END-IF                                                         
556501                                                                          
556601           PERFORM IMS-GU-WLARTC01                                        
556701           MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                          
556702           MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                          
556801           PERFORM IMS-GHNP-WLARTC11                                      
556901           MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                      
557001           MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                     
557101                                                                          
557201           IF SEND-DCS-CDC OR SEND-DCS-DDC                                
557301                                                                          
557401              PERFORM S16-PRIS-TILLAMPNING                                
557501              MOVE PRIS-PRARTNTO        TO WS-FIXAD-PRARTNTO              
557601              MOVE 'SEK'                TO WS-KDVALISO                    
557701              IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                
557801                MOVE PRIS-KDVALISO      TO WS-KDVALISO                    
557901              END-IF                                                      
558001              IF SEND-DCS-DDC                                             
558101                 MOVE '1441 '    TO INL-IDLEVNR                           
558201              ELSE                                                        
558301                 MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                  
558401              END-IF                                                      
558501                                                                          
558601              PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                          
558701              MOVE 6302-IDDISTR    TO WS-SAP-IDDISTR                      
558801              MOVE W-SPAR-IDKUNDNR TO WS-SAP-IDKUNDNR                     
558901              PERFORM S09-SKAPA-EKOTRANS-A03                              
559001                                                                          
559101              PERFORM IMS-GET-WDK711                                      
559201              MOVE SLAG-PRAVCOST        TO EKOTRA03-PRAVCOST-OLD          
559301              MOVE WS-KVANTMOT-INM-NUM  TO AVG-KVANTMOT                   
559401                                           EKOTRA03-KVANTMOT              
559501              MOVE ZERO                 TO EKOTRA03-KVSKROT               
559601              MOVE '21'                 TO EKOTRA03-KDANMORS              
559701                                           6308-KDANMORS                  
559801              MOVE WS-SUMMA-KVANT       TO 6308-KVLEVANM                  
559901                                                                          
560001              PERFORM S11-SKAPA-LEVANM-TRANS                              
560101              MOVE 'SJU'  TO STEXT                                        
560201              PERFORM S10-OMRAKN-MEDELPRIS-NA                             
560301                                                                          
560401              MOVE W-TIME-N             TO AKTUELL-TID                    
560501              PERFORM S18-FIXA-LOKALTID                                   
560601              MOVE MSGI-TILOKDAT        TO SLAG-TIAVCOST                  
560701              MOVE AVG-PRAVCOST-NEW     TO EKOTRA03-PRAVCOST              
560801                                           SLAG-PRAVCOST                  
560901              MOVE AVG-REMARKUP         TO EKOTRA03-REMARKUP              
561001              MOVE AVG-PRKURS           TO EKOTRA03-PRKURS                
561101                                                                          
561201              IF WS-KVANTMOT-INM  > 0                                     
561301                 PERFORM IMS-REPL-WDK711                                  
561401              ELSE                                                        
561501                 MOVE EKOTRA03-PRAVCOST-OLD                               
561601                                        TO EKOTRA03-PRAVCOST              
561701              END-IF                                                      
561801                                                                          
561901              IF MID-CMD-INM = 'DAM'                                      
562001                 MOVE '43'               TO 6308-KDANMORS                 
562101                 MOVE WS-KVSKROT-INM-NUM TO 6308-KVLEVANM                 
562201                                            EKOTRA03-KVSKROT              
562301                 PERFORM S11-SKAPA-LEVANM-TRANS                           
562401              END-IF                                                      
562501              PERFORM S20-SKAPA-REFILLTRANS                               
562601                                                                          
562701           ELSE                                                           
562801                                                                          
562901             IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                        
563001                 AND (DCS-NDC-NA AND DCS-CANADA)                          
563101             OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                     
563201                 AND (DCS-NDC-NA AND DCS-USA)                             
563301                                                                          
563401                 MOVE SEND-WS-IDDC TO W-IDDC                              
563501                 PERFORM IMS-GET-WDK711                                   
563601                 MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO                  
563701                 IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                   
563801                    MOVE 'CAD'      TO WS-KDVALISO                        
563901                 ELSE                                                     
564001                    MOVE 'USD'      TO WS-KDVALISO                        
564101                 END-IF                                                   
564201                                                                          
564301                 MOVE W-SPAR-IDDC TO W-IDDC                               
564401                 PERFORM IMS-GET-WDK711                                   
564501                 MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                  
564601                 PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                       
564701                                                                          
564801                 PERFORM S09-SKAPA-EKOTRANS-A03                           
564901                                                                          
565001                 MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD        
565101                 MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT                 
565201                                             EKOTRA03-KVANTMOT            
565301                 MOVE ZERO                TO EKOTRA03-KVSKROT             
565401                 MOVE '21'                TO EKOTRA03-KDANMORS            
565501                                             6308-KDANMORS                
565601                 MOVE WS-SUMMA-KVANT      TO 6308-KVLEVANM                
565701                                                                          
565801                 PERFORM S11-SKAPA-LEVANM-TRANS                           
565901                 MOVE 'ATT'  TO STEXT                                     
566001                 PERFORM S10-OMRAKN-MEDELPRIS-NA                          
566101                                                                          
566201                 MOVE W-TIME-N            TO AKTUELL-TID                  
566301                 PERFORM S18-FIXA-LOKALTID                                
566401                 MOVE MSGI-TILOKDAT       TO SLAG-TIAVCOST                
566501                 MOVE AVG-PRAVCOST-NEW    TO EKOTRA03-PRAVCOST            
566601                                             SLAG-PRAVCOST                
566701                 MOVE AVG-REMARKUP        TO EKOTRA03-REMARKUP            
566801                 MOVE AVG-PRKURS          TO EKOTRA03-PRKURS              
566901                                                                          
567001                 IF WS-KVANTMOT-INM  > 0                                  
567101                    PERFORM IMS-REPL-WDK711                               
567201                 ELSE                                                     
567301                    MOVE EKOTRA03-PRAVCOST-OLD                            
567401                                          TO EKOTRA03-PRAVCOST            
567501                 END-IF                                                   
567601                                                                          
567701                 IF MID-CMD-INM = 'DAM'                                   
567801                    MOVE '42'               TO 6308-KDANMORS              
567901                    MOVE WS-KVSKROT-INM-NUM TO 6308-KVLEVANM              
568001                                               EKOTRA03-KVSKROT           
568101                    PERFORM S11-SKAPA-LEVANM-TRANS                        
568201                 END-IF                                                   
568301                                                                          
568401                 PERFORM S20-SKAPA-REFILLTRANS                            
568501             ELSE                                                         
568601                                                                          
568701                 IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                    
568801                 AND (DCS-NDC-NA AND DCS-USA)                             
568901                                                                          
569001                    MOVE SEND-WS-IDDC TO W-IDDC                           
569101                    PERFORM IMS-GET-WDK711                                
569201                    SUBTRACT WS-SUMMA-KVANT FROM SLAG-KVLS                
569301                    MOVE SLAG-PRAVCOST      TO WS-FIXAD-PRARTNTO          
569401                    MOVE 'USD'              TO WS-KDVALISO                
569501                    PERFORM IMS-REPL-WDK711                               
569601* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
569701                    PERFORM S21-SALDOLOGG-DATA                            
569801                    MOVE SEND-WS-IDDC   TO LOGG-IDDC                      
569901                    MOVE 'MISC'         TO LOGG-IDHUVTYP                  
570001                    MOVE 'R34'          TO LOGG-IDSUBTYP                  
570101                    MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO               
570201                    MOVE SLAG-KVLS      TO LOGG-KVLS                      
570301                    MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                     
570401                    MOVE SLAG-KVEFRS    TO LOGG-KVEFRS                    
570501                    MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                 
570601                    MOVE '-'            TO LOGG-IDTECKEN-KVLS             
570701                    MOVE SPACE          TO LOGG-IDTECKEN-KVAKS            
570801                    MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV        
570901                    MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS           
571001                    PERFORM S22-ISRT-SALDOLOGG                            
571101* ---SLUT SALDOFÖRÄNDRINGAR                                               
571201                                                                          
571301                    MOVE W-SPAR-IDDC TO W-IDDC                            
571401                    PERFORM IMS-GET-WDK711                                
571501                    MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR               
571601                    PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                    
571701                                                                          
571801                    PERFORM S09-SKAPA-EKOTRANS-A03                        
571901                                                                          
572001                    MOVE SLAG-PRAVCOST TO EKOTRA03-PRAVCOST-OLD           
572101                    MOVE WS-KVANTMOT-INM-NUM TO AVG-KVANTMOT              
572201                                                EKOTRA03-KVANTMOT         
572301                    MOVE ZERO                TO EKOTRA03-KVSKROT          
572401                    MOVE '21'                TO EKOTRA03-KDANMORS         
572501                                                                          
572601                    MOVE 'NIO'  TO STEXT                                  
572701                    PERFORM S10-OMRAKN-MEDELPRIS-NA                       
572801                                                                          
572901                    MOVE W-TIME-N            TO AKTUELL-TID               
573001                    PERFORM S18-FIXA-LOKALTID                             
573101                    MOVE MSGI-TILOKDAT       TO SLAG-TIAVCOST             
573201                    MOVE AVG-PRAVCOST-NEW    TO EKOTRA03-PRAVCOST         
573301                                                SLAG-PRAVCOST             
573401                    MOVE AVG-REMARKUP        TO EKOTRA03-REMARKUP         
573501                    MOVE AVG-PRKURS          TO EKOTRA03-PRKURS           
573601                    MOVE 1.0                 TO EKOTRA03-PRKURS           
573701                                                                          
573801                    IF WS-KVANTMOT-INM  > 0                               
573901                       PERFORM IMS-REPL-WDK711                            
574001                    ELSE                                                  
574101                       MOVE EKOTRA03-PRAVCOST-OLD                         
574201                                             TO EKOTRA03-PRAVCOST         
574301                    END-IF                                                
574401                                                                          
574501                    MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR               
574601                    PERFORM S13-SKAPA-NDC-HIST-SANDANDE                   
574701                                                                          
574801                    IF MID-CMD-INM = 'DAM'                                
574901                       MOVE WS-KVSKROT-INM-NUM TO EKOTRA03-KVSKROT        
575001                    END-IF                                                
575101                                                                          
575201                    PERFORM S20-SKAPA-REFILLTRANS                         
575301                                                                          
575401                 END-IF                                                   
575501              END-IF                                                      
575601           END-IF                                                         
575701                                                                          
575801        END-IF                                                            
575901     END-IF                                                               
576001                                                                          
576101     PERFORM S08-UPPDATERA-6301                                           
576201     .                                                                    
576301     EJECT                                                                
576401                                                                          
576501 HG-NY-NYUPPLAEGG-ART      SECTION.                                       
576601                                                                          
576701     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
576801        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
576901        PERFORM IMS-GU-WDB601-SEND                                        
577001     END-IF                                                               
577101                                                                          
577201     MOVE ZERO            TO W-KVRADER                                    
577301     ADD +1               TO W-KVRADER                                    
577401     MOVE JA              TO NYUPPLAEGG-SW                                
577501     MOVE W-SPAR-IDDC     TO W-IDDC                                       
577601     MOVE ZERO            TO WS-OLD-KVLS                                  
577701     MOVE ZERO            TO WS-OLD-KVEFRS                                
577801                                                                          
577901     MOVE WS-KVANTMOT-INM TO WS-KVANTMOT-INM-NUM                          
578001     MOVE WS-KVSKROT-INM  TO WS-KVSKROT-INM-NUM                           
578101     COMPUTE WS-SUMMA-KVANT = (WS-KVANTMOT-INM-NUM +                      
578201           WS-KVSKROT-INM-NUM)                                            
578301     END-COMPUTE                                                          
578401     MOVE WS-SUMMA-KVANT  TO W-TEMP-KVANT                                 
578501     MOVE ZERO            TO W-KVAVIS                                     
578601                                                                          
578701     MOVE ALL '+'         TO WDK7-W005WDK7                                
578801     MOVE 'WDK711'        TO WDK7-IDSEGM                                  
578901     MOVE W-IDARTNR-INM   TO WDK7-IDARTNR-KFB                             
579001     MOVE W-SPAR-IDDC     TO WDK7-IDDC-KFB                                
579101                             WDK7-IDDC                                    
579201     MOVE W-ADLAGOMR-INM  TO WDK7-ADLAGOMR                                
579301                             W-ADLAGOMR-SPAR                              
579401                             W-ADLAGOMR-LOCB                              
579501     MOVE W-ADGANG-INM    TO WDK7-ADGANG                                  
579601                             W-ADGANG-SPAR                                
579701                             W-ADGANG-LOCB                                
579801     MOVE W-ADPLATS-INM   TO WDK7-ADPLATS                                 
579901                             W-ADPLATS-SPAR                               
580001                             W-ADPLATS-LOCB                               
580101     IF SEND-DCS-CHINA                                                    
580201       MOVE WS-SUMMA-KVANT      TO WDK7-KVLS                              
580301     ELSE                                                                 
580401* (SWEDEN TO CHINA)                                                       
580501       MOVE WS-KVANTMOT-INM-NUM TO WDK7-KVLS                              
580601     END-IF                                                               
580701     IF NDC-CN                                                            
580801     OR (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                            
580901        MOVE JA                  TO WDK7-FLORDSP                          
581001                                    WDK7-FLSPBULK                         
581101     END-IF                                                               
581201                                                                          
581301     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
581401                                       ARTC-PCB WDK7-PCB                  
581501     PERFORM S30-UPPDATERA-WDJ9                                           
581601                                                                          
581701     MOVE W-SPAR-IDDC       TO W-IDDC                                     
581801     PERFORM IMS-GET-WDK711-GE                                            
581901                                                                          
582001* ---LOGGAR SALDOFÖRÄNDRINGAR I WDL9                                      
582101     PERFORM S21-SALDOLOGG-DATA                                           
582201     MOVE W-SPAR-IDDC    TO LOGG-IDDC                                     
582301     MOVE SLAG-KVLS      TO LOGG-KVLS                                     
582401     MOVE SLAG-KVAKS-SDC TO LOGG-KVAKS                                    
582501     MOVE SLAG-KVEFRS    TO LOGG-KVEFRS                                   
582601     MOVE SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                                
582701     MOVE '+'            TO LOGG-IDTECKEN-KVLS                            
582801     MOVE SPACE          TO LOGG-IDTECKEN-KVAKS                           
582901     MOVE SPACE          TO LOGG-IDTECKEN-KVAKS-PAV                       
583001     MOVE SPACE          TO LOGG-IDTECKEN-KVEFRS                          
583101     IF SEND-DCS-CHINA                                                    
583201        MOVE WS-SUMMA-KVANT TO LOGG-KVART-SALDO                           
583301     ELSE                                                                 
583401        MOVE WS-KVANTMOT-INM-NUM TO LOGG-KVART-SALDO                      
583501     END-IF                                                               
583601     PERFORM S22-ISRT-SALDOLOGG                                           
583701* ---SLUT SALDOFÖRÄNDRINGAR                                               
583801                                                                          
583901                                                                          
584001     MOVE DCS-FLINLREP   TO FILC3-FLINLREP                                
584101     MOVE ZERO           TO FILC3-KVAVIS                                  
584201     MOVE WS-SUMMA-KVANT TO FILC3-KVANTAL                                 
584301     MOVE 'NY'           TO FILC3-AVVIKELSETYP                            
584401     MOVE 6              TO FILC3-KDSORT1                                 
584501     PERFORM S33-SKAPA-LDC-TRANS                                          
584601     IF WS-KVSKROT-INM-NUM > ZERO                                         
584701        MOVE WS-KVSKROT-INM-NUM TO FILC3-KVANTAL                          
584801        MOVE 'DAM'              TO FILC3-AVVIKELSETYP                     
584901        MOVE 5                  TO FILC3-KDSORT1                          
585001        PERFORM S33-SKAPA-LDC-TRANS                                       
585101     END-IF                                                               
585201                                                                          
585301     PERFORM IMS-GU-WLARTC01                                              
585401     MOVE K6-ART-KDPRODSL    TO W-KDPRODSL                                
585402     MOVE K6-ART-IDFKNGRP    TO W-IDFKNGRP                                
585501     PERFORM IMS-GHNP-WLARTC11                                            
585601     MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                            
585701     MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                           
585801                                                                          
585901     PERFORM S16-PRIS-TILLAMPNING                                         
586001     MOVE PRIS-PRARTNTO        TO WS-FIXAD-PRARTNTO                       
586101     MOVE 'USD'                TO WS-KDVALISO                             
586201     IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                         
586301       MOVE PRIS-KDVALISO      TO WS-KDVALISO                             
586401     END-IF                                                               
586501     MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                              
586601                                                                          
586701     PERFORM S15-SKAPA-SDC-NDC-HIST-MOT                                   
586801     MOVE 6302-IDDISTR    TO WS-SAP-IDDISTR                               
586901     MOVE W-SPAR-IDKUNDNR TO WS-SAP-IDKUNDNR                              
587001                                                                          
587101     PERFORM IMS-GET-WDK711                                               
587201     MOVE WS-KVANTMOT-INM-NUM  TO AVG-KVANTMOT                            
587301     MOVE '21'                 TO 6308-KDANMORS                           
587401     MOVE WS-SUMMA-KVANT       TO 6308-KVLEVANM                           
587501                                                                          
587601     PERFORM S11-SKAPA-LEVANM-TRANS                                       
587701                                                                          
587801     PERFORM S16-PRIS-TILLAMPNING                                         
587901     MOVE PRIS-PRARTNTO        TO R8-EKH-PRARTNTO                         
588001     PERFORM S31-SKAPA-SAP-TRANS                                          
588101                                                                          
588201     IF WS-KVANTMOT-INM  > 0                                              
588301        PERFORM IMS-REPL-WDK711                                           
588401     END-IF                                                               
588501                                                                          
588601     IF MID-CMD-INM = 'DAM'                                               
588701        MOVE '43'               TO 6308-KDANMORS                          
588801        MOVE WS-KVSKROT-INM-NUM TO 6308-KVLEVANM                          
588901        PERFORM S11-SKAPA-LEVANM-TRANS                                    
589001     END-IF                                                               
589101     PERFORM S20-SKAPA-REFILLTRANS                                        
589201                                                                          
589301* BERÄKNING AVGCOST                                                       
589401     MOVE W-SPAR-IDDC          TO W-IDDC                                  
589501     PERFORM IMS-GET-WDK711                                               
589601     MOVE WS-KVANTMOT-INM-NUM  TO AVG-KVANTMOT                            
589701     PERFORM S10-OMRAKN-MEDELPRIS                                         
589801     MOVE W-TIME-N             TO AKTUELL-TID                             
589901     MOVE W-DAGENS-DATUM       TO SLAG-TIAVCOST                           
590001     MOVE AVG-PRAVCOST-NEW     TO SLAG-PRAVCOST                           
590101     PERFORM IMS-REPL-WDK711                                              
590201                                                                          
590301     PERFORM S08-UPPDATERA-6301                                           
590401     .                                                                    
590501     EJECT                                                                
590601 I-KOLLA-INDATA-FINNS SECTION.                                            
590701                                                                          
590801     MOVE NEJ TO INDATA-FINNS-SW                                          
590901                                                                          
591001     IF MFS-UPD-X                                                         
591101        MOVE +1 TO INDX                                                   
591201        PERFORM UNTIL INDX > MAX-INDX OR INDATA-FINNS                     
591301            IF  MID-MID-IDKUNDRF(INDX) NOT = ALL '+'                      
591401            OR  MID-MID-IDKUNDNR(INDX) NOT = ALL '+'                      
591501            OR  MID-MID-IDKOLLI (INDX) NOT = ALL '+'                      
591601                MOVE JA      TO INDATA-FINNS-SW                           
591701            END-IF                                                        
591801            ADD +1 TO INDX                                                
591901        END-PERFORM                                                       
592001     END-IF                                                               
592101                                                                          
592201     IF  MID-KOLLI-KLART  NOT = ALL '+'                                   
592301     AND MID-KOLLI-KLART  NOT = SPACE                                     
592401         MOVE JA TO INDATA-FINNS-SW                                       
592501     END-IF                                                               
592601                                                                          
592701     MOVE +1 TO INDX                                                      
592801     PERFORM UNTIL INDX > MAX-INDX OR INDATA-FINNS                        
592901         IF  MID-CMD-IN      (INDX) NOT = ALL '+'                         
593001         OR  MID-KVANTMOT-IN (INDX) NOT = ALL '+'                         
593101         OR  MID-KVSKROT-IN  (INDX) NOT = ALL '+'                         
593201         OR  MID-ADLAGOMR    (INDX) NOT = ALL '+'                         
593301         OR  MID-ADGANG      (INDX) NOT = ALL '+'                         
593401         OR  MID-ADPLATS     (INDX) NOT = ALL '+'                         
593501             MOVE JA      TO INDATA-FINNS-SW                              
593601         END-IF                                                           
593701         ADD +1 TO INDX                                                   
593801     END-PERFORM                                                          
593901                                                                          
594001     IF  INDATA-SAKNAS                                                    
594101         IF  MID-IDARTNR-INM  NOT = ALL '+'                               
594201         OR  MID-KVANTMOT-INM NOT = ALL '+'                               
594301         OR  MID-KVSKROT-INM NOT  = ALL '+'                               
594401         OR  MID-ADLAGOMR-INM NOT = ALL '+'                               
594501         OR  MID-ADGANG-INM NOT   = ALL '+'                               
594601         OR  MID-ADPLATS-INM NOT  = ALL '+'                               
594701         OR  MID-CMD-INM NOT      = ALL '+'                               
594801             MOVE JA TO INDATA-FINNS-SW                                   
594901         END-IF                                                           
595001     END-IF                                                               
595101     .                                                                    
595201     EJECT                                                                
595301 K-UPPDATERA-X-TRANS SECTION.                                             
595401                                                                          
595501     MOVE +1 TO TAB-IX                                                    
595601     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
595701       IF BIN-TAB-IDFAKT(TAB-IX) > ZERO                                   
595801           MOVE BIN-TAB-IDFAKT(TAB-IX)     TO W-SPAR-IDFAKT               
595901           MOVE BIN-TAB-IDKUNDRF(TAB-IX)   TO W-SPAR-IDKUNDRF             
596001           MOVE BIN-TAB-IDKUNDNR(TAB-IX)   TO W-SPAR-IDKUNDNR             
596101           MOVE BIN-TAB-IDKOLLI(TAB-IX)    TO W-SPAR-IDKOLLI              
596201                                                                          
596301           IF (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                      
596401             PERFORM HA-UPPDATERA-KOLLI-KLART                             
596501           ELSE                                                           
596601             PERFORM HA-UPPDATERA-KOLLI-KLART-PV                          
596701           END-IF                                                         
596801                                                                          
596901       END-IF                                                             
597001       ADD +1 TO TAB-IX                                                   
597101     END-PERFORM                                                          
597201     .                                                                    
597301     EJECT                                                                
597401 L-STARTA-6302-TRANS SECTION.                                             
597501                                                                          
597601     COMPUTE P-TO-P-LL-2 =   LNG-P-TO-P-PREFIX +                          
597701                   LENGTH OF 6302-MID-W6I30201                            
597801                                                                          
597901     MOVE LOW-VALUE      TO P-TO-P-Z1-2                                   
598001                            P-TO-P-Z2-2                                   
598101     MOVE 'W6T302 '      TO P-TO-P-TRANSKOD-2                             
598201     MOVE '630C'         TO P-TO-P-FROM-MID-2                             
598301     MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR-2                             
598401                                                                          
598501     MOVE LOW-VALUE TO 6302-MID-W6I30201                                  
598601     MOVE W-SPAR-IDFAKT TO 6302-MID-IDFAKT-IN                             
598701     INSPECT 6302-MID-IDFAKT-IN REPLACING LEADING ZERO BY SPACE           
598801     .                                                                    
598901     EJECT                                                                
599001 S01-DISTRIKT-RETUR         SECTION.                                      
599101*--------------------------------------------------                       
599201*SO-FIX ENL. SAMTAL MED SUSSI 20160617 CHINA EXPORT                       
599301*---FÖR ATT SKILJA DE OLIKA SKROTNINGARNA PÅ CDC                          
599401*---FÖR DISTR 70, RSKROT-DISTR + KUNDNR 57561 OCH                         
599501*---VID REFILL FRÅN KINA,SÅ HÅRDKODAR VI KUNDNR 71.                       
599601*---PÅ W603-MOTTAGNINGEN.                                                 
599701*---FORTSÄTTER MED SKROTDISTR. 70-57561 FÖR RETURER.                      
599801*---EN BÄTTRE LÖSNING MED NYTT DISTRIKT SENARE.                           
599901*--------------------------------------------------                       
600001                                                                          
600101     MOVE W-SPAR-IDDC          TO W-IDDC-B6                               
600201     PERFORM IMS-GU-WDB601                                                
600301     IF DCS-CDC                                                           
600401       MOVE DCS-IDDISTR-RSKROT                                            
600501                               TO W-IDDISTR-RETUR                         
600601       IF SEND-DCS-NDC-CN                                                 
600701                                                                          
600801         MOVE '000071'                                                    
600901                               TO W-IDKUNDNR-RETUR                        
601001       ELSE                                                               
601101         MOVE DCS-IDKUNDNR-RSKROT                                         
601201                               TO W-IDKUNDNR-RETUR                        
601301                                                                          
601401       END-IF                                                             
601501     ELSE                                                                 
601601       MOVE DCS-IDDISTR-QSKROT                                            
601701                               TO W-IDDISTR-RETUR                         
601801       MOVE DCS-IDKUNDNR-QSKROT                                           
601901                               TO W-IDKUNDNR-RETUR                        
602001     END-IF                                                               
602101                                                                          
602201     .                                                                    
602301     EJECT                                                                
602401 S03-CALL-W006KOM        SECTION.                                         
602501                                                                          
602601     MOVE KOM-AREA        TO P-TO-P-DATA                                  
602701     CALL W006KOM         USING MSG-PCB                                   
602801                                ALT-PCB                                   
602901                                KOMA-PCB                                  
603001                                MSG-KOM-WMSGKOM                           
603101                                P-TO-P-AREA                               
603201                                                                          
603301     .                                                                    
603401     EJECT                                                                
603501 S04-SKAPA-DAINLEV-IDLOPNRM  SECTION.                                     
603601                                                                          
603701     IF  W-IDLOPNRM             = ZERO                                    
603801         PERFORM IMS-GHU-W6LOPA11                                         
603901         MOVE 6018-IDLOPNRM     TO W-IDLOPNRM                             
604001     END-IF                                                               
604101                                                                          
604201     IF DAT-TIAAVVD-GRP (3:3) = W-VVD                                     
604301       ADD +1                     TO W-LLLL                               
604401     ELSE                                                                 
604501       MOVE DAT-TIAAVVD-GRP (3:3) TO W-VVD                                
604601       MOVE +1                    TO W-LLLL                               
604701     END-IF                                                               
604801                                                                          
604901     CALL CHECK USING W-IDLOPNRM (2:7) FLT-LGD                            
605001          VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B               
605101                                                                          
605201     MOVE FUNCTION CURRENT-DATE (1:8) TO W-TIAAAAMMDDTTMMSSTH-DATE        
605301     ACCEPT W-TIAAAAMMDDTTMMSSTH-TIME FROM TIME                           
605401                                                                          
605501     COMPUTE W-DAINLEV          = 9999999999999999                        
605601                                 - W-TIAAAAMMDDTTMMSSTH                   
605701     END-COMPUTE                                                          
605801     .                                                                    
605901     EJECT                                                                
606001 S07-SKAPA-HISTORIK SECTION.                                              
606101                                                                          
606201     PERFORM S04-SKAPA-DAINLEV-IDLOPNRM                                   
606301                                                                          
606401     PERFORM IMS-GU-WLINLE01                                              
606501     IF SEGMENT-SAKNAS                                                    
606601       MOVE W-IDARTNR TO INLE-ART-IDARTNR                                 
606701       PERFORM IMS-ISRT-WLINLE01                                          
606801     END-IF                                                               
606901                                                                          
607001     MOVE W-DAINLEV TO INLE-INL-DAINLEV                                   
607101     PERFORM IMS-ISRT-WLINLE11                                            
607201     PERFORM UNTIL INSERT-OK                                              
607301       SUBTRACT 1 FROM W-DAINLEV                                          
607401       MOVE W-DAINLEV TO INLE-INL-DAINLEV                                 
607501       PERFORM IMS-ISRT-WLINLE11                                          
607601     END-PERFORM                                                          
607701                                                                          
607801     MOVE 'R34'            TO INLE-DIR-IDPTYP                             
607901     MOVE W-IDLOPNRM       TO INLE-DIR-IDLOPNRM                           
608001     MOVE W-SPAR-IDFAKT    TO INLE-DIR-IDAVINR                            
608101     MOVE W-DAGENS-DATUM   TO INLE-DIR-TIAVSDAT                           
608201     MOVE WC-CDC-SE        TO INLE-DIR-IDDC                               
608301     MOVE +8               TO INLE-DIR-KDRT                               
608401     MOVE SPACE            TO INLE-DIR-IDANALYS                           
608501                              INLE-DIR-IDKST                              
608601                                                                          
608701     SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                                  
608801                       GIVING INLE-DIR-KVAVIS                             
608901                                                                          
609001     IF W-KVAVIS < W-TEMP-KVANT                                           
609101        MOVE SEND-DCS-IDLEVNR-DC TO INLE-DIR-IDLEVNR                      
609201        MOVE DCS-IDLEVNR-DC      TO INLE-DIR-IDKONTO                      
609301     ELSE                                                                 
609401        MOVE DCS-IDLEVNR-DC      TO INLE-DIR-IDLEVNR                      
609501        MOVE SEND-DCS-IDLEVNR-DC TO INLE-DIR-IDKONTO                      
609601     END-IF                                                               
609701     MOVE ZERO             TO INLE-DIR-IDDISTR                            
609801                              INLE-DIR-IDPRODNR                           
609901     MOVE W-SPAR-IDKUNDNR  TO INLE-DIR-IDKUNDNR                           
610001     MOVE W-SPAR-IDKUNDRF  TO INLE-DIR-IDKUNDRF                           
610101     MOVE W-SPAR-IDFAKT    TO INLE-DIR-IDFAKT                             
610201                                                                          
610301     PERFORM IMS-ISRT-WLINLE22                                            
610401     .                                                                    
610501     EJECT                                                                
610601 S08-UPPDATERA-6301 SECTION.                                              
610701                                                                          
610801     MOVE ZERO            TO W-KVKOLLI-MOT                                
610901     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
611001     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
611101     MOVE W-SPAR-IDFAKT   TO W-SEQA-IDFAKT-MIN                            
611201                             W-SEQA-IDFAKT-MAX                            
611301     MOVE W-SPAR-IDKUNDRF TO W-SEQA-IDKUNDRF-MIN                          
611401                             W-SEQA-IDKUNDRF-MAX                          
611501     MOVE W-SPAR-IDKUNDNR TO W-SEQA-IDKUNDNR-MIN                          
611601                             W-SEQA-IDKUNDNR-MAX                          
611701     MOVE W-SPAR-IDKOLLI  TO W-SEQA-IDKOLLI-MIN                           
611801                             W-SEQA-IDKOLLI-MAX                           
611901                                                                          
612001     PERFORM IMS-GU-WLINLD01-FIRST                                        
612101     IF SEGMENT-SAKNAS                                                    
612201        MOVE +1 TO W-KVKOLLI-MOT                                          
612301     END-IF                                                               
612401                                                                          
612501     MOVE W-SPAR-IDFAKT TO W-IDFAKT                                       
612601     MOVE W-SPAR-IDDC   TO W-6301-IDDC                                    
612701     PERFORM IMS-GHU-WL630111                                             
612801     ADD W-KVRADER      TO 6302-KVRADER-MOT                               
612901     ADD W-KVKOLLI-MOT  TO 6302-KVKOLLI-MOT                               
613001     PERFORM IMS-REPL-WL630111                                            
613101     .                                                                    
613201     EJECT                                                                
613301 S09-SKAPA-EKOTRANS-A03 SECTION.                                          
613401                                                                          
613501     IF WS-A03-SKAPAD = JA                                                
613601        PERFORM S091-SKRIV-EKOTRANS-A03                                   
613701     END-IF                                                               
613801                                                                          
613901     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
614001        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
614101        PERFORM IMS-GU-WDB601-SEND                                        
614201     END-IF                                                               
614301                                                                          
614401     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
614501        MOVE WC-CDC-SE    TO EKOTRA03-IDDC-SEND                           
614601        MOVE INL-IDDISTR  TO DIST35-IDDISTR                               
614701                             TEST-IDDISTR                                 
614801        IF DIST35-REFILL-NA                                               
614901           MOVE 'T10'           TO EKOTRA03-KDEKOHT                       
615001           IF DIST35-CDC-NDC41-REFILL                                     
615101           OR DIST35-CDC-NDC43-REFILL                                     
615201           OR DIST35-CDC-NDC44-REFILL                                     
615301           OR DIST35-CDC-NDC45-REFILL                                     
615401           OR DIST35-CDC-NDC46-REFILL                                     
615402           OR DIST35-CDC-NDC47-REFILL                                     
615501              MOVE 53           TO EKOTRA03-IDFTG                         
615601           ELSE                                                           
615701              MOVE 54           TO EKOTRA03-IDFTG                         
615801           END-IF                                                         
615901        ELSE                                                              
616001           IF DIST35-REFILL-NA-JAP                                        
616101              MOVE 'I20'        TO EKOTRA03-KDEKOHT                       
616201              IF DIST35-JAP-NDC41-REFILL                                  
616301              OR DIST35-JAP-NDC43-REFILL                                  
616401              OR DIST35-JAP-NDC44-REFILL                                  
616501                 MOVE 53         TO EKOTRA03-IDFTG                        
616601              ELSE                                                        
616701                 MOVE 54         TO EKOTRA03-IDFTG                        
616801              END-IF                                                      
616901           END-IF                                                         
617001        END-IF                                                            
617101        IF DIST79-DEALER-PRICE                                            
617201          MOVE INL-KDVALISO  TO EKOTRA03-KDVALISO                         
617301        ELSE                                                              
617401          MOVE 'SEK'         TO EKOTRA03-KDVALISO                         
617501        END-IF                                                            
617601     ELSE                                                                 
617701        MOVE 6302-IDDC-SEND  TO EKOTRA03-IDDC-SEND                        
617801        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
617901           AND (DCS-NDC-NA AND DCS-CANADA)                                
618001             MOVE 'T50'      TO EKOTRA03-KDEKOHT                          
618101             MOVE 54         TO EKOTRA03-IDFTG                            
618201             MOVE 'USD'      TO EKOTRA03-KDVALISO                         
618301        ELSE                                                              
618401           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
618501           AND (DCS-NDC-NA AND DCS-USA)                                   
618601             MOVE 'T40'      TO EKOTRA03-KDEKOHT                          
618701             MOVE 53         TO EKOTRA03-IDFTG                            
618801             MOVE 'CAD'      TO EKOTRA03-KDVALISO                         
618901           ELSE                                                           
619001              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
619101              AND (DCS-NDC-NA AND DCS-USA)                                
619201                  MOVE 'T30' TO EKOTRA03-KDEKOHT                          
619301                  MOVE 53    TO EKOTRA03-IDFTG                            
619401                  MOVE 'USD' TO EKOTRA03-KDVALISO                         
619501              END-IF                                                      
619601           END-IF                                                         
619701        END-IF                                                            
619801     END-IF                                                               
619901                                                                          
620001     MOVE 'A03'           TO EKOTRA03-IDPTYP                              
620101     MOVE INL-IDDISTR     TO EKOTRA03-IDDISTR                             
620201     MOVE INL-IDKUNDNR    TO EKOTRA03-IDKUNDNR                            
620301     MOVE 6302-TIFAKT     TO WS-SEKEL-KOLL                                
620401                             WS-EKOA03-AAMMDD                             
620501     IF WS-SEKEL = 9                                                      
620601        MOVE 19           TO WS-EKOA03-SS                                 
620701     ELSE                                                                 
620801        MOVE 20           TO WS-EKOA03-SS                                 
620901     END-IF                                                               
621001     MOVE WS-AAAAMMDD     TO EKOTRA03-DAFAKT                              
621101                                                                          
621201     MOVE ZERO            TO EKOTRA03-IDORDNR7                            
621301     MOVE INL-IDORDNR5    TO EKOTRA03-IDORDNR7                            
621401     MOVE INL-PRARTNTO    TO EKOTRA03-PRARTNTO                            
621501                                                                          
621601     MOVE INL-TIINLINL    TO WS-SEKEL-KOLL                                
621701                             WS-EKOA03-AAMMDD                             
621801     IF WS-SEKEL = 9                                                      
621901        MOVE 19           TO WS-EKOA03-SS                                 
622001     ELSE                                                                 
622101        MOVE 20           TO WS-EKOA03-SS                                 
622201     END-IF                                                               
622301     MOVE WS-AAAAMMDD     TO EKOTRA03-DAINLINL                            
622401                                                                          
622501     MOVE INL-KVAVIS      TO EKOTRA03-KVLEVART                            
622601     MOVE INL-PRKURS      TO EKOTRA03-PRKURS                              
622701     MOVE INL-IDKOLLI     TO EKOTRA03-IDKOLLI                             
622801     MOVE WS-OLD-KVLS     TO EKOTRA03-KVLS-OLD                            
622901     MOVE W-SPAR-IDDC     TO EKOTRA03-IDDC-REC                            
623001     MOVE W-SPAR-IDFAKT   TO EKOTRA03-IDFAKT                              
623101     MOVE W-IDARTNR       TO EKOTRA03-IDARTNR                             
623201     MOVE W-KDPRODSL      TO EKOTRA03-KDPRODSL                            
623301     MOVE W-KDPRODSL-LOC  TO EKOTRA03-KDPSLLOC                            
623401     MOVE SPACE           TO EKOTRA03-FLSLUT                              
623501                                                                          
623601     MOVE JA              TO WS-A03-SKAPAD                                
623701     .                                                                    
623801     EJECT                                                                
623901 S091-SKRIV-EKOTRANS-A03 SECTION.                                         
624001                                                                          
624101     MOVE 'W6030300'        TO FIL-IDPGM IN FIL-WDR801                    
624201     MOVE W-DAGENS-DATUM    TO FIL-TIREGDAT                               
624301     ADD +1                 TO W-TIKLOCK                                  
624401     MOVE W-TIKLOCK         TO FIL-TIKLOCK IN FIL-WDR801                  
624501     ADD +1                 TO W-IDSEKVNR-A03                             
624601     MOVE W-IDSEKVNR-A03    TO FIL-IDSEKVNR IN FIL-WDR801                 
624701     MOVE 'W510'            TO FIL-CT-IDSYSTEM IN FIL-WDR801              
624801     MOVE 'A03'             TO FIL-CT-IDPTYP IN FIL-WDR801                
624901     MOVE ' '               TO FIL-CT-IDVTYP IN FIL-WDR801                
625001     MOVE EKOTRA03-W510A03  TO FIL-WDR801-DATA                            
625101                                                                          
625201     PERFORM IMS-ISRT-WLFILB01                                            
625301                                                                          
625401     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
625501        ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                              
625601        PERFORM IMS-ISRT-WLFILB01                                         
625701     END-PERFORM                                                          
625801                                                                          
625901     MOVE NEJ TO WS-A03-SKAPAD                                            
626001     .                                                                    
626101     EJECT                                                                
626201 S10-OMRAKN-MEDELPRIS-NA SECTION.                                         
626301                                                                          
626401     MOVE W-SPAR-IDDC    TO AVG-IDDC                                      
626501     MOVE SLAG-PRAVCOST  TO AVG-PRAVCOST-OLD                              
626601     MOVE WS-OLD-KVLS    TO AVG-KVLS-OLD                                  
626701     MOVE INL-PRARTNTO   TO AVG-PRARTNTO                                  
626801     MOVE W-KDPRODSL-LOC TO AVG-KDPSLLOC                                  
626802     MOVE W-KDPRODSL     TO AVG-KDPRODSL                                  
626803     MOVE W-IDFKNGRP     TO AVG-IDFKNGRP                                  
626901                                                                          
627001     IF NY-BEFINTLIG-ART                                                  
627101     OR NY-NYUPPLAEGG-ART                                                 
627201        MOVE WS-KDVALISO TO AVG-KDVALISO                                  
627301     END-IF                                                               
627401                                                                          
627501     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
627601        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
627701        PERFORM IMS-GU-WDB601-SEND                                        
627801     END-IF                                                               
627901                                                                          
628001     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
628101        IF DIST79-DEALER-PRICE                                            
628201          MOVE '010'       TO AVG-KDCALL                                  
628301          MOVE 1.0         TO AVG-PRKURS                                  
628401          MOVE 'USD'       TO AVG-KDVALISO                                
628501        ELSE                                                              
628601          MOVE '010'       TO AVG-KDCALL                                  
628701          MOVE INL-PRKURS  TO AVG-PRKURS                                  
628801        END-IF                                                            
628901     ELSE                                                                 
629001        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
629101        AND (DCS-NDC-NA AND DCS-CANADA)                                   
629201          IF DIST79-DEALER-PRICE                                          
629301            MOVE '030'     TO AVG-KDCALL                                  
629401            MOVE 1.0       TO AVG-PRKURS                                  
629501            MOVE 'USD'     TO AVG-KDVALISO                                
629601          ELSE                                                            
629701            MOVE '030'      TO AVG-KDCALL                                 
629801            MOVE ZERO       TO AVG-PRKURS                                 
629901            MOVE 'USD'      TO AVG-KDVALISO                               
630001          END-IF                                                          
630101        ELSE                                                              
630201           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
630301           AND (DCS-NDC-NA AND DCS-USA)                                   
630401             IF DIST79-DEALER-PRICE                                       
630501               MOVE '030'      TO AVG-KDCALL                              
630601               MOVE 1.0         TO AVG-PRKURS                             
630701               MOVE 'USD'       TO AVG-KDVALISO                           
630801             ELSE                                                         
630901               MOVE '030'      TO AVG-KDCALL                              
631001               MOVE ZERO       TO AVG-PRKURS                              
631101               MOVE 'CAD'      TO AVG-KDVALISO                            
631201             END-IF                                                       
631301           ELSE                                                           
631401              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
631501              AND (DCS-NDC-NA AND DCS-USA)                                
631601                IF DIST79-DEALER-PRICE                                    
631701                  MOVE '020'       TO AVG-KDCALL                          
631801                  MOVE 1.0         TO AVG-PRKURS                          
631901                  MOVE 'USD'       TO AVG-KDVALISO                        
632001                ELSE                                                      
632101                  MOVE '020'      TO AVG-KDCALL                           
632201                  MOVE ZERO       TO AVG-PRKURS                           
632301                END-IF                                                    
632401              END-IF                                                      
632501           END-IF                                                         
632601        END-IF                                                            
632701     END-IF                                                               
632801                                                                          
632901     MOVE W-DAGENS-DATUM(1:2) TO AVG-TIAA                                 
632902     MOVE W-DAGENS-DATUM(3:2) TO AVG-TIMM                                 
633001     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
633101                        AVG-WDB6-PCB                                      
633201     IF AVG-KDSVAR = SPACE OR '4'                                         
633301        CONTINUE                                                          
633401     ELSE                                                                 
633501        MOVE MARKUP-IS-MISSING                                            
633601                              TO MOD-TEMFSFEL                             
633701        PERFORM MFS-ROER-EJ-FAELT-UT                                      
633801        PERFORM MFS-ROER-EJ-FAELT-IN                                      
633901        PERFORM IMS-ROLLBACK                                              
634001        COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30301 + 4                     
634101        PERFORM IMS-INSERT-MSG                                            
634201        MOVE ZERO TO RETURN-CODE                                          
634301        GOBACK                                                            
634401     END-IF                                                               
634501     .                                                                    
634601     EJECT                                                                
634701 S10-OMRAKN-MEDELPRIS    SECTION.                                         
634801     MOVE W-SPAR-IDDC    TO AVG-IDDC                                      
634901     MOVE SLAG-PRAVCOST  TO AVG-PRAVCOST-OLD                              
635001     COMPUTE AVG-KVLS-OLD = WS-OLD-KVLS + WS-OLD-KVEFRS                   
635101     MOVE INL-PRARTNTO   TO AVG-PRARTNTO                                  
635201     MOVE W-KDPRODSL-LOC TO AVG-KDPSLLOC                                  
635202     MOVE W-KDPRODSL     TO AVG-KDPRODSL                                  
635203     MOVE W-IDFKNGRP     TO AVG-IDFKNGRP                                  
635301                                                                          
635401     IF NY-BEFINTLIG-ART                                                  
635501     OR NY-NYUPPLAEGG-ART                                                 
635601        MOVE WS-KDVALISO TO AVG-KDVALISO                                  
635701     END-IF                                                               
635801                                                                          
635901     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
636001        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
636101        PERFORM IMS-GU-WDB601-SEND                                        
636201     END-IF                                                               
636301                                                                          
636401     MOVE W-DAGENS-DATUM(1:2) TO AVG-TIAA                                 
636402     MOVE W-DAGENS-DATUM(3:2) TO AVG-TIMM                                 
636501                                                                          
636601     IF SEND-DCS-CDC                                                      
636701**** HÄMTA RÄTT FAKTURAMÅNAD FÖR ATT RÄKNA UT RÄTT AVERAGECOST            
636801        COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                      
636901                                  - INL-DAINLEV                           
637001        MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                    
637101        MOVE WS-FAKTURA-DATUM(3:2) TO AVG-TIAA                            
637102        MOVE WS-FAKTURA-DATUM(5:2) TO AVG-TIMM                            
637201****                                                                      
637301        IF DIST79-DEALER-PRICE                                            
637401          IF NDC-US                                                       
637501            MOVE '010'     TO AVG-KDCALL                                  
637601            MOVE 1.0       TO AVG-PRKURS                                  
637701            MOVE 'USD'     TO AVG-KDVALISO                                
637801          END-IF                                                          
637901        ELSE                                                              
638001          IF NDC-US                                                       
638101            MOVE '010'     TO AVG-KDCALL                                  
638201            MOVE ZERO      TO AVG-PRKURS                                  
638301          END-IF                                                          
638401        END-IF                                                            
638501     END-IF                                                               
638601                                                                          
638701     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
638801                        AVG-WDB6-PCB                                      
638901     IF AVG-KDSVAR = SPACE OR '4'                                         
639001        CONTINUE                                                          
639101     ELSE                                                                 
639201        MOVE MARKUP-IS-MISSING                                            
639301                              TO MOD-TEMFSFEL                             
639401        PERFORM MFS-ROER-EJ-FAELT-UT                                      
639501        PERFORM MFS-ROER-EJ-FAELT-IN                                      
639601        PERFORM IMS-ROLLBACK                                              
639701        COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30301 + 4                     
639801        PERFORM IMS-INSERT-MSG                                            
639901        MOVE ZERO TO RETURN-CODE                                          
640001        GOBACK                                                            
640101     END-IF                                                               
640201     .                                                                    
640301     EJECT                                                                
640401 S11-SKAPA-LEVANM-TRANS SECTION.                                          
640501                                                                          
640601     MOVE 6302-TIFAKT       TO 6306-TIFAKT                                
640701     MOVE 6302-IDFAKT       TO 6306-IDFAKT                                
640801     MOVE W-SPAR-IDDC       TO 6306-IDDC-REC                              
640901     MOVE SEND-WS-IDDC      TO 6306-IDDC-SEND                             
641001     MOVE 6302-IDDC-LEV     TO 6306-IDDC-LEV                              
641101                                                                          
641201     PERFORM IMS-GHU-WL630511                                             
641301     IF SEGMENT-FINNS                                                     
641401        MOVE 6302-IDDISTR       TO 6308-IDDISTR                           
641501                                   TEST-IDDISTR                           
641601                                   DIST35-IDDISTR                         
641701        MOVE 6302-IDKUNDNR      TO 6308-IDKUNDNR                          
641801        MOVE 6302-IDFAKT        TO 6308-IDRAPPNR                          
641901                                                                          
642001        IF NY-BEFINTLIG-ART OR NY-NYUPPLAEGG-ART                          
642101           MOVE ZERO              TO 6308-KDFRAKT                         
642201           MOVE W-SPAR-IDKUNDRF   TO 6308-IDKUNDRF                        
642301           MOVE W-SPAR-IDKOLLI    TO 6308-IDKOLLI                         
642401        ELSE                                                              
642501           MOVE INL-KDFRAKT       TO 6308-KDFRAKT                         
642601           MOVE INL-IDKUNDRF      TO 6308-IDKUNDRF                        
642701           MOVE INL-IDKOLLI       TO 6308-IDKOLLI                         
642801           MOVE INL-IDDISTR       TO 6308-IDDISTR                         
642901                                     TEST-IDDISTR                         
643001                                     DIST35-IDDISTR                       
643101           MOVE INL-IDKUNDNR      TO 6308-IDKUNDNR                        
643201        END-IF                                                            
643301                                                                          
643401**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA.INL-PRARTNTO          
643501**- KOMMER FRÅN BILL-IT OCH ÄR I LOCAL VALUTA .REFILLDISTRIKT             
643601**- 8141-8143, 8151,8541-8543, 8551.TRANSFER-DISTR.USA-CAN/CAN-USA        
643701**- 874X, 8751 ANVÄNDER PRAVCOST OCH BERÖRS EJ ENLIGT SUSSI 2/2-05        
643801**- 9111 DC71 ANVÄNDER PRAVCOST OCH CNY MEN NYUPPLÄGG KOD 21 HAR          
643901**- STANDARDPRIS OCH SEK.ÄNDRING 2016-06 KINA EXPORT.                     
644001**- ÄNDRING 2018-08 GLOBAL EXPORT                                         
644101**- OBS! I DEN EKONOMISKA BOKNINGEN DÅ INGEN TF-FAKTURA SKAPAS.           
644201**- OBS! MAN HÄMTAR RÄTT PRIS OCH VALUTA FRÅN W335PRIS FÖR LEV.ANM        
644301**- 9211 DC4X ANVÄNDER PRAVCOST OCH USD MEN NYUPPLÄGG KOD 21 HAR          
644401**- STANDARDPRIS OCH SEK.SE 9111.                                         
644501                                                                          
644601        IF DIST35-US-CAN-TRANSFER OR DIST35-CAN-US-TRANSFER               
644701            MOVE INL-PRARTNTO    TO 6308-PRARTBTO                         
644801            MOVE ZERO            TO 6308-PRARTBTO-LOC                     
644901            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
645001        ELSE                                                              
645101          IF DIST79-DEALER-PRICE                                          
645201            MOVE INL-PRARTNTO    TO 6308-PRARTBTO-LOC                     
645301            MOVE ZERO            TO 6308-PRARTBTO                         
645401            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
645501          ELSE                                                            
645601            IF DIST35-NDCCN-CDC-REFILL OR                                 
645701               DIST35-NDCUS-CDC-REFILL                                    
645801                                                                          
645901*--- OBS! OM INL-PRARTNTO = 0 SÅ ÄR WDK711-PRAVCOST = 0                   
646001*--- FAKTURAN HÄMTAR PRIS FRÅN ORDERRADEN(FIX PRIS)WDL6 FRÅN K711         
646101*--- ------------------------------------------------------------         
646201              MOVE INL-PRARTNTO    TO 6308-PRARTBTO                       
646301              MOVE ZERO            TO 6308-PRARTBTO-LOC                   
646401              IF 6308-KDANMORS = '21'                                     
646501                MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                
646601                MOVE ZERO                 TO 6308-PRARTBTO-LOC            
646701                MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                
646801              ELSE                                                        
646901                IF DIST35-NDCCN-CDC-REFILL                                
647001                  MOVE 'CNY'       TO 6308-KDVALISO                       
647101                ELSE                                                      
647201                  MOVE 'USD'       TO 6308-KDVALISO                       
647301                END-IF                                                    
647401              END-IF                                                      
647501            ELSE                                                          
647601              IF 6308-KDANMORS = '21'                                     
647701                MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                
647801                MOVE ZERO                 TO 6308-PRARTBTO-LOC            
647901                MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                
648001              ELSE                                                        
648101                MOVE INL-PRARTNTO    TO 6308-PRARTBTO                     
648201                MOVE ZERO            TO 6308-PRARTBTO-LOC                 
648301                MOVE 'SEK'           TO 6308-KDVALISO                     
648401              END-IF                                                      
648501            END-IF                                                        
648601          END-IF                                                          
648701        END-IF                                                            
648801                                                                          
648901        MOVE W-IDARTNR         TO 6308-IDARTNR                            
649001        MOVE ZERO              TO 6308-IDRADNR                            
649101        MOVE W-DAGENS-DATUM    TO 6308-TILEVANM                           
649201        IF 6308-KDANMORS = '43'                                           
649301           MOVE 2              TO 6308-KDEMBLEV                           
649401        ELSE                                                              
649501           MOVE ZERO           TO 6308-KDEMBLEV                           
649601        END-IF                                                            
649701                                                                          
649801        PERFORM IMS-ISRT-WL630521                                         
649901        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
650001           ADD +1 TO 6308-IDRADNR                                         
650101           PERFORM IMS-ISRT-WL630521                                      
650201        END-PERFORM                                                       
650301     ELSE                                                                 
650401        MOVE 'N'            TO 6306-FLKLAR                                
650501                               6306-FLDIRLEV                              
650601        PERFORM IMS-ISRT-WL630511                                         
650701        MOVE 6302-IDDISTR   TO 6308-IDDISTR                               
650801                               TEST-IDDISTR                               
650901                               DIST35-IDDISTR                             
651001        MOVE 6302-IDKUNDNR  TO 6308-IDKUNDNR                              
651101        MOVE 6302-IDFAKT    TO 6308-IDRAPPNR                              
651201                                                                          
651301        IF NY-BEFINTLIG-ART OR NY-NYUPPLAEGG-ART                          
651401           MOVE ZERO              TO 6308-KDFRAKT                         
651501           MOVE W-SPAR-IDKUNDRF   TO 6308-IDKUNDRF                        
651601           MOVE W-SPAR-IDKOLLI    TO 6308-IDKOLLI                         
651701        ELSE                                                              
651801           MOVE INL-IDDISTR       TO 6308-IDDISTR                         
651901                                     TEST-IDDISTR                         
652001                                     DIST35-IDDISTR                       
652101           MOVE INL-IDKUNDNR      TO 6308-IDKUNDNR                        
652201           MOVE INL-KDFRAKT       TO 6308-KDFRAKT                         
652301           MOVE INL-IDKUNDRF      TO 6308-IDKUNDRF                        
652401           MOVE INL-IDKOLLI       TO 6308-IDKOLLI                         
652501        END-IF                                                            
652601                                                                          
652701**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA.INL-PRARTNTO          
652801**- KOMMER FRÅN BILL-IT OCH ÄR I LOCAL VALUTA .REFILLDISTRIKT             
652901**- 8141-8143, 8151,8541-8543, 8551.TRANSFER-DISTR.USA-CAN/CAN-USA        
653001**- 874X, 8751 ANVÄNDER PRAVCOST OCH BERÖRS EJ ENLIGT SUSSI 2/2-05        
653101                                                                          
653201        IF DIST35-US-CAN-TRANSFER OR DIST35-CAN-US-TRANSFER               
653301            MOVE INL-PRARTNTO    TO 6308-PRARTBTO                         
653401            MOVE ZERO            TO 6308-PRARTBTO-LOC                     
653501            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
653601        ELSE                                                              
653701          IF DIST79-DEALER-PRICE                                          
653801            MOVE INL-PRARTNTO    TO 6308-PRARTBTO-LOC                     
653901            MOVE ZERO            TO 6308-PRARTBTO                         
654001            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
654101          ELSE                                                            
654201            IF DIST35-NDCCN-CDC-REFILL OR                                 
654301               DIST35-NDCUS-CDC-REFILL                                    
654401                                                                          
654501              MOVE INL-PRARTNTO    TO 6308-PRARTBTO                       
654601              MOVE ZERO            TO 6308-PRARTBTO-LOC                   
654701              IF 6308-KDANMORS = '21'                                     
654801                MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                
654901                MOVE ZERO                 TO 6308-PRARTBTO-LOC            
655001                MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                
655101              ELSE                                                        
655201                IF DIST35-NDCCN-CDC-REFILL                                
655301                  MOVE 'CNY'         TO 6308-KDVALISO                     
655401                ELSE                                                      
655501                  MOVE 'USD'         TO 6308-KDVALISO                     
655601                END-IF                                                    
655701              END-IF                                                      
655801            ELSE                                                          
655901              IF 6308-KDANMORS = '21'                                     
656001                MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                
656101                MOVE ZERO                 TO 6308-PRARTBTO-LOC            
656201                MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                
656301              ELSE                                                        
656401                MOVE INL-PRARTNTO    TO 6308-PRARTBTO                     
656501                MOVE ZERO            TO 6308-PRARTBTO-LOC                 
656601                MOVE 'SEK'           TO 6308-KDVALISO                     
656701              END-IF                                                      
656801            END-IF                                                        
656901          END-IF                                                          
657001        END-IF                                                            
657101                                                                          
657201        MOVE W-IDARTNR      TO 6308-IDARTNR                               
657301        MOVE ZERO           TO 6308-IDRADNR                               
657401        MOVE W-DAGENS-DATUM TO 6308-TILEVANM                              
657501        IF 6308-KDANMORS = '43'                                           
657601           MOVE 2           TO 6308-KDEMBLEV                              
657701        ELSE                                                              
657801           MOVE ZERO        TO 6308-KDEMBLEV                              
657901        END-IF                                                            
658001                                                                          
658101        PERFORM IMS-ISRT-WL630521                                         
658201        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
658301           ADD +1 TO 6308-IDRADNR                                         
658401           PERFORM IMS-ISRT-WL630521                                      
658501        END-PERFORM                                                       
658601     END-IF                                                               
658701     .                                                                    
658801     EJECT                                                                
658901 S13-SKAPA-NDC-HIST-SANDANDE SECTION.                                     
659001                                                                          
659101     MOVE DCS-IDLEVNR-DC     TO INL-IDLEVNR                               
659201     MOVE W-IDARTNR         TO ART-IDARTNR                                
659301     PERFORM IMS-ISRT-WLINLC01                                            
659401     MOVE FUNCTION CURRENT-DATE (1:8) TO W-TIAAAAMMDDTTMMSSTH-DATE        
659501     ACCEPT W-TIAAAAMMDDTTMMSSTH-TIME FROM TIME                           
659601                                                                          
659701     COMPUTE W-DAINLEV = 9999999999999999                                 
659801                       - W-TIAAAAMMDDTTMMSSTH                             
659901     END-COMPUTE                                                          
660001                                                                          
660101     MOVE W-DAINLEV         TO INL-DAINLEV                                
660201     MOVE ZERO              TO INL-ADLAGOMR                               
660301                               INL-ADGANG                                 
660401                               INL-ADPLATS                                
660501     MOVE SPACE             TO INL-FLMAKUL                                
660601                               INL-FLSKAKOL                               
660701     MOVE 'N'               TO INL-FLPRIO                                 
660702                               INL-FLTULLST                               
660801     MOVE SEND-WS-IDDC      TO INL-IDDC                                   
660901     MOVE SPACE             TO INL-IDDC-LEV                               
661001     MOVE ZERO              TO INL-IDLOPNRM                               
661101     MOVE W-SPAR-IDFAKT     TO INL-IDFAKT                                 
661201     MOVE 6302-IDDISTR      TO INL-IDDISTR                                
661301     MOVE 6302-IDKUNDNR     TO INL-IDKUNDNR                               
661401     MOVE W-SPAR-IDKUNDRF   TO INL-IDKUNDRF                               
661501     MOVE W-SPAR-IDKOLLI    TO INL-IDKOLLI                                
661601     MOVE 'R34'             TO INL-IDPTYP                                 
661701     IF MID-IDUSER-003 = ALL '+' OR SPACE                                 
661801        MOVE SPACE          TO INL-IDUSER-003                             
661901     ELSE                                                                 
662001        MOVE WS-IDUSER-003  TO INL-IDUSER-003                             
662101     END-IF                                                               
662201     MOVE ZERO              TO INL-KDFRAKT                                
662301     MOVE SPACE             TO INL-KDKOLLI                                
662401                               INL-KDVALISO                               
662501                               INL-ADINLOMR                               
662601                               INL-IDANALYS                               
662701                               INL-IDKST                                  
662801     MOVE ZERO              TO INL-KVANTMOT                               
662901                               INL-KVART-SKROT                            
663001                               INL-KDRT                                   
663101                               INL-IDKONTO                                
663201     SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                                  
663301                      GIVING   INL-KVAVIS                                 
663401     MOVE ZERO              TO INL-PRARTNTO                               
663501                               INL-PRKURS                                 
663601                               INL-TIBERANK                               
663701                               INL-TIINLINL                               
663801                               INL-TIINLMOT                               
663901                               INL-TIINLMTI                               
664001                               INL-TIINLITI                               
664301                               INL-KVTULRET                               
664302                               INL-KVRETUR                                
664401                               INL-KDAVVANT                               
664501                               INL-TIAVIDAT                               
664601                                                                          
664701     PERFORM IMS-ISRT-WLINLC11                                            
664801     PERFORM UNTIL INSERT-OK                                              
664901       SUBTRACT 1      FROM W-DAINLEV                                     
665001       MOVE W-DAINLEV  TO   INL-DAINLEV                                   
665101       PERFORM IMS-ISRT-WLINLC11                                          
665201     END-PERFORM                                                          
665301     .                                                                    
665401     EJECT                                                                
665501 S15-SKAPA-SDC-NDC-HIST-MOT SECTION.                                      
665601                                                                          
665701     MOVE W-IDARTNR-INM TO ART-IDARTNR                                    
665801     PERFORM IMS-ISRT-WLINLC01                                            
665901                                                                          
666001     MOVE FUNCTION CURRENT-DATE (1:8) TO W-TIAAAAMMDDTTMMSSTH-DATE        
666101     ACCEPT W-TIAAAAMMDDTTMMSSTH-TIME FROM TIME                           
666201                                                                          
666301     COMPUTE W-DAINLEV = 9999999999999999                                 
666401                       - W-TIAAAAMMDDTTMMSSTH                             
666501     END-COMPUTE                                                          
666601                                                                          
666701     MOVE W-DAINLEV           TO INL-DAINLEV                              
666801     MOVE W-ADLAGOMR-SPAR     TO INL-ADLAGOMR                             
666901     MOVE W-ADGANG-SPAR       TO INL-ADGANG                               
667001     MOVE W-ADPLATS-SPAR      TO INL-ADPLATS                              
667101     MOVE W-SPAR-IDDC         TO INL-IDDC                                 
667201     MOVE SPACE               TO INL-IDDC-LEV                             
667301     MOVE W-SPAR-IDFAKT       TO INL-IDFAKT                               
667401                                                                          
667501     MOVE 6302-IDDISTR        TO INL-IDDISTR                              
667601                                 DIST35-IDDISTR                           
667701     MOVE W-SPAR-IDKUNDNR     TO INL-IDKUNDNR                             
667801     MOVE W-SPAR-IDKUNDRF     TO INL-IDKUNDRF                             
667901     MOVE W-SPAR-IDKOLLI      TO INL-IDKOLLI                              
668001     MOVE 'R32'               TO INL-IDPTYP                               
668101     IF MID-IDUSER-003 = ALL '+' OR SPACE                                 
668201        MOVE SPACE            TO INL-IDUSER-003                           
668301     ELSE                                                                 
668401        MOVE WS-IDUSER-003    TO INL-IDUSER-003                           
668501     END-IF                                                               
668601     MOVE WS-KVANTMOT-INM-NUM TO INL-KVANTMOT                             
668701     MOVE WS-KVSKROT-INM-NUM  TO INL-KVART-SKROT                          
668801     MOVE ZERO                TO INL-KVAVIS                               
668901     MOVE WS-FIXAD-PRARTNTO   TO INL-PRARTNTO                             
669001     IF DIST35-RETUR                                                      
669101        MOVE +8               TO INL-KDRT                                 
669201     ELSE                                                                 
669301        MOVE ZERO             TO INL-KDRT                                 
669401     END-IF                                                               
669501     MOVE ZERO                TO INL-IDLOPNRM                             
669601                                 INL-KDFRAKT                              
669701                                 INL-PRKURS                               
669801                                 INL-TIBERANK                             
670001                                 INL-IDKONTO                              
670302                                 INL-KVTULRET                             
670303                                 INL-KVRETUR                              
670401                                 INL-KDAVVANT                             
670501                                 INL-TIAVIDAT                             
670601     MOVE SPACE               TO INL-KDVALISO                             
670701                                 INL-KDKOLLI                              
670801                                 INL-IDKST                                
670901                                 INL-FLMAKUL                              
671001                                 INL-FLSKAKOL                             
671101                                 INL-ADINLOMR                             
671201                                 INL-IDANALYS                             
671301     MOVE 'N'                 TO INL-FLPRIO                               
671302                                 INL-FLTULLST                             
671401     MOVE W-TIME-N            TO AKTUELL-TID                              
671501     IF DCS-NDC-NA OR DCS-NDC-PF                                          
671601        PERFORM S18-FIXA-LOKALTID                                         
671701        MOVE MSGI-TILOKDAT  TO INL-TIINLINL                               
671801                               INL-TIINLMOT                               
671901        MOVE MSGI-TILOKTID  TO INL-TIINLMTI                               
672001                               INL-TIINLITI                               
672101     ELSE                                                                 
672201        MOVE W-DAGENS-DATUM TO INL-TIINLINL                               
672301                               INL-TIINLMOT                               
672401        MOVE AKTUELL-TTMM   TO INL-TIINLITI                               
672501                               INL-TIINLMTI                               
672601     END-IF                                                               
672701                                                                          
672801     IF NY-BEFINTLIG-ART OR NY-NYUPPLAEGG-ART                             
672901        MOVE WS-KDVALISO    TO INL-KDVALISO                               
673001     END-IF                                                               
673101                                                                          
673201     PERFORM IMS-ISRT-WLINLC11                                            
673301     PERFORM UNTIL INSERT-OK                                              
673401       SUBTRACT 1      FROM W-DAINLEV                                     
673501       MOVE W-DAINLEV  TO   INL-DAINLEV                                   
673601       PERFORM IMS-ISRT-WLINLC11                                          
673701     END-PERFORM                                                          
673801     .                                                                    
673901     EJECT                                                                
674001 S16-PRIS-TILLAMPNING SECTION.                                            
674101                                                                          
674201*--- KOD 21 FÖR DISTR 9111 OCH 9211 EXP-TO-CDC, 914X CN-TO-US             
674301     MOVE ZERO   TO WS-6308-TF-PRARTNTO                                   
674401     MOVE SPACE  TO WS-6308-TF-KDVALISO                                   
674501                                                                          
674601     MOVE NEJ TO WS-FLYGORDER                                             
674701                 WS-BAATORDER                                             
674801                                                                          
674901     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
675001        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
675101        PERFORM IMS-GU-WDB601-SEND                                        
675201     END-IF                                                               
675301                                                                          
675401     IF SEND-DCS-DDC                                                      
675501        MOVE WC-CDC-SE   TO W-IDDC-WDB3                                   
675601                            W-IDDC-WDB3-DEF                               
675701     ELSE                                                                 
675801        MOVE 6302-IDDC-SEND TO W-IDDC-WDB3                                
675901                               W-IDDC-WDB3-DEF                            
676001     END-IF                                                               
676101     MOVE 6302-IDDISTR   TO W-IDDISTR-WDB3                                
676201                            W-IDDISTR-WDB3-DEF                            
676301     MOVE 6302-IDKUNDNR  TO W-IDKUNDNR-WDB3                               
676401                                                                          
676501     PERFORM IMS-GU-WDB301                                                
676601     IF SEGMENT-FINNS                                                     
676701        IF W-KDFRAKT = DC-KDGENFRA-VOR                                    
676801           MOVE JA TO WS-FLYGORDER                                        
676901        ELSE                                                              
677001           MOVE NEJ TO WS-FLYGORDER                                       
677101        END-IF                                                            
677201     END-IF                                                               
677301                                                                          
677401     MOVE 1                   TO PRIS-KDCALL                              
677501     MOVE 'W6030300'          TO PRIS-IDPGM                               
677601     MOVE W-IDARTNR           TO PRIS-IDARTNR                             
677701     MOVE 6302-IDDISTR        TO PRIS-IDDISTR                             
677801     MOVE 6302-IDDISTR        TO DIST35-IDDISTR                           
677901     MOVE SEND-WS-IDDC        TO PRIS-IDDC                                
678001     MOVE 6302-IDKUNDNR       TO PRIS-IDKUNDNR                            
678101     IF WS-FLYGORDER = JA                                                 
678201        MOVE +1               TO PRIS-KDORDKL                             
678301     ELSE                                                                 
678401        MOVE +4               TO PRIS-KDORDKL                             
678501     END-IF                                                               
678601     MOVE +1                  TO PRIS-KVBEART                             
678701     MOVE NEJ                 TO PRIS-FLINVEST                            
678801                                                                          
678901     CALL W335PRIS USING PRIS-W335PRIS ARTC-PCB                           
679001                                   PRIS-WDK7-PCB GMTA-PCB                 
679101                                   BETA-PCB GPRIA-PCB GPRIB-PCB           
679201                                   PRIS-COST-WDK6-PCB                     
679301                                   PRIS-COST-WDK7-PCB                     
679401                                   PRIS-COST-WDF1-PCB                     
679501                                   PRIS-COST-9305-PCB                     
679601                                   PRIS-COST-WDK72-PCB                    
679701                                   PRIS-COST-WDB6-PCB                     
679901                                                                          
680001     IF PRIS-KDSVAR = SPACE                                               
680101        MOVE PRIS-PRARTNTO    TO WS-FIXAD-PRARTNTO                        
680201                                                                          
680301        IF PRIS-PRAVCOST > ZERO                                           
680401          MOVE PRIS-PRAVCOST  TO WS-6308-TF-PRARTNTO                      
680501        ELSE                                                              
680601          MOVE PRIS-PRARTNTO  TO WS-6308-TF-PRARTNTO                      
680701        END-IF                                                            
680801        MOVE PRIS-KDVALISO    TO WS-6308-TF-KDVALISO                      
680901                                                                          
681001     ELSE                                                                 
681101        MOVE 'FEL RETURKOD FRÅN W335PRIS' TO FELTEXT                      
681201        DISPLAY FELTEXT                                                   
681301        CALL FELLOG                                                       
681401     END-IF                                                               
681501     .                                                                    
681601     EJECT                                                                
681701 S17-NDC-OOVER-UNDER SECTION.                                             
681801                                                                          
681901     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
682001        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
682101        PERFORM IMS-GU-WDB601-SEND                                        
682201     END-IF                                                               
682301                                                                          
682401     IF SEND-DCS-CDC OR SEND-DCS-DDC OR                                   
682501        (NDC-US AND DIST35-NDCCN-NDCUS-REFILL)                            
682601                                                                          
682701        IF W-KVAVIS < W-TEMP-KVANT                                        
682801           MOVE '11' TO 6308-KDANMORS                                     
682901                        EKOTRA03-KDANMORS                                 
683001        ELSE                                                              
683101           MOVE '00' TO 6308-KDANMORS                                     
683201                        EKOTRA03-KDANMORS                                 
683301        END-IF                                                            
683401        MOVE W-DIFF-KVANT TO 6308-KVLEVANM                                
683501        PERFORM S11-SKAPA-LEVANM-TRANS                                    
683601     ELSE                                                                 
683701                                                                          
683801        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
683901           AND (DCS-NDC-NA AND DCS-CANADA)                                
684001        OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                          
684101           AND (DCS-NDC-NA AND DCS-USA)                                   
684201           IF W-KVAVIS < W-TEMP-KVANT                                     
684301              MOVE '11' TO 6308-KDANMORS                                  
684401                           EKOTRA03-KDANMORS                              
684501           ELSE                                                           
684601              MOVE '00' TO 6308-KDANMORS                                  
684701                           EKOTRA03-KDANMORS                              
684801           END-IF                                                         
684901           MOVE W-DIFF-KVANT TO 6308-KVLEVANM                             
685001           PERFORM S11-SKAPA-LEVANM-TRANS                                 
685101        ELSE                                                              
685201                                                                          
685301           IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                          
685401           AND (DCS-NDC-NA AND DCS-USA)                                   
685501                                                                          
685601              IF W-KVAVIS < W-TEMP-KVANT                                  
685701                 MOVE '11' TO 6308-KDANMORS                               
685801                              EKOTRA03-KDANMORS                           
685901              ELSE                                                        
686001                 MOVE '00' TO 6308-KDANMORS                               
686101                              EKOTRA03-KDANMORS                           
686201              END-IF                                                      
686301                                                                          
686401              MOVE SEND-WS-IDDC TO W-IDDC                                 
686501              PERFORM IMS-GET-WDK711                                      
686601              IF MID-CMD-IN(INDX) = 'DAM'                                 
686701                 COMPUTE SLAG-KVLS = SLAG-KVLS                            
686801                                     + (W-KVAVIS                          
686901                                     - W-KVANTMOT(INDX)                   
687001                                     - W-KVSKROT (INDX))                  
687101                 END-COMPUTE                                              
687201                 COMPUTE LOGG-KVART-SALDO =                               
687301                                       (W-KVAVIS                          
687401                                     - W-KVANTMOT(INDX)                   
687501                                     - W-KVSKROT (INDX))                  
687601                 END-COMPUTE                                              
687701              ELSE                                                        
687801                 COMPUTE SLAG-KVLS = SLAG-KVLS                            
687901                                     + (W-KVAVIS                          
688001                                     - W-KVANTMOT(INDX))                  
688101                 END-COMPUTE                                              
688201                 COMPUTE LOGG-KVART-SALDO =                               
688301                                       (W-KVAVIS                          
688401                                     - W-KVANTMOT(INDX))                  
688501                 END-COMPUTE                                              
688601              END-IF                                                      
688701              PERFORM S21-SALDOLOGG-DATA                                  
688801              MOVE SLAG-IDDC            TO LOGG-IDDC                      
688901              MOVE 'MISC'               TO LOGG-IDHUVTYP                  
689001              MOVE 'R34'                TO LOGG-IDSUBTYP                  
689101              MOVE SLAG-KVLS            TO LOGG-KVLS                      
689201              MOVE SLAG-KVAKS-SDC       TO LOGG-KVAKS                     
689301              MOVE '+'                  TO LOGG-IDTECKEN-KVLS             
689401              MOVE SLAG-KVEFRS          TO LOGG-KVEFRS                    
689501              MOVE SLAG-KVAKS-PAV       TO LOGG-KVAKS-PAV                 
689601              MOVE SPACE                TO LOGG-IDTECKEN-KVAKS-PAV        
689701              MOVE SPACE                TO LOGG-IDTECKEN-KVAKS            
689801              MOVE SPACE                TO LOGG-IDTECKEN-KVEFRS           
689901              PERFORM S22-ISRT-SALDOLOGG                                  
690001              PERFORM IMS-REPL-WDK711                                     
690101                                                                          
690201              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
690301              PERFORM S13-SKAPA-NDC-HIST-SANDANDE                         
690401           END-IF                                                         
690501        END-IF                                                            
690601     END-IF                                                               
690701     .                                                                    
690801     EJECT                                                                
690901 S12-SKAPA-BILLIT-TRANS SECTION.                                          
691001                                                                          
691101     MOVE 6302-TIFAKT     TO WS-BILLIT-KOLL                               
691201                             WS-BILLIT-AAMMDD                             
691301     IF WS-BILLIT-SEKEL = 9                                               
691401        MOVE 19           TO WS-BILLIT-SS                                 
691501     ELSE                                                                 
691601        MOVE 20           TO WS-BILLIT-SS                                 
691701     END-IF                                                               
691801     MOVE WS-BILLIT-AAAAMMDD   TO FILC4-DAREGDAT                          
691901     MOVE W-IDARTNR       TO FILC4-IDARTNR                                
692001     MOVE W-SPAR-IDFAKT   TO FILC4-IDFAKT                                 
692101     MOVE W-SPAR-IDKUNDNR TO FILC4-IDKUNDNR                               
692201     MOVE W-SPAR-IDKUNDRF TO FILC4-IDKUNDRF                               
692301     MOVE W-SPAR-IDKOLLI  TO FILC4-IDKOLLI                                
692401     MOVE W-KVAVIS        TO FILC4-KVAVIS                                 
692501     MOVE W-TEMP-KVANT    TO FILC4-KVANTMOT                               
692601     IF W-KVAVIS > W-TEMP-KVANT                                           
692701        MOVE '11'         TO FILC4-IDDC-SEND                              
692801        MOVE W-SPAR-IDDC  TO FILC4-IDDC-REC                               
692901     ELSE                                                                 
693001        MOVE W-SPAR-IDDC TO FILC4-IDDC-SEND                               
693101        MOVE '11'        TO FILC4-IDDC-REC                                
693201     END-IF                                                               
693301                                                                          
693401     MOVE FILC4-W61227      TO FILC4-FIL-WDR301-DATA                      
693501     ACCEPT W-TID FROM TIME                                               
693601     IF W-TID = FILC4-FIL-TIKLOCK                                         
693701        ADD +1              TO FILC4-FIL-IDSEKVNR                         
693801     ELSE                                                                 
693901        MOVE W-TID          TO FILC4-FIL-TIKLOCK                          
694001        MOVE +1             TO FILC4-FIL-IDSEKVNR                         
694101     END-IF                                                               
694201** LASSI *-MÄRKER DETTA TILLS BILL-IT ÄR KLAR ATT TA EMOT.                
694301** IDAG LÄGGS DESSA UPP PÅ R3 MEN ALDRIG TAS HAND OM/RENSAS!              
694401*    PERFORM IMS-ISRT-WLFILC4                                             
694501     .                                                                    
694601     EJECT                                                                
694701 S18-FIXA-LOKALTID SECTION.                                               
694801                                                                          
694901     MOVE '011'            TO MSGI-KDCALL                                 
695001     MOVE W-SPAR-IDDC      TO WS-IDDC-TID                                 
695101     MOVE WS-IDDC-KOLL     TO MSGI-IDUSER                                 
695201     MOVE W-DAGENS-DATUM   TO MSGI-TILOKDAT                               
695301     MOVE AKTUELL-TTMM     TO MSGI-TILOKTID                               
695401                                                                          
695501     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
695601     IF MSGI-KDSVAR = 'F'                                                 
695701        MOVE 'FEL RETURKOD FRÅN WMSGINIT' TO FELTEXT                      
695801        DISPLAY FELTEXT                                                   
695901        CALL FELLOG                                                       
696001     END-IF                                                               
696101     .                                                                    
696201     EJECT                                                                
696301 S19-EV-RO-TACKNING SECTION.                                              
696401                                                                          
696501     IF SLAG-KVROS-BULK > ZERO                                            
696601     OR SLAG-KVROS-DAG > ZERO                                             
696701        IF SLAG-KDLEVSP = ZERO                                            
696801           MOVE W-IDARTNR      TO 4506-IDARTNR                            
696901           MOVE +1             TO 4506-KDTAKORS                           
697001           MOVE WS-RO-KVANTMOT TO 4506-KVANTMOT                           
697101           MOVE W-SPAR-IDDC    TO 4505-IDDC                               
697201           PERFORM IMS-ISRT-4506                                          
697301           MOVE SPACE          TO 4506-WDGX4506                           
697401        END-IF                                                            
697501     END-IF                                                               
697601     .                                                                    
697701     EJECT                                                                
697801 S19-EV-RO-TACKNING-CDC SECTION.                                          
697901                                                                          
698001     IF CLAG-KVROS > ZERO                                                 
698101        IF CLAG-KDLEVSP = ZERO                                            
698201           MOVE W-IDARTNR      TO 4506-IDARTNR                            
698301           MOVE +1             TO 4506-KDTAKORS                           
698401           MOVE WS-RO-KVANTMOT TO 4506-KVANTMOT                           
698501           MOVE W-SPAR-IDDC    TO 4505-IDDC                               
698601           PERFORM IMS-ISRT-4506                                          
698701           MOVE SPACE          TO 4506-WDGX4506                           
698801        END-IF                                                            
698901     END-IF                                                               
699001     .                                                                    
699101     EJECT                                                                
699201 S20-SKAPA-REFILLTRANS SECTION.                                           
699301                                                                          
699401     MOVE W-IDARTNR           TO FILC-IDARTNR                             
699501     MOVE WS-KVANTMOT-INM-NUM TO FILC-KVLEVANM                            
699601     MOVE WS-KVSKROT-INM-NUM  TO FILC-KVSKROT                             
699701     MOVE SEND-WS-IDDC        TO FILC-IDDC-SEND                           
699801     MOVE W-SPAR-IDDC         TO FILC-IDDC-REC                            
699901     MOVE W-DAGENS-DATUM      TO FILC-TILEVANM                            
700001     MOVE FILC-W61236         TO FILC-FIL-WDR301-DATA                     
700101     ACCEPT W-TID FROM TIME                                               
700201     IF W-TID = FILC-FIL-TIKLOCK                                          
700301        ADD +1                TO FILC-FIL-IDSEKVNR                        
700401     ELSE                                                                 
700501        MOVE W-TID            TO FILC-FIL-TIKLOCK                         
700601        MOVE +1               TO FILC-FIL-IDSEKVNR                        
700701     END-IF                                                               
700801     PERFORM IMS-ISRT-WLFILC                                              
700901     .                                                                    
701001     EJECT                                                                
701101 S21-SALDOLOGG-DATA SECTION.                                              
701201     ACCEPT WS-TID                   FROM TIME                            
701301     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
701401     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
701501     MOVE W-IDARTNR          TO LOGG-IDARTNR                              
701601     MOVE 9                  TO LOGG-IDSEKVNR                             
701701     MOVE 'INBO'             TO LOGG-IDHUVTYP                             
701801     MOVE 'R32'              TO LOGG-IDSUBTYP                             
701901     MOVE 'W6030300'         TO LOGG-IDPGM                                
702001     MOVE '6303'             TO LOGG-IDTRANS                              
702101     MOVE MSG-SIGNON-USERID  TO LOGG-IDUSER                               
702201     MOVE SPACE              TO LOGG-REF                                  
702301     MOVE W-SPAR-IDFAKT      TO LOGG-IDFAKT                               
702401     MOVE W-SPAR-IDKUNDRF    TO LOGG-IDKUNDRF                             
702501     MOVE W-SPAR-IDKUNDNR    TO LOGG-IDKUNDNR                             
702601     MOVE '00000000'         TO LOGG-DAREGDAT-LADD                        
702701     .                                                                    
702801     EJECT                                                                
702901 S22-ISRT-SALDOLOGG SECTION.                                              
703001     PERFORM IMS-ISRT-WDL901                                              
703101     IF SEGMENT-FINNS-REDAN                                               
703201       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
703301         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
703401         PERFORM IMS-ISRT-WDL901                                          
703501       END-PERFORM                                                        
703601     END-IF                                                               
703701     .                                                                    
703801     EJECT                                                                
703901                                                                          
704001 S30-UPPDATERA-WDJ9 SECTION.                                              
704101                                                                          
704201     PERFORM IMS-GU-LOCB01                                                
704301     IF SEGMENT-SAKNAS                                                    
704401       MOVE W-IDARTNR TO LOCB-ART-IDARTNR                                 
704501       PERFORM IMS-ISRT-LOCB01                                            
704601       PERFORM IMS-GU-LOCB01                                              
704701     END-IF                                                               
704801     IF SEGMENT-FINNS                                                     
704901       PERFORM UNTIL SEGMENT-SAKNAS OR LOCB-HIST-KDLOC = 'P'              
705001         PERFORM IMS-GHNP-LOCB11                                          
705101         IF SEGMENT-FINNS AND LOCB-HIST-KDLOC = 'P'                       
705201           MOVE FUNCTION CURRENT-DATE(1:8) TO LOCB-HIST-DASTODAT          
705301           MOVE MSG-SIGNON-USERID TO LOCB-HIST-IDUSER-STO                 
705401           PERFORM IMS-REPL-LOCB11                                        
705501         END-IF                                                           
705601       END-PERFORM                                                        
705701       MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-LOGG-DATUM                  
705801       MOVE FUNCTION CURRENT-DATE(9:6)  TO WS-LOGG-TID                    
705901       COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                     
706001                                                 WS-LOGG-DATUM            
706101       COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - WS-LOGG-TID           
706201       MOVE MSGI-IDDC                   TO LOCB-HIST-IDDC                 
706301       MOVE WS-PRIME-LOCATION           TO LOCB-HIST-KDLOC                
706401       MOVE W-ADLAGOMR-LOCB             TO LOCB-HIST-ADLAGOMR             
706501       MOVE W-ADGANG-LOCB               TO LOCB-HIST-ADGANG               
706601       MOVE W-ADPLATS-LOCB              TO LOCB-HIST-ADPLATS              
706701       MOVE MSG-SIGNON-USERID          TO LOCB-HIST-IDUSER                
706801       MOVE SPACE                       TO LOCB-HIST-IDUSER-STO           
706901       MOVE ZERO                        TO LOCB-HIST-DASTODAT             
707001                                                                          
707101       PERFORM IMS-ISRT-LOCB11                                            
707201     END-IF                                                               
707301     .                                                                    
707401     EJECT                                                                
707501 S31-SKAPA-SAP-TRANS-VCCS SECTION.                                        
707601******************************************************************        
707701* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
707801* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
707901*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
708001*         LIGGER KVAR TILLS VIDARE (WDR8).                                
708101******************************************************************        
708201                                                                          
708301     MOVE SPACE                         TO  EKH-KDTRADP                   
708401     IF  W-KVAVIS         < W-TEMP-KVANT                                  
708501*****    ÖVERLEVERANS                                                     
708601         IF DCS-CDC                                                       
708701         AND (SEND-DCS-NDC-CN OR SEND-DCS-USA)                            
708801           MOVE '102'                   TO EKH-KDEKHHT                    
708901           MOVE '122'                   TO EKH-KDEKSHT                    
709001         ELSE                                                             
709101           MOVE 6302-IDDISTR            TO DIST35-IDDISTR                 
709201           IF DIST35-RETUR                                                
709301             MOVE '502'                 TO EKH-KDEKHHT                    
709401             MOVE '502'                 TO EKH-KDEKSHT                    
709501             MOVE 'SEPV'                TO EKH-KDTRADP                    
709601           ELSE                                                           
709701             MOVE '503'                 TO EKH-KDEKHHT                    
709801             MOVE '502'                 TO EKH-KDEKSHT                    
709901           END-IF                                                         
710001         END-IF                                                           
710101         SUBTRACT W-KVAVIS FROM W-TEMP-KVANT                              
710201                           GIVING EKH-KVANTAL                             
710301     ELSE                                                                 
710401*****    UNDERLEVERANS                                                    
710501         IF DCS-CDC                                                       
710601         AND (SEND-DCS-NDC-CN OR SEND-DCS-USA)                            
710701           MOVE '102'                   TO EKH-KDEKHHT                    
710801           MOVE '122'                   TO EKH-KDEKSHT                    
710901         ELSE                                                             
711001           MOVE 6302-IDDISTR            TO DIST35-IDDISTR                 
711101           IF DIST35-RETUR                                                
711201             MOVE '502'                 TO EKH-KDEKHHT                    
711301             MOVE '503'                 TO EKH-KDEKSHT                    
711401             MOVE 'SEPV'                TO EKH-KDTRADP                    
711501           ELSE                                                           
711601             MOVE '503'                 TO EKH-KDEKHHT                    
711701             MOVE '503'                 TO EKH-KDEKSHT                    
711801           END-IF                                                         
711901         END-IF                                                           
712001         SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                              
712101                           GIVING EKH-KVANTAL                             
712201         COMPUTE EKH-KVANTAL = EKH-KVANTAL * -1                           
712301     END-IF                                                               
712401     MOVE 'W6030300'                  TO FIL-IDPGM IN FIL-WDR901          
712501     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
712601     MOVE WS-SAP-AAAAMMDD             TO FIL-DAREGDAT                     
712701                                         EKH-DAVERDAT                     
712801     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
712901     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR901        
713001     ADD +1                           TO W-IDSEKVNR-SAP                   
713101     MOVE W-IDSEKVNR-SAP            TO FIL-IDSEKVNR IN FIL-WDR901         
713201     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
713301     MOVE SEND-WS-IDDC                TO EKH-IDDC-SEND                    
713401     MOVE W-SPAR-IDDC                 TO EKH-IDDC-REC                     
713501     MOVE WS-SAP-IDDISTR              TO EKH-IDDISTR                      
713602                                         DIST35-IDDISTR                   
713701     MOVE WS-SAP-IDKUNDNR             TO EKH-IDKUNDNR                     
713801     MOVE ZERO TO NOLL-RAKNARE                                            
713901     MOVE W-SPAR-IDFAKT              TO WS-SAP-IDFAKT                     
714001     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
714101     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
714201          FOR LEADING ZERO                                                
714301     ADD +1 TO NOLL-RAKNARE                                               
714401     UNSTRING WS-SAP-X-IDFAKT      INTO EKH-IDVERGL                       
714501          WITH POINTER NOLL-RAKNARE                                       
714601     MOVE WS-SAP-PRARTSTD             TO EKH-PRARTSTD                     
714701     MOVE W-KDPRODSL                  TO EKH-KDPRODSL                     
714801     MOVE ZERO                        TO EKH-KDPSLLOC                     
714901                                         EKH-PRARTNTO                     
715001                                         EKH-PRARTSJK                     
715101                                         EKH-PRHEMTAG                     
715201                                         EKH-PRINK                        
715301                                         EKH-PRDIRLON                     
715401                                         EKH-PRDMTRL                      
715501                                         EKH-PROVRPAL                     
715601                                         EKH-SUBEL                        
715701     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
715801     MOVE SPACE                       TO EKH-FLLSBOK                      
715901     MOVE 'SEK'                       TO EKH-KDVALISO                     
716001     MOVE 1.00                        TO EKH-PRKURS                       
716101     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT IN                  
716201                                                      FIL-WDR901          
716301     MOVE '6303'                      TO EKH-IDTRANS                      
716401     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER IN FIL-WDR901         
716502     IF DIST35-RETUR                                                      
716602        MOVE +8                       TO EKH-KDRT                         
716702     ELSE                                                                 
716802        MOVE ZERO                     TO EKH-KDRT                         
716902     END-IF                                                               
717001     MOVE ZERO                        TO EKH-BEVAT                        
717101                                         EKH-IDANALYS                     
717201                                         EKH-IDKONTO                      
717301                                         EKH-KDANMORS                     
717401                                         EKH-SUVAT                        
717501                                         EKH-KDFRAKT                      
717601                                         EKH-PRLANDCO                     
717701                                         EKH-DAAVIDAT                     
717801                                         EKH-IDAVINR                      
717901                                         EKH-KDAVVTYP                     
718101                                         EKH-KVANTMOT                     
718201                                         EKH-KVAVIS                       
718301     MOVE WS-KDSORT                  TO  EKH-KDSORT                       
718401     MOVE SPACE                      TO  EKH-IDLEVNR                      
718501                                         EKH-FLDCET                       
718601                                         EKH-IDKUNDRF                     
718701                                         EKH-IDFAKT-EXP                   
718801                                         EKH-IDKST                        
718901                                                                          
719001     PERFORM IMS-ISRT-WLSAPA01                                            
719101                                                                          
719201     PERFORM UNTIL SEGMENT-FINNS                                          
719301         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                             
719401         PERFORM IMS-ISRT-WLSAPA01                                        
719501     END-PERFORM                                                          
719601     .                                                                    
719701     EJECT                                                                
719801 S31-SKAPA-SAP-TRANS SECTION.                                             
719901******************************************************************        
720001* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
720101* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
720201*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
720301*         LIGGER KVAR TILLS VIDARE (WDR8).                                
720401******************************************************************        
720501     IF  W-KVAVIS         < W-TEMP-KVANT                                  
720601*****    ÖVERLEVERANS                                                     
720701         IF SEND-DCS-CDC OR SEND-DCS-DDC                                  
720801           IF DIST35-NONVCC-NONVCC-REFILL                                 
720901             MOVE '132'               TO R8-EKH-KDEKSHT                   
721001           ELSE                                                           
721101             MOVE '122'               TO R8-EKH-KDEKSHT                   
721201           END-IF                                                         
721301         ELSE                                                             
721401           MOVE '502'                 TO R8-EKH-KDEKSHT                   
721501         END-IF                                                           
721601         SUBTRACT W-KVAVIS FROM W-TEMP-KVANT                              
721701                           GIVING R8-EKH-KVANTAL                          
721801     ELSE                                                                 
721901*****    UNDERLEVERANS                                                    
722001         IF SEND-DCS-CDC OR SEND-DCS-DDC                                  
722101           IF DIST35-NONVCC-NONVCC-REFILL                                 
722201             MOVE '132'               TO R8-EKH-KDEKSHT                   
722301           ELSE                                                           
722401             MOVE '122'               TO R8-EKH-KDEKSHT                   
722501           END-IF                                                         
722601           SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                            
722701                             GIVING R8-EKH-KVANTAL                        
722801           COMPUTE R8-EKH-KVANTAL = R8-EKH-KVANTAL * -1                   
722901         ELSE                                                             
723001           MOVE '503'                 TO R8-EKH-KDEKSHT                   
723101           SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                            
723201                             GIVING R8-EKH-KVANTAL                        
723301           COMPUTE R8-EKH-KVANTAL = R8-EKH-KVANTAL * -1                   
723401         END-IF                                                           
723501     END-IF                                                               
723601     MOVE 'W6030300'                  TO FIL-IDPGM IN FIL-WDR801          
723701     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
723801     ACCEPT FIL-TIKLOCK IN FIL-WDR801 FROM TIME                           
723901     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
724001     MOVE WS-SAP-AAAAMMDD             TO R8-EKH-DAVERDAT                  
724101     ADD +1                           TO W-IDSEKVNR-SAP                   
724201     MOVE W-IDSEKVNR-SAP          TO FIL-IDSEKVNR IN FIL-WDR801           
724301     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
724401       MOVE '102'                     TO R8-EKH-KDEKHHT                   
724501     ELSE                                                                 
724601       MOVE '503'                     TO R8-EKH-KDEKHHT                   
724701     END-IF                                                               
724801     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
724901     MOVE SEND-WS-IDDC                TO R8-EKH-IDDC-SEND                 
725001     MOVE W-SPAR-IDDC                 TO R8-EKH-IDDC-REC                  
725101     MOVE WS-SAP-IDDISTR              TO R8-EKH-IDDISTR                   
725201     MOVE WS-SAP-IDKUNDNR             TO R8-EKH-IDKUNDNR                  
725301     MOVE ZERO TO NOLL-RAKNARE                                            
725401     MOVE W-SPAR-IDFAKT               TO WS-SAP-IDFAKT                    
725501     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
725601     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
725701          FOR LEADING ZERO                                                
725801     ADD +1 TO NOLL-RAKNARE                                               
725901     UNSTRING WS-SAP-X-IDFAKT      INTO R8-EKH-IDVERGL                    
726001          WITH POINTER NOLL-RAKNARE                                       
726101     MOVE SLAG-PRAVCOST               TO R8-EKH-PRARTSTD                  
726201     MOVE W-KDPRODSL                  TO R8-EKH-KDPRODSL                  
726301     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
726401                                                                          
726501     MOVE ZERO                        TO R8-EKH-PRHEMTAG                  
726601                                         R8-EKH-PRARTSJK                  
726701                                         R8-EKH-PRINK                     
726801                                         R8-EKH-PRDIRLON                  
726901                                         R8-EKH-PRDMTRL                   
727001                                         R8-EKH-PROVRPAL                  
727101                                         R8-EKH-SUBEL                     
727201     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
727301     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
727401     MOVE 'USD'                       TO R8-EKH-KDVALISO                  
727501     MOVE 'W561EKHA'             TO FIL-IDCPYTXT IN FIL-WDR801            
727601     MOVE 1.00                        TO R8-EKH-PRKURS                    
727701     MOVE '6303'                      TO R8-EKH-IDTRANS                   
727801     MOVE ZERO                        TO R8-EKH-BEVAT                     
727901                                         R8-EKH-IDANALYS                  
728001                                         R8-EKH-IDKONTO                   
728101                                         R8-EKH-KDANMORS                  
728201                                         R8-EKH-SUVAT                     
728301                                         R8-EKH-KDFRAKT                   
728401                                         R8-EKH-PRLANDCO                  
728501                                         R8-EKH-DAAVIDAT                  
728601                                         R8-EKH-IDAVINR                   
728701                                         R8-EKH-KDAVVTYP                  
728801                                         R8-EKH-KDRT                      
728901                                         R8-EKH-KVANTMOT                  
729001                                         R8-EKH-KVAVIS                    
729101     MOVE WS-KDSORT                  TO  R8-EKH-KDSORT                    
729201     MOVE 'US01'                     TO  R8-EKH-KDTRADP                   
729301     MOVE SPACE                      TO  R8-EKH-IDLEVNR                   
729401     MOVE SPACE                      TO  R8-EKH-FLDCET                    
729501                                         R8-EKH-IDKST                     
729601     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
729701     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
729801                                                                          
729901     PERFORM IMS-ISRT-WLFILB01                                            
730001                                                                          
730101     PERFORM UNTIL SEGMENT-FINNS                                          
730201         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
730301         PERFORM IMS-ISRT-WLFILB01                                        
730401     END-PERFORM                                                          
730501     .                                                                    
730601     EJECT                                                                
730701 S31-SKAPA-SAP-TRANS-DAM SECTION.                                         
730801                                                                          
730901     MOVE 'W6030300'                  TO FIL-IDPGM IN FIL-WDR801          
731001     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
731101     ACCEPT FIL-TIKLOCK IN FIL-WDR801  FROM TIME                          
731201     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
731301     MOVE WS-SAP-AAAAMMDD             TO R8-EKH-DAVERDAT                  
731401     ADD +1                           TO W-IDSEKVNR-SAP                   
731501     MOVE W-IDSEKVNR-SAP           TO FIL-IDSEKVNR IN FIL-WDR801          
731601     MOVE '102'                       TO R8-EKH-KDEKHHT                   
731701     IF DIST35-NONVCC-NONVCC-REFILL                                       
731801       MOVE '132'                     TO R8-EKH-KDEKSHT                   
731901     ELSE                                                                 
732001       MOVE '122'                     TO R8-EKH-KDEKSHT                   
732101     END-IF                                                               
732201     COMPUTE R8-EKH-KVANTAL =                                             
732301             W-KVSKROT(INDX) * -1                                         
732401     END-COMPUTE                                                          
732501     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
732601     MOVE SEND-WS-IDDC                TO R8-EKH-IDDC-SEND                 
732701     MOVE W-SPAR-IDDC                 TO R8-EKH-IDDC-REC                  
732801     MOVE WS-SAP-IDDISTR              TO R8-EKH-IDDISTR                   
732901     MOVE WS-SAP-IDKUNDNR             TO R8-EKH-IDKUNDNR                  
733001     MOVE ZERO TO NOLL-RAKNARE                                            
733101     MOVE W-SPAR-IDFAKT               TO WS-SAP-IDFAKT                    
733201     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
733301     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
733401          FOR LEADING ZERO                                                
733501     ADD +1 TO NOLL-RAKNARE                                               
733601     UNSTRING WS-SAP-X-IDFAKT      INTO R8-EKH-IDVERGL                    
733701          WITH POINTER NOLL-RAKNARE                                       
733801     MOVE SLAG-PRAVCOST               TO R8-EKH-PRARTSTD                  
733901     MOVE W-KDPRODSL                  TO R8-EKH-KDPRODSL                  
734001     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
734101                                                                          
734201     MOVE ZERO                        TO R8-EKH-PRHEMTAG                  
734301                                         R8-EKH-PRARTSJK                  
734401                                         R8-EKH-PRINK                     
734501                                         R8-EKH-PRDIRLON                  
734601                                         R8-EKH-PRDMTRL                   
734701                                         R8-EKH-PROVRPAL                  
734801                                         R8-EKH-SUBEL                     
734901     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
735001     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
735101     MOVE 'USD'                       TO R8-EKH-KDVALISO                  
735201     MOVE 1.00                        TO R8-EKH-PRKURS                    
735301     MOVE 'W561EKHA'             TO FIL-IDCPYTXT IN FIL-WDR801            
735401     MOVE '6303'                      TO R8-EKH-IDTRANS                   
735501     MOVE ZERO                        TO R8-EKH-BEVAT                     
735601                                         R8-EKH-IDANALYS                  
735701                                         R8-EKH-IDKONTO                   
735801                                         R8-EKH-KDANMORS                  
735901                                         R8-EKH-SUVAT                     
736001                                         R8-EKH-KDFRAKT                   
736101                                         R8-EKH-PRLANDCO                  
736201                                         R8-EKH-DAAVIDAT                  
736301                                         R8-EKH-IDAVINR                   
736401                                         R8-EKH-KDAVVTYP                  
736501                                         R8-EKH-KDRT                      
736601                                         R8-EKH-KVANTMOT                  
736701                                         R8-EKH-KVAVIS                    
736801     MOVE WS-KDSORT                  TO  R8-EKH-KDSORT                    
736901     MOVE 'US01'                     TO  R8-EKH-KDTRADP                   
737001     MOVE SPACE                      TO  R8-EKH-IDLEVNR                   
737101                                         R8-EKH-IDKST                     
737201     MOVE SPACE                      TO  R8-EKH-FLDCET                    
737301     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
737401     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
737501                                                                          
737601     PERFORM IMS-ISRT-WLFILB01                                            
737701                                                                          
737801     PERFORM UNTIL SEGMENT-FINNS                                          
737901         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
738001         PERFORM IMS-ISRT-WLFILB01                                        
738101     END-PERFORM                                                          
738201     .                                                                    
738301     EJECT                                                                
738401                                                                          
738501 S32-SKAPA-DIFF-TRANS SECTION.                                            
738601                                                                          
738701     MOVE 6302-TIFAKT     TO WS-SEKEL-TEST                                
738801                             WS-DIFF-AAMMDD                               
738901     IF WS-SEK = 9                                                        
739001        MOVE 19           TO WS-DIFF-SS                                   
739101     ELSE                                                                 
739201        MOVE 20           TO WS-DIFF-SS                                   
739301     END-IF                                                               
739401     MOVE WS-DIFF-AAAAMMDD  TO FILC2-DAFAKT                               
739501     MOVE W-IDARTNR         TO FILC2-IDARTNR                              
739601     MOVE SEND-WS-IDDC      TO FILC2-IDDC-SEND                            
739701     MOVE W-SPAR-IDDC       TO FILC2-IDDC-REC                             
739801     MOVE W-SPAR-IDFAKT     TO FILC2-IDFAKT                               
739901     MOVE W-SPAR-IDKUNDRF   TO FILC2-IDKUNDRF                             
740001     MOVE ZERO              TO FILC2-PRARTBES-PR                          
740101     MOVE 6302-IDLEVNR      TO FILC2-IDLEVNR                              
740201     MOVE FILC2-W61244      TO FILC2-FIL-WDR301-DATA                      
740301     ACCEPT W-TID FROM TIME                                               
740401     IF W-TID = FILC2-FIL-TIKLOCK                                         
740501        ADD +1              TO FILC2-FIL-IDSEKVNR                         
740601     ELSE                                                                 
740701        MOVE W-TID          TO FILC2-FIL-TIKLOCK                          
740801        MOVE +1             TO FILC2-FIL-IDSEKVNR                         
740901     END-IF                                                               
741001     PERFORM IMS-ISRT-WLFILC2                                             
741101     .                                                                    
741201     EJECT                                                                
741301 S33-SKAPA-LDC-TRANS SECTION.                                             
741401                                                                          
741501     MOVE 6302-TIFAKT     TO WS-SEKEL-TEST                                
741601                             WS-DIFF-AAMMDD                               
741701     IF WS-SEK = 9                                                        
741801        MOVE 19           TO WS-DIFF-SS                                   
741901     ELSE                                                                 
742001        MOVE 20           TO WS-DIFF-SS                                   
742101     END-IF                                                               
742201     MOVE WS-DIFF-AAAAMMDD  TO FILC3-DAFAKT                               
742301     MOVE W-IDARTNR         TO FILC3-IDARTNR                              
742401     IF 6302-IDDC-LEV NOT = SPACE                                         
742501        MOVE 6302-IDDC-LEV  TO FILC3-IDDC-SEND                            
742601     ELSE                                                                 
742701        MOVE SEND-WS-IDDC   TO FILC3-IDDC-SEND                            
742801     END-IF                                                               
742901     MOVE W-SPAR-IDDC       TO FILC3-IDDC-REC                             
743001     MOVE W-SPAR-IDFAKT     TO FILC3-IDFAKT                               
743101     MOVE W-SPAR-IDKUNDRF   TO FILC3-IDKUNDRF                             
743201     MOVE W-SPAR-IDKUNDNR   TO FILC3-IDKUNDNR                             
743301     MOVE W-SPAR-IDKOLLI    TO FILC3-IDKOLLI                              
743401     MOVE ZERO              TO FILC3-PRARTSTD                             
743501     MOVE W-DAGENS-DATUM    TO FILC3-DAREGDAT                             
743601     ADD 20000000           TO FILC3-DAREGDAT                             
743701     IF MID-IDUSER-003 = ALL '+'                                          
743801        MOVE SPACE            TO FILC3-IDUSER                             
743901     ELSE                                                                 
744001        MOVE MID-IDUSER-003   TO FILC3-IDUSER                             
744101     END-IF                                                               
744201     MOVE FILC3-W61247      TO FILC3-FIL-WDR301-DATA                      
744301     ACCEPT W-TID FROM TIME                                               
744401     IF W-TID = FILC3-FIL-TIKLOCK                                         
744501        ADD +1              TO FILC3-FIL-IDSEKVNR                         
744601     ELSE                                                                 
744701        MOVE W-TID          TO FILC3-FIL-TIKLOCK                          
744801        MOVE +1             TO FILC3-FIL-IDSEKVNR                         
744901     END-IF                                                               
745001     PERFORM IMS-ISRT-WLFILC3                                             
745101     .                                                                    
745201     EJECT                                                                
745301 S35-KOLLA-IDUSER SECTION.                                                
745401                                                                          
745501     IF MID-IDUSER-003 = ALL '+' OR SPACE                                 
745601        IF DCS-FLBINNUT = JA                                              
745701           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-003-ATTR                 
745801           MOVE NEJ TO INDATA-SW                                          
745901        END-IF                                                            
746001     ELSE                                                                 
746101        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDUSER-003-ATTR                  
746201        MOVE MID-IDUSER-003 TO WS-IDUSER-003                              
746301     END-IF                                                               
746401     .                                                                    
746501     EJECT                                                                
746601 S37-SKAPA-NDC-HIST-SANDANDE SECTION.                                     
746701                                                                          
746801     MOVE DCS-IDLEVNR-DC     TO INL-IDLEVNR                               
746901     MOVE W-IDARTNR         TO ART-IDARTNR                                
747001     PERFORM IMS-ISRT-WLINLC01                                            
747101     MOVE FUNCTION CURRENT-DATE (1:8) TO W-TIAAAAMMDDTTMMSSTH-DATE        
747201     ACCEPT W-TIAAAAMMDDTTMMSSTH-TIME FROM TIME                           
747301                                                                          
747401     COMPUTE W-DAINLEV = 9999999999999999                                 
747501                       - W-TIAAAAMMDDTTMMSSTH                             
747601     END-COMPUTE                                                          
747701                                                                          
747801     MOVE W-DAINLEV         TO INL-DAINLEV                                
747901     MOVE ZERO              TO INL-ADLAGOMR                               
748001                               INL-ADGANG                                 
748101                               INL-ADPLATS                                
748201     MOVE SPACE             TO INL-FLMAKUL                                
748301                               INL-FLSKAKOL                               
748401     MOVE 'N'               TO INL-FLPRIO                                 
748402                               INL-FLTULLST                               
748501     MOVE SEND-WS-IDDC      TO INL-IDDC                                   
748601     MOVE SPACE             TO INL-IDDC-LEV                               
748701     MOVE ZERO              TO INL-IDLOPNRM                               
748801     MOVE W-SPAR-IDFAKT     TO INL-IDFAKT                                 
748901     MOVE 6302-IDDISTR      TO INL-IDDISTR                                
749003                               DIST35-IDDISTR                             
749101     MOVE 6302-IDKUNDNR     TO INL-IDKUNDNR                               
749201***  MOVE 6302-IDKUNDRF     TO INL-IDKUNDRF 050926                        
749301     MOVE W-SPAR-IDKUNDRF   TO INL-IDKUNDRF                               
749401     MOVE W-SPAR-IDKOLLI    TO INL-IDKOLLI                                
749501     MOVE 'R34'             TO INL-IDPTYP                                 
749601     MOVE SPACE             TO INL-IDUSER-003                             
749701     MOVE ZERO              TO INL-KDFRAKT                                
749801     MOVE SPACE             TO INL-KDKOLLI                                
749901                               INL-KDVALISO                               
750001                               INL-ADINLOMR                               
750101                               INL-IDANALYS                               
750201                               INL-IDKST                                  
750303     IF DIST35-RETUR                                                      
750403        MOVE +8             TO INL-KDRT                                   
750503     ELSE                                                                 
750603        MOVE ZERO           TO INL-KDRT                                   
750703     END-IF                                                               
750801     MOVE ZERO              TO INL-KVAVIS                                 
750901                               INL-KVART-SKROT                            
751101                               INL-IDKONTO                                
751201                               INL-KVTULRET                               
751202                               INL-KVRETUR                                
751301                               INL-KDAVVANT                               
751401                               INL-TIAVIDAT                               
751501     SUBTRACT W-TEMP-KVANT FROM W-KVAVIS                                  
751601                            GIVING INL-KVANTMOT                           
751701     MOVE ZERO              TO INL-PRARTNTO                               
751801                               INL-PRKURS                                 
751901                               INL-TIBERANK                               
752001                               INL-TIINLINL                               
752101                               INL-TIINLMOT                               
752201                               INL-TIINLMTI                               
752301                               INL-TIINLITI                               
752601                                                                          
752701     PERFORM IMS-ISRT-WLINLC11                                            
752801     PERFORM UNTIL INSERT-OK                                              
752901       SUBTRACT 1      FROM W-DAINLEV                                     
753001       MOVE W-DAINLEV  TO   INL-DAINLEV                                   
753101       PERFORM IMS-ISRT-WLINLC11                                          
753201     END-PERFORM                                                          
753301     .                                                                    
753401     EJECT                                                                
753501 S99-SKAPA-SKROT-ORDER SECTION.                                           
753601                                                                          
753701     IF  NOT TRANS-OHUVUD-DAM-SKAPAD                                      
753801         PERFORM S991-SKAPA-TRANS-ORDERHUVUD                              
753901                                                                          
754001         PERFORM S992-SKAPA-HUVUD-ORDERRADER                              
754101                                                                          
754201         MOVE 1           TO ORAD-IX                                      
754301     END-IF                                                               
754401                                                                          
754501     PERFORM S993-EDIT-TRANS-ORDERRADER                                   
754601                                                                          
754701* EFTERSOM PÅ BILDEN KAN MAN HA 12 RADER OCH 4252-RAD-TABELLEN            
754801* HAR MINSKATS TILL 6 SÅ MÅSTE MAN BRYTA HÄR OCH ANROPA W006KOM!          
754901                                                                          
755001     IF ORAD-IX = ORAD-IX-MAX                                             
755101       PERFORM S03-CALL-W006KOM                                           
755201       PERFORM S992-SKAPA-HUVUD-ORDERRADER                                
755301       MOVE 0             TO ORAD-IX                                      
755401     END-IF                                                               
755501                                                                          
755601     ADD +1               TO ORAD-IX                                      
755701     .                                                                    
755801     EJECT                                                                
755901 S991-SKAPA-TRANS-ORDERHUVUD SECTION.                                     
756001                                                                          
756101     MOVE JA              TO TRANS-OHUVUD-DAM-SKAPAD-SW                   
756201                                                                          
756301     MOVE SPACE           TO MSG-KOM-WMSGKOM                              
756401     MOVE +54             TO MSG-KOM-KVLL                                 
756501     MOVE LOW-VALUE       TO MSG-KOM-KDZ1                                 
756601     MOVE LOW-VALUE       TO MSG-KOM-KDZ2                                 
756701     MOVE SPACE           TO MSG-KOM-KDTRANS                              
756801     MOVE 'W4I25101'      TO MSG-KOM-IDCPYTXT                             
756901                                                                          
757001     IF DCS-CDC                                                           
757101       MOVE 'CDC-RET '      TO MSG-KOM-IDSNDNOD                           
757201     ELSE                                                                 
757301       MOVE 'SDC-RET '      TO MSG-KOM-IDSNDNOD                           
757401     END-IF                                                               
757501                                                                          
757601     MOVE 'W6030300'      TO MSG-KOM-IDSNDJOB                             
757701                                                                          
757801     ACCEPT MSG-KOM-TIREGDAT                                              
757901                          FROM DATE                                       
758001                                                                          
758101     ACCEPT MSG-KOM-TIKLOCK                                               
758201                          FROM TIME                                       
758301                                                                          
758401     MOVE SPACE           TO MSG-KOM-IDMFSMED                             
758501                             MSG-KOM-KDSVAR                               
758601                                                                          
758701     COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                             
758801                          LENGTH OF OHUV-MID-W4I25101                     
758901                                                                          
759001     MOVE LOW-VALUE              TO P-TO-P-Z1                             
759101     MOVE LOW-VALUE              TO P-TO-P-Z2                             
759201     MOVE 'W4T251X'              TO P-TO-P-TRANSKOD                       
759301     MOVE '4251'                 TO P-TO-P-FROM-MID                       
759401     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
759501                                                                          
759601     PERFORM S01-DISTRIKT-RETUR                                           
759701                                                                          
759801     MOVE SPACE                  TO KOM-AREA                              
759901                                                                          
760001     MOVE 'W603'                 TO OHUV-MID-IDSYSTEM                     
760101     MOVE W-IDDISTR-RETUR        TO OHUV-MID-IDDISTR                      
760201     MOVE W-IDKUNDNR-RETUR       TO OHUV-MID-IDKUNDNR                     
760301                                                                          
760401     MOVE DAT-TIVV        TO W-IDORDNR-VV                                 
760501     MOVE DAT-TID         TO W-IDORDNR-D                                  
760601     ACCEPT W-TIME-X      FROM TIME                                       
760701                                                                          
760801     MOVE W-TIME-TT       TO W-IDORDNR-TT                                 
760901                                                                          
761001*********** KONTROLL OM ORDERNR FINNS                                     
761101                                                                          
761201     MOVE ZERO             TO W-SEQC-IDDISTR                              
761301                              W-SEQC-IDKUNDNR                             
761401                              W-SEQC-IDORDNR7                             
761501     MOVE W-IDDISTR-RETUR  TO W-SEQC-IDDISTR                              
761601     MOVE W-IDKUNDNR-RETUR TO W-SEQC-IDKUNDNR                             
761701     MOVE W-IDORDNR-X      TO W-SEQC-IDORDNR7                             
761801     PERFORM IMS-GET-WDQ2C                                                
761901     PERFORM UNTIL SEGMENT-SAKNAS                                         
762001        ADD +1 TO W-IDORDNR-TT                                            
762101        MOVE W-IDORDNR-X TO W-SEQC-IDORDNR7                               
762201        PERFORM IMS-GET-WDQ2C                                             
762301     END-PERFORM                                                          
762401                                                                          
762501     MOVE W-IDORDNR-X     TO OHUV-MID-IDORDNR                             
762601     MOVE '1'             TO OHUV-MID-KDORDKL                             
762701                                                                          
762801     MOVE SPACE           TO OHUV-MID-KDFRAKT                             
762901                             OHUV-MID-TIRFS                               
763001     MOVE SPACE           TO OHUV-MID-BEKUNDRF                            
763101                             OHUV-MID-KDFAKTYP                            
763201     MOVE NEJ             TO OHUV-MID-FLRESTN                             
763301     MOVE SPACE           TO OHUV-MID-KDTPOTYP                            
763401                             OHUV-MID-TITPO                               
763501                             OHUV-MID-BELAGINS                            
763601                             OHUV-MID-BEGMT                               
763701                             OHUV-MID-ADGMT-GATA                          
763801                             OHUV-MID-ADGMT-PADR                          
763901                             OHUV-MID-KDROPACK                            
764001                             OHUV-MID-BEVARREF                            
764101                             OHUV-MID-KDTULLVE                            
764201                             OHUV-MID-KDNOTES                             
764301     MOVE W-IDDC          TO W-IDKONTO-IDDC                               
764401     MOVE W-KDPRODSL      TO W-IDKONTO-KDPRODSL                           
764501     MOVE SPACE           TO OHUV-MID-IDKONTO                             
764601     MOVE SPACE           TO OHUV-MID-IDKST                               
764701     MOVE JA              TO OHUV-MID-FLAUTFAK                            
764801     MOVE JA              TO OHUV-MID-FLAUTPAC                            
764901     MOVE NEJ             TO OHUV-MID-FLEMBORD                            
765001     MOVE NEJ             TO OHUV-MID-FLOVRLEV                            
765101     MOVE '57'            TO OHUV-MID-IDFTG                               
765201     MOVE SPACE           TO OHUV-MID-IDKAMPRF                            
765301                             OHUV-MID-ADBET                               
765401                             OHUV-MID-BEBET                               
765501                             OHUV-MID-IDSKYLT                             
765601                             OHUV-MID-FLLSBOK                             
765701     MOVE W-IDDC          TO OHUV-MID-IDDC                                
765801     MOVE SPACE           TO OHUV-MID-IDANALYS                            
765901     MOVE ZERO            TO OHUV-MID-IDDEPT                              
766001     MOVE SPACE           TO OHUV-MID-KDORDTYP-LDC                        
766101     MOVE ZERO            TO OHUV-MID-TIREPDAT                            
766201     MOVE NEJ             TO OHUV-MID-FLFORBI                             
766301                             OHUV-MID-FLORDTIL                            
766401     MOVE ZERO            TO OHUV-MID-IDGROSS                             
766501     MOVE SPACE           TO OHUV-MID-IDBILREG                            
766601                             OHUV-MID-IDVIN                               
766701                             OHUV-MID-IDCISNR                             
766801                                                                          
766901     PERFORM S03-CALL-W006KOM                                             
767001     .                                                                    
767101     EJECT                                                                
767201 S992-SKAPA-HUVUD-ORDERRADER SECTION.                                     
767301                                                                          
767401     COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                             
767501                          LENGTH OF ORAD-MID-W4I25201                     
767601                                                                          
767701     MOVE LOW-VALUE        TO P-TO-P-Z1                                   
767801     MOVE LOW-VALUE        TO P-TO-P-Z2                                   
767901     MOVE 'W4T252X'        TO P-TO-P-TRANSKOD                             
768001     MOVE '4252'           TO P-TO-P-FROM-MID                             
768101     MOVE MFS-KDMFSFOR     TO P-TO-P-KDMFSFOR                             
768201                                                                          
768301     MOVE SPACE            TO KOM-AREA                                    
768401                                                                          
768501     MOVE 'W603'           TO ORAD-MID-IDSYSTEM                           
768601     MOVE W-IDDISTR-RETUR  TO ORAD-MID-IDDISTR                            
768701     MOVE W-IDKUNDNR-RETUR TO ORAD-MID-IDKUNDNR                           
768801     MOVE W-IDORDNR-X      TO ORAD-MID-IDORDNR                            
768901     MOVE SPACE            TO ORAD-MID-BEVOLREF                           
769001     MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                        
769101     MOVE 'N'              TO ORAD-MID-FLSLUT                             
769201                                                                          
769301     .                                                                    
769401     EJECT                                                                
769501 S993-EDIT-TRANS-ORDERRADER SECTION.                                      
769601                                                                          
769701     MOVE W-IDARTNR       TO ORAD-MID-IDARTNR      (ORAD-IX)              
769801                             REK-IDARTNR                                  
769901     MOVE 9               TO REK-LNGD                                     
770001     MOVE 0               TO REK-REKSIFFR                                 
770101                                                                          
770201     CALL W009KSIF        USING REK-IDARTNR                               
770301                                REK-LNGD                                  
770401                                REK-REKSIFFR                              
770501                                                                          
770601     MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR  (ORAD-IX)                
770701     IF NY-SKROT                                                          
770801         MOVE WS-KVSKROT-INM TO W-KVSKROT-6                               
770901         MOVE W-KVSKROT-6-X TO ORAD-MID-KVBEART      (ORAD-IX)            
771001         MOVE NEJ TO NY-SKROT-SW                                          
771101     ELSE                                                                 
771201         MOVE W-KVSKROT (INDX) TO W-KVSKROT-6                             
771301         MOVE W-KVSKROT-6-X    TO ORAD-MID-KVBEART   (ORAD-IX)            
771401     END-IF                                                               
771501     MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)                
771601                              ORAD-MID-TITPO     (ORAD-IX)                
771701                              ORAD-MID-FLRESTN   (ORAD-IX)                
771801                              ORAD-MID-KDKVBRYT  (ORAD-IX)                
771901                              ORAD-MID-FLINVEST  (ORAD-IX)                
772001     MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)                
772101     MOVE SPACE            TO ORAD-MID-IDKONTO   (ORAD-IX)                
772201                              ORAD-MID-IDKST     (ORAD-IX)                
772301                              ORAD-MID-BERADREF  (ORAD-IX)                
772401                              ORAD-MID-KDDSP     (ORAD-IX)                
772501                              ORAD-MID-IDBIL     (ORAD-IX)                
772601     MOVE NEJ              TO ORAD-MID-FLSLATT   (ORAD-IX)                
772701     MOVE SPACE         TO ORAD-MID-PRARTNTO-LOC (ORAD-IX)                
772801     MOVE SPACE         TO ORAD-MID-PRARTBTO-LOC (ORAD-IX)                
772901     MOVE SPACE         TO     ORAD-MID-KDVALISO (ORAD-IX)                
773001     MOVE SPACE         TO     ORAD-MID-KDVAT    (ORAD-IX)                
773101     MOVE 0             TO     ORAD-MID-RERAB    (ORAD-IX)                
773201     MOVE SPACE         TO     ORAD-MID-KDRAB    (ORAD-IX)                
773301     MOVE SPACE         TO ORAD-MID-BEART-VIPS   (ORAD-IX)                
773401     MOVE ZERO          TO ORAD-MID-ADLAGOMR-CD  (ORAD-IX)                
773501                           ORAD-MID-ADGANG-CD    (ORAD-IX)                
773601                           ORAD-MID-ADPLATS-CD   (ORAD-IX)                
773701     MOVE SPACE         TO ORAD-MID-IDKUNDRF-WIP (ORAD-IX)                
773801                                                                          
773901     .                                                                    
774001     EJECT                                                                
774101 MFS-RENSA-NYCKLAR-UT SECTION.                                            
774201                                                                          
774301     MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-UT                                
774401                             MOD-IDKUNDRF-UT                              
774501                             MOD-IDKUNDNR-UT                              
774601                             MOD-IDKOLLI-UT                               
774701                             MOD-IDSPRAK-UT                               
774801                             MOD-IDDC-UT                                  
774901     .                                                                    
775001     EJECT                                                                
775101 MFS-RENSA-FAELT-UT SECTION.                                              
775201                                                                          
775301*    --- ALLA UTDATA-FÄLT                                                 
775401*    --- INKL. BLÄDDRINGSNYCKLAR                                          
775501     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ENTER                            
775601                             MOD-IDARTNR-NEXT                             
775701                             MOD-DAINLEV-ENTER                            
775801                             MOD-DAINLEV-NEXT                             
775901                             MOD-KOLLI-KLART                              
776001                             MOD-IDUSER-003                               
776101                             MOD-KVANTMOT-INM                             
776201                             MOD-KVSKROT-INM                              
776301                             MOD-IDARTNR-INM                              
776401                             MOD-ADLAGOMR-INM                             
776501                             MOD-ADGANG-INM                               
776601                             MOD-ADPLATS-INM                              
776701                             MOD-CMD-INM                                  
776801                                                                          
776901     MOVE +1 TO INDX                                                      
777001     PERFORM UNTIL INDX > MAX-INDX                                        
777101         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
777201         ADD +1 TO INDX                                                   
777301     END-PERFORM                                                          
777401     .                                                                    
777501     EJECT                                                                
777601 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
777701                                                                          
777801*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
777901     MOVE MFS-RENSA-FAELT TO MOD-CMD-IN       (INDX)                      
778001                             MOD-KVANTMOT-IN  (INDX)                      
778101                             MOD-KVAVIS       (INDX)                      
778201                             MOD-IDARTNR      (INDX)                      
778301                             MOD-ADLAGOMR     (INDX)                      
778401                             MOD-ADGANG       (INDX)                      
778501                             MOD-ADPLATS      (INDX)                      
778601                             MOD-BEART        (INDX)                      
778701                             MOD-KDPRIO       (INDX)                      
778801                             MOD-KVSKROT      (INDX)                      
778901     .                                                                    
779001     EJECT                                                                
779101 MFS-STAENG-RAD-FAELT-UT SECTION.                                         
779201                                                                          
779301     MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR      (INDX)                    
779401                              MOD-KVANTMOT-ATTR (INDX)                    
779501                              MOD-ADLAGOMR-ATTR (INDX)                    
779601                              MOD-ADGANG-ATTR   (INDX)                    
779701                              MOD-ADPLATS-ATTR  (INDX)                    
779801                              MOD-KVSKROT-ATTR  (INDX)                    
779901     .                                                                    
780001     EJECT                                                                
780101 MFS-RENSA-FAELT-IN SECTION.                                              
780201                                                                          
780301*    --- ALLA INDATA-FÄLT                                                 
780401     MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-IN                                
780501                             MOD-IDKUNDRF-IN                              
780601                             MOD-IDKUNDNR-IN                              
780701                             MOD-IDKOLLI-IN                               
780801                             MOD-IDSPRAK-IN                               
780901                             MOD-IDDC-IN                                  
781001     .                                                                    
781101     EJECT                                                                
781201 MFS-RENSA-NY-ARTIKEL-FAELT SECTION.                                      
781301                                                                          
781401     MOVE MFS-RENSA-FAELT TO MOD-KVANTMOT-INM                             
781501                             MOD-IDARTNR-INM                              
781601                             MOD-KVANTMOT-INM                             
781701                             MOD-KVSKROT-INM                              
781801                             MOD-ADLAGOMR-INM                             
781901                             MOD-ADGANG-INM                               
782001                             MOD-ADPLATS-INM                              
782101                             MOD-CMD-INM                                  
782201     .                                                                    
782301     EJECT                                                                
782401 MFS-STAENG-INDATA-FAELT  SECTION.                                        
782501                                                                          
782601*    --- ALLA INDATA-FÄLT FÖRUTOM NYCKLAR STÄNGS                          
782701     MOVE MFS-STAENG-FAELT TO MOD-KOLLI-KLART-ATTR                        
782801                              MOD-IDUSER-003-ATTR                         
782901                                                                          
783001     MOVE +1 TO INDX                                                      
783101     PERFORM UNTIL INDX > MAX-INDX                                        
783201         PERFORM MFS-STAENG-RAD-FAELT-UT                                  
783301         ADD +1 TO INDX                                                   
783401     END-PERFORM                                                          
783501                                                                          
783601     MOVE MFS-STAENG-FAELT  TO MOD-KVANTMOT-INM-ATTR                      
783701                               MOD-IDARTNR-INM-ATTR                       
783801                               MOD-KVSKROT-INM-ATTR                       
783901                               MOD-ADLAGOMR-INM-ATTR                      
784001                               MOD-ADGANG-INM-ATTR                        
784101                               MOD-ADPLATS-INM-ATTR                       
784201                               MOD-CMD-INM-ATTR                           
784301     .                                                                    
784401     EJECT                                                                
784501 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
784601                                                                          
784701*    --- ALLA UTDATA-FÄLT                                                 
784801*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
784901     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFAKT-UT                              
785001                               MOD-IDKUNDRF-UT                            
785101                               MOD-IDKUNDNR-UT                            
785201                               MOD-IDKOLLI-UT                             
785301                               MOD-IDSPRAK-UT                             
785401                               MOD-IDDC-UT                                
785501                               MOD-IDARTNR-ENTER                          
785601                               MOD-IDARTNR-NEXT                           
785701                               MOD-DAINLEV-ENTER                          
785801                               MOD-DAINLEV-NEXT                           
785901                               MOD-KOLLI-KLART                            
786001                               MOD-IDUSER-003                             
786101     MOVE +1 TO INDX                                                      
786201     PERFORM UNTIL INDX > MAX-INDX                                        
786301       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
786401       ADD +1 TO INDX                                                     
786501     END-PERFORM                                                          
786601     .                                                                    
786701     EJECT                                                                
786801 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
786901                                                                          
787001*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
787101     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-IN       (INDX)                    
787201                               MOD-KVANTMOT-IN  (INDX)                    
787301                               MOD-KVAVIS       (INDX)                    
787401                               MOD-IDARTNR      (INDX)                    
787501                               MOD-ADLAGOMR     (INDX)                    
787601                               MOD-ADGANG       (INDX)                    
787701                               MOD-ADPLATS      (INDX)                    
787801                               MOD-BEART        (INDX)                    
787901                               MOD-KDPRIO       (INDX)                    
788001                               MOD-KVSKROT      (INDX)                    
788101     .                                                                    
788201     SKIP3                                                                
788301 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
788401                                                                          
788501*    --- ALLA INDATA-FÄLT                                                 
788601     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFAKT-IN                              
788701                               MOD-IDKUNDRF-IN                            
788801                               MOD-IDKUNDNR-IN                            
788901                               MOD-IDKOLLI-IN                             
789001                               MOD-IDSPRAK-IN                             
789101                               MOD-IDDC-IN                                
789201                               MOD-KVANTMOT-INM                           
789301                               MOD-IDARTNR-INM                            
789401                               MOD-KVSKROT-INM                            
789501                               MOD-ADLAGOMR-INM                           
789601                               MOD-ADGANG-INM                             
789701                               MOD-ADPLATS-INM                            
789801                               MOD-CMD-INM                                
789901     .                                                                    
790001     EJECT                                                                
790101* --- IMS SEKTIONER ---                                                   
790201     SKIP3                                                                
790301 IMS-GET-MSG SECTION.                                                     
790401                                                                          
790501     MOVE '  QC' TO GODK-STATUSKODER                                      
790601     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
790701     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
790801     PERFORM IMS-STATUSKONTROLL                                           
790901     .                                                                    
791001     SKIP3                                                                
791101 IMS-INSERT-MSG SECTION.                                                  
791201                                                                          
791301     IF SWEDISH-TEXT                                                      
791401        IF MSGI-IDLAND-SPR NOT = 'GB'                                     
791501           MOVE '0' TO MFS-KDHUVOMR                                       
791601        END-IF                                                            
791701     END-IF                                                               
791801                                                                          
791901     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
792001     MOVE SPACE TO GODK-STATUSKODER                                       
792101     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
792201                                                                          
792301     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
792401     PERFORM IMS-STATUSKONTROLL                                           
792501     .                                                                    
792601     SKIP2                                                                
792701 IMS-ISRT-ALT2-PCB-6302 SECTION.                                          
792801                                                                          
792901     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
793001     MOVE SPACE TO GODK-STATUSKODER                                       
793101     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-AREA-2                       
793201     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
793301     PERFORM IMS-STATUSKONTROLL                                           
793401     .                                                                    
793501     EJECT                                                                
793601 IMS-GHU-WL630111 SECTION.                                                
793701                                                                          
793801     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
793901          DELIMITED BY SIZE INTO SSA1                                     
794001     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
794101          DELIMITED BY SIZE INTO SSA2                                     
794201     MOVE SPACE TO GODK-STATUSKODER                                       
794301     CALL CBLTDLI USING GHU 6301-PCB DLI-IO-AREA-WDGX SSA1 SSA2           
794401                                                                          
794501     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
794601     PERFORM IMS-STATUSKONTROLL                                           
794701     .                                                                    
794801     SKIP3                                                                
794901 IMS-GHU-WL630111-GE SECTION.                                             
795001                                                                          
795101     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
795201          DELIMITED BY SIZE INTO SSA1                                     
795301     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
795401          DELIMITED BY SIZE INTO SSA2                                     
795501     MOVE '  GE' TO GODK-STATUSKODER                                      
795601     CALL CBLTDLI USING GHU 6301-PCB DLI-IO-AREA-WDGX SSA1 SSA2           
795701     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
795801     PERFORM IMS-STATUSKONTROLL                                           
795901     .                                                                    
796001     SKIP3                                                                
796101 IMS-REPL-WL630111 SECTION.                                               
796201                                                                          
796301     MOVE SPACE           TO GODK-STATUSKODER                             
796401     CALL CBLTDLI USING REPL 6301-PCB DLI-IO-AREA-WDGX                    
796501                                                                          
796601     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
796701     PERFORM IMS-STATUSKONTROLL                                           
796801     .                                                                    
796901     SKIP3                                                                
797001 IMS-DLET-WL630111 SECTION.                                               
797101                                                                          
797201     MOVE SPACE           TO GODK-STATUSKODER                             
797301     CALL CBLTDLI USING DLET 6301-PCB DLI-IO-AREA-WDGX                    
797401                                                                          
797501     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
797601     PERFORM IMS-STATUSKONTROLL                                           
797701     .                                                                    
797801     EJECT                                                                
797901 IMS-GU-WLINLD01-FIRST    SECTION.                                        
798001                                                                          
798101     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
798201                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
798301          DELIMITED BY SIZE INTO SSA1                                     
798401     MOVE '  GE' TO GODK-STATUSKODER                                      
798501     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA-WDL6 SSA1                 
798601                                                                          
798701     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
798801     PERFORM IMS-STATUSKONTROLL                                           
798901     .                                                                    
799001     SKIP3                                                                
799101 IMS-GU-WLINLD01-FIRST-310 SECTION.                                       
799201                                                                          
799301     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
799401                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
799501                    '&IDPTYP  = ' W-IDPTYP ')'                            
799601          DELIMITED BY SIZE INTO SSA1                                     
799701     MOVE '  GE' TO GODK-STATUSKODER                                      
799801     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA-WDL6 SSA1                 
799901     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
800001     PERFORM IMS-STATUSKONTROLL                                           
800101     .                                                                    
800201     EJECT                                                                
800301 IMS-GN-WLINLD01          SECTION.                                        
800401                                                                          
800501     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
800601                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
800701                    '&IDPTYP  = ' W-IDPTYP ')'                            
800801          DELIMITED BY SIZE INTO SSA1                                     
800901     MOVE '  GE' TO GODK-STATUSKODER                                      
801001     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA-WDL6 SSA1                 
801101                                                                          
801201     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
801301     PERFORM IMS-STATUSKONTROLL                                           
801401     .                                                                    
801501     SKIP3                                                                
801601 IMS-GU-WLINLD01 SECTION.                                                 
801701                                                                          
801801     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
801901                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
802001                    '&IDPTYP  = ' W-IDPTYP ')'                            
802101          DELIMITED BY SIZE INTO SSA1                                     
802201     MOVE '  ' TO GODK-STATUSKODER                                        
802301     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA-WDL6 SSA1                 
802401     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
802501     PERFORM IMS-STATUSKONTROLL                                           
802601     .                                                                    
802701     EJECT                                                                
802801 IMS-GET-WDK711 SECTION.                                                  
802901                                                                          
803001     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
803101          DELIMITED BY SIZE INTO SSA1                                     
803201     STRING 'WDK711  (IDDC    = ' W-IDDC    ')'                           
803301          DELIMITED BY SIZE INTO SSA2                                     
803401     MOVE SPACE TO GODK-STATUSKODER                                       
803501     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
803601     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
803701     PERFORM IMS-STATUSKONTROLL                                           
803801     .                                                                    
803901     SKIP3                                                                
804001 IMS-REPL-WDK711 SECTION.                                                 
804101                                                                          
804201     MOVE '  ' TO GODK-STATUSKODER                                        
804301     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
804401     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
804501     PERFORM IMS-STATUSKONTROLL                                           
804601     .                                                                    
804701     SKIP3                                                                
804801 IMS-GU-WDK7-WDK711 SECTION.                                              
804901                                                                          
805001     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
805101          DELIMITED BY SIZE INTO SSA1                                     
805201     STRING 'WDK711  (IDDC    = ' W-IDDC    ')'                           
805301          DELIMITED BY SIZE INTO SSA2                                     
805401     MOVE '  GE' TO GODK-STATUSKODER                                      
805501     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2            
805601     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
805701     PERFORM IMS-STATUSKONTROLL                                           
805801     .                                                                    
805901     SKIP3                                                                
806001 IMS-GU-WLARTC01   SECTION.                                               
806101                                                                          
806201     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
806301          DELIMITED BY SIZE INTO SSA1                                     
806401     MOVE SPACE  TO GODK-STATUSKODER                                      
806501     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-WDK6 SSA1                
806601     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
806701     PERFORM IMS-STATUSKONTROLL                                           
806801     .                                                                    
806901     SKIP3                                                                
807001 IMS-GET-WLARTC11   SECTION.                                              
807101                                                                          
807201     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
807301          DELIMITED BY SIZE INTO SSA1                                     
807401     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
807501          DELIMITED BY SIZE INTO SSA2                                     
807601     MOVE SPACE  TO GODK-STATUSKODER                                      
807701     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-WDK6 SSA1 SSA2           
807801     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
807901     PERFORM IMS-STATUSKONTROLL                                           
808001     .                                                                    
808101     SKIP3                                                                
808201 IMS-GET-WLARTC01   SECTION.                                              
808301                                                                          
808401     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
808501          DELIMITED BY SIZE INTO SSA1                                     
808601     MOVE 'GE  ' TO GODK-STATUSKODER                                      
808701     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-WDK6 SSA1                
808801     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
808901     PERFORM IMS-STATUSKONTROLL                                           
809001     .                                                                    
809101     SKIP3                                                                
809201 IMS-GHNP-WLARTC11   SECTION.                                             
809301                                                                          
809401     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
809501          DELIMITED BY SIZE INTO SSA1                                     
809601     MOVE SPACE  TO GODK-STATUSKODER                                      
809701     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-WDK6 SSA1               
809801     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
809901     PERFORM IMS-STATUSKONTROLL                                           
810001     .                                                                    
810101     EJECT                                                                
810201 IMS-GHU-WLARTC11   SECTION.                                              
810301                                                                          
810401     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
810501          DELIMITED BY SIZE INTO SSA1                                     
810601     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
810701          DELIMITED BY SIZE INTO SSA2                                     
810801     MOVE SPACE  TO GODK-STATUSKODER                                      
810901     CALL CBLTDLI USING GHU  ARTC-PCB DLI-IO-AREA-WDK6 SSA1 SSA2          
811001     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
811101     PERFORM IMS-STATUSKONTROLL                                           
811201     .                                                                    
811301     SKIP3                                                                
811401 IMS-REPL-WLARTC11 SECTION.                                               
811501                                                                          
811601     MOVE '  ' TO GODK-STATUSKODER                                        
811701     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-WDK6                    
811801                                                                          
811901     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
812001     PERFORM IMS-STATUSKONTROLL                                           
812101     .                                                                    
812201     EJECT                                                                
812301 IMS-GHU-WLARTC11-GE SECTION.                                             
812401                                                                          
812501     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
812601          DELIMITED BY SIZE INTO SSA1                                     
812701     STRING 'WLARTC11(KDSEGKEY= 1)'                                       
812801          DELIMITED BY SIZE INTO SSA2                                     
812901     MOVE '  GE' TO GODK-STATUSKODER                                      
813001     CALL CBLTDLI USING GHU  ARTC-PCB DLI-IO-AREA-WDK6 SSA1 SSA2          
813101     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
813201     PERFORM IMS-STATUSKONTROLL                                           
813301     .                                                                    
813401     SKIP3                                                                
813501 IMS-GU-WLINLC11   SECTION.                                               
813601                                                                          
813701     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
813801          DELIMITED BY SIZE INTO SSA1                                     
813901     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
814001          DELIMITED BY SIZE INTO SSA2                                     
814101     MOVE SPACE  TO GODK-STATUSKODER                                      
814201     CALL CBLTDLI USING GU  INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2           
814301     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
814401     PERFORM IMS-STATUSKONTROLL                                           
814501     .                                                                    
814601     SKIP3                                                                
814701 IMS-GHU-WLINLC11   SECTION.                                              
814801                                                                          
814901     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
815001          DELIMITED BY SIZE INTO SSA1                                     
815101     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
815201          DELIMITED BY SIZE INTO SSA2                                     
815301     MOVE SPACE  TO GODK-STATUSKODER                                      
815401     CALL CBLTDLI USING GHU  INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2          
815501     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
815601     PERFORM IMS-STATUSKONTROLL                                           
815701     .                                                                    
815801     EJECT                                                                
815901 IMS-REPL-WLINLC11 SECTION.                                               
816001                                                                          
816101     MOVE '  ' TO GODK-STATUSKODER                                        
816201     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-AREA-WDL6                    
816301                                                                          
816401     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
816501     PERFORM IMS-STATUSKONTROLL                                           
816601     .                                                                    
816701     SKIP3                                                                
816801 IMS-ISRT-WLINLC01   SECTION.                                             
816901                                                                          
817001     MOVE 'WLINLC01 '      TO SSA1                                        
817101     MOVE '  II'           TO GODK-STATUSKODER                            
817201     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-WDL6 SSA1               
817301     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
817401     PERFORM IMS-STATUSKONTROLL                                           
817501     .                                                                    
817601     SKIP3                                                                
817701 IMS-ISRT-WLINLC11   SECTION.                                             
817801                                                                          
817901     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
818001          DELIMITED BY SIZE INTO SSA1                                     
818101     MOVE 'WLINLC11 '         TO SSA2                                     
818201     MOVE '  II'              TO GODK-STATUSKODER                         
818301     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2          
818401     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
818501     PERFORM IMS-STATUSKONTROLL                                           
818601     .                                                                    
818701     EJECT                                                                
818801 IMS-GU-WLINLE01 SECTION.                                                 
818901     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
819001             DELIMITED BY SIZE INTO SSA1                                  
819101     MOVE '  GE' TO GODK-STATUSKODER                                      
819201     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA-WDL2 SSA1                 
819301     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
819401     PERFORM IMS-STATUSKONTROLL                                           
819501     .                                                                    
819601     EJECT                                                                
819701 IMS-ISRT-WLINLE01 SECTION.                                               
819801                                                                          
819901     MOVE 'WLINLE01 ' TO SSA1                                             
820001     MOVE '  ' TO GODK-STATUSKODER                                        
820101     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA-WDL2 SSA1               
820201     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
820301     PERFORM IMS-STATUSKONTROLL                                           
820401     .                                                                    
820501     SKIP3                                                                
820601 IMS-ISRT-WLINLE11 SECTION.                                               
820701                                                                          
820801     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
820901          DELIMITED BY SIZE INTO SSA1                                     
821001     MOVE 'WLINLE11 ' TO SSA2                                             
821101     MOVE '  II' TO GODK-STATUSKODER                                      
821201     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA-WDL2 SSA1 SSA2          
821301     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
821401     PERFORM IMS-STATUSKONTROLL                                           
821501     .                                                                    
821601     SKIP3                                                                
821701 IMS-ISRT-WLINLE22 SECTION.                                               
821801                                                                          
821901     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
822001          DELIMITED BY SIZE INTO SSA1                                     
822101     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
822201          DELIMITED BY SIZE INTO SSA2                                     
822301     MOVE 'WLINLE22 ' TO SSA3                                             
822401     MOVE '  ' TO GODK-STATUSKODER                                        
822501     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA-WDL2                    
822601                        SSA1 SSA2 SSA3                                    
822701     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
822801     PERFORM IMS-STATUSKONTROLL                                           
822901     .                                                                    
823001     EJECT                                                                
823101 IMS-ISRT-WLFILB01 SECTION.                                               
823201                                                                          
823301     MOVE '  ' TO GODK-STATUSKODER                                        
823401     MOVE 'WLFILB01 ' TO SSA1                                             
823501     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA-WDR8 SSA1               
823601     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
823701     PERFORM IMS-STATUSKONTROLL                                           
823801     .                                                                    
823901     SKIP3                                                                
824001 IMS-GU-WLBENA11          SECTION.                                        
824101     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
824201            DELIMITED BY SIZE INTO SSA1                                   
824301     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT ')'                           
824401            DELIMITED BY SIZE INTO SSA2                                   
824501     MOVE '  GE' TO GODK-STATUSKODER                                      
824601     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-WDD3 SSA1 SSA2            
824701     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
824801     PERFORM IMS-STATUSKONTROLL                                           
824901     .                                                                    
825001     EJECT                                                                
825101 IMS-GHU-W6LOPA11 SECTION.                                                
825201     STRING 'W6LOPA01(W6GXKEY  =' W-6017KEY-X ')'                         
825301          DELIMITED BY SIZE INTO SSA1                                     
825401     MOVE 'W6LOPA11 ' TO SSA2                                             
825501     MOVE '  ' TO GODK-STATUSKODER                                        
825601     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA-W6GX SSA1 SSA2           
825701     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
825801     PERFORM IMS-STATUSKONTROLL                                           
825901     .                                                                    
826001     SKIP3                                                                
826101 IMS-REPL-W6LOPA SECTION.                                                 
826201     MOVE '  ' TO GODK-STATUSKODER                                        
826301     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA-W6GX                    
826401     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
826501     PERFORM IMS-STATUSKONTROLL                                           
826601     .                                                                    
826701     EJECT                                                                
826801 IMS-GHU-WL630511 SECTION.                                                
826901     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X ')'                         
827001          DELIMITED BY SIZE INTO SSA1                                     
827101     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X  ')'                         
827201          DELIMITED BY SIZE INTO SSA2                                     
827301     MOVE 'GE  ' TO GODK-STATUSKODER                                      
827401     CALL CBLTDLI USING GHU 6305-PCB DLI-IO-AREA-WDGX2 SSA1 SSA2          
827501     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
827601     PERFORM IMS-STATUSKONTROLL                                           
827701     .                                                                    
827801     SKIP2                                                                
827901 IMS-ISRT-WL630511 SECTION.                                               
828001     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X  ')'                        
828101          DELIMITED BY SIZE INTO SSA1                                     
828201     MOVE 'WL630511 ' TO SSA2                                             
828301     MOVE '  ' TO GODK-STATUSKODER                                        
828401     CALL CBLTDLI USING ISRT 6305-PCB DLI-IO-AREA-WDGX2 SSA1 SSA2         
828501     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
828601     PERFORM IMS-STATUSKONTROLL                                           
828701     .                                                                    
828801     EJECT                                                                
828901 IMS-ISRT-WL630521 SECTION.                                               
829001     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X    ')'                      
829101          DELIMITED BY SIZE INTO SSA1                                     
829201     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X      ')'                     
829301          DELIMITED BY SIZE INTO SSA2                                     
829401     MOVE 'WL630521 ' TO SSA3                                             
829501     MOVE 'II  ' TO GODK-STATUSKODER                                      
829601     CALL CBLTDLI USING ISRT 6305-PCB DLI-IO-AREA-WDGX2                   
829701                                      SSA1 SSA2 SSA3                      
829801     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
829901     PERFORM IMS-STATUSKONTROLL                                           
830001     .                                                                    
830101     SKIP3                                                                
830201 IMS-REPL-WL630511 SECTION.                                               
830301     MOVE SPACE TO GODK-STATUSKODER                                       
830401     CALL CBLTDLI USING REPL 6305-PCB DLI-IO-AREA-WDGX2                   
830501     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
830601     PERFORM IMS-STATUSKONTROLL                                           
830701     .                                                                    
830801     EJECT                                                                
830901 IMS-ISRT-4506 SECTION.                                                   
831001     STRING 'WL450501(WDGXKEY  =' W-4505-KEY-X ')'                        
831101          DELIMITED BY SIZE INTO SSA1                                     
831201     MOVE 'WL450511 ' TO SSA2                                             
831301     MOVE '  ' TO GODK-STATUSKODER                                        
831401     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA-4505 SSA1 SSA2          
831501     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
831601     PERFORM IMS-STATUSKONTROLL                                           
831701     .                                                                    
831801     SKIP3                                                                
831901 IMS-GU-WDB301 SECTION.                                                   
832001     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
832101                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
832201          DELIMITED BY SIZE INTO SSA1                                     
832301     MOVE '  GE' TO GODK-STATUSKODER                                      
832401     CALL CBLTDLI USING GU KNDB-PCB DLI-IO-AREA-WDB3 SSA1                 
832501     MOVE KNDB-STATUS-CODE TO STATUS-WS                                   
832601     PERFORM IMS-STATUSKONTROLL                                           
832701     .                                                                    
832801     SKIP3                                                                
832901 IMS-ISRT-WLFILC SECTION.                                                 
833001     STRING 'WLFILC01    '                                                
833101          DELIMITED BY SIZE INTO SSA1                                     
833201     MOVE '   ' TO GODK-STATUSKODER                                       
833301     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
833401     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
833501     PERFORM IMS-STATUSKONTROLL                                           
833601     .                                                                    
833701     EJECT                                                                
833801 IMS-GET-WDK711-GE SECTION.                                               
833901                                                                          
834001     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
834101          DELIMITED BY SIZE INTO SSA1                                     
834201     STRING 'WDK711  (IDDC    = ' W-IDDC    ')'                           
834301          DELIMITED BY SIZE INTO SSA2                                     
834401     MOVE '  GE' TO GODK-STATUSKODER                                      
834501     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
834601     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
834701     PERFORM IMS-STATUSKONTROLL                                           
834801     .                                                                    
834901     SKIP3                                                                
835001 IMS-ISRT-WDL901 SECTION.                                                 
835101                                                                          
835201     MOVE 'WLLOGA01 ' TO SSA1                                             
835301     MOVE '  II' TO GODK-STATUSKODER                                      
835401     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
835501     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
835601     PERFORM IMS-STATUSKONTROLL                                           
835701     .                                                                    
835801     EJECT                                                                
835901                                                                          
836001 IMS-GU-LOCB01 SECTION.                                                   
836101                                                                          
836201     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
836301          DELIMITED BY SIZE INTO SSA1                                     
836401     MOVE '  GE' TO GODK-STATUSKODER                                      
836501     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-AREA-LOCB SSA1                 
836601     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
836701     PERFORM IMS-STATUSKONTROLL                                           
836801     .                                                                    
836901     EJECT                                                                
837001                                                                          
837101 IMS-ISRT-LOCB01 SECTION.                                                 
837201                                                                          
837301     MOVE 'WLLOCB01 ' TO SSA1                                             
837401     MOVE '  ' TO GODK-STATUSKODER                                        
837501     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA-LOCB SSA1               
837601     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
837701     PERFORM IMS-STATUSKONTROLL                                           
837801     .                                                                    
837901     SKIP3                                                                
838001                                                                          
838101 IMS-GHNP-LOCB11 SECTION.                                                 
838201                                                                          
838301     STRING 'WLLOCB11(IDDC     =' W-IDDC   ')'                            
838401             DELIMITED BY SIZE INTO SSA1                                  
838501     MOVE '  GE' TO GODK-STATUSKODER                                      
838601     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-AREA-LOCB SSA1               
838701     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
838801     PERFORM IMS-STATUSKONTROLL                                           
838901                                                                          
839001     EJECT                                                                
839101     .                                                                    
839201                                                                          
839301 IMS-REPL-LOCB11 SECTION.                                                 
839401                                                                          
839501     MOVE '  ' TO GODK-STATUSKODER                                        
839601     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-AREA-LOCB                    
839701     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
839801     PERFORM IMS-STATUSKONTROLL                                           
839901     .                                                                    
840001     SKIP3                                                                
840101 IMS-ISRT-LOCB11 SECTION.                                                 
840201                                                                          
840301     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
840401          DELIMITED BY SIZE INTO SSA1                                     
840501     MOVE 'WLLOCB11 ' TO SSA2                                             
840601     MOVE '  II' TO GODK-STATUSKODER                                      
840701     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA-LOCB SSA1 SSA2          
840801     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
840901     PERFORM IMS-STATUSKONTROLL                                           
841001     .                                                                    
841101     EJECT                                                                
841201 IMS-ISRT-WLSAPA01 SECTION.                                               
841301     MOVE 'WLSAPA01 ' TO SSA1                                             
841401     MOVE '  II' TO GODK-STATUSKODER                                      
841501     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
841601     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
841701     PERFORM IMS-STATUSKONTROLL                                           
841801     .                                                                    
841901     SKIP3                                                                
842001 IMS-ISRT-WLFILC2 SECTION.                                                
842101     STRING 'WLFILC01    '                                                
842201          DELIMITED BY SIZE INTO SSA1                                     
842301     MOVE '   ' TO GODK-STATUSKODER                                       
842401     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC2 SSA1              
842501     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
842601     PERFORM IMS-STATUSKONTROLL                                           
842701     .                                                                    
842801     SKIP3                                                                
842901 IMS-ISRT-WLFILC3 SECTION.                                                
843001     STRING 'WLFILC01    '                                                
843101          DELIMITED BY SIZE INTO SSA1                                     
843201     MOVE '   ' TO GODK-STATUSKODER                                       
843301     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC3 SSA1              
843401     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
843501     PERFORM IMS-STATUSKONTROLL                                           
843601     .                                                                    
843701     EJECT                                                                
843801 IMS-ISRT-WLFILC4 SECTION.                                                
843901     STRING 'WLFILC01    '                                                
844001          DELIMITED BY SIZE INTO SSA1                                     
844101     MOVE '   ' TO GODK-STATUSKODER                                       
844201     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC4 SSA1              
844301     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
844401     PERFORM IMS-STATUSKONTROLL                                           
844501     .                                                                    
844601     EJECT                                                                
844701 IMS-GET-WDQ2C SECTION.                                                   
844801     STRING 'WLORQL01(WDQ2C1KY =' W-WDQ2C1KY-X ')'                        
844901          DELIMITED BY SIZE INTO SSA1                                     
845001     MOVE '  GE' TO GODK-STATUSKODER                                      
845101     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-WDQ2C1 SSA1                   
845201     MOVE WDQ2C-STATUS-CODE TO STATUS-WS                                  
845301     PERFORM IMS-STATUSKONTROLL                                           
845401     .                                                                    
845501     SKIP3                                                                
845601                                                                          
845701 IMS-GU-WDB601 SECTION.                                                   
845801     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
845901         DELIMITED BY SIZE INTO SSA1                                      
846001     MOVE '  GE' TO GODK-STATUSKODER                                      
846101     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
846201     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
846301     PERFORM IMS-STATUSKONTROLL                                           
846401     IF SEGMENT-SAKNAS                                                    
846501        MOVE SPACE TO DCS-KDDC                                            
846601     END-IF                                                               
846701     .                                                                    
846801                                                                          
846901 IMS-GU-WDB601-SEND SECTION.                                              
847001     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
847101         DELIMITED BY SIZE INTO SSA1                                      
847201     MOVE '  GE' TO GODK-STATUSKODER                                      
847301     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
847401     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
847501     PERFORM IMS-STATUSKONTROLL                                           
847601     IF SEGMENT-SAKNAS                                                    
847701        MOVE SPACE TO SEND-DCS-KDDC                                       
847801     END-IF                                                               
847901     .                                                                    
848001                                                                          
848101 IMS-GU-WDB601-SW SECTION.                                                
848201     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
848301         DELIMITED BY SIZE INTO SSA1                                      
848401     MOVE '  GE' TO GODK-STATUSKODER                                      
848501     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SW SSA1              
848601     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
848701     PERFORM IMS-STATUSKONTROLL                                           
848801     IF SEGMENT-SAKNAS                                                    
848901        MOVE SPACE TO SW-DCS-KDDC                                         
849001     END-IF                                                               
849101     .                                                                    
849201     EJECT                                                                
849301 IMS-GU-DC71-WDL711 SECTION.                                              
849401                                                                          
849501     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
849601          DELIMITED BY SIZE INTO SSA1                                     
849701     STRING 'WLOIGA11(IDDC    = ' W-IDDC  ')'                             
849801          DELIMITED BY SIZE INTO SSA2                                     
849901     MOVE '  GE' TO GODK-STATUSKODER                                      
850001     CALL CBLTDLI USING GU OIGA-PCB DLI-IO-OIGA11 SSA1 SSA2               
850101     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
850201     PERFORM IMS-STATUSKONTROLL                                           
850301     .                                                                    
850401     SKIP3                                                                
850501 IMS-ROLLBACK       SECTION.                                              
850601     MOVE '  ' TO GODK-STATUSKODER                                        
850701     CALL CBLTDLI USING ROLB MSG-PCB                                      
850801     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
850901     PERFORM IMS-STATUSKONTROLL                                           
851001     .                                                                    
851101     SKIP3                                                                
851201 IMS-STATUSKONTROLL SECTION.                                              
851301                                                                          
851401     SET STATUS-IX TO 1                                                   
851501     SEARCH GODK-STATUS                                                   
851601       AT END                                                             
851701         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
851801         DELIMITED BY SIZE INTO FELTEXT                                   
851901         CALL FELLOG                                                      
852001       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
852101         CONTINUE                                                         
852201     END-SEARCH                                                           
853001     .                                                                    
