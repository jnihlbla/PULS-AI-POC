000100 PROCESS DYNAM                                                            
000200*                                                                         
000300******************************************************************        
000400*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0129      *        
000500******************************************************************        
000600*                                                                         
000700 ID DIVISION.                                                             
000800                                                                          
000900 PROGRAM-ID.     W4034100.                                                
001000 AUTHOR.         CAP GEMINI / BOH.                                        
001100 DATE-WRITTEN.   FEB  86.                                                 
001200 DATE-COMPILED.                                                           
001300                                                                          
001400*    REMARKS.                                                             
001500*                                                                         
001600*    VAL 'U' VID UTSKRIFT AV FÖLJESEDEL SKALL INTE GE                     
001700*    NÅGON UTSKRIFT, MED ÄR ETT GODKÄNT VAL.                              
001800*    FÖR ATT HITTA PRIOBERÄKN. SÖK MED "*PRIO".                           
001900*                                                                         
002000*    FUNKTION.                                                            
002100*        PROGRAMMET SKRIVER FÖLJESEDEL.                                   
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T341U                                             
002500*        MID:         W4I34101                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W4O34101                                            
002900*                     + RAD 1 I MODEN TILL SAMTLIGA ANDRA                 
003000*                       PROGRAM SOM DRAGIT IGÅNG DETTA                    
003100*                       PROGRAM                                           
003200*        LISTA:       FÖLJESEDEL (ENDAST SVENSKA DISTRIKT)                
003300*                     ELLER                                               
003400*        LISTA:       BINNING LIST (ENDAST SDC REFILL-DISTRIKT)           
003500*                                                                         
003600* CHANGE LOG:                                                             
003700*                                                                         
003800*    SKIP3                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000     SKIP3                                                                
004100 DATA DIVISION.                                                           
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP3                                                                
004500                                                                          
004600*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)   VALUE 'W4034100'.             
004800 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004900 77  JA                          PIC X      VALUE 'J'.                    
005000 77  YES                         PIC X      VALUE 'J'.                    
005100 77  NEJ                         PIC X      VALUE 'N'.                    
005200                                                                          
005300 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
005400                                                                          
005500 77  FEL                         PIC X      VALUE 'F'.                    
005600 77  RAETT                       PIC X      VALUE 'R'.                    
005700 77  MER-FINNS                   PIC X      VALUE 'N'.                    
005800 77  PBV-FORMAT                  PIC X      VALUE 'N'.                    
005900 77  LYNK-PART                   PIC X      VALUE 'N'.                    
006000 77  LAST-LYNK-LINE              PIC X      VALUE 'N'.                    
006100 77  FIRST-LINE                  PIC X      VALUE 'J'.                    
006200                                                                          
006300 77  INDX                        PIC S9(9)  VALUE +0   COMP SYNC.         
006400 77  FK-INDX                     PIC S9(9)  VALUE +0   COMP SYNC.         
006500 77  MAX-FK-INDX                 PIC S9(9)  VALUE +6   COMP SYNC.         
006600 77  MOD-LAENGD-OEVRIGA          PIC S9(4)  VALUE +48  COMP SYNC.         
006700 77  MAX-MOD-LENGTH              PIC S9(4)  VALUE +900 COMP SYNC.         
006800 77  KOLLI-MAX-IX                PIC S9(4)  VALUE +130 COMP SYNC.         
006900 77  KOLLI-AKTUELL-IX            PIC S9(4)  VALUE ZERO COMP SYNC.         
007000 77  PERIOD-MANAD-IX             PIC S9(4)  VALUE +0    COMP SYNC.        
007100 77  WS-KOLLI-IX                 PIC 9(4).                                
007200 77  WS-KOLLI-AKTUELL-IX         PIC 9(4).                                
007300 77  WS-EMB-TARA-9               PIC 9(7).                                
007400 77  WS-IDDISTR                  PIC X(4)   VALUE SPACE.                  
007500 77  WS-IDDISTR-NUM5             PIC 9(5)   VALUE ZERO.                   
007600 77  WS-IDKUNDNR                 PIC X(6)   VALUE SPACE.                  
007700 77  WS-IDKUNDNR-NUM7            PIC 9(7)   VALUE ZERO.                   
007800 77  WS-IDORDNR                  PIC X(5)   VALUE SPACE.                  
007900 77  WS-IDORDNR-NUM7             PIC 9(7)   VALUE ZERO.                   
008000 77  WS-IDPRODNR                 PIC 9(7)   VALUE ZERO.                   
008100 77  WS-IDPRODNR-E420F           PIC 9(7)   VALUE ZERO.                   
008200 77  WS-IDKOLLI                  PIC X(5)   VALUE SPACE.                  
008300 77  WS-IDKOLLI-TOM              PIC X(5)   VALUE SPACE.                  
008400 77  WS-DUMMY                    PIC X(2)   VALUE SPACE.                  
008500 77  WS-KDMFSFOR                 PIC 9(1)   VALUE ZERO.                   
008600 77  WS-KDPRTVAL                 PIC XX     VALUE SPACE.                  
008700 77    FILLER                    PIC X(8)   VALUE 'AAAAAAAA'.             
008800 77  WS-TAB-IDKOLLI1             PIC  9(5).                               
008900 77  WS-TAB-IDKOLLI2             PIC  9(5).                               
009000 77  WS-TAB-IDKOLLI3             PIC  9(5).                               
009100 77  WS-TAB-IDKOLLI4             PIC  9(5).                               
009200 77  W-IDDISTR                   PIC S9(5)  VALUE ZERO COMP-3.            
009300 77  W-IDKUNDNR                  PIC S9(7)  VALUE ZERO COMP-3.            
009400 77  W-IDKOLLI                   PIC S9(5)  VALUE ZERO COMP-3.            
009500 77  W-IDKOLLI-TOM               PIC S9(5)  VALUE ZERO COMP-3.            
009600 77  ACK-ANTAL-RADER             PIC S9(4)  VALUE +0   COMP SYNC.         
009700 77  ACK-ANTAL-SIDOR             PIC S9(4)  VALUE +0   COMP SYNC.         
009800 77  MAX-LETTER-RAD-PER-SIDA     PIC S9(4)  VALUE +55  COMP SYNC.         
009900 77  MAX-RADER-PER-SIDA          PIC S9(4)  VALUE +60  COMP SYNC.         
010000 77  MAX-RAD-BINN-LIST-SIDA      PIC S9(4)  VALUE +50  COMP SYNC.         
010100 77  MAX-ANTAL-SIDOR             PIC S9(4)  VALUE +24  COMP SYNC.         
010200 77  WS-SUM-ART                  PIC S9(5)  VALUE +0   COMP-3.            
010300 77  WS-SUM-LEV                  PIC S9(11) VALUE +0   COMP-3.            
010400 77  WS-KDCLAGER                 PIC S9     VALUE +0   COMP-3.            
010500 77  WS-KDFRAKT                  PIC S9(3)  VALUE +0   COMP-3.            
010600 77  WS-FL-SVENSK-FSEDEL         PIC X.                                   
010700 77  WS-VKORDBTO                 PIC S9(4)V9(3) VALUE 0.                  
010800 77  WS-VKARTNTO                 PIC S9(4)V9(3) VALUE 0.                  
010900 77  WS-VLORDBTO                 PIC S9(6)V9(3) VALUE 0.                  
011000 77  WS-VLARTNTO                 PIC S9(8)V9    VALUE 0.                  
011100 77  RADENS-VLARTNTO             PIC S9(6)V9    VALUE 0.                  
011200 77  KOLLITS-VLARTNTO            PIC S9(6)V9    VALUE 0.                  
011300 77  RADENS-VKARTNTO             PIC S9(4)V9(3) VALUE 0.                  
011400 77  KOLLITS-VOLYM               PIC S9(4)V9(3) VALUE 0.                  
011500 77  WS-RATT-PRODNR              PIC X      VALUE SPACE.                  
011600 77  WS-VORD-TIUTSKR             PIC S9(7)  VALUE +0   COMP-3.            
011700 77  WS-VORD-TIUTSTID            PIC S9(7)  VALUE +0   COMP-3.            
011800 01  WS-DISP                     PIC S9(9)  VALUE 0.                      
011900 01  WS-IDTIDZON                 PIC 9(2)   VALUE 0.                      
012000 01  WS-TODAYS-DATE              PIC 9(6)   VALUE 0.                      
012100 77    FILLER                    PIC X(8)   VALUE 'BBBBBBBB'.             
012200 77  WS-INFO-DAREGDAT            PIC  9(8)  VALUE ZERO.                   
012300 77  WS-KORD-IDPLKLST            PIC  9(3).                               
012400 77  WS-KVBEHOV                  PIC S9(7)V9(1) VALUE ZERO.               
012500 77  WS-KVTILLGANG               PIC S9(7)V9(1) VALUE ZERO.               
012600 77  WS-DIFF                     PIC S9(7)V9(1) VALUE ZERO.               
012700 77  WS-IDFAKT                   PIC S9(7)  COMP-3 VALUE ZERO.            
012800 77  WS-ORAD-KVBEART             PIC S9(7)  COMP-3 VALUE ZERO.            
012900 77  WS-SLAG-KVAKS-SDC           PIC S9(7)  COMP-3 VALUE ZERO.            
013000 77  WS-SLAG-KVLS                PIC S9(7)  COMP-3 VALUE ZERO.            
013100 77  WS-SLAG-KVROS               PIC S9(7)  COMP-3 VALUE ZERO.            
013200 77  WS-NDC6-KVLS-TOT            PIC S9(7)  COMP-3 VALUE ZERO.            
013300 77  WS-DCS-IDFTG                PIC  X(2)  VALUE SPACE.                  
013400                                                                          
013500 01  WS-IDPLKLST-GRP             PIC  9(3).                               
013600 01  FILLER REDEFINES WS-IDPLKLST-GRP.                                    
013700     03 FILLER-PLKLST            PIC  9(2).                               
013800     03 WS-IDPLKLST              PIC  9(1).                               
013900                                                                          
014000 01  WS-IDPLOCK-GRP              PIC 9(7).                                
014100 01  FILLER REDEFINES WS-IDPLOCK-GRP.                                     
014200     03  WS-IDPLOCK-PREFIX       PIC 9(2).                                
014300     03  WS-IDPLOCK              PIC 9(5).                                
014400                                                                          
014500 01  WS-TIME-HHMMSS              PIC 9(06).                               
014600 01  FILLER REDEFINES WS-TIME-HHMMSS.                                     
014700     03  WS-TIME-HOUR            PIC 9(02).                               
014800     03  WS-TIME-MINUTE          PIC 9(02).                               
014900     03  WS-TIME-SEC             PIC 9(02).                               
015000                                                                          
015100 01  WS-TIPACTID                 PIC 9(06).                               
015200 01  FILLER REDEFINES WS-TIPACTID.                                        
015300     03  WS-TIPACTIDTIM          PIC 9(02).                               
015400     03  WS-TIPACTIDMIN          PIC 9(02).                               
015500     03  WS-TIPACTIDSEK          PIC 9(02).                               
015600                                                                          
015700 01  WS-DARFS                    PIC 9(12).                               
015800 01  FILLER REDEFINES WS-DARFS.                                           
015900     03  WS-FILLER1              PIC 9(02).                               
016000     03  WS-DARFS-DATE           PIC 9(06).                               
016100     03  WS-DARFS-TIM            PIC 9(02).                               
016200     03  WS-DARFS-MIN            PIC 9(02).                               
016300 77    FILLER                    PIC X(8)   VALUE 'CCCCCCCC'.             
016400                                                                          
016500 01  WS-DATE-YYMMDD              PIC 9(07).                               
016600 01  FILLER REDEFINES WS-DATE-YYMMDD.                                     
016700     03  WS-FILLER1              PIC 9(01).                               
016800     03  WS-YEAR                 PIC 9(02).                               
016900     03  WS-MONTH                PIC 9(02).                               
017000     03  WS-DAY                  PIC 9(02).                               
017100                                                                          
017200 01  WS-BEGDSMRK                 PIC X(94).                               
017300 01  FILLER REDEFINES WS-BEGDSMRK.                                        
017400     03  WS-BEGDSMRK-DEL1        PIC X(30).                               
017500     03  WS-BEGDSMRK-DEL2        PIC X(30).                               
017600     03  WS-BEGDSMRK-DEL3        PIC X(30).                               
017700     03  WS-BEGDSMRK-DEL4        PIC X(04).                               
017800                                                                          
018900 01  WS-IDPRTLST.                                                         
019000     03 WS-SYSTDEL               PIC X(1).                                
019100     03 WS-LISTTYP               PIC X(2).                                
019200     03 WS-DC                    PIC X(2).                                
019300     03 WS-KDPRT                 PIC X(3).                                
019400                                                                          
019500 77  WS-IDTRANS                  PIC X(4).                                
019600     88  EGEN-BILD                          VALUE '4341'.                 
019700     88  6302-BILD                          VALUE '6302'.                 
019800     88  GODKAEND-BILD                      VALUE '433A'                  
019900                                                  '4341'                  
020000                                                  '431D'                  
020100                                                  '431E'                  
020200                                                  '433H'                  
020300                                                  '6302'                  
020310                                                  'L197'                  
020320                                                  'L199'                  
020330                                                  '4327'                  
020400                                                  '433Z'.                 
020500                                                                          
020600 77  WS-FEL-FUNNET               PIC X      VALUE 'N'.                    
020700     88  FEL-FUNNET                         VALUE 'J'.                    
020800     88  FEL-EJ-FUNNET                      VALUE 'N'.                    
020900                                                                          
021000 77  SW-NYCKLAR-OK               PIC X      VALUE 'J'.                    
021100     88  NYCKLAR-OK                         VALUE 'J'.                    
021200     88  NYCKLAR-FEL                        VALUE 'N'.                    
021300                                                                          
021400 77  SW-PURAD                    PIC X      VALUE 'J'.                    
021500     88  PURAD-LOOP                         VALUE 'J'.                    
021600 77  SW-BREAK-LOOP               PIC X      VALUE 'J'.                    
021700     88  BREAK-LOOP                         VALUE 'J'.                    
021800                                                                          
021900 77  IDINLEV-HITTAD-SW           PIC X      VALUE 'N'.                    
022000     88  IDINLEV-HITTAD                     VALUE 'J'.                    
022100                                                                          
022200 77  SAMMA-ART-KOPPL-SW          PIC X      VALUE 'N'.                    
022300     88  SAMMA-ART-KOPPL-E421               VALUE 'J'.                    
022400*                                NÅGON, DVS MINST EN PLOCKLISTA           
022500*                                ÄR GODKÄND.                              
022600*                                FÖLJESEDEL SKALL SKRIVAS.                
022700 77  WS-NGN-PLKLST-GODK          PIC X(1)   VALUE SPACE.                  
022800*                                NÅGON PLOCKLISTA ÄR FELAKTIG.            
022900*                                FÄLTET ANVÄNDS FÖR ATT SPARA             
023000*                                FELTEXT VID 1:A FEL.                     
023100 77  WS-NGN-PLKLST-FEL           PIC X(1)   VALUE SPACE.                  
023200*                                FELTEXT SOM SPARAS VID 1:A FEL.          
023300*                                VISAS OM INGEN PLKLST GODKÄND.           
023400 77  WS-MOD-TEMFSFEL             PIC X(40)  VALUE SPACE.                  
023500 01 DB2-LASNING.                                                          
023600     03 FILLER                   PIC X(16)   VALUE                        
023700                                             'WS-DB2-SEKTION'.            
023800     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
023900                                                                          
024000 01  W-CHECK-DIGTS-SEED.                                                  
024100     03  W-IDPRODNR-SEED         PIC 9(7)   VALUE ZERO.                   
024200     03  W-IDKOLLI-SEED          PIC 9(2)   VALUE ZERO.                   
024300                                                                          
024400 77  WS-SEED                     PIC 9(9)   VALUE ZERO.                   
024500                                                                          
024600 01  WS-VARIABLES.                                                        
024700     03 W-RANDOM-NUM                             PIC V999.                
024800     03 W-RANDOM-NUM-RED REDEFINES W-RANDOM-NUM  PIC 9(3).                
024900                                                                          
025000                                                                          
025100 01  RAD-SKIP                    PIC S9(3)  COMP-3.                       
025200 01  NEXT-RAD-SKIP               PIC S9(3)  COMP-3.                       
025300                                                                          
025400     EJECT                                                                
025500 01 NYCKLAR-TP4TRAN.                                                      
025600     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
025700                                                                          
025800                                                                          
025900 01  GENERELLA-SUBPROGRAM.                                                
026000     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
026100     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
026200     03  WINTSOR                 PIC X(8)   VALUE 'WINTSOR '.             
026300     03  W006PRS1                PIC X(8)   VALUE 'W006PRS1'.             
026400     03  W006PRT                 PIC X(8)   VALUE 'W006PRT '.             
026500     03  W005INIT                PIC X(8)   VALUE 'W005INIT'.             
026600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
026700     SKIP2                                                                
026800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
026900 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT '.            
027000*01 -COPY WMSGINIT                                                        
027100*                                                                         
027200 01  FILLER                      PIC X(16)  VALUE 'WDATAREA '.            
027300*   -COPY WDATAREA                                                        
027400     SKIP2                                                                
027500 01  FILLER                      PIC X(16)  VALUE 'WDAGAREA '.            
027600*   -COPY WDAGAREA                                                        
027700     SKIP2                                                                
027800 01  FILLER                      PIC X(16)  VALUE 'DISTR USA/CAN'.        
027900*   -COPY WWDIST07                                                        
028000     EJECT                                                                
028100 01  FILLER                      PIC X(16)  VALUE 'SATS-DISTR'.           
028200*   -COPY WWDIST19                                                        
028300     EJECT                                                                
028400 01  FILLER                      PIC X(16)  VALUE 'REFILLDISTR'.          
028500*   -COPY WWDIST35                                                        
028600     EJECT                                                                
028700 01  FILLER                      PIC X(16)  VALUE 'DC99KONSTAN'.          
028800*   -COPY WWDC99    -PRE DC99-                                            
028900     EJECT                                                                
029000*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
029100*                                                                         
029200 01  FILLER                      PIC X(16)  VALUE 'LÄNKAREOR'.            
029300*                                                                         
029400 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
029500*   -COPY W006PRT                                                         
029600     EJECT                                                                
029700 01  FILLER                      PIC X(16)  VALUE 'STARTTAB '.            
029800 01  TABENTRY-LNGD               PIC S9(9)  COMP.                         
029900 01  ANTAL-ENTRY                 PIC S9(9)  COMP.                         
030000 01  SORTBGP-LNGD                PIC S9(9)  COMP.                         
030100                                                                          
030200 01  IX-TAB                      PIC S9(9)  COMP-3  VALUE ZERO.           
030300 01  TAB-ANT                     PIC S9(9)  COMP-3  VALUE ZERO.           
030400 01  TAB-MAX                     PIC S9(9)  COMP-3  VALUE 1100.           
030500 01  TAB-MAX-NDC                 PIC S9(9)  COMP-3  VALUE 1100.           
030600 01  NDC-IX                      PIC S9(9)  COMP-3  VALUE ZERO.           
030700 01  FILLER                      PIC X(16)  VALUE 'SORT-TAB '.            
030800 01  TABELL.                                                              
030900     03  TABELL-POST    OCCURS 1100.                                      
031000         05 TAB-PURAD            PIC S9(5)  COMP-3.                       
031100         05 TAB-IDARTNR          PIC S9(9)  COMP-3.                       
031200         05 TAB-REKSIFFR         PIC S9(1)  COMP-3.                       
031300         05 TAB-IDRONR           PIC S9(5)  COMP-3.                       
031400         05 TAB-BERADREF         PIC X(10).                               
031500         05 TAB-ORG              PIC X(2).                                
031600         05 TAB-IDARTNR-ERS      PIC X(1).                                
031700         05 TAB-KVLEVART         PIC S9(7)  COMP-3.                       
031800         05 TAB-INDEX            PIC S9(5)  COMP-3.                       
031900         05 TAB-IDSYSTEM         PIC X(4).                                
032000                                                                          
032100 01  FILLER                      PIC X(16)  VALUE 'NDC-SORT-TAB '.        
032200 01  NDC-TABELL.                                                          
032300     03 NDC-TABELL-POST OCCURS 1100.                                      
032400         05 NDC-TAB-PURAD        PIC S9(5)  COMP-3.                       
032500         05 NDC-TAB-IDARTNR      PIC S9(9)  COMP-3.                       
032600         05 NDC-TAB-REKSIFFR     PIC S9(1)  COMP-3.                       
032700         05 NDC-TAB-KVBEART      PIC S9(7)  COMP-3.                       
032800         05 NDC-TAB-KVLEVART     PIC S9(7)  COMP-3.                       
032900         05 NDC-TAB-IDKOLLI      PIC S9(5)  COMP-3.                       
033000         05 NDC-TAB-BERADREF     PIC X(10).                               
033100         05 NDC-TAB-IDORDNR5-RO  PIC S9(5)  COMP-3.                       
033200         05 NDC-TAB-INDEX        PIC S9(5)  COMP-3.                       
033300*                                                                         
033400 01  FILLER                     PIC X(16) VALUE 'REFILL-SORT-TAB'.        
033500 01  REFILL-TABELL.                                                       
033600     03  REFILL-TABELL-POST    OCCURS 1100.                               
033700         05 REFILL-TAB-PURAD        PIC S9(5)  COMP-3.                    
033800         05 REFILL-TAB-ADLAGPL.                                           
033900           07 REFILL-TAB-ADLAGOMR  PIC S9(3)  COMP-3.                     
034000           07 REFILL-TAB-ADGANG    PIC S9(3)  COMP-3.                     
034100           07 REFILL-TAB-ADPLATS   PIC S9(5)  COMP-3.                     
034200         05 REFILL-TAB-IDARTNR     PIC S9(9)  COMP-3.                     
034300         05 REFILL-TAB-REKSIFFR    PIC S9(1)  COMP-3.                     
034400         05 REFILL-TAB-KVLEVART    PIC S9(7)  COMP-3.                     
034500         05 REFILL-TAB-PRIO        PIC  X(4).                             
034600         05 REFILL-TAB-NEW         PIC  X(3).                             
034700         05 REFILL-TAB-KDARTURS    PIC  X(2).                             
034800         05 REFILL-TAB-BACKORDERED PIC  X(2).                             
034900         05 REFILL-TAB-INDEX       PIC S9(5)  COMP-3.                     
035000     SKIP3                                                                
035100 01  FILLER                   PIC X(16)  VALUE 'SPAR-AREA'.               
035200 01  SPAR.                                                                
035300     03 SPAR-IDRADNR-KO          PIC S9(5)  COMP-3.                       
035400     03 SPAR-IDRONR              PIC S9(5)  COMP-3.                       
035500     03 SPAR-IDARTNR             PIC S9(9)  COMP-3.                       
035600     03 SPAR-REKSIFFR            PIC S9(1)  COMP-3.                       
035700     03 SPAR-IDRADNR             PIC S9(5)  COMP-3.                       
035800     03 SPAR-IDARTNR-ERS         PIC X(1).                                
035900     03 SPAR-KVLEVART            PIC S9(7)  COMP-3.                       
036000     03 SPAR-BEART               PIC X(25).                               
036100     03 SPAR-BERADREF            PIC X(10).                               
036200     03 SPAR-IDSYSTEM            PIC X(4).                                
036300     EJECT                                                                
036400 01  WS-IDKUNDRF.                                                         
036500     03  WS-IDORDNR-KREF         PIC 9(5).                                
036600     03  FILLER                  PIC X(5)   VALUE SPACE.                  
036700                                                                          
036800 01  WS-IDKUNDRF-RO.                                                      
036900     03  WS-IDORDNR-RO           PIC 9(5).                                
037000     03  FILLER                  PIC X(5)   VALUE SPACE.                  
037100                                                                          
037200 01  FILLER                    PIC X(16) VALUE 'TABELL-1'.                
037300 01  TABELL1.                                                             
037400     03  TABELL-POST1     OCCURS 1100.                                    
037500         05  TAB-BEART1          PIC X(25).                               
037600                                                                          
037700 01  FILLER                    PIC X(16) VALUE 'REFILL-TAB'.              
037800 01  REFILL-TABELL1.                                                      
037900     03  REFILL-TABELL-POST1     OCCURS 1100.                             
038000         05  REFILL-TAB-BEART1          PIC X(25).                        
038100                                                                          
038200 01  TABELL-INDEX                PIC S9(5)  COMP-3.                       
038300                                                                          
038400 01  FILLER                    PIC X(16) VALUE 'KOLLI-TAB'.               
038500***  KOLLI-TABELL  *** ANV. FÖR NDC-DEL-NOTE.                             
038600 01  KOLLI-TAB.                                                           
038700   03  KOLLI-TABELL OCCURS 130  INDEXED BY KOLLI-IX.                      
038800     05  TAB-IDKOLLI             PIC S9(5).                               
038900     EJECT                                                                
039000 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
039100 01  NYCKLAR-TILL-DLI.                                                    
039200*                                                                         
039300     03  W-WDE421KY-X.                                                    
039400         05  W-IDPRODNR-E4       PIC S9(7)   COMP-3.                      
039500         05  W-IDKOLLI-e4        PIC S9(5)   COMP-3.                      
039600     03  W-E601-IDPRODNR-X.                                               
039700         05  W-E601-IDPRODNR     PIC S9(7)   COMP-3.                      
039800*                                                                         
039900     03  W-E611-IDKOLLI-X.                                                
040000         05  W-E611-IDKOLLI      PIC S9(5)   COMP-3.                      
040100*                                                                         
040200     03  W-E611-IDKOLLI-TOM-X.                                            
040300         05  W-E611-IDKOLLI-TOM  PIC S9(5)   COMP-3.                      
040400*                                                                         
040500     03  W-DAINLEV-MIN-X.                                                 
040600         05  W-DAINLEV-MIN       PIC  9(16).                              
040700*                                                                         
040800     03  W-DAINLEV-MAX-X.                                                 
040900         05  W-DAINLEV-MAX       PIC  9(16).                              
041000*                                                                         
041100     03  W-E4A1-WDE4KEY-X.                                                
041200         05  W-E4A1-IDDISTR      PIC S9(5)   COMP-3.                      
041300         05  W-E4A1-IDKUNDNR     PIC S9(7)   COMP-3.                      
041400         05  W-E4A1-IDKUNDRF.                                             
041500             07  W-E4A1-IDORDNR  PIC 9(5).                                
041600             07  FILLER          PIC X(5)    VALUE SPACE.                 
041700*                                                                         
041800     03  W-E4F1-WDE4KEY-X.                                                
041900         05  W-E4F1-IDPRODNR     PIC S9(4)   COMP-3.                      
042000         05  W-E4F1-IDKOLLI      PIC S9(3)   COMP-3.                      
042100*                                                                         
042200*  03    W-WDE4F1KY-MAX-X.                                                
042300   03    W-WDE4F1KY.                                                      
042400     05    W-E4F1-IDPRODNR-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
042500     05    W-E4F1-IDKOLLI-MAX    PIC S9(5)   VALUE ZERO  COMP-3.          
042600*    05    W-WDE4F1-MAX.                                                  
042700*    07    W-E4F1-DISTR-MAX    PIC S9(5) COMP-3 VALUE 99999.              
042800*    07    W-E4F1-KUNDNR-MAX    PIC S9(7) COMP-3 VALUE 9999999.           
042900*    07    W-E4F1-KUNDRF-MAX    PIC X(10) VALUE HIGH-VALUE.               
043000*    07    W-E4F1-PLKLST-MAX    PIC S9(3) COMP-3 VALUE 999.               
043100*    07    W-E4F1-PURAD-MAX    PIC S9(5) COMP-3 VALUE 99999.              
043200                                                                          
043300   03    W-WDE4F1KY-MIN-X.                                                
043400     05    W-E4F1-IDPRODNR-MIN   PIC S9(7)   VALUE ZERO  COMP-3.          
043500     05    W-E4F1-IDKOLLI-MIN    PIC S9(5)   VALUE ZERO  COMP-3.          
043600     05    W-WDE4F1-MIN.                                                  
043700     07    W-E4F1-DISTR-MIN    PIC S9(5) COMP-3 VALUE ZERO.               
043800     07    W-E4F1-KUNDNR-MIN    PIC S9(7) COMP-3 VALUE ZERO.              
043900     07    W-E4F1-KUNDRF-MIN    PIC X(10) VALUE LOW-VALUE.                
044000     07    W-E4F1-PLKLST-MIN    PIC S9(3) COMP-3 VALUE ZERO.              
044100     07    W-E4F1-PURAD-MIN    PIC S9(5) COMP-3 VALUE ZERO.               
044200                                                                          
044300                                                                          
044400     EJECT                                                                
044500     03  W-E401-WDE4KEY-X.                                                
044600         05  W-E401-IDDISTR      PIC S9(5)   COMP-3.                      
044700         05  W-E401-IDKUNDNR     PIC S9(7)   COMP-3.                      
044800         05  W-E401-IDKUNDRF.                                             
044900             07  W-E401-IDORDNR  PIC 9(5).                                
045000             07  FILLER          PIC X(5)    VALUE SPACE.                 
045100         05  W-E401-IDPRODNR     PIC S9(7)   COMP-3.                      
045200         05  W-E401-IDPLKLST     PIC S9(3)   COMP-3.                      
045300*                                                                         
045400     03  W-E411-IDPURAD-X.                                                
045500         05  W-E411-IDPURAD      PIC S9(5)   COMP-3.                      
045600*                                                                         
045700     03  W-KDKOLLI               PIC X(8)    VALUE SPACE.                 
045800*                                                                         
045900     03 W-WDB101KY-X.                                                     
046000         05 W-IDPARTNR           PIC  X(9).                               
046100         05 W-IDFTG              PIC  9(2).                               
046200*                                                                         
046300     03 W-IDGMT-X.                                                        
046400         05 W-B201-IDDISTR       PIC S9(5)   COMP-3.                      
046500         05 W-B201-IDKUNDNR      PIC S9(7)   COMP-3.                      
046600*                                                                         
046700     03 W-WDB501KY-X.                                                     
046800         05 W-501-IDDC           PIC X(2).                                
046900         05 W-501-KDFRAKT        PIC S9(3)   VALUE ZERO  COMP-3.          
047000         05  W-IDGMT-WDB5.                                                
047100           07 W-501-IDDISTR      PIC S9(5)   VALUE ZERO  COMP-3.          
047200           07 W-501-IDKUNDNR     PIC S9(7)   VALUE ZERO  COMP-3.          
047300*                                                                         
047400     03 W-WDB501KY-DEFAULT-X.                                             
047500         05 W-501-IDDC-DEFAULT     PIC X(2).                              
047600         05 W-501-KDFRAKT-DEFAULT  PIC S9(3) VALUE ZERO COMP-3.           
047700         05  W-IDGMT-WDB5-DEFAULT.                                        
047800         07 W-501-IDDISTR-DEFAULT PIC S9(5) VALUE ZERO COMP-3.            
047900         07 W-501-IDKUNDNR-DEFAULT PIC S9(7) VALUE 9999999 COMP-3.        
048000*                                                                         
048100     03  W-WDQ301KY-X.                                                    
048200         05  W-ODEL-IDORDER      PIC S9(7)   COMP-3.                      
048300         05  W-ODEL-IDDC         PIC  X(2).                               
048400         05  W-ODEL-IDPRODNR     PIC S9(7)   COMP-3.                      
048500         05  W-ODEL-IDPLKLST     PIC S9(3)   COMP-3.                      
048600*                                                                         
048700     03  W-IDORDER-X.                                                     
048800         05  W-IDORDER           PIC S9(7)   COMP-3.                      
048900*                                                                         
049000     03  W-IDARTNR-X.                                                     
049100         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
049200*                                                                         
049300     03  W-IDDC-X.                                                        
049400         05  W-IDDC              PIC X(2).                                
049500*                                                                         
049600     03  W-IDSKYLT-X.                                                     
049700         05  W-IDSKYLT           PIC X(3).                                
049800*                                                                         
049900     03  W-KDSEGKEY-X.                                                    
050000         05  W-KDSEGKEY          PIC 9(1)   VALUE 1.                      
050100*                                                                         
050200     03 W-WDQ5A1KY-MIN-X.                                                 
050300                                                                          
050400       05  W-Q5A-IDGMTREF-MIN.                                            
050500         07 W-Q5A-IDDISTR-MIN      PIC S9(05)  VALUE ZERO COMP-3.         
050600         07 W-Q5A-IDKUNDNR-MIN     PIC S9(07)  VALUE ZERO COMP-3.         
050700         07 W-Q5A-IDKUNDRF-GRP-MIN.                                       
050800           09 W-Q5A-IDKUNDRF-MIN   PIC  X(10).                            
050900           09 W-Q5A-IDORDNR5 REDEFINES W-Q5A-IDKUNDRF-MIN.                
051000             11 W-Q5A-IDORDNR5-MIN PIC  9(05).                            
051100             11 FILLER             PIC  X(05).                            
051200           09 W-Q5A-IDORDNR7 REDEFINES W-Q5A-IDKUNDRF-MIN.                
051300             11 W-Q5A-IDORDNR7-MIN PIC  9(07).                            
051400             11 FILLER             PIC  X(03).                            
051500       05  FILLER                  PIC X(17) VALUE LOW-VALUE.             
052500                                                                          
052600     03 W-WDQ5A1KY-MAX-X.                                                 
052700       05  W-Q5A-IDGMTREF-MAX.                                            
052800         07 W-Q5A-IDDISTR-MAX      PIC S9(05)   VALUE ZERO                
052900                                                COMP-3.                   
053000         07 W-Q5A-IDKUNDNR-MAX     PIC S9(07)   VALUE ZERO                
053100                                                COMP-3.                   
053200         07 W-Q5A-IDKUNDRF-GRP-MAX.                                       
053300           09 W-Q5A-IDKUNDRF-MAX   PIC  X(10).                            
053400           09 W-Q5A-IDORDNR5 REDEFINES W-Q5A-IDKUNDRF-MAX.                
053500             11 W-Q5A-IDORDNR5-MAX PIC  9(05).                            
053600             11 FILLER             PIC  X(05).                            
053700           09 W-Q5A-IDORDNR7 REDEFINES W-Q5A-IDKUNDRF-MAX.                
053800             11 W-Q5A-IDORDNR7-MAX PIC  9(07).                            
053900             11 FILLER             PIC  X(03).                            
054000       05  FILLER                  PIC X(17) VALUE HIGH-VALUE.            
054900*                                                                         
055000     03  W-IDDC-B6-X.                                                     
055100         05 W-IDDC-B6                  PIC X(2).                          
055200                                                                          
055300                                                                          
055400                                                                          
055500     EJECT                                                                
055600 01  FILLER                    PIC X(16) VALUE 'ERR-INFO-AREA   '.        
055700 01  MEDDELANDE.                                                          
055800                                                                          
055900     03  FEL-1.                                                           
056000         05 FILLER               PIC X(40)   VALUE                        
056100             '770 EJ TILLÅTEN ATT STARTA 4341         '.                  
056200         05 FILLER               PIC X(40)   VALUE                        
056300             '770 NOT ALLOWED TO START 4341           '.                  
056400     03  FEL-770 REDEFINES FEL-1 OCCURS 2  PIC X(40).                     
056500                                                                          
056600     03  FEL-2.                                                           
056700         05 FILLER               PIC X(40)   VALUE                        
056800             '771 PRODNR / KOLLI SKALL VARA NUMERISKA '.                  
056900         05 FILLER               PIC X(40)   VALUE                        
057000             '771 NOT NUMERIC                         '.                  
057100     03  FEL-771 REDEFINES FEL-2 OCCURS 2  PIC X(40).                     
057200                                                                          
057300     03  FEL-3.                                                           
057400         05 FILLER               PIC X(40)   VALUE                        
057500             '772 FELAKTIG PRINTER                    '.                  
057600         05 FILLER               PIC X(40)   VALUE                        
057700             '772 WRONG PRINTER                       '.                  
057800     03  FEL-772 REDEFINES FEL-3 OCCURS 2  PIC X(40).                     
057900                                                                          
058000     03  FEL-4.                                                           
058100         05 FILLER               PIC X(40)   VALUE                        
058200             '773 ENDAST FÖLJESEDEL FÖR SVENSKA DISTR '.                  
058300         05 FILLER               PIC X(40)   VALUE                        
058400             '773 ONLY DELIVERY NOTE FOT SWEDISH DISTR'.                  
058500     03  FEL-773 REDEFINES FEL-4 OCCURS 2  PIC X(40).                     
058600                                                                          
058700     03  FEL-5.                                                           
058800         05 FILLER               PIC X(40)   VALUE                        
058900             '758 KOLLI SAKNAS                        '.                  
059000         05 FILLER               PIC X(40)   VALUE                        
059100             '758 CASE IS MISSING                     '.                  
059200     03  FEL-758 REDEFINES FEL-5 OCCURS 2  PIC X(40).                     
059300                                                                          
059400     03  FEL-61.                                                          
059500         05 FILLER               PIC X(40)   VALUE                        
059600             '7011 ORDERN SAKNAS                      '.                  
059700         05 FILLER               PIC X(40)   VALUE                        
059800             '7011 ORDER MISSING                      '.                  
059900     03  FEL-7011 REDEFINES FEL-61 OCCURS 2  PIC X(40).                   
060000                                                                          
060100     03  FEL-63.                                                          
060200         05 FILLER               PIC X(40)   VALUE                        
060300             '7015 ORDERN SAKNAS                      '.                  
060400         05 FILLER               PIC X(40)   VALUE                        
060500             '7015 ORDER MISSING                      '.                  
060600     03  FEL-7015 REDEFINES FEL-63 OCCURS 2  PIC X(40).                   
060700                                                                          
060800     03  FEL-62.                                                          
060900         05 FILLER               PIC X(40)   VALUE                        
061000             '7012 ORDERN SAKNAS                      '.                  
061100         05 FILLER               PIC X(40)   VALUE                        
061200             '7012 ORDER MISSING                      '.                  
061300     03  FEL-7012 REDEFINES FEL-62 OCCURS 2  PIC X(40).                   
061400                                                                          
061500     03  FEL-71.                                                          
061600         05 FILLER               PIC X(40)   VALUE                        
061700             '7491 FEL NYCKEL                         '.                  
061800         05 FILLER               PIC X(40)   VALUE                        
061900             '7491 WRONG KEY                          '.                  
062000     03  FEL-7491 REDEFINES FEL-71 OCCURS 2  PIC X(40).                   
062100                                                                          
062200     03  FEL-72.                                                          
062300         05 FILLER               PIC X(40)   VALUE                        
062400             '7492 FEL NYCKEL                         '.                  
062500         05 FILLER               PIC X(40)   VALUE                        
062600             '7492 WRONG KEY                          '.                  
062700     03  FEL-7492 REDEFINES FEL-72 OCCURS 2  PIC X(40).                   
062800                                                                          
062900     03  FEL-73.                                                          
063000         05 FILLER               PIC X(40)   VALUE                        
063100             '7493 FEL NYCKEL                         '.                  
063200         05 FILLER               PIC X(40)   VALUE                        
063300             '7493 WRONG KEY                          '.                  
063400     03  FEL-7493 REDEFINES FEL-73 OCCURS 2  PIC X(40).                   
063500                                                                          
063600     03  FEL-74.                                                          
063700         05 FILLER               PIC X(40)   VALUE                        
063800             '7494 FEL NYCKEL                         '.                  
063900         05 FILLER               PIC X(40)   VALUE                        
064000             '7494 WRONG KEY                          '.                  
064100     03  FEL-7494 REDEFINES FEL-74 OCCURS 2  PIC X(40).                   
064200                                                                          
064300     03  FEL-75.                                                          
064400         05 FILLER               PIC X(40)   VALUE                        
064500             '7495 FEL NYCKEL                         '.                  
064600         05 FILLER               PIC X(40)   VALUE                        
064700             '7495 WRONG KEY                          '.                  
064800     03  FEL-7495 REDEFINES FEL-75 OCCURS 2  PIC X(40).                   
064900                                                                          
065000     03  FEL-76.                                                          
065100         05 FILLER               PIC X(40)   VALUE                        
065200             '7496 FEL NYCKEL                         '.                  
065300         05 FILLER               PIC X(40)   VALUE                        
065400             '7496 WRONG KEY                          '.                  
065500     03  FEL-7496 REDEFINES FEL-76 OCCURS 2  PIC X(40).                   
065600                                                                          
065700     03  FEL-77.                                                          
065800         05 FILLER               PIC X(40)   VALUE                        
065900             '7497 ORDEDEL INTE AVSLUTAD.             '.                  
066000         05 FILLER               PIC X(40)   VALUE                        
066100             '7497 ORDERPART IS NOT FINISHED.         '.                  
066200     03  FEL-7497 REDEFINES FEL-77 OCCURS 2  PIC X(40).                   
066300                                                                          
066400     03  FEL-8.                                                           
066500         05 FILLER               PIC X(40)   VALUE                        
066600             '750 ENDAST ETT KOLLI / BINNING-LISTA    '.                  
066700         05 FILLER               PIC X(40)   VALUE                        
066800             '750 ONLY ONE CASE PER BINNING-LIST.     '.                  
066900     03  FEL-750 REDEFINES FEL-8 OCCURS 2  PIC X(40).                     
067000                                                                          
067100     03  FEL-9.                                                           
067200         05 FILLER               PIC X(40)   VALUE                        
067300             '7591 PLOCKLISTA SAKNAS                 '.                   
067400         05 FILLER               PIC X(40)   VALUE                        
067500             '7591 ORDER PART MISSING                 '.                  
067600     03  FEL-7591 REDEFINES FEL-9 OCCURS 2  PIC X(40).                    
067700                                                                          
067800     03  FEL-10.                                                          
067900         05 FILLER               PIC X(40)   VALUE                        
068000             '760 FÖR MÅNGA RADER (>1100) FÖR LISTAN.'.                   
068100         05 FILLER               PIC X(40)   VALUE                        
068200             '760 TOO MUCH LINES (>1100) FOR THIS LIST'.                  
068300     03  FEL-760 REDEFINES FEL-10 OCCURS 2  PIC X(40).                    
068400                                                                          
068500     03  FEL-10.                                                          
068600         05 FILLER               PIC X(40)   VALUE                        
068700             '7592 PLOCKLISTA SAKNAS                 '.                   
068800         05 FILLER               PIC X(40)   VALUE                        
068900             '7592 ORDER PART MISSING                '.                   
069000     03  FEL-7592 REDEFINES FEL-10 OCCURS 2  PIC X(40).                   
069100                                                                          
069200     03  FEL-11.                                                          
069300         05 FILLER               PIC X(40)   VALUE                        
069400             '761 SATSORDER HAR INTE FÖLJESEDEL.     '.                   
069500         05 FILLER               PIC X(40)   VALUE                        
069600             '761 NO DEL.NOTE FOR KIT ORDER.         '.                   
069700     03  FEL-761  REDEFINES FEL-11 OCCURS 2  PIC X(40).                   
069800                                                                          
069900     03  FEL-12.                                                          
070000         05 FILLER               PIC X(40)   VALUE                        
070100             '799 DEL NOTE FINNS EJ.                 '.                   
070200         05 FILLER               PIC X(40)   VALUE                        
070300             '799 NO DEL.NOTE FOR THIS ORDER.        '.                   
070400     03  FEL-799  REDEFINES FEL-12 OCCURS 2  PIC X(40).                   
070500                                                                          
070600     03  FEL-13.                                                          
070700         05 FILLER               PIC X(40)   VALUE                        
070800             '798 DEL NOTE FINNS EJ FÖR LAGRET       '.                   
070900         05 FILLER               PIC X(40)   VALUE                        
071000             '798 NO DEL.NOTE FOR THIS WAREHOUSE     '.                   
071100     03  FEL-798  REDEFINES FEL-13 OCCURS 2  PIC X(40).                   
071200                                                                          
071300     03  MED-1.                                                           
071400         05 FILLER               PIC X(40)   VALUE                        
071500             '    UTSKRIFT STARTAD                    '.                  
071600         05 FILLER               PIC X(40)   VALUE                        
071700             '    PRINT STARTED                       '.                  
071800     03  MED-001 REDEFINES MED-1 OCCURS 2  PIC X(40).                     
071900     EJECT                                                                
072000 01  LIST-TEXTER.                                                         
072100     03  PV-SV-NAMN              PIC X(29)                                
072200          VALUE 'VOLVO PERSONVAGNAR AB, PARTS '.                          
072300     03  PV-ENG-NAMN             PIC X(29)                                
072400          VALUE 'VOLVO CAR CORPORATION, PARTS '.                          
072500     03  LV-PV-ADRESS            PIC X(29)                                
072600          VALUE 'SE-405 31 GÖTEBORG, SWEDEN   '.                          
072700     EJECT                                                                
072800*****************D E L I V E R Y    N O T E ****************              
072900 01  FILLER                    PIC X(16) VALUE 'PRINT-LIST-AREA'.         
073000 01  LIST-RADER.                                                          
073100                                                                          
073200     03  RUBRIKRAD-1.                                                     
073300         05  FILLER              PIC X(15)   VALUE SPACE.                 
073400         05  RUB1-LIST-NAMN      PIC X(42).                               
073500         05  FILLER              PIC X       VALUE SPACE.                 
073600         05  RUB1-CHECK-TEXT     PIC X(10).                               
073700         05  RUB1-CHECK-DIGIT    PIC XXX.                                 
073800         05  FILLER              PIC X(4)    VALUE SPACE.                 
073900         05  RUB1-SIDNR          PIC Z9.                                  
074000         05  FILLER              PIC X(5)    VALUE SPACE.                 
074100                                                                          
074200                                                                          
074300     03  RUBRIKRAD-2.                                                     
074400         05  FILLER              PIC X(1)    VALUE SPACE.                 
074500         05  RAD2-IDDISTR        PIC Z(3)9.                               
074600         05  FILLER              PIC X(9)    VALUE SPACE.                 
074700         05  RAD2-IDKUNDNR       PIC Z(5)9.                               
074800         05  FILLER              PIC X(8)    VALUE SPACE.                 
074900         05  RAD2-IDORDNR        PIC Z(4)9.                               
075000         05  FILLER              PIC X(7)    VALUE SPACE.                 
075100         05  RAD2-KDORDKL        PIC 9.                                   
075200         05  FILLER              PIC X(6)    VALUE SPACE.                 
075300         05  RAD2-IDBORD         PIC Z(3).                                
075400         05  FILLER              PIC X(2)    VALUE SPACE.                 
075500         05  RAD2-IDPLOCK        PIC Z(4)9.                               
075600         05  FILLER              PIC X(06)   VALUE SPACE.                 
075700         05  RAD2-IDPRODNR       PIC Z(6)9.                               
075800         05  FILLER              PIC X(10)   VALUE SPACE.                 
075900                                                                          
076000     03  ADRESSRAD-3.                                                     
076100         05  FILLER              PIC X(1)    VALUE SPACE.                 
076200         05  RAD3-ADDRESS        PIC X(7).                                
076300         05  FILLER              PIC X(28)   VALUE SPACE.                 
076400         05  RAD3-IDKUNDRF       PIC X(12).                               
076500         05  FILLER              PIC X(17)   VALUE SPACE.                 
076600         05  RAD3-RFS            PIC X(13).                               
076700         05  FILLER              PIC X(02)   VALUE SPACE.                 
076800                                                                          
076900     03  ADRESSRAD-4.                                                     
077000         05  FILLER              PIC X(1)    VALUE SPACE.                 
077100         05  RAD4-BEGMT-RAD1     PIC X(34).                               
077200         05  FILLER              PIC X(1)    VALUE SPACE.                 
077300         05  RAD4-BEKUNDRF       PIC X(15)   VALUE SPACE.                 
077400         05  FILLER              PIC X(13)   VALUE SPACE.                 
077500         05  RAD4-RFS-DATE       PIC 9(6).                                
077600         05  FILLER              PIC X(1)    VALUE SPACE.                 
077700         05  RAD4-RFS-TIM        PIC Z(1)9.                               
077800         05  FILLER              PIC X(1)    VALUE ':'.                   
077900         05  RAD4-RFS-MIN        PIC 9(2).                                
078000         05  FILLER              PIC X(3)    VALUE SPACE.                 
078100                                                                          
078200     03  ADRESSRAD-5.                                                     
078300         05  FILLER              PIC X(1)    VALUE SPACE.                 
078400         05  RAD5-BEGMT-RAD2     PIC X(35).                               
078500         05  FILLER              PIC X(46)   VALUE SPACE.                 
078600                                                                          
078700     03  ADRESSRAD-6.                                                     
078800         05  FILLER              PIC X(1)    VALUE SPACE.                 
078900         05  RAD6-ADGMT-GATA     PIC X(34).                               
079000         05  FILLER              PIC X(1)    VALUE SPACE.                 
079100         05  RAD6-CASENO         PIC X(8).                                
079200         05  FILLER              PIC X(4)    VALUE SPACE.                 
079300         05  RAD6-FREIGHT-CODE   PIC X(12).                               
079400         05  FILLER              PIC X(5)    VALUE SPACE.                 
079500         05  RAD6-PACKDATE       PIC X(12).                               
079600         05  FILLER              PIC X(03)   VALUE SPACE.                 
079700                                                                          
079800     03  ADRESSRAD-7.                                                     
079900         05  FILLER              PIC X(1)    VALUE SPACE.                 
080000         05  RAD7-ADGMT-PADR     PIC X(32).                               
080100         05  FILLER              PIC X(1)    VALUE SPACE.                 
080200         05  RAD7-IDKOLLI-FOM    PIC Z(4)9.                               
080300         05  FILLER              PIC X       VALUE SPACE.                 
080400         05  RAD7-HYPEN          PIC X       VALUE SPACE.                 
080500         05  FILLER              PIC X       VALUE SPACE.                 
080600         05  RAD7-IDKOLLI-TOM    PIC ZZZZZ.                               
080700         05  FILLER              PIC X(1)    VALUE SPACE.                 
080800         05  RAD7-KDFRAKT        PIC Z(2)9.                               
080900         05  FILLER              PIC X(15)   VALUE SPACE.                 
081000         05  RAD7-TIPACKN        PIC 9(6).                                
081100         05  FILLER              PIC X(1)    VALUE SPACE.                 
081200         05  RAD7-TIPACTIDTIM    PIC Z(1)9.                               
081300         05  FILLER              PIC X(1)    VALUE ':'.                   
081400         05  RAD7-TIPACTIDMIN    PIC 9(2).                                
081500         05  FILLER              PIC X(03)   VALUE SPACE.                 
081600                                                                          
081700     03  ADRESSRAD-8.                                                     
081800         05  FILLER              PIC X(1)    VALUE SPACE.                 
081900         05  RAD8-ADGMT-LAND     PIC X(35).                               
082000         05  FILLER              PIC X(46)   VALUE SPACE.                 
082100                                                                          
082200     03  RUBRIKRAD-8.                                                     
082300         05  FILLER              PIC X(1)    VALUE SPACE.                 
082400         05  RAD8-O-REF          PIC X(8).                                
082500         05  FILLER              PIC X(2)    VALUE SPACE.                 
082600         05  RAD8-PARTNO         PIC X(9).                                
082700         05  FILLER              PIC X(1)    VALUE SPACE.                 
082800         05  RAD8-BERADREF       PIC X(7).                                
082900         05  FILLER              PIC X(5)    VALUE SPACE.                 
083000         05  RAD8-ORG            PIC X(3)    VALUE 'ORG'.                 
083100         05  FILLER              PIC X(2)    VALUE SPACE.                 
083200         05  RAD8-ETTA           PIC X(1)    VALUE '1'.                   
083300         05  FILLER              PIC X(2)    VALUE SPACE.                 
083400         05  RAD8-DESC           PIC X(11).                               
083500         05  FILLER              PIC X(16)   VALUE SPACE.                 
083600         05  RAD8-DELIVERED      PIC X(10).                               
083700         05  FILLER              PIC X(2)    VALUE SPACE.                 
083800                                                                          
083900     03  DETALJRAD.                                                       
084000         05  FILLER              PIC X(1)    VALUE SPACE.                 
084100         05  RAD-IDORDNR-RO      PIC Z(5).                                
084200         05  FILLER              PIC X(2)    VALUE SPACE.                 
084300         05  RAD-IDARTNR         PIC Z(7)9.                               
084400         05  FILLER              PIC X(1)    VALUE '-'.                   
084500         05  RAD-REKSIFFR        PIC 9.                                   
084600         05  FILLER              PIC X(1)    VALUE SPACE.                 
084700         05  RAD-BERADREF        PIC X(10).                               
084800         05  FILLER              PIC X(2)    VALUE SPACE.                 
084900         05  RAD-ORG             PIC Z(2).                                
085000         05  FILLER              PIC X(2)    VALUE SPACE.                 
085100         05  RAD-IDARTNR-ERS     PIC X.                                   
085200         05  FILLER              PIC X(4)    VALUE SPACE.                 
085300         05  RAD-BEART           PIC X(25)   VALUE SPACE.                 
085400         05  FILLER              PIC X(2)    VALUE SPACE.                 
085500         05  RAD-KVLEVART        PIC Z(6).                                
085600         05  FILLER              PIC X(6)    VALUE SPACE.                 
085700                                                                          
085800     03  DETALJLYNK.                                                      
085900         05  FILLER              PIC X(3)    VALUE SPACE.                 
086000         05  LYNK-IDARTNR        PIC X(13).                               
086100         05  FILLER              PIC X(1)    VALUE '-'.                   
086200         05  LYNK-REKSIFFR       PIC 9.                                   
086300         05  FILLER              PIC X(22)   VALUE SPACE.                 
086400         05  LYNK-BEART          PIC X(25)   VALUE SPACE.                 
086500         05  FILLER              PIC X(14)   VALUE SPACE.                 
086600                                                                          
086700     03  SLUT-RAD.                                                        
086800         05  FILLER              PIC X(1)    VALUE SPACE.                 
086900         05  SLUT-RAD-VOLUME     PIC X(7)    VALUE 'VOLUME:'.             
087000         05  FILLER              PIC X(1)    VALUE SPACE.                 
087100         05  SLUT-RAD-VLORDBTO   PIC Z(3)Z.Z(3).                          
087200         05  FILLER              PIC X(1)    VALUE SPACE.                 
087300         05  SLUT-RAD-M3         PIC X(2)    VALUE 'M3'.                  
087400         05  FILLER              PIC X(1)    VALUE SPACE.                 
087500         05  SLUT-RAD-WEIGHT     PIC X(7).                                
087600         05  FILLER              PIC X(1)    VALUE SPACE.                 
087700         05  SLUT-RAD-VKORDBTO   PIC Z(5)Z.Z.                             
087800         05  FILLER              PIC X(1)    VALUE SPACE.                 
087900         05  SLUT-RAD-KG         PIC X(2)    VALUE 'KG'.                  
088000         05  FILLER              PIC X(4)    VALUE SPACE.                 
088100         05  SLUT-RAD-NUM        PIC X(16)                                
088200             VALUE 'NUMBER-OF-LINES:'.                                    
088300         05  FILLER              PIC X(1)    VALUE SPACE.                 
088400         05  SLUT-RAD-SUM-ART    PIC Z(4)9.                               
088500         05  FILLER              PIC X(17)   VALUE SPACE.                 
088600                                                                          
088700     03  BLANKRAD.                                                        
088800         05  FILLER              PIC X(80)   VALUE SPACE.                 
088900                                                                          
089000     EJECT                                                                
089100*JAPAN*OCH*AUSTRALIEN***** D E L I V E R Y    N O T E ************        
089200*PREFIX  PAC=PACIFIC                                                      
089300 01  PAC-RADER.                                                           
089400                                                                          
089500     03  PAC-RUBRIKRAD-1.                                                 
089600         05  FILLER              PIC X(15)   VALUE SPACE.                 
089700         05  PAC-RUB1-LIST-NAMN  PIC X(42).                               
089800         05  FILLER              PIC X(18)   VALUE SPACE.                 
089900         05  PAC-RUB1-SIDNR      PIC Z9.                                  
090000         05  FILLER              PIC X(5)    VALUE SPACE.                 
090100                                                                          
090200                                                                          
090300     03  PAC-RUBRIKRAD-2.                                                 
090400         05  FILLER              PIC X(1)    VALUE SPACE.                 
090500         05  PAC-RAD2-IDDISTR    PIC Z(3)9.                               
090600         05  FILLER              PIC X(9)    VALUE SPACE.                 
090700         05  PAC-RAD2-IDKUNDNR   PIC Z(5)9.                               
090800         05  FILLER              PIC X(8)    VALUE SPACE.                 
090900         05  PAC-RAD2-IDORDNR    PIC Z(4)9.                               
091000         05  FILLER              PIC X(7)    VALUE SPACE.                 
091100         05  PAC-RAD2-KDORDKL    PIC 9.                                   
091200         05  FILLER              PIC X(6)    VALUE SPACE.                 
091300         05  PAC-RAD2-IDBORD     PIC Z(3).                                
091400         05  FILLER              PIC X(2)    VALUE SPACE.                 
091500         05  PAC-RAD2-IDPLOCK    PIC Z(4)9.                               
091600         05  FILLER              PIC X(06)   VALUE SPACE.                 
091700         05  PAC-RAD2-IDPRODNR   PIC Z(6)9.                               
091800         05  FILLER              PIC X(1)    VALUE '-'.                   
091900         05  PAC-RAD2-IDPLKLST   PIC 9(3).                                
092000         05  FILLER              PIC X(10)   VALUE SPACE.                 
092100                                                                          
092200     03  PAC-ADRESSRAD-3.                                                 
092300         05  FILLER              PIC X(1)    VALUE SPACE.                 
092400         05  PAC-RAD3-BEGMT-RAD1 PIC X(35).                               
092500         05  FILLER              PIC X(01)   VALUE SPACE.                 
092600         05  PAC-RAD3-PRC        PIC X(03).                               
092700         05  FILLER              PIC X(06)   VALUE SPACE.                 
092800         05  PAC-RAD3-PU         PIC X(03).                               
092900         05  FILLER              PIC X(15)   VALUE SPACE.                 
093000         05  PAC-RAD3-REGDATE    PIC X(08).                               
093100         05  FILLER              PIC X(09)   VALUE SPACE.                 
093200                                                                          
093300     03  PAC-ADRESSRAD-4.                                                 
093400         05  FILLER              PIC X(37)   VALUE SPACE.                 
093500         05  PAC-RAD4-IDPRC      PIC X(04)   VALUE SPACE.                 
093600         05  FILLER              PIC X(05)   VALUE SPACE.                 
093700         05  PAC-RAD4-IDLOTNR    PIC Z(3)9.                               
093800         05  FILLER              PIC X(14)   VALUE SPACE.                 
093900         05  PAC-RAD4-RFSDATE    PIC 9(6).                                
094000         05  FILLER              PIC X(1)    VALUE SPACE.                 
094100         05  PAC-RAD4-RFS-TIM    PIC Z(1)9.                               
094200         05  FILLER              PIC X(1)    VALUE ':'.                   
094300         05  PAC-RAD4-RFS-MIN    PIC 9(2).                                
094400         05  FILLER              PIC X(03)   VALUE SPACE.                 
094500                                                                          
094600     03  PAC-ADRESSRAD-5.                                                 
094700         05  FILLER              PIC X(1)    VALUE SPACE.                 
094800         05  PAC-RAD5-BEGMT-RAD2 PIC X(35).                               
094900         05  FILLER              PIC X(47)   VALUE SPACE.                 
095000                                                                          
095100     03  PAC-ADRESSRAD-6.                                                 
095200         05  FILLER              PIC X(37)   VALUE SPACE.                 
095300         05  PAC-RAD6-FC         PIC X(2).                                
095400         05  FILLER              PIC X(04)   VALUE SPACE.                 
095500         05  PAC-RAD6-CARRIER    PIC X(07).                               
095600         05  FILLER              PIC X(16)   VALUE SPACE.                 
095700         05  PAC-RAD6-PRINTDATE  PIC X(11).                               
095800         05  FILLER              PIC X(06)   VALUE SPACE.                 
095900                                                                          
096000     03  PAC-ADRESSRAD-7.                                                 
096100         05  FILLER              PIC X(1)    VALUE SPACE.                 
096200         05  PAC-RAD7-ADGMT-GATA PIC X(35).                               
096300         05  FILLER              PIC X(1)    VALUE SPACE.                 
096400         05  PAC-RAD7-KDFRAKT    PIC Z(1)9.                               
096500         05  FILLER              PIC X(04)   VALUE SPACE.                 
096600         05  PAC-RAD7-BEKUNDRF   PIC X(15).                               
096700         05  FILLER              PIC X(07)   VALUE SPACE.                 
096800         05  PAC-RAD7-TIPACKN    PIC 9(6).                                
096900         05  FILLER              PIC X(1)    VALUE SPACE.                 
097000         05  PAC-RAD7-TIPACHH    PIC Z(1)9.                               
097100         05  FILLER              PIC X(1)    VALUE ':'.                   
097200         05  PAC-RAD7-TIPACMM    PIC 9(2).                                
097300         05  FILLER              PIC X(03)   VALUE SPACE.                 
097400                                                                          
097500     03  PAC-ADRESSRAD-8.                                                 
097600         05  FILLER              PIC X(1)    VALUE SPACE.                 
097700         05  PAC-RAD8-ADGMT-PADR PIC X(35).                               
097800         05  FILLER              PIC X(47)   VALUE SPACE.                 
097900                                                                          
098000     03  PAC-ADRESSRAD-9.                                                 
098100         05  FILLER              PIC X(1)    VALUE SPACE.                 
098200         05  PAC-RAD9-ADGMT-LAND PIC X(35).                               
098300         05  FILLER              PIC X(47)   VALUE SPACE.                 
098400                                                                          
098500     03  PAC-RUBRIKRAD-8.                                                 
098600         05  FILLER              PIC X(1)    VALUE SPACE.                 
098700         05  PAC-RAD8-PARTNO     PIC X(9).                                
098800         05  FILLER              PIC X(1)    VALUE SPACE.                 
098900         05  PAC-RAD8-DESC       PIC X(25).                               
099000         05  FILLER              PIC X(1)    VALUE SPACE.                 
099100         05  PAC-RAD8-ORDQTY     PIC X(06).                               
099200         05  FILLER              PIC X(3)    VALUE SPACE.                 
099300         05  PAC-RAD8-DELQTY     PIC X(06).                               
099400         05  FILLER              PIC X(1)    VALUE SPACE.                 
099500         05  PAC-RAD8-CASENO     PIC X(08).                               
099600         05  FILLER              PIC X(3)    VALUE SPACE.                 
099700         05  PAC-RAD8-LOCATION   PIC X(08).                               
099800         05  FILLER              PIC X(3)    VALUE SPACE.                 
099900         05  PAC-RAD8-O-REF      PIC X(5).                                
100000         05  FILLER              PIC X(2)    VALUE SPACE.                 
100100                                                                          
100200     03  PAC-DETALJRAD.                                                   
100300         05  FILLER              PIC X(1)    VALUE SPACE.                 
100400         05  PAC-RAD-IDARTNR     PIC Z(7)9.                               
100500         05  FILLER              PIC X(1)    VALUE '-'.                   
100600         05  PAC-RAD-REKSIFFR    PIC 9.                                   
100700         05  FILLER              PIC X(1)    VALUE SPACE.                 
100800         05  PAC-RAD-BEART       PIC X(25)   VALUE SPACE.                 
100900         05  PAC-RAD-KVBEART     PIC Z(6).                                
101000         05  FILLER              PIC X(2)    VALUE SPACE.                 
101100         05  PAC-RAD-KVLEVART    PIC Z(6).                                
101200         05  FILLER              PIC X(2)    VALUE SPACE.                 
101300         05  PAC-RAD-IDKOLLI     PIC Z(5).                                
101400         05  FILLER              PIC X(5)    VALUE SPACE.                 
101500         05  PAC-RAD-BERADREF    PIC X(10).                               
101600         05  FILLER              PIC X(2)    VALUE SPACE.                 
101700         05  PAC-RAD-IDORDNR-RO  PIC Z(5).                                
101800         05  FILLER              PIC X(2)    VALUE SPACE.                 
101900                                                                          
102000     03  PAC-END-LINE.                                                    
102100         05  FILLER              PIC X(01)    VALUE SPACE.                
102200         05  PAC-END-LINE-GRWEIGHT PIC X(10)  VALUE 'GR WEIGHT:'.         
102300         05  FILLER              PIC X(01)    VALUE SPACE.                
102400         05  PAC-END-LINE-VKORDBTO PIC Z(05)Z.Z.                          
102500         05  FILLER              PIC X(01)    VALUE SPACE.                
102600         05  PAC-END-LINE-KG     PIC X(06)    VALUE 'KG    '.             
102700         05  FILLER              PIC X(04)    VALUE SPACE.                
102800         05  PAC-END-LINE-VOLUME PIC X(08)    VALUE 'VOLUME: '.           
102900         05  FILLER              PIC X(01)    VALUE SPACE.                
103000         05  PAC-END-LINE-VLORDBTO PIC Z(03)Z.Z(3).                       
103100         05  FILLER              PIC X(01)    VALUE SPACE.                
103200         05  PAC-END-LINE-M3     PIC X(03)    VALUE 'M3 '.                
103300         05  FILLER              PIC X(01)    VALUE SPACE.                
103400         05  PAC-END-LINE-NUM    PIC X(12)                                
103500             VALUE 'NO OF LINES:'.                                        
103600         05  FILLER              PIC X(1)     VALUE SPACE.                
103700         05  PAC-END-LINE-SUM-ART PIC Z(4)9.                              
103800         05  FILLER              PIC X(14)    VALUE '**'.                 
103900                                                                          
104000     03  PAC-ERROR-LINE.                                                  
104100         05  FILLER              PIC X(80)   VALUE SPACE.                 
104200                                                                          
104300     EJECT                                                                
104400*BINN****REFILL ORDER****B I N N I N G    L I S T**************           
104500 01  REFILL-RADER.                                                        
104600                                                                          
104700     03  REF-RUBRIKRAD-1.                                                 
104800         05  FILLER              PIC X(05)   VALUE SPACE.                 
104900         05  REF-RAD1-LIST-NAMN  PIC X(30).                               
105000         05  FILLER              PIC X(39)   VALUE SPACE.                 
105100         05  REF-RAD1-SIDNR      PIC Z9.                                  
105200         05  FILLER              PIC X(4)    VALUE SPACE.                 
105300                                                                          
105400     03  REF-RUBRIKRAD-2.                                                 
105500         05  FILLER              PIC X(1)    VALUE SPACE.                 
105600         05  REF-RAD2-IDDISTR    PIC Z(3)9.                               
105700         05  FILLER              PIC X(7)    VALUE SPACE.                 
105800         05  REF-RAD2-IDKUNDNR   PIC Z(5)9.                               
105900         05  FILLER              PIC X(7)    VALUE SPACE.                 
106000         05  REF-RAD2-IDORDNR    PIC Z(4)9.                               
106100         05  FILLER              PIC X(7)    VALUE SPACE.                 
106200         05  REF-RAD2-KDORDKL    PIC 9.                                   
106300         05  FILLER              PIC X(6)    VALUE SPACE.                 
106400         05  REF-RAD2-IDBORD     PIC Z(3).                                
106500         05  FILLER              PIC X(2)    VALUE SPACE.                 
106600         05  REF-RAD2-IDPLOCK    PIC Z(4)9.                               
106700         05  FILLER              PIC X(06)   VALUE SPACE.                 
106800         05  REF-RAD2-IDPRODNR   PIC Z(6)9.                               
106900         05  FILLER              PIC X(12)   VALUE SPACE.                 
107000                                                                          
107100     03  REF-ADRESSRAD-3.                                                 
107200         05  FILLER              PIC X(1)    VALUE SPACE.                 
107300         05  REF-RAD3-ADDRESS    PIC X(7)    VALUE 'ADDRESS'.             
107400         05  FILLER              PIC X(28)   VALUE SPACE.                 
107500         05  REF-RAD3-KDKOLLI    PIC X(12)  VALUE 'PACKAGE CODE'.         
107600         05  FILLER              PIC X(07)   VALUE SPACE.                 
107700         05  REF-RAD3-RFS        PIC X(3)    VALUE 'RFS'.                 
107800         05  FILLER              PIC X(22)   VALUE SPACE.                 
107900                                                                          
108000     03  REF-ADRESSRAD-4.                                                 
108100         05  FILLER              PIC X(1)    VALUE SPACE.                 
108200         05  REF-RAD4-BEGMT-RAD1 PIC X(34).                               
108300         05  FILLER              PIC X(1)    VALUE SPACE.                 
108400         05  REF-RAD4-KDKOLLI    PIC X(8).                                
108500         05  FILLER              PIC X(11)   VALUE SPACE.                 
108600         05  REF-RAD4-RFS-DATE   PIC X(6).                                
108700         05  FILLER              PIC X(1)    VALUE SPACE.                 
108800         05  REF-RAD4-RFS-TIM    PIC X(2).                                
108900         05  FILLER              PIC X(1)    VALUE ':'.                   
109000         05  REF-RAD4-RFS-MIN    PIC X(2).                                
109100         05  FILLER              PIC X(13)   VALUE SPACE.                 
109200                                                                          
109300     03  REF-ADRESSRAD-5.                                                 
109400         05  FILLER              PIC X(1)    VALUE SPACE.                 
109500         05  REF-RAD5-BEGMT-RAD2 PIC X(34).                               
109600         05  FILLER              PIC X(1)    VALUE SPACE.                 
109700         05  REF-RAD5-INVOICENO  PIC X(10)    VALUE 'INVOICE NO'.         
109800         05  FILLER              PIC X(01)   VALUE SPACE.                 
109900         05  REF-RAD5-IDFAKT     PIC Z(6)9.                               
110000         05  FILLER              PIC X(26)   VALUE SPACE.                 
110100                                                                          
110200     03  REF-ADRESSRAD-6.                                                 
110300         05  FILLER              PIC X(1)    VALUE SPACE.                 
110400         05  REF-RAD6-ADGMT-GATA PIC X(34).                               
110500         05  FILLER              PIC X(1)    VALUE SPACE.                 
110600         05  REF-RAD6-CASENO     PIC X(07)    VALUE 'CASE NO'.            
110700         05  REF-RAD7-IDKOLLI-FOM PIC Z(3)9.                              
110800         05  FILLER              PIC X       VALUE SPACE.                 
110900         05  REF-RAD7-HYPEN      PIC X       VALUE '-'.                   
111000         05  FILLER              PIC X       VALUE SPACE.                 
111100         05  REF-RAD7-IDKOLLI-TOM PIC ZZZZ.                               
111200         05  FILLER              PIC X(01)   VALUE SPACE.                 
111300         05  REF-RAD6-PACKDATE   PIC X(09)    VALUE 'PACK DATE'.          
111400         05  FILLER              PIC X(15)   VALUE SPACE.                 
111500                                                                          
111600     03  REF-ADRESSRAD-7.                                                 
111700         05  FILLER              PIC X(1)    VALUE SPACE.                 
111800         05  REF-RAD7-ADGMT-PADR PIC X(31).                               
111900         05  FILLER              PIC X(23)   VALUE SPACE.                 
112000         05  REF-RAD7-TIPACKN    PIC 9(6).                                
112100         05  FILLER              PIC X(1)    VALUE SPACE.                 
112200         05  REF-RAD7-TIPACTIDTIM    PIC Z(1)9.                           
112300         05  FILLER              PIC X(1)    VALUE ':'.                   
112400         05  REF-RAD7-TIPACTIDMIN    PIC 9(2).                            
112500         05  FILLER              PIC X(13)   VALUE SPACE.                 
112600                                                                          
112700     03  REF-ADRESSRAD-8.                                                 
112800         05  FILLER              PIC X(1)    VALUE SPACE.                 
112900         05  REF-RAD8-ADGMT-LAND PIC X(35).                               
113000         05  FILLER              PIC X(47)   VALUE SPACE.                 
113100                                                                          
113200     03  REF-RUBRIKRAD-8.                                                 
113300         05  FILLER              PIC X(1)    VALUE SPACE.                 
113400         05  REF-RAD8-NEW        PIC X(3)    VALUE 'NEW'.                 
113500         05  FILLER              PIC X(1)    VALUE '/'.                   
113600         05  REF-RAD8-PRIO       PIC X(4)    VALUE 'PRIO'.                
113700         05  FILLER              PIC X(1)    VALUE SPACE.                 
113800         05  REF-RAD8-PARTNO     PIC X(07)   VALUE 'PART NO'.             
113900         05  FILLER              PIC X(4)    VALUE SPACE.                 
114000         05  REF-RAD8-SDC-LOC    PIC X(11)   VALUE 'DC LOCATION'.         
114100         05  FILLER              PIC X(1)    VALUE SPACE.                 
114200         05  REF-RAD8-QUALITY    PIC X(04)   VALUE 'QUAL'.                
114300         05  FILLER              PIC X(1)    VALUE SPACE.                 
114400         05  REF-RAD8-HAZARDOUS  PIC X(01)   VALUE 'H'.                   
114500         05  FILLER              PIC X(1)    VALUE SPACE.                 
114600         05  REF-RAD8-ORI        PIC X(03)   VALUE 'ORI'.                 
114700         05  FILLER              PIC X(1)    VALUE SPACE.                 
114800         05  REF-RAD8-DESC       PIC X(11)   VALUE 'DESCRIPTION'.         
114900         05  FILLER              PIC X(09)   VALUE SPACE.                 
115000         05  REF-RAD8-QUANTITY   PIC X(3)    VALUE 'QTY'.                 
115100         05  FILLER              PIC X(2)    VALUE SPACE.                 
115200         05  REF-RAD8-BIN        PIC X(3)    VALUE 'BIN'.                 
115300         05  FILLER              PIC X(1)    VALUE SPACE.                 
115400                                                                          
115500     03  REF-RUBRIKRAD-8-2.                                               
115600         05  FILLER              PIC X(33)   VALUE SPACE.                 
115700         05  REF-RAD8-NOTE       PIC X(04)   VALUE 'NOTE'.                
115800         05  FILLER              PIC X(03)   VALUE SPACE.                 
115900         05  REF-RAD8-GIN        PIC X(03)   VALUE 'GIN'.                 
116000         05  FILLER              PIC X(24)   VALUE SPACE.                 
116100                                                                          
116200     03  REF-DETALJRAD.                                                   
116300         05  FILLER              PIC X(1)    VALUE SPACE.                 
116400         05  REF-RAD-NEW         PIC X(3).                                
116500         05  FILLER              PIC X(1)    VALUE SPACE.                 
116600         05  REF-RAD-PRIO        PIC X(4).                                
116700         05  REF-RAD-IDARTNR     PIC Z(08)9.                              
116800         05  FILLER              PIC X(01)    VALUE '-'.                  
116900         05  REF-RAD-REKSIFFR    PIC 9.                                   
117000         05  FILLER              PIC X(01)    VALUE SPACE.                
117100         05  REF-RAD-ADLAGOMR    PIC ZZ.                                  
117200         05  FILLER              PIC X        VALUE SPACE.                
117300         05  REF-RAD-ADGANG      PIC ZZ.                                  
117400         05  FILLER              PIC X        VALUE SPACE.                
117500         05  REF-RAD-ADPLATS     PIC Z(04)9.                              
117600         05  FILLER              PIC X(02)    VALUE SPACE.                
117700         05  REF-RAD-KVALITETSNOTERING PIC X(02)    VALUE SPACE.          
117800         05  FILLER              PIC X(02)    VALUE SPACE.                
117900         05  REF-RAD-HAZARDOUS   PIC X(02)    VALUE SPACE.                
118000         05  REF-RAD-ORIGIN      PIC X(03)    VALUE SPACE.                
118100         05  FILLER              PIC X(01)    VALUE SPACE.                
118200         05  REF-RAD-BEART       PIC X(18)    VALUE SPACE.                
118300         05  REF-RAD-KVLEVART    PIC Z(05)9.                              
118400         05  FILLER              PIC X(01)    VALUE SPACE.                
118500         05  REF-RAD-DEVIATION   PIC X(04)    VALUE '....'.               
118600         05  FILLER              PIC X(07)   VALUE SPACE.                 
118700*NDC-NA RUBRIKRAD-NDC-NA OCH DETALJRAD-NDC-NA.                            
118800     03  REF-RUBRIKRAD-NDC-NA.                                            
118900         05  FILLER              PIC X(1)    VALUE SPACE.                 
119000         05  REF-NDC-NA-NEW      PIC X(3)    VALUE 'NEW'.                 
119100         05  FILLER              PIC X(1)    VALUE '/'.                   
119200         05  REF-NDC-NA-PRIO     PIC X(4)    VALUE 'PRIO'.                
119300         05  FILLER              PIC X(1)    VALUE SPACE.                 
119400         05  REF-NDC-NA-PARTNO   PIC X(07)   VALUE 'PART NO'.             
119500         05  FILLER              PIC X(4)    VALUE SPACE.                 
119600         05  REF-NDC-NA-DESC     PIC X(11)   VALUE 'DESCRIPTION'.         
119700         05  FILLER              PIC X(08)   VALUE SPACE.                 
119800         05  REF-NDC-NA-SDC-LOC  PIC X(11)   VALUE 'DC LOCATION'.         
119900         05  FILLER              PIC X(2)    VALUE SPACE.                 
120000         05  REF-NDC-NA-QUANTITY PIC X(3)    VALUE 'QTY'.                 
120100         05  FILLER              PIC X(5)    VALUE SPACE.                 
120200         05  REF-NDC-NA-BIN      PIC X(3)    VALUE 'BIN'.                 
120300         05  FILLER              PIC X(3)    VALUE SPACE.                 
120400         05  REF-NDC-NA-HAZARDOUS PIC X(01)  VALUE 'H'.                   
120500         05  FILLER              PIC X(1)    VALUE SPACE.                 
120600         05  REF-NDC-NA-ORI      PIC X(03)   VALUE 'ORI'.                 
120700         05  FILLER              PIC X(1)    VALUE SPACE.                 
120800         05  REF-NDC-NA-QUALITY  PIC X(04)   VALUE 'QUAL'.                
120900         05  FILLER              PIC X(1)    VALUE SPACE.                 
121000                                                                          
121100     03  REF-RUBRIKRAD-NDC-NA-2.                                          
121200         05  FILLER              PIC X(69)   VALUE SPACE.                 
121300         05  REF-NDC-NA-GIN      PIC X(03)   VALUE '   '.                 
121400         05  FILLER              PIC X(01)   VALUE SPACE.                 
121500         05  REF-NDC-NA-NOTE     PIC X(04)   VALUE 'NOTE'.                
121600         05  FILLER              PIC X(09)   VALUE SPACE.                 
121700                                                                          
121800     03  REF-DETALJRAD-NDC-NA.                                            
121900         05  FILLER              PIC X(1)    VALUE SPACE.                 
122000         05  REF-NDC-NEW         PIC X(3).                                
122100         05  FILLER              PIC X(1)    VALUE SPACE.                 
122200         05  REF-NDC-PRIO        PIC X(4).                                
122300         05  REF-NDC-IDARTNR     PIC Z(08)9.                              
122400         05  FILLER              PIC X(01)    VALUE '-'.                  
122500         05  REF-NDC-REKSIFFR    PIC 9.                                   
122600         05  FILLER              PIC X(01)    VALUE SPACE.                
122700         05  REF-NDC-BEART       PIC X(18)    VALUE SPACE.                
122800         05  FILLER              PIC X(01)    VALUE SPACE.                
122900         05  REF-NDC-ADLAGOMR    PIC 99.                                  
123000         05  FILLER              PIC X        VALUE SPACE.                
123100         05  REF-NDC-ADGANG      PIC 99.                                  
123200         05  FILLER              PIC X        VALUE SPACE.                
123300         05  REF-NDC-ADPLATS     PIC 9(05).                               
123400         05  REF-NDC-KVLEVART    PIC Z(05)9.                              
123500         05  FILLER              PIC X(01)    VALUE SPACE.                
123600         05  REF-NDC-DEVIATION   PIC X(08)    VALUE '........'.           
123700         05  FILLER              PIC X(01)    VALUE SPACE.                
123800         05  REF-NDC-HAZARDOUS   PIC X(02)    VALUE SPACE.                
123900         05  REF-NDC-ORIGIN      PIC X(03)    VALUE SPACE.                
124000         05  FILLER              PIC X(02)    VALUE SPACE.                
124100         05  REF-NDC-KVALITETSNOTERING PIC X(02)    VALUE SPACE.          
124200         05  FILLER              PIC X(04)   VALUE SPACE.                 
124300                                                                          
124400*END NDC-NA                                                               
124500*NDC-PACIFIC RUBRIKRAD NDC-PACIFIC OCH DETALJRAD-NDC-PACIFIC.             
124600                                                                          
124700     03  REF-RUBRIKRAD-NDC-PAC.                                           
124800         05  FILLER                    PIC X(1) VALUE SPACE.              
124900         05  REF-NDC-PACIFIC-NEW       PIC X(3) VALUE 'NEW'.              
125000         05  FILLER                    PIC X(1) VALUE '/'.                
125100         05  REF-NDC-PACIFIC-PRIO      PIC X(4) VALUE 'PRIO'.             
125200         05  FILLER                    PIC X(1) VALUE SPACE.              
125300         05  REF-NDC-PACIFIC-PARTNO    PIC X(07) VALUE 'PART NO'.         
125400         05  FILLER                    PIC X(4) VALUE SPACE.              
125500         05  REF-NDC-PACIFIC-LOC    PIC X(11) VALUE 'DC LOCATION'.        
125600         05  FILLER                    PIC X(2) VALUE SPACE.              
125700         05  REF-NDC-PACIFIC-QUALITY   PIC X(04) VALUE 'QUAL'.            
125800         05  FILLER                    PIC X(1) VALUE SPACE.              
125900         05  REF-NDC-PACIFIC-HAZARDOUS PIC X(01) VALUE 'H'.               
126000         05  FILLER                    PIC X(1) VALUE SPACE.              
126100         05  REF-NDC-PACIFIC-BO        PIC X(02) VALUE 'BO'.              
126200         05  FILLER                    PIC X(1) VALUE SPACE.              
126300         05  REF-NDC-PACIFIC-DESC   PIC X(11) VALUE 'DESCRIPTION'.        
126400         05  FILLER                    PIC X(08) VALUE SPACE.             
126500         05  REF-NDC-PACIFIC-QUANTITY  PIC X(3) VALUE 'QTY'.              
126600         05  FILLER                    PIC X(5) VALUE SPACE.              
126700         05  REF-NDC-PACIFIC-BIN       PIC X(3) VALUE 'BIN'.              
126800         05  FILLER                    PIC X(3) VALUE SPACE.              
126900                                                                          
127000     03  REF-RUBRIKRAD-NDC-PAC-2.                                         
127100         05  FILLER                    PIC X(34) VALUE SPACE.             
127200         05  REF-NDC-PACIFIC-NOTE      PIC X(04) VALUE 'NOTE'.            
127300         05  FILLER                    PIC X(33) VALUE SPACE.             
127400                                                                          
127500     03  REF-DETALJRAD-NDC-PACIFIC.                                       
127600         05  FILLER                    PIC X(1)  VALUE SPACE.             
127700         05  REF-NDC-PAC-NEW           PIC X(3).                          
127800         05  FILLER                    PIC X(1)  VALUE SPACE.             
127900         05  REF-NDC-PAC-PRIO          PIC X(4).                          
128000         05  REF-NDC-PAC-IDARTNR       PIC Z(08)9.                        
128100         05  FILLER                    PIC X(01) VALUE '-'.               
128200         05  REF-NDC-PAC-REKSIFFR      PIC 9.                             
128300         05  FILLER                    PIC X(01) VALUE SPACE.             
128400         05  REF-NDC-PAC-ADLAGOMR      PIC 99.                            
128500         05  FILLER                    PIC X     VALUE SPACE.             
128600         05  REF-NDC-PAC-ADGANG        PIC 99.                            
128700         05  FILLER                    PIC X     VALUE SPACE.             
128800         05  REF-NDC-PAC-ADPLATS       PIC 9(05).                         
128900         05  FILLER                    PIC X(01) VALUE SPACE.             
129000         05  REF-NDC-PAC-KVALITETSNOTERING PIC X(02) VALUE SPACE.         
129100         05  FILLER                    PIC X(04) VALUE SPACE.             
129200         05  REF-NDC-PAC-HAZARDOUS     PIC X(03) VALUE SPACE.             
129300         05  REF-NDC-PAC-BACKORDERED   PIC X(02) VALUE SPACE.             
129400         05  FILLER                    PIC X(01) VALUE SPACE.             
129500         05  REF-NDC-PAC-BEART         PIC X(18) VALUE SPACE.             
129600         05  FILLER                    PIC X(01) VALUE SPACE.             
129700         05  REF-NDC-PAC-KVLEVART      PIC Z(05)9.                        
129800         05  FILLER                    PIC X(01) VALUE SPACE.             
129900         05  REF-NDC-PAC-DEVIATION     PIC X(08) VALUE '........'.        
130000         05  FILLER                    PIC X(04) VALUE SPACE.             
130100                                                                          
130200     03  REF-END-LINE.                                                    
130300         05  FILLER              PIC X(1)    VALUE SPACE.                 
130400         05  END-LINE-VOL        PIC X(7)    VALUE 'VOLUME:'.             
130500         05  FILLER              PIC X(1)    VALUE SPACE.                 
130600         05  END-LINE-VLORDBTO   PIC Z(3)Z.Z(3).                          
130700         05  FILLER              PIC X(1)    VALUE SPACE.                 
130800         05  END-LINE-M3         PIC X(3)    VALUE ' M3'.                 
130900         05  FILLER              PIC X(1)    VALUE SPACE.                 
131000         05  END-LINE-WEI        PIC X(7)    VALUE SPACE.                 
131100         05  FILLER              PIC X(1)    VALUE SPACE.                 
131200         05  END-LINE-VKORDBTO   PIC Z(5)Z.Z.                             
131300         05  FILLER              PIC X(1)    VALUE SPACE.                 
131400         05  END-LINE-KG         PIC X(2)    VALUE 'KG'.                  
131500         05  FILLER              PIC X(4)    VALUE SPACE.                 
131600         05  END-LINE-NUM        PIC X(16)                                
131700             VALUE 'NUMBER-OF-PARTS:'.                                    
131800         05  FILLER              PIC X(1)    VALUE SPACE.                 
131900         05  END-LINE-SUM-ART    PIC Z(4)9.                               
132000         05  FILLER              PIC X(14)   VALUE SPACE.                 
132100                                                                          
132200     03  REF-BLANKRAD.                                                    
132300         05  FILLER              PIC X(80)   VALUE SPACE.                 
132400                                                                          
132500     EJECT                                                                
132600*01  -COPY W006PRAR                                                       
132700     EJECT                                                                
132800 01    FILLER                 PIC X(16) VALUE 'MID W4I34101 MID'.         
132900                                                                          
133000                                                                          
133100                                                                          
133200*01    MID -COPY W4I34101.                                                
133300     EJECT                                                                
133400*01    -COPY WMSGAREA                                                     
133500     EJECT                                                                
133600*  03    MOD -COPY W4O34101  -RED MSG-AREA.                               
133700     EJECT                                                                
133800 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
133900                                                                          
134000                                                                          
134100                                                                          
134200*01    -COPY WMFSAREA                                                     
134300     EJECT                                                                
134400 01    IMS-WS.                                                            
134500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
134600                                                                          
134700                                                                          
134800                                                                          
134900*                        **** STATUS-KOD FRÅN IMS                         
135000   03    STATUS-WDE401-SEK-WS    PIC XX.                                  
135100     88    WDE401-SEK-FINNS                  VALUE '  '.                  
135200     88    WDE401-SEK-SAKNAS                 VALUE 'GE' 'GB'.             
135300   03    STATUS-WS               PIC XX.                                  
135400     88    SEGMENT-FINNS                     VALUE '  '.                  
135500     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
135600   03    STATUS-WS-ARTS          PIC XX.                                  
135700     88    SEGMENT-FINNS-ARTS                VALUE '  '.                  
135800     88    SEGMENT-SAKNAS-ARTS               VALUE 'GE'.                  
135900   03    STATUS-WS-KOLLI         PIC XX.                                  
136000     88    SEGMENT-FINNS-KOLLI               VALUE '  '.                  
136100     88    SEGMENT-SAKNAS-KOLLI              VALUE 'GE'.                  
136200   03    STATUS-WS-RAD           PIC XX.                                  
136300     88    SEGMENT-FINNS-RAD                 VALUE '  '.                  
136400     88    SEGMENT-SAKNAS-RAD                VALUE 'GE'.                  
136500                                                                          
136600                                                                          
136700                                                                          
136800   03    GODK-STATUSKODER.                                                
136900     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
137000                                                                          
137100                                                                          
137200                                                                          
137300 01    SSA1                      PIC X(160).                              
137400 01    SSA2                      PIC X(64).                               
137500     EJECT                                                                
137600*                            DB2 FUNKTIONSKODER                           
137700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
137800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
137900                                                                          
138000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
138100 01  DB2-WS.                                                              
138200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
138300         88  CURSOR-OK                       VALUE 000.                   
138400         88  RADER-FINNS                     VALUE 000.                   
138500         88  RADER-SAKNAS                    VALUE 100.                   
138600         88  ATKOMST-FEL                     VALUE 904.                   
138700     03  GODK-SQLCODEKODER.                                               
138800         05  GODK-SQLCODE OCCURS 5                                        
138900             INDEXED BY SQLCODE-IX PIC 9(3).                              
139000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
139100     EJECT                                                                
139200     SKIP2                                                                
139300*                            IMS FUNKTIONSKODER                           
139400*01    -COPY W0003                                                        
139500     SKIP2                                                                
139600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
139700 01    DLI-IO-AREA.                                                       
139800   03    IO-AREA                 PIC X(652)  VALUE SPACE.                 
139900                                                                          
140000*  03    WDE601   -COPY WDE601             -RED IO-AREA.                  
140100     SKIP2                                                                
140200*  03    WDE611   -COPY WDE611             -RED IO-AREA.                  
140300     SKIP2                                                                
140400*  03    WLORQI01 -COPY WDQ201             -RED IO-AREA.                  
140500     SKIP2                                                                
140600*  03    WLBENA01 -COPY WDD311             -RED IO-AREA.                  
140700     SKIP2                                                                
140800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA2'.          
140900 01    DLI-IO-AREA2.                                                      
141000   03    IO-AREA2                PIC X(272)  VALUE SPACE.                 
141100                                                                          
141200*  03 WDE401   -COPY  WDE401            -RED IO-AREA2.                    
141300     SKIP2                                                                
141400 01  FILLER                      PIC X(16)  VALUE 'E4-WDE411-ARE'.        
141500 01  WDE4F-AREA.                                                          
141600   03    WDE411   -COPY WDE411                                            
141700   03    WDE421   -COPY WDE421                                            
141800     SKIP2                                                                
141900 01  FILLER                      PIC X(16)  VALUE 'WDB1-AREA'.            
142000*01  -COPY WDB101                                                         
142100     SKIP2                                                                
142200 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
142300*01  -COPY WDB201                                                         
142400     SKIP2                                                                
142500 01  FILLER                      PIC X(16)  VALUE 'WDB5-AREA'.            
142600*01  -COPY WDB501                                                         
142700     SKIP2                                                                
142800 01  FILLER                      PIC X(16)  VALUE 'WDQ3-AREA'.            
142900*01  -COPY WDQ301                                                         
143000     SKIP2                                                                
143100 01  FILLER                      PIC X(16)  VALUE 'WDK611-AREA'.          
143200*01  -COPY WDK611                                                         
143300     SKIP3                                                                
143400 01  FILLER                      PIC X(16)  VALUE 'W6D211-AREA'.          
143500*01  -COPY W6D211                                                         
143600     SKIP3                                                                
143700 01  FILLER                      PIC X(16)  VALUE 'WDD801-AREA'.          
143800*01  -COPY WDD801                                                         
143900     SKIP3                                                                
144000 01  FILLER                      PIC X(16)  VALUE 'WDD811-AREA'.          
144100*01  -COPY WDD811                                                         
144200     SKIP3                                                                
144300 01  FILLER                      PIC X(16)  VALUE 'WDL601-AREA'.          
144400*01  -COPY WDL601 -PRE INL-                                               
144500     SKIP3                                                                
144600 01  FILLER                      PIC X(16)  VALUE 'WDL611-AREA'.          
144700*01  -COPY WDL611                                                         
144800     EJECT                                                                
144900                                                                          
145000 01  FILLER                      PIC X(16)   VALUE 'WDQ5A1-AREA'.         
145100 01  DLI-IO-WDQ5A1.                                                       
145200*    03  -COPY WDQ5A1                                                     
145300     EJECT                                                                
145400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
145500 01   DLI-IO-AREA-B601.                                                   
145600*     03  -COPY WDB601                                                    
145700                                                                          
145800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF502'.             
145900 01  DLI-IO-WDF502.                                                       
146000*  03  -COPY WDF502                                                       
146100     EJECT                                                                
146200                                                                          
146300*---MSG-AERA FÖR HOPP TILL 4349-UTSKRIFT DELIVERY NOTE NA                 
146400 01  FILLER                PIC X(16)  VALUE '4349-MSG-IO-AREA'.           
146500 01  4349-MSG-IO-AREA.                                                    
146600     03  4349-LL              PIC S9(4)  VALUE +0   COMP SYNC.            
146700     03  4349-Z1              PIC X.                                      
146800     03  4349-Z2              PIC X.                                      
146900     03  4349-TRANSKOD        PIC X(8)   VALUE 'W4T349X '.                
147000     03  4349-IDTRANS         PIC X(4)   VALUE '4341'.                    
147100     03  4349-SPRAK           PIC X.                                      
147200*    03 -COPY W4I34901  -PRE 4349-                                        
147300*                                                                         
147400                                                                          
147500     EJECT                                                                
147600 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
147700                                                                          
147800*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
147900     EJECT                                                                
148000     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
148100     EJECT                                                                
148200                                                                          
148300 LINKAGE SECTION.                                                         
148400*01    -COPY W0009     -PRE MSG-                                          
148500     SKIP2                                                                
148600*01    -COPY W0009     -PRE ALT-                                          
148700     SKIP2                                                                
148800*                                                                         
148900*01    -COPY W0009     -PRE ALT49-                                        
149000     SKIP2                                                                
149100*                                                                         
149200*01    -COPY W0008     -PRE USEA-                                         
149300     05  FILLER                  PIC X.                                   
149400     SKIP2                                                                
149500*01    -COPY W0008     -PRE WDE4-                                         
149600     05  FILLER                  PIC X.                                   
149700     SKIP2                                                                
149800*01    -COPY W0008     -PRE WDE4A-                                        
149900     05  FILLER                  PIC X.                                   
150000     SKIP2                                                                
150100*01    -COPY W0008     -PRE WDE4F-                                        
150200     05  FILLER                  PIC X.                                   
150300     SKIP2                                                                
150400*01    -COPY W0008     -PRE GMTA-                                         
150500     05  FILLER                  PIC X.                                   
150600     SKIP2                                                                
150700*01    -COPY W0008     -PRE ORQI-                                         
150800     05  FILLER                  PIC X.                                   
150900     SKIP2                                                                
151000*01    -COPY W0008     -PRE BENA-                                         
151100     05  FILLER                  PIC X.                                   
151200     SKIP2                                                                
151300*01    -COPY W0008     -PRE ARTS-                                         
151400     05  FILLER                  PIC X.                                   
151500     SKIP2                                                                
151600*01    -COPY W0008     -PRE WDE6-                                         
151700     05  FILLER                  PIC X.                                   
151800     SKIP2                                                                
151900*01    -COPY W0008     -PRE BETC-                                         
152000     05  FILLER                  PIC X.                                   
152100     SKIP2                                                                
152200*01    -COPY W0008     -PRE GMTC-                                         
152300     05  FILLER                  PIC X.                                   
152400     SKIP2                                                                
152500*01    -COPY W0008     -PRE ORQA-                                         
152600     05  FILLER                  PIC X.                                   
152700     SKIP2                                                                
152800*01    -COPY W0008     -PRE ARTC-                                         
152900     05  FILLER                  PIC X.                                   
153000     SKIP2                                                                
153100*01    -COPY W0008     -PRE KVAH-                                         
153200     05  FILLER                  PIC X.                                   
153300     SKIP2                                                                
153400*01    -COPY W0008     -PRE ARTD-                                         
153500     05  FILLER                  PIC X.                                   
153600     SKIP2                                                                
153700*01    -COPY W0008     -PRE INLC-                                         
153800     05  FILLER                  PIC X.                                   
153900     SKIP2                                                                
154000*                                                                         
154100*01    -COPY W0008     -PRE WDQ5A-                                        
154200     05  FILLER                  PIC X.                                   
154300     EJECT                                                                
154400*01    -COPY W0008     -PRE WDB6-                                         
154500     05  FILLER                  PIC X.                                   
154600     EJECT                                                                
154700*01    -COPY W0008     -PRE WDF5-                                         
154800     05  FILLER                  PIC X.                                   
154900     EJECT                                                                
155000                                                                          
155100 PROCEDURE DIVISION USING  MSG-PCB   ALT-PCB   ALT49-PCB                  
155200                           USEA-PCB  WDE4A-PCB WDE4F-PCB                  
155300                           GMTA-PCB  ORQI-PCB  BENA-PCB                   
155400                           WDE6-PCB  BETC-PCB  WDQ5A-PCB                  
155500                           WDB6-PCB  WDF5-PCB.                            
155600                                                                          
155700 MAIN SECTION.                                                            
155800     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB   ALT49-PCB                  
155900                           USEA-PCB  WDE4A-PCB WDE4F-PCB                  
156000                           GMTA-PCB  ORQI-PCB  BENA-PCB                   
156100                           WDE6-PCB  BETC-PCB  WDQ5A-PCB                  
156200                           WDB6-PCB  WDF5-PCB.                            
156300                                                                          
156400     PERFORM IMS-GET-MSG                                                  
156500     IF SEGMENT-FINNS                                                     
156600         MOVE NEJ            TO WS-FEL-FUNNET                             
156700         PERFORM A-INIT                                                   
156800                                                                          
156900         IF GODKAEND-BILD                                                 
157000             PERFORM B-FORMELL-KONTROLL                                   
157100                                                                          
157200             IF FEL-EJ-FUNNET                                             
157300                                                                          
157400                IF PRT-KDSVAR = RAETT                                     
157500                                                                          
157600******************************************************************        
157700*                                                                         
157800*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
157900*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
158000*                                                                         
158100******************************************************************        
158200                                                                          
158300                  MOVE WS-IDDISTR       TO W-TP4TRAN-IDDISTR              
158400                                                                          
158500                  PERFORM DB2-SELECT-TP4TRAN                              
158600                                                                          
158700                  MOVE WS-IDDISTR       TO DIST35-IDDISTR                 
158800                                           DIST19-IDDISTR                 
158900                  EVALUATE TRUE                                           
159000                     WHEN DIST35-REFILL                                   
159100                      OR DIST35-NONVCC-CDC-REFILL                         
159200                      OR DIST35-REFILL-NA-JAP                             
159300                      OR DIST35-NA-TRANSFER                               
159400                      OR DIST35-PACIFIC-TRANSFER                          
159500                      OR DIST35-REFILL-INOM-JP                            
159600                      OR DIST35-NA-NDC-RETURNS                            
159700                      OR RADER-FINNS                                      
159800                        PERFORM C-LAES-BASER-SKRIV-LISTA                  
159900                     WHEN DCS-NDC-PF                                      
160000                        PERFORM C-LAES-BASER-SKRIV-LISTA                  
160100                     WHEN DCS-NDC-NA                                      
160200                        PERFORM F-DELIVERY-NOTE-NA                        
160300                     WHEN DIST19-SATS                                     
160400                       MOVE FEL-761(INDX) TO MOD-TEMFSFEL                 
160500                       MOVE JA          TO WS-FEL-FUNNET                  
160600                     WHEN OTHER                                           
160700                        PERFORM C-LAES-BASER-SKRIV-LISTA                  
160800                  END-EVALUATE                                            
160900                END-IF                                                    
161000                IF FEL-EJ-FUNNET                                          
161100                   MOVE MED-001(INDX)   TO MOD-TEMFSINF                   
161200                   MOVE NEJ             TO WS-FEL-FUNNET                  
161300                END-IF                                                    
161400             END-IF                                                       
161500         END-IF                                                           
161600                                                                          
161700         IF FEL-FUNNET                                                    
161800             IF NOT EGEN-BILD                                             
161900                 PERFORM D-ADRESS-TILL-RAETT-MOD                          
162000             END-IF                                                       
162100                                                                          
162200         END-IF                                                           
162300                                                                          
162400         IF MFS-IDTRANS = '433A' OR '433H' OR '431D' OR '431E' OR         
162500                          '6302' OR '433Z' OR 'L197' OR 'L199' OR         
162510                          '4327'                                          
162600           CONTINUE                                                       
162700         ELSE                                                             
162800           PERFORM S10-BEHANDLA-KOLLI-FAELT                               
162900                                                                          
163000           MOVE MAX-MOD-LENGTH TO MSG-KVLL                                
163100           PERFORM IMS-INSERT-MSG                                         
163200         END-IF                                                           
163300                                                                          
163400     END-IF                                                               
163500                                                                          
163600     MOVE ZERO TO RETURN-CODE                                             
163700     GOBACK                                                               
163800     .                                                                    
163900     EJECT                                                                
164000 A-INIT             SECTION.                                              
164100                                                                          
164200     IF MSG-DUBBLA-TRANSKODER                                             
164300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I34101               
164400       MOVE MSG-IDTRANS-2                 TO   MFS-IDTRANS                
164500                                               WS-IDTRANS                 
164600       MOVE MSG-KDMFSFOR-2                TO   MFS-KDMFSFOR               
164700                                               WS-KDMFSFOR                
164800       MOVE MSG-KDTRTYP                   TO   MFS-KDTRTYP                
164900       MOVE MSG-IDPFK                     TO   MFS-IDPFK                  
165000     ELSE                                                                 
165100       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I34101               
165200       MOVE MSG-IDTRANS-1                 TO   MFS-IDTRANS                
165300                                               WS-IDTRANS                 
165400       MOVE MSG-KDMFSFOR-1                TO   MFS-KDMFSFOR               
165500                                               WS-KDMFSFOR                
165600       MOVE ' '                           TO   MFS-KDTRTYP                
165700     END-IF                                                               
165800                                                                          
165900     ACCEPT WS-TODAYS-DATE  FROM DATE                                     
166000     MOVE   WS-TODAYS-DATE  (3:2) TO PERIOD-MANAD-IX                      
166100     MOVE JA                      TO SW-NYCKLAR-OK                        
166200                                                                          
166300     SET KOLLI-IX                 TO 1                                    
166400     PERFORM UNTIL KOLLI-IX > KOLLI-MAX-IX                                
166500       MOVE ZERO                  TO TAB-IDKOLLI (KOLLI-IX)               
166600       SET KOLLI-IX UP BY 1                                               
166700     END-PERFORM                                                          
166800                                                                          
166900     MOVE 1                   TO IX-TAB                                   
167000     PERFORM UNTIL IX-TAB   > TAB-MAX-NDC                                 
167100       MOVE ZERO              TO NDC-TAB-PURAD   (IX-TAB)                 
167200       MOVE ZERO              TO NDC-TAB-KVLEVART (IX-TAB)                
167300       ADD 1 TO IX-TAB                                                    
167400     END-PERFORM                                                          
167500     MOVE 1                   TO IX-TAB                                   
167600     PERFORM UNTIL IX-TAB   > TAB-MAX                                     
167700       MOVE ZERO              TO REFILL-TAB-ADLAGOMR (IX-TAB)             
167800                                 REFILL-TAB-ADGANG   (IX-TAB)             
167900                                 REFILL-TAB-ADPLATS  (IX-TAB)             
168000                                 REFILL-TAB-IDARTNR  (IX-TAB)             
168100                                 REFILL-TAB-REKSIFFR (IX-TAB)             
168200                                 REFILL-TAB-KVLEVART (IX-TAB)             
168300                                 REFILL-TAB-INDEX    (IX-TAB)             
168400       MOVE ZERO              TO TAB-PURAD   (IX-TAB)                     
168500       MOVE ZERO              TO TAB-KVLEVART (IX-TAB)                    
168600       MOVE ZERO              TO REFILL-TAB-PURAD   (IX-TAB)              
168700       MOVE ZERO              TO REFILL-TAB-KVLEVART (IX-TAB)             
168800       MOVE SPACE             TO REFILL-TAB-PRIO     (IX-TAB)             
168900                                 REFILL-TAB-NEW      (IX-TAB)             
169000                                 REFILL-TAB-KDARTURS (IX-TAB)             
169100                                 REFILL-TAB-BACKORDERED(IX-TAB)           
169200       ADD 1 TO IX-TAB                                                    
169300     END-PERFORM                                                          
169400                                                                          
169500     MOVE ALL '+'                 TO MSGI-WMSGINIT                        
169600     MOVE '001'                   TO MSGI-KDCALL                          
169700     MOVE MSG-SIGNON-USERID       TO MSGI-IDUSER                          
169800     MOVE MSG-LTERM-NAME          TO MSGI-IDLTERM-USER                    
169900     MOVE '4341'                  TO MSGI-IDTRANS                         
170000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
170100                                                                          
170200     IF MID-IDDISTR-IN = ALL '+'                                          
170300         MOVE MID-IDDISTR-UT              TO   WS-IDDISTR                 
170400         INSPECT WS-IDDISTR  REPLACING LEADING SPACE BY ZERO              
170500     ELSE                                                                 
170600         MOVE MID-IDDISTR-IN              TO   WS-IDDISTR                 
170700     END-IF                                                               
170800                                                                          
170900     IF MID-IDKUNDNR-IN = ALL '+'                                         
171000         MOVE MID-IDKUNDNR-UT             TO   WS-IDKUNDNR                
171100         INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
171200     ELSE                                                                 
171300         MOVE MID-IDKUNDNR-IN             TO   WS-IDKUNDNR                
171400     END-IF                                                               
171500                                                                          
171600     IF MID-IDORDNR-IN = ALL '+'                                          
171700         MOVE MID-IDORDNR-UT              TO   WS-IDORDNR                 
171800         INSPECT WS-IDORDNR  REPLACING LEADING SPACE BY ZERO              
171900     ELSE                                                                 
172000         MOVE MID-IDORDNR-IN              TO   WS-IDORDNR                 
172100     END-IF                                                               
172200                                                                          
172300     IF MID-IDPLKLST-IN = '+'                                             
172400         MOVE MID-IDPLKLST-UT             TO WS-IDPLKLST                  
172500     ELSE                                                                 
172600         MOVE MID-IDPLKLST-IN             TO WS-IDPLKLST                  
172700     END-IF                                                               
172800                                                                          
172900     IF ENGLISH-TEXT                                                      
173000       MOVE MSGI-IDDC                     TO WS-IDDC                      
173100     ELSE                                                                 
173200       IF MID-IDDC-IN = ALL '+'                                           
173300         IF MID-IDDC-UT = SPACE                                           
173400           MOVE NEJ                       TO SW-NYCKLAR-OK                
173500         ELSE                                                             
173600           MOVE MID-IDDC-UT               TO WS-IDDC                      
173700         END-IF                                                           
173800       ELSE                                                               
173900         MOVE MID-IDDC-IN                 TO WS-IDDC                      
174000       END-IF                                                             
174100     END-IF                                                               
174200                                                                          
174300     IF WS-IDDC IS > SPACE                                                
174400       MOVE WS-IDDC                       TO W-IDDC                       
174500                                             W-ODEL-IDDC                  
174600     ELSE                                                                 
174700       MOVE NEJ                           TO SW-NYCKLAR-OK                
174800     END-IF                                                               
174900                                                                          
175000     IF MID-IDKOLLI-IN = ALL '+'                                          
175100         MOVE MID-IDKOLLI-UT              TO   WS-IDKOLLI                 
175200         INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO               
175300     ELSE                                                                 
175400         MOVE MID-IDKOLLI-IN              TO   WS-IDKOLLI                 
175500     END-IF                                                               
175600                                                                          
175700     IF MID-IDKOLLI-TOM-IN = ALL '+'                                      
175800         MOVE ZERO                        TO   WS-IDKOLLI-TOM             
175900     ELSE                                                                 
176000         MOVE MID-IDKOLLI-TOM-IN          TO   WS-IDKOLLI-TOM             
176100     END-IF                                                               
176200                                                                          
176300     IF MID-KDPRTVAL-IN = ALL '+'                                         
176400         MOVE MID-KDPRTVAL-UT           TO   WS-KDPRTVAL                  
176500     ELSE                                                                 
176600         MOVE MID-KDPRTVAL-IN             TO   WS-KDPRTVAL                
176700     END-IF                                                               
176800                                                                          
176900     IF MID-FL-SVENSK-FSEDEL NOT = ALL '+'                                
177000       MOVE MID-FL-SVENSK-FSEDEL          TO   WS-FL-SVENSK-FSEDEL        
177100     END-IF                                                               
177200                                                                          
177300     MOVE LOW-VALUE                       TO   MSG-AREA                   
177400     MOVE ZERO                            TO   SPAR-IDRADNR-KO            
177500                                               SPAR-KVLEVART              
177600     MOVE 'W4O341N1'                      TO   MFS-IDMOD                  
177700     MOVE '4341'                          TO   MOD-IDTRANS                
177800     COMPUTE MSG-KVLL = LENGTH OF MAX-MOD-LENGTH  +  4                    
177900                                                                          
178000     MOVE WS-IDDISTR                      TO   MOD-IDDISTR-UT             
178100     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
178200                                                                          
178300     MOVE WS-IDKUNDNR                     TO   MOD-IDKUNDNR-UT            
178400     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
178500                                                                          
178600     MOVE WS-IDORDNR                      TO   MOD-IDORDNR-UT             
178700     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
178800                                                                          
178900                                                                          
179000     MOVE WS-IDKOLLI                      TO   MOD-IDKOLLI-UT             
179100     INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE               
179200                                                                          
179300     MOVE WS-IDKOLLI-TOM                  TO   MOD-IDKOLLI-TOM-UT         
179400     INSPECT MOD-IDKOLLI-TOM-UT                                           
179500                            REPLACING LEADING ZERO BY SPACE               
179600                                                                          
179700     MOVE WS-IDDC                         TO   MOD-IDDC-UT                
179800     MOVE WS-KDPRTVAL                     TO   MOD-KDPRTVAL-UT            
179900                                                                          
180000     IF MSGI-IDLAND-SPR = 'GB'                                            
180100         MOVE +2                          TO   INDX                       
180200     ELSE                                                                 
180300         MOVE +1                          TO   INDX                       
180400     END-IF                                                               
180500                                                                          
180600     IF NOT GODKAEND-BILD                                                 
180700                                                                          
180800       MOVE MFS-RENSA-FAELT               TO MOD-IDDISTR-UT               
180900                                             MOD-IDKUNDNR-UT              
181000                                             MOD-IDORDNR-UT               
181100                                             MOD-IDKOLLI-UT               
181200                                             MOD-IDKOLLI-TOM-UT           
181300                                             MOD-KDPRTVAL-UT              
181400                                             MOD-FL-SVENSK-FSEDEL         
181500                                             MOD-TEMFSFEL                 
181600                                             MOD-TEMFSINF                 
181700     END-IF                                                               
181800                                                                          
181900     MOVE MFS-RENSA-FAELT                 TO MOD-IDDISTR-IN               
182000                                             MOD-IDKUNDNR-IN              
182100                                             MOD-IDORDNR-IN               
182200                                             MOD-IDKOLLI-IN               
182300                                             MOD-IDKOLLI-TOM-IN           
182400                                             MOD-KDPRTVAL-IN              
182500                                             MOD-FL-SVENSK-FSEDEL         
182600                                             MOD-TEMFSFEL                 
182700                                             MOD-TEMFSINF                 
182800      MOVE ZERO                    TO FILLER-PLKLST                       
182900                                                                          
183000*    DELIVERY NOTES FOR PIC-BY-VOICE PACKING HAVE A SPECIAL FORMAT        
183100     IF WS-IDTRANS = '433Z'                                               
183200       MOVE JA  TO PBV-FORMAT                                             
183300     ELSE                                                                 
183400       MOVE NEJ TO PBV-FORMAT                                             
183500     END-IF                                                               
183600     .                                                                    
183700     EJECT                                                                
183800 B-FORMELL-KONTROLL  SECTION.                                             
183900                                                                          
184000     IF NYCKLAR-OK                                                        
184100        MOVE WS-IDDC TO W-IDDC-B6                                         
184200        PERFORM IMS-GU-WDB601                                             
184300        IF SEGMENT-SAKNAS                                                 
184400           IF FEL-EJ-FUNNET                                               
184500              MOVE FEL-7491(INDX)   TO MOD-TEMFSFEL                       
184600              MOVE JA               TO WS-FEL-FUNNET                      
184700           END-IF                                                         
184800        ELSE                                                              
184900           MOVE DCS-IDFTG           TO WS-DCS-IDFTG                       
185000                                                                          
185100           IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                            
185200              IF WS-IDKOLLI NOT NUMERIC                                   
185300              OR WS-IDKOLLI-TOM NOT NUMERIC                               
185400              OR NYCKLAR-FEL                                              
185500                 IF FEL-EJ-FUNNET                                         
185600                    MOVE FEL-7491(INDX)    TO MOD-TEMFSFEL                
185700                    MOVE JA               TO WS-FEL-FUNNET                
185800                 END-IF                                                   
185900              END-IF                                                      
186000           END-IF                                                         
186100        END-IF                                                            
186200     ELSE                                                                 
186300        IF FEL-EJ-FUNNET                                                  
186400           MOVE FEL-7491(INDX)       TO MOD-TEMFSFEL                      
186500           MOVE JA                  TO WS-FEL-FUNNET                      
186600        END-IF                                                            
186700     END-IF                                                               
186800                                                                          
186900     IF WS-IDDISTR  NOT NUMERIC                                           
187000     OR WS-IDKUNDNR NOT NUMERIC                                           
187100     OR WS-IDORDNR  NOT NUMERIC                                           
187200     OR NYCKLAR-FEL                                                       
187300                                                                          
187400        IF FEL-EJ-FUNNET                                                  
187500            MOVE FEL-7492(INDX)          TO MOD-TEMFSFEL                  
187600            MOVE JA                     TO WS-FEL-FUNNET                  
187700        END-IF                                                            
187800     ELSE                                                                 
187900       IF DCS-NDC-NA                                                      
188000         IF WS-IDDISTR  = ZERO                                            
188100         OR WS-IDORDNR  = ZERO                                            
188200             IF FEL-EJ-FUNNET                                             
188300                 MOVE FEL-7493(INDX)     TO MOD-TEMFSFEL                  
188400                 MOVE JA                TO WS-FEL-FUNNET                  
188500             END-IF                                                       
188600         END-IF                                                           
188700       ELSE                                                               
188800         IF WS-IDDISTR  = ZERO                                            
188900         OR WS-IDORDNR  = ZERO                                            
189000         OR WS-IDKOLLI  = ZERO                                            
189100             IF FEL-EJ-FUNNET                                             
189200                 MOVE FEL-7493(INDX)     TO MOD-TEMFSFEL                  
189300                 MOVE JA                TO WS-FEL-FUNNET                  
189400             END-IF                                                       
189500         END-IF                                                           
189600         IF  WS-IDKOLLI-TOM NOT = ZERO                                    
189700         AND WS-IDKOLLI-TOM < WS-IDKOLLI                                  
189800             IF FEL-EJ-FUNNET                                             
189900                 MOVE FEL-7494(INDX)     TO MOD-TEMFSFEL                  
190000                 MOVE JA                TO WS-FEL-FUNNET                  
190100             END-IF                                                       
190200         END-IF                                                           
190300       END-IF                                                             
190400       MOVE WS-IDDISTR           TO DIST35-IDDISTR                        
190500                                                                          
190600     MOVE WS-IDDISTR TO    DIST07-IDDISTR                                 
190700     IF (DCS-NDC-NA                 AND                                   
190800         DIST07-USA-RETAILER-DNOTE) OR                                    
190900        (DCS-NDC-NA                 AND                                   
191000         DIST07-CAN-RETAILER)                                             
191100                                                                          
191200        MOVE 'R'            TO PRT-KDSVAR                                 
191300                                                                          
191400     ELSE                                                                 
191500       IF WS-KDPRTVAL = 'U '                                              
191600          CONTINUE                                                        
191700       ELSE                                                               
191800          MOVE '4'              TO WS-SYSTDEL                             
191900          MOVE 'FS'             TO WS-LISTTYP                             
192000          MOVE WS-IDDC          TO WS-DC                                  
192100          MOVE WS-KDPRTVAL      TO WS-KDPRT                               
192200                                                                          
192300          MOVE 001              TO PRT-KDCALL                             
192400          MOVE WS-IDPRTLST      TO PRT-IDPRTLST                           
192500                                                                          
192600          CALL W006PRT USING PRT-W006PRT                                  
192700                                                                          
192800          IF PRT-KDSVAR = FEL                                             
192900             IF FEL-EJ-FUNNET                                             
193000                 MOVE FEL-772(INDX)     TO MOD-TEMFSFEL                   
193100                 MOVE JA                TO WS-FEL-FUNNET                  
193200             END-IF                                                       
193300          END-IF                                                          
193400        END-IF                                                            
193500     END-IF                                                               
193600                                                                          
193700     IF WS-FL-SVENSK-FSEDEL NOT = JA                                      
193800       MOVE 'N'                         TO   WS-FL-SVENSK-FSEDEL          
193900     END-IF                                                               
194000     END-IF                                                               
194100                                                                          
194200     IF WS-IDKOLLI NOT NUMERIC                                            
194300     OR WS-IDKOLLI-TOM NOT NUMERIC                                        
194400     OR NYCKLAR-FEL                                                       
194500         IF FEL-EJ-FUNNET                                                 
194600             MOVE FEL-7495(INDX)         TO MOD-TEMFSFEL                  
194700             MOVE JA                    TO WS-FEL-FUNNET                  
194800         END-IF                                                           
194900     END-IF                                                               
195000     .                                                                    
195100     EJECT                                                                
195200 C-LAES-BASER-SKRIV-LISTA SECTION.                                        
195300                                                                          
195400     MOVE ZERO                       TO ACK-ANTAL-SIDOR                   
195500                                        ACK-ANTAL-RADER                   
195600     MOVE NEJ                        TO WS-NGN-PLKLST-GODK                
195700     MOVE NEJ                        TO WS-NGN-PLKLST-FEL                 
195800                                                                          
195900     MOVE WS-IDDISTR                 TO W-IDDISTR                         
196000     MOVE WS-IDKUNDNR                TO W-IDKUNDNR                        
196100     MOVE WS-IDORDNR                 TO WS-IDORDNR-KREF                   
196200                                        RAD2-IDORDNR                      
196300                                                                          
196400     IF WS-IDDC NOT = W-IDDC-B6                                           
196500        MOVE WS-IDDC TO W-IDDC-B6                                         
196600        PERFORM IMS-GU-WDB601                                             
196700     END-IF                                                               
196800     EVALUATE TRUE                                                        
196900       WHEN DCS-SDC AND DCS-AUSTRIA                                       
197000         MOVE 'D E L I V E R Y   N O T E            '                     
197100           TO RUB1-LIST-NAMN                                              
197200         MOVE 0                      TO RAD2-IDBORD                       
197300         MOVE 'ADDRESS'              TO RAD3-ADDRESS                      
197400         MOVE 'HÄNDLER REF'          TO RAD3-IDKUNDRF                     
197500         MOVE 'RFS          '        TO RAD3-RFS                          
197600         MOVE 'KOLLI NR'             TO RAD6-CASENO                       
197700         MOVE 'FRACHT-CODE '         TO RAD6-FREIGHT-CODE                 
197800         MOVE 'DATUM       '         TO RAD6-PACKDATE                     
197900         MOVE 'O-REF   '             TO RAD8-O-REF                        
198000         MOVE 'TEILE NR '            TO RAD8-PARTNO                       
198100         MOVE 'ZEILENREF'            TO RAD8-BERADREF                     
198200         MOVE 'ORG'                  TO RAD8-ORG                          
198300         MOVE 'BEZEICHNUNG'          TO RAD8-DESC                         
198400         MOVE 'GELIEFERT '           TO RAD8-DELIVERED                    
198500         MOVE 'ANZHAL ZEILEN  :'     TO SLUT-RAD-NUM                      
198600       WHEN OTHER                                                         
198700         MOVE 'D E L I V E R Y   N O T E            '                     
198800           TO RUB1-LIST-NAMN                                              
198900         MOVE 0                      TO RAD2-IDBORD                       
199000         MOVE 'ADDRESS'              TO RAD3-ADDRESS                      
199100         MOVE 'CUSTOMER REF'         TO RAD3-IDKUNDRF                     
199200         MOVE 'RFS          '        TO RAD3-RFS                          
199300         MOVE 'CASE NO '             TO RAD6-CASENO                       
199400         MOVE 'FREIGHT CODE'         TO RAD6-FREIGHT-CODE                 
199500         MOVE 'PACK DATE   '         TO RAD6-PACKDATE                     
199600         MOVE 'O-REF   '             TO RAD8-O-REF                        
199700         MOVE 'PART NO  '            TO RAD8-PARTNO                       
199800         MOVE 'LINE REF  '           TO RAD8-BERADREF                     
199900         MOVE 'ORG'                  TO RAD8-ORG                          
200000         MOVE 'DESCRIPTION'          TO RAD8-DESC                         
200100         MOVE 'DELIVERED '           TO RAD8-DELIVERED                    
200200         MOVE 'NUMBER OF LINES:'     TO SLUT-RAD-NUM                      
200300     END-EVALUATE                                                         
200400     MOVE WS-IDKOLLI                 TO W-IDKOLLI                         
200500     IF  WS-IDKOLLI-TOM = ZERO                                            
200600         MOVE WS-IDKOLLI             TO W-IDKOLLI-TOM                     
200700         MOVE SPACE                  TO RAD7-HYPEN                        
200800         MOVE ZERO                   TO RAD7-IDKOLLI-TOM                  
200900     ELSE                                                                 
201000         MOVE WS-IDKOLLI-TOM         TO W-IDKOLLI-TOM                     
201100                                        RAD7-IDKOLLI-TOM                  
201200         MOVE '-'                    TO RAD7-HYPEN                        
201300     END-IF                                                               
201400*                                                                         
201500     MOVE W-IDDISTR                  TO W-E4A1-IDDISTR                    
201600     MOVE W-IDKUNDNR                 TO W-E4A1-IDKUNDNR                   
201700     MOVE WS-IDORDNR-KREF            TO W-E4A1-IDORDNR                    
201710                                                                          
201800     PERFORM IMS-GU-E401-KVAL-SEK                                         
201900*                                                                         
202000     IF WDE401-SEK-FINNS                                                  
202200       PERFORM UNTIL (WDE401-SEK-SAKNAS) OR WS-RATT-PRODNR = JA           
202300         IF  KORD-IDDC     = WS-IDDC                                      
202400         AND KORD-KVORDRAD-LEVPL   = ZERO                                 
202500           MOVE KORD-IDDISTR             TO W-E401-IDDISTR                
202600                                              RAD2-IDDISTR                
202700           MOVE KORD-IDKUNDNR            TO W-E401-IDKUNDNR               
202800                                              RAD2-IDKUNDNR               
202900           MOVE KORD-IDKUNDRF            TO W-E401-IDKUNDRF               
203000           MOVE KORD-IDPRODNR            TO W-E601-IDPRODNR               
203100                                            W-E401-IDPRODNR               
203200                                            WS-IDPRODNR                   
203300           MOVE KORD-IDPLKLST            TO W-E401-IDPLKLST               
203400           MOVE KORD-KDORDKL             TO RAD2-KDORDKL                  
203500           MOVE KORD-IDORDER             TO W-IDORDER                     
203600           MOVE JA                       TO WS-RATT-PRODNR                
203700         ELSE                                                             
203800           PERFORM IMS-GN-E401-KVAL-SEK                                   
203900         END-IF                                                           
204000       END-PERFORM                                                        
204100*                                                                         
204200***  COMPUTE CHECK DIGITS                                                 
204300                                                                          
204400       IF WS-IDTRANS = '433Z'                                             
204500         MOVE KORD-IDPRODNR           TO W-IDPRODNR-SEED                  
204600         MOVE WS-IDKOLLI              TO W-IDKOLLI-SEED                   
204700         MOVE W-CHECK-DIGTS-SEED      TO WS-SEED                          
204800         COMPUTE W-RANDOM-NUM = FUNCTION RANDOM(WS-SEED)                  
204900         MOVE W-RANDOM-NUM-RED        TO RUB1-CHECK-DIGIT                 
205000         MOVE 'CHECK DIG '            TO RUB1-CHECK-TEXT                  
205100       ELSE                                                               
205200         MOVE SPACE                   TO RUB1-CHECK-DIGIT                 
205300                                         RUB1-CHECK-TEXT                  
205400       END-IF                                                             
205500                                                                          
205600       IF WDE401-SEK-FINNS                                                
205700         PERFORM IMS-GU-E601-KVAL                                         
205800         IF SEGMENT-FINNS                                                 
205900           PERFORM CC-HAEMTA-DATA-I-E601                                  
206000           MOVE W-IDKOLLI      TO W-E611-IDKOLLI                          
206100           MOVE W-IDKOLLI-TOM  TO W-E611-IDKOLLI-TOM                      
206200           PERFORM IMS-GNP-E611-KVAL                                      
206300                                                                          
206400           IF  SEGMENT-FINNS-KOLLI                                        
206500             MOVE JA           TO WS-NGN-PLKLST-GODK                      
206600             PERFORM CE-HAMTA-KLI-RAD-DATA                                
206700           ELSE                                                           
206800*            * KOLLI SAKNAS                                               
206900             IF  WS-NGN-PLKLST-FEL = NEJ                                  
207000               MOVE JA            TO WS-NGN-PLKLST-FEL                    
207100               MOVE FEL-758(INDX) TO WS-MOD-TEMFSFEL                      
207200             END-IF                                                       
207300           END-IF                                                         
207400*                                                                         
207500         ELSE                                                             
207600*          * ORDER SAKNAS (EG. FEL C-LAGER ELLER DIRLEV.)                 
207700           IF  WS-NGN-PLKLST-FEL = NEJ                                    
207800             MOVE JA            TO WS-NGN-PLKLST-FEL                      
207900             MOVE FEL-7011(INDX) TO WS-MOD-TEMFSFEL                       
208000           END-IF                                                         
208100         END-IF                                                           
208200       ELSE                                                               
208300         IF    WS-NGN-PLKLST-FEL = NEJ                                    
208400           MOVE JA              TO WS-NGN-PLKLST-FEL                      
208500           MOVE FEL-7012(INDX) TO WS-MOD-TEMFSFEL                         
208600         END-IF                                                           
208700       END-IF                                                             
208800                                                                          
208900       IF  WS-NGN-PLKLST-GODK = JA                                        
209000*        * NÅGON PLOCKLISTA HAR GODKÄNTS - SKRIV FÖLJESEDEL               
209100                                                                          
209200        MOVE 1 TO IX-TAB                                                  
209300        PERFORM UNTIL TAB-PURAD (IX-TAB) < 1                              
209400           MOVE IX-TAB TO TAB-ANT                                         
209500           ADD +1 TO IX-TAB                                               
209600        END-PERFORM                                                       
209700         SUBTRACT 1 FROM IX-TAB                                           
209800         MOVE IX-TAB                  TO TAB-ANT                          
209900         PERFORM CM-SORTERA-TABELL                                        
210000         PERFORM CB-INITIERA-LISTA                                        
210100         PERFORM IMS-GU-ORQI                                              
210200         IF WS-FL-SVENSK-FSEDEL = JA                                      
210300           MOVE 'S  '                   TO W-IDSKYLT                      
210400         ELSE                                                             
210500           MOVE OHUV-IDSKYLT            TO W-IDSKYLT                      
210600         END-IF                                                           
210700                                                                          
210800         PERFORM S11-DIST-KUND-LDC                                        
210900                                                                          
211000         MOVE OHUV-BEKUNDRF         TO RAD4-BEKUNDRF                      
211100         INSPECT RAD4-BEKUNDRF   REPLACING ALL 'Ü' BY 'U'                 
211200         INSPECT RAD4-BEKUNDRF   REPLACING ALL 'ü' BY 'U'                 
211300         INSPECT RAD4-BEKUNDRF   REPLACING ALL '¤' BY 'U'                 
211400         INSPECT RAD4-BEKUNDRF   REPLACING ALL '#' BY 'O'                 
211500         IF GMT-FLLDCKND = YES                                            
211600           MOVE OHUV-BEGMT-RAD1  TO RAD4-BEGMT-RAD1                       
211700           MOVE OHUV-BEGMT-RAD2  TO RAD5-BEGMT-RAD2                       
211800           MOVE OHUV-ADGMT-GATA  TO RAD6-ADGMT-GATA                       
211900           MOVE OHUV-ADGMT-PADR  TO RAD7-ADGMT-PADR                       
212000         ELSE                                                             
212100           MOVE OHUV-BEKUNDRF       TO RAD4-BEKUNDRF                      
212200           INSPECT RAD4-BEKUNDRF REPLACING ALL 'Ü' BY 'U'                 
212300           INSPECT RAD4-BEKUNDRF REPLACING ALL 'ü' BY 'U'                 
212400           INSPECT RAD4-BEKUNDRF REPLACING ALL '¤' BY 'U'                 
212500           INSPECT RAD4-BEKUNDRF REPLACING ALL '#' BY 'O'                 
212600           IF OHUV-BEGMT = SPACE AND                                      
212700              OHUV-ADGMT = SPACE                                          
212800                                                                          
212900             PERFORM CD-LAES-HAEMTA-B201-B101                             
213000           ELSE                                                           
213100             IF OHUV-BEGMT-RAD1 = SPACE                                   
213200               MOVE OHUV-ADGMT-GATA TO RAD5-BEGMT-RAD2                    
213300               MOVE OHUV-ADGMT-PADR TO RAD6-ADGMT-GATA                    
213400               MOVE SPACE         TO RAD7-ADGMT-PADR                      
213500                                       RAD8-ADGMT-LAND                    
213600             ELSE                                                         
213700               MOVE OHUV-BEGMT-RAD1 TO RAD4-BEGMT-RAD1                    
213800               MOVE OHUV-BEGMT-RAD2 TO RAD5-BEGMT-RAD2                    
213900               MOVE OHUV-ADGMT-GATA TO RAD6-ADGMT-GATA                    
214000               MOVE OHUV-ADGMT-PADR TO RAD7-ADGMT-PADR                    
214100               MOVE OHUV-ADGMT-LAND TO RAD8-ADGMT-LAND                    
214200             END-IF                                                       
214300           END-IF                                                         
214400         END-IF                                                           
214500                                                                          
214600         MOVE 1                       TO IX-TAB                           
214800         IF DCS-NDC-PF AND DCS-JAPAN                                      
214900           PERFORM UNTIL (IX-TAB > TAB-ANT)                               
215000             PERFORM CF-SKRIV-RUBRIKER                                    
215100                                                                          
215200             PERFORM UNTIL (IX-TAB > TAB-ANT OR                           
216000                   ACK-ANTAL-RADER > MAX-LETTER-RAD-PER-SIDA)             
216100               PERFORM CI-SKRIV-DETALJRAD                                 
216200             END-PERFORM                                                  
216300           END-PERFORM                                                    
216400         ELSE                                                             
216500           PERFORM UNTIL (IX-TAB > TAB-ANT)                               
216600             PERFORM CF-SKRIV-RUBRIKER                                    
216700                                                                          
216800             PERFORM UNTIL (IX-TAB > TAB-ANT OR                           
216900                            ACK-ANTAL-RADER > MAX-RADER-PER-SIDA)         
217000               PERFORM CI-SKRIV-DETALJRAD                                 
217100             END-PERFORM                                                  
217200           END-PERFORM                                                    
217300         END-IF                                                           
217400                                                                          
217500         MOVE WS-SUM-ART             TO SLUT-RAD-SUM-ART                  
217600                                                                          
217700         PERFORM CG-SKRIV-SLUTRAD                                         
217800         PERFORM CJ-AVSLUTA-LISTA                                         
217900                                                                          
218000       ELSE                                                               
218100*        * INGEN PLKLST GODKÄND (ORDER ELLER KOLLI SAKNAS)                
218200         MOVE WS-MOD-TEMFSFEL     TO MOD-TEMFSFEL                         
218300         MOVE JA                  TO WS-FEL-FUNNET                        
218400       END-IF                                                             
218500                                                                          
218600     ELSE                                                                 
218700*      * INGEN PLKLST MED ANGIVET DISTR, KUNDNR, ORDNR                    
218800       MOVE FEL-7011(INDX)         TO MOD-TEMFSFEL                        
218900       MOVE JA                     TO WS-FEL-FUNNET                       
219000     END-IF                                                               
219100     .                                                                    
219200     EJECT                                                                
219300 CB-INITIERA-LISTA SECTION.                                               
219310                                                                          
219400     MOVE 'W40341-001'          TO PRT-IDLIST                             
219500     MOVE SPACE                 TO WS-DUMMY                               
219600                                                                          
219700     MOVE '4'                   TO WS-SYSTDEL                             
219800     MOVE 'FS'                  TO WS-LISTTYP                             
219900     MOVE WS-IDDC               TO WS-DC                                  
220000     MOVE WS-KDPRTVAL           TO WS-KDPRT                               
220100                                                                          
220200     MOVE 001                   TO PRT-KDCALL                             
220300     MOVE WS-IDPRTLST           TO PRT-IDPRTLST                           
220400                                                                          
220500     CALL W006PRT USING PRT-W006PRT                                       
220600                                                                          
220700     IF PRT-BEPRTLST (1:5) = 'IBMLA'                                      
220800       IF WS-IDPRTLST = '4FS11F  '                                        
220900       OR WS-IDPRTLST = '4FS11JK '                                        
221000       OR WS-IDPRTLST = '4FS11L  '                                        
221100       OR WS-IDPRTLST = '4FS11ZZ '                                        
221201       OR WS-IDPRTLST = '4FS11PO '                                        
221301       OR WS-IDPRTLST = '4FS11KO '                                        
221401       OR WS-IDPRTLST = '4FS11LP '                                        
221501         MOVE 'W40342' TO PRT-PFDEF-A4S                                   
221601       ELSE                                                               
221701         MOVE 'W40341' TO PRT-PFDEF-A4S                                   
221801       END-IF                                                             
221901     END-IF                                                               
222001                                                                          
222101*TO GET LARGER FONT ON DEL NOTE                                           
222201*FONT 377A FOR PBV DELIVERY NOTE                                          
222301     IF WS-IDPRTLST = '4FS1110 '                                          
222401     OR WS-IDPRTLST = '4FS1113 '                                          
222501     OR WS-IDPRTLST = '4FS1120 '                                          
222601     OR WS-IDPRTLST = '4FS1121 '                                          
222701     OR WS-IDPRTLST = '4FS1122 '                                          
222801     OR WS-IDPRTLST = '4FS1123 '                                          
222901     OR WS-IDPRTLST = '4FS1125 '                                          
223001     OR WS-IDPRTLST = '4FS1126 '                                          
223101     OR WS-IDPRTLST = '4FS1130 '                                          
223201     OR WS-IDPRTLST = '4FS1131 '                                          
223301     OR WS-IDPRTLST = '4FS1132 '                                          
223401     OR WS-IDPRTLST = '4FS1133 '                                          
223501     OR WS-IDPRTLST = '4FS1135 '                                          
223601     OR WS-IDPRTLST = '4FS1144 '                                          
223701     OR WS-IDPRTLST = '4FS1150 '                                          
223801     OR WS-IDPRTLST = '4FS1155 '                                          
223901     OR WS-IDPRTLST = '4FS1166 '                                          
224001     OR WS-IDPRTLST = '4FS1177 '                                          
224101     OR WS-IDPRTLST = '4FS1188 '                                          
224201       MOVE '377A  ' TO PRT-PFDEF-A4S                                     
224301     END-IF                                                               
224401                                                                          
224501     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
224601                         PRT-OPEN                                         
224701                         PRT-IDPRTLST                                     
224801                         ALT-PCB                                          
224901                         WS-DUMMY                                         
225001                         PRT-IDLIST                                       
225101     .                                                                    
225201     EJECT                                                                
225301 CC-HAEMTA-DATA-I-E601 SECTION.                                           
225401                                                                          
225501     MOVE VORD-IDPRODNR                TO RAD2-IDPRODNR                   
225601     MOVE VORD-IDPRODNR                TO WS-IDPRODNR-E420F               
225701     MOVE VORD-KDFRAKT                 TO WS-KDFRAKT                      
225801     MOVE VORD-TIUTSKR                 TO WS-VORD-TIUTSKR                 
225901     MOVE VORD-TIUTSTID                TO WS-VORD-TIUTSTID                
226001     MOVE VORD-BEGMRK                  TO WS-BEGDSMRK                     
226101     .                                                                    
226201     EJECT                                                                
226301 CE-HAMTA-KLI-RAD-DATA SECTION.                                           
226401                                                                          
226501     MOVE KOLLI-IDKOLLI                TO RAD7-IDKOLLI-FOM                
226601                                                                          
226701     IF KOLLI-TIPACKN > ZERO                                              
226801       MOVE KOLLI-TIPACKN              TO RAD7-TIPACKN                    
226901       MOVE KOLLI-TIPACTID             TO WS-TIME-HHMMSS                  
227001       MOVE WS-TIME-HOUR               TO RAD7-TIPACTIDTIM                
227101       MOVE WS-TIME-MINUTE             TO RAD7-TIPACTIDMIN                
227201     ELSE                                                                 
227301                                                                          
227401       MOVE 'PRINT DATE  '             TO RAD6-PACKDATE                   
227501       MOVE WS-VORD-TIUTSKR            TO RAD7-TIPACKN                    
227601       MOVE WS-VORD-TIUTSTID           TO WS-TIME-HHMMSS                  
227701       MOVE WS-TIME-HOUR               TO RAD7-TIPACTIDTIM                
227801       MOVE WS-TIME-MINUTE             TO RAD7-TIPACTIDMIN                
227901     END-IF                                                               
228001                                                                          
228101     MOVE KOLLI-IDPLOCK                TO WS-IDPLOCK-GRP                  
228201     MOVE WS-IDPLOCK                   TO RAD2-IDPLOCK                    
228301     MOVE KOLLI-DARFS                  TO WS-DARFS                        
228401     MOVE WS-KDFRAKT                   TO RAD7-KDFRAKT                    
228501     MOVE WS-DARFS-DATE                TO RAD4-RFS-DATE                   
228601     MOVE WS-DARFS-TIM                 TO RAD4-RFS-TIM                    
228701     MOVE WS-DARFS-MIN                 TO RAD4-RFS-MIN                    
228801     MOVE ZERO                         TO WS-VLORDBTO                     
228901                                          WS-VKORDBTO                     
229001                                          SPAR-KVLEVART                   
229101                                          SPAR-IDRADNR                    
229201                                          IX-TAB                          
229301     PERFORM UNTIL SEGMENT-SAKNAS-KOLLI                                   
229401       MOVE KOLLI-IDKOLLI              TO W-E611-IDKOLLI                  
229501       ADD KOLLI-VLORDBTO-KOLLI        TO WS-VLORDBTO                     
229601       ADD KOLLI-VKORDBTO-KOLLI        TO WS-VKORDBTO                     
229701       MOVE WS-IDPRODNR-E420F        TO W-E4F1-IDPRODNR-MAX               
229801       MOVE KOLLI-IDKOLLI              TO W-E4F1-IDKOLLI-MAX              
229901       MOVE WS-IDPRODNR-E420F        TO W-E4F1-IDPRODNR-MIN               
230001       MOVE KOLLI-IDKOLLI              TO W-E4F1-IDKOLLI-MIN              
230101       MOVE WS-IDPRODNR-E420F          TO W-IDPRODNR-E4                   
230201       MOVE KOLLI-IDKOLLI              TO W-IDKOLLI-E4                    
230301       PERFORM IMS-GN-WDE411-21-FSEQ                                      
230401                                                                          
230501       IF SEGMENT-FINNS-RAD                                               
230601                                                                          
230701         IF DCS-CDC OR DCS-SDC AND DCS-SWEDEN                             
230801           MOVE ORAD-BERADREF          TO RAD8-ADGMT-LAND                 
230901           INSPECT RAD8-ADGMT-LAND REPLACING ALL 'Ü' BY 'U'               
231001           INSPECT RAD8-ADGMT-LAND REPLACING ALL 'ü' BY 'U'               
231101           INSPECT RAD8-ADGMT-LAND REPLACING ALL '¤' BY 'U'               
231201           INSPECT RAD8-ADGMT-LAND REPLACING ALL '#' BY 'O'               
231301         END-IF                                                           
231401                                                                          
231501         MOVE ZERO TO IX-TAB                                              
231601         PERFORM UNTIL SEGMENT-SAKNAS-RAD OR IX-TAB >= TAB-MAX            
231701           ADD +1 TO IX-TAB                                               
231801           PERFORM CEA-HAMTA-DATA-E411                                    
231901           PERFORM CEB-LAGRA                                              
232001           PERFORM IMS-GN-WDE411-21-FSEQ                                  
232101         END-PERFORM                                                      
232201       END-IF                                                             
232301       PERFORM IMS-GNP-E611-MIN-MAX                                       
232401     END-PERFORM                                                          
232501                                                                          
232601                                                                          
232701     IF WS-VKORDBTO = ZERO                                                
232801       MOVE ZERO                       TO SLUT-RAD-VKORDBTO               
232901       MOVE '       '                  TO SLUT-RAD-WEIGHT                 
233001       MOVE SPACE                      TO SLUT-RAD-KG                     
233101     ELSE                                                                 
233201           MOVE 'WEIGHT:'              TO SLUT-RAD-WEIGHT                 
233301       MOVE 'KG'                       TO SLUT-RAD-KG                     
233401     END-IF                                                               
233501     IF WS-VLORDBTO = ZERO                                                
233601       MOVE ZERO                       TO SLUT-RAD-VLORDBTO               
233701       MOVE '       '                  TO SLUT-RAD-VOLUME                 
233801       MOVE SPACE                      TO SLUT-RAD-M3                     
233901     ELSE                                                                 
234001       MOVE 'VOLUME:'                  TO SLUT-RAD-VOLUME                 
234101       MOVE 'M3'                       TO SLUT-RAD-M3                     
234201     END-IF                                                               
234301     .                                                                    
234401     EJECT                                                                
234501 CEA-HAMTA-DATA-E411 SECTION.                                             
234601                                                                          
234701     MOVE ORAD-IDPURAD          TO SPAR-IDRADNR-KO                        
234801     MOVE ORAD-IDKUNDRF-RO      TO WS-IDKUNDRF-RO                         
234901     MOVE WS-IDORDNR-RO         TO SPAR-IDRONR                            
235001     MOVE ORAD-IDARTNR          TO SPAR-IDARTNR                           
235101     MOVE ORAD-REKSIFFR         TO SPAR-REKSIFFR                          
235201     MOVE KKOLLI-KVLEVART       TO SPAR-KVLEVART                          
235301     MOVE ORAD-BEART            TO SPAR-BEART                             
235401     MOVE ORAD-BERADREF         TO SPAR-BERADREF                          
235501     MOVE ORAD-IDSYSTEM         TO SPAR-IDSYSTEM                          
235601     INSPECT SPAR-BERADREF   REPLACING ALL 'Ü' BY 'U'                     
235701     INSPECT SPAR-BERADREF   REPLACING ALL 'ü' BY 'U'                     
235801     INSPECT SPAR-BERADREF   REPLACING ALL '¤' BY 'U'                     
235901     INSPECT SPAR-BERADREF   REPLACING ALL '#' BY 'O'                     
236001     IF ORAD-FLTILLK > ZERO                                               
236101       MOVE '*'                 TO SPAR-IDARTNR-ERS                       
236201     ELSE                                                                 
236301       MOVE SPACE               TO SPAR-IDARTNR-ERS                       
236401     END-IF                                                               
236501     .                                                                    
236601     EJECT                                                                
236701 CEB-LAGRA SECTION.                                                       
236801                                                                          
236901               MOVE ORAD-IDPURAD     TO TAB-PURAD    (IX-TAB)             
237001               MOVE SPAR-IDRONR      TO TAB-IDRONR   (IX-TAB)             
237101               MOVE SPAR-IDARTNR     TO TAB-IDARTNR  (IX-TAB)             
237201               MOVE SPAR-REKSIFFR    TO TAB-REKSIFFR (IX-TAB)             
237301               ADD  SPAR-KVLEVART    TO TAB-KVLEVART (IX-TAB)             
237401               MOVE SPAR-IDARTNR-ERS TO TAB-IDARTNR-ERS (IX-TAB)          
237501               MOVE IX-TAB           TO TAB-INDEX    (IX-TAB)             
237601               MOVE SPAR-BEART       TO TAB-BEART1   (IX-TAB)             
237701               MOVE SPAR-BERADREF    TO TAB-BERADREF (IX-TAB)             
237801               MOVE SPAR-IDSYSTEM    TO TAB-IDSYSTEM (IX-TAB)             
237901     .                                                                    
238001     EJECT                                                                
238101 CF-SKRIV-RUBRIKER SECTION.                                               
238102                                                                          
238201     ADD +1                  TO   ACK-ANTAL-SIDOR                         
238301     MOVE ACK-ANTAL-SIDOR    TO   RUB1-SIDNR                              
238401                                                                          
238501     IF DCS-CDC AND WS-KDPRTVAL = 'G '                                    
238601       CALL W006PRS1 USING PRT-SPOOL-A4S                                  
238701                           PRT-WRITE                                      
238801                           PRT-IDPRTLST                                   
238901                           ALT-PCB                                        
239001                           PRT-NYSIDA-RAD2                                
239101                           RUBRIKRAD-1                                    
239201     ELSE                                                                 
239301       IF (DCS-CDC AND                                                    
239401           WS-KDPRTVAL = '0' OR '2' OR '3' OR '4' OR '5' OR               
239501                         '6' OR '7' OR '8' OR '9' OR 'S')                 
239601          OR                                                              
239701          (DCS-SDC AND DCS-SWEDEN AND                                     
239801           (WS-KDPRTVAL = 'AA' OR 'LG' OR 'C' OR '11'))                   
239901         IF (DCS-CDC AND WS-KDPRTVAL = 'S ')                              
240001             CALL W006PRS1 USING PRT-SPOOL-A4S                            
240101                                 PRT-WRITE                                
240201                                 PRT-IDPRTLST                             
240301                                 ALT-PCB                                  
240401                                 PRT-AFTER-4                              
240501                                 RUBRIKRAD-1                              
240601         ELSE                                                             
240701           IF (DCS-CDC AND WS-KDPRTVAL = '0 ')                            
240801               CALL W006PRS1 USING PRT-SPOOL-A4S                          
240901                                   PRT-WRITE                              
241001                                   PRT-IDPRTLST                           
241101                                   ALT-PCB                                
241201                                   PRT-AFTER-4                            
241301                                   RUBRIKRAD-1                            
241401           ELSE                                                           
241501             IF ACK-ANTAL-SIDOR = +1                                      
241601               CALL W006PRS1 USING PRT-SPOOL-A4S                          
241701                                   PRT-WRITE                              
241801                                   PRT-IDPRTLST                           
241901                                   ALT-PCB                                
242001                                   PRT-AFTER-4                            
242101                                   RUBRIKRAD-1                            
242201             ELSE                                                         
242301               CALL W006PRS1 USING PRT-SPOOL-A4S                          
242401                                   PRT-WRITE                              
242501                                   PRT-IDPRTLST                           
242601                                   ALT-PCB                                
242701                                   PRT-NYSIDA-RAD4                        
242801                                   RUBRIKRAD-1                            
242901             END-IF                                                       
243001           END-IF                                                         
243101         END-IF                                                           
243201       ELSE                                                               
243301         CALL W006PRS1 USING PRT-SPOOL-A4S                                
243401                             PRT-WRITE                                    
243501                             PRT-IDPRTLST                                 
243601                             ALT-PCB                                      
243701                             PRT-NYSIDA-RAD4                              
243801                             RUBRIKRAD-1                                  
243901       END-IF                                                             
244001     END-IF                                                               
244101                                                                          
244201     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
244301                         PRT-WRITE                                        
244401                         PRT-IDPRTLST                                     
244501                         ALT-PCB                                          
244601                         PRT-AFTER-3                                      
244701                         RUBRIKRAD-2                                      
244801                                                                          
244901     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
245001                         PRT-WRITE                                        
245101                         PRT-IDPRTLST                                     
245201                         ALT-PCB                                          
245301                         PRT-AFTER-2                                      
245401                         ADRESSRAD-3                                      
245501                                                                          
245601     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
245701                         PRT-WRITE                                        
245801                         PRT-IDPRTLST                                     
245901                         ALT-PCB                                          
246001                         PRT-AFTER-1                                      
246101                         ADRESSRAD-4                                      
246201                                                                          
246301     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
246401                         PRT-WRITE                                        
246501                         PRT-IDPRTLST                                     
246601                         ALT-PCB                                          
246701                         PRT-AFTER-1                                      
246801                         ADRESSRAD-5                                      
246901                                                                          
247001     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
247101                         PRT-WRITE                                        
247201                         PRT-IDPRTLST                                     
247301                         ALT-PCB                                          
247401                         PRT-AFTER-1                                      
247501                         ADRESSRAD-6                                      
247601                                                                          
247701     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
247801                         PRT-WRITE                                        
247901                         PRT-IDPRTLST                                     
248001                         ALT-PCB                                          
248101                         PRT-AFTER-1                                      
248201                         ADRESSRAD-7                                      
248301                                                                          
248401     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
248501                         PRT-WRITE                                        
248601                         PRT-IDPRTLST                                     
248701                         ALT-PCB                                          
248801                         PRT-AFTER-1                                      
248901                         ADRESSRAD-8                                      
249001                                                                          
249101     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
249201                         PRT-WRITE                                        
249301                         PRT-IDPRTLST                                     
249401                         ALT-PCB                                          
249501                         PRT-AFTER-3                                      
249601                         RUBRIKRAD-8                                      
249701                                                                          
249801     MOVE +15 TO  ACK-ANTAL-RADER                                         
249901     MOVE 4 TO NEXT-RAD-SKIP                                              
250001     .                                                                    
250101     EJECT                                                                
250201 CG-SKRIV-SLUTRAD SECTION.                                                
250301                                                                          
250401     MOVE WS-VLORDBTO        TO SLUT-RAD-VLORDBTO                         
250501     MOVE WS-VKORDBTO        TO SLUT-RAD-VKORDBTO                         
250601                                                                          
250701     IF PBV-FORMAT = JA                                                   
250801       MOVE PRT-AFTER-3      TO RAD-SKIP                                  
250901     ELSE                                                                 
251001       MOVE PRT-AFTER-2      TO RAD-SKIP                                  
251101     END-IF                                                               
251201                                                                          
251301     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
251401                         PRT-WRITE                                        
251501                         PRT-IDPRTLST                                     
251601                         ALT-PCB                                          
251701                         RAD-SKIP                                         
251801                         SLUT-RAD                                         
251901     .                                                                    
252001     EJECT                                                                
252101 CI-SKRIV-DETALJRAD SECTION.                                              
252201                                                                          
252301     ADD  +1                    TO WS-SUM-ART                             
252401     ADD  TAB-KVLEVART (IX-TAB) TO WS-SUM-LEV                             
252501     MOVE TAB-IDRONR   (IX-TAB) TO RAD-IDORDNR-RO                         
252601     MOVE TAB-IDARTNR  (IX-TAB) TO RAD-IDARTNR                            
252701                                   W-IDARTNR                              
252801     INSPECT RAD-IDARTNR REPLACING LEADING ZERO BY SPACE                  
252901     MOVE TAB-BERADREF (IX-TAB) TO RAD-BERADREF                           
253001     MOVE TAB-REKSIFFR (IX-TAB) TO RAD-REKSIFFR                           
253101     MOVE TAB-IDARTNR-ERS (IX-TAB)                                        
253201                                TO RAD-IDARTNR-ERS                        
253301     MOVE TAB-KVLEVART (IX-TAB) TO RAD-KVLEVART                           
253401     MOVE TAB-INDEX    (IX-TAB) TO TABELL-INDEX                           
253501                                                                          
253601     IF WS-FL-SVENSK-FSEDEL = JA  OR                                      
253701        W-IDDISTR > 799                                                   
253801        PERFORM IMS-GET-BENA                                              
253901        IF SEGMENT-FINNS                                                  
254001           MOVE TEXT-BEART      TO RAD-BEART                              
254101        ELSE                                                              
254201           MOVE TAB-BEART1 (TABELL-INDEX) TO RAD-BEART                    
254301        END-IF                                                            
254401     ELSE                                                                 
254501        MOVE TAB-BEART1    (TABELL-INDEX) TO RAD-BEART                    
254601     END-IF                                                               
254701                                                                          
254801*LK* ADD LYNK&CO TO DELIVERY NOTE                                         
254802     MOVE WS-IDKUNDNR                  TO W-B201-IDKUNDNR                 
254803     MOVE WS-IDDISTR                   TO W-B201-IDDISTR                  
254804     PERFORM IMS-GU-GMTA01-WDB201                                         
254805                                                                          
254806     IF SEGMENT-FINNS                                                     
254904        IF GMT-KDKUNDKAT = 3                                              
255101           MOVE TAB-REKSIFFR (IX-TAB) TO LYNK-REKSIFFR                    
255201           MOVE 'LYNK&CO PART NO'  TO LYNK-BEART                          
255301                                                                          
255401           PERFORM IMS-GU-WDF502                                          
255501           IF SEGMENT-FINNS                                               
255601             MOVE XLEV-IDLEVART    TO LYNK-IDARTNR                        
255701           END-IF                                                         
255702                                                                          
255703           MOVE JA                 TO LYNK-PART                           
255901        END-IF                                                            
255902     END-IF                                                               
256001                                                                          
256101     IF PBV-FORMAT = JA                                                   
256201       MOVE NEXT-RAD-SKIP    TO RAD-SKIP                                  
256301     ELSE                                                                 
256401       IF FIRST-LINE = JA                                                 
256501         MOVE PRT-AFTER-1    TO RAD-SKIP                                  
256601         MOVE NEJ            TO FIRST-LINE                                
256701       ELSE                                                               
256801         IF LYNK-PART = NEJ                                               
256901           IF LAST-LYNK-LINE = JA                                         
257001             MOVE PRT-AFTER-2    TO RAD-SKIP                              
257101             MOVE NEJ            TO LAST-LYNK-LINE                        
257201           ELSE                                                           
257301             MOVE PRT-AFTER-1    TO RAD-SKIP                              
257401           END-IF                                                         
257501         ELSE                                                             
257601           MOVE PRT-AFTER-2    TO RAD-SKIP                                
257701         END-IF                                                           
257801       END-IF                                                             
257901     END-IF                                                               
258001                                                                          
258101     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
258201                         PRT-WRITE                                        
258301                         PRT-IDPRTLST                                     
258401                         ALT-PCB                                          
258501                         RAD-SKIP                                         
258601                         DETALJRAD                                        
258701                                                                          
258801     IF LYNK-PART = JA                                                    
258901        CALL W006PRS1 USING PRT-SPOOL-A4S                                 
259001                            PRT-WRITE                                     
259101                            PRT-IDPRTLST                                  
259201                            ALT-PCB                                       
259301                            PRT-AFTER-1                                   
259401                            DETALJLYNK                                    
259501                                                                          
259601        MOVE NEJ            TO LYNK-PART                                  
259701        MOVE JA             TO LAST-LYNK-LINE                             
259801     END-IF                                                               
259901                                                                          
260001     ADD RAD-SKIP               TO ACK-ANTAL-RADER                        
260101     ADD +1                     TO IX-TAB                                 
260201     MOVE 3                     TO NEXT-RAD-SKIP                          
260301     .                                                                    
260401     EJECT                                                                
260501 CJ-AVSLUTA-LISTA SECTION.                                                
260601                                                                          
260701     MOVE WS-IDPRTLST      TO PRT-IDPRTLST                                
260801       IF WS-KDPRTVAL = 'AA'                                              
260901         CALL W006PRS1 USING PRT-SPOOL-A4S                                
261001                             PRT-WRITE                                    
261101                             PRT-IDPRTLST                                 
261201                             ALT-PCB                                      
261301                             PRT-NYSIDA-RAD4                              
261401                             BLANKRAD                                     
261501       ELSE                                                               
261601         CALL W006PRS1 USING PRT-SPOOL-A4S                                
261701                             PRT-WRITE                                    
261801                             PRT-IDPRTLST                                 
261901                             ALT-PCB                                      
262001                             PRT-AFTER-4                                  
262101                             BLANKRAD                                     
262201       END-IF                                                             
262301                                                                          
262401     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
262501                         PRT-CLOSE                                        
262601                         PRT-IDPRTLST                                     
262701                         ALT-PCB                                          
262801                         WS-DUMMY                                         
262901                         PRT-IDLIST                                       
263001     .                                                                    
263101     EJECT                                                                
263201 CM-SORTERA-TABELL SECTION.                                               
263301                                                                          
263401     MOVE +36            TO TABENTRY-LNGD                                 
263501     MOVE TAB-ANT        TO ANTAL-ENTRY                                   
263601     MOVE +5             TO SORTBGP-LNGD                                  
263701                                                                          
263801     CALL WINTSOR    USING TABELL                                         
263901                           TABENTRY-LNGD                                  
264001                           ANTAL-ENTRY                                    
264101                           TAB-IDARTNR (1)                                
264201                           SORTBGP-LNGD                                   
264301     .                                                                    
264401     EJECT                                                                
264501 CD-LAES-HAEMTA-B201-B101      SECTION.                                   
264601                                                                          
264701     MOVE WS-IDKUNDNR                  TO W-B201-IDKUNDNR                 
264801     MOVE WS-IDDISTR                   TO W-B201-IDDISTR                  
264901     PERFORM IMS-GU-GMTA01-WDB201                                         
265001                                                                          
265101     IF SEGMENT-FINNS                                                     
265201         IF  GMT-ADGMT  = SPACE                                           
265301         AND GMT-BEGMT  = SPACE                                           
265401             MOVE GMT-IDPARTNR         TO W-IDPARTNR                      
265501             MOVE WS-DCS-IDFTG         TO W-IDFTG                         
265601                                                                          
265701             PERFORM IMS-GU-BETC01-WDB101                                 
265801             IF SEGMENT-FINNS                                             
265901               MOVE BET-BEBETRAD-1     TO RAD4-BEGMT-RAD1                 
266001               MOVE BET-BEBETRAD-2     TO RAD5-BEGMT-RAD2                 
266101               MOVE BET-ADBETRAD-1     TO RAD6-ADGMT-GATA                 
266201               MOVE BET-ADBETRAD-2     TO RAD7-ADGMT-PADR                 
266301               MOVE SPACE              TO RAD8-ADGMT-LAND                 
266401             ELSE                                                         
266501               MOVE 'TEXT MISSING'     TO RAD4-BEGMT-RAD1                 
266601             END-IF                                                       
266701                                                                          
266801         ELSE                                                             
266901             IF GMT-BEGMT NOT = SPACE                                     
267001                 MOVE GMT-BEGMT-RAD1       TO RAD4-BEGMT-RAD1             
267101                 MOVE GMT-BEGMT-RAD2       TO RAD5-BEGMT-RAD2             
267201                 MOVE GMT-ADGMT-GATA       TO RAD6-ADGMT-GATA             
267301                 MOVE GMT-ADGMT-PADR       TO RAD7-ADGMT-PADR             
267401                 MOVE GMT-ADGMT-LAND       TO RAD8-ADGMT-LAND             
267501             ELSE                                                         
267601                 MOVE GMT-ADGMT-GATA       TO RAD4-BEGMT-RAD1             
267701                 MOVE GMT-ADGMT-PADR       TO RAD5-BEGMT-RAD2             
267801                 MOVE SPACE                TO RAD6-ADGMT-GATA             
267901                                              RAD7-ADGMT-PADR             
268001                                              RAD8-ADGMT-LAND             
268101             END-IF                                                       
268201         END-IF                                                           
268301     ELSE                                                                 
268401         MOVE SPACE                    TO RAD4-BEGMT-RAD1                 
268501                                          RAD5-BEGMT-RAD2                 
268601                                          RAD6-ADGMT-GATA                 
268701                                          RAD7-ADGMT-PADR                 
268801                                          RAD8-ADGMT-LAND                 
268901     END-IF                                                               
269001     .                                                                    
269101     EJECT                                                                
269201 D-ADRESS-TILL-RAETT-MOD SECTION.                                         
269301                                                                          
269401     MOVE MFS-IDTRANS                  TO MOD-IDTRANS                     
269501     MOVE MSG-MOD-NAME                 TO MFS-IDMOD                       
269601     INSPECT MFS-IDMOD   REPLACING FIRST 'I' BY 'O'                       
269701     MOVE MOD-LAENGD-OEVRIGA           TO MSG-KVLL                        
269801     .                                                                    
269901     EJECT                                                                
270001 F-DELIVERY-NOTE-NA SECTION.                                              
270101                                                                          
270201     MOVE WS-IDDISTR          TO WS-IDDISTR-NUM5                          
270301                                 W-E4A1-IDDISTR                           
270401     MOVE WS-IDKUNDNR         TO WS-IDKUNDNR-NUM7                         
270501                                 W-E4A1-IDKUNDNR                          
270601     MOVE WS-IDORDNR          TO WS-IDORDNR-NUM7                          
270701                                 W-E4A1-IDORDNR                           
270801                                                                          
270901     MOVE WS-IDDISTR-NUM5     TO W-Q5A-IDDISTR-MIN                        
271001                                 W-Q5A-IDDISTR-MAX                        
271101     MOVE WS-IDKUNDNR-NUM7    TO W-Q5A-IDKUNDNR-MIN                       
271201                                 W-Q5A-IDKUNDNR-MAX                       
271301     MOVE WS-IDORDNR-NUM7     TO W-Q5A-IDORDNR7-MIN                       
271401                                 W-Q5A-IDORDNR7-MAX                       
271501                                                                          
271601     PERFORM IMS-GU-WDQ5A1-MIN-MAX                                        
271701     IF SEGMENT-FINNS                                                     
271801                                                                          
271901        PERFORM IMS-GU-E401-KVAL-SEK                                      
272001        IF WDE401-SEK-FINNS                                               
272101                                                                          
272201          MOVE NEJ            TO WS-RATT-PRODNR                           
272301                                                                          
272401          PERFORM UNTIL (WDE401-SEK-SAKNAS) OR                            
272501                        WS-RATT-PRODNR = JA                               
272601             IF  KORD-IDDC             = WS-IDDC                          
272701             AND KORD-KVORDRAD-LEVPL   = ZERO                             
272801                MOVE KORD-IDPRODNR    TO WS-IDPRODNR                      
272901                MOVE JA               TO WS-RATT-PRODNR                   
273001             END-IF                                                       
273101             PERFORM IMS-GN-E401-KVAL-SEK                                 
273201          END-PERFORM                                                     
273301        END-IF                                                            
273401                                                                          
273501        IF WS-RATT-PRODNR = JA                                            
273601           COMPUTE 4349-LL =                                              
273701           LENGTH OF 4349-MID-W4I34901 + 17                               
273801           MOVE MFS-KDMFSFOR        TO 4349-SPRAK                         
273901           MOVE SEQA-IDDISTR        TO 4349-MID-IDDISTR                   
274001           MOVE SEQA-IDKUNDNR       TO 4349-MID-IDKUNDNR                  
274101           MOVE SEQA-IDORDNR7       TO 4349-MID-IDORDNR7                  
274201           MOVE WS-IDPRODNR         TO 4349-MID-IDPRODNR                  
274301           MOVE WS-IDDC             TO 4349-MID-IDDC                      
274401           MOVE SEQA-IDORDER        TO 4349-MID-IDORDER                   
274501                                                                          
274601           PERFORM IMS-INSERT-TRANS4349                                   
274701        ELSE                                                              
274801           MOVE FEL-798 (INDX)     TO MOD-TEMFSFEL                        
274901           MOVE JA                 TO WS-FEL-FUNNET                       
275001        END-IF                                                            
275101      ELSE                                                                
275201         MOVE FEL-799 (INDX)     TO MOD-TEMFSFEL                          
275301         MOVE JA                 TO WS-FEL-FUNNET                         
275401      END-IF                                                              
275501     .                                                                    
275601     EJECT                                                                
275701                                                                          
275801                                                                          
275901 S10-BEHANDLA-KOLLI-FAELT    SECTION.                                     
276001                                                                          
276101     IF NYCKLAR-OK                                                        
276201       IF WS-IDDC NOT = W-IDDC-B6                                         
276301          MOVE WS-IDDC TO W-IDDC-B6                                       
276401          PERFORM IMS-GU-WDB601                                           
276501       END-IF                                                             
276601       EVALUATE TRUE                                                      
276701         WHEN DCS-NDC-PF                                                  
276801              CONTINUE                                                    
276901         WHEN DCS-NDC-NA                                                  
277001          MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDKOLLI-IN                   
277101                                         MOD-IDKOLLI-TOM-IN               
277201          MOVE MFS-RENSA-FAELT        TO MOD-IDKOLLI-IN                   
277301                                         MOD-IDKOLLI-TOM-IN               
277401       END-EVALUATE                                                       
277501     END-IF                                                               
277601     .                                                                    
277701     SKIP2                                                                
277801 S11-DIST-KUND-LDC SECTION.                                               
277901                                                                          
278001     PERFORM IMS-GU-GMTA01-WDB201                                         
278101                                                                          
278201     IF SEGMENT-SAKNAS                                                    
278301        MOVE NEJ              TO GMT-FLLDCKND                             
278401     END-IF                                                               
278501     .                                                                    
278601     EJECT                                                                
278701                                                                          
278801* IMS SEKTIONER                                                           
278901                                                                          
279001 IMS-GET-MSG SECTION.                                                     
279101     MOVE '  QC' TO GODK-STATUSKODER                                      
279201     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
279301     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
279401     PERFORM IMS-STATUSKONTROLL                                           
279501     .                                                                    
279601 IMS-INSERT-MSG SECTION.                                                  
279701     IF NOT ENGLISH-TEXT                                                  
279801       MOVE '0' TO MFS-KDHUVOMR                                           
279901     END-IF                                                               
280001     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
280101     MOVE SPACE TO GODK-STATUSKODER                                       
280201     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
280301     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
280401     PERFORM IMS-STATUSKONTROLL                                           
280501     .                                                                    
280601     EJECT                                                                
280701 IMS-GU-E601-KVAL SECTION.                                                
280801                                                                          
280901     MOVE 'WDE601  '      TO SSA1                                         
281001     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
281101            DELIMITED BY SIZE INTO SSA1                                   
281201     MOVE '  ' TO GODK-STATUSKODER                                        
281301     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
281401     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
281501     PERFORM IMS-STATUSKONTROLL                                           
281601     .                                                                    
281701     SKIP3                                                                
281801 IMS-GU-E401-KVAL-SEK SECTION.                                            
281901     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
282001            DELIMITED BY SIZE INTO SSA1                                   
282101     MOVE '  GEGB' TO GODK-STATUSKODER                                    
282201     CALL CBLTDLI USING GU                                                
282301                          WDE4A-PCB                                       
282401                          DLI-IO-AREA2                                    
282501                          SSA1                                            
282601     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
282701                               STATUS-WDE401-SEK-WS                       
282801     PERFORM IMS-STATUSKONTROLL                                           
282901     .                                                                    
283001     SKIP3                                                                
283101 IMS-GN-WDE411-21-FSEQ SECTION.                                           
283201     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4F1KY ')'                        
283301            DELIMITED BY SIZE INTO SSA1                                   
283401     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
283501            DELIMITED BY SIZE INTO SSA2                                   
283601     MOVE '  GE' TO GODK-STATUSKODER                                      
283701     CALL CBLTDLI USING GN WDE4F-PCB WDE4F-AREA SSA1 SSA2                 
283801     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
283901                               STATUS-WS-RAD                              
284001     PERFORM IMS-STATUSKONTROLL                                           
284101     .                                                                    
284201     EJECT                                                                
284301 IMS-GN-E401-KVAL-SEK SECTION.                                            
284401     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
284501            DELIMITED BY SIZE INTO SSA1                                   
284601     MOVE '  GEGB' TO GODK-STATUSKODER                                    
284701     CALL CBLTDLI USING GN                                                
284801                          WDE4A-PCB                                       
284901                          DLI-IO-AREA2                                    
285001                          SSA1                                            
285101     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
285201                               STATUS-WDE401-SEK-WS                       
285301     PERFORM IMS-STATUSKONTROLL                                           
285401     .                                                                    
285501     EJECT                                                                
285601 IMS-GU-GMTA01-WDB201      SECTION.                                       
285701     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
285801            DELIMITED BY SIZE INTO SSA1                                   
285901     MOVE '  GE' TO GODK-STATUSKODER                                      
286001     CALL CBLTDLI USING GU GMTA-PCB GMT-WDB201 SSA1                       
286101     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
286201     PERFORM IMS-STATUSKONTROLL                                           
286301     SKIP3                                                                
286401     .                                                                    
286501 IMS-GU-BETC01-WDB101      SECTION.                                       
286601     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
286701            DELIMITED BY SIZE INTO SSA1                                   
286801     MOVE '  GE' TO GODK-STATUSKODER                                      
286901     CALL CBLTDLI USING GU BETC-PCB BET-WDB101 SSA1                       
287001     MOVE BETC-STATUS-CODE TO STATUS-WS                                   
287101     PERFORM IMS-STATUSKONTROLL                                           
287201     SKIP3                                                                
287301     .                                                                    
287401 IMS-GNP-E611-KVAL SECTION.                                               
287501                                                                          
287601     STRING 'WDE611  (IDKOLLI  =' W-E611-IDKOLLI-X ')'                    
287701            DELIMITED BY SIZE INTO SSA1                                   
287801     MOVE '  GE' TO GODK-STATUSKODER                                      
287901     CALL CBLTDLI USING GNP                                               
288001                          WDE6-PCB                                        
288101                          DLI-IO-AREA                                     
288201                          SSA1                                            
288301     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
288401                              STATUS-WS-KOLLI                             
288501     PERFORM IMS-STATUSKONTROLL                                           
288601     .                                                                    
288701     SKIP3                                                                
288801 IMS-GNP-E611-MIN-MAX   SECTION.                                          
288901                                                                          
289001     STRING 'WDE611  (IDKOLLI >=' W-E611-IDKOLLI-X                        
289101                    '&IDKOLLI <=' W-E611-IDKOLLI-TOM-X ')'                
289201            DELIMITED BY SIZE INTO SSA1                                   
289301     MOVE '  GE' TO GODK-STATUSKODER                                      
289401     CALL CBLTDLI USING GNP                                               
289501                          WDE6-PCB                                        
289601                          DLI-IO-AREA                                     
289701                          SSA1                                            
289801     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
289901                              STATUS-WS-KOLLI                             
290001     PERFORM IMS-STATUSKONTROLL                                           
290101     .                                                                    
290201     SKIP3                                                                
290301 IMS-GU-ORQI        SECTION.                                              
290401                                                                          
290501     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X  ')'                        
290601            DELIMITED BY SIZE INTO SSA1                                   
290701     MOVE '  ' TO GODK-STATUSKODER                                        
290801     CALL CBLTDLI USING GU                                                
290901                        ORQI-PCB                                          
291001                        DLI-IO-AREA                                       
291101                        SSA1                                              
291201     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
291301     PERFORM IMS-STATUSKONTROLL                                           
291401     .                                                                    
291501     EJECT                                                                
291601 IMS-GET-BENA            SECTION.                                         
291701                                                                          
291801     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
291901            DELIMITED BY SIZE INTO SSA1                                   
292001     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
292101            DELIMITED BY SIZE INTO SSA2                                   
292201     MOVE '  GE' TO GODK-STATUSKODER                                      
292301     CALL  CBLTDLI  USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2               
292401     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
292501     PERFORM IMS-STATUSKONTROLL                                           
292601     SKIP2                                                                
292701                                                                          
292801     .                                                                    
292901*DN4                                                                      
293001 IMS-INSERT-TRANS4349 SECTION.                                            
293101                                                                          
293201     MOVE LOW-VALUE            TO 4349-Z1                                 
293301                                  4349-Z2                                 
293401     MOVE SPACE                TO GODK-STATUSKODER                        
293501     CALL CBLTDLI USING ISRT ALT49-PCB 4349-MSG-IO-AREA                   
293601     MOVE ALT49-STATUS-CODE   TO STATUS-WS                                
293701     PERFORM IMS-STATUSKONTROLL                                           
293801     .                                                                    
293901     EJECT                                                                
294001                                                                          
294101                                                                          
294201 IMS-GU-WDQ5A1-MIN-MAX SECTION.                                           
294301                                                                          
294401     STRING 'WDQ5A1  (WDQ5A1KY=>' W-WDQ5A1KY-MIN-X                        
294501                    '&WDQ5A1KY<=' W-WDQ5A1KY-MAX-X ')'                    
294601          DELIMITED BY SIZE INTO SSA1                                     
294701     MOVE '  GE'               TO GODK-STATUSKODER                        
294801     CALL CBLTDLI USING GU WDQ5A-PCB DLI-IO-WDQ5A1 SSA1                   
294901     MOVE WDQ5A-STATUS-CODE   TO STATUS-WS                                
295001     PERFORM IMS-STATUSKONTROLL                                           
295101     .                                                                    
295201     SKIP2                                                                
295301                                                                          
295401                                                                          
295501*DN4                                                                      
295601                                                                          
295701                                                                          
295801 IMS-STATUSKONTROLL SECTION.                                              
295901     SET STATUS-IX TO 1                                                   
296001     SEARCH GODK-STATUS                                                   
296101       AT END                                                             
296201         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
296301           DELIMITED BY SIZE INTO FELTEXT                                 
296401           CALL FELLOG                                                    
296501       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
296601         CONTINUE                                                         
296701     END-SEARCH                                                           
296801     .                                                                    
296901                                                                          
297001 IMS-GU-WDB601    SECTION.                                                
297101     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
297201          DELIMITED BY SIZE INTO SSA1                                     
297301     MOVE '  GE'   TO GODK-STATUSKODER                                    
297401     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
297501     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
297601     PERFORM IMS-STATUSKONTROLL                                           
297701     IF SEGMENT-SAKNAS                                                    
297801        MOVE SPACE TO DCS-KDDC                                            
297901     END-IF                                                               
298001     .                                                                    
298101     EJECT                                                                
298201                                                                          
298301 IMS-GU-WDF502 SECTION.                                                   
298401     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
298501            DELIMITED BY SIZE INTO SSA1                                   
298601     MOVE 'WDF502  '       TO SSA2                                        
298701     MOVE '  GE'           TO GODK-STATUSKODER                            
298801     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
298901     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
299001     PERFORM IMS-STATUSKONTROLL                                           
299101     .                                                                    
299201                                                                          
299301 DB2-SELECT-TP4TRAN     SECTION.                                          
299401     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
299501                                                                          
299601     MOVE 000100 TO GODK-SQLCODEKODER                                     
299701                                                                          
299801     EXEC SQL                                                             
299901           SELECT  DISTINCT                                               
300001                   IDDC_REC                                               
300101                                                                          
300201           INTO   :TP4TRAN-IDDC-REC                                       
300301                                                                          
300401           FROM    TP4TRAN                                                
300501                                                                          
300601           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
300701     END-EXEC                                                             
300801                                                                          
300901     MOVE SQLCODE TO SQLCODE-WS                                           
301001     PERFORM DB2-STATUSKONTROLL                                           
301101     .                                                                    
301201     EJECT                                                                
301301 DB2-STATUSKONTROLL  SECTION.                                             
301401                                                                          
301501     SET SQLCODE-IX TO 1                                                  
301601     SEARCH GODK-SQLCODE                                                  
301701       AT END                                                             
301801          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
301901          DELIMITED BY SIZE INTO FELTEXT                                  
302001          CALL ABEND USING RKOD-ABEND-DB2                                 
302101       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
302201     END-SEARCH                                                           
303000     .                                                                    
