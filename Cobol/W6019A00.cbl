000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6019A00.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/09/09.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BAKGRUNDS MPP SOM SKRIVER UT PLATSSÄTTARLISTA.                   
001100*        STARTAS AV W60141 OCH W20205.                                    
001200                                                                          
001300*        PROGRAMMET          UPPDATERAR WLLISB (WDG8)                     
001400*        PROGRAMMET          LÄSER      W6INLA (W6D1)                     
001600*                                       WLARTC (WDK6)                     
001700*                                       WLERSA (WDD7)                     
001800*                                       WLBENA (WDD3)                     
001900*    SUB PROGRAMMET W611STYR LÄSER      W6INLA (W6G1)                     
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W6T19A                                              
002300*        MID:         W6I19A01                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        PLATSSÄTTARLISTA                                                 
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W6019900'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
004200 77  MAX-TAB-IX                  PIC S9(3)   VALUE +3 COMP-3.             
004300 77  MAX-KVRADER                 PIC S9(3)   VALUE +42 COMP-3.            
004400 77  W-KVRADER                   PIC S9(7)   VALUE ZERO COMP-3.           
004500 77  W-KVINLART-VOR              PIC S9(7)   VALUE ZERO COMP-3.           
004600 77  W-IDSIDNR                   PIC S9(3)   VALUE ZERO COMP-3.           
004700 77  W-IDARTNR-EMBQ3             PIC S9(9)   VALUE ZERO COMP-3.           
004800 77  W-ADLAGOMR                  PIC  9(2)   VALUE ZERO.                  
004810 77  W-SPAR-IDILIST              PIC  9(5)   VALUE ZERO.                  
004900                                                                          
005000     EJECT                                                                
005100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005200 01  GENERELLA-SUBPROGRAM.                                                
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005500     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
005600     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
005700     SKIP3                                                                
005800* VARIABLER TILL SUBPROGRAM W006PRR1                                      
005900*01  -COPY W006PRAR                                                       
006000     SKIP2                                                                
006100     EJECT                                                                
006200* VARIABLER TILL SUBPROGRAM W611STYR                                      
006300*01  -COPY W611STYR                                                       
006400     EJECT                                                                
006500 01  WS-RAPP-AREA.                                                        
006600     03  WS-RAPP-LISTID.                                                  
006610         05  FILLER              PIC X(5)    VALUE 'PLATS'.               
006620         05  WS-RAPP-IDILIST     PIC 9(5)    VALUE ZERO.                  
006700     03  WS-RAPP-LISTRAD.                                                 
006800         05  FILLER              PIC X(2)    VALUE SPACE.                 
006900         05  WS-RAPP-RAD         PIC X(114).                              
007000     03  WS-DUMMY                PIC X(1).                                
007100     03  WS-RAPP-PRINTER         PIC X(8).                                
007200     EJECT                                                                
007300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007400*                                                                         
007500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007600     SKIP3                                                                
007700*01  MID -COPY W6I19A01                                                   
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008000     SKIP3                                                                
008100*01  -COPY WMSGAREA                                                       
008200     EJECT                                                                
008300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008400*                                                                         
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  NYCKLAR-TILL-DLI.                                                    
008900     03  W-W6D1D1KY-MIN-X.                                                
009000        05  W-D1D1KY-IDILIST-MIN     PIC  9(5) VALUE ZERO.                
009100        05  W-D1D1KY-IDILIRAD-MIN    PIC S9(5) COMP-3 VALUE ZERO.         
009200        05  W-D1D1KY-IDRADNR-INL-MIN PIC S9(5) COMP-3 VALUE ZERO.         
009300        05  W-D1D1KY-IDDC-MIN        PIC X(2)  VALUE '11'.                
009310        05  W-D1D1KY-IDLEVNR-MIN     PIC X(5)  VALUE SPACE.               
009400        05  W-D1D1KY-IDFS-MIN        PIC  X(8) VALUE SPACE.               
009500        05  W-D1D1KY-TIAVIDAT-MIN    PIC S9(7) COMP-3 VALUE ZERO.         
009600        05  W-D1D1KY-IDRADNR-MIN     PIC S9(5) COMP-3 VALUE ZERO.         
009700                                                                          
009800     03  W-W6D1D1KY-MAX-X.                                                
009900        05  W-D1D1KY-IDILIST-MAX     PIC  9(5) VALUE ZERO.                
010000        05  W-D1D1KY-IDILIRAD-MAX    PIC S9(5) COMP-3 VALUE ZERO.         
010100        05  W-D1D1KY-IDRADNR-INL-MAX PIC S9(5) COMP-3 VALUE ZERO.         
010200        05  W-D1D1KY-IDDC-MAX        PIC X(2)  VALUE '11'.                
010210        05  W-D1D1KY-IDLEVNR-MAX     PIC X(5)  VALUE SPACE.               
010300        05  W-D1D1KY-IDFS-MAX        PIC  X(8) VALUE SPACE.               
010400        05  W-D1D1KY-TIAVIDAT-MAX    PIC S9(7) COMP-3 VALUE ZERO.         
010500        05  W-D1D1KY-IDRADNR-MAX     PIC S9(5) COMP-3 VALUE ZERO.         
010600                                                                          
010700     03  W-W6D101KY-X.                                                    
010800         05  W-D101KY-IDDC       PIC X(2)     VALUE '11'.                 
010810         05  W-D101KY-IDLEVNR    PIC X(5)     VALUE SPACE.                
010900         05  W-D101KY-IDFS       PIC X(8)     VALUE SPACE.                
011000         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
011100                                                                          
011200     03  W-IDARTNR-X.                                                     
011300        05  W-IDARTNR                PIC S9(9) COMP-3 VALUE ZERO.         
011400                                                                          
011410     03  W-IDRADNR-INL-X.                                                 
011420        05  W-IDRADNR-INL            PIC S9(5) COMP-3 VALUE ZERO.         
011430                                                                          
011500     03  W-IDRADNR-X.                                                     
011600        05  W-IDRADNR                PIC S9(5) COMP-3 VALUE ZERO.         
011700                                                                          
012100     03  W-W6D1CSEQ-X.                                                    
012200        05  W-D1CSEQ-IDLEVNR-KOLLI   PIC X(5)  VALUE SPACE.               
012300        05  W-D1CSEQ-IDOKOLLI        PIC  9(9) VALUE ZERO.                
012400                                                                          
012500     03  W-IDLEVNR-KOLLI-X.                                               
012600        05  W-IDLEVNR-KOLLI          PIC X(5)  VALUE SPACE.               
012700                                                                          
012800     03  W-IDOKOLLI-X.                                                    
012900        05  W-IDOKOLLI               PIC  9(9) VALUE ZERO.                
013000                                                                          
013100    03  W-WDD7A1KY-MIN-X.                                                 
013200     05  W-D7A1KY-TILLK-IDARTNR-MIN  PIC S9(9) COMP-3 VALUE ZERO.         
013300     05  W-D7A1KY-ERS-IDARTNR-MIN    PIC S9(9) COMP-3 VALUE ZERO.         
013400     05  W-D7A1KY-TILLK-IDKORTNR-MIN PIC S9(3) COMP-3 VALUE ZERO.         
013500                                                                          
013600    03  W-WDD7A1KY-MAX-X.                                                 
013700     05  W-D7A1KY-TILLK-IDARTNR-MAX  PIC S9(9) COMP-3 VALUE ZERO.         
013800     05  W-D7A1KY-ERS-IDARTNR-MAX    PIC S9(9) COMP-3 VALUE ZERO.         
013900     05  W-D7A1KY-TILLK-IDKORTNR-MAX PIC S9(3) COMP-3 VALUE ZERO.         
014000                                                                          
014100     03  W-IDSKYLT-X.                                                     
014200         05  W-IDSKYLT               PIC X(3)    VALUE SPACE.             
014300     SKIP2                                                                
014400*    --- STATUS-KOD FRÅN IMS                                              
014500 01  STATUS-WS                   PIC XX.                                  
014600     88  SEGMENT-FINNS                       VALUE '  '.                  
014700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
015000     SKIP2                                                                
015100 01  GODK-STATUSKODER.                                                    
015200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015300     SKIP3                                                                
015400 01  SSA1                        PIC X(128).                              
015500 01  SSA2                        PIC X(64).                               
015600     EJECT                                                                
015700*    --- IMS FUNKTIONSKODER                                               
015800*01  -COPY W0003                                                          
015900     EJECT                                                                
016000*    ---  DLI INPUT-OUTPUT AREA                                           
016100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016200     SKIP3                                                                
016300 01  DLI-IO-AREA1.                                                        
016400     03  IO-AREA1                PIC X(900)  VALUE SPACE.                 
016500     SKIP3                                                                
016600     03  W6INLE01 REDEFINES IO-AREA1.                                     
016700*        05  -COPY W6D1D1                                                 
016800     EJECT                                                                
017500     03  WLARTC11 REDEFINES IO-AREA1.                                     
017600*        05  -COPY WDK611     -PRE ARTC-                                  
017700     EJECT                                                                
018400     03  WLERSB01 REDEFINES IO-AREA1.                                     
018500*        05  -COPY WDD7A1                                                 
018600     EJECT                                                                
018700     03  WLBENA01 REDEFINES IO-AREA1.                                     
018800*        05  -COPY WDD311    -PRE BENA11-                                 
018900     EJECT                                                                
019000 01  DLI-IO-AREA2.                                                        
019100     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
019200     SKIP3                                                                
019300     03  W6INLA11 REDEFINES IO-AREA2.                                     
019400*        05  -COPY W6D111                                                 
019500     EJECT                                                                
019600 01  DLI-IO-AREA3.                                                        
019700     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
019800     SKIP3                                                                
019900     03  W6INLA11 REDEFINES IO-AREA3.                                     
020000*        05  -COPY W6D111   -PRE DIV-                                     
020100     EJECT                                                                
020110     03  W6INLA21 REDEFINES IO-AREA3.                                     
020120*        05  -COPY W6D121                                                 
020130     EJECT                                                                
020200 01  DLI-IO-AREA4.                                                        
020300     03  IO-AREA4                PIC X(150)  VALUE SPACE.                 
020400     SKIP3                                                                
020500     03  W6INLA01 REDEFINES IO-AREA4.                                     
020600*        05  -COPY W6D101                                                 
020700     EJECT                                                                
020800*  PRINTRADER FÖR INLÄGGNINGSLISTA RAPPORT                                
020900                                                                          
021000 01  LIST-HRAD1.                                                          
021100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
021200     03   FILLER                  PIC X(22) VALUE                         
021300                                        'VOLVO CAR AFTER SALES '.         
021400     03   FILLER                  PIC X(19) VALUE SPACE.                  
021500     03   FILLER                  PIC X(6)  VALUE 'W6019A'.               
021600     03   FILLER                  PIC X(11) VALUE SPACE.                  
021700     03   FILLER                  PIC X(16) VALUE                         
021800                                        'PLATSSÄTTARLISTA'.               
021900     03   FILLER                  PIC X(24) VALUE SPACE.                  
022000     03   HRAD1-DATUM             PIC X(6)  VALUE SPACE.                  
022100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
022200     03   FILLER                  PIC X(4)  VALUE 'SID '.                 
022300     03   HRAD1-IDSIDNR           PIC Z(2)9 VALUE ZERO.                   
022400                                                                          
022500 01  LIST-HRAD2.                                                          
022600     03   FILLER                  PIC X(59) VALUE SPACE.                  
022700     03   FILLER                  PIC X(19) VALUE                         
022800                                        'INLÄGGNINGSLISTA NR'.            
022900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023000     03   HRAD2-IDILIST           PIC Z(5)  VALUE ZERO.                   
023100                                                                          
023200 01  LIST-HRAD3.                                                          
023300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023400     03   FILLER                  PIC X(7)  VALUE 'LEVNR  '.              
023500     03   HRAD3-IDLEVNR-KOLLI     PIC X(5)  VALUE SPACE.                  
023600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023700     03   FILLER                  PIC X(8)  VALUE 'KOLLINR '.             
023800     03   HRAD3-IDOKOLLI          PIC Z(9)  VALUE ZERO.                   
023900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
024000     03   FILLER                  PIC X(5)  VALUE 'VAGN '.                
024100     03   HRAD3-IDINLVGN          PIC Z(3)  VALUE ZERO.                   
024200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
024300     03   FILLER                  PIC X(5)  VALUE 'PLAC '.                
024400     03   HRAD3-ADINLOMR          PIC X(4)  VALUE SPACE.                  
024500     03   FILLER                  PIC X(8)  VALUE SPACE.                  
024600     03   FILLER                  PIC X(11) VALUE 'LISTA FÖR: '.          
024700     03   HRAD3-ADLAGOMR          PIC X(4)  VALUE SPACE.                  
024800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
024900     03   FILLER                  PIC X(5)  VALUE 'TORG '.                
025000     03   HRAD3-ADINLOMR-TORG     PIC X(4)  VALUE SPACE.                  
025100                                                                          
025200 01  LIST-HRUB4.                                                          
025300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
025400     03   FILLER                  PIC X(4)  VALUE 'RAD '.                 
025500     03   FILLER                  PIC X(11) VALUE 'LAGERPLATS '.          
025600     03   FILLER                  PIC X(5)  VALUE SPACE.                  
025700     03   FILLER                  PIC X(6)  VALUE 'ARTNR '.               
025800     03   FILLER                  PIC X(10) VALUE 'BENÄMNING '.           
025900     03   FILLER                  PIC X(17) VALUE SPACE.                  
026000     03   FILLER                  PIC X(5)  VALUE 'ANTAL'.                
026100     03   FILLER                  PIC X(5)  VALUE SPACE.                  
026200     03   FILLER                  PIC X(4)  VALUE 'VOR '.                 
026300     03   FILLER                  PIC X(5)  VALUE 'PRIO '.                
026400     03   FILLER                  PIC X(2)  VALUE 'FB'.                   
026500     03   FILLER                  PIC X(4)  VALUE SPACE.                  
026600     03   FILLER                  PIC X(3)  VALUE 'FT '.                  
026700     03   FILLER                  PIC X(6)  VALUE 'LEVNR '.               
026800     03   FILLER                  PIC X(12) VALUE 'PARTI/KOLLI '.         
026900     03   FILLER                  PIC X(11) VALUE 'NOTERINGAR'.           
027000                                                                          
027100 01  LIST-LRAD1.                                                          
027200     03   FILLER                   PIC X(1).                              
027300     03   LRAD1-IDILIRAD           PIC Z(3).                              
027400     03   FILLER                   PIC X(1)  VALUE SPACE.                 
027500     03   LRAD1-ADLAGOMR           PIC Z(1)9.                             
027600     03   FILLER                   PIC X(1)  VALUE SPACE.                 
027700     03   LRAD1-ADGANG             PIC Z(1)9.                             
027800     03   FILLER                   PIC X(2)  VALUE SPACE.                 
027900     03   LRAD1-ADPLATS            PIC Z(4)9.                             
028000     03   FILLER                   PIC X(1)  VALUE SPACE.                 
028100     03   LRAD1-IDARTNR            PIC Z(8).                              
028200     03   FILLER                   PIC X(1)  VALUE SPACE.                 
028300     03   LRAD1-BEART              PIC X(25).                             
028400     03   FILLER                   PIC X(1)  VALUE SPACE.                 
028500     03   LRAD1-KVINLART           PIC Z(6).                              
028600     03   FILLER                   PIC X(2)  VALUE SPACE.                 
028700     03   LRAD1-KVINLART-VOR       PIC Z(6).                              
028800     03   FILLER                   PIC X(3)  VALUE SPACE.                 
028900     03   LRAD1-KDPRIO             PIC X(1).                              
029000     03   FILLER                   PIC X(2)  VALUE SPACE.                 
029100     03   LRAD1-ADINLOMR-FB        PIC X(4).                              
029200     03   FILLER                   PIC X(1)  VALUE SPACE.                 
029300     03   LRAD1-BEFT               PIC Z(3).                              
029400     03   FILLER                   PIC X(1)  VALUE SPACE.                 
029500     03   LRAD1-IDLEVNR-KOLLI      PIC X(5).                              
029600     03   FILLER                   PIC X(3)  VALUE SPACE.                 
029700     03   LRAD1-IDLOPNRM-IDOKOLLI  PIC Z(9).                              
029800                                                                          
029900 01  LIST-LRAD2.                                                          
030000     03   FILLER                   PIC X(1)  VALUE SPACE.                 
030100     03   FILLER                   PIC X(6)  VALUE 'PB-SEP'.              
030200     03   FILLER                   PIC X(4)  VALUE SPACE.                 
030300     03   LRAD2-KVPB-SEP           PIC Z(5)9.9.                           
030400     03   FILLER                   PIC X(5)  VALUE SPACE.                 
030500     03   FILLER                   PIC X(6)  VALUE 'ANSKNR'.              
030600     03   FILLER                   PIC X(8)  VALUE SPACE.                 
030700     03   LRAD2-IDANSK             PIC Z(2)9.                             
030800     03   FILLER                   PIC X(6)  VALUE SPACE.                 
030900     03   FILLER                   PIC X(10) VALUE 'LAGERPLATS'.          
031000     03   FILLER                   PIC X(3)  VALUE SPACE.                 
031100     03   LRAD2-ADLAGOMR           PIC Z(1)9.                             
031200     03   FILLER                   PIC X(1)  VALUE SPACE.                 
031300     03   LRAD2-ADGANG             PIC Z(1)9.                             
031400     03   FILLER                   PIC X(1)  VALUE SPACE.                 
031500     03   LRAD2-ADPLATS            PIC Z(4)9.                             
031600     03   FILLER                   PIC X(6)  VALUE SPACE.                 
031700     03   FILLER                   PIC X(4)  VALUE 'VIKT'.                
031800     03   FILLER                   PIC X(5)  VALUE SPACE.                 
031900     03   LRAD2-VKART              PIC Z(6)9.                             
032000                                                                          
032100 01  LIST-LRAD3.                                                          
032200     03   FILLER                   PIC X(1)  VALUE SPACE.                 
032300     03   FILLER                   PIC X(7)  VALUE 'PB-SATS'.             
032400     03   FILLER                   PIC X(3)  VALUE SPACE.                 
032500     03   LRAD3-KVPB-SATS          PIC Z(5)9.9.                           
032600     03   FILLER                   PIC X(5)  VALUE SPACE.                 
032700     03   FILLER                   PIC X(8)  VALUE 'Q3-KVANT'.            
032800     03   FILLER                   PIC X(2)  VALUE SPACE.                 
032900     03   LRAD3-KVQPACK-3          PIC Z(6)9.                             
033000                                                                          
033100 01  LIST-LRAD4.                                                          
033200     03   FILLER                   PIC X(1)  VALUE SPACE.                 
033300     03   FILLER                   PIC X(8)  VALUE 'MAXPUNKT'.            
033400     03   FILLER                   PIC X(3)  VALUE SPACE.                 
033500     03   LRAD4-KVMP               PIC Z(6)9.                             
033600     03   FILLER                   PIC X(5)  VALUE SPACE.                 
033700     03   FILLER                   PIC X(9)  VALUE 'ERSÄTTER '.           
033800     03   LRAD4-ERS-IDARTNR        PIC Z(7)9.                             
033900     03   FILLER                   PIC X(6)  VALUE SPACE.                 
034000     03   FILLER                   PIC X(9)  VALUE 'EMBALLAGE'.           
034100     03   FILLER                   PIC X(7)  VALUE SPACE.                 
034110     03   LRAD4-KDLAGEMB-GRP.                                             
034120       05 LRAD4-KDLAGEMB-SPACE     PIC X(4)  VALUE SPACE.                 
034130       05 LRAD4-KDLAGEMB           PIC X(4).                              
034140     03   LRAD4-IDARTNR-EMBQ3 REDEFINES LRAD4-KDLAGEMB-GRP                
034150                                   PIC Z(7)9.                             
034300     03   FILLER                   PIC X(6)  VALUE SPACE.                 
034400     03   FILLER                   PIC X(6)  VALUE 'VOLYM '.              
034500     03   LRAD4-VLARTNTO           PIC Z(7)9.9.                           
034600                                                                          
034700 01  LIST-LRAD5.                                                          
034800     03   FILLER                   PIC X(1)  VALUE SPACE.                 
034900     03   FILLER                   PIC X(12) VALUE                        
035000                                   'FARLIGT GODS'.                        
035100     03   FILLER                   PIC X(5)  VALUE SPACE.                 
035200     03   LRAD5-KDFARLIG           PIC X(1).                              
035300     03   FILLER                   PIC X(5)  VALUE SPACE.                 
035400     03   FILLER                   PIC X(7)  VALUE 'S-MÄRKT'.             
035500     03   FILLER                   PIC X(9)  VALUE SPACE.                 
035600     03   LRAD5-KDUART             PIC X(1).                              
035700                                                                          
035800 01  LIST-LRAD6.                                                          
035900     03   FILLER                   PIC X(77) VALUE SPACE.                 
036000     03   FILLER                   PIC X(4)  VALUE 'VSOP'.                
036100     03   FILLER                   PIC X(9)  VALUE SPACE.                 
036200     03   LRAD6-KDVSOP             PIC X(3).                              
036300                                                                          
036400 LINKAGE SECTION.                                                         
036500                                                                          
036600*01  -COPY W0009   -PRE MSG-                                              
036700     EJECT                                                                
036800*01  -COPY W0009   -PRE ALT-                                              
036900     EJECT                                                                
037000*01  -COPY W0008  -PRE LISB-                                              
037100     05  FILLER                  PIC X.                                   
037200     EJECT                                                                
037300*01  -COPY W0008  -PRE INLE-                                              
037400     05  FILLER                  PIC X.                                   
037500     EJECT                                                                
037600*01  -COPY W0008  -PRE INLA1-                                             
037700     05  FILLER                  PIC X.                                   
037800     EJECT                                                                
037900*01  -COPY W0008  -PRE INLA2-                                             
038000     05  FILLER                  PIC X.                                   
038100     EJECT                                                                
038500*01  -COPY W0008  -PRE ARTC-                                              
038600     05  FILLER                  PIC X.                                   
038700     EJECT                                                                
038800*01  -COPY W0008  -PRE ERSB-                                              
038900     05  FILLER                  PIC X.                                   
039000     EJECT                                                                
039100*01  -COPY W0008  -PRE BENA-                                              
039200     05  FILLER                  PIC X.                                   
039300                                                                          
039400*    PCB'ER FÖR SUBPGM                                                    
039500                                                                          
039600 01 STYR-HANA-PCB                PIC X.                                   
039700                                                                          
039710 01 STYR-PLAA-PCB                PIC X.                                   
039720                                                                          
039800     EJECT                                                                
039900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB LISB-PCB INLE-PCB              
040000                                   INLA1-PCB INLA2-PCB                    
040100                                   ARTC-PCB  ERSB-PCB BENA-PCB            
040200                                   STYR-HANA-PCB                          
040210                                   STYR-PLAA-PCB.                         
040300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB LISB-PCB INLE-PCB              
040400                                   INLA1-PCB INLA2-PCB                    
040500                                   ARTC-PCB  ERSB-PCB BENA-PCB            
040600                                   STYR-HANA-PCB                          
040610                                   STYR-PLAA-PCB.                         
040700                                                                          
040800     PERFORM IMS-GET-MSG                                                  
040900     IF SEGMENT-FINNS                                                     
041000       PERFORM A-INIT                                                     
041100       PERFORM B-SKAPA-PLATSLISTA                                         
041200     END-IF                                                               
041300                                                                          
041400     PERFORM Z-FINIT                                                      
041500     MOVE ZERO TO RETURN-CODE                                             
041600     GOBACK                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 A-INIT SECTION.                                                          
042000                                                                          
042100     IF MSG-DUBBLA-TRANSKODER                                             
042200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I19A01                 
042300     ELSE                                                                 
042400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I19A01                 
042500     END-IF                                                               
042600                                                                          
042700     PERFORM AA-OPEN-PRINTER                                              
042800     .                                                                    
042900     EJECT                                                                
043000 AA-OPEN-PRINTER         SECTION.                                         
043100                                                                          
043200     MOVE MID-IDPRTLST         TO WS-RAPP-PRINTER                         
043210     MOVE +1                   TO INDX                                    
043220     MOVE MID-IDILIST(INDX)    TO WS-RAPP-IDILIST                         
043230                                  W-SPAR-IDILIST                          
043300     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
043400                         PRT-OPEN                                         
043500                         WS-RAPP-PRINTER                                  
043600                         ALT-PCB                                          
043700                         LISB-PCB                                         
043800                         WS-RAPP-LISTID                                   
043900                         WS-DUMMY                                         
044000                         WS-DUMMY                                         
044100     .                                                                    
044200     EJECT                                                                
044300 B-SKAPA-PLATSLISTA    SECTION.                                           
044400                                                                          
044500     MOVE +1                   TO INDX                                    
044600     PERFORM UNTIL INDX        >  MID-KVPOST                              
044700         MOVE ZERO             TO W-IDSIDNR                               
044800         PERFORM BA-BEHANDLA-PLATSLISTA                                   
044900         ADD +1                TO INDX                                    
045000     END-PERFORM                                                          
045100     .                                                                    
045200     EJECT                                                                
045300 BA-BEHANDLA-PLATSLISTA SECTION.                                          
045400                                                                          
045500     MOVE 99                    TO W-KVRADER                              
045600     IF MID-IDPGM               =  'W6014100'                             
045700         MOVE LOW-VALUE         TO W-W6D1D1KY-MIN-X                       
045800         MOVE HIGH-VALUE        TO W-W6D1D1KY-MAX-X                       
045900         MOVE MID-IDILIST(INDX) TO W-D1D1KY-IDILIST-MIN                   
046000                                     W-D1D1KY-IDILIST-MAX                 
046100         MOVE 1                 TO W-D1D1KY-IDILIRAD-MIN                  
046200                                     W-D1D1KY-IDILIRAD-MAX                
046300         PERFORM IMS-GU-INLE-INLE01                                       
046400                                                                          
046500         PERFORM UNTIL                                                    
046510                 W-D1D1KY-IDILIRAD-MIN > MID-KVRADER(INDX) OR             
046520                 SEGMENT-SAKNAS                            OR             
046530                 SEGMENT-SLUT                                             
046600             MOVE SEQD-IDLEVNR               TO W-D101KY-IDLEVNR          
046700             MOVE SEQD-IDFS                  TO W-D101KY-IDFS             
046800             MOVE SEQD-TIAVIDAT              TO W-D101KY-TIAVIDAT         
046900             MOVE SEQD-IDRADNR-INL           TO W-IDRADNR-INL             
047000             MOVE SEQD-IDRADNR               TO W-IDRADNR                 
047100                                                                          
047200             PERFORM IMS-GU-INLA1-INLA01                                  
047210             PERFORM IMS-GNP-INLA1-INLA11                                 
047220                                                                          
047230             MOVE ART-IDARTNR        TO W-IDARTNR                         
047300             PERFORM BAA-SKAPA-RAD1                                       
047400             ADD +1                  TO W-D1D1KY-IDILIRAD-MIN             
047500                                        W-D1D1KY-IDILIRAD-MAX             
047600             PERFORM IMS-GN-INLE-INLE01                                   
047700         END-PERFORM                                                      
047800       ELSE                                                               
047900         MOVE MID-IDARTNR(INDX)     TO W-IDARTNR                          
048000         PERFORM BAA-SKAPA-RAD1                                           
048100     END-IF                                                               
048101                                                                          
048102     IF W-IDARTNR NOT = +0                                                
048110       PERFORM S02-ART-UPPG                                               
048300       PERFORM BAB-SKAPA-RAD2-RAD6                                        
048310     END-IF                                                               
048320*    DETTA VAR MIN LILLA FIX                                              
048400     .                                                                    
048500     EJECT                                                                
048600 BAA-SKAPA-RAD1      SECTION.                                             
048700                                                                          
049000     IF MID-IDPGM              = 'W6014100'                               
049200         PERFORM IMS-GNP-INLA1-INLA21                                     
049300                                                                          
049400         MOVE RAD-IDILIRAD     TO LRAD1-IDILIRAD                          
049500         MOVE ART-ADLAGOMR     TO LRAD1-ADLAGOMR                          
049600         MOVE ART-ADGANG       TO LRAD1-ADGANG                            
049700         MOVE ART-ADPLATS      TO LRAD1-ADPLATS                           
049800         MOVE ART-IDARTNR      TO LRAD1-IDARTNR                           
049900         MOVE ART-BEART        TO LRAD1-BEART                             
050000         MOVE RAD-KVINLART     TO LRAD1-KVINLART                          
050100         MOVE ART-BEFT         TO LRAD1-BEFT                              
050200                                                                          
050300         IF RAD-FLPRIO         =  JA                                      
050400             MOVE 'P'          TO LRAD1-KDPRIO                            
050500         END-IF                                                           
050600                                                                          
050700         IF MID-IDOKOLLI(INDX)  >  ZERO                                   
050800             MOVE ART-IDLOPNRM  TO LRAD1-IDLOPNRM-IDOKOLLI                
050900          ELSE                                                            
051000             MOVE RAD-IDLEVNR-KOLLI TO LRAD1-IDLEVNR-KOLLI                
051100             MOVE RAD-IDOKOLLI  TO LRAD1-IDLOPNRM-IDOKOLLI                
051200         END-IF                                                           
051300                                                                          
051400         PERFORM BAAB-KOLLA-OM-VOR                                        
051500                                                                          
051600         PERFORM BAAC-TA-FRAM-ADINLOMR-FB                                 
051700      ELSE                                                                
051800         MOVE SPACE                 TO LIST-LRAD1                         
051900         MOVE MID-IDARTNR(INDX)     TO LRAD1-IDARTNR                      
052000         PERFORM BAAD-TA-FRAM-BEART                                       
052100     END-IF                                                               
052200                                                                          
052300     IF W-KVRADER              >  MAX-KVRADER                             
052400         PERFORM S10-SKAPA-HUVUD                                          
052500         MOVE PRT-AFTER-1      TO PRT-RADSKIP                             
052600         ADD +1                TO W-KVRADER                               
052700      ELSE                                                                
052800         MOVE PRT-AFTER-2      TO PRT-RADSKIP                             
052900         ADD +2                TO W-KVRADER                               
053000     END-IF                                                               
053100                                                                          
053200     MOVE LIST-LRAD1           TO WS-RAPP-RAD                             
053300     PERFORM S01-SKRIV-RAD                                                
053400     .                                                                    
053500     EJECT                                                                
055200 BAAB-KOLLA-OM-VOR   SECTION.                                             
055766                                                                          
055767     MOVE ZERO                 TO W-KVINLART-VOR                          
055768     MOVE RAD-IDLEVNR-KOLLI    TO W-D1CSEQ-IDLEVNR-KOLLI                  
055769                                  W-IDLEVNR-KOLLI                         
055770     MOVE RAD-IDOKOLLI         TO W-D1CSEQ-IDOKOLLI                       
055771                                  W-IDOKOLLI                              
055772                                                                          
055773     IF RAD-FLDIVKLI = JA                                                 
055774        PERFORM IMS-GU-INLA2-INLA11-FIRST                                 
055775        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                   
055776                      DIV-ART-IDLOPNRM = ART-IDLOPNRM                     
055777           PERFORM IMS-GN-INLA2-INLA11                                    
055778        END-PERFORM                                                       
055779        IF SEGMENT-FINNS                                                  
055780           IF DIV-ART-IDLOPNRM = ART-IDLOPNRM                             
055781              PERFORM IMS-GNP-INLA2-INLA21-FIRST                          
055782              PERFORM UNTIL SEGMENT-SAKNAS                                
055783                IF RAD-IDOKOLLI     = MID-IDOKOLLI(INDX)                  
055784                  IF RAD-KDINLSTA   = 'VOR'                               
055785                      COMPUTE W-KVINLART-VOR =                            
055786                              W-KVINLART-VOR + RAD-KVINLART               
055787                  END-IF                                                  
055788                END-IF                                                    
055789                PERFORM IMS-GNP-INLA2-INLA21                              
055790              END-PERFORM                                                 
055791           END-IF                                                         
055792        END-IF                                                            
055793     ELSE                                                                 
055794        PERFORM IMS-GU-INLA2-INLA11-FIRST                                 
055795        IF SEGMENT-FINNS                                                  
055796              PERFORM IMS-GNP-INLA2-INLA21-FIRST                          
055797              PERFORM UNTIL SEGMENT-SAKNAS                                
055798                IF RAD-IDOKOLLI     = MID-IDOKOLLI(INDX)                  
055799                  IF RAD-KDINLSTA   = 'VOR'                               
055800                      COMPUTE W-KVINLART-VOR =                            
055801                              W-KVINLART-VOR + RAD-KVINLART               
055802                  END-IF                                                  
055803                END-IF                                                    
055804                PERFORM IMS-GNP-INLA2-INLA21                              
055805              END-PERFORM                                                 
055806        END-IF                                                            
055807     END-IF                                                               
055808                                                                          
055809     MOVE W-KVINLART-VOR       TO LRAD1-KVINLART-VOR                      
055810     .                                                                    
055811     EJECT                                                                
057000 BAAC-TA-FRAM-ADINLOMR-FB   SECTION.                                      
057100                                                                          
057200     MOVE ART-IDDC             TO STYR-IDDC                               
057210     MOVE ART-IDARTNR          TO STYR-IDARTNR                            
057300     MOVE ART-IDFKNGRP         TO STYR-IDFKNGRP                           
057400     MOVE INL-IDLEVNR          TO STYR-IDLEVNR                            
057500     MOVE ART-BEFT             TO STYR-BEFT                               
057600     CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                      
057700                                       STYR-PLAA-PCB                      
057800     MOVE STYR-ADINLOMR-FB     TO LRAD1-ADINLOMR-FB                       
057900     .                                                                    
058000     EJECT                                                                
058100 BAAD-TA-FRAM-BEART         SECTION.                                      
058200                                                                          
058300     MOVE 'S  '                 TO W-IDSKYLT                              
058400     PERFORM IMS-GU-BENA11                                                
058500     IF SEGMENT-FINNS                                                     
058600         MOVE BENA11-TEXT-BEART TO LRAD1-BEART                            
058700      ELSE                                                                
058800         MOVE SPACE             TO LRAD1-BEART                            
058900     END-IF                                                               
059000     .                                                                    
059100     EJECT                                                                
059200 BAB-SKAPA-RAD2-RAD6 SECTION.                                             
059300                                                                          
060600     IF MID-IDPGM              =  'W6014100'                              
060610         MOVE SPACE            TO LRAD4-KDLAGEMB-SPACE                    
060700         MOVE ART-KDLAGEMB     TO LRAD4-KDLAGEMB                          
060800      ELSE                                                                
060810         MOVE W-IDARTNR-EMBQ3  TO LRAD4-IDARTNR-EMBQ3                     
061000     END-IF                                                               
061100                                                                          
061200     MOVE LOW-VALUE            TO W-WDD7A1KY-MIN-X                        
061300     MOVE HIGH-VALUE           TO W-WDD7A1KY-MAX-X                        
061400     MOVE W-IDARTNR            TO W-D7A1KY-TILLK-IDARTNR-MIN              
061500                                  W-D7A1KY-TILLK-IDARTNR-MAX              
061600                                                                          
061700     PERFORM IMS-GU-ERSB-ERSB01                                           
061800     IF SEGMENT-FINNS                                                     
061900         MOVE ERS-IDARTNR      TO LRAD4-ERS-IDARTNR                       
062000     END-IF                                                               
062100                                                                          
062200     MOVE PRT-AFTER-4          TO PRT-RADSKIP                             
062300     MOVE LIST-LRAD2           TO WS-RAPP-RAD                             
062400     PERFORM S01-SKRIV-RAD                                                
062500                                                                          
062600     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
062700     MOVE LIST-LRAD3           TO WS-RAPP-RAD                             
062800     PERFORM S01-SKRIV-RAD                                                
062900                                                                          
063000     MOVE LIST-LRAD4           TO WS-RAPP-RAD                             
063100     PERFORM S01-SKRIV-RAD                                                
063200                                                                          
063300     MOVE LIST-LRAD5           TO WS-RAPP-RAD                             
063400     PERFORM S01-SKRIV-RAD                                                
063500                                                                          
063600     MOVE LIST-LRAD6           TO WS-RAPP-RAD                             
063700     PERFORM S01-SKRIV-RAD                                                
063800     .                                                                    
063900     EJECT                                                                
064000 Z-FINIT                   SECTION.                                       
064100                                                                          
064200     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
064300                         PRT-CLOSE                                        
064400                         WS-RAPP-PRINTER                                  
064500                         ALT-PCB                                          
064600                         LISB-PCB                                         
064700                         WS-RAPP-LISTID                                   
064800                         WS-DUMMY                                         
064900                         WS-DUMMY                                         
065000     .                                                                    
065100     EJECT                                                                
065200 S01-SKRIV-RAD SECTION.                                                   
065300                                                                          
065400     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
065500                         PRT-WRITE                                        
065600                         WS-RAPP-PRINTER                                  
065700                         ALT-PCB                                          
065800                         LISB-PCB                                         
065900                         WS-RAPP-LISTID                                   
066000                         PRT-RADSKIP                                      
066100                         WS-RAPP-LISTRAD                                  
066200                                                                          
066300     MOVE SPACE                TO WS-RAPP-LISTRAD                         
066400     .                                                                    
066500     EJECT                                                                
066510 S02-ART-UPPG SECTION.                                                    
066520                                                                          
066530     PERFORM IMS-GU-WLARTC11                                              
066555                                                                          
066562     MOVE ARTC-CLAG-IDANSK        TO LRAD2-IDANSK                         
066563                                                                          
066565     MOVE ARTC-CLAG-KVPB-SEP   TO LRAD2-KVPB-SEP                          
066566     MOVE ARTC-CLAG-KVPB-SATS  TO LRAD3-KVPB-SATS                         
066567     MOVE ARTC-CLAG-KVMP       TO LRAD4-KVMP                              
066568                                                                          
066570     MOVE ARTC-CLAG-KVQPACK-3  TO LRAD3-KVQPACK-3                         
066571     MOVE ARTC-CLAG-KDUART     TO LRAD5-KDUART                            
066572     MOVE ARTC-CLAG-VKART      TO LRAD2-VKART                             
066573     MOVE ARTC-CLAG-VLARTNTO   TO LRAD4-VLARTNTO                          
066574     MOVE ARTC-CLAG-KDFARLIG   TO LRAD5-KDFARLIG                          
066575     MOVE ARTC-CLAG-KDVSOP     TO LRAD6-KDVSOP                            
066576     MOVE ARTC-CLAG-IDARTNR-EMBQ3 TO W-IDARTNR-EMBQ3                      
066577     MOVE ARTC-CLAG-BEFT       TO LRAD1-BEFT                              
066578                                                                          
066580     MOVE ARTC-CLAG-ADLAGOMR   TO LRAD2-ADLAGOMR                          
066581     MOVE ARTC-CLAG-ADGANG     TO LRAD2-ADGANG                            
066582     MOVE ARTC-CLAG-ADPLATS    TO LRAD2-ADPLATS                           
066583     .                                                                    
066590     EJECT                                                                
066600                                                                          
066700 S10-SKAPA-HUVUD     SECTION.                                             
066800                                                                          
066810     IF MID-IDILIST(INDX) NOT     =  W-SPAR-IDILIST                       
066820        MOVE MID-IDILIST(INDX)    TO W-SPAR-IDILIST                       
066830                                     WS-RAPP-IDILIST                      
066831        CALL W006PRR1 USING PRT-SPOOL-OVR                                 
066832                            PRT-PURGE                                     
066833                            WS-RAPP-PRINTER                               
066834                            ALT-PCB                                       
066835                            LISB-PCB                                      
066836                            WS-RAPP-LISTID                                
066837                            WS-DUMMY                                      
066838                            WS-DUMMY                                      
066840     END-IF                                                               
066850                                                                          
066900     COMPUTE W-IDSIDNR            =  W-IDSIDNR + 1                        
067000     MOVE W-IDSIDNR               TO HRAD1-IDSIDNR                        
067100     ACCEPT HRAD1-DATUM           FROM DATE                               
067200                                                                          
067300     MOVE PRT-NYSIDA-RAD3         TO PRT-RADSKIP                          
067400     MOVE LIST-HRAD1              TO WS-RAPP-RAD                          
067500     PERFORM S01-SKRIV-RAD                                                
067600                                                                          
067700     MOVE PRT-AFTER-2             TO PRT-RADSKIP                          
067800     MOVE MID-IDILIST(INDX)       TO HRAD2-IDILIST                        
067900     MOVE LIST-HRAD2              TO WS-RAPP-RAD                          
068000     PERFORM S01-SKRIV-RAD                                                
068100                                                                          
068200     IF MID-IDPGM                     =  'W6014100'                       
068300         MOVE MID-ADINLOMR(INDX)      TO HRAD3-ADINLOMR                   
068400         MOVE MID-IDLEVNR-KOLLI(INDX) TO HRAD3-IDLEVNR-KOLLI              
068500         MOVE MID-IDOKOLLI(INDX)      TO HRAD3-IDOKOLLI                   
068600         MOVE MID-IDINLVGN(INDX)      TO HRAD3-IDINLVGN                   
068700         MOVE ART-ADLAGOMR            TO W-ADLAGOMR                       
068800         MOVE 'L'                     TO HRAD3-ADLAGOMR (1:1)             
068900         MOVE W-ADLAGOMR              TO HRAD3-ADLAGOMR (2:2)             
069000      ELSE                                                                
069100         MOVE SPACE                   TO HRAD3-ADINLOMR                   
069200                                         HRAD3-ADLAGOMR                   
069300                                         HRAD3-IDLEVNR-KOLLI              
069400         MOVE ZERO                    TO HRAD3-IDOKOLLI                   
069500                                         HRAD3-IDINLVGN                   
069600     END-IF                                                               
069700                                                                          
069800     MOVE SPACE                       TO HRAD3-ADINLOMR-TORG              
069900     MOVE PRT-AFTER-2                 TO PRT-RADSKIP                      
070000     MOVE LIST-HRAD3                  TO WS-RAPP-RAD                      
070100     PERFORM S01-SKRIV-RAD                                                
070200                                                                          
070300     MOVE PRT-AFTER-3                 TO PRT-RADSKIP                      
070400     MOVE LIST-HRUB4                  TO WS-RAPP-RAD                      
070500     PERFORM S01-SKRIV-RAD                                                
070600     MOVE +11                         TO W-KVRADER                        
070700     .                                                                    
070800     EJECT                                                                
070900* --- IMS SEKTIONER ---                                                   
071000     SKIP3                                                                
071100 IMS-GET-MSG SECTION.                                                     
071200                                                                          
071300     MOVE '  QC' TO GODK-STATUSKODER                                      
071400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
071500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071600     PERFORM IMS-STATUSKONTROLL                                           
071700     .                                                                    
071800     SKIP3                                                                
071900 IMS-GU-INLE-INLE01 SECTION.                                              
072000     STRING 'W6INLE01(W6D1D1KY>=' W-W6D1D1KY-MIN-X                        
072100                    '&W6D1D1KY<=' W-W6D1D1KY-MAX-X ')'                    
072200          DELIMITED BY SIZE INTO SSA1                                     
072300     MOVE '  GE' TO GODK-STATUSKODER                                      
072400     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA1 SSA1                     
072500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
072600     PERFORM IMS-STATUSKONTROLL                                           
072700     .                                                                    
072800     SKIP2                                                                
072900 IMS-GN-INLE-INLE01 SECTION.                                              
073000     STRING 'W6INLE01(W6D1D1KY>=' W-W6D1D1KY-MIN-X                        
073100                    '&W6D1D1KY<=' W-W6D1D1KY-MAX-X ')'                    
073200          DELIMITED BY SIZE INTO SSA1                                     
073300     MOVE '  GE' TO GODK-STATUSKODER                                      
073400     CALL CBLTDLI USING GN INLE-PCB DLI-IO-AREA1 SSA1                     
073500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
073600     PERFORM IMS-STATUSKONTROLL                                           
073700     .                                                                    
073800     SKIP2                                                                
073900 IMS-GU-INLA1-INLA01 SECTION.                                             
074000     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
074100          DELIMITED BY SIZE INTO SSA1                                     
074200     MOVE '    ' TO GODK-STATUSKODER                                      
074300     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA4 SSA1                    
074400     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700     SKIP2                                                                
074800 IMS-GNP-INLA1-INLA11 SECTION.                                            
074900     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
075000          DELIMITED BY SIZE INTO SSA1                                     
075100     MOVE '    ' TO GODK-STATUSKODER                                      
075200     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA2 SSA1                   
075300     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
075400     PERFORM IMS-STATUSKONTROLL                                           
075500     .                                                                    
075600     SKIP2                                                                
075700 IMS-GNP-INLA1-INLA21 SECTION.                                            
075800     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
075900          DELIMITED BY SIZE INTO SSA1                                     
076000     MOVE '    ' TO GODK-STATUSKODER                                      
076100     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA3 SSA1                   
076200     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
076300     PERFORM IMS-STATUSKONTROLL                                           
076400     .                                                                    
076500     EJECT                                                                
076510 IMS-GU-INLA2-INLA11-FIRST  SECTION.                                      
076520     STRING 'W6INLA11*F(W6D1CSEQ =' W-W6D1CSEQ-X ')'                      
076530          DELIMITED BY SIZE INTO SSA1                                     
076540     MOVE '  GE' TO GODK-STATUSKODER                                      
076550     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA3 SSA1                    
076560     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
076570     PERFORM IMS-STATUSKONTROLL                                           
076580     .                                                                    
076590     SKIP3                                                                
076591 IMS-GN-INLA2-INLA11 SECTION.                                             
076592     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
076593          DELIMITED BY SIZE INTO SSA1                                     
076594     MOVE '  GEGB' TO GODK-STATUSKODER                                    
076595     CALL CBLTDLI USING GN INLA2-PCB DLI-IO-AREA3 SSA1                    
076596     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
076597     PERFORM IMS-STATUSKONTROLL                                           
076598     .                                                                    
076599     EJECT                                                                
076600 IMS-GNP-INLA2-INLA21-FIRST SECTION.                                      
076601     MOVE  'W6INLA21*F' TO SSA1                                           
076602     MOVE '  GE' TO GODK-STATUSKODER                                      
076603     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA3 SSA1                   
076604     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
076605     PERFORM IMS-STATUSKONTROLL                                           
076606     .                                                                    
076607     SKIP3                                                                
076608 IMS-GNP-INLA2-INLA21       SECTION.                                      
076609     MOVE  'W6INLA21'   TO SSA1                                           
076610     MOVE '  GE' TO GODK-STATUSKODER                                      
076611     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA3 SSA1                   
076612     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
076613     PERFORM IMS-STATUSKONTROLL                                           
076614     .                                                                    
076615     EJECT                                                                
079700 IMS-GU-WLARTC11 SECTION.                                                 
079800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
079900          DELIMITED BY SIZE INTO SSA1                                     
080000     MOVE 'WLARTC11 ' TO SSA2                                             
080100     MOVE '    ' TO GODK-STATUSKODER                                      
080200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1 SSA2                
080300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
080400     PERFORM IMS-STATUSKONTROLL                                           
080500     .                                                                    
080600     SKIP3                                                                
082400 IMS-GU-ERSB-ERSB01 SECTION.                                              
082500     STRING 'WLERSB01(WDD7A1KY>=' W-WDD7A1KY-MIN-X                        
082600                    '&WDD7A1KY<=' W-WDD7A1KY-MAX-X ')'                    
082700          DELIMITED BY SIZE INTO SSA1                                     
082800     MOVE '  GE' TO GODK-STATUSKODER                                      
082900     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA1 SSA1                     
083000     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
083100     PERFORM IMS-STATUSKONTROLL                                           
083200     .                                                                    
083300     SKIP3                                                                
083400 IMS-GU-BENA11 SECTION.                                                   
083500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
083600          DELIMITED BY SIZE INTO SSA1                                     
083700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
083800          DELIMITED BY SIZE INTO SSA2                                     
083900     MOVE '  GE' TO GODK-STATUSKODER                                      
084000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA1 SSA1 SSA2                
084100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
084200     PERFORM IMS-STATUSKONTROLL                                           
084300     .                                                                    
084400     SKIP3                                                                
084500 IMS-STATUSKONTROLL SECTION.                                              
084600                                                                          
084700     SET STATUS-IX TO 1                                                   
084800     SEARCH GODK-STATUS                                                   
084900       AT END                                                             
085000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
085100         DELIMITED BY SIZE INTO FELTEXT                                   
085200         CALL FELLOG                                                      
085300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
085400         CONTINUE                                                         
085500     END-SEARCH                                                           
085600     .                                                                    
