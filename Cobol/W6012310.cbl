000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6012310.                                                
000300 AUTHOR.         RAHUL JAIN.                                              
000400 DATE-WRITTEN.   JUL 2012                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        ALLMÄN BESKRIVNING:                                              
000900*        PROGRAMMET ÄR EN MPP SOM VISAR ARTIKEL OCH/                      
001000*        ELLER PARTIINFORMATION AV VAD SOM FINNS PÅ                       
001100*        INLEVERANSREGISTRET.                                             
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001400*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001500*        PROGRAMMET LÄSER              WDB6                               
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W6T123                                              
001900*        REQU:        W60123I1                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        RESP:        W60123O1                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700                                                                          
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6012310'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  WS-CP-UNICODE               PIC X(4)    VALUE 'UTF8'.                
004000 77  WS-CP-EBCDIC                PIC X(3)    VALUE '278'.                 
004100                                                                          
004200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004300 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  IX1                         PIC S9(4)  VALUE +0    COMP SYNC.        
004500 77  IX2                         PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-KVRADER1                PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800 77  WS-FLKLAR                   PIC X      VALUE SPACE.                  
004900                                                                          
005000*    --- RÄKNARE                                                          
005100 77  FELRAKN                     PIC S9(2)  VALUE +0    COMP SYNC.        
005200                                                                          
005300***********************************                                       
005400*        ARBETSFÄLT               *                                       
005500***********************************                                       
005600 01  WS-AREA.                                                             
005700                                                                          
005800     03  STATUS-TAB OCCURS 500.                                           
005900         05  STATUS-JA            PIC X(1)  VALUE 'N'.                    
006000                                                                          
006100     03  WS-IDPRT.                                                        
006200         05  WS-IDPRT1            PIC X(2).                               
006300         05  WS-IDPRT2            PIC X(4).                               
006400         05  WS-IDPRT3            PIC X(2).                               
006500                                                                          
006600     03  WS-IDLOPNRM              PIC X(9).                               
006700                                                                          
006800     03  WS-ADGANG                PIC 9(3).                               
006900     03  WS-ADGANG-X REDEFINES WS-ADGANG.                                 
007000         05  FILLER               PIC X(1).                               
007100         05  WS-RED-ADGANG        PIC X(2).                               
007200                                                                          
007300     03  WS-ADLAGOMR              PIC 9(3).                               
007400     03  WS-ADLAGOMR-X REDEFINES WS-ADLAGOMR.                             
007500         05  FILLER               PIC X(1).                               
007600         05  WS-RED-ADLAGOMR      PIC X(2).                               
007700                                                                          
007800     03  WS-TEST-ADGANG             PIC 9(3).                             
007900     03  WS-TEST-ADGANG-X REDEFINES WS-TEST-ADGANG.                       
008000         05  FILLER                 PIC X(1).                             
008100         05  WS-TEST-RED-ADGANG     PIC X(2).                             
008200                                                                          
008300     03  WS-TEST-ADLAGOMR           PIC 9(3).                             
008400     03  WS-TEST-ADLAGOMR-X REDEFINES WS-TEST-ADLAGOMR.                   
008500         05  FILLER                 PIC X(1).                             
008600         05  WS-TEST-RED-ADLAGOMR   PIC X(2).                             
008700                                                                          
008800     03  WS-TEST-ADPLATS            PIC 9(5).                             
008900                                                                          
009000     03  WS-TEST-KDLAGEMB           PIC X(4).                             
009100                                                                          
009200     03  WS-IDRADNR               PIC 9(5).                               
009300     03  WS-IDRADNR-X REDEFINES WS-IDRADNR.                               
009400         05  FILLER               PIC X(2).                               
009500         05  WS-RED-IDRADNR       PIC X(3).                               
009600                                                                          
009700     03  WS-KVINLART              PIC S9(7).                              
009800                                                                          
009900     03  WS-IDLEVNR-NEW           PIC X(5) VALUE SPACE.                   
010000                                                                          
010100     03  WS-SPAR-IDLOPNRM         PIC 9(9) VALUE ZERO.                    
010200     03  WS-SPAR-IDLEVNR          PIC X(5) VALUE SPACE.                   
010300     03  WS-SPAR-PRARTSTD         PIC 9(7)V9(2).                          
010400     03  WS-SPAR-FLFRD            PIC X(1).                               
010500                                                                          
010600     03  WS-IDLOPNRM-CHECK        PIC 9(9).                               
010700     03  WS-BEART                 PIC X(25) VALUE SPACE.                  
010800                                                                          
010900     03  SPAR-TIINLMOT            PIC 9(6).                               
011000     03  SPAR-IDARTNR             PIC 9(9).                               
011100     03  SPAR-IDLOPNRM            PIC 9(9) VALUE ZERO.                    
011200     03  SPAR-VKART               PIC 9(7).                               
011300     03  SPAR-VKART-KG            PIC 9(4)V9(3).                          
011400     03  SPAR-ADLAGOMR            PIC 9(2).                               
011500     03  SPAR-ADGANG              PIC 9(2).                               
011600     03  SPAR-ADPLATS             PIC 9(5).                               
011700     03  SPAR-KDSORT              PIC X(2).                               
011800     03  SPAR-BEART               PIC X(25).                              
011900     03  SPAR-IDDC                PIC X(2).                               
012000     03  SPAR-BEFT                PIC 9(2).                               
012100     03  SPAR-KDINLSTA-NEW        PIC X(3).                               
012200     03  SPAR-KDINLSTA-OLD        PIC X(3).                               
012300     03  SPAR-RAD  OCCURS 14.                                             
012400         05  SPAR-IDLEVNR-KOLLI   PIC X(5)   VALUE SPACE.                 
012500         05  SPAR-IDOKOLLI        PIC 9(9).                               
012600                                                                          
012700     03  W-PTOP1-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
012800     03  W-PTOP2-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
012900     03  W-PTOP3-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
013000                                                                          
013100*----TILL W611FRD                                                         
013200     03  WS-SPAR-ADINLOMR-OLD       PIC X(4) VALUE SPACE.                 
013300     03  WS-SPAR-ADINLOMR-NXT-OLD   PIC X(4) VALUE SPACE.                 
013400                                                                          
013500*                                                                         
013600*    --- ARBETSFÄLT FÖR SWITCHAR                                          
013700                                                                          
013800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013900     88  INDATA-OK                           VALUE 'J'.                   
014000     88  INDATA-FEL                          VALUE 'N'.                   
014100                                                                          
014200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
014300     88  NYCKLAR-OK                          VALUE 'J'.                   
014400     88  NYCKLAR-FEL                         VALUE 'N'.                   
014500                                                                          
014600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
014700     88  ALLT-OK                             VALUE 'J'.                   
014800                                                                          
014900 77  ARTNR-SW                    PIC X       VALUE 'J'.                   
015000     88  ARTNR-OK                            VALUE 'J'.                   
015100                                                                          
015200 77  ET-SW                       PIC X       VALUE 'J'.                   
015300     88  ET-JA                               VALUE 'J'.                   
015400     88  ET-NEJ                              VALUE 'N'.                   
015500                                                                          
015600 77  FL-SW                       PIC X       VALUE 'J'.                   
015700     88  FL-JA                               VALUE 'J'.                   
015800     88  FL-NEJ                              VALUE 'N'.                   
015900                                                                          
016000 77  SLUT-SW                     PIC X       VALUE 'J'.                   
016100     88  SLUT-JA                             VALUE 'J'.                   
016200     88  SLUT-NEJ                            VALUE 'N'.                   
016300                                                                          
016400 77  TRANS91-SW                  PIC X       VALUE 'J'.                   
016500     88  TRANS91-JA                          VALUE 'J'.                   
016600     88  TRANS91-NEJ                         VALUE 'N'.                   
016700                                                                          
016800 77  TRANS94-SW                  PIC X       VALUE 'J'.                   
016900     88  TRANS94-JA                          VALUE 'J'.                   
017000     88  TRANS94-NEJ                         VALUE 'N'.                   
017100                                                                          
017200 77  TRANS95-SW                  PIC X       VALUE 'J'.                   
017300     88  TRANS95-JA                          VALUE 'J'.                   
017400     88  TRANS95-NEJ                         VALUE 'N'.                   
017500                                                                          
017600 77  TRAEFF-SW                   PIC X       VALUE 'J'.                   
017700     88  TRAEFF-JA                           VALUE 'J'.                   
017800     88  TRAEFF-NEJ                          VALUE 'N'.                   
017900                                                                          
018000 77  UPD-SW                      PIC X       VALUE 'J'.                   
018100     88  UPD-OK                              VALUE 'J'.                   
018200     88  UPD-NEJ                             VALUE 'N'.                   
018300                                                                          
018400 77  UPDGJORD-SW                 PIC X       VALUE 'N'.                   
018500     88  UPDGJORD-JA                         VALUE 'J'.                   
018600     88  UPDGJORD-NEJ                        VALUE 'N'.                   
018700                                                                          
018800 77  INPUT-SW                    PIC X       VALUE 'N'.                   
018900     88  INPUT-EXISTS                        VALUE 'J'.                   
019000     88  INPUR-EMPTY                         VALUE 'N'.                   
019100                                                                          
019200 77  PF23-OK                     PIC X       VALUE 'J'.                   
019300                                                                          
019400*----------------------------------------------------------------*        
019500*   NKLTYP1=PARTINR, NKLTYP2=ARTNR, NKLTYP3=ARTNR-LEVNR-FS                
019600*----------------------------------------------------------------*        
019700 77  NKLTYP-SW                  PIC X.                                    
019800     88  NKLTYP1                            VALUE '1'.                    
019900     88  NKLTYP2                            VALUE '2'.                    
020000     88  NKLTYP3                            VALUE '3'.                    
020100                                                                          
020200 01  ALL-SPACE.                                                           
020300     03 FILLER                   PIC X(80)   VALUE SPACE.                 
020400 01  ALL-PLUS.                                                            
020500     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
020600     EJECT                                                                
020700 01  ALL-UTF8-SPACE.                                                      
020800     03 FILLER                   PIC X(50)   VALUE ALL X'20'.             
020900 01  ALL-UTF8-PLUS.                                                       
021000     03 FILLER                   PIC X(50)   VALUE ALL X'2B'.             
021100                                                                          
021200                                                                          
021300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021400 01  GENERELLA-SUBPROGRAM.                                                
021500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021700     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
021800     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
021900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
022000     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
022100     EJECT                                                                
022200                                                                          
022300*01 -COPY WMSGINIT                                                        
022400     SKIP3                                                                
022500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
022600*01 -COPY WMEDAREA                                                        
022700     SKIP3                                                                
022800 01  MESSAGE-CODES.                                                       
022900     03  ERR-KONFLIKT            PIC X(3)    VALUE '020'.                 
023000     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
023100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
023200     03  ERR-UPDATE-NOT-VALID    PIC X(3)    VALUE '007'.                 
023300     03  ERR-SAKN-I-REG          PIC X(3)    VALUE '025'.                 
023400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
023500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
023600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
023700     03  INF-LAST-PAGE           PIC X(3)    VALUE '012'.                 
023800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
023900     03  ERR-CODE-NOT-VALID      PIC X(3)    VALUE '023'.                 
024000     03  ERR-PRINT-SAKN          PIC X(3)    VALUE '347'.                 
024100     03  ERR-DIVKOLLI            PIC X(3)    VALUE '354'.                 
024200     03  ERR-SATS-PRIO-JA        PIC X(3)    VALUE '370'.                 
024300     03  INF-PRESS-PF23          PIC X(3)    VALUE '013'.                 
024400     EJECT                                                                
024500                                                                          
024600*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
024700*    --- COPYTEXTER FÖR W006PRT, W611PMRK, W60191                         
024800*                                                                         
024900*01  -COPY W006PRT                                                        
025000     EJECT                                                                
025100*                                                                         
025200*01  -COPY W611PMRK                                                       
025300     EJECT                                                                
025400                                                                          
025500 01  P-TO-P-T91.                                                          
025600*----TILL W60191                                                          
025700     03  PTOP1-LL              PIC S9(4)   COMP SYNC.                     
025800     03  PTOP1-Z1              PIC X(1)    VALUE LOW-VALUE.               
025900     03  PTOP1-Z2              PIC X(1)    VALUE LOW-VALUE.               
026000     03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W6T191X'.               
026100     03  FILLER                PIC X(1)    VALUE SPACE.                   
026200     03  PTOP1-IDTRANS         PIC X(4)    VALUE '6123'.                  
026300     03  PTOP1-KDMFSFOR        PIC X(1)    VALUE SPACE.                   
026400*    03  MID -COPY W6I19101       -PRE T91-                               
026500     EJECT                                                                
026600*                                                                         
026700 01  P-TO-P-T94.                                                          
026800*----TILL W60194                                                          
026900     03  PTOP2-LL              PIC S9(4)   COMP SYNC.                     
027000     03  PTOP2-Z1              PIC X(1)    VALUE LOW-VALUE.               
027100     03  PTOP2-Z2              PIC X(1)    VALUE LOW-VALUE.               
027200     03  PTOP2-TRANSKOD        PIC X(7)    VALUE 'W6T194X'.               
027300     03  FILLER                PIC X(1)    VALUE SPACE.                   
027400     03  PTOP2-IDTRANS         PIC X(4)    VALUE '6123'.                  
027500     03  PTOP2-KDMFSFOR        PIC X(1)    VALUE SPACE.                   
027600*    03  MID -COPY W6I19401       -PRE T94-                               
027700     EJECT                                                                
027800*                                                                         
027900 01  P-TO-P-T95.                                                          
028000*----TILL W60195                                                          
028100     03  PTOP3-LL              PIC S9(4)   COMP SYNC.                     
028200     03  PTOP3-Z1              PIC X(1)    VALUE LOW-VALUE.               
028300     03  PTOP3-Z2              PIC X(1)    VALUE LOW-VALUE.               
028400     03  PTOP3-TRANSKOD        PIC X(7)    VALUE 'W6T195X'.               
028500     03  FILLER                PIC X(1)    VALUE SPACE.                   
028600     03  PTOP3-IDTRANS         PIC X(4)    VALUE '6123'.                  
028700     03  PTOP3-KDMFSFOR        PIC X(1)    VALUE SPACE.                   
028800*    03  MID -COPY W6I19501       -PRE T95-                               
028900     EJECT                                                                
029000                                                                          
029100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
029200*                                                                         
029300 01  FILLER                      PIC X(16)  VALUE 'MSG/RESP-AREA'.        
029400     SKIP3                                                                
029500*01  -COPY WMSGAREA                                                       
029600     EJECT                                                                
029700                                                                          
029800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
029900     SKIP3                                                                
030000*01  -COPY WMFSAREA                                                       
030100     EJECT                                                                
030200 01  FILLER                      PIC X(16)   VALUE 'DC CODES   '.         
030300     SKIP3                                                                
030400*   -COPY WWDC99                                                          
030500 01  FILLER                      PIC X(16)  VALUE 'WTRAUTF8-AREA'.        
030600*01  -COPY WTRAUTF8                                                       
030700                                                                          
030800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
030900*                                                                         
031000     EJECT                                                                
031100                                                                          
031200*                                                                         
031300*    --- DLI- NYCKLAR TILL IMS-SEKTIONERNA                                
031400*                                                                         
031500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031600     SKIP3                                                                
031700 01  NYCKLAR-TILL-BLAEDDRING.                                             
031800  02 W-MINKEY-W6D101KY.                                                   
031900     03  W-MINKEY-IDLEVNR     PIC   X(5)          VALUE SPACE.            
032000     03  W-MINKEY-IDFS        PIC   X(8)          VALUE SPACE.            
032100     03  W-MINKEY-TIAVIDAT    PIC  S9(7)   COMP-3 VALUE +0.               
032200     03  W-MINKEY-IDRADNR-INL PIC  S9(5)   COMP-3 VALUE +0.               
032300     03  W-MINKEY-IDRADNR     PIC  S9(5)   COMP-3 VALUE +0.               
032400     03  W-MINKEY-IDARTNR     PIC  S9(9)   COMP-3 VALUE +0.               
032500     03  W-MINKEY-IDLOPNRM    PIC  S9(9)   COMP-3 VALUE +0.               
032600     SKIP3                                                                
032700  02 W-W6D101KY-NEXT.                                                     
032800     03  W-IDLEVNR-NEXT       PIC   X(5)          VALUE SPACE.            
032900     03  W-IDFS-NEXT          PIC   X(8)          VALUE SPACE.            
033000     03  W-TIAVIDAT-NEXT      PIC  S9(7)   COMP-3 VALUE +0.               
033100     03  W-IDRADNR-INL-NEXT   PIC  S9(5)   COMP-3 VALUE +0.               
033200     03  W-IDRADNR-NEXT       PIC  S9(5)   COMP-3 VALUE +0.               
033300     03  W-IDARTNR-NEXT       PIC  S9(9)   COMP-3 VALUE +0.               
033400     03  W-IDLOPNRM-NEXT      PIC  S9(9)   COMP-3 VALUE +0.               
033500     SKIP3                                                                
033600 01  NYCKLAR-TILL-DLI.                                                    
033700*--------FYSISK NKL TILL INLA                                             
033800     03  W-W6D101KY-X.                                                    
033900         05  W-IDDC              PIC X(2)     VALUE SPACE.                
034000         05  W-IDLEVNR           PIC X(5)     VALUE SPACE.                
034100         05  W-IDFS              PIC X(8)     VALUE SPACE.                
034200         05  W-TIAVIDAT          PIC S9(7)    COMP-3.                     
034300                                                                          
034400     03  W-IDARTNR-X.                                                     
034500         05  W-IDARTNR          PIC S9(9)   COMP-3.                       
034600                                                                          
034700     03  W-IDSKYLT-X.                                                     
034800         05  W-IDSKYLT          PIC X(3)    VALUE SPACE.                  
034900                                                                          
035000     03  W-IDRADNR-INL-X.                                                 
035100         05  W-IDRADNR-INL      PIC S9(5)   COMP-3.                       
035200                                                                          
035300     03  W-IDRADNR-X.                                                     
035400         05  W-IDRADNR           PIC S9(5)   COMP-3.                      
035500                                                                          
035600*--------FYSISK NKL TILL INLH (EG. INLI)                                  
035700     03  W-W6D1H1KY-X.                                                    
035800         05  WH1-IDARTNR         PIC S9(9)    COMP-3 VALUE ZERO.          
035900         05  WH1-IDDC            PIC X(2)     VALUE SPACE.                
036000         05  WH1-IDLEVNR         PIC X(5)     VALUE SPACE.                
036100         05  WH1-IDFS            PIC X(8)     VALUE SPACE.                
036200         05  WH1-TIAVIDAT        PIC S9(7)    COMP-3.                     
036300         05  WH1-IDRADNR-INL     PIC S9(5)    COMP-3.                     
036400                                                                          
036500     03  W-W6D1H1KY-MIN-X.                                                
036600         05  WH1-MIN-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
036700         05  WH1-MIN-IDDC        PIC X(2)     VALUE SPACE.                
036800         05  WH1-MIN-IDLEVNR     PIC X(5)     VALUE SPACE.                
036900         05  WH1-MIN-IDFS        PIC X(8)     VALUE SPACE.                
037000         05  WH1-MIN-TIAVIDAT    PIC S9(7)    COMP-3.                     
037100         05  WH1-MIN-IDRADNR-INL PIC S9(5)    COMP-3 VALUE ZERO.          
037200                                                                          
037300     03  W-W6D1H1KY-MAX-X.                                                
037400         05  WH1-MAX-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
037500         05  WH1-MAX-IDDC        PIC X(2)     VALUE SPACE.                
037600         05  WH1-MAX-IDLEVNR     PIC X(5)     VALUE SPACE.                
037700         05  WH1-MAX-IDFS        PIC X(8)     VALUE SPACE.                
037800         05  WH1-MAX-TIAVIDAT    PIC S9(7)    COMP-3.                     
037900         05  WH1-MAX-IDRADNR-INL PIC S9(5)    COMP-3 VALUE +99999.        
038000                                                                          
038100     03  W-IDLOPNRM-X.                                                    
038200         05  W-IDLOPNRM         PIC S9(9)   VALUE ZERO COMP-3.            
038300                                                                          
038400*--------SEQ NKL TILL INLB                                                
038500     03  W1-IDLOPNRM-X.                                                   
038600         05  W1-IDLOPNRM         PIC S9(9)   VALUE ZERO COMP-3.           
038700                                                                          
038800*--------FYSISK NKL TILL LOPA                                             
038900     03  WL-W6GX01KEY-X.                                                  
039000         05  WL-IDHTYP           PIC X(4)    VALUE SPACE.                 
039100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
039200                                                                          
039300*--------FYSISK NKL TILL PLAA                                             
039400     03  W-W6GX01KEY-X.                                                   
039500         05  WGX-6005-IDHTYP     PIC X(4)    VALUE '6005'.                
039600         05  WGX-6005-IDDC       PIC X(2)    VALUE SPACE.                 
039700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
039800                                                                          
039900     03  W-W6GX11KEY-X.                                                   
040000         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
040100         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
040200                                                                          
040300*--------FYSISK NKL TILL PLAA                                             
040400     03  W-IDDC-B6-X.                                                     
040500         05 W-IDDC-B6            PIC X(2).                                
040600                                                                          
040700     SKIP2                                                                
040800                                                                          
040900*    --- STATUS-KOD FRÅN IMS                                              
041000 01  STATUS-WS                   PIC XX.                                  
041100     88  SEGMENT-FINNS                       VALUE '  '.                  
041200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
041300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041400     SKIP2                                                                
041500                                                                          
041600 01  GODK-STATUSKODER.                                                    
041700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041800     SKIP3                                                                
041900                                                                          
042000 01  SSA1                        PIC X(90).                               
042100 01  SSA2                        PIC X(64).                               
042200 01  SSA3                        PIC X(64).                               
042300     EJECT                                                                
042400                                                                          
042500*    --- IMS FUNKTIONSKODER                                               
042600*01  -COPY W0003                                                          
042700     EJECT                                                                
042800                                                                          
042900*    ---  DLI INPUT-OUTPUT AREA                                           
043000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
043100     SKIP3                                                                
043200                                                                          
043300 01  DLI-IO-AREA-1.                                                       
043400     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
043500     SKIP3                                                                
043600                                                                          
043700     03  W6INLA01 REDEFINES IO-AREA-1.                                    
043800*        05  -COPY W6D101                                                 
043900     SKIP3                                                                
044000                                                                          
044100     03  W6INLA11 REDEFINES IO-AREA-1.                                    
044200*        05  -COPY W6D111                                                 
044300     SKIP3                                                                
044400                                                                          
044500     03  W6INLA21 REDEFINES IO-AREA-1.                                    
044600*        05  -COPY W6D121                                                 
044700     SKIP3                                                                
044800                                                                          
044900     03  W6INLH11 REDEFINES IO-AREA-1.                                    
045000*        05  -COPY W6D1H1                                                 
045100     SKIP3                                                                
045200                                                                          
045300     03  W6INLB11 REDEFINES IO-AREA-1.                                    
045400*        05  -COPY W6D1B1                                                 
045500     SKIP3                                                                
045600                                                                          
045700 01  DLI-IO-AREA-2.                                                       
045800     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
045900     SKIP3                                                                
046000                                                                          
046100     03  W6LOPA01 REDEFINES IO-AREA-2.                                    
046200*        05  -COPY W6GX01                                                 
046300     SKIP3                                                                
046400                                                                          
046500     03  W6LOPA11 REDEFINES IO-AREA-2.                                    
046600*        05  -COPY W6GX6018                                               
046700                                                                          
046800 01  DLI-IO-AREA-3.                                                       
046900     03  IO-AREA-3               PIC X(150)  VALUE SPACE.                 
047000     SKIP3                                                                
047100                                                                          
047200     03  W6PLAA01 REDEFINES IO-AREA-3.                                    
047300*        05  -COPY W6GX01                                                 
047400     SKIP3                                                                
047500                                                                          
047600     03  W6PLAA11 REDEFINES IO-AREA-3.                                    
047700*        05  -COPY W6GX6006                                               
047800                                                                          
047900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
048000 01   DLI-IO-AREA-B601.                                                   
048100*     03  -COPY WDB601                                                    
048200 01  FILLER               PIC X(16)   VALUE 'WLBENA11 AREA'.              
048300 01  DLI-IO-AREA-BENA11.                                                  
048400*    03  -COPY WDD311  -PRE BENA-                                         
048500                                                                          
048600     EJECT                                                                
048700                                                                          
048800 LINKAGE SECTION.                                                         
048900                                                                          
049000 01  REQU-AREA.                                                           
049100*    03 -COPY WZ01REQU                                                    
049200*    03 -COPY W60123I1                                                    
049300     EJECT                                                                
049400 01  RESP-AREA.                                                           
049500*    03 -COPY WZ01RESP                                                    
049600*    03 -COPY W60123O1                                                    
049700     EJECT                                                                
049800 01  MAX-KVRADER                 PIC S9(4)  COMP.                         
049900*                                                                         
050000*01  -COPY W0009   -PRE ALT1-                                             
050100     EJECT                                                                
050200                                                                          
050300*01  -COPY W0009   -PRE ALT2-                                             
050400     EJECT                                                                
050500                                                                          
050600*01  -COPY W0009   -PRE ALT3-                                             
050700     EJECT                                                                
050800                                                                          
050900*01  -COPY W0008  -PRE USEA-                                              
051000     05  FILLER                  PIC X.                                   
051100     EJECT                                                                
051200                                                                          
051300*01  -COPY W0008  -PRE INLA-                                              
051400     05  FILLER                  PIC X.                                   
051500     EJECT                                                                
051600                                                                          
051700*01  -COPY W0008  -PRE INLB1-                                             
051800     05  FILLER                  PIC X.                                   
051900     EJECT                                                                
052000                                                                          
052100*01  -COPY W0008  -PRE INLB-                                              
052200     05  FILLER                  PIC X.                                   
052300     EJECT                                                                
052400                                                                          
052500*01  -COPY W0008  -PRE INLH1-                                             
052600     05  FILLER                  PIC X.                                   
052700     EJECT                                                                
052800                                                                          
052900*01  -COPY W0008  -PRE LOPA-                                              
053000     05  FILLER                  PIC X.                                   
053100     EJECT                                                                
053200                                                                          
053300*01  -COPY W0008  -PRE PLAA-                                              
053400     05  FILLER                  PIC X.                                   
053500     EJECT                                                                
053600                                                                          
053700*01  -COPY W0008  -PRE INLB-PMRK-                                         
053800     05  FILLER                  PIC X.                                   
053900     EJECT                                                                
054000                                                                          
054100*01  -COPY W0008  -PRE INLC-PMRK-                                         
054200     05  FILLER                  PIC X.                                   
054300     EJECT                                                                
054400                                                                          
054500*01  -COPY W0008  -PRE WDB6-                                              
054600     05  FILLER                  PIC X.                                   
054700     EJECT                                                                
054800                                                                          
054900*01  -COPY W0008  -PRE BENA-                                              
055000     05  FILLER                  PIC X.                                   
055100     EJECT                                                                
055200                                                                          
055300 PROCEDURE DIVISION  USING REQU-AREA                                      
055400                           RESP-AREA                                      
055500                           MAX-KVRADER                                    
055600                           ALT1-PCB                                       
055700                           ALT2-PCB                                       
055800                           ALT3-PCB                                       
055900                           USEA-PCB                                       
056000                           INLA-PCB                                       
056100                           INLB1-PCB                                      
056200                           INLB-PCB                                       
056300                           INLH1-PCB                                      
056400                           LOPA-PCB                                       
056500                           PLAA-PCB                                       
056600                           INLB-PMRK-PCB                                  
056700                           INLC-PMRK-PCB                                  
056800                           WDB6-PCB                                       
056900                           BENA-PCB.                                      
057000                                                                          
057100*----------------------------------------------------------------*        
057200     PERFORM A-INIT                                                       
057300     PERFORM B-KOLLA-NYCKLAR                                              
057400     IF NYCKLAR-OK                                                        
057500        IF REQU-UPDATE OR REQU-UPD-V                                      
057600           PERFORM G-KOLLA-INPUT                                          
057700           IF INDATA-OK                                                   
057800              PERFORM H-UPPDATERA                                         
057900              MOVE MSGI-SPAR-AREA     TO                                  
058000                                   NYCKLAR-TILL-BLAEDDRING                
058100              PERFORM S10-NKL-FOER-BLAEDDRING                             
058200              PERFORM F-LAES-VISA-INFO                                    
058300           END-IF                                                         
058400        ELSE                                                              
058500           IF REQU-FIRST                                                  
058600              PERFORM C-FOERSTA-SIDA                                      
058700           ELSE                                                           
058800              IF REQU-NEXT                                                
058900                 PERFORM D-NAESTA-SIDA                                    
059000              ELSE                                                        
059100                 PERFORM E-SAMMA-SIDA                                     
059200              END-IF                                                      
059300           END-IF                                                         
059400           IF ALLT-OK AND INDATA-OK                                       
059500              PERFORM F-LAES-VISA-INFO                                    
059600           END-IF                                                         
059700        END-IF                                                            
059800     END-IF                                                               
059900     PERFORM S90-BLANKUTF-NUM-FAELT                                       
060000                                                                          
060100     GOBACK                                                               
060200     .                                                                    
060300     EJECT                                                                
060400*----------------------------------------------------------------*        
060500 A-INIT SECTION.                                                          
060600                                                                          
060700     MOVE ALL '+'                   TO RESP-W60123O1                      
060800                                                                          
060900     MOVE REQU-KVRADER              TO RESP-KVRADER                       
061000     MOVE 001                       TO RESP-IDMSGVER                      
061100     MOVE SPACE                     TO RESP-IDMSG-ERROR                   
061200                                       RESP-IDMSG-INFO                    
061300                                       RESP-IDELMT-ERROR                  
061400                                                                          
061500     ADD MAX-KVRADER 1          GIVING MAX-KVRADER1                       
061600                                                                          
061700     MOVE ALL '+' TO MSGI-WMSGINIT                                        
061800     MOVE '001'                  TO MSGI-KDCALL                           
061900     MOVE REQU-IDUSER            TO MSGI-IDUSER                           
062000     MOVE '6123'                 TO MSGI-IDTRANS                          
062100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062200     .                                                                    
062300     EJECT                                                                
062400*----------------------------------------------------------------*        
062500 B-KOLLA-NYCKLAR SECTION.                                                 
062600                                                                          
062700     MOVE NEJ                TO UPDGJORD-SW                               
062800     MOVE JA                 TO NYCKLAR-SW                                
062900     MOVE SPACE              TO NKLTYP-SW                                 
063000                                                                          
063100     INSPECT REQU-IDLOPNRM-KEY REPLACING LEADING SPACE BY ZERO            
063200     INSPECT REQU-IDARTNR-KEY  REPLACING LEADING SPACE BY ZERO            
063300                                                                          
063400     MOVE ALL '+'                TO MSGI-WMSGINIT                         
063500     MOVE '001'                  TO MSGI-KDCALL                           
063600     MOVE REQU-IDUSER            TO MSGI-IDUSER                           
063700     MOVE '6123'                 TO MSGI-IDTRANS                          
063800                                                                          
063900     IF REQU-IDLOPNRM-KEY NUMERIC AND REQU-IDLOPNRM-KEY > +0              
064000        MOVE REQU-IDLOPNRM-KEY   TO MSGI-IDLOPNRM                         
064100     ELSE                                                                 
064200        MOVE +0                  TO MSGI-IDLOPNRM                         
064300     END-IF                                                               
064400                                                                          
064500     IF REQU-IDARTNR-KEY NUMERIC AND REQU-IDARTNR-KEY > +0                
064600        MOVE REQU-IDARTNR-KEY    TO MSGI-IDARTNR                          
064700     ELSE                                                                 
064800        MOVE +0                  TO MSGI-IDARTNR                          
064900     END-IF                                                               
065000                                                                          
065100     MOVE REQU-IDLEVNR-KEY       TO MSGI-IDLEVNR                          
065200     MOVE REQU-IDFS-KEY          TO MSGI-IDFS                             
065300     MOVE REQU-IDLBBET-KEY       TO MSGI-IDLBBET                          
065400     MOVE REQU-IDDC-KEY          TO MSGI-IDDC                             
065500                                    WS-IDDC                               
065600     MOVE REQU-ADINLOMR-PRT      TO MSGI-ADINLOMR-PRT                     
065700                                                                          
065800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
065900                                                                          
066000     IF REQU-IDLOPNRM-KEY = ZERO AND                                      
066100        REQU-IDARTNR-KEY  = ZERO AND                                      
066200        REQU-IDLEVNR-KEY  = SPACE AND                                     
066300        REQU-IDFS-KEY     = SPACE                                         
066400*-FEL - 401                                                               
066500        MOVE NEJ           TO NYCKLAR-SW                                  
066600        MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                            
066700     ELSE                                                                 
066800*----------GAMLA NYCKLAR ELLER PF-HOPP                                    
066900        PERFORM BB-FORMELLA-KONTR                                         
067000        PERFORM BAD-KONTR-IDDC                                            
067100     END-IF                                                               
067200                                                                          
067300     IF MSGI-ADINLOMR-PRT NOT = ALL '+' AND                               
067400        MSGI-ADINLOMR-PRT NOT = SPACE                                     
067500        MOVE MSGI-ADINLOMR-PRT     TO RESP-ADINLOMR-PRT                   
067600        MOVE MFS-ADD-LAES-IN-FAELT TO RESP-ADINLOMR-PRT-ATTR              
067700     END-IF                                                               
067800                                                                          
067900     IF MSGI-IDLBBET    NOT = ALL '+' AND                                 
068000        MSGI-IDLBBET    NOT = SPACE                                       
068100        MOVE MSGI-IDLBBET           TO RESP-IDLBBET-KEY                   
068200     END-IF                                                               
068300                                                                          
068400     EVALUATE REQU-IDSPRAK                                                
068500       WHEN 'ZH'                                                          
068600        MOVE 'RCN'           TO W-IDSKYLT                                 
068700        MOVE WS-CP-UNICODE   TO TRAUTF8-KDCP                              
068800       WHEN 'SV'                                                          
068900        MOVE 'S  '           TO W-IDSKYLT                                 
069000        MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                              
069100       WHEN OTHER                                                         
069200        MOVE 'GB '           TO W-IDSKYLT                                 
069300        MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                              
069400     END-EVALUATE                                                         
069500     .                                                                    
069600     EJECT                                                                
069700*----------------------------------------------------------------*        
069800 BAD-KONTR-IDDC    SECTION.                                               
069900                                                                          
070000     IF REQU-IDDC-KEY = ALL '+'                                           
070100        MOVE MSGI-IDDC      TO W-IDDC                                     
070200     ELSE                                                                 
070300        MOVE REQU-IDDC-KEY  TO W-IDDC                                     
070400     END-IF                                                               
070500                                                                          
070600     MOVE W-IDDC            TO RESP-IDDC-KEY                              
070700                               WGX-6005-IDDC                              
070800                               WH1-IDDC                                   
070900                               WH1-MIN-IDDC                               
071000                               WH1-MAX-IDDC                               
071100                                                                          
071200     MOVE W-IDDC TO W-IDDC-B6                                             
071300     PERFORM IMS-GU-WDB601                                                
071400                                                                          
071500     IF DCS-KDDC = SPACE OR DCS-DDC                                       
071600*-FEL - 401                                                               
071700        MOVE NEJ                   TO NYCKLAR-SW                          
071800        MOVE ERR-WRONG-KEY         TO RESP-IDMSG-ERROR                    
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200*----------------------------------------------------------------*        
072300 BB-FORMELLA-KONTR SECTION.                                               
072400                                                                          
072500*----PARTINR                                                              
072600     IF REQU-IDLOPNRM-KEY > ZERO AND                                      
072700       (REQU-IDARTNR-KEY = ZERO OR                                        
072800        REQU-IDARTNR-KEY > ZERO) AND                                      
072900       (REQU-IDLEVNR-KEY = SPACE OR                                       
073000        REQU-IDLEVNR-KEY = ALL '+') AND                                   
073100       (REQU-IDFS-KEY = SPACE OR                                          
073200        REQU-IDFS-KEY = ALL '+')                                          
073300        PERFORM BBA-KONTR-PARTINR                                         
073400     ELSE                                                                 
073500*-------ARTNR                                                             
073600        IF REQU-IDLOPNRM-KEY = ZERO AND                                   
073700           REQU-IDARTNR-KEY > ZERO AND                                    
073800           REQU-IDLEVNR-KEY = SPACE AND                                   
073900           REQU-IDFS-KEY = SPACE                                          
074000           PERFORM BBB-KONTR-ARTNR                                        
074100        ELSE                                                              
074200*----------ARTNR,LEVNR,FS                                                 
074300           IF (REQU-IDLOPNRM-KEY = ZERO) AND                              
074400              (REQU-IDARTNR-KEY > ZERO OR                                 
074500              REQU-IDLEVNR-KEY NOT = SPACE OR                             
074600              REQU-IDFS-KEY  NOT = SPACE)                                 
074700              PERFORM BBC-KONTR-ART-LEV-FS                                
074800           ELSE                                                           
074900*-FEL - 401                                                               
075000              MOVE NEJ              TO NYCKLAR-SW                         
075100              MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                   
075200                                                                          
075300              IF REQU-IDLOPNRM-KEY = ALL '+'                              
075400                 MOVE SPACE         TO RESP-IDLOPNRM-KEY                  
075500              ELSE                                                        
075600                 MOVE REQU-IDLOPNRM-KEY TO RESP-IDLOPNRM-KEY              
075700              END-IF                                                      
075800              IF REQU-IDARTNR-KEY = ALL '+'                               
075900                 MOVE SPACE         TO RESP-IDARTNR-KEY                   
076000              ELSE                                                        
076100                 MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                
076200              END-IF                                                      
076300              IF REQU-IDLEVNR-KEY = ALL '+'                               
076400                 MOVE SPACE         TO RESP-IDLEVNR-KEY                   
076500              ELSE                                                        
076600                 MOVE REQU-IDLEVNR-KEY TO RESP-IDLEVNR-KEY                
076700              END-IF                                                      
076800              IF REQU-IDFS-KEY = ALL '+'                                  
076900                 MOVE SPACE         TO RESP-IDFS-KEY                      
077000              ELSE                                                        
077100                 MOVE REQU-IDFS-KEY TO RESP-IDFS-KEY                      
077200              END-IF                                                      
077300           END-IF                                                         
077400        END-IF                                                            
077500     END-IF                                                               
077600     .                                                                    
077700     EJECT                                                                
077800*----------------------------------------------------------------*        
077900 BBA-KONTR-PARTINR SECTION.                                               
078000                                                                          
078100     MOVE '1'                      TO NKLTYP-SW                           
078200     IF REQU-IDLOPNRM-KEY > ZERO                                          
078300*-------GML NKL - PARTINR                                                 
078400        MOVE MSGI-IDLOPNRM         TO RESP-IDLOPNRM-KEY                   
078500        MOVE REQU-IDARTNR-KEY      TO RESP-IDARTNR-KEY                    
078600        MOVE MSGI-IDDC             TO RESP-IDDC-KEY                       
078700     END-IF                                                               
078800     .                                                                    
078900     EJECT                                                                
079000*----------------------------------------------------------------*        
079100 BBB-KONTR-ARTNR SECTION.                                                 
079200                                                                          
079300     MOVE '2'                      TO NKLTYP-SW                           
079400     IF REQU-IDARTNR-KEY > ZERO                                           
079500*-------GML NKL - ARTNR                                                   
079600        MOVE MSGI-IDARTNR          TO RESP-IDARTNR-KEY                    
079700        MOVE MSGI-IDDC             TO RESP-IDDC-KEY                       
079800     END-IF                                                               
079900     .                                                                    
080000     EJECT                                                                
080100*----------------------------------------------------------------*        
080200 BBC-KONTR-ART-LEV-FS SECTION.                                            
080300                                                                          
080400     MOVE '3'                     TO NKLTYP-SW                            
080500     MOVE REQU-IDDC-KEY           TO RESP-IDDC-KEY                        
080600     IF REQU-IDARTNR-KEY > ZERO                                           
080700*-------GML NKL - ARTNR                                                   
080800        MOVE MSGI-IDARTNR      TO RESP-IDARTNR-KEY                        
080900     END-IF                                                               
081000                                                                          
081100     IF REQU-IDLEVNR-KEY NOT = SPACE                                      
081200*------GML NKL - LEVNR                                                    
081300        MOVE MSGI-IDLEVNR       TO RESP-IDLEVNR-KEY                       
081400     END-IF                                                               
081500                                                                          
081600     IF REQU-IDFS-KEY NOT = SPACE                                         
081700*------GML NKL - FS                                                       
081800        MOVE MSGI-IDFS          TO RESP-IDFS-KEY                          
081900     END-IF                                                               
082000     .                                                                    
082100     EJECT                                                                
082200 BC-KOLLA-ARTNR-HOPP SECTION.                                             
082300                                                                          
082400     MOVE MSGI-IDARTNR  TO RESP-IDARTNR-KEY                               
082500                                                                          
082600     INSPECT RESP-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO             
082700     IF RESP-IDARTNR-KEY NUMERIC AND RESP-IDARTNR-KEY > ZERO              
082800       MOVE JA TO NYCKLAR-SW                                              
082900       MOVE '2' TO NKLTYP-SW                                              
083000     ELSE                                                                 
083100       MOVE NEJ TO ARTNR-SW                                               
083200     END-IF                                                               
083300     .                                                                    
083400     EJECT                                                                
083500*----------------------------------------------------------------*        
083600 C-FOERSTA-SIDA SECTION.                                                  
083700                                                                          
083800     MOVE INF-FIRST-PAGE     TO RESP-IDMSG-INFO                           
083900                                                                          
084000*----BLANKA/NOLLA UT BLÄDDRINGSNYCKLAR                                    
084100     PERFORM MFS-RENSA-FAELT-UT                                           
084200                                                                          
084300     PERFORM S12-INIT-NKL                                                 
084400                                                                          
084500     MOVE JA                 TO ALLT-SW                                   
084600     .                                                                    
084700     EJECT                                                                
084800*----------------------------------------------------------------*        
084900 D-NAESTA-SIDA SECTION.                                                   
085000                                                                          
085100     MOVE MSGI-SPAR-AREA    TO NYCKLAR-TILL-BLAEDDRING                    
267100     IF W-MINKEY-TIAVIDAT    NOT NUMERIC                                  
267200       MOVE ZERO TO W-MINKEY-TIAVIDAT                                     
267300     END-IF                                                               
267700     IF W-MINKEY-IDRADNR     NOT NUMERIC                                  
267800       MOVE ZERO TO W-MINKEY-IDRADNR                                      
267900     END-IF                                                               
085200     IF NKLTYP1                                                           
085300       IF MSGI-IDTRANS = '6123'                                           
085400        IF W-IDRADNR-NEXT   = ZERO                                        
085500*----------FEL DETTA ÄR SISTA SIDAN                                       
085600           MOVE NEJ                 TO ALLT-SW                            
085700           MOVE INF-LAST-PAGE       TO RESP-IDMSG-INFO                    
085800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
085900           PERFORM MFS-LAES-IN-IGEN                                       
086000        ELSE                                                              
086100           MOVE W-IDRADNR-NEXT      TO W-MINKEY-IDRADNR                   
086200           MOVE W-IDLOPNRM-NEXT     TO W-MINKEY-IDLOPNRM                  
086300           PERFORM S10-NKL-FOER-BLAEDDRING                                
086400           MOVE JA                  TO ALLT-SW                            
086500        END-IF                                                            
086600       ELSE                                                               
086700         MOVE NEJ                   TO ALLT-SW                            
086800         MOVE ERR-KONFLIKT          TO RESP-IDMSG-INFO                    
086900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
087000       END-IF                                                             
087100     END-IF                                                               
087200                                                                          
087300     IF NKLTYP2                                                           
087400       IF MSGI-IDTRANS = '6123' AND                                       
087500          MSGI-IDTRANS-OLD = '6123'                                       
087600        IF W-MINKEY-IDRADNR  = ZERO AND                                   
087700           W-MINKEY-IDLEVNR  = SPACE AND                                  
087800           W-MINKEY-IDFS     = SPACE AND                                  
087900           W-MINKEY-TIAVIDAT = ZERO                                       
088000*----------FEL DETTA ÄR SISTA SIDAN                                       
088100           MOVE NEJ                 TO ALLT-SW                            
088200           MOVE INF-LAST-PAGE       TO RESP-IDMSG-INFO                    
088300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
088400        ELSE                                                              
088500           MOVE W-W6D101KY-NEXT     TO W-MINKEY-W6D101KY                  
088600           PERFORM S10-NKL-FOER-BLAEDDRING                                
088700           MOVE JA                  TO ALLT-SW                            
088800        END-IF                                                            
088900       ELSE                                                               
089000         MOVE NEJ                   TO ALLT-SW                            
089100         MOVE ERR-KONFLIKT          TO RESP-IDMSG-INFO                    
089200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
089300       END-IF                                                             
089400     END-IF                                                               
089500                                                                          
089600     IF NKLTYP3                                                           
089700       IF MSGI-IDTRANS = '6123' AND                                       
087500          MSGI-IDTRANS-OLD = '6123'                                       
089800        IF W-MINKEY-IDRADNR = ZERO AND                                    
089900           W-MINKEY-TIAVIDAT = ZERO                                       
090000*----------FEL DETTA ÄR SISTA SIDAN                                       
090100           MOVE NEJ                 TO ALLT-SW                            
090200           MOVE INF-LAST-PAGE       TO RESP-IDMSG-INFO                    
090300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
090400           PERFORM MFS-LAES-IN-IGEN                                       
090500        ELSE                                                              
090600           MOVE W-W6D101KY-NEXT     TO W-MINKEY-W6D101KY                  
090700           PERFORM S10-NKL-FOER-BLAEDDRING                                
090800           MOVE JA                  TO ALLT-SW                            
090900        END-IF                                                            
091000       ELSE                                                               
091100         MOVE NEJ          TO ALLT-SW                                     
091200         MOVE ERR-KONFLIKT          TO RESP-IDMSG-INFO                    
091300         PERFORM MFS-ROER-EJ-FAELT-UT                                     
091400       END-IF                                                             
091500     END-IF                                                               
091600     .                                                                    
091700     EJECT                                                                
091800*----------------------------------------------------------------*        
091900 E-SAMMA-SIDA SECTION.                                                    
092000                                                                          
092100     MOVE MSGI-SPAR-AREA     TO NYCKLAR-TILL-BLAEDDRING                   
092200*----BLANKA/NOLLA UT BLÄDDRINGSNYCKLAR                                    
092300     PERFORM MFS-RENSA-FAELT-UT                                           
092400     MOVE JA                        TO ALLT-SW                            
092500     MOVE ZERO                      TO IX                                 
092600     ADD 1                          TO IX                                 
092700     PERFORM UNTIL IX > MAX-KVRADER                                       
092800        IF (REQU-KDCMDVAL-INPUT(IX) = ALL '+' OR                          
092900           REQU-KDCMDVAL-INPUT(IX)  = SPACE)                              
093000           CONTINUE                                                       
093100        ELSE                                                              
093200           MOVE NEJ                 TO ALLT-SW                            
093300        END-IF                                                            
093400        ADD 1                       TO IX                                 
093500     END-PERFORM                                                          
093600                                                                          
093700     IF ALLT-OK                                                           
093800        PERFORM S10-NKL-FOER-BLAEDDRING                                   
093900     ELSE                                                                 
094000        MOVE INF-PRESS-PF11         TO RESP-IDMSG-INFO                    
094100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
094200        PERFORM MFS-LAES-IN-IGEN                                          
094300     END-IF                                                               
094400     .                                                                    
094500     EJECT                                                                
094600*----------------------------------------------------------------*        
094700 F-LAES-VISA-INFO SECTION.                                                
094800                                                                          
094900*----LÄS BEROENDE PÅ NKLTYP                                               
095000                                                                          
095100     IF NKLTYP1                                                           
095200        PERFORM FA-BEARB-NKLTYP1                                          
095300     END-IF                                                               
095400                                                                          
095500     IF NKLTYP2                                                           
095600        PERFORM FB-BEARB-NKLTYP2                                          
095700     END-IF                                                               
095800                                                                          
095900     IF NKLTYP3                                                           
096000        PERFORM FC-BEARB-NKLTYP3                                          
096100     END-IF                                                               
096200     .                                                                    
096300     EJECT                                                                
096400*----------------------------------------------------------------*        
096500 FA-BEARB-NKLTYP1 SECTION.                                                
096600                                                                          
096700     PERFORM IMS-GU-INLB-D111                                             
096800                                                                          
096900     IF SEGMENT-SAKNAS                                                    
097000*-FEL - 010                                                               
097100*-------ART SAKNAS PÅ ANGIVET PARTI                                       
097200        PERFORM MFS-RENSA-FAELT-UT                                        
097300        MOVE ERR-SAKN-I-REG        TO RESP-IDMSG-ERROR                    
097400        MOVE W1-IDLOPNRM       TO W-MINKEY-IDLOPNRM                       
097500                                  W-IDLOPNRM-NEXT                         
097600        MOVE NYCKLAR-TILL-BLAEDDRING TO MSGI-SPAR-AREA                    
097700        MOVE '002'             TO MSGI-KDCALL                             
097800        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
097900     ELSE                                                                 
098000        PERFORM S20-FLYTTA-SEGM-11-ART-UPPG                               
098100        MOVE ART-IDARTNR       TO RESP-IDARTNR-KEY                        
098200                                  W-MINKEY-IDARTNR                        
098300        MOVE ART-IDLOPNRM      TO W-MINKEY-IDLOPNRM                       
098400        PERFORM IMS-GNP-INLB-D121                                         
098500                                                                          
098600        IF SEGMENT-FINNS AND SPAR-IDDC = W-IDDC                           
098700           MOVE RAD-IDRADNR    TO W-MINKEY-IDRADNR                        
098800           PERFORM FAA-LAES-RADDATA                                       
098900                                                                          
099000*----------OM UPPDATERING SKETT DVS. MAN KOMMER FRÅN H-SECT               
099100*----------FÅR INTE DEN INFORMATIONSTEXTEN SKRIVAS ÖVER                   
099200           IF SEGMENT-FINNS                                               
099300*-------------FLER    RADER PÅ REG                                        
099400              IF UPDGJORD-NEJ                                             
099500                 MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO             
099600              END-IF                                                      
099700           ELSE                                                           
099800              IF UPDGJORD-NEJ                                             
099900                 MOVE INF-LAST-PAGE   TO RESP-IDMSG-INFO                  
100000              END-IF                                                      
100100           END-IF                                                         
100200        ELSE                                                              
100300*-FEL - 010                                                               
100400*--------- RADER/ART SAKNAS PÅ ANGIVET PARTI                              
100500           MOVE ERR-SAKN-I-REG           TO RESP-IDMSG-ERROR              
100600           PERFORM MFS-RENSA-FAELT-UT                                     
100700        END-IF                                                            
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100*----------------------------------------------------------------*        
101200 FAA-LAES-RADDATA SECTION.                                                
101300                                                                          
101400*----FLYTTA RAD TILL MODRAD(IX), RENSA EV MODRAD(IX), LÄS NÄSTA           
101500     MOVE +1 TO IX                                                        
101600     PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-KVRADER                     
101700        PERFORM S21-FLYTTA-SEGM-21-RAD-UPPG                               
101800        PERFORM IMS-GNP-INLB-D121                                         
101900                                                                          
102000        ADD 1 TO IX                                                       
102100     END-PERFORM                                                          
102200                                                                          
102300     SUBTRACT 1 FROM IX GIVING RESP-KVRADER                               
102400                                                                          
102500     IF SEGMENT-FINNS AND IX > MAX-KVRADER                                
102600       MOVE WS-IDRADNR      TO W-IDRADNR-NEXT                             
102700       MOVE W1-IDLOPNRM     TO W-IDLOPNRM-NEXT                            
102800     ELSE                                                                 
102900       MOVE ZERO            TO W-IDFS-NEXT                                
103000                               W-TIAVIDAT-NEXT                            
103100                               W-IDARTNR-NEXT                             
103200                               W-IDLOPNRM-NEXT                            
103300                               W-IDRADNR-INL-NEXT                         
103400                               W-IDRADNR-NEXT                             
103500       MOVE SPACE           TO W-IDLEVNR-NEXT                             
103600     END-IF                                                               
103700     MOVE NYCKLAR-TILL-BLAEDDRING TO MSGI-SPAR-AREA                       
103800     MOVE '002' TO MSGI-KDCALL                                            
103900     MOVE '6123' TO MSGI-IDTRANS                                          
104000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
104100     .                                                                    
104200     EJECT                                                                
104300*----------------------------------------------------------------*        
104400*----------------------------------------------------------------*        
104500*  - VID LÄSNING MED NYCKELTYP 2 (ARTNR)                                  
104600*    LÄSES INDEX-H-BASEN MED HJÄLP AV                                     
104700*    FYSISK NKL OCH INLH1-PCB                                             
104800*    KVAL  =  IDARTNR                                                     
104900*    OKVAL =  IDLEVNR OCH IDFS SAMT TIAVIDAT > 0                          
105000*  - VID TRÄFF LÄSES SEDAN VIA INLA PCB PÅ VANLIG VÄG                     
105100*    OBS. FLERA FÖREKOMSTER KAN FINNAS PGA ARTNR                          
105200*  - IDLEVNR,IDFS,TIAVIDAT SPARAS FÖR ATT ANVÄNDAS SOM STARTVÄRDEN        
105300*    VID BLÄDDRING                                                        
105400*----------------------------------------------------------------*        
105500 FB-BEARB-NKLTYP2 SECTION.                                                
105600                                                                          
105700     MOVE ZERO                 TO RESP-KVRADER                            
105800*------ LÄSNING MED OKVALIFICERAT                                         
105900*-------(LÄSES > MED WH1-MIN/MAX)                                         
106000     PERFORM IMS-GU-INLH1-D111-X                                          
106100                                                                          
106200     IF SEGMENT-SAKNAS                                                    
106300*-FEL - 010                                                               
106400*------ ART SAKNAS                                                        
106500        PERFORM MFS-RENSA-FAELT-UT                                        
106600        MOVE ERR-SAKN-I-REG        TO RESP-IDMSG-ERROR                    
106700        MOVE WH1-MIN-IDARTNR   TO W-MINKEY-IDARTNR                        
106800                                  W-IDARTNR-NEXT                          
106900        MOVE NYCKLAR-TILL-BLAEDDRING TO MSGI-SPAR-AREA                    
107000        MOVE '002'             TO MSGI-KDCALL                             
107100        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
107200     ELSE                                                                 
107300*-------INITIERA  FÖR DIREKT-LÄSNING PÅ INLA-D111                         
107400        MOVE SEQH-IDARTNR          TO W-IDARTNR                           
107500                                      W-MINKEY-IDARTNR                    
107600        MOVE SEQH-IDLEVNR          TO W-IDLEVNR                           
107700                                      W-MINKEY-IDLEVNR                    
107800                                      WS-SPAR-IDLEVNR                     
107900        MOVE SEQH-IDFS             TO W-IDFS                              
108000                                      W-MINKEY-IDFS                       
108100        MOVE SEQH-TIAVIDAT         TO W-TIAVIDAT                          
108200                                      W-MINKEY-TIAVIDAT                   
108300        MOVE SEQH-IDRADNR-INL      TO W-IDRADNR-INL                       
108400                                      W-MINKEY-IDRADNR-INL                
108500        PERFORM FBA-BEARB-RADDATA                                         
108600     END-IF                                                               
108700     .                                                                    
108800     EJECT                                                                
108900*----------------------------------------------------------------*        
109000 FBA-BEARB-RADDATA SECTION.                                               
109100                                                                          
109200     PERFORM IMS-GU-INLA-D111                                             
109300     IF SEGMENT-FINNS                                                     
109400        PERFORM S20-FLYTTA-SEGM-11-ART-UPPG                               
109500        MOVE ART-FLKLAR TO WS-FLKLAR                                      
109600        PERFORM IMS-GNP-INLA-D121                                         
109700        MOVE RAD-IDRADNR    TO W-MINKEY-IDRADNR                           
109800                                                                          
109900        PERFORM FBAA-LAES-RADDATA                                         
110000        PERFORM FBAB-AVSLUTA                                              
110100     END-IF                                                               
110200     .                                                                    
110300     EJECT                                                                
110400*----------------------------------------------------------------*        
110500 FBAA-LAES-RADDATA SECTION.                                               
110600                                                                          
110700     MOVE NEJ                      TO SLUT-SW                             
110800*----FLYTTA RAD TILL MODRAD(IX), RENSA EV MODRAD(IX), LÄS NÄSTA           
110900     MOVE +1 TO IX                                                        
111000     PERFORM UNTIL IX > MAX-KVRADER1                                      
111100                                                                          
111200        IF INLA-STATUS-CODE = SPACE AND SPAR-IDDC = W-IDDC                
111300           IF IX < MAX-KVRADER1                                           
111400              MOVE NEJ             TO TRAEFF-SW                           
111500              IF WS-FLKLAR = NEJ                                          
111600                PERFORM S21-FLYTTA-SEGM-21-RAD-UPPG                       
111700                ADD +1             TO RESP-KVRADER                        
111800              END-IF                                                      
111900              IF IX = 1                                                   
112000                 MOVE WS-TEST-RED-ADGANG   TO RESP-ADGANG                 
112100                 MOVE WS-TEST-RED-ADLAGOMR TO RESP-ADLAGOMR               
112200                 MOVE WS-TEST-ADPLATS      TO RESP-ADPLATS                
112300                 MOVE WS-TEST-KDLAGEMB     TO RESP-KDLAGEMB               
112400              END-IF                                                      
112500              MOVE RAD-IDRADNR        TO W-IDRADNR                        
112600              PERFORM IMS-GNP-INLA-D121                                   
112700           END-IF                                                         
112800        ELSE                                                              
112900           PERFORM UNTIL TRAEFF-JA OR SLUT-JA                             
113000              PERFORM IMS-GN-INLH1-D111-X                                 
113100              IF INLH1-STATUS-CODE = SPACE                                
113200                 IF IX < MAX-KVRADER1                                     
113300                    MOVE SEQH-IDLEVNR TO WH1-MIN-IDLEVNR                  
113400                                          W-IDLEVNR                       
113500                                          WS-SPAR-IDLEVNR                 
113600                    MOVE SEQH-IDFS  TO WH1-MIN-IDFS                       
113700                                          W-IDFS                          
113800                    MOVE SEQH-TIAVIDAT TO WH1-MIN-TIAVIDAT                
113900                                          W-TIAVIDAT                      
114000                    MOVE SEQH-IDRADNR-INL TO WH1-MIN-IDRADNR-INL          
114100                                             W-IDRADNR-INL                
114200                    PERFORM IMS-GU-INLA-D111                              
114300                    IF INLA-STATUS-CODE = SPACE AND                       
114400                                          ART-IDDC = W-IDDC               
114500                       MOVE ART-FLKLAR TO WS-FLKLAR                       
114600                       MOVE ZERO      TO W-IDRADNR                        
114700                       MOVE ART-IDLOPNRM TO WS-SPAR-IDLOPNRM              
114800                       MOVE ART-ADGANG   TO WS-TEST-ADGANG                
114900                       MOVE ART-ADLAGOMR TO WS-TEST-ADLAGOMR              
115000                       MOVE ART-ADPLATS  TO WS-TEST-ADPLATS               
115100                       MOVE ART-KDLAGEMB TO WS-TEST-KDLAGEMB              
115200                       PERFORM IMS-GNP-INLA-D121                          
115300                       IF INLA-STATUS-CODE = SPACE                        
115400                          MOVE JA   TO TRAEFF-SW                          
115500                       END-IF                                             
115600                    END-IF                                                
115700                 ELSE                                                     
115800*-------------------SKULLE IX=12 AVSLUTAR MAN GENOM ATT LÅTSAS            
115900*-------------------ATT FÅ TRÄFF                                          
116000                    MOVE JA        TO TRAEFF-SW                           
116100                    ADD 1          TO IX                                  
116200                 END-IF                                                   
116300              ELSE                                                        
116400                 MOVE JA           TO SLUT-SW                             
116500              END-IF                                                      
116600           END-PERFORM                                                    
116700           IF SEGMENT-SAKNAS OR SLUT-JA                                   
116800              PERFORM MFS-RENSA-RAD-FAELT-UT                              
116900           END-IF                                                         
117000        END-IF                                                            
117100        IF TRAEFF-NEJ                                                     
117200          IF WS-FLKLAR = NEJ  OR SLUT-JA                                  
117300            ADD 1 TO IX                                                   
117400          END-IF                                                          
117500        END-IF                                                            
117600                                                                          
117700     END-PERFORM                                                          
117800                                                                          
117900     IF IX > MAX-KVRADER1 AND SEGMENT-FINNS                               
118000       MOVE WS-SPAR-IDLOPNRM    TO W-IDLOPNRM-NEXT                        
118100       MOVE W-IDRADNR           TO W-IDRADNR-NEXT                         
118200       MOVE WH1-MIN-IDARTNR     TO W-IDARTNR-NEXT                         
118300       MOVE WH1-MIN-IDLEVNR     TO W-IDLEVNR-NEXT                         
118400       MOVE WH1-MIN-IDFS        TO W-IDFS-NEXT                            
118500       MOVE WH1-MIN-TIAVIDAT    TO W-TIAVIDAT-NEXT                        
118600       MOVE WH1-MIN-IDRADNR-INL TO W-IDRADNR-INL-NEXT                     
118700     ELSE                                                                 
118800       MOVE ZERO                TO W-TIAVIDAT-NEXT                        
118900                                   W-IDRADNR-INL-NEXT                     
119000                                   W-IDARTNR-NEXT                         
119100                                   W-IDLOPNRM-NEXT                        
119200                                   W-IDRADNR-NEXT                         
119300       MOVE SPACE               TO W-IDFS-NEXT                            
119400                                   W-IDLEVNR-NEXT                         
119500     END-IF                                                               
119600                                                                          
119700     MOVE NYCKLAR-TILL-BLAEDDRING TO MSGI-SPAR-AREA                       
119800     MOVE '002' TO MSGI-KDCALL                                            
119900     MOVE '6123' TO MSGI-IDTRANS                                          
120000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
120100     .                                                                    
120200     EJECT                                                                
120300*----------------------------------------------------------------*        
120400 FBAB-AVSLUTA      SECTION.                                               
120500                                                                          
120600*----OM UPPDATERING SKETT DVS. MAN KOMMER FRÅN H-SECT                     
120700*----FÅR INTE DEN INFORMATIONSTEXTEN SKRIVAS ÖVER                         
120800     IF SLUT-JA AND                                                       
120900        INLH1-STATUS-CODE = 'GE'                                          
121000*------ INGA FLER RADER PÅ REG - RENSA NKL:AR                             
121100        IF UPDGJORD-NEJ                                                   
121200           MOVE INF-LAST-PAGE        TO RESP-IDMSG-INFO                   
121300        END-IF                                                            
121400     ELSE                                                                 
121500*------ FLER RADER PÅ REG - SPAR-NEXT                                     
121600        IF UPDGJORD-NEJ                                                   
121700           MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                   
121800        END-IF                                                            
121900     END-IF                                                               
122000     .                                                                    
122100     EJECT                                                                
122200*----------------------------------------------------------------*        
122300*  - VID LÄSNING MED NYCKELTYP 3 (ARTNR,LEVNR,FS)                         
122400*    LÄSES INDEX-H-BASEN KVAL MED HJÄLP AV                                
122500*    FYSISK NKL OCH INLH1-PCB                                             
122600*    IDARTNR, IDLEVNR OCH IDFS SAMT TIAVIDAT > 0                          
122700*  - VID TRÄFF LÄSES SEDAN VIA INLA PCB PÅ VANLIG VÄG                     
122800*    OBS. FLERA FÖREKOMSTER KAN FINNAS PGA TIAVIDAT                       
122900*  - TIAVIDAT SPARAS FÖR ATT ANVÄNDAS SOM STARTVÄRDE VID                  
123000*    BLÄDDRING                                                            
123100*----------------------------------------------------------------*        
123200 FC-BEARB-NKLTYP3 SECTION.                                                
123300                                                                          
123400     IF W-MINKEY-TIAVIDAT > ZERO                                          
123500*-------LÄSNING MED KVALIFICERAT TIAVIDAT (LÄSES = MED WH1-)              
123600*OBSOBSOBS DENNA LÄSNINGEN FUNGERAR NOG EJ                                
123700*OBS                                                                      
123800*OBS                                                                      
123900        PERFORM IMS-GU-INLH1-D111-KVAL                                    
124000        IF SEGMENT-SAKNAS                                                 
124100          PERFORM IMS-GU-INLH1-D111-X                                     
124200        END-IF                                                            
124300     ELSE                                                                 
124400*-------LÄSNING MED OKVALIFICERAT TIAVIDAT                                
124500*-------(LÄSES > MED WH1-MIN/MAX)                                         
124600        PERFORM IMS-GU-INLH1-D111-X                                       
124700     END-IF                                                               
124800                                                                          
124900     IF SEGMENT-SAKNAS                                                    
125000*-FEL - 010                                                               
125100*------ ART SAKNAS                                                        
125200        PERFORM MFS-RENSA-FAELT-UT                                        
125300        MOVE ERR-SAKN-I-REG        TO RESP-IDMSG-ERROR                    
125400        MOVE WH1-MIN-IDARTNR   TO W-MINKEY-IDARTNR                        
125500        MOVE WH1-MIN-IDLEVNR   TO W-MINKEY-IDLEVNR                        
125600        MOVE WH1-MIN-IDFS      TO W-MINKEY-IDFS                           
125700        MOVE NYCKLAR-TILL-BLAEDDRING TO MSGI-SPAR-AREA                    
125800        MOVE '002'             TO MSGI-KDCALL                             
125900        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
126000     ELSE                                                                 
126100*------INITIERA FÖR DIREKT-LÄSNING PÅ INLA-D111                           
126200       MOVE SEQH-IDLEVNR           TO W-IDLEVNR                           
126300                                      W-MINKEY-IDLEVNR                    
126400       MOVE SEQH-IDFS              TO W-IDFS                              
126500                                      W-MINKEY-IDFS                       
126600       MOVE SEQH-IDARTNR           TO W-IDARTNR                           
126700                                      W-MINKEY-IDARTNR                    
126800       MOVE SEQH-TIAVIDAT          TO W-TIAVIDAT                          
126900                                      W-MINKEY-TIAVIDAT                   
127000                                      WH1-MIN-TIAVIDAT                    
127100       MOVE SEQH-IDRADNR-INL       TO W-IDRADNR-INL                       
127200                                      WH1-MIN-IDRADNR-INL                 
127300                                      W-MINKEY-IDRADNR-INL                
127400       PERFORM FCA-BEARB-RADDATA                                          
127500     END-IF                                                               
127600     .                                                                    
127700     EJECT                                                                
127800*----------------------------------------------------------------*        
127900 FCA-BEARB-RADDATA SECTION.                                               
128000                                                                          
128100     PERFORM IMS-GU-INLA-D111                                             
128200     IF SEGMENT-FINNS                                                     
128300        PERFORM S20-FLYTTA-SEGM-11-ART-UPPG                               
128400        PERFORM IMS-GNP-INLA-D121                                         
128500        MOVE RAD-IDRADNR TO W-MINKEY-IDRADNR                              
128600                                                                          
128700        PERFORM FCAA-LAES-RADDATA                                         
128800        PERFORM FCAB-AVSLUTA                                              
128900     END-IF                                                               
129000     .                                                                    
129100     EJECT                                                                
129200*----------------------------------------------------------------*        
129300 FCAA-LAES-RADDATA SECTION.                                               
129400                                                                          
129500     MOVE NEJ                      TO SLUT-SW                             
129600                                      TRAEFF-SW                           
129700*----FLYTTA RAD TILL MODRAD(IX), RENSA EV MODRAD(IX), LÄS NÄSTA           
129800     MOVE +1 TO IX                                                        
129900     PERFORM UNTIL IX > MAX-KVRADER1                                      
130000                                                                          
130100        IF INLA-STATUS-CODE = SPACE AND SPAR-IDDC = W-IDDC                
130200           IF IX < MAX-KVRADER1                                           
130300              MOVE NEJ             TO TRAEFF-SW                           
130400              PERFORM S21-FLYTTA-SEGM-21-RAD-UPPG                         
130500              MOVE RAD-IDRADNR        TO W-IDRADNR                        
130600              PERFORM IMS-GNP-INLA-D121                                   
130700           END-IF                                                         
130800        ELSE                                                              
130900           PERFORM UNTIL TRAEFF-JA OR SLUT-JA                             
131000              PERFORM IMS-GN-INLH1-D111-X                                 
131100              IF INLH1-STATUS-CODE = SPACE                                
131200                 IF IX < MAX-KVRADER1                                     
131300                    MOVE SEQH-TIAVIDAT TO WH1-MIN-TIAVIDAT                
131400                                          W-TIAVIDAT                      
131500                                          W-MINKEY-TIAVIDAT               
131600                    MOVE SEQH-IDRADNR-INL TO W-IDRADNR-INL                
131700                                          WH1-MIN-IDRADNR-INL             
131800                                          W-MINKEY-IDRADNR-INL            
131900                    MOVE SEQH-IDLEVNR     TO W-MINKEY-IDLEVNR             
132000                    MOVE SEQH-IDFS        TO W-MINKEY-IDFS                
132100                    PERFORM IMS-GU-INLA-D111                              
132200                    IF INLA-STATUS-CODE = SPACE AND                       
132300                                          ART-IDDC = W-IDDC               
132400                       MOVE ZERO         TO W-IDRADNR                     
132500                       MOVE ART-IDLOPNRM TO WS-SPAR-IDLOPNRM              
132600                       PERFORM IMS-GNP-INLA-D121                          
132700                       IF INLA-STATUS-CODE = SPACE                        
132800                          MOVE JA   TO TRAEFF-SW                          
132900                       END-IF                                             
133000                    END-IF                                                
133100                 ELSE                                                     
133200*-------------------SKULLE IX=12 AVSLUTAR MAN GENOM ATT LÅTSAS            
133300*-------------------ATT FÅ TRÄFF                                          
133400                    MOVE JA        TO TRAEFF-SW                           
133500                    ADD 1          TO IX                                  
133600                 END-IF                                                   
133700              ELSE                                                        
133800                 MOVE JA           TO SLUT-SW                             
133900              END-IF                                                      
134000           END-PERFORM                                                    
134100           IF SEGMENT-SAKNAS OR SLUT-JA                                   
134200              PERFORM MFS-RENSA-RAD-FAELT-UT                              
134300           END-IF                                                         
134400        END-IF                                                            
134500        IF TRAEFF-NEJ                                                     
134600           ADD 1 TO IX                                                    
134700        END-IF                                                            
134800                                                                          
134900     END-PERFORM                                                          
135000     IF IX > MAX-KVRADER1 AND SEGMENT-FINNS                               
135100       MOVE WS-SPAR-IDLOPNRM TO W-IDLOPNRM-NEXT                           
135200       MOVE W-IDRADNR        TO W-IDRADNR-NEXT                            
135300*      ARTIKELNUMMER FLYTTAT VID ENTER KEY                                
135400     ELSE                                                                 
135500       MOVE ZERO             TO W-TIAVIDAT-NEXT                           
135600                                W-IDRADNR-INL-NEXT                        
135700                                W-IDARTNR-NEXT                            
135800                                W-IDLOPNRM-NEXT                           
135900                                W-IDRADNR-NEXT                            
136000       MOVE SPACE            TO W-IDFS-NEXT                               
136100                                W-IDLEVNR-NEXT                            
136200     END-IF                                                               
136300                                                                          
136400     MOVE NYCKLAR-TILL-BLAEDDRING TO MSGI-SPAR-AREA                       
136500     MOVE '002' TO MSGI-KDCALL                                            
136600     MOVE '6123' TO MSGI-IDTRANS                                          
136700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
136800     .                                                                    
136900     EJECT                                                                
137000*----------------------------------------------------------------*        
137100 FCAB-AVSLUTA      SECTION.                                               
137200                                                                          
137300*----OM UPPDATERING SKETT DVS. MAN KOMMER FRÅN H-SECT                     
137400*----FÅR INTE DEN INFORMATIONSTEXTEN SKRIVAS ÖVER                         
137500     IF SLUT-JA AND                                                       
137600        INLH1-STATUS-CODE = 'GE'                                          
137700        IF UPDGJORD-NEJ                                                   
137800           MOVE INF-LAST-PAGE        TO RESP-IDMSG-INFO                   
137900        END-IF                                                            
138000     ELSE                                                                 
138100        IF UPDGJORD-NEJ                                                   
138200           MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                   
138300        END-IF                                                            
138400     END-IF                                                               
138500     .                                                                    
138600     EJECT                                                                
138700*----------------------------------------------------------------*        
138800 G-KOLLA-INPUT SECTION.                                                   
138900                                                                          
139000*----OM MER ÄN ETT FEL UPPSTÅR - LÄGGS TEXT PÅ FÖRSTA FELAKTIGA           
139100*----FROM SECTION GA-            FÄLT UT (KOLLAS MED 'FELRAKN')           
139200*----TOM          GAAF-          DÄREMOT HIGHLIGHTAS ALLA FELEN.          
139300                                                                          
139400     MOVE 0   TO FELRAKN                                                  
139500     MOVE JA  TO INDATA-SW                                                
139600                                                                          
139700     PERFORM GB-KOLLA-INPUT                                               
139800     IF INDATA-OK                                                         
139900        MOVE NEJ                    TO INPUT-SW                           
140000        MOVE ZERO                   TO IX                                 
140100        ADD 1                       TO IX                                 
140200        PERFORM UNTIL IX > MAX-KVRADER                                    
140300          IF REQU-KDCMDVAL-INPUT(IX) NOT = ALL '+'                        
140400             MOVE JA                TO INPUT-SW                           
140500          END-IF                                                          
140600          ADD 1                     TO IX                                 
140700        END-PERFORM                                                       
140800                                                                          
140900        IF INPUT-EXISTS                                                   
141000           PERFORM GA-KOLLA-CMDVAL                                        
141100        END-IF                                                            
141200                                                                          
141300        IF INDATA-FEL                                                     
141400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
141500*--------------------------------------------------------------*          
141600*----DENNA DEL GÖRS FÖR ATT FÅ ORDNING PÅ ÖPPNA/STÄNGDA FÄLT---*          
141700*--------------------------------------------------------------*          
141800           MOVE ZERO TO IX                                                
141900           ADD 1 TO IX                                                    
142000           PERFORM UNTIL IX > MAX-KVRADER                                 
142100              INSPECT REQU-IDRADNR(IX)                                    
142200                      REPLACING LEADING SPACE BY ZERO                     
142300              IF REQU-IDRADNR(IX) > ZERO                                  
142400                 MOVE REQU-IDRADNR(IX) TO RESP-IDRADNR-LINE(IX)           
142500                 MOVE REQU-KDINLSTA(IX) TO RESP-KDINLSTA-LINE(IX)         
142600              END-IF                                                      
142700              ADD 1 TO IX                                                 
142800           END-PERFORM                                                    
142900*--------------------------------------------------------------*          
143000        END-IF                                                            
143100     END-IF                                                               
143200     .                                                                    
143300     EJECT                                                                
143400*----------------------------------------------------------------*        
143500 GA-KOLLA-CMDVAL SECTION.                                                 
143600                                                                          
143700     MOVE NEJ                TO FL-SW                                     
143800                                ET-SW                                     
143900     MOVE JA                 TO PF23-OK                                   
144000     MOVE ZERO TO IX                                                      
144100     ADD  1    TO IX                                                      
144200     PERFORM UNTIL IX > MAX-KVRADER                                       
144300        IF REQU-KDCMDVAL-INPUT(IX) NOT = ALL '+' AND                      
144400           REQU-KDCMDVAL-INPUT(IX) NOT = SPACE AND                        
144500           REQU-KDCMDVAL-INPUT(IX) NOT = LOW-VALUE                        
144600                                                                          
144700** PF23 FÖR ATT UPPDATERA 'SAK' 'MIS'                                     
144800           IF REQU-KDCMDVAL-INPUT(IX) = 'SAK' OR 'MIS'                    
144900             IF REQU-UPD-V OR REQU-IDMSGVER = 001                         
145000               CONTINUE                                                   
145100             ELSE                                                         
145200               MOVE NEJ            TO INDATA-SW                           
145300               MOVE NEJ            TO PF23-OK                             
145400               MOVE INF-PRESS-PF23 TO RESP-IDMSG-INFO                     
145500               MOVE 'PF23'         TO RESP-IDELMT-ERROR                   
145600               MOVE MFS-ALFA-FAELT-FEL                                    
145700                       TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)               
145800             END-IF                                                       
145900           END-IF                                                         
146000                                                                          
146100*----------SEGMENTEN    MÅSTE LÄSAS FÖR ATT KUNNA KOLLAS                  
146200           IF INDATA-OK                                                   
146300             PERFORM GAC-LAES-D111-D121                                   
146400           END-IF                                                         
146500           IF INDATA-OK                                                   
146600             IF (REQU-KDCMDVAL-INPUT(IX) = 'P  ' OR 'UPH' OR              
146700                                       'O  ' OR 'SAK' OR                  
146800                                       'FL ' OR 'ET ' OR                  
146900                                       'CAN' OR 'KIT' OR                  
147000                                       'MIS' OR 'LA ' OR                  
147100                                       'ML ')                             
147200** PF23 FÖR ATT UPPDATERA 'UPH' OM RAD-KDINLSTA = 'SAK'                   
147300               IF RAD-KDINLSTA = 'SAK' OR 'MIS'                           
147400                 IF REQU-KDCMDVAL-INPUT(IX) = 'UPH' OR 'CAN'              
147500                   IF REQU-UPD-V OR REQU-IDMSGVER = 001                   
147600                     CONTINUE                                             
147700                   ELSE                                                   
147800                     MOVE NEJ            TO INDATA-SW                     
147900                     MOVE NEJ            TO PF23-OK                       
148000                     MOVE INF-PRESS-PF23 TO RESP-IDMSG-INFO               
148100                     MOVE 'PF23'         TO RESP-IDELMT-ERROR             
148200                     MOVE MFS-ALFA-FAELT-FEL                              
148300                            TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)          
148400                   END-IF                                                 
148500                 END-IF                                                   
148600               END-IF                                                     
148700                                                                          
148800               IF (RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE)                
148900                  IF (REQU-KDCMDVAL-INPUT(IX) = 'FL ' OR 'LA ')           
149000                     IF (RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK')          
149100                       MOVE JA  TO FL-SW                                  
149200                     ELSE                                                 
149300                        MOVE NEJ                 TO INDATA-SW             
149400                        IF FELRAKN = 0                                    
149500*-FEL - 007                                                               
149600*----------------------FEL-STATUS                                         
149700                          MOVE ERR-UPDATE-NOT-VALID  TO                   
149800                                                  RESP-IDMSG-ERROR        
149900                        END-IF                                            
150000                        ADD 1 TO FELRAKN                                  
150100                        MOVE MFS-ALFA-FAELT-FEL                           
150200                            TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)          
150300                     END-IF                                               
150400                  END-IF                                                  
150500                  IF (REQU-KDCMDVAL-INPUT(IX) = 'ET ' OR 'ML ')           
150600                    IF DCS-CDC OR DCS-CDC-TR                              
150700                       IF (RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK')        
150800                         MOVE JA  TO ET-SW                                
150900                       ELSE                                               
151000                          MOVE NEJ                 TO INDATA-SW           
151100                          IF FELRAKN = 0                                  
151200*-FEL - 007                                                               
151300*----------------------FEL-STATUS                                         
151400                            MOVE ERR-UPDATE-NOT-VALID  TO                 
151500                                                 RESP-IDMSG-ERROR         
151600                          END-IF                                          
151700                          ADD 1 TO FELRAKN                                
151800                          MOVE MFS-ALFA-FAELT-FEL                         
151900                              TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)        
152000                       END-IF                                             
152100                    ELSE                                                  
152200                      MOVE NEJ                 TO INDATA-SW               
152300                      IF FELRAKN = 0                                      
152400*-NDC-ER SKA INTE SKRIVA ETIKETTER                                        
152500*-FEL - 007                                                               
152600*---------------------FEL-STATUS                                          
152700                        MOVE ERR-UPDATE-NOT-VALID  TO                     
152800                                             RESP-IDMSG-ERROR             
152900                      END-IF                                              
153000                      ADD 1 TO FELRAKN                                    
153100                      MOVE MFS-ALFA-FAELT-FEL                             
153200                              TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)        
153300                     END-IF                                               
153400                  END-IF                                                  
153500                  IF INDATA-OK                                            
153600                     PERFORM GAA-KONTR-VAL-UPD-OK                         
153700                     IF INDATA-OK                                         
153800                        MOVE MFS-ALFA-FAELT-RAETT                         
153900                              TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)        
154000                     END-IF                                               
154100                  END-IF                                                  
154200               ELSE                                                       
154300                 MOVE NEJ                  TO INDATA-SW                   
154400*-FEL - 007                                                               
154500*----------------------FEL-STATUS                                         
154600                 MOVE ERR-UPDATE-NOT-VALID TO RESP-IDMSG-ERROR            
154700                 MOVE MFS-ALFA-FAELT-FEL                                  
154800                         TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)             
154900               END-IF                                                     
155000             ELSE                                                         
155100               MOVE NEJ                  TO INDATA-SW                     
155200               MOVE ERR-CODE-NOT-VALID TO RESP-IDMSG-ERROR                
155300*-FEL - 416                                                               
155400              MOVE MFS-ALFA-FAELT-FEL                                     
155500                      TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                
155600             END-IF                                                       
155700           ELSE                                                           
155800              IF PF23-OK = JA                                             
155900                 MOVE NEJ               TO INDATA-SW                      
156000                 IF FELRAKN = 0                                           
156100                    IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE             
156200*-FEL - 416                                                               
156300                       MOVE ERR-CODE-NOT-VALID TO RESP-IDMSG-ERROR        
156400                    ELSE                                                  
156500*-FEL - 007                                                               
156600*-------------------FEL-FKN PGA STATUS                                    
156700                       MOVE ERR-UPDATE-NOT-VALID                          
156800                                               TO RESP-IDMSG-ERROR        
156900                    END-IF                                                
157000                 END-IF                                                   
157100                 ADD 1     TO FELRAKN                                     
157200                 MOVE MFS-ALFA-FAELT-FEL                                  
157300                         TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)             
157400              END-IF                                                      
157500           END-IF                                                         
157600        END-IF                                                            
157700        ADD 1  TO IX                                                      
157800     END-PERFORM                                                          
157900                                                                          
158000     IF FL-JA OR ET-JA                                                    
158100        PERFORM GAB-KOLLA-PRINTER                                         
158200     END-IF                                                               
158300     .                                                                    
158400     EJECT                                                                
158500*----------------------------------------------------------------*        
158600 GAA-KONTR-VAL-UPD-OK SECTION.                                            
158700                                                                          
158800     IF REQU-KDCMDVAL-INPUT(IX) = 'P  '                                   
158900        PERFORM GAAA-KONTR-VAL-P                                          
159000     END-IF                                                               
159100                                                                          
159200     IF REQU-KDCMDVAL-INPUT(IX) = 'O  ' OR 'KIT'                          
159300        PERFORM GAAB-KONTR-VAL-OFR                                        
159400     END-IF                                                               
159500                                                                          
159600     IF REQU-KDCMDVAL-INPUT(IX) = 'FL ' OR 'LA '                          
159700        PERFORM GAAC-KONTR-VAL-FL                                         
159800     END-IF                                                               
159900                                                                          
160000     IF REQU-KDCMDVAL-INPUT(IX) = 'ET ' OR 'ML '                          
160100        PERFORM GAAD-KONTR-VAL-ET                                         
160200     END-IF                                                               
160300     .                                                                    
160400     EJECT                                                                
160500*----------------------------------------------------------------*        
160600 GAAA-KONTR-VAL-P SECTION.                                                
160700                                                                          
160800     IF RAD-IDOKOLLI > ZERO AND                                           
160900        RAD-FLSATS = NEJ AND                                              
161000        (RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE)                          
161100        IF RAD-IDLEVNR-KOLLI NOT = SPACE                                  
161200           CONTINUE                                                       
161300        ELSE                                                              
161400*-FEL - 010                                                               
161500*-----------LEVNR-SAKNAS                                                  
161600           MOVE NEJ                   TO INDATA-SW                        
161700           IF FELRAKN = 0                                                 
161800              MOVE ERR-SAKN-I-REG     TO RESP-IDMSG-ERROR                 
161900           END-IF                                                         
162000           ADD 1                      TO FELRAKN                          
162100           MOVE MFS-ALFA-FAELT-FEL                                        
162200                   TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                   
162300        END-IF                                                            
162400     ELSE                                                                 
162500*-FEL - 007                                                               
162600        MOVE NEJ                      TO INDATA-SW                        
162700        IF FELRAKN = 0                                                    
162800           MOVE ERR-UPDATE-NOT-VALID TO RESP-IDMSG-ERROR                  
162900        END-IF                                                            
163000        ADD 1                         TO FELRAKN                          
163100        MOVE MFS-ALFA-FAELT-FEL                                           
163200                TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                      
163300     END-IF                                                               
163400     .                                                                    
163500     EJECT                                                                
163600*----------------------------------------------------------------*        
163700 GAAB-KONTR-VAL-OFR SECTION.                                              
163800                                                                          
163900     IF RAD-FLPRIO = 'N' AND                                              
164000        RAD-IDOKOLLI > ZERO AND                                           
164100        (RAD-KDINLSTA = SPACE OR 'SAK') AND                               
164200        RAD-FLDIVKLI = NEJ                                                
164300        CONTINUE                                                          
164400     ELSE                                                                 
164500        MOVE NEJ                      TO INDATA-SW                        
164600        IF RAD-FLPRIO = JA                                                
164700*-FEL - 183                                                               
164800           IF FELRAKN = 0                                                 
164900              MOVE ERR-SATS-PRIO-JA   TO RESP-IDMSG-ERROR                 
165000           END-IF                                                         
165100           ADD 1                      TO FELRAKN                          
165200           IF RAD-IDOKOLLI = ZERO                                         
165300*-FEL - 007                                                               
165400              IF FELRAKN = 0                                              
165500                 MOVE ERR-UPDATE-NOT-VALID TO RESP-IDMSG-ERROR            
165600              END-IF                                                      
165700              ADD 1                   TO FELRAKN                          
165800              IF RAD-KDINLSTA NOT = 'SAK' OR SPACE                        
165900*-FEL - 007                                                               
166000                 IF FELRAKN = 0                                           
166100                    MOVE ERR-UPDATE-NOT-VALID TO RESP-IDMSG-ERROR         
166200                 END-IF                                                   
166300                 ADD 1                TO FELRAKN                          
166400                 IF RAD-FLDIVKLI = JA                                     
166500*-FEL - 182                                                               
166600                    IF FELRAKN = 0                                        
166700                       MOVE ERR-DIVKOLLI      TO RESP-IDMSG-ERROR         
166800                    END-IF                                                
166900                    ADD 1             TO FELRAKN                          
167000                 END-IF                                                   
167100               END-IF                                                     
167200           END-IF                                                         
167300        END-IF                                                            
167400        MOVE MFS-ALFA-FAELT-FEL                                           
167500                TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                      
167600     END-IF                                                               
167700     .                                                                    
167800     EJECT                                                                
167900*----------------------------------------------------------------*        
168000 GAAC-KONTR-VAL-FL  SECTION.                                              
168100                                                                          
168200     IF RAD-IDRADNR NOT = 1  AND RAD-FLDIVKLI = NEJ                       
168300        CONTINUE                                                          
168400     ELSE                                                                 
168500        MOVE NEJ                      TO INDATA-SW                        
168600*-FEL - 007                                                               
168700*----------KOLLI FEL                                                      
168800        IF FELRAKN = 0                                                    
168900          IF RAD-FLDIVKLI = JA                                            
169000            MOVE ERR-DIVKOLLI         TO RESP-IDMSG-ERROR                 
169100          ELSE                                                            
169200            MOVE ERR-UPDATE-NOT-VALID TO RESP-IDMSG-ERROR                 
169300          END-IF                                                          
169400        END-IF                                                            
169500        ADD 1                         TO FELRAKN                          
169600        MOVE MFS-ALFA-FAELT-FEL                                           
169700                TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                      
169800     END-IF                                                               
169900     .                                                                    
170000     EJECT                                                                
170100*----------------------------------------------------------------*        
170200 GAAD-KONTR-VAL-ET  SECTION.                                              
170300                                                                          
170400     IF RAD-IDRADNR NOT = 1                                               
170500       IF  RAD-IDOKOLLI NOT = ZERO                                        
170600         IF RAD-FLDIVKLI = JA                                             
170700          CONTINUE                                                        
170800         ELSE                                                             
170900            MOVE NEJ                      TO INDATA-SW                    
171000*-FEL - 007                                                               
171100*----------KOLLI FEL                                                      
171200            IF FELRAKN = 0                                                
171300              MOVE ERR-UPDATE-NOT-VALID TO RESP-IDMSG-ERROR               
171400            END-IF                                                        
171500            ADD 1                         TO FELRAKN                      
171600            MOVE MFS-ALFA-FAELT-FEL                                       
171700                    TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                  
171800         END-IF                                                           
171900       END-IF                                                             
172000     ELSE                                                                 
172100        MOVE NEJ                      TO INDATA-SW                        
172200*-FEL - 007                                                               
172300*----------KOLLI FEL                                                      
172400        IF FELRAKN = 0                                                    
172500          MOVE ERR-UPDATE-NOT-VALID TO RESP-IDMSG-ERROR                   
172600        END-IF                                                            
172700        ADD 1                         TO FELRAKN                          
172800        MOVE MFS-ALFA-FAELT-FEL                                           
172900                TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                      
173000     END-IF                                                               
173100     .                                                                    
173200     EJECT                                                                
173300*----------------------------------------------------------------*        
173400 GAB-KOLLA-PRINTER SECTION.                                               
173500                                                                          
173600*----KOLLA PRINTER - HÄMTA NYTT IDLEVNR-KOLLI FRÅN W6PLAA                 
173700                                                                          
173800     IF REQU-ADINLOMR-PRT NOT = ALL '+' AND                               
173900        REQU-ADINLOMR-PRT NOT = SPACE                                     
174000        PERFORM GABA-KOLLA-PLAC-REG                                       
174100        IF SEGMENT-SAKNAS                                                 
174200*-FEL - 772                                                               
174300*----------PRINTER-PLACERING SAKNAS                                       
174400           MOVE NEJ                    TO INDATA-SW                       
174500           IF FELRAKN = 0                                                 
174600              MOVE ERR-PRINT-SAKN      TO RESP-IDMSG-ERROR                
174700           END-IF                                                         
174800           ADD 1                       TO FELRAKN                         
174900           MOVE MFS-ALFA-FAELT-FEL     TO RESP-ADINLOMR-PRT-ATTR          
175000        ELSE                                                              
175100           MOVE 6006-IDLEVNR           TO WS-IDLEVNR-NEW                  
175200           PERFORM GABB-KOLLA-PRINTER                                     
175300           IF PRT-KDSVAR = 'R'                                            
175400              MOVE PRT-BEPRTLST        TO RESP-BEPRTLST                   
175500              MOVE MFS-ALFA-FAELT-RAETT TO RESP-ADINLOMR-PRT-ATTR         
175600           ELSE                                                           
175700*-FEL - 772                                                               
175800*-------------PRINTER-SAKNAS                                              
175900              MOVE NEJ                 TO INDATA-SW                       
176000              IF FELRAKN = 0                                              
176100                 MOVE ERR-PRINT-SAKN   TO RESP-IDMSG-ERROR                
176200              END-IF                                                      
176300              ADD 1                    TO FELRAKN                         
176400              MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-PRT-ATTR          
176500           END-IF                                                         
176600        END-IF                                                            
176700     ELSE                                                                 
176800*-FEL - 772                                                               
176900*-------PRINTER-PLACERING OBL VID FL                                      
177000        MOVE NEJ                       TO INDATA-SW                       
177100        IF FELRAKN = 0                                                    
177200           MOVE ERR-PRINT-SAKN         TO RESP-IDMSG-ERROR                
177300        END-IF                                                            
177400        ADD 1                          TO FELRAKN                         
177500        MOVE MFS-ALFA-FAELT-FEL        TO RESP-ADINLOMR-PRT-ATTR          
177600     END-IF                                                               
177700     .                                                                    
177800     EJECT                                                                
177900*----------------------------------------------------------------*        
178000 GABA-KOLLA-PLAC-REG SECTION.                                             
178100                                                                          
178200     MOVE REQU-ADINLOMR-PRT  TO W-ADINLOMR                                
178300                                                                          
178400     PERFORM IMS-GU-PLAA-G111                                             
178500     .                                                                    
178600     EJECT                                                                
178700*----------------------------------------------------------------*        
178800 GABB-KOLLA-PRINTER SECTION.                                              
178900                                                                          
179000     IF FL-JA                                                             
179100       MOVE 001                TO PRT-KDCALL                              
179200       MOVE '6F'               TO WS-IDPRT1                               
179300       MOVE REQU-ADINLOMR-PRT  TO WS-IDPRT2                               
179400       MOVE SPACE              TO WS-IDPRT3                               
179500       MOVE WS-IDPRT           TO PRT-IDPRTLST                            
179600       MOVE SPACE              TO PRT-IDLTERM                             
179700     ELSE                                                                 
179800       MOVE 001                TO PRT-KDCALL                              
179900       MOVE '6E'               TO WS-IDPRT1                               
180000       MOVE REQU-ADINLOMR-PRT  TO WS-IDPRT2                               
180100       MOVE SPACE              TO WS-IDPRT3                               
180200       MOVE WS-IDPRT           TO PRT-IDPRTLST                            
180300       MOVE SPACE              TO PRT-IDLTERM                             
180400     END-IF                                                               
180500                                                                          
180600     CALL W006PRT USING PRT-W006PRT                                       
180700     .                                                                    
180800     EJECT                                                                
180900*----------------------------------------------------------------*        
181000 GAC-LAES-D111-D121 SECTION.                                              
181100                                                                          
181200*----LÄSNING FÖR ATT KOLLA RAD SKER ALLTID VIA RADENS                     
181300*    IDLOPNR + RADNR SOM ÄR UNIKT  (INDEX B ANVÄNDS)                      
181400*    GÄLLER OAVSETT NKLTYP                                                
181500                                                                          
181600     MOVE REQU-IDLOPNRM(IX) TO WS-IDLOPNRM                                
181700     INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO                  
181800     IF (WS-IDLOPNRM NOT NUMERIC) OR                                      
181900        (WS-IDLOPNRM NOT > ZERO)                                          
182000       MOVE NEJ TO INDATA-SW                                              
182100       MOVE ERR-UPDATE-NOT-VALID  TO RESP-IDMSG-ERROR                     
182200       ADD 1 TO FELRAKN                                                   
182300       MOVE MFS-ALFA-FAELT-FEL                                            
182400                             TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)         
182500     ELSE                                                                 
182600       MOVE WS-IDLOPNRM          TO W1-IDLOPNRM                           
182700       PERFORM IMS-GU-INLB-D111                                           
182800                                                                          
182900       IF SEGMENT-FINNS                                                   
183000          MOVE ART-PRARTSTD         TO WS-SPAR-PRARTSTD                   
183100          INSPECT REQU-IDRADNR(IX) REPLACING LEADING SPACE BY ZERO        
183200          MOVE REQU-IDRADNR(IX)     TO W-IDRADNR                          
183300          PERFORM IMS-GU-INLB-D121                                        
183400       END-IF                                                             
183500     END-IF                                                               
183600     .                                                                    
183700     EJECT                                                                
183800*----------------------------------------------------------------*        
183900 GB-KOLLA-INPUT  SECTION.                                                 
184000                                                                          
184100     MOVE NEJ                       TO INDATA-SW                          
184200     MOVE ZERO                      TO IX                                 
184300     ADD 1                          TO IX                                 
184400     PERFORM UNTIL IX > MAX-KVRADER                                       
184500        IF (REQU-KDCMDVAL-INPUT(IX) NOT = ALL '+' AND                     
184600            REQU-KDCMDVAL-INPUT(IX) NOT = SPACE  AND                      
184700            REQU-KDCMDVAL-INPUT(IX) NOT = LOW-VALUE)                      
184800           MOVE JA                  TO INDATA-SW                          
184900           MOVE REQU-KDCMDVAL-INPUT(IX)                                   
185000                                  TO RESP-KDCMDVAL-INPUT-LINE(IX)         
185100        END-IF                                                            
185200        ADD 1                       TO IX                                 
185300     END-PERFORM                                                          
185400                                                                          
185500     IF INDATA-FEL                                                        
185600        PERFORM MFS-ROER-EJ-FAELT-UT                                      
185700        PERFORM MFS-LAES-IN-IGEN                                          
185800        MOVE ERR-PF11-AND-NO-DATA   TO RESP-IDMSG-ERROR                   
185900     END-IF                                                               
186000     .                                                                    
186100     EJECT                                                                
186200******************************************************************        
186300*  UPPDATERA REGISTER                                            *        
186400******************************************************************        
186500*----------------------------------------------------------------*        
186600 H-UPPDATERA SECTION.                                                     
186700                                                                          
186800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
186900     MOVE ZERO       TO WS-IDLOPNRM-CHECK                                 
187000     MOVE ZERO       TO T91-MID-KVPOST                                    
187100     MOVE NEJ        TO TRANS91-SW                                        
187200     MOVE ZERO       TO T94-MID-KVPOST                                    
187300     MOVE ZERO       TO T95-MID-KVPOST                                    
187400     MOVE NEJ        TO TRANS94-SW                                        
187500     MOVE NEJ        TO TRANS95-SW                                        
187600     MOVE JA         TO ALLT-SW                                           
187700     MOVE ZERO       TO IX                                                
187800     ADD  1          TO IX                                                
187900     PERFORM UNTIL IX > MAX-KVRADER                                       
188000        MOVE NEJ     TO STATUS-JA(IX)                                     
188100        IF REQU-KDCMDVAL-INPUT(IX) NOT = ALL '+' AND                      
188200           REQU-KDCMDVAL-INPUT(IX) NOT = SPACE AND                        
188300           REQU-KDCMDVAL-INPUT(IX) NOT = LOW-VALUE                        
188400                                                                          
188500     INSPECT REQU-IDLOPNRM(IX) REPLACING LEADING SPACE BY ZERO            
188600     INSPECT REQU-IDRADNR(IX) REPLACING LEADING SPACE BY ZERO             
188700           IF FL-JA  OR ET-JA                                             
188800              PERFORM HH-LAES-INLB1-BAS                                   
188900           END-IF                                                         
189000                                                                          
189100*----------LÄS SEGMENT FÖR UPPDATERING                                    
189200           PERFORM HG-LAES-GHU-D121                                       
189300                                                                          
189400           IF SEGMENT-FINNS                                               
189500              IF REQU-KDCMDVAL-INPUT(IX) = 'P  '                          
189600                 PERFORM HA-UPD-VAL-P                                     
189700              END-IF                                                      
189800              IF REQU-KDCMDVAL-INPUT(IX) = 'UPH' OR 'CAN'                 
189900                 PERFORM HB-UPD-VAL-UPH                                   
190000              END-IF                                                      
190100              IF REQU-KDCMDVAL-INPUT(IX) = 'O  ' OR 'KIT'                 
190200                 PERFORM HC-UPD-VAL-OFR                                   
190300              END-IF                                                      
190400              IF REQU-KDCMDVAL-INPUT(IX) = 'SAK' OR 'MIS'                 
190500                 PERFORM HD-UPD-VAL-SAK                                   
190600              END-IF                                                      
190700              IF REQU-KDCMDVAL-INPUT(IX) = 'FL ' OR 'LA '                 
190800                 PERFORM HE-UPD-VAL-FL                                    
190900              END-IF                                                      
191000              IF REQU-KDCMDVAL-INPUT(IX) = 'ET ' OR 'ML '                 
191100                 PERFORM HF-UPD-VAL-ET                                    
191200              END-IF                                                      
191300           END-IF                                                         
191400        END-IF                                                            
191500        MOVE MFS-RENSA-FAELT      TO RESP-KDCMDVAL-INPUT-LINE(IX)         
191600        MOVE MFS-FORMATETS-ATTR                                           
191700                              TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)        
191800*--------------------------------------------------------------*          
191900*----DENNA DEL GÖRS FÖR ATT FÅ ORDNING PÅ ÖPPNA/STÄNGDA FÄLT---*          
192000*--------------------------------------------------------------*          
192100        INSPECT REQU-IDRADNR(IX) REPLACING LEADING SPACE BY ZERO          
192200        IF REQU-IDRADNR(IX) NUMERIC AND                                   
192300           REQU-IDRADNR(IX) > ZERO                                        
192400           MOVE REQU-IDRADNR(IX)   TO RESP-IDRADNR-LINE(IX)               
192500           MOVE REQU-KDINLSTA(IX)  TO RESP-KDINLSTA-LINE(IX)              
192600        END-IF                                                            
192700*--------------------------------------------------------------*          
192800        ADD 1  TO IX                                                      
192900     END-PERFORM                                                          
193000                                                                          
193100     MOVE JA              TO UPDGJORD-SW                                  
193200     IF TRANS91-JA                                                        
193300*-------SKICKA TRANS TILL BAKGRUNDS-MPP                                   
193400        PERFORM S50-SKICKA-W60191                                         
193500     END-IF                                                               
193600     IF TRANS94-JA                                                        
193700*-------SKICKA TRANS TILL BAKGRUNDS-MPP                                   
193800        PERFORM S51-SKICKA-W60194                                         
193900     END-IF                                                               
194000     IF TRANS95-JA                                                        
194100*-------SKICKA TRANS TILL BAKGRUNDS-MPP                                   
194200        PERFORM S52-SKICKA-W60195                                         
194300     END-IF                                                               
194400     .                                                                    
194500     EJECT                                                                
194600*----------------------------------------------------------------*        
194700 HA-UPD-VAL-P   SECTION.                                                  
194800                                                                          
194900     IF RAD-KDINLSTA = 'SAK'                                              
195000        MOVE SPACE                    TO RAD-KDINLSTA                     
195100                                         REQU-KDINLSTA(IX)                
195200                                         RESP-KDINLSTA-LINE(IX)           
195300                                         SPAR-KDINLSTA-NEW                
195400        MOVE RAD-KDINLSTA             TO SPAR-KDINLSTA-OLD                
195500        MOVE MFS-ADD-LAES-IN-FAELT-HI                                     
195600                                    TO RESP-KDINLSTA-LINE-ATTR(IX)        
195700        MOVE JA      TO STATUS-JA(IX)                                     
195800        PERFORM IMS-REPL-INLB                                             
195900*-------INITIERA TILL TRANS TILL BAKGRUNDS-MPP ' W60191'                  
196000        PERFORM S40-TRANS-W60191                                          
196100     END-IF                                                               
196200                                                                          
196300     IF RAD-FLPRIO = NEJ                                                  
196400        MOVE 'P'                   TO RESP-KDKLIPRI-LINE(IX)              
196500        MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-KDKLIPRI-LINE-ATTR(IX)         
196600                                                                          
196700        INSPECT REQU-IDOKOLLI(IX) REPLACING LEADING SPACE BY ZERO         
196800        MOVE REQU-IDLEVNR(IX)  TO PMRK-IDLEVNR                            
196900        MOVE REQU-IDOKOLLI(IX) TO PMRK-IDOKOLLI                           
197000        MOVE ZERO              TO PMRK-IDLOPNRM                           
197100                                  PMRK-IDRADNR                            
197200                                                                          
197300        CALL W611PMRK USING    PMRK-W611PMRK                              
197400                               INLB-PMRK-PCB                              
197500                               INLC-PMRK-PCB                              
197600        IF PMRK-KDSVAR = SPACE                                            
197700           MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                        
197800        END-IF                                                            
197900     END-IF                                                               
198000     .                                                                    
198100     EJECT                                                                
198200*----------------------------------------------------------------*        
198300 HB-UPD-VAL-UPH SECTION.                                                  
198400                                                                          
198500     MOVE NEJ                         TO UPD-SW                           
198600     IF RAD-KDINLSTA = 'SAK'                                              
198700        MOVE SPACE                    TO RAD-KDINLSTA                     
198800                                         REQU-KDINLSTA(IX)                
198900                                         RESP-KDINLSTA-LINE(IX)           
199000                                         SPAR-KDINLSTA-NEW                
199100        MOVE RAD-KDINLSTA             TO SPAR-KDINLSTA-OLD                
199200        MOVE MFS-ADD-LAES-IN-FAELT-HI                                     
199300                                    TO RESP-KDINLSTA-LINE-ATTR(IX)        
199400        MOVE JA                       TO UPD-SW                           
199500        MOVE JA      TO STATUS-JA(IX)                                     
199600*-------SKICKA TRANS TILL BAKGRUNDS-MPP ' W60191'                         
199700        PERFORM S40-TRANS-W60191                                          
199800     END-IF                                                               
199900                                                                          
200000     IF RAD-FLSATS = JA                                                   
200100        MOVE NEJ                      TO RAD-FLSATS                       
200200        MOVE SPACE                    TO RESP-FLSATS-LINE(IX)             
200300        MOVE MFS-ADD-LYS-UPP-FAELT    TO RESP-FLSATS-LINE-ATTR(IX)        
200400        MOVE JA                       TO UPD-SW                           
200500     END-IF                                                               
200600                                                                          
200700     IF UPD-OK                                                            
200800        PERFORM IMS-REPL-INLB                                             
200900     END-IF                                                               
201000                                                                          
201100     IF RAD-FLPRIO = JA                                                   
201200        MOVE SPACE                 TO RESP-KDKLIPRI-LINE(IX)              
201300        MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-KDKLIPRI-LINE-ATTR(IX)         
201400                                                                          
201500        INSPECT REQU-IDOKOLLI(IX) REPLACING LEADING SPACE BY ZERO         
201600        MOVE REQU-IDLEVNR(IX)  TO PMRK-IDLEVNR                            
201700        MOVE REQU-IDOKOLLI(IX) TO PMRK-IDOKOLLI                           
201800        MOVE ZERO              TO PMRK-IDLOPNRM                           
201900                                  PMRK-IDRADNR                            
202000                                                                          
202100        CALL W611PMRK USING    PMRK-W611PMRK                              
202200                               INLB-PMRK-PCB                              
202300                               INLC-PMRK-PCB                              
202400        IF PMRK-KDSVAR = SPACE                                            
202500           MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                        
202600        END-IF                                                            
202700     END-IF                                                               
202800     .                                                                    
202900     EJECT                                                                
203000*----------------------------------------------------------------*        
203100 HC-UPD-VAL-OFR SECTION.                                                  
203200                                                                          
203300     MOVE JA                          TO RAD-FLSATS                       
203400                                         RESP-FLSATS-LINE(IX)             
203500     MOVE MFS-ADD-LYS-UPP-FAELT       TO RESP-FLSATS-LINE-ATTR(IX)        
203600                                                                          
203700     IF RAD-KDINLSTA = 'SAK'                                              
203800        MOVE SPACE                    TO RAD-KDINLSTA                     
203900                                         REQU-KDINLSTA(IX)                
204000                                         RESP-KDINLSTA-LINE(IX)           
204100                                         SPAR-KDINLSTA-NEW                
204200        MOVE RAD-KDINLSTA             TO SPAR-KDINLSTA-OLD                
204300        MOVE MFS-ADD-LAES-IN-FAELT-HI                                     
204400                                    TO RESP-KDINLSTA-LINE-ATTR(IX)        
204500        MOVE JA      TO STATUS-JA(IX)                                     
204600*-------SKICKA TRANS TILL BAKGRUNDS-MPP ' W60191'                         
204700        PERFORM S40-TRANS-W60191                                          
204800     END-IF                                                               
204900                                                                          
205000     PERFORM IMS-REPL-INLB                                                
205100                                                                          
205200     MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                              
205300     .                                                                    
205400     EJECT                                                                
205500*----------------------------------------------------------------*        
205600 HD-UPD-VAL-SAK SECTION.                                                  
205700                                                                          
205800*UPPDATERA D121-SEGM                                                      
205900     MOVE RAD-ADINLOMR             TO WS-SPAR-ADINLOMR-OLD                
206000     MOVE SPACE                    TO RAD-ADINLOMR                        
206100                                      RESP-ADINLOMR-LINE(IX)              
206200                                                                          
206300     MOVE RAD-ADINLOMR-NXT         TO WS-SPAR-ADINLOMR-NXT-OLD            
206400     MOVE SPACE                    TO RAD-ADINLOMR-NXT                    
206500                                      RESP-ADINLOMR-NXT-LINE(IX)          
206600                                                                          
206700     MOVE ZERO                     TO RAD-IDINLVGN                        
206800                                      RESP-IDINLVGN-LINE(IX)              
206900     IF RAD-FLSATS = JA                                                   
207000        MOVE NEJ                   TO RAD-FLSATS                          
207100        MOVE SPACE                 TO RESP-FLSATS-LINE(IX)                
207200        MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-FLSATS-LINE-ATTR(IX)           
207300     END-IF                                                               
207400                                                                          
207500     MOVE RAD-KDINLSTA             TO SPAR-KDINLSTA-OLD                   
207600     MOVE 'SAK'                    TO RAD-KDINLSTA                        
207700                                      REQU-KDINLSTA(IX)                   
207800                                      RESP-KDINLSTA-LINE(IX)              
207900                                      SPAR-KDINLSTA-NEW                   
208000     IF ENGLISH-TEXT OR REQU-IDMSGVER = 001                               
208100       MOVE 'MIS'                  TO RESP-KDINLSTA-LINE(IX)              
208200     END-IF                                                               
208300     MOVE MFS-ADD-LAES-IN-FAELT-HI TO RESP-KDINLSTA-LINE-ATTR(IX)         
208400     MOVE JA         TO STATUS-JA(IX)                                     
208500                                                                          
208600     IF RAD-FLDIVKLI = JA                                                 
208700        MOVE SPACE                 TO RAD-IDLEVNR-KOLLI                   
208800                                      RESP-IDLEVNR-LINE(IX)               
208900        MOVE ZERO                  TO RAD-IDOKOLLI                        
209000                                      RESP-IDOKOLLI-LINE(IX)              
209100     END-IF                                                               
209200                                                                          
209300     IF RAD-FLPRIO = JA                                                   
209400        MOVE NEJ                   TO RAD-FLPRIO                          
209500        MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-KDKLIPRI-LINE-ATTR(IX)         
209600        MOVE SPACE                 TO RESP-KDKLIPRI-LINE(IX)              
209700     END-IF                                                               
209800                                                                          
209900     PERFORM IMS-REPL-INLB                                                
210000                                                                          
210100*----SKICKA TRANS TILL BAKGRUNDS-MPP ' W60191'                            
210200        PERFORM S40-TRANS-W60191                                          
210300                                                                          
210400     IF RAD-FLPRIO = JA                                                   
210500        INSPECT REQU-IDOKOLLI(IX) REPLACING LEADING SPACE BY ZERO         
210600        MOVE REQU-IDLEVNR(IX)  TO PMRK-IDLEVNR                            
210700        MOVE REQU-IDOKOLLI(IX) TO PMRK-IDOKOLLI                           
210800        MOVE ZERO              TO PMRK-IDLOPNRM                           
210900                                  PMRK-IDRADNR                            
211000        CALL W611PMRK USING    PMRK-W611PMRK                              
211100                               INLB-PMRK-PCB                              
211200                               INLC-PMRK-PCB                              
211300        IF PMRK-KDSVAR = SPACE                                            
211400           MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                        
211500        END-IF                                                            
211600     END-IF                                                               
211700     .                                                                    
211800     EJECT                                                                
211900*----------------------------------------------------------------*        
212000 HE-UPD-VAL-FL  SECTION.                                                  
212100                                                                          
212200     MOVE RAD-ADINLOMR             TO WS-SPAR-ADINLOMR-OLD                
212300     MOVE RAD-ADINLOMR-NXT         TO WS-SPAR-ADINLOMR-NXT-OLD            
212400                                                                          
212500     IF (RAD-IDLEVNR-KOLLI > '99399'                                      
212600     AND RAD-IDLEVNR-KOLLI < '99600')                                     
212700     OR  RAD-IDLEVNR-KOLLI = '3324 '                                      
212800        MOVE RAD-IDLEVNR-KOLLI     TO SPAR-IDLEVNR-KOLLI(IX)              
212900        MOVE RAD-IDOKOLLI          TO SPAR-IDOKOLLI(IX)                   
213000     ELSE                                                                 
213100*-------LÄS/UPD LOPA-REG FÖR ATT HÄMTA KOLLINR                            
213200        PERFORM S30-HAEMTA-IDOKOLLINR                                     
213300        MOVE 6018-IDOKOLLI              TO SPAR-IDOKOLLI(IX)              
213400        MOVE WS-IDLEVNR-NEW             TO SPAR-IDLEVNR-KOLLI(IX)         
213500        MOVE SPAR-IDLEVNR-KOLLI(IX)     TO RESP-IDLEVNR-LINE(IX)          
213600                                           RAD-IDLEVNR-KOLLI              
213700        MOVE SPAR-IDOKOLLI(IX)          TO RESP-IDOKOLLI-LINE(IX)         
213800                                           RAD-IDOKOLLI                   
213900     END-IF                                                               
214000                                                                          
214100     IF RAD-KDINLSTA = 'SAK'                                              
214200        MOVE SPACE                    TO RAD-KDINLSTA                     
214300                                         REQU-KDINLSTA(IX)                
214400                                         RESP-KDINLSTA-LINE(IX)           
214500        MOVE MFS-ADD-LAES-IN-FAELT-HI                                     
214600                                    TO RESP-KDINLSTA-LINE-ATTR(IX)        
214700        MOVE JA      TO STATUS-JA(IX)                                     
214800     END-IF                                                               
214900                                                                          
215000     MOVE NEJ                         TO RAD-FLDIVKLI                     
215100     PERFORM IMS-REPL-INLB                                                
215200                                                                          
215300*----INITIERA TILL BAKGRUNDS-MPP ' W60194'                                
215400     PERFORM S41-TRANS-W60194                                             
215500     MOVE PRT-BEPRTLST          TO RESP-BEPRTLST                          
215600     .                                                                    
215700     EJECT                                                                
215800*----------------------------------------------------------------*        
215900 HF-UPD-VAL-ET    SECTION.                                                
216000                                                                          
216100*----INITIERA TILL BAKGRUNDS-MPP ' W60195'                                
216200     PERFORM S42-TRANS-W60195                                             
216300     MOVE PRT-BEPRTLST          TO RESP-BEPRTLST                          
216400     .                                                                    
216500     EJECT                                                                
216600*----------------------------------------------------------------*        
216700 HG-LAES-GHU-D121 SECTION.                                                
216800                                                                          
216900*-------LÄSNING FÖR ATT KOLLA RAD SKER ALLTID VIA RADENS                  
217000*       IDLOPNR + RADNR SOM ÄR UNIKT  (INDEX B ANVÄNDS)                   
217100*       GÄLLER OAVSETT NKLTYP                                             
217200                                                                          
217300        MOVE REQU-IDLOPNRM(IX)    TO W1-IDLOPNRM                          
217400        MOVE REQU-IDRADNR(IX)     TO W-IDRADNR                            
217500                                                                          
217600        PERFORM IMS-GHU-INLB-D121                                         
217700     .                                                                    
217800     EJECT                                                                
217900*----------------------------------------------------------------*        
218000 HH-LAES-INLB1-BAS SECTION.                                               
218100                                                                          
218200     IF REQU-IDLOPNRM(IX) = WS-IDLOPNRM-CHECK                             
218300*-------FORTSÄTT -- VÄRDEN FRÅN FÖRRA LÄSNING GÄLLER ÄNNU                 
218400        CONTINUE                                                          
218500     ELSE                                                                 
218600*-------NYTT IDLOPNR -- LÄS IN NYA VÄRDEN                                 
218700        MOVE REQU-IDLOPNRM(IX) TO W-IDLOPNRM                              
218800                                                                          
218900        PERFORM IMS-GU-INLB1-D111                                         
219000        IF SEGMENT-FINNS                                                  
219100           PERFORM HHA-LAES-INLA                                          
219200        END-IF                                                            
219300     END-IF                                                               
219400     .                                                                    
219500     EJECT                                                                
219600*----------------------------------------------------------------*        
219700 HHA-LAES-INLA       SECTION.                                             
219800                                                                          
219900     MOVE SEQB-IDLEVNR       TO W-IDLEVNR                                 
220000     MOVE SEQB-IDFS          TO W-IDFS                                    
220100     MOVE SEQB-TIAVIDAT      TO W-TIAVIDAT                                
220200     MOVE SEQB-IDRADNR-INL   TO W-IDRADNR-INL                             
220300     PERFORM IMS-GU-INLA-D101                                             
220400     IF SEGMENT-FINNS                                                     
220500        MOVE INL-TIINLMOT       TO SPAR-TIINLMOT                          
220600        PERFORM IMS-GU-INLA-D111                                          
220700        IF SEGMENT-FINNS                                                  
220800           MOVE ART-IDARTNR     TO SPAR-IDARTNR                           
220900                                   W-IDARTNR                              
221000           MOVE ART-IDLOPNRM    TO SPAR-IDLOPNRM                          
221100           MOVE ART-ADLAGOMR    TO SPAR-ADLAGOMR                          
221200           MOVE ART-ADGANG      TO SPAR-ADGANG                            
221300           MOVE ART-ADPLATS     TO SPAR-ADPLATS                           
221400           MOVE ART-KDSORT      TO SPAR-KDSORT                            
221500           MOVE ART-VKART       TO SPAR-VKART                             
221600           MOVE ART-BEFT        TO SPAR-BEFT                              
221700           MOVE ART-PRARTSTD    TO WS-SPAR-PRARTSTD                       
221800           MOVE ART-IDLOPNRM    TO WS-IDLOPNRM-CHECK                      
                 MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                        
                 IF DCS-UNICODE-IDSKYLT                                         
                    MOVE 'UTF8'             TO TRAUTF8-KDCP                     
                 ELSE                                                           
                    MOVE '278 '             TO TRAUTF8-KDCP                     
                 END-IF                                                         
222000           PERFORM S13-GET-BEART                                          
222100           MOVE WS-BEART     TO SPAR-BEART                                
222500        END-IF                                                            
222600     END-IF                                                               
222700     .                                                                    
222800     EJECT                                                                
222900******************************************************************        
223000*    MFS-REDIGERING AV BILDENS FÄLT                              *        
223100******************************************************************        
223200*----------------------------------------------------------------*        
223300 MFS-RENSA-FAELT-UT SECTION.                                              
223400                                                                          
223500*    --- ALLA UTDATA-FÄLT                                                 
223600*    --- INKL. BLÄDDRINGSNYCKLAR - SPAR-FÄLT                              
223700                                                                          
223800     IF NDC-CN                                                            
223900       MOVE ALL-UTF8-SPACE   TO RESP-BEART                                
224000     END-IF                                                               
224100                                                                          
224200     MOVE ALL-SPACE          TO RESP-KDLAGEMB                             
224300                                RESP-ADLAGOMR                             
224400                                RESP-ADGANG                               
224500                                RESP-ADPLATS                              
224600                                RESP-KDSORT                               
224700                                RESP-KDFARLIG-TXT                         
224800                                                                          
224900*--- RENSA INDEXERADE RADER                                               
225000                                                                          
225100     MOVE +1 TO IX                                                        
225200     PERFORM UNTIL IX > MAX-KVRADER                                       
225300        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
225400        ADD +1 TO IX                                                      
225500     END-PERFORM                                                          
225600     .                                                                    
225700     SKIP2                                                                
225800     EJECT                                                                
225900*----------------------------------------------------------------*        
226000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
226100                                                                          
226200*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
226300     IF IX <= MAX-KVRADER                                                 
226400       MOVE ALL-SPACE        TO RESP-IDLOPNRM-LINE(IX)                    
226500                                RESP-KDCMDVAL-INPUT-LINE(IX)              
226600                                RESP-IDRADNR-LINE(IX)                     
226700                                RESP-IDLEVNR-LINE(IX)                     
226800                                RESP-IDOKOLLI-LINE(IX)                    
226900                                RESP-KVINLART-LINE(IX)                    
227000                                RESP-ADINLOMR-LINE(IX)                    
227100                                RESP-IDINLVGN-LINE(IX)                    
227200                                RESP-ADINLOMR-NXT-LINE(IX)                
227300                                RESP-KDKLIPRI-LINE(IX)                    
227400                                RESP-FLSATS-LINE(IX)                      
227500                                RESP-KDINLSTA-LINE(IX)                    
227600     END-IF                                                               
227700     .                                                                    
227800     EJECT                                                                
227900     SKIP2                                                                
228000*----------------------------------------------------------------*        
228100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
228200                                                                          
228300*    --- ALLA UTDATA-FÄLT                                                 
228400*    --- INKL BLÄDDRINGSNYCKLAR                                           
228500                                                                          
228600     IF NDC-CN                                                            
228700       MOVE ALL-UTF8-PLUS    TO RESP-BEART                                
228800     END-IF                                                               
228900                                                                          
229000     MOVE ALL-PLUS           TO RESP-KDLAGEMB                             
229100                                RESP-ADLAGOMR                             
229200                                RESP-ADGANG                               
229300                                RESP-ADPLATS                              
229400                                RESP-KDSORT                               
229500                                RESP-KDFARLIG-TXT                         
229600                                RESP-ADINLOMR-PRT                         
229700                                                                          
229800                                                                          
229900*--- RÖREJ INDEXERADE RADER                                               
230000                                                                          
230100     MOVE +1 TO IX                                                        
230200     PERFORM UNTIL IX > MAX-KVRADER                                       
230300                                                                          
230400        MOVE ALL-PLUS          TO RESP-IDLOPNRM-LINE(IX)                  
230500*                                 RESP-KDCMDVAL-INPUT-LINE(IX)            
230600                                  RESP-IDRADNR-LINE(IX)                   
230700                                  RESP-IDLEVNR-LINE(IX)                   
230800                                  RESP-IDOKOLLI-LINE(IX)                  
230900                                  RESP-KVINLART-LINE(IX)                  
231000                                  RESP-ADINLOMR-LINE(IX)                  
231100                                  RESP-IDINLVGN-LINE(IX)                  
231200                                  RESP-ADINLOMR-NXT-LINE(IX)              
231300                                  RESP-KDKLIPRI-LINE(IX)                  
231400                                  RESP-FLSATS-LINE(IX)                    
231500                                  RESP-KDINLSTA-LINE(IX)                  
231600        ADD +1 TO IX                                                      
231700     END-PERFORM                                                          
231800     .                                                                    
231900     EJECT                                                                
232000     SKIP2                                                                
232100*----------------------------------------------------------------*        
232200 MFS-LAES-IN-IGEN SECTION.                                                
232300                                                                          
232400*    INDATA-FÄLT                                                          
232500                                                                          
232600     MOVE MFS-ADD-LAES-IN-FAELT    TO RESP-ADINLOMR-PRT-ATTR              
232700                                                                          
232800*--- RENSA INDEXERADE RADER                                               
232900                                                                          
233000     MOVE +1 TO IX                                                        
233100     PERFORM UNTIL IX > MAX-KVRADER                                       
233200                                                                          
233300        MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLSATS-LINE-ATTR(IX)           
233400                                      RESP-KDKLIPRI-LINE-ATTR(IX)         
233500                                      RESP-KDINLSTA-LINE-ATTR(IX)         
233600                                 RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)        
233700        ADD +1 TO IX                                                      
233800     END-PERFORM                                                          
233900                                                                          
234000     .                                                                    
234100     EJECT                                                                
234200******************************************************************        
234300*  IMS-SECTIONER                                                 *        
234400******************************************************************        
234500     SKIP3                                                                
234600******************************************************************        
234700*    ALT1-PCB  (TRANS W60191)                                    *        
234800******************************************************************        
234900*----------------------------------------------------------------*        
235000 IMS-ISRT-MSG-ALT1-6191 SECTION.                                          
235100                                                                          
235200     MOVE SPACE              TO GODK-STATUSKODER                          
235300     CALL CBLTDLI USING      ISRT ALT1-PCB                                
235400                                  P-TO-P-T91                              
235500     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
235600     PERFORM IMS-STATUSKONTROLL                                           
235700     .                                                                    
235800     SKIP3                                                                
235900******************************************************************        
236000*    ALT2-PCB  (TRANS W60194)                                    *        
236100******************************************************************        
236200*----------------------------------------------------------------*        
236300 IMS-ISRT-MSG-ALT2-6194 SECTION.                                          
236400                                                                          
236500     MOVE SPACE              TO GODK-STATUSKODER                          
236600     CALL CBLTDLI USING      ISRT ALT2-PCB                                
236700                                  P-TO-P-T94                              
236800     MOVE ALT2-STATUS-CODE   TO STATUS-WS                                 
236900     PERFORM IMS-STATUSKONTROLL                                           
237000     .                                                                    
237100     SKIP3                                                                
237200******************************************************************        
237300*    ALT3-PCB  (TRANS W60195)                                    *        
237400******************************************************************        
237500*----------------------------------------------------------------*        
237600 IMS-ISRT-MSG-ALT3-6195 SECTION.                                          
237700                                                                          
237800     MOVE SPACE              TO GODK-STATUSKODER                          
237900     CALL CBLTDLI USING      ISRT ALT3-PCB                                
238000                                  P-TO-P-T95                              
238100     MOVE ALT3-STATUS-CODE   TO STATUS-WS                                 
238200     PERFORM IMS-STATUSKONTROLL                                           
238300     .                                                                    
238400     EJECT                                                                
238500******************************************************************        
238600*    INLA-PCB                                                    *        
238700******************************************************************        
238800     SKIP3                                                                
238900*----------------------------------------------------------------*        
239000 IMS-GU-INLA-D101 SECTION.                                                
239100     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
239200             DELIMITED BY SIZE INTO SSA1                                  
239300     MOVE '  GE'            TO GODK-STATUSKODER                           
239400     CALL CBLTDLI USING GU  INLA-PCB                                      
239500                            DLI-IO-AREA-1                                 
239600                            SSA1                                          
239700     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
239800     PERFORM IMS-STATUSKONTROLL                                           
239900     .                                                                    
240000     EJECT                                                                
240100     SKIP3                                                                
240200*----------------------------------------------------------------*        
240300 IMS-GU-INLA-D111 SECTION.                                                
240400     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
240500             DELIMITED BY SIZE INTO SSA1                                  
240600     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
240700          DELIMITED BY SIZE INTO SSA2                                     
240800     MOVE '  GE'            TO GODK-STATUSKODER                           
240900     CALL CBLTDLI USING GU  INLA-PCB                                      
241000                            DLI-IO-AREA-1                                 
241100                            SSA1                                          
241200                            SSA2                                          
241300     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
241400     PERFORM IMS-STATUSKONTROLL                                           
241500     .                                                                    
241600     EJECT                                                                
241700     SKIP3                                                                
241800*----------------------------------------------------------------*        
241900 IMS-GNP-INLA-D121 SECTION.                                               
242000     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
242100             DELIMITED BY SIZE INTO SSA1                                  
242200     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
242300          DELIMITED BY SIZE INTO SSA2                                     
242400     STRING 'W6INLA21(IDRADNR =>' W-IDRADNR-X ')'                         
242500          DELIMITED BY SIZE INTO SSA3                                     
242600     MOVE '  GE'            TO GODK-STATUSKODER                           
242700     CALL CBLTDLI USING GNP INLA-PCB                                      
242800                            DLI-IO-AREA-1                                 
242900                            SSA1                                          
243000                            SSA2                                          
243100                            SSA3                                          
243200     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
243300     PERFORM IMS-STATUSKONTROLL                                           
243400     .                                                                    
243500     EJECT                                                                
243600     SKIP3                                                                
243700******************************************************************        
243800*    INLB1-PCB                                                   *        
243900******************************************************************        
244000     SKIP3                                                                
244100*----------------------------------------------------------------*        
244200 IMS-GU-INLB1-D111    SECTION.                                            
244300     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
244400             DELIMITED BY SIZE INTO SSA1                                  
244500     MOVE '  GE'                 TO GODK-STATUSKODER                      
244600     CALL CBLTDLI USING GU       INLB1-PCB                                
244700                                 DLI-IO-AREA-1                            
244800                                 SSA1                                     
244900     MOVE INLB1-STATUS-CODE      TO STATUS-WS                             
245000     PERFORM IMS-STATUSKONTROLL                                           
245100     .                                                                    
245200     EJECT                                                                
245300     SKIP3                                                                
245400******************************************************************        
245500*    INLB-PCB                                                    *        
245600******************************************************************        
245700     SKIP3                                                                
245800*----------------------------------------------------------------*        
245900 IMS-GU-INLB-D111  SECTION.                                               
246000     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X ')'                       
246100             DELIMITED BY SIZE INTO SSA1                                  
246200     MOVE '  GE'                 TO GODK-STATUSKODER                      
246300     CALL CBLTDLI USING GU       INLB-PCB                                 
246400                                 DLI-IO-AREA-1                            
246500                                 SSA1                                     
246600     MOVE INLB-STATUS-CODE       TO STATUS-WS                             
246700     PERFORM IMS-STATUSKONTROLL                                           
246800     .                                                                    
246900     EJECT                                                                
247000     SKIP3                                                                
247100*----------------------------------------------------------------*        
247200 IMS-GU-INLB-D121    SECTION.                                             
247300     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X ')'                       
247400          DELIMITED BY SIZE INTO SSA1                                     
247500     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
247600          DELIMITED BY SIZE INTO SSA2                                     
247700     MOVE '  GE'            TO GODK-STATUSKODER                           
247800     CALL CBLTDLI USING GU  INLB-PCB                                      
247900                            DLI-IO-AREA-1                                 
248000                            SSA1                                          
248100                            SSA2                                          
248200     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
248300     PERFORM IMS-STATUSKONTROLL                                           
248400     .                                                                    
248500     EJECT                                                                
248600     SKIP3                                                                
248700*----------------------------------------------------------------*        
248800 IMS-GNP-INLB-D121        SECTION.                                        
248900     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X ')'                       
249000          DELIMITED BY SIZE INTO SSA1                                     
249100     STRING 'W6INLA21(IDRADNR =>' W-IDRADNR-X ')'                         
249200          DELIMITED BY SIZE INTO SSA2                                     
249300     MOVE '  GE'            TO GODK-STATUSKODER                           
249400     CALL CBLTDLI USING GNP INLB-PCB                                      
249500                            DLI-IO-AREA-1                                 
249600                            SSA1                                          
249700                            SSA2                                          
249800     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
249900     PERFORM IMS-STATUSKONTROLL                                           
250000     .                                                                    
250100     EJECT                                                                
250200     SKIP3                                                                
250300******************************************************************        
250400*    INLH1-PCB                                                   *        
250500******************************************************************        
250600     SKIP3                                                                
250700*----------------------------------------------------------------*        
250800 IMS-GU-INLH1-D111-KVAL  SECTION.                                         
250900     STRING 'W6INLI01(W6D1H1KY =' W-W6D1H1KY-X ')'                        
251000             DELIMITED BY SIZE INTO SSA1                                  
251100     MOVE '  GE'                 TO GODK-STATUSKODER                      
251200     CALL CBLTDLI USING GU       INLH1-PCB                                
251300                                 DLI-IO-AREA-1                            
251400                                 SSA1                                     
251500     MOVE INLH1-STATUS-CODE      TO STATUS-WS                             
251600     PERFORM IMS-STATUSKONTROLL                                           
251700     .                                                                    
251800     EJECT                                                                
251900     SKIP3                                                                
252000*----------------------------------------------------------------*        
252100 IMS-GU-INLH1-D111-X  SECTION.                                            
252200     STRING 'W6INLI01(W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
252300                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
252400             DELIMITED BY SIZE INTO SSA1                                  
252500     MOVE '  GE'                 TO GODK-STATUSKODER                      
252600     CALL CBLTDLI USING GU       INLH1-PCB                                
252700                                 DLI-IO-AREA-1                            
252800                                 SSA1                                     
252900     MOVE INLH1-STATUS-CODE      TO STATUS-WS                             
253000     PERFORM IMS-STATUSKONTROLL                                           
253100     .                                                                    
253200     EJECT                                                                
253300     SKIP3                                                                
253400*----------------------------------------------------------------*        
253500 IMS-GN-INLH1-D111-X  SECTION.                                            
253600     STRING 'W6INLI01(W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
253700                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
253800             DELIMITED BY SIZE INTO SSA1                                  
253900     MOVE '  GE'                 TO GODK-STATUSKODER                      
254000     CALL CBLTDLI USING GN       INLH1-PCB                                
254100                                 DLI-IO-AREA-1                            
254200                                 SSA1                                     
254300     MOVE INLH1-STATUS-CODE      TO STATUS-WS                             
254400     PERFORM IMS-STATUSKONTROLL                                           
254500     .                                                                    
254600     EJECT                                                                
254700     SKIP3                                                                
254800******************************************************************        
254900*    IMS-UPPDATERING VIA INLB-PCB                                *        
255000******************************************************************        
255100*----------------------------------------------------------------*        
255200 IMS-GHU-INLB-D121        SECTION.                                        
255300     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X ')'                       
255400          DELIMITED BY SIZE INTO SSA1                                     
255500     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
255600          DELIMITED BY SIZE INTO SSA2                                     
255700     MOVE '  GE'            TO GODK-STATUSKODER                           
255800     CALL CBLTDLI USING GHU INLB-PCB                                      
255900                            DLI-IO-AREA-1                                 
256000                            SSA1                                          
256100                            SSA2                                          
256200     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
256300     PERFORM IMS-STATUSKONTROLL                                           
256400     .                                                                    
256500     EJECT                                                                
256600     SKIP3                                                                
256700*----------------------------------------------------------------*        
256800 IMS-REPL-INLB SECTION.                                                   
256900                                                                          
257000     MOVE '  '               TO GODK-STATUSKODER                          
257100     CALL CBLTDLI USING REPL INLB-PCB                                     
257200                             DLI-IO-AREA-1                                
257300     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
257400     PERFORM IMS-STATUSKONTROLL                                           
257500     .                                                                    
257600     EJECT                                                                
257700     SKIP3                                                                
257800******************************************************************        
257900*    LOPA-PCB                                                    *        
258000******************************************************************        
258100     SKIP3                                                                
258200*----------------------------------------------------------------*        
258300 IMS-GHU-LOPA-G111 SECTION.                                               
258400     STRING 'W6LOPA01(W6GXKEY  =' WL-W6GX01KEY-X ')'                      
258500          DELIMITED BY SIZE INTO SSA1                                     
258600     MOVE 'W6LOPA11 '       TO SSA2                                       
258700     MOVE '  GE'            TO GODK-STATUSKODER                           
258800     CALL CBLTDLI USING GHU LOPA-PCB                                      
258900                            DLI-IO-AREA-2                                 
259000                            SSA1                                          
259100                            SSA2                                          
259200     MOVE LOPA-STATUS-CODE  TO STATUS-WS                                  
259300     PERFORM IMS-STATUSKONTROLL                                           
259400     .                                                                    
259500     EJECT                                                                
259600     SKIP3                                                                
259700*----------------------------------------------------------------*        
259800 IMS-REPL-LOPA SECTION.                                                   
259900                                                                          
260000     MOVE '  '               TO GODK-STATUSKODER                          
260100     CALL CBLTDLI USING REPL LOPA-PCB                                     
260200                             DLI-IO-AREA-2                                
260300     MOVE LOPA-STATUS-CODE   TO STATUS-WS                                 
260400     PERFORM IMS-STATUSKONTROLL                                           
260500     .                                                                    
260600     EJECT                                                                
260700     SKIP3                                                                
260800******************************************************************        
260900*    PLAA-PCB                                                    *        
261000******************************************************************        
261100     SKIP3                                                                
261200*----------------------------------------------------------------*        
261300 IMS-GU-PLAA-G111 SECTION.                                                
261400     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
261500          DELIMITED BY SIZE INTO SSA1                                     
261600     STRING 'W6PLAA11(W6GXKEY  =' W-W6GX11KEY-X ')'                       
261700          DELIMITED BY SIZE INTO SSA2                                     
261800     MOVE '  GE'            TO GODK-STATUSKODER                           
261900     CALL CBLTDLI USING GU  PLAA-PCB                                      
262000                            DLI-IO-AREA-3                                 
262100                            SSA1                                          
262200                            SSA2                                          
262300     MOVE PLAA-STATUS-CODE  TO STATUS-WS                                  
262400     PERFORM IMS-STATUSKONTROLL                                           
262500     .                                                                    
262600     EJECT                                                                
262700     SKIP3                                                                
262800 IMS-GU-WDB601    SECTION.                                                
262900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
263000          DELIMITED BY SIZE INTO SSA1                                     
263100     MOVE '  GE' TO GODK-STATUSKODER                                      
263200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
263300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
263400     PERFORM IMS-STATUSKONTROLL                                           
263500     IF SEGMENT-SAKNAS                                                    
263600         MOVE SPACE TO DCS-KDDC                                           
263700     END-IF                                                               
263800     .                                                                    
263900     EJECT                                                                
264000 IMS-GET-BENA-TEXT SECTION.                                               
264100     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
264200             DELIMITED BY SIZE INTO SSA1                                  
264300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
264400              DELIMITED BY SIZE INTO SSA2                                 
264500     MOVE '  GE' TO GODK-STATUSKODER                                      
264600     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA11 SSA1 SSA2          
264700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
264800     PERFORM IMS-STATUSKONTROLL                                           
264900     .                                                                    
265000*----------------------------------------------------------------*        
265100 IMS-STATUSKONTROLL SECTION.                                              
265200                                                                          
265300     SET STATUS-IX TO 1                                                   
265400     SEARCH GODK-STATUS                                                   
265500       AT END                                                             
265600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
265700         DELIMITED BY SIZE INTO FELTEXT                                   
265800         CALL FELLOG                                                      
265900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
266000         CONTINUE                                                         
266100     END-SEARCH                                                           
266200     .                                                                    
266300     EJECT                                                                
266400     SKIP3                                                                
266500******************************************************************        
266600*  INITERA BLÄDDRINGS-NKL:AR                                     *        
266700******************************************************************        
266800*----------------------------------------------------------------*        
266900 S10-NKL-FOER-BLAEDDRING SECTION.                                         
267000                                                                          
267100     IF W-MINKEY-TIAVIDAT    NOT NUMERIC                                  
267200       MOVE ZERO TO W-MINKEY-TIAVIDAT                                     
267300     END-IF                                                               
267400     IF W-MINKEY-IDRADNR-INL NOT NUMERIC                                  
267500       MOVE ZERO TO W-MINKEY-IDRADNR-INL                                  
267600     END-IF                                                               
267700     IF W-MINKEY-IDRADNR     NOT NUMERIC                                  
267800       MOVE ZERO TO W-MINKEY-IDRADNR                                      
267900     END-IF                                                               
268000     IF W-MINKEY-IDARTNR     NOT NUMERIC                                  
268100       MOVE ZERO TO W-MINKEY-IDARTNR                                      
268200     END-IF                                                               
268300     IF W-MINKEY-IDLOPNRM    NOT NUMERIC                                  
268400       MOVE ZERO TO W-MINKEY-IDLOPNRM                                     
268500     END-IF                                                               
268600                                                                          
268700     IF MSGI-IDTRANS = '6123'                                             
268800       IF NKLTYP1                                                         
268900          MOVE W-MINKEY-IDLOPNRM     TO W1-IDLOPNRM                       
269000          MOVE W-MINKEY-IDRADNR      TO W-IDRADNR                         
269100       END-IF                                                             
269200                                                                          
269300       IF NKLTYP2                                                         
269400          MOVE W-MINKEY-IDLEVNR      TO WH1-IDLEVNR                       
269500                                        WH1-MIN-IDLEVNR                   
269600          MOVE W-MINKEY-IDFS         TO WH1-IDFS                          
269700                                        WH1-MIN-IDFS                      
269800          MOVE W-MINKEY-TIAVIDAT     TO WH1-TIAVIDAT                      
269900                                        WH1-MIN-TIAVIDAT                  
270000          MOVE W-MINKEY-IDARTNR      TO WH1-IDARTNR                       
270100                                        WH1-MIN-IDARTNR                   
270200                                        WH1-MAX-IDARTNR                   
270300          MOVE W-MINKEY-IDRADNR-INL  TO WH1-IDRADNR-INL                   
270400                                        WH1-MIN-IDRADNR-INL               
270500                                                                          
270600          MOVE HIGH-VALUE            TO WH1-MAX-IDLEVNR                   
270700          MOVE HIGH-VALUE            TO WH1-MAX-IDFS                      
270800          MOVE +9999999              TO WH1-MAX-TIAVIDAT                  
270900          MOVE +99999                TO WH1-MAX-IDRADNR-INL               
271000                                                                          
271100          MOVE W-MINKEY-IDRADNR      TO W-IDRADNR                         
271200                                                                          
271300          MOVE 'N'                   TO TRAEFF-SW                         
271400       END-IF                                                             
271500                                                                          
271600       IF NKLTYP3                                                         
271700          MOVE W-MINKEY-IDLEVNR      TO WH1-IDLEVNR                       
271800                                        WH1-MIN-IDLEVNR                   
271900                                        WH1-MAX-IDLEVNR                   
272000          MOVE W-MINKEY-IDFS         TO WH1-IDFS                          
272100                                        WH1-MIN-IDFS                      
272200                                        WH1-MAX-IDFS                      
272300          MOVE W-MINKEY-TIAVIDAT     TO WH1-TIAVIDAT                      
272400                                        WH1-MIN-TIAVIDAT                  
272500          MOVE W-MINKEY-IDARTNR      TO WH1-IDARTNR                       
272600                                        WH1-MIN-IDARTNR                   
272700                                        WH1-MAX-IDARTNR                   
272800          MOVE W-MINKEY-IDRADNR-INL  TO WH1-IDRADNR-INL                   
272900                                        WH1-MIN-IDRADNR-INL               
273000                                                                          
273100                                                                          
273200          MOVE +9999999              TO WH1-MAX-TIAVIDAT                  
273300          MOVE +99999                TO WH1-MAX-IDRADNR-INL               
273400                                                                          
273500          MOVE W-MINKEY-IDRADNR      TO W-IDRADNR                         
273600       END-IF                                                             
273700     ELSE                                                                 
273800          MOVE +0                    TO WH1-TIAVIDAT                      
273900                                        WH1-MIN-TIAVIDAT                  
274000                                        WH1-IDARTNR                       
274100                                        WH1-MIN-IDARTNR                   
274200                                        WH1-MAX-IDARTNR                   
274300                                        WH1-IDRADNR-INL                   
274400                                        WH1-MIN-IDRADNR-INL               
274500                                        W1-IDLOPNRM                       
274600                                        W-IDRADNR                         
274700                                                                          
274800          MOVE SPACE                 TO WH1-IDFS                          
274900                                        WH1-MIN-IDFS                      
275000                                        WH1-MAX-IDFS                      
275100                                        WH1-IDLEVNR                       
275200                                        WH1-MIN-IDLEVNR                   
275300                                                                          
275400          MOVE HIGH-VALUE            TO WH1-MAX-IDLEVNR                   
275500          MOVE HIGH-VALUE            TO WH1-MAX-IDFS                      
275600          MOVE +9999999              TO WH1-MAX-TIAVIDAT                  
275700          MOVE +99999                TO WH1-MAX-IDRADNR-INL               
275800                                                                          
275900          MOVE 'N'                   TO TRAEFF-SW                         
276000                                                                          
276100     END-IF                                                               
276200     .                                                                    
276300     EJECT                                                                
276400     SKIP3                                                                
276500*----------------------------------------------------------------*        
276600 S12-INIT-NKL   SECTION.                                                  
276700                                                                          
276800     IF NKLTYP1                                                           
276900        MOVE REQU-IDLOPNRM-KEY     TO W1-IDLOPNRM                         
277000        MOVE ZERO                  TO W-IDRADNR                           
277100     END-IF                                                               
277200                                                                          
277300     IF NKLTYP2                                                           
277400        MOVE REQU-IDARTNR-KEY      TO WH1-MIN-IDARTNR                     
277500        MOVE LOW-VALUE             TO WH1-MIN-IDLEVNR                     
277600        MOVE LOW-VALUE             TO WH1-MIN-IDFS                        
277700        MOVE +0000000              TO WH1-MIN-TIAVIDAT                    
277800                                                                          
277900        MOVE REQU-IDARTNR-KEY      TO WH1-MAX-IDARTNR                     
278000        MOVE HIGH-VALUE            TO WH1-MAX-IDLEVNR                     
278100        MOVE HIGH-VALUE            TO WH1-MAX-IDFS                        
278200        MOVE +9999999              TO WH1-MAX-TIAVIDAT                    
278300                                                                          
278400        MOVE ZERO                  TO W-IDRADNR                           
278500                                                                          
278600        MOVE 'N'                   TO TRAEFF-SW                           
278700     END-IF                                                               
278800                                                                          
278900     IF NKLTYP3                                                           
279000        MOVE REQU-IDARTNR-KEY      TO WH1-MIN-IDARTNR                     
279100        MOVE REQU-IDLEVNR-KEY      TO WH1-MIN-IDLEVNR                     
279200        MOVE REQU-IDFS-KEY         TO WH1-MIN-IDFS                        
279300        MOVE +0000000              TO WH1-MIN-TIAVIDAT                    
279400                                                                          
279500        MOVE REQU-IDARTNR-KEY      TO WH1-MAX-IDARTNR                     
279600        MOVE REQU-IDLEVNR-KEY      TO WH1-MAX-IDLEVNR                     
279700        MOVE REQU-IDFS-KEY         TO WH1-MAX-IDFS                        
279800        MOVE +9999999              TO WH1-MAX-TIAVIDAT                    
279900                                                                          
280000        MOVE ZERO                  TO W-IDRADNR                           
280100                                                                          
280200        MOVE 'N'                   TO TRAEFF-SW                           
280300     END-IF                                                               
280400     .                                                                    
280500     EJECT                                                                
280600     SKIP3                                                                
280700 S13-GET-BEART  SECTION.                                                  
280800                                                                          
280900     PERFORM IMS-GET-BENA-TEXT                                            
281000     IF SEGMENT-FINNS                                                     
281100        MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                    
281200     ELSE                                                                 
281300        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
281400                                   BENA-TEXT-BEART                        
281500        MOVE WS-CP-EBCDIC       TO TRAUTF8-KDCP                           
281600     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GET-BENA-TEXT                                           
            MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                      
           END-IF                                                               
281700                                                                          
           IF REQU-IDMSGVER = '001'                                             
281800*    CALL FROM WEB AND NDC CHINA                                          
281900*    CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE           
282000      CALL WTRAUTF8 USING TRAUTF8-AREA                                    
282100      MOVE TRAUTF8-TECONV-TO  TO WS-BEART                                 
           ELSE                                                                 
            MOVE BENA-TEXT-BEART TO WS-BEART                                    
           END-IF                                                               
282200     .                                                                    
282300     EJECT                                                                
282400                                                                          
282500*----------------------------------------------------------------*        
282600 S20-FLYTTA-SEGM-11-ART-UPPG  SECTION.                                    
282700                                                                          
282800     MOVE ART-IDDC           TO SPAR-IDDC                                 
282900     MOVE ART-ADGANG         TO WS-ADGANG                                 
283000                                WS-TEST-ADGANG                            
283100     MOVE WS-RED-ADGANG      TO RESP-ADGANG                               
283200                                                                          
283300     MOVE ART-ADLAGOMR       TO WS-ADLAGOMR                               
283400                                WS-TEST-ADLAGOMR                          
283500     MOVE WS-RED-ADLAGOMR    TO RESP-ADLAGOMR                             
283600                                                                          
283700     MOVE ART-ADPLATS        TO RESP-ADPLATS                              
283800                                WS-TEST-ADPLATS                           
283900                                                                          
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
284100     PERFORM S13-GET-BEART                                                
284200     MOVE WS-BEART           TO RESP-BEART                                
284600     MOVE ART-KDLAGEMB       TO RESP-KDLAGEMB                             
284700     MOVE ART-KDSORT         TO RESP-KDSORT                               
284800                                                                          
284900     MOVE ART-IDLOPNRM       TO WS-SPAR-IDLOPNRM                          
285000                                                                          
285100     IF ART-KDFARLIG = +4                                                 
285200     OR ART-KDFARLIG = +7                                                 
285300       IF ENGLISH-TEXT OR REQU-IDMSGVER = 001                             
285400         MOVE 'YES'           TO RESP-KDFARLIG-TXT                        
285500       ELSE                                                               
285600         MOVE 'JA'            TO RESP-KDFARLIG-TXT                        
285700       END-IF                                                             
285800     ELSE                                                                 
285900        IF ART-KDFARLIG = +5                                              
286000          IF ENGLISH-TEXT OR REQU-IDMSGVER = 001                          
286100            MOVE 'ASBEST'     TO RESP-KDFARLIG-TXT                        
286200          ELSE                                                            
286300            MOVE 'ASBEST'     TO RESP-KDFARLIG-TXT                        
286400          END-IF                                                          
286500        ELSE                                                              
286600           IF ART-KDFARLIG = +6                                           
286700             IF ENGLISH-TEXT OR REQU-IDMSGVER = 001                       
286800               MOVE 'CHEMICALS ' TO RESP-KDFARLIG-TXT                     
286900             ELSE                                                         
287000               MOVE 'KEMIKALIER' TO RESP-KDFARLIG-TXT                     
287100             END-IF                                                       
287200           END-IF                                                         
287300        END-IF                                                            
287400     END-IF                                                               
287500     .                                                                    
287600     EJECT                                                                
287700     SKIP3                                                                
287800*----------------------------------------------------------------*        
287900 S21-FLYTTA-SEGM-21-RAD-UPPG  SECTION.                                    
288000                                                                          
288100     MOVE WS-SPAR-IDLOPNRM   TO RESP-IDLOPNRM-LINE(IX)                    
288200     MOVE RAD-IDRADNR        TO WS-IDRADNR                                
288300     MOVE WS-RED-IDRADNR     TO RESP-IDRADNR-LINE(IX)                     
288400     MOVE RAD-IDOKOLLI       TO RESP-IDOKOLLI-LINE(IX)                    
288500     MOVE ZERO               TO WS-KVINLART                               
288600     MOVE RAD-KVINLART       TO WS-KVINLART                               
288700     IF RAD-KDINLSTA = 'AVV' OR 'ANT' OR 'KVA'                            
288800        IF WS-KVINLART < ZERO                                             
288900          COMPUTE WS-KVINLART = WS-KVINLART * - 1                         
289000        END-IF                                                            
289100     END-IF                                                               
289200     MOVE WS-KVINLART        TO RESP-KVINLART-LINE(IX)                    
289300     MOVE RAD-ADINLOMR       TO RESP-ADINLOMR-LINE(IX)                    
289400     MOVE RAD-IDINLVGN       TO RESP-IDINLVGN-LINE(IX)                    
289500     MOVE RAD-ADINLOMR-NXT   TO RESP-ADINLOMR-NXT-LINE(IX)                
289600     MOVE RAD-KDINLSTA       TO RESP-KDINLSTA-LINE(IX)                    
289700     IF ENGLISH-TEXT OR REQU-IDMSGVER = 001                               
289800       EVALUATE RESP-KDINLSTA-LINE(IX)                                    
289900         WHEN 'FPK'                                                       
290000            MOVE 'PP '         TO RESP-KDINLSTA-LINE(IX)                  
290100         WHEN 'INL'                                                       
290200            MOVE 'BIN'         TO RESP-KDINLSTA-LINE(IX)                  
290300         WHEN 'SAK'                                                       
290400            MOVE 'MIS'         TO RESP-KDINLSTA-LINE(IX)                  
290500         WHEN 'AVV'                                                       
290600            MOVE 'DEV'         TO RESP-KDINLSTA-LINE(IX)                  
290700         WHEN 'ANT'                                                       
290800            MOVE 'DEV'         TO RESP-KDINLSTA-LINE(IX)                  
290900         WHEN 'KVA'                                                       
291000            MOVE 'Q-D'         TO RESP-KDINLSTA-LINE(IX)                  
291100         WHEN 'RET'                                                       
291200            MOVE 'RET'         TO RESP-KDINLSTA-LINE(IX)                  
291300         WHEN 'FRD'                                                       
291400            MOVE 'TRP'         TO RESP-KDINLSTA-LINE(IX)                  
291500         WHEN 'MAK'                                                       
291600            MOVE 'CAN'         TO RESP-KDINLSTA-LINE(IX)                  
291700       END-EVALUATE                                                       
291800     END-IF                                                               
291900                                                                          
292000     IF RAD-IDLEVNR-KOLLI = SPACE                                         
292100        MOVE WS-SPAR-IDLEVNR TO RESP-IDLEVNR-LINE(IX)                     
292200     ELSE                                                                 
292300        MOVE RAD-IDLEVNR-KOLLI TO RESP-IDLEVNR-LINE(IX)                   
292400     END-IF                                                               
292500                                                                          
292600     IF RAD-FLSATS = JA                                                   
292700        MOVE RAD-FLSATS      TO RESP-FLSATS-LINE(IX)                      
292800     ELSE                                                                 
292900        MOVE SPACE           TO RESP-FLSATS-LINE(IX)                      
293000     END-IF                                                               
293100                                                                          
293200     IF RAD-FLPRIO = JA                                                   
293300        MOVE 'P'             TO RESP-KDKLIPRI-LINE(IX)                    
293400     ELSE                                                                 
293500        MOVE SPACE           TO RESP-KDKLIPRI-LINE(IX)                    
293600     END-IF                                                               
293700     .                                                                    
293800     EJECT                                                                
293900     SKIP3                                                                
294000*----------------------------------------------------------------*        
294100 S30-HAEMTA-IDOKOLLINR SECTION.                                           
294200                                                                          
294300     MOVE LOW-VALUE          TO WL-W6GX01KEY-X                            
294400     MOVE '6017'             TO WL-IDHTYP                                 
294500     PERFORM IMS-GHU-LOPA-G111                                            
294600     IF SEGMENT-FINNS                                                     
294700        ADD 1               TO 6018-IDOKOLLI                              
294800        PERFORM IMS-REPL-LOPA                                             
294900     END-IF                                                               
295000     .                                                                    
295100     EJECT                                                                
295200*----------------------------------------------------------------*        
295300 S40-TRANS-W60191      SECTION.                                           
295400                                                                          
295500     IF T91-MID-KVPOST = ZERO                                             
295600        MOVE JA                  TO TRANS91-SW                            
295700        MOVE SPACE               TO T91-MID-W6I19101                      
295800        MOVE +1                  TO T91-MID-KVPOST                        
295900                                                                          
296000        MOVE IDPGM               TO T91-MID-IDPGM                         
296100        MOVE W-IDDC              TO T91-MID-IDDC                          
296200                                                                          
296300        MOVE REQU-IDLOPNRM(IX)     TO T91-MID-IDLOPNRM(1)                 
296400        INSPECT T91-MID-IDLOPNRM(1)                                       
296500                REPLACING LEADING SPACE BY ZERO                           
296600        MOVE WS-SPAR-PRARTSTD      TO T91-MID-PRARTSTD(1)                 
296700        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(1)                  
296800        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(1)                
296900        MOVE +0                    TO T91-MID-KVKOLLI (1)                 
297000        MOVE 'N'                   TO T91-MID-FLINLI   (1)                
297100        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-OLD(1)             
297200        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(1)             
297300        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-OLD(1)         
297400        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-NEW(1)         
297500        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(1)             
297600        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(1)             
297700        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(1)             
297800        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(1)             
297900     ELSE                                                                 
298000        COMPUTE IX2 = T91-MID-KVPOST + 1                                  
298100        ADD 1                         TO T91-MID-KVPOST                   
298200                                                                          
298300        MOVE REQU-IDLOPNRM(IX)     TO T91-MID-IDLOPNRM(IX2)               
298400        INSPECT T91-MID-IDLOPNRM(IX2)                                     
298500                REPLACING LEADING SPACE BY ZERO                           
298600        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(IX2)                
298700        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(IX2)              
298800        MOVE +0                    TO T91-MID-KVKOLLI (IX2)               
298900        MOVE 'N'                   TO T91-MID-FLINLI   (IX2)              
299000        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-OLD(IX2)           
299100        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(IX2)           
299200        MOVE RAD-ADINLOMR-NXT                                             
299300             TO T91-MID-ADINLOMR-NXT-OLD(IX2)                             
299400        MOVE RAD-ADINLOMR-NXT                                             
299500             TO T91-MID-ADINLOMR-NXT-NEW(IX2)                             
299600        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(IX2)           
299700        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(IX2)           
299800        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(IX2)           
299900        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(IX2)           
300000     END-IF                                                               
300100     .                                                                    
300200     EJECT                                                                
300300*----------------------------------------------------------------*        
300400 S41-TRANS-W60194      SECTION.                                           
300500                                                                          
300600     MOVE ZERO                TO SPAR-VKART-KG                            
300700                                                                          
300800     IF T94-MID-KVPOST = ZERO                                             
300900        MOVE ZERO             TO T94-MID-VKKOLLIN(1)                      
301000        MOVE JA                       TO TRANS94-SW                       
301100                                                                          
301200        MOVE SPACE                    TO T94-MID-IDPRTLST                 
301300        MOVE '6F'                     TO T94-MID-IDPRTLST(1:2)            
301400        MOVE REQU-ADINLOMR-PRT        TO T94-MID-IDPRTLST(3:6)            
301500        MOVE IDPGM                    TO T94-MID-IDPGM                    
301600        MOVE +1                       TO T94-MID-KVPOST                   
301700                                                                          
301800        MOVE SPAR-IDARTNR             TO T94-MID-IDARTNR(1)               
301900        MOVE SPAR-IDLOPNRM            TO T94-MID-IDLOPNRM(1)              
302000        MOVE RAD-KVINLART             TO T94-MID-KVINLART(1)              
302100        MOVE SPAR-IDLEVNR-KOLLI(IX)   TO T94-MID-IDLEVNR-KOLLI(1)         
302200        MOVE SPAR-IDOKOLLI(IX)        TO T94-MID-IDOKOLLI(1)              
302300        MOVE SPAR-TIINLMOT            TO T94-MID-TIINLMOT(1)              
302400        COMPUTE SPAR-VKART-KG =  SPAR-VKART / 1000                        
302500        COMPUTE T94-MID-VKKOLLIN(1) ROUNDED =                             
302600                RAD-KVINLART * SPAR-VKART-KG                              
302700        MOVE ZERO                     TO T94-MID-VKKOLLIB(1)              
302800        MOVE SPAR-ADLAGOMR            TO T94-MID-ADLAGOMR(1)              
302900        MOVE SPAR-ADGANG              TO T94-MID-ADGANG(1)                
303000        MOVE SPAR-ADPLATS             TO T94-MID-ADPLATS(1)               
303100        MOVE SPAR-KDSORT              TO T94-MID-KDSORT(1)                
303200        MOVE SPAR-BEFT                TO T94-MID-BEFT(1)                  
303300     ELSE                                                                 
303400        COMPUTE IX1 = T94-MID-KVPOST + 1                                  
303500        ADD 1                         TO T94-MID-KVPOST                   
303600        MOVE ZERO                     TO T94-MID-VKKOLLIN(IX1)            
303700        MOVE SPAR-IDARTNR             TO T94-MID-IDARTNR(IX1)             
303800        MOVE SPAR-IDLOPNRM            TO T94-MID-IDLOPNRM(IX1)            
303900        MOVE RAD-KVINLART             TO T94-MID-KVINLART(IX1)            
304000        MOVE SPAR-IDLEVNR-KOLLI(IX) TO T94-MID-IDLEVNR-KOLLI(IX1)         
304100        MOVE SPAR-IDOKOLLI(IX)        TO T94-MID-IDOKOLLI(IX1)            
304200        MOVE SPAR-TIINLMOT            TO T94-MID-TIINLMOT(IX1)            
304300        COMPUTE SPAR-VKART-KG =  SPAR-VKART / 1000                        
304400        COMPUTE T94-MID-VKKOLLIN(IX1) ROUNDED =                           
304500                RAD-KVINLART * SPAR-VKART-KG                              
304600        MOVE ZERO                     TO T94-MID-VKKOLLIB(IX1)            
304700        MOVE SPAR-ADLAGOMR            TO T94-MID-ADLAGOMR(IX1)            
304800        MOVE SPAR-ADGANG              TO T94-MID-ADGANG(IX1)              
304900        MOVE SPAR-ADPLATS             TO T94-MID-ADPLATS(IX1)             
305000        MOVE SPAR-KDSORT              TO T94-MID-KDSORT(IX1)              
305100        MOVE SPAR-BEFT                TO T94-MID-BEFT(IX1)                
305200     END-IF                                                               
305300     .                                                                    
305400     EJECT                                                                
305500*----------------------------------------------------------------*        
305600 S42-TRANS-W60195      SECTION.                                           
305700                                                                          
305800     IF T95-MID-KVPOST = ZERO                                             
305900       MOVE JA                       TO TRANS95-SW                        
306000       MOVE IDPGM                    TO T95-MID-IDPGM                     
306100       MOVE SPACE                    TO T95-MID-IDPRTLST                  
306200       MOVE '6E'                     TO T95-MID-IDPRTLST(1:2)             
306300       MOVE REQU-ADINLOMR-PRT        TO T95-MID-IDPRTLST(3:6)             
306400       MOVE +1                       TO T95-MID-KVPOST                    
306500       MOVE SPAR-IDLOPNRM            TO T95-MID-IDLOPNRM(1)               
306600       MOVE RAD-IDRADNR              TO T95-MID-IDRADNR(1)                
306700       MOVE SPAR-IDARTNR             TO T95-MID-IDARTNR(1)                
306800       MOVE RAD-KVINLART             TO T95-MID-KVINLART(1)               
306900       MOVE SPAR-BEART               TO T95-MID-BEART(1)                  
307000       MOVE SPAR-ADLAGOMR            TO T95-MID-ADLAGOMR(1)               
307100       MOVE SPAR-ADGANG              TO T95-MID-ADGANG(1)                 
307200       MOVE SPAR-ADPLATS             TO T95-MID-ADPLATS(1)                
307300       MOVE JA                       TO TRANS95-SW                        
307400     ELSE                                                                 
307500       COMPUTE IX1 = T95-MID-KVPOST + 1                                   
307600       ADD  1                        TO T95-MID-KVPOST                    
307700       MOVE SPAR-IDLOPNRM            TO T95-MID-IDLOPNRM(IX1)             
307800       MOVE RAD-IDRADNR              TO T95-MID-IDRADNR(IX1)              
307900       MOVE SPAR-IDARTNR             TO T95-MID-IDARTNR(IX1)              
308000       MOVE RAD-KVINLART             TO T95-MID-KVINLART(IX1)             
308100       MOVE SPAR-BEART               TO T95-MID-BEART(IX1)                
308200       MOVE SPAR-ADLAGOMR            TO T95-MID-ADLAGOMR(IX1)             
308300       MOVE SPAR-ADGANG              TO T95-MID-ADGANG(IX1)               
308400       MOVE SPAR-ADPLATS             TO T95-MID-ADPLATS(IX1)              
308500     END-IF                                                               
308600     .                                                                    
308700     EJECT                                                                
308800*----------------------------------------------------------------*        
308900 S50-SKICKA-W60191 SECTION.                                               
309000                                                                          
309100     COMPUTE W-PTOP1-OCC-LL = T91-MID-KVPOST * 64                         
309200     COMPUTE PTOP1-LL = W-PTOP1-OCC-LL + 35                               
309300     MOVE MFS-KDMFSFOR          TO PTOP1-KDMFSFOR                         
309400                                                                          
309500     PERFORM IMS-ISRT-MSG-ALT1-6191                                       
309600     .                                                                    
309700     EJECT                                                                
309800*----------------------------------------------------------------*        
309900 S51-SKICKA-W60194 SECTION.                                               
310000                                                                          
310100     COMPUTE W-PTOP2-OCC-LL = T94-MID-KVPOST * 67                         
310200     COMPUTE PTOP2-LL = W-PTOP2-OCC-LL + 40                               
310300     MOVE MFS-KDMFSFOR          TO PTOP2-KDMFSFOR                         
310400                                                                          
310500     PERFORM IMS-ISRT-MSG-ALT2-6194                                       
310600     .                                                                    
310700     EJECT                                                                
310800*----------------------------------------------------------------*        
310900 S52-SKICKA-W60195 SECTION.                                               
311000                                                                          
311100     COMPUTE W-PTOP3-OCC-LL = T95-MID-KVPOST * 61                         
311200     COMPUTE PTOP3-LL           = W-PTOP3-OCC-LL + 40                     
311300     MOVE MFS-KDMFSFOR          TO PTOP3-KDMFSFOR                         
311400                                                                          
311500     PERFORM IMS-ISRT-MSG-ALT3-6195                                       
311600     .                                                                    
311700     EJECT                                                                
311800******************************************************************        
311900*  BILD-REDIGERING                                               *        
312000******************************************************************        
312100*----------------------------------------------------------------*        
312200 S90-BLANKUTF-NUM-FAELT SECTION.                                          
312300                                                                          
312400     INSPECT RESP-IDLOPNRM-KEY REPLACING LEADING ZERO BY SPACE            
312500     INSPECT RESP-IDARTNR-KEY  REPLACING LEADING ZERO BY SPACE            
312600                                                                          
312700*----HUV-FÄLT                                                             
312800     INSPECT RESP-ADLAGOMR  REPLACING LEADING ZERO BY SPACE               
312900     INSPECT RESP-ADGANG    REPLACING LEADING ZERO BY SPACE               
313000     INSPECT RESP-ADPLATS   REPLACING LEADING ZERO BY SPACE               
313100                                                                          
313200*----RAD-FÄLT                                                             
313300*--- INDEXERADE RADER                                                     
313400                                                                          
313500     MOVE +1 TO IX                                                        
313600     PERFORM UNTIL IX > MAX-KVRADER                                       
313700        INSPECT RESP-IDLOPNRM-LINE(IX)                                    
313800                                  REPLACING LEADING ZERO BY SPACE         
313900        INSPECT RESP-IDRADNR-LINE(IX)                                     
314000                                 REPLACING LEADING ZERO BY SPACE          
314100        INSPECT RESP-IDOKOLLI-LINE(IX)                                    
314200                                  REPLACING LEADING ZERO BY SPACE         
314300        INSPECT RESP-IDINLVGN-LINE(IX)                                    
314400                                  REPLACING LEADING ZERO BY SPACE         
314500                                                                          
314600        IF RESP-KDKLIPRI-LINE(IX) = 'P'                                   
314700           MOVE MFS-ADD-LYS-UPP-FAELT                                     
314800                                    TO RESP-KDKLIPRI-LINE-ATTR(IX)        
314900        END-IF                                                            
315000        IF INDATA-OK                                                      
315100           IF RESP-IDRADNR-LINE(IX) = SPACE                               
315200              MOVE MFS-STAENG-FAELT                                       
315300                   TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                   
315400           ELSE                                                           
315500              IF RESP-KDINLSTA-LINE(IX) = ('FPK' OR 'SAK' OR              
315600                            SPACE OR 'PP ' OR 'MIS')                      
315700                 MOVE MFS-OEPPNA-ALFA-FAELT                               
315800                      TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                
315900              ELSE                                                        
316000                 MOVE MFS-STAENG-FAELT                                    
316100                      TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                
316200              END-IF                                                      
316300           END-IF                                                         
316400        END-IF                                                            
316500                                                                          
316600*--------------------------------------------------------------*          
316700*----DENNA DEL GÖRS FÖR ATT FÅ ORDNING PÅ ÖPPNA/STÄNGDA FÄLT---*          
316800*--------------------------------------------------------------*          
316900        IF STATUS-JA(IX) = NEJ                                            
317000           MOVE MFS-ADD-LAES-IN-FAELT                                     
317100                                    TO RESP-KDINLSTA-LINE-ATTR(IX)        
317200        END-IF                                                            
317300*--------------------------------------------------------------*          
317400        ADD +1 TO IX                                                      
317500     END-PERFORM                                                          
317600     .                                                                    
