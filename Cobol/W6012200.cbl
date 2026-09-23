000618 ID DIVISION.                                                             
000619     SKIP2                                                                
000620 PROGRAM-ID.     W6012200.                                                
000621*AUTHOR.         ROS-MARIE CLASON - GUIDE DATAKONSULT AB.                 
000622*DATE-WRITTEN.   92/05/19.                                                
000630                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET ÄR EN MPP SOM VISAR ARTIKEL OCH/                      
001200*        ELLER PARTIINFORMATION AV VAD SOM FINNS PÅ                       
001300*        INLEVERANSREGISTRET.                                             
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001600*                              W6LOPA (W6G1)                              
001700*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T122                                              
002100*        MID:         W6I12201                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W6O12201                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900                                                                          
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003101                                                                          
003110*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6012200'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003710 77  YES                         PIC X       VALUE 'Y'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004100 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004110 77  T91-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  T94-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  MAX-IX                      PIC S9(4)  VALUE +12   COMP SYNC.        
004500 77  FL-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
004510 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004600                                                                          
004700*    --- RÄKNARE                                                          
004800 77  IDLOPNR-RAKN                PIC S9(2)  VALUE +0    COMP SYNC.        
004900                                                                          
005000*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = (395) MOD-LÄNGD + 4              
005100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +399 COMP SYNC.         
005200                                                                          
005300***********************************                                       
005400*        ARBETSFÄLT *                                                     
005500***********************************                                       
005600 01  WS-AREA.                                                             
006300*DIV                                                                      
006410     03  W-KVINLART               PIC 9(7).                               
006500     03  W-KVINLART-X REDEFINES W-KVINLART.                               
006600         05  FILLER               PIC X(1).                               
006700         05  W-RED-KVINLART       PIC X(6).                               
006800                                                                          
006805*REDIGERING (NUM-ALFA)                                                    
006841                                                                          
006842     03  W-ARTNR-N                PIC 9(9).                               
006843     03  W-ARTNR-X REDEFINES W-ARTNR-N.                                   
006844         05  FILLER               PIC X(1).                               
006845         05  W-RED-ARTNR          PIC X(8).                               
006846                                                                          
006847     03  W-KVAVIS-N                PIC 9(7).                              
006848     03  W-KVAVIS-X REDEFINES W-KVAVIS-N.                                 
006849         05  FILLER               PIC X(1).                               
006850         05  W-RED-KVAVIS         PIC X(6).                               
006851                                                                          
006852*ACKAR                                                                    
006853     03  SPAR-IDRADNR             PIC S9(5)  COMP-3 VALUE ZERO.           
006854     03  SPAR-ACK-KVFLETI         PIC S9(3)  COMP-3 VALUE ZERO.           
006855     03  SPAR-ACK-KVINLART        PIC S9(7)  COMP-3 VALUE ZERO.           
006856     03  SPAR-RAD1-KVINLART       PIC S9(7)  COMP-3 VALUE ZERO.           
006857     03  SPAR-ACK-IDOKOLLI        PIC 9(9)   VALUE ZERO.                  
006858*                                                                         
006859     03  SPAR-KVINLART            PIC 9(7).                               
006860     03  SPAR-KVINLART-X REDEFINES SPAR-KVINLART.                         
006861         05  FILLER               PIC X(1).                               
006862         05  SPAR-RED-KVINLART    PIC X(6).                               
006863                                                                          
006864*                                                                         
006865     03  SPAR-KVFLETI             PIC 9(2).                               
006866     03  SPAR-KVFLETI-X REDEFINES SPAR-KVFLETI.                           
006867         05  FILLER               PIC X(2).                               
006868                                                                          
006869*PRINTER                                                                  
006870     03  WS-IDPRT.                                                        
006871         05  WS-IDPRT1            PIC X(2).                               
006872         05  WS-IDPRT2            PIC X(4).                               
006873         05  WS-IDPRT3            PIC X(2).                               
006874                                                                          
006880*PLAA                                                                     
006900     03  SPAR-IDLEVNR-NEW         PIC X(5)            VALUE SPACE.        
006910                                                                          
007200*INLA-D111                                                                
007300     03  SPAR-PRARTSTD            PIC 9(7)V9(2)       VALUE ZERO.         
007400     03  SPAR-ADLAGOMR            PIC S9(3)   COMP-3  VALUE ZERO.         
007401     03  SPAR-ADGANG              PIC S9(3)   COMP-3  VALUE ZERO.         
007402     03  SPAR-ADPLATS             PIC S9(5)   COMP-3  VALUE ZERO.         
007403     03  SPAR-BEFT                PIC  9(3)           VALUE ZERO.         
007404     03  SPAR-KDSORT              PIC X(2)            VALUE SPACE.        
007410     03  SPAR-KDINLSTA-NEW        PIC X(3)            VALUE SPACE.        
007420     03  SPAR-KDINLSTA-OLD        PIC X(3)            VALUE SPACE.        
007600     03  SPAR-TIINLMOT            PIC 9(6)            VALUE ZERO.         
007700     03  SPAR-IDARTNR             PIC 9(9)            VALUE ZERO.         
007800     03  SPAR-VKART               PIC 9(7)            VALUE ZERO.         
007900     03  SPAR-VKART-KG            PIC 9(4)V9(3)       VALUE ZERO.         
007901                                                                          
009200*T91,T94                                                                  
009300     03  SPAR-KVINLART-OLD          PIC S9(7) COMP-3 VALUE ZERO.          
009310     03  SPAR-ADINLOMR-OLD          PIC X(4) VALUE SPACE.                 
009400     03  SPAR-ADINLOMR-NEW          PIC X(4) VALUE SPACE.                 
009500                                                                          
009510*FRÅN RADNR=1                                                             
009521     03  W-RAD1-ADINLOMR-NXT     PIC X(4)     VALUE SPACE.                
009530     03  W-RAD1-FLPRIO           PIC X(1)     VALUE SPACE.                
009540     03  W-RAD1-FLKVAANT         PIC X(1)     VALUE SPACE.                
009550     03  W-RAD1-FLINLFP          PIC X(1)     VALUE SPACE.                
009560     03  W-RAD1-FLINLFB          PIC X(1)     VALUE SPACE.                
009570     03  W-RAD1-IDINLVGN         PIC S9(3)    COMP-3 VALUE ZERO.          
009590     03  W-RAD1-KDINLPRIO        PIC S9(3)    COMP-3 VALUE ZERO.          
009591     03  W-RAD1-KDINLSTA         PIC X(3)     VALUE SPACE.                
009592     03  W-RAD1-TIUPPDAT         PIC S9(7)    COMP-3 VALUE ZERO.          
009600*                                                                         
009610     03  W-PTOP1-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
009620     03  W-PTOP2-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
009630                                                                          
009640*      --- VALID IDDC CODES                                               
009650*                                                                         
009660*01    -COPY WWDC99                                                       
009670       EJECT                                                              
009700*    --- ARBETSFÄLT FÖR SWITCHAR                                          
009800                                                                          
009900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010000     88  INDATA-OK                           VALUE 'J'.                   
010100     88  INDATA-FEL                          VALUE 'N'.                   
010200                                                                          
010300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010400     88  NYCKLAR-OK                          VALUE 'J'.                   
010500     88  NYCKLAR-FEL                         VALUE 'N'.                   
010600                                                                          
010610 77  NYA-NYCKLAR-SW              PIC X       VALUE 'J'.                   
010620     88  NYA-NYCKLAR-JA                      VALUE 'J'.                   
010630     88  NYA-NYCKLAR-NEJ                     VALUE 'N'.                   
010640                                                                          
011900 77  SLUT-SW                     PIC X       VALUE 'J'.                   
012000     88  SLUT-JA                             VALUE 'J'.                   
012100     88  SLUT-NEJ                            VALUE 'N'.                   
012200                                                                          
012220 77  FOERSTA-T91-SW              PIC X       VALUE 'N'.                   
012230     88  FOERSTA-T91-JA                      VALUE 'J'.                   
012240     88  FOERSTA-T91-NEJ                     VALUE 'N'.                   
012250                                                                          
012320 77  TRANS91-SW                  PIC X       VALUE 'J'.                   
012400     88  TRANS91-JA                          VALUE 'J'.                   
012500     88  TRANS91-NEJ                         VALUE 'N'.                   
012600                                                                          
012610 77  FOERSTA-T94-SW              PIC X       VALUE 'N'.                   
012620     88  FOERSTA-T94-JA                      VALUE 'J'.                   
012630     88  FOERSTA-T94-NEJ                     VALUE 'N'.                   
012640                                                                          
012700 77  TRANS94-SW                  PIC X       VALUE 'J'.                   
012800     88  TRANS94-JA                          VALUE 'J'.                   
012900     88  TRANS94-NEJ                         VALUE 'N'.                   
013000                                                                          
013100 77  FLER-FL-SW                  PIC X       VALUE 'N'.                   
013200     88  FLER-FL-JA                          VALUE 'J'.                   
013300     88  FLER-FL-NEJ                         VALUE 'N'.                   
013310                                                                          
013320 77  MID-SW                      PIC X       VALUE 'N'.                   
013330     88  MID-EJ-IFYLLD                       VALUE 'N'.                   
013400                                                                          
013800                                                                          
013900*----------------------------------------------------------------*        
014000*   NKLTYP1=PARTINR,  NKLTYP2=ARTNR-LEVNR-FS                              
014100*----------------------------------------------------------------*        
014200 77  NKLTYP-SW                  PIC X.                                    
014300     88  NKLTYP1                            VALUE '1'.                    
014400     88  NKLTYP2                            VALUE '2'.                    
014500                                                                          
014600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014700     88  EGEN-MID                            VALUE '6122'.                
014800     88  GODK-MID                            VALUE '6121' '6122'          
014900                                                   '6123'.                
015000     88  HELP-MID                            VALUE '0551'.                
015100     EJECT                                                                
015200                                                                          
015300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015400 01  GENERELLA-SUBPROGRAM.                                                
015500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015900     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
015910     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016000     EJECT                                                                
016100                                                                          
016110*01 -COPY WMSGINIT                                                        
016120     SKIP3                                                                
016200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016300*01 -COPY WMEDAREA                                                        
016400     SKIP3                                                                
016500 01  MESSAGE-CODES.                                                       
016600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
016700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016800     03  ERR-UPDATE-NOT-VALID    PIC X(3)    VALUE '007'.                 
016900     03  ERR-SAKN-I-REG          PIC X(3)    VALUE '010'.                 
017500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
017700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
017800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017900     03  ERR-CODE-NOT-VALID      PIC X(3)    VALUE '416'.                 
018011     03  ERR-PLATS-SAKN          PIC X(3)    VALUE '764'.                 
018012     03  ERR-PRINT-SAKN          PIC X(3)    VALUE '772'.                 
018013     03  ERR-RADNR-1-SAKN        PIC X(3)    VALUE '178'.                 
018014     03  ERR-ANT-EJ-OK           PIC X(3)    VALUE '181'.                 
018015     03  ERR-KVFLETI-EJ-OK       PIC X(3)    VALUE '195'.                 
018017     03  INF-FLER-FLAGGOR        PIC X(3)    VALUE '199'.                 
018102     03  ERR-ANGE-PARTI          PIC X(3)    VALUE '226'.                 
018300     EJECT                                                                
018400                                                                          
018500*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
018600*    --- COPYTEXTER FÖR W006PRT, W60191, W60194                           
018800*                                                                         
018900*01  -COPY W006PRT                                                        
018910     EJECT                                                                
019000                                                                          
019100 01  P-TO-P-T91.                                                          
019200*----TILL W60191                                                          
019300     03  PTOP1-LL              PIC S9(4)   COMP SYNC.                     
019400     03  PTOP1-Z1              PIC X(1)    VALUE LOW-VALUE.               
019500     03  PTOP1-Z2              PIC X(1)    VALUE LOW-VALUE.               
019600     03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W6T191X'.               
019700     03  FILLER                PIC X(1)    VALUE SPACE.                   
019800     03  PTOP1-IDTRANS         PIC X(4)    VALUE '6122'.                  
019900     03  PTOP1-KDMFSFOR        PIC X(1).                                  
020000*    03  MID -COPY W6I19101       -PRE T91-                               
020100     EJECT                                                                
020200*                                                                         
020300 01  P-TO-P-T94.                                                          
020400*----TILL W60194                                                          
020500     03  PTOP2-LL              PIC S9(4)   COMP SYNC.                     
020600     03  PTOP2-Z1              PIC X(1)    VALUE LOW-VALUE.               
020700     03  PTOP2-Z2              PIC X(1)    VALUE LOW-VALUE.               
020800     03  PTOP2-TRANSKOD        PIC X(7)    VALUE 'W6T194X'.               
020900     03  FILLER                PIC X(1)    VALUE SPACE.                   
021000     03  PTOP2-IDTRANS         PIC X(4)    VALUE '6122'.                  
021100     03  PTOP2-KDMFSFOR        PIC X(1).                                  
021200*    03  MID -COPY W6I19401       -PRE T94-                               
021300     EJECT                                                                
021400                                                                          
021450                                                                          
021500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021600*                                                                         
021700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021800     SKIP3                                                                
021900*01  MID -COPY W6I12201                                                   
022000     EJECT                                                                
022100                                                                          
022200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
022300     SKIP3                                                                
022400*01  -COPY WMSGAREA                                                       
022500     EJECT                                                                
022600                                                                          
022700     03  MOD REDEFINES MSG-AREA.                                          
022800*      05  -COPY W6O12201                                                 
022900     EJECT                                                                
023000                                                                          
023100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023200     SKIP3                                                                
023300*01  -COPY WMFSAREA                                                       
023400     EJECT                                                                
023500                                                                          
023600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023700*                                                                         
023800     EJECT                                                                
023900                                                                          
024000*                                                                         
024100*    --- DLI- NYCKLAR TILL IMS-SEKTIONERNA                                
024200*                                                                         
024300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024400     SKIP3                                                                
024500 01  NYCKLAR-TILL-DLI.                                                    
024600*--------FYSISK NKL TILL INLA                                             
024700     03  W-W6D101KY-X.                                                    
024900         05  W-IDDC-101KY        PIC  X(2)    VALUE SPACE.                
024910         05  W-IDLEVNR           PIC X(5)     VALUE SPACE.                
025000         05  W-IDFS              PIC X(8)     VALUE SPACE.                
025100         05  W-TIAVIDAT          PIC S9(7)    COMP-3.                     
025200                                                                          
025300     03  W-IDLOPNRM-X.                                                    
025400         05  W-IDLOPNRM         PIC S9(9)   COMP-3.                       
025410                                                                          
025420     03  W-IDARTNR-X.                                                     
025430         05  W-IDARTNR          PIC S9(9)   COMP-3.                       
025500                                                                          
025501     03 W-KDSEGKEY-X.                                                     
025502         05 W-KDSEGKEY           PIC X(1)    VALUE '1'.                   
025505                                                                          
025510     03  W-IDRADNR-INL-X.                                                 
025520         05  W-IDRADNR-INL      PIC S9(5)   COMP-3.                       
025530                                                                          
025600     03  W-IDRADNR-X.                                                     
025700         05  W-IDRADNR           PIC S9(5)   COMP-3.                      
025800                                                                          
025810     03  W-IDDC-X.                                                        
025820         05  W-IDDC              PIC  X(2).                               
025830                                                                          
027310*--------FYSISK NKL TILL INLH (EG. INLI)                                  
027320     03  W-W6D1H1KY-MIN-X.                                                
027340         05  WH1-MIN-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
027341         05  WH1-MIN-IDDC        PIC X(2)     VALUE SPACE.                
027350         05  WH1-MIN-IDLEVNR     PIC X(5)     VALUE SPACE.                
027360         05  WH1-MIN-IDFS        PIC X(8)     VALUE SPACE.                
027370         05  WH1-MIN-TIAVIDAT    PIC S9(7)    COMP-3.                     
027371         05  WH1-MIN-IDRADNR-INL PIC S9(5)    COMP-3.                     
027380                                                                          
027400     03  W-W6D1H1KY-MAX-X.                                                
027600         05  WH1-MAX-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
027610         05  WH1-MAX-IDDC        PIC X(2)     VALUE SPACE.                
027700         05  WH1-MAX-IDLEVNR     PIC X(5)     VALUE SPACE.                
027800         05  WH1-MAX-IDFS        PIC X(8)     VALUE SPACE.                
027900         05  WH1-MAX-TIAVIDAT    PIC S9(7)    COMP-3.                     
028000         05  WH1-MAX-IDRADNR-INL PIC S9(5)    COMP-3.                     
029700                                                                          
029800*--------SEQ NKL TILL INLB                                                
029900     03  W-W6D1B1KY-X.                                                    
030100         05  WB-IDLOPNRM         PIC S9(9)   COMP-3.                      
030200                                                                          
030300*--------FYSISK NKL TILL LOPA                                             
030400     03  WL-W6GX01KEY-X.                                                  
030500         05  WL-IDHTYP           PIC X(4)    VALUE SPACE.                 
030600         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
030700                                                                          
030800*--------FYSISK NKL TILL PLAA                                             
030900     03  W-W6GX01KEY-X.                                                   
031000         05  WGX-IDHTYP          PIC X(4)    VALUE SPACE.                 
031001         05  WGX-IDDC            PIC X(2)    VALUE SPACE.                 
031010         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
031300                                                                          
031400     03  W-W6GX11KEY-X.                                                   
031500         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
031600         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
031700     SKIP2                                                                
031800                                                                          
031900*    --- STATUS-KOD FRÅN IMS                                              
032000 01  STATUS-WS                   PIC XX.                                  
032100     88  SEGMENT-FINNS                       VALUE '  '.                  
032200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032400     SKIP2                                                                
032500                                                                          
032600 01  GODK-STATUSKODER.                                                    
032700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032800     SKIP3                                                                
032900                                                                          
033000 01  SSA1                        PIC X(90).                               
033100 01  SSA2                        PIC X(64).                               
033200 01  SSA3                        PIC X(64).                               
033300     EJECT                                                                
033400                                                                          
033500*    --- IMS FUNKTIONSKODER                                               
033600*01  -COPY W0003                                                          
033700     EJECT                                                                
033800                                                                          
033900*    ---  DLI INPUT-OUTPUT AREA                                           
034000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
034100     SKIP3                                                                
034200                                                                          
034300 01  DLI-IO-AREA-1.                                                       
034400     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
034500     SKIP3                                                                
034600                                                                          
034700     03  W6INLA01 REDEFINES IO-AREA-1.                                    
034800*        05  -COPY W6D101                                                 
034900     SKIP3                                                                
035000                                                                          
035100     03  W6INLA11 REDEFINES IO-AREA-1.                                    
035200*        05  -COPY W6D111                                                 
035300     SKIP3                                                                
035400                                                                          
035500     03  W6INLA21 REDEFINES IO-AREA-1.                                    
035600*        05  -COPY W6D121                                                 
035700     SKIP3                                                                
035800                                                                          
035900     03  W6INLB11 REDEFINES IO-AREA-1.                                    
036000*        05  -COPY W6D1B1                                                 
036100     SKIP3                                                                
036200                                                                          
036210     03  W6INLH11 REDEFINES IO-AREA-1.                                    
036220*        05  -COPY W6D1H1                                                 
036230     SKIP3                                                                
036231                                                                          
036232 01  DLI-IO-AREA-2.                                                       
036233     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
036234     SKIP3                                                                
036240                                                                          
036700     03  W6LOPA01 REDEFINES IO-AREA-2.                                    
036800*        05  -COPY W6GX01                                                 
036900     SKIP3                                                                
037000                                                                          
037100     03  W6LOPA11 REDEFINES IO-AREA-2.                                    
037200*        05  -COPY W6GX6018                                               
037300                                                                          
037310 01  DLI-IO-AREA-3.                                                       
037320     03  IO-AREA-3               PIC X(150)  VALUE SPACE.                 
037330     SKIP3                                                                
037340                                                                          
037400     03  W6PLAA01 REDEFINES IO-AREA-3.                                    
037500*        05  -COPY W6GX01                                                 
037600     SKIP3                                                                
037700                                                                          
037800     03  W6PLAA11 REDEFINES IO-AREA-3.                                    
037900*        05  -COPY W6GX6006                                               
037901                                                                          
037910 01 FILLER                       PIC X(16)   VALUE 'DLI-IO-AREA4'.        
037920     SKIP3                                                                
037930 01 DLI-IO-AREA4.                                                         
037940     03 IO-AREA4                 PIC X(900)  VALUE SPACE.                 
037950     SKIP3                                                                
037960     03 WDK601 REDEFINES IO-AREA4.                                        
037970*        05 -COPY WDK601   -PRE K6-                                       
037980     SKIP3                                                                
037990     03 WDK611 REDEFINES IO-AREA4.                                        
037991*        05 -COPY WDK611                                                  
038000     EJECT                                                                
038100                                                                          
038200 LINKAGE SECTION.                                                         
038300                                                                          
038400*01  -COPY W0009   -PRE MSG-                                              
038500     EJECT                                                                
038600                                                                          
038700*01  -COPY W0009   -PRE ALT1-                                             
038800     EJECT                                                                
038900                                                                          
039000*01  -COPY W0009   -PRE ALT2-                                             
039100     EJECT                                                                
039200                                                                          
039300*01  -COPY W0008  -PRE USEA-                                              
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600                                                                          
039700*01  -COPY W0008  -PRE INLA-                                              
039800     05  FILLER                  PIC X.                                   
039900     EJECT                                                                
040000                                                                          
040100*01  -COPY W0008  -PRE INLB-                                              
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400                                                                          
040410*01  -COPY W0008  -PRE INLB1-                                             
040420     05  FILLER                  PIC X.                                   
040430     EJECT                                                                
040440                                                                          
040500*01  -COPY W0008  -PRE INLH1-                                             
040600     05  FILLER                  PIC X.                                   
040700     EJECT                                                                
040800                                                                          
040900*01  -COPY W0008  -PRE LOPA-                                              
041000     05  FILLER                  PIC X.                                   
041100     EJECT                                                                
041200                                                                          
041300*01  -COPY W0008  -PRE PLAA-                                              
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041501                                                                          
041510*01  -COPY W0008 -PRE WDK6-                                               
041520     05 FILLER                   PIC X.                                   
041530     EJECT                                                                
042600                                                                          
042700 PROCEDURE DIVISION  USING MSG-PCB                                        
042800                           ALT1-PCB                                       
042900                           ALT2-PCB                                       
043000                           USEA-PCB                                       
043100                           INLA-PCB                                       
043200                           INLB-PCB                                       
043300                           INLB1-PCB                                      
043310                           INLH1-PCB                                      
043400                           LOPA-PCB                                       
043500                           PLAA-PCB                                       
043600                           WDK6-PCB.                                      
043800                                                                          
043900     ENTRY 'DLITCBL' USING MSG-PCB                                        
044000                           ALT1-PCB                                       
044100                           ALT2-PCB                                       
044200                           USEA-PCB                                       
044300                           INLA-PCB                                       
044400                           INLB-PCB                                       
044410                           INLB1-PCB                                      
044500                           INLH1-PCB                                      
044600                           LOPA-PCB                                       
044700                           PLAA-PCB                                       
044800                           WDK6-PCB.                                      
045000                                                                          
045100     EJECT                                                                
045200                                                                          
045300*----------------------------------------------------------------*        
045400     PERFORM IMS-GET-MSG                                                  
045500     IF SEGMENT-FINNS                                                     
045600        PERFORM A-INIT                                                    
045700        PERFORM B-KOLLA-NYCKLAR                                           
045800        IF NYCKLAR-OK                                                     
045900           IF MFS-UPDATE                                                  
046000              PERFORM G-KOLLA-INPUT                                       
046100              IF INDATA-OK                                                
046200                 PERFORM H-UPPDATERA                                      
046210                 MOVE MFS-ROER-EJ-FAELT  TO MOD-SPAR-IDLOPNRM             
046220                                            MOD-BEART                     
046230                                            MOD-KVAVIS                    
046240                                            MOD-KDSORT                    
046250                                            MOD-KDFARLIG-TXT              
046300              END-IF                                                      
046400           ELSE                                                           
046600              PERFORM F-LAES-VISA-INFO                                    
046800           END-IF                                                         
046810        ELSE                                                              
046820           PERFORM MFS-RENSA-FAELT-IN                                     
046900        END-IF                                                            
047000        PERFORM S90-BLANKUTF-NUM-FAELT                                    
047100        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
047200        PERFORM IMS-INSERT-MSG                                            
047300     END-IF                                                               
047400                                                                          
047500     MOVE ZERO TO RETURN-CODE                                             
047600     GOBACK                                                               
047700     .                                                                    
047800     EJECT                                                                
047900*----------------------------------------------------------------*        
048000 A-INIT SECTION.                                                          
048100                                                                          
048200     IF MSG-DUBBLA-TRANSKODER                                             
048300        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I12201                
048400        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
048500        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
048600     ELSE                                                                 
048700        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I12201                 
048800        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
048900        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
049000     END-IF                                                               
049100                                                                          
049200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
049300     MOVE MSG-IDPFK TO MFS-IDPFK                                          
049400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
049500                                                                          
049600     MOVE LOW-VALUE TO MSG-AREA                                           
049700     MOVE 'W6O122N1' TO MFS-IDMOD                                         
049800     MOVE '6122' TO MOD-IDTRANS                                           
049900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
050000                                                                          
050100     IF EGEN-MID OR HELP-MID                                              
050110        CONTINUE                                                          
050120     ELSE                                                                 
050200        MOVE SPACE TO MFS-KDTRTYP                                         
050300        MOVE '7' TO MFS-IDPFK                                             
050400     END-IF                                                               
050500                                                                          
050510     PERFORM AA-INIT-NYCKLAR                                              
050520                                                                          
050600     IF MSGI-IDLAND-SPR = 'GB'                                            
050700        MOVE +2 TO SPRAK-IX                                               
050800        MOVE 'GB ' TO MED-IDSKYLT                                         
050900     ELSE                                                                 
051000        MOVE +1 TO SPRAK-IX                                               
051100        MOVE 'S  ' TO MED-IDSKYLT                                         
051200     END-IF                                                               
051202     .                                                                    
051203     EJECT                                                                
051204*----------------------------------------------------------------*        
051205 AA-INIT-NYCKLAR SECTION.                                                 
051206                                                                          
051207     MOVE ALL '+' TO MSGI-WMSGINIT                                        
051208     MOVE '001'                  TO MSGI-KDCALL                           
051209     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
051210     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
051211     MOVE '6122'                 TO MSGI-IDTRANS                          
051220     IF MFS-IDTRANS = '6122'                                              
051250       MOVE MID-IDLBBET-IN   TO MSGI-IDLBBET                              
051260       MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                              
051270       MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                              
051280       MOVE MID-IDFS-IN      TO MSGI-IDFS                                 
051290     END-IF                                                               
051291     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
051300     .                                                                    
051400     EJECT                                                                
051500*----------------------------------------------------------------*        
051600 B-KOLLA-NYCKLAR SECTION.                                                 
051700                                                                          
051800     MOVE JA                 TO NYCKLAR-SW                                
051900     MOVE SPACE              TO NKLTYP-SW                                 
051901*    -- KONTROLL AV IDDC                                                  
051902                                                                          
051903     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
051904     IF MID-IDDC-IN = ALL '+'                                             
051905       MOVE MSGI-IDDC   TO WS-IDDC                                        
051906     ELSE                                                                 
051907       MOVE MID-IDDC-IN TO WS-IDDC                                        
051908       MOVE '7'         TO MFS-IDPFK                                      
051909       MOVE SPACE       TO MFS-KDTRTYP                                    
051911       MOVE JA          TO NYA-NYCKLAR-SW                                 
051912     END-IF                                                               
051913                                                                          
051916*NDC                                                                      
051917     IF CDC OR NDC                                                        
051919         MOVE WS-IDDC   TO W-IDDC                                         
051920                           WGX-IDDC                                       
051921                           MOD-IDDC-UT                                    
051922     ELSE                                                                 
051923         MOVE NEJ       TO NYCKLAR-SW                                     
051924         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
051925         PERFORM S01-CALL-WMEDKONV-FEL                                    
051930     END-IF                                                               
052000*----NKL-FÄLT-UT                                                          
052100     INSPECT MID-IDLOPNRM-IN REPLACING LEADING SPACE BY ZERO              
052200     INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO               
052400     INSPECT MID-IDLOPNRM-UT REPLACING LEADING SPACE BY ZERO              
052500     INSPECT MID-IDARTNR-UT REPLACING LEADING SPACE BY ZERO               
052700     INSPECT MOD-IDLOPNRM-UT REPLACING LEADING SPACE BY ZERO              
052800     INSPECT MOD-IDARTNR-UT REPLACING LEADING SPACE BY ZERO               
053000                                                                          
054500     IF MID-IDLOPNRM-IN = ALL '+' AND                                     
054600        MID-IDARTNR-IN  = ALL '+' AND                                     
054700        MID-IDLEVNR-IN  = ALL '+' AND                                     
054800        MID-IDFS-IN     = ALL '+'                                         
055000        IF MID-IDLOPNRM-UT = ZERO  AND                                    
055100           MID-IDARTNR-UT  = ZERO  AND                                    
055200           MID-IDLEVNR-UT  = SPACE AND                                    
055300           MID-IDFS-UT     = SPACE                                        
055400*-FEL - 401                                                               
055500           MOVE NEJ          TO NYCKLAR-SW                                
055600           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
055700           PERFORM S01-CALL-WMEDKONV-FEL                                  
055800           MOVE '7'          TO MFS-IDPFK                                 
055900           MOVE SPACE        TO MFS-KDTRTYP                               
056000        ELSE                                                              
056100*----------GAMLA NYCKLAR ELLER PF-HOPP                                    
056110           MOVE NEJ          TO NYA-NYCKLAR-SW                            
056200           PERFORM BB-FORMELLA-KONTR                                      
056300        END-IF                                                            
056400     ELSE                                                                 
056500*-------NYA NYCKLAR                                                       
056510        MOVE SPACE TO MFS-KDTRTYP                                         
056520        MOVE '7' TO MFS-IDPFK                                             
056530        MOVE JA              TO NYA-NYCKLAR-SW                            
056600        PERFORM BA-FORMELLA-KONTR                                         
056700     END-IF                                                               
056800                                                                          
056810     IF MID-ADINLOMR-PRT NOT = ALL '+' AND                                
056820        (MID-ADINLOMR-PRT NOT = SPACE OR                                  
056821        MID-ADINLOMR-PRT NOT =  LOW-VALUE)                                
056830        MOVE MID-ADINLOMR-PRT TO MOD-ADINLOMR-PRT                         
056832        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PRT-ATTR               
056840     END-IF                                                               
056841                                                                          
056842     IF MID-KDPRTVAL NOT = ALL '+'                                        
056843       MOVE MID-KDPRTVAL     TO MOD-KDPRTVAL                              
056844       MOVE MFS-ADD-LAES-IN-FAELT                                         
056845                             TO MOD-KDPRTVAL-ATTR                         
056846     ELSE                                                                 
056847       MOVE MFS-RENSA-FAELT  TO MOD-KDPRTVAL                              
056848     END-IF                                                               
056850                                                                          
056860     IF MID-IDLBBET-IN NOT = ALL '+' AND                                  
056870        MID-IDLBBET-IN NOT = SPACE                                        
056890        MOVE MID-IDLBBET-IN TO MOD-IDLBBET-UT                             
056891     ELSE                                                                 
056892        IF MID-IDLBBET-UT NOT = ALL '+' AND                               
056893           MID-IDLBBET-UT NOT = SPACE                                     
056894           MOVE MID-IDLBBET-UT TO MOD-IDLBBET-UT                          
056896        END-IF                                                            
056897     END-IF                                                               
056898                                                                          
056899     IF GODK-MID OR HELP-MID                                              
056900        CONTINUE                                                          
057000     ELSE                                                                 
057100        MOVE MFS-RENSA-FAELT    TO MOD-IDLOPNRM-UT                        
057200                                   MOD-IDARTNR-UT                         
057300                                   MOD-IDLEVNR-UT                         
057400                                   MOD-IDFS-UT                            
057500                                   MOD-IDLBBET-UT                         
057600                                   MOD-ADINLOMR-PRT                       
057700                                   MOD-TEMFSFEL                           
057800        PERFORM MFS-RENSA-FAELT-IN                                        
057900        PERFORM MFS-RENSA-FAELT-UT                                        
058000     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500*----------------------------------------------------------------*        
058600 BA-FORMELLA-KONTR SECTION.                                               
058700                                                                          
059210     PERFORM MFS-RENSA-FAELT-UT-HUV                                       
059220     PERFORM MFS-RENSA-FAELT-UT                                           
059300*----PARTINR                                                              
059400     IF MID-IDLOPNRM-IN NOT = ALL '+' AND                                 
059500        MID-IDARTNR-IN    = ALL '+' AND                                   
059600        MID-IDLEVNR-IN    = ALL '+' AND                                   
059700        MID-IDFS-IN       = ALL '+'                                       
059800        PERFORM BAA-KONTR-PARTINR                                         
059900     ELSE                                                                 
060700*-------ARTNR,LEVNR,FS                                                    
060800        IF (MID-IDLOPNRM-IN        = ALL '+') AND                         
060900           (MID-IDARTNR-IN     NOT = ALL '+' OR                           
061000            MID-IDLEVNR-IN     NOT = ALL '+' OR                           
061100            MID-IDFS-IN        NOT = ALL '+')                             
061200           PERFORM BAB-KONTR-ART-LEV-FS                                   
061300        ELSE                                                              
061310*-FEL - 401                                                               
061500           MOVE NEJ                TO NYCKLAR-SW                          
061600           MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                        
061700           PERFORM S01-CALL-WMEDKONV-FEL                                  
061710           IF MID-IDLOPNRM-IN  = ALL '+'                                  
061800              MOVE SPACE           TO MOD-IDLOPNRM-UT                     
061801           ELSE                                                           
061802              MOVE MID-IDLOPNRM-IN TO MOD-IDLOPNRM-UT                     
061810           END-IF                                                         
061820           IF MID-IDARTNR-IN     = ALL '+'                                
061900              MOVE SPACE           TO MOD-IDARTNR-UT                      
061910           ELSE                                                           
061911              MOVE MID-IDARTNR-IN  TO MOD-IDARTNR-UT                      
061920           END-IF                                                         
061930           IF MID-IDLEVNR-IN  = ALL '+'                                   
062000              MOVE SPACE           TO MOD-IDLEVNR-UT                      
062010           ELSE                                                           
062011              MOVE MID-IDLEVNR-IN  TO MOD-IDLEVNR-UT                      
062020           END-IF                                                         
062030           IF MID-IDFS-IN  = ALL '+'                                      
062100              MOVE SPACE           TO MOD-IDFS-UT                         
062200           ELSE                                                           
062201              MOVE MID-IDFS-IN     TO MOD-IDFS-UT                         
062210           END-IF                                                         
062300        END-IF                                                            
062400     END-IF                                                               
062800     .                                                                    
062900     EJECT                                                                
063000*----------------------------------------------------------------*        
063100 BAA-KONTR-PARTINR SECTION.                                               
063200                                                                          
063700     IF (MID-IDLOPNRM-IN NOT = ALL '+') AND                               
063800        (MID-IDLOPNRM-IN = ZERO OR                                        
063900         MID-IDLOPNRM-IN NOT NUMERIC)                                     
063910*-FEL - 401                                                               
064100        MOVE NEJ                   TO NYCKLAR-SW                          
064200        MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                        
064300        PERFORM S01-CALL-WMEDKONV-FEL                                     
064400        MOVE MID-IDLOPNRM-IN       TO MOD-IDLOPNRM-UT                     
064500     ELSE                                                                 
064600        MOVE '1'                   TO NKLTYP-SW                           
064700                                                                          
064800        IF MID-IDLOPNRM-IN > ZERO                                         
064900*-------NY    NKL - PARTINR                                               
065000           MOVE MID-IDLOPNRM-IN    TO MOD-IDLOPNRM-UT                     
065210                                      MOD-SPAR-IDLOPNRM                   
065300        END-IF                                                            
065400     END-IF                                                               
065500     .                                                                    
065600     EJECT                                                                
068800*----------------------------------------------------------------*        
068900 BAB-KONTR-ART-LEV-FS SECTION.                                            
069000                                                                          
069510*----NYA NYCKLAR - KONTROLLERA  EN OCH EN                                 
069520*----ARTNR                                                                
069600     IF (MID-IDARTNR-IN NOT = ALL '+') AND                                
069700        (MID-IDARTNR-IN = ZERO OR                                         
069800         MID-IDARTNR-IN NOT NUMERIC)                                      
069810*-FEL - 401                                                               
069910         MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                         
070000         MOVE NEJ               TO NYCKLAR-SW                             
070100         MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                          
070200         PERFORM S01-CALL-WMEDKONV-FEL                                    
070310     ELSE                                                                 
070311        IF MID-IDARTNR-IN NOT = ALL '+'                                   
070320*----------NY    NKL - ARTNR                                              
070330           MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                       
070340        ELSE                                                              
070350           IF MID-IDARTNR-UT NOT = ALL '+'                                
070360              MOVE MID-IDARTNR-UT TO MOD-IDARTNR-UT                       
070370           ELSE                                                           
070380              MOVE SPACE          TO MOD-IDARTNR-UT                       
070390           END-IF                                                         
070391        END-IF                                                            
070392     END-IF                                                               
070393                                                                          
070394*----LEVNR                                                                
071010     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
071020*-------NY    NKL - LEVNR                                                 
071030        MOVE MID-IDLEVNR-IN       TO MOD-IDLEVNR-UT                       
071040     ELSE                                                                 
071050        IF MID-IDLEVNR-UT NOT = ALL '+'                                   
071060           MOVE MID-IDLEVNR-UT    TO MOD-IDLEVNR-UT                       
071070        ELSE                                                              
071080           MOVE SPACE             TO MOD-IDLEVNR-UT                       
071090        END-IF                                                            
071091     END-IF                                                               
071300                                                                          
071310*----FS                                                                   
071400     IF MID-IDFS-IN NOT = ALL '+'                                         
071500*-------NY NKL - FS                                                       
071600        MOVE MID-IDFS-IN          TO MOD-IDFS-UT                          
071700     ELSE                                                                 
071800        IF MID-IDFS-UT NOT = ALL '+'                                      
071900           MOVE MID-IDFS-UT       TO MOD-IDFS-UT                          
072000        ELSE                                                              
072100           MOVE SPACE             TO MOD-IDFS-UT                          
072200        END-IF                                                            
072300     END-IF                                                               
072400                                                                          
073100     IF NYCKLAR-OK                                                        
073200        MOVE '2'                  TO NKLTYP-SW                            
073300        PERFORM BABA-KONTR-FORTS                                          
073400     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800*----------------------------------------------------------------*        
073900 BABA-KONTR-FORTS SECTION.                                                
074000                                                                          
077337*----NYA NYCKLAR - KONTROLLERA  SAMBAND                                   
077338     IF MOD-IDARTNR-UT > ZERO AND                                         
077339        MOD-IDLEVNR-UT NOT = SPACE AND                                    
077340        MOD-IDFS-UT NOT = SPACE                                           
077341        CONTINUE                                                          
077342     ELSE                                                                 
077343*-FEL - 401                                                               
077345        MOVE NEJ               TO NYCKLAR-SW                              
077346        MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                           
077347        PERFORM S01-CALL-WMEDKONV-FEL                                     
077348     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600*----------------------------------------------------------------*        
077700 BB-FORMELLA-KONTR SECTION.                                               
077800                                                                          
078500*----PARTINR                                                              
078600     IF MID-IDLOPNRM-UT > ZERO AND                                        
078700       (MID-IDARTNR-UT  = ZERO OR                                         
078800        MID-IDARTNR-UT  > ZERO) AND                                       
078900        MID-IDLEVNR-UT  = SPACE AND                                       
079000        MID-IDFS-UT     = SPACE                                           
079100        PERFORM BBA-KONTR-PARTINR                                         
079200     ELSE                                                                 
080000*-------ARTNR,LEVNR,FS                                                    
080100        IF (MID-IDLOPNRM-UT = ZERO) AND                                   
080200           (MID-IDARTNR-UT  > ZERO AND                                    
080300           MID-IDLEVNR-UT NOT = SPACE AND                                 
080400           MID-IDFS-UT  NOT = SPACE)                                      
080500           PERFORM BBB-KONTR-ART-LEV-FS                                   
080501        ELSE                                                              
080502*-FEL - 401                                                               
080520           MOVE NEJ          TO NYCKLAR-SW                                
080530           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
080540           PERFORM S01-CALL-WMEDKONV-FEL                                  
080550           MOVE '7'               TO MFS-IDPFK                            
080560           MOVE SPACE             TO MFS-KDTRTYP                          
080561           MOVE MID-IDLOPNRM-UT   TO MOD-IDLOPNRM-UT                      
080570           MOVE MID-IDARTNR-UT    TO MOD-IDARTNR-UT                       
080580           MOVE MID-IDLEVNR-UT    TO MOD-IDLEVNR-UT                       
080590           MOVE MID-IDFS-UT       TO MOD-IDFS-UT                          
080591           MOVE MID-IDLBBET-UT    TO MOD-IDLBBET-UT                       
080592           PERFORM MFS-RENSA-FAELT-IN                                     
080600        END-IF                                                            
080800     END-IF                                                               
080900     .                                                                    
081000     EJECT                                                                
081100*----------------------------------------------------------------*        
081200 BBA-KONTR-PARTINR SECTION.                                               
081300                                                                          
081700     MOVE '1'                      TO NKLTYP-SW                           
081800     IF MID-IDLOPNRM-IN = ALL '+' AND                                     
081900        MID-IDLOPNRM-UT > ZERO                                            
082000*-------GML NKL - PARTINR                                                 
082100        MOVE MID-IDLOPNRM-UT       TO MOD-IDLOPNRM-UT                     
082110                                      MOD-SPAR-IDLOPNRM                   
082200        MOVE MID-IDARTNR-UT        TO MOD-IDARTNR-UT                      
082300     END-IF                                                               
082600     .                                                                    
082700     EJECT                                                                
084400*----------------------------------------------------------------*        
084500 BBB-KONTR-ART-LEV-FS SECTION.                                            
084600                                                                          
085500     MOVE '2'                     TO NKLTYP-SW                            
085510     MOVE MID-SPAR-IDLOPNRM       TO MOD-SPAR-IDLOPNRM                    
085520                                                                          
085600     IF MID-IDARTNR-IN = ALL '+' AND                                      
085700        MID-IDARTNR-UT > ZERO                                             
085800*-------GML NKL - ARTNR                                                   
085900        MOVE MID-IDARTNR-UT       TO MOD-IDARTNR-UT                       
086030     END-IF                                                               
086100                                                                          
086200     IF MID-IDLEVNR-IN = ALL '+' AND                                      
086210        MID-IDLEVNR-UT NOT = (SPACE OR LOW-VALUE)                         
086400*------GML NKL - LEVNR                                                    
086510        MOVE MID-IDLEVNR-UT       TO MOD-IDLEVNR-UT                       
086600     END-IF                                                               
086700                                                                          
086800     IF MID-IDFS-IN = ALL '+' AND                                         
086900        MID-IDFS-UT NOT = (SPACE OR LOW-VALUE)                            
087000*------GML NKL - FS                                                       
087110        MOVE MID-IDFS-UT          TO MOD-IDFS-UT                          
087200     END-IF                                                               
087400     .                                                                    
087500     EJECT                                                                
087700*----------------------------------------------------------------*        
087800 F-LAES-VISA-INFO SECTION.                                                
087900                                                                          
089101*----INITIERA                                                             
089108     MOVE JA                       TO INDATA-SW                           
089113                                                                          
089119     PERFORM MFS-RENSA-FAELT-UT                                           
089120     PERFORM S12-INIT-NKL                                                 
089610*----LÄS BEROENDE PÅ NKLTYP                                               
089620     IF NKLTYP1                                                           
089630        PERFORM FA-BEARB-NKLTYP1                                          
089640     END-IF                                                               
089650     IF NKLTYP2                                                           
089660        PERFORM FB-BEARB-NKLTYP2                                          
089670     END-IF                                                               
089680     IF HELP-MID                                                          
089690        PERFORM FC-MID-TILL-MOD                                           
089691     END-IF                                                               
089700                                                                          
089800     IF EGEN-MID OR HELP-MID                                              
089810        IF MID-RAD(1) = ALL '+' AND                                       
089900           MID-RAD(2) = ALL '+' AND                                       
090000           MID-RAD(3) = ALL '+' AND                                       
090100           MID-RAD(4) = ALL '+' AND                                       
090200           MID-RAD(5) = ALL '+' AND                                       
090300           MID-RAD(6) = ALL '+' AND                                       
090400           MID-RAD(7) = ALL '+' AND                                       
090500           MID-RAD(8) = ALL '+' AND                                       
090600           MID-RAD(9) = ALL '+' AND                                       
090700           MID-RAD(10) = ALL '+' AND                                      
090800           MID-RAD(11) = ALL '+' AND                                      
090900           MID-RAD(12) = ALL '+'                                          
091000           CONTINUE                                                       
091800        ELSE                                                              
091801           MOVE JA TO MID-SW                                              
091810           IF MFS-FIRST OR HELP-MID                                       
091811              CONTINUE                                                    
091820           ELSE                                                           
091830              IF INDATA-OK                                                
091900                 MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                      
092000                 PERFORM S01-CALL-WMEDKONV-FEL                            
092200                 PERFORM MFS-ROER-EJ-FAELT-UT                             
092300                 PERFORM MFS-LAES-IN-IGEN                                 
092400              END-IF                                                      
092401           END-IF                                                         
092410        END-IF                                                            
092411     END-IF                                                               
092420     PERFORM MFS-RENSA-FAELT-IN                                           
092430     IF INDATA-FEL                                                        
092431        PERFORM MFS-RENSA-FAELT-UT                                        
092440     END-IF                                                               
092600     .                                                                    
092700     EJECT                                                                
093003*----------------------------------------------------------------*        
093004 FA-BEARB-NKLTYP1 SECTION.                                                
093005                                                                          
093100     PERFORM IMS-GU-INLB-D111                                             
093200                                                                          
093300     IF SEGMENT-SAKNAS                                                    
093310*-FEL - 010                                                               
093400*------ ART SAKNAS PÅ ANGIVET PARTI                                       
093500        MOVE NEJ                     TO INDATA-SW                         
093600        MOVE ERR-SAKN-I-REG        TO MED-IDMFSFEL                        
093700        MOVE MFS-RENSA-FAELT       TO MOD-TEMFSINF                        
093800        PERFORM S01-CALL-WMEDKONV-FEL                                     
093900        PERFORM MFS-RENSA-FAELT-UT                                        
094000     ELSE                                                                 
094010        IF ART-FLKLAR = JA                                                
094011*-FEL - 010                                                               
094012*----------FLKLAR = JA PÅ W6INLA11                                        
094013           MOVE NEJ                  TO INDATA-SW                         
094014           MOVE ERR-SAKN-I-REG     TO MED-IDMFSFEL                        
094015           MOVE MFS-RENSA-FAELT    TO MOD-TEMFSINF                        
094016           PERFORM S01-CALL-WMEDKONV-FEL                                  
094017           PERFORM MFS-RENSA-FAELT-UT                                     
094020        ELSE                                                              
094100           PERFORM S20-FLYTTA-SEGM-11-ART-UPPG                            
094300           MOVE W-ARTNR-X         TO MOD-IDARTNR-UT                       
094310           IF ART-ADLAGOMR = ZERO                                         
094320*-FEL - 764                                                               
094500*-------------PLATS/PLAC SAKNAS PÅ ANGIVET PARTI                          
094600              MOVE NEJ               TO INDATA-SW                         
094700              MOVE ERR-PLATS-SAKN    TO MED-IDMFSFEL                      
094800              PERFORM S01-CALL-WMEDKONV-FEL                               
095000           END-IF                                                         
095100        END-IF                                                            
095110     END-IF                                                               
095200     .                                                                    
095300     EJECT                                                                
095400*----------------------------------------------------------------*        
095500*----------------------------------------------------------------*        
095600*  - VID LÄSNING MED NYCKELTYP 2 (ARTNR,LEVNR,FS)                         
095700*    LÄSES INDEX-H-BASEN KVAL MED HJÄLP AV                                
095800*    FYSISK NKL OCH INLH1-PCB                                             
095900*    IDARTNR, IDLEVNR OCH IDFS SAMT TIAVIDAT > 0                          
096000*  - VID TRÄFF LÄSES SEDAN VIA INLA PCB PÅ VANLIG VÄG                     
096100*    OBS. FLERA FÖREKOMSTER KAN FINNAS PGA TIAVIDAT                       
096200*  - OM FLERA FÖREKOMSTER FINNS FÅR BARA 'EN/1' HA                        
096300*    TIINLMOT > ZERO/0                                                    
096400*----------------------------------------------------------------*        
096500 FB-BEARB-NKLTYP2 SECTION.                                                
096600                                                                          
096900*--- LÄSNING MED OKVALIFICERAT TIAVIDAT                                   
097000*    (LÄSES > MED WH1-MIN/MAX)                                            
097200     PERFORM IMS-GU-INLH1-D111-X                                          
097300                                                                          
097400     IF SEGMENT-SAKNAS                                                    
097410*-FEL - 010                                                               
097500*------ ART SAKNAS                                                        
097600        MOVE NEJ                     TO INDATA-SW                         
097700        PERFORM MFS-RENSA-FAELT-UT                                        
097710        MOVE ERR-SAKN-I-REG        TO MED-IDMFSFEL                        
097900        MOVE MFS-RENSA-FAELT       TO MOD-TEMFSINF                        
098000        PERFORM S01-CALL-WMEDKONV-FEL                                     
098100     ELSE                                                                 
098200*------INITIERA  FÖR DIREKT-LÄSNING PÅ INLA-D111                          
098400        MOVE MOD-IDLEVNR-UT        TO W-IDLEVNR                           
098500        MOVE MOD-IDFS-UT           TO W-IDFS                              
098510        MOVE W-IDDC                TO W-IDDC-101KY                        
098600        MOVE SEQH-TIAVIDAT         TO W-TIAVIDAT                          
098700        MOVE MOD-IDARTNR-UT        TO W-IDARTNR                           
098800        MOVE SEQH-TIAVIDAT         TO WH1-MIN-TIAVIDAT                    
098900        MOVE SEQH-IDRADNR-INL      TO WH1-MIN-IDRADNR-INL                 
099000                                      W-IDRADNR-INL                       
099500                                                                          
099600        PERFORM FBA-LAES-INLA                                             
099700        IF INDATA-OK                                                      
099701           IF IDLOPNR-RAKN = ZERO                                         
099710*-FEL - 010                                                               
099720*-------------INGET    PARTI FINNS FÖR ANG 'ART-LEV-FS'                   
099730              MOVE NEJ               TO INDATA-SW                         
099740              MOVE ERR-SAKN-I-REG    TO MED-IDMFSFEL                      
099750              MOVE MFS-RENSA-FAELT   TO MOD-TEMFSINF                      
099760              PERFORM S01-CALL-WMEDKONV-FEL                               
099770           ELSE                                                           
099800              IF IDLOPNR-RAKN > 1                                         
099810*-FEL - 226                                                               
099900*----------------FLERA    PARTIER FINNS FÖR ANG 'ART-LEV-FS'              
100000                 MOVE NEJ            TO INDATA-SW                         
100200                 MOVE ERR-ANGE-PARTI  TO MED-IDMFSFEL                     
100300                 MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                     
100400                 PERFORM S01-CALL-WMEDKONV-FEL                            
100500              END-IF                                                      
100570           END-IF                                                         
100580        END-IF                                                            
100600     END-IF                                                               
100800     .                                                                    
100900     EJECT                                                                
101000*----------------------------------------------------------------*        
101100 FBA-LAES-INLA     SECTION.                                               
101200                                                                          
101500     MOVE NEJ                      TO SLUT-SW                             
101600     MOVE ZERO                     TO IDLOPNR-RAKN                        
101700                                                                          
101800     PERFORM IMS-GU-INLA-D111                                             
102000     IF SEGMENT-FINNS AND                                                 
102001        ART-IDDC      =  W-IDDC                                           
102010        IF ART-FLKLAR = JA                                                
102020*-FEL - 010                                                               
102030*----------FLKLAR = JA PÅ W6INLA11                                        
102040           MOVE NEJ                  TO INDATA-SW                         
102050           MOVE ERR-SAKN-I-REG     TO MED-IDMFSFEL                        
102060           MOVE MFS-RENSA-FAELT    TO MOD-TEMFSINF                        
102070           PERFORM S01-CALL-WMEDKONV-FEL                                  
102080           PERFORM MFS-RENSA-FAELT-UT                                     
102090        ELSE                                                              
102100           IF ART-ADLAGOMR > ZERO                                         
102200              IF ART-IDLOPNRM > ZERO                                      
102300                 PERFORM S20-FLYTTA-SEGM-11-ART-UPPG                      
102400                 ADD 1 TO IDLOPNR-RAKN                                    
102500              END-IF                                                      
102600           ELSE                                                           
102610*-FEL - 764                                                               
102700*-------------PLATS     SAKNAS                                            
102800              MOVE NEJ               TO INDATA-SW                         
102810              MOVE ERR-PLATS-SAKN    TO MED-IDMFSFEL                      
103000              PERFORM S01-CALL-WMEDKONV-FEL                               
103200           END-IF                                                         
103300           PERFORM UNTIL SLUT-JA OR INDATA-FEL                            
103400              PERFORM IMS-GN-INLH1-D111-X                                 
103500              IF SEGMENT-FINNS                                            
103600                 MOVE SEQH-TIAVIDAT TO WH1-MIN-TIAVIDAT                   
103700                 MOVE SEQH-TIAVIDAT TO W-TIAVIDAT                         
103800                 PERFORM IMS-GU-INLA-D111                                 
103801                 IF SEGMENT-FINNS                                         
103810                    IF ART-FLKLAR = JA                                    
103820*-FEL  - 010                                                              
103830*----------------------FLKLAR = JA PÅ W6INLA11                            
103840                       MOVE NEJ      TO INDATA-SW                         
103850                       MOVE ERR-SAKN-I-REG TO MED-IDMFSFEL                
103860                       MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF               
103870                       PERFORM S01-CALL-WMEDKONV-FEL                      
103880                       PERFORM MFS-RENSA-FAELT-UT                         
103890                    ELSE                                                  
104000                       IF ART-ADLAGOMR > ZERO                             
104100                          IF ART-IDLOPNRM > ZERO                          
104200                             PERFORM S20-FLYTTA-SEGM-11-ART-UPPG          
104300                             ADD 1 TO IDLOPNR-RAKN                        
104400                          END-IF                                          
104500                       ELSE                                               
104510*-FEL - 764                                                               
104600*-------------------------PLATS       SAKNAS                              
104700                          MOVE NEJ   TO INDATA-SW                         
104710                          MOVE ERR-PLATS-SAKN TO MED-IDMFSFEL             
104900                          PERFORM S01-CALL-WMEDKONV-FEL                   
105100                       END-IF                                             
105200                    END-IF                                                
105210                 END-IF                                                   
105300              ELSE                                                        
105400                 IF INLH1-STATUS-CODE = 'GE'                              
105500                    MOVE JA        TO SLUT-SW                             
105600                 END-IF                                                   
105710              END-IF                                                      
105800           END-PERFORM                                                    
105900        END-IF                                                            
106000     END-IF                                                               
106700     .                                                                    
106800     EJECT                                                                
106900*----------------------------------------------------------------*        
107000 FC-MID-TILL-MOD  SECTION.                                                
107120                                                                          
107130     MOVE MID-SPAR-IDLOPNRM  TO MOD-SPAR-IDLOPNRM                         
107132                                                                          
107133     MOVE  +1             TO IX                                           
107134     PERFORM UNTIL IX > MAX-IX                                            
107135     INSPECT MID-KVFLETI(IX) REPLACING LEADING SPACE BY ZERO              
107136     INSPECT MID-KVINLART(IX) REPLACING LEADING SPACE BY ZERO             
107137        IF MID-KVFLETI(IX) = ALL '+'                                      
107138           MOVE MFS-RENSA-FAELT       TO MOD-KVFLETI(IX)                  
107140        ELSE                                                              
107141           MOVE MID-KVFLETI(IX)       TO MOD-KVFLETI(IX)                  
107143           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVFLETI-ATTR(IX)             
107145        END-IF                                                            
107146        IF MID-KVINLART(IX) = ALL '+'                                     
107147           MOVE MFS-RENSA-FAELT       TO MOD-KVINLART(IX)                 
107149        ELSE                                                              
107151           MOVE MID-KVINLART(IX)      TO MOD-KVINLART(IX)                 
107152           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVINLART-ATTR(IX)            
107154        END-IF                                                            
107155        ADD 1             TO IX                                           
107160     END-PERFORM                                                          
107200     .                                                                    
107300     EJECT                                                                
107910*----------------------------------------------------------------*        
107920 G-KOLLA-INPUT SECTION.                                                   
107930                                                                          
108001     MOVE JA                 TO INDATA-SW                                 
108019     PERFORM GD-KOLLA-INPUT                                               
108020     PERFORM GE-KOLLA-INLEVREG                                            
108022     IF MID-KDPRTVAL = JA OR YES                                          
108031       MOVE ART-IDARTNR      TO W-IDARTNR                                 
108032       PERFORM IMS-GU-WDK611                                              
108033       IF      SEGMENT-FINNS                                              
108034       AND CLAG-ADLAGOMR-SVS > ZERO                                       
108035         CONTINUE                                                         
108036       ELSE                                                               
108037         MOVE MFS-ALFA-FAELT-FEL                                          
108038                             TO MOD-KDPRTVAL-ATTR                         
108039         MOVE NEJ            TO INDATA-SW                                 
108040       END-IF                                                             
108041     END-IF                                                               
108042     IF INDATA-OK                                                         
108043        IF MID-RAD(1) NOT = ALL '+' OR                                    
108044           MID-RAD(2) NOT = ALL '+' OR                                    
108045           MID-RAD(3) NOT = ALL '+' OR                                    
108046           MID-RAD(4) NOT = ALL '+' OR                                    
108047           MID-RAD(5) NOT = ALL '+' OR                                    
108048           MID-RAD(6) NOT = ALL '+' OR                                    
108049           MID-RAD(7) NOT = ALL '+' OR                                    
108050           MID-RAD(8) NOT = ALL '+' OR                                    
108051           MID-RAD(9) NOT = ALL '+' OR                                    
108052           MID-RAD(10) NOT = ALL '+' OR                                   
108053           MID-RAD(11) NOT = ALL '+' OR                                   
108054           MID-RAD(12) NOT = ALL '+'                                      
108055           PERFORM GC-KOLLA-PRINTER                                       
108056           IF INDATA-OK                                                   
108057              PERFORM GA-KOLLA-RAD1                                       
108058              IF INDATA-OK                                                
108059                 PERFORM GB-KOLLA-RADER                                   
108060              ELSE                                                        
108061                 PERFORM MFS-LAES-IN-IGEN                                 
108062              END-IF                                                      
108063           ELSE                                                           
108064              PERFORM MFS-LAES-IN-IGEN                                    
108065           END-IF                                                         
108066        END-IF                                                            
108067                                                                          
108068     END-IF                                                               
108069     IF INDATA-FEL                                                        
108070        PERFORM MFS-ROER-EJ-FAELT-UT                                      
108071        PERFORM MFS-ROER-EJ-FAELT-IN                                      
108072     END-IF                                                               
108073     PERFORM MFS-RENSA-FAELT-IN                                           
108074     .                                                                    
108075     EJECT                                                                
108076*----------------------------------------------------------------*        
108077 GA-KOLLA-RAD1         SECTION.                                           
108078                                                                          
108079     MOVE ZERO               TO SPAR-RAD1-KVINLART                        
108080     MOVE MID-SPAR-IDLOPNRM  TO WB-IDLOPNRM                               
108081                                                                          
108082     PERFORM IMS-GU-INLB-D111                                             
108084                                                                          
108085     IF SEGMENT-SAKNAS                                                    
108086*-FEL - 010                                                               
108087*-------PARTI SAKNAS                                                      
108088        MOVE NEJ                     TO INDATA-SW                         
108089        MOVE ERR-SAKN-I-REG          TO MED-IDMFSFEL                      
108090        MOVE MFS-ALFA-FAELT-FEL      TO MOD-KVFLETI-ATTR(1)               
108091                                        MOD-KVINLART-ATTR(1)              
108093        PERFORM S01-CALL-WMEDKONV-FEL                                     
108094     ELSE                                                                 
108095        MOVE ART-PRARTSTD      TO SPAR-PRARTSTD                           
108096        MOVE ART-VKART         TO SPAR-VKART                              
108097        MOVE ART-ADLAGOMR      TO SPAR-ADLAGOMR                           
108098        MOVE ART-ADGANG        TO SPAR-ADGANG                             
108099        MOVE ART-ADPLATS       TO SPAR-ADPLATS                            
108100        MOVE ART-BEFT          TO SPAR-BEFT                               
108101        MOVE ART-KDSORT        TO SPAR-KDSORT                             
108102        MOVE ART-IDRADNR-INL   TO W-IDRADNR-INL                           
108103        MOVE ZERO              TO W-IDRADNR                               
108104        PERFORM IMS-GNP-INLB-D121                                         
108105                                                                          
108106        IF SEGMENT-FINNS                                                  
108107           IF RAD-IDRADNR = 1                                             
108108              IF (RAD-KDINLSTA NOT = ('FPK' OR 'SAK' OR SPACE))           
108109                 AND (RAD-IDOKOLLI = ZERO)                                
108110                 MOVE RAD-KVINLART     TO SPAR-RAD1-KVINLART              
108112              ELSE                                                        
108113*-FEL - 007                                                               
108114*----------------STATUS/KOLLI FEL                                         
108115                 MOVE NEJ                   TO INDATA-SW                  
108116                 MOVE ERR-UPDATE-NOT-VALID  TO MED-IDMFSFEL               
108117                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KVFLETI-ATTR(1)           
108118                                            MOD-KVINLART-ATTR(1)          
108119                 PERFORM S01-CALL-WMEDKONV-FEL                            
108120              END-IF                                                      
108121           ELSE                                                           
108122*-FEL - 178                                                               
108123              MOVE NEJ                 TO INDATA-SW                       
108124              MOVE ERR-RADNR-1-SAKN    TO MED-IDMFSFEL                    
108125              MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVFLETI-ATTR(1)             
108126                                          MOD-KVINLART-ATTR(1)            
108128              PERFORM S01-CALL-WMEDKONV-FEL                               
108129           END-IF                                                         
108130        END-IF                                                            
108131     END-IF                                                               
108133     .                                                                    
108134     EJECT                                                                
108135*----------------------------------------------------------------*        
108136 GB-KOLLA-RADER    SECTION.                                               
108137                                                                          
108142     MOVE NEJ                TO FLER-FL-SW                                
108143     MOVE ZERO               TO SPAR-ACK-KVINLART                         
108144     MOVE ZERO               TO SPAR-ACK-KVFLETI                          
108145     MOVE ZERO               TO IX                                        
108146     ADD 1                   TO IX                                        
108147                                                                          
108148     PERFORM UNTIL IX > MAX-IX                                            
108154        IF MID-RAD(IX) NOT = ALL '+'                                      
108155           PERFORM GBA-KOLLA-RAD-NUM                                      
108156           IF INDATA-OK                                                   
108157              MOVE ZERO            TO SPAR-KVFLETI                        
108158              MOVE ZERO            TO SPAR-KVINLART                       
108159              MOVE MID-KVFLETI(IX) TO SPAR-KVFLETI-X                      
108160              MOVE MID-KVINLART(IX) TO SPAR-RED-KVINLART                  
108163              COMPUTE SPAR-KVINLART = SPAR-KVINLART * SPAR-KVFLETI        
108165              ADD SPAR-KVINLART    TO SPAR-ACK-KVINLART                   
108167              ADD SPAR-KVFLETI     TO SPAR-ACK-KVFLETI                    
108168           END-IF                                                         
108169        END-IF                                                            
108171        ADD 1 TO IX                                                       
108172     END-PERFORM                                                          
108173                                                                          
108177     IF SPAR-ACK-KVFLETI > 100                                            
108178*-FEL - 195                                                               
108179*-------SPÄRR PÅ ANTAL FLAGGOR - FÅR EJ VARA > 100                        
108180        MOVE NEJ               TO INDATA-SW                               
108181        MOVE ERR-KVFLETI-EJ-OK TO MED-IDMFSFEL                            
108182        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVFLETI-ATTR(1)                   
108183        PERFORM S01-CALL-WMEDKONV-FEL                                     
108184     END-IF                                                               
108185                                                                          
108186     IF SPAR-ACK-KVINLART > SPAR-RAD1-KVINLART                            
108187*-FEL - 181                                                               
108188*-------SCAN/REG ANTAL STÖRRE ÄN BEFINTLIGT                               
108189        MOVE NEJ               TO INDATA-SW                               
108190        MOVE ERR-ANT-EJ-OK     TO MED-IDMFSFEL                            
108191        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVINLART-ATTR(1)                  
108192        PERFORM S01-CALL-WMEDKONV-FEL                                     
108193     END-IF                                                               
108194                                                                          
108195*-------SCAN/REG ANTAL STÖRRE ÄN BEFINTLIGT                               
108196     IF SPAR-ACK-KVINLART < SPAR-RAD1-KVINLART                            
108197        MOVE JA              TO FLER-FL-SW                                
108198     END-IF                                                               
108199     .                                                                    
108200     EJECT                                                                
108201*----------------------------------------------------------------*        
108202 GBA-KOLLA-RAD-NUM   SECTION.                                             
108203                                                                          
108204     INSPECT MID-KVFLETI(IX) REPLACING LEADING SPACE BY ZERO              
108205     INSPECT MID-KVINLART(IX) REPLACING LEADING SPACE BY ZERO             
108206                                                                          
108207     IF (MID-KVFLETI(IX) NOT = ALL '+' AND                                
108208         MID-KVFLETI(IX) NUMERIC       AND                                
108209         MID-KVFLETI(IX) > ZERO)                                          
108210*-------OK                                                                
108211        MOVE MFS-NUM-FAELT-RAETT       TO MOD-KVFLETI-ATTR(IX)            
108212     ELSE                                                                 
108213*-FEL - 002                                                               
108214        MOVE NEJ                           TO INDATA-SW                   
108215        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
108216        PERFORM S01-CALL-WMEDKONV-FEL                                     
108217        MOVE MFS-NUM-FAELT-FEL    TO MOD-KVFLETI-ATTR(IX)                 
108218     END-IF                                                               
108219     IF (MID-KVINLART(IX) NOT = ALL '+' AND                               
108220         MID-KVINLART(IX) NUMERIC       AND                               
108221         MID-KVINLART(IX) > ZERO)                                         
108222*-------OK                                                                
108223        MOVE MFS-NUM-FAELT-RAETT       TO MOD-KVINLART-ATTR(IX)           
108224     ELSE                                                                 
108225*-FEL - 002                                                               
108226        MOVE NEJ                           TO INDATA-SW                   
108227        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
108228        PERFORM S01-CALL-WMEDKONV-FEL                                     
108229        MOVE MFS-NUM-FAELT-FEL    TO MOD-KVINLART-ATTR(IX)                
108230                                     MOD-KVINLART-ATTR(IX)                
108231     END-IF                                                               
108240     .                                                                    
108300     EJECT                                                                
108480*----------------------------------------------------------------*        
108481 GC-KOLLA-PRINTER SECTION.                                                
108482                                                                          
108600     IF MID-ADINLOMR-PRT NOT = ALL '+' AND                                
108700        (MID-ADINLOMR-PRT NOT = SPACE OR                                  
108710        MID-ADINLOMR-PRT NOT = LOW-VALUE)                                 
108900        PERFORM GCA-KOLLA-PLAC-REG                                        
109100        IF SEGMENT-SAKNAS                                                 
109110*-FEL - 772                                                               
109200*----------PRINTER-PLACERING SAKNAS                                       
109300           MOVE NEJ                    TO INDATA-SW                       
109400           MOVE ERR-PRINT-SAKN         TO MED-IDMFSFEL                    
109800           MOVE MFS-ALFA-FAELT-FEL     TO MOD-ADINLOMR-PRT-ATTR           
109810           PERFORM S01-CALL-WMEDKONV-FEL                                  
109900        ELSE                                                              
110000           MOVE 6006-IDLEVNR           TO SPAR-IDLEVNR-NEW                
110100           PERFORM GCB-KOLLA-PRINTER                                      
110200           IF PRT-KDSVAR = 'R'                                            
110201              MOVE PRT-BEPRTLST        TO MOD-TEMFSFEL                    
110202              MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PRT-ATTR          
110203           ELSE                                                           
110210*-FEL - 772                                                               
110300*-------------W006PRT-SAKNAS                                              
110400              MOVE NEJ                 TO INDATA-SW                       
110600              MOVE ERR-PRINT-SAKN      TO MED-IDMFSFEL                    
110900              MOVE MFS-ALFA-FAELT-FEL  TO MOD-ADINLOMR-PRT-ATTR           
110910              PERFORM S01-CALL-WMEDKONV-FEL                               
111000           END-IF                                                         
111100        END-IF                                                            
111200     ELSE                                                                 
111210*-FEL - 772                                                               
111300*------ PRINTER-PLACERING OBL                                             
111400        MOVE NEJ                       TO INDATA-SW                       
111500        MOVE ERR-PRINT-SAKN            TO MED-IDMFSFEL                    
111900        MOVE MFS-ALFA-FAELT-FEL        TO MOD-ADINLOMR-PRT-ATTR           
111910        PERFORM S01-CALL-WMEDKONV-FEL                                     
112000     END-IF                                                               
112300     .                                                                    
112400     EJECT                                                                
112520*----------------------------------------------------------------*        
112600 GCA-KOLLA-PLAC-REG SECTION.                                              
112700                                                                          
112800     MOVE '6005'             TO WGX-IDHTYP                                
113000     MOVE MID-ADINLOMR-PRT   TO W-ADINLOMR                                
113100                                                                          
113200     PERFORM IMS-GU-PLAA-G111                                             
113300     .                                                                    
113400     EJECT                                                                
113500*----------------------------------------------------------------*        
113600 GCB-KOLLA-PRINTER SECTION.                                               
113700                                                                          
113800     MOVE 001                TO PRT-KDCALL                                
113810     MOVE '6F'               TO WS-IDPRT1                                 
113900     MOVE MID-ADINLOMR-PRT   TO WS-IDPRT2                                 
114000     MOVE SPACE              TO WS-IDPRT3                                 
114100     MOVE WS-IDPRT           TO PRT-IDPRTLST                              
114200     MOVE SPACE              TO PRT-IDLTERM                               
114300                                                                          
114400     CALL W006PRT USING PRT-W006PRT                                       
114900     .                                                                    
115000     EJECT                                                                
115100*----------------------------------------------------------------*        
115200 GD-KOLLA-INPUT    SECTION.                                               
115300                                                                          
115400     IF (MID-RAD(1)     = ALL '+' OR                                      
115410         MID-RAD(1)     = SPACE   OR                                      
115420         MID-RAD(1)     = LOW-VALUE) AND                                  
115500        (MID-RAD(2)     = ALL '+' AND                                     
115510         MID-RAD(2)     = SPACE   OR                                      
115520         MID-RAD(2)     = LOW-VALUE) AND                                  
115600        (MID-RAD(3)     = ALL '+' AND                                     
115610         MID-RAD(3)     = SPACE   OR                                      
115620         MID-RAD(3)     = LOW-VALUE) AND                                  
115700        (MID-RAD(4)     = ALL '+' AND                                     
115710         MID-RAD(4)     = SPACE   OR                                      
115720         MID-RAD(4)     = LOW-VALUE) AND                                  
115800        (MID-RAD(5)     = ALL '+' AND                                     
115810         MID-RAD(5)     = SPACE   OR                                      
115820         MID-RAD(5)     = LOW-VALUE) AND                                  
115900        (MID-RAD(6)     = ALL '+' AND                                     
115910         MID-RAD(6)     = SPACE   OR                                      
115920         MID-RAD(6)     = LOW-VALUE) AND                                  
116000        (MID-RAD(7)     = ALL '+' AND                                     
116010         MID-RAD(7)     = SPACE   OR                                      
116020         MID-RAD(7)     = LOW-VALUE) AND                                  
116100        (MID-RAD(8)     = ALL '+' AND                                     
116110         MID-RAD(8)     = SPACE   OR                                      
116120         MID-RAD(8)     = LOW-VALUE) AND                                  
116200        (MID-RAD(9)     = ALL '+' AND                                     
116210         MID-RAD(9)     = SPACE   OR                                      
116220         MID-RAD(9)     = LOW-VALUE) AND                                  
116300        (MID-RAD(10)    = ALL '+' AND                                     
116310         MID-RAD(10)    = SPACE   OR                                      
116320         MID-RAD(10)    = LOW-VALUE) AND                                  
116400        (MID-RAD(11)    = ALL '+' AND                                     
116410         MID-RAD(11)    = SPACE   OR                                      
116420         MID-RAD(11)    = LOW-VALUE) AND                                  
116500        (MID-RAD(12)    = ALL '+' OR                                      
116501         MID-RAD(12)    = SPACE   OR                                      
116502         MID-RAD(12)    = LOW-VALUE)                                      
116503                                                                          
116504        MOVE NEJ                   TO INDATA-SW                           
116505        MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                        
116506        PERFORM S01-CALL-WMEDKONV-FEL                                     
116507     END-IF                                                               
116508     .                                                                    
116509     EJECT                                                                
116510*----------------------------------------------------------------*        
116511 GE-KOLLA-INLEVREG SECTION.                                               
116512                                                                          
116513     MOVE MID-SPAR-IDLOPNRM  TO WB-IDLOPNRM                               
116514                                                                          
116515     PERFORM IMS-GU-INLB-D111                                             
116516     IF SEGMENT-FINNS                                                     
116517       CONTINUE                                                           
116518     ELSE                                                                 
116519       MOVE NEJ TO INDATA-SW                                              
116520        MOVE ERR-SAKN-I-REG        TO MED-IDMFSFEL                        
116521        MOVE MFS-RENSA-FAELT       TO MOD-TEMFSINF                        
116522        PERFORM S01-CALL-WMEDKONV-FEL                                     
116523        PERFORM MFS-RENSA-FAELT-UT                                        
116524     END-IF                                                               
116525                                                                          
116526     .                                                                    
116527     EJECT                                                                
116528******************************************************************        
116529*  UPPDATERA REGISTER                                            *        
116530******************************************************************        
116540*----------------------------------------------------------------*        
116550 H-UPPDATERA SECTION.                                                     
116560                                                                          
116580     MOVE ZERO           TO T91-MID-KVPOST                                
116593     MOVE ZERO           TO T94-MID-KVPOST                                
116594     MOVE NEJ            TO TRANS91-SW                                    
116595     MOVE NEJ            TO TRANS94-SW                                    
116597     MOVE JA             TO FOERSTA-T91-SW                                
116598     MOVE JA             TO FOERSTA-T94-SW                                
116599                                                                          
116622     MOVE ZERO           TO IX                                            
116623     ADD  1              TO IX                                            
116624     PERFORM HA-INIT                                                      
116625     PERFORM HE-HAEMTA-KOLLINR                                            
116626*----LÄS SISTA RAD-SEGMENT FÖR UPPDATERING                                
116627     PERFORM HB-LAES-SISTA-D121                                           
116628                                                                          
116629     PERFORM UNTIL IX > MAX-IX                                            
116636        IF MID-RAD(IX) NOT = ALL '+' AND                                  
116637           MID-RAD(IX) > ZERO                                             
116638                                                                          
116639           IF SEGMENT-FINNS                                               
116640              MOVE ZERO            TO SPAR-KVFLETI                        
116641              MOVE MID-KVFLETI(IX) TO SPAR-KVFLETI-X                      
116642              MOVE +1 TO FL-IX                                            
116643              PERFORM UNTIL FL-IX > SPAR-KVFLETI                          
116644                 PERFORM HC-ISRT-NY-RAD                                   
116645                 ADD 1 TO FL-IX                                           
116646              END-PERFORM                                                 
116647           END-IF                                                         
116648        END-IF                                                            
116649        ADD 1  TO IX                                                      
116650     END-PERFORM                                                          
116651                                                                          
116652     PERFORM HD-AVSLUTA-RAD1                                              
116653     PERFORM HF-UPPDAT-KOLLINR                                            
116654     PERFORM HG-SKICKA-TRANSAR                                            
116655     IF FLER-FL-JA                                                        
116656        MOVE INF-FLER-FLAGGOR  TO MED-IDMFSINF                            
116657        PERFORM S02-CALL-WMEDKONV-INF                                     
116659     END-IF                                                               
116662     PERFORM MFS-RENSA-FAELT-UT                                           
116664     .                                                                    
116665     EJECT                                                                
116666*----------------------------------------------------------------*        
116667 HA-INIT      SECTION.                                                    
116668                                                                          
116670     PERFORM HAA-INIT-INLA                                                
116671     PERFORM HAB-INIT-RAD1                                                
116672     .                                                                    
116673     EJECT                                                                
116674*----------------------------------------------------------------*        
116675 HAA-INIT-INLA    SECTION.                                                
116676                                                                          
116679     MOVE MID-SPAR-IDLOPNRM    TO W-IDLOPNRM                              
116699     PERFORM IMS-GU-INLB1-D111-X                                          
116700                                                                          
116701     IF SEGMENT-FINNS                                                     
116702*-------DIREKT-LÄSNING AV INLA-D101                                       
116705        MOVE SEQB-IDLEVNR           TO W-IDLEVNR                          
116706        MOVE SEQB-IDFS              TO W-IDFS                             
116707        MOVE SEQB-IDDC              TO W-IDDC-101KY                       
116708        MOVE SEQB-TIAVIDAT          TO W-TIAVIDAT                         
116712        PERFORM IMS-GU-INLA-D101                                          
116715        IF SEGMENT-FINNS                                                  
116716           MOVE INL-TIINLMOT        TO SPAR-TIINLMOT                      
116717        END-IF                                                            
116718     END-IF                                                               
116719     .                                                                    
116720     EJECT                                                                
116721*----------------------------------------------------------------*        
116722 HAB-INIT-RAD1    SECTION.                                                
116723                                                                          
116727     MOVE MID-SPAR-IDLOPNRM    TO WB-IDLOPNRM                             
116728     MOVE ZERO                 TO SPAR-KVINLART                           
116729                                                                          
116730*----LÄS RADNR 1 FÖR ATT HÄMTA KVINLART                                   
116731     MOVE 1                    TO W-IDRADNR                               
116732     PERFORM IMS-GU-INLB-D121                                             
116733     IF SEGMENT-FINNS                                                     
116736*-------SPARA    VISSA FÄLT FRÅN RADNR 1 TILL W-FÄLT                      
116737        IF RAD-ADINLOMR = SPACE                                           
116739           MOVE W-ADINLOMR          TO SPAR-ADINLOMR-NEW                  
116741           MOVE SPACE               TO SPAR-ADINLOMR-OLD                  
116742        ELSE                                                              
116743           MOVE RAD-ADINLOMR        TO SPAR-ADINLOMR-NEW                  
116744                                       SPAR-ADINLOMR-OLD                  
116745        END-IF                                                            
116746        MOVE RAD-ADINLOMR-NXT    TO W-RAD1-ADINLOMR-NXT                   
116747        MOVE RAD-IDINLVGN        TO W-RAD1-IDINLVGN                       
116748        MOVE RAD-FLPRIO          TO W-RAD1-FLPRIO                         
116749        MOVE RAD-FLKVAANT        TO W-RAD1-FLKVAANT                       
116751        MOVE RAD-FLINLFB         TO W-RAD1-FLINLFB                        
116752        MOVE RAD-KDINLPRIO       TO W-RAD1-KDINLPRIO                      
116753        IF RAD-KDINLSTA = 'SAK'                                           
116754           MOVE SPACE            TO W-RAD1-KDINLSTA                       
116755        ELSE                                                              
116756           MOVE RAD-KDINLSTA     TO W-RAD1-KDINLSTA                       
116757        END-IF                                                            
116761        MOVE RAD-KVINLART        TO SPAR-KVINLART                         
116762                                    SPAR-KVINLART-OLD                     
116763        COMPUTE SPAR-KVINLART = SPAR-KVINLART - SPAR-ACK-KVINLART         
116765        MOVE RAD-TIUPPDAT        TO W-RAD1-TIUPPDAT                       
116766                                                                          
116768        PERFORM S40-TRANS-W60191                                          
116769                                                                          
116770*-------RADNR 1 PÅ T91 UPPDATERAS REDAN HÄR MED NEDRÄKNAT ANTAL           
116772        MOVE SPAR-KVINLART        TO T91-MID-KVINLART-NEW(T91-IX)         
116774     END-IF                                                               
116775     .                                                                    
116776     EJECT                                                                
116777*----------------------------------------------------------------*        
116778 HB-LAES-SISTA-D121 SECTION.                                              
116779                                                                          
116781     MOVE MID-SPAR-IDLOPNRM        TO WB-IDLOPNRM                         
116782                                      W-IDLOPNRM                          
116783                                                                          
116786*----LÄS SISTA FÖREKOMST FÖR ATT FÅ HÖGSTA RADNR                          
116787     PERFORM IMS-GHU-INLB-D121-LAST                                       
116788                                                                          
116789     COMPUTE SPAR-IDRADNR = RAD-IDRADNR                                   
116792     .                                                                    
116793     EJECT                                                                
116794*----------------------------------------------------------------*        
116795 HC-ISRT-NY-RAD SECTION.                                                  
116800                                                                          
116801*----INITIERA FÖR INSERT AV NY RAD                                        
116803     COMPUTE SPAR-IDRADNR = SPAR-IDRADNR + 1                              
116804                                                                          
116805*----FRÅN ACKAR ETC                                                       
116806     MOVE SPAR-IDRADNR              TO RAD-IDRADNR                        
116807     MOVE NEJ                       TO RAD-FLSATS                         
116808     MOVE SPAR-IDLEVNR-NEW          TO RAD-IDLEVNR-KOLLI                  
116809     ADD  1                         TO SPAR-ACK-IDOKOLLI                  
116810     MOVE SPAR-ACK-IDOKOLLI         TO RAD-IDOKOLLI                       
116811     MOVE NEJ                       TO RAD-FLDIVKLI                       
116812     MOVE ZERO                      TO RAD-IDANSTNR                       
116813                                                                          
116814*----FRÅN BILDEN                                                          
116815     MOVE MID-KVINLART(IX)          TO W-RED-KVINLART                     
116816     MOVE W-KVINLART                TO RAD-KVINLART                       
116817     MOVE ZERO                      TO SPAR-KVINLART-OLD                  
116818                                                                          
116819*----FRÅN RADNR 1 OCH ANNAT                                               
116820     MOVE SPAR-ADINLOMR-NEW         TO RAD-ADINLOMR                       
116821     MOVE W-RAD1-ADINLOMR-NXT       TO RAD-ADINLOMR-NXT                   
116822     MOVE W-RAD1-FLPRIO             TO RAD-FLPRIO                         
116823     MOVE W-RAD1-IDINLVGN           TO RAD-IDINLVGN                       
116824     MOVE W-RAD1-FLKVAANT           TO RAD-FLKVAANT                       
116826     MOVE NEJ                       TO RAD-FLSVSLS                        
116827     MOVE NEJ                       TO RAD-FLINLFP                        
116828     MOVE W-RAD1-FLINLFB            TO RAD-FLINLFB                        
116829     MOVE +0                        TO RAD-IDILIRAD                       
116830     MOVE ZERO                      TO RAD-IDILIST                        
116831     MOVE W-RAD1-KDINLPRIO          TO RAD-KDINLPRIO                      
116832     MOVE W-RAD1-KDINLSTA           TO RAD-KDINLSTA                       
116833                                       SPAR-KDINLSTA-NEW                  
116834     MOVE SPACE                     TO SPAR-KDINLSTA-OLD                  
116835     MOVE W-RAD1-TIUPPDAT           TO RAD-TIUPPDAT                       
116856                                                                          
116857     PERFORM IMS-ISRT-INLB                                                
116862                                                                          
116863     PERFORM HCA-BEARB-T91                                                
116864     PERFORM HCB-BEARB-T94                                                
116865     .                                                                    
116866     EJECT                                                                
116867*----------------------------------------------------------------*        
116868 HCA-BEARB-T91  SECTION.                                                  
116869                                                                          
116873     IF T91-MID-KVPOST = 24                                               
116874        PERFORM S50-SKICKA-W60191                                         
116877        MOVE ZERO      TO T91-MID-KVPOST                                  
116878     END-IF                                                               
116879     PERFORM S40-TRANS-W60191                                             
116881     .                                                                    
116882     EJECT                                                                
116883*----------------------------------------------------------------*        
116884 HCB-BEARB-T94  SECTION.                                                  
116885                                                                          
116893     IF T94-MID-KVPOST = 15                                               
116900        PERFORM S51-SKICKA-W60194                                         
116902        MOVE ZERO      TO T94-MID-KVPOST                                  
116903     END-IF                                                               
116904     PERFORM S41-TRANS-W60194                                             
116925     .                                                                    
116926     EJECT                                                                
116927*----------------------------------------------------------------*        
116928 HD-AVSLUTA-RAD1    SECTION.                                              
116929                                                                          
116933     MOVE MID-SPAR-IDLOPNRM        TO WB-IDLOPNRM                         
116935                                                                          
116936*----LÄS FÖRSTA RAD FÖR ATT RÄKNA NED KVINLART                            
116937     MOVE 1                          TO W-IDRADNR                         
116938     PERFORM IMS-GHU-INLB-D121                                            
116939     IF SEGMENT-FINNS                                                     
116944                                                                          
116945        MOVE SPAR-KVINLART        TO RAD-KVINLART                         
116946                                                                          
116950        IF RAD-KVINLART = ZERO                                            
116951           PERFORM IMS-DLET-INLB                                          
116952        ELSE                                                              
116953           IF RAD-KDINLSTA = 'SAK'                                        
116954              MOVE RAD-KDINLSTA       TO SPAR-KDINLSTA-OLD                
116955              MOVE SPACE              TO RAD-KDINLSTA                     
116956              MOVE SPACE              TO SPAR-KDINLSTA-NEW                
116957           ELSE                                                           
116958              MOVE RAD-KDINLSTA       TO SPAR-KDINLSTA-OLD                
116959                                         SPAR-KDINLSTA-NEW                
116960           END-IF                                                         
116961           IF RAD-ADINLOMR = SPACE                                        
116963              MOVE SPAR-ADINLOMR-NEW TO RAD-ADINLOMR                      
116964           END-IF                                                         
116965           PERFORM IMS-REPL-INLB                                          
116966        END-IF                                                            
116967     END-IF                                                               
116969     .                                                                    
116970     EJECT                                                                
116971*----------------------------------------------------------------*        
116972 HE-HAEMTA-KOLLINR    SECTION.                                            
116973                                                                          
116975*----LÄS/UPD LOPA-REG FÖR ATT HÄMTA KOLLINR                               
116976                                                                          
116977     MOVE LOW-VALUE          TO WL-W6GX01KEY-X                            
116978     MOVE '6017'             TO WL-IDHTYP                                 
116979     PERFORM IMS-GHU-LOPA-G111                                            
116980     IF SEGMENT-FINNS                                                     
116982        MOVE 6018-IDOKOLLI          TO SPAR-ACK-IDOKOLLI                  
116983     END-IF                                                               
116985     .                                                                    
116986     EJECT                                                                
116987*----------------------------------------------------------------*        
116988 HF-UPPDAT-KOLLINR    SECTION.                                            
116989                                                                          
116991     MOVE SPAR-ACK-IDOKOLLI     TO 6018-IDOKOLLI                          
116992                                                                          
116994     PERFORM IMS-REPL-LOPA                                                
116995     .                                                                    
116996     EJECT                                                                
116997*----------------------------------------------------------------*        
116998 HG-SKICKA-TRANSAR    SECTION.                                            
116999                                                                          
117003     IF TRANS91-JA                                                        
117004*-------SKICKA TRANS TILL MPP                                             
117005        PERFORM S50-SKICKA-W60191                                         
117006     END-IF                                                               
117007                                                                          
117008     IF TRANS94-JA                                                        
117009*-------SKICKA TRANS TILL MPP                                             
117010        PERFORM S51-SKICKA-W60194                                         
117013     END-IF                                                               
117014     .                                                                    
117015     EJECT                                                                
117016******************************************************************        
117017*    MFS-REDIGERING AV BILDENS FÄLT                              *        
117018******************************************************************        
117019*----------------------------------------------------------------*        
117020 MFS-RENSA-FAELT-UT-HUV SECTION.                                          
117100                                                                          
117200*    --- ALLA UTDATA-FÄLT                                                 
117300*    --- INKL. BLÄDDRINGSNYCKLAR - SPAR-FÄLT                              
117400                                                                          
117500     MOVE MFS-RENSA-FAELT    TO MOD-BEART                                 
117600                                MOD-KVAVIS                                
117700                                MOD-KDSORT                                
117800                                MOD-KDFARLIG-TXT                          
117910                                MOD-SPAR-IDLOPNRM                         
117920     .                                                                    
117930     EJECT                                                                
118000                                                                          
118010*----------------------------------------------------------------*        
118020 MFS-RENSA-FAELT-UT SECTION.                                              
118030                                                                          
118040*    --- ALLA UTDATA-FÄLT RADER                                           
118100*--- RENSA INDEXERADE RADER                                               
118200                                                                          
118300     MOVE +1 TO IX                                                        
118400     PERFORM UNTIL IX > MAX-IX                                            
118500        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
118600        ADD +1 TO IX                                                      
118700     END-PERFORM                                                          
118800     .                                                                    
118900     SKIP2                                                                
119000     EJECT                                                                
119100*----------------------------------------------------------------*        
119200 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
119300                                                                          
119400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
119500                                                                          
119600     MOVE MFS-RENSA-FAELT    TO MOD-KVFLETI(IX)                           
119700                                MOD-KVINLART(IX)                          
119900     .                                                                    
120000     EJECT                                                                
120100     SKIP2                                                                
120200*----------------------------------------------------------------*        
120300 MFS-RENSA-FAELT-IN SECTION.                                              
120400                                                                          
120500*    --- ALLA INDATA-FÄLT                                                 
120600     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                              
120700                             MOD-IDARTNR-IN                               
120800                             MOD-IDLEVNR-IN                               
120900                             MOD-IDFS-IN                                  
121000                             MOD-IDLBBET-IN                               
121100     .                                                                    
121200     EJECT                                                                
121300     SKIP2                                                                
122540*----------------------------------------------------------------*        
122550 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
122560                                                                          
122570*    --- ALLA UTDATA-FÄLT                                                 
122580*    --- ALLA BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
122590                                                                          
122591     MOVE MFS-ROER-EJ-FAELT  TO MOD-SPAR-IDLOPNRM                         
122592                                MOD-BEART                                 
122593                                MOD-KVAVIS                                
122594                                MOD-KDSORT                                
122595                                MOD-KDFARLIG-TXT                          
122596                                                                          
122600*--- RENSA INDEXERADE RADER                                               
122700                                                                          
122800     MOVE +1 TO IX                                                        
122900     PERFORM UNTIL IX > MAX-IX                                            
123000        PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                  
123100        ADD +1 TO IX                                                      
123200     END-PERFORM                                                          
123300     .                                                                    
123400     SKIP2                                                                
123500     EJECT                                                                
123600*----------------------------------------------------------------*        
123700 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
123800                                                                          
123900*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
124000                                                                          
124100     MOVE MFS-ROER-EJ-FAELT  TO MOD-KVFLETI(IX)                           
124200                                MOD-KVINLART(IX)                          
124400     .                                                                    
124500     EJECT                                                                
124600     SKIP2                                                                
124700*----------------------------------------------------------------*        
124800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
124900                                                                          
125000*    --- ALLA INDATA-FÄLT                                                 
125100                                                                          
125200     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLOPNRM-IN                           
125300                                MOD-IDARTNR-IN                            
125400                                MOD-IDLEVNR-IN                            
125500                                MOD-IDFS-IN                               
125600                                MOD-IDLBBET-IN                            
125700     .                                                                    
125800     EJECT                                                                
125900     SKIP2                                                                
126000*----------------------------------------------------------------*        
126100 MFS-LAES-IN-IGEN SECTION.                                                
126200                                                                          
126300*    INDATA-FÄLT                                                          
126400                                                                          
126700*--- RENSA INDEXERADE RADER                                               
126800                                                                          
126900     MOVE +1 TO IX                                                        
127000     PERFORM UNTIL IX > MAX-IX                                            
127100                                                                          
127200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVFLETI-ATTR(IX)                
127300                                      MOD-KVINLART-ATTR(IX)               
127500        ADD +1 TO IX                                                      
127600     END-PERFORM                                                          
127700                                                                          
127800     .                                                                    
127900     EJECT                                                                
128010******************************************************************        
128100*  IMS-SECTIONER                                                 *        
128200******************************************************************        
128300     SKIP3                                                                
128400*----------------------------------------------------------------*        
128500 IMS-GET-MSG SECTION.                                                     
128600                                                                          
128700     MOVE '  QC' TO GODK-STATUSKODER                                      
128800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
128900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
129040     PERFORM IMS-STATUSKONTROLL                                           
129100     .                                                                    
129200     SKIP3                                                                
129300*----------------------------------------------------------------*        
129400 IMS-INSERT-MSG SECTION.                                                  
129500                                                                          
129510     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
129520       MOVE '0' TO MFS-KDHUVOMR                                           
129800     END-IF                                                               
129900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
130000     MOVE SPACE TO GODK-STATUSKODER                                       
130100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
130200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
130300     PERFORM IMS-STATUSKONTROLL                                           
130400     .                                                                    
130500     EJECT                                                                
130600     SKIP3                                                                
130700                                                                          
130800******************************************************************        
130900*    ALT1-PCB  (TRANS W60191)                                    *        
131000******************************************************************        
131100*----------------------------------------------------------------*        
131200 IMS-ISRT-MSG-ALT1-6191 SECTION.                                          
131300                                                                          
131400     MOVE SPACE              TO GODK-STATUSKODER                          
131500     CALL CBLTDLI USING      ISRT ALT1-PCB                                
131600                                  P-TO-P-T91                              
131700     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
131800     PERFORM IMS-STATUSKONTROLL                                           
131900     .                                                                    
132000     EJECT                                                                
132100     SKIP3                                                                
132200                                                                          
132290******************************************************************        
132291*    ALT1-PCB  (TRANS W60191)                                    *        
132292******************************************************************        
132293*----------------------------------------------------------------*        
132294 IMS-PURG-MSG-ALT1-6191 SECTION.                                          
132295                                                                          
132296     MOVE SPACE              TO GODK-STATUSKODER                          
132297     CALL CBLTDLI USING      PURG ALT1-PCB                                
132298                                  P-TO-P-T91                              
132299     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
132300     PERFORM IMS-STATUSKONTROLL                                           
132301     .                                                                    
132302     EJECT                                                                
132303     SKIP3                                                                
132304                                                                          
132310******************************************************************        
132400*    ALT2-PCB  (TRANS W60194)                                    *        
132500******************************************************************        
132600*----------------------------------------------------------------*        
132700 IMS-ISRT-MSG-ALT2-6194 SECTION.                                          
132800                                                                          
132900     MOVE SPACE              TO GODK-STATUSKODER                          
133000     CALL CBLTDLI USING      ISRT ALT2-PCB                                
133100                                  P-TO-P-T94                              
133200     MOVE ALT2-STATUS-CODE   TO STATUS-WS                                 
133300     PERFORM IMS-STATUSKONTROLL                                           
133400     .                                                                    
133500     EJECT                                                                
133600     SKIP3                                                                
133700                                                                          
133710******************************************************************        
133720*    ALT2-PCB  (TRANS W60194)                                    *        
133730******************************************************************        
133740*----------------------------------------------------------------*        
133750 IMS-PURG-MSG-ALT2-6194 SECTION.                                          
133760                                                                          
133770     MOVE SPACE              TO GODK-STATUSKODER                          
133780     CALL CBLTDLI USING      PURG ALT2-PCB                                
133790                                  P-TO-P-T94                              
133791     MOVE ALT2-STATUS-CODE   TO STATUS-WS                                 
133792     PERFORM IMS-STATUSKONTROLL                                           
133793     .                                                                    
133794     EJECT                                                                
133795     SKIP3                                                                
133796                                                                          
133800******************************************************************        
133900*    INLA-PCB                                                    *        
134000******************************************************************        
134100     SKIP3                                                                
135500*----------------------------------------------------------------*        
135600 IMS-GU-INLA-D101 SECTION.                                                
135700     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
135800             DELIMITED BY SIZE INTO SSA1                                  
136100     MOVE '  GE'            TO GODK-STATUSKODER                           
136200     CALL CBLTDLI USING GU  INLA-PCB                                      
136300                            DLI-IO-AREA-1                                 
136400                            SSA1                                          
136600     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
136700     PERFORM IMS-STATUSKONTROLL                                           
136800     .                                                                    
136900     EJECT                                                                
137000     SKIP3                                                                
137100*----------------------------------------------------------------*        
137200 IMS-GU-INLA-D111 SECTION.                                                
137300     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
137400             DELIMITED BY SIZE INTO SSA1                                  
137500     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
137600          DELIMITED BY SIZE INTO SSA2                                     
137700     MOVE '  GE'            TO GODK-STATUSKODER                           
137800     CALL CBLTDLI USING GU  INLA-PCB                                      
137900                            DLI-IO-AREA-1                                 
138000                            SSA1                                          
138100                            SSA2                                          
138200     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
138300     PERFORM IMS-STATUSKONTROLL                                           
138400     .                                                                    
138500     EJECT                                                                
138600     SKIP3                                                                
140800******************************************************************        
140900*    INLB-PCB                                                    *        
141000******************************************************************        
141100     SKIP3                                                                
141200*----------------------------------------------------------------*        
141300 IMS-GU-INLB-D111  SECTION.                                               
141400     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1B1KY-X                            
141410                    '&IDDC     =' W-IDDC-X ')'                            
141500             DELIMITED BY SIZE INTO SSA1                                  
141600     MOVE '  GE'                 TO GODK-STATUSKODER                      
141700     CALL CBLTDLI USING GU       INLB-PCB                                 
141800                                 DLI-IO-AREA-1                            
141900                                 SSA1                                     
142000     MOVE INLB-STATUS-CODE       TO STATUS-WS                             
142100     PERFORM IMS-STATUSKONTROLL                                           
142200     .                                                                    
142300     EJECT                                                                
142400     SKIP3                                                                
142800*----------------------------------------------------------------*        
142900 IMS-GU-INLB-D121        SECTION.                                         
143000     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1B1KY-X ')'                        
143100          DELIMITED BY SIZE INTO SSA1                                     
143200     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
143300          DELIMITED BY SIZE INTO SSA2                                     
143400     MOVE '  GE'            TO GODK-STATUSKODER                           
143500     CALL CBLTDLI USING GU  INLB-PCB                                      
143600                            DLI-IO-AREA-1                                 
143700                            SSA1                                          
143800                            SSA2                                          
143900     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
144000     PERFORM IMS-STATUSKONTROLL                                           
144100     .                                                                    
144200     EJECT                                                                
144300     SKIP3                                                                
144400*----------------------------------------------------------------*        
144500 IMS-GNP-INLB-D121    SECTION.                                            
144600     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1B1KY-X ')'                        
144700          DELIMITED BY SIZE INTO SSA1                                     
144800     STRING 'W6INLA21(IDRADNR  >' W-IDRADNR-X ')'                         
144900          DELIMITED BY SIZE INTO SSA2                                     
145000     MOVE '  GE'            TO GODK-STATUSKODER                           
145100     CALL CBLTDLI USING GNP INLB-PCB                                      
145200                            DLI-IO-AREA-1                                 
145300                            SSA1                                          
145400                            SSA2                                          
145500     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
145600     PERFORM IMS-STATUSKONTROLL                                           
145610     .                                                                    
145620     EJECT                                                                
145630     SKIP3                                                                
145640******************************************************************        
145650*    IMS-UPPDATERING VIA INLB-PCB                                *        
145660******************************************************************        
145670*----------------------------------------------------------------*        
145680 IMS-GHU-INLB-D121        SECTION.                                        
145690     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1B1KY-X ')'                        
145691          DELIMITED BY SIZE INTO SSA1                                     
145692     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
145693          DELIMITED BY SIZE INTO SSA2                                     
145694     MOVE '  GE'            TO GODK-STATUSKODER                           
145695     CALL CBLTDLI USING GHU INLB-PCB                                      
145696                            DLI-IO-AREA-1                                 
145697                            SSA1                                          
145698                            SSA2                                          
145699     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
145700     PERFORM IMS-STATUSKONTROLL                                           
145701     .                                                                    
145702     EJECT                                                                
145703     SKIP3                                                                
145704*----------------------------------------------------------------*        
145705 IMS-GHU-INLB-D121-LAST    SECTION.                                       
145706     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1B1KY-X ')'                        
145707          DELIMITED BY SIZE INTO SSA1                                     
145708     MOVE 'W6INLA21*L'        TO SSA2                                     
145709     MOVE '  GE'              TO GODK-STATUSKODER                         
145710     CALL CBLTDLI USING GHU   INLB-PCB                                    
145711                              DLI-IO-AREA-1                               
145712                              SSA1                                        
145713                              SSA2                                        
145714     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
145715     PERFORM IMS-STATUSKONTROLL                                           
145716     .                                                                    
145717     EJECT                                                                
145718     SKIP3                                                                
145719*----------------------------------------------------------------*        
145720 IMS-REPL-INLB SECTION.                                                   
145721                                                                          
145722     MOVE '  '               TO GODK-STATUSKODER                          
145723     CALL CBLTDLI USING REPL INLB-PCB                                     
145724                             DLI-IO-AREA-1                                
145725     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
145726     PERFORM IMS-STATUSKONTROLL                                           
145727     .                                                                    
145728     EJECT                                                                
145729     SKIP3                                                                
145730*----------------------------------------------------------------*        
145731 IMS-ISRT-INLB SECTION.                                                   
145732                                                                          
145733     MOVE 'W6INLA21 '        TO SSA1                                      
145734     MOVE '  '               TO GODK-STATUSKODER                          
145735     CALL CBLTDLI USING ISRT INLB-PCB                                     
145736                             DLI-IO-AREA-1                                
145737                             SSA1                                         
145738     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
145739     PERFORM IMS-STATUSKONTROLL                                           
145740     .                                                                    
145741     EJECT                                                                
145742     SKIP3                                                                
145743*----------------------------------------------------------------*        
145744 IMS-DLET-INLB SECTION.                                                   
145745                                                                          
145746     MOVE '  '               TO GODK-STATUSKODER                          
145747     CALL CBLTDLI USING DLET INLB-PCB                                     
145748                             DLI-IO-AREA-1                                
145749     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
145750     PERFORM IMS-STATUSKONTROLL                                           
145751     .                                                                    
145752     EJECT                                                                
145753     SKIP3                                                                
145760******************************************************************        
145800*    INLB1-PCB                                                   *        
145900******************************************************************        
147300     SKIP3                                                                
147400*----------------------------------------------------------------*        
147500 IMS-GU-INLB1-D111-X  SECTION.                                            
147600     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
147800             DELIMITED BY SIZE INTO SSA1                                  
147900     MOVE '  GE'                 TO GODK-STATUSKODER                      
148000     CALL CBLTDLI USING GU       INLB1-PCB                                
148100                                 DLI-IO-AREA-1                            
148200                                 SSA1                                     
148300     MOVE INLB1-STATUS-CODE      TO STATUS-WS                             
148400     PERFORM IMS-STATUSKONTROLL                                           
148500     .                                                                    
148600     EJECT                                                                
148700     SKIP3                                                                
148710******************************************************************        
148720*    INLH1-PCB                                                   *        
148730******************************************************************        
148740     SKIP3                                                                
148750*----------------------------------------------------------------*        
148760 IMS-GU-INLH1-D111-X  SECTION.                                            
148770     STRING 'W6INLI01(W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
148780                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
148790             DELIMITED BY SIZE INTO SSA1                                  
148791     MOVE '  GE'                 TO GODK-STATUSKODER                      
148792     CALL CBLTDLI USING GU       INLH1-PCB                                
148793                                 DLI-IO-AREA-1                            
148794                                 SSA1                                     
148795     MOVE INLH1-STATUS-CODE      TO STATUS-WS                             
148796     PERFORM IMS-STATUSKONTROLL                                           
148797     .                                                                    
148798     EJECT                                                                
148799     SKIP3                                                                
148800*----------------------------------------------------------------*        
148900 IMS-GN-INLH1-D111-X  SECTION.                                            
149000     STRING 'W6INLI01(W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
149100                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
149200             DELIMITED BY SIZE INTO SSA1                                  
149300     MOVE '  GE'                 TO GODK-STATUSKODER                      
149400     CALL CBLTDLI USING GN       INLH1-PCB                                
149500                                 DLI-IO-AREA-1                            
149600                                 SSA1                                     
149700     MOVE INLH1-STATUS-CODE      TO STATUS-WS                             
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000     EJECT                                                                
150100     SKIP3                                                                
153200******************************************************************        
153300*    LOPA-PCB                                                    *        
153400******************************************************************        
153500     SKIP3                                                                
153600*----------------------------------------------------------------*        
153700 IMS-GHU-LOPA-G111 SECTION.                                               
153800     STRING 'W6LOPA01(W6GXKEY  =' WL-W6GX01KEY-X ')'                      
153900          DELIMITED BY SIZE INTO SSA1                                     
154000     MOVE 'W6LOPA11 '       TO SSA2                                       
154100     MOVE '  GE'            TO GODK-STATUSKODER                           
154200     CALL CBLTDLI USING GHU LOPA-PCB                                      
154300                            DLI-IO-AREA-2                                 
154400                            SSA1                                          
154500                            SSA2                                          
154600     MOVE LOPA-STATUS-CODE  TO STATUS-WS                                  
154700     PERFORM IMS-STATUSKONTROLL                                           
154800     .                                                                    
154900     EJECT                                                                
155000     SKIP3                                                                
155100*----------------------------------------------------------------*        
155200 IMS-REPL-LOPA SECTION.                                                   
155300                                                                          
155400     MOVE '  '               TO GODK-STATUSKODER                          
155500     CALL CBLTDLI USING REPL LOPA-PCB                                     
155600                             DLI-IO-AREA-2                                
155700     MOVE LOPA-STATUS-CODE   TO STATUS-WS                                 
155800     PERFORM IMS-STATUSKONTROLL                                           
155900     .                                                                    
156000     EJECT                                                                
156100     SKIP3                                                                
156200******************************************************************        
156300*    PLAA-PCB                                                    *        
156400******************************************************************        
156500     SKIP3                                                                
156600*----------------------------------------------------------------*        
156700 IMS-GU-PLAA-G111 SECTION.                                                
156800     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
156900          DELIMITED BY SIZE INTO SSA1                                     
157000     STRING 'W6PLAA11(W6GXKEY  =' W-W6GX11KEY-X ')'                       
157100          DELIMITED BY SIZE INTO SSA2                                     
157200     MOVE '  GE'            TO GODK-STATUSKODER                           
157300     CALL CBLTDLI USING GU  PLAA-PCB                                      
157400                            DLI-IO-AREA-3                                 
157500                            SSA1                                          
157600                            SSA2                                          
157700     MOVE PLAA-STATUS-CODE  TO STATUS-WS                                  
157800     PERFORM IMS-STATUSKONTROLL                                           
157900     .                                                                    
158000     EJECT                                                                
158010 IMS-GU-WDK611 SECTION.                                                   
158020     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
158030          DELIMITED BY SIZE INTO SSA1                                     
158040     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY ')'                          
158050          DELIMITED BY SIZE INTO SSA2                                     
158060     MOVE '  GE' TO GODK-STATUSKODER                                      
158070     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA4 SSA1 SSA2                
158080     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
158090     PERFORM IMS-STATUSKONTROLL                                           
158091     .                                                                    
158100     SKIP3                                                                
158200*----------------------------------------------------------------*        
158300 IMS-STATUSKONTROLL SECTION.                                              
158400                                                                          
158500     SET STATUS-IX TO 1                                                   
158600     SEARCH GODK-STATUS                                                   
158700       AT END                                                             
158800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
158900         DELIMITED BY SIZE INTO FELTEXT                                   
159000         CALL FELLOG                                                      
159100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
159200         CONTINUE                                                         
159300     END-SEARCH                                                           
159400     .                                                                    
159500     EJECT                                                                
159600     SKIP3                                                                
159700******************************************************************        
159800*  INFO/FELMEDDELANDE                                            *        
159900******************************************************************        
160000*----------------------------------------------------------------*        
160100 S01-CALL-WMEDKONV-FEL SECTION.                                           
160200                                                                          
160300           CALL WMEDKONV USING MED-WMEDAREA                               
160400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
160500     .                                                                    
160600     EJECT                                                                
160700     SKIP3                                                                
160800*----------------------------------------------------------------*        
160900 S02-CALL-WMEDKONV-INF SECTION.                                           
161000                                                                          
161100           CALL WMEDKONV USING MED-WMEDAREA                               
161200           MOVE MED-MFSINF TO MOD-TEMFSINF                                
161300     .                                                                    
161400     EJECT                                                                
161500     SKIP3                                                                
161600******************************************************************        
161700*  INITERA NKL:AR                                                *        
161800******************************************************************        
161900 S12-INIT-NKL   SECTION.                                                  
162740                                                                          
162800     IF NKLTYP1                                                           
163000        MOVE MOD-IDLOPNRM-UT       TO WB-IDLOPNRM                         
163200     END-IF                                                               
163300                                                                          
163400     IF NKLTYP2                                                           
163500        MOVE MOD-IDARTNR-UT        TO WH1-MIN-IDARTNR                     
163600        MOVE MOD-IDDC-UT           TO WH1-MIN-IDDC                        
163700        MOVE MOD-IDLEVNR-UT        TO WH1-MIN-IDLEVNR                     
163800        MOVE MOD-IDFS-UT           TO WH1-MIN-IDFS                        
163900        MOVE +0000000              TO WH1-MIN-TIAVIDAT                    
163910        MOVE +00000                TO WH1-MIN-IDRADNR-INL                 
164000                                                                          
164100        MOVE MOD-IDARTNR-UT        TO WH1-MAX-IDARTNR                     
164200        MOVE MOD-IDDC-UT           TO WH1-MAX-IDDC                        
164300        MOVE MOD-IDLEVNR-UT        TO WH1-MAX-IDLEVNR                     
164400        MOVE MOD-IDFS-UT           TO WH1-MAX-IDFS                        
164500        MOVE +9999999              TO WH1-MAX-TIAVIDAT                    
164600        MOVE +99999                TO WH1-MAX-IDRADNR-INL                 
164800     END-IF                                                               
166400     .                                                                    
166500     EJECT                                                                
166700*----------------------------------------------------------------*        
166800 S20-FLYTTA-SEGM-11-ART-UPPG  SECTION.                                    
166900                                                                          
167310     MOVE ART-KVAVIS         TO W-KVAVIS-N                                
167320     MOVE W-RED-KVAVIS       TO MOD-KVAVIS                                
167330     MOVE ART-BEART          TO MOD-BEART                                 
167340     MOVE ART-KDSORT         TO MOD-KDSORT                                
167350                                                                          
167400     MOVE ART-IDLOPNRM       TO MOD-SPAR-IDLOPNRM                         
167420     MOVE ART-IDARTNR        TO W-ARTNR-N                                 
167430     MOVE ART-IDRADNR-INL    TO W-IDRADNR-INL                             
167500                                                                          
167600     IF ART-KDFARLIG = +4                                                 
167601     OR ART-KDFARLIG = +7                                                 
167610       IF CDC-SE                                                          
167700         MOVE 'JA'            TO MOD-KDFARLIG-TXT                         
167710       ELSE                                                               
167711         MOVE 'YES'           TO MOD-KDFARLIG-TXT                         
167720       END-IF                                                             
167800     ELSE                                                                 
167900        IF ART-KDFARLIG = +5                                              
167910          IF CDC-SE                                                       
168000            MOVE 'ASBEST'     TO MOD-KDFARLIG-TXT                         
168010          ELSE                                                            
168011            MOVE 'ASBEST'     TO MOD-KDFARLIG-TXT                         
168020          END-IF                                                          
168100        ELSE                                                              
168200           IF ART-KDFARLIG = +6                                           
168210             IF CDC-SE                                                    
168300               MOVE 'KEMIKALIER' TO MOD-KDFARLIG-TXT                      
168400             ELSE                                                         
168410               MOVE 'CHEMICALS ' TO MOD-KDFARLIG-TXT                      
168500             END-IF                                                       
168600           END-IF                                                         
168700        END-IF                                                            
168800     END-IF                                                               
168900     .                                                                    
169000     EJECT                                                                
169100     SKIP3                                                                
173900*----------------------------------------------------------------*        
174000 S40-TRANS-W60191      SECTION.                                           
174100                                                                          
175700     IF T91-MID-KVPOST = ZERO                                             
175800        MOVE JA                  TO TRANS91-SW                            
175900        MOVE SPACE               TO T91-MID-W6I19101                      
176000        MOVE +1                  TO T91-MID-KVPOST                        
176010        MOVE +1                  TO T91-IX                                
176100                                                                          
176200        MOVE IDPGM               TO T91-MID-IDPGM                         
176300        MOVE WS-IDDC             TO T91-MID-IDDC                          
176400                                                                          
176500        MOVE MID-SPAR-IDLOPNRM   TO T91-MID-IDLOPNRM(1)                   
176600        INSPECT T91-MID-IDLOPNRM(1)                                       
176700                REPLACING LEADING SPACE BY ZERO                           
176800        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(1)                 
176900        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(1)                  
177000        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(1)                
177001        IF RAD-IDRADNR = +1                                               
177002          MOVE SPAR-ACK-KVFLETI      TO T91-MID-KVKOLLI  (1)              
177003        ELSE                                                              
177004          MOVE +0                    TO T91-MID-KVKOLLI  (1)              
177005        END-IF                                                            
177020        MOVE 'N'                   TO T91-MID-FLINLI   (1)                
177100        MOVE SPAR-ADINLOMR-OLD     TO T91-MID-ADINLOMR-OLD(1)             
177200        MOVE SPAR-ADINLOMR-NEW     TO T91-MID-ADINLOMR-NEW(1)             
177300        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-OLD(1)         
177400        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-NEW(1)         
177500        MOVE SPAR-KVINLART-OLD     TO T91-MID-KVINLART-OLD(1)             
177600        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(1)             
177700        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(1)             
177800        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(1)             
177900     ELSE                                                                 
178000        COMPUTE T91-IX = T91-MID-KVPOST + 1                               
178100        ADD 1                         TO T91-MID-KVPOST                   
178200                                                                          
178300        MOVE MID-SPAR-IDLOPNRM     TO T91-MID-IDLOPNRM(T91-IX)            
178400        INSPECT T91-MID-IDLOPNRM(T91-IX)                                  
178500                REPLACING LEADING SPACE BY ZERO                           
178510        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(T91-IX)            
178600        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(T91-IX)             
178700        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(T91-IX)           
178710        MOVE +0                    TO T91-MID-KVKOLLI  (T91-IX)           
178720        MOVE 'N'                   TO T91-MID-FLINLI   (T91-IX)           
178800        MOVE SPAR-ADINLOMR-OLD    TO T91-MID-ADINLOMR-OLD(T91-IX)         
178900        MOVE SPAR-ADINLOMR-NEW    TO T91-MID-ADINLOMR-NEW(T91-IX)         
179000        MOVE RAD-ADINLOMR-NXT                                             
179100             TO T91-MID-ADINLOMR-NXT-OLD(T91-IX)                          
179200        MOVE RAD-ADINLOMR-NXT                                             
179300             TO T91-MID-ADINLOMR-NXT-NEW(T91-IX)                          
179400        MOVE SPAR-KVINLART-OLD     TO T91-MID-KVINLART-OLD(T91-IX)        
179500        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(T91-IX)        
179600        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(T91-IX)        
179700        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(T91-IX)        
179800     END-IF                                                               
180200     .                                                                    
180300     EJECT                                                                
180400*----------------------------------------------------------------*        
180500 S41-TRANS-W60194      SECTION.                                           
187311                                                                          
187351     MOVE ZERO                TO SPAR-VKART-KG                            
187352                                                                          
187353     IF T94-MID-KVPOST = ZERO                                             
187354        MOVE ZERO                     TO T94-MID-VKKOLLIN(1)              
187355        MOVE JA                       TO TRANS94-SW                       
187356                                                                          
187358        MOVE SPACE                    TO T94-MID-IDPRTLST                 
187359        MOVE '6F'                     TO T94-MID-IDPRTLST(1:2)            
187360        MOVE MID-ADINLOMR-PRT         TO T94-MID-IDPRTLST(3:6)            
187361        MOVE IDPGM                    TO T94-MID-IDPGM                    
187362        MOVE +1                       TO T94-MID-KVPOST                   
187365                                                                          
187366        MOVE MID-IDARTNR-UT           TO W-ARTNR-X                        
187367        MOVE W-ARTNR-N                TO T94-MID-IDARTNR(1)               
187368        MOVE WB-IDLOPNRM              TO T94-MID-IDLOPNRM(1)              
187369        MOVE RAD-KVINLART             TO T94-MID-KVINLART(1)              
187370        MOVE RAD-IDLEVNR-KOLLI        TO T94-MID-IDLEVNR-KOLLI(1)         
187371        MOVE RAD-IDOKOLLI             TO T94-MID-IDOKOLLI(1)              
187372        MOVE SPAR-TIINLMOT            TO T94-MID-TIINLMOT(1)              
187373                                                                          
187375        COMPUTE SPAR-VKART-KG =  SPAR-VKART / 1000                        
187376        COMPUTE T94-MID-VKKOLLIN(1) ROUNDED =                             
187377                RAD-KVINLART * SPAR-VKART-KG                              
187380        MOVE ZERO                     TO T94-MID-VKKOLLIB(1)              
187381                                                                          
187382        IF (MID-KDPRTVAL = YES                                            
187383        OR MID-KDPRTVAL = JA)                                             
187384          MOVE CLAG-ADLAGOMR-SVS      TO T94-MID-ADLAGOMR(1)              
187385          MOVE CLAG-ADGANG-SVS        TO T94-MID-ADGANG(1)                
187386          MOVE CLAG-ADPLATS-SVS       TO T94-MID-ADPLATS(1)               
187390        ELSE                                                              
187391          MOVE SPAR-ADLAGOMR          TO T94-MID-ADLAGOMR(1)              
187392          MOVE SPAR-ADGANG            TO T94-MID-ADGANG(1)                
187393          MOVE SPAR-ADPLATS           TO T94-MID-ADPLATS(1)               
187394        END-IF                                                            
187395        MOVE SPAR-BEFT                TO T94-MID-BEFT(1)                  
187396        MOVE SPAR-KDSORT              TO T94-MID-KDSORT(1)                
187397     ELSE                                                                 
187398        COMPUTE T94-IX = T94-MID-KVPOST + 1                               
187399        ADD 1                         TO T94-MID-KVPOST                   
187400        MOVE ZERO                     TO T94-MID-VKKOLLIN(T94-IX)         
187401        MOVE MID-IDARTNR-UT           TO W-ARTNR-X                        
187402        MOVE W-ARTNR-N                TO T94-MID-IDARTNR(T94-IX)          
187403        MOVE WB-IDLOPNRM              TO T94-MID-IDLOPNRM(T94-IX)         
187404        MOVE RAD-KVINLART             TO T94-MID-KVINLART(T94-IX)         
187405        MOVE RAD-IDLEVNR-KOLLI  TO T94-MID-IDLEVNR-KOLLI(T94-IX)          
187406        MOVE RAD-IDOKOLLI             TO T94-MID-IDOKOLLI(T94-IX)         
187407        MOVE SPAR-TIINLMOT            TO T94-MID-TIINLMOT(T94-IX)         
187408                                                                          
187409        COMPUTE SPAR-VKART-KG =  SPAR-VKART / 1000                        
187410        COMPUTE T94-MID-VKKOLLIN(T94-IX) ROUNDED =                        
187411                RAD-KVINLART * SPAR-VKART-KG                              
187412        MOVE ZERO                     TO T94-MID-VKKOLLIB(T94-IX)         
187413                                                                          
187414        IF (MID-KDPRTVAL = YES                                            
187415        OR MID-KDPRTVAL = JA)                                             
187416          MOVE CLAG-ADLAGOMR-SVS      TO T94-MID-ADLAGOMR(T94-IX)         
187417          MOVE CLAG-ADGANG-SVS        TO T94-MID-ADGANG(T94-IX)           
187418          MOVE CLAG-ADPLATS-SVS       TO T94-MID-ADPLATS(T94-IX)          
187419        ELSE                                                              
187420          MOVE SPAR-ADLAGOMR          TO T94-MID-ADLAGOMR(T94-IX)         
187421          MOVE SPAR-ADGANG            TO T94-MID-ADGANG(T94-IX)           
187422          MOVE SPAR-ADPLATS           TO T94-MID-ADPLATS(T94-IX)          
187423        END-IF                                                            
187424        MOVE SPAR-BEFT                TO T94-MID-BEFT(T94-IX)             
187425        MOVE SPAR-KDSORT              TO T94-MID-KDSORT(T94-IX)           
187426     END-IF                                                               
187427     .                                                                    
187428     EJECT                                                                
187533*----------------------------------------------------------------*        
187534 S50-SKICKA-W60191 SECTION.                                               
187535                                                                          
187600     COMPUTE W-PTOP1-OCC-LL = T91-MID-KVPOST * 64                         
187700     COMPUTE PTOP1-LL = W-PTOP1-OCC-LL + 35                               
187710                                                                          
187800     MOVE MFS-KDMFSFOR            TO PTOP1-KDMFSFOR                       
188100                                                                          
188120     IF FOERSTA-T91-JA                                                    
188130        PERFORM IMS-ISRT-MSG-ALT1-6191                                    
188140        MOVE NEJ     TO FOERSTA-T91-SW                                    
188150     ELSE                                                                 
188160        PERFORM IMS-PURG-MSG-ALT1-6191                                    
188170     END-IF                                                               
188500     .                                                                    
188600     EJECT                                                                
188700*----------------------------------------------------------------*        
188800 S51-SKICKA-W60194 SECTION.                                               
188900                                                                          
189100     COMPUTE W-PTOP2-OCC-LL = T94-MID-KVPOST * 67                         
189200     COMPUTE PTOP2-LL = W-PTOP2-OCC-LL + 40                               
189210                                                                          
189300     MOVE MFS-KDMFSFOR            TO PTOP2-KDMFSFOR                       
189600                                                                          
189710     IF FOERSTA-T91-JA                                                    
189711        PERFORM IMS-ISRT-MSG-ALT2-6194                                    
189730        MOVE NEJ     TO FOERSTA-T94-SW                                    
189740     ELSE                                                                 
189750        PERFORM IMS-PURG-MSG-ALT2-6194                                    
189760     END-IF                                                               
190000     .                                                                    
190100     EJECT                                                                
190200******************************************************************        
190300*  BILD-REDIGERING                                               *        
190400******************************************************************        
190500*----------------------------------------------------------------*        
190600 S90-BLANKUTF-NUM-FAELT SECTION.                                          
190700                                                                          
190800*----NKL-FÄLT                                                             
190900     INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE              
191000     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
191200*----HUV-FÄLT                                                             
191300     INSPECT MOD-KVAVIS REPLACING LEADING ZERO BY SPACE                   
191400                                                                          
191500*----RAD-FÄLT                                                             
191600*--- INDEXERADE RADER                                                     
191700                                                                          
191710     IF MID-EJ-IFYLLD                                                     
191800       MOVE +1 TO IX                                                      
191900       PERFORM UNTIL IX > MAX-IX                                          
192000          INSPECT MOD-KVFLETI(IX) REPLACING LEADING ZERO BY SPACE         
192100          INSPECT MOD-KVINLART(IX) REPLACING LEADING ZERO BY SPACE        
192200             IF INDATA-OK AND NYCKLAR-OK AND                              
192201                (GODK-MID OR HELP-MID)                                    
192202                MOVE MFS-FORMATETS-ATTR   TO MOD-KVFLETI-ATTR(IX)         
192210                                             MOD-KVINLART-ATTR(IX)        
192220             ELSE                                                         
192222                IF INDATA-FEL                                             
192223                   CONTINUE                                               
192224                ELSE                                                      
192225                   MOVE MFS-STAENG-FAELT TO MOD-KVFLETI-ATTR(IX)          
192226                                            MOD-KVINLART-ATTR(IX)         
192230                END-IF                                                    
192240             END-IF                                                       
192300                                                                          
192400          ADD +1 TO IX                                                    
192500       END-PERFORM                                                        
192510     END-IF                                                               
192600     .                                                                    
192700     EJECT                                                                
192800     SKIP3                                                                
