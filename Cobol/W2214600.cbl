000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2214600.                                    
000300 AUTHOR.                     IDK INGVAR CARLSSON.                         
000400 DATE-WRITTEN.               FEBRUARI 1979.                               
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
001000*        PROGRAMMET SKAPAR ETT LEVERANSPLANEKONCEPT FÖR                   
001100*        VARJE ARTIKEL PÅ INFILEN W22151                                  
001200*                                                                         
001300*    SUBPROGRAM.                                                          
001400*        W2214610    SKÖTER ALL IMS-HANTERING.                            
001500*                                                                         
001600*    RETURKODER.                                                          
001700*        21          PERIODTABELL FÖR LITEN.                              
001800*                                                                         
001900*    ÄNDRING.                                                             
002000*       2003-09-19.  INLAGT FILSKAPANDE FÖR SPX (EUROPE) GMBH             
002100*                                            /CE- -P.D.                   
002200*       2007-12-20.                                                       
002300*        ETRACKER 4820410. DO NOT INCLUDE OVERSTOCK AT MICRO-LDC          
002400*        TILLKOMMER CALL PÅ WDB6-LÄSNING. /CE                             
002500*                                                                         
002600*       2014-05-18.                                                       
002700*        ETRACKER 10235336 Borttag av leveransplankoncept.                
002800*        /Inger Stening                                                   
002900*                                                                         
003000     EJECT                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 CONFIGURATION SECTION.                                                   
003400 SPECIAL-NAMES.                                                           
003500     DECIMAL-POINT IS COMMA.                                              
003600                                                                          
003700 INPUT-OUTPUT SECTION.                                                    
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000*--------------------------------------- TRANSAKTIONER FÖR FRAM-          
004100*                                        STÄLLNING AV LEVERANS-           
004200*                                        PLANEKONCEPT                     
004300                                                                          
004400     SELECT W22151 ASSIGN UT-S-W22146D1.                                  
004500     SKIP2                                                                
004600*--------------------------------------- LEVERANSPLANEKONCEPT 3           
004700                                                                          
004800     SELECT MAIL2-FIL ASSIGN UT-S-W22146D2.                               
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100 FILE SECTION.                                                            
005200     SKIP2                                                                
005300 FD  W22151                                                               
005400     RECORDING F                                                          
005500     BLOCK 0                                                              
005600     LABEL RECORD STANDARD.                                               
005700                                                                          
005800*01  -COPY W221LI51   -L                                                  
005900     SKIP3                                                                
006000 FD  MAIL2-FIL                                                            
006100     RECORDING V                                                          
006200     BLOCK 0                                                              
006300     LABEL RECORD STANDARD.                                               
006400 01  MAIL2-POST     PIC X(246).                                           
006500                                                                          
006600     EJECT                                                                
006700 WORKING-STORAGE SECTION.                                                 
006800                                                                          
006900 77  IDPGM                       PIC X(8)    VALUE 'W2214600'.            
007000 77  CURRENT-SECTION             PIC X(30)  VALUE SPACE.                  
007100 77  DBS-SECTION                 PIC X(30)  VALUE SPACE.                  
007200     SKIP2                                                                
007300*    -COPY WY2000W7                                                       
007400     SKIP3                                                                
007500*    -COPY WY2000W3                                                       
007600     SKIP3                                                                
007700*    -COPY WY2000W1                                                       
007800     SKIP3                                                                
007900*    -COPY WY2000W9                                                       
008000     SKIP3                                                                
008100 77  FELTEXT                 PIC X(80) VALUE SPACE.                       
008200 01  RKOD                    PIC S9(4)               COMP SYNC.           
008300                                                                          
008400*--------------------------------------- ALLMÄNNA ARBETSAREOR             
008500 01  W.                                                                   
008600   03  FILLER.                                                            
008700     05  W-DDATUM-AAVV       PIC 9(4).                                    
008800     05  FILLER              REDEFINES W-DDATUM-AAVV.                     
008900         10  W-DDATUM-AA     PIC 9(2).                                    
009000         10  W-DDATUM-VV     PIC 9(2).                                    
009100     05  W-DDATUM-AAPP       PIC 9(4).                                    
009200     05  FILLER              REDEFINES W-DDATUM-AAPP.                     
009300         10  FILLER          PIC 9(2).                                    
009400         10  W-DDATUM-PP     PIC 9(2).                                    
009500     05  W-DATUM-AAVV        PIC 9(4).                                    
009600     05  FILLER              REDEFINES W-DATUM-AAVV.                      
009700         10  W-DATUM-AA      PIC 9(2).                                    
009800         10  W-DATUM-VV      PIC 9(2).                                    
009900     05  W-PERIOD-AAPP       PIC 9(4).                                    
010000     05  FILLER              REDEFINES W-PERIOD-AAPP.                     
010100         10  W-PERIOD-AA     PIC 9(2).                                    
010200         10  W-PERIOD-PP     PIC 9(2).                                    
010300     05  W-START-AAAAPP      PIC 9(6).                                    
010400     05  FILLER  REDEFINES W-START-AAAAPP.                                
010500         07  W-START-SEKEL   PIC 9(2).                                    
010600         07  W-START-AAPP    PIC 9(4).                                    
010700         07  FILLER          REDEFINES W-START-AAPP.                      
010800             10  W-START-AA  PIC 9(2).                                    
010900             10  W-START-PP  PIC 9(2).                                    
011000     05  FILLER  REDEFINES W-START-AAAAPP.                                
011100         07  W-START-AAAA    PIC 9(4).                                    
011200         07  FILLER          PIC 9(2).                                    
011300     05  W-TIAAPP            PIC 9(4).                                    
011400     05  FILLER              REDEFINES W-TIAAPP.                          
011500         10  W-TIAA          PIC 9(2).                                    
011600         10  W-TIPP          PIC 9(2).                                    
011700     05  W-START-TIAA        PIC S9(3)    COMP-3.                         
011800     05  W-START-TIPP        PIC S9(3)    COMP-3.                         
011900     05  WS-AAPP             PIC 9(4)    VALUE ZERO.                      
012000     05  FILLER REDEFINES    WS-AAPP.                                     
012100         07 WS-AA            PIC 9(2).                                    
012200         07 WS-PP            PIC 9(2).                                    
012300     05  WS-TIAAVV-GRP       PIC 9(4)    VALUE ZERO.                      
012400     05  W-OIPP              PIC 9(2)    OCCURS 12.                       
012500     05  W-SUTPO-TOT         PIC S9(7)               COMP-3.              
012600     05  W-TILEVPL-1         PIC S9(7)               COMP-3.              
012700     05  W-TILEVPL-AAVVD     PIC  9(5).                                   
012800     05 FILLER               PIC X(16) VALUE 'WSUM'.                      
012900*    ARBETSFÄLT FÖR SUMMERING I M-SUMMERA-OKS ------------                
013000     05  WSUM-KVOKS-DAG      PIC S9(7) OCCURS 2      COMP-3.              
013100     05  WSUM-KVOKS-BULK     PIC S9(7) OCCURS 2      COMP-3.              
013200     05  WSUM-KVOKS-VOR      PIC S9(7) OCCURS 2      COMP-3.              
013300*----------------------------------------------------------               
013400   03  W-TX.                                                              
013500     05  W-ORS-TX            PIC X(9)    OCCURS 3.                        
013600                                                                          
013700   03  W-TPO.                                                             
013800     05  W-SUTPO-TOT-VECKA   PIC S9(7)   OCCURS 7    COMP-3.              
013900     05  W-STRECK            PIC X       OCCURS 7.                        
014000     05  W-TIBEHOV           PIC S9(5)   OCCURS 7    COMP-3.              
014100                                                                          
014200*----------------------------------------------------------               
014300   03  IDAG-AAAAMMDD         PIC 9(8).                                    
014400   03  FILLER  REDEFINES IDAG-AAAAMMDD.                                   
014500       05  IDAG-SEKEL        PIC 99.                                      
014600       05  IDAG-AA           PIC 99.                                      
014700       05  IDAG-MM           PIC 99.                                      
014800       05  IDAG-DD           PIC 99.                                      
014900   03  FILLER  REDEFINES IDAG-AAAAMMDD.                                   
015000       05  IDAG-TIAAAA       PIC 9(4).                                    
015100       05  FILLER            PIC 9(4).                                    
015200                                                                          
015300   03  WS-KVPB-PLAN-MASK     PIC S9(6)V9(1) COMP-3 VALUE ZERO.            
015400                                                                          
015500   03  ARB-FAELT.                                                         
015600       05  ARB-SALDO         PIC S9(7)  VALUE ZERO   COMP-3.              
015700       05  ARB-KVAKS         PIC S9(7)  VALUE ZERO   COMP-3.              
015800       05  ARB-KVLS          PIC S9(7)  VALUE ZERO   COMP-3.              
015900       05  ARB-KVRESS        PIC S9(7)  VALUE ZERO   COMP-3.              
016000       05  ARB-KVROS         PIC S9(7)  VALUE ZERO   COMP-3.              
016100       05  ARB-TIDISPIN      PIC S9(7)  VALUE ZERO   COMP-3.              
016200       05  ARB-KVOKS-BULK    PIC S9(7)  VALUE ZERO   COMP-3.              
016300       05  ARB-KVOKS-DAG     PIC S9(7)  VALUE ZERO   COMP-3.              
016400       05  ARB-KVOKS-VOR     PIC S9(7)  VALUE ZERO   COMP-3.              
016500       05  ARB-SUTPO-TOT     PIC S9(7)  VALUE ZERO   COMP-3.              
016600       05  ARB-SUTPO-TOT-VECKA    PIC S9(7)  VALUE ZERO   COMP-3.         
016700       05  ARB-SUTPO-NAESTA-INLEV PIC S9(7)  VALUE ZERO   COMP-3.         
016800                                                                          
016900     EJECT                                                                
017000*--------------------------------------- SWITCHAR                         
017100 01  SWITCHAR.                                                            
017200     05  SW-W22151-EOF       PIC X       VALUE 'N'.                       
017300     SKIP3                                                                
017400*--------------------------------------- INDEXFÄLT                        
017500 01  INDEXFALT.                                                           
017600     05  IX                  PIC S9(9)               COMP SYNC.           
017700     05  IY                  PIC S9(9)               COMP SYNC.           
017800     05  IXORS               PIC S9(9)               COMP SYNC.           
017900     05  IXDO                PIC S9(9)               COMP SYNC.           
018000     SKIP3                                                                
018100*--------------------------------------- KONSTANTER                       
018200 01  KONSTANTER.                                                          
018300     05  W-PROGNAMN          PIC X(6)    VALUE 'W22146'.                  
018400     05  JA                  PIC X       VALUE 'J'.                       
018500     05  NEJ                 PIC X       VALUE 'N'.                       
018600     05  MAIL2-POST-SKRIVEN   PIC X       VALUE 'N'.                      
018700     05  LAS-ALLMAN-INFO     PIC S9(3)   VALUE +461  COMP-3.              
018800     05  LAS-BESTREST        PIC S9(3)   VALUE +469  COMP-3.              
018900     05  LAS-LEV-INFO        PIC S9(3)   VALUE +470  COMP-3.              
019000     05  LAS-TPO-ORDER       PIC S9(3)   VALUE +471  COMP-3.              
019100     05  LAES-WDB6-DC-INFO   PIC S9(3)   VALUE +475  COMP-3.              
019200     EJECT                                                                
019300*--------------------------------------- DYNAMISKA SUBPROGRAM             
019400 01  DYNAMISKA-SUBPROGRAM.                                                
019500     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
019600     05  W2214610            PIC X(8)    VALUE 'W2214610'.                
019700     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
019800     05  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
019900     05  ABEND               PIC X(8)    VALUE 'ABEND   '.                
020000     05  FELLOG              PIC X(8)    VALUE 'FELLOG '.                 
020100     05  W222PBTO            PIC X(8)    VALUE 'W222PBTO'.                
020200     EJECT                                                                
020300*01  -COPY W0005 -PRE POSTSUM-                                            
020400     EJECT                                                                
020500*--------------------------------------- PARAMETRAR TILL DATKORT          
020600 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
020700*01  -COPY WDATKORT                                                       
020800     EJECT                                                                
020900*                            *** PARAMETRAR TILL DATKONV                  
021000*01  -COPY WDATAREA                                                       
021100     EJECT                                                                
021200*01  -COPY W222PBTO                                                       
021300     EJECT                                                                
021400*--------------------------------------- TABELL INNEHÅLLANDE              
021500*                                        INFO FRÅN PERIODFILEN            
021600                                                                          
021700*01  -COPY W221W005   -PRE LPORS-                                         
021800     EJECT                                                                
021900*--------------------------------------- AREA FÖR W22153-POST             
022000*--------------------------------------- EXCEL VIA MAIL                   
022100 01  MAIL-EXCEL-RUBRIK.                                                   
022200     03  FILLER  PIC X(9)  VALUE ' Supplier'.                             
022300     03  FILLER  PIC X     VALUE X'05'.                                   
022400     03  FILLER  PIC X(8)  VALUE 'Procurer'.                              
022500     03  FILLER  PIC X     VALUE X'05'.                                   
022600     03  FILLER  PIC X(7)  VALUE 'Partno.'.                               
022700     03  FILLER  PIC X     VALUE X'05'.                                   
022800     03  FILLER  PIC X(12) VALUE 'Volvo Descr.'.                          
022900     03  FILLER  PIC X     VALUE X'05'.                                   
023000     03  FILLER  PIC X(6)  VALUE 'Reason'.                                
023100     03  FILLER  PIC X     VALUE X'05'.                                   
023200     03  FILLER  PIC X(13) VALUE 'Suppl. Descr.'.                         
023300     03  FILLER  PIC X     VALUE X'05'.                                   
023400     03  FILLER  PIC X(10) VALUE 'St.on hand'.                            
023500     03  FILLER  PIC X     VALUE X'05'.                                   
023600     03  FILLER  PIC X(10) VALUE 'F.c. plan.'.                            
023700     03  FILLER  PIC X     VALUE X'05'.                                   
023800     03  FILLER  PIC X(6)  VALUE 'Note 1'.                                
023900     03  FILLER  PIC X     VALUE X'05'.                                   
024000     03  FILLER  PIC X(6)  VALUE 'Note 2'.                                
024100     03  FILLER  PIC X     VALUE X'05'.                                   
024200     03  FILLER  PIC X(8)  VALUE 'Sup.code'.                              
024300     03  FILLER  PIC X     VALUE X'05'.                                   
024400     03  FILLER  PIC X(13) VALUE 'This schedule'.                         
024500     03  FILLER  PIC X     VALUE X'05'.                                   
024600     03  FILLER  PIC X(14) VALUE 'Earlier sched.'.                        
024700     03  FILLER  PIC X     VALUE X'05'.                                   
024800*              sum 146 bytes wide                                         
024900*                                                                         
025000 01  MAIL2-EXCEL-NODATA.                                                  
025100     03  FILLER  PIC X(85) VALUE ' No parts for delivery scheduled        
025200-                                'proposal this week for SPX (EUR         
025300-                                'OPE) GMBH, procurers.'.                 
025400     03  FILLER  PIC X     VALUE X'05'.                                   
025500     03  FILLER  PIC X     VALUE X'05'.                                   
025600     03  FILLER  PIC X     VALUE X'05'.                                   
025700     03  FILLER  PIC X     VALUE X'05'.                                   
025800     03  FILLER  PIC X     VALUE X'05'.                                   
025900     03  FILLER  PIC X     VALUE X'05'.                                   
026000     03  FILLER  PIC X     VALUE X'05'.                                   
026100     03  FILLER  PIC X     VALUE X'05'.                                   
026200     03  FILLER  PIC X     VALUE X'05'.                                   
026300     03  FILLER  PIC X     VALUE X'05'.                                   
026400     03  FILLER  PIC X     VALUE X'05'.                                   
026500     03  FILLER  PIC X     VALUE X'05'.                                   
026600     03  FILLER  PIC X     VALUE X'05'.                                   
026700*                                                                         
026800*                                                                         
026900 01  MAIL-EXCEL-RAD.                                                      
027000     03  FILLER           PIC X       VALUE SPACE.                        
027100     03  MAIL-IDLEVNR     PIC X(5)    VALUE SPACE.                        
027200     03  FILLER           PIC X       VALUE X'05'.                        
027300     03  MAIL-IDANSK      PIC 9(3)    VALUE ZERO.                         
027400     03  FILLER           PIC X       VALUE X'05'.                        
027500     03  MAIL-IDARTNR     PIC 9(9)    VALUE ZERO.                         
027600     03  FILLER           PIC X       VALUE X'05'.                        
027700     03  MAIL-BEART       PIC X(25)   VALUE SPACE.                        
027800     03  FILLER           PIC X       VALUE X'05'.                        
027900     03  MAIL-ORS-TX-1    PIC X(9)    VALUE SPACES.                       
028000     03  FILLER           PIC X       VALUE ' '.                          
028100     03  MAIL-ORS-TX-2    PIC X(9)    VALUE SPACES.                       
028200     03  FILLER           PIC X       VALUE ' '.                          
028300     03  MAIL-ORS-TX-3    PIC X(9)    VALUE SPACES.                       
028400     03  FILLER           PIC X       VALUE X'05'.                        
028500     03  MAIL-BELEV       PIC X(35)   VALUE space.                        
028600     03  FILLER           PIC X       VALUE X'05'.                        
028700     03  MAIL-ARB-SALDO   PIC -(6)9   VALUE ZERO.                         
028800     03  FILLER           PIC X       VALUE X'05'.                        
028900     03  MAIL-KVPB-PLAN-MASK PIC -(6)9,9 BLANK WHEN ZERO.                 
029000     03  FILLER              PIC X    VALUE X'05'.                        
029100     03  MAIL-TEARTNOT-1  PIC X(40)   VALUE SPACE.                        
029200     03  FILLER           PIC X       VALUE X'05'.                        
029300     03  MAIL-TEARTNOT-2  PIC X(40)   VALUE SPACE.                        
029400     03  FILLER           PIC X       VALUE X'05'.                        
029500     03  MAIL-KDERS       PIC 9(2)    BLANK WHEN ZERO.                    
029600     03  FILLER              PIC X    VALUE X'05'.                        
029700     03  FILLER           PIC X       VALUE '*'.                          
029800     03  MAIL-DDATUM-AAVV    PIC 9(4) VALUE ZERO.                         
029900     03  FILLER              PIC X    VALUE X'05'.                        
030000     03  FILLER           PIC X       VALUE '*'.                          
030100     03  MAIL-TILEVPL-AAVVD  PIC 9(5) VALUE ZERO.                         
030200     03  FILLER              PIC X    VALUE X'05'.                        
030300*                       tot 242 bytes wide                                
030400     EJECT                                                                
030500                                                                          
030600*--------------------------------------- AREA FÖR W22151-POST             
030700                                                                          
030800*01  AREA -COPY W221LI51   -PRE TRANS-                                    
030900     EJECT                                                                
031000******************************************************************        
031100     EJECT                                                                
031200 01  FILLER                 PIC X(16)    VALUE 'IMS1-AREA'.               
031300*01  AREA -COPY W221L461   -PRE IMS1-                                     
031400     EJECT                                                                
031500 01  FILLER                 PIC X(16)    VALUE 'IMS9-AREA'.               
031600*01  AREA -COPY W221L469   -PRE IMS9-                                     
031700     EJECT                                                                
031800 01  FILLER                 PIC X(16)    VALUE 'IMS10-AREA'.              
031900*01  AREA -COPY W221L470   -PRE IMS10-                                    
032000     EJECT                                                                
032100 01  FILLER                 PIC X(16)    VALUE 'IMS11-AREA'.              
032200*01  AREA -COPY W221L471   -PRE IMS11-                                    
032300     EJECT                                                                
032400 LINKAGE SECTION.                                                         
032500     SKIP2                                                                
032600 01  WDF5-PCB                PIC X.                                       
032700     SKIP1                                                                
032800 01  WLARTC2-PCB             PIC X.                                       
032900     SKIP1                                                                
033000 01  WLINLB-PCB              PIC X.                                       
033100     SKIP1                                                                
033200 01  WLBENA-PCB              PIC X.                                       
033300     SKIP1                                                                
033400 01  WLARTM-PCB              PIC X.                                       
033500     SKIP1                                                                
033600 01  WDK7-PCB                PIC X.                                       
033700     SKIP1                                                                
033800 01  WDB601-PCB              PIC X.                                       
033900     SKIP1                                                                
034000 01  WL2501-PCB              PIC X.                                       
034100     SKIP1                                                                
034200 01  REFL-WDB6-PCB           PIC X.                                       
034300     SKIP1                                                                
034400 01  REFL-WDK7-PCB           PIC X.                                       
034500     SKIP1                                                                
034600 01  WDK6-PCB                PIC X.                                       
034700     SKIP1                                                                
034800 01  WDB6-PCB                PIC X.                                       
034900     EJECT                                                                
035000 01  WDD7-PCB                PIC X.                                       
035100     EJECT                                                                
035200 01  WDK7E-PCB               PIC X.                                       
035300     EJECT                                                                
035400 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
035500     EJECT                                                                
035600 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
035700     EJECT                                                                
035800 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
035900     EJECT                                                                
036000 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
036100     EJECT                                                                
036200 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
036300     EJECT                                                                
036400 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
036500     EJECT                                                                
036600 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
036700     EJECT                                                                
036800 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
036900     EJECT                                                                
037000 PROCEDURE DIVISION USING WDF5-PCB WLARTC2-PCB                            
037100                    WLINLB-PCB WLBENA-PCB    WLARTM-PCB                   
037200                    WDK7-PCB   WDB6-PCB                                   
037300                    WL2501-PCB REFL-WDB6-PCB REFL-WDK7-PCB                
037400                    WDK6-PCB   WDB6-PCB                                   
037500                    WDD7-PCB   WDK7E-PCB                                  
037600                    PBTO-W222-UTIL-WDK6-PCB                               
037700                    PBTO-W222-UTIL-WDK7-PCB                               
037800                    PBTO-W222-UTIL-WDB6-PCB                               
037900                    PBTO-W222-UTUP-WDK7-PCB                               
038000                    PBTO-W222-UTUP-WDB6-PCB                               
038100                    PBTO-W222-UTUP-UTIL-WDK6-PCB                          
038200                    PBTO-W222-UTUP-UTIL-WDK7-PCB                          
038300                    PBTO-W222-UTUP-UTIL-WDB6-PCB                          
038400                    .                                                     
038500                                                                          
038600     ENTRY 'DLITCBL' USING WDF5-PCB WLARTC2-PCB                           
038700                    WLINLB-PCB WLBENA-PCB    WLARTM-PCB                   
038800                    WDK7-PCB   WDB6-PCB                                   
038900                    WL2501-PCB REFL-WDB6-PCB REFL-WDK7-PCB                
039000                    WDK6-PCB   WDB6-PCB                                   
039100                    WDD7-PCB   WDK7E-PCB                                  
039200                    PBTO-W222-UTIL-WDK6-PCB                               
039300                    PBTO-W222-UTIL-WDK7-PCB                               
039400                    PBTO-W222-UTIL-WDB6-PCB                               
039500                    PBTO-W222-UTUP-WDK7-PCB                               
039600                    PBTO-W222-UTUP-WDB6-PCB                               
039700                    PBTO-W222-UTUP-UTIL-WDK6-PCB                          
039800                    PBTO-W222-UTUP-UTIL-WDK7-PCB                          
039900                    PBTO-W222-UTUP-UTIL-WDB6-PCB                          
040000                    .                                                     
040100     SKIP2                                                                
040200     PERFORM A-INITIERING                                                 
040300     PERFORM B-LAS-TRANS                                                  
040400                                                                          
040500     PERFORM UNTIL SW-W22151-EOF = JA                                     
040600       PERFORM C-INITIERA-LIST-FLT                                        
040700       PERFORM D-LAS-ALLMAN-INFO                                          
040800       IF IMS1-ANROP-OK                                                   
040900         PERFORM F-LAS-LEV-INFO                                           
041000         PERFORM K-LAS-TPO-ORDER                                          
041100         PERFORM N-SKRIV-RLILEV-RADER                                     
041200                                                                          
041300         If ( TRANS-IDANSK = 810 Or 811 Or 812 Or 813 Or 814              
041400                   Or 815 Or 816 Or 817 Or 818 Or 819 )                   
041500         AND ( IMS1-IDLEVNR = 'MWAJB' )                                   
041600           PERFORM P-SKRIV-MAIL-RAD                                       
041700         End-If                                                           
041800                                                                          
041900       END-IF                                                             
042000       PERFORM B-LAS-TRANS                                                
042100     END-PERFORM                                                          
042200                                                                          
042300     PERFORM Z-AVSLUTNING                                                 
042400     MOVE ZERO TO RETURN-CODE                                             
042500     GOBACK                                                               
042600     .                                                                    
042700     EJECT                                                                
042800******************************************************************        
042900*                                                                *        
043000*    INITIERING                                                  *        
043100*    SKRIV SEPARATORSIDOR                                        *        
043200*    INITIERA ARBETSFÄLT                                         *        
043300*    REDIGERA VECKA OCH PERIOD FRÅN DATUMKORT                    *        
043400*                                                                *        
043500******************************************************************        
043600                                                                          
043700 A-INITIERING SECTION.                                                    
043800     MOVE 'A-INITIERING          ' TO CURRENT-SECTION                     
043900                                                                          
044000     OPEN INPUT W22151                                                    
044100         OUTPUT MAIL2-FIL                                                 
044200                                                                          
044300     CALL DATKORT        USING W-PROGNAMN DATUMKORT-ID DATUMKORT          
044400     MOVE D-AAR             TO W-DDATUM-AA IDAG-AA                        
044500     MOVE D-VECKA           TO W-DDATUM-VV                                
044600     MOVE W-DDATUM-AAVV     TO IMS1-TIAAVV-AKT                            
044700     MOVE D-AAR             TO W-PERIOD-AA                                
044800                                                                          
044900     MOVE W-DDATUM-AAVV     TO DAT-I-TIDATUM                              
045000     MOVE 'AAVV'            TO DAT-KDDATFORM                              
045100     CALL WDATKONV       USING DAT-KDDATFORM                              
045200                               DAT-I-TIDATUM                              
045300                               DAT-O-TIDATUM                              
045400                               DAT-KDSVAR                                 
045500     IF DAT-KDSVAR-FEL                                                    
045600          MOVE 'FELAKTIGT DATUM - DATKONV 1 ' TO FELTEXT                  
045700          DISPLAY 'FELTEXT ' FELTEXT                                      
045800          CALL FELLOG                                                     
045900     END-IF                                                               
046000     MOVE DAT-TIAA          TO W-START-TIAA                               
046100     MOVE DAT-TIRP          TO W-START-TIPP W-PERIOD-PP                   
046200                                                                          
046300     MOVE W-PERIOD-AAPP     TO W-DDATUM-AAPP                              
046400     MOVE W-PERIOD-AAPP     TO W-START-AAPP                               
046500     DISPLAY ' W-START-AAPP ' W-START-AAPP                                
046600     MOVE D-MAANAD          TO IDAG-MM                                    
046700     MOVE D-DAG             TO IDAG-DD                                    
046800     IF IDAG-AA > 50                                                      
046900        MOVE 19             TO IDAG-SEKEL  W-START-SEKEL                  
047000     ELSE                                                                 
047100        MOVE 20             TO IDAG-SEKEL  W-START-SEKEL                  
047200     END-IF                                                               
047300                                                                          
047400     PERFORM AA-SKRIV-EXCEL-RUBRIK                                        
047500                                                                          
047600*    --- BYGG UPP IDDC-TABELL I W2214610                                  
047700     Move LAES-WDB6-DC-INFO To IMS1-KDCALL                                
047800     Call W2214610 Using IMS1-AREA                                        
047900                   WDF5-PCB WLARTC2-PCB                                   
048000                   WLINLB-PCB WLBENA-PCB WLARTM-PCB                       
048100                   WDK7-PCB   WDB6-PCB                                    
048200*--------------------------------------- POSTSUM PARMFIL                  
048300     MOVE 'W22151' TO POSTSUM-FDNAMN                                      
048400     MOVE 'W22146D1' TO POSTSUM-DDNAMN2                                   
048500     .                                                                    
048600     EJECT                                                                
048700                                                                          
048800 AA-SKRIV-EXCEL-RUBRIK  SECTION.                                          
048900     MOVE 'AC-SKRIV-EXCEL-RUBRIK ' TO CURRENT-SECTION                     
049000                                                                          
049100     WRITE MAIL2-POST FROM MAIL-EXCEL-RUBRIK                              
049200                                                                          
049300     MOVE 'W22153'   TO POSTSUM-FDNAMN                                    
049400     MOVE 'W22146D2' TO POSTSUM-DDNAMN2                                   
049500     MOVE 'Mai2'     TO POSTSUM-TRANSTYP                                  
049600     CALL POSTSUM USING POSTSUM-PARM                                      
049700     .                                                                    
049800     EJECT                                                                
049900                                                                          
050000 B-LAS-TRANS SECTION.                                                     
050100     MOVE 'B-LAS-TRANS           ' TO CURRENT-SECTION                     
050200                                                                          
050300     READ W22151 INTO TRANS-AREA                                          
050400         AT END MOVE JA TO SW-W22151-EOF                                  
050500     END-READ                                                             
050600                                                                          
050700     IF SW-W22151-EOF = NEJ                                               
050800         MOVE 'W22151'   TO POSTSUM-FDNAMN                                
050900         MOVE 'W22146D1' TO POSTSUM-DDNAMN2                               
051000         MOVE 'LI51'     TO POSTSUM-TRANSTYP                              
051100         CALL POSTSUM USING POSTSUM-PARM                                  
051200     END-IF                                                               
051300     .                                                                    
051400     EJECT                                                                
051500******************************************************************        
051600*                                                                *        
051700*    INITIERA LIST FLT                                           *        
051800*    NOLLA OCH BLANKA UT ARBETSFÄLT TILL LISTAN                  *        
051900*                                                                *        
052000******************************************************************        
052100                                                                          
052200 C-INITIERA-LIST-FLT SECTION.                                             
052300     MOVE 'C-INITIERA-LIST-FLT   ' TO CURRENT-SECTION                     
052400                                                                          
052500     MOVE ZERO TO W-TILEVPL-1                                             
052600                  W-TILEVPL-AAVVD                                         
052700                                                                          
052800     MOVE SPACE TO W-TX                                                   
052900     .                                                                    
053000     EJECT                                                                
053100******************************************************************        
053200*                                                                *        
053300*    LAS ALLMAN INFO                                             *        
053400*    REDIGERAR UT ARTIKELINFORMATION TILL LISTAN                 *        
053500*                                                                *        
053600******************************************************************        
053700                                                                          
053800 D-LAS-ALLMAN-INFO SECTION.                                               
053900     MOVE 'D-LAS-ALLMAN-INFO     ' TO CURRENT-SECTION                     
054000                                                                          
054100     MOVE TRANS-IDARTNR             TO IMS1-IDARTNR                       
054200     MOVE LAS-ALLMAN-INFO           TO IMS1-KDCALL                        
054300     CALL W2214610 USING IMS1-AREA                                        
054400                   WDF5-PCB WLARTC2-PCB                                   
054500                   WLINLB-PCB WLBENA-PCB WLARTM-PCB                       
054600                   WDK7-PCB   WDB6-PCB                                    
054700*--------------------------------------- SENASTE INLEVERANSEN AV          
054800*                                        HUVUDLEVERANTÖR TILL             
054900*                                        C1/C2                            
055000     IF IMS1-ANROP-OK                                                     
055100       MOVE IMS1-KVLS   (1)         TO ARB-KVLS                           
055200                                                                          
055300       MOVE IMS1-KVRESS (1)         TO ARB-KVRESS                         
055400                                                                          
055500       MOVE IMS1-KVAKS  (1)         TO ARB-KVAKS                          
055600                                                                          
055700       MOVE IMS1-KVROS  (1)         TO ARB-KVROS                          
055800*                                                                         
055900*--------------------------------------------MANUELLT KVPB ?              
056000                                                                          
056100       MOVE IMS1-IDARTNR            TO PBTO-IDARTNR                       
056200       CALL W222PBTO USING PBTO-W222PBTO                                  
056300                           WDK6-PCB                                       
056400                           WDK7-PCB                                       
056500                           WLARTM-PCB                                     
056600                           WL2501-PCB                                     
056700                           REFL-WDB6-PCB                                  
056800                           REFL-WDK7-PCB                                  
056900                           WDB6-PCB                                       
057000                           WDD7-PCB                                       
057100                           WDK7E-PCB                                      
057200                           PBTO-W222-UTIL-WDK6-PCB                        
057300                           PBTO-W222-UTIL-WDK7-PCB                        
057400                           PBTO-W222-UTIL-WDB6-PCB                        
057500                           PBTO-W222-UTUP-WDK7-PCB                        
057600                           PBTO-W222-UTUP-WDB6-PCB                        
057700                           PBTO-W222-UTUP-UTIL-WDK6-PCB                   
057800                           PBTO-W222-UTUP-UTIL-WDK7-PCB                   
057900                           PBTO-W222-UTUP-UTIL-WDB6-PCB                   
058000                                                                          
058100       IF PBTO-KDSVAR = JA                                                
058200          MOVE PBTO-KVPB-PLAN       TO WS-KVPB-PLAN-MASK                  
058300       ELSE                                                               
058400          MOVE ZERO                 TO WS-KVPB-PLAN-MASK                  
058500       END-IF                                                             
058600                                                                          
058700     END-IF                                                               
058800     .                                                                    
058900     EJECT                                                                
059000                                                                          
059100******************************************************************        
059200*                                                                *        
059300*    LAS TPO-ORDER INFO                                          *        
059400*                                                                *        
059500******************************************************************        
059600                                                                          
059700 K-LAS-TPO-ORDER SECTION.                                                 
059800     MOVE 'K-LAS-TPO-ORDER       ' TO CURRENT-SECTION                     
059900                                                                          
060000     MOVE ZERO TO IMS11-SUTPO-TOT (1)                                     
060100                  IMS11-SUTPO-TOT (2)                                     
060200                  IMS11-KVOKS-DAG (1)                                     
060300                  IMS11-KVOKS-DAG (2)                                     
060400                  IMS11-KVOKS-BULK (1)                                    
060500                  IMS11-KVOKS-BULK (2)                                    
060600                  IMS11-KVOKS-VOR (1)                                     
060700                  IMS11-KVOKS-VOR (2)                                     
060800                                                                          
060900     MOVE +1 TO IXDO                                                      
061000     PERFORM UNTIL IXDO > 7                                               
061100        MOVE ZERO  TO W-TIBEHOV (IXDO)                                    
061200        MOVE SPACE TO W-STRECK (IXDO)                                     
061300        MOVE ZERO  TO W-SUTPO-TOT-VECKA (IXDO)                            
061400                                                                          
061500        MOVE ZERO  TO IMS11-TIBEHOV (IXDO)                                
061600        MOVE ZERO  TO IMS11-SUTPO-TOT-VECKA (IXDO)                        
061700                                                                          
061800        ADD +1     TO IXDO                                                
061900     END-PERFORM                                                          
062000                                                                          
062100     MOVE TRANS-IDARTNR        TO IMS11-IDARTNR                           
062200     MOVE LAS-TPO-ORDER        TO IMS11-KDCALL                            
062300                                                                          
062400     CALL W2214610 USING IMS11-AREA                                       
062500                   WDF5-PCB WLARTC2-PCB                                   
062600                   WLINLB-PCB WLBENA-PCB WLARTM-PCB                       
062700                   WDK7-PCB   WDB6-PCB                                    
062800                                                                          
062900      ADD IMS11-SUTPO-TOT (1)                                             
063000          IMS11-SUTPO-TOT (2) GIVING W-SUTPO-TOT                          
063100      MOVE IMS11-SUTPO-TOT (1) TO ARB-SUTPO-TOT                           
063200                                                                          
063300      MOVE IMS11-KVOKS-DAG (1) TO WSUM-KVOKS-DAG (1) ARB-KVOKS-DAG        
063400      MOVE IMS11-KVOKS-DAG (2) TO WSUM-KVOKS-DAG (2)                      
063500                                                                          
063600      MOVE IMS11-KVOKS-BULK(1) TO WSUM-KVOKS-BULK (1)                     
063700                                                    ARB-KVOKS-BULK        
063800      MOVE IMS11-KVOKS-BULK(2) TO WSUM-KVOKS-BULK (2)                     
063900                                                                          
064000      MOVE IMS11-KVOKS-VOR (1) TO WSUM-KVOKS-VOR (1) ARB-KVOKS-VOR        
064100      MOVE IMS11-KVOKS-VOR (2) TO WSUM-KVOKS-VOR (2)                      
064200                                                                          
064300*__IMS11-KVOKS- BULK, DAG, VOR, SUMMERAS I SECTION M-SUMMERA-OKS          
064400                                                                          
064500     IF IMS11-ANROP-OK                                                    
064600        MOVE +1 TO IXDO                                                   
064700        PERFORM UNTIL IXDO > +7 OR IMS11-TIBEHOV (IXDO) = ZERO            
064800           MOVE IMS11-TIBEHOV (IXDO)                                      
064900                               TO W-TIBEHOV (IXDO)                        
065000           MOVE '-'            TO W-STRECK (IXDO)                         
065100           MOVE IMS11-SUTPO-TOT-VECKA (IXDO)                              
065200                               TO W-SUTPO-TOT-VECKA (IXDO)                
065300           ADD +1              TO IXDO                                    
065400        END-PERFORM                                                       
065500                                                                          
065600     END-IF                                                               
065700                                                                          
065800     MOVE ZERO  TO ARB-SUTPO-TOT-VECKA                                    
065900     IF ARB-TIDISPIN = ZERO                                               
066000        MOVE ARB-SUTPO-TOT     TO ARB-SUTPO-NAESTA-INLEV                  
066100     ELSE                                                                 
066200        MOVE +1 TO IXDO                                                   
066300        PERFORM UNTIL IXDO > +7                                           
066400          IF W-TIBEHOV (IXDO) NOT > ARB-TIDISPIN                          
066500            ADD W-SUTPO-TOT-VECKA (IXDO) TO ARB-SUTPO-TOT-VECKA           
066600          END-IF                                                          
066700          ADD +1 TO IXDO                                                  
066800        END-PERFORM                                                       
066900        MOVE ARB-SUTPO-TOT-VECKA TO ARB-SUTPO-NAESTA-INLEV                
067000     END-IF                                                               
067100     COMPUTE ARB-SALDO ROUNDED = ARB-KVLS    - ARB-KVRESS                 
067200                                             - ARB-KVROS                  
067300                               - ARB-SUTPO-NAESTA-INLEV                   
067400                                             - ARB-KVOKS-DAG              
067500                                             - ARB-KVOKS-BULK             
067600                                             - ARB-KVOKS-VOR              
067700                               + ARB-KVAKS                                
067800     .                                                                    
067900     EJECT                                                                
068000******************************************************************        
068100*                                                                *        
068200*    LAS LEV INFO                                                *        
068300*    HÄMTAR INFORMATION FRÅN LEVERANSPLANEREGISTRET              *        
068400*                                                                *        
068500******************************************************************        
068600                                                                          
068700 F-LAS-LEV-INFO SECTION.                                                  
068800     MOVE 'F-LAS-LEV-INFO        ' TO CURRENT-SECTION                     
068900                                                                          
069000     MOVE TRANS-IDARTNR TO IMS9-IDARTNR                                   
069100     MOVE LAS-BESTREST TO IMS9-KDCALL                                     
069200     CALL W2214610 USING IMS9-AREA                                        
069300                   WDF5-PCB WLARTC2-PCB                                   
069400                   WLINLB-PCB WLBENA-PCB WLARTM-PCB                       
069500                   WDK7-PCB   WDB6-PCB                                    
069600                                                                          
069700     PERFORM UNTIL IMS9-ANROP-FEL                                         
069800         IF  IMS9-IDLEVNR  = IMS1-IDLEVNR                                 
069900*--------------------------------------- HUVUDLEVERANTÖR                  
070000             MOVE TRANS-IDARTNR TO IMS10-IDARTNR                          
070100             MOVE IMS1-IDLEVNR  TO IMS10-IDLEVNR                          
070200             MOVE LAS-LEV-INFO  TO IMS10-KDCALL                           
070300             CALL W2214610 USING IMS10-AREA                               
070400                           WDF5-PCB WLARTC2-PCB                           
070500                           WLINLB-PCB WLBENA-PCB WLARTM-PCB               
070600                           WDK7-PCB   WDB6-PCB                            
070700                                                                          
070800*--------------------------------------- ORSAK TILL LEVERANSPLAN          
070900             MOVE 1 TO IX                                                 
071000             PERFORM UNTIL IX > 3                                         
071100                 MOVE IMS10-KDLPORS-TAB (IX) TO IXORS                     
071200                 IF IXORS > ZERO                                          
071300                 AND IXORS NOT > LPORS-MAXINDEX-1                         
071400                     MOVE LPORS-TELPORS (IXORS) TO W-ORS-TX (IX)          
071500                 END-IF                                                   
071600                 ADD +1 TO IX                                             
071700             END-PERFORM                                                  
071800             MOVE 1 TO IX                                                 
071900             PERFORM UNTIL IX > 3 OR                                      
072000                     TRANS-KDLPORS-TAB (IX) NOT = ZERO                    
072100                 ADD +1 TO IX                                             
072200             END-PERFORM                                                  
072300             IF  IX NOT > 3                                               
072400                 MOVE 1 TO IX                                             
072500                 PERFORM UNTIL IX > 3                                     
072600                     MOVE SPACE TO W-ORS-TX (IX)                          
072700                     MOVE TRANS-KDLPORS-TAB (IX) TO IXORS                 
072800                     IF IXORS > ZERO                                      
072900                     AND IXORS NOT > LPORS-MAXINDEX-1                     
073000                       MOVE LPORS-TELPORS (IXORS) TO W-ORS-TX (IX)        
073100                     END-IF                                               
073200                     ADD +1 TO IX                                         
073300                 END-PERFORM                                              
073400             END-IF                                                       
073500                                                                          
073600             MOVE IMS9-TILEVPL TO W-TILEVPL-1                             
073700             PERFORM FB-TIDIGARE-PLAN                                     
073800         END-IF                                                           
073900                                                                          
074000         MOVE LAS-BESTREST TO IMS9-KDCALL                                 
074100         CALL W2214610 USING IMS9-AREA                                    
074200                       WDF5-PCB WLARTC2-PCB                               
074300                       WLINLB-PCB WLBENA-PCB WLARTM-PCB                   
074400                       WDK7-PCB   WDB6-PCB                                
074500                                                                          
074600     END-PERFORM                                                          
074700     .                                                                    
074800     EJECT                                                                
074900******************************************************************        
075000*                                                                *        
075100*    KONVERTERAR DATUM SENASTE PLAN TILL AAVV                    *        
075200*                                                                *        
075300******************************************************************        
075400                                                                          
075500 FB-TIDIGARE-PLAN SECTION.                                                
075600     MOVE 'FB-TIDIGARE-PLAN      ' TO CURRENT-SECTION                     
075700                                                                          
075800                                                                          
075900     MOVE W-TILEVPL-1       TO DAT-I-TIDATUM                              
076000     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
076100     CALL WDATKONV USING       DAT-KDDATFORM,                             
076200                               DAT-I-TIDATUM,                             
076300                               DAT-O-TIDATUM,                             
076400                               DAT-KDSVAR                                 
076500     IF DAT-KDSVAR-OK                                                     
076600        MOVE DAT-TIAAVVD    TO W-TILEVPL-AAVVD                            
076700     ELSE                                                                 
076800        MOVE ZERO           TO W-TILEVPL-AAVVD                            
076900     END-IF                                                               
077000     EJECT                                                                
077100     .                                                                    
077200                                                                          
077300*****************************************************************         
077400*                                                               *         
077500* FLYTTAR VÄRDEN TILL RADERNA OCH SKRIVER UT HELA KONCEPTET     *         
077600*                                                               *         
077700*****************************************************************         
077800 N-SKRIV-RLILEV-RADER SECTION.                                            
077900     MOVE 'N-SKRIV-RLILEV-RADER  ' TO CURRENT-SECTION                     
078000                                                                          
078100     MOVE TRANS-IDANSK         TO MAIL-IDANSK                             
078200     MOVE W-DDATUM-AAVV        TO MAIL-DDATUM-AAVV                        
078300     MOVE W-TILEVPL-AAVVD      TO MAIL-TILEVPL-AAVVD                      
078400     MOVE IMS1-IDLEVNR         TO MAIL-IDLEVNR                            
078500     MOVE IMS1-IDARTNR         TO MAIL-IDARTNR                            
078600                                                                          
078700     IF IMS1-BEART-GB NOT = SPACE                                         
078800*      -- GB benämning i första hand för Bosch                            
078900       MOVE IMS1-BEART-GB      TO MAIL-BEART                              
079000     ELSE                                                                 
079100*      -- Svensk benämning i sista hand för Bosch                         
079200       MOVE IMS1-BEART-SVE     TO MAIL-BEART                              
079300     END-IF                                                               
079400                                                                          
079500     MOVE W-ORS-TX(1)          TO MAIL-ORS-TX-1                           
079600     MOVE W-ORS-TX(2)          TO MAIL-ORS-TX-2                           
079700     MOVE W-ORS-TX(3)          TO MAIL-ORS-TX-3                           
079800                                                                          
079900     MOVE IMS1-BELEV           TO MAIL-BELEV                              
080000                                                                          
080100     MOVE ARB-SALDO            TO MAIL-ARB-SALDO                          
080200                                                                          
080300     MOVE WS-KVPB-PLAN-MASK    TO MAIL-KVPB-PLAN-MASK                     
080400                                                                          
080500     MOVE IMS1-KDERS (1)       TO MAIL-KDERS                              
080600                                                                          
080700     MOVE IMS1-TEARTNOT(1)     TO MAIL-TEARTNOT-1                         
080800     MOVE IMS1-TEARTNOT(2)     TO MAIL-TEARTNOT-2                         
080900     .                                                                    
081000     EJECT                                                                
081100******************************************************************        
081200*                                                                *        
081300*    AVSLUTNING                                                  *        
081400*    SKRIV AVSLUTANDE SEPARATORSIDOR                             *        
081500*    STÄNG FILER                                                 *        
081600*    SKRIV POSTSUMS RÄKNEVERK                                    *        
081700*                                                                *        
081800******************************************************************        
081900                                                                          
082000 P-SKRIV-MAIL-RAD SECTION.                                                
082100     MOVE 'P-SKRIV-MAIL-RAD      ' TO CURRENT-SECTION                     
082200     SKIP2                                                                
082300*----------RUBRIKEN SKRIVEN I AC- SECTION                                 
082400     Move JA To MAIL2-POST-SKRIVEN                                        
082500                                                                          
082600     WRITE MAIL2-POST FROM MAIL-EXCEL-RAD                                 
082700                                                                          
082800     MOVE 'W22153'   TO POSTSUM-FDNAMN                                    
082900     MOVE 'W22146D2' TO POSTSUM-DDNAMN2                                   
083000     MOVE 'Mai2'     TO POSTSUM-TRANSTYP                                  
083100     CALL POSTSUM USING POSTSUM-PARM                                      
083200     .                                                                    
083300     EJECT                                                                
083400 Z-AVSLUTNING SECTION.                                                    
083500     MOVE 'Z-AVSLUTNING          ' TO CURRENT-SECTION                     
083600                                                                          
083700     If MAIL2-POST-SKRIVEN = NEJ                                          
083800       WRITE MAIL2-POST FROM MAIL2-EXCEL-NODATA                           
083900                                                                          
084000       MOVE 'W22153'   TO POSTSUM-FDNAMN                                  
084100       MOVE 'W22146D2' TO POSTSUM-DDNAMN2                                 
084200       MOVE 'Mai2'     TO POSTSUM-TRANSTYP                                
084300       CALL POSTSUM USING POSTSUM-PARM                                    
084400     End-If                                                               
084500                                                                          
084600     CLOSE W22151                                                         
084700           MAIL2-FIL                                                      
084800                                                                          
084900     MOVE 'S' TO POSTSUM-OPKOD                                            
085000     CALL POSTSUM USING POSTSUM-PARM                                      
085100     .                                                                    
085200     EJECT                                                                
085300*    -COPY WY2000P9                                                       
085400     EJECT                                                                
085500*    -COPY WY2000P1                                                       
085600     EJECT                                                                
085700*    -COPY WY2000P3                                                       
085800     EJECT                                                                
085900*    -COPY WY2000Q3                                                       
086000     EJECT                                                                
086100*    -COPY WY2000P7                                                       
086200     EJECT                                                                
086300*    -COPY WY2000Q7                                                       
