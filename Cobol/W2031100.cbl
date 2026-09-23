000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2031100.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   90/10/01.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR ETT FRÅGE-/UPPDATERINGS-PROGRAM MOT                
001100*        ARTIKELREG.(WLARTC), KAMPANJREG.(WDM2) OCH                       
001300*        ARTIKELREG.(WLARTM).                                             
001400*        PROG. UPPGIFT ÄR ATT UTFÖRA NYREG., ÄNDRING OCH                  
001500*        BORTTAG AV KAMPMID-IDKAMPRF-INGÅENDE ARTIKLAR.                   
001600*                                                                         
001700*        I PROGRAMMET FINNS MÖJLIGHET ATT:                                
001800*        - SÖKA PÅ UPPFÖLJNINGSREFERENS.                                  
001900*        - ÄNDRA/UPPDATERA OCH NYREGISTRERA UPPFÖLJNINGSREFERENS          
002000*          OCH ARTIKLAR INGÅENDE I UPPFÖLJNINGSREFERENSEN.                
002100*        - BLÄDDRA GENOM ARTIKLAR.                                        
002200*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002300*        PROGRAMMET UPPDATERAR         WDM2                               
002600*        PROGRAMMET UPPDATERAR WLARTM (WDK9)                              
002700*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002800*                                                                         
002900*                                                                         
003000*    INDATA.                                                              
003100*        TRANSAKTION: W2T311                                              
003200*        MID:         W2I31101                                            
003300*                                                                         
003400*    UTDATA.                                                              
003500*        MOD:         W2O31101                                            
003510*                                                                         
003600*    CHANGE LOG                                                           
003601*                                                                         
003610*    ETRACK 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100 WORKING-STORAGE SECTION.                                                 
004200*    -COPY WY2000W1                                                       
004300     SKIP3                                                                
004410 77  IDPGM                       PIC X(08)   VALUE 'W2031100'.            
004500                                                                          
004600 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004700 77  CURRENT-IMS-SECTION         PIC X(80)   VALUE SPACE.                 
004800                                                                          
004900 77  JA                          PIC X      VALUE 'J'.                    
005000 77  NEJ                         PIC X      VALUE 'N'.                    
005100                                                                          
005200 77  INDX                        PIC S9(2)  VALUE +00   COMP-3.           
005300 77  MAX-INDX                    PIC S9(2)  VALUE +9    COMP-3.           
005500 77  MAX-MOD-LAENGD              PIC S9(3)  VALUE +0648 COMP-3.           
005600                                                                          
005700 77  WS-KAMPRF                   PIC X(7).                                
005800 77  WS-IDDC                     PIC X(2).                                
005900 77  WS-ARTNR                    PIC X(9).                                
005910 77  WS-MID-IDARTNR              PIC 9(8).                                
005920 77  WS-MID-CMD                  PIC X(1).                                
006000 77  WS-UPDATE-IDARTNR           PIC X(9).                                
006100 77  WS-IDARTNR-ZERO             PIC S9(9) COMP-3 VALUE ZERO.             
006200 77  WS-START-DATUM              PIC 9(6)    VALUE ZERO.                  
006300 77  WS-SLUT-DATUM               PIC 9(6)    VALUE ZERO.                  
006400 77  WS-KDLTK                    PIC 9       VALUE ZERO.                  
006500 77  WS-KART-KVBEART-KAMP        PIC 9(6)    VALUE ZERO.                  
006600 77  WS-MID-KVBEART-KAMP         PIC 9(6)    VALUE ZERO.                  
006700 77  WS-TOT-KVBEART-KAMP         PIC 9(8)    VALUE ZERO.                  
006800 77  ACC-KMRK-KVBEART-KAMP       PIC 9(6)    VALUE ZERO.                  
006900 77  WS-KART-TIRES               PIC 9(6)    VALUE ZERO.                  
007000 77  WS-MID-TIRES                PIC 9(6)    VALUE ZERO.                  
007100 77  SPAR-TIRES-HIGHNR           PIC 9(6)    VALUE ZERO.                  
007200 77  WS-KART-KVBEART-KUND        PIC 9(6)    VALUE ZERO.                  
007300 77  WS-KVRESS-KAMP              PIC 9(6)    VALUE ZERO.                  
007400 77  WS-DAGENS-DATUM             PIC 9(6)    VALUE ZERO.                  
007500 77  WS-OKNING                   PIC S9(7)   VALUE ZERO COMP-3.           
007600 77  WS-MINSKNING                PIC S9(7)   VALUE ZERO COMP-3.           
007700                                                                          
007800 01  WS-MID-TIAAVV               PIC 9(5).                                
007900 01  FILLER REDEFINES WS-MID-TIAAVV.                                      
008000     03 FILLER                   PIC X.                                   
008100     03 WS-YEAR-MID              PIC X(2).                                
008200     03 WS-WEEK-MID              PIC X(2).                                
008300 01  FILLER REDEFINES WS-MID-TIAAVV.                                      
008400     03 WS-TIAAVV-MID            PIC S9(5).                               
008500                                                                          
008600 01  WS-TIRES-TIAAAAVV           PIC 9(6).                                
008700 01  FILLER REDEFINES WS-TIRES-TIAAAAVV.                                  
008800     03 WS-SEKEL-TIRES           PIC 9(2).                                
008900     03 WS-YEAR-TIRES            PIC 9(2).                                
009000     03 WS-WEEK-TIRES            PIC 9(2).                                
009100 01  FILLER REDEFINES WS-TIRES-TIAAAAVV.                                  
009200     03 WS-TIAAAAVV-TIRES        PIC 9(6).                                
009300                                                                          
009400 01  WS-ARTNR-ED                 PIC Z(7)9.                               
009500 01  FILLER         REDEFINES  WS-ARTNR-ED.                               
009600     03  WS-ARTNR9                   PIC 9(8).                            
009700                                                                          
009800 01  WS-IDANSK-ED                PIC Z(2)9.                               
009900 01  FILLER         REDEFINES  WS-IDANSK-ED.                              
010000     03  WS-IDANSK9                  PIC 9(3).                            
010100                                                                          
010200 01  WS-IDLEVNR-ED               PIC X(5).                                
010300 01  FILLER         REDEFINES  WS-IDLEVNR-ED.                             
010400     03  WS-IDLEVNR9                 PIC X(5).                            
010500                                                                          
010600 01  WS-KVBEART-KAMP-ED          PIC Z(5)9.                               
010700 01  FILLER         REDEFINES  WS-KVBEART-KAMP-ED.                        
010800     03  WS-KVBEART-KAMP9            PIC 9(6).                            
010900                                                                          
011000 01  WS-TIRES-ED                 PIC Z(5)9.                               
011100 01  FILLER         REDEFINES  WS-TIRES-ED.                               
011200     03  WS-TIRES9                   PIC 9(6).                            
011300                                                                          
011400 01  WS-KVRESS-KAMP-ED           PIC Z(6)9.                               
011500 01  FILLER         REDEFINES  WS-KVRESS-KAMP-ED.                         
011600     03  WS-KVRESS-KAMP9             PIC 9(7).                            
011700                                                                          
011800 01  WS-KVBEART-KUND-ED          PIC Z(5)9.                               
011900 01  FILLER         REDEFINES  WS-KVBEART-KUND-ED.                        
012000     03  WS-KVBEART-KUND9            PIC 9(6).                            
012100                                                                          
012200 77  WDM211-UPDATE               PIC X.                                   
012300     88  WDM211-AENDRING                     VALUE '1'.                   
012400     88  WDM211-NYUPPLAEGG                   VALUE '2'.                   
012500     88  WDM211-NO-UPDATE                    VALUE '3'.                   
012600                                                                          
013000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013100     88  INDATA-OK                           VALUE 'J'.                   
013200     88  INDATA-FEL                          VALUE 'N'.                   
013300                                                                          
013310 77  INDATA-DELETE-SW            PIC X       VALUE 'J'.                   
013320     88  INDATA-DELETE-OK                    VALUE 'J'.                   
013330     88  INDATA-DELETE-FEL                   VALUE 'N'.                   
013340                                                                          
013400 77  INDATA-EXISTS-SW            PIC X       VALUE 'J'.                   
013500     88  INDATA-EXISTS                       VALUE 'J'.                   
013600                                                                          
013700 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
013800     88  UPDATE-OK                           VALUE 'J'.                   
013900                                                                          
014000 77  ARTNR-SW                    PIC X       VALUE 'J'.                   
014100     88  ARTNR-ANGIVET                       VALUE 'J'.                   
014200                                                                          
014300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
014400     88  NYCKLAR-OK                          VALUE 'J'.                   
014500     88  NYCKLAR-FEL                         VALUE 'N'.                   
014600                                                                          
014700 77  NYA-NYCKLAR-SW              PIC X       VALUE 'N'.                   
014800     88  NYA-NYCKLAR                         VALUE 'J'.                   
014900                                                                          
015000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
015100     88  ALLT-OK                             VALUE 'J'.                   
015200                                                                          
015300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015400     88  EGEN-MID                            VALUE '2311'.                
015410     88  2317-MID                            VALUE '2317'.                
015500     88  GODK-MID                            VALUE '2311' '2312'.         
015700 77  WDM201-SW                   PIC X       VALUE 'J'.                   
015800     88  WDM201-UPDATE                       VALUE 'J'.                   
015900                                                                          
016000 77  WDM211-SW                   PIC X       VALUE 'J'.                   
016100     88  WDM211-FINNS                        VALUE 'J'.                   
016200                                                                          
016300     EJECT                                                                
016400*      --- VALID IDDC CODES                                               
016500*                                                                         
016600*01    -COPY WWDCKONS                                                     
016610       EJECT                                                              
016700*01    -COPY WWDC99                                                       
016800       EJECT                                                              
016900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017000 01  GENERELLA-SUBPROGRAM.                                                
017100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
017110     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017300     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
017400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017600     EJECT                                                                
017700*   -COPY WMEDAREA                                                        
017800     EJECT                                                                
017900*   -COPY WDATAREA                                                        
018000     EJECT                                                                
018100*   -COPY W411ARTM                                                        
018200     EJECT                                                                
018300 01  MESSAGE-CODES.                                                       
018400     03  ERR-HIGHLITED-FIELDS-WRONG   PIC X(3)    VALUE '001'.            
018500     03  ERR-KEYS-ARE-MISSING         PIC X(3)    VALUE '005'.            
018600     03  ERR-UPDATE-FORBIDDEN         PIC X(3)    VALUE '007'.            
018700     03  ERR-PF11-AND-NO-DATA         PIC X(3)    VALUE '011'.            
018800     03  ERR-WRONG-KEY                PIC X(3)    VALUE '401'.            
018810     03  ERR-DELETE-NOT-ALLOWED       PIC X(3)    VALUE '365'.            
018900     03  INF-PRESS-PF11               PIC X(3)    VALUE '003'.            
019000     03  INF-FIRST-PAGE               PIC X(3)    VALUE '006'.            
019100     03  INF-UPDATE-DONE              PIC X(3)    VALUE '101'.            
019200     03  INF-LAST-PAGE                PIC X(3)    VALUE '106'.            
019300     03  INF-MORE-INFO-EXISTS         PIC X(3)    VALUE '105'.            
019400     EJECT                                                                
019501*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
019502*                                                                         
019503 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019504     SKIP3                                                                
019505*01 -COPY WMSGINIT                                                        
019510*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
019520*                                                                         
019530 01  SAVE-AREA.                                                           
019531    10 SAVE-IDTRANS                   PIC X(04)  VALUE '2311'.            
019533    10 SAVE-IDKAMPRF-KEY              PIC X(07)  VALUE SPACE.             
019534    10 SAVE-IDDC-KEY                  PIC X(02)  VALUE SPACE.             
019535    10 SAVE-IDARTNR-KEY               PIC X(09)  VALUE SPACE.             
019550    10 SAVE-IDARTNR-ENTER             PIC X(09)  VALUE SPACE.             
019580    10 SAVE-IDARTNR-NEXT              PIC X(09)  VALUE SPACE.             
019592     EJECT                                                                
019600                                                                          
019700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019800*                                                                         
019900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020000     SKIP3                                                                
020100*01  MID -COPY W2I31101                                                   
020200     EJECT                                                                
020300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020400     SKIP3                                                                
020500*01  -COPY WMSGAREA                                                       
020600     EJECT                                                                
020700     03  MOD REDEFINES MSG-AREA.                                          
020800*      05  -COPY W2O31101C0                                               
020900     EJECT                                                                
021000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021100     SKIP3                                                                
021200*01  -COPY WMFSAREA                                                       
021300     EJECT                                                                
021400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021500*                                                                         
021600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021700     SKIP3                                                                
021800 01  NYCKLAR-TILL-DLI.                                                    
021810     03  W-IDDC-B6-X.                                                     
021820         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
021830                                                                          
021900     03  W-WDM201-X.                                                      
022000         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
022100         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
022200                                                                          
022300     03  W-WDM211-IDARTNR-X.                                              
022400         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
022500                                                                          
022600     03  W-WDM221-MIN-X.                                                  
022800         05  W-KMRK-IDDISTRMIN-F  PIC S9(05)  VALUE ZERO COMP-3.          
022900         05  W-KMRK-IDDISTRMIN-T  PIC S9(05)  VALUE ZERO COMP-3.          
023000         05  W-KMRK-IDKUNDNRMIN-F PIC S9(07)  VALUE ZERO COMP-3.          
023100         05  W-KMRK-IDKUNDNRMIN-T PIC S9(07)  VALUE ZERO COMP-3.          
023200                                                                          
023300     03  W-WDM221-MAX-X.                                                  
023500         05  W-KMRK-IDDISTRMAX-F  PIC S9(05)  VALUE ZERO COMP-3.          
023600         05  W-KMRK-IDDISTRMAX-T  PIC S9(05)  VALUE ZERO COMP-3.          
023700         05  W-KMRK-IDKUNDNRMAX-F PIC S9(07)  VALUE ZERO COMP-3.          
023800         05  W-KMRK-IDKUNDNRMAX-T PIC S9(07)  VALUE ZERO COMP-3.          
023900                                                                          
024000     03  W-WDK601-X.                                                      
024100         05  W-K601-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
024200                                                                          
024300     03  W-WDK611-X.                                                      
024400         05  W-K611-KDSEGKEY     PIC  X       VALUE '1'.                  
024500                                                                          
024600     03  W-WDK901-X.                                                      
024700         05  W-ARTM-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
024800                                                                          
024900     03  W-WDK911KY-X.                                                    
025000         05  W-ANT-DABEHOV       PIC  9(6)    VALUE ZERO.                 
025001                                                                          
025002     03  W-WDQ4BSEQ-MIN-X.                                                
025003         05  W-WDQ4B-IDARTNR-MIN PIC S9(9)    COMP-3.                     
025004         05  FILLER              PIC  X(19)   VALUE SPACE.                
025005*                                                                         
025006     03  W-WDQ4BSEQ-MAX-X.                                                
025007         05  W-WDQ4B-IDARTNR-MAX PIC S9(9)    COMP-3.                     
025008         05  FILLER              PIC  X(19)   VALUE SPACE.                
025009*                                                                         
025010     03  W-WDA5ASEQ-MIN-X.                                                
025011         05  W-WDA5A-IDARTNR-MIN PIC S9(9) VALUE ZERO COMP-3.             
025012         05  W-WDA5A-IDDC-MIN    PIC X(2)  VALUE SPACE.                   
025013         05  FILLER              PIC X(2)  VALUE SPACE.                   
025014                                                                          
025015     03  W-WDA5ASEQ-MAX-X.                                                
025016         05  W-WDA5A-IDARTNR-MAX PIC S9(9) VALUE ZERO COMP-3.             
025017         05  W-WDA5A-IDDC-MAX    PIC X(2)  VALUE SPACE.                   
025018         05  FILLER              PIC X(2)  VALUE SPACE.                   
025019                                                                          
025020     03  W-WDE4CSEQ-MIN-X.                                                
025021         05  W-WDE4C-IDARTNR-MIN PIC S9(9) VALUE ZERO COMP-3.             
025030                                                                          
025040     03  W-WDE4CSEQ-MAX-X.                                                
025050         05  W-WDE4C-IDARTNR-MAX PIC S9(9) VALUE ZERO COMP-3.             
025060                                                                          
025100*    --- STATUS-KOD FRÅN IMS                                              
025200 01  STATUS-WS                   PIC XX.                                  
025300     88  SEGMENT-FOUND                       VALUE '  '.                  
025400     88  SEGMENT-ALREADY-EXISTS              VALUE 'II'.                  
025500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
025600     88  SEGMENT-END                         VALUE 'GB'.                  
025700     SKIP2                                                                
025800 01  GODK-STATUSKODER.                                                    
025900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026000     SKIP3                                                                
026100 01  SSA1                        PIC X(128).                              
026200 01  SSA2                        PIC X(128).                              
026300 01  SSA3                        PIC X(128).                              
026400     EJECT                                                                
026500*    --- IMS FUNKTIONSKODER                                               
026600*01  -COPY W0003                                                          
026700     EJECT                                                                
026800*    ---  DLI INPUT-OUTPUT AREA                                           
026810 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB601'.           
026820 01  DLI-IO-WDB601.                                                       
026830*    03  -COPY WDB601                                                     
026840     EJECT                                                                
026900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM201'.         
027000 01  DLI-IO-WDM201.                                                       
027100*    03 -COPY WDM201                                                      
027200     EJECT                                                                
027300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
027400 01  DLI-IO-WDM211.                                                       
027500*    03 -COPY WDM211                                                      
027600     EJECT                                                                
027700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
027800 01  DLI-IO-WDM221.                                                       
027900*    03 -COPY WDM221                                                      
028000     EJECT                                                                
028100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-ARTC'.           
028200 01  DLI-IO-ARTC-01.                                                      
028300     03  IO-ARTC-01            PIC X(200)  VALUE SPACE.                   
028400     03  WLARTC01 REDEFINES IO-ARTC-01.                                   
028500*        05  -COPY WDK601                                                 
028600     EJECT                                                                
028700 01  DLI-IO-ARTC-11.                                                      
028800     03  IO-ARTC-11            PIC X(900)  VALUE SPACE.                   
028900     03  WLARTC11 REDEFINES IO-ARTC-11.                                   
029000*        05  -COPY WDK611                                                 
029100     EJECT                                                                
029200 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-ARTM'.           
029300 01  DLI-IO-ARTM.                                                         
029400     03  IO-ARTM               PIC X(110)  VALUE SPACE.                   
029500     03  WLARTM01 REDEFINES IO-ARTM.                                      
029600*        05  -COPY WDK901                                                 
029700     EJECT                                                                
029800     03  WLARTM11 REDEFINES IO-ARTM.                                      
029900*        05  -COPY WDK911                                                 
030000     EJECT                                                                
030001 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDA501'.             
030002 01  DLI-IO-WDA501.                                                       
030003*    03            -COPY WDA501                                           
030004     EJECT                                                                
030005 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDQ401'.             
030006 01  DLI-IO-WDQ401.                                                       
030007*    03            -COPY WDQ401                                           
030008     EJECT                                                                
030010 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE401'.             
030020 01  DLI-IO-WDE401.                                                       
030030*    03            -COPY WDE401                                           
030040     EJECT                                                                
030050 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE411'.             
030060 01  DLI-IO-WDE411.                                                       
030070*    03            -COPY WDE411 -PRE E411-                                
030080     EJECT                                                                
030100 LINKAGE SECTION.                                                         
030200                                                                          
030300*01  -COPY W0009      -PRE MSG-                                           
030400     EJECT                                                                
030410*01  -COPY W0008      -PRE WDP7-                                          
030420     05  FILLER                  PIC X.                                   
030500*01  -COPY W0008      -PRE WDB6-                                          
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030710*01  -COPY W0008      -PRE KAMP-                                          
030720     05  FILLER                  PIC X.                                   
030730     EJECT                                                                
030800*01  -COPY W0008      -PRE ARTC-                                          
030900     05  FILLER                  PIC X.                                   
031000     EJECT                                                                
031100*01  -COPY W0008      -PRE ARTM-                                          
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031310*01  -COPY W0008      -PRE WDQ4B-                                         
031320     05  FILLER                  PIC X.                                   
031321     EJECT                                                                
031330*01  -COPY W0008      -PRE WDA5A-                                         
031340     05  FILLER                  PIC X.                                   
031350     EJECT                                                                
031360*01  -COPY W0008      -PRE WDE4C-                                         
031370     05  FILLER                  PIC X.                                   
031380     EJECT                                                                
031400 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB KAMP-PCB             
031410                                   ARTC-PCB ARTM-PCB                      
031420                                   WDQ4B-PCB WDA5A-PCB WDE4C-PCB.         
031500     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB KAMP-PCB             
031510                                   ARTC-PCB ARTM-PCB                      
031511                                   WDQ4B-PCB WDA5A-PCB WDE4C-PCB.         
031600                                                                          
031700     PERFORM IMS-GET-MSG                                                  
031800     IF SEGMENT-FOUND                                                     
031900       PERFORM A-INIT                                                     
032000       PERFORM B-NYCKEL-KONTROLL                                          
032100       IF NYCKLAR-OK                                                      
032200         IF NYA-NYCKLAR                                                   
032300           CONTINUE                                                       
032400         ELSE                                                             
032500           IF MFS-UPDATE                                                  
032600             PERFORM C-KONTROLL-OCH-UPDATE                                
032700           ELSE                                                           
032800             PERFORM D-MFS-IDPFK-KONTROLL                                 
032900           END-IF                                                         
033000         END-IF                                                           
033100         IF ALLT-OK                                                       
033200           PERFORM E-LAES-VISA-INFO                                       
033300         END-IF                                                           
033400       END-IF                                                             
033500       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
033600       PERFORM IMS-INSERT-MSG                                             
033700     END-IF                                                               
033800                                                                          
033900     MOVE ZERO TO RETURN-CODE                                             
034000     GOBACK                                                               
034100     .                                                                    
034200     EJECT                                                                
034300 A-INIT SECTION.                                                          
034400     MOVE 'A-INIT                       ' TO CURRENT-SECTION              
034500                                                                          
034600     ACCEPT WS-DAGENS-DATUM FROM DATE                                     
034700                                                                          
034800     IF MSG-DUBBLA-TRANSKODER                                             
034900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I31101-CTX             
035000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
035100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035200     ELSE                                                                 
035300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I31101-CTX              
035400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
035500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
035600     END-IF                                                               
035700                                                                          
035800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
035900     MOVE MSG-IDPFK            TO MFS-IDPFK                               
036000     MOVE MFS-IDTRANS          TO W-IDTRANS                               
036100                                                                          
036200     MOVE LOW-VALUE            TO MSG-AREA                                
036600     MOVE 'W2O311N1'           TO MFS-IDMOD                               
036700     MOVE '2311'               TO MOD-IDTRANS                             
036800     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
036900                                                                          
037000     IF NOT EGEN-MID                                                      
037100       MOVE SPACE              TO MFS-KDTRTYP                             
037200       MOVE '7'                TO MFS-IDPFK                               
037300     END-IF                                                               
037400                                                                          
037700     .                                                                    
037800     EJECT                                                                
037900 B-NYCKEL-KONTROLL SECTION.                                               
038000     MOVE 'B-NYCKEL-KONTROLL            ' TO CURRENT-SECTION              
038100                                                                          
038200     MOVE JA                TO NYCKLAR-SW ALLT-SW WDM201-SW               
038300                               ARTNR-SW INDATA-SW                         
038400     MOVE NEJ               TO UPDATE-SW WDM211-SW                        
038800                                                                          
038802     MOVE ALL '+'            TO MSGI-WMSGINIT                             
038803     MOVE '001'              TO MSGI-KDCALL                               
038804     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
038805     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
038806     MOVE '2311'             TO MSGI-IDTRANS                              
038807     IF EGEN-MID OR 2317-MID                                              
038809        MOVE MID-IDKAMPRF-IN TO MSGI-IDKAMPRF                             
038811        MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                             
038812        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
038813     END-IF                                                               
038814                                                                          
038815     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
038817     IF MSGI-SPAR-AREA (1:4) = '2311' OR '2312'                           
038818        MOVE MSGI-SPAR-AREA  TO SAVE-AREA                                 
038826     END-IF                                                               
038827                                                                          
038828*    - LANGUAGE TO BE USED BY MEDKONV                                     
038829     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
038830                                                                          
038831     MOVE JA                 TO NYCKLAR-SW                                
038832                                                                          
038833* IDKAMPRF                                                                
038834     MOVE MFS-RENSA-FAELT     TO MOD-IDKAMPRF-IN                          
038835     IF MID-IDKAMPRF-IN NOT = ALL '+'                                     
038836       MOVE '7'               TO MFS-IDPFK                                
038837       MOVE SPACE             TO MFS-KDTRTYP                              
038838     END-IF                                                               
038840     INSPECT MSGI-IDKAMPRF REPLACING LEADING SPACE BY ZERO                
038841     IF MSGI-IDKAMPRF NUMERIC                                             
038842        MOVE MSGI-IDKAMPRF      TO WS-KAMPRF                              
038843                                   W-KAMP-IDKAMPRF                        
038845     ELSE                                                                 
038846       MOVE NEJ                 TO NYCKLAR-SW                             
038847     END-IF                                                               
040500                                                                          
040510* IDDC                                                                    
040511*    -- KONTROLL AV IDDC                                                  
040512     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
040513     IF MID-IDDC-IN NOT = ALL '+'                                         
040515        MOVE '7'             TO MFS-IDPFK                                 
040516        MOVE SPACE           TO MFS-KDTRTYP                               
040517     END-IF                                                               
040518     MOVE MSGI-IDDC-KEY      TO WS-IDDC                                   
040520                                W-KAMP-IDDC                               
041710                                W-IDDC-B6                                 
041730     PERFORM IMS-GU-WDB601                                                
041740     IF SEGMENT-FOUND                                                     
041750        IF DCS-CDC                                                        
041760           CONTINUE                                                       
041770        ELSE                                                              
041771           MOVE NEJ          TO NYCKLAR-SW                                
041790        END-IF                                                            
041791     ELSE                                                                 
041792        MOVE NEJ             TO NYCKLAR-SW                                
041794     END-IF                                                               
041800                                                                          
041810* IDARTNR                                                                 
041811     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
041812     IF MID-IDARTNR-IN NOT = ALL '+'                                      
041813        MOVE '7'             TO MFS-IDPFK                                 
041814        MOVE SPACE           TO MFS-KDTRTYP                               
041815     END-IF                                                               
041816                                                                          
041819     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
041820     IF MSGI-IDARTNR NUMERIC                                              
041821        MOVE MSGI-IDARTNR     TO WS-ARTNR                                 
041822                                 W-KART-IDARTNR                           
041823     ELSE                                                                 
041824        MOVE NEJ              TO NYCKLAR-SW                               
041826     END-IF                                                               
043400                                                                          
043500     IF GODK-MID OR NYCKLAR-OK                                            
043600       MOVE MSGI-IDKAMPRF    TO MOD-IDKAMPRF-UT                           
043700       INSPECT MOD-IDKAMPRF-UT REPLACING LEADING ZEROES BY SPACE          
044000                                                                          
044010       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
044020                                                                          
044100       MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                            
044200       INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZEROES BY SPACE          
044300     ELSE                                                                 
044400       MOVE MFS-RENSA-FAELT  TO MOD-IDKAMPRF-UT                           
044500                                MOD-IDDC-UT                               
044600                                MOD-IDARTNR-UT                            
044700     END-IF                                                               
044800                                                                          
044900     IF NYCKLAR-FEL                                                       
045000       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
045100       PERFORM S01-ERR-RUTINE                                             
045200       PERFORM MFS-RENSA-FAELT-IN                                         
045300       PERFORM MFS-RENSA-FAELT-UT                                         
045301       PERFORM MFS-STAENG-FAELT-IN                                        
045310                                                                          
045400     END-IF                                                               
045500     .                                                                    
045600     EJECT                                                                
045700 C-KONTROLL-OCH-UPDATE SECTION.                                           
045800     MOVE 'C-KONTROLL-OCH-UPDATE        ' TO CURRENT-SECTION              
045900                                                                          
046000     PERFORM CA-INFAELT-KONTROLL                                          
046100     IF INDATA-EXISTS                                                     
046200        IF MID-TISTADAT-IN  = ALL '+' AND                                 
046300           MID-TISTODAT-IN  = ALL '+' AND                                 
046400           MID-IDARTNR      = ALL '+' AND                                 
046500           MID-KVBEART-KAMP = ALL '+' AND                                 
046600           MID-TIRES        = ALL '+'                                     
046700           CONTINUE                                                       
046800        ELSE                                                              
046900           PERFORM CB-KONTROLL-OCH-UPDATE                                 
047000        END-IF                                                            
047010                                                                          
047100        IF MID-CMD-RAD1     = ALL '+' AND                                 
047110           MID-CMD-RAD (01) = ALL '+' AND                                 
047200           MID-CMD-RAD (02) = ALL '+' AND                                 
047300           MID-CMD-RAD (03) = ALL '+' AND                                 
047400           MID-CMD-RAD (04) = ALL '+' AND                                 
047500           MID-CMD-RAD (05) = ALL '+' AND                                 
047600           MID-CMD-RAD (06) = ALL '+' AND                                 
047700           MID-CMD-RAD (07) = ALL '+' AND                                 
047800           MID-CMD-RAD (08) = ALL '+' AND                                 
047900           MID-CMD-RAD (09) = ALL '+'                                     
048100           CONTINUE                                                       
048200        ELSE                                                              
048300          PERFORM CC-DELETE-KONTROLL                                      
048310          IF INDATA-OK                                                    
048320             PERFORM CD-DELETE-UPDATE                                     
048400          END-IF                                                          
048410        END-IF                                                            
048500                                                                          
048600        IF INDATA-FEL                                                     
048700          MOVE NEJ TO ALLT-SW                                             
048710          IF MED-IDMFSFEL = ERR-DELETE-NOT-ALLOWED                        
048720             CONTINUE                                                     
048730          ELSE                                                            
048800             MOVE ERR-HIGHLITED-FIELDS-WRONG TO MED-IDMFSFEL              
048810          END-IF                                                          
048900          PERFORM S01-ERR-RUTINE                                          
049000          PERFORM MFS-ROER-EJ-FAELT-IN                                    
049100          PERFORM MFS-ROER-EJ-FAELT-UT                                    
049200        ELSE                                                              
049300          MOVE JA        TO UPDATE-SW                                     
049310          PERFORM MFS-RENSA-FAELT-IN                                      
049400        END-IF                                                            
049500     ELSE                                                                 
049600        MOVE NEJ TO INDATA-SW ALLT-SW UPDATE-SW                           
049700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
049800        PERFORM S01-ERR-RUTINE                                            
049900        PERFORM MFS-ROER-EJ-FAELT-IN                                      
050000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
050100     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400 CA-INFAELT-KONTROLL SECTION.                                             
050500     MOVE 'CA-INFAELT-KONTROLL          ' TO CURRENT-SECTION              
050600                                                                          
050700     IF MID-TISTADAT-IN = ALL '+' AND                                     
050800       MID-TISTODAT-IN  = ALL '+' AND                                     
050900       MID-IDARTNR      = ALL '+' AND                                     
051000       MID-KVBEART-KAMP = ALL '+' AND                                     
051100       MID-TIRES        = ALL '+' AND                                     
051200       MID-CMD-RAD1     = ALL '+' AND                                     
051210       MID-CMD-RAD (01) = ALL '+' AND                                     
051300       MID-CMD-RAD (02) = ALL '+' AND                                     
051400       MID-CMD-RAD (03) = ALL '+' AND                                     
051500       MID-CMD-RAD (04) = ALL '+' AND                                     
051600       MID-CMD-RAD (05) = ALL '+' AND                                     
051700       MID-CMD-RAD (06) = ALL '+' AND                                     
051800       MID-CMD-RAD (07) = ALL '+' AND                                     
051900       MID-CMD-RAD (08) = ALL '+' AND                                     
052000       MID-CMD-RAD (09) = ALL '+'                                         
052200       MOVE NEJ        TO INDATA-EXISTS-SW                                
052300     ELSE                                                                 
052400       MOVE JA         TO INDATA-EXISTS-SW                                
052500       PERFORM S06-LAES-IN-IGEN                                           
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 CB-KONTROLL-OCH-UPDATE SECTION.                                          
053000     MOVE 'CB-KONTROLL-OCH-UPDATE       ' TO CURRENT-SECTION              
053100                                                                          
053200     PERFORM CBA-KONTROLL-KAMP                                            
053300     IF INDATA-OK                                                         
053400       PERFORM CBB-KONTROLL-KAMP                                          
053500     ELSE                                                                 
053600       MOVE NEJ          TO WDM201-SW                                     
053700       MOVE +3           TO WDM211-UPDATE                                 
053800     END-IF                                                               
053900                                                                          
054000     IF WDM201-UPDATE                                                     
054100       PERFORM CBC-UPDATE-KAMP                                            
054200     END-IF                                                               
054300     PERFORM CBD-EV-UPDATE-KAMP                                           
054400     .                                                                    
054500     EJECT                                                                
054600 CBA-KONTROLL-KAMP SECTION.                                               
054700     MOVE 'CBA-KONTROLL-KAMP            ' TO CURRENT-SECTION              
054911                                                                          
054920     IF MID-TISTADAT-IN = ALL '+' AND                                     
055000        MID-TISTODAT-IN = ALL '+'                                         
055100       CONTINUE                                                           
055200     ELSE                                                                 
055300       PERFORM CBAA-KONTROLL-MID-TISTADAT                                 
055400       PERFORM CBAB-KONTROLL-MID-TISTODAT                                 
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 CBAA-KONTROLL-MID-TISTADAT SECTION.                                      
055900     MOVE 'CBAA-KONTROLL-MID-TISTADAT   ' TO CURRENT-SECTION              
056000                                                                          
056100     MOVE ZERO                  TO WS-START-DATUM                         
056200     IF MID-TISTADAT-IN = ALL '+'                                         
056300         MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTADAT-IN-ATTR                 
056400     ELSE                                                                 
056500       IF MID-TISTADAT-IN IS NUMERIC                                      
056600         MOVE MID-TISTADAT-IN     TO WS-START-DATUM                       
056700         MOVE WS-START-DATUM      TO DAT-I-TIDATUM                        
056800         MOVE 'AAMMDD'            TO DAT-KDDATFORM                        
056900         CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                 
057000                             DAT-O-TIDATUM, DAT-KDSVAR                    
057100         IF DAT-KDSVAR-OK                                                 
057200           PERFORM CBAAA-KONTROL-TISTADAT-TIRES                           
057300         ELSE                                                             
057400           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-IN-ATTR                 
057500           MOVE NEJ TO INDATA-SW                                          
057600         END-IF                                                           
057700       ELSE                                                               
057800         MOVE MFS-NUM-FAELT-FEL   TO MOD-TISTADAT-IN-ATTR                 
057900         MOVE NEJ                 TO INDATA-SW                            
058000       END-IF                                                             
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400 CBAAA-KONTROL-TISTADAT-TIRES SECTION.                                    
058500     MOVE 'CBAAA-KONTROL-TISTADAT-TIRES ' TO CURRENT-SECTION              
058600                                                                          
058700     PERFORM IMS-GU-WDM201                                                
058800     IF SEGMENT-FOUND                                                     
058810       IF WS-START-DATUM = ZERO                                           
058900          MOVE KAMP-TISTADAT         TO WS-START-DATUM                    
058910       END-IF                                                             
059000       PERFORM IMS-GNP-WDM211                                             
059100       IF SEGMENT-FOUND                                                   
059200         MOVE KART-TIRES          TO SPAR-TIRES-HIGHNR                    
059300                                                                          
059400         PERFORM UNTIL SEGMENT-MISSING                                    
059500           MOVE KART-TIRES        TO TMP1-YYMMDD                          
059600           MOVE SPAR-TIRES-HIGHNR TO TMP2-YYMMDD                          
059700           PERFORM WY2000P1                                               
059800           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
059900             MOVE KART-TIRES      TO SPAR-TIRES-HIGHNR                    
060000           END-IF                                                         
060100           PERFORM IMS-GNP-WDM211                                         
060200         END-PERFORM                                                      
060300                                                                          
060400         MOVE WS-START-DATUM      TO TMP1-YYMMDD                          
060500         MOVE SPAR-TIRES-HIGHNR   TO TMP2-YYMMDD                          
060600         MOVE WS-DAGENS-DATUM     TO TMP3-YYMMDD                          
060700         PERFORM WY2000Q1                                                 
060800         IF TMP1-YYMMDD > TMP2-YYMMDD  AND                                
060900            TMP1-YYMMDD >= TMP3-YYMMDD                                    
061000           MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTADAT-IN-ATTR               
061100         ELSE                                                             
061200           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-IN-ATTR                 
061300           MOVE NEJ TO INDATA-SW                                          
061400         END-IF                                                           
061500       END-IF                                                             
061600     END-IF                                                               
061700     .                                                                    
061800     EJECT                                                                
061900 CBAB-KONTROLL-MID-TISTODAT SECTION.                                      
062000     MOVE 'CBAB-KONTROLL-MID-TISTODAT   ' TO CURRENT-SECTION              
062100                                                                          
062200     IF MID-TISTODAT-IN = ALL '+'                                         
062300         MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTODAT-IN-ATTR                 
062400     ELSE                                                                 
062500       IF MID-TISTODAT-IN IS NUMERIC                                      
062600         IF MID-TISTADAT-IN    IS NUMERIC                                 
062700             MOVE MID-TISTADAT-IN TO WS-START-DATUM                       
063500         END-IF                                                           
063600         MOVE MID-TISTODAT-IN    TO WS-SLUT-DATUM                         
063700         MOVE WS-SLUT-DATUM      TO DAT-I-TIDATUM                         
063800         MOVE 'AAMMDD'           TO DAT-KDDATFORM                         
063900         CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                 
064000                             DAT-O-TIDATUM, DAT-KDSVAR                    
064100         MOVE MID-TISTODAT-IN   TO TMP1-YYMMDD                            
064200         MOVE WS-START-DATUM    TO TMP2-YYMMDD                            
064300         PERFORM WY2000P1                                                 
064400         IF DAT-KDSVAR-OK AND TMP1-YYMMDD >= TMP2-YYMMDD                  
064500           MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTODAT-IN-ATTR               
064600         ELSE                                                             
064700           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTODAT-IN-ATTR                 
064800           MOVE NEJ TO INDATA-SW                                          
064900         END-IF                                                           
065000       ELSE                                                               
065100         MOVE MFS-NUM-FAELT-FEL TO MOD-TISTODAT-IN-ATTR                   
065200         MOVE NEJ TO INDATA-SW                                            
065300       END-IF                                                             
065400     END-IF                                                               
065500     .                                                                    
065600     EJECT                                                                
065700 CBB-KONTROLL-KAMP SECTION.                                               
065710                                                                          
065800     MOVE 'CBB-KONTROLL-KAMP            ' TO CURRENT-SECTION              
065900                                                                          
066000     PERFORM CBBA-GENERELL-KONTROLL-KAMP                                  
066100     IF INDATA-OK AND NOT WDM211-FINNS                                    
066200       PERFORM CBBB-NYREG-KONTROLL-KAMP                                   
066300      ELSE                                                                
066400       PERFORM CBBC-AENDRA-KONTROLL-KAMP                                  
066500     END-IF                                                               
066600                                                                          
066700     IF INDATA-FEL                                                        
066800       MOVE +3  TO WDM211-UPDATE                                          
066900       MOVE NEJ TO UPDATE-SW WDM201-SW                                    
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300 CBBA-GENERELL-KONTROLL-KAMP SECTION.                                     
067400     MOVE 'CBBA-GENERELL-KONTROLL-KAMP  ' TO CURRENT-SECTION              
067500                                                                          
067700     PERFORM CBBAA-KONTROLL-MID-IDARTNR                                   
067800                                                                          
067900     PERFORM CBBAB-KONTR-MID-KVBEART-KAMP                                 
068000                                                                          
068100     PERFORM CBBAC-KONTROLL-MID-TIRES                                     
068200                                                                          
068300     IF MID-IDARTNR      = ALL '+' AND                                    
068400        MID-KVBEART-KAMP = ALL '+' AND                                    
068500        MID-TIRES        = ALL '+'                                        
068600        MOVE JA                   TO WDM211-SW                            
068700     END-IF                                                               
068800     .                                                                    
068900     EJECT                                                                
069000 CBBAA-KONTROLL-MID-IDARTNR SECTION.                                      
069100     MOVE 'CBBAA-KONTROLL-MID-IDARTNR   ' TO CURRENT-SECTION              
069200                                                                          
069300     IF MID-IDARTNR    = ALL '+'                                          
069400       MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDARTNR-ATTR                 
069500       MOVE NEJ                       TO ARTNR-SW                         
069600     ELSE                                                                 
069700       IF MID-IDARTNR  NUMERIC                                            
069800         MOVE MID-IDARTNR             TO W-K601-IDARTNR                   
069900                                         W-KART-IDARTNR                   
070200                                         W-ARTM-IDARTNR                   
070300                                         WS-UPDATE-IDARTNR                
070400         PERFORM IMS-GU-WDM211                                            
070500         IF SEGMENT-FOUND                                                 
070600           MOVE  1                    TO WDM211-UPDATE                    
070700           MOVE  JA                   TO WDM211-SW                        
070800           MOVE KART-KVBEART-KAMP     TO WS-KART-KVBEART-KAMP             
070900           MOVE KART-KVBEART-KUND     TO WS-KART-KVBEART-KUND             
071000           MOVE KART-TIRES            TO WS-KART-TIRES                    
071300         ELSE                                                             
071400           MOVE 2                     TO  WDM211-UPDATE                   
071500         END-IF                                                           
071600         PERFORM IMS-GU-ARTC11                                            
071700         IF SEGMENT-FOUND                                                 
071800           MOVE CLAG-KDLTK            TO WS-KDLTK                         
071900           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDARTNR-ATTR                 
072000         ELSE                                                             
072100           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDARTNR-ATTR                 
072200           MOVE NEJ TO INDATA-SW                                          
072300         END-IF                                                           
072400       ELSE                                                               
072500         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDARTNR-ATTR                 
072600         MOVE NEJ                     TO INDATA-SW                        
072700       END-IF                                                             
072800     END-IF                                                               
072900     .                                                                    
073000     EJECT                                                                
073100 CBBAB-KONTR-MID-KVBEART-KAMP SECTION.                                    
073200     MOVE 'CBBAB-KONTR-MID-KVBEART-KAMP ' TO CURRENT-SECTION              
073300                                                                          
073400     IF MID-KVBEART-KAMP = ALL '+'                                        
073500       MOVE MFS-NUM-FAELT-RAETT       TO MOD-KVBEART-KAMP-ATTR            
073600     ELSE                                                                 
073700       IF MID-KVBEART-KAMP IS NUMERIC                                     
073800         IF MID-IDARTNR      = ALL '+' AND                                
073900            MID-TIRES        = ALL '+'                                    
074000           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVBEART-KAMP-ATTR            
074100           MOVE NEJ TO INDATA-SW                                          
074200         ELSE                                                             
074300           IF MID-KVBEART-KAMP  >=  WS-KART-KVBEART-KUND                  
074400                                                                          
075000             PERFORM IMS-GU-WDM211                                        
075100             IF SEGMENT-FOUND                                             
075101                MOVE LOW-VALUE           TO W-WDM221-MIN-X                
075102                MOVE HIGH-VALUE          TO W-WDM221-MAX-X                
075200                PERFORM IMS-GNP-MIN-MAX-WDM221                            
075300                PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END              
075400                  ADD KMRK-KVBEART-KAMP TO ACC-KMRK-KVBEART-KAMP          
075500                  PERFORM IMS-GNP-MIN-MAX-WDM221                          
075600                END-PERFORM                                               
075700             END-IF                                                       
075800                                                                          
075900             IF SEGMENT-FOUND                                             
076000                 COMPUTE WS-TOT-KVBEART-KAMP =                            
076100                         MID-KVBEART-KAMP + KART-KVBEART-KAMP             
076200              ELSE                                                        
076300                 COMPUTE WS-TOT-KVBEART-KAMP =  MID-KVBEART-KAMP          
076400             END-IF                                                       
076500                                                                          
076600             IF WS-TOT-KVBEART-KAMP >= ACC-KMRK-KVBEART-KAMP              
076700                 MOVE MID-KVBEART-KAMP    TO WS-MID-KVBEART-KAMP          
076800                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KVBEART-KAMP-ATTR        
076900               ELSE                                                       
077000                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVBEART-KAMP-ATTR          
077100                 MOVE NEJ TO INDATA-SW                                    
077200             END-IF                                                       
077300           ELSE                                                           
077400             MOVE MFS-NUM-FAELT-FEL TO MOD-KVBEART-KAMP-ATTR              
077500             MOVE NEJ TO INDATA-SW                                        
077600           END-IF                                                         
077700         END-IF                                                           
077800       ELSE                                                               
077900         MOVE MFS-NUM-FAELT-FEL TO MOD-KVBEART-KAMP-ATTR                  
078000         MOVE NEJ TO INDATA-SW                                            
078100       END-IF                                                             
078200     END-IF                                                               
078300     .                                                                    
078400     EJECT                                                                
078500 CBBAC-KONTROLL-MID-TIRES SECTION.                                        
078600     MOVE 'CBBAC-KONTROLL-MID-TIRES     ' TO CURRENT-SECTION              
078700                                                                          
078800     IF MID-TIRES = ALL '+'                                               
078900       MOVE MFS-NUM-FAELT-RAETT    TO MOD-TIRES-ATTR                      
079000       MOVE WS-KART-TIRES          TO DAT-I-TIDATUM                       
079100       MOVE 'AAMMDD'               TO DAT-KDDATFORM                       
079200       CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                   
079300                           DAT-O-TIDATUM, DAT-KDSVAR                      
079400       MOVE DAT-TIAA-VECKA         TO WS-YEAR-MID                         
079500       MOVE DAT-TIVV               TO WS-WEEK-MID                         
079600     ELSE                                                                 
079700       IF MID-TIRES IS NUMERIC                                            
079800         MOVE MID-TIRES            TO DAT-I-TIDATUM                       
079900         MOVE 'AAMMDD'             TO DAT-KDDATFORM                       
080000         CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                 
080100                             DAT-O-TIDATUM, DAT-KDSVAR                    
080200         IF DAT-KDSVAR-OK                                                 
080400           PERFORM IMS-GU-WDM201                                          
080500           IF SEGMENT-FOUND                                               
080600             MOVE KAMP-TISTADAT    TO WS-START-DATUM                      
080700           ELSE                                                           
080800             IF MID-TISTADAT-IN = ALL '+'                                 
080900               MOVE ZERO           TO WS-START-DATUM                      
081000             END-IF                                                       
081100           END-IF                                                         
081200           MOVE MID-TIRES        TO TMP1-YYMMDD                           
081300           MOVE WS-START-DATUM   TO TMP2-YYMMDD                           
081400           PERFORM WY2000P1                                               
081500           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
081600           OR WS-START-DATUM = ZERO                                       
081700             MOVE MFS-NUM-FAELT-RAETT TO MOD-TIRES-ATTR                   
081800             MOVE MID-TIRES        TO WS-MID-TIRES                        
081900             MOVE DAT-TIAA-VECKA   TO WS-YEAR-MID                         
082000             MOVE DAT-TIVV         TO WS-WEEK-MID                         
082100           ELSE                                                           
082200             MOVE MFS-NUM-FAELT-FEL TO MOD-TIRES-ATTR                     
082300             MOVE NEJ TO INDATA-SW                                        
082400           END-IF                                                         
082500         ELSE                                                             
082600           MOVE MFS-NUM-FAELT-FEL TO MOD-TIRES-ATTR                       
082700           MOVE NEJ TO INDATA-SW                                          
082800         END-IF                                                           
082900       ELSE                                                               
083000         MOVE MFS-NUM-FAELT-FEL TO MOD-TIRES-ATTR                         
083100         MOVE NEJ TO INDATA-SW                                            
083200       END-IF                                                             
083300     END-IF                                                               
083400     .                                                                    
083500     EJECT                                                                
083600 CBBB-NYREG-KONTROLL-KAMP SECTION.                                        
083700     MOVE 'CBBB-NYREG-KONTROLL-KAMP     ' TO CURRENT-SECTION              
083800                                                                          
083900     IF MID-IDARTNR = ALL '+'                                             
084000       MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR                       
084100       MOVE NEJ                 TO INDATA-SW                              
084200     ELSE                                                                 
084300       IF MID-KVBEART-KAMP = ALL '+' OR                                   
084400          MID-TIRES = ALL '+'                                             
084500         MOVE MFS-NUM-FAELT-FEL TO MOD-KVBEART-KAMP-ATTR                  
084600                                   MOD-TIRES-ATTR                         
084700         MOVE NEJ               TO INDATA-SW                              
084800       END-IF                                                             
084900     END-IF                                                               
085000                                                                          
085100     IF MID-KVBEART-KAMP = ALL '+'                                        
085200       MOVE MFS-NUM-FAELT-FEL   TO MOD-KVBEART-KAMP-ATTR                  
085300       MOVE NEJ                 TO INDATA-SW                              
085400     ELSE                                                                 
085500       IF ARTNR-ANGIVET                                                   
085600         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVBEART-KAMP-ATTR                
085700       ELSE                                                               
085800         MOVE MFS-NUM-FAELT-FEL TO MOD-KVBEART-KAMP-ATTR                  
085900         MOVE NEJ               TO INDATA-SW                              
086000       END-IF                                                             
086100     END-IF                                                               
086200                                                                          
086300     IF MID-TIRES = ALL '+'                                               
086400       MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRES-ATTR                         
086500       MOVE NEJ                 TO INDATA-SW                              
086600     ELSE                                                                 
086700       IF ARTNR-ANGIVET                                                   
086800         MOVE MFS-NUM-FAELT-RAETT TO MOD-TIRES-ATTR                       
086900       ELSE                                                               
087000         MOVE MFS-NUM-FAELT-FEL TO MOD-TIRES-ATTR                         
087100         MOVE NEJ TO INDATA-SW                                            
087200       END-IF                                                             
087300     END-IF                                                               
087400     .                                                                    
087500     EJECT                                                                
087600                                                                          
087700 CBBC-AENDRA-KONTROLL-KAMP SECTION.                                       
087800     MOVE 'CBBC-AENDRA-KONTROLL-KAMP    ' TO CURRENT-SECTION              
087900                                                                          
088000     IF MID-IDARTNR             = ALL '+'                                 
088100         CONTINUE                                                         
088200      ELSE                                                                
088300         IF MID-KVBEART-KAMP    = ALL '+' AND                             
088400            MID-TIRES           = ALL '+'                                 
088500             MOVE MFS-NUM-FAELT-FEL TO MOD-KVBEART-KAMP-ATTR              
088600                                       MOD-TIRES-ATTR                     
088700             MOVE NEJ           TO INDATA-SW                              
088800         END-IF                                                           
088900     END-IF                                                               
089000     .                                                                    
089100     EJECT                                                                
089200 CBC-UPDATE-KAMP SECTION.                                                 
089300     MOVE 'CBC-UPDATE-KAMP              ' TO CURRENT-SECTION              
089400                                                                          
089500     PERFORM CBCA-MOVE-TO-IO-AREA-MOD                                     
089600     PERFORM IMS-ISRT-WDM201                                              
089700                                                                          
089800     IF SEGMENT-ALREADY-EXISTS                                            
089900       PERFORM IMS-GHU-WDM201                                             
090000       PERFORM CBCB-MOVE-TO-IO-AREA-MOD                                   
090100       PERFORM IMS-REPL-WDM201                                            
090200     END-IF                                                               
090300     .                                                                    
090400     EJECT                                                                
090500 CBCA-MOVE-TO-IO-AREA-MOD SECTION.                                        
090600     MOVE 'CBCA-MOVE-TO-IO-AREA-MOD     ' TO CURRENT-SECTION              
090700                                                                          
090800     MOVE WS-KAMPRF         TO KAMP-IDKAMPRF                              
090900     MOVE WS-IDDC           TO KAMP-IDDC                                  
091100     MOVE WS-START-DATUM    TO KAMP-TISTADAT                              
091200     MOVE WS-SLUT-DATUM     TO KAMP-TISTODAT                              
091300     MOVE WS-DAGENS-DATUM   TO KAMP-TIREGDAT                              
091500     .                                                                    
091600     EJECT                                                                
091700 CBCB-MOVE-TO-IO-AREA-MOD SECTION.                                        
091800     MOVE 'CBCB-MOVE-TO-IO-AREA-MOD     ' TO CURRENT-SECTION              
091900                                                                          
092000     IF MID-IDKAMPRF-IN NOT = ALL '+'                                     
092100       MOVE WS-KAMPRF       TO KAMP-IDKAMPRF                              
092200     END-IF                                                               
092300     IF MID-IDDC-IN NOT = ALL '+'                                         
092400       MOVE WS-IDDC         TO KAMP-IDKAMPRF                              
092500     END-IF                                                               
092700     IF MID-TISTADAT-IN NOT = ALL '+'                                     
092800       MOVE WS-START-DATUM  TO KAMP-TISTADAT                              
092900     END-IF                                                               
093000     IF MID-TISTODAT-IN NOT = ALL '+'                                     
093100       MOVE WS-SLUT-DATUM   TO KAMP-TISTODAT                              
093200     END-IF                                                               
093400     .                                                                    
093500     EJECT                                                                
093600 CBD-EV-UPDATE-KAMP SECTION.                                              
093700     MOVE 'CBD-EV-UPDATE-KAMP           ' TO CURRENT-SECTION              
093800                                                                          
093900     EVALUATE WDM211-UPDATE                                               
094000       WHEN 1                                                             
094100*        REPLACE-ARTIKELN FINNS PÅ WDM211                                 
094200         PERFORM IMS-GHU-WDM211                                           
094300         IF KART-KVRESS-KAMP > ZERO                                       
094400             PERFORM CBDA-UPDATE-WDK6                                     
094500          ELSE                                                            
094600             PERFORM CBDB-UPDATE-WDK9                                     
094700         END-IF                                                           
094800         PERFORM CBDC-MOVE-TO-IO-AREA                                     
094900         PERFORM IMS-REPL-WDM211                                          
095000       WHEN 2                                                             
095400         PERFORM CBDE-MOVE-TO-IO-AREA                                     
095500         PERFORM IMS-ISRT-WDM211                                          
095600         PERFORM CBDF-NYREG-WDK9                                          
095700       WHEN 3                                                             
095800         CONTINUE                                                         
095900     END-EVALUATE                                                         
096000     .                                                                    
096100     EJECT                                                                
096200 CBDA-UPDATE-WDK6  SECTION.                                               
096300     MOVE 'CBDA-UPDATE-WDK6             ' TO CURRENT-SECTION              
096400*                                                                         
096500     IF MID-KVBEART-KAMP = ALL '+'                                        
096600       CONTINUE                                                           
096700     ELSE                                                                 
096800       IF WS-MID-KVBEART-KAMP   >  KART-KVBEART-KAMP                      
096900           PERFORM CBDAA-OKA-KVBEART-KAMP                                 
097000        ELSE                                                              
097100           PERFORM CBDAB-MINSKA-KVBEART-KAMP                              
097200       END-IF                                                             
097300       MOVE WS-MID-KVBEART-KAMP TO KART-KVBEART-KAMP                      
097400     END-IF                                                               
097500     .                                                                    
097600     EJECT                                                                
097700 CBDAA-OKA-KVBEART-KAMP SECTION.                                          
097800     MOVE 'CBDAA-OKA-KVBEART-KAMP       ' TO CURRENT-SECTION              
097900                                                                          
098000     PERFORM IMS-GHU-ARTC11                                               
098100     COMPUTE WS-OKNING         = WS-MID-KVBEART-KAMP -                    
098200                                 KART-KVBEART-KAMP                        
098300     COMPUTE CLAG-KVRESS       = CLAG-KVRESS     + WS-OKNING              
098400     COMPUTE KART-KVRESS-ART = KART-KVRESS-ART +         WS-OKNING        
098500     PERFORM IMS-REPL-ARTC11                                              
098600     .                                                                    
098700     EJECT                                                                
098800 CBDAB-MINSKA-KVBEART-KAMP SECTION.                                       
098900     MOVE 'CBDAB-MINSKA-KVBEART-KAMP    ' TO CURRENT-SECTION              
099000                                                                          
099100     PERFORM IMS-GHU-ARTC11                                               
099200     COMPUTE WS-MINSKNING      = KART-KVBEART-KAMP -                      
099300                                 WS-MID-KVBEART-KAMP                      
099400     COMPUTE CLAG-KVRESS       = CLAG-KVRESS      - WS-MINSKNING          
099500     COMPUTE KART-KVRESS-ART =                                            
099600             KART-KVRESS-ART - WS-MINSKNING                               
099700                                                                          
099800     IF WS-MID-KVBEART-KAMP    < KART-KVRESS-KAMP                         
099900         MOVE WS-MID-KVBEART-KAMP  TO KART-KVRESS-KAMP                    
100000     END-IF                                                               
100100                                                                          
100200     PERFORM IMS-REPL-ARTC11                                              
100300     .                                                                    
100400     EJECT                                                                
100500 CBDB-UPDATE-WDK9 SECTION.                                                
100600     MOVE 'CBDB-UPDATE-WDK9             ' TO CURRENT-SECTION              
100700                                                                          
100800     IF WS-MID-TIRES = WS-KART-TIRES OR                                   
100900        WS-MID-TIRES = ZERO                                               
101000       IF WS-MID-KVBEART-KAMP NOT = WS-KART-KVBEART-KAMP                  
101100         PERFORM CBDBA-UPDATE-ARTM01-11                                   
101200       END-IF                                                             
101300     ELSE                                                                 
101400       IF WS-MID-KVBEART-KAMP = WS-KART-KVBEART-KAMP OR                   
101500          WS-MID-KVBEART-KAMP = ZERO                                      
101600         PERFORM CBDBB-UPDATE-ARTM11                                      
101700       ELSE                                                               
101800         PERFORM CBDBC-UPDATE-ARTM01-11                                   
101900       END-IF                                                             
102000     END-IF                                                               
102100     .                                                                    
102200     EJECT                                                                
102300 CBDBA-UPDATE-ARTM01-11 SECTION.                                          
102400     MOVE 'CBDBA-UPDATE-ARTM01-11       ' TO CURRENT-SECTION              
102500                                                                          
102600     PERFORM S04-LAS-ARTM01                                               
102700                                                                          
102800     COMPUTE ART-SUTPO-TOT          =                                     
102900             ART-SUTPO-TOT          - WS-KART-KVBEART-KAMP +              
103000             WS-MID-KVBEART-KAMP                                          
103100                                                                          
103200     PERFORM IMS-REPL-ARTM                                                
103300                                                                          
103400     PERFORM S05-WDATKONV                                                 
103500                                                                          
103600     MOVE WS-TIAAAAVV-TIRES       TO W-ANT-DABEHOV                        
103700     PERFORM IMS-GHU-ARTM11                                               
103800                                                                          
103900     IF NOT SEGMENT-FOUND                                                 
104000       MOVE WS-TIAAAAVV-TIRES     TO ANT-DABEHOV                          
104100       MOVE ZERO                  TO ANT-SUTPO-PB                         
104200       MOVE ZERO                  TO ANT-SUTPO-EJPB                       
104300                                                                          
104400       COMPUTE ANT-SUTPO-EJPB = ANT-SUTPO-EJPB -                          
104500               WS-KART-KVBEART-KAMP + WS-MID-KVBEART-KAMP                 
104600                                                                          
104700       PERFORM IMS-ISRT-ARTM11                                            
104800     ELSE                                                                 
104900                                                                          
105000       COMPUTE ANT-SUTPO-EJPB = ANT-SUTPO-EJPB -                          
105100               WS-KART-KVBEART-KAMP + WS-MID-KVBEART-KAMP                 
105200       IF ANT-SUTPO-EJPB = 0 AND ANT-SUTPO-PB = 0                         
105300         PERFORM IMS-DLET-ARTM                                            
105400       ELSE                                                               
105500         PERFORM IMS-REPL-ARTM                                            
105600       END-IF                                                             
105700     END-IF                                                               
105800     .                                                                    
105900     EJECT                                                                
106000 CBDBB-UPDATE-ARTM11 SECTION.                                             
106100     MOVE 'CBDBB-UPDATE-ARTM11          ' TO CURRENT-SECTION              
106200                                                                          
106300     PERFORM S05-WDATKONV                                                 
106400                                                                          
106500     MOVE WS-TIAAAAVV-TIRES     TO W-ANT-DABEHOV                          
106600     PERFORM S04-LAS-ARTM01                                               
106700                                                                          
106800     PERFORM IMS-GHU-ARTM11                                               
106900     IF SEGMENT-FOUND                                                     
107000       SUBTRACT WS-KART-KVBEART-KAMP FROM ANT-SUTPO-EJPB                  
107100       IF ANT-SUTPO-EJPB = 0 AND ANT-SUTPO-PB = 0                         
107200         PERFORM IMS-DLET-ARTM                                            
107300       ELSE                                                               
107400         PERFORM IMS-REPL-ARTM                                            
107500       END-IF                                                             
107600     END-IF                                                               
107700                                                                          
107800     MOVE WS-TIAAVV-MID           TO W-ANT-DABEHOV                        
107900     IF WS-TIAAVV-MID NOT = ZERO                                          
108000       IF WS-TIAAVV-MID < 5000                                            
108100         MOVE 20                  TO W-ANT-DABEHOV (1:2)                  
108200       ELSE                                                               
108300         IF WS-TIAAVV-MID < 9999                                          
108400           MOVE 19                TO W-ANT-DABEHOV (1:2)                  
108500         ELSE                                                             
108600           MOVE 9999              TO W-ANT-DABEHOV                        
108700         END-IF                                                           
108800       END-IF                                                             
108900     END-IF                                                               
109000                                                                          
109100     PERFORM IMS-GHU-ARTM11                                               
109200     IF SEGMENT-FOUND                                                     
109300       ADD WS-KART-KVBEART-KAMP    TO ANT-SUTPO-EJPB                      
109400       PERFORM IMS-REPL-ARTM                                              
109500     ELSE                                                                 
109600       MOVE WS-TIAAVV-MID         TO ANT-DABEHOV                          
109700       IF WS-TIAAVV-MID NOT = ZERO                                        
109800         IF WS-TIAAVV-MID < 5000                                          
109900           MOVE 20                TO ANT-DABEHOV (1:2)                    
110000         ELSE                                                             
110100           IF WS-TIAAVV-MID < 9999                                        
110200             MOVE 19              TO ANT-DABEHOV (1:2)                    
110300           ELSE                                                           
110400             MOVE 999999          TO ANT-DABEHOV                          
110500           END-IF                                                         
110600         END-IF                                                           
110700       END-IF                                                             
110800       MOVE ZERO                  TO ANT-SUTPO-PB                         
110900       MOVE WS-KART-KVBEART-KAMP   TO ANT-SUTPO-EJPB                      
111000       PERFORM IMS-ISRT-ARTM11                                            
111100     END-IF                                                               
111200     .                                                                    
111300     EJECT                                                                
111400 CBDBC-UPDATE-ARTM01-11 SECTION.                                          
111500     MOVE 'CBDBC-UPDATE-ARTM01-11       ' TO CURRENT-SECTION              
111600                                                                          
111700     PERFORM S04-LAS-ARTM01                                               
111800                                                                          
111900     COMPUTE ART-SUTPO-TOT          =                                     
112000            ART-SUTPO-TOT          - WS-KART-KVBEART-KAMP +               
112100            WS-MID-KVBEART-KAMP                                           
112200                                                                          
112300     PERFORM IMS-REPL-ARTM                                                
112400                                                                          
112500     PERFORM S05-WDATKONV                                                 
112600                                                                          
112700     MOVE WS-TIAAAAVV-TIRES       TO W-ANT-DABEHOV                        
112800     PERFORM IMS-GHU-ARTM11                                               
112900     IF SEGMENT-FOUND                                                     
113000       SUBTRACT WS-KART-KVBEART-KAMP FROM ANT-SUTPO-EJPB                  
113100       IF ANT-SUTPO-EJPB = 0 AND ANT-SUTPO-PB = 0                         
113200         PERFORM IMS-DLET-ARTM                                            
113300       ELSE                                                               
113400         PERFORM IMS-REPL-ARTM                                            
113500       END-IF                                                             
113600     END-IF                                                               
113700                                                                          
113800     MOVE WS-TIAAVV-MID           TO W-ANT-DABEHOV                        
113900     IF WS-TIAAVV-MID NOT = ZERO                                          
114000       IF WS-TIAAVV-MID < 5000                                            
114100         MOVE 20                  TO W-ANT-DABEHOV (1:2)                  
114200       ELSE                                                               
114300         IF WS-TIAAVV-MID < 9999                                          
114400           MOVE 19                TO W-ANT-DABEHOV (1:2)                  
114500         ELSE                                                             
114600           MOVE 999999            TO W-ANT-DABEHOV                        
114700         END-IF                                                           
114800       END-IF                                                             
114900     END-IF                                                               
115000                                                                          
115100     PERFORM IMS-GHU-ARTM11                                               
115200     IF SEGMENT-FOUND                                                     
115300       ADD WS-MID-KVBEART-KAMP    TO ANT-SUTPO-EJPB                       
115400       PERFORM IMS-REPL-ARTM                                              
115500     ELSE                                                                 
115600       MOVE WS-TIAAVV-MID         TO ANT-DABEHOV                          
115700       IF WS-TIAAVV-MID NOT = ZERO                                        
115800         IF WS-TIAAVV-MID < 5000                                          
115900           MOVE 20                TO ANT-DABEHOV (1:2)                    
116000         ELSE                                                             
116100           IF WS-TIAAVV-MID < 9999                                        
116200             MOVE 19              TO ANT-DABEHOV (1:2)                    
116300           ELSE                                                           
116400             MOVE 999999          TO ANT-DABEHOV                          
116500           END-IF                                                         
116600         END-IF                                                           
116700       END-IF                                                             
116800       MOVE ZERO                  TO ANT-SUTPO-PB                         
116900       MOVE WS-MID-KVBEART-KAMP   TO ANT-SUTPO-EJPB                       
117000       PERFORM IMS-ISRT-ARTM11                                            
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400 CBDC-MOVE-TO-IO-AREA SECTION.                                            
117500     MOVE 'CBDC-MOVE-TO-IO-AREA         ' TO CURRENT-SECTION              
117600*                                                                         
117700     IF MID-KVBEART-KAMP = ALL '+'                                        
117800       CONTINUE                                                           
117900     ELSE                                                                 
118000       MOVE WS-MID-KVBEART-KAMP TO KART-KVBEART-KAMP                      
118100     END-IF                                                               
118200*                                                                         
118300     IF MID-TIRES = ALL '+'                                               
118400       CONTINUE                                                           
118500     ELSE                                                                 
118600       MOVE WS-MID-TIRES        TO KART-TIRES                             
118700     END-IF                                                               
118800     PERFORM S03-LYS-UPP-RAD1                                             
118900     .                                                                    
119000     EJECT                                                                
119800 CBDE-MOVE-TO-IO-AREA SECTION.                                            
119900     MOVE 'CBDE-MOVE-TO-IO-AREA         ' TO CURRENT-SECTION              
120000                                                                          
120100     MOVE WS-UPDATE-IDARTNR   TO KART-IDARTNR                             
120400     MOVE WS-MID-KVBEART-KAMP TO KART-KVBEART-KAMP                        
120500     MOVE ZERO                TO KART-KVBEART-TPO4                        
120600                                 KART-KVRESS-ART                          
120700                                 KART-KVRESS-KAMP                         
120800                                 KART-KVBEART-KUND                        
120900     MOVE WS-MID-TIRES        TO KART-TIRES                               
121000     PERFORM S03-LYS-UPP-RAD1                                             
121100     .                                                                    
121200     EJECT                                                                
121300 CBDF-NYREG-WDK9 SECTION.                                                 
121400     MOVE 'CBDF-NYREG-WDK9              ' TO CURRENT-SECTION              
121500                                                                          
121600     PERFORM S04-LAS-ARTM01                                               
121700     ADD WS-MID-KVBEART-KAMP       TO ART-SUTPO-TOT                       
121800                                                                          
121900     PERFORM IMS-REPL-ARTM                                                
122000                                                                          
122100     MOVE WS-TIAAVV-MID            TO W-ANT-DABEHOV                       
122200     IF WS-TIAAVV-MID NOT = ZERO                                          
122300       IF WS-TIAAVV-MID < 5000                                            
122400         MOVE 20                   TO W-ANT-DABEHOV (1:2)                 
122500       ELSE                                                               
122600         IF WS-TIAAVV-MID < 9999                                          
122700           MOVE 19                 TO W-ANT-DABEHOV (1:2)                 
122800         ELSE                                                             
122900           MOVE 999999             TO W-ANT-DABEHOV                       
123000         END-IF                                                           
123100       END-IF                                                             
123200     END-IF                                                               
123210                                                                          
123400     PERFORM IMS-GHU-ARTM11                                               
123500                                                                          
123600     IF SEGMENT-FOUND                                                     
123700       ADD WS-MID-KVBEART-KAMP     TO ANT-SUTPO-EJPB                      
123800       PERFORM IMS-REPL-ARTM                                              
123900     ELSE                                                                 
124000       MOVE WS-TIAAVV-MID          TO ANT-DABEHOV                         
124100       IF WS-TIAAVV-MID NOT = ZERO                                        
124200         IF WS-TIAAVV-MID < 5000                                          
124300           MOVE 20                 TO ANT-DABEHOV (1:2)                   
124400         ELSE                                                             
124500           IF WS-TIAAVV-MID < 9999                                        
124600             MOVE 19               TO ANT-DABEHOV (1:2)                   
124700           ELSE                                                           
124800             MOVE 999999           TO ANT-DABEHOV                         
124900           END-IF                                                         
125000         END-IF                                                           
125100       END-IF                                                             
125200                                                                          
125300       MOVE ZERO                   TO ANT-SUTPO-PB                        
125400       MOVE WS-MID-KVBEART-KAMP    TO ANT-SUTPO-EJPB                      
125500       PERFORM IMS-ISRT-ARTM11                                            
125600     END-IF                                                               
125700     .                                                                    
125800     EJECT                                                                
125900 CC-DELETE-KONTROLL SECTION.                                              
126000     MOVE 'CC-DELETE-KONTROLL    ' TO CURRENT-SECTION                     
126100                                                                          
126400     MOVE +0                   TO INDX                                    
126500     PERFORM UNTIL INDX > MAX-INDX                                        
126600       IF INDX = +0                                                       
126710          MOVE MID-CMD-RAD1            TO WS-MID-CMD                      
126711          MOVE MID-IDARTNR-RAD1        TO WS-MID-IDARTNR                  
126720       ELSE                                                               
126721          MOVE MID-CMD-RAD    (INDX)   TO WS-MID-CMD                      
126722          MOVE MID-IDARTNR-RAD(INDX)   TO WS-MID-IDARTNR                  
126730       END-IF                                                             
126800       IF WS-MID-CMD NOT = ALL '+'                                        
126900          IF WS-MID-CMD = 'D' OR 'B'                                      
127400             PERFORM CCA-CHECK-ORDER-BACKORDER                            
127500             IF INDATA-DELETE-OK                                          
129000                MOVE SAVE-IDARTNR-ENTER  TO W-KART-IDARTNR                
129100                                            W-K601-IDARTNR                
129600             ELSE                                                         
129602                MOVE NEJ                 TO INDATA-DELETE-SW              
129603                MOVE NEJ                 TO INDATA-SW                     
129605                IF INDX = 0                                               
129606                  MOVE MFS-NUM-FAELT-FEL TO MOD-CMD-UPD-RAD1-ATTR         
129607                ELSE                                                      
129608                  MOVE MFS-NUM-FAELT-FEL TO MOD-CMD-UPD-ATTR(INDX)        
129609                END-IF                                                    
129610             END-IF                                                       
129611          ELSE                                                            
129613             MOVE NEJ                    TO INDATA-DELETE-SW              
129614             MOVE NEJ                    TO INDATA-SW                     
129616             IF INDX = 0                                                  
129617               MOVE MFS-NUM-FAELT-FEL    TO MOD-CMD-UPD-RAD1-ATTR         
129618             ELSE                                                         
129619               MOVE MFS-NUM-FAELT-FEL    TO MOD-CMD-UPD-ATTR(INDX)        
129620             END-IF                                                       
129621          END-IF                                                          
129630       END-IF                                                             
129700       ADD +1                            TO INDX                          
129800     END-PERFORM                                                          
130200     .                                                                    
130300     EJECT                                                                
130301 CCA-CHECK-ORDER-BACKORDER SECTION.                                       
130303     MOVE 'C-CHECK-ORDER-BACKORDER ' TO CURRENT-SECTION                   
130304                                                                          
130305     MOVE JA                   TO INDATA-DELETE-SW                        
130306                                                                          
130307     MOVE WS-MID-IDARTNR          TO W-KART-IDARTNR                       
130308                                     W-ARTM-IDARTNR                       
130309                                     W-K601-IDARTNR                       
130310                                                                          
130311     PERFORM IMS-GU-WDM211                                                
130312     IF SEGMENT-FOUND                                                     
130313        MOVE LOW-VALUE            TO W-WDQ4BSEQ-MIN-X                     
130314        MOVE HIGH-VALUE           TO W-WDQ4BSEQ-MAX-X                     
130315        MOVE KART-IDARTNR         TO W-WDQ4B-IDARTNR-MIN                  
130316                                     W-WDQ4B-IDARTNR-MAX                  
130317        PERFORM IMS-GU-WDQ4BSEQ                                           
130318        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END OR                   
130319                      INDATA-FEL                                          
130320          IF ORAD-IDKAMPRF = W-KAMP-IDKAMPRF                              
130321         AND ORAD-IDDC     = W-KAMP-IDDC                                  
130322             MOVE NEJ             TO INDATA-DELETE-SW                     
130323             MOVE ERR-DELETE-NOT-ALLOWED                                  
130324                                  TO MED-IDMFSFEL                         
130325          END-IF                                                          
130326          PERFORM IMS-GN-WDQ4BSEQ                                         
130327        END-PERFORM                                                       
130328                                                                          
130329        IF INDATA-OK                                                      
130330           MOVE LOW-VALUE         TO W-WDA5ASEQ-MIN-X                     
130331           MOVE HIGH-VALUE        TO W-WDA5ASEQ-MAX-X                     
130332           MOVE KART-IDARTNR      TO W-WDA5A-IDARTNR-MIN                  
130333                                     W-WDA5A-IDARTNR-MAX                  
130334           MOVE W-KAMP-IDDC       TO W-WDA5A-IDDC-MIN                     
130335                                     W-WDA5A-IDDC-MAX                     
130336           PERFORM IMS-GU-WDA5ASEQ                                        
130337           PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END OR                
130338                         INDATA-FEL                                       
130339              IF RAD-IDKAMPRF = W-KAMP-IDKAMPRF                           
130340                 IF RAD-KDSTARAD NOT = '4'                                
130341                    MOVE NEJ      TO INDATA-DELETE-SW                     
130342                    MOVE ERR-DELETE-NOT-ALLOWED                           
130343                                  TO MED-IDMFSFEL                         
130344                 END-IF                                                   
130345              END-IF                                                      
130346              PERFORM IMS-GN-WDA5ASEQ                                     
130347           END-PERFORM                                                    
130348        END-IF                                                            
130349                                                                          
130350        IF INDATA-OK                                                      
130351           MOVE LOW-VALUE         TO W-WDE4CSEQ-MIN-X                     
130352           MOVE HIGH-VALUE        TO W-WDE4CSEQ-MAX-X                     
130353           MOVE KART-IDARTNR      TO W-WDE4C-IDARTNR-MIN                  
130354                                     W-WDE4C-IDARTNR-MAX                  
130355           PERFORM IMS-GU-WDE4CSEQ                                        
130356           PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END OR                
130357                         INDATA-FEL                                       
130358             IF E411-ORAD-IDKAMPRF = W-KAMP-IDKAMPRF                      
130359                PERFORM IMS-GNP-WDE401                                    
130360                IF SEGMENT-FOUND                                          
130361                   IF KORD-IDDC  = W-KAMP-IDDC                            
130362                      MOVE NEJ       TO INDATA-DELETE-SW                  
130363                      MOVE ERR-DELETE-NOT-ALLOWED                         
130364                                     TO MED-IDMFSFEL                      
130365                   END-IF                                                 
130366                END-IF                                                    
130367             END-IF                                                       
130368             PERFORM IMS-GN-WDE4CSEQ                                      
130369           END-PERFORM                                                    
130370        END-IF                                                            
130371     END-IF                                                               
130372     .                                                                    
130373     EJECT                                                                
130374 CD-DELETE-UPDATE SECTION.                                                
130376     MOVE 'CD-DELETE-UPDATE    ' TO CURRENT-SECTION                       
130377                                                                          
130378     MOVE +0                   TO INDX                                    
130379     PERFORM UNTIL INDX > MAX-INDX                                        
130380       IF INDX = +0                                                       
130381          MOVE MID-CMD-RAD1            TO WS-MID-CMD                      
130382          MOVE MID-IDARTNR-RAD1        TO WS-MID-IDARTNR                  
130390       ELSE                                                               
130391          MOVE MID-CMD-RAD    (INDX)   TO WS-MID-CMD                      
130392          MOVE MID-IDARTNR-RAD(INDX)   TO WS-MID-IDARTNR                  
130395       END-IF                                                             
130396       IF WS-MID-CMD = 'D' OR 'B'                                         
130397          MOVE WS-MID-IDARTNR          TO W-KART-IDARTNR                  
130398                                          W-ARTM-IDARTNR                  
130399                                          W-K601-IDARTNR                  
130400          PERFORM IMS-GHU-WDM221                                          
130401          PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                    
130402            PERFORM IMS-DLET-WDM221                                       
130410            PERFORM IMS-GHN-WDM221                                        
130420          END-PERFORM                                                     
130430                                                                          
130440          PERFORM IMS-GHU-WDM211                                          
130450          PERFORM IMS-DLET-WDM211                                         
130460                                                                          
130470          IF KART-KVRESS-KAMP > ZERO                                      
130480             PERFORM CDB-UPDATE-WDK6                                      
130490          ELSE                                                            
130491             PERFORM CDC-UPDATE-WDK9                                      
130492          END-IF                                                          
130493          MOVE SAVE-IDARTNR-ENTER TO W-KART-IDARTNR                       
130494                                     W-K601-IDARTNR                       
130594       END-IF                                                             
130595       ADD +1                       TO INDX                               
130596     END-PERFORM                                                          
130598     .                                                                    
130599     EJECT                                                                
130600                                                                          
133660 CDB-UPDATE-WDK6 SECTION.                                                 
133700     MOVE 'CDB-UPDATE-WDK6              ' TO CURRENT-SECTION              
133710                                                                          
133900     PERFORM IMS-GHU-ARTC11                                               
134000     COMPUTE CLAG-KVRESS       = CLAG-KVRESS - KART-KVRESS-ART            
134100                                                                          
134200     PERFORM IMS-REPL-ARTC11                                              
134300     .                                                                    
134400     EJECT                                                                
134500                                                                          
134600 CDC-UPDATE-WDK9 SECTION.                                                 
134700     MOVE 'CDC-UPDATE-WDK9              ' TO CURRENT-SECTION              
134800                                                                          
134900     IF KART-TIRES > ZERO                                                 
135000       MOVE KART-TIRES            TO DAT-I-TIDATUM                        
135100       MOVE 'AAMMDD'              TO DAT-KDDATFORM                        
135200       CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                   
135300                           DAT-O-TIDATUM, DAT-KDSVAR                      
135400                                                                          
135500       IF DAT-KDSVAR-OK                                                   
135600         MOVE DAT-TIAA-VECKA      TO WS-YEAR-TIRES                        
135700         MOVE DAT-TIVV            TO WS-WEEK-TIRES                        
135800         MOVE DAT-TISEKEL         TO WS-SEKEL-TIRES                       
135900       END-IF                                                             
136000     ELSE                                                                 
136100       MOVE ZERO                  TO WS-TIRES-TIAAAAVV                    
136200     END-IF                                                               
136300                                                                          
136400     MOVE KART-KVBEART-KAMP       TO WS-KART-KVBEART-KAMP                 
136500     PERFORM S04-LAS-ARTM01                                               
136600                                                                          
136700     COMPUTE ART-SUTPO-TOT          =                                     
136800             ART-SUTPO-TOT          - WS-KART-KVBEART-KAMP                
136900                                                                          
137000     PERFORM IMS-REPL-ARTM                                                
137100                                                                          
137200     MOVE WS-TIAAAAVV-TIRES       TO W-ANT-DABEHOV                        
137300     PERFORM IMS-GHU-ARTM11                                               
137400                                                                          
137500     IF SEGMENT-FOUND                                                     
137600       COMPUTE ANT-SUTPO-EJPB =                                           
137700               ANT-SUTPO-EJPB - WS-KART-KVBEART-KAMP                      
137800       IF ANT-SUTPO-EJPB = 0 AND ANT-SUTPO-PB = 0                         
137900         PERFORM IMS-DLET-ARTM                                            
138000       ELSE                                                               
138100         PERFORM IMS-REPL-ARTM                                            
138200       END-IF                                                             
138300     ELSE                                                                 
138400       MOVE W-ANT-DABEHOV        TO ANT-DABEHOV                           
138500       MOVE WS-KART-KVBEART-KAMP TO ANT-SUTPO-PB                          
138600       MOVE WS-MID-KVBEART-KAMP  TO ANT-SUTPO-EJPB                        
138700       PERFORM IMS-ISRT-ARTM11                                            
138800     END-IF                                                               
138900     .                                                                    
139000     EJECT                                                                
139100 D-MFS-IDPFK-KONTROLL SECTION.                                            
139200     MOVE 'D-MFS-IDPFK-KONTROLL         ' TO CURRENT-SECTION              
139300                                                                          
139400     IF MFS-FIRST                                                         
139500       PERFORM DA-FIRST-PAGE                                              
139600     ELSE                                                                 
139700       IF MFS-NEXT                                                        
139800         PERFORM DB-NEXT-PAGE                                             
139900       ELSE                                                               
140000         PERFORM DC-SAME-PAGE                                             
140100       END-IF                                                             
140200     END-IF                                                               
140300     .                                                                    
140400     EJECT                                                                
140500 DA-FIRST-PAGE SECTION.                                                   
140600     MOVE 'DA-FIRST-PAGE                ' TO CURRENT-SECTION              
140700                                                                          
140800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
140900     PERFORM S02-INF-RUTINE                                               
141000                                                                          
141100     PERFORM MFS-RENSA-FAELT-IN                                           
141110                                                                          
141200     MOVE JA               TO ALLT-SW                                     
141300     .                                                                    
141400     EJECT                                                                
141500 DB-NEXT-PAGE SECTION.                                                    
141600     MOVE 'DB-NEXT-PAGE                 ' TO CURRENT-SECTION              
141700                                                                          
142000     MOVE JA                     TO ALLT-SW                               
142010     IF SAVE-IDTRANS = '2311'                                             
142011       INSPECT SAVE-IDKAMPRF-KEY REPLACING LEADING SPACE BY ZERO          
142012       MOVE SAVE-IDKAMPRF-KEY    TO W-KAMP-IDKAMPRF                       
142014       MOVE SAVE-IDDC-KEY        TO W-KAMP-IDDC                           
142020       INSPECT SAVE-IDARTNR-NEXT  REPLACING LEADING SPACE BY ZERO         
142030       MOVE SAVE-IDARTNR-NEXT    TO W-KART-IDARTNR                        
142040                                    W-K601-IDARTNR                        
142070     ELSE                                                                 
142080       PERFORM MFS-RENSA-FAELT-IN                                         
142090     END-IF                                                               
142100     .                                                                    
142200     EJECT                                                                
142300 DC-SAME-PAGE SECTION.                                                    
142400     MOVE ' DC-SAME-PAGE                ' TO CURRENT-SECTION              
142500                                                                          
142501     IF SAVE-IDTRANS = '2311' OR '0551'                                   
142502       MOVE SAVE-IDKAMPRF-KEY    TO W-KAMP-IDKAMPRF                       
142504       MOVE SAVE-IDDC-KEY        TO W-KAMP-IDDC                           
142505       MOVE SAVE-IDARTNR-ENTER   TO W-KART-IDARTNR                        
142507       IF  MID-TISTADAT-IN      = ALL '+'                                 
142508       AND MID-TISTODAT-IN      = ALL '+'                                 
142509       AND MID-CMD-RAD1         = ALL '+'                                 
142510       AND MID-CMD-RAD (01)     = ALL '+'                                 
142511       AND MID-CMD-RAD (02)     = ALL '+'                                 
142512       AND MID-CMD-RAD (03)     = ALL '+'                                 
142513       AND MID-CMD-RAD (04)     = ALL '+'                                 
142514       AND MID-CMD-RAD (05)     = ALL '+'                                 
142515       AND MID-CMD-RAD (06)     = ALL '+'                                 
142516       AND MID-CMD-RAD (07)     = ALL '+'                                 
142517       AND MID-CMD-RAD (08)     = ALL '+'                                 
142518       AND MID-CMD-RAD (09)     = ALL '+'                                 
142519       AND MID-IDARTNR          = ALL '+'                                 
142520       AND MID-KVBEART-KAMP     = ALL '+'                                 
142521       AND MID-TIRES            = ALL '+'                                 
142522         PERFORM MFS-RENSA-FAELT-IN                                       
142523       ELSE                                                               
142525         MOVE INF-PRESS-PF11   TO MED-IDMFSINF                            
142526         CALL WMEDKONV      USING MED-WMEDAREA                            
142527         MOVE MED-MFSINF       TO MOD-TEMFSFEL                            
142528         PERFORM DC-MID-INDATA-FOR-MOD                                    
142531       END-IF                                                             
142532     ELSE                                                                 
142533       PERFORM MFS-RENSA-FAELT-IN                                         
142534     END-IF                                                               
142535     .                                                                    
142536     EJECT                                                                
142537 DC-MID-INDATA-FOR-MOD SECTION.                                           
142538     MOVE 'DC-MID-INDATA-FOR-MOD   '  TO CURRENT-SECTION                  
142540                                                                          
142542     IF MID-TISTADAT-IN NOT = ALL '+'                                     
142544       MOVE MFS-ROER-EJ-FAELT     TO MOD-TISTADAT-IN                      
142545     ELSE                                                                 
142547       MOVE MFS-RENSA-FAELT       TO MOD-TISTADAT-IN                      
142548     END-IF                                                               
142549     MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TISTADAT-IN-ATTR                 
142550                                                                          
142551     IF MID-TISTODAT-IN NOT = ALL '+'                                     
142552       MOVE MFS-ROER-EJ-FAELT     TO MOD-TISTODAT-IN                      
142553     ELSE                                                                 
142554       MOVE MFS-RENSA-FAELT       TO MOD-TISTODAT-IN                      
142555     END-IF                                                               
142556     MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TISTODAT-IN-ATTR                 
142557                                                                          
142559     IF MID-CMD-RAD1 NOT = ALL '+'                                        
142560       MOVE MFS-ROER-EJ-FAELT     TO MOD-CMD-UPD-RAD1                     
142561     ELSE                                                                 
142562       MOVE MFS-RENSA-FAELT       TO MOD-CMD-UPD-RAD1                     
142563     END-IF                                                               
142564     MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-CMD-UPD-RAD1-ATTR                
142565                                                                          
142566     MOVE +1                      TO INDX                                 
142568     PERFORM UNTIL INDX > MAX-INDX                                        
142569       IF MID-CMD-RAD (INDX) NOT = ALL '+'                                
142570         MOVE MFS-ROER-EJ-FAELT   TO MOD-CMD-UPD       (INDX)             
142571       ELSE                                                               
142572         MOVE MFS-RENSA-FAELT     TO MOD-CMD-UPD       (INDX)             
142573       END-IF                                                             
142574       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-UPD-ATTR  (INDX)             
142575                                                                          
142576       ADD +1                     TO INDX                                 
142577     END-PERFORM                                                          
142578                                                                          
142579     IF MID-IDARTNR NOT = ALL '+'                                         
142580       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDARTNR                          
142581     ELSE                                                                 
142582       MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR                          
142583     END-IF                                                               
142584     MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDARTNR-ATTR                     
142585                                                                          
142586     IF MID-KVBEART-KAMP NOT = ALL '+'                                    
142587       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVBEART-KAMP                     
142588     ELSE                                                                 
142589       MOVE MFS-RENSA-FAELT       TO MOD-KVBEART-KAMP                     
142590     END-IF                                                               
142591     MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KVBEART-KAMP-ATTR                
142592                                                                          
142593     IF MID-TIRES NOT = ALL '+'                                           
142594       MOVE MFS-ROER-EJ-FAELT     TO MOD-TIRES                            
142595     ELSE                                                                 
142596       MOVE MFS-RENSA-FAELT       TO MOD-TIRES                            
142597     END-IF                                                               
142598     MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TIRES-ATTR                       
142599     .                                                                    
142600     EJECT                                                                
145400 E-LAES-VISA-INFO SECTION.                                                
145500     MOVE 'E-LAES-VISA-INFO             ' TO CURRENT-SECTION              
145600                                                                          
147000     PERFORM EA-VISA-KAMPANJ-INFO                                         
147100                                                                          
147200     PERFORM EB-VISA-RAD1-TILL-RAD10                                      
147300                                                                          
147400     IF SEGMENT-FOUND                                                     
147500       MOVE KART-IDARTNR          TO SAVE-IDARTNR-NEXT                    
147600       MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                         
147700       PERFORM S02-INF-RUTINE                                             
147800     ELSE                                                                 
147900       MOVE SAVE-IDARTNR-ENTER    TO SAVE-IDARTNR-NEXT                    
147910       MOVE INF-LAST-PAGE         TO MED-IDMFSINF                         
147920       PERFORM S02-INF-RUTINE                                             
148000     END-IF                                                               
148100                                                                          
148200     IF MFS-FIRST                                                         
148300       MOVE INF-FIRST-PAGE        TO MED-IDMFSINF                         
148400       PERFORM S02-INF-RUTINE                                             
148500     END-IF                                                               
149100                                                                          
149200     IF UPDATE-OK                                                         
149300       MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                         
149400       PERFORM S02-INF-RUTINE                                             
149500     END-IF                                                               
149810     MOVE '002'             TO MSGI-KDCALL                                
149820     MOVE '2311'            TO SAVE-IDTRANS                               
149830     MOVE SAVE-AREA         TO MSGI-SPAR-AREA                             
149840     CALL W005INIT       USING MSGI-WMSGINIT WDP7-PCB                     
149900     .                                                                    
150000     EJECT                                                                
150100 EA-VISA-KAMPANJ-INFO SECTION.                                            
150200     MOVE 'EA-VISA-KAMPANJ-INFO         ' TO CURRENT-SECTION              
150300                                                                          
150510     PERFORM IMS-GU-WDM201                                                
150520     IF SEGMENT-FOUND                                                     
150530       PERFORM EAA-MOVE-DATA-TO-MOD                                       
150600       PERFORM IMS-GNP-WDM211                                             
150700       IF SEGMENT-FOUND                                                   
150800         MOVE KART-IDARTNR       TO SAVE-IDARTNR-ENTER                    
150900         MOVE KART-IDARTNR       TO W-K601-IDARTNR                        
151000       ELSE                                                               
151010         MOVE WS-KAMPRF          TO SAVE-IDKAMPRF-KEY                     
151020         MOVE WS-IDDC            TO SAVE-IDDC-KEY                         
151030         MOVE WS-ARTNR           TO SAVE-IDARTNR-KEY                      
151100         MOVE ZERO               TO SAVE-IDARTNR-ENTER                    
151200         MOVE ZERO               TO W-K601-IDARTNR                        
151301       END-IF                                                             
151302     ELSE                                                                 
151304       MOVE WS-KAMPRF            TO SAVE-IDKAMPRF-KEY                     
151305       MOVE WS-IDDC              TO SAVE-IDDC-KEY                         
151306       MOVE WS-ARTNR             TO SAVE-IDARTNR-KEY                      
151307       MOVE ZERO                 TO SAVE-IDARTNR-ENTER                    
151308       MOVE ZERO                 TO SAVE-IDARTNR-NEXT                     
151312       MOVE ERR-KEYS-ARE-MISSING TO MED-IDMFSFEL                          
151313       PERFORM S01-ERR-RUTINE                                             
151314       PERFORM MFS-RENSA-FAELT-UT                                         
151320     END-IF                                                               
151400     .                                                                    
151500     EJECT                                                                
151600 EAA-MOVE-DATA-TO-MOD SECTION.                                            
151700     MOVE 'EAA-MOVE-DATA-TO-MOD         ' TO CURRENT-SECTION              
151800                                                                          
151900     MOVE KAMP-TISTADAT         TO MOD-TISTADAT-UT                        
152000     MOVE KAMP-TISTODAT         TO MOD-TISTODAT-UT                        
152100     .                                                                    
152200     EJECT                                                                
152300 EB-VISA-RAD1-TILL-RAD10 SECTION.                                         
152400     MOVE 'EB-VISA-RAD1-TILL-RAD10      ' TO CURRENT-SECTION              
152500                                                                          
152600     MOVE +0                    TO INDX                                   
152800     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-MISSING                     
152810       PERFORM EBD-MOVE-ARTC-INFO-TO-MOD                                  
152900       IF SEGMENT-FOUND                                                   
153000         IF INDX = 0                                                      
153100           PERFORM EBA-MOVE-RAD1-TO-MOD                                   
153200         ELSE                                                             
153300           PERFORM EBB-MOVE-RAD2-TO-RAD10-TO-MOD                          
153500         END-IF                                                           
153800       ELSE                                                               
153900         IF INDX = 0                                                      
154000           PERFORM EBC-MOVE-RENSA-FAELT-TO-RAD1                           
154100           MOVE SPACE           TO MFS-IDPFK                              
154200         ELSE                                                             
154300           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
154400         END-IF                                                           
154500       END-IF                                                             
154510       PERFORM IMS-GNP-WDM211                                             
154600       ADD 1                    TO INDX                                   
154700     END-PERFORM                                                          
154710     IF SEGMENT-FOUND                                                     
154740       MOVE KART-IDARTNR        TO SAVE-IDARTNR-NEXT                      
154750     ELSE                                                                 
154753       MOVE SAVE-IDARTNR-ENTER  TO SAVE-IDARTNR-NEXT                      
154754       IF INDX = +0                                                       
154755         MOVE MFS-STAENG-FAELT  TO MOD-CMD-UPD-RAD1-ATTR                  
154756         MOVE +1                TO INDX                                   
154757       END-IF                                                             
154758       PERFORM UNTIL INDX > MAX-INDX                                      
154759         MOVE MFS-STAENG-FAELT  TO MOD-CMD-UPD-ATTR (INDX)                
154760         ADD 1                  TO INDX                                   
154761       END-PERFORM                                                        
154770     END-IF                                                               
154800     .                                                                    
154900     EJECT                                                                
155000 EBD-MOVE-ARTC-INFO-TO-MOD SECTION.                                       
155100     MOVE 'EBD-MOVE-ARTC-INFO-TO-MOD    ' TO CURRENT-SECTION              
155200                                                                          
155210     MOVE KART-IDARTNR          TO W-K601-IDARTNR                         
155300     PERFORM IMS-GU-ARTC01                                                
155400     IF SEGMENT-FOUND                                                     
155500       IF INDX = 0                                                        
155600         MOVE ART-IDLEVNR       TO WS-IDLEVNR9                            
155700         MOVE WS-IDLEVNR-ED     TO MOD-IDLEVNR-RAD1                       
155800       ELSE                                                               
155900         MOVE ART-IDLEVNR       TO WS-IDLEVNR9                            
156000         MOVE WS-IDLEVNR-ED     TO MOD-IDLEVNR-RAD(INDX)                  
156100       END-IF                                                             
156200       PERFORM IMS-GU-ARTC11                                              
156300       IF SEGMENT-FOUND                                                   
156400         IF INDX = 0                                                      
156500           MOVE CLAG-IDANSK     TO WS-IDANSK9                             
156600           MOVE WS-IDANSK-ED    TO MOD-IDANSK-RAD1                        
156700         ELSE                                                             
156800           MOVE CLAG-IDANSK     TO WS-IDANSK9                             
156900           MOVE WS-IDANSK-ED    TO MOD-IDANSK-RAD(INDX)                   
157000         END-IF                                                           
157100       END-IF                                                             
157200     END-IF                                                               
157300     .                                                                    
157400     EJECT                                                                
157500 EBA-MOVE-RAD1-TO-MOD SECTION.                                            
157600     MOVE 'EBA-MOVE-RAD1-TO-MOD         ' TO CURRENT-SECTION              
157700                                                                          
157800     MOVE KART-IDARTNR          TO WS-ARTNR9                              
157900     MOVE WS-ARTNR-ED           TO MOD-IDARTNR-RAD1                       
158000     MOVE KART-KVBEART-KAMP     TO WS-KVBEART-KAMP9                       
158100     MOVE WS-KVBEART-KAMP-ED    TO MOD-KVBEART-KAMP-RAD1                  
158200     MOVE KART-KVBEART-KUND     TO WS-KVBEART-KUND9                       
158300     MOVE WS-KVBEART-KUND-ED    TO MOD-KVBEART-KUND-RAD1                  
158400     MOVE KART-KVRESS-KAMP      TO WS-KVRESS-KAMP9                        
158500     MOVE WS-KVRESS-KAMP-ED     TO MOD-KVRESS-KAMP-RAD1                   
158600     MOVE KART-TIRES            TO WS-TIRES9                              
158700     MOVE WS-TIRES-ED           TO MOD-TIRES-RAD1                         
158710                                                                          
158720     MOVE KAMP-IDKAMPRF         TO SAVE-IDKAMPRF-KEY                      
158730     MOVE KAMP-IDDC             TO SAVE-IDDC-KEY                          
158740     MOVE KART-IDARTNR          TO SAVE-IDARTNR-KEY                       
158750     MOVE KART-IDARTNR          TO SAVE-IDARTNR-ENTER                     
158800     .                                                                    
158900     EJECT                                                                
159000 EBB-MOVE-RAD2-TO-RAD10-TO-MOD SECTION.                                   
159100     MOVE 'EBB-MOVE-RAD2-TO-RAD10-TO-MOD' TO CURRENT-SECTION              
159200                                                                          
159300     MOVE KART-IDARTNR          TO WS-ARTNR9                              
159400     MOVE WS-ARTNR-ED           TO MOD-IDARTNR-RAD     (INDX)             
159500     MOVE KART-KVBEART-KAMP     TO WS-KVBEART-KAMP9                       
159600     MOVE WS-KVBEART-KAMP-ED    TO MOD-KVBEART-KAMP-RAD(INDX)             
159700     MOVE KART-KVBEART-KUND     TO WS-KVBEART-KUND9                       
159800     MOVE WS-KVBEART-KUND-ED    TO MOD-KVBEART-KUND-RAD(INDX)             
159900     MOVE KART-KVRESS-KAMP      TO WS-KVRESS-KAMP9                        
160000     MOVE WS-KVRESS-KAMP-ED     TO MOD-KVRESS-KAMP-RAD (INDX)             
160100     MOVE KART-TIRES            TO WS-TIRES9                              
160200     MOVE WS-TIRES-ED           TO MOD-TIRES-RAD(INDX)                    
160300     .                                                                    
160400     EJECT                                                                
160500 EBC-MOVE-RENSA-FAELT-TO-RAD1 SECTION.                                    
160600     MOVE 'EBC-MOVE-RENSA-FAELT-TO-RAD1 ' TO CURRENT-SECTION              
160700                                                                          
160800       MOVE MFS-RENSA-FAELT         TO MOD-CMD-UPD-RAD1                   
160810                                       MOD-IDARTNR-RAD1                   
160900                                       MOD-IDANSK-RAD1                    
161000                                       MOD-IDLEVNR-RAD1                   
161100                                       MOD-KVBEART-KAMP-RAD1              
161200                                       MOD-TIRES-RAD1                     
161300                                       MOD-KVRESS-KAMP-RAD1               
161400                                       MOD-KVBEART-KUND-RAD1              
161500     .                                                                    
161600     EJECT                                                                
161700 S01-ERR-RUTINE SECTION.                                                  
161800     MOVE 'S01-ERR-RUTINE               ' TO CURRENT-SECTION              
161900                                                                          
162000     CALL WMEDKONV USING MED-WMEDAREA                                     
162100     MOVE MED-MFSFEL                TO MOD-TEMFSFEL                       
162200     .                                                                    
162300 S02-INF-RUTINE SECTION.                                                  
162400     MOVE 'S02-INF-RUTINE               ' TO CURRENT-SECTION              
162500                                                                          
162600     CALL WMEDKONV USING MED-WMEDAREA                                     
162700     MOVE MED-MFSINF                TO MOD-TEMFSINF                       
162800     .                                                                    
162900     EJECT                                                                
163000 S03-LYS-UPP-RAD1 SECTION.                                                
163100     MOVE 'S03-LYS-UPP-RAD1             ' TO CURRENT-SECTION              
163200                                                                          
163300     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-RAD1-ATTR                  
163400     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANSK-RAD1-ATTR                   
163500     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLEVNR-RAD1-ATTR                  
163600     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVBEART-KAMP-RAD1-ATTR             
163700     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIRES-RAD1-ATTR                    
163800     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVRESS-KAMP-RAD1-ATTR              
163900     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVBEART-KUND-RAD1-ATTR             
164000     .                                                                    
164100     EJECT                                                                
164200 S04-LAS-ARTM01 SECTION.                                                  
164300     MOVE 'S04-LAS-ARTM01               ' TO CURRENT-SECTION              
164400                                                                          
164500     PERFORM IMS-GHU-ARTM01-GODK-GE                                       
164600     IF SEGMENT-MISSING                                                   
164700       MOVE WS-IDARTNR-ZERO     TO ARTM-IDARTNR-IN                        
164800       MOVE W-ARTM-IDARTNR      TO ARTM-IDARTNR-IN                        
164900       CALL W411ARTM USING ARTM-W411ARTM                                  
165000       PERFORM IMS-GHU-ARTM01-GODK-EJ-GE                                  
165100     END-IF                                                               
165200     .                                                                    
165300     EJECT                                                                
165400 S05-WDATKONV SECTION.                                                    
165500     MOVE 'S05-WDATKONV                 ' TO CURRENT-SECTION              
165600                                                                          
165700     MOVE WS-KART-TIRES         TO DAT-I-TIDATUM                          
165800     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
165900     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                     
166000                         DAT-O-TIDATUM, DAT-KDSVAR                        
166100                                                                          
166200     IF DAT-KDSVAR-OK                                                     
166300       MOVE DAT-TIAA-VECKA      TO WS-YEAR-TIRES                          
166400       MOVE DAT-TIVV            TO WS-WEEK-TIRES                          
166500       MOVE DAT-TISEKEL         TO WS-SEKEL-TIRES                         
166600     END-IF                                                               
166700     .                                                                    
166800     EJECT                                                                
166900 S06-LAES-IN-IGEN SECTION.                                                
167000     MOVE 'S06-LAES-IN-IGEN             ' TO CURRENT-SECTION              
167100                                                                          
167200     IF MID-TISTADAT-IN NOT = ALL '+'                                     
167300       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TISTADAT-IN-ATTR               
167400     END-IF                                                               
167500                                                                          
167600     IF MID-TISTODAT-IN NOT = ALL '+'                                     
167700       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TISTODAT-IN-ATTR               
167800     END-IF                                                               
167900                                                                          
167910     IF MID-CMD-RAD1    NOT = ALL '+'                                     
167920       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-CMD-UPD-RAD1-ATTR              
167930     END-IF                                                               
167940                                                                          
168000     MOVE +1                        TO INDX                               
168100     PERFORM UNTIL INDX > MAX-INDX                                        
168200       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-CMD-UPD-ATTR (INDX)            
168300       ADD +1                       TO INDX                               
168400     END-PERFORM                                                          
168500                                                                          
168600     IF MID-IDARTNR NOT = ALL '+'                                         
168700       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDARTNR-ATTR                   
168800     END-IF                                                               
168900                                                                          
169000     IF MID-KVBEART-KAMP NOT = ALL '+'                                    
169100       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KVBEART-KAMP-ATTR              
169200     END-IF                                                               
169300                                                                          
169400     IF MID-TIRES NOT = ALL '+'                                           
169500       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TIRES-ATTR                     
169600     END-IF                                                               
169700                                                                          
169800     .                                                                    
169900     EJECT                                                                
170000                                                                          
170100 MFS-RENSA-FAELT-UT SECTION.                                              
170200                                                                          
170500     MOVE MFS-RENSA-FAELT           TO MOD-TISTADAT-UT                    
170600                                       MOD-TISTODAT-UT                    
170610                                       MOD-CMD-UPD-RAD1                   
170700                                       MOD-IDARTNR-RAD1                   
170800                                       MOD-IDANSK-RAD1                    
170900                                       MOD-IDLEVNR-RAD1                   
171000                                       MOD-KVBEART-KAMP-RAD1              
171100                                       MOD-TIRES-RAD1                     
171200                                       MOD-KVRESS-KAMP-RAD1               
171300                                       MOD-KVBEART-KUND-RAD1              
171400                                                                          
171500     MOVE +1                          TO INDX                             
171600     PERFORM UNTIL INDX > MAX-INDX                                        
171700       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
171800       ADD +1                         TO INDX                             
171900     END-PERFORM                                                          
172000     .                                                                    
172100     SKIP2                                                                
172200 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
172300                                                                          
172400     MOVE MFS-RENSA-FAELT TO MOD-CMD-UPD         (INDX)                   
172500                             MOD-IDARTNR-RAD     (INDX)                   
172600                             MOD-IDANSK-RAD      (INDX)                   
172700                             MOD-IDLEVNR-RAD     (INDX)                   
172800                             MOD-KVBEART-KAMP-RAD(INDX)                   
172900                             MOD-TIRES-RAD       (INDX)                   
173000                             MOD-KVRESS-KAMP-RAD (INDX)                   
173100                             MOD-KVBEART-KUND-RAD(INDX)                   
173200                                                                          
173300     .                                                                    
173400     SKIP2                                                                
173500 MFS-RENSA-FAELT-IN SECTION.                                              
173600                                                                          
173700     MOVE MFS-RENSA-FAELT TO MOD-IDKAMPRF-IN                              
173800                             MOD-IDDC-IN                                  
173900                             MOD-IDARTNR-IN                               
174000                             MOD-TISTADAT-IN                              
174100                             MOD-TISTODAT-IN                              
174200                             MOD-IDARTNR                                  
174300                             MOD-KVBEART-KAMP                             
174400                             MOD-TIRES                                    
174401                                                                          
174402                             MOD-CMD-UPD-RAD1                             
174600     MOVE +1                          TO INDX                             
174700     PERFORM UNTIL INDX > MAX-INDX                                        
174800       MOVE MFS-RENSA-FAELT TO MOD-CMD-UPD(INDX)                          
174900       ADD +1               TO INDX                                       
175000     END-PERFORM                                                          
175100     .                                                                    
175200     EJECT                                                                
175300 MFS-ROER-EJ-FAELT-UT SECTION.                                            
175400                                                                          
175500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKAMPRF-UT                            
175600                               MOD-IDDC-UT                                
175700                               MOD-IDARTNR-UT                             
176000                               MOD-TISTADAT-UT                            
176100                               MOD-TISTODAT-UT                            
176110                               MOD-CMD-UPD-RAD1                           
176200                               MOD-IDARTNR-RAD1                           
176300                               MOD-IDANSK-RAD1                            
176400                               MOD-IDLEVNR-RAD1                           
176500                               MOD-KVBEART-KAMP-RAD1                      
176600                               MOD-TIRES-RAD1                             
176700                               MOD-KVRESS-KAMP-RAD1                       
176800                               MOD-KVBEART-KUND-RAD1                      
176900     MOVE +1 TO INDX                                                      
177000     PERFORM UNTIL INDX > MAX-INDX                                        
177100       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
177200       ADD +1 TO INDX                                                     
177300     END-PERFORM                                                          
177400     .                                                                    
177500     SKIP2                                                                
177600 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
177700                                                                          
177900     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-UPD         (INDX)                 
178000                               MOD-IDARTNR-RAD     (INDX)                 
178100                               MOD-IDANSK-RAD      (INDX)                 
178200                               MOD-IDLEVNR-RAD     (INDX)                 
178300                               MOD-KVBEART-KAMP-RAD(INDX)                 
178400                               MOD-TIRES-RAD       (INDX)                 
178500                               MOD-KVRESS-KAMP-RAD (INDX)                 
178600                               MOD-KVBEART-KUND-RAD(INDX)                 
178700     .                                                                    
178800     SKIP2                                                                
178900 MFS-ROER-EJ-FAELT-IN SECTION.                                            
179000                                                                          
179100     MOVE MFS-ROER-EJ-FAELT TO MOD-TISTADAT-IN                            
179200                               MOD-TISTODAT-IN                            
179300                               MOD-IDARTNR                                
179400                               MOD-KVBEART-KAMP                           
179500                               MOD-TIRES                                  
179600                               MOD-CMD-UPD-RAD1                           
179700     MOVE +1 TO INDX                                                      
179800     PERFORM UNTIL INDX > MAX-INDX                                        
179900       MOVE MFS-ROER-EJ-FAELT TO  MOD-CMD-UPD(INDX)                       
180000       ADD +1 TO INDX                                                     
180100     END-PERFORM                                                          
180200     .                                                                    
180210 MFS-STAENG-FAELT-IN SECTION.                                             
180211                                                                          
180220     MOVE MFS-STAENG-FAELT    TO MOD-TISTADAT-IN-ATTR                     
180230                                 MOD-TISTODAT-IN-ATTR                     
180240                                 MOD-IDARTNR-RAD1-ATTR                    
180250                                 MOD-CMD-UPD-RAD1-ATTR                    
180260                                                                          
180270     MOVE +1                  TO INDX                                     
180280     PERFORM UNTIL INDX > MAX-INDX                                        
180290       MOVE MFS-STAENG-FAELT  TO MOD-CMD-UPD-ATTR (INDX)                  
180291       ADD 1                  TO INDX                                     
180292     END-PERFORM                                                          
180293     MOVE MFS-STAENG-FAELT    TO MOD-IDARTNR-ATTR                         
180294                                 MOD-KVBEART-KAMP-ATTR                    
180295                                 MOD-TIRES-ATTR                           
180296     .                                                                    
180300* --- IMS SEKTIONER ---                                                   
180400     SKIP3                                                                
180500 IMS-GET-MSG SECTION.                                                     
180600                                                                          
180700     MOVE '  QC' TO GODK-STATUSKODER                                      
180800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
180900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
181000     PERFORM IMS-STATUSKONTROLL                                           
181100     .                                                                    
181200     SKIP3                                                                
181300 IMS-INSERT-MSG SECTION.                                                  
181400                                                                          
181800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
181900     MOVE SPACE TO GODK-STATUSKODER                                       
182000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
182100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
182200     PERFORM IMS-STATUSKONTROLL                                           
182300     .                                                                    
182400     EJECT                                                                
182500 IMS-GU-ARTC01 SECTION.                                                   
182600     MOVE 'IMS-GU-ARTC01              ' TO CURRENT-IMS-SECTION            
182700                                                                          
182800     STRING 'WLARTC01(IDARTNR  =' W-WDK601-X ')'                          
182900          DELIMITED BY SIZE INTO SSA1                                     
183000     MOVE '  GE' TO GODK-STATUSKODER                                      
183100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC-01 SSA1                   
183200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
183300     PERFORM IMS-STATUSKONTROLL                                           
183400     .                                                                    
183500 IMS-GU-ARTC11 SECTION.                                                   
183600     MOVE 'IMS-GU-ARTC11              ' TO CURRENT-IMS-SECTION            
183700                                                                          
183800     STRING 'WLARTC01(IDARTNR  =' W-WDK601-X ')'                          
183900          DELIMITED BY SIZE INTO SSA1                                     
184000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
184100     MOVE '  GE' TO GODK-STATUSKODER                                      
184200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC-11 SSA1 SSA2              
184300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
184400     PERFORM IMS-STATUSKONTROLL                                           
184500     .                                                                    
184600 IMS-GHU-ARTC11 SECTION.                                                  
184700     MOVE 'IMS-GHU-ARTC11             ' TO CURRENT-IMS-SECTION            
184800                                                                          
184900     STRING 'WLARTC01(IDARTNR  =' W-WDK601-X ')'                          
185000          DELIMITED BY SIZE INTO SSA1                                     
185100     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
185200     MOVE '  ' TO GODK-STATUSKODER                                        
185300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC-11 SSA1 SSA2             
185400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
185500     PERFORM IMS-STATUSKONTROLL                                           
185600     .                                                                    
185700 IMS-REPL-ARTC11 SECTION.                                                 
185800     MOVE 'IMS-REPL-ARTC11            ' TO CURRENT-IMS-SECTION            
185900                                                                          
186000     MOVE '  ' TO GODK-STATUSKODER                                        
186100     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC-11                      
186200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
186300     PERFORM IMS-STATUSKONTROLL                                           
186400     .                                                                    
186500 IMS-GHU-ARTM01-GODK-GE SECTION.                                          
186600     MOVE 'IMS-GHU-ARTM01-GODK-GE     ' TO CURRENT-IMS-SECTION            
186700                                                                          
186800     STRING 'WLARTM01(IDARTNR  =' W-WDK901-X ')'                          
186900          DELIMITED BY SIZE INTO SSA1                                     
187000     MOVE '  GE' TO GODK-STATUSKODER                                      
187100     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-ARTM SSA1                     
187200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
187300     PERFORM IMS-STATUSKONTROLL                                           
187400     .                                                                    
187500 IMS-GHU-ARTM01-GODK-EJ-GE SECTION.                                       
187600     MOVE 'IMS-GHU-ARTM01-GODK-EJ-GE  ' TO CURRENT-IMS-SECTION            
187700                                                                          
187800     STRING 'WLARTM01(IDARTNR  =' W-WDK901-X ')'                          
187900          DELIMITED BY SIZE INTO SSA1                                     
188000     MOVE '  ' TO GODK-STATUSKODER                                        
188100     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-ARTM SSA1                     
188200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
188300     PERFORM IMS-STATUSKONTROLL                                           
188400     .                                                                    
188500 IMS-GHU-ARTM11 SECTION.                                                  
188600     MOVE 'IMS-GHU-ARTM11             ' TO CURRENT-IMS-SECTION            
188700                                                                          
188800     STRING 'WLARTM01(IDARTNR  =' W-WDK901-X ')'                          
188900          DELIMITED BY SIZE INTO SSA1                                     
189000     STRING 'WLARTM11(DABEHOV  =' W-WDK911KY-X ')'                        
189100          DELIMITED BY SIZE INTO SSA2                                     
189200     MOVE '  GE' TO GODK-STATUSKODER                                      
189300     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-ARTM SSA1 SSA2                
189400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
189500     PERFORM IMS-STATUSKONTROLL                                           
189600     .                                                                    
189700 IMS-REPL-ARTM SECTION.                                                   
189800     MOVE 'IMS-REPL-ARTM              ' TO CURRENT-IMS-SECTION            
189900                                                                          
190000     MOVE '  ' TO GODK-STATUSKODER                                        
190100     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-ARTM                         
190200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
190300     PERFORM IMS-STATUSKONTROLL                                           
190400     .                                                                    
190500 IMS-DLET-ARTM SECTION.                                                   
190600     MOVE 'IMS-DLET-ARTM              ' TO CURRENT-IMS-SECTION            
190700                                                                          
190800     MOVE '  ' TO GODK-STATUSKODER                                        
190900     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-ARTM                         
191000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
191100     PERFORM IMS-STATUSKONTROLL                                           
191200     .                                                                    
191300 IMS-ISRT-ARTM11 SECTION.                                                 
191400     MOVE 'IMS-ISRT-ARTM11            ' TO CURRENT-IMS-SECTION            
191500                                                                          
191600     STRING 'WLARTM01(IDARTNR  =' W-WDK901-X ')'                          
191700          DELIMITED BY SIZE INTO SSA1                                     
191800     MOVE 'WLARTM11' TO SSA2                                              
191900     MOVE '  II' TO GODK-STATUSKODER                                      
192000     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-ARTM SSA1 SSA2               
192100     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
192200     PERFORM IMS-STATUSKONTROLL                                           
192300     .                                                                    
192400 IMS-GU-WDB601 SECTION.                                                   
192500     MOVE 'IMS-GU-WDB601 '   TO CURRENT-IMS-SECTION                       
192600                                                                          
192700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
192800          DELIMITED BY SIZE INTO SSA1                                     
192910     MOVE '  GE'              TO GODK-STATUSKODER                         
193000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
193100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
193200     PERFORM IMS-STATUSKONTROLL                                           
193300     .                                                                    
193310     EJECT                                                                
193400 IMS-GU-WDM201 SECTION.                                                   
193500     MOVE 'IMS-GU-WDM201       ' TO CURRENT-IMS-SECTION                   
193600                                                                          
193700     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
193800          DELIMITED BY SIZE INTO SSA1                                     
193900     MOVE '  GE'              TO GODK-STATUSKODER                         
194000     CALL CBLTDLI USING GU KAMP-PCB DLI-IO-WDM201 SSA1                    
194100     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
194200     PERFORM IMS-STATUSKONTROLL                                           
194300     .                                                                    
194400                                                                          
195500 IMS-GU-WDM211 SECTION.                                                   
195600     MOVE 'IMS-GU-WDM211       ' TO CURRENT-IMS-SECTION                   
195700                                                                          
195800     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
195900          DELIMITED BY SIZE INTO SSA1                                     
196000     STRING 'WDM211  (IDARTNR  =' W-WDM211-IDARTNR-X ')'                  
196100          DELIMITED BY SIZE INTO SSA2                                     
196200     MOVE '  GE'              TO GODK-STATUSKODER                         
196300     CALL CBLTDLI USING GU KAMP-PCB DLI-IO-WDM211 SSA1 SSA2               
196400     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
196500     PERFORM IMS-STATUSKONTROLL                                           
196600     .                                                                    
196610                                                                          
197600 IMS-GNP-WDM211 SECTION.                                                  
197700     MOVE 'IMS-GNP-WDM211      ' TO CURRENT-IMS-SECTION                   
197800                                                                          
197810     STRING 'WDM211  (IDARTNR =>' W-WDM211-IDARTNR-X ')'                  
197820          DELIMITED BY SIZE INTO SSA1                                     
198000     MOVE '  GE'              TO GODK-STATUSKODER                         
198100     CALL CBLTDLI USING GNP KAMP-PCB DLI-IO-WDM211 SSA1                   
198200     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
198300     PERFORM IMS-STATUSKONTROLL                                           
198400     .                                                                    
198500                                                                          
199600 IMS-GHU-WDM211 SECTION.                                                  
199700     MOVE 'IMS-GHU-WDM211      ' TO CURRENT-IMS-SECTION                   
199800                                                                          
199900     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
200000          DELIMITED BY SIZE INTO SSA1                                     
200100     STRING 'WDM211  (IDARTNR  =' W-WDM211-IDARTNR-X ')'                  
200200          DELIMITED BY SIZE INTO SSA2                                     
200300     MOVE '  '                TO GODK-STATUSKODER                         
200400     CALL CBLTDLI USING GHU KAMP-PCB DLI-IO-WDM211 SSA1 SSA2              
200500     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
200600     PERFORM IMS-STATUSKONTROLL                                           
200700     .                                                                    
200800                                                                          
202700 IMS-ISRT-WDM211 SECTION.                                                 
202800     MOVE 'IMS-ISRT-WDM211     ' TO CURRENT-IMS-SECTION                   
202900                                                                          
203000     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
203100          DELIMITED BY SIZE INTO SSA1                                     
203200     MOVE 'WDM211 '           TO SSA2                                     
203300     MOVE '  II' TO GODK-STATUSKODER                                      
203400     CALL CBLTDLI USING ISRT KAMP-PCB DLI-IO-WDM211 SSA1 SSA2             
203500     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
203600     PERFORM IMS-STATUSKONTROLL                                           
203700     .                                                                    
203710                                                                          
204500 IMS-REPL-WDM211 SECTION.                                                 
204600     MOVE 'IMS-REPL-WDM211     ' TO CURRENT-IMS-SECTION                   
204700                                                                          
204800     MOVE '  '             TO GODK-STATUSKODER                            
204900     CALL CBLTDLI USING REPL KAMP-PCB DLI-IO-WDM211                       
205000     MOVE KAMP-STATUS-CODE TO STATUS-WS                                   
205100     PERFORM IMS-STATUSKONTROLL                                           
205200     .                                                                    
205300                                                                          
206100 IMS-DLET-WDM211 SECTION.                                                 
206200     MOVE 'IMS-DLET-WDM211     ' TO CURRENT-IMS-SECTION                   
206300                                                                          
206400     MOVE '  '             TO GODK-STATUSKODER                            
206500     CALL CBLTDLI USING DLET KAMP-PCB DLI-IO-WDM211                       
206600     MOVE KAMP-STATUS-CODE TO STATUS-WS                                   
206700     PERFORM IMS-STATUSKONTROLL                                           
206800     .                                                                    
206900     EJECT                                                                
210300 IMS-GHU-WDM201 SECTION.                                                  
210400     MOVE 'IMS-GHU-WDM201      ' TO CURRENT-IMS-SECTION                   
210500                                                                          
210600     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
210700          DELIMITED BY SIZE INTO SSA1                                     
210800     MOVE '  GE'              TO GODK-STATUSKODER                         
210900     CALL CBLTDLI USING GHU KAMP-PCB DLI-IO-WDM201 SSA1                   
211000     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
211100     PERFORM IMS-STATUSKONTROLL                                           
211200     .                                                                    
211300                                                                          
213300 IMS-ISRT-WDM201 SECTION.                                                 
213400     MOVE 'IMS-ISRT-WDM201     ' TO CURRENT-IMS-SECTION                   
213500                                                                          
213710     MOVE 'WDM201  '          TO SSA1                                     
213800     MOVE '  II'              TO GODK-STATUSKODER                         
213900     CALL CBLTDLI USING ISRT KAMP-PCB DLI-IO-WDM201 SSA1                  
214000     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
214100     PERFORM IMS-STATUSKONTROLL                                           
214200     .                                                                    
214300                                                                          
215000 IMS-REPL-WDM201 SECTION.                                                 
215100     MOVE 'IMS-REPL-WDM201     ' TO CURRENT-IMS-SECTION                   
215200                                                                          
215300     MOVE '  '             TO GODK-STATUSKODER                            
215400     CALL CBLTDLI USING REPL KAMP-PCB DLI-IO-WDM201                       
215500     MOVE KAMP-STATUS-CODE TO STATUS-WS                                   
215600     PERFORM IMS-STATUSKONTROLL                                           
215700     .                                                                    
216800                                                                          
218100 IMS-GHU-WDM221 SECTION.                                                  
218200     MOVE 'IMS-GHU-WDM221      ' TO CURRENT-IMS-SECTION                   
218300                                                                          
218400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
218500          DELIMITED BY SIZE INTO SSA1                                     
218600     STRING 'WDM211  (IDARTNR  =' W-WDM211-IDARTNR-X ')'                  
218700          DELIMITED BY SIZE INTO SSA2                                     
218800     STRING 'WDM221  (WDM221KY>=' W-WDM221-MIN-X                          
218900                    '&WDM221KY<=' W-WDM221-MAX-X ')'                      
219000          DELIMITED BY SIZE INTO SSA3                                     
219100                                                                          
219200     MOVE '  GE'              TO GODK-STATUSKODER                         
219300     CALL CBLTDLI USING GHU KAMP-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
219400     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
219500     PERFORM IMS-STATUSKONTROLL                                           
219600     .                                                                    
219700                                                                          
220700 IMS-GNP-MIN-MAX-WDM221 SECTION.                                          
220800     MOVE 'IMS-GNP-MIN-MAX-WDM221' TO CURRENT-IMS-SECTION                 
220900                                                                          
221000     STRING 'WDM221  (WDM221KY>=' W-WDM221-MIN-X                          
221100                    '&WDM221KY<=' W-WDM221-MAX-X ')'                      
221200          DELIMITED BY SIZE INTO SSA1                                     
221300     MOVE '  GEGB'            TO GODK-STATUSKODER                         
221400     CALL CBLTDLI USING GNP KAMP-PCB DLI-IO-WDM221 SSA1                   
221500     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
221600     PERFORM IMS-STATUSKONTROLL                                           
221700     .                                                                    
221800                                                                          
222800 IMS-GHN-WDM221 SECTION.                                                  
222900     MOVE 'IMS-GHN-WDM221      ' TO CURRENT-IMS-SECTION                   
223000                                                                          
223100     STRING 'WDM221  (WDM221KY>=' W-WDM221-MIN-X                          
223200                    '&WDM221KY<=' W-WDM221-MAX-X ')'                      
223300          DELIMITED BY SIZE INTO SSA1                                     
223400     MOVE '  GEGB'            TO GODK-STATUSKODER                         
223500     CALL CBLTDLI USING GHN KAMP-PCB DLI-IO-WDM221 SSA1                   
223600     MOVE KAMP-STATUS-CODE    TO STATUS-WS                                
223700     PERFORM IMS-STATUSKONTROLL                                           
223800     .                                                                    
224600     EJECT                                                                
224700 IMS-DLET-WDM221 SECTION.                                                 
224800     MOVE 'IMS-DLET-WDM221     ' TO CURRENT-IMS-SECTION                   
224900                                                                          
225000     MOVE '  '             TO GODK-STATUSKODER                            
225100     CALL CBLTDLI USING DLET KAMP-PCB DLI-IO-WDM221                       
225200     MOVE KAMP-STATUS-CODE TO STATUS-WS                                   
225210     PERFORM IMS-STATUSKONTROLL                                           
225400     .                                                                    
225500     EJECT                                                                
225501 IMS-GU-WDQ4BSEQ SECTION.                                                 
225503     MOVE 'IMS-GN-WDQ4BSEQ     ' TO CURRENT-IMS-SECTION                   
225504                                                                          
225505     STRING 'WDQ401  (WDQ4BSEQ>=' W-WDQ4BSEQ-MIN-X                        
225506                    '&WDQ4BSEQ<=' W-WDQ4BSEQ-MAX-X ')'                    
225507          DELIMITED BY SIZE INTO SSA1                                     
225508     MOVE '  GE'            TO GODK-STATUSKODER                           
225509     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ401 SSA1                   
225510     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
225511     PERFORM IMS-STATUSKONTROLL                                           
225513     .                                                                    
225514                                                                          
225515 IMS-GN-WDQ4BSEQ SECTION.                                                 
225517     MOVE 'IMS-GN-WDQ4BSEQ     ' TO CURRENT-IMS-SECTION                   
225518                                                                          
225519     STRING 'WDQ401  (WDQ4BSEQ>=' W-WDQ4BSEQ-MIN-X                        
225520                    '&WDQ4BSEQ<=' W-WDQ4BSEQ-MAX-X ')'                    
225521          DELIMITED BY SIZE INTO SSA1                                     
225522     MOVE '  GEGB'          TO GODK-STATUSKODER                           
225523     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ401 SSA1                   
225524     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
225525     PERFORM IMS-STATUSKONTROLL                                           
225527     .                                                                    
225528                                                                          
225529 IMS-GU-WDA5ASEQ SECTION.                                                 
225531     MOVE 'IMS-GN-WDA5ASEQ     ' TO CURRENT-IMS-SECTION                   
225532                                                                          
225533     STRING 'WDA501  (WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
225534                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X ')'                    
225535          DELIMITED BY SIZE INTO SSA1                                     
225536     MOVE '  GE'            TO GODK-STATUSKODER                           
225537     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-WDA501 SSA1                   
225538     MOVE WDA5A-STATUS-CODE   TO STATUS-WS                                
225539     PERFORM IMS-STATUSKONTROLL                                           
225541     .                                                                    
225542                                                                          
225543 IMS-GN-WDA5ASEQ SECTION.                                                 
225545     MOVE 'IMS-GN-WDA5ASEQ     ' TO CURRENT-IMS-SECTION                   
225546                                                                          
225547     STRING 'WDA501  (WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
225548                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X ')'                    
225549          DELIMITED BY SIZE INTO SSA1                                     
225550     MOVE '  GEGB'          TO GODK-STATUSKODER                           
225551     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-WDA501 SSA1                   
225552     MOVE WDA5A-STATUS-CODE   TO STATUS-WS                                
225553     PERFORM IMS-STATUSKONTROLL                                           
225555     .                                                                    
225556                                                                          
225557 IMS-GU-WDE4CSEQ SECTION.                                                 
225559     MOVE 'IMS-GN-WDE4CSEQ     ' TO CURRENT-IMS-SECTION                   
225560                                                                          
225561     STRING 'WDE411  (WDE4CSEQ>=' W-WDE4CSEQ-MIN-X                        
225562                    '&WDE4CSEQ<=' W-WDE4CSEQ-MAX-X ')'                    
225570          DELIMITED BY SIZE INTO SSA1                                     
225580     MOVE '  GE'            TO GODK-STATUSKODER                           
225590     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-WDE411 SSA1                   
225591     MOVE WDE4C-STATUS-CODE   TO STATUS-WS                                
225592     PERFORM IMS-STATUSKONTROLL                                           
225594     .                                                                    
225595                                                                          
225596 IMS-GN-WDE4CSEQ SECTION.                                                 
225598     MOVE 'IMS-GN-WDE4CSEQ     ' TO CURRENT-IMS-SECTION                   
225599                                                                          
225600     STRING 'WDE411  (WDE4CSEQ>=' W-WDE4CSEQ-MIN-X                        
225601                    '&WDE4CSEQ<=' W-WDE4CSEQ-MAX-X ')'                    
225602          DELIMITED BY SIZE INTO SSA1                                     
225603     MOVE '  GEGB'          TO GODK-STATUSKODER                           
225604     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-WDE411 SSA1                   
225605     MOVE WDE4C-STATUS-CODE   TO STATUS-WS                                
225606     PERFORM IMS-STATUSKONTROLL                                           
225608     .                                                                    
225609                                                                          
225610 IMS-GNP-WDE401 SECTION.                                                  
225612     MOVE 'IMS-GNP-WDE401      ' TO CURRENT-IMS-SECTION                   
225613                                                                          
225614     MOVE 'WDE401 '         TO SSA1                                       
225615     MOVE '  '              TO GODK-STATUSKODER                           
225616     CALL CBLTDLI USING GNP WDE4C-PCB DLI-IO-WDE401 SSA1                  
225617     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
225618     PERFORM IMS-STATUSKONTROLL                                           
225619     .                                                                    
225620     EJECT                                                                
225630 IMS-STATUSKONTROLL SECTION.                                              
225700                                                                          
225800     SET STATUS-IX TO 1                                                   
225900     SEARCH GODK-STATUS                                                   
226000       AT END CALL FELLOG                                                 
226100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
226200     END-SEARCH                                                           
226300     .                                                                    
226400     EJECT                                                                
226500*    -COPY WY2000P1                                                       
226600     EJECT                                                                
226700*    -COPY WY2000Q1                                                       
