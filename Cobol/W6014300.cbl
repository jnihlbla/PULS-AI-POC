000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6014300.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   JUNI 92.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR EN UPPDATERINGS-MPP, SOM UPPDATERAR IN-            
001100*        LAGT KOLLI, SKAPAR AVVIKELSESEGMENT OCH SKICKAR TRANSAR          
001200*        TILL PROGRAMMEN FÖR AVSLUTA PARTI OCH UPPFÖLJNINGS-              
001300*        STATUS.                                                          
001400*                                                                         
001500*        MAN LÄSER : W6G1 - 01 (W6PLAA01 - PLACERINGSREG)                 
001600*                         - 30 (W6PLAA11 - PLACERINGSOMRÅDEN)             
001700*                                                                         
001800*                    W6D1 - 01 (W6INLA01 - INLEVERANSREG)                 
001900*                         - 11 (W6INLA11 - PARTI)                         
002000*                         - 21 (W6INLA21 - ARTIKELRAD/KOLLI)              
002100*                         - C1 (W6INLD01 - KOLLI - SEK INDEX)             
002200*                                                                         
002300*                    WDD8 - 01 (WLARTD01 - BUFFERTREG)                    
002400*                         - 11 (WLARTD11 - SALDOREG)                      
002401*                                                                         
002402*                    WDB6 - 01 (WDB601   - DCREG)                         
002403*                                                                         
002410*        PROGRAMMET          LÄSER      W6UPFA (W6L1)                     
002500*                                                                         
002600*        PROGRAMMET UPPDATERAR - W6D111 (W6INLA11 - PARTI)                
002700*                              - W6D121 (W6INLA21 - ARTRAD/KOLLI)         
002800*                              - SKAPAR UPPFÖLJNINGSTRANS (GENOM          
002900*                                ATT ANROPA W6019100)                     
003000*                              - AVSLUTAR PARTIET (GENOM ATT AN-          
003100*                                ROPA W6019300)                           
003110*                                                                         
003120*                              - WDD801 (WLARTD01 - BUFFERTREG)           
003130*                              - WDD811 (WLARTD11 - SALDOREG)             
003200*                                                                         
003300*    INDATA.                                                              
003400*        TRANSAKTION: W6T143  W6T143U                                     
003500*                     W6T191X                                             
003600*                     W6T193X (VIA DISPATCHER)                            
003700*        MID:         W6I14301                                            
003710*                     W6I14303                                            
003800*                                                                         
003900*    UTDATA.                                                              
004000*        MOD:         W6O14301                                            
004100*                     W6I19101 (PROG-TO-PROG-SW)                          
004200*                     W6I19301 (VIA DISPATCHER)                           
004300                                                                          
004400     SKIP3                                                                
004500 ENVIRONMENT DIVISION.                                                    
004600     EJECT                                                                
004700 DATA DIVISION.                                                           
004800 WORKING-STORAGE SECTION.                                                 
004801*    -- CHECKED BY WY2000                                                 
004810     SKIP3                                                                
004900 77  IDPGM                       PIC X(08)   VALUE 'W6014300'.            
005000                                                                          
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300                                                                          
005400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005600 77  TRANS-INDX                  PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77  MAX-TRANS-INDX              PIC S9(9)   VALUE +24  COMP SYNC.        
005800 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005900 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +350 COMP SYNC.        
006000                                                                          
006100 77  P-TO-P-PREFIX-LNG           PIC S9(4)   VALUE +17  COMP SYNC.        
006200 77  MID-6191-FASTDEL-LNG        PIC S9(4)   VALUE +15  COMP SYNC.        
006300 77  MID-6191-UPPF-POST-LNG      PIC S9(4)   VALUE +64  COMP SYNC.        
006400                                                                          
006500 77  WS-KVINLART                 PIC S9(7)   VALUE ZERO COMP-3.           
006510 77  WS-KVINLART-VOR             PIC S9(7)   VALUE ZERO COMP-3.           
006511 77  WS-KVINLART-UPP             PIC S9(7)   VALUE ZERO COMP-3.           
006520 77  WS-FUNNA-RADER              PIC S9(3)   VALUE ZERO COMP-3.           
006600                                                                          
006700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006800 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
006900 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
006901 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
006910 77  WS-ADBUFFOMR                PIC X(2)    VALUE SPACE.                 
006920 77  WS-ADBUFFGANG               PIC X(2)    VALUE SPACE.                 
006930 77  WS-ADBUFFPL                 PIC X(5)    VALUE SPACE.                 
007000                                                                          
007100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007200     88  INDATA-OK                           VALUE 'J'.                   
007300     88  INDATA-FEL                          VALUE 'N'.                   
007400                                                                          
007500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007600     88  NYCKLAR-OK                          VALUE 'J'.                   
007700     88  NYCKLAR-FEL                         VALUE 'N'.                   
008200                                                                          
008300 77  DIVKOLLI-SW                 PIC X       VALUE 'N'.                   
008400     88  DIVERSEKOLLI                        VALUE 'J'.                   
008500                                                                          
008510 77  SATS-SW                     PIC X       VALUE 'N'.                   
008520     88  SATSKOLLI                           VALUE 'J'.                   
008530                                                                          
008600 77  STATUS-SW                   PIC X       VALUE 'J'.                   
008710     88  STATUS-RAETT                        VALUE 'J'.                   
008720     88  STATUS-FEL                          VALUE 'N'.                   
008800                                                                          
008900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009000     88  ALLT-OK                             VALUE 'J'.                   
009010     88  NGT-FEL                             VALUE 'N'.                   
009020                                                                          
009030 77  KOLLI-SW                    PIC X       VALUE 'J'.                   
009040     88  KOLLI-FINNS                         VALUE 'J'.                   
009050     88  KOLLI-SAKNAS                        VALUE 'N'.                   
009100                                                                          
009110 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
009120     88  FOERSTA-6191                        VALUE 'J'.                   
009130                                                                          
009200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009300     88  EGEN-MID                            VALUE '6143'.                
009400     88  GODK-MID                            VALUE '6143' '6144'          
009410                                                   '6145'.                
009500     88  HELP-MID                            VALUE '0551'.                
009600     EJECT                                                                
009610*    SPAR-AREA FÖR INLA21                                                 
009700 01  FILLER                      PIC X(16)   VALUE 'SPAR AREA'.           
009800*01  -COPY W6D121    -PRE SPAR-                                           
010300     EJECT                                                                
010500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010600 01  GENERELLA-SUBPROGRAM.                                                
010700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
011001     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
011010     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
011020     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011100     EJECT                                                                
011110*   -COPY WMSGINIT                                                        
011120     EJECT                                                                
011200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011300*   -COPY WMEDAREA                                                        
011400     EJECT                                                                
011500 01  FILLER                        PIC X(16)   VALUE 'FELM AREA'.         
011600 01  FELM-CODES.                                                          
011700     03  FELM-KOR-UPPLYSTA-FAELT   PIC X(3)    VALUE '001'.               
011710     03  FELM-KONFLIKT             PIC X(3)    VALUE '002'.               
011800     03  FELM-OTILL-UPPDAT         PIC X(3)    VALUE '007'.               
011900     03  FELM-FINNS-EJ             PIC X(3)    VALUE '010'.               
012000     03  FELM-PF11-O-TOM-INDATRAD  PIC X(3)    VALUE '011'.               
012400     03  FELM-DIVERSEKOLLI         PIC X(3)    VALUE '182'.               
012410     03  FELM-SATSKOLLI            PIC X(3)    VALUE '185'.               
012500     03  FELM-KVALITETSFEL         PIC X(3)    VALUE '189'.               
012510     03  FELM-EJ-RT-3-EL-77        PIC X(3)    VALUE '227'.               
012520     03  FELM-KARANTAEN            PIC X(3)    VALUE '228'.               
012600     03  FELM-FEL-NYCKEL           PIC X(3)    VALUE '401'.               
012700     03  FELM-PLATS-SAKNAS         PIC X(3)    VALUE '764'.               
012800     03  ERR-CONTROL-NOT-COMPL     PIC X(3)    VALUE '215'.               
012810     03  ERR-WEIGHT-MISSING        PIC X(3)    VALUE '792'.               
012820     03  ERR-VOLUME-MISSING        PIC X(3)    VALUE '793'.               
012830     03  ERR-ORIGIN-MISSING        PIC X(3)    VALUE '794'.               
012900     SKIP3                                                                
013000 01  FILLER                        PIC X(16)   VALUE 'INFO AREA'.         
013100 01  MESSAGE-CODES.                                                       
013200     03  INFO-TRYCK-PF11           PIC X(3)    VALUE '003'.               
013300     03  INFO-UPPDAT-GJORD         PIC X(3)    VALUE '101'.               
013500     EJECT                                                                
013600 01  FILLER                        PIC X(16)   VALUE 'W611STYR'.          
013700     SKIP3                                                                
013800*    -COPY W611STYR                                                       
013900     EJECT                                                                
013910 01  FILLER                        PIC X(16)   VALUE 'W611PMRK'.          
013920     SKIP3                                                                
013930*    -COPY W611PMRK                                                       
013940     EJECT                                                                
014000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014100*                                                                         
014200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014300     SKIP3                                                                
014400*01  MID -COPY W6I14301                                                   
014500     EJECT                                                                
014510     SKIP3                                                                
014520*01  MID -COPY W6I14303                                                   
014530     EJECT                                                                
014600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014700     SKIP3                                                                
014800*01  -COPY WMSGAREA                                                       
014900     EJECT                                                                
015000*    03  MOD -COPY W6O14301   -RED MSG-AREA.                              
015100     EJECT                                                                
015110 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
015120                                                                          
015121 01  KOM-MSG-IO-AREA.                                                     
015130*03  -COPY WMSGKOM                                                        
015140     EJECT                                                                
015200 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
015300     SKIP3                                                                
015400*01  -COPY WMSGSNUF            -PRE P-TO-P-                               
016300     EJECT                                                                
016310 01  FILLER                    PIC X(16) VALUE '6191-MID-R6I9101'.        
016320                                                                          
016400     -COPY W6I19101   -PRE 6191-                                          
016500     EJECT                                                                
016510 01  FILLER                    PIC X(16) VALUE '6193-MID-R6I9301'.        
016520                                                                          
016600     -COPY W6I19301  -PRE 6193-                                           
016700     EJECT                                                                
017600 01  FILLER                    PIC X(16) VALUE 'MFS-AREA'.                
017700     SKIP3                                                                
017800*01  -COPY WMFSAREA                                                       
017900     EJECT                                                                
018000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018100*                                                                         
018200 01  FILLER                    PIC X(16) VALUE 'IMS-WS'.                  
018300                                                                          
018400 01  NYCKLAR-TILL-DLI.                                                    
018410*--------W6D1C (EGEN BAS)                                                 
018430     03  W-W6D1C1KY-MIN-X.                                                
018440         05  W-D1C1KY-IDLEVKLI-MIN   PIC X(5)  VALUE SPACE.               
018450         05  W-D1C1KY-IDOKOLLI-MIN   PIC 9(9)  VALUE ZERO.                
018451         05  W-D1C1KY-IDRADNR-INL-MIN PIC S9(5) VALUE ZERO COMP-3.        
018452         05  W-D1C1KY-IDDC-MIN       PIC X(2)  VALUE SPACE.               
018453         05  W-D1C1KY-IDLEVNR-MIN    PIC  X(5) VALUE SPACE.               
018454         05  W-D1C1KY-IDFS-MIN       PIC X(8)  VALUE SPACE.               
018455         05  W-D1C1KY-TIAVIDAT-MIN   PIC S9(7) VALUE ZERO COMP-3.         
018456         05  W-D1C1KY-IDRADNR-MIN    PIC S9(5) VALUE ZERO COMP-3.         
018460                                                                          
018470     03  W-W6D1C1KY-MAX-X.                                                
018480         05  W-D1C1KY-IDLEVKLI-MAX   PIC X(5)  VALUE SPACE.               
018490         05  W-D1C1KY-IDOKOLLI-MAX   PIC 9(9)  VALUE ZERO.                
018491         05  W-D1C1KY-IDRADNR-INL-MAX PIC S9(5) VALUE ZERO COMP-3.        
018492         05  W-D1C1KY-IDDC-MAX       PIC X(2)  VALUE SPACE.               
018493         05  W-D1C1KY-IDLEVNR-MAX    PIC  X(5) VALUE SPACE.               
018494         05  W-D1C1KY-IDFS-MAX       PIC X(8)  VALUE SPACE.               
018495         05  W-D1C1KY-TIAVIDAT-MAX   PIC S9(7) VALUE ZERO COMP-3.         
018496         05  W-D1C1KY-IDRADNR-MAX    PIC S9(5) VALUE ZERO COMP-3.         
018500                                                                          
018600*--------W6D1C (SEK INGÅNG)                                               
018700     03  W-W6D1CSEQ-X.                                                    
018900         05  W-SEQC-IDLEVNR-KOLLI  PIC  X(5)  VALUE SPACE.                
019000         05  W-SEQC-IDOKOLLI       PIC 9(9)   VALUE ZERO.                 
019100                                                                          
019200*--------W6D1                                                             
019300     03  W-W6D101KY-X.                                                    
019500         05  W-INL-IDLEVNR         PIC  X(5)  VALUE SPACE.                
019600         05  W-INL-IDFS            PIC X(8)   VALUE SPACE.                
019700         05  W-INL-TIAVIDAT        PIC S9(7)  VALUE ZERO COMP-3.          
019800                                                                          
019900     03  W-W6D111KY-X.                                                    
020000         05  W-ART-IDRADNR-INL     PIC S9(9)  VALUE ZERO COMP-3.          
020100                                                                          
020200     03  W-W6D121KY-X.                                                    
020300         05  W-RAD-IDRADNR         PIC S9(5)  VALUE ZERO COMP-3.          
020400                                                                          
020500     03  W-RAD-IDLEVNR-KOLLI-X.                                           
020600         05  W-RAD-IDLEVNR-KOLLI   PIC  X(5)  VALUE SPACE.                
020700                                                                          
020710     03  W-RAD-IDOKOLLI-X.                                                
020720         05  W-RAD-IDOKOLLI        PIC 9(9)   VALUE ZERO.                 
020730                                                                          
020800*--------WDD8                                                             
020900     03  W-WDD801KY-X.                                                    
021000         05  W-ART-IDARTNR-WDD8    PIC S9(9)  VALUE ZERO COMP-3.          
021100                                                                          
021200     03  W-WDD811KY-X.                                                    
021300         05  W-SALDO-IDDC          PIC X(2)   VALUE SPACE.                
021400         05  W-SALDO-ADBUFFOMR     PIC S9(3)  VALUE ZERO COMP-3.          
021410         05  W-SALDO-ADBUFPAF      PIC  9(8)  VALUE ZERO.                 
021500         05  W-SALDO-ADBUFFGANG    PIC S9(3)  VALUE ZERO COMP-3.          
021600         05  W-SALDO-ADBUFFPL      PIC S9(5)  VALUE ZERO COMP-3.          
021710                                                                          
021800                                                                          
021900*--------W6G1                                                             
022000     03  W-W6GXKEY-GX01-X.                                                
022100         05  W-IDHTYP-GX01         PIC X(4)    VALUE SPACE.               
022200         05  W-IDDC-GX01           PIC X(2)    VALUE SPACE.               
022300         05  FILLER                PIC X(24)   VALUE LOW-VALUE.           
022400                                                                          
022500     03  W-W6GXKEY-6006-X.                                                
022600         05  W-ADINLOMR-6006       PIC X(4)    VALUE SPACE.               
022700         05  FILLER                PIC X(1)    VALUE LOW-VALUE.           
022800                                                                          
022810     03  W-IDDC                    PIC X(2)    VALUE SPACE.               
022811                                                                          
022820*--------W6L101                                                           
022830     03  W-IDLOPNRM-X.                                                    
022840         05  W-IDLOPNRM              PIC S9(9) COMP-3 VALUE ZERO.         
022841                                                                          
022842*--------WDB601                                                           
022843     03  W-IDDC-B6-X.                                                     
022844         05 W-IDDC-B6            PIC X(2).                                
022850                                                                          
022900*    --- STATUS-KOD FRÅN IMS                                              
023000 01  STATUS-WS                     PIC XX.                                
023100     88  SEGMENT-FINNS                       VALUE '  '.                  
023200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023300     88  BASEN-SLUT                          VALUE 'GB'.                  
023400                                                                          
023500 01  GODK-STATUSKODER.                                                    
023600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023700     SKIP2                                                                
023800 01  SSA1                          PIC X(128).                            
023900 01  SSA2                          PIC X(64).                             
024000 01  SSA3                          PIC X(64).                             
024100     EJECT                                                                
024200*    --- IMS FUNKTIONSKODER                                               
024300*01  -COPY W0003                                                          
024400     EJECT                                                                
024500*    ---  DLI INPUT-OUTPUT AREA                                           
024600 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
024700                                                                          
024800 01  DLI-IO-AREA1.                                                        
024900     03  IO-AREA1              PIC X(150)   VALUE SPACE.                  
025000     SKIP2                                                                
026000     03  W6INLA11 REDEFINES IO-AREA1.                                     
026100*        05  -COPY W6D111                                                 
026200     EJECT                                                                
026220 01  DLI-IO-AREA2.                                                        
026230     03  IO-AREA2              PIC X(150)   VALUE SPACE.                  
026240     SKIP2                                                                
026250     03  W6INLA21 REDEFINES IO-AREA2.                                     
026700*        05  -COPY W6D121                                                 
026800     EJECT                                                                
026810 01  DLI-IO-AREA3.                                                        
026820     03  IO-AREA3              PIC X(150)   VALUE SPACE.                  
026830     SKIP2                                                                
026840     03  WLARTD01 REDEFINES IO-AREA3.                                     
027300*        05  -COPY WDD801   -PRE WDD8-                                    
027400     EJECT                                                                
027800     03  WLARTD11 REDEFINES IO-AREA3.                                     
027900*        05  -COPY WDD811                                                 
028000     EJECT                                                                
028001     03  W6INLD01 REDEFINES IO-AREA3.                                     
028002*        05  -COPY W6D1C1                                                 
028003     EJECT                                                                
028010 01  DLI-IO-AREA4.                                                        
028020     03  IO-AREA4              PIC X(150)   VALUE SPACE.                  
028030     SKIP2                                                                
028040     03  W6PLAA11 REDEFINES IO-AREA4.                                     
028050*        05  -COPY W6GX6006 -PRE PLAA-                                    
028060     EJECT                                                                
028070 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-UPFA'.         
028080     SKIP3                                                                
028090 01  DLI-IO-UPFA.                                                         
028091     03  IO-UPFA                PIC X(100)  VALUE SPACE.                  
028092     03  W6UPFA01 REDEFINES IO-UPFA.                                      
028093*        05  -COPY W6L101                                                 
028094     EJECT                                                                
028095 01  DLI-IO-AREA-UPFA11.                                                  
028096     03  W6UPFA11.                                                        
028097*        05  -COPY W6L111                                                 
028098     SKIP3                                                                
028099 01  DLI-IO-AREA-UPFA12.                                                  
028100     03  W6UPFA12.                                                        
028101*        05  -COPY W6L112                                                 
028102                                                                          
028103 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
028104 01   DLI-IO-AREA-B601.                                                   
028105*     03  -COPY WDB601                                                    
028106                                                                          
028107     EJECT                                                                
028110 LINKAGE SECTION.                                                         
028200                                                                          
028300*01  -COPY W0009      -PRE MSG-                                           
028600     EJECT                                                                
028610*01  -COPY W0009      -PRE ALT1-                                          
028620     EJECT                                                                
028700*01  -COPY W0009      -PRE DISP-                                          
028800     EJECT                                                                
028900*01  -COPY W0008      -PRE USEA-                                          
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029110*01  -COPY W0008      -PRE INLA-C-                                        
029120     05  FILLER                  PIC X.                                   
029130     EJECT                                                                
029200*01  -COPY W0008      -PRE INLD-                                          
029300     05  FILLER                  PIC X.                                   
029400     EJECT                                                                
029500*01  -COPY W0008      -PRE PLAA-                                          
029600     05  FILLER                  PIC X.                                   
029700     EJECT                                                                
029800*01  -COPY W0008      -PRE ARTD-                                          
029900     05  FILLER                  PIC X.                                   
029901     EJECT                                                                
029910*01  -COPY W0008      -PRE UPFA-                                          
029920     05  FILLER                  PIC X.                                   
030000     SKIP3                                                                
030001*01  -COPY W0008      -PRE WDB6-                                          
030002     05  FILLER                  PIC X.                                   
030003     SKIP3                                                                
030010***  PCB-ER FÖR SUBPROGRAM                                                
030100 01  STYR-HANA-PCB               PIC X.                                   
030110     SKIP3                                                                
030111 01  STYR-PLAA-PCB               PIC X.                                   
030112     SKIP3                                                                
030113 01  PMRK-INLB-PCB               PIC X.                                   
030114     SKIP3                                                                
030115 01  PMRK-INLC-PCB               PIC X.                                   
030116     SKIP3                                                                
030117 01  PMRK-PLAA-PCB               PIC X.                                   
030118     SKIP3                                                                
030120 01  KOM-KOMA-PCB                PIC X.                                   
030200     EJECT                                                                
030300 PROCEDURE DIVISION  USING MSG-PCB  ALT1-PCB DISP-PCB USEA-PCB            
030400                                    INLA-C-PCB      INLD-PCB              
030410                                    PLAA-PCB                              
030500                                    ARTD-PCB UPFA-PCB WDB6-PCB            
030501                                    STYR-HANA-PCB                         
030502                                    STYR-PLAA-PCB                         
030510                                    PMRK-INLB-PCB   PMRK-INLC-PCB         
030520                                    PMRK-PLAA-PCB                         
030600                                    KOM-KOMA-PCB.                         
030700     ENTRY 'DLITCBL' USING MSG-PCB  ALT1-PCB DISP-PCB USEA-PCB            
030800                                    INLA-C-PCB      INLD-PCB              
030810                                    PLAA-PCB                              
030900                                    ARTD-PCB UPFA-PCB WDB6-PCB            
030901                                    STYR-HANA-PCB                         
030902                                    STYR-PLAA-PCB                         
030910                                    PMRK-INLB-PCB   PMRK-INLC-PCB         
030920                                    PMRK-PLAA-PCB                         
031000                                    KOM-KOMA-PCB.                         
031100                                                                          
031200     PERFORM IMS-GET-MSG                                                  
031300     IF SEGMENT-FINNS                                                     
031400       PERFORM A-INIT                                                     
031500       PERFORM B-KOLLA-NYCKLAR                                            
031600       IF NYCKLAR-OK                                                      
032200          IF MFS-UPDATE  OR MFS-UPD-V OR MFS-UPD-X                        
032300             PERFORM G-KOLLA-INPUT                                        
032400             IF INDATA-OK                                                 
032401                PERFORM H-UPPDATERA                                       
032402             ELSE                                                         
032403                PERFORM F-LAES-VISA-INFO                                  
032405             END-IF                                                       
032406          ELSE                                                            
032407             IF MFS-FIRST                                                 
032408                PERFORM C-FOERSTA-SIDA                                    
032409             ELSE                                                         
032420                PERFORM E-SAMMA-SIDA                                      
032430             END-IF                                                       
032610             IF INDATA-OK                                                 
032800                PERFORM F-LAES-VISA-INFO                                  
032900             END-IF                                                       
033000          END-IF                                                          
033500       END-IF                                                             
033600       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
033700       PERFORM IMS-INSERT-MSG                                             
033800     END-IF                                                               
033900                                                                          
034000     MOVE ZERO TO RETURN-CODE                                             
034100     GOBACK                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 A-INIT SECTION.                                                          
034500                                                                          
034600     IF MSG-DUBBLA-TRANSKODER                                             
034700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I14301                 
034710                                             HTERM-MID-W6I14303           
034800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
034900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
035000     ELSE                                                                 
035100       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I14301                 
035110                                             HTERM-MID-W6I14303           
035200       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
035300       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
035400     END-IF                                                               
035500                                                                          
035600     MOVE MSG-KDTRTYP   TO MFS-KDTRTYP                                    
035700     MOVE MSG-IDPFK     TO MFS-IDPFK                                      
035800     MOVE MFS-IDTRANS   TO W-IDTRANS                                      
035900                                                                          
036000     MOVE LOW-VALUE       TO MSG-AREA                                     
036100     MOVE 'W6O143N1'      TO MFS-IDMOD                                    
036200     MOVE '6143'          TO MOD-IDTRANS                                  
036300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
036400                             MOD-TEMFSINF                                 
036500     IF EGEN-MID  OR HELP-MID OR MFS-UPD-X                                
036600       CONTINUE                                                           
036700     ELSE                                                                 
036800       MOVE SPACE         TO MFS-KDTRTYP                                  
036900       MOVE '7'           TO MFS-IDPFK                                    
037100     END-IF                                                               
037200                                                                          
037300     ACCEPT DAGENS-DATUM FROM DATE                                        
038210                                                                          
038220     MOVE JA TO FOERSTA-6191-SW                                           
038230     IF MFS-UPD-V                                                         
038240       MOVE ALL '+'            TO MID-W6I14301                            
038250       MOVE HTERM-MID-IDLEVNR-KOLLI-UT TO MID-IDLEVNR-KOLLI-UT            
038260       MOVE HTERM-MID-IDOKOLLI-UT      TO MID-IDOKOLLI-UT                 
038262       IF HTERM-MID-KVINLART-UPP  = ALL '+'  OR                           
038263          HTERM-MID-KVINLART-UPP  = ALL '-'                               
038264         CONTINUE                                                         
038265       ELSE                                                               
038266         MOVE HTERM-MID-KVINLART-UPP   TO MID-KVINLART-UPP                
038267                                          MOD-KVINLART-UPP                
038268         MOVE HTERM-MID-IDANSTNR-UPP     TO MID-IDANSTNR-UPP              
038269                                            MOD-IDANSTNR-UPP              
038270       END-IF                                                             
038271       IF HTERM-MID-ADINLOMR-NXT-UPP = ALL '+' OR                         
038272          HTERM-MID-ADINLOMR-NXT-UPP = ALL '-'                            
038273         CONTINUE                                                         
038274       ELSE                                                               
038275         MOVE HTERM-MID-ADINLOMR-NXT-UPP TO MID-ADINLOMR-NXT-UPP          
038276                                            MOD-ADINLOMR-NXT-UPP          
038277       END-IF                                                             
038280     END-IF                                                               
038281     PERFORM AA-INIT-NYCKLAR                                              
038282                                                                          
038283     IF MSGI-IDLAND-SPR = 'GB'                                            
038284       MOVE +2    TO SPRAK-IX                                             
038285       MOVE 'B  ' TO MED-IDSKYLT                                          
038286     ELSE                                                                 
038287       MOVE +1    TO SPRAK-IX                                             
038288       MOVE 'S  ' TO MED-IDSKYLT                                          
038289     END-IF                                                               
038290     .                                                                    
038291     EJECT                                                                
038292*----------------------------------------------------------------*        
038293 AA-INIT-NYCKLAR SECTION.                                                 
038294                                                                          
038295     MOVE ALL '+' TO MSGI-WMSGINIT                                        
038296     MOVE '013'                  TO MSGI-KDCALL                           
038297     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
038298     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
038299     MOVE '6143'                 TO MSGI-IDTRANS                          
038300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038310     .                                                                    
038400     EJECT                                                                
038500 B-KOLLA-NYCKLAR SECTION.                                                 
038600                                                                          
038800     MOVE JA    TO NYCKLAR-SW                                             
039000                                                                          
039100     PERFORM BA-KOLLA-LEVNR                                               
039200     PERFORM BB-KOLLA-KOLLINR                                             
039300     PERFORM BC-BEHANDLA-PARTINR                                          
039310     PERFORM BD-KOLLA-DC                                                  
039400                                                                          
039500     IF GODK-MID OR NYCKLAR-OK                                            
039600       MOVE WS-IDLEVNR-KOLLI TO MOD-IDLEVNR-KOLLI-UT                      
039900       MOVE WS-IDOKOLLI TO MOD-IDOKOLLI-UT                                
040000       INSPECT MOD-IDOKOLLI-UT REPLACING LEADING ZERO BY SPACE            
040001                                                                          
040010       MOVE WS-IDLOPNRM TO MOD-IDLOPNRM-UT                                
040020       INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE            
040030                                                                          
040040       MOVE DCS-IDDC    TO MOD-IDDC-UT                                    
040100     ELSE                                                                 
040200       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI-UT                       
040300                               MOD-IDOKOLLI-UT                            
040310                               MOD-IDLOPNRM-UT                            
040320                               MOD-IDDC-UT                                
040400     END-IF                                                               
040500                                                                          
040600     IF NYCKLAR-FEL                                                       
040700       MOVE FELM-FEL-NYCKEL TO MED-IDMFSFEL                               
040800       CALL WMEDKONV USING MED-WMEDAREA                                   
040900       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
041000       PERFORM MFS-RENSA-FAELT-IN                                         
041100       PERFORM MFS-RENSA-FAELT-UT                                         
041200     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 BA-KOLLA-LEVNR SECTION.                                                  
042500                                                                          
042600     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI-IN                         
042700                                                                          
042800     IF MID-IDLEVNR-KOLLI-IN = ALL '+'                                    
042900       MOVE MID-IDLEVNR-KOLLI-UT TO WS-IDLEVNR-KOLLI                      
043100                                                                          
043200     ELSE                                                                 
043300       MOVE MID-IDLEVNR-KOLLI-IN TO WS-IDLEVNR-KOLLI                      
043900     END-IF                                                               
044000                                                                          
044100     IF WS-IDLEVNR-KOLLI NOT = SPACE                                      
044200         MOVE WS-IDLEVNR-KOLLI TO W-SEQC-IDLEVNR-KOLLI                    
044210                                  W-RAD-IDLEVNR-KOLLI                     
044410     ELSE                                                                 
044420       MOVE NEJ TO NYCKLAR-SW                                             
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 BB-KOLLA-KOLLINR SECTION.                                                
044900                                                                          
045000     MOVE MFS-RENSA-FAELT TO MOD-IDOKOLLI-IN                              
045100                                                                          
045200     IF MID-IDOKOLLI-IN = ALL '+'                                         
045300       MOVE MID-IDOKOLLI-UT TO WS-IDOKOLLI                                
045400       INSPECT WS-IDOKOLLI REPLACING LEADING SPACE BY ZERO                
045500                                                                          
045600     ELSE                                                                 
045700       MOVE MID-IDOKOLLI-IN TO WS-IDOKOLLI                                
             IF NOT MFS-UPD-X                                                   
046100       MOVE '7'               TO MFS-IDPFK                                
046200       MOVE SPACE             TO MFS-KDTRTYP                              
             END-IF                                                             
046300     END-IF                                                               
046400                                                                          
046500     IF WS-IDOKOLLI NUMERIC                                               
046510       IF WS-IDOKOLLI > ZERO                                              
046600         MOVE WS-IDOKOLLI TO W-SEQC-IDOKOLLI                              
046601                             W-RAD-IDOKOLLI                               
046610       ELSE                                                               
046620         MOVE NEJ TO NYCKLAR-SW                                           
046630       END-IF                                                             
046700     ELSE                                                                 
046800       MOVE NEJ TO NYCKLAR-SW                                             
046900     END-IF                                                               
047000     .                                                                    
047100     EJECT                                                                
047200 BC-BEHANDLA-PARTINR SECTION.                                             
047300                                                                          
047400     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                              
047500                                                                          
047600     IF MID-IDLOPNRM-IN = ALL '+'                                         
047700       MOVE MID-IDLOPNRM-UT TO WS-IDLOPNRM                                
047710       INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO                
047800     ELSE                                                                 
047900       MOVE MID-IDLOPNRM-IN TO MOD-IDLOPNRM-UT                            
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048201 BD-KOLLA-DC      SECTION.                                                
048202                                                                          
048203     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
048204                                                                          
048205     IF MID-IDDC-IN = ALL '+'                                             
048206       MOVE MSGI-IDDC   TO W-IDDC-B6                                      
048209     ELSE                                                                 
048210       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
             IF NOT MFS-UPD-X                                                   
048211       MOVE '7'               TO MFS-IDPFK                                
048212       MOVE SPACE             TO MFS-KDTRTYP                              
             END-IF                                                             
048213     END-IF                                                               
048214     PERFORM IMS-GU-WDB601                                                
048215                                                                          
048216     IF DCS-KDDC = SPACE OR DCS-DDC                                       
048217       MOVE NEJ TO NYCKLAR-SW                                             
048218     ELSE                                                                 
048219       IF DCS-CDC OR DCS-CDC-TR                                           
048220         MOVE DCS-IDDC        TO W-IDDC                                   
048221       ELSE                                                               
048222         MOVE NEJ TO NYCKLAR-SW                                           
048223       END-IF                                                             
048225     END-IF                                                               
048226     .                                                                    
048227     EJECT                                                                
048228 C-FOERSTA-SIDA SECTION.                                                  
048230                                                                          
048250     PERFORM MFS-RENSA-FAELT-IN                                           
048251     EJECT                                                                
048252     .                                                                    
048253 E-SAMMA-SIDA SECTION.                                                    
048254                                                                          
048255     IF EGEN-MID OR HELP-MID                                              
048256        IF MID-INPUT-OVR = ALL '+'                                        
048259           PERFORM MFS-RENSA-FAELT-IN                                     
048260        ELSE                                                              
048270           MOVE INFO-TRYCK-PF11 TO MED-IDMFSINF                           
048280           CALL WMEDKONV USING MED-WMEDAREA                               
048290           MOVE MED-MFSINF TO MOD-TEMFSINF                                
048291           PERFORM EA-MID-INDATA-TILL-MOD                                 
048292        END-IF                                                            
048293     ELSE                                                                 
048294        PERFORM MFS-RENSA-FAELT-IN                                        
048295     END-IF                                                               
048296     .                                                                    
048297     EJECT                                                                
048298 EA-MID-INDATA-TILL-MOD SECTION.                                          
048299                                                                          
048332     IF MID-KVINLART-DIN-UPP        =  ALL '+'                            
048333        MOVE MFS-RENSA-FAELT        TO MOD-KVINLART-DIN-UPP               
048334     ELSE                                                                 
048335        MOVE MID-KVINLART-DIN-UPP   TO MOD-KVINLART-DIN-UPP               
048336        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVINLART-DIN-UPP-ATTR          
048337     END-IF                                                               
048338                                                                          
048339     IF MID-KVINLART-UPP            =  ALL '+'                            
048340        MOVE MFS-RENSA-FAELT        TO MOD-KVINLART-UPP                   
048341     ELSE                                                                 
048342        MOVE MID-KVINLART-UPP       TO MOD-KVINLART-UPP                   
048343        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVINLART-UPP-ATTR              
048344     END-IF                                                               
048345                                                                          
048346     IF MID-IDANSTNR-UPP            =  ALL '+'                            
048347        MOVE MFS-RENSA-FAELT        TO MOD-IDANSTNR-UPP                   
048348     ELSE                                                                 
048349        MOVE MID-IDANSTNR-UPP       TO MOD-IDANSTNR-UPP                   
048350        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDANSTNR-UPP-ATTR              
048351     END-IF                                                               
048352                                                                          
048353     IF MID-ADINLOMR-NXT-UPP        =  ALL '+'                            
048354        MOVE MFS-RENSA-FAELT        TO MOD-ADINLOMR-NXT-UPP               
048355     ELSE                                                                 
048356        MOVE MID-ADINLOMR-NXT-UPP   TO MOD-ADINLOMR-NXT-UPP               
048357        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADINLOMR-NXT-UPP-ATTR          
048358     END-IF                                                               
048360     .                                                                    
048361     EJECT                                                                
048370 F-LAES-VISA-INFO SECTION.                                                
048380                                                                          
048381     MOVE JA TO KOLLI-SW                                                  
048382                                                                          
048390     PERFORM IMS-GU-W6D111-ART                                            
048391     IF SEGMENT-FINNS                                                     
048392        PERFORM FA-LAES-RADDATA                                           
048393        IF DIVERSEKOLLI OR  STATUS-FEL OR  WS-FUNNA-RADER > 1             
048394           PERFORM FF-FLYTTA-FELMEDDELANDE                                
048395        ELSE                                                              
048396           PERFORM FB-FLYTTA-D111FAELT-TILL-MOD                           
048397           PERFORM FC-KOLLA-OM-KVALITETSFEL                               
048398           PERFORM FD-FLYTTA-RADDATA-TILL-MOD                             
048399           PERFORM FE-HAEMTA-BUFFPLATS-WDD8                               
048400        END-IF                                                            
048401     ELSE                                                                 
048402        MOVE NEJ TO KOLLI-SW                                              
048403        PERFORM FF-FLYTTA-FELMEDDELANDE                                   
048410     END-IF                                                               
048492     .                                                                    
048493     EJECT                                                                
048494 FA-LAES-RADDATA SECTION.                                                 
048495                                                                          
048496     MOVE JA    TO STATUS-SW                                              
048497     MOVE NEJ   TO DIVKOLLI-SW                                            
048498                   SATS-SW                                                
048499     MOVE ZERO  TO WS-FUNNA-RADER                                         
048500     MOVE ZERO  TO WS-KVINLART                                            
048501     MOVE ZERO  TO WS-KVINLART-VOR                                        
048502     MOVE ZERO  TO WS-KVINLART-UPP                                        
048503                                                                          
048510     PERFORM IMS-GNP-W6D121-ARTRAD                                        
048520                                                                          
048530     PERFORM UNTIL SEGMENT-SAKNAS OR DIVERSEKOLLI OR                      
048531                                     WS-FUNNA-RADER > 1                   
048540        IF RAD-FLDIVKLI = NEJ                                             
048541           IF RAD-FLSATS   = NEJ OR ART-ADLAGOMR = 30                     
048550              IF RAD-KDINLSTA = 'SAK' OR 'FPK' OR SPACE                   
048560                 ADD 1 TO WS-FUNNA-RADER                                  
048570                 MOVE RAD-KVINLART TO WS-KVINLART                         
048590              ELSE                                                        
048591                 IF RAD-KDINLSTA = 'VOR'                                  
048592                    ADD RAD-KVINLART TO WS-KVINLART-VOR                   
048593                 ELSE                                                     
048594                    CONTINUE                                              
048595                 END-IF                                                   
048596              END-IF                                                      
048597           ELSE                                                           
048598              MOVE JA TO SATS-SW                                          
048599           END-IF                                                         
048600        ELSE                                                              
048601           MOVE JA TO DIVKOLLI-SW                                         
048602        END-IF                                                            
048610                                                                          
048611        PERFORM IMS-GNP-W6D121-ARTRAD                                     
048620     END-PERFORM                                                          
048621                                                                          
048622     IF WS-FUNNA-RADER NOT = 1                                            
048623        MOVE NEJ TO STATUS-SW                                             
048630     END-IF                                                               
048631     .                                                                    
048640     EJECT                                                                
048650 FB-FLYTTA-D111FAELT-TILL-MOD SECTION.                                    
048660                                                                          
048670     MOVE ART-IDARTNR   TO MOD-IDARTNR                                    
048680     MOVE ART-BEART     TO MOD-BEART                                      
048690     MOVE ART-KDSORT    TO MOD-KDSORT                                     
048691     MOVE ART-BEFT      TO MOD-BEFT                                       
048692     MOVE ART-ADLAGOMR  TO MOD-ADLAGOMR                                   
048694     MOVE ART-ADGANG    TO MOD-ADGANG                                     
048696     MOVE ART-ADPLATS   TO MOD-ADPLATS                                    
048698                                                                          
048702                                                                          
048703     EVALUATE ART-KDFARLIG                                                
048704                                                                          
048705       WHEN 4                                                             
048706         IF ENGLISH-TEXT                                                  
048707           MOVE 'JA'         TO MOD-KDFARLIG                              
048708         ELSE                                                             
048709           MOVE 'YES'        TO MOD-KDFARLIG                              
048710         END-IF                                                           
048712       WHEN 5                                                             
048720         MOVE 'ASBEST'       TO MOD-KDFARLIG                              
048740       WHEN 6                                                             
048741         IF ENGLISH-TEXT                                                  
048750           MOVE 'KEMIKALIER' TO MOD-KDFARLIG                              
048751         ELSE                                                             
048752           MOVE 'CHEMICALS ' TO MOD-KDFARLIG                              
048760         END-IF                                                           
048761       WHEN 7                                                             
048762         IF ENGLISH-TEXT                                                  
048763           MOVE 'JA'         TO MOD-KDFARLIG                              
048764         ELSE                                                             
048765           MOVE 'YES'        TO MOD-KDFARLIG                              
048766         END-IF                                                           
048770       WHEN OTHER                                                         
048780         MOVE SPACE        TO MOD-KDFARLIG                                
048790                                                                          
048791     END-EVALUATE                                                         
048792     .                                                                    
048793     EJECT                                                                
048794 FC-KOLLA-OM-KVALITETSFEL SECTION.                                        
048795                                                                          
048796     IF ART-FLKVAFEL = JA   OR                                            
048797        ART-FLKVAKAR = JA                                                 
048798                                                                          
048799       MOVE LOW-VALUE    TO W-W6D1C1KY-MIN-X                              
048800       MOVE HIGH-VALUE   TO W-W6D1C1KY-MAX-X                              
048801       MOVE ART-IDRADNR-INL TO W-D1C1KY-IDRADNR-INL-MIN                   
048802                            W-D1C1KY-IDRADNR-INL-MAX                      
048803       MOVE ART-IDDC     TO W-D1C1KY-IDDC-MIN                             
048804                            W-D1C1KY-IDDC-MAX                             
048805       MOVE RAD-IDOKOLLI TO W-D1C1KY-IDOKOLLI-MIN                         
048806                            W-D1C1KY-IDOKOLLI-MAX                         
048807       MOVE RAD-IDLEVNR-KOLLI TO W-D1C1KY-IDLEVKLI-MIN                    
048808                                 W-D1C1KY-IDLEVKLI-MAX                    
048809       PERFORM IMS-GU-INLD-INLD01                                         
048810                                                                          
048811       MOVE ART-IDDC     TO STYR-IDDC                                     
048812       MOVE ART-IDARTNR  TO STYR-IDARTNR                                  
048813       MOVE ART-IDFKNGRP TO STYR-IDFKNGRP                                 
048814       MOVE SEQC-IDLEVNR TO STYR-IDLEVNR                                  
048815       MOVE ART-BEFT     TO STYR-BEFT                                     
048816                                                                          
048820       CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                    
048830                                         STYR-PLAA-PCB                    
048840       IF STYR-KDSVAR-OK                                                  
048850         MOVE STYR-ADINLOMR-FB TO MOD-ADINLOMR-FB                         
048851         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADINLOMR-FB-ATTR               
048860       ELSE                                                               
048870         MOVE SPACE                   TO MOD-ADINLOMR-FB                  
048880         MOVE FELM-KOR-UPPLYSTA-FAELT TO MOD-TEMFSFEL                     
048891         CALL WMEDKONV USING MED-WMEDAREA                                 
048892         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
048893         MOVE MFS-ALFA-FAELT-FEL      TO MOD-ADINLOMR-FB                  
048895       END-IF                                                             
048896     END-IF                                                               
048897     .                                                                    
048898     EJECT                                                                
048899 FD-FLYTTA-RADDATA-TILL-MOD SECTION.                                      
048900                                                                          
048901     MOVE WS-KVINLART     TO MOD-KVINLART                                 
048902     MOVE WS-KVINLART-VOR TO MOD-KVINLART-VOR                             
048910     .                                                                    
048920     EJECT                                                                
048930 FE-HAEMTA-BUFFPLATS-WDD8 SECTION.                                        
048940                                                                          
048950     MOVE +1 TO RAD-INDX                                                  
048960                                                                          
048970     MOVE ART-IDARTNR TO W-ART-IDARTNR-WDD8                               
048990     PERFORM IMS-GU-WDD811-CLAGER                                         
048991                                                                          
048992     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
048993                   RAD-INDX > +3                                          
048994       IF SALDO-ADBUFFOMR NOT = ART-ADLAGOMR                              
048995         MOVE SALDO-ADBUFFOMR  TO MOD-ADBUFFOMR(RAD-INDX)                 
048996         MOVE SALDO-ADBUFFGANG TO MOD-ADBUFFGANG(RAD-INDX)                
048997         MOVE SALDO-ADBUFFPL   TO MOD-ADBUFFPL(RAD-INDX)                  
048999                                                                          
049000         ADD +1 TO RAD-INDX                                               
049010       END-IF                                                             
049020                                                                          
049030       PERFORM IMS-GNP-WDD811-MBUFFPL                                     
049040     END-PERFORM                                                          
049050     .                                                                    
049060     EJECT                                                                
049070 FF-FLYTTA-FELMEDDELANDE SECTION.                                         
049080                                                                          
049081     IF KOLLI-SAKNAS                                                      
049082        MOVE FELM-FINNS-EJ        TO MED-IDMFSFEL                         
049083     ELSE                                                                 
049090        IF DIVERSEKOLLI                                                   
049092           MOVE FELM-DIVERSEKOLLI TO MED-IDMFSFEL                         
049093        ELSE                                                              
049095           IF SATSKOLLI                                                   
049096              MOVE FELM-SATSKOLLI TO MED-IDMFSFEL                         
049097           ELSE                                                           
049099              IF STATUS-FEL                                               
049100                 MOVE FELM-FINNS-EJ TO MED-IDMFSFEL                       
049101              END-IF                                                      
049102           END-IF                                                         
049103        END-IF                                                            
049104     END-IF                                                               
049110                                                                          
049120     CALL WMEDKONV USING MED-WMEDAREA                                     
049130     MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                   
049140                                                                          
049150     PERFORM MFS-RENSA-FAELT-IN                                           
049160     .                                                                    
049170     EJECT                                                                
059000 G-KOLLA-INPUT SECTION.                                                   
059100                                                                          
059110     MOVE SPACE        TO MED-IDMFSFEL                                    
059121     MOVE JA           TO INDATA-SW                                       
059123                                                                          
059200     PERFORM IMS-GU-W6D111-ART                                            
059300                                                                          
059310     IF SEGMENT-FINNS                                                     
059400       IF ART-FLKVAFEL = JA  OR                                           
059401          ART-FLKVAKAR = JA                                               
059410         IF MID-ADINLOMR-NXT-UPP    =  ALL '+'                            
059510           MOVE NEJ                   TO INDATA-SW                        
059700           MOVE MFS-ALFA-FAELT-FEL    TO MOD-ADINLOMR-FB                  
059800           IF ART-FLKVAFEL = JA                                           
059810              MOVE FELM-KVALITETSFEL  TO MED-IDMFSFEL                     
059900           ELSE                                                           
059910              MOVE FELM-KARANTAEN     TO MED-IDMFSFEL                     
060100           END-IF                                                         
060200         ELSE                                                             
060210           PERFORM GA-KOLLA-PLATS-OCH-RADDATA                             
060211           IF INDATA-OK                                                   
060212             PERFORM GC-KOLLA-VIKT-VOLYM-URSPR                            
060213           END-IF                                                         
060220         END-IF                                                           
060300       ELSE                                                               
060400         PERFORM GA-KOLLA-PLATS-OCH-RADDATA                               
060410         IF INDATA-OK                                                     
060420           PERFORM GC-KOLLA-VIKT-VOLYM-URSPR                              
060430         END-IF                                                           
060500       END-IF                                                             
060600                                                                          
060601        IF INDATA-OK                                                      
060610           IF MID-INPUT-OVR NOT = ALL '+'                                 
060800              PERFORM GB-BEARBETA-TYP-AV-UPPDAT                           
061100           END-IF                                                         
061110        END-IF                                                            
061120     ELSE                                                                 
061130        MOVE FELM-FINNS-EJ      TO MED-IDMFSFEL                           
061140        MOVE NEJ                TO INDATA-SW                              
061150     END-IF                                                               
061151                                                                          
061160** KOLLAR OM KONTROLLERAD PÅ 6139                                         
061170     IF INDATA-OK                                                         
061180        MOVE ART-IDLOPNRM     TO W-IDLOPNRM                               
061190        PERFORM IMS-GU-UPFA-01                                            
061191        IF SEGMENT-FINNS                                                  
061192          IF UPPF-KVKVAPRIM > 0                                           
061193** ARTIKEL UTTAGEN FÖR PRIMÄRKONTROLL                                     
061194             IF UPPF-KDKVASTA-PRI = '2' OR '3'                            
061195** PRIMÄRKONTROLL SATT SOM JA/NEJ. (OM NEJ HAR KR SKAPATS).               
061196                CONTINUE                                                  
061197             ELSE                                                         
061198                MOVE '215'     TO MED-IDMFSFEL                            
061199                MOVE NEJ       TO INDATA-SW                               
061200             END-IF                                                       
061201          END-IF                                                          
061202          IF UPPF-KVKVASEK > 0                                            
061203             IF UPPF-KDKVASTA-SEK = '2' OR '3'                            
061204                CONTINUE                                                  
061205             ELSE                                                         
061206                MOVE '215'     TO MED-IDMFSFEL                            
061207                MOVE NEJ       TO INDATA-SW                               
061208             END-IF                                                       
061209          END-IF                                                          
061210        END-IF                                                            
061211     END-IF                                                               
061212                                                                          
061213** KOLLAR ATT EVENTUELLT GAMLA KR BLIVIT BEDÖMDA                          
061214     IF INDATA-OK                                                         
061215       PERFORM IMS-GU-UPFA-01                                             
061216       IF SEGMENT-FINNS                                                   
061217         PERFORM IMS-GNP-UPFA11                                           
061218         PERFORM UNTIL SEGMENT-SAKNAS                                     
061219           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
061220             CONTINUE                                                     
061221           ELSE                                                           
061222             MOVE '215'     TO MED-IDMFSFEL                               
061223             MOVE NEJ       TO INDATA-SW                                  
061224           END-IF                                                         
061225           PERFORM IMS-GNP-UPFA11                                         
061226         END-PERFORM                                                      
061227       END-IF                                                             
061228     END-IF                                                               
061229                                                                          
061230** KOLLAR ATT EVENTUELL SPECIALKONTROLL ÄR GJORD                          
061231     IF INDATA-OK                                                         
061232       PERFORM IMS-GU-UPFA-01                                             
061233       IF SEGMENT-FINNS                                                   
061234         PERFORM IMS-GNP-UPFA12                                           
061235         PERFORM UNTIL SEGMENT-SAKNAS                                     
061236           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
061237             CONTINUE                                                     
061238           ELSE                                                           
061239             MOVE '215'     TO MED-IDMFSFEL                               
061240             MOVE NEJ       TO INDATA-SW                                  
061241           END-IF                                                         
061242           PERFORM IMS-GNP-UPFA12                                         
061243         END-PERFORM                                                      
061244       END-IF                                                             
061245     END-IF                                                               
061246                                                                          
061247     IF INDATA-FEL                                                        
061248        IF MED-IDMFSFEL         =   SPACE                                 
061249           MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                   
061250        END-IF                                                            
061251                                                                          
061252        CALL WMEDKONV USING MED-WMEDAREA                                  
061253        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
061254        MOVE FELM-OTILL-UPPDAT TO MED-IDMFSINF                            
061255        CALL WMEDKONV USING MED-WMEDAREA                                  
061256        MOVE MED-MFSINF     TO MOD-TEMFSINF                               
061257                                                                          
061258        IF NOT MFS-UPD-V                                                  
061259          PERFORM MFS-ROER-EJ-FAELT-IN                                    
061260        END-IF                                                            
061261        PERFORM MFS-ROER-EJ-FAELT-UT                                      
061262                                                                          
061270     END-IF                                                               
061300     .                                                                    
061400     EJECT                                                                
061500 GA-KOLLA-PLATS-OCH-RADDATA  SECTION.                                     
061600                                                                          
061610     MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSTNR-UPP-ATTR                    
061611     MOVE SPACE               TO MED-IDMFSFEL                             
061620                                                                          
061621** KOLLAR ATT PLATS ÄR UPPDATERAD FÖR CDC                                 
061622     IF DCS-CDC                                                           
061623       IF ART-ADTRDEST(1:2) = 'CD' OR ART-ADLAGOMR = 90                   
061624         CONTINUE                                                         
061625       ELSE                                                               
061630         IF  ART-ADPLATS = ZERO                                           
061632             MOVE NEJ                   TO INDATA-SW                      
061633             MOVE FELM-PLATS-SAKNAS     TO MED-IDMFSFEL                   
061635             MOVE MFS-NUM-FAELT-FEL     TO MOD-ADLAGOMR-ATTR              
061636                                           MOD-ADGANG-ATTR                
061637                                           MOD-ADPLATS-ATTR               
061638         END-IF                                                           
061639       END-IF                                                             
061640     END-IF                                                               
061641                                                                          
061642     MOVE ZERO TO WS-FUNNA-RADER                                          
061643                                                                          
061644     PERFORM IMS-GNP-W6D121-ARTRAD                                        
061645                                                                          
061646     PERFORM UNTIL SEGMENT-SAKNAS OR DIVERSEKOLLI OR                      
061647                                     WS-FUNNA-RADER > 1                   
061648        IF RAD-FLDIVKLI = NEJ                                             
061649           IF RAD-FLSATS = NEJ OR ART-ADLAGOMR = 30                       
061650              IF RAD-KDINLSTA = 'SAK' OR 'FPK' OR SPACE                   
061651                 ADD 1 TO WS-FUNNA-RADER                                  
061652                 MOVE RAD-W6D121 TO SPAR-RAD-W6D121                       
061653              END-IF                                                      
061654           ELSE                                                           
061655              MOVE NEJ TO INDATA-SW                                       
061656              MOVE FELM-SATSKOLLI TO MED-IDMFSFEL                         
061657           END-IF                                                         
061658        ELSE                                                              
061659           MOVE NEJ TO INDATA-SW                                          
061660           MOVE FELM-DIVERSEKOLLI TO MED-IDMFSFEL                         
061661        END-IF                                                            
061662                                                                          
061663        PERFORM IMS-GNP-W6D121-ARTRAD                                     
061664     END-PERFORM                                                          
061665                                                                          
061666     IF WS-FUNNA-RADER NOT = 1                                            
061667        MOVE NEJ TO INDATA-SW                                             
061668        IF MED-IDMFSFEL    = SPACE                                        
061670           MOVE FELM-FINNS-EJ TO MED-IDMFSFEL                             
061680        END-IF                                                            
061715     END-IF                                                               
062100                                                                          
063500     .                                                                    
063600     EJECT                                                                
063700 GB-BEARBETA-TYP-AV-UPPDAT  SECTION.                                      
063800                                                                          
063900     IF MID-ADINLOMR-NXT-UPP = ALL '+'                                    
064000                                                                          
070000                                                                          
070120       IF MID-KVINLART-UPP NOT    = ALL '+'                               
070130          IF MID-KVINLART-DIN-UPP = ALL '+'                               
070200             PERFORM GBB-KOLLA-OM-AVV-OCH-KDRT                            
070300          ELSE                                                            
070301             MOVE MFS-NUM-FAELT-FEL  TO MOD-KVINLART-UPP-ATTR             
070302             MOVE MFS-NUM-FAELT-FEL  TO MOD-KVINLART-DIN-UPP-ATTR         
070303             MOVE NEJ                TO INDATA-SW                         
070304             MOVE FELM-KONFLIKT      TO MED-IDMFSFEL                      
070305          END-IF                                                          
070306       ELSE                                                               
070307          IF MID-KVINLART-DIN-UPP NOT = ALL '+'                           
070308             PERFORM GBE-KOLLA-DELINLAEGGNING                             
070309          END-IF                                                          
070310       END-IF                                                             
070320                                                                          
070500     ELSE                                                                 
070600       PERFORM GBC-KOLLA-OM-ANNAN-INDATA                                  
070700                                                                          
070800       IF INDATA-FEL                                                      
070900         MOVE MFS-ALFA-FAELT-FEL      TO MOD-ADINLOMR-NXT-UPP-ATTR        
071000         MOVE FELM-KONFLIKT           TO MED-IDMFSFEL                     
071600       ELSE                                                               
071700         PERFORM GBD-KOLLA-OM-RAETT-ADRESS                                
071800       END-IF                                                             
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
074300 GBB-KOLLA-OM-AVV-OCH-KDRT SECTION.                                       
074400                                                                          
074410     IF ART-KDRT   =  +3 OR +77                                           
074420        MOVE NEJ                 TO INDATA-SW                             
074430        MOVE MFS-NUM-FAELT-FEL   TO MOD-KVINLART-UPP-ATTR                 
074431        MOVE FELM-EJ-RT-3-EL-77  TO MED-IDMFSFEL                          
074440     ELSE                                                                 
074501        IF MID-KVINLART-UPP NUMERIC  AND                                  
074510           MID-KVINLART-UPP NOT = SPAR-RAD-KVINLART                       
074700                                                                          
074800          IF MID-IDANSTNR-UPP NOT = ALL '+' AND                           
074810             MID-IDANSTNR-UPP NUMERIC                                     
075000                                                                          
075100            MOVE MFS-NUM-FAELT-RAETT TO MOD-KVINLART-UPP-ATTR             
075200                                        MOD-IDANSTNR-UPP-ATTR             
075300          ELSE                                                            
075400            MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSTNR-UPP-ATTR               
075500            MOVE MFS-NUM-FAELT-RAETT TO MOD-KVINLART-UPP-ATTR             
075600            MOVE NEJ              TO INDATA-SW                            
075700          END-IF                                                          
075800                                                                          
075900        ELSE                                                              
076000          MOVE MFS-NUM-FAELT-FEL  TO MOD-KVINLART-UPP-ATTR                
076100          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSTNR-UPP-ATTR               
076200          MOVE NEJ                TO INDATA-SW                            
076300        END-IF                                                            
076310     END-IF                                                               
076400     .                                                                    
076500     EJECT                                                                
076610 GBC-KOLLA-OM-ANNAN-INDATA SECTION.                                       
076700                                                                          
078800     IF MID-KVINLART-DIN-UPP NOT = ALL '+'                                
078900       MOVE MFS-NUM-FAELT-FEL     TO MOD-KVINLART-DIN-UPP-ATTR            
079000       MOVE NEJ                   TO INDATA-SW                            
079100     END-IF                                                               
079110                                                                          
079120     IF MID-KVINLART-UPP NOT = ALL '+'                                    
079130       MOVE MFS-NUM-FAELT-FEL     TO MOD-KVINLART-UPP-ATTR                
079140       MOVE NEJ                   TO INDATA-SW                            
079150     END-IF                                                               
079200                                                                          
079300     IF MID-IDANSTNR-UPP NOT = ALL '+'                                    
079400       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDANSTNR-UPP-ATTR                
079500       MOVE NEJ                   TO INDATA-SW                            
079600     END-IF                                                               
079700     .                                                                    
079800     EJECT                                                                
079900 GBD-KOLLA-OM-RAETT-ADRESS SECTION.                                       
080000                                                                          
080001     IF MID-ADINLOMR-NXT-UPP = SPAR-RAD-ADINLOMR                          
080002        MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                      
080003        MOVE NEJ                    TO INDATA-SW                          
080004        MOVE MFS-ALFA-FAELT-FEL     TO MOD-ADINLOMR-NXT-UPP-ATTR          
080005     ELSE                                                                 
080006                                                                          
080010        MOVE '6005'            TO W-IDHTYP-GX01                           
080020        MOVE DCS-IDDC          TO W-IDDC-GX01                             
080100        MOVE MID-ADINLOMR-NXT-UPP TO W-ADINLOMR-6006                      
080200        PERFORM IMS-GU-W6G130-PLACERING                                   
080300                                                                          
080400        IF SEGMENT-SAKNAS                                                 
080500           MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                   
080900           MOVE NEJ                 TO INDATA-SW                          
081000           MOVE MFS-ALFA-FAELT-FEL  TO MOD-ADINLOMR-NXT-UPP-ATTR          
081100                                                                          
081300        END-IF                                                            
081310     END-IF                                                               
081400     .                                                                    
081500     EJECT                                                                
081501 GBE-KOLLA-DELINLAEGGNING   SECTION.                                      
081502                                                                          
081510     IF MID-KVINLART-DIN-UPP NUMERIC     AND                              
081520        MID-KVINLART-DIN-UPP     > ZERO  AND                              
081521        MID-KVINLART-DIN-UPP     < SPAR-RAD-KVINLART                      
081530                                                                          
081570        MOVE MFS-NUM-FAELT-RAETT TO MOD-KVINLART-DIN-UPP-ATTR             
081590     ELSE                                                                 
081592        MOVE MFS-NUM-FAELT-FEL   TO MOD-KVINLART-DIN-UPP-ATTR             
081593        MOVE NEJ                 TO INDATA-SW                             
081594     END-IF                                                               
081595     .                                                                    
081600     EJECT                                                                
081601 GC-KOLLA-VIKT-VOLYM-URSPR   SECTION.                                     
081602                                                                          
081603     IF  ART-VKART = ZERO                                                 
081604        MOVE NEJ                TO INDATA-SW                              
081606        MOVE '792'              TO MED-IDMFSFEL                           
081607     END-IF                                                               
081608                                                                          
081609     IF  ART-VLARTNTO = ZERO                                              
081610        MOVE NEJ                TO INDATA-SW                              
081612        MOVE '793'              TO MED-IDMFSFEL                           
081613     END-IF                                                               
081614                                                                          
081616     IF  ART-KDARTURS = SPACE                                             
081617        MOVE NEJ                TO INDATA-SW                              
081618        MOVE '794'              TO MED-IDMFSFEL                           
081619     END-IF                                                               
081620     .                                                                    
081621     EJECT                                                                
081630 H-UPPDATERA SECTION.                                                     
081700                                                                          
081710     MOVE +1 TO TRANS-INDX                                                
081720                                                                          
081800     IF MID-INPUT-OVR = ALL '+'                                           
081900        PERFORM HA-INLAEGGNING-AV-KOLLI                                   
082100     ELSE                                                                 
083700       IF MID-ADINLOMR-NXT-UPP NOT = ALL '+'                              
083800         PERFORM HG-RETUR-AV-KOLLI                                        
083900       END-IF                                                             
083901                                                                          
083910       IF MID-KVINLART-UPP NOT = ALL '+'                                  
083920          PERFORM HC-INLAEGGNING-MED-AVVIKELSE                            
083930       END-IF                                                             
083940                                                                          
083950       IF MID-KVINLART-DIN-UPP NOT = ALL '+'                              
083960          PERFORM HD-DELINLAEGGNING                                       
083970       END-IF                                                             
084000                                                                          
084100     END-IF                                                               
084110                                                                          
084130     MOVE INFO-UPPDAT-GJORD TO MED-IDMFSINF                               
084140     CALL WMEDKONV USING MED-WMEDAREA                                     
084150     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
084160     PERFORM MFS-FORM-ATTR                                                
084180     PERFORM HH-LYS-UPP-UPPDAT-FAELT                                      
084200     .                                                                    
084300     EJECT                                                                
084360                                                                          
084397                                                                          
084420 HA-INLAEGGNING-AV-KOLLI SECTION.                                         
084500                                                                          
084600     PERFORM IMS-GU-W6D111-ART                                            
084700     PERFORM IMS-GHNP-W6D121-ARTRAD                                       
084800                                                                          
084900     PERFORM UNTIL SEGMENT-SAKNAS                                         
085000                                                                          
085100       IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR                                
085200                          SPACE                                           
085300         MOVE RAD-W6D121  TO SPAR-RAD-W6D121                              
085301         IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')                     
085302            PERFORM S07-CALL-W611PMRK                                     
085303            PERFORM S08-LAES-IGEN                                         
085304         END-IF                                                           
085310         PERFORM S01-UPPDAT-W6D121                                        
085400         PERFORM S02-SKAPA-UPPF-TRANS-W60191                              
085500         PERFORM S03-SKICKA-UPPF-TRANS-W60191                             
085600         PERFORM S04-AVSLUTA-PARTI-W60193                                 
085700       END-IF                                                             
085800                                                                          
085900       PERFORM IMS-GHNP-W6D121-ARTRAD                                     
086000     END-PERFORM                                                          
086100     .                                                                    
086200     EJECT                                                                
090700 HC-INLAEGGNING-MED-AVVIKELSE SECTION.                                    
090800                                                                          
090900     PERFORM IMS-GU-W6D111-ART                                            
091000     PERFORM IMS-GHNP-W6D121-ARTRAD                                       
091100                                                                          
091200     PERFORM UNTIL SEGMENT-SAKNAS                                         
091300                                                                          
091400       IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR                                
091500                          SPACE                                           
091600                                                                          
092010         MOVE RAD-W6D121   TO SPAR-RAD-W6D121                             
092020         IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')                     
092030            PERFORM S07-CALL-W611PMRK                                     
092040            PERFORM S08-LAES-IGEN                                         
092050         END-IF                                                           
092060                                                                          
092300                                                                          
092310         IF MID-KVINLART-UPP NOT  =  ZERO                                 
092320            MOVE MID-KVINLART-UPP TO RAD-KVINLART                         
092400            PERFORM S01-UPPDAT-W6D121                                     
092710         ELSE                                                             
092720            PERFORM S09-UPPDAT-W6D121-AVV                                 
092800         END-IF                                                           
092801                                                                          
092802         PERFORM S02-SKAPA-UPPF-TRANS-W60191                              
092803         PERFORM S04-AVSLUTA-PARTI-W60193                                 
092804                                                                          
092810       END-IF                                                             
092900                                                                          
093000       PERFORM IMS-GHNP-W6D121-ARTRAD                                     
093100     END-PERFORM                                                          
093200                                                                          
093210     IF MID-KVINLART-UPP NOT = ZERO                                       
093300       PERFORM IMS-GU-W6D111-ART                                          
093400       PERFORM IMS-GNP-W6D121-ARTRAD-LAST                                 
093500                                                                          
093600       PERFORM S06-UPPDAT-AVV-W6D121                                      
093601                                                                          
093602       IF RAD-FLPRIO  = 'J' AND                                           
093603          RAD-KVINLART > 0                                                
093610          PERFORM S07-CALL-W611PMRK                                       
093611       END-IF                                                             
093620                                                                          
093700       PERFORM S10-SKAPA-UPPF-W60191-NY-RAD                               
093801     END-IF                                                               
093802                                                                          
093803     PERFORM S03-SKICKA-UPPF-TRANS-W60191                                 
093900     .                                                                    
094000     EJECT                                                                
094010 HD-DELINLAEGGNING SECTION.                                               
094020                                                                          
094030     PERFORM IMS-GU-W6D111-ART                                            
094040     PERFORM IMS-GHNP-W6D121-ARTRAD                                       
094050                                                                          
094060     PERFORM UNTIL SEGMENT-SAKNAS                                         
094070                                                                          
094080       IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR                                
094090                          SPACE                                           
094091                                                                          
094092         MOVE RAD-W6D121            TO SPAR-RAD-W6D121                    
094093         MOVE MID-KVINLART-DIN-UPP  TO WS-KVINLART-UPP                    
094094         COMPUTE RAD-KVINLART = RAD-KVINLART - WS-KVINLART-UPP            
094095                                                                          
094096         PERFORM IMS-REPL-W6D121-ARTRAD                                   
094097         PERFORM S02-SKAPA-UPPF-TRANS-W60191                              
094098                                                                          
094099       END-IF                                                             
094100                                                                          
094101       PERFORM IMS-GHNP-W6D121-ARTRAD                                     
094102     END-PERFORM                                                          
094103                                                                          
094104     PERFORM IMS-GU-W6D111-ART                                            
094105     PERFORM IMS-GNP-W6D121-ARTRAD-LAST                                   
094106                                                                          
094107     PERFORM S11-SKAPA-DELINLAGD-RAD                                      
094108     PERFORM S10-SKAPA-UPPF-W60191-NY-RAD                                 
094109     PERFORM S03-SKICKA-UPPF-TRANS-W60191                                 
094110     IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')                         
094111        PERFORM S07-CALL-W611PMRK                                         
094112     END-IF                                                               
094113     PERFORM S04-AVSLUTA-PARTI-W60193                                     
094122     .                                                                    
094123     EJECT                                                                
100120 HG-RETUR-AV-KOLLI SECTION.                                               
100200                                                                          
100300     PERFORM IMS-GU-W6D111-ART                                            
100400     PERFORM IMS-GHNP-W6D121-ARTRAD                                       
100500                                                                          
100600     PERFORM UNTIL SEGMENT-SAKNAS                                         
100700                                                                          
100800       IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR                                
100900                          SPACE                                           
101000                                                                          
101010         MOVE RAD-W6D121           TO SPAR-RAD-W6D121                     
101100         MOVE MID-ADINLOMR-NXT-UPP TO RAD-ADINLOMR-NXT                    
101110         MOVE ZERO                 TO RAD-IDILIRAD                        
101120                                      RAD-IDILIST                         
101130                                      RAD-IDINLVGN                        
101140                                      RAD-TIUPPDAT                        
101300                                                                          
101400         PERFORM IMS-REPL-W6D121-ARTRAD                                   
101500         PERFORM S02-SKAPA-UPPF-TRANS-W60191                              
101600         PERFORM S03-SKICKA-UPPF-TRANS-W60191                             
101700       END-IF                                                             
101800                                                                          
101900       PERFORM IMS-GHNP-W6D121-ARTRAD                                     
102000     END-PERFORM                                                          
102100     .                                                                    
102200     EJECT                                                                
102300 HH-LYS-UPP-UPPDAT-FAELT SECTION.                                         
102400                                                                          
103200     IF MID-KVINLART-DIN-UPP NOT = ALL '+'                                
103300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVINLART-DIN-UPP-ATTR            
103400       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVINLART-UPP                     
103700     END-IF                                                               
103800                                                                          
103810     IF MID-KVINLART-UPP NOT = ALL '+'                                    
103820       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVINLART-UPP-ATTR                
103830       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVINLART-UPP                     
103840       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANSTNR-UPP-ATTR                
103850       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDANSTNR-UPP                     
103860     END-IF                                                               
103870                                                                          
103900     IF MID-ADINLOMR-NXT-UPP NOT = ALL '+'                                
104000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADINLOMR-NXT-UPP-ATTR            
104100       MOVE MFS-ROER-EJ-FAELT     TO MOD-ADINLOMR-NXT-UPP                 
104200     END-IF                                                               
104300     .                                                                    
104400     EJECT                                                                
117330 S01-UPPDAT-W6D121 SECTION.                                               
117400                                                                          
117500     MOVE SPACE TO RAD-ADINLOMR                                           
117600                   RAD-ADINLOMR-NXT                                       
117800     MOVE ZERO  TO RAD-IDILIRAD                                           
117900                   RAD-IDILIST                                            
117910                   RAD-IDINLVGN                                           
117920     MOVE DAGENS-DATUM TO RAD-TIUPPDAT                                    
118000     MOVE 'INL' TO RAD-KDINLSTA                                           
118100                                                                          
118200     PERFORM IMS-REPL-W6D121-ARTRAD                                       
118300     .                                                                    
118400     EJECT                                                                
118500 S02-SKAPA-UPPF-TRANS-W60191 SECTION.                                     
118600                                                                          
118701     MOVE 'W6014300'         TO 6191-MID-IDPGM                            
118710     MOVE ART-IDDC           TO 6191-MID-IDDC                             
118720     MOVE ART-IDLOPNRM       TO 6191-MID-IDLOPNRM(TRANS-INDX)             
118800     MOVE RAD-IDRADNR        TO 6191-MID-IDRADNR(TRANS-INDX)              
118900     MOVE RAD-KDINLPRIO      TO 6191-MID-KDINLPRIO(TRANS-INDX)            
119000     MOVE ART-PRARTSTD       TO 6191-MID-PRARTSTD(TRANS-INDX)             
119010     MOVE +0                 TO 6191-MID-KVKOLLI (TRANS-INDX)             
119020     MOVE 'N'                TO 6191-MID-FLINLI  (TRANS-INDX)             
119100     MOVE SPAR-RAD-ADINLOMR  TO 6191-MID-ADINLOMR-OLD(TRANS-INDX)         
119200     MOVE SPAR-RAD-ADINLOMR-NXT  TO                                       
119300                             6191-MID-ADINLOMR-NXT-OLD(TRANS-INDX)        
119400     MOVE SPAR-RAD-KDINLSTA  TO 6191-MID-KDINLSTA-OLD(TRANS-INDX)         
119500     MOVE SPAR-RAD-KVINLART  TO 6191-MID-KVINLART-OLD(TRANS-INDX)         
119600     MOVE RAD-ADINLOMR       TO 6191-MID-ADINLOMR-NEW(TRANS-INDX)         
119700     MOVE RAD-ADINLOMR-NXT   TO                                           
119800                             6191-MID-ADINLOMR-NXT-NEW(TRANS-INDX)        
119900     MOVE RAD-KDINLSTA       TO 6191-MID-KDINLSTA-NEW(TRANS-INDX)         
120000     MOVE RAD-KVINLART       TO 6191-MID-KVINLART-NEW(TRANS-INDX)         
120010                                                                          
120020     ADD +1                  TO TRANS-INDX                                
120030     IF TRANS-INDX           >  MAX-TRANS-INDX                            
120040        PERFORM S03-SKICKA-UPPF-TRANS-W60191                              
120050     END-IF                                                               
120100     .                                                                    
120200     EJECT                                                                
120300 S03-SKICKA-UPPF-TRANS-W60191 SECTION.                                    
120400                                                                          
120410     COMPUTE 6191-MID-KVPOST =  TRANS-INDX - 1                            
120900     COMPUTE P-TO-P-MSG-KVLL =  P-TO-P-PREFIX-LNG + 17 +                  
121100                                (6191-MID-KVPOST * 64)                    
121200                                                                          
121210     MOVE 'W6T191X '         TO P-TO-P-MSG-KDTRANS                        
121220     MOVE '6143'             TO P-TO-P-MSG-IDTRANS                        
121230     MOVE MFS-KDMFSFOR       TO P-TO-P-MSG-KDMFSFOR                       
121300     MOVE 6191-MID-W6I19101  TO P-TO-P-MSG-INDATA                         
121310                                                                          
121320     IF FOERSTA-6191                                                      
121321        PERFORM IMS-ISRT-ALT1-MSG-6191                                    
121330        MOVE NEJ    TO FOERSTA-6191-SW                                    
121340     ELSE                                                                 
121350        PERFORM IMS-PURG-ALT1-MSG-6191                                    
121360     END-IF                                                               
121410     MOVE +1        TO TRANS-INDX                                         
121500     .                                                                    
121600     EJECT                                                                
121700 S04-AVSLUTA-PARTI-W60193 SECTION.                                        
121800                                                                          
122000     MOVE ART-IDLOPNRM       TO 6193-MID-IDLOPNRM                         
122100     MOVE RAD-IDRADNR        TO 6193-MID-IDRADNR                          
122201                                                                          
122210     MOVE SPACE              TO MSG-KOM-WMSGKOM                           
           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
122230     MOVE LOW-VALUE          TO MSG-KOM-KDZ1                              
122240     MOVE LOW-VALUE          TO MSG-KOM-KDZ2                              
122250     MOVE SPACE              TO MSG-KOM-KDTRANS                           
122260     MOVE 'W6I19301'         TO MSG-KOM-IDCPYTXT                          
122270     MOVE 'INLEV   '         TO MSG-KOM-IDSNDNOD                          
122280     MOVE 'W6014300'         TO MSG-KOM-IDSNDJOB                          
122300                                                                          
122310     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
122320     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
122330     MOVE SPACE              TO MSG-KOM-IDMFSMED                          
122331                                                                          
122340     MOVE +29                TO P-TO-P-MSG-KVLL                           
122341     MOVE '6143'             TO P-TO-P-MSG-IDTRANS                        
122350     MOVE 'W6T193X '         TO P-TO-P-MSG-KDTRANS                        
122360     MOVE MFS-KDMFSFOR       TO P-TO-P-MSG-KDMFSFOR                       
122370                                                                          
122380     MOVE 6193-MID-W6I19301  TO P-TO-P-MSG-INDATA                         
122390                                                                          
122400     CALL W006KOM USING MSG-PCB                                           
122410                        DISP-PCB                                          
122420                        KOM-KOMA-PCB                                      
122430                        MSG-KOM-WMSGKOM                                   
122440                        P-TO-P-MSG-IO-AREA-SNUF                           
122450                                                                          
122500     .                                                                    
122600     EJECT                                                                
123900 S06-UPPDAT-AVV-W6D121 SECTION.                                           
124000                                                                          
124100     COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                           
124200                                                                          
124210     MOVE SPAR-RAD-W6D121    TO RAD-W6D121                                
124300     MOVE SPACE              TO RAD-ADINLOMR                              
124400                                RAD-ADINLOMR-NXT                          
125100     MOVE MID-IDANSTNR-UPP   TO RAD-IDANSTNR                              
125200     MOVE ZERO               TO RAD-IDILIRAD                              
125300                                RAD-IDILIST                               
125400                                RAD-IDINLVGN                              
125500     MOVE DAGENS-DATUM       TO RAD-TIUPPDAT                              
125900     MOVE 'AVV'              TO RAD-KDINLSTA                              
126000                                                                          
126100     COMPUTE RAD-KVINLART = SPAR-RAD-KVINLART - MID-KVINLART-UPP          
126400                                                                          
126500     PERFORM IMS-ISRT-W6D121-ARTRAD                                       
126510     .                                                                    
126520     EJECT                                                                
126530 S07-CALL-W611PMRK  SECTION.                                              
126540                                                                          
126550     MOVE RAD-IDLEVNR-KOLLI  TO PMRK-IDLEVNR                              
126560     MOVE RAD-IDOKOLLI       TO PMRK-IDOKOLLI                             
126561     MOVE ZERO               TO PMRK-IDLOPNRM                             
126562                                PMRK-IDRADNR                              
126570     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
126580                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
126600     .                                                                    
126700     EJECT                                                                
126710 S08-LAES-IGEN SECTION.                                                   
126711                                                                          
126720     PERFORM IMS-GU-W6D111-ART                                            
126730     PERFORM IMS-GHNP-W6D121-ARTRAD                                       
126740                                                                          
126750     PERFORM UNTIL SEGMENT-SAKNAS   OR                                    
126751           RAD-IDRADNR = SPAR-RAD-IDRADNR                                 
126752        PERFORM IMS-GHNP-W6D121-ARTRAD                                    
126753     END-PERFORM                                                          
126754     .                                                                    
126755     EJECT                                                                
126756 S09-UPPDAT-W6D121-AVV SECTION.                                           
126757                                                                          
126758     MOVE SPACE TO RAD-ADINLOMR                                           
126759                   RAD-ADINLOMR-NXT                                       
126760     MOVE ZERO  TO RAD-IDILIRAD                                           
126761                   RAD-IDILIST                                            
126762                   RAD-IDINLVGN                                           
126763                   RAD-TIUPPDAT                                           
126764     MOVE 'AVV' TO RAD-KDINLSTA                                           
126765                                                                          
126766     PERFORM IMS-REPL-W6D121-ARTRAD                                       
126767     .                                                                    
126768     EJECT                                                                
126769 S10-SKAPA-UPPF-W60191-NY-RAD SECTION.                                    
126770                                                                          
126771     MOVE 'W6014300'         TO 6191-MID-IDPGM                            
126772     MOVE ART-IDDC           TO 6191-MID-IDDC                             
126773     MOVE ART-IDLOPNRM       TO 6191-MID-IDLOPNRM(TRANS-INDX)             
126774     MOVE RAD-IDRADNR        TO 6191-MID-IDRADNR(TRANS-INDX)              
126775     MOVE RAD-KDINLPRIO      TO 6191-MID-KDINLPRIO(TRANS-INDX)            
126776     MOVE ART-PRARTSTD       TO 6191-MID-PRARTSTD(TRANS-INDX)             
126777     MOVE +0                 TO 6191-MID-KVKOLLI (TRANS-INDX)             
126778     MOVE 'N'                TO 6191-MID-FLINLI  (TRANS-INDX)             
126779     MOVE SPACE              TO 6191-MID-ADINLOMR-OLD(TRANS-INDX)         
126780                             6191-MID-ADINLOMR-NXT-OLD(TRANS-INDX)        
126781                                6191-MID-KDINLSTA-OLD(TRANS-INDX)         
126782     MOVE ZERO               TO 6191-MID-KVINLART-OLD(TRANS-INDX)         
126783     MOVE RAD-ADINLOMR       TO 6191-MID-ADINLOMR-NEW(TRANS-INDX)         
126784     MOVE RAD-ADINLOMR-NXT   TO                                           
126785                             6191-MID-ADINLOMR-NXT-NEW(TRANS-INDX)        
126786     MOVE RAD-KDINLSTA       TO 6191-MID-KDINLSTA-NEW(TRANS-INDX)         
126787     MOVE RAD-KVINLART       TO 6191-MID-KVINLART-NEW(TRANS-INDX)         
126788                                                                          
126789     ADD +1                  TO TRANS-INDX                                
126790     IF TRANS-INDX           >  MAX-TRANS-INDX                            
126791        PERFORM S03-SKICKA-UPPF-TRANS-W60191                              
126792     END-IF                                                               
126793     .                                                                    
126794     EJECT                                                                
126795 S11-SKAPA-DELINLAGD-RAD    SECTION.                                      
126796                                                                          
126797     COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                           
126798                                                                          
126799     MOVE SPAR-RAD-W6D121    TO RAD-W6D121                                
126800     MOVE SPACE              TO RAD-ADINLOMR                              
126801                                RAD-ADINLOMR-NXT                          
126802     MOVE ZERO               TO RAD-IDILIRAD                              
126803                                RAD-IDILIST                               
126804                                RAD-IDINLVGN                              
126805     MOVE DAGENS-DATUM       TO RAD-TIUPPDAT                              
126806     MOVE 'INL'              TO RAD-KDINLSTA                              
126807     MOVE WS-KVINLART-UPP    TO RAD-KVINLART                              
126808                                                                          
126809     PERFORM IMS-ISRT-W6D121-ARTRAD                                       
126810     .                                                                    
126811     EJECT                                                                
126812                                                                          
129800 MFS-RENSA-FAELT-UT SECTION.                                              
129900                                                                          
130000*    --- ALLA UTDATA-FÄLT                                                 
130100     MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR                                
130200                               MOD-BEART                                  
130300                               MOD-KDSORT                                 
130400                               MOD-KDFARLIG                               
130500                               MOD-ADINLOMR-FB                            
130600                               MOD-KVINLART                               
130700                               MOD-KVINLART-VOR                           
130800                               MOD-ADLAGOMR                               
130900                               MOD-ADGANG                                 
131000                               MOD-ADPLATS                                
131100                               MOD-KVINLART-VOR                           
131200                               MOD-ADLAGOMR                               
131300                               MOD-ADGANG                                 
131400                               MOD-ADPLATS                                
131900     MOVE +1                TO RAD-INDX                                   
132000     PERFORM UNTIL RAD-INDX > +3                                          
132100       MOVE MFS-RENSA-FAELT TO MOD-ADBUFFOMR(RAD-INDX)                    
132200                               MOD-ADBUFFGANG(RAD-INDX)                   
132300                               MOD-ADBUFFPL(RAD-INDX)                     
132500       ADD +1 TO RAD-INDX                                                 
132600     END-PERFORM                                                          
132700     .                                                                    
132800     EJECT                                                                
132900 MFS-RENSA-FAELT-IN SECTION.                                              
133000                                                                          
133100*    --- ALLA INDATA-FÄLT                                                 
133200     MOVE MFS-RENSA-FAELT TO MOD-KVINLART-DIN-UPP                         
133610                             MOD-KVINLART-UPP                             
133700                             MOD-IDANSTNR-UPP                             
133800                             MOD-ADINLOMR-NXT-UPP                         
133900     .                                                                    
134000     EJECT                                                                
134100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
134200                                                                          
134300*    --- ALLA INDATA-FÄLT                                                 
134400     MOVE MFS-ROER-EJ-FAELT TO MOD-KVINLART-DIN-UPP                       
134810                               MOD-KVINLART-UPP                           
134900                               MOD-IDANSTNR-UPP                           
135000                               MOD-ADINLOMR-NXT-UPP                       
135100     .                                                                    
135200     EJECT                                                                
135210 MFS-ROER-EJ-FAELT-UT SECTION.                                            
135220                                                                          
135230*    --- ALLA UTDATA-FÄLT                                                 
135240     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR                                
135250                               MOD-BEART                                  
135260                               MOD-KDSORT                                 
135270                               MOD-KDFARLIG                               
135280                               MOD-ADINLOMR-FB                            
135290                               MOD-KVINLART                               
135291                               MOD-KVINLART-VOR                           
135292                               MOD-ADLAGOMR                               
135293                               MOD-ADGANG                                 
135294                               MOD-ADPLATS                                
135295                               MOD-KVINLART-VOR                           
135296                               MOD-ADLAGOMR                               
135297                               MOD-ADGANG                                 
135298                               MOD-ADPLATS                                
135306     MOVE +1                TO RAD-INDX                                   
135307     PERFORM UNTIL RAD-INDX > +3                                          
135308       MOVE MFS-ROER-EJ-FAELT TO MOD-ADBUFFOMR(RAD-INDX)                  
135309                                 MOD-ADBUFFGANG(RAD-INDX)                 
135310                                 MOD-ADBUFFPL(RAD-INDX)                   
135312       ADD +1 TO RAD-INDX                                                 
135313     END-PERFORM                                                          
135314     .                                                                    
135315     EJECT                                                                
135320 MFS-FORM-ATTR SECTION.                                                   
135400                                                                          
135500*    --- ALLA INDATA-FÄLT                                                 
135600     MOVE MFS-FORMATETS-ATTR TO MOD-KVINLART-DIN-UPP-ATTR                 
136010                                MOD-KVINLART-UPP-ATTR                     
136100                                MOD-IDANSTNR-UPP-ATTR                     
136200                                MOD-ADINLOMR-NXT-UPP-ATTR                 
136300     .                                                                    
136400     EJECT                                                                
137700* --- IMS SEKTIONER ---                                                   
137800     SKIP3                                                                
137900 IMS-GET-MSG SECTION.                                                     
138000                                                                          
138100     MOVE '  QC' TO GODK-STATUSKODER                                      
138200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
138300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138400     PERFORM IMS-STATUSKONTROLL                                           
138500     .                                                                    
138600     SKIP3                                                                
138700 IMS-INSERT-MSG SECTION.                                                  
138800                                                                          
138810     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
138820       MOVE '0' TO MFS-KDHUVOMR                                           
139100     END-IF                                                               
139200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
139300     MOVE SPACE TO GODK-STATUSKODER                                       
139400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
139500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
139600     PERFORM IMS-STATUSKONTROLL                                           
139700     .                                                                    
139800     EJECT                                                                
139900 IMS-ISRT-ALT1-MSG-6191 SECTION.                                          
140000                                                                          
140100     MOVE SPACE TO GODK-STATUSKODER                                       
140200     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF             
140300     IF ALT1-STATUS-CODE NOT = SPACE                                      
140400       CALL FELLOG                                                        
140500     END-IF                                                               
140600     .                                                                    
140700     SKIP3                                                                
140800 IMS-PURG-ALT1-MSG-6191 SECTION.                                          
140900                                                                          
141000     MOVE SPACE TO GODK-STATUSKODER                                       
141100     CALL CBLTDLI USING PURG ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF             
141200     IF ALT1-STATUS-CODE NOT = SPACE                                      
141300       CALL FELLOG                                                        
141400     END-IF                                                               
141500     .                                                                    
141600     EJECT                                                                
141610* -- LÄSNING OCH UPPDATERING AV W6D1                                      
141620     SKIP3                                                                
141700 IMS-GU-W6D111-ART SECTION.                                               
141800                                                                          
141900     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X                            
141910                    '&IDDC     =' W-IDDC ')'                              
142000          DELIMITED BY SIZE INTO SSA1                                     
142100     MOVE '  GE' TO GODK-STATUSKODER                                      
142200     CALL CBLTDLI USING GU INLA-C-PCB DLI-IO-AREA1 SSA1                   
142300     MOVE INLA-C-STATUS-CODE TO STATUS-WS                                 
142400     PERFORM IMS-STATUSKONTROLL                                           
142500     .                                                                    
142600     SKIP3                                                                
142700 IMS-GNP-W6D121-ARTRAD SECTION.                                           
142800                                                                          
142900     STRING 'W6INLA21(IDLEVNRK =' W-RAD-IDLEVNR-KOLLI-X                   
142910                    '&IDOKOLLI =' W-RAD-IDOKOLLI-X ')'                    
143000          DELIMITED BY SIZE INTO SSA1                                     
143110     MOVE '  GE' TO GODK-STATUSKODER                                      
143200     CALL CBLTDLI USING GNP INLA-C-PCB DLI-IO-AREA2 SSA1                  
143300     MOVE INLA-C-STATUS-CODE TO STATUS-WS                                 
143400     PERFORM IMS-STATUSKONTROLL                                           
143500     .                                                                    
143600     EJECT                                                                
143601 IMS-GNP-W6D121-ARTRAD-LAST SECTION.                                      
143602                                                                          
143603     MOVE 'W6INLA21*L' TO SSA1                                            
143605     MOVE '  GE' TO GODK-STATUSKODER                                      
143606     CALL CBLTDLI USING GNP INLA-C-PCB DLI-IO-AREA2 SSA1                  
143607     MOVE INLA-C-STATUS-CODE TO STATUS-WS                                 
143608     PERFORM IMS-STATUSKONTROLL                                           
143609     .                                                                    
143610     SKIP3                                                                
143611 IMS-GHNP-W6D121-ARTRAD SECTION.                                          
143620                                                                          
143621     STRING 'W6INLA21(IDLEVNRK =' W-RAD-IDLEVNR-KOLLI-X                   
143622                    '&IDOKOLLI =' W-RAD-IDOKOLLI-X ')'                    
143642          DELIMITED BY SIZE INTO SSA1                                     
143650     MOVE '  GE' TO GODK-STATUSKODER                                      
143660     CALL CBLTDLI USING GHNP INLA-C-PCB DLI-IO-AREA2 SSA1                 
143670     MOVE INLA-C-STATUS-CODE TO STATUS-WS                                 
143680     PERFORM IMS-STATUSKONTROLL                                           
143690     .                                                                    
143691     EJECT                                                                
143692 IMS-REPL-W6D121-ARTRAD SECTION.                                          
143693                                                                          
143694     MOVE '  ' TO GODK-STATUSKODER                                        
143695     CALL CBLTDLI USING REPL INLA-C-PCB DLI-IO-AREA2                      
143696     MOVE INLA-C-STATUS-CODE TO STATUS-WS                                 
143697     PERFORM IMS-STATUSKONTROLL                                           
143698     .                                                                    
143699     SKIP3                                                                
143700 IMS-ISRT-W6D121-ARTRAD SECTION.                                          
143701                                                                          
143702     MOVE 'W6INLA21 ' TO SSA1                                             
143703     MOVE '  ' TO GODK-STATUSKODER                                        
143704     CALL CBLTDLI USING ISRT INLA-C-PCB DLI-IO-AREA2 SSA1                 
143705     MOVE INLA-C-STATUS-CODE TO STATUS-WS                                 
143706     PERFORM IMS-STATUSKONTROLL                                           
143707     .                                                                    
143708     EJECT                                                                
143709 IMS-GU-INLD-INLD01 SECTION.                                              
143710                                                                          
143711     STRING 'W6INLD01(W6D1C1KY>=' W-W6D1C1KY-MIN-X                        
143712                    '&W6D1C1KY<=' W-W6D1C1KY-MAX-X ')'                    
143713          DELIMITED BY SIZE INTO SSA1                                     
143714     MOVE '  GE' TO GODK-STATUSKODER                                      
143715     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA3 SSA1                     
143716     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
143717     PERFORM IMS-STATUSKONTROLL                                           
143718     .                                                                    
143719     EJECT                                                                
143720* -- LÄSNING OCH UPPDATERING AV WDD8                                      
143800     SKIP3                                                                
144813 IMS-GU-WDD811-CLAGER  SECTION.                                           
144820                                                                          
144830     STRING 'WLARTD01*P(IDARTNR  =' W-WDD801KY-X ')'                      
144840          DELIMITED BY SIZE INTO SSA1                                     
144850     STRING 'WLARTD11(IDDC     =' W-IDDC ')'                              
144860          DELIMITED BY SIZE INTO SSA2                                     
144870     MOVE '  GE' TO GODK-STATUSKODER                                      
144880     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA3 SSA1 SSA2                
144890     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
144891     PERFORM IMS-STATUSKONTROLL                                           
144892     .                                                                    
144893     EJECT                                                                
144906 IMS-GNP-WDD811-MBUFFPL SECTION.                                          
144907                                                                          
144908     STRING 'WLARTD11(IDDC     =' W-IDDC ')'                              
144909          DELIMITED BY SIZE INTO SSA1                                     
144910     MOVE '  GE' TO GODK-STATUSKODER                                      
144911     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA3 SSA1                    
144912     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
144913     PERFORM IMS-STATUSKONTROLL                                           
144914     .                                                                    
144915     EJECT                                                                
144936* -- LÄSNING OCH UPPDATERING AV W6G1                                      
144937     SKIP3                                                                
144940 IMS-GU-W6G130-PLACERING SECTION.                                         
145000                                                                          
145100     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-GX01-X ')'                    
145200          DELIMITED BY SIZE INTO SSA1                                     
145300     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
145400          DELIMITED BY SIZE INTO SSA2                                     
145500     MOVE '  GE' TO GODK-STATUSKODER                                      
145600     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA4 SSA1 SSA2                
145700     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
145800     PERFORM IMS-STATUSKONTROLL                                           
145900     .                                                                    
146000     EJECT                                                                
146100 IMS-GU-UPFA-01  SECTION.                                                 
146200     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
146300          DELIMITED BY SIZE INTO SSA1                                     
146400     MOVE '  GE' TO GODK-STATUSKODER                                      
146500     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-UPFA SSA1                      
146600     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
146700     PERFORM IMS-STATUSKONTROLL                                           
146800     .                                                                    
146900     SKIP2                                                                
147000 IMS-GNP-UPFA11 SECTION.                                                  
147100                                                                          
147200     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
147210          DELIMITED BY SIZE INTO SSA1                                     
147220     MOVE 'W6UPFA11 ' TO SSA2                                             
147230     MOVE '  GE' TO GODK-STATUSKODER                                      
147240     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
147250     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
147260     PERFORM IMS-STATUSKONTROLL                                           
147270     .                                                                    
147280     SKIP3                                                                
147290 IMS-GNP-UPFA12 SECTION.                                                  
147291                                                                          
147292     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
147293          DELIMITED BY SIZE INTO SSA1                                     
147294     MOVE 'W6UPFA12 ' TO SSA2                                             
147295     MOVE '  GE' TO GODK-STATUSKODER                                      
147296     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
147297     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
147298     PERFORM IMS-STATUSKONTROLL                                           
147299     .                                                                    
147300     SKIP3                                                                
147301 IMS-GU-WDB601    SECTION.                                                
147302     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
147303          DELIMITED BY SIZE INTO SSA1                                     
147304     MOVE '  GE' TO GODK-STATUSKODER                                      
147305     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
147306     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
147307     PERFORM IMS-STATUSKONTROLL                                           
147308     IF SEGMENT-SAKNAS                                                    
147309         MOVE SPACE TO DCS-KDDC                                           
147310     END-IF                                                               
147311     .                                                                    
147320 IMS-STATUSKONTROLL SECTION.                                              
147400                                                                          
147500     SET STATUS-IX TO 1                                                   
147600     SEARCH GODK-STATUS                                                   
147700       AT END CALL FELLOG                                                 
147800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
147900     END-SEARCH                                                           
148000     .                                                                    
